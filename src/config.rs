use std::error::Error;
use serde::{Serialize, Deserialize};
use std::collections::HashMap;
use std::{fs, fmt};
use log::error;

use crate::aws::CloudFormation;

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct ConfigFile {

    #[serde(rename = "Configurations")]
    pub configurations:  HashMap<String, ConfigurationDefinition>
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct ConfigurationDefinition {
    #[serde(rename = "Name")]
    pub name: String,
    #[serde(rename = "Path")]
    pub path: String,
    #[serde(rename = "Region")]
    pub region: String,
    #[serde(rename = "Capabilities")]
    pub capabilities: Option<String>,
    #[serde(rename = "Parameters")]
    pub parameters: Option<HashMap<String, String>>,
    #[serde(rename = "Instances")]
    pub instances: Option<HashMap<String, ConfigurationInstance>>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct ConfigurationInstance {
    #[serde(rename = "Name")]
    pub name: Option<String>,
    #[serde(rename = "Path")]
    pub path: Option<String>,
    #[serde(rename = "Region")]
    pub region: Option<String>,
    #[serde(rename = "AwsAccountId")]
    pub aws_account_id: Option<String>,
    #[serde(rename = "Capabilities")]
    pub capabilities: Option<String>,
    #[serde(rename = "Parameters")]
    pub parameters: Option<HashMap<String, String>>
}

fn parse_config_file(config_file_path: String) -> Result<ConfigFile, Box<dyn Error>> {
    let config = fs::read_to_string(config_file_path)?;
    let conf: Result<ConfigFile, _> = serde_yaml::from_str(&config);
    match conf {
        Ok(cfg) => {

            Ok(cfg)
        }
        Err(error) => {
            Err(error.to_string().into())
        }
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub struct Configuration {
    pub deployment_name: String,
    pub name: String,
    pub path: String,
    pub region: String,
    pub aws_account_id: String,
    pub capabilities: Option<String>,
    pub parameters: HashMap<String, String>
}

impl fmt::Display for Configuration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl Configuration {
    fn format_parameters(&self) -> String {
        if self.parameters.is_empty() {
            return "None".to_string();
        }

        let mut keys: Vec<_> = self.parameters.keys().collect();
        keys.sort();

        let params = keys
            .iter()
            .map(|key| format!("{}: {}", key, self.parameters.get(*key).unwrap()))
            .collect::<Vec<_>>()
            .join("\n    ");

        format!("{}{}", "\n    ", params)
    }

    pub fn to_string(&self) -> String {
        format!(
            "Name: {}\nRegion: {}\nAwsAccountId: {}\nCapabilities: {}\nParameters: {}",
            self.name,
            self.region,
            self.aws_account_id,
            self.capabilities
                .as_ref()
                .map_or_else(|| "None".to_string(), |cap| cap.to_string()),
            self.format_parameters()
        )
    }

    pub fn to_cmd(&self, cmd_type: CloudFormation) -> Vec<String> {
        let mut cmd: Vec<String> = vec!["cloudformation".to_string()];

        match cmd_type {
            CloudFormation::Create => {
                cmd.extend(vec!["create-stack".to_string(),
                                "--region".to_string(), self.region.clone(),
                                "--stack-name".to_string(), self.name.clone(),
                                "--template-body".to_string(), format!("file://{}", self.path.clone()),
                ]);
                if self.capabilities.is_some() {
                    cmd.extend(vec!["--capabilities".to_string(), self.capabilities.as_ref().unwrap().clone()]);
                };
                if !self.parameters.is_empty() {
                    cmd.extend(vec!["--parameters".to_string(),
                                    format!("[{}]",
                                            self.parameters
                                            .iter()
                                            .map(|(key, value)| format!("{{\"ParameterKey\": \"{}\", \"ParameterValue\": \"{}\"}}", key, value))
                                            .collect::<Vec<String>>()
                                            .join(","))]);
                };
            },
            CloudFormation::Update => {
                cmd.extend(vec!["update-stack".to_string(),
                                "--region".to_string(), self.region.clone(),
                                "--stack-name".to_string(), self.name.clone(),
                                "--template-body".to_string(), format!("file://{}", self.path.clone()),
                ]);
                if self.capabilities.is_some() {
                    cmd.extend(vec!["--capabilities".to_string(), self.capabilities.as_ref().unwrap().clone()]);
                };
                if !self.parameters.is_empty() {
                    cmd.extend(vec!["--parameters".to_string(),
                                    format!("[{}]",
                                            self.parameters
                                            .iter()
                                            .map(|(key, value)| format!("{{\"ParameterKey\": \"{}\", \"ParameterValue\": \"{}\"}}", key, value))
                                            .collect::<Vec<String>>()
                                            .join(","))]);
                };
            },
            CloudFormation::Delete => {
                cmd.extend(vec!["delete-stack".to_string(),
                                "--region".to_string(), self.region.clone(),
                                "--stack-name".to_string(), self.name.clone()
                ]);
            },
            CloudFormation::Describe => {
                cmd.extend(vec!["describe-stacks".to_string(),
                                "--region".to_string(), self.region.clone(),
                                "--stack-name".to_string(), self.name.clone()
                ]);
            },
            CloudFormation::GetTemplate => {
                cmd.extend(vec!["get-template".to_string(),
                                "--region".to_string(), self.region.clone(),
                                "--stack-name".to_string(), self.name.clone(),
                                "--template-stage".to_string(), "Original".to_string(),
                                "--query".to_string(), "TemplateBody".to_string(),
                                "--output".to_string(), "text".to_string()
                ]);
            },
        }

        cmd
    }
}

pub fn get_configurations(config_file_path: String) -> Result<HashMap<String, Configuration>, Box<dyn Error>> {
    let config = parse_config_file(config_file_path)?;
    
    let deployments =
        config.configurations
              .iter()
              .filter_map(|(stack_key, stack)| {
                  stack.instances.as_ref().map(|instances| {
                      instances.iter().map(move |(instance_key, instance)| {
                          let deployment_name = format!("{}:{}", stack_key, instance_key);

                          let deployment = Configuration {
                              deployment_name: deployment_name.to_string(),
                              name: instance.name
                                            .as_ref()
                                            .unwrap_or(&stack.name).to_string(),
                              path: instance.path
                                            .as_ref()
                                            .unwrap_or(&stack.path).to_string(),
                              region: instance.region
                                            .as_ref()
                                            .unwrap_or(&stack.region).to_string(),
                              aws_account_id: instance.aws_account_id
                                                      .as_ref()
                                                      .unwrap_or(&String::default()).to_string(),
                              capabilities: instance.capabilities
                                                    .as_ref()
                                                    .or(stack.capabilities.as_ref()).cloned(),
                              parameters: stack.parameters
                                               .clone()
                                               .unwrap_or_default()
                                               .into_iter()
                                               .chain(instance.parameters
                                                      .clone()
                                                      .unwrap_or_default()
                                               ).collect()
                          };

                          (deployment_name, deployment)
                      })
                  })
              })
              .flatten()
              .collect();

    Ok(deployments)
}

pub fn get_deployment(configs: &HashMap<String, Configuration>, deployment: &String) -> Option<Configuration> {
    let configuration = configs.get(deployment);
    match configuration {
        Some(config) => Some(config.clone()),
        None => {
            error!("Unknown deployment '{}'", deployment);
            None
        }
    }
}

pub fn get_template(template_file_path: &String) -> Result<String, Box<dyn Error>>  {
    let template = fs::read_to_string(template_file_path);
    if template.is_err() {
        error!("Failed to fetch template '{}': {}", template_file_path, template.as_ref().unwrap_err());
    }
    Ok(template?)
}
