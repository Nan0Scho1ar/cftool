use std::error::Error;
use serde::{Serialize, Deserialize};
use std::collections::HashMap;
use std::{fs, fmt};

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct ConfigFile {
    pub configurations:  HashMap<String, ConfigurationDefinition>
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct ConfigurationDefinition {
    pub name: String,
    pub path: String,
    pub aws_account_id: String,
    pub capabilities: Option<String>,
    pub parameters: Option<HashMap<String, String>>,
    pub instances: Option<HashMap<String, ConfigurationInstance>>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct ConfigurationInstance {
    pub name: Option<String>,
    pub path: Option<String>,
    pub aws_account_id: Option<String>,
    pub capabilities: Option<String>,
    pub parameters: Option<HashMap<String, String>>
}

fn parse_config_file() -> Result<ConfigFile, Box<dyn Error>> {
    let config = fs::read_to_string(String::from("./src/example.yaml"))?;
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

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Configuration {
    pub name: String,
    pub path: String,
    pub aws_account_id: String,
    pub capabilities: Option<String>,
    pub parameters: HashMap<String, String>
}

impl fmt::Display for Configuration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "name: {}\npath: {}\naws_account_id: {}\ncapabilities: {}\nparameters: {}",
            self.name,
            self.path,
            self.aws_account_id,
            self.capabilities
                .as_ref()
                .map_or_else(|| "None".to_string(), |cap| cap.to_string()),
            self.format_parameters()
        )
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
}

pub fn get_configurations() -> Result<HashMap<String, Configuration>, Box<dyn Error>> {
    let config = parse_config_file()?;
    
    let deployments =
        config.configurations
              .iter()
              .filter_map(|(stack_key, stack)| {
                  stack.instances.as_ref().map(|instances| {
                      instances.iter().map(move |(instance_key, instance)| {
                          let deployment_name = format!("{}:{}", stack_key, instance_key);

                          let deployment = Configuration {
                              name: instance.name
                                            .as_ref()
                                            .unwrap_or(&stack.name).to_string(),
                              path: instance.path
                                            .as_ref()
                                            .unwrap_or(&stack.path).to_string(),
                              aws_account_id: instance.aws_account_id
                                                      .as_ref()
                                                      .unwrap_or(&stack.aws_account_id).to_string(),
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
