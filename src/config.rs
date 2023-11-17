use std::error::Error;
use serde::{Serialize, Deserialize};
use std::collections::HashMap;
use std::fs;

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Config {
    pub configurations:  HashMap<String, Configuration>
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Configuration {
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

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Deployment {
    pub name: String,
    pub path: String,
    pub aws_account_id: String,
    pub capabilities: Option<String>,
    pub parameters: HashMap<String, String>
}

pub fn parse_config_file() -> Result<Config, Box<dyn Error>> {
    let config = fs::read_to_string(String::from("./src/example.yaml"))?;
    let conf: Result<Config, _> = serde_yaml::from_str(&config);
    match conf {
        Ok(cfg) => {

            Ok(cfg)
        }
        Err(error) => {
            Err(error.to_string().into())
        }
    }
}

pub fn get_configurations() -> Result<HashMap<String, Deployment>, Box<dyn Error>> {
    let config = parse_config_file()?;
    
    let deployments = config.configurations.iter()
        .filter_map(|(stack_key, stack)| {
            stack.instances.as_ref().map(|instances| {
                instances.iter().map(move |(instance_key, instance)| {
                    let deployment_name = format!("{}:{}", stack_key, instance_key);

                    let deployment = Deployment {
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
