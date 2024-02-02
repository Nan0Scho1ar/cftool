use crate::{helpers, config};

use std::env;
use std::fmt;
use std::error::Error;
use std::process::Command;
use serde::{Deserialize, Serialize};
use log::error;

#[derive(Serialize, Deserialize, Debug)]
pub enum StackStatus {
    #[serde(rename = "CREATE_COMPLETE")]
    CreateComplete,
    #[serde(rename = "UPDATE_COMPLETE")]
    UpdateComplete,
}

#[derive(Serialize, Deserialize, Debug)]
pub enum StackDriftStatus {
    #[serde(rename = "NOT_CHECKED")]
    NotChecked,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct StackDriftInformation {
    #[serde(rename = "StackDriftStatus")]
    pub stack_drift_status: StackDriftStatus
}

#[derive(Serialize, Deserialize, Debug)]
pub struct StackRollbackConfiguration {
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct StackParameter {

    #[serde(rename = "ParameterKey")]
    pub parameter_key: String,
    #[serde(rename = "ParameterValue")]
    pub parameter_value: String
}

#[derive(Serialize, Deserialize, Debug)]
pub struct StackTag {

    #[serde(rename = "Key")]
    pub tag_key: String,
    #[serde(rename = "Value")]
    pub tag_value: String
}
#[derive(Serialize, Deserialize, Debug)]
pub struct Stack {
    #[serde(rename = "StackId")]
    pub stack_id: String,
    #[serde(rename = "StackName")]
    pub stack_name: String,
    #[serde(rename = "Description")]
    pub description: Option<String>,
    #[serde(rename = "Parameters")]
    pub parameters: Option<Vec<StackParameter>>,
    #[serde(rename = "CreationTime")]
    pub creation_time: String,
    #[serde(rename = "Capabilities")]
    pub capabilities: Option<Vec<String>>,
    // #[serde(rename = "")]
    // rollback_configuration: StackRollbackConfiguration,
    // #[serde(rename = "")]
    // stack_status: StackStatus,
    #[serde(rename = "DisableRollback")]
    pub disable_rollback: bool,
    #[serde(rename = "NotificationARNs")]
    pub notification_arns: Vec<String>,
    #[serde(rename = "Tags")]
    pub tags: Vec<StackTag>,
    // #[serde(rename = "")]
    // drift_information: StackDriftInformation,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Stacks {
    #[serde(rename = "Stacks")]
    pub stacks: Vec<Stack>
}

impl fmt::Display for Stack {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl Stack {
    fn format_parameters(&self) -> String {
        if self.parameters.is_none() || self.parameters.as_ref().unwrap().is_empty() {
            return "None".to_string()
        }

        let mut params: Vec<StackParameter> = self.parameters.as_ref().unwrap().to_vec();
        params.sort_by(|a, b| a.parameter_key.cmp(&b.parameter_key));

        let params = params
            .iter()
            .map(|key| format!("{}: {}", key.parameter_key.clone(), key.parameter_value.clone()))
            .collect::<Vec<String>>()
            .join("\n    ");

        format!("{}{}", "\n    ", params)
    }

    pub fn to_string(&self) -> String {
        let arn_fragments: Vec<String> = self.stack_id.split(":").map(|str| str.to_string()).collect();
        let region = arn_fragments.get(3).unwrap_or(&String::new()).clone();
        let aws_account_id = arn_fragments.get(4).unwrap_or(&String::new()).clone();
        
        format!(
            "Name: {}\nRegion: {}\nAwsAccountId: {}\nCapabilities: {}\nParameters: {}",
            self.stack_name,
            region,
            aws_account_id,
            self.capabilities
                .as_ref()
                .map_or_else(|| "None".to_string(), |cap| cap.join(" ")),
            self.format_parameters()
        )
    }
}

pub enum CloudFormation {
    Create,
    Update,
    Delete,
    Describe,
    GetTemplate
}

pub fn command(args: Vec<String>) -> Result<String, String> {
    let mut aws_cli_command = Command::new("aws");
    for arg in &args[0..] {
        aws_cli_command.arg(arg);
    }        

    if log::log_enabled!(log::Level::Debug) {
        dbg!(&aws_cli_command);
    }

    match aws_cli_command.output() {
        Ok(output) =>  {
            if output.status.success() {
                let stdout = String::from_utf8_lossy(&output.stdout).to_string();
                Ok(stdout)
            } else {
                let stderr = String::from_utf8_lossy(&output.stderr).to_string();
                Err(format!("AWS CLI error:\n{}", stderr).into())
            }
        }
        Err(e) => {
            Err(format!("Error executing AWS CLI command: {}", e).into())
        }
    }
}

pub fn get_stack_template(config: &config::Configuration) -> Result<String, Box<dyn Error>> {
    profile_valid(&config)?;
    let cmd = config.to_cmd(CloudFormation::GetTemplate);
    let result = command(cmd);
    match result {
        Ok(r) => Ok(r[..r.len()-1].to_string()),
        Err(e) => {
            error!("{}", format!("Failed to fetch stack template for deployment '{}':\n{}", config.deployment_name, e));
            Err(e.into())
        },
    }
    
}

pub fn describe_stack(config: &config::Configuration) -> Result<Stack, Box<dyn Error>> {
    profile_valid(&config)?;
    let cmd = config.to_cmd(CloudFormation::Describe);
    let result = command(cmd);

    match result {
        Ok(r) => {
             match serde_json::from_str::<Stacks>(&r) {
                Ok(stacks) => {
                    match helpers::get_single(stacks.stacks) {
                        Ok(stack) => Ok(stack),
                        Err(e) => {
                            error!("{}", format!("Failed to fetch stack for deployment '{}':\n{}", config.deployment_name, e));
                            Err(e.into())
                        },
                    }
                },
                Err(e) => {
                    error!("{}", format!("Failed to parse stack for deployment '{}':\n{}", config.deployment_name, e));
                    Err(e.into())
                },
            }
        }
        Err(e) => {
            error!("{}", format!("Failed to fetch stack for deployment '{}':\n{}", config.deployment_name, e));
            Err(e.into())
        },
    }

}


pub fn create_stack(config: config::Configuration) -> Result<String, String> {
    profile_valid(&config)?;
    let cmd = config.to_cmd(CloudFormation::Create);

    command(cmd)
}

pub fn update_stack(config: config::Configuration) -> Result<String, String> {
    profile_valid(&config)?;
    let cmd = config.to_cmd(CloudFormation::Update);

    command(cmd)
}

pub fn delete_stack(config: config::Configuration) -> Result<String, String> {
    profile_valid(&config)?;
    let cmd = config.to_cmd(CloudFormation::Delete);

    command(cmd)
}

fn profile_valid(config: &config::Configuration) -> Result<(), String> {
    get_current_aws_profile()?;
    let current_account_id = get_aws_account_id()?;
    if current_account_id == config.aws_account_id {
        Ok(())
    } else {
        Err("AWS_PROFILE does not match requested deployment".to_string())
    }
}

pub fn get_current_aws_profile() -> Result<(), String> {
    match env::var("AWS_PROFILE") {
        Ok(value) => {
            println!("Using AWS_PROFILE=\"{}\"", value);
            Ok(())
        },
        Err(_) => Err("AWS_PROFILE is not set".to_string())
    }
}

pub fn get_aws_account_id() -> Result<String, String> {
    let cmd = vec!["sts".to_string(),      "get-caller-identity".to_string(),
                   "--query".to_string(),  "Account".to_string(),
                   "--output".to_string(), "text".to_string()];

    Ok(command(cmd)?.trim().to_string())
}
