use crate::helpers;
use std::error::Error;
use std::process::Command;
use serde::{Deserialize, Serialize};

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

#[derive(Serialize, Deserialize, Debug)]
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

pub fn command(args: Vec<&str>) -> Result<String, String> {
    // AWS CLI command to list objects in an S3 bucket
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

pub fn get_stack_template(stack_name: String) -> Result<String, Box<dyn Error>> {
    let result = command(vec!["cloudformation", "get-template",
                              "--stack-name", &stack_name,
                              "--template-stage", "Original",
                              "--query", "TemplateBody",
                              "--output", "text"])?;
    Ok(result[..result.len()-1].to_string())
}

pub fn describe_stack(stack_name: String) -> Result<Stack, Box<dyn Error>> {
    let result = command(vec!["cloudformation", "describe-stacks", "--stack-name", &stack_name])?;
    let stacks: Stacks = serde_json::from_str(&result)?;
    Ok(helpers::get_single(stacks.stacks)?)
}

pub fn describe_stacks() -> Result<Vec<Stack>, Box<dyn Error>> {
    let result = command(vec!["cloudformation", "describe-stacks"])?;
    let stacks: Stacks = serde_json::from_str(&result)?;
    Ok(stacks.stacks)
}


pub fn create_stack(region: Option<String>,
                    stack_name: Option<String>,
                    template_body: Option<String>,
                    capabilities: Option<String>,
                    parameters: Option<String>) -> Result<String, String> {

    let mut cmd: Vec<String> = Vec::new();

    if let Some(reg) = region {
        cmd.push("--region".to_string());
        cmd.push(reg);
    }
    if let Some(name) = stack_name {
        cmd.push("--stack-name".to_string());
        cmd.push(name);
    }
    if let Some(body) = template_body {
        cmd.push("--template-body".to_string());
        cmd.push(body);
    }
    if let Some(caps) = capabilities {
        cmd.push("--capabilities".to_string());
        cmd.push(caps);
    }
    if let Some(params) = parameters {
        cmd.push("--parameters".to_string());
        cmd.push(params);
    }

    Ok(cmd.join(" "))

}
