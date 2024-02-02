mod helpers;
mod config;
mod aws;


// use std::error::Error;
use std::collections::HashMap;
use clap::{Parser, Subcommand};
// use serde::{Deserialize, Serialize};
use log::{ LevelFilter, debug, error };
use env_logger::Builder;
use itertools::Itertools;
use tabled::Table;

/// CFTool - CloudFormation made easy
#[derive(Parser, Debug)]
#[command(name = "CFTool")]
#[command(author = "Christopher Mackinga <christopher.mackinga@gmail.com>")]
#[command(version = "2.0.0-alpha")]
struct Cli {
    /// Explicitly specify the name of the config file
    #[arg(short, long, value_name = "FILE")]
    config: Option<String>,
    #[arg(long)]
    /// Enable verbose debug logging
    debug: bool,
    #[command(subcommand)]
    command: BaseCommands,
}

#[derive(Subcommand, Debug)]
enum BaseCommands {
    /// List available deployments for usage with other commands (Current AWS Account)
    List,
    /// List available deployments for usage with other commands (All AWS Accounts)
    ListAll,
    /// Get an overview of all deployments matching current AWS Account
    Status,
    /// Give an overview of the specified deployment
    Describe {
        deployment: String,
    },
    /// Diff the local Template and Configuration against the provisioned Stack in CloudFormation
    Diff {
        deployment: String,
    },
    /// Create a new Stack in CloudFormation using the local Template and Configuration
    Create {
        #[arg(short, long)]
        wait: bool,
        deployment: String,
    },
    /// Update an existing Stack in CloudFormation using the local Template and Configuration
    Update {
        #[arg(short, long)]
        wait: bool,
        deployment: String,
        
    },
    /// Delete the existing Stack in CloudFormation which is associated with this deployment
    Delete {
        #[arg(short, long)]
        wait: bool,
        deployment: String,
    },
    /// Subcommands for comparing distinct deployments
    Compare {
        #[command(subcommand)]
        compare_command: CompareCommands,
    },
}

#[derive(Subcommand, Debug)]
enum CompareCommands {
    /// Compare the local templates of two distinct deployments
    Templates {
        deployment1: String,
        deployment2: String,
    },
    /// Compare the local configurations of two distinct deployments
    Configs {
        deployment1: String,
        deployment2: String,
    },
    /// Compare the remote stacks of two distinct deployments
    Stacks {
        deployment1: String,
        deployment2: String,
    },
}

fn main() {
    let cli = Cli::parse();
    debug!("cli.debug: {}", cli.debug);

    let log_level = if cli.debug {LevelFilter::max()} else {LevelFilter::Error};
    Builder::new().filter_level(log_level).init();

    let result = config::get_configurations(cli.config.unwrap_or(String::from("./cftool-config.yaml")));

    if result.is_err() {
        error!("Failed to load config file: {}", result.unwrap_err());
        std::process::abort();
    }

    let configs = result.unwrap();
    
    match cli.command {
        BaseCommands::List => { list_deployments(configs, false); }
        BaseCommands::ListAll => { list_deployments(configs, true); }
        BaseCommands::Status => { status(configs); }
        BaseCommands::Describe { deployment } => { describe_deployment(configs, deployment); }
        BaseCommands::Diff { deployment } => { diff_deployment(configs, deployment); }
        BaseCommands::Create { deployment, wait } => { create_deployment(configs, deployment, wait); }
        BaseCommands::Update { deployment, wait } => { update_deployment(configs, deployment, wait); }
        BaseCommands::Delete { deployment, wait } => { delete_deployment(configs, deployment, wait); }
        BaseCommands::Compare { compare_command } => {
            match compare_command {
                CompareCommands::Templates { deployment1, deployment2 } => { compare_template(configs, deployment1, deployment2); }  
                CompareCommands::Configs { deployment1, deployment2 } => { compare_config(configs, deployment1, deployment2); }  
                CompareCommands::Stacks { deployment1, deployment2 } => { compare_stack(configs, deployment1, deployment2); }  
            }
        } 
    };

}

fn status(configs: HashMap<String, config::Configuration>) -> () {
    
    let _ = aws::get_current_aws_profile();
    println!();
    if let Ok(current_account_id) = aws::get_aws_account_id() {
        let diffs = configs
            .iter()
            .filter(|c| c.1.aws_account_id == current_account_id) 
            .sorted_by_key(|c| c.0)
            .map(|c| {
                println!("Diffing {}...", c.0);
                aws::create_deployment_diff(&configs, &c.0)
            })
            .filter_map(Result::ok)
            .collect::<Vec<helpers::DeploymentDiff>>();

        println!("{}", Table::new(diffs).to_string())
        // diffs.iter()
        //      .for_each(|d| println!("{}   CONFIG: {}    TEMPLATE: {}", d.deployment_name, d.config_state(), d.template_state()));
    }
}


fn diff_deployment(configs: HashMap<String, config::Configuration>, deployment: String) -> () {
    let Ok(diff) = aws::create_deployment_diff(&configs, &deployment) else { return };
    
    println!("{}", diff.deployment_name);
    diff.print_diff();
    println!();

}


fn describe_deployment(configs: HashMap<String, config::Configuration>, deployment: String) -> () {
    if let Some(config) = config::get_deployment(&configs, &deployment) {
        println!("\nPath: {}\n", &config.path);
        println!("{}\n", &config);
    }
}


fn list_deployments(configs: HashMap<String, config::Configuration>, all: bool) -> () {
    let mut current_account_id: String = "".to_string();

    if !all {
        let _ = aws::get_current_aws_profile();
        println!();
        if let Ok(id) = aws::get_aws_account_id() {
            current_account_id = id;
        }
    }

    configs.iter()
           .filter(|c| c.1.aws_account_id == current_account_id)
           .map(|c| c.0)
           .sorted()
           .for_each(|c| println!("{}", c));
}


fn compare_stack(configs: HashMap<String, config::Configuration>, deployment1: String, deployment2: String) -> () {
    let Some(config1) = config::get_deployment(&configs, &deployment1) else { return };
    let Some(config2) = config::get_deployment(&configs, &deployment2) else { return };

    let Ok(stack_config1) = aws::describe_stack(&config1) else { return };
    let Ok(stack_config2) = aws::describe_stack(&config2) else { return };

    let Ok(stack_template1) = aws::get_stack_template(&config1) else { return };
    let Ok(stack_template2) = aws::get_stack_template(&config2) else { return };

    let stack_cfg1 = stack_config1.to_string();
    let stack_cfg2 = stack_config2.to_string();
    
    if &stack_cfg1 == &stack_cfg2 {
        println!("CONFIG: IDENTICAL");
    } else {
        println!("CONFIG:");
        helpers::print_diff(&stack_cfg1.to_string(), &stack_cfg2.to_string());
        println!("\n");
    }

    if &stack_template1 == &stack_template2 {
        println!("TEMPLATE: IDENTICAL");
    } else {
        println!("TEMPLATE:");
        helpers::print_diff(&stack_template1.to_string(), &stack_template2.to_string());
    }

}


fn compare_template(configs: HashMap<String, config::Configuration>, deployment1: String, deployment2: String) -> () {
    let Some(config1) = config::get_deployment(&configs, &deployment1) else { return };
    let Some(config2) = config::get_deployment(&configs, &deployment2) else { return };

    let Ok(template_str1) = config::get_template(&config1.path) else { return };
    let Ok(template_str2) = config::get_template(&config2.path) else { return };

    if &template_str1 == &template_str2 {
        println!("TEMPLATE: IDENTICAL")
    } else {
        helpers::print_diff(&template_str1, &template_str2);
    }
}


fn compare_config(configs: HashMap<String, config::Configuration>, deployment1: String, deployment2: String) -> () {
    let Some(config1) = config::get_deployment(&configs, &deployment1) else { return };
    let Some(config2) = config::get_deployment(&configs, &deployment2) else { return };

    let cfg_str1 = config1.to_string();
    let cfg_str2 = config2.to_string();

    if &cfg_str1 == &cfg_str2 {
        println!("CONFIG: IDENTICAL")
    } else {
        helpers::print_diff(&cfg_str1, &cfg_str2);
    }
}


// TODO confirm stack does not exist before creating
fn create_deployment(configs: HashMap<String, config::Configuration>, deployment: String, wait: bool) -> () {
    let Some(config) = config::get_deployment(&configs, &deployment) else { return };

    match aws::create_stack(config) {
        Ok(res) => print!("{}", res),
        Err(error) => error!("Failed to create stack '{}'\n{}", deployment, error)
    }
}


// TODO confirm stack exists before updating 
fn update_deployment(configs: HashMap<String, config::Configuration>, deployment: String, wait: bool) -> () {
    let Some(config) = config::get_deployment(&configs, &deployment) else { return };

    match aws::update_stack(config) {
        Ok(res) => print!("{}", res),
        Err(error) => error!("Failed to update stack '{}'\n{}", deployment, error)
    }
}


// TODO confirm stack exists before deleting 
fn delete_deployment(configs: HashMap<String, config::Configuration>, deployment: String, wait: bool) -> () {
    let Some(config) = config::get_deployment(&configs, &deployment) else { return };

    match aws::delete_stack(config) {
        Ok(res) => print!("{}", res),
        Err(error) => error!("Failed to delete stack '{}'\n{}", deployment, error)
    }
}
