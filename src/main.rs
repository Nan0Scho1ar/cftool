mod helpers;
mod config;
mod aws;


use std::path::PathBuf;
use std::fs;
// use std::error::Error;
use std::collections::HashMap;
use clap::{Parser, Subcommand};
// use serde::{Deserialize, Serialize};
use log::{ LevelFilter, trace, debug, info, warn, error };
use env_logger::Builder;

/// CFTool - CloudFormation made easy
#[derive(Parser, Debug)]
#[command(name = "CFTool")]
#[command(author = "Christopher Mackinga <christopher.mackinga@gmail.com>")]
#[command(version = "2.0.0-alpha")]
struct Cli {
    /// TODO implement manual config path
    #[arg(short, long, value_name = "FILE")]
    config: Option<PathBuf>,
    ///TODO implement dry_run
    #[arg(short, long, default_value_t = false)]
    dry_run: bool,
    #[arg(long)]
    /// Enable verbose debug logging
    debug: bool,
    #[command(subcommand)]
    command: BaseCommands,
}

#[derive(Subcommand, Debug)]
enum BaseCommands {
    /// List all available deployments for usage with other commands
    List,
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
        deployment: String,
    },
    /// Update an existing Stack in CloudFormation using the local Template and Configuration
    Update {
        deployment: String,
        
    },
    /// Delete the existing Stack in CloudFormation which is associated with this deployment
    Delete {
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
    debug!("cli.dry_run: {}", cli.dry_run);
    debug!("cli.debug: {}", cli.debug);

    let log_level = if cli.debug {LevelFilter::max()} else {LevelFilter::Error};
    Builder::new().filter_level(log_level).init();

    // TODO implement loading non default config file 
    let result = config::get_configurations();

    if result.is_err() {
        error!("Failed to load config file: {}", result.unwrap_err());
        std::process::abort();
    }

    let configs = result.unwrap();
    
    match cli.command {
        BaseCommands::List => { list_deployments(configs); }
        BaseCommands::Describe { deployment } => { describe_deployment(configs, deployment); }
        BaseCommands::Diff { deployment } => { diff_deployment(configs, deployment); }
        BaseCommands::Create { deployment } => { create_deployment(configs, deployment); }
        BaseCommands::Update { deployment } => { update_deployment(configs, deployment); }
        BaseCommands::Delete { deployment } => { delete_deployment(configs, deployment); }
        BaseCommands::Compare { compare_command } => {
            match compare_command {
                CompareCommands::Templates { deployment1, deployment2 } => { compare_template(configs, deployment1, deployment2); }  
                CompareCommands::Configs { deployment1, deployment2 } => { compare_config(configs, deployment1, deployment2); }  
                CompareCommands::Stacks { deployment1, deployment2 } => { compare_stack(configs, deployment1, deployment2); }  
            }
        } 
    };

}

fn diff_deployment(configs: HashMap<String, config::Configuration>, deployment: String) -> () {
    todo!()
}

fn describe_deployment(configs: HashMap<String, config::Configuration>, deployment: String) -> () {
    let configuration = configs.get(&deployment);
    match configuration {
        Some(config) => println!("{}", config),
        None => todo!(),
    };
}

fn list_deployments(configs: HashMap<String, config::Configuration>) -> () {
    let mut deployment_names: Vec<&String> = configs.keys().collect();
    deployment_names.sort(); 
    deployment_names.iter().for_each(|s| println!("{}", s));
}

// fn list_remote() -> () {
//     let result = aws::describe_stacks();
//     match result {
//         Ok(stacks) => {
//             let stack_names: Vec<String> = stacks.iter().map(|s| s.stack_name.clone()).collect();
//             dbg!(stack_names);
//         }
//         Err(error) => {
//             error!("Encountered error in list_remote(): {}", error.to_string());
//         }
//     };
// }

fn compare_stack(configs: HashMap<String, config::Configuration>, deployment1: String, deployment2: String) -> () {
    let result1 = aws::get_stack_template(deployment1);
    let result2 = aws::get_stack_template(deployment2);
    if let (Ok(stack1), Ok(stack2)) = (&result1, &result2) {
        helpers::print_diff(&stack1, &stack2);
    } else {
        if let Err(error1) = result1 {
            error!("Encountered error in compare_remote(): {}", error1.to_string());
        }
        if let Err(error2) = result2 {
            error!("Encountered error in compare_remote(): {}", error2.to_string());
        }
    }
}

fn compare_template(configs: HashMap<String, config::Configuration>, deployment1: String, deployment2: String) -> () {
    let result1 = fs::read_to_string(deployment1);
    let result2 = fs::read_to_string(deployment2);
    if let (Ok(stack1), Ok(stack2)) = (&result1, &result2) {
        helpers::print_diff(&stack1, &stack2);
    } else {
        if let Err(error1) = result1 {
            error!("Encountered error in compare_local(): {}", error1.to_string());
        }
        if let Err(error2) = result2 {
            error!("Encountered error in compare_local(): {}", error2.to_string());
        }
    }
}

fn compare_config(configs: HashMap<String, config::Configuration>, deployment1: String, deployment2: String) -> () {
    // let result1 = fs::read_to_string(deployment_name);
    // let result2 = aws::get_stack_template(String::from("MOSTinstance-app3"));
    // if let (Ok(stack1), Ok(stack2)) = (&result1, &result2) {
    //     helpers::print_diff(&stack1, &stack2);
    // } else {
    //     if let Err(error1) = result1 {
    //         error!("Encountered error in compare_deployment(): {}", error1.to_string());
    //     }
    //     if let Err(error2) = result2 {
    //         error!("Encountered error in compare_deployment(): {}", error2.to_string());
    //     }
    // }
}

fn create_deployment(configs: HashMap<String, config::Configuration>, deployment: String) -> () {
    dbg!(deployment);
    let result = aws::create_stack(Some("ap-southeast-2".to_string()),
                                   Some("test-stack".to_string()),
                                   Some("Some-body".to_string()),
                                   None,
                                   None);
    match result {
        Ok(res) => {
            dbg!(res);
        }
        Err(_) => todo!(),
    }
}

fn update_deployment(configs: HashMap<String, config::Configuration>, deployment: String) -> () {
    dbg!(deployment);
}

fn delete_deployment(configs: HashMap<String, config::Configuration>, deployment: String) -> () {
    dbg!(deployment);
}
