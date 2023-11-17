mod helpers;
mod config;
mod aws;


use std::path::PathBuf;
use std::fs;
// use std::error::Error;
use clap::{Parser, Subcommand};
// use serde::{Deserialize, Serialize};
use log::{ LevelFilter, trace, debug, info, warn, error };
use env_logger::Builder;

/// AWS-CLI CloudFormation made easy
#[derive(Parser, Debug)]
#[command(name = "CFTool")]
#[command(author = "Christopher Mackinga <christopher.mackinga@gmail.com>")]
#[command(version = "2.0.0-alpha")]
struct Cli {
    #[arg(short, long, value_name = "FILE")]
    config: Option<PathBuf>,
    #[arg(short, long, default_value_t = false)]
    dry_run: bool,
    #[arg(long)]
    debug: bool,
    #[command(subcommand)]
    command: BaseCommands,
}

#[derive(Subcommand, Debug)]
enum BaseCommands {
    /// List stacks currently in AWS
    List {
        #[command(subcommand)]
        list_command: ListCommands,
    },
    /// Compare your local configuration against AWS
    Compare {
        /// The stack to compare
        #[command(subcommand)]
        compare_command: CompareCommands,
    },
    Create {
        /// The stacks to create
        stacknames: Vec<String>,
    },
    Update {
        /// The stacks to update
        stacknames: Vec<String>,
        
    },
    Delete {
        /// The stacks to delete
        stacknames: Vec<String>,
    },
}

#[derive(Subcommand, Debug)]
enum ListCommands {
    Deployment,
    Local,
    Remote,
}

#[derive(Subcommand, Debug)]
enum CompareCommands {
    /// <stack-name>:<instance-name> referring to a deployed instance of a stack
    Deployment {
        deployment_name: String,
    },
    Local {
        stackname1: String,
        stackname2: String,
    },
    Remote {
        stackname1: String,
        stackname2: String,
    },
}

fn main() {
    let cli = Cli::parse();
    debug!("cli.dry_run: {}", cli.dry_run);
    debug!("cli.debug: {}", cli.debug);

    let log_level = if cli.debug {LevelFilter::max()} else {LevelFilter::Error};
    Builder::new().filter_level(log_level).init();

    match cli.command {
        BaseCommands::List { list_command } => {
            match list_command {
                ListCommands::Deployment => { list_deployment(); }
                ListCommands::Local => { list_local(); }
                ListCommands::Remote => { list_remote(); }
            }
        }
        BaseCommands::Compare { compare_command } => {
            match compare_command {
                CompareCommands::Local { stackname1, stackname2 } => { compare_local(stackname1, stackname2); }  
                CompareCommands::Remote { stackname1, stackname2 } => { compare_remote(stackname1, stackname2); }  
                CompareCommands::Deployment { deployment_name } => { compare_deployment(deployment_name); }  
            }
        } 
        BaseCommands::Create { stacknames } => { create_stacks(&stacknames); }
        BaseCommands::Update { stacknames } => { update_stacks(&stacknames); }
        BaseCommands::Delete { stacknames } => { delete_stacks(&stacknames); }
    };

}

fn list_deployment() -> () {
    todo!();
}

fn list_local() -> () {
    let result = config::get_configurations();
    match result {
        Ok(deployments) => {
            dbg!(deployments);
        }
        
        Err(error) => {
            error!("Encountered error in list_local(): {}", error.to_string());
        }
    }

}

fn list_remote() -> () {
    let result = aws::describe_stacks();
    match result {
        Ok(stacks) => {
            let stack_names: Vec<String> = stacks.iter().map(|s| s.stack_name.clone()).collect();
            dbg!(stack_names);
        }
        Err(error) => {
            error!("Encountered error in list_remote(): {}", error.to_string());
        }
    };
}

fn compare_remote(stackname1: String, stackname2: String) -> () {
    let result1 = aws::get_stack_template(stackname1);
    let result2 = aws::get_stack_template(stackname2);
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

fn compare_local(stackname1: String, stackname2: String) -> () {
    let result1 = fs::read_to_string(stackname1);
    let result2 = fs::read_to_string(stackname2);
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

fn compare_deployment(deployment_name: String) -> () {
    let result1 = fs::read_to_string(deployment_name);
    let result2 = aws::get_stack_template(String::from("MOSTinstance-app3"));
    if let (Ok(stack1), Ok(stack2)) = (&result1, &result2) {
        helpers::print_diff(&stack1, &stack2);
    } else {
        if let Err(error1) = result1 {
            error!("Encountered error in compare_deployment(): {}", error1.to_string());
        }
        if let Err(error2) = result2 {
            error!("Encountered error in compare_deployment(): {}", error2.to_string());
        }
    }
}

fn create_stacks(stacknames: &Vec<String>) -> () {
    dbg!(stacknames);
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

fn update_stacks(stacknames: &Vec<String>) -> () {
    dbg!(stacknames);
}

fn delete_stacks(stacknames: &Vec<String>) -> () {
    dbg!(stacknames);
}
