use std::fmt;
use std::error::Error;

use console::{style, Style};
use similar::{ChangeTag, TextDiff};
use tabled::Tabled;
use owo_colors::OwoColorize;

// Define a custom error type
#[derive(Debug)]
pub struct TooManyElementsError;

impl fmt::Display for TooManyElementsError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Too many elements in the Vec")
    }
}

impl Error for TooManyElementsError {}

// Function to get the first element or return an error
pub fn get_single<T>(vec: Vec<T>) -> Result<T, Box<dyn Error>> {
    if vec.len() > 1 {
        return Err(Box::new(TooManyElementsError));
    }
    if let Some(element) = vec.into_iter().next() {
        Ok(element)
    } else {
        // Handle the case where the Vec is empty
        Err("The Vec is empty".into())
    }
}

// pub fn print_lines_range(input_string: &str, start_line: usize, end_line: usize) {
//     for (line_number, line) in input_string.lines().enumerate() {
//         if line_number + 1 >= start_line && line_number + 1 <= end_line {
//             println!("Line {}: {}", line_number + 1, line);
//         }
//     }
// }

struct Line(Option<usize>);

impl fmt::Display for Line {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0 {
            None => write!(f, "    "),
            Some(idx) => write!(f, "{:<4}", idx + 1),
        }
    }
}

pub fn print_diff(string1: &String, string2: &String) -> () {
    let diff = TextDiff::from_lines(string1, string2);
    for (idx, group) in diff.grouped_ops(3).iter().enumerate() {
        if idx > 0 {
            println!("{:-^1$}", "-", 80);
        }
        for op in group {
            for change in diff.iter_inline_changes(op) {
                let (sign, s) = match change.tag() {
                    ChangeTag::Delete => ("-", Style::new().red()),
                    ChangeTag::Insert => ("+", Style::new().green()),
                    ChangeTag::Equal => (" ", Style::new().dim()),
                };
                print!(
                    " {} {} |{}",
                    style(Line(change.old_index())).dim(),
                    style(Line(change.new_index())).dim(),
                    s.apply_to(sign).bold(),
                );
                for (emphasized, value) in change.iter_strings_lossy() {
                    if emphasized {
                        print!("{}", s.apply_to(value).underlined().on_black());
                    } else {
                        print!("{}", s.apply_to(value));
                    }
                }
                if change.missing_newline() {
                    println!();
                }
            }
        }
    }
}

#[derive(Tabled)]
pub struct DeploymentDiff {
    #[tabled(rename = "DEPLOYMENT")]
    pub deployment_name: String,
    #[tabled(skip)]
    pub local_config: String,
    #[tabled(rename = "CONFIG")]
    #[tabled(display_with("Self::config_state", self))]
    pub remote_config: String,
    #[tabled(skip)]
    pub local_template: String,
    #[tabled(rename = "TEMPLATE")]
    #[tabled(display_with("Self::template_state", self))]
    pub remote_template: String,
}

impl DeploymentDiff {
    pub fn print_diff(&self) {
        if self.local_config == self.remote_config {
            println!("CONFIG: IDENTICAL");
        } else {
            println!("CONFIG:");
            print_diff(&self.remote_config, &self.local_config);
            println!("\n");
        }

        if self.local_template == self.remote_template {
            println!("TEMPLATE: IDENTICAL");
        } else {
            println!("TEMPLATE:");
            print_diff(&self.remote_template, &self.local_template);
        }
        println!();
    }

    pub fn config_state(&self) -> String {
        if self.local_config == self.remote_config { "IDENTICAL".green().to_string() } else { "DRIFTED".red().to_string() }
    }

    pub fn template_state(&self) -> String {
        if self.local_template == self.remote_template { "IDENTICAL".green().to_string() } else { "DRIFTED".red().to_string() }
    }
}
