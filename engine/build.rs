#[cfg(not(debug_assertions))]
use std::{
    io::{BufRead, BufReader},
    process::{Command, Stdio},
};

#[cfg(not(debug_assertions))]
use build_print::info;

#[cfg(all(not(debug_assertions), windows))]
const NPM_COMMAND: &str = "npm.cmd";

#[cfg(all(not(debug_assertions), not(windows)))]
const NPM_COMMAND: &str = "npm";

fn main() {
    #[cfg(not(debug_assertions))]
    {
        let output = Command::new(NPM_COMMAND)
            .args(["run", "build"])
            .current_dir("../frontend")
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit())
            .spawn()
            .and_then(|mut child| {
                let stdout = child.stdout.take().expect("Failed to capture stdout");
                let reader = BufReader::new(stdout);
                for line in reader.lines() {
                    let line = line?;
                    info!("{}", line.trim());
                }
                child.wait_with_output()
            })
            .expect("Failed to execute command");

        if !output.status.success() {
            panic!("Command executed with failing error code");
        }
    }
}
