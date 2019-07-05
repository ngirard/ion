use super::Status;
use crate as ion_shell;
use crate::{
    shell::{Shell, ValueRef},
    types,
};
use builtins_proc::builtin;

use std::{borrow::Cow, env};

#[builtin(
    names = "which, type",
    desc = "locate a program file in the current user's path",
    man = "
SYNOPSIS
    which PROGRAM

DESCRIPTION
    The which utility takes a list of command names and searches for the
    alias/builtin/function/executable that would be executed if you ran that command."
)]
pub fn which(args: &[types::Str], shell: &mut Shell<'_>) -> Status {
    if args.len() == 1 {
        return Status::bad_argument("which: Expected at least 1 args, got only 0");
    }

    let mut result = Status::SUCCESS;
    for command in &args[1..] {
        match get_command_info(command, shell) {
            Ok(c_type) => match c_type.as_ref() {
                "alias" => {
                    if let Some(ValueRef::Alias(alias)) = shell.variables().get(&**command) {
                        println!("{}: alias to {}", command, alias.0);
                    }
                }
                "function" => println!("{}: function", command),
                "builtin" => println!("{}: built-in shell command", command),
                path => println!("{}", path),
            },
            Err(_) => result = Status::from_exit_code(1),
        }
    }
    result
}

fn get_command_info<'a>(command: &str, shell: &mut Shell<'_>) -> Result<Cow<'a, str>, ()> {
    match shell.variables().get(command) {
        Some(ValueRef::Alias(_)) => Ok("alias".into()),
        Some(ValueRef::Function(_)) => Ok("function".into()),
        _ if shell.builtins().contains(command) => Ok("builtin".into()),
        _ => {
            let paths = env::var_os("PATH").unwrap_or_else(|| "/bin".into());
            for path in env::split_paths(&paths) {
                let executable = path.join(command);
                if executable.is_file() {
                    return Ok(executable.display().to_string().into());
                }
            }
            Err(())
        }
    }
}
