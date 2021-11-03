use clap::{IntoApp, Parser};
use std::{env::set_current_dir, fs, io, process::exit};
use termion::color;

#[derive(Parser)]
struct Opts {
    #[clap(subcommand)]
    subcmd: SubCommand,
}

#[derive(Parser)]
enum SubCommand {
    Build(SubCommandOpts),
    Clean(SubCommandOpts),
    Graph(SubCommandOpts),
    Completion(CompletionOpts),
}

#[derive(Parser)]
struct CompletionOpts {
    #[clap(
        short,
        long,
        possible_value = "fish",
        possible_value = "zsh",
        possible_value = "bash",
        possible_value = "elvish",
        possible_value = "powershell"
    )]
    shell: String,
}

#[derive(Parser)]
struct SubCommandOpts {
    #[clap(short = 'C', default_value = ".")]
    cwd: String,
}

fn run() -> Result<(), bake::Error> {
    let opts = Opts::parse();
    match opts.subcmd {
        SubCommand::Build(opts) => {
            set_current_dir(&opts.cwd).map_err(|_| {
                bake::Error::RuntimeError(format!(
                    "Cannot set current working directory to {}",
                    opts.cwd
                ))
            })?;
            let config = fs::read_to_string("bake.yml").map_err(bake::Error::ConfigLoadError)?;
            let config = bake::parser::config_from_str_src(&config)?;
            let expanded = bake::engine::eval(&mut bake::engine::Env::new_with_std(), &config);
            println!("{:#?}", expanded);
            unimplemented!()
        }
        SubCommand::Clean(_) => {
            unimplemented!()
        }
        SubCommand::Graph(_) => {
            unimplemented!()
        }
        SubCommand::Completion(opts) => {
            let shell = match opts.shell.as_str() {
                "fish" => clap_generate::Shell::Fish,
                "zsh" => clap_generate::Shell::Zsh,
                "bash" => clap_generate::Shell::Bash,
                "elvish" => clap_generate::Shell::Elvish,
                "Powershell" => clap_generate::Shell::PowerShell,
                _ => unreachable!(),
            };
            clap_generate::generate(shell, &mut Opts::into_app(), "bake", &mut io::stdout());
            Ok(())
        }
    }
}

fn print_error(msg: &str) {
    println!(
        "{}[Error]{} {}",
        color::Fg(color::Red),
        color::Fg(color::Reset),
        msg
    );
    exit(-1);
}

fn main() {
    match run() {
        Ok(()) => (),
        Err(bake::Error::ConfigLoadError(e)) => {
            print_error(&format!("Cannot load config file due to {:?}", e))
        }
        Err(bake::Error::ConfigScanError(e)) => {
            print_error(&format!("Cannot parse config file due to {:?}", e))
        }
        Err(bake::Error::SyntaxError(pest::error::LineColLocation::Span(start, end))) => {
            // FIXME: correct position
            print_error(&format!(
                "Syntax error on {}:{} - {}:{}",
                start.0, start.1, end.0, end.1
            ))
        }
        Err(bake::Error::ConfigError(msg)) => {
            print_error(&msg);
        }
        Err(bake::Error::RuntimeError(msg)) => {
            print_error(&msg);
        }
        Err(bake::Error::SyntaxError(pest::error::LineColLocation::Pos((line, col)))) => {
            // FIXME: correct position
            print_error(&format!("Syntax error on {}:{}", line, col))
        }
    }
}
