use clap::Parser;
use std::{env::set_current_dir, fs, process::exit};
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
            let config = bake::Config::from_str_src(&config)?;
            println!("{:#?}", config);
            unimplemented!()
        }
        SubCommand::Clean(_) => {
            unimplemented!()
        }
        SubCommand::Graph(_) => {
            unimplemented!()
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
