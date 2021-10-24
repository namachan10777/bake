use clap::Parser;
use std::{fs, process::exit};
use termion::color;

#[derive(Parser)]
struct Opts {
    #[clap(subcommand)]
    subcmd: SubCommand,
}

#[derive(Parser)]
enum SubCommand {
    Build,
    Clean,
    Graph,
}

fn run() -> Result<(), bake::Error> {
    let opts = Opts::parse();
    let config = fs::read_to_string("builder.yml").map_err(|e| bake::Error::ConfigLoadError(e))?;
    let config = bake::parser::parse(&config)?;
    match opts.subcmd {
        SubCommand::Build => {
            println!("{:?}", config);
            unimplemented!()
        }
        SubCommand::Clean => {
            println!("{:?}", config);
            unimplemented!()
        }
        SubCommand::Graph => {
            println!("{:?}", config);
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
    }
}
