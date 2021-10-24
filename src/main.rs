use clap::Parser;
use std::{collections::HashMap, fs, io, process::exit};
use termion::color;
use yaml_rust::Yaml;

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

#[derive(Debug)]
struct Exp {}

#[derive(Debug)]
enum CommandElem {
    Text(String),
    Exp(Exp),
}

type Command = Vec<CommandElem>;

#[derive(Debug)]
enum Expand {
    Array(Exp),
    Text(Exp),
}

#[derive(Debug)]
struct Task {
    deps: HashMap<String, Expand>,
    command: Command,
}

#[derive(Debug)]
struct Config {
    tasks: Vec<Task>,
}

enum Error {
    ConfigLoadError(io::Error),
    ConfigScanError(yaml_rust::ScanError),
}

fn parse(_: &Yaml) -> Result<Config, Error> {
    unimplemented!()
}

fn run() -> Result<(), Error> {
    let opts = Opts::parse();
    let config = fs::read_to_string("builder.yml").map_err(|e| Error::ConfigLoadError(e))?;
    let config =
        yaml_rust::YamlLoader::load_from_str(&config).map_err(|e| Error::ConfigScanError(e))?;
    if let &[config] = &config.as_slice() {
        let config = parse(&config)?;
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
    Ok(())
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
        Err(Error::ConfigLoadError(e)) => {
            print_error(&format!("Cannot load config file due to {:?}", e))
        }
        Err(Error::ConfigScanError(e)) => {
            print_error(&format!("Cannot parse config file due to {:?}", e))
        }
    }
}
