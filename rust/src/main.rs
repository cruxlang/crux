use clap::{Parser, Subcommand};
use std::env;
use std::error::Error;
use std::fs::{self, OpenOptions};
use std::io::{self, Write};
use std::path::{Path, PathBuf};

const EMACS_MODE: &str = include_str!("../../tools/emacs/crux-mode.el");
const EMACS_INIT_SNIPPET: &str = "\
;; Crux mode (installed by crux editor install-emacs)\n\
(add-to-list 'load-path (expand-file-name \"lisp\" user-emacs-directory))\n\
(require 'crux-mode)\n";
const EMACS_INIT_MARKER: &str = ";; Crux mode (installed by crux editor install-emacs)";

#[derive(Parser)]
#[command(version, about = "The Crux compiler")]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Compile a Crux program to runnable JavaScript.
    #[command(visible_alias = "c")]
    Compile {
        /// Entry point source file.
        source: PathBuf,
        /// Write JavaScript to a file instead of stdout.
        #[arg(short, long)]
        output: Option<PathBuf>,
    },
    /// Install editor support.
    Editor {
        #[command(subcommand)]
        command: EditorCommand,
    },
}

#[derive(Subcommand)]
enum EditorCommand {
    /// Install crux-mode into your local Emacs configuration.
    InstallEmacs,
}

fn main() {
    if let Err(error) = run(Cli::parse()) {
        eprintln!("crux: {error}");
        std::process::exit(1);
    }
}

fn run(cli: Cli) -> Result<(), Box<dyn Error>> {
    match cli.command {
        Command::Compile { source, output } => {
            let javascript = crux::compiler::compile_path_executable(&source)?;
            if let Some(path) = output {
                if path.exists() && path.canonicalize()? == source.canonicalize()? {
                    return Err("output path must differ from the Crux source file".into());
                }
                fs::write(&path, javascript)?;
            } else {
                io::stdout().write_all(javascript.as_bytes())?;
            }
        }
        Command::Editor {
            command: EditorCommand::InstallEmacs,
        } => {
            let home = env::var_os("HOME")
                .map(PathBuf::from)
                .ok_or("HOME is not set; cannot locate the Emacs configuration")?;
            let xdg_config_home = env::var_os("XDG_CONFIG_HOME").map(PathBuf::from);
            let (mode_path, init_path) = install_emacs(&home, xdg_config_home.as_deref())?;
            println!("Installed {}", mode_path.display());
            println!("Configured {}", init_path.display());
        }
    }
    Ok(())
}

fn install_emacs(home: &Path, xdg_config_home: Option<&Path>) -> io::Result<(PathBuf, PathBuf)> {
    let legacy_dir = home.join(".emacs.d");
    let legacy_el = home.join(".emacs.el");
    let legacy_file = home.join(".emacs");
    let xdg_dir = xdg_config_home
        .map(Path::to_path_buf)
        .unwrap_or_else(|| home.join(".config"))
        .join("emacs");

    let user_dir = if legacy_el.exists() || legacy_file.exists() || legacy_dir.exists() {
        legacy_dir
    } else if xdg_dir.exists() {
        xdg_dir
    } else {
        legacy_dir
    };
    let init_path = if legacy_el.exists() {
        legacy_el
    } else if legacy_file.exists() {
        legacy_file
    } else {
        user_dir.join("init.el")
    };

    let lisp_dir = user_dir.join("lisp");
    fs::create_dir_all(&lisp_dir)?;
    let mode_path = lisp_dir.join("crux-mode.el");
    if fs::read(&mode_path).ok().as_deref() != Some(EMACS_MODE.as_bytes()) {
        fs::write(&mode_path, EMACS_MODE)?;
    }

    let init = match fs::read(&init_path) {
        Ok(contents) => contents,
        Err(error) if error.kind() == io::ErrorKind::NotFound => Vec::new(),
        Err(error) => return Err(error),
    };
    if !init
        .windows(EMACS_INIT_MARKER.len())
        .any(|window| window == EMACS_INIT_MARKER.as_bytes())
    {
        let mut file = OpenOptions::new()
            .create(true)
            .append(true)
            .open(&init_path)?;
        if !init.is_empty() && !init.ends_with(b"\n") {
            file.write_all(b"\n")?;
        }
        file.write_all(EMACS_INIT_SNIPPET.as_bytes())?;
    }

    Ok((mode_path, init_path))
}
