use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::atomic::{AtomicUsize, Ordering};

static NEXT_TEMP_DIR: AtomicUsize = AtomicUsize::new(0);

struct TempHome(PathBuf);

impl TempHome {
    fn new() -> Self {
        let path = std::env::temp_dir().join(format!(
            "crux-cli-test-{}-{}",
            std::process::id(),
            NEXT_TEMP_DIR.fetch_add(1, Ordering::Relaxed)
        ));
        fs::create_dir(&path).unwrap();
        Self(path)
    }

    fn path(&self) -> &Path {
        &self.0
    }
}

impl Drop for TempHome {
    fn drop(&mut self) {
        fs::remove_dir_all(&self.0).unwrap();
    }
}

fn crux() -> Command {
    Command::new(env!("CARGO_BIN_EXE_crux"))
}

#[test]
fn compile_and_c_alias_emit_the_same_executable_javascript() {
    let temp = TempHome::new();
    let source = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/integration/arithmetic/main.cx");
    let compile = crux().arg("compile").arg(&source).output().unwrap();
    let alias = crux().arg("c").arg(&source).output().unwrap();
    assert!(
        compile.status.success(),
        "{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    assert!(
        alias.status.success(),
        "{}",
        String::from_utf8_lossy(&alias.stderr)
    );
    assert_eq!(compile.stdout, alias.stdout);
    assert!(String::from_utf8_lossy(&compile.stdout).contains("$module_main.main();"));

    let output_path = temp.path().join("main.js");
    let to_file = crux()
        .args(["c", "-o"])
        .arg(&output_path)
        .arg(&source)
        .output()
        .unwrap();
    assert!(
        to_file.status.success(),
        "{}",
        String::from_utf8_lossy(&to_file.stderr)
    );
    assert!(to_file.stdout.is_empty());
    assert_eq!(fs::read(output_path).unwrap(), compile.stdout);

    let local_source = temp.path().join("main.cx");
    let original = fs::read(&source).unwrap();
    fs::write(&local_source, &original).unwrap();
    let same_path = crux()
        .args(["compile", "-o"])
        .arg(&local_source)
        .arg(&local_source)
        .output()
        .unwrap();
    assert!(!same_path.status.success());
    assert_eq!(fs::read(local_source).unwrap(), original);
}

#[test]
fn install_emacs_uses_xdg_directory_and_is_idempotent() {
    let home = TempHome::new();
    let config = home.path().join("config");
    let emacs_dir = config.join("emacs");
    fs::create_dir_all(&emacs_dir).unwrap();

    for _ in 0..2 {
        let output = crux()
            .args(["editor", "install-emacs"])
            .env("HOME", home.path())
            .env("XDG_CONFIG_HOME", &config)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "{}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    let mode = fs::read_to_string(emacs_dir.join("lisp/crux-mode.el")).unwrap();
    assert_eq!(mode, include_str!("../../tools/emacs/crux-mode.el"));
    let init = fs::read_to_string(emacs_dir.join("init.el")).unwrap();
    assert_eq!(
        init.matches("installed by crux editor install-emacs")
            .count(),
        1
    );
    assert!(init.contains("(require 'crux-mode)"));
}

#[test]
fn install_emacs_respects_an_existing_legacy_init_file() {
    let home = TempHome::new();
    fs::create_dir_all(home.path().join("config/emacs")).unwrap();
    fs::write(
        home.path().join(".emacs"),
        b"(setq inhibit-startup-screen t)",
    )
    .unwrap();

    let output = crux()
        .args(["editor", "install-emacs"])
        .env("HOME", home.path())
        .env("XDG_CONFIG_HOME", home.path().join("config"))
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(home.path().join(".emacs.d/lisp/crux-mode.el").is_file());
    let init = fs::read_to_string(home.path().join(".emacs")).unwrap();
    assert!(init.starts_with("(setq inhibit-startup-screen t)\n"));
    assert!(init.contains("(require 'crux-mode)"));
    assert!(!home.path().join("config/emacs/init.el").exists());
}
