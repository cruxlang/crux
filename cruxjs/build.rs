use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};

fn main() {
    let manifest = PathBuf::from(std::env::var_os("CARGO_MANIFEST_DIR").unwrap());
    let library = manifest.join("../lib");
    let mut modules = Vec::new();
    collect_modules(&library, &library, &mut modules);
    modules.sort_by(|left, right| left.0.cmp(&right.0));

    let output = PathBuf::from(std::env::var_os("OUT_DIR").unwrap()).join("standard_library.rs");
    let mut file = fs::File::create(output).unwrap();
    writeln!(file, "const STANDARD_LIBRARY: &[(&str, &str)] = &[").unwrap();
    for (name, path) in modules {
        println!("cargo:rerun-if-changed={}", path.display());
        writeln!(
            file,
            "    ({name:?}, include_str!({:?})),",
            path.to_string_lossy()
        )
        .unwrap();
    }
    writeln!(file, "];").unwrap();
}

fn collect_modules(root: &Path, directory: &Path, modules: &mut Vec<(String, PathBuf)>) {
    for entry in fs::read_dir(directory).unwrap() {
        let path = entry.unwrap().path();
        if path.is_dir() {
            collect_modules(root, &path, modules);
        } else if path.extension().is_some_and(|extension| extension == "cx") {
            let name = path
                .strip_prefix(root)
                .unwrap()
                .to_string_lossy()
                .replace(std::path::MAIN_SEPARATOR, "/");
            modules.push((name, path));
        }
    }
}
