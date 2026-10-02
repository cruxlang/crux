use std::collections::BTreeMap;
use wasm_bindgen::prelude::*;

include!(concat!(env!("OUT_DIR"), "/standard_library.rs"));

/// Compile one Crux program, including the embedded standard library and RTS.
/// The returned object has either a `result` or `error` string property.
#[wasm_bindgen(js_name = compileCrux)]
pub fn compile_crux(source: &str) -> JsValue {
    let output = js_sys::Object::new();
    let (property, value) = match compile_source(source) {
        Ok(result) => ("result", result),
        Err(error) => ("error", error),
    };
    js_sys::Reflect::set(
        &output,
        &JsValue::from_str(property),
        &JsValue::from_str(&value),
    )
    .expect("setting a property on a fresh JavaScript object cannot fail");
    output.into()
}

/// Idiomatic Wasm binding: returns JavaScript or throws a string on failure.
#[wasm_bindgen(js_name = compile)]
pub fn compile_wasm(source: &str) -> Result<String, JsValue> {
    compile_source(source).map_err(|error| JsValue::from_str(&error))
}

fn compile_source(source: &str) -> Result<String, String> {
    let mut sources = STANDARD_LIBRARY
        .iter()
        .map(|(path, source)| ((*path).to_owned(), (*source).to_owned()))
        .collect::<BTreeMap<_, _>>();
    sources.insert("main.cx".into(), source.into());
    crux::compiler::compile_in_memory_executable("main.cx", &sources)
        .map_err(|error| error.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn compiles_with_the_embedded_standard_library() {
        let javascript = compile_source(
            "import option(...)\nfun main() {\n  match Some(1) {\n    Some(n) => print(n)\n  }\n}",
        )
        .unwrap();
        assert!(javascript.contains("$module_main.main();"));
        assert!(javascript.contains("function Some"));

        #[cfg(not(target_arch = "wasm32"))]
        {
            let output = std::process::Command::new("node")
                .arg("-e")
                .arg(javascript)
                .output()
                .unwrap();
            assert!(
                output.status.success(),
                "{}",
                String::from_utf8_lossy(&output.stderr)
            );
            assert_eq!(output.stdout, b"1\n");
        }
    }

    #[test]
    fn returns_rendered_compiler_errors() {
        let error = compile_source("fun main() { missing }").unwrap_err();
        assert!(error.contains("unbound value missing"), "{error}");
    }
}
