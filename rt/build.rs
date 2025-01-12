use std::{collections::HashMap, env};

use cbindgen::{Config, ExportConfig, ItemType};

const PRELUDE: &str = r#"
typedef struct runtime_t runtime_t;
typedef struct value_t {
    uint64_t _0;
    uint64_t _1;
} value_t;
 typedef void *fn_ptr_t;
"#;

fn main() {
    let crate_dir = env::var("CARGO_MANIFEST_DIR").unwrap();

    let rename = {
        let mut r = HashMap::new();
        r.insert("Value".to_string(), "value_t".to_string());
        r.insert("Runtime".to_string(), "runtime_t".to_string());
        r.insert("FnPtr".to_string(), "fn_ptr_t".to_string());
        r
    };

    cbindgen::Builder::new()
        .with_crate(crate_dir)
        .with_config(Config {
            language: cbindgen::Language::C,
            after_includes: Some(PRELUDE.to_string()),
            include_guard: Some("AL_RUNTIME_H".to_string()),
            style: cbindgen::Style::Type,
            export: ExportConfig {
                rename,
                item_types: vec![ItemType::Functions],
                ..Default::default()
            },
            ..Default::default()
        })
        .generate()
        .expect("Unable to generate bindings")
        .write_to_file("include/aoclang/runtime.h");
}
