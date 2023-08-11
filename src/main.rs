use loadgen_dsl::compile::Compiler;

fn main() {
    let mut args = std::env::args();
    let file = args.nth(1).expect("Missing file path");
    let source = std::fs::read_to_string(file).unwrap();
    let config = Compiler::new().compile(&source).unwrap();
    let config = serde_yaml::to_string(&config).unwrap();
    println!("{}", config);
}
