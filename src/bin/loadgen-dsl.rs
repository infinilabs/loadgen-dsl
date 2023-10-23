use miette::MietteHandlerOpts;
use std::env;

fn main() -> miette::Result<()> {
    miette::set_hook(Box::new(|_| {
        Box::new(MietteHandlerOpts::new().color(false).unicode(true).build())
    }))
    .unwrap();

    let path = env::args().nth(1).expect("missing input path");
    let input = std::fs::read_to_string(&path).expect("io error");
    let output = loadgen_dsl_compiler::compile(&input)
        .map_err(|e| miette::Error::new(e).with_source_code(input.to_owned()))?;
    println!("{}", serde_yaml::to_string(&output).unwrap());
    Ok(())
}