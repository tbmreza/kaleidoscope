use clap::Parser;

/// # Example
///
/// ```bash
/// kaleidoscope --dl --dp --dc
/// kaleidoscope -lpc
/// ```
#[derive(Parser, Debug)]
pub struct Args {
    #[clap(long, alias = "input", default_value = "")]
    pub program_path: String,

    // Display lexer output.
    #[clap(short, alias = "dl")]
    pub lexer: bool,

    // Display parser output.
    #[clap(short, alias = "dp")]
    pub parser: bool,

    // Display compiler output.
    #[clap(short, alias = "dc")]
    pub compiler: bool,

    // Display tracing logs.
    #[clap(short)]
    pub verbose: bool,
}

pub fn opts() -> Args {
    Args::parse()
}
