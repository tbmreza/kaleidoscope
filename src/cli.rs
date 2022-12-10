use clap::Parser;

/// # Example
///
/// ```bash
/// kaleidoscope --dl --dp --dc
/// kaleidoscope -lpc
/// ```
#[derive(Parser, Debug)]
pub struct Args {
    // Display lexer output.
    #[clap(short, alias = "dl")]
    pub lexer: bool,

    // Display parser output.
    #[clap(short, alias = "dp")]
    pub parser: bool,

    // Display compiler output.
    #[clap(short, alias = "dc")]
    pub compiler: bool,
}

pub fn opts() -> Args {
    Args::parse()
}
