set positional-arguments

# Regression test runs main binary with all flags set
regtest:
	cargo r -- --input programs/draft.kal -lpc

# Run a .kal program. Usage: just run programs/filename.kal [FLAGS]
@run *program='':
	cargo r -- --input "$@"
