build:
	nix-shell --pure --run "cargo build"

run:
	nix-shell --pure --run "cargo run"

check:
	nix-shell --pure --run "cargo check"

dev:
	nix-shell --pure --run "cargo-watch --clear"
