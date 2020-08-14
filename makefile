.PHONY = build-shell shell build run check dev

build:
	cargo build

run:
	cargo run

check:
	cargo check

dev:
	cargo-watch --clear
