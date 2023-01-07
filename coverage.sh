#!/usr/bin/env sh

cargo tarpaulin \
	-o Html \
	-o Xml \
	--all-features \
	--output-dir target/tarpaulin \
	--rustflags="-C opt-level=0"
