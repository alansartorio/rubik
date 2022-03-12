#!/bin/bash

(
	scrambled=`cargo run --bin scrambler`
	printf "$scrambled\n"
	echo "==="
	cargo run --bin solver <<< "$scrambled"
) | tee >(stdbuf -oL tr '\ ' '\n' | awk 'NF' | cargo run --bin animate)

