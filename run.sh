#!/bin/bash

(
	scrambled=`cargo run --bin scrambler`
	printf "$scrambled\n"
	echo "==="
	cargo run --release --bin solver <<< "$scrambled" | stdbuf -oL tr '\ ' '\n' | stdbuf -oL awk 'NF'
) | tee >(cargo run --bin animate)

