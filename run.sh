#!/bin/bash

(
	scrambled=`cargo run --bin scrambler`
	printf "$scrambled\n"
	echo "==="
	cargo run --bin solver <<< "$scrambled"
) | tee >(cargo run --bin animate)

