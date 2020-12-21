#!/bin/bash

day=$1

. .cookie

filename="input${day}.txt"

if [ -f "$filename" ]; then
	echo "$filename exists"
else
	curl -o "$filename" -b "$AOC_COOKIE" https://adventofcode.com/2020/day/${day}/input
fi
