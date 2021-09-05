#! /bin/sh

asmTemp="$(mktemp).s"
objTemp="$(mktemp).o"

cleanUp () {
	rm -f "$asmTemp"
	rm -f "$objTemp"
}

if ! cabal run -v0 < "$1.lp" > "$asmTemp" ; then
	cleanUp
	exit 1
fi

nasm -f elf64 -o "$objTemp" "$asmTemp"

ld -e start -o "$1" "$objTemp"

cleanUp
