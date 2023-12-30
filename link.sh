#!/bin/sh

opam exec -- apelink \
  -o bob.com \
  -l $(opam var bin)/ape-x86_64.elf \
  -l $(opam var bin)/ape-aarch64.elf \
  -M $(opam var bin)/ape-m1.c \
  _build/default.x86_64_esperanto/bin/bob.exe.dbg \
  _build/default.aarch64_esperanto/bin/bob.exe.dbg
