#!/bin/bash

nasm -f elf64 -g -F dwarf -w+all -w+error -o diakrytynizator.o diakrytynizator.asm &&
ld --fatal-warnings -o diakrytynizator diakrytynizator.o
