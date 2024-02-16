#!/bin/bash

mkdir -p bin

cd src

nasm pure64.asm -o ../bin/pure64.sys -l ../bin/pure64-debug.txt

x86_64-elf-gcc -c kernel.c -o kernel.o -mno-red-zone -fno-stack-protector -fomit-frame-pointer 

x86_64-elf-ld -T k.ld -o kernel.bin kernel.o 

cd boot

nasm mbr.asm -o ../../bin/mbr.sys
nasm pxestart.asm -o ../../bin/pxestart.sys
nasm multiboot.asm -o ../../bin/multiboot.sys
nasm multiboot2.asm -o ../../bin/multiboot2.sys
nasm uefi.asm -o ../../bin/uefi.sys

cd ../..
