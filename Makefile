cc=x86_64-elf-gcc
ld=x86_64-elf-ld
cflag= -c -o kernel.o -mno-red-zone -fno-stack-protector -fomit-frame-pointer
#ldflag= -T ../src/k.ld -o kernel.bin kernel.o 
src=../src/kernel.c 
#nasm ../src/k.asm -o kernel.bin 

#dd if=/dev/zero of=disk.img count=128 bs=512
#dd if=/dev/zero of=disk.img count=128 bs=1048576
#cat pure64.sys kernel.bin > software.sys

#dd if=mbr.sys of=disk.img conv=notrunc
#dd if=software.sys of=disk.img bs=512 seek=16 conv=notrunc

