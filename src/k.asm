BITS 64
ORG 0x00100000
    mov edi, 0xa0000
    mov rax, 0xff00ff00
    mov [edi],rax
    ;jmp $
