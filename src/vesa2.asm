; https://blog.csdn.net/wascm/article/details/704603
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Simulated Linear Frame Buffer
; Christopher Giese <geezer@execpc.com>
; http://www.execpc.com/~geezer/os/
;
; Updated March 6, 2001: can now return to V86 mode when done
;
; Some stuff (actually, pmode VESA BIOS bank switching) fixed by
; Alexei A. Frounze <alexfru@chat.ru>
; http://alexfru.chat.ru
;
; You can do anything with this code but blame us for it.
;
; To run this code, you need a PC with a 32-bit processor (386SX or better)
; and DOS. This code has been tested with the following SVGA video boards:
;
;      bank-switch function
; buss  chipset   used by this code
; ----  -------   --------------------
; 16-bit ISA Cirrus 5422  Cirrus
; 32-bit PCI S3 86c765 (Trio 64V+) S3
; 32-bit PCI (?) STB Nitro (?)  S3
;
; To assemble this code, you need NASM (http://www.web-sites.co.uk/nasm/)
; nasm -f bin -o slfb.com slfb.asm
;
; bugs/to do:
; - test with other systems
;
; Here is an interesting link:
; http://marc.theaimsgroup.com/?m=88102879813311&w=2
; Also, look at the "vflat" files in the source code to the
; SciTech MGL graphics library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

LFB_BASE equ 0C0000000h

%if 1
VESA_MODE equ 101h ; 640x480x256
WIDTH  equ 640
HEIGHT  equ 480
%else
VESA_MODE equ 103h ; 800x600x256
WIDTH  equ 800
HEIGHT  equ 600
%endif

SEQN_INDX       equ     3C4h
SEQN_DATA       equ     3C5h
CRTC_INDX       equ     3D4h
CRTC_DATA       equ     3D5h
GRFX_INDX       equ     3CEh
GRFX_DATA       equ     3CFh

 BITS 16
 ORG 100h

; save CS value for return to real mode
 mov [real_mode_cs],cs

; there is no foolproof way to ID an Intel CPU, but we'll try.
; If we are in V86 mode, the VMM may interfere with our attempts
; to change the IOPL and NT bits
 pushf
  pushf
  pop ax
  xor ah,07h ; try changing b14 (NT) and b13:b12
  push ax  ; (IOPL) of FLAGS
  popf
  pushf
  pop bx
 popf   ; restore old FLAGS value
 cmp ah,bh
 je is_32bit
 mov si,not_32bit_msg
 jmp msg_and_exit

not_32bit_msg:
 db "Sorry; this code requires a 32-bit CPU (386SX or better)."
 db 13, 10, 0

is_32bit:
; check for supported video card. First, look for VESA 2.x BIOS
 call probe_vesa
 je vid_ok

; no VESA 2.x BIOS; try direct hardware probing for Cirrus 5422
 call probe_cirrus
 je vid_ok

; not Cirrus; try probing for S3
 call probe_s3
 je vid_ok
 mov si,sorry_msg
 jmp msg_and_exit

sorry_msg:
 db 13, 10, "Sorry, no supported video card was found.", 13, 10, 0

ok_msg:
 db "DETECTED", 13, 10, 0

vid_ok:
 mov si,ok_msg
 call wrstr

; our pmode code will use the stack DOS gave us, but we have
; to zero the top 16 bits of the stack pointer (ESP)
 xor eax,eax
 mov ax,sp
 mov esp,eax

; get the virtual-to-physical conversion value. Though we turn on paging,
; this code runs in memory that is identity-mapped (i.e. no page-based
; address translation). We do, however, use segment-based address
; translation, to give things the same addresses in both real and pmode.
 xor ebx,ebx
 mov bx,cs
 shl ebx,4

; set base addresses of the 16- and 32-bit code segment descriptors
 mov eax,ebx
 mov [gdt2 + 2],ax
 mov [gdt4 + 2],ax
 shr eax,16
 mov [gdt2 + 4],al
 mov [gdt4 + 4],al
 mov [gdt2 + 7],ah
 ; mov [gdt4 + 7],ah ; no; this one is 16-bit

; now do the same with the data/stack segment descriptors
 mov eax,ebx
 mov [gdt3 + 2],ax
 mov [gdt5 + 2],ax
 shr eax,16
 mov [gdt3 + 4],al
 mov [gdt5 + 4],al
 mov [gdt3 + 7],ah
 ; mov [gdt5 + 7],ah ; no; this one is 16-bit

; point the TSS descriptor to the LINEAR/PHYSICAL address of 'tss'
 mov eax,tss
 add eax,ebx
 mov [gdt6 + 2],ax
 shr eax,16
 mov [gdt6 + 4],al
 mov [gdt6 + 7],ah

; point the interrupt gates in the Interrupt Descriptor Table (IDT)
; to the default interrupt handler 'unhand'
 mov di,idt
 mov cx,(idt_end - idt) / 8 ; number of IDT entries (gates)
do_idt:
 mov eax,unhand  ; point all gates to 'unhand'
 mov [di],ax  ; set bits 15:0 of gate offset
 shr eax,16
 mov [di+6],ax  ; set bits 31:16 of gate offset
 add di,8  ; next gate
 loop do_idt

 mov di,idt_0e  ; IDT entry 0Eh -> page fault
 mov eax,page_fault
 mov [di],ax
 shr eax,16
 mov [di+16],ax

; point 'gdt_ptr' to LINEAR/PHYSICAL address of the GDT;
; 'idt_ptr' to LINEAR/PHYSICAL adr of the IDT
 add [gdt_ptr + 2],ebx
 add [idt_ptr + 2],ebx

; DOS doesn't load .COM or .EXE files on a page (4K) boundary, so
; we must now find a page boundary for the page directory/tables.
 xor esi,esi
 mov si,page_info ; SI = offset of page_info
 add esi,ebx  ; ESI = LINEAR/PHYSICAL adr of page_info
 add esi,4095  ; round to 4K boundary

 and esi,0FFFFF000h ; ESI = LINEAR/PHYSICAL adr of page dir
 mov edi,esi
 sub edi,ebx  ; DI = offset of page dir

; save page dir address for later use, by both VCPI and "raw" code
 mov [vcpi_cr3],esi

; EAX = LINEAR/PHYSICAL address of "framebuffer" page table
; (4K above the page directory)
 mov eax,esi
 add eax,4096
; create entry in page dir for "framebuffer" page table
 or al,7   ; Ring 3, writable, present
 mov [di + (((LFB_BASE >> 22) & 3FFh) << 2)],eax

; EAX = LINEAR/PHYSICAL address of "kernel" page table
; (8K above the page directory)
 mov eax,esi
 add eax,8192
; create entry in page dir for "kernel" page table
 or al,7   ; Ring 3, writable, present
 mov [di + 0],eax

; advance DI from page directory to "framebuffer" page table
 add edi,4096

; save the LINEAR/PHYSICAL pointer to the "framebuffer" page table
 mov eax,edi
 add eax,ebx
 mov [fb_page_table],eax

; Map the simulated linear framebuffer from virtual address LFB_BASE
; to the actual banked framebuffer at physical address 000A0000,
; repeating every 64K (16 pages)
;
; 1600x1200x256 display needs 1875K simulated framebuffer
; == 30 banks of 64K each == 469 pages of 4K each
;
; If the simulated framebuffer crosses a 4 meg boundary, or is larger
; than 4 meg, you will need more than one "framebuffer" page table.

 push di
  add di,(((LFB_BASE >> 12) & 3FFh) << 2)
  mov dl,30
map_fb1:
  mov ecx,16
  mov eax,0A0006h ; Ring 3, writable, NOT PRESENT
map_fb2:
  stosd
  add eax,4096 ; next page
  loop map_fb2
  dec dl
  jne map_fb1
 pop di

; advance DI to "kernel" page table
 add di,4096

; using the "kernel" page table, identity-map
; (virtual == physical) the bottom 4M of RAM
; With VCPI, we can skip this step, and simply take over EMM386's
; conventional memory page table using INT 67h AX=DE01h
 mov cx,1024
 mov eax,7  ; Ring 3, writable, present
identity_map:
 stosd
 add eax,4096  ; next page
 loop identity_map

; move DI back to "kernel" page table, in case we want to change it
 sub di,4096

; check for virtual 8086 mode (Windows DOS box or EMM386 loaded)
 smsw ax
 test al,1  ; look at PE bit of MSW (CR0)
 je real0

 mov si,v86_msg
 call wrstr

; if it's EMM386, we can use VCPI to switch to protected mode
; Testing for VCPI like this makes Win95 pop up a dialog suggesting
; "MS-DOS mode". This may or may not be a good thing.
 mov ax,0DE00h
 push bx   ; save EBX; not interested in VCPI version
  int 67h
 pop bx
 cmp ah,0
 je vcpi0

; no VCPI? probably Windows DOS box; check for that
 mov si,err_msg
 mov ax,1600h
 int 2Fh
 cmp al,0
 je msg_and_exit
 cmp al,80h
 je msg_and_exit
 mov si,win_msg  ; yup, it's 'Doze

msg_and_exit:
 call wrstr

; exit to DOS with errorlevel 1
 mov ax,4C01h
 int 21h

vcpi0:
; get "kernel" page table and partial GDT from the VCPI server.
; If we set up our own identity-mapped page table, this step is optional.
; (we still need to do INT 67h AX=DE01h to get the pmode entry
; point to the VCPI server -- so we can return to V86 mode later)
 mov si,gdt7
 mov ax,0DE01h
 push ebx
  int 67h
  mov [vcpi_entry],ebx
 pop ebx
 mov si,vcpi_err_msg
 cmp ah,0
 jne msg_and_exit

; set up the VCPI control block for the switch to pmode
 add [vcpi_gdtr],ebx
 add [vcpi_idtr],ebx

; await keypress; switch to graphics mode; disable interrupts
 mov si,vcpi_msg
 call setup
; OK, let's do it
 mov esi,vcpi_control_block
 add esi,ebx
 mov ax,0DE0Ch
; if all goes well, the interrupt will return at 'from_vcpi'
 int 67h

real0:
; await keypress; switch to graphics mode; disable interrupts
 mov si,real_msg
 call setup

; set vital registers: page directory base register (PDBR = CR3)
 mov eax,[vcpi_cr3]
 mov cr3,eax
; ...GDTR and IDTR
 lgdt [gdt_ptr]
 lidt [idt_ptr]
; ...enable paging and pmode
 mov eax,cr0
 or eax,80000001h
 mov cr0,eax
 jmp CODE_SEL:from_real

; we jump here (in 16-bit pmode) when returning to real mode
; Back-to-real-mode sequence from 14.5 of 386INTEL.TXT:
; 3. load segment registers with values "appropriate to real mode"
real2:
 mov ax,DATA_SEL16
 mov ss,ax
 mov ds,ax
 mov es,ax
 mov fs,ax
 mov gs,ax

; 4. disable interrupts
 ; (already done)
; 5. clear the PE bit
 mov eax,cr0
 and al,0FEh
 mov cr0,eax

; 6. far jump to true real mode
 mov word [real_mode_ip],real3
 jmp far [real_mode_ip]

; 7. load an IDT that is compatible with real-mode IVT
real3:
 lidt [real_idt_ptr]

; 9. restore real-mode segment registers
real4:
 mov ax,cs
 mov ss,ax
 mov ds,ax
 mov es,ax
 mov fs,ax
 mov gs,ax

; 8. enable interrupts
 sti

; go back to text mode
 mov ax,3
 int 10h

 mov si,done_msg
 jmp msg_and_exit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; REAL MODE SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; name:   setup
; action:  1. calls wrstr
;   2. prompts the user to press a key, and waits for it
;   3. sets VESA graphics mode VESA_MODE
;      (terminates if mode-set fails)
;   4. zeroes EFLAGS register (disables interrupts,
;      sets IOPL=0, and clears NT bit)
; in:   0-terminated string at DS:SI
; out:   (nothing)
; modifies:  AX
; minimum CPU:  386SX
; notes:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

press_key_msg:
 db "Press a key to continue", 13, 10, 0

vesa_err_msg:
 db "Error setting VESA graphics mode", 13, 10, 0

setup:
 push bx
; display the message pointed to by SI
  call wrstr
; prompt user to press a key
  mov si,press_key_msg
  call wrstr
; wait for the key
  mov ah,0
  int 16h
; switch to VESA graphics mode
  mov ax,4F02h
  mov bx,VESA_MODE
  int 10h
  cmp ah,0
  je setup1
  cmp al,4Fh
  je setup1
  mov si,vesa_err_msg
  jmp msg_and_exit
setup1:
  push dword 0
  popfd
 pop bx
 ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; name:   wrstr
; action:  writes ASCIZ string to text screen
; in:   0-terminated string at DS:SI
; out:   (nothing)
; modifies:  (nothing)
; minimum CPU:  8088
; notes:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

wrstr: push ax
 push bx
 push si
  mov ah,0Eh ; INT 10h: teletype output
  xor bx,bx ; video page 0
  jmp wrstr2
wrstr1:  int 10h
wrstr2:  lodsb
  or al,al
  jne wrstr1
 pop si
 pop bx
 pop ax
 ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; name:   probe_vesa
; action:  detects VESA 2.x video BIOS
; in:   (nothing)
; out (success): ZF=1, [bank_switch_fn] set
; out (failure): ZF=0
; modifies:  EAX, BX, DI, SI
; minimum CPU:  386SX
; notes:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

vesa_msg:
 db "Checking for VESA 2.x video BIOS...", 0

probe_vesa:
 mov si,vesa_msg
 call wrstr
 push es
  mov ax,4F00h
  mov di,vesa_info
  int 10h
  cmp ah,0
  jne no_vesa
; locate the pmode bank-switch function in the video BIOS ROM
  mov ax,4F0Ah
  mov bx,1
  int 10h
  cmp ax,004Fh
  jne no_vesa
; Alexei fixed this for me:
; convert address of bank-switch function from seg:off to linear
  add di, [es:di] ; add function offset to table offset
  xor eax,eax
  mov ax,es
  sub ax,[real_mode_cs] ; our 32-bit code base address isn't 0,
     ; gotta adjust this!!!
  shl eax,4
  movzx edi, di ; yep, this is required as well :)
  add eax,edi
; ...and store it
  mov [bank_switch_fn],eax
; return ZF=1 for success
  xor ax,ax
no_vesa:
 pop es
 ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; name:   probe_s3
; action:  detects S3 video board
; in:   (nothing)
; out (success): ZF=1, [bank_switch_fn] set
; out (failure): ZF=0
; modifies:  EAX, SI
; minimum CPU:  386SX
; notes:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

s3_msg:
 db 13, 10, "no; checking for S3 video...", 0

probe_s3:
 mov si,s3_msg
 call wrstr
 push dx
; lock the enhanced registers
  mov dx,CRTC_INDX
  mov al,039h
  out dx,al

  inc dx
  mov al,00h
  out dx,al

  dec dx
  mov al,38h
  out dx,al

  inc dx
  mov al,00h
  out dx,al
; see if CRTC reg 35h is read-only
  dec dx
  mov al,35h
  out dx,al

  inc dx
  in al,dx
  mov ah,al

  xor al,0Fh
  out dx,al

  in al,dx
  cmp al,ah
  jne no_s3
; unlock S3 registers
  dec dx
  mov al,38h
  out dx,al

  inc dx
  mov al,48h
  out dx,al

  dec dx
  mov al,39h
  out dx,al

  inc dx
  mov al,0A5h
  out dx,al
; can CRTC reg 35h be changed now?
  dec dx
  mov al,35h
  out dx,al

  inc dx
  in al,dx
  mov ah,al

  xor al,0Fh
  out dx,al

  in al,dx
  cmp al,ah
  je no_s3
; found S3; restore old value of reg 35h
  mov al,ah
  out dx,al
; set [bank_switch_fn] to linear address of set_s3_bank
  mov dword [bank_switch_fn],set_s3_bank
; return ZF=1 for success
  xor ax,ax
no_s3:
 pop dx
 ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; name:   probe_cirrus
; action:  detects video board with Cirrus 5422 chipset
; in:   (nothing)
; out (success): ZF=1, [bank_switch_fn] set
; out (failure): ZF=0
; modifies:  EAX, SI
; minimum CPU:  386SX
; notes:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

cirrus_msg:
 db 13, 10, "no; checking for Cirrus 5422 video...", 0

probe_cirrus:
 mov si,cirrus_msg
 call wrstr
 push dx
; unlock Cirrus registers
  mov dx,SEQN_INDX
  mov al,06h
  out dx,al
  inc dx
  mov al,12h
  out dx,al
; lock reg should now read as 12h
  in al,dx
  cmp al,12h
  jne no_cirrus
; check the chip ID
  mov dx,CRTC_INDX
  mov al,27h
  out dx,al
  inc dx
  in al,dx
  shr al,2
  cmp al,22h
  jb no_cirrus
  cmp al,28h
  ja no_cirrus
; found a Cirrus board
; set [bank_switch_fn] to linear address of set_cirrus_bank
  mov dword [bank_switch_fn],set_cirrus_bank
; return ZF=1 for success
  xor ax,ax
no_cirrus:
 pop dx
 ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 32-BIT PROTECTED MODE SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 BITS 32

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; name:   set_cirrus_bank
; action:  sets 64K video bank (Cirrus 5422 video only)
; in:   bank number in DL
; out:   (nothing)
; modifies:  (nothing)
; minimum CPU:  386SX
; notes:  EDX and EAX are used only to make this code
;   smaller. DX and AX could be used instead,
;   if you want code to run on 16-bit CPUs.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

set_cirrus_bank:
 push edx
 push eax
  mov eax,edx

  and al,0Fh
  shl al,4
  mov ah,al

  mov dx,GRFX_INDX
  mov al,09h
  out dx,al

  inc edx
  mov al,ah
  out dx,al
 pop eax
 pop edx
 ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; name:   set_s3_bank
; action:  sets 64K video bank (S3 video only)
; in:   bank number in DL
; out:   (nothing)
; modifies:  (nothing)
; minimum CPU:  386SX
; notes:  EDX and EAX are used only to make this code
;   smaller. DX and AX could be used instead,
;   if you want code to run on 16-bit CPUs.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

set_s3_bank:
 push edx
 push eax
  mov eax,edx

  and al,0Fh
  mov ah,al

; grrrr...mode-set locked the S3 registers, so unlock them again
; xxx - do this after mode-set
  mov dx,CRTC_INDX
  mov al,38h
  out dx,al

  inc edx
  mov al,48h
  out dx,al

  dec edx
  mov al,39h
  out dx,al

  inc edx
  mov al,0A5h
  out dx,al

; now: do the bank-switch
  mov dx,CRTC_INDX
  mov al,35h
  out dx,al

  inc edx
  in al,dx
  and al,0F0h

  or al,ah
  out dx,al
 pop eax
 pop edx
 ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; name:   unhand
; action:  handler for otherwise unhandled exceptions;
;   draws short red diagonal line on SVGA screen
; in:   (nothing)
; out:   (nothing)
; modifies:  (nothing)
; minimum CPU:  386SX
; notes:  DOES NOT RETURN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; write to physical address A0000 instead of virtual address
; LFB_BASE to avoid more page faults
unhand:
 mov ax,LINEAR_SEL
 mov es,ax
 mov byte [es:0A0000h + (WIDTH + 1) * 0],4
 mov byte [es:0A0000h + (WIDTH + 1) * 1],4
 mov byte [es:0A0000h + (WIDTH + 1) * 2],4
 mov byte [es:0A0000h + (WIDTH + 1) * 3],4

 jmp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; name:   page_fault
; action:  handler for INT 0Eh (page fault); performs
;   SVGA bank-switching for faults that come
;   from the simulated linear framebuffer
; in:   (nothing)
; out:   (nothing)
; modifies:  (nothing)
; minimum CPU:  386SX
; notes:  This is an interrupt handler, so be careful.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

page_fault:
 pusha

; did it come from the LFB? jump to 'unhand' if not
  mov eax,cr2
  sub eax,LFB_BASE
  jc unhand
  cmp eax, WIDTH*HEIGHT
  jnc unhand  ; some extra checking :)

; set SVGA bank
  mov edx,cr2
  shr edx,16
  mov bh,0  ; bh=0 - set bank no. (VESA only)

  call [bank_switch_fn]

; unmap all pages in the LFB
; xxx - this is slow; need only unmap the pages that are currently mapped
  mov ebx,[fb_page_table]
  mov ecx,1024
unmap:
  and byte [es:ebx],0FEh ; Present=0
  add ebx,4
  loop unmap

; bits 31:22 of faulting adr are page dir index
; bits 21:12 of faulting adr are page table index
  mov ebx,cr2
  shr ebx,12
  and ebx,3FFh
  shl ebx,2
  add ebx,[fb_page_table]

; map in 16 4K pages (one 64K bank)
  mov ecx,16
map_fb:
  or byte [es:ebx],01h ; Present=1
  add ebx,4
  loop map_fb

; we changed the page tables, so reload CR3 to flush the page
; table cache (the TLB). 486+ could use INVLPG instruction instead
  mov eax,cr3
  mov cr3,eax
 popa

; drop error code
 add esp,4
 iret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; name:   draw_lines
; action:  draw green and blue 'X' on the screen,
;   return after a delay
; in:   (nothing)
; out:   (nothing)
; modifies:  ECX, EBX
; minimum CPU:  386SX
; notes:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Note the absence of bank-switching code here. Simple, eh?
; xxx - this code for 256-color screen only

draw_lines:
 mov ecx,HEIGHT
 mov ebx,LFB_BASE + (WIDTH - HEIGHT) / 2
ul_lr:
 mov byte [es:ebx],1 ; blue
 add ebx,WIDTH + 1
 loop ul_lr

 mov ecx,HEIGHT
 mov ebx,LFB_BASE + (WIDTH - HEIGHT) / 2 + HEIGHT
ur_ll:
 mov byte [es:ebx],2 ; green
 add ebx,WIDTH - 1
 loop ur_ll

; xxx - await keypress here -- somehow
 mov ecx,03FFFFFFh
spin:
 loop spin
 ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MAIN 32-BIT PMODE CODE, here from VCPI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

from_vcpi:
 mov ax,DATA_SEL
 mov ss,ax
 mov ds,ax
 mov ax,LINEAR_SEL
 mov es,ax
 mov fs,ax
 mov gs,ax

 call draw_lines

; return to V86 mode
 mov eax,0000DE0Ch

; from E-67DE0C of Ralf Brown's list:
;   DS = segment selector mapping entire linear
;  address space obtained via AX=DE01h
; I'm not sure what that means, but I peeked inside EMM386.EXE,
; and it sets DS for me, so I don't have to.

 movzx ebx,word [real_mode_cs]
 mov ebp,esp
 push ebx   ; GS
 push ebx   ; FS
 push ebx   ; DS
 push ebx   ; ES

 push ebx   ; SS
 push ebp   ; ESP
 push dword 00023020h  ; EFLAGS
 push ebx   ; CS
 push dword real4  ; EIP

 call far [vcpi_entry]

; should not reach this
 jmp short $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MAIN 32-BIT PMODE CODE, here from "raw" mode (real mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

from_real:
 mov ax,DATA_SEL
 mov ss,ax
 mov ds,ax
 mov ax,LINEAR_SEL
 mov es,ax
 mov fs,ax
 mov gs,ax

 call draw_lines

; back-to-real-mode sequence from 14.5 of 386INTEL.TXT
; 1. TURN OFF PAGING
; 1a. jump to code memory which is identity-mapped
 ; (already done)
; 1b. clear the PG bit
 mov eax,cr0
 and eax,7FFFFFFFh
 mov cr0,eax

; 1c. "Move zeros to CR3 to clear out the paging cache."
 xor eax,eax
 mov cr3,eax

; 2. jump to 16-bit code segment (real2 is above, in the BITS 16 section)
 jmp CODE_SEL16:real2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DATA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

v86_msg:
 db "CPU is in virtual 8086 mode...", 13, 10, 0

err_msg:
 db "...and I don't know why (no EMM386, no Windows).", 13, 10
 db "Sorry, can't switch to protected mode.", 13, 10, 0

win_msg:
 db "Sorry, this code won't run in a Windows DOS box.", 13, 10
 db "Re-start your computer in 'MS-DOS mode'.", 13, 10, 0

vcpi_msg:
 db "VCPI server detected (probably EMM386).", 13, 10
 db "I will try to use VCPI to switch to protected mode.", 13, 10, 0

vcpi_err_msg:
 db "Error getting VCPI server entry point.", 13, 10, 0

real_msg:
 db "CPU is in real mode.", 13, 10, 0

done_msg:
 db "Back to DOS, wheee!", 13, 10, 0

; Even if you don't use TSS-based task-switching, you need one
; TSS to hold the user stack pointer.
tss:
 dw 0, 0   ; back link

 dd 0   ; ESP0
 dw DATA_SEL, 0  ; SS0, reserved

 dd 0   ; ESP1
 dw 0, 0   ; SS1, reserved

 dd 0   ; ESP2
 dw 0, 0   ; SS2, reserved

 dd 0   ; CR3
 dd 0, 0   ; EIP, EFLAGS
 dd 0, 0, 0, 0  ; EAX, ECX, EDX, EBX
 dd 0, 0, 0, 0  ; ESP, EBP, ESI, EDI
 dw 0, 0   ; ES, reserved
 dw 0, 0   ; CS, reserved
 dw 0, 0   ; SS, reserved
 dw 0, 0   ; DS, reserved
 dw 0, 0   ; FS, reserved
 dw 0, 0   ; GS, reserved
 dw 0, 0   ; LDT, reserved
 dw 0, 0   ; debug, IO perm. bitmap

; null descriptor
gdt:
 dw 0   ; limit 15:0
 dw 0   ; base 15:0
 db 0   ; base 23:16
 db 0   ; type
 db 0   ; limit 19:16, flags
 db 0   ; base 31:24
LINEAR_SEL      equ     $-gdt
 dw 0FFFFh
 dw 0
 db 0
 db 92h   ; present, ring 0, data, expand-up, writable
 db 0CFh   ; page-granular, 32-bit
 db 0
CODE_SEL equ $-gdt
gdt2:
 dw 0FFFFh
 dw 0
 db 0
 db 9Ah   ; present, ring 0, code, non-conforming, readable
 db 0CFh   ; page-granular, 32-bit
 db 0
DATA_SEL equ $-gdt
gdt3:
 dw 0FFFFh
 dw 0
 db 0
 db 92h   ; present, ring 0, data, expand-up, writable
 db 0CFh   ; page-granular, 32-bit
 db 0
CODE_SEL16 equ $-gdt
gdt4:
 dw 0FFFFh
 dw 0
 db 0
 db 9Ah   ; present, ring 0, code, non-conforming, readable
 db 0   ; byte-granular, 16-bit
 db 0
DATA_SEL16 equ $-gdt
gdt5:
 dw 0FFFFh
 dw 0
 db 0
 db 92h   ; present, ring 0, data, expand-up, writable
 db 0   ; byte-granular, 16-bit
 db 0
TSS_SEL  equ $-gdt
gdt6:
 dw 103
 dw 0
 db 0
 db 089h   ; Ring 0 available 32-bit TSS
 db 0
 db 0
VCPI_SEL equ $-gdt
gdt7:
 dd 0, 0   ; dummy descriptors used by VCPI

 dd 0, 0

 dd 0, 0
gdt_end:

idt:
 %rep 14

 dw 0   ; low 16 bits of ISR address
 dw CODE_SEL  ; selector
 db 0   ; word count
 db 8Eh   ; access byte: Present, Ring 0, '386 intr gate
 dw 0   ; high 16 bits of ISR

 %endrep
idt_0e:
 %rep 18

 dw 0   ; low 16 bits of ISR address
 dw CODE_SEL  ; selector
 db 0   ; word count
 db 8Eh   ; access byte: Present, Ring 0, '386 intr gate
 dw 0   ; high 16 bits of ISR

 %endrep
idt_end:

gdt_ptr:
 dw gdt_end - gdt - 1 ; GDT limit
 dd gdt   ; linear, physical address of GDT

idt_ptr:
 dw idt_end - idt - 1 ; IDT limit
 dd idt   ; linear, physical address of IDT

real_idt_ptr:
 dw 3FFh   ; limit 1023
 dd 0   ; IDT (IVT, actually) at address 0

bank_switch_fn:
 dd 0

krnl_page_table:
 dw 0

fb_page_table:
 dd 0

real_mode_ip:
 dw 0
real_mode_cs:
 dw 0

page_info:
 times 4096 db 0         ; padding to 4K boundary
 times 4096 db 0         ; page dir somewhere in here
 times 4096 db 0         ; "framebuffer" page table for SLFB
 times 4096 db 0         ; "kernel" page table for bottom 4 meg

vcpi_entry:
 dd 0
 dw VCPI_SEL

vcpi_control_block:
vcpi_cr3:
 dd 0
vcpi_gdtr:
 dd gdt_ptr
vcpi_idtr:
 dd idt_ptr
vcpi_ldtr:
 dw 0
vcpi_tr:
 dw TSS_SEL
vcpi_eip:
 dd from_vcpi
vcpi_cs:
 dw CODE_SEL

vesa_info:
 db "VBE2"
vesa_minor:
 db 0
vesa_major:
 db 0
 times 506 db 0
