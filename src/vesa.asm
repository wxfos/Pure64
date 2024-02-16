# VBEVESA BIOS EXTENSION,1024*768 256
# Baby OS LOGO
# 2012-12-28 21:03
# guzhoudiaoke@126.com
# https://blog.csdn.net/guzhou_diaoke/article/details/8450689
# https://blog.csdn.net/weixin_33912246/article/details/92007209
# https://blog.csdn.net/wascm/article/details/704603
# https://blog.csdn.net/mirkerson/article/details/5897499

.section .text
.global _start
.code16
 
_start:
	jmp		main
 
#--------------------------------------------------------------
# 
#	0
clear_screen:				# 
	movb	$0x06,	%ah		# 0x06
	movb	$0,		%al		# 
	movb	$0,		%ch		# 
	movb	$0,		%ch		# 	
	movb	$24,	%dh		# 
	movb	$79,	%dl		# 
	movb	$0x07,	%bh		# 
	int		$0x10
	ret
 
#----------------------------------------------------------------
# VGA 0x13
set_video_mode_0x13:
	movb	$0,			%ah			# 0x0
	movb	$MODE_0x13,	%al			# 
	int		$0x10
 
	movw	$0x13,		video_mode
	movw	$320,		screen_x
	movw	$200,		screen_y
	movl	$0xb8000,	video_ram
	
	ret
 
#----------------------------------------------------------------
# VBE 0x103
#	alah
set_video_mode_vbe_0x103:
	# VBE
	movw	$BUFFER_SEG,		%ax
	movw	%ax,				%es
	movw	%ax,				%ds
	xorw	%di,				%di
	
	# VBEVBE
	movb	$0x4f,				%ah			# VBE
	movb	$0x00,				%al			# 
	int		$0x10
 
	cmp		$0x004f,			%ax			# VBEAX0x004f
	jne		1f
	movw	0x04(%di),			%ax
	cmp		$0x0200,			%ax			# VBE2.0
	jb		1f
 
	# MODE_VBE_0x13
	movw	$MODE_VBE_0x103,	%cx
	movb	$0x4f,				%ah			# VBE
	movb	$0x01,				%al			# 
	int		$0x10
 
	cmpb	$0x00,				%ah			# 
	jne		1f
	cmpb	$0x4f,				%al			# 
	jne		1f
	cmpb	$8,					0x19(%di)	# 8bit
	jne		1f
	cmpb	$4,					0x1b(%si)	# 4
	jne		1f
	movw	(%di),				%ax
	andw	$0x0080,			%ax
	jz		1f								# AX71
 
	# 
	movw	$MODE_VBE_0x103,	%bx
	addw	$0x4000,			%bx			# BX14
	movb	$0x4f,				%ah			# VBE
	movb	$0x02,				%al			# 
	int		$0x10
 
	# 
	movw	$MODE_VBE_0x103,	video_mode
	movw	0x12(%di),			%ax
	movw	%ax,				screen_x
	movw	0x02(%di),			%ax
	movw	%ax,				screen_y
	movl	0x28(%di),			%eax
	movl	%eax,				video_ram
	movw	$1,					%ax
	ret
1:
	movw	$0,					%ax
	ret
 
	
#----------------------------------------------------------------
# 
set_screen_bk_color:
	movw	$VIDEO_PALLETE_PORT,	%dx
	movb	$PA_INDEX_BACKGROUND,	%al
	outb	%al,					%dx
 
	movw	$COLOR_SELECTION_PORT,	%dx
	movb	$0,						%al		# 
	outb	%al,					%dx
	movb	$0,						%al		# 
	outb	%al,					%dx
	movb	$18,					%al		# 18/63
	outb	%al,					%dx
	ret
 
#----------------------------------------------------------------
# 
#	1
#	ESDIPA_INDEX_WHITE
draw_some_pixels:
	# 1636363
	movw	$VIDEO_PALLETE_PORT,	%dx
	movb	$PA_INDEX_WHITE,		%al
	outb	%al,					%dx
	movw	$COLOR_SELECTION_PORT,	%dx
	movb	$63,					%al		# 
	outb	%al,					%dx
	movb	$63,					%al		# 
	outb	%al,					%dx
	movb	$63,					%al		# 
	outb	%al,					%dx
 
	# ES
	movw	$VIDEO_SEG_GRAPHIC,		%ax
	movw	%ax,					%es
 
	# 
	movw	$(800*5),				%di		# 
	movb	$PA_INDEX_WHITE,		%al
	movw	$800,					%cx		# 800
 
draw_a_pixel:
	stosb
	#addl	$799,					%edi
	loop	draw_a_pixel
 
	ret
 
main:
	movw	%cx,	%ax
	movw	%ax,	%ds
	movw	%ax,	%es
 
	call	clear_screen					# 
	call	set_video_mode_vbe_0x103		# 
 
	cmpw	$0,		%ax
	jne		1f
	call	set_video_mode_0x13
1:
	call	set_screen_bk_color	# 
	call	draw_some_pixels	# 
 
1:
	jmp		1b
 
# 
	VIDEO_SEG_TEXT		= 0x0e00
	VIDEO_SEG_GRAPHIC	= 0xa000
	BUFFER_SEG			= 0x800
 
	VIDEO_PALLETE_PORT	= 0x3c8
	COLOR_SELECTION_PORT= 0x3c9
	
	MODE_0x13			= 0x13
	MODE_VBE_0x105		= 0x0105
	MODE_VBE_0x103		= 0x0103
 
	PA_INDEX_BACKGROUND	= 0x0
	PA_INDEX_WHITE		= 0x1
 
video_mode:
	.short	0
screen_x:
	.short	0
screen_y:
	.short	0
video_ram:
	.long	0
 
	.org	0x1fe,	0x90
	.word	0xaa55
