!
! SYS_SIZE is the number of clicks (16 bytes) to be loaded.
! 0x3000 is 0x30000 bytes = 196kB, more than enough for current
! versions of linux
!
SYSSIZE = 0x3000
!
!	bootsect.s		(C) 1991 Linus Torvalds
!
! bootsect.s is loaded at 0x7c00 by the bios-startup routines, and moves
! iself out of the way to address 0x90000, and jumps there.
!
! It then loads 'setup' directly after itself (0x90200), and the system
! at 0x10000, using BIOS interrupts. 
!
! NOTE! currently system is at most 8*65536 bytes long. This should be no
! problem, even in the future. I want to keep it simple. This 512 kB
! kernel size should be enough, especially as this doesn't contain the
! buffer cache as in minix
!
! The loader has been made as simple as possible, and continuos
! read errors will result in a unbreakable loop. Reboot by hand. It
! loads pretty fast by getting whole sectors at a time whenever possible.

.globl begtext, begdata, begbss, endtext, enddata, endbss
.text
begtext:
.data
begdata:
.bss
begbss:
.text

SETUPLEN = 4				    ! nr of setup-sectors
BOOTSEG  = 0x07c0			    ! original address of boot-sector
INITSEG  = 0x9000			    ! we move boot here - out of the way
SETUPSEG = 0x9020			    ! setup starts here
SYSSEG   = 0x1000			    ! system loaded at 0x10000 (65536).
ENDSEG   = SYSSEG + SYSSIZE		! where to stop loading

! ROOT_DEV:	0x000 - same type of floppy as boot.
!		0x301 - first partition on first drive etc
ROOT_DEV = 0x306

; 梦开始的地方

entry start
start:
	mov	ax,#BOOTSEG         ; 把ds设置成0x07c0
	mov	ds,ax               ; 方便访问内存的时候利用这个段基址寻址
	mov	ax,#INITSEG
	mov	es,ax               ; 把es设置成0x9000
	mov	cx,#256             ; 把cx设置成256
	sub	si,si
	sub	di,di               ; si和di清零
	rep                     ; 重复执行movw执行，即每次复制一个字（两个字节）
	movw                    ; 即将ds:cs开始的256个字的数据复制到es:di处（0x07c00 -> 0x90000）
	jmpi	go,INITSEG      ; 段间跳转，跳转到0x9000:go处继续执行

; bootsect编译后放置在硬盘的第一扇区
; BIOS加载这个所谓的启动区共512字节的内容到0x07c00后
; 又原样复制了一份到0x90000处
; 执行完上面的代码后，内存情况如下所示
;            ___________
;            |         |
;            |         |
;            |         |
;            |         |
;            |         |
;      ----->|---------|
;  bootsect  | 512byte | 
;      ----->|---------|<-0x90000
;            |         |         /\\
;            |         |           \ 复制
;            |         |          / 
;      ----->|---------|         / 
;  bootsect  | 512byte |        /
;      ----->|---------|<-0x07c00
;            |         |
;            |_________|
;              memory


; 上一段代码跳到了go,0x9000处
; go,0x9000表示地址0x9000:go
; 从之前的分析可以看出0x90000处存放的是编译好的bootsect
; 其实可以这么认为，0x90000是修正了基地址
; 而go标签则是代码在内存中的偏移量
; 最终的结果CPU跳到了这儿继续执行
; 至于为啥非得加这么一条jump，那就不得不说jump的副作用了
; 或者说jump为什么能“jump”
; jump实际上做的是设置cs:ip，上面的jumpi执行后cs=0x9000，ip=go标签的地址

; 好了好了，继续往下看
go:	mov	ax,cs                   ; cs在之前jump后被设置为0x9000
	mov	ds,ax
	mov	es,ax
! put stack at 0x9ff00.
	mov	ss,ax                   ; 将三个段寄存器ds/es/ss都设置为0x9000，即设置了如何访问数据、代码和栈
	mov	sp,#0xFF00		        ; arbitrary value >>512，设置栈顶指针为0xff00
                                ; 那么拼接上ss，最终栈顶地址为0x9ff00

; 在之前的内存分布图上添加栈顶指针
;            ___________
;            |         |
;            |    栈   |<-0x9ff00 (stack top pointer)
;            |    ||   |
;            |    ||   |
;            |    \/   |
;            |         |
;      ----->|---------|
;  bootsect  | 512byte | 
;      ----->|---------|<-0x90000
;            |         |         /\\
;            |         |           \ 复制
;            |         |          / 
;      ----->|---------|         / 
;  bootsect  | 512byte |        /
;      ----->|---------|<-0x07c00
;            |         |
;            |_________|
;              memory

! load the setup-sectors directly after the bootblock.
! Note that 'es' is already set up. es = 0x9000
load_setup:
	mov	dx,#0x0000		    ! drive 0, head 0
	mov	cx,#0x0002		    ! sector 2, track 0
	mov	bx,#0x0200		    ! address = 512, in INITSEG
	mov	ax,#0x0200+SETUPLEN	! service 2, nr of sectors
	int	0x13			    ! read it
	jnc	ok_load_setup		! ok - continue
	mov	dx,#0x0000
	mov	ax,#0x0000		    ! reset the diskette
	int	0x13                ; 没看明白这里为啥要再发起一次0x13中断
	j	load_setup          ; 如果setup分区加载失败，则重试直到成功

; 代码看起来长，其实好理解
; 就是发起了0x13号中断来读取磁盘第2扇区开始，共计4个扇区的数据到到内存0x90200处
;            ___________
;            |         |
;            |    栈   |<-0x9ff00 (stack top pointer)
;            |    ||   |
;            |    ||   |
;            |    \/   |
;            |         |
;      ----->|---------| 
;            | 512byte | 
;            |---------|
;            | 512byte |
;    setup   |---------|
;            | 512byte |
;            |---------| 
;            | 512byte | 
;      ----->|---------|<-0x90200
;  bootsect  | 512byte | 
;      ----->|---------|<-0x90000
;            |         |         /\\
;            |         |           \ 复制
;            |         |          / 
;      ----->|---------|         / 
;  bootsect  | 512byte |        /
;      ----->|---------|<-0x07c00
;            |         |
;            |_________|
;              memory


ok_load_setup:
! Get disk drive parameters, specifically nr of sectors/track

	mov	dl,#0x00
	mov	ax,#0x0800		! AH=8 is get drive parameters
	int	0x13
	mov	ch,#0x00
	seg cs
	mov	sectors,cx
	mov	ax,#INITSEG
	mov	es,ax

! Print some inane message

	mov	ah,#0x03		! read cursor pos
	xor	bh,bh
	int	0x10
	
	mov	cx,#24
	mov	bx,#0x0007		! page 0, attribute 7 (normal)
	mov	bp,#msg1
	mov	ax,#0x1301		! write string, move cursor
	int	0x10            ; 打印提示信息

! ok, we've written the message, now
! we want to load the system (at 0x10000)

	mov	ax,#SYSSEG      ; SYSSEG = 0x1000
	mov	es,ax	     	! segment of 0x010000
	call	read_it     ; 将硬盘第6分区开始的共计240个分区加载次内存0x10000处
;            ___________
;            |         |
;            |    栈   |<-0x9ff00 (stack top pointer)
;            |    ||   |
;            |    ||   |
;            |    \/   |
;            |         |
;      ----->|---------| 
;            | 512byte | 
;            |---------|
;            | 512byte |
;    setup   |---------|
;            | 512byte |
;            |---------| 
;            | 512byte | 
;      ----->|---------|<-0x90200              -------  ---
;  bootsect  | 512byte |                       |          |
;      ----->|---------|<-0x90000              |          |
;            |         |         /\\           |          |
;            |         |           \ 复制      |----------|<---------
;      ----->|---------|            |          |          |
;            | 512byte |            |          |          |
;            |---------|            |          |          |
;            |    | |  |            |          |  system  |  240个扇区      <--- others
;    system  |    | |  |            |          |          |
;(240个扇区) |    | |  |            |          |          |
;            |---------|            |          |          |
;            | 512byte |           /           |----------|<---------
;      ----->|---------|<-0x10000 /            |          | 
;            |---------|         /             |   setup  |  4个扇区        <--- setup.s
;  bootsect  | 512byte |       /               |          |        
;      ----->|---------|<-0x07c00              |----------|<---------
;            |         |                       | bootsect |  1个扇区        <--- bootsect.s
;            |_________|                       |__________|<--------- 
;              memory                             disk
; 至此，整个操作系统的全部代码都加载进了内存中

	call	kill_motor

! After that we check which root-device to use. If the device is
! defined (!= 0), nothing is done and the given device is used.
! Otherwise, either /dev/PS0 (2,28) or /dev/at0 (2,8), depending
! on the number of sectors that the BIOS reports currently.

	seg cs
	mov	ax,root_dev
	cmp	ax,#0
	jne	root_defined
	seg cs
	mov	bx,sectors
	mov	ax,#0x0208		! /dev/ps0 - 1.2Mb
	cmp	bx,#15
	je	root_defined
	mov	ax,#0x021c		! /dev/PS0 - 1.44Mb
	cmp	bx,#18
	je	root_defined
undef_root:
	jmp undef_root
root_defined:
	seg cs
	mov	root_dev,ax

! after that (everyting loaded), we jump to
! the setup-routine loaded directly after
! the bootblock:
	jmpi	0,SETUPSEG      ; SETUPSEG = 0x9020
; 当一切加载就绪，跳转到0x9020:0，即0x90200地址处
; 从上一幅内存分布图可知，这个地址是setup起始处
; 至此，bootsect.s告一段落，接下来进入setup.s

! This routine loads the system at address 0x10000, making sure
! no 64kB boundaries are crossed. We try to load it as fast as
! possible, loading whole tracks whenever we can.
!
! in:	es - starting address segment (normally 0x1000)
!
sread:	.word 1+SETUPLEN	! sectors read of current track
head:	.word 0	    		! current head
track:	.word 0	    		! current track

read_it:
	mov ax,es
	test ax,#0x0fff
die:
    jne die			    ! es must be at 64kB boundary
	xor bx,bx		    ! bx is starting address within segment
rp_read:
	mov ax,es
	cmp ax,#ENDSEG		! have we loaded all yet?
	jb ok1_read
	ret
ok1_read:
	seg cs
	mov ax,sectors
	sub ax,sread
	mov cx,ax
	shl cx,#9
	add cx,bx
	jnc ok2_read
	je ok2_read
	xor ax,ax
	sub ax,bx
	shr ax,#9
ok2_read:
	call read_track
	mov cx,ax
	add ax,sread
	seg cs
	cmp ax,sectors
	jne ok3_read
	mov ax,#1
	sub ax,head
	jne ok4_read
	inc track
ok4_read:
	mov head,ax
	xor ax,ax
ok3_read:
	mov sread,ax
	shl cx,#9
	add bx,cx
	jnc rp_read
	mov ax,es
	add ax,#0x1000
	mov es,ax
	xor bx,bx
	jmp rp_read

read_track:
	push ax
	push bx
	push cx
	push dx
	mov dx,track
	mov cx,sread
	inc cx
	mov ch,dl
	mov dx,head
	mov dh,dl
	mov dl,#0
	and dx,#0x0100
	mov ah,#2
	int 0x13
	jc bad_rt
	pop dx
	pop cx
	pop bx
	pop ax
	ret
bad_rt:	mov ax,#0
	mov dx,#0
	int 0x13
	pop dx
	pop cx
	pop bx
	pop ax
	jmp read_track

/*
 * This procedure turns off the floppy drive motor, so
 * that we enter the kernel in a known state, and
 * don't have to worry about it later.
 */
kill_motor:
	push dx
	mov dx,#0x3f2
	mov al,#0
	outb
	pop dx
	ret

sectors:
	.word 0

msg1:
	.byte 13,10
	.ascii "Loading system ..."
	.byte 13,10,13,10

.org 508
root_dev:
	.word ROOT_DEV
boot_flag:
	.word 0xAA55

.text
endtext:
.data
enddata:
.bss
endbss:
