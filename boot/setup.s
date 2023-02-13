!
!	setup.s		(C) 1991 Linus Torvalds
!
! setup.s is responsible for getting the system data from the BIOS,
! and putting them into the appropriate places in system memory.
! both setup.s and system has been loaded by the bootblock.
!
! This code asks the bios for memory/disk/other parameters, and
! puts them in a "safe" place: 0x90000-0x901FF, ie where the
! boot-block used to be. It is then up to the protected mode
! system to read them from there before the area is overwritten
! for buffer-blocks.
!

; bootblock将setup和system加载到内存后
; setup通过BIOS预置的中断服务程序获取设备硬件信息，并保存到0x90000~0x901ff处
; 也就是覆盖掉了从0x7c00复制到0x90000的一份bootsect拷贝

! NOTE! These had better be the same as in bootsect.s!

INITSEG  = 0x9000	! we move boot here - out of the way
SYSSEG   = 0x1000	! system loaded at 0x10000 (65536).
SETUPSEG = 0x9020	! this is the current segment

.globl begtext, begdata, begbss, endtext, enddata, endbss
.text
begtext:
.data
begdata:
.bss
begbss:
.text

entry start
start:

! ok, the read went well so we get current cursor position and save it for
! posterity.

	mov	ax,#INITSEG	! this is done in bootsect already, but... ; 哈哈哈
	mov	ds,ax
	mov	ah,#0x03	! read cursor pos
	xor	bh,bh
	int	0x10		! save it in known place, con_init fetches
	mov	[0],dx		! it from 0x90000.

    ; int 0x10是BIOS提供的显示服务中断处理程序
    ; 当ah = 0x03时，表示读取光标位置
    ; 执行结束后，dx里保存着光标的位置，高8位dh存储行号，低8位dl存储列号
    ; 接下来的大部分代码都做着类似的工作


! Get memory size (extended mem, kB)

	mov	ah,#0x88
	int	0x15
	mov	[2],ax

! Get video-card data:

	mov	ah,#0x0f
	int	0x10
	mov	[4],bx		! bh = display page
	mov	[6],ax		! al = video mode, ah = window width

! check for EGA/VGA and some config parameters

	mov	ah,#0x12
	mov	bl,#0x10
	int	0x10
	mov	[8],ax
	mov	[10],bx
	mov	[12],cx

! Get hd0 data

	mov	ax,#0x0000
	mov	ds,ax
	lds	si,[4*0x41]
	mov	ax,#INITSEG
	mov	es,ax
	mov	di,#0x0080
	mov	cx,#0x10
	rep
	movsb

! Get hd1 data

	mov	ax,#0x0000
	mov	ds,ax
	lds	si,[4*0x46]
	mov	ax,#INITSEG
	mov	es,ax
	mov	di,#0x0090
	mov	cx,#0x10
	rep
	movsb

; 只写了hd0、hd1，合着那会儿最多两块硬盘是吧

! Check that there IS a hd1 :-)

	mov	ax,#0x01500
	mov	dl,#0x81
	int	0x13
	jc	no_disk1
	cmp	ah,#3
	je	is_disk1
no_disk1:
	mov	ax,#INITSEG
	mov	es,ax
	mov	di,#0x0090
	mov	cx,#0x10
	mov	ax,#0x00
	rep
	stosb
is_disk1:

; 这种约定存取地址的方式在今天看起来是有些难以想象的
; 但考虑到实模式都是直接用的物理地址
; 如此这般确实是最简洁优雅的方式
; setup通过BIOS获取的参数如下所示
;   ____________
;   |          |
;   |          |
;   |          |
;   |----------|
;   | 根设备号 | 
;   |          | 
;   |----------|<- 0x901fc
;   |          | 
;   |          | 
;   |----------|
;   | 盘2参数  | 
;   |----------|<- 0x90090
;   | 盘1参数  | 
;   |----------|<- 0x90080
;   |          | 
;   |          | 
;   |          | 
;   |----------|<- 0x90010
;   | 屏幕列数 | 
;   |----------|<- 0x9000f
;   | 屏幕行数 | 
;   |----------|<- 0x9000e
;   | 显卡参数 | 
;   |          | 
;   |----------|<- 0x9000c
;   | 显示状态 | 
;   |----------|<- 0x9000b
;   | 显示内存 | 
;   |----------|<- 0x9000a
;   | 未    知 | 
;   |          | 
;   |----------|<- 0x90008
;   | 字符列数 | 
;   |----------|<- 0x90007
;   | 显示模式 | 
;   |----------|<- 0x90006
;   | 显示页面 | 
;   |          | 
;   |----------|<- 0x90004
;   | 内存大小 | 
;   |          | 
;   |----------|<- 0x90002
;   | 光标位置 |
;   |          | 
;   |----------|<- 0x90000
;      memory


! now we want to move to protected mode ...
;
; 马上要用系统的中断向量表替换BIOS的中断向量表，因而需要先关中断

	cli			        ! no interrupts allowed !

! first we move the system to it's rightful place

	mov	ax,#0x0000
	cld			        ! 'direction'=0, movs moves forward
do_move:
	mov	es,ax		    ! destination segment
	add	ax,#0x1000
	cmp	ax,#0x9000
	jz	end_move
	mov	ds,ax		    ! source segment
	sub	di,di
	sub	si,si
	mov cx,#0x8000      ; 复制从0x10000到0x90000，共计0x80000的数据
	rep
	movsw               ; 将ds:si开始的system复制到es:di处，即0x10000 -> 0x00000
	jmp	do_move

; 其实就是把system挪个地儿，挪到了0地址处
;            ___________
;            |         |         
;            |   栈    |<- 0x9ff00 (stack top pointer)
;            |   ||    |         
;            |   ||    |         
;            |   \/    |         
;            |         |         
;      ----->|---------|<- 0x90a00
;            |         |
;     setup  |         |         
;      ----->|---------|<- 0x90200
;            |         |
;      ----->|---------|
;   hardware |         |
;     info   |         | 
;      ----->|---------|<- 0x90000
;            |         |         
;            |         |         
;      ----->|---------|<- 0x80000          
;            |         |        
;    system  |  512KB  |
;            |         |
;      ----->|_________|<- 0x00000
;              memory


; 实模式切换到保护模式后，需要使用全局描述符表（或者说段表）来寻址
; 段表的地址保存在gdtr寄存器中
;        ______________________________________
; gdtr:  |  addrress(32bit)  | boundary(16bit)|
;        --------------------------------------
; boundary和length哪一个设计会更好一些呢？
; 模式切换后，物理地址计算规则会发生变化，以ds:ax举例
; 在实模式下，实际上地址线是有20bit的，因而将ds左移四位加上ax即可
; 到了保护模式里，ds等段寄存器不再保存基址，改为保存段选择子（可以认为是该段在段表中的序号）
; 通过gdtr找到段表，用段选择子找到对应的段表项，即段描述符
; 段描述符里保存着基址、段长以及权限位等
; 拿着段描述符里的基址，加上偏移量即可得到线性地址（不开分页等机制可以认为是物理地址）
; 保护模式下逻辑地址到线性地址的转换可以参考Intel手册
;   Volume 3 Chapter 3.4 Logical And Linear Address
; 从上面可以发现，gdt/gdtr在地址变换中很重要
; 而下面的代码就是设置gdt/idt（idt即中断描述符表
;
! then we load the segment descriptors

end_move:
	mov	ax,#SETUPSEG	! right, forgot this at first. didn't work :-)
	mov	ds,ax
	lidt	idt_48		! load idt with 0,0
	lgdt	gdt_48		! load gdt with whatever appropriate
    ; lidt/lgdt就是加载idtr/gdtr


! that was painless, now we enable A20
;
; empty_8042会一直忙到到keyboard command queue为空
; 没明白干嘛的，也许后面打开A20地址线需要？
; 8086CPU只有20位地址线，而打开A20地址线即突破地址线20位宽的限制，变成32位

	call	empty_8042
	mov	al,#0xD1		! command write
	out	#0x64,al
	call	empty_8042
	mov	al,#0xDF		! A20 on
	out	#0x60,al
	call	empty_8042


! well, that went ok, I hope. Now we have to reprogram the interrupts :-(
! we put them right after the intel-reserved hardware interrupts, at
! int 0x20-0x2F. There they won't mess up anything. Sadly IBM really
! messed this up with the original PC, and they haven't been able to
! rectify it afterwards. Thus the bios puts interrupts at 0x08-0x0f,
! which is used for the internal hardware interrupts as well. We just
! have to reprogram the 8259's, and it isn't fun.
; 
; Intel和IBM没谈拢，中断号冲突，所以需要对可编程中断控制器9259芯片重新编程

	mov	al,#0x11		! initialization sequence
	out	#0x20,al		! send it to 8259A-1
	.word	0x00eb,0x00eb		! jmp $+2, jmp $+2
	out	#0xA0,al		! and to 8259A-2
	.word	0x00eb,0x00eb
	mov	al,#0x20		! start of hardware int's (0x20)
	out	#0x21,al
	.word	0x00eb,0x00eb
	mov	al,#0x28		! start of hardware int's 2 (0x28)
	out	#0xA1,al
	.word	0x00eb,0x00eb
	mov	al,#0x04		! 8259-1 is master
	out	#0x21,al
	.word	0x00eb,0x00eb
	mov	al,#0x02		! 8259-2 is slave
	out	#0xA1,al
	.word	0x00eb,0x00eb
	mov	al,#0x01		! 8086 mode for both
	out	#0x21,al
	.word	0x00eb,0x00eb
	out	#0xA1,al
	.word	0x00eb,0x00eb
	mov	al,#0xFF		! mask off all interrupts for now
	out	#0x21,al
	.word	0x00eb,0x00eb
	out	#0xA1,al

! well, that certainly wasn't fun :-(. Hopefully it works, and we don't
! need no steenking BIOS anyway (except for the initial loading :-).
! The BIOS-routine wants lots of unnecessary data, and it's less
! "interesting" anyway. This is how REAL programmers do it.
;
; 当了一段时间BIOS的调包侠后，终于不再需要BIOS啦，好耶
; 接下来就要切到实模式啦
!
! Well, now's the time to actually move into protected mode. To make
! things as simple as possible, we do no register set-up or anything,
! we let the gnu-compiled 32-bit programs do that. We just jump to
! absolute address 0x00000, in 32-bit protected mode.

; 前面的一大长串都是为了模式平滑的切换
; 实际上的切换只有两条汇编语句
; 因为决定处于哪种模式由机器状态字寄存器cr0的最后一位PE字段决定
; PE=0 为关闭保护模式，PE=1表示开始保护模式
	mov	ax,#0x0001	! protected mode (PE) bit
	lmsw	ax		! This is it!
	jmpi	0,8		! jmp offset 0 of segment 8 (cs)
; 前面说到，切换到保护模式后，内存寻址方式发生了改变
; 同样是jumpi指令，在实模式下，表示跳转到0x80000处
; 而保护模式下，此处的‘8’不再是基址，而是段选择子
;           _____________________________________________
; 段选择子：| gd index (13bit) | TI (1bit) | PRL (2bit) |
;           ---------------------------------------------
;       8 : 0000'0000'0000'1 0 00 
; 那么就该拿着gdtr找到gdt，找到索引为1的段描述符，从中读出基址
; 从前所述，可以是代码段，且段基址和偏移都为0
; 因而最终得到的地址还是0，故而会跳到内存地址为0处继续执行
; 0地址处是之前挪了个地儿的system啊
; 
; 好耶，终于要进到系统里啦
; 然而，从Makefile里可知，编译system还需要head.s
; 那接下来就该继续读head.s哩
; 
; tools/system:	boot/head.o init/main.o

; 再看一眼目前的内存布局
;            ___________
;            |         |         
;            |   栈    |<- 0x9ff00 (stack top pointer)
;            |   ||    |         
;            |   ||    |         
;            |   \/    |         
;            |         |         
;      ----->|---------|<- 0x90a00
;            |   idt   |                 idtr: point to idt
;     setup  |   gdt   |                 gdtr: point to gdt
;      ----->|---------|<- 0x90200
;            |         |
;      ----->|---------|
;   hardware |         |
;     info   |         | 
;      ----->|---------|<- 0x90000
;            |         |         
;            |         |         
;      ----->|---------|<- 0x80000          
;            |         |        
;    system  |  512KB  |
;            |         |
;      ----->|_________|<- 0x00000
;              memory


! This routine checks that the keyboard command queue is empty
! No timeout is used - if this hangs there is something wrong with
! the machine, and we probably couldn't proceed anyway.
empty_8042:
	.word	0x00eb,0x00eb
	in	al,#0x64	! 8042 status port
	test al,#2		! is input buffer full?
	jnz	empty_8042	! yes - loop
	ret


; 每个段描述符8B，可以看到预设了三个段描述符
; 第一个置空了
; 第二个是代码段，基址和偏移都为0，可读可执行
; 第三个是数据段，基址和偏移都为0，可读可写
gdt:
	.word	0,0,0,0		! dummy

	.word	0x07FF		! 8Mb - limit=2047 (2048*4096=8Mb)
	.word	0x0000		! base address=0
	.word	0x9A00		! code read/exec
	.word	0x00C0		! granularity=4096, 386

	.word	0x07FF		! 8Mb - limit=2047 (2048*4096=8Mb)
	.word	0x0000		! base address=0
	.word	0x9200		! data read/write
	.word	0x00C0		! granularity=4096, 386

idt_48:
	.word	0			! idt limit=0
	.word	0,0			! idt base=0L

gdt_48:
	.word	0x800		! gdt limit=2048, 256 GDT entries
	.word	512+gdt,0x9	! gdt base = 0X9xxxx
; gdtr的结构如下所示，上面给出了limit=2048
; 每个段描述符64bit，即8B，2048/8 = 256，即linux0.11最多支持256个段
;        ________________________________________
; gdtr:  |  base addrress(32bit)  | limit(16bit)|
;        ----------------------------------------
; 而后给出了段描述符的基地址，0x90000 + 512 + gdt
; 0x90000 + 512 = 0x90200，即setup加载到内存中的基址
; 加上gdt标签在setup里的偏移即是gdt的存放位置
; 那么，跳到gdt标签处继续看

	
.text
endtext:
.data
enddata:
.bss
endbss:
