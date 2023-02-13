/*
 *  linux/boot/head.s
 *
 *  (C) 1991  Linus Torvalds
 */

/*
 *  head.s contains the 32-bit startup code.
 *
 * NOTE!!! Startup happens at absolute address 0x00000000, which is also where
 * the page directory will exist. The startup code will be overwritten by
 * the page directory.
 */
 ; 这里的意思是之后设置分页机制的时候，页目录会放置在_pg_dir这个标号处

.text
.globl _idt,_gdt,_pg_dir,_tmp_floppy_area


; 码风突变，前面一直是intel，这里变at&t了
; 啥子情况

_pg_dir:
startup_32:
	movl $0x10,%eax
	mov %ax,%ds
	mov %ax,%es
	mov %ax,%fs             ; 将ds、es、fs、gs这几个段寄存器设置为0x10
	mov %ax,%gs             ; 从前所述，指向gdt中的第2个段描述符，即数据段
	lss _stack_start,%esp   ; lss指令ss:esp指向_stack_start这个标号（ss:esp原先为0x9ff00
                            ; stack_start定义于kernel/sched.c里，是一个大数组

	call setup_idt          ; idt里都用ignore_int初始化
	call setup_gdt          ; gdt其实和之前的设置是相同的
    ; 至于为啥要重新设置idt和gdt，是因为之前存放在了setup区域
    ; 而这块内存区后面可能会被缓冲区覆盖掉

; 目前的内存布局如下所示：
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
;    system  |  512KB  |          idtr: point to idt
;            |   idt   |          gdtr: point to gdt
;            |   gdt   |
;      ----->|_________|<- 0x00000
;              memory


	movl $0x10,%eax		    # reload all the segment registers
	mov %ax,%ds		        # after changing gdt. CS was already
	mov %ax,%es		        # reloaded in 'setup_gdt'
	mov %ax,%fs
	mov %ax,%gs             ; 段寄存器现在存放的其实是段选择子
	lss _stack_start,%esp   ; 段选择子高度依赖gdt，因而gdt修改之后也需要重新设置段寄存器

	xorl %eax,%eax
1:	incl %eax		        # check that A20 really IS enabled
	movl %eax,0x000000	    # loop forever if it isn't
	cmpl %eax,0x100000
	je 1b

/*
 * NOTE! 486 should set bit 16, to check for write-protect in supervisor
 * mode. Then it would be unnecessary with the "verify_area()"-calls.
 * 486 users probably want to set the NE (#5) bit also, so as to use
 * int 16 for math errors.
 */
	movl %cr0,%eax		    # check math chip
	andl $0x80000011,%eax	# Save PG,PE,ET
/* "orl $0x10020,%eax" here for 486 might be good */
	orl $2,%eax		        # set MP
	movl %eax,%cr0
	call check_x87

    ; 做了一些检测工作之后，开启分页机制并跳转到main函数执行
	jmp after_page_tables


/*
 * We depend on ET to be correct. This checks for 287/387.
 */
check_x87:
	fninit
	fstsw %ax
	cmpb $0,%al
	je 1f			         /* no coprocessor: have to set bits */
	movl %cr0,%eax
	xorl $6,%eax		     /* reset MP, set EM */
	movl %eax,%cr0
	ret
.align 2
1:	.byte 0xDB,0xE4		     /* fsetpm for 287, ignored by 387 */
	ret

/*
 *  setup_idt
 *
 *  sets up a idt with 256 entries pointing to
 *  ignore_int, interrupt gates. It then loads
 *  idt. Everything that wants to install itself
 *  in the idt-table may do so themselves. Interrupts
 *  are enabled elsewhere, when we can be relatively
 *  sure everything is ok. This routine will be over-
 *  written by the page tables.
 */

; setup_idt只是给256个中断描述符都指向了一个默认的中断服务例程ignore_int
; 也就是目前中断毫无用处，后面添加中断服务例程时仍旧需要改动idt
setup_idt:
	lea ignore_int,%edx
	movl $0x00080000,%eax
	movw %dx,%ax		    /* selector = 0x0008 = cs */
	movw $0x8E00,%dx	    /* interrupt gate - dpl=0, present */

	lea _idt,%edi
	mov $256,%ecx           ; 同样是支持最多256个中断描述符
rp_sidt:
	movl %eax,(%edi)
	movl %edx,4(%edi)
	addl $8,%edi
	dec %ecx
	jne rp_sidt
	lidt idt_descr
	ret

/*
 *  setup_gdt
 *
 *  This routines sets up a new gdt and loads it.
 *  Only two entries are currently built, the same
 *  ones that were built in init.s. The routine
 *  is VERY complicated at two whole lines, so this
 *  rather long comment is certainly needed :-).
 *  This routine will beoverwritten by the page tables.
 */
setup_gdt:
	lgdt gdt_descr
	ret

/*
 * I put the kernel page tables right after the page directory,
 * using 4 of them to span 16 Mb of physical memory. People with
 * more than 16MB will have to expand this.
 */
.org 0x1000
pg0:

.org 0x2000
pg1:

.org 0x3000
pg2:

.org 0x4000
pg3:

.org 0x5000
/*
 * tmp_floppy_area is used by the floppy-driver when DMA cannot
 * reach to a buffer-block. It needs to be aligned, so that it isn't
 * on a 64kB border.
 */
_tmp_floppy_area:
	.fill 1024,1,0


; 妙啊，跳到main居然是push stack+ret实现的
after_page_tables:
	pushl $0		    # These are the parameters to main :-)
	pushl $0            ; main居然有三个参数么？奇怪
	pushl $0            ; 这里是不是和下一行的ret addr写反了啊，这里是eip之类?
                        ; 虽然eip也不是必须的，比如near call和near ret的组合就不需要改变段基址eip
                        ; 那就是说main真是三个参数喽
	pushl $L6		    # return address for main, if it decides to.
	pushl $_main
	jmp setup_paging    ; 先跳去把分页开了，然后ret的方式call main函数
L6:
	jmp L6			    # main should never return here, but
				        # just in case, we know what happens.
                        ; L6做为main的返回地址
                        ; 对于操作系统来说，不会返回的，
                        ; 因而返回到这里设置成了死循环，方便排错


; 好耶，终于可以进去main不用再看汇编了


/* This is the default interrupt "handler" :-) */
int_msg:
	.asciz "Unknown interrupt\n\r"
.align 2
ignore_int:
	pushl %eax
	pushl %ecx
	pushl %edx
	push %ds
	push %es
	push %fs
	movl $0x10,%eax
	mov %ax,%ds
	mov %ax,%es
	mov %ax,%fs
	pushl $int_msg
	call _printk
	popl %eax
	pop %fs
	pop %es
	pop %ds
	popl %edx
	popl %ecx
	popl %eax
	iret


/*
 * Setup_paging
 *
 * This routine sets up paging by setting the page bit
 * in cr0. The page tables are set up, identity-mapping
 * the first 16MB. The pager assumes that no illegal
 * addresses are produced (ie >4Mb on a 4Mb machine).
 *
 * NOTE! Although all physical memory should be identity
 * mapped by this routine, only the kernel page functions
 * use the >1Mb addresses directly. All "normal" functions
 * use just the lower 1Mb, or the local data space, which
 * will be mapped to some other place - mm keeps track of
 * that.
 *
 * For those with more memory than 16 Mb - tough luck. I've
 * not got it, why should you :-) The source is here. Change
 * it. (Seriously - it shouldn't be too difficult. Mostly
 * change some constants etc. I left it at 16Mb, as my machine
 * even cannot be extended past that (ok, but it was cheap :-)
 * I've tried to show which constants to change by having
 * some kind of marker at them (search for "16Mb"), but I
 * won't guarantee that's all :-( )
 */

 ; linus写这部分代码的时候，认为内存不会超过16MB
 ; 页目录项目/页表项大小都是4B
 ; 当设置页大小为4KB，且采用二级页表的时候
 ; 4KB/4B = 1k = 1024
 ; 即一个页表可以索引1K个页，也就是4KB * 1K = 4MB
 ; 那么16MB的内存需要4个页表
 ; 同样的最多支持1024个页表（根页表只能存放在同一页）
 ; 那么4个页表自然不在话下
 ;
 ; 因而最终通过1个页目录表+4个页表实现了对16MB内存的寻址
 ; 也就是开启分页机制用掉了5个页

.align 2
setup_paging:
	movl $1024*5,%ecx		    /* 5 pages - pg_dir+4 page tables */
	xorl %eax,%eax
	xorl %edi,%edi			    /* pg_dir is at 0x000 */
	cld;rep;stosl
	movl $pg0+7,_pg_dir		    /* set present bit/user r/w */
	movl $pg1+7,_pg_dir+4		/*  --------- " " --------- */
	movl $pg2+7,_pg_dir+8		/*  --------- " " --------- */
	movl $pg3+7,_pg_dir+12		/*  --------- " " --------- */
	movl $pg3+4092,%edi
	movl $0xfff007,%eax		    /*  16Mb - 4096 + 7 (r/w user,p) */
	std
1:	stosl		            	/* fill pages backwards - more efficient :-) */
	subl $0x1000,%eax
	jge 1b

    ; 设置了页目录表和页表后，将保存页目录表物理地址保存到cr3寄存器里以告知操作系统
	xorl %eax,%eax		        /* pg_dir is at 0x0000 */
	movl %eax,%cr3		        /* cr3 - page directory start */

    ; 前面为开启分页做了相应的配置，现在来开启分页
    ; 和开启分段类似，设置机器状态寄存器cr0第一位为1即开启分页机制
    ; 当然，为了不该表cr0其他位
    ; 需要先拿到cr0当前的值，然后通过位运算将第一位置1
	movl %cr0,%eax
	orl $0x80000000,%eax
	movl %eax,%cr0		        /* set paging (PG) bit */

	ret			                /* this also flushes prefetch-queue */
    ; ret会将栈顶元素值当作返回地址
    ; 而此时栈顶是main的地址，因而push+ret就实现了跳转到main

; 目前的内存布局如下所示：
;            ____________
;            |          |
;            |    栈    |<- 0x9ff00 (stack top pointer)
;            |    ||    |
;            |    ||    |
;            |    \/    |
;            |          |
;      ----->|----------|<- 0x90a00
;            |          |
;     setup  |          |
;      ----->|----------|<- 0x90200
;            |          |
;      ----->|----------|
;   hardware |          |
;     info   |          |
;      ----->|----------|<- 0x90000
;            |          |
;            |          |
;      ----->|----------|<- 0x80000
;            |          |
;    system  |   512KB  |              idtr: point to idt
;            |    idt   |              gdtr: point to gdt
;            |    gdt   |              cr3: piint to PDT (address of PDT = 0x00000)
;            |          |
;            |          |
;            |   PDE 3  |
;            |   PDE 2  |
;            |   PDE 1  |              PDE: Page Directory Entry
;            |   PDE 0  |              PDE: Page Directory Pointer Table Entry
;            |    PDT   |
;      ----->|----------|<- 0x00000
;              memory
; 页目录表和页表使用的5个页从0x00000处开始
; 因为此处的代码已经执行完毕，因而将页目录表/页表写到这里是合适的
; 不得不说，16MB内存的时代，为了节省内存真得来点儿奇技淫巧


; 开启分页之后，逻辑地址到物理地址的变换规则又发生了变化
; 在之前开启保护模式之前的配置中，配置了分段机制
; 此时逻辑地址通过段选择子到gdt里查段描述符得到基址后加上偏移得到的线性地址就是物理地址
; 而开启分页机制后，线性地址还需要进一步转换才能得到物理地址
; 即分段：逻辑地址 -> 线性地址（物理地址）
; 而分页：逻辑地址 -> 线性地址（虚拟地址）-> 物理地址

.align 2
.word 0
idt_descr:
	.word 256*8-1		        # idt contains 256 entries
	.long _idt
.align 2
.word 0
gdt_descr:
	.word 256*8-1		        # so does gdt (not that that's any
	.long _gdt		            # magic number, but it works for me :^)

	.align 3
_idt:
    .fill 256,8,0		        # idt is uninitialized

_gdt:
    .quad 0x0000000000000000	/* NULL descriptor */
	.quad 0x00c09a0000000fff	/* 16Mb */
	.quad 0x00c0920000000fff	/* 16Mb */
	.quad 0x0000000000000000	/* TEMPORARY - don't use */
	.fill 252,8,0			    /* space for LDT's and TSS's etc */
