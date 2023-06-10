.global brainfuck

# This is a brainfuck *compiler* (not interpreter), with some basic optimizations
# It works by allocating a memory buffer, emitting instructions in that buffer, ending
# with a return (like a proper function), and we will update the protection mode with the
# mmap and mproect syscalls. Initially, the instruction buffer has PROT_READ | PROT_WRITE
# permissions, but before trying to run it, we should change the protection mode to
# PROT_READ | PROT_EXECUTE.

# The allocation and changing of permissions is handled by the bf_mmap_alloc and
# bf_mmap_change_to_executable functions below.

# It is hyper specific for x86_64.


##########################################################
# Working principles.................................... #


# enum BfInstrKind { // sizeof = 8 bytes
#     Add = 1,
.equ InstrKindADD, 1
#     Shift = 2,
.equ InstrKindSHIFT, 2
#     Read = 3,
.equ InstrKindREAD, 3
#     Write = 4,
.equ InstrKindWRITE, 4
#     BeginLoop = 5,
.equ InstrKindBEGINLOOP, 5
#     EndLoop = 6,
.equ InstrKindENDLOOP, 6
# };
# 
# struct BfInstr { // sizeof = 24 bytes,
#     enum BfInstrKind kind; // 8 bytes (base + 0)
#     long long data; // 8 bytes (base + 8), some data
#                     // for Add = number to add
#                     // for Shift = number of cells to shift by
#                     // for Read / Write = unused (undefined value)
#                     // for BeginLoop = address of EndLoop instruction
#                     // for EndLoop = address of BeginLoop instruction
#     struct BfEffectTable* eff_table; // 8 bytes (base + 16)
#                                      // pointer to effect table
#                                      // or NULL if not available
# };
#
# struct LoopStackElem {
#     struct BfInstr* start;
# };
# 
# struct BfEffectTable { // sizeof = 32 + EFFECT_TABLE_SIZE
#     char eff_table[EFFECT_TABLE_SIZE];
#     long long off_start; // base + EFFECT_TABLE_SIZE
#     long long off_end; // base + EFFECT_TABLE_SIZE + 8
#     long long final_head_offset; // base + EFFECT_TABLE_SIZE + 16
#     BfInstr* after_effect_table; // base + EFFECT_TABLE_SIZE + 24
# };





##########################################################
# Beginning............................................. #

# The emit_* functions below follow the following calling convention:
# - the position of the next instruction in the instruction buffer is always stored in %rbx,
# and as such, callees are free to modify it.

.data

instr_buffer:
.quad 0
instr_buffer_size:
.quad 0

.equ TAPE_SIZE, 0x1000000
.equ EFFECT_TABLE_SIZE, 0x400
.equ EFFECT_TABLE_CENTER, EFFECT_TABLE_SIZE / 2
.equ EFFECT_TABLE_STRUCT_SIZEOF, EFFECT_TABLE_SIZE + 32
.equ EFFECT_TABLES, 0x10000

Tape:
	.space TAPE_SIZE, 0
ProgInstructions:
	.space 0x100000, 0
LoopStack:
	.space 0x10000, 0

.bss

.lcomm EffectTableMemory, EFFECT_TABLES * EFFECT_TABLE_STRUCT_SIZEOF

.text

.macro copysc x, lb
	movq $\x\()_size, %rsi
	movq $0, %rdi
\lb\()loop_start:
	cmpq %rdi, %rsi
	jle \lb\()loop_end
	movb \x(%rdi), %al
	movb %al, (%rbx)

	incq %rdi
	incq %rbx

	jmp \lb\()loop_start
\lb\()loop_end:
.endm

.balign 16

emit_dot_instr:
	movq %r8, %rbx
	movb (%r8), %dil
	movq $bf_rt_putc, %rax # indirect, otherwise assembler generates relative jump and we don't want that
	callq *%rax
	movq %rbx, %r8
emit_dot_instr_end:
.equ emit_dot_instr_size, .-emit_dot_instr

emit_dot_instr_size_v:
	.quad emit_dot_instr_size

emit_dot:
	pushq %rax
	copysc emit_dot_instr, emit_dot_l1
	popq %rax
	ret

.balign 16

emit_comma_instr:
	movq %r8, %rbx
	movq $bf_rt_getc, %rax # indirection, otherwise assembler generates relative jump and we don't want that
	callq *%rax
	movq %rbx, %r8
	movb %al, (%r8)
emit_comma_instr_end:
.equ emit_comma_instr_size, .-emit_comma_instr
emit_comma_instr_size_v:
	.quad emit_comma_instr_size

emit_comma:
	pushq %rax
	copysc emit_comma_instr, emit_comma_l1
	popq %rax
	ret


emit_add_simple:
	# addb   $0xXX, (%r8) = 41 80 00 XX

	// assume Little Endian
	movl $0x00008041, (%rbx)
	// we are actually doing the following, but in a single instruction:
	// then update the last byte (which is the immediate to add).
	// movb $41, (%rbx)
	// movb $80, 1(%rbx)
	// movb $00, 2(%rbx)
	movb %dil, 3(%rbx) # the value to add should be stored 
	addq $4, %rbx
	retq

emit_shift_simple:
	# addq   $0xXXXXXXXX, %r8 = 49 81 c0 XX XX XX XX
	// same as above, assume Little Endian and put the first 3 bytes in a single instruction,
	// then update the last 4 bytes (the immediate to add).
	movl $0x00C08149, (%rbx)
	movl %edi, 3(%rbx)
	addq $7, %rbx
	retq

emit_effect_table_effects:
	movq EFFECT_TABLE_SIZE(%rdi), %r8 # effect start offset
	movq EFFECT_TABLE_SIZE+8(%rdi), %r9 # effect end offset
emit_effect_table_effects_loop_start:
	cmpq %r8, %r9
	jl emit_effect_table_effects_loop_end

	movb EFFECT_TABLE_CENTER(%rdi, %r8), %r10b
	orb %r10b, %r10b
	jz emit_effect_table_effects_loop_next

	# 41 80 40 XX YY = addb $0xYY, 0xXX(%r8)
	movl $0x00408041, (%rbx)
	movb %r8b, 3(%rbx)
	movb %r10b, 4(%rbx)
	addq $5, %rbx

emit_effect_table_effects_loop_next:
	incq %r8
	jmp emit_effect_table_effects_loop_start
emit_effect_table_effects_loop_end:
	movq EFFECT_TABLE_SIZE+16(%rdi), %r8
	orq %r8, %r8
	je emit_effect_table_effects_end

	movq %r8, %rdi
	callq emit_shift_simple

emit_effect_table_effects_end:
	retq

.data

LoopStackPtrs:
	.space 0x10000, 0
	# index = %r15

.text

emit_begin_loop:
	movq %rbx, LoopStackPtrs(,%r15,8)
	incq %r15
	# 41 80 38 00       = cmpb   $0, (%r8)
	# 0F 84 XX XX XX XX = je relative XX XX XX XX
	# address to store jump end = address in LoopStackPtrs + 6
	movl $0x00388041, (%rbx)
	movl $0x0000840F, 4(%rbx)
	addq $10, %rbx
	retq

emit_end_loop:
	pushq %rax
	decq %r15
	# 41 80 38 00       = cmpb   $0, (%r8)
	# 0F 85 XX XX XX XX = jne relative XX XX XX XX
	movl $0x00388041, (%rbx)
	movl $0x0000850F, 4(%rbx)
	addq $10, %rbx
	# address to store jump end = -4(%rbx)

	movq %rbx, %rax
	movq LoopStackPtrs(,%r15,8), %r14
	addq $10, %r14
	subq %r14, %rax
	movl %eax, -4(%r14)

	subq %rbx, %r14
	movl %r14d, -4(%rbx)

	popq %rax
	retq

emit_loop_efftable:
	cmpq $-1, %rdi
	je emit_loop_efftable_clear_loop

	# 48 31 C0 = xorq %rax, %rax
	movl $0x00C03148, (%rbx)
	addq $3, %rbx

	movq $0, %r8
	movb EFFECT_TABLE_CENTER(%rdi), %r8b
	cmpb $-16, %r8b
	jl emit_loop_efftable_coef_div

	cmpb $0, %r8b
	jge emit_loop_efftable_coef_div # technically this should be UB

.data

EmitLoopEfftableCoefJmpTable:
.rept (256-16)
.quad 0
.endr
.quad emit_loop_efftable_coef_m16, emit_loop_efftable_coef_div, emit_loop_efftable_coef_div, emit_loop_efftable_coef_div, emit_loop_efftable_coef_div, emit_loop_efftable_coef_div, emit_loop_efftable_coef_div, emit_loop_efftable_coef_div, emit_loop_efftable_coef_m8, emit_loop_efftable_coef_div, emit_loop_efftable_coef_div, emit_loop_efftable_coef_div, emit_loop_efftable_coef_m4, emit_loop_efftable_coef_div, emit_loop_efftable_coef_m2, emit_loop_efftable_coef_m1

.text

	movq EmitLoopEfftableCoefJmpTable(,%r8,8), %r10
	jmp *%r10

emit_loop_efftable_coef_m1:
	# 41 8A 00 = movb (%r8), %al
	movl $0x00008A41, (%rbx)
	addq $3, %rbx

	jmp emit_loop_efftable_start

emit_loop_efftable_coef_m2:
	# 41 8A 00 = movb (%r8), %al
	# 66 C1 E8 01 = shr $0x01, %ax
	movl $0x66008A41, (%rbx)
	movl $0x0001E8C1, 4(%rbx)
	addq $7, %rbx

	jmp emit_loop_efftable_start

emit_loop_efftable_coef_m4:
	# 41 8A 00 = movb (%r8), %al
	# 66 C1 E8 02 = shr $0x02, %ax
	movl $0x66008A41, (%rbx)
	movl $0x0002E8C1, 4(%rbx)
	addq $7, %rbx

	jmp emit_loop_efftable_start

emit_loop_efftable_coef_m8:
	# 41 8A 00 = movb (%r8), %al
	# 66 C1 E8 03 = shr $0x03, %ax
	movl $0x66008A41, (%rbx)
	movl $0x0003E8C1, 4(%rbx)
	addq $7, %rbx

	jmp emit_loop_efftable_start

emit_loop_efftable_coef_m16:
	# 41 8A 00 = movb (%r8), %al
	# 66 C1 E8 04 = shr $0x04, %ax
	movl $0x66008A41, (%rbx)
	movl $0x0004E8C1, 4(%rbx)
	addq $7, %rbx

	jmp emit_loop_efftable_start

emit_loop_efftable_coef_div:
	# 48 31 D2 = xorq %rdx, %rdx
	# 41 8A 00 = movb (%r8), %al
	# 41 B1 XX = movb $0xXX, %r9b
	# 41 F6 F1 = divb %r9b
	movl $0x41D23148, (%rbx)
	movl $0xB141008A, 4(%rbx)
	movl $0xF1F64100, 8(%rbx) # to store negated 0 effect immediate byte at 8(%rbx)

	negb %r8b # we have to negate it, or we end up with negative results.
	movb %r8b, 8(%rbx)

	addq $12, %rbx

emit_loop_efftable_start:

	# now %al will have the multiplier we need.

	movq EFFECT_TABLE_SIZE(%rdi), %r8 # = start
	movq EFFECT_TABLE_SIZE+8(%rdi), %r9 # = end

emit_loop_efftable_loop_start:
	cmpq %r8, %r9
	jl emit_loop_efftable_end

	movq $0, %r10
	movb EFFECT_TABLE_CENTER(%rdi, %r8), %r10b
	cmpb $0, %r10b
	je emit_loop_efftable_loop_next

.data

EmitLoopEfftableJMPTable:
.quad 0, emit_loop_efftable_loop_1, emit_loop_efftable_loop_2, emit_loop_efftable_loop_3, emit_loop_efftable_loop_4, emit_loop_efftable_loop_5, emit_loop_efftable_loop_normal, emit_loop_efftable_loop_normal, emit_loop_efftable_loop_8, emit_loop_efftable_loop_9
.rept (256-10-9)
.quad emit_loop_efftable_loop_normal
.endr
.quad emit_loop_efftable_loop_m9, emit_loop_efftable_loop_m8, emit_loop_efftable_loop_normal, emit_loop_efftable_loop_normal, emit_loop_efftable_loop_m5, emit_loop_efftable_loop_m4, emit_loop_efftable_loop_m3, emit_loop_efftable_loop_m2, emit_loop_efftable_loop_m1

.text
	movq EmitLoopEfftableJMPTable(,%r10,8), %r11
	jmp *%r11


emit_loop_efftable_loop_9:
	# 4C 8D 0C C0 = leaq (%rax, %rax, 8), %r9
	# 45 00 48 XX = addb %r9b, 0xXX(%r8)
	movl $0xC00C8D4C, (%rbx)
	movl $0x00480045, 4(%rbx)
	movb %r8b, 7(%rbx)
	addq $8, %rbx

	jmp emit_loop_efftable_loop_next

emit_loop_efftable_loop_8:
	# 4C 8D 0C C5 00 00 00 00 = leaq (, %rax, 8), %r9
	# 45 28 00 XX = addb %r9b, 0xXX(%r8)
	movl $0xC50C8D4C, (%rbx)
	movl $0x00000000, 4(%rbx)
	movl $0x00480045, 8(%rbx)
	movb %r8b, 11(%rbx)
	addq $12, %rbx

	jmp emit_loop_efftable_loop_next

emit_loop_efftable_loop_5:
	# 4C 8D 0C 80 = leaq (%rax, %rax, 4), %r9
	# 45 00 48 XX = addb %r9b, 0xXX(%r8)
	movl $0x800C8D4C, (%rbx)
	movl $0x00480045, 4(%rbx)
	movb %r8b, 7(%rbx)
	addq $8, %rbx

	jmp emit_loop_efftable_loop_next

emit_loop_efftable_loop_4:
	# 4C 8D 0C 85 00 00 00 00 = leaq (, %rax, 4), %r9
	# 45 00 48 XX = addb %r9b, 0xXX(%r8)
	movl $0x850C8D4C, (%rbx)
	movl $0x00000000, 4(%rbx)
	movl $0x00480045, 8(%rbx)
	movb %r8b, 11(%rbx)
	addq $12, %rbx

	jmp emit_loop_efftable_loop_next

emit_loop_efftable_loop_3:
	# 4C 8D 0C 40 = leaq (%rax, %rax, 2), %r9
	# 45 00 48 XX = addb %r9b, 0xXX(%r8)
	movl $0x400C8D4C, (%rbx)
	movl $0x00480045, 4(%rbx)
	movb %r8b, 7(%rbx)
	addq $8, %rbx

	jmp emit_loop_efftable_loop_next

emit_loop_efftable_loop_2:
	# 4C 8D 0C 45 00 00 00 00 = leaq (, %rax, 2), %r9
	# 45 00 48 XX = addb %r9b, 0xXX(%r8)
	movl $0x450C8D4C, (%rbx)
	movl $0x00000000, 4(%rbx)
	movl $0x00480045, 8(%rbx)
	movb %r8b, 11(%rbx)
	addq $12, %rbx

	jmp emit_loop_efftable_loop_next

emit_loop_efftable_loop_1:
	# 41 00 40 XX = addb %al, 0xXX(%r8)
	movl $0x00400041, (%rbx)
	movb %r8b, 3(%rbx)
	addq $4, %rbx

	jmp emit_loop_efftable_loop_next

emit_loop_efftable_loop_m1:
	# 41 28 40 XX = subb %al, 0xXX(%r8)
	movl $0x00402841, (%rbx)
	movb %r8b, 3(%rbx)
	addq $4, %rbx

	jmp emit_loop_efftable_loop_next

emit_loop_efftable_loop_m2:
	# 4C 8D 0C 45 00 00 00 00 = leaq (, %rax, 2), %r9
	# 45 28 48 XX = subb %r9b, 0xXX(%r8)
	movl $0x450C8D4C, (%rbx)
	movl $0x00000000, 4(%rbx)
	movl $0x00482845, 8(%rbx)
	movb %r8b, 11(%rbx)
	addq $12, %rbx

	jmp emit_loop_efftable_loop_next

emit_loop_efftable_loop_m3:
	# 4C 8D 0C 40 = leaq (%rax, %rax, 2), %r9
	# 45 28 48 XX = subb %r9b, 0xXX(%r8)
	movl $0x400C8D4C, (%rbx)
	movl $0x00482845, 4(%rbx)
	movb %r8b, 7(%rbx)
	addq $8, %rbx

	jmp emit_loop_efftable_loop_next

emit_loop_efftable_loop_m4:
	# 4C 8D 0C 85 00 00 00 00 = leaq (, %rax, 4), %r9
	# 45 28 48 XX = subb %r9b, 0xXX(%r8)
	movl $0x850C8D4C, (%rbx)
	movl $0x00000000, 4(%rbx)
	movl $0x00482845, 8(%rbx)
	movb %r8b, 11(%rbx)
	addq $12, %rbx

	jmp emit_loop_efftable_loop_next

emit_loop_efftable_loop_m5:
	# 4C 8D 0C 80 = leaq (%rax, %rax, 4), %r9
	# 45 28 48 XX = subb %r9b, 0xXX(%r8)
	movl $0x800C8D4C, (%rbx)
	movl $0x00482845, 4(%rbx)
	movb %r8b, 7(%rbx)
	addq $8, %rbx

	jmp emit_loop_efftable_loop_next

emit_loop_efftable_loop_m8:
	# 4C 8D 0C C5 00 00 00 00 = leaq (, %rax, 8), %r9
	# 45 28 48 XX = subb %r9b, 0xXX(%r8)
	movl $0xC50C8D4C, (%rbx)
	movl $0x00000000, 4(%rbx)
	movl $0x00482845, 8(%rbx)
	movb %r8b, 11(%rbx)
	addq $12, %rbx

	jmp emit_loop_efftable_loop_next

emit_loop_efftable_loop_m9:
	# 4C 8D 0C C0 = leaq (%rax, %rax, 8), %r9
	# 45 28 48 XX = subb %r9b, 0xXX(%r8)
	movl $0xC00C8D4C, (%rbx)
	movl $0x00482845, 4(%rbx)
	movb %r8b, 7(%rbx)
	addq $8, %rbx

	jmp emit_loop_efftable_loop_next

emit_loop_efftable_loop_normal:
	# 66 44 6B C8 XX = imul $0xXX, %ax, %r9w
	# 45 00 48 XX = addb %r9b, 0xXX(%r8)

	movl $0xC86B4466, (%rbx)
	movb %r10b, 4(%rbx)
	movl $0x00480045, 5(%rbx)
	movb %r8b, 8(%rbx)
	addq $9, %rbx

emit_loop_efftable_loop_next:
	incq %r8
	jmp emit_loop_efftable_loop_start

emit_loop_efftable_loop_end:

	jmp emit_loop_efftable_end

emit_loop_efftable_clear_loop:
	# 41 C6 00 00 = movb $0, (%r8)
	movl $0x0000C641, (%rbx)
	addq $4, %rbx

emit_loop_efftable_end:
	retq

emit_begin:
	# 55 = pushq %rbp
	# 48 89 E5 = movq %rsp, %rbp
	movl $0xE5894855, (%rbx)

	# 48 83 EC XX = subq $XX, %rsp
	# ^ XX = 32 bytes, which should be enough
	movl $0x20EC8348, 4(%rbx)

	addq $8, %rbx
	retq

emit_end:
	# 48 89 EC = movq %rbp, %rsp
	# 5D = popq %rbp
	movq $0x5DEC8948, (%rbx)
	# C3 = retq
	movb $0xC3, 4(%rbx)
	addq $5, %rbx
	retq


# This is the entry point. It receives one argument: a pointer to a null-terminated string (in %rdi).
# It's job is to execute the brainfuck code contained in the string and then return the control to the caller.
brainfuck:
.cfi_startproc
	pushq %rbp
	movq %rsp, %rbp
	subq $32, %rsp

	movq %rdi, -8(%rbp)
	movq %rbx, -24(%rbp)

	# =======================================
	# =======================================
	# === Input and 1st optimization pass ===
	# =======================================
	# =======================================

	# During the 1st optimization pass, chains of + and -,
	# respectively of > and < are merged into a single instruction.


	# %rax = i (loop counter)
	# %rcx = loop stack pointer
	# %rdx = prog instruction (increment by `sizeof(BfInstr)` instead of 1)
	# as such, the current instruction is always at `(%rdx)`
	# %r8 = temp register
	# %r9 = temp register

	movq $0, %rax
	movq $0, %rbx
	movq $0, %rcx
	movq $ProgInstructions, %rdx
	movq $0, %r8
	
brainfuck_src_loop:
	movb (%rdi, %rax), %r8b # input char
	orq %r8, %r8 # check if 0
	jz brainfuck_src_end # if so, jump to end

.data

SRC_JMP_TABLE:
	# most of them are just ignored, therefore entries pointing to the brainfuck_src_next label.
	# the ones that aren't are for the ascii codes of the meaningful brainfuck characters: +-<>[].,
	.quad brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_plus, brainfuck_src_comma, brainfuck_src_minus, brainfuck_src_dot, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_lessthan, brainfuck_src_next, brainfuck_src_greaterthan, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_openbracket, brainfuck_src_next, brainfuck_src_closedbracket, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next, brainfuck_src_next

.text

	movq SRC_JMP_TABLE(,%r8,8), %r9 # read into the jump table
	jmp *%r9





brainfuck_src_plus:
	cmpq $ProgInstructions, %rdx # if there are no previous instructions, we have to add a new instruction
	je brainfuck_src_plus_new
	# check if the previous instruction was a + or -
	cmpq $InstrKindADD, -24(%rdx)
	jne brainfuck_src_plus_new # if not, we have to make a new instruction

	# otherwise apply +- chain optimization
	incq -16(%rdx) # ++ previous instruction data

	jmp brainfuck_src_next # go to next character

brainfuck_src_plus_new:
	# add a new Add instruction
	movq $InstrKindADD, (%rdx)
	movq $1, 8(%rdx)
	movq $0, 16(%rdx)

	addq $24, %rdx

	jmp brainfuck_src_next








brainfuck_src_minus:
	cmpq $ProgInstructions, %rdx # if there are no previous instructions, we have to add a new instruction
	je brainfuck_src_minus_new
	# check if the previous instruction was a + or -
	cmpq $InstrKindADD, -24(%rdx)
	jne brainfuck_src_minus_new # if not, we have to make a new instruction

	# otherwise apply +- chain optimization
	decq -16(%rdx) # -- previous instruction data

	jmp brainfuck_src_next # go to next character

brainfuck_src_minus_new:
	# add a new Add instruction
	movq $InstrKindADD, (%rdx)
	movq $-1, 8(%rdx)
	movq $0, 16(%rdx)

	addq $24, %rdx

	jmp brainfuck_src_next










brainfuck_src_lessthan:
	cmpq $ProgInstructions, %rdx # if there are no previous instructions, we have to add a new instruction
	je brainfuck_src_lessthan_new
	# check if the previous instruction was a > or <
	cmpq $InstrKindSHIFT, -24(%rdx)
	jne brainfuck_src_lessthan_new # if not, we have to make a new instruction

	# otherwise apply >< chain optimization
	subq $1, -16(%rdx) # -- previous instruction data

	jmp brainfuck_src_next # go to next character

brainfuck_src_lessthan_new:
	# add a new Shift instruction
	movq $InstrKindSHIFT, (%rdx)
	movq $-1, 8(%rdx)
	movq $0, 16(%rdx)

	addq $24, %rdx

	jmp brainfuck_src_next












brainfuck_src_greaterthan:
	cmpq $ProgInstructions, %rdx # if there are no previous instructions, we have to add a new instruction
	je brainfuck_src_greaterthan_new
	# check if the previous instruction was a > or <
	cmpq $InstrKindSHIFT, -24(%rdx)
	jne brainfuck_src_greaterthan_new # if not, we have to make a new instruction

	# otherwise apply >< chain optimization
	addq $1, -16(%rdx) # ++ previous instruction data

	jmp brainfuck_src_next # go to next character

brainfuck_src_greaterthan_new:
	# add a new Shift instruction
	movq $InstrKindSHIFT, (%rdx)
	movq $1, 8(%rdx)
	movq $0, 16(%rdx)

	addq $24, %rdx

	jmp brainfuck_src_next












brainfuck_src_comma:
	# add a new Read instruction
	movq $InstrKindREAD, (%rdx)
	// movq $0, 8(%rdx) // optimized away unused value
	// movq $0, 16(%rdx) // optimized away unused value

	addq $24, %rdx

	jmp brainfuck_src_next









brainfuck_src_dot:
	# add a new Write instruction
	movq $InstrKindWRITE, (%rdx)
	// movq $0, 8(%rdx) // optimized away unused value
	// movq $0, 16(%rdx) // optimized away unused value

	addq $24, %rdx

	jmp brainfuck_src_next






brainfuck_src_openbracket:
	# Store current instruction pointer in the LoopStack
	movq %rdx, LoopStack(%rcx)
	addq $8, %rcx

	# Add a BeginLoop instruction
	movq $InstrKindBEGINLOOP, (%rdx)
	// movq $0, 8(%rdx) // optimized away unused value (should get overwritten by when parsing corresponding ])
	movq $0, 16(%rdx)
	addq $24, %rdx

	jmp brainfuck_src_next
brainfuck_src_closedbracket:
	# Pop the address of the BeginLoop at the top of the LoopStack, and
	# store the address of the EndLoop instruction to the data of the BeginLoop instruction
	subq $8, %rcx
	movq LoopStack(%rcx), %r9
	movq %rdx, 8(%r9)

	# Add the EndLoop instruction
	movq $InstrKindENDLOOP, (%rdx)
	movq %r9, 8(%rdx)
	// movq $0, 16(%rdx) // optimized away unused value
	addq $24, %rdx

	jmp brainfuck_src_next

brainfuck_src_next:
	incq %rax
	jmp brainfuck_src_loop

brainfuck_src_end:

	# ==============================================
	# ==============================================
	# === End of input and 1st optimization pass ===
	# ==============================================
	# ==============================================

	// jmp bf_2nd_opt_pass_skip

	# ======================================
	# ======================================
	# === Begin of 2nd optimization pass ===
	# ======================================
	# ======================================

	# Here we look at adjacent chains of +/- and </>, and try to
	# build "effect tables" from them
	# 
	# For example:
	# +>--<->++<
	# Has an effect of 0, but we don't know that yet.
	# The pupose of this optimization pass is to compile
	# those consecutive chains of +/- and >/< into an "effect
	# table", which would tell us exactly what is going on,
	# but in a form that is a lot easier to compile too (and will
	# help us to optimize what I like to call "basic loops," see the
	# 3rd optimization pass below).
	# 
	# Let's take another example:
	# >-<++>+
	# This has the following effects on the tape:
	# - The value where the head started is incremented by 2.
	# - The value immediately to the right does not change.
	# - The head moves one cell to the right.

	# %rax = current IR instruction
	# %rbx = start instruction of current effect loop
	# %rdx = pointer beyond the last IR instruction
	# %rcx = address of effect table
	# %r8 = temp register
	# %r9 = temp register
	# %r10 = current effect table offset

	movq $ProgInstructions, %rax
	movq $EffectTableMemory, %rcx
bf_eff_loop_start:
	leaq 24(%rax), %r8 # we need at least 2 IR instructions for any meaningful effects
	cmpq %rdx, %r8
	jge bf_eff_loop_end

.data

EffBuildJumpTable:
	.quad 0, bf_effbuild_start, bf_effbuild_start, bf_effbuild_fail, bf_effbuild_fail, bf_effbuild_fail, bf_effbuild_fail

.text
	movq (%rax), %r8
	movq EffBuildJumpTable(,%r8,8), %r8
	jmpq *%r8

bf_effbuild_start:
	#; if we got here, that means we started with either +/- or </>.
	# now we have to check that the next instruction is also +/- or </>.
	cmpq $InstrKindSHIFT, 24(%rax)
	jg bf_effbuild_fail_1

	movq $0, %r10
	movq %rax, %rbx

	bf_effbuild_inner_start:
		cmpq %rdx, %rax
		jge bf_effbuild_inner_end

	.data

	EffBuildInnerJumpTable:
		.quad 0, bf_effbuild_inner_add, bf_effbuild_inner_shift, bf_effbuild_inner_end, bf_effbuild_inner_end, bf_effbuild_inner_end, bf_effbuild_inner_end

	.text
		movq (%rax), %r8
		movq EffBuildInnerJumpTable(,%r8,8), %r8
		jmpq *%r8

		bf_effbuild_inner_add:
			# check if smaller than start
			cmpq %r10, EFFECT_TABLE_SIZE(%rcx)
			jle bf_effbuild_inner_add_bigger_than_start
			movq %r10, EFFECT_TABLE_SIZE(%rcx)

		bf_effbuild_inner_add_bigger_than_start:

			# check if greater than end
			cmpq %r10, EFFECT_TABLE_SIZE+8(%rcx)
			jge bf_effbuild_inner_add_smaller_than_end
			movq %r10, EFFECT_TABLE_SIZE+8(%rcx)
		bf_effbuild_inner_add_smaller_than_end:

			# add the effect to the table
			movq 8(%rax), %r8
			addb %r8b, EFFECT_TABLE_CENTER(%rcx, %r10)
			jmp bf_effbuild_inner_nextinstr

		bf_effbuild_inner_shift:
			movq 8(%rax), %r8
			addq %r8, %r10

		bf_effbuild_inner_nextinstr:
			addq $24, %rax
			jmp bf_effbuild_inner_start

	bf_effbuild_inner_end:
		movq %rcx, 16(%rbx)
		movq %r10, EFFECT_TABLE_SIZE+16(%rcx)
		movq %rax, EFFECT_TABLE_SIZE+24(%rcx)
		addq $EFFECT_TABLE_STRUCT_SIZEOF, %rcx
		jmp bf_effbuild_next

#bf_effbuild_fail_3:
#	addq $24, %rax
#bf_effbuild_fail_2:
#	addq $24, %rax
bf_effbuild_fail_1:
	addq $24, %rax
bf_effbuild_fail:
bf_effbuild_next:
	addq $24, %rax
	jmp bf_eff_loop_start
bf_eff_loop_end:

bf_2nd_opt_pass_skip:

	# ====================================
	# ====================================
	# === End of 2nd optimization pass ===
	# ====================================
	# ====================================

	// jmp bf_3rd_opt_pass_skip

	# =============================
	# =============================
	# === 3rd optimization pass ===
	# =============================
	# =============================

	# Here, we look particularly at loops.
	# If there is a loop where we have the following:
	# 1. Only consists of +, -, >, <
	# 2. It is balanced wrt. < and >es, that is, the head ends up
	# in the same position after we execute the body of the loop.
	# Then we can call the loop "basic".
	# Examples of such loops are:
	# [-] (the "clear loop"): symbolized by a `-1` for the effect table pointer.
	# [->>>+<<<] (adds the value of the current cell to the cell 3 positions to the right)
	# [-->+<] (adds half the value of the current cell to the next cell)

	# We are going to impose a few extra restrictions on such loops,
	# for them to make sense:
	# 1. The value in the effect table for the cell 0 must be negative (otherwise it ends up being an infinite loop and / or UB).
	# 2. The value of the cell where the head starts during the loop must be a multiple of the value mentioned at 1 (otherwise also UB).
	
	# Then we can know ahead of time how many times the loop will run:
	# We take the effect for position 0 from the effect table, negate it, and
	# then divide the value on the tape by that value.
	# We then apply the effect table to all the cells exactly that many times, by a simple multiplication instead of looping.
	# Therefore we get the time complexity from O(n) to O(1) for such loops.

	# We will store the effect table pointer on the Begin loop IR instruction.
	#
	# Special values for it are:
	# - if it's the all-common clear loop (`[-]`) we will store -1.
	#
	# Important: This should only be done if the first instruction of the loop has an effect table
	# that covers the entire body of the loop; that is, the next instruction associated with the effect
	# table is the `]` of the loop.

	# %rax = pointer to current IR instruction
	# %rdx = pointer beyond the last IR instruction
	# %r8  = temp register
	# %r9  = temp register
	movq $ProgInstructions, %rax

bf_eff_looplink_loop_start:
 	cmpq %rdx, %rax
	jge bf_eff_looplink_loop_end

	cmpq $InstrKindBEGINLOOP, (%rax)
	jne bf_eff_looplink_loop_next

	movq 24+16(%rax), %r8 # %r8 = pointer to effect table from first instruction
	cmpq $0, %r8
	je bf_eff_looplink_efftable_0

	movq EFFECT_TABLE_SIZE+24(%r8), %r9 # next instruction after effect loop.
	cmpq %r9, 8(%rax) # compare to pointer to ]
	jne bf_eff_looplink_loop_next

	cmpq $0, EFFECT_TABLE_SIZE+16(%r8) # check that the head offset after the loop body is 0
									# (that is, the loop has the same amount of > and < instructions)
	jne bf_eff_looplink_loop_next

	movq %r8, 16(%rax) # then finally link the effect table to the BEGINLOOP instruction
	movq 8(%rax), %rax # skip straight to the end of the loop
	jmp bf_eff_looplink_loop_next # will continue to the next instruction after the loop.

bf_eff_looplink_efftable_0:
	# special case the clear loop, it shouldn't have an effect table attached.
	cmpq $InstrKindADD, 24(%rax)
	jne bf_eff_looplink_loop_next

	cmpq $-1, 24+8(%rax)
	jne bf_eff_looplink_loop_next

	cmpq $InstrKindENDLOOP, 24+24(%rax)
	jne bf_eff_looplink_loop_next

	# we have a clear loop now.
	movq $-1, 16(%rax)
	movq 8(%rax), %rax # jump to end of loop
	# jmp bf_eff_looplink_loop_next # nop

bf_eff_looplink_loop_next:
	addq $24, %rax
	jmp bf_eff_looplink_loop_start
bf_eff_looplink_loop_end:

bf_3rd_opt_pass_skip:

	# ====================================
	# ====================================
	# === End of 3rd optimization pass ===
	# ====================================
	# ====================================

	# ===============================================================
	# ===============================================================
	# ====== Actually emitting the machine code from the IR =========
	# ===============================================================
	# ===============================================================

	movq %rdx, -16(%rbp)

	movq %rdx, %rax
	subq $ProgInstructions, %rax
	movq $256, %rbx
	mulq %rbx

	movq %rax, instr_buffer_size
	movq %rax, %rdi
	callq bf_mmap_alloc
	movq %rax, instr_buffer
	movq %rax, %rbx

	movq -16(%rbp), %rdx

	callq emit_begin

	# %rax = pointer to current IR instruction
	# %rdx = pointer beyond the last IR instruction
	# %rbx = current instruction pointer
	# %r8  = temp register
	# %r9  = temp register
	# %r10 = temp register
	# %r11 = temp register
	# %r15 = LoopStackPtrs index register

	movq $ProgInstructions, %rax
	movq $0, %r15

bf_emit_loop:
 	cmpq %rdx, %rax
	jge bf_emit_loop_end

.data

EmitJumpTable:
	.quad 0, bf_emit_loop_add, bf_emit_loop_shift, bf_emit_loop_read, bf_emit_loop_write, bf_emit_loop_beginloop, bf_emit_loop_endloop

.text

	movq (%rax), %r11
	movq EmitJumpTable(,%r11,8), %r11
	jmp *%r11

bf_emit_loop_add:
	movq 16(%rax), %rdi
	orq %rdi, %rdi
	jz bf_emit_loop_add_simple

	movq EFFECT_TABLE_SIZE+24(%rdi), %rax
	callq emit_effect_table_effects
	jmp bf_emit_loop

bf_emit_loop_add_simple:
	movq 8(%rax), %rdi
	callq emit_add_simple
	jmp bf_emit_loop_next

bf_emit_loop_shift:
	movq 16(%rax), %rdi
	orq %rdi, %rdi
	jz bf_emit_loop_shift_simple

	movq EFFECT_TABLE_SIZE+24(%rdi), %rax
	callq emit_effect_table_effects
	jmp bf_emit_loop

bf_emit_loop_shift_simple:
	movq 8(%rax), %rdi
	callq emit_shift_simple
	jmp bf_emit_loop_next

bf_emit_loop_read:
	callq emit_comma
	jmp bf_emit_loop_next

bf_emit_loop_write:
	callq emit_dot
	jmp bf_emit_loop_next

bf_emit_loop_beginloop:
	movq 16(%rax), %rdi
	cmpq $0, %rdi
	je bf_emit_loop_beginloop_no_efftable

	callq emit_loop_efftable
	movq 8(%rax), %rax
	jmp bf_emit_loop_next

bf_emit_loop_beginloop_no_efftable:
	callq emit_begin_loop
	jmp bf_emit_loop_next

bf_emit_loop_endloop:
	callq emit_end_loop
	# jmp bf_emit_loop_next # nop

bf_emit_loop_next:
	addq $24, %rax
	jmp bf_emit_loop

bf_emit_loop_end:

	callq emit_end

	# =============================================================
	# =============================================================
	# ====== End of emitting the machine code from the IR =========
	# =============================================================
	# =============================================================


	# Change permissions for the instruction buffer to PROT_READ | PROT_EXEC
	movq instr_buffer, %rdi
	movq instr_buffer_size, %rsi
	callq bf_mmap_change_to_executable

	# Setup the runtime 
	movq $Tape, %r8

	# And run
	movq instr_buffer, %rax
	callq *%rax

	# Now here it should be over, we just have to:

	# Flush
	call bf_rt_flush

	# Clean up
	movq -24(%rbp), %rbx
	movq %rbp, %rsp
	popq %rbp
	ret
.cfi_endproc


#############################################################
# IO functions ............................................ #

# Those are runtime functions that are referenced
# from the emitted instructions.

# We are buffering I/O, and flushing at the end, to make sure
# that no characters are left in the buffer.

# We are also using the read and write syscalls directly,
# rather than going through libc code, as that can slow us
# down a bit.

.equ WBufferSize, 0x8000
.equ RBufferSize, 0x8000


.data

WBuffer:
	.space WBufferSize, 0
WBufferCursor:
	.quad 0

RBuffer:
	.space RBufferSize, 0
RBufferBytes:
	.quad 0
RBufferCursor:
	.quad 0

.text
bf_rt_flush:
	movq WBufferCursor, %rdx # WBufferCursor bytes
	orq %rdx, %rdx
	jz bf_rt_flush_end

	movq $1, %rax # __NR_write
	movq $1, %rdi # stdout
	movq $WBuffer, %rsi # WBuffer
	syscall

	movq $0, WBufferCursor

bf_rt_flush_end:
	retq

bf_rt_getc:
	movq RBufferCursor, %r8
	cmpq %r8, RBufferBytes
	je bf_rt_getc_read

	incq RBufferCursor
	movb RBuffer(%r8), %al
	retq

bf_rt_getc_read:
	call bf_rt_flush

	movq $0, %rax # __NR_read
	movq $0, %rdi # stdin
	movq $RBuffer, %rsi # to RBuffer
	movq $RBufferSize, %rdx # RBufferSize bytes

	syscall

	movq %rax, RBufferBytes
	movq $1, RBufferCursor
	movb RBuffer, %al

	retq

bf_rt_putc:
	movq WBufferCursor, %r8
	cmpq $WBufferSize, %r8

	jne bf_rt_putc_continue
	# full buffer
	pushq %rdi
	callq bf_rt_flush
	popq %rdi
	movq $0, %r8
bf_rt_putc_continue:

	movb %dil, WBuffer(%r8)
	incq WBufferCursor

	retq

#####################################################
# Functions to handle virtual memeory               #

# Those functions are here to allocate readable and
# writable memory for the executable code, before we actually write it,
# and then after we wrote it we have to change the permissions to
# readable and executable.
# Having both write and execute permissions at the same time is
# a big no no for security reasons.

.equ PROT_READ, 0x1
.equ PROT_WRITE, 0x2
.equ PROT_EXEC, 0x4

.equ MAP_PRIVATE, 0x2
.equ MAP_ANONYMOUS, 0x20

bf_mmap_alloc:
	movq %rdi, %rsi # size
	movq $-1, %r8 # fd
	movq $9, %rax # __NR_mmap
	movq $0, %rdi # operating system will choose mapping destination
	movq $(PROT_READ | PROT_WRITE), %rdx # new memory region will be marked read and write
	movq $(MAP_PRIVATE | MAP_ANONYMOUS), %r10 # pages will not be shared
                      
	movq $0, %r9
	syscall # now rax will point to mapped location
	retq

bf_mmap_change_to_executable:
	# rdi and rsi already contain the addr and size
	movq $(PROT_READ | PROT_EXEC), %rdx
	movq $10, %rax # __NR_mprotect
	syscall
	retq
