    .text
    .globl main
    # ToyC Compiler Generated Code
    # 遵循RISC-V ABI规范
factorial:
    # Function: factorial
    addi sp, sp, -16
    sw ra, 12(sp)
    sw s0, 8(sp)
    sw s1, 4(sp)
    addi s0, sp, 16
    sw a0, -12(s0)
    lw t0, -12(s0)
    li t1, 1
    slt t2, t1, t0
    xori t2, t2, 1
    beq t2, zero, factorial_else0
    li t0, 1
    mv a0, t0
    lw ra, 12(sp)
    lw s0, 8(sp)
    lw s1, 4(sp)
    addi sp, sp, 16
    ret
    j factorial_endif1
factorial_else0:
factorial_endif1:
    lw t0, -12(s0)
    addi sp, sp, -76
    sw t0, 0(sp)
    sw t1, 4(sp)
    sw t2, 8(sp)
    sw t3, 12(sp)
    sw t4, 16(sp)
    sw t5, 20(sp)
    sw t6, 24(sp)
    sw s0, 28(sp)
    sw s1, 32(sp)
    sw s2, 36(sp)
    sw s3, 40(sp)
    sw s4, 44(sp)
    sw s5, 48(sp)
    sw s6, 52(sp)
    sw s7, 56(sp)
    sw s8, 60(sp)
    sw s9, 64(sp)
    sw s10, 68(sp)
    sw s11, 72(sp)
    lw t1, -12(s0)
    li t2, 1
    sub t3, t1, t2
    mv a0, t3
    jal ra, factorial
    lw t0, 0(sp)
    lw t1, 4(sp)
    lw t2, 8(sp)
    lw t3, 12(sp)
    lw t4, 16(sp)
    lw t5, 20(sp)
    lw t6, 24(sp)
    lw s0, 28(sp)
    lw s1, 32(sp)
    lw s2, 36(sp)
    lw s3, 40(sp)
    lw s4, 44(sp)
    lw s5, 48(sp)
    lw s6, 52(sp)
    lw s7, 56(sp)
    lw s8, 60(sp)
    lw s9, 64(sp)
    lw s10, 68(sp)
    lw s11, 72(sp)
    addi sp, sp, 76
    mul t1, t0, a0
    mv a0, t1
    lw ra, 12(sp)
    lw s0, 8(sp)
    lw s1, 4(sp)
    addi sp, sp, 16
    ret
fibonacci:
    # Function: fibonacci
    addi sp, sp, -16
    sw ra, 12(sp)
    sw s0, 8(sp)
    sw s1, 4(sp)
    addi s0, sp, 16
    sw a0, -12(s0)
    lw t0, -12(s0)
    li t1, 0
    slt t2, t1, t0
    xori t2, t2, 1
    beq t2, zero, fibonacci_else0
    li a0, 0
    lw ra, 12(sp)
    lw s0, 8(sp)
    lw s1, 4(sp)
    addi sp, sp, 16
    ret
    j fibonacci_endif1
fibonacci_else0:
fibonacci_endif1:
    lw t0, -12(s0)
    li t1, 1
    sub t2, t0, t1
    sltiu t2, t2, 1
    beq t2, zero, fibonacci_else2
    li t0, 1
    mv a0, t0
    lw ra, 12(sp)
    lw s0, 8(sp)
    lw s1, 4(sp)
    addi sp, sp, 16
    ret
    j fibonacci_endif3
fibonacci_else2:
fibonacci_endif3:
    addi sp, sp, -76
    sw t0, 0(sp)
    sw t1, 4(sp)
    sw t2, 8(sp)
    sw t3, 12(sp)
    sw t4, 16(sp)
    sw t5, 20(sp)
    sw t6, 24(sp)
    sw s0, 28(sp)
    sw s1, 32(sp)
    sw s2, 36(sp)
    sw s3, 40(sp)
    sw s4, 44(sp)
    sw s5, 48(sp)
    sw s6, 52(sp)
    sw s7, 56(sp)
    sw s8, 60(sp)
    sw s9, 64(sp)
    sw s10, 68(sp)
    sw s11, 72(sp)
    lw t0, -12(s0)
    li t1, 1
    sub t2, t0, t1
    mv a0, t2
    jal ra, fibonacci
    lw t0, 0(sp)
    lw t1, 4(sp)
    lw t2, 8(sp)
    lw t3, 12(sp)
    lw t4, 16(sp)
    lw t5, 20(sp)
    lw t6, 24(sp)
    lw s0, 28(sp)
    lw s1, 32(sp)
    lw s2, 36(sp)
    lw s3, 40(sp)
    lw s4, 44(sp)
    lw s5, 48(sp)
    lw s6, 52(sp)
    lw s7, 56(sp)
    lw s8, 60(sp)
    lw s9, 64(sp)
    lw s10, 68(sp)
    lw s11, 72(sp)
    addi sp, sp, 76
    addi sp, sp, -76
    sw t0, 0(sp)
    sw t1, 4(sp)
    sw t2, 8(sp)
    sw t3, 12(sp)
    sw t4, 16(sp)
    sw t5, 20(sp)
    sw t6, 24(sp)
    sw s0, 28(sp)
    sw s1, 32(sp)
    sw s2, 36(sp)
    sw s3, 40(sp)
    sw s4, 44(sp)
    sw s5, 48(sp)
    sw s6, 52(sp)
    sw s7, 56(sp)
    sw s8, 60(sp)
    sw s9, 64(sp)
    sw s10, 68(sp)
    sw s11, 72(sp)
    lw t0, -12(s0)
    li t1, 2
    sub t2, t0, t1
    mv a0, t2
    jal ra, fibonacci
    lw t0, 0(sp)
    lw t1, 4(sp)
    lw t2, 8(sp)
    lw t3, 12(sp)
    lw t4, 16(sp)
    lw t5, 20(sp)
    lw t6, 24(sp)
    lw s0, 28(sp)
    lw s1, 32(sp)
    lw s2, 36(sp)
    lw s3, 40(sp)
    lw s4, 44(sp)
    lw s5, 48(sp)
    lw s6, 52(sp)
    lw s7, 56(sp)
    lw s8, 60(sp)
    lw s9, 64(sp)
    lw s10, 68(sp)
    lw s11, 72(sp)
    addi sp, sp, 76
    add t0, a0, a0
    mv a0, t0
    lw ra, 12(sp)
    lw s0, 8(sp)
    lw s1, 4(sp)
    addi sp, sp, 16
    ret
gcd:
    # Function: gcd
    addi sp, sp, -16
    sw ra, 12(sp)
    sw s0, 8(sp)
    sw s1, 4(sp)
    addi s0, sp, 16
    sw a0, -12(s0)
    sw a1, -16(s0)
    lw t0, -16(s0)
    li t1, 0
    sub t2, t0, t1
    sltiu t2, t2, 1
    beq t2, zero, gcd_else0
    lw t0, -12(s0)
    mv a0, t0
    lw ra, 12(sp)
    lw s0, 8(sp)
    lw s1, 4(sp)
    addi sp, sp, 16
    ret
    j gcd_endif1
gcd_else0:
gcd_endif1:
    addi sp, sp, -76
    sw t0, 0(sp)
    sw t1, 4(sp)
    sw t2, 8(sp)
    sw t3, 12(sp)
    sw t4, 16(sp)
    sw t5, 20(sp)
    sw t6, 24(sp)
    sw s0, 28(sp)
    sw s1, 32(sp)
    sw s2, 36(sp)
    sw s3, 40(sp)
    sw s4, 44(sp)
    sw s5, 48(sp)
    sw s6, 52(sp)
    sw s7, 56(sp)
    sw s8, 60(sp)
    sw s9, 64(sp)
    sw s10, 68(sp)
    sw s11, 72(sp)
    lw t0, -16(s0)
    mv a0, t0
    lw t0, -12(s0)
    lw t1, -16(s0)
    rem t2, t0, t1
    mv a1, t2
    jal ra, gcd
    lw t0, 0(sp)
    lw t1, 4(sp)
    lw t2, 8(sp)
    lw t3, 12(sp)
    lw t4, 16(sp)
    lw t5, 20(sp)
    lw t6, 24(sp)
    lw s0, 28(sp)
    lw s1, 32(sp)
    lw s2, 36(sp)
    lw s3, 40(sp)
    lw s4, 44(sp)
    lw s5, 48(sp)
    lw s6, 52(sp)
    lw s7, 56(sp)
    lw s8, 60(sp)
    lw s9, 64(sp)
    lw s10, 68(sp)
    lw s11, 72(sp)
    addi sp, sp, 76
    mv a0, a0
    lw ra, 12(sp)
    lw s0, 8(sp)
    lw s1, 4(sp)
    addi sp, sp, 16
    ret
is_prime:
    # Function: is_prime
    addi sp, sp, -16
    sw ra, 12(sp)
    sw s0, 8(sp)
    sw s1, 4(sp)
    addi s0, sp, 16
    sw a0, -12(s0)
    lw t0, -12(s0)
    li t1, 1
    slt t2, t1, t0
    xori t2, t2, 1
    beq t2, zero, is_prime_else0
    li a0, 0
    lw ra, 12(sp)
    lw s0, 8(sp)
    lw s1, 4(sp)
    addi sp, sp, 16
    ret
    j is_prime_endif1
is_prime_else0:
is_prime_endif1:
    lw t0, -12(s0)
    li t1, 3
    slt t2, t1, t0
    xori t2, t2, 1
    beq t2, zero, is_prime_else2
    li t0, 1
    mv a0, t0
    lw ra, 12(sp)
    lw s0, 8(sp)
    lw s1, 4(sp)
    addi sp, sp, 16
    ret
    j is_prime_endif3
is_prime_else2:
is_prime_endif3:
    lw t0, -12(s0)
    li t1, 2
    rem t2, t0, t1
    li t0, 0
    sub t1, t2, t0
    sltiu t1, t1, 1
    lw t0, -12(s0)
    li t2, 3
    rem t3, t0, t2
    li t0, 0
    sub t2, t3, t0
    sltiu t2, t2, 1
    or t0, t1, t2
    sltu t0, zero, t0
    beq t0, zero, is_prime_else4
    li a0, 0
    lw ra, 12(sp)
    lw s0, 8(sp)
    lw s1, 4(sp)
    addi sp, sp, 16
    ret
    j is_prime_endif5
is_prime_else4:
is_prime_endif5:
    li t0, 5
    sw t0, -16(s0)
is_prime_loop6:
    lw t0, -16(s0)
    lw t1, -16(s0)
    mul t2, t0, t1
    lw t0, -12(s0)
    slt t1, t0, t2
    xori t1, t1, 1
    beq t1, zero, is_prime_endloop7
    lw t0, -12(s0)
    lw t2, -16(s0)
    rem t3, t0, t2
    li t0, 0
    sub t2, t3, t0
    sltiu t2, t2, 1
    lw t0, -12(s0)
    lw t3, -16(s0)
    li t4, 2
    add t5, t3, t4
    rem t3, t0, t5
    li t0, 0
    sub t4, t3, t0
    sltiu t4, t4, 1
    or t0, t2, t4
    sltu t0, zero, t0
    beq t0, zero, is_prime_else8
    li a0, 0
    lw ra, 12(sp)
    lw s0, 8(sp)
    lw s1, 4(sp)
    addi sp, sp, 16
    ret
    j is_prime_endif9
is_prime_else8:
is_prime_endif9:
    lw t0, -16(s0)
    li t2, 6
    add t3, t0, t2
    sw t3, -16(s0)
    j is_prime_loop6
is_prime_endloop7:
    li t0, 1
    mv a0, t0
    lw ra, 12(sp)
    lw s0, 8(sp)
    lw s1, 4(sp)
    addi sp, sp, 16
    ret
main:
    # Function: main
    addi sp, sp, -128
    sw ra, 124(sp)
    sw s0, 120(sp)
    sw s1, 116(sp)
    addi s0, sp, 128
    li t0, 678
    sw t0, -12(s0)
    li t0, 934
    sw t0, -16(s0)
    li t0, 879
    sub t1, zero, t0
    sw t1, -20(s0)
    li t0, 884
    sw t0, -24(s0)
    lw t0, -12(s0)
    lw t1, -16(s0)
    sub t2, t0, t1
    lw t0, -20(s0)
    mul t1, t2, t0
    lw t0, -24(s0)
    lw t2, -12(s0)
    mul t3, t0, t2
    add t0, t1, t3
    lw t1, -16(s0)
    lw t2, -20(s0)
    add t3, t1, t2
    lw t1, -12(s0)
    lw t2, -24(s0)
    sub t4, zero, t2
    add t2, t1, t4
    li t1, 2048
    add t4, t2, t1
    rem t1, t3, t4
    li t2, 1
    add t3, t1, t2
    div t1, t0, t3
    lw t0, -12(s0)
    sub t2, zero, t0
    lw t0, -16(s0)
    sub t3, zero, t0
    mul t0, t2, t3
    lw t2, -20(s0)
    mv t3, t2
    mul t2, t0, t3
    lw t0, -24(s0)
    li t3, 2
    mv t4, t3
    sub t3, t0, t4
    lw t0, -20(s0)
    sub t4, zero, t0
    sub t0, t3, t4
    mul t3, t2, t0
    sub t0, t1, t3
    sw t0, -28(s0)
    li t0, 754
    sub t1, zero, t0
    sw t1, -32(s0)
    li t0, 368
    sw t0, -36(s0)
    li t0, 15
    sub t1, zero, t0
    sw t1, -40(s0)
    li t0, 0
    sw t0, -44(s0)
    lw t0, -32(s0)
    lw t1, -36(s0)
    slt t2, t1, t0
    lw t0, -40(s0)
    li t1, 1
    add t3, t0, t1
    li t0, 1
    sub t1, t3, t0
    sltiu t1, t1, 1
    sltu t0, zero, t2
    sltu t1, zero, t1
    and t0, t0, t1
    beq t0, zero, main_else0
    li t1, 1
    sw t1, -44(s0)
    j main_endif1
main_else0:
main_endif1:
    li t0, 0
    sw t0, -48(s0)
    lw t0, -32(s0)
    lw t1, -36(s0)
    slt t2, t0, t1
    lw t0, -40(s0)
    li t1, 2
    add t3, t0, t1
    li t0, 2
    sub t1, t3, t0
    sltiu t1, t1, 1
    or t0, t2, t1
    sltu t0, zero, t0
    beq t0, zero, main_else2
    li t1, 1
    sw t1, -48(s0)
    j main_endif3
main_else2:
main_endif3:
    li t0, 0
    sw t0, -52(s0)
    lw t0, -32(s0)
    li t1, 390
    sub t2, zero, t1
    slt t1, t2, t0
    lw t0, -36(s0)
    li t2, 600
    sub t3, zero, t2
    slt t2, t0, t3
    sltu t0, zero, t1
    sltu t1, zero, t2
    and t0, t0, t1
    lw t1, -40(s0)
    li t2, 743
    sub t3, zero, t2
    slt t2, t3, t1
    lw t1, -32(s0)
    li t3, 959
    sub t4, zero, t3
    slt t3, t1, t4
    sltu t0, zero, t2
    sltu t1, zero, t3
    and t1, t0, t1
    or t2, t0, t1
    sltu t2, zero, t2
    sltiu t0, t2, 1
    lw t1, -36(s0)
    li t2, 833
    slt t3, t2, t1
    lw t1, -32(s0)
    li t2, 315
    sub t4, zero, t2
    slt t2, t1, t4
    or t1, t3, t2
    sltu t1, zero, t1
    sltu t0, zero, t0
    sltu t1, zero, t1
    and t2, t0, t1
    beq t2, zero, main_else4
    li t0, 1
    sw t0, -52(s0)
    j main_endif5
main_else4:
main_endif5:
    li t0, 12137
    sw t0, -56(s0)
    li t0, 11136
    sw t0, -60(s0)
    li t0, 1921
    sw t0, -64(s0)
    addi sp, sp, -76
    sw t0, 0(sp)
    sw t1, 4(sp)
    sw t2, 8(sp)
    sw t3, 12(sp)
    sw t4, 16(sp)
    sw t5, 20(sp)
    sw t6, 24(sp)
    sw s0, 28(sp)
    sw s1, 32(sp)
    sw s2, 36(sp)
    sw s3, 40(sp)
    sw s4, 44(sp)
    sw s5, 48(sp)
    sw s6, 52(sp)
    sw s7, 56(sp)
    sw s8, 60(sp)
    sw s9, 64(sp)
    sw s10, 68(sp)
    sw s11, 72(sp)
    addi sp, sp, -76
    sw t0, 0(sp)
    sw t1, 4(sp)
    sw t2, 8(sp)
    sw t3, 12(sp)
    sw t4, 16(sp)
    sw t5, 20(sp)
    sw t6, 24(sp)
    sw s0, 28(sp)
    sw s1, 32(sp)
    sw s2, 36(sp)
    sw s3, 40(sp)
    sw s4, 44(sp)
    sw s5, 48(sp)
    sw s6, 52(sp)
    sw s7, 56(sp)
    sw s8, 60(sp)
    sw s9, 64(sp)
    sw s10, 68(sp)
    sw s11, 72(sp)
    lw t0, -60(s0)
    li t1, 12
    rem t2, t0, t1
    mv a0, t2
    lw t0, -64(s0)
    li t1, 12
    rem t2, t0, t1
    mv a1, t2
    jal ra, gcd
    lw t0, 0(sp)
    lw t1, 4(sp)
    lw t2, 8(sp)
    lw t3, 12(sp)
    lw t4, 16(sp)
    lw t5, 20(sp)
    lw t6, 24(sp)
    lw s0, 28(sp)
    lw s1, 32(sp)
    lw s2, 36(sp)
    lw s3, 40(sp)
    lw s4, 44(sp)
    lw s5, 48(sp)
    lw s6, 52(sp)
    lw s7, 56(sp)
    lw s8, 60(sp)
    lw s9, 64(sp)
    lw s10, 68(sp)
    lw s11, 72(sp)
    addi sp, sp, 76
    mv a0, a0
    jal ra, factorial
    lw t0, 0(sp)
    lw t1, 4(sp)
    lw t2, 8(sp)
    lw t3, 12(sp)
    lw t4, 16(sp)
    lw t5, 20(sp)
    lw t6, 24(sp)
    lw s0, 28(sp)
    lw s1, 32(sp)
    lw s2, 36(sp)
    lw s3, 40(sp)
    lw s4, 44(sp)
    lw s5, 48(sp)
    lw s6, 52(sp)
    lw s7, 56(sp)
    lw s8, 60(sp)
    lw s9, 64(sp)
    lw s10, 68(sp)
    lw s11, 72(sp)
    addi sp, sp, 76
    addi sp, sp, -76
    sw t0, 0(sp)
    sw t1, 4(sp)
    sw t2, 8(sp)
    sw t3, 12(sp)
    sw t4, 16(sp)
    sw t5, 20(sp)
    sw t6, 24(sp)
    sw s0, 28(sp)
    sw s1, 32(sp)
    sw s2, 36(sp)
    sw s3, 40(sp)
    sw s4, 44(sp)
    sw s5, 48(sp)
    sw s6, 52(sp)
    sw s7, 56(sp)
    sw s8, 60(sp)
    sw s9, 64(sp)
    sw s10, 68(sp)
    sw s11, 72(sp)
    lw t0, -56(s0)
    li t1, 10
    rem t2, t0, t1
    li t0, 2
    add t1, t2, t0
    mv a0, t1
    jal ra, fibonacci
    lw t0, 0(sp)
    lw t1, 4(sp)
    lw t2, 8(sp)
    lw t3, 12(sp)
    lw t4, 16(sp)
    lw t5, 20(sp)
    lw t6, 24(sp)
    lw s0, 28(sp)
    lw s1, 32(sp)
    lw s2, 36(sp)
    lw s3, 40(sp)
    lw s4, 44(sp)
    lw s5, 48(sp)
    lw s6, 52(sp)
    lw s7, 56(sp)
    lw s8, 60(sp)
    lw s9, 64(sp)
    lw s10, 68(sp)
    lw s11, 72(sp)
    addi sp, sp, 76
    add t0, a0, a0
    sw t0, -68(s0)
    li t0, 0
    sw t0, -72(s0)
    lw t0, -56(s0)
    lw t1, -60(s0)
    slt t2, t1, t0
    lw t0, -56(s0)
    lw t1, -64(s0)
    slt t3, t1, t0
    sltu t0, zero, t2
    sltu t1, zero, t3
    and t0, t0, t1
    beq t0, zero, main_else6
    lw t1, -56(s0)
    sw t1, -72(s0)
    j main_endif7
main_else6:
    lw t1, -60(s0)
    lw t2, -56(s0)
    slt t3, t2, t1
    lw t1, -60(s0)
    lw t2, -64(s0)
    slt t4, t2, t1
    sltu t0, zero, t3
    sltu t1, zero, t4
    and t1, t0, t1
    beq t1, zero, main_else8
    lw t2, -60(s0)
    sw t2, -72(s0)
    j main_endif9
main_else8:
    lw t2, -64(s0)
    sw t2, -72(s0)
main_endif9:
main_endif7:
    li t0, 0
    sw t0, -76(s0)
    li t0, 1
    sw t0, -80(s0)
main_loop10:
    lw t0, -80(s0)
    li t1, 15
    slt t2, t1, t0
    xori t2, t2, 1
    beq t2, zero, main_endloop11
    lw t0, -80(s0)
    li t1, 2
    rem t3, t0, t1
    li t0, 0
    sub t1, t3, t0
    sltiu t1, t1, 1
    beq t1, zero, main_else12
    lw t0, -76(s0)
    lw t3, -80(s0)
    lw t4, -80(s0)
    mul t5, t3, t4
    add t3, t0, t5
    sw t3, -76(s0)
    j main_endif13
main_else12:
    lw t0, -80(s0)
    li t3, 5
    rem t4, t0, t3
    li t0, 0
    sub t3, t4, t0
    sltiu t3, t3, 1
    beq t3, zero, main_else14
    lw t0, -76(s0)
    lw t4, -80(s0)
    lw t5, -80(s0)
    mul t6, t4, t5
    lw t4, -80(s0)
    mul t5, t6, t4
    add t4, t0, t5
    sw t4, -76(s0)
    j main_endif15
main_else14:
    lw t0, -76(s0)
    lw t4, -80(s0)
    add t5, t0, t4
    sw t5, -76(s0)
main_endif15:
main_endif13:
    lw t0, -80(s0)
    li t1, 1
    add t3, t0, t1
    sw t3, -80(s0)
    j main_loop10
main_endloop11:
    li t0, 0
    sw t0, -84(s0)
    li t0, 1
    sw t0, -80(s0)
main_loop16:
    lw t0, -80(s0)
    li t1, 7
    slt t2, t1, t0
    xori t2, t2, 1
    beq t2, zero, main_endloop17
    li t0, 1
    sw t0, -88(s0)
    li t0, 1
    sw t0, -92(s0)
main_loop18:
    lw t0, -88(s0)
    lw t1, -80(s0)
    slt t3, t1, t0
    xori t3, t3, 1
    beq t3, zero, main_endloop19
    lw t0, -92(s0)
    lw t1, -88(s0)
    mul t4, t0, t1
    sw t4, -92(s0)
    lw t0, -88(s0)
    li t1, 1
    add t4, t0, t1
    sw t4, -88(s0)
    j main_loop18
main_endloop19:
    lw t0, -84(s0)
    lw t1, -92(s0)
    add t3, t0, t1
    sw t3, -84(s0)
    lw t0, -80(s0)
    li t1, 1
    add t3, t0, t1
    sw t3, -80(s0)
    j main_loop16
main_endloop17:
    li t0, 0
    sw t0, -88(s0)
    addi sp, sp, -76
    sw t0, 0(sp)
    sw t1, 4(sp)
    sw t2, 8(sp)
    sw t3, 12(sp)
    sw t4, 16(sp)
    sw t5, 20(sp)
    sw t6, 24(sp)
    sw s0, 28(sp)
    sw s1, 32(sp)
    sw s2, 36(sp)
    sw s3, 40(sp)
    sw s4, 44(sp)
    sw s5, 48(sp)
    sw s6, 52(sp)
    sw s7, 56(sp)
    sw s8, 60(sp)
    sw s9, 64(sp)
    sw s10, 68(sp)
    sw s11, 72(sp)
    lw t0, -56(s0)
    mv a0, t0
    jal ra, is_prime
    lw t0, 0(sp)
    lw t1, 4(sp)
    lw t2, 8(sp)
    lw t3, 12(sp)
    lw t4, 16(sp)
    lw t5, 20(sp)
    lw t6, 24(sp)
    lw s0, 28(sp)
    lw s1, 32(sp)
    lw s2, 36(sp)
    lw s3, 40(sp)
    lw s4, 44(sp)
    lw s5, 48(sp)
    lw s6, 52(sp)
    lw s7, 56(sp)
    lw s8, 60(sp)
    lw s9, 64(sp)
    lw s10, 68(sp)
    lw s11, 72(sp)
    addi sp, sp, 76
    beq a0, zero, main_else20
    addi sp, sp, -76
    sw t0, 0(sp)
    sw t1, 4(sp)
    sw t2, 8(sp)
    sw t3, 12(sp)
    sw t4, 16(sp)
    sw t5, 20(sp)
    sw t6, 24(sp)
    sw s0, 28(sp)
    sw s1, 32(sp)
    sw s2, 36(sp)
    sw s3, 40(sp)
    sw s4, 44(sp)
    sw s5, 48(sp)
    sw s6, 52(sp)
    sw s7, 56(sp)
    sw s8, 60(sp)
    sw s9, 64(sp)
    sw s10, 68(sp)
    sw s11, 72(sp)
    lw t0, -60(s0)
    mv a0, t0
    jal ra, is_prime
    lw t0, 0(sp)
    lw t1, 4(sp)
    lw t2, 8(sp)
    lw t3, 12(sp)
    lw t4, 16(sp)
    lw t5, 20(sp)
    lw t6, 24(sp)
    lw s0, 28(sp)
    lw s1, 32(sp)
    lw s2, 36(sp)
    lw s3, 40(sp)
    lw s4, 44(sp)
    lw s5, 48(sp)
    lw s6, 52(sp)
    lw s7, 56(sp)
    lw s8, 60(sp)
    lw s9, 64(sp)
    lw s10, 68(sp)
    lw s11, 72(sp)
    addi sp, sp, 76
    beq a0, zero, main_else22
    lw t0, -56(s0)
    lw t1, -60(s0)
    mul t2, t0, t1
    sw t2, -88(s0)
    j main_endif23
main_else22:
    addi sp, sp, -76
    sw t0, 0(sp)
    sw t1, 4(sp)
    sw t2, 8(sp)
    sw t3, 12(sp)
    sw t4, 16(sp)
    sw t5, 20(sp)
    sw t6, 24(sp)
    sw s0, 28(sp)
    sw s1, 32(sp)
    sw s2, 36(sp)
    sw s3, 40(sp)
    sw s4, 44(sp)
    sw s5, 48(sp)
    sw s6, 52(sp)
    sw s7, 56(sp)
    sw s8, 60(sp)
    sw s9, 64(sp)
    sw s10, 68(sp)
    sw s11, 72(sp)
    lw t0, -64(s0)
    mv a0, t0
    jal ra, is_prime
    lw t0, 0(sp)
    lw t1, 4(sp)
    lw t2, 8(sp)
    lw t3, 12(sp)
    lw t4, 16(sp)
    lw t5, 20(sp)
    lw t6, 24(sp)
    lw s0, 28(sp)
    lw s1, 32(sp)
    lw s2, 36(sp)
    lw s3, 40(sp)
    lw s4, 44(sp)
    lw s5, 48(sp)
    lw s6, 52(sp)
    lw s7, 56(sp)
    lw s8, 60(sp)
    lw s9, 64(sp)
    lw s10, 68(sp)
    lw s11, 72(sp)
    addi sp, sp, 76
    beq a0, zero, main_else24
    lw t0, -56(s0)
    lw t1, -64(s0)
    mul t2, t0, t1
    sw t2, -88(s0)
    j main_endif25
main_else24:
    lw t0, -56(s0)
    sw t0, -88(s0)
main_endif25:
main_endif23:
    j main_endif21
main_else20:
    addi sp, sp, -76
    sw t0, 0(sp)
    sw t1, 4(sp)
    sw t2, 8(sp)
    sw t3, 12(sp)
    sw t4, 16(sp)
    sw t5, 20(sp)
    sw t6, 24(sp)
    sw s0, 28(sp)
    sw s1, 32(sp)
    sw s2, 36(sp)
    sw s3, 40(sp)
    sw s4, 44(sp)
    sw s5, 48(sp)
    sw s6, 52(sp)
    sw s7, 56(sp)
    sw s8, 60(sp)
    sw s9, 64(sp)
    sw s10, 68(sp)
    sw s11, 72(sp)
    lw t0, -60(s0)
    mv a0, t0
    jal ra, is_prime
    lw t0, 0(sp)
    lw t1, 4(sp)
    lw t2, 8(sp)
    lw t3, 12(sp)
    lw t4, 16(sp)
    lw t5, 20(sp)
    lw t6, 24(sp)
    lw s0, 28(sp)
    lw s1, 32(sp)
    lw s2, 36(sp)
    lw s3, 40(sp)
    lw s4, 44(sp)
    lw s5, 48(sp)
    lw s6, 52(sp)
    lw s7, 56(sp)
    lw s8, 60(sp)
    lw s9, 64(sp)
    lw s10, 68(sp)
    lw s11, 72(sp)
    addi sp, sp, 76
    beq a0, zero, main_else26
    addi sp, sp, -76
    sw t0, 0(sp)
    sw t1, 4(sp)
    sw t2, 8(sp)
    sw t3, 12(sp)
    sw t4, 16(sp)
    sw t5, 20(sp)
    sw t6, 24(sp)
    sw s0, 28(sp)
    sw s1, 32(sp)
    sw s2, 36(sp)
    sw s3, 40(sp)
    sw s4, 44(sp)
    sw s5, 48(sp)
    sw s6, 52(sp)
    sw s7, 56(sp)
    sw s8, 60(sp)
    sw s9, 64(sp)
    sw s10, 68(sp)
    sw s11, 72(sp)
    lw t0, -64(s0)
    mv a0, t0
    jal ra, is_prime
    lw t0, 0(sp)
    lw t1, 4(sp)
    lw t2, 8(sp)
    lw t3, 12(sp)
    lw t4, 16(sp)
    lw t5, 20(sp)
    lw t6, 24(sp)
    lw s0, 28(sp)
    lw s1, 32(sp)
    lw s2, 36(sp)
    lw s3, 40(sp)
    lw s4, 44(sp)
    lw s5, 48(sp)
    lw s6, 52(sp)
    lw s7, 56(sp)
    lw s8, 60(sp)
    lw s9, 64(sp)
    lw s10, 68(sp)
    lw s11, 72(sp)
    addi sp, sp, 76
    beq a0, zero, main_else28
    lw t0, -60(s0)
    lw t1, -64(s0)
    mul t2, t0, t1
    sw t2, -88(s0)
    j main_endif29
main_else28:
    lw t0, -60(s0)
    sw t0, -88(s0)
main_endif29:
    j main_endif27
main_else26:
    addi sp, sp, -76
    sw t0, 0(sp)
    sw t1, 4(sp)
    sw t2, 8(sp)
    sw t3, 12(sp)
    sw t4, 16(sp)
    sw t5, 20(sp)
    sw t6, 24(sp)
    sw s0, 28(sp)
    sw s1, 32(sp)
    sw s2, 36(sp)
    sw s3, 40(sp)
    sw s4, 44(sp)
    sw s5, 48(sp)
    sw s6, 52(sp)
    sw s7, 56(sp)
    sw s8, 60(sp)
    sw s9, 64(sp)
    sw s10, 68(sp)
    sw s11, 72(sp)
    lw t0, -64(s0)
    mv a0, t0
    jal ra, is_prime
    lw t0, 0(sp)
    lw t1, 4(sp)
    lw t2, 8(sp)
    lw t3, 12(sp)
    lw t4, 16(sp)
    lw t5, 20(sp)
    lw t6, 24(sp)
    lw s0, 28(sp)
    lw s1, 32(sp)
    lw s2, 36(sp)
    lw s3, 40(sp)
    lw s4, 44(sp)
    lw s5, 48(sp)
    lw s6, 52(sp)
    lw s7, 56(sp)
    lw s8, 60(sp)
    lw s9, 64(sp)
    lw s10, 68(sp)
    lw s11, 72(sp)
    addi sp, sp, 76
    beq a0, zero, main_else30
    lw t0, -64(s0)
    sw t0, -88(s0)
    j main_endif31
main_else30:
    lw t0, -56(s0)
    lw t1, -60(s0)
    add t2, t0, t1
    lw t0, -64(s0)
    add t1, t2, t0
    sw t1, -88(s0)
main_endif31:
main_endif27:
main_endif21:
    li t0, 0
    sw t0, -92(s0)
    li t0, 16085975
    sw t0, -96(s0)
    li t0, 0
    sw t0, -100(s0)
main_loop32:
    lw t0, -96(s0)
    li t1, 0
    slt t2, t1, t0
    beq t2, zero, main_endloop33
    lw t0, -96(s0)
    li t1, 2
    rem t3, t0, t1
    li t0, 1
    sub t1, t3, t0
    sltiu t1, t1, 1
    beq t1, zero, main_else34
    lw t0, -100(s0)
    li t3, 1
    add t4, t0, t3
    sw t4, -100(s0)
    j main_endif35
main_else34:
main_endif35:
    lw t0, -96(s0)
    li t1, 2
    div t3, t0, t1
    sw t3, -96(s0)
    j main_loop32
main_endloop33:
    lw t0, -28(s0)
    lw t1, -44(s0)
    sub t2, t0, t1
    lw t0, -48(s0)
    sub t1, t2, t0
    lw t0, -52(s0)
    add t2, t1, t0
    lw t0, -68(s0)
    sub t1, t2, t0
    lw t0, -72(s0)
    add t2, t1, t0
    lw t0, -76(s0)
    sub t1, t2, t0
    lw t0, -84(s0)
    add t2, t1, t0
    lw t0, -88(s0)
    sub t1, t2, t0
    lw t0, -100(s0)
    sub t2, t1, t0
    sw t2, -104(s0)
    lw t0, -104(s0)
    mv a0, t0
    lw ra, 124(sp)
    lw s0, 120(sp)
    lw s1, 116(sp)
    addi sp, sp, 128
    ret
