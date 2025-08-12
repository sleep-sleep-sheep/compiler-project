    .text
    .globl main
    # ToyC Compiler Generated Code
factorial:
    # Function: factorial
    addi sp, sp, -16
    sw ra, 12(sp)
    sw fp, 8(sp)
    addi fp, sp, 16
    sw a0, -12(fp)
    lw t0, -12(fp)
    li t1, 1
    slt t2, t1, t0
    xori t2, t2, 1
    beq t2, zero, factorial_else0
    li t3, 1
    mv a0, t3
    lw ra, 12(sp)
    lw fp, 8(sp)
    addi sp, sp, 16
    ret
    j factorial_endif1
factorial_else0:
factorial_endif1:
    lw t4, -12(fp)
    addi sp, sp, -28
    sw t0, 24(sp)
    sw t1, 20(sp)
    sw t2, 16(sp)
    sw t3, 12(sp)
    sw t4, 8(sp)
    sw t5, 4(sp)
    sw t6, 0(sp)
    lw t5, -12(fp)
    li t6, 1
    sub t0, t5, t6
    mv a0, t0
    jal ra, factorial
    lw t0, 24(sp)
    lw t1, 20(sp)
    lw t2, 16(sp)
    lw t3, 12(sp)
    lw t4, 8(sp)
    lw t5, 4(sp)
    lw t6, 0(sp)
    addi sp, sp, 28
    mul t1, t4, a0
    mv a0, t1
    lw ra, 12(sp)
    lw fp, 8(sp)
    addi sp, sp, 16
    ret
fibonacci:
    # Function: fibonacci
    addi sp, sp, -16
    sw ra, 12(sp)
    sw fp, 8(sp)
    addi fp, sp, 16
    sw a0, -12(fp)
    lw t0, -12(fp)
    li t1, 0
    slt t2, t1, t0
    xori t2, t2, 1
    beq t2, zero, fibonacci_else0
    li a0, 0
    lw ra, 12(sp)
    lw fp, 8(sp)
    addi sp, sp, 16
    ret
    j fibonacci_endif1
fibonacci_else0:
fibonacci_endif1:
    lw t3, -12(fp)
    li t4, 1
    sub t5, t3, t4
    sltiu t5, t5, 1
    beq t5, zero, fibonacci_else2
    li t6, 1
    mv a0, t6
    lw ra, 12(sp)
    lw fp, 8(sp)
    addi sp, sp, 16
    ret
    j fibonacci_endif3
fibonacci_else2:
fibonacci_endif3:
    addi sp, sp, -28
    sw t0, 24(sp)
    sw t1, 20(sp)
    sw t2, 16(sp)
    sw t3, 12(sp)
    sw t4, 8(sp)
    sw t5, 4(sp)
    sw t6, 0(sp)
    lw t0, -12(fp)
    li t1, 1
    sub t2, t0, t1
    mv a0, t2
    jal ra, fibonacci
    lw t0, 24(sp)
    lw t1, 20(sp)
    lw t2, 16(sp)
    lw t3, 12(sp)
    lw t4, 8(sp)
    lw t5, 4(sp)
    lw t6, 0(sp)
    addi sp, sp, 28
    addi sp, sp, -28
    sw t0, 24(sp)
    sw t1, 20(sp)
    sw t2, 16(sp)
    sw t3, 12(sp)
    sw t4, 8(sp)
    sw t5, 4(sp)
    sw t6, 0(sp)
    lw t3, -12(fp)
    li t4, 2
    sub t5, t3, t4
    mv a0, t5
    jal ra, fibonacci
    lw t0, 24(sp)
    lw t1, 20(sp)
    lw t2, 16(sp)
    lw t3, 12(sp)
    lw t4, 8(sp)
    lw t5, 4(sp)
    lw t6, 0(sp)
    addi sp, sp, 28
    add t6, a0, a0
    mv a0, t6
    lw ra, 12(sp)
    lw fp, 8(sp)
    addi sp, sp, 16
    ret
gcd:
    # Function: gcd
    addi sp, sp, -16
    sw ra, 12(sp)
    sw fp, 8(sp)
    addi fp, sp, 16
    sw a0, -12(fp)
    sw a1, -16(fp)
    lw t0, -16(fp)
    li t1, 0
    sub t2, t0, t1
    sltiu t2, t2, 1
    beq t2, zero, gcd_else0
    lw t3, -12(fp)
    mv a0, t3
    lw ra, 12(sp)
    lw fp, 8(sp)
    addi sp, sp, 16
    ret
    j gcd_endif1
gcd_else0:
gcd_endif1:
    addi sp, sp, -28
    sw t0, 24(sp)
    sw t1, 20(sp)
    sw t2, 16(sp)
    sw t3, 12(sp)
    sw t4, 8(sp)
    sw t5, 4(sp)
    sw t6, 0(sp)
    lw t4, -16(fp)
    mv a0, t4
    lw t5, -12(fp)
    lw t6, -16(fp)
    rem t0, t5, t6
    mv a1, t0
    jal ra, gcd
    lw t0, 24(sp)
    lw t1, 20(sp)
    lw t2, 16(sp)
    lw t3, 12(sp)
    lw t4, 8(sp)
    lw t5, 4(sp)
    lw t6, 0(sp)
    addi sp, sp, 28
    mv a0, a0
    lw ra, 12(sp)
    lw fp, 8(sp)
    addi sp, sp, 16
    ret
is_prime:
    # Function: is_prime
    addi sp, sp, -16
    sw ra, 12(sp)
    sw fp, 8(sp)
    addi fp, sp, 16
    sw a0, -12(fp)
    lw t0, -12(fp)
    li t1, 1
    slt t2, t1, t0
    xori t2, t2, 1
    beq t2, zero, is_prime_else0
    li a0, 0
    lw ra, 12(sp)
    lw fp, 8(sp)
    addi sp, sp, 16
    ret
    j is_prime_endif1
is_prime_else0:
is_prime_endif1:
    lw t3, -12(fp)
    li t4, 3
    slt t5, t4, t3
    xori t5, t5, 1
    beq t5, zero, is_prime_else2
    li t6, 1
    mv a0, t6
    lw ra, 12(sp)
    lw fp, 8(sp)
    addi sp, sp, 16
    ret
    j is_prime_endif3
is_prime_else2:
is_prime_endif3:
    lw t0, -12(fp)
    li t1, 2
    rem t2, t0, t1
    li t3, 0
    sub t4, t2, t3
    sltiu t4, t4, 1
    lw t5, -12(fp)
    li t6, 3
    rem t0, t5, t6
    li t1, 0
    sub t2, t0, t1
    sltiu t2, t2, 1
    or t3, t4, t2
    sltu t3, zero, t3
    beq t3, zero, is_prime_else4
    li a0, 0
    lw ra, 12(sp)
    lw fp, 8(sp)
    addi sp, sp, 16
    ret
    j is_prime_endif5
is_prime_else4:
is_prime_endif5:
    li t4, 5
    sw t4, -16(fp)
is_prime_loop6:
    lw t5, -16(fp)
    lw t6, -16(fp)
    mul t0, t5, t6
    lw t1, -12(fp)
    slt t2, t1, t0
    xori t2, t2, 1
    beq t2, zero, is_prime_endloop7
    lw t3, -12(fp)
    lw t4, -16(fp)
    rem t5, t3, t4
    li t6, 0
    sub t0, t5, t6
    sltiu t0, t0, 1
    lw t1, -12(fp)
    lw t2, -16(fp)
    li t3, 2
    add t4, t2, t3
    rem t5, t1, t4
    li t6, 0
    sub t0, t5, t6
    sltiu t0, t0, 1
    or t1, t0, t0
    sltu t1, zero, t1
    beq t1, zero, is_prime_else8
    li a0, 0
    lw ra, 12(sp)
    lw fp, 8(sp)
    addi sp, sp, 16
    ret
    j is_prime_endif9
is_prime_else8:
is_prime_endif9:
    lw t2, -16(fp)
    li t3, 6
    add t4, t2, t3
    sw t4, -16(fp)
    j is_prime_loop6
is_prime_endloop7:
    li t5, 1
    mv a0, t5
    lw ra, 12(sp)
    lw fp, 8(sp)
    addi sp, sp, 16
    ret
main:
    # Function: main
    addi sp, sp, -112
    sw ra, 108(sp)
    sw fp, 104(sp)
    addi fp, sp, 112
    li t0, 678
    sw t0, -12(fp)
    li t1, 934
    sw t1, -16(fp)
    li t2, 879
    sub t3, zero, t2
    sw t3, -20(fp)
    li t4, 884
    sw t4, -24(fp)
    lw t5, -12(fp)
    lw t6, -16(fp)
    sub t0, t5, t6
    lw t1, -20(fp)
    mul t2, t0, t1
    lw t3, -24(fp)
    lw t4, -12(fp)
    mul t5, t3, t4
    add t6, t2, t5
    lw t0, -16(fp)
    lw t1, -20(fp)
    add t2, t0, t1
    lw t3, -12(fp)
    lw t4, -24(fp)
    sub t5, zero, t4
    add t6, t3, t5
    li t0, 2048
    add t1, t6, t0
    rem t2, t2, t1
    li t3, 1
    add t4, t2, t3
    div t5, t6, t4
    lw t6, -12(fp)
    sub t0, zero, t6
    lw t1, -16(fp)
    sub t2, zero, t1
    mul t3, t0, t2
    lw t4, -20(fp)
    mv t5, t4
    mul t6, t3, t5
    lw t0, -24(fp)
    li t1, 2
    mv t2, t1
    sub t3, t0, t2
    lw t4, -20(fp)
    sub t5, zero, t4
    sub t6, t3, t5
    mul t0, t6, t6
    sub t1, t5, t0
    sw t1, -28(fp)
    li t2, 754
    sub t3, zero, t2
    sw t3, -32(fp)
    li t4, 368
    sw t4, -36(fp)
    li t5, 15
    sub t6, zero, t5
    sw t6, -40(fp)
    li t0, 0
    sw t0, -44(fp)
    lw t1, -32(fp)
    lw t2, -36(fp)
    slt t3, t2, t1
    lw t4, -40(fp)
    li t5, 1
    add t6, t4, t5
    li t0, 1
    sub t1, t6, t0
    sltiu t1, t1, 1
    sltu t0, zero, t3
    sltu t1, zero, t1
    and t2, t0, t1
    beq t2, zero, main_else0
    li t3, 1
    sw t3, -44(fp)
    j main_endif1
main_else0:
main_endif1:
    li t4, 0
    sw t4, -48(fp)
    lw t5, -32(fp)
    lw t6, -36(fp)
    slt t0, t5, t6
    lw t1, -40(fp)
    li t2, 2
    add t3, t1, t2
    li t4, 2
    sub t5, t3, t4
    sltiu t5, t5, 1
    or t6, t0, t5
    sltu t6, zero, t6
    beq t6, zero, main_else2
    li t0, 1
    sw t0, -48(fp)
    j main_endif3
main_else2:
main_endif3:
    li t1, 0
    sw t1, -52(fp)
    lw t2, -32(fp)
    li t3, 390
    sub t4, zero, t3
    slt t5, t4, t2
    lw t6, -36(fp)
    li t0, 600
    sub t1, zero, t0
    slt t2, t6, t1
    sltu t0, zero, t5
    sltu t1, zero, t2
    and t3, t0, t1
    lw t4, -40(fp)
    li t5, 743
    sub t6, zero, t5
    slt t0, t6, t4
    lw t1, -32(fp)
    li t2, 959
    sub t3, zero, t2
    slt t4, t1, t3
    sltu t0, zero, t0
    sltu t1, zero, t4
    and t5, t0, t1
    or t6, t3, t5
    sltu t6, zero, t6
    sltiu t0, t6, 1
    lw t1, -36(fp)
    li t2, 833
    slt t3, t2, t1
    lw t4, -32(fp)
    li t5, 315
    sub t6, zero, t5
    slt t0, t4, t6
    or t1, t3, t0
    sltu t1, zero, t1
    sltu t0, zero, t0
    sltu t1, zero, t1
    and t2, t0, t1
    beq t2, zero, main_else4
    li t3, 1
    sw t3, -52(fp)
    j main_endif5
main_else4:
main_endif5:
    li t4, 12137
    sw t4, -56(fp)
    li t5, 11136
    sw t5, -60(fp)
    li t6, 1921
    sw t6, -64(fp)
    addi sp, sp, -28
    sw t0, 24(sp)
    sw t1, 20(sp)
    sw t2, 16(sp)
    sw t3, 12(sp)
    sw t4, 8(sp)
    sw t5, 4(sp)
    sw t6, 0(sp)
    addi sp, sp, -28
    sw t0, 24(sp)
    sw t1, 20(sp)
    sw t2, 16(sp)
    sw t3, 12(sp)
    sw t4, 8(sp)
    sw t5, 4(sp)
    sw t6, 0(sp)
    lw t0, -60(fp)
    li t1, 12
    rem t2, t0, t1
    mv a0, t2
    lw t3, -64(fp)
    li t4, 12
    rem t5, t3, t4
    mv a1, t5
    jal ra, gcd
    lw t0, 24(sp)
    lw t1, 20(sp)
    lw t2, 16(sp)
    lw t3, 12(sp)
    lw t4, 8(sp)
    lw t5, 4(sp)
    lw t6, 0(sp)
    addi sp, sp, 28
    mv a0, a0
    jal ra, factorial
    lw t0, 24(sp)
    lw t1, 20(sp)
    lw t2, 16(sp)
    lw t3, 12(sp)
    lw t4, 8(sp)
    lw t5, 4(sp)
    lw t6, 0(sp)
    addi sp, sp, 28
    addi sp, sp, -28
    sw t0, 24(sp)
    sw t1, 20(sp)
    sw t2, 16(sp)
    sw t3, 12(sp)
    sw t4, 8(sp)
    sw t5, 4(sp)
    sw t6, 0(sp)
    lw t6, -56(fp)
    li t0, 10
    rem t1, t6, t0
    li t2, 2
    add t3, t1, t2
    mv a0, t3
    jal ra, fibonacci
    lw t0, 24(sp)
    lw t1, 20(sp)
    lw t2, 16(sp)
    lw t3, 12(sp)
    lw t4, 8(sp)
    lw t5, 4(sp)
    lw t6, 0(sp)
    addi sp, sp, 28
    add t4, a0, a0
    sw t4, -68(fp)
    li t5, 0
    sw t5, -72(fp)
    lw t6, -56(fp)
    lw t0, -60(fp)
    slt t1, t0, t6
    lw t2, -56(fp)
    lw t3, -64(fp)
    slt t4, t3, t2
    sltu t0, zero, t1
    sltu t1, zero, t4
    and t5, t0, t1
    beq t5, zero, main_else6
    lw t6, -56(fp)
    sw t6, -72(fp)
    j main_endif7
main_else6:
    lw t0, -60(fp)
    lw t1, -56(fp)
    slt t2, t1, t0
    lw t3, -60(fp)
    lw t4, -64(fp)
    slt t5, t4, t3
    sltu t0, zero, t2
    sltu t1, zero, t5
    and t6, t0, t1
    beq t6, zero, main_else8
    lw t0, -60(fp)
    sw t0, -72(fp)
    j main_endif9
main_else8:
    lw t1, -64(fp)
    sw t1, -72(fp)
main_endif9:
main_endif7:
    li t2, 0
    sw t2, -76(fp)
    li t3, 1
    sw t3, -80(fp)
main_loop10:
    lw t4, -80(fp)
    li t5, 15
    slt t6, t5, t4
    xori t6, t6, 1
    beq t6, zero, main_endloop11
    lw t0, -80(fp)
    li t1, 2
    rem t2, t0, t1
    li t3, 0
    sub t4, t2, t3
    sltiu t4, t4, 1
    beq t4, zero, main_else12
    lw t5, -76(fp)
    lw t6, -80(fp)
    lw t0, -80(fp)
    mul t1, t6, t0
    add t2, t5, t1
    sw t2, -76(fp)
    j main_endif13
main_else12:
    lw t3, -80(fp)
    li t4, 5
    rem t5, t3, t4
    li t6, 0
    sub t0, t5, t6
    sltiu t0, t0, 1
    beq t0, zero, main_else14
    lw t1, -76(fp)
    lw t2, -80(fp)
    lw t3, -80(fp)
    mul t4, t2, t3
    lw t5, -80(fp)
    mul t6, t4, t5
    add t0, t1, t6
    sw t0, -76(fp)
    j main_endif15
main_else14:
    lw t1, -76(fp)
    lw t2, -80(fp)
    add t3, t1, t2
    sw t3, -76(fp)
main_endif15:
main_endif13:
    lw t4, -80(fp)
    li t5, 1
    add t6, t4, t5
    sw t6, -80(fp)
    j main_loop10
main_endloop11:
    li t0, 0
    sw t0, -84(fp)
    li t1, 1
    sw t1, -80(fp)
main_loop16:
    lw t2, -80(fp)
    li t3, 7
    slt t4, t3, t2
    xori t4, t4, 1
    beq t4, zero, main_endloop17
    li t5, 1
    sw t5, -88(fp)
    li t6, 1
    sw t6, -92(fp)
main_loop18:
    lw t0, -88(fp)
    lw t1, -80(fp)
    slt t2, t1, t0
    xori t2, t2, 1
    beq t2, zero, main_endloop19
    lw t3, -92(fp)
    lw t4, -88(fp)
    mul t5, t3, t4
    sw t5, -92(fp)
    lw t6, -88(fp)
    li t0, 1
    add t1, t6, t0
    sw t1, -88(fp)
    j main_loop18
main_endloop19:
    lw t2, -84(fp)
    lw t3, -92(fp)
    add t4, t2, t3
    sw t4, -84(fp)
    lw t5, -80(fp)
    li t6, 1
    add t0, t5, t6
    sw t0, -80(fp)
    j main_loop16
main_endloop17:
    li t1, 0
    sw t1, -88(fp)
    addi sp, sp, -28
    sw t0, 24(sp)
    sw t1, 20(sp)
    sw t2, 16(sp)
    sw t3, 12(sp)
    sw t4, 8(sp)
    sw t5, 4(sp)
    sw t6, 0(sp)
    lw t2, -56(fp)
    mv a0, t2
    jal ra, is_prime
    lw t0, 24(sp)
    lw t1, 20(sp)
    lw t2, 16(sp)
    lw t3, 12(sp)
    lw t4, 8(sp)
    lw t5, 4(sp)
    lw t6, 0(sp)
    addi sp, sp, 28
    beq a0, zero, main_else20
    addi sp, sp, -28
    sw t0, 24(sp)
    sw t1, 20(sp)
    sw t2, 16(sp)
    sw t3, 12(sp)
    sw t4, 8(sp)
    sw t5, 4(sp)
    sw t6, 0(sp)
    lw t3, -60(fp)
    mv a0, t3
    jal ra, is_prime
    lw t0, 24(sp)
    lw t1, 20(sp)
    lw t2, 16(sp)
    lw t3, 12(sp)
    lw t4, 8(sp)
    lw t5, 4(sp)
    lw t6, 0(sp)
    addi sp, sp, 28
    beq a0, zero, main_else22
    lw t4, -56(fp)
    lw t5, -60(fp)
    mul t6, t4, t5
    sw t6, -88(fp)
    j main_endif23
main_else22:
    addi sp, sp, -28
    sw t0, 24(sp)
    sw t1, 20(sp)
    sw t2, 16(sp)
    sw t3, 12(sp)
    sw t4, 8(sp)
    sw t5, 4(sp)
    sw t6, 0(sp)
    lw t0, -64(fp)
    mv a0, t0
    jal ra, is_prime
    lw t0, 24(sp)
    lw t1, 20(sp)
    lw t2, 16(sp)
    lw t3, 12(sp)
    lw t4, 8(sp)
    lw t5, 4(sp)
    lw t6, 0(sp)
    addi sp, sp, 28
    beq a0, zero, main_else24
    lw t1, -56(fp)
    lw t2, -64(fp)
    mul t3, t1, t2
    sw t3, -88(fp)
    j main_endif25
main_else24:
    lw t4, -56(fp)
    sw t4, -88(fp)
main_endif25:
main_endif23:
    j main_endif21
main_else20:
    addi sp, sp, -28
    sw t0, 24(sp)
    sw t1, 20(sp)
    sw t2, 16(sp)
    sw t3, 12(sp)
    sw t4, 8(sp)
    sw t5, 4(sp)
    sw t6, 0(sp)
    lw t5, -60(fp)
    mv a0, t5
    jal ra, is_prime
    lw t0, 24(sp)
    lw t1, 20(sp)
    lw t2, 16(sp)
    lw t3, 12(sp)
    lw t4, 8(sp)
    lw t5, 4(sp)
    lw t6, 0(sp)
    addi sp, sp, 28
    beq a0, zero, main_else26
    addi sp, sp, -28
    sw t0, 24(sp)
    sw t1, 20(sp)
    sw t2, 16(sp)
    sw t3, 12(sp)
    sw t4, 8(sp)
    sw t5, 4(sp)
    sw t6, 0(sp)
    lw t6, -64(fp)
    mv a0, t6
    jal ra, is_prime
    lw t0, 24(sp)
    lw t1, 20(sp)
    lw t2, 16(sp)
    lw t3, 12(sp)
    lw t4, 8(sp)
    lw t5, 4(sp)
    lw t6, 0(sp)
    addi sp, sp, 28
    beq a0, zero, main_else28
    lw t0, -60(fp)
    lw t1, -64(fp)
    mul t2, t0, t1
    sw t2, -88(fp)
    j main_endif29
main_else28:
    lw t3, -60(fp)
    sw t3, -88(fp)
main_endif29:
    j main_endif27
main_else26:
    addi sp, sp, -28
    sw t0, 24(sp)
    sw t1, 20(sp)
    sw t2, 16(sp)
    sw t3, 12(sp)
    sw t4, 8(sp)
    sw t5, 4(sp)
    sw t6, 0(sp)
    lw t4, -64(fp)
    mv a0, t4
    jal ra, is_prime
    lw t0, 24(sp)
    lw t1, 20(sp)
    lw t2, 16(sp)
    lw t3, 12(sp)
    lw t4, 8(sp)
    lw t5, 4(sp)
    lw t6, 0(sp)
    addi sp, sp, 28
    beq a0, zero, main_else30
    lw t5, -64(fp)
    sw t5, -88(fp)
    j main_endif31
main_else30:
    lw t6, -56(fp)
    lw t0, -60(fp)
    add t1, t6, t0
    lw t2, -64(fp)
    add t3, t1, t2
    sw t3, -88(fp)
main_endif31:
main_endif27:
main_endif21:
    li t4, 0
    sw t4, -92(fp)
    li t5, 16085975
    sw t5, -96(fp)
    li t6, 0
    sw t6, -100(fp)
main_loop32:
    lw t0, -96(fp)
    li t1, 0
    slt t2, t1, t0
    beq t2, zero, main_endloop33
    lw t3, -96(fp)
    li t4, 2
    rem t5, t3, t4
    li t6, 1
    sub t0, t5, t6
    sltiu t0, t0, 1
    beq t0, zero, main_else34
    lw t1, -100(fp)
    li t2, 1
    add t3, t1, t2
    sw t3, -100(fp)
    j main_endif35
main_else34:
main_endif35:
    lw t4, -96(fp)
    li t5, 2
    div t6, t4, t5
    sw t6, -96(fp)
    j main_loop32
main_endloop33:
    lw t0, -28(fp)
    lw t1, -44(fp)
    sub t2, t0, t1
    lw t3, -48(fp)
    sub t4, t2, t3
    lw t5, -52(fp)
    add t6, t4, t5
    lw t0, -68(fp)
    sub t1, t6, t0
    lw t2, -72(fp)
    add t3, t1, t2
    lw t4, -76(fp)
    sub t5, t3, t4
    lw t6, -84(fp)
    add t0, t5, t6
    lw t1, -88(fp)
    sub t2, t0, t1
    lw t3, -100(fp)
    sub t4, t2, t3
    sw t4, -104(fp)
    lw t5, -104(fp)
    mv a0, t5
    lw ra, 108(sp)
    lw fp, 104(sp)
    addi sp, sp, 112
    ret
