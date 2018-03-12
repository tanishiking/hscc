# hscc
## Haskell Small C Compiler written
## Target language: MIPS

### Usage
1. `stack build`
2. `stack exec hscc [example.sc]`

OR

1. `cabal build`
2. `cabal install`
2. `./hscc [example.sc]`

hscc print compiled mips language in standard output.

### Example
`fib.sc`
```c
void main();
int fib(int n);
int fib(int n) {
  if(n < 2)
    return n;
  else
    return fib(n-1) + fib(n-2);
}

void main() {
  print(fib(24) == 46368);
}
```

`stack exec hscc fib.sc` write the following MIPS code to stdout
```
    .text
    .globl main
fib:
    move $t0, $sp
    addi $sp, $sp, -48
    sw $fp, 0($sp)
    sw $ra, 4($sp)
    addi $fp, $t0, -4
    li $t0, 2
    sw $t0, -8($fp)
    lw $t1, 0($fp)
    lw $t2, -8($fp)
    slt $t0, $t1, $t2
    sw $t0, -4($fp)
    lw $t0, -4($fp)
    beq $t0, $zero, else1
    lw $v0, 0($fp)
    j finally0
else1:
    li $t0, 1
    sw $t0, -24($fp)
    lw $t1, 0($fp)
    lw $t2, -24($fp)
    sub $t0, $t1, $t2
    sw $t0, -20($fp)
    lw $t0, -20($fp)
    sw $t0, -4($sp)
    jal fib
    sw $v0, -16($fp)
    li $t0, 2
    sw $t0, -36($fp)
    lw $t1, 0($fp)
    lw $t2, -36($fp)
    sub $t0, $t1, $t2
    sw $t0, -32($fp)
    lw $t0, -32($fp)
    sw $t0, -4($sp)
    jal fib
    sw $v0, -28($fp)
    lw $t1, -16($fp)
    lw $t2, -28($fp)
    add $t0, $t1, $t2
    sw $t0, -12($fp)
    lw $v0, -12($fp)
finally0:
    lw $ra, 4($sp)
    lw $fp, 0($sp)
    addi $sp, $sp, 48
    jr $ra
main:
    move $t0, $sp
    addi $sp, $sp, -24
    sw $fp, 0($sp)
    sw $ra, 4($sp)
    addi $fp, $t0, 0
    li $t0, 24
    sw $t0, -12($fp)
    lw $t0, -12($fp)
    sw $t0, -4($sp)
    jal fib
    sw $v0, -8($fp)
    li $t0, 46368
    sw $t0, -16($fp)
    lw $t1, -8($fp)
    lw $t2, -16($fp)
    beq $t1, $t2, true2
    li $t0, 0
    j false3
true2:
    li $t0, 1
false3:
    sw $t0, -4($fp)
    li $v0, 1
    lw $a0, -4($fp)
    syscall
    lw $ra, 4($sp)
    lw $fp, 0($sp)
    addi $sp, $sp, 24
    jr $ra
```
