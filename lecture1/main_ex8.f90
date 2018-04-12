PROGRAM main_ex8

IMPLICIT NONE

INTEGER :: n
INTEGER :: i, j
REAL(8), ALLOCATABLE :: f(:)
REAL(8) :: sum_f

n = 5

ALLOCATE( f(n) )

f = 0.0D0

f(1) = 1.0D0
f(2) = 1.0D1
f(3) = 1.0D2
f(4) = 1.0D3
f(5) = 1.0D4

CALL summation_f(n, f, sum_f) !memo: fortranのサブルーチンの引数は参照渡しなので、sum_fが書き換えられる

WRITE(6, '(A, F0.8)') 'sum_f = ', sum_f
WRITE(6, *)

CALL summation_g(n, f, sum_f)

WRITE(6, '(A, F0.8)') 'sum_f = ', sum_f
WRITE(6, *)

DEALLOCATE( f )

STOP

END PROGRAM main_ex8
