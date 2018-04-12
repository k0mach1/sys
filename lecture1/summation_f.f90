SUBROUTINE summation_f(n, f, sum_f) !memo: サブルーチンを定義

  IMPLICIT NONE

  INTEGER :: n
  REAL(8) :: f(n)
  REAL(8) :: sum_f

  INTEGER :: i

  sum_f = 0.0D0

  DO i = 1, n
    sum_f = sum_f + f(i)
  END DO

  RETURN

END SUBROUTINE summation_f
