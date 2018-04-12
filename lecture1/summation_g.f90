SUBROUTINE summation_g(n, g, sum_g)

  IMPLICIT NONE

  INTEGER :: n
  REAL(8) :: g(n)
  REAL(8) :: sum_g

  INTEGER :: i

  sum_g = 0.0D0

  DO i = 1, n
    sum_g = sum_g+ g(i)
  END DO

  RETURN

END SUBROUTINE summation_g
