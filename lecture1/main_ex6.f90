PROGRAM main_ex6

IMPLICIT NONE

INTEGER :: n
INTEGER :: i, j
REAL(8), ALLOCATABLE :: e(:)
REAL(8) :: sum_e

n = 5

ALLOCATE( e(n) )

e = 0.0D0

DO i = 1, n
  e(i) = DFLOAT(i) !memo: INTEGER => REAL(8) にキャスト
END DO

sum_e = 0.0D0

DO i = 1, n
  sum_e = sum_e + e(i)
END DO

WRITE(6, '(A, F0.8)') 'sum_e = ', sum_e
WRITE(6, *)

j = 0
sum_e = 0.0D0

DO WHILE( j .LT. 3) !memo: LT は < と等価
  j = j + 1
  sum_e = sum_e + e(j)
END DO

WRITE(6, '(A, F0.8)') 'sum_e = ', sum_e
WRITE(6, *)

DEALLOCATE( e )

STOP

END PROGRAM main_ex6
