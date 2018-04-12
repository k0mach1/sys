PROGRAM main_ex7

IMPLICIT NONE

INTEGER :: i, j
REAL(8) :: sum_1

sum_1 = 0.0D0

DO i = 1, 100
  sum_1 = sum_1 + 1.0D0
END DO

WRITE(6, '(A, F0.8)') 'sum_1  = ', sum_1
WRITE(6, *)

j = 0
sum_1 = 0.0D0

DO i = 1, 1000
  j = j + 1
  sum_1 = sum_1 + 1.0D0

  IF( j .EQ. 200 ) THEN
    EXIT
  END IF
END DO

WRITE(6, '(A, F0.8)') 'sum_1 = ', sum_1
WRITE(6, *)

j = 0
sum_1 = 0.0D0

DO
  j = j + 1
  sum_1 = sum_1 + 1.0D0

  IF( j .EQ. 300 ) THEN
    EXIT
  END IF
END DO

WRITE(6, '(A, F0.8)') 'sum_1 = ', sum_1
WRITE(6, *)

j = 0
sum_1 = 0.0D0

DO
  j = j + 1
  sum_1 = sum_1 + 1.0D0

  IF( j .EQ. 5 ) THEN
    WRITE(6, '(3A, F0.8)') 'j =   5', ',', 'sum_1 = ', sum_1
  ELSE IF(j .EQ. 10) THEN
    WRITE(6, '(3A, F0.8)') 'j =  10', ',', 'sum_1 = ', sum_1
    WRITE(6, *)
    EXIT
  ELSE
    WRITE(6, '(A, I3, 2A, F0.8)') 'j = ', j, ',', 'sum_1 = ', sum_1
  END IF
END DO

STOP

END PROGRAM main_ex7
