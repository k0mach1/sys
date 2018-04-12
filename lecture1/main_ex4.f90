PROGRAM main_ex4

IMPLICIT NONE

CHARACTER(10) :: c
CHARACTER(10) :: cc
CHARACTER(20) :: ccc

WRITE(6, *) 'c?'
READ(5, *) c
WRITE(6, *)

WRITE(6, *) 'cc?'
READ(5, *) cc
WRITE(6, *)

WRITE(6, *) 'c = ', c
WRITE(6, '(2A)') 'c = ', c
WRITE(6, '(A, 1X, A)') 'c = ', c
WRITE(6, '(A, 2X, A)') 'c = ', c
WRITE(6, *)

WRITE(6, *) 'cc(1)= ', cc(1), ', ', 'cc(2) = ', cc(2)
WRITE(6, '(2A)') 'cc = ', cc
WRITE(6, *)

ccc = c//cc

WRITE(6, '(2A)') 'ccc = ', ccc
WRITE(6, *)

ccc = TRIM(c)//TRIM(cc)

WRITE(6, '(2A)') 'ccc = ', ccc
WRITE(6, *)

STOP

END PROGRAM main_ex4
