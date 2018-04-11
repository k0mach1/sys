PROGRAM main_ex4

IMPLICIT NONE

CHARACTER(10) :: c
CHARACTER(10) :: cc
CHARACTER(20) :: ccc

WRITE(6, *) 'c?'
READ(5, *) c
WRITE(6, *)

WRITE(6, *) 'cc(1) and cc(2)?'
READ(5, *) cc(1), cc(2)
WRITE(6, *)

WRITE(6, *) 'c = ', c
WRITE(6, '(2A)') 'c = ', c
WRITE(6, '(A, 1X, A)') 'c = ', c
WRITE(6, '(A, 2X, A)') 'c = ', c
WRITE(6, *)

WRITE(6, *) 'cc(1)= ', cc(1), ', ', 'cc(2) = ', cc(2)
WRITE(6, '( (A, I6, 2A, I6) )') 'cc(1) = ', cc(1), ', ', 'cc(2) = ', cc(2)
WRITE(6, *)

ccc(1) = cc(1) + cc(2)
ccc(2) = cc(1) - cc(2)
ccc(3) = cc(1) * cc(2)
ccc(4) = cc(1) / cc(2)

WRITE(6, '(4(A, I8, A) )') 'ccc(1) = ', ccc(1), ', ', 'ccc(2) = ', ccc(2), ', ', 'ccc(3) = ', ccc(3), ', ', 'ccc(4) = ', ccc(4)
WRITE(6, *)

ccc(1) = c * ( cc(1)+cc(2) )
ccc(2) = c * cc(1) + cc(2)
ccc(3) = c * ( cc(1) * cc(2) )
ccc(4) = c * cc(1) * cc(2)

WRITE(6, '(4(A, I8, A) )') 'ccc(1) = ', ccc(1), ', ', 'ccc(2) = ', ccc(2), ', ', 'ccc(3) = ', ccc(3), ', ', 'ccc(4) = ', ccc(4)
WRITE(6, *)

STOP

END PROGRAM main_ex4