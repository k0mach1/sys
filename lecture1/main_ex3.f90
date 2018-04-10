PROGRAM main_ex3

IMPLICIT NONE

REAL(8) :: b
REAL(8) :: bb(2)
REAL(8) :: bbb(4)

WRITE(6, *) 'b?'
READ(5, *) b
WRITE(6, *)

WRITE(6, *) 'bb(1) and bb(2)?'
READ(5, *) bb(1), bb(2)
WRITE(6, *)

WRITE(6, *) 'b = ', b
WRITE(6, '(A, F0.6)') 'b = ', b
WRITE(6, '(A, F0.8)') 'b = ', b
WRITE(6, '(A, 1X, F0.8)') 'b = ', b
WRITE(6, '(A, 2X, F0.8)') 'b = ', b
WRITE(6, *)

WRITE(6, *) 'bb(1)= ', bb(1), ', ', 'bb(2) = ', bb(2)
WRITE(6, '( (A, F0.8, 2A, F0.8) )') 'bb(1) = ', bb(1), ', ', 'bb(2) = ', bb(2)
WRITE(6, *)

bbb(1) = bb(1) + bb(2)
bbb(2) = bb(1) - bb(2)
bbb(3) = bb(1) * bb(2)
bbb(4) = bb(1) / bb(2)

WRITE(6, '(4(A, F0.8, A) )') 'bbb(1) = ', bbb(1), ', ', 'bbb(2) = ', bbb(2), ', ', 'bbb(3) = ', bbb(3), ', ', 'bbb(4) = ', bbb(4)
WRITE(6, *)

bbb(1) = b * ( bb(1)+bb(2) )
bbb(2) = b * bb(1) + bb(2)
bbb(3) = b * ( bb(1) * bb(2) )
bbb(4) = b * bb(1) * bb(2)

WRITE(6, '(4(A, F0.8, A) )') 'bbb(1) = ', bbb(1), ', ', 'bbb(2) = ', bbb(2), ', ', 'bbb(3) = ', bbb(3), ', ', 'bbb(4) = ', bbb(4)
WRITE(6, *)

STOP

END PROGRAM main_ex3