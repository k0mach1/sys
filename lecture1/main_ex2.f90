PROGRAM main_ex2

IMPLICIT NONE

INTEGER :: a
INTEGER :: aa(2)
INTEGER :: aaa(4)

WRITE(6, *) 'a?'
READ(5, *) a
WRITE(6, *)

WRITE(6, *) 'aa(1) and aa(2)?'
READ(5, *) aa(1), aa(2)
WRITE(6, *)

WRITE(6, *) 'a = ', a
WRITE(6, '(A, I6)') 'a = ', a
WRITE(6, '(A, I8)') 'a = ', a
WRITE(6, '(A, 1X, I8)') 'a = ', a
WRITE(6, '(A, 2X, I8)') 'a = ', a
WRITE(6, *)

WRITE(6, *) 'aa(1)= ', aa(1), ', ', 'aa(2) = ', aa(2)
WRITE(6, '( (A, I6, 2A, I6) )') 'aa(1) = ', aa(1), ', ', 'aa(2) = ', aa(2)
WRITE(6, *)

aaa(1) = aa(1) + aa(2)
aaa(2) = aa(1) - aa(2)
aaa(3) = aa(1) * aa(2)
aaa(4) = aa(1) / aa(2)

WRITE(6, '(4(A, I8, A) )') 'aaa(1) = ', aaa(1), ', ', 'aaa(2) = ', aaa(2), ', ', 'aaa(3) = ', aaa(3), ', ', 'aaa(4) = ', aaa(4)
WRITE(6, *)

aaa(1) = a * ( aa(1)+aa(2) )
aaa(2) = a * aa(1) + aa(2)
aaa(3) = a * ( aa(1) * aa(2) )
aaa(4) = a * aa(1) * aa(2)

WRITE(6, '(4(A, I8, A) )') 'aaa(1) = ', aaa(1), ', ', 'aaa(2) = ', aaa(2), ', ', 'aaa(3) = ', aaa(3), ', ', 'aaa(4) = ', aaa(4)
WRITE(6, *)

STOP

END PROGRAM main_ex2