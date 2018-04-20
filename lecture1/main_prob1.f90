PROGRAM main_prob1

IMPLICIT NONE

Integer :: info, lda, ldb, n, i
REAL, Allocatable :: a(:, :), b(:)
Integer, Allocatable :: ipiv(:)
CHARACTER :: dataname

! ここも動的に変えたい
n = 5
lda = n
ldb = n

ALLOCATE(a(lda, n), b(ldb), ipiv(n))

OPEN(10, FILE = 'input2.dat')
READ(10, *) dataname
READ(10, *) a(0, 0), a(0,1), a(0,2), a(0,3), a(0,4)
READ(10, *) dataname
READ(10, *) a(1,0), a(1,1), a(1,2), a(1,3), a(1,4)
READ(10, *) dataname
READ(10, *) a(2,0), a(2,1), a(2,2), a(2,3), a(2,4)
READ(10, *) dataname
READ(10, *) a(3,0), a(3,1), a(3,2), a(3,3), a(3,4)
READ(10, *) dataname
READ(10, *) a(4,0), a(4,1), a(4,2), a(4,3), a(4,4)
READ(10, *) dataname
READ(10, *) b(0), b(1), b(2), b(3), b(4)
CLOSE(10)

CALL dgesv(n, 1, a, lda, ipiv, b, ldb, info)

IF (info .EQ. 0) THEN
  DO i = 1, n
    WRITE(6, *) i, b(i)
  END DO
END IF

END PROGRAM main_prob1
