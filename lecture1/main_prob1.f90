PROGRAM main_prob1

IMPLICIT NONE

Integer :: info, lda, ldb, n, i
REAL(8), Allocatable :: a(:, :), b(:)
Integer, Allocatable :: ipiv(:)
CHARACTER :: dataname

! ここも動的に変えたい
n = 5
lda = n
ldb = n

ALLOCATE(a(lda, n), b(ldb), ipiv(n))

OPEN(10, FILE = 'input2.dat')
READ(10, *) dataname
READ(10, *) a(1,1), a(1,2), a(1,3), a(1,4), a(1,5)
READ(10, *) dataname
READ(10, *) a(2,1), a(2,2), a(2,3), a(2,4), a(2,5)
READ(10, *) dataname
READ(10, *) a(3,1), a(3,2), a(3,3), a(3,4), a(3,5)
READ(10, *) dataname
READ(10, *) a(4,1), a(4,2), a(4,3), a(4,4), a(4,5)
READ(10, *) dataname
READ(10, *) a(5,1), a(5,2), a(5,3), a(5,4), a(5,5)
READ(10, *) dataname
READ(10, *) b(1), b(2), b(3), b(4), b(5)
CLOSE(10)

CALL dgesv(n, 1, a, lda, ipiv, b, ldb, info)

IF (info .EQ. 0) THEN
  DO i = 1, n
    WRITE(6, *) i, b(i)
  END DO
END IF

END PROGRAM main_prob1
