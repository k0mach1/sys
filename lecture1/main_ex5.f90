PROGRAM main_ex5

IMPLICIT NONE

INTEGER :: n
REAL(8), ALLOCATABLE :: d(:) !memo: 配列の大きさがわからない場合など、プログラムの実行時に 配列の領域を確保したい場合に宣言
REAL(8), ALLOCATABLE :: dd(:, :)
INTEGER :: size_d
INTEGER :: size_dd

WRITE(6, *) 'n?'
READ(5, *) n
WRITE(6, *)

ALLOCATE( d(n) ) !memo: 配列の動的な割付を行う

d = 0.0D0 !memo: 倍精度実数の0

ALLOCATE( dd(n, n))

dd = 0.0D0

size_d = SIZE( d )
size_dd = SIZE( dd )

WRITE(6, '(A, I8)') 'size_d = ', size_d
WRITE(6, '(A, I8)') 'size_dd = ', size_dd
WRITE(6, *)

DEALLOCATE( d ) !memo: 明示的に配列の領域を解放する
DEALLOCATE( dd )

STOP

END PROGRAM main_ex5
