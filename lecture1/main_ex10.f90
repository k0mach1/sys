PROGRAM main_ex10

IMPLICIT NONE

TYPE :: struct_vertices
  !memo: 頂点群を表すデータ型

  INTEGER :: n
  REAL(8) , ALLOCATABLE :: x(:), y(:), z(:)
END TYPE struct_vertices

TYPE :: struct_tetrahedron
  !memo: 四面体を表すデータ型

  REAL(8) :: volume
END TYPE struct_tetrahedron

TYPE(struct_vertices) :: vs
TYPE(struct_tetrahedron) :: t
REAL(8) :: ax, ay, az
REAL(8) :: bx, by, bz
REAL(8) :: cx, cy, cz
REAL(8) :: dx, dy, dz
CHARACTER :: dataname

vs%n = 4 !memo: 各プロパティへのアクセスは % で行う

ALLOCATE( vs%x(4) )
ALLOCATE( vs%y(4) )
ALLOCATE( vs%z(4) )

vs%x = 0.0D0
vs%y = 0.0D0
vs%z = 0.0D0

t%volume = 0.0D0

OPEN(10, FILE = 'input.dat')
READ(10, *) dataname
READ(10, *) vs%x(1), vs%y(1), vs%z(1)
READ(10, *) dataname
READ(10, *) vs%x(2), vs%y(2), vs%z(2)
READ(10, *) dataname
READ(10, *) vs%x(3), vs%y(3), vs%z(3)
READ(10, *) dataname
READ(10, *) vs%x(4), vs%y(4), vs%z(4)
CLOSE(10)

ax = vs%x(2) - vs%x(1)
ay = vs%y(2) - vs%y(1)
az = vs%z(2) - vs%z(1)

bx = vs%x(3) - vs%x(1)
by = vs%y(3) - vs%y(1)
bz = vs%z(3) - vs%z(1)

cx = vs%x(4) - vs%x(1)
cy = vs%y(4) - vs%y(1)
cz = vs%z(4) - vs%z(1)

dx = ay * bz - az * by
dy = az * bx - ax * bz
dz = ax * by - ay * bx

t%volume = ( 1.0D0 / 6.0D0 ) * ( cx * dx + cy * dy + cz * dz )

WRITE(6, '(A, F0.8)') 'volume = ', t%volume
WRITE(6, *)

OPEN(90, FILE = 'output.dat')
WRITE(90, '(A, F0.8)') 'volume = ', t%volume
CLOSE(90)

STOP

END PROGRAM main_ex10
