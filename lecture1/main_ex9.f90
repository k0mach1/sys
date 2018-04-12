!theme: 四面体の体積を算出するプログラム
PROGRAM main_ex9

IMPLICIT NONE

REAL(8) :: x(4), y(4), z(4)
REAL(8) :: volume
REAL(8) :: ax, ay, az
REAL(8) :: bx, by, bz
REAL(8) :: cx, cy, cz
REAL(8) :: dx, dy, dz
CHARACTER :: dataname

OPEN(10, FILE = 'input.dat')
READ(10, *) dataname
READ(10, *) x(1), y(1), z(1)
READ(10, *) dataname
READ(10, *) x(2), y(2), z(2)
READ(10, *) dataname
READ(10, *) x(3), y(3), z(3)
READ(10, *) dataname
READ(10, *) x(4), y(4), z(4)
CLOSE(10)

ax = x(2) - x(1)
ay = y(2) - y(1)
az = z(2) - z(1)

bx = x(3) - x(1)
by = y(3) - y(1)
bz = z(3) - z(1)

cx = x(4) - x(1)
cy = y(4) - y(1)
cz = z(4) - z(1)

dx = ay * bz - az * by
dy = az * bx - ax * bz
dz = ax * by - ay * bx

volume = ( 1.0D0 / 6.0D0 ) * ( cx * dx + cy * dy + cz * dz )

WRITE(6, '(A, F0.8)') 'volume = ', volume
WRITE(6, *)

OPEN(90, FILE = 'output.dat')
WRITE(90, '(A, F0.8)') 'volume = ', volume
CLOSE(90)

STOP

END PROGRAM main_ex9
