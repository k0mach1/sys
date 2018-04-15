MODULE mod_tetrahedron
  USE mod_vertices

  IMPLICIT NONE

  TYPE :: struct_tetrahedron

    PRIVATE
    TYPE(struct_vertices), POINTER :: vs => NULL()
    REAL(8) :: volume

  END TYPE struct_tetrahedron

  CONTAINS

  SUBROUTINE set_tetrahedron_volume(v, volume)
    TYPE(struct_tetrahedron), POINTER, INTENT(OUT) :: t

    REAL(8), INTENT(IN) :: volume

    t%volume = volume

    RETURN
  END SUBROUTINE

  SUBROUTINE get_tetrahedron_volume(t, volume)
    TYPE(struct_tetrahedron), POINTER, INTENT(OUT) :: t

    REAL(8), INTENT(OUT) :: volume

    volume = t%volume

    RETURN
  END SUBROUTINE get_tetrahedron_volume

  SUBROUTINE init_tetrahedron(t, vs)
    TYPE(struct_tetrahedron), POINTER, INTENT(OUT) :: t

    TYPE(struct_vertices) ,POINTER, INTENT(IN) :: vs

    t%volume = 0.0D0

    RETURN
  END SUBROUTINE init_tetrahedron

  SUBROUTINE cal_tetrahedron(t)
    TYPE(struct_tetrahedron), POINTER, INTENT(INOUT) :: t

    REAL(8) :: vs_x(3, 4)
    REAL(8) :: ax, ay, az
    REAL(8) :: bx, by, bz
    REAL(8) :: cx, cy, cz
    REAL(8) :: dx, dy, dz

    CALL get_vertices_x(t%vs, vs_x)

    ax = vs_x(1, 2) - vs_x(1, 1)
    ay = vs_x(2, 2) - vs_x(2, 1)
    az = vs_x(3, 2) - vs_x(3, 1)

    bx = vs_x(1, 3) - vs_x(1, 1)
    by = vs_x(2, 3) - vs_x(2, 1)
    bz = vs_x(3, 3) - vs_x(3, 1)

    cx = vs_x(1, 4) - vs_x(1, 1)
    cy = vs_x(2, 4) - vs_x(2, 1)
    cz = vs_x(3, 4) - vs_x(3, 1)

    dx = ay * bz - az * by
    dy = az * bx - ax * bz
    dz = ax * by - ay * bx

    t%volume = ( 1.0D0 / 6.0D0 ) * ( cx * dx + cy * dy + cz * dz )

    RETURN
  END SUBROUTINE cal_tetrahedron

  SUBROUTINE del_tetrahedron(t)
    TYPE(struct_tetrahedron), POINTER, INTENT(INOUT) :: t

    NULLIFY(t%vs)

    t%volume = 0.0D0

    RETURN
  END SUBROUTINE del_tetrahedron

END MODULE mod_tetrahedron
