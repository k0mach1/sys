MODULE mod_appli

  USE mod_vertices
  USE mod_tetrahedron

  IMPLICIT NONE

  TYPE(struct_vertices), POINTER :: vs
  TYPE(struct_tetrahedron), POINTER :: t

  CONTAINS

  SUBROUTINE start_appli()
    REAL(8) :: vs_x(3, 4)
    CHARACTER(1) :: dataname

    ALLOCATE( vs )
    ALLOCATE( t )

    CALL init_vertices(vs, 4)
    CALL init_tetrahedron(t, vs)

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

    CALL set_vertices_x(vs, vs_x)

  END SUBROUTINE start_appli

  SUBROUTINE run_appli()
    REAL(8) :: t_volume

    CALL cal_tetrahedron(t)
    CALL get_tetrahedron_volume(t, t_volume)

    WRITE(6, '(A, F0.8)') 'volume = ', t_volume
    WRITE(6, *)

    OPEN(90, FILE = 'output.dat')
    WRITE(90, '(A, F0.8)') 'volume = ', t_volume

    CLOSE(90)

    RETURN
  END SUBROUTINE run_appli

  SUBROUTINE finish_appli()
    CALL del_vertices(vs)
    CALL del_tetrahedron(t)

    DEALLOCATE( vs )
    DEALLOCATE( t )

    RETURN
  END SUBROUTINE finish_appli

END MODULE mod_appli
