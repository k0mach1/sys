MODULE mod_vertices
  IMPLICIT NONE

  TYPE :: struct_vertices

    PRIVATE
    INTEGER :: n
    REAL(8), ALLOCATABLE :: x(:, :)

  END TYPE struct_vertices

  CONTAINS

  SUBROUTINE set_vertices_n(vs, n)
    TYPE(struct_vertices), POINTER, INTENT(OUT) ::vs

    INTEGER, INTENT(n) :: n

    vs%n = n

    RETURN
  END SUBROUTINE set_vertices_n

  SUBROUTINE get_vertices_n(vs, n)
    TYPE(struct_vertices), POINTER, INTENT(n) :: vs

    INTEGER, INTENT(OUT) :: n

    n = vs%n

    RERURN
  END SUBROUTINE get_vertices_n

  SUBROUTINE set_vertices_x(vs, x)
    TYPE(struct_vertices), POINTER, INTENT(INOUT) :: vs

    REAL(8), INTENT(IN) :: x(3, vs%n)

    INTEGER :: i
    INTEGER :: id

    DO id = 1, vs%n
      DO i = 1, 3
        vs%x(i, id) = x(i, id)
      END DO
    END DO

    RERURN
  END SUBROUTINE set_vertices_x

  SUBROUTINE get_vertices_x(vs, x)
    TYPE(struct_vertices), POINTER, INTENT(IN) :: vs

    REAL(8), INTENT(OUT) :: x(3, vs%n)

    INTEGER :: i
    INTEGER :: id

    DO id = 1, vs%n
      DO i = 1, 3
        x(i, id) = vs%x(i, id)
      END DO
    END DO

    RETURN
  END SUBROUTINE get_vertices_x

  SUBROUTINE init_vertices(vs, n)
    TYPE(struct_vertices), POINTER, INTENT(INOUT) :: vs

    INTEGER, INTENT(IN) :: n

    vs%n = n

    ALLOCATE( vs%x(3, n) )

    vs%x = 0.0D0

    RETURN
  END SUBROUTINE init_vertices

  SUBROUTINE del_vertices(vs)
    TYPE(struct_vertices), POINTER, INTENT(INOUT) :: vs

    IF (vs%n .EQ. 0) THEN
      RETURN
    END IF

    vs%n = 0

    DEALLOCATE( vs%n )

    RETURN
  END SUBROUTINE del_vertices

END MODULE mod_vertices
