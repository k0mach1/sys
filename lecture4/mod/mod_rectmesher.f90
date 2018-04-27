MODULE mod_rectmesher3d
!####################################################################

USE mod_nodes3d
USE mod_localelement3d
USE mod_elements3d

!--------------------------------------------------------------------

IMPLICIT NONE

!--------------------------------------------------------------------

TYPE :: struct_rectmesher3d

!--------------------------------------------------------

PRIVATE

!--------------------------------------------------------

TYPE(struct_nodes3d), POINTER        :: ns3d => NULL()
TYPE(struct_localelement3d), POINTER :: le3d => NULL()
TYPE(struct_elements3d), POINTER     :: es3d => NULL()

!--------------------------------------------------------
!
! n_x(3)
! The total number of divisions
! n_x, n_y, n_z
!
! x_start(3)
! Cartesian coordinates of the start point
! (x_start, y_start, z_start)
!
! x_end(3)
! Cartesian coordinates of the end point
! (x_end, y_end, z_end)
!
! x_center(3)
! Cartesian coordinates of the center point
! (x_center, y_center, z_centr)
!
! length_x(3)
! Distance between the start and end points
!
!--------------------------------------------------------

INTEGER :: n_x(3)

REAL(8) :: x_start(3)
REAL(8) :: x_end(3)
REAL(8) :: x_center(3)
REAL(8) :: length_x(3)

!--------------------------------------------------------

END TYPE struct_rectmesher3d

!--------------------------------------------------------------------

PRIVATE :: cal_rectmesher3d_connectivity_hex
PRIVATE :: cal_rectmesher3d_coordinates_hex

!--------------------------------------------------------------------

CONTAINS

! Get the total number of divisions
!####################################################################
SUBROUTINE get_rectmesher3d_n(rm3d, n)
!####################################################################

  TYPE(struct_rectmesher3d), INTENT(INOUT) :: rm3d

  INTEGER, INTENT(OUT) :: n(3)

!--------------------------------------------------------------------

  n(1) = rm3d%n_x(1)
  n(2) = rm3d%n_x(2)
  n(3) = rm3d%n_x(3)

!--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE get_rectmesher3d_n
!####################################################################

! Get Cartesian coordinates of the start and end points
!####################################################################
SUBROUTINE get_rectmesher3d_x_start_x_end (rm3d, x_start, x_end, x_center, length_x)

  TYPE(struct_rectmesher3d), INTENT(IN) :: rm3d

  REAL(8), INTENT(OUT) :: x_start(3)
  REAL(8), INTENT(OUT) :: x_end(3)
  REAL(8), INTENT(OUT) :: x_center(3)
  REAL(8), INTENT(OUT) :: length_x(3)

!--------------------------------------------------------------------

  x_start(1) = rm3d%x_start(1)
  x_start(2) = rm3d%x_start(2)
  x_start(3) = rm3d%x_start(3)

  x_end(1) = rm3d%x_end(1)
  x_end(2) = rm3d%x_end(2)
  x_end(3) = rm3d%x_end(3)

  x_center(1) = rm3d%x_center(1)
  x_center(2) = rm3d%x_center(2)
  x_center(3) = rm3d%x_center(3)

  length_x(1) = rm3d%length_x(1)
  length_x(2) = rm3d%length_x(2)
  length_x(3) = rm3d%length_x(3)

!--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE get_rectmesher3d_x_start_x_end
!####################################################################


! Initialize rectmesher3d
!####################################################################
SUBROUTINE init_rectmesher3d (rm3d, ns3d, le3d, es3d, n_x, x_start, x_end)
!####################################################################

  TYPE(struct_rectmesher3d), INTENT(INOUT) :: rm3d

  TYPE(struct_nodes3d), TARGET, INTENT(INOUT)        :: ns3d
  TYPE(struct_localelement3d), TARGET, INTENT(INOUT) :: le3d
  TYPE(struct_elements3d), TARGET, INTENT(INOUT)     :: es3d

  INTEGER, INTENT(IN) :: n_x(3)
  REAL(8), INTENT(IN) :: x_start(3)
  REAL(8), INTENT(IN) :: x_end(3)

!--------------------------------------------------------------------

  INTEGER :: ns3d_n
  INTEGER :: le3d_nboundaries
  INTEGER :: le3d_nnodes
  INTEGER :: es3d_n

!--------------------------------------------------------------------

  rm3d%ns3d => ns3d
  rm3d%le3d => le3d
  rm3d%es3d => es3d

!--------------------------------------------------------------------

  rm3d%n_x(1) = n_x(1)
  rm3d%n_x(2) = n_x(2)
  rm3d%n_x(3) = n_x(3)

  rm3d%x_start(1) = x_start(1)
  rm3d%x_start(2) = x_start(2)
  rm3d%x_start(3) = x_start(3)

  rm3d%x_end(1) = x_end(1)
  rm3d%x_end(2) = x_end(2)
  rm3d%x_end(3) = x_end(3)

  rm3d%x_center(1) = 0.5D0*( x_start(1)+x_end(1) )
  rm3d%x_center(2) = 0.5D0*( x_start(2)+x_end(2) )
  rm3d%x_center(3) = 0.5D0*( x_start(3)+x_end(3) )

  rm3d%length_x(1) = x_end(1)-x_start(1)
  rm3d%length_x(2) = x_end(2)-x_start(2)
  rm3d%length_x(3) = x_end(3)-x_start(3)

!--------------------------------------------------------------------

  ns3d_n = ( rm3d%n_x(1)+1 )*( rm3d%n_x(2)+1 )*( rm3d%n_x(3)+1 )

  le3d_nboundaries = 6
  le3d_nnodes      = 8

  es3d_n = rm3d%n_x(1)*rm3d%n_x(2)*rm3d%n_x(3)

!--------------------------------------------------------------------

  CALL set_nodes3d_n(ns3d, ns3d_n)

  CALL set_elements3d_n(es3d, es3d_n)

  CALL set_localelement3d_nboundaries(le3d, le3d_nboundaries)
  CALL set_localelement3d_nnodes(le3d, le3d_nnodes)

!--------------------------------------------------------------------

  WRITE( 6, '( (A, I8) )' ) 'The total number of nodes: ', ns3d_n
  WRITE( 6, '( (A, I8) )' ) 'The total number of local element boundaries: ', le3d_nboundaries
  WRITE( 6, '( (A, I8) )' ) 'The total number of local element nodes: ', le3d_nnodes
  WRITE( 6, '( (A, I8) )' ) 'The total number of elements: ', es3d_n
  WRITE(6, *)

!--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE init_rectmesher3d
!####################################################################


! Calculate rectmesher3d
!####################################################################
SUBROUTINE cal_rectmesher3d(rm3d)
!####################################################################

  TYPE(struct_rectmesher3d), INTENT(INOUT) :: rm3d

!--------------------------------------------------------------------

  CALL cal_rectmesher3d_connectivity_hex(rm3d)

  CALL cal_rectmesher3d_coordinates_hex(rm3d)

!--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE cal_rectmesher3d
!####################################################################


! Calculate rectmesher3d (connectivity, hexahedral elements)
!####################################################################
SUBROUTINE cal_rectmesher3d_connectivity_hex(rm3d)
!####################################################################

  TYPE(struct_rectmesher3d), INTENT(INOUT) :: rm3d

!--------------------------------------------------------------------

  INTEGER :: le3d_nnodes
  INTEGER :: es3d_n
  INTEGER, ALLOCATABLE :: es3d_connectivity(:, :)
  INTEGER :: n_x_1(3)
  INTEGER :: i, j, k
  INTEGER :: ie

!--------------------------------------------------------------------

  CALL get_localelement3d_nnodes(rm3d%le3d, le3d_nnodes)

  CALL get_elements3d_n(rm3d%es3d, es3d_n)
  ALLOCATE( es3d_connectivity(le3d_nnodes, es3d_n) )

!--------------------------------------------------------------------

  n_x_1(1) = rm3d%n_x(1)+1
  n_x_1(2) = rm3d%n_x(2)+1
  n_x_1(3) = rm3d%n_x(3)+1

!--------------------------------------------------------------------

  ie = 0

!--------------------------------------------------------------

  DO k = 1, rm3d%n_x(3)

    DO j = 1, rm3d%n_x(2)

      DO i = 1, rm3d%n_x(1)

        !------------------------------------------------------

        ! Element no.
        ie = ie+1

        IF( le3d_nnodes .EQ. 8 ) THEN

          es3d_connectivity(1, ie) = n_x_1(1)*n_x_1(2)*( k-1 )+n_x_1(1)*( j-1 )+i
          es3d_connectivity(2, ie) = es3d_connectivity(1, ie)+1
          es3d_connectivity(4, ie) = es3d_connectivity(1, ie)+n_x_1(1)
          es3d_connectivity(3, ie) = es3d_connectivity(4, ie)+1
          es3d_connectivity(5, ie) = es3d_connectivity(1, ie)+n_x_1(1)*n_x_1(2)
          es3d_connectivity(6, ie) = es3d_connectivity(5, ie)+1
          es3d_connectivity(8, ie) = es3d_connectivity(5, ie)+n_x_1(1)
          es3d_connectivity(7, ie) = es3d_connectivity(8, ie)+1

        END IF

        !------------------------------------------------------

      END DO

    END DO

  END DO

!--------------------------------------------------------------

  CALL set_elements3d_connectivity(rm3d%es3d, es3d_connectivity)

!--------------------------------------------------------------------

  DEALLOCATE( es3d_connectivity )

!--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE cal_rectmesher3d_connectivity_hex
!####################################################################


! Calculate rectmesher3d (coordinates, hexahedral elements)
!####################################################################
SUBROUTINE cal_rectmesher3d_coordinates_hex(rm3d)
!####################################################################

  TYPE(struct_rectmesher3d), INTENT(INOUT) :: rm3d

!--------------------------------------------------------------------

  INTEGER :: ns3d_n
  INTEGER :: le3d_nnodes
  INTEGER :: es3d_n
  INTEGER, ALLOCATABLE :: es3d_connectivity(:, :)
  INTEGER :: i, j, k
  INTEGER :: id
  INTEGER :: na
  INTEGER :: ie

  REAL(8), ALLOCATABLE :: ns3d_x(:, :)
  REAL(8) :: x_vertex(3, 8)
  REAL(8), ALLOCATABLE :: x_local(:, :)
  REAL(8) :: delta_x(3)

!--------------------------------------------------------------------

  CALL get_nodes3d_n(rm3d%ns3d, ns3d_n)
  ALLOCATE( ns3d_x(3, ns3d_n) )

  CALL get_localelement3d_nnodes(rm3d%le3d, le3d_nnodes)

  CALL get_elements3d_n(rm3d%es3d, es3d_n)
  ALLOCATE( es3d_connectivity(le3d_nnodes, es3d_n) )
  CALL get_elements3d_connectivity(rm3d%es3d, es3d_connectivity)

  ALLOCATE( x_local(3, le3d_nnodes) )

!--------------------------------------------------------------------

  delta_x(1) = ( rm3d%x_end(1)-rm3d%x_start(1) )/DFLOAT( rm3d%n_x(1) )
  delta_x(2) = ( rm3d%x_end(2)-rm3d%x_start(2) )/DFLOAT( rm3d%n_x(2) )
  delta_x(3) = ( rm3d%x_end(3)-rm3d%x_start(3) )/DFLOAT( rm3d%n_x(3) )

!--------------------------------------------------------------------

  ie = 0

!--------------------------------------------------------------

  ! Coordinates of Vertex 1
  x_vertex(1, 1) = rm3d%x_start(1)
  x_vertex(2, 1) = rm3d%x_start(2)
  x_vertex(3, 1) = rm3d%x_start(3)

!--------------------------------------------------------------

  DO k = 1, rm3d%n_x(3)

  !--------------------------------------------------------1

    DO j = 1, rm3d%n_x(2)

      !--------------------------------------------------

      DO i = 1, rm3d%n_x(1)

        !--------------------------------------------

        ! Coordinates of Vertex 2
        x_vertex(1, 2) = x_vertex(1, 1)+delta_x(1)
        x_vertex(2, 2) = x_vertex(2, 1)
        x_vertex(3, 2) = x_vertex(3, 1)

        ! Coordinates of Vertex 3
        x_vertex(1, 3) = x_vertex(1, 2)
        x_vertex(2, 3) = x_vertex(2, 2)+delta_x(2)
        x_vertex(3, 3) = x_vertex(3, 2)

        ! Coordinates of Vertex 4
        x_vertex(1, 4) = x_vertex(1, 1)
        x_vertex(2, 4) = x_vertex(2, 1)+delta_x(2)
        x_vertex(3, 4) = x_vertex(3, 1)

        ! Coordinates of Vertex 5
        x_vertex(1, 5) = x_vertex(1, 1)
        x_vertex(2, 5) = x_vertex(2, 1)
        x_vertex(3, 5) = x_vertex(3, 1)+delta_x(3)

        ! Coordinates of Vertex 6
        x_vertex(1, 6) = x_vertex(1, 5)+delta_x(1)
        x_vertex(2, 6) = x_vertex(2, 5)
        x_vertex(3, 6) = x_vertex(3, 5)

        ! Coordinates of Vertex 7
        x_vertex(1, 7) = x_vertex(1, 6)
        x_vertex(2, 7) = x_vertex(2, 6)+delta_x(2)
        x_vertex(3, 7) = x_vertex(3, 6)

        ! Coordinates of Vertex 8
        x_vertex(1, 8) = x_vertex(1, 5)
        x_vertex(2, 8) = x_vertex(2, 5)+delta_x(2)
        x_vertex(3, 8) = x_vertex(3, 5)

        !--------------------------------------------

        IF( i .EQ. 1 ) THEN

          ! x-coordinte of Vertex 1
          x_vertex(1, 1) = rm3d%x_start(1)
          ! x-coordinte of Vertex 4
          x_vertex(1, 4) = rm3d%x_start(1)
          ! x-coordinte of Vertex 5
          x_vertex(1, 5) = rm3d%x_start(1)
          ! x-coordinte of Vertex 8
          x_vertex(1, 8) = rm3d%x_start(1)

        ELSE IF( i .EQ. rm3d%n_x(1) ) THEN

          ! x-coordinte of Vertex 2
          x_vertex(1, 2) = rm3d%x_end(1)
          ! x-coordinte of Vertex 3
          x_vertex(1, 3) = rm3d%x_end(1)
          ! x-coordinte of Vertex 6
          x_vertex(1, 6) = rm3d%x_end(1)
          ! x-coordinte of Vertex 7
          x_vertex(1, 7) = rm3d%x_end(1)

        END IF

        IF( j .EQ. 1 ) THEN

          ! y-coordinte of Vertex 1
          x_vertex(2, 1) = rm3d%x_start(2)
          ! y-coordinte of Vertex 2
          x_vertex(2, 2) = rm3d%x_start(2)
          ! y-coordinte of Vertex 5
          x_vertex(2, 5) = rm3d%x_start(2)
          ! y-coordinte of Vertex 6
          x_vertex(2, 6) = rm3d%x_start(2)

         ELSE IF( j .EQ. rm3d%n_x(2) ) THEN

          ! y-coordinte of Vertex 3
          x_vertex(2, 3) = rm3d%x_end(2)
          ! y-coordinte of Vertex 4
          x_vertex(2, 4) = rm3d%x_end(2)
          ! y-coordinte of Vertex 7
          x_vertex(2, 7) = rm3d%x_end(2)
          ! y-coordinte of Vertex 8
          x_vertex(2, 8) = rm3d%x_end(2)

        END IF

        IF( k .EQ. 1 ) THEN

          ! z-coordinte of Vertex 1
          x_vertex(3, 1) = rm3d%x_start(3)
          ! z-coordinte of Vertex 2
          x_vertex(3, 2) = rm3d%x_start(3)
          ! z-coordinte of Vertex 3
          x_vertex(3, 3) = rm3d%x_start(3)
          ! z-coordinte of Vertex 4
          x_vertex(3, 4) = rm3d%x_start(3)

        ELSE IF( k .EQ. rm3d%n_x(3) ) THEN

          ! z-coordinte of Vertex 5
          x_vertex(3, 5) = rm3d%x_end(3)
          ! z-coordinte of Vertex 6
          x_vertex(3, 6) = rm3d%x_end(3)
          ! z-coordinte of Vertex 7
          x_vertex(3, 7) = rm3d%x_end(3)
          ! z-coordinte of Vertex 8
          x_vertex(3, 8) = rm3d%x_end(3)

        END IF

        !--------------------------------------------
        ! Element no.
        ie = ie+1

        IF( le3d_nnodes .EQ. 8 ) THEN

          ! na = 1, 2, 3, 4, 5, 6, 7, 8
          DO na = 1, 8

            x_local(1, na) = x_vertex(1, na)
            x_local(2, na) = x_vertex(2, na)
            x_local(3, na) = x_vertex(3, na)

          END DO

        END IF

        DO na = 1, le3d_nnodes

          id = es3d_connectivity(na, ie)

          ns3d_x(1, id) = x_local(1, na)
          ns3d_x(2, id) = x_local(2, na)
          ns3d_x(3, id) = x_local(3, na)

        END DO

        !--------------------------------------------

        ! Coordinates of Vertex 1
        x_vertex(1, 1) = x_vertex(1, 2)
        x_vertex(2, 1) = x_vertex(2, 2)
        x_vertex(3, 1) = x_vertex(3, 2)

        !--------------------------------------------

      END DO

      !--------------------------------------------------

      ! Coordinates of Vertex 1
      x_vertex(1, 1) = rm3d%x_start(1)
      x_vertex(2, 1) = x_vertex(2, 2)+delta_x(2)
      x_vertex(3, 1) = x_vertex(3, 2)

      !--------------------------------------------------

    END DO

    !--------------------------------------------------------

    ! Coordinates of Vertex 1
    x_vertex(1, 1) = rm3d%x_start(1)
    x_vertex(2, 1) = rm3d%x_start(2)
    x_vertex(3, 1) = x_vertex(3, 2)+delta_x(3)

    !--------------------------------------------------------

  END DO

  !--------------------------------------------------------------

  CALL set_nodes3d_x(rm3d%ns3d, ns3d_x)

  !--------------------------------------------------------------------

  DEALLOCATE( ns3d_x )

  DEALLOCATE( es3d_connectivity )

  DEALLOCATE( x_local )

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE cal_rectmesher3d_coordinates_hex
!####################################################################


!####################################################################
SUBROUTINE del_rectmesher3d(rm3d)
!####################################################################

  TYPE(struct_rectmesher3d), INTENT(INOUT) :: rm3d

!--------------------------------------------------------------------

  NULLIFY( rm3d%ns3d )
  NULLIFY( rm3d%le3d )
  NULLIFY( rm3d%es3d )

!--------------------------------------------------------------------

  rm3d%n_x = 0
  rm3d%x_start  = 0.0D0
  rm3d%x_end    = 0.0D0
  rm3d%x_center = 0.0D0
  rm3d%length_x = 0.0D0

!--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE del_rectmesher3d
!####################################################################


!####################################################################
END MODULE mod_rectmesher3d
