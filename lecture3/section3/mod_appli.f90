MODULE mod_appli
!####################################################################

USE mod_nodes3d
USE mod_localelement3d
USE mod_elements3d

!--------------------------------------------------------------------

IMPLICIT NONE

!--------------------------------------------------------------------

TYPE(struct_nodes3d), POINTER        :: ns3d
TYPE(struct_localelement3d), POINTER :: le3d
TYPE(struct_elements3d), POINTER     :: es3d

!--------------------------------------------------------------------

CONTAINS

! Start appli
!####################################################################
SUBROUTINE start_appli()
!####################################################################

  INTEGER :: ns3d_n
  INTEGER :: le3d_nboundaries
  INTEGER :: le3d_nnodes
  INTEGER :: es3d_n
  INTEGER, ALLOCATABLE :: es3d_connectivity(:, :)

  REAL(8), ALLOCATABLE :: ns3d_x(:, :)

  CHARACTER(1) :: dataname

!--------------------------------------------------------------------

  ALLOCATE( ns3d )
  ALLOCATE( le3d )
  ALLOCATE( es3d )

!--------------------------------------------------------------------

  ns3d_n = 8
  le3d_nboundaries = 6
  le3d_nnodes = 8
  es3d_n = 1

!--------------------------------------------------------------------

  CALL init_nodes3d(ns3d, ns3d_n)

  CALL init_localelement3d(le3d, le3d_nboundaries, le3d_nnodes)

  CALL init_elements3d(es3d, ns3d, le3d, es3d_n)

!--------------------------------------------------------------------

  ALLOCATE( ns3d_x(3, ns3d_n) )

  ! Node 1
  ns3d_x(1, 1) = 0.0D0
  ns3d_x(2, 1) = 0.0D0
  ns3d_x(3, 1) = 0.0D0
  ! Node 2
  ns3d_x(1, 2) = 1.0D0
  ns3d_x(2, 2) = 0.0D0
  ns3d_x(3, 2) = 0.0D0
  ! Node 3
  ns3d_x(1, 3) = 0.0D0
  ns3d_x(2, 3) = 1.0D0
  ns3d_x(3, 3) = 0.0D0
  ! Node 4
  ns3d_x(1, 4) = 1.0D0
  ns3d_x(2, 4) = 1.0D0
  ns3d_x(3, 4) = 0.0D0
  ! Node 5
  ns3d_x(1, 5) = 0.0D0
  ns3d_x(2, 5) = 0.0D0
  ns3d_x(3, 5) = 1.0D0
  ! Node 6
  ns3d_x(1, 6) = 1.0D0
  ns3d_x(2, 6) = 0.0D0
  ns3d_x(3, 6) = 1.0D0
  ! Node 7
  ns3d_x(1, 7) = 0.0D0
  ns3d_x(2, 7) = 1.0D0
  ns3d_x(3, 7) = 1.0D0
  ! Node 8
  ns3d_x(1, 8) = 1.0D0
  ns3d_x(2, 8) = 1.0D0
  ns3d_x(3, 8) = 1.0D0

  CALL set_nodes3d_x(ns3d, ns3d_x)

  ALLOCATE( es3d_connectivity(le3d_nnodes, es3d_n) )

  ! Element 1
  es3d_connectivity(1, 1) = 1
  es3d_connectivity(2, 1) = 2
  es3d_connectivity(3, 1) = 4
  es3d_connectivity(4, 1) = 3
  es3d_connectivity(5, 1) = 5
  es3d_connectivity(6, 1) = 6
  es3d_connectivity(7, 1) = 8
  es3d_connectivity(8, 1) = 7

  CALL set_elements3d_connectivity(es3d, es3d_connectivity)

!--------------------------------------------------------------------

  DEALLOCATE( ns3d_x )
  DEALLOCATE( es3d_connectivity )

!--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE start_appli
!####################################################################

! Run appli
!####################################################################
SUBROUTINE run_appli()
!####################################################################

  INTEGER :: ns3d_n
  INTEGER, ALLOCATABLE :: ns3d_bc(:)
  INTEGER :: le3d_nboundaries
  INTEGER :: le3d_nnodes
  INTEGER :: es3d_n
  INTEGER, ALLOCATABLE :: es3d_connectivity(:, :)
  INTEGER :: es3d_ie_max_volume
  INTEGER :: es3d_ie_min_volume
  INTEGER :: i
  INTEGER :: id
  INTEGER :: na
  INTEGER :: ie

  REAL(8), ALLOCATABLE :: ns3d_x(:, :)
  REAL(8), ALLOCATABLE :: ns3d_u(:)
  REAL(8), ALLOCATABLE :: es3d_volume(:)
  REAL(8) :: es3d_max_volume
  REAL(8) :: es3d_min_volume
  REAL(8) :: es3d_sum_volume

!--------------------------------------------------------------------

  CALL get_nodes3d_n(ns3d, ns3d_n)

  ALLOCATE( ns3d_x(3, ns3d_n) )
  CALL get_nodes3d_x(ns3d, ns3d_x)
  ALLOCATE( ns3d_u(3*ns3d_n) )
  ns3d_u = 0.0D0
  ALLOCATE( ns3d_bc(3*ns3d_n) )
  ns3d_bc = 0

  CALL get_localelement3d_nboundaries(le3d, le3d_nboundaries)
  CALL get_localelement3d_nnodes(le3d, le3d_nnodes)

  CALL get_elements3d_n(es3d, es3d_n)
  ALLOCATE( es3d_volume(es3d_n) )
  CALL get_elements3d_volume(es3d, es3d_volume, es3d_max_volume, es3d_ie_max_volume, es3d_min_volume, es3d_ie_min_volume, es3d_sum_volume)
  ALLOCATE( es3d_connectivity(le3d_nnodes, es3d_n) )
  CALL get_elements3d_connectivity(es3d, es3d_connectivity)

!--------------------------------------------------------------------

  OPEN(14, FILE = 'mesh.inp')

  WRITE(14, '( 5(I8, 1X) )') ns3d_n, es3d_n, 3, 13, 0

  DO id = 1, ns3d_n

    WRITE( 14, '( (I8, 1X), 3(E17.8, 1X) )' ) id, ( ns3d_x(i, id), i = 1, 3 )

  END DO

  DO ie = 1, es3d_n

    WRITE( 14, '( 2(I8, 1X), (A5, 1X), 27(I8, 1X) )' ) ie, 1, '  hex', ( es3d_connectivity(na, ie), na = 1, le3d_nnodes )

  END DO

  WRITE(14, '( 4(I8, 1X) )') 1, 3
  WRITE(14, '(A)') 'DISPLACEMENT, m'

  DO id = 1, ns3d_n

    WRITE( 14, '( (I8, 1X), 3(E17.8, 1X) )' ) id, ( ns3d_u( 3*(id-1)+i ), i = 1, 3 )

  END DO

  WRITE(14, '( 14I8 )') 1, 1
  WRITE(14, '( (A, 1X) )') 'VOLUME, m3'

  DO ie = 1, es3d_n

    WRITE( 14, '( (I8, 1X), (E17.8, 1X) )' ) ie, es3d_volume(ie)

  END DO

  CLOSE(14)

!--------------------------------------------------------------------

  DEALLOCATE( ns3d_x )
  DEALLOCATE( ns3d_u )

  DEALLOCATE( es3d_volume )
  DEALLOCATE( es3d_connectivity )

!--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE run_appli
!####################################################################


!####################################################################
SUBROUTINE finish_appli()
!####################################################################

  CALL del_nodes3d(ns3d)
  CALL del_localelement3d(le3d)
  CALL del_elements3d(es3d)

!--------------------------------------------------------------------

  DEALLOCATE( ns3d )
  DEALLOCATE( le3d )
  DEALLOCATE( es3d )

!--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE finish_appli
!####################################################################


!####################################################################
END MODULE mod_appli
