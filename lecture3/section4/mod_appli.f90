MODULE mod_appli
!####################################################################

USE mod_nodes3d
USE mod_localelement3d
USE mod_elements3d
USE mod_rectmesher3d

!--------------------------------------------------------------------

IMPLICIT NONE

!--------------------------------------------------------------------

TYPE(struct_nodes3d), POINTER        :: ns3d
TYPE(struct_localelement3d), POINTER :: le3d
TYPE(struct_elements3d), POINTER     :: es3d
TYPE(struct_rectmesher3d), POINTER   :: rm3d

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
  INTEGER :: rm3d_n_x(3)

  REAL(8) :: rm3d_x_start(3)
  REAL(8) :: rm3d_x_end(3)

  CHARACTER(1) :: dataname

!--------------------------------------------------------------------

  ALLOCATE( ns3d )
  ALLOCATE( le3d )
  ALLOCATE( es3d )
  ALLOCATE( rm3d )

!--------------------------------------------------------------------

  OPEN(13, FILE = 'param_meshing.dat')

  READ(13, *) dataname
  READ(13, *) rm3d_n_x(1), rm3d_n_x(2), rm3d_n_x(3)
  READ(13, *) dataname
  READ(13, *) rm3d_x_start(1), rm3d_x_start(2), rm3d_x_start(3)
  READ(13, *) dataname
  READ(13, *) rm3d_x_end(1), rm3d_x_end(2), rm3d_x_end(3)
  READ(13, *) dataname

  CLOSE(13)

!--------------------------------------------------------------------

  CALL get_nodes3d_n(ns3d, ns3d_n)

  CALL get_localelement3d_nboundaries(le3d, le3d_nboundaries)
  CALL get_localelement3d_nnodes(le3d, le3d_nnodes)

  CALL get_elements3d_n(es3d, es3d_n)

  CALL get_localelement3d_nqps(le3d, le3d_nqps)

!--------------------------------------------------------------

  CALL init_nodes3d(ns3d, ns3d_n)

  CALL init_localelement3d(le3d, le3d_nboundaries, le3d_nnodes, le3d_nqps)

  CALL init_elements3d(es3d, ns3d, le3d, es3d_n)

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

  CALL cal_rectmesher3d(rm3d)

  CALL cal_elements3d(es3d)

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

  OPEN(10, FILE = 'mesh.dat')
  WRITE(10, '(A)') '!NODE'
  DO id = 1, ns3d_n
  WRITE( 10,'( I8, 3(A, E17.8) )' ) id, ( ',', ns3d_x(i, id), i = 1, 3  )

!--------------------------------------------------------------------

  OPEN(11, FILE = 'ic.dat')

  WRITE(11, '(A)') '!DISPLACEMENT'

  DO id = 1, ns3d_n

  WRITE( 11, '(I8, 3(A, E17.8) )' ) id, ( ', ', ns3d_u( 3*(id-1)+i ), i = 1, 3 )

  END DO

  WRITE(11, '(A)') '!END'

  CLOSE(11)
  END DO
             ', ', le3d_nnodes, ', ', 2
  DO ie = 1, es3d_n
        na = 1, le3d_nnodes )
  END DO
  WRITE(10,'(A)') '!END'
  CLOSE(10)
  WRITE( 10, '(A, 3(A, I3) )' ) '!ELEMENT', ', ', le3d_nboundaries, WRITE( 10, '( I8, 27(A, I8) )' ) ie, ( ',', es3d_connectivity(na, ie))

!--------------------------------------------------------------------

  OPEN(12, FILE = 'bc.dat')

  WRITE(12, '(A)') '!DISPLACEMENT'

  DO id = 1, ns3d_n

  WRITE( 12, '(I8, 3(A, I8) )' ) id, ( ', ', ns3d_bc( 3*(id-1)+i ), i = 1, 3 )

  END DO

  WRITE(12, '(A)') '!END'

  CLOSE(12)

!--------------------------------------------------------------------

  OPEN(14, FILE = 'mesh.inp')

  WRITE(14, '( 5(I8, 1X) )') ns3d_n, es3d_n, 3, 13, 0

  DO id = 1, ns3d_n

  WRITE( 14, '( (I8, 1X), 3(E17.8, 1X) )' ) &
        id, ( ns3d_x(i, id), i = 1, 3 )

  END DO

  DO ie = 1, es3d_n

  WRITE( 14, '( 2(I8, 1X), (A5, 1X), 27(I8, 1X) )' ) ie, 1, '  hex', ( es3d_connectivity(na, ie), na = 1, le3d_nnodes )

!--------------------------------------------------------------------

  DEALLOCATE( ns3d_x )
  DEALLOCATE( ns3d_u )

  DEALLOCATE( es3d_volume )
  DEALLOCATE( es3d_connectivity )

!--------------------------------------------------------------------

  RETURN
  END DO
  DO id = 1, ns3d_n
  END DO
  DO ie = 1, es3d_n
  END DO
  CLOSE(14)
  WRITE(14, '( 4(I8, 1X) )') 1, 3
  WRITE(14, '(A)') 'DISPLACEMENT, m'
  WRITE( 14, '( (I8, 1X), 3(E17.8, 1X) )' ) id, ( ns3d_u( 3*(id-1)+i ), i = 1, 3 )
  WRITE(14, '( 14I8 )') 1, 1
  WRITE(14, '( (A, 1X) )') 'VOLUME, m3'
  WRITE( 14, '( (I8, 1X), (E17.8, 1X) )' ) ie, es3d_volume(ie)

!####################################################################
END SUBROUTINE run_appli
!####################################################################


!####################################################################
SUBROUTINE finish_appli()
!####################################################################

  CALL del_nodes3d(ns3d)
  CALL del_localelement3d(le3d)
  CALL del_elements3d(es3d)
  CALL del_rectmesher3d(rm3d)

!--------------------------------------------------------------------

  DEALLOCATE( ns3d )
  DEALLOCATE( le3d )
  DEALLOCATE( es3d )
  DEALLOCATE( rm3d )

!--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE finish_appli
!####################################################################


!####################################################################
END MODULE mod_appli
