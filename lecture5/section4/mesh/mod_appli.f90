MODULE mod_appli
!####################################################################

USE mod_nodes3d
USE mod_localelement3d
USE mod_elements3d
USE mod_elemstiffmat3d
USE mod_elemexforcevec3d
USE mod_fem3d
USE mod_rectmesher3d

!--------------------------------------------------------------------

IMPLICIT NONE

!--------------------------------------------------------------------

TYPE(struct_nodes3d), POINTER          :: ns3d
TYPE(struct_localelement3d), POINTER   :: le3d
TYPE(struct_elements3d), POINTER       :: es3d
TYPE(struct_elemstiffmat3d), POINTER   :: esm3d
TYPE(struct_elemexforcevec3d), POINTER :: efv3d
TYPE(struct_fem3d), POINTER            :: fem3d
TYPE(struct_rectmesher3d), POINTER     :: rm3d

!--------------------------------------------------------------
!
! Problem number
! prob
!
!--------------------------------------------------------------

INTEGER :: prob

!--------------------------------------------------------------------

CONTAINS


! Start appli
!####################################################################
SUBROUTINE start_appli()
!####################################################################

  INTEGER :: ns3d_n
  INTEGER :: le3d_nboundaries
  INTEGER :: le3d_nnodes
  INTEGER :: le3d_nqps
  INTEGER :: es3d_n
  INTEGER :: rm3d_n_x(3)

  REAL(8) :: rm3d_x_start(3)
  REAL(8) :: rm3d_x_end(3)
  REAL(8) :: e
  REAL(8) :: nu
  REAL(8) :: rho
  REAL(8) :: g

  CHARACTER(1) :: dataname

  !--------------------------------------------------------------------

  ALLOCATE( ns3d )
  ALLOCATE( le3d )
  ALLOCATE( es3d )
  ALLOCATE( esm3d )
  ALLOCATE( efv3d )
  ALLOCATE( fem3d )
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
  READ(13, *) prob
  READ(13, *) dataname
  READ(13, *) e
  READ(13, *) dataname
  READ(13, *) nu
  READ(13, *) dataname
  READ(13, *) rho
  READ(13, *) dataname
  READ(13, *) g
  READ(13, *) dataname

  CLOSE(13)

  !--------------------------------------------------------------------

  OPEN(13, FILE = 'param_fea.dat')

  WRITE(13, '(A)') '!ANALYSIS_TYPE'
  WRITE(13, '(A)') 'STATIC_ANALYSIS'
  WRITE(13, '(A)') "!YOUNG'S_MODULUS"
  WRITE(13, '(E17.8)') e
  WRITE(13, '(A)') "!POISSON'S_RATIO"
  WRITE(13, '(E17.8)') nu
  WRITE(13,  '(A)') '!DENSITY'
  WRITE(13, '(E17.8)') rho
  WRITE(13,  '(A)') '!GRAVITATIONAL_ACCELERATION'
  WRITE(13, '(E17.8)') g

  !--------------------------------------------------------------------

  CALL init_rectmesher3d                    &
  (rm3d, ns3d, le3d, es3d,             &
  rm3d_n_x, rm3d_x_start, rm3d_x_end)

  !--------------------------------------------------------------------

  CALL get_nodes3d_n(ns3d, ns3d_n)

  CALL get_localelement3d_nboundaries(le3d, le3d_nboundaries)
  CALL get_localelement3d_nnodes(le3d, le3d_nnodes)
  CALL get_localelement3d_nqps(le3d, le3d_nqps)

  CALL get_elements3d_n(es3d, es3d_n)

  !--------------------------------------------------------------

  CALL init_nodes3d(ns3d, ns3d_n)

  CALL init_localelement3d                              &
     (le3d, le3d_nboundaries, le3d_nnodes, le3d_nqps)

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
  INTEGER :: le3d_nnodes_boundary
  INTEGER, ALLOCATABLE :: le3d_table_na(:, :)
  INTEGER :: es3d_n
  INTEGER, ALLOCATABLE :: es3d_connectivity(:, :)
  INTEGER :: es3d_ie_max_volume
  INTEGER :: es3d_ie_min_volume
  INTEGER :: efv3d_nelemboundaries
  INTEGER, ALLOCATABLE :: efv3d_table_ie(:)
  INTEGER, ALLOCATABLE :: efv3d_table_ma(:)
  INTEGER :: fem3d_ndofs
  INTEGER :: fem3d_nnodes_loaded
  INTEGER, ALLOCATABLE :: fem3d_id_loaded(:)
  INTEGER :: i
  INTEGER :: id
  INTEGER :: id_l
  INTEGER :: ma
  INTEGER :: na
  INTEGER :: ie
  INTEGER :: idof
  INTEGER :: ib

  REAL(8), ALLOCATABLE :: ns3d_x(:, :)
  REAL(8), ALLOCATABLE :: ns3d_u(:)
  REAL(8), ALLOCATABLE :: es3d_volume(:)
  REAL(8) :: es3d_max_volume
  REAL(8) :: es3d_min_volume
  REAL(8) :: es3d_sum_volume
  REAL(8), ALLOCATABLE :: efv3d_t(:, :)
  REAL(8), ALLOCATABLE :: fem3d_f_loaded(:, :)
  REAL(8) :: rm3d_x_start(3)
  REAL(8) :: rm3d_x_end(3)
  REAL(8) :: rm3d_x_center(3)
  REAL(8) :: rm3d_length_x(3)
  REAL(8), ALLOCATABLE :: x_local(:, :)

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
  CALL get_localelement3d_nnodes_boundary(le3d, le3d_nnodes_boundary)
  ALLOCATE( le3d_table_na(le3d_nnodes_boundary, le3d_nboundaries) )
  CALL get_localelement3d_table_na(le3d, le3d_table_na)

  CALL get_elements3d_n(es3d, es3d_n)
  ALLOCATE( es3d_volume(es3d_n) )
  CALL get_elements3d_volume                 &
     (es3d, es3d_volume,                   &
      es3d_max_volume, es3d_ie_max_volume, &
      es3d_min_volume, es3d_ie_min_volume, &
      es3d_sum_volume)
  ALLOCATE( es3d_connectivity(le3d_nnodes, es3d_n) )
  CALL get_elements3d_connectivity(es3d, es3d_connectivity)
  CALL get_rectmesher3d_x_start_x_end   &
     (rm3d, rm3d_x_start, rm3d_x_end, &
      rm3d_x_center, rm3d_length_x)

  ALLOCATE( x_local(3, le3d_nnodes_boundary) )

  !--------------------------------------------------------------------

  ! Tensile deformation
  IF( prob .EQ. 1 ) THEN

    !--------------------------------------------------------

    id_l = 0

    DO id = 1, ns3d_n

      IF( DABS( ns3d_x(1, id)-rm3d_x_end(1) ) .LT. EPSILON(1.0D0) ) THEN

        id_l = id_l+1

      END IF

    END DO

    fem3d_nnodes_loaded = id_l
    efv3d_nelemboundaries = ib

    !--------------------------------------------------------

  ! Tensile deformation
  ELSE IF( prob .EQ. 2 ) THEN

    !--------------------------------------------------------

    id_l = 0
    ib = 0

    DO ie = 1, es3d_n

      DO ma = 1, le3d_nboundaries

        na = le3d_table_na(1, ma)
        id = es3d_connectivity(na, ie)
        x_local(1, 1) = ns3d_x(1, id)
        x_local(2, 1) = ns3d_x(2, id)
        x_local(3, 1) = ns3d_x(3, id)

        na = le3d_table_na(2, ma)
        id = es3d_connectivity(na,ie)
        x_local(1, 2) = ns3d_x(1, id)
        x_local(2, 2) = ns3d_x(2, id)
        x_local(3, 2) = ns3d_x(3, id)

        na = le3d_table_na(3, ma)
        id = es3d_connectivity(na, ie)
        x_local(1, 3) = ns3d_x(1, id)
        x_local(2, 3) = ns3d_x(2, id)
        x_local(3, 3) = ns3d_x(3, id)

        na = le3d_table_na(4, ma)
        id = es3d_connectivity(na, ie)
        x_local(1, 4) = ns3d_x(1, id)
        x_local(2, 4) = ns3d_x(2, id)
        x_local(3, 4) = ns3d_x(3, id)

        IF( ( DABS( x_local(1, 1)-rm3d_x_end(1) ) .LT. EPSILON(1.0D0) ) .AND.  &
            ( DABS( x_local(1, 2)-rm3d_x_end(1) ) .LT. EPSILON(1.0D0) ) .AND.  &
            ( DABS( x_local(1, 3)-rm3d_x_end(1) ) .LT. EPSILON(1.0D0) ) .AND.  &
            ( DABS( x_local(1, 4)-rm3d_x_end(1) ) .LT. EPSILON(1.0D0) ) ) THEN

          ib = ib+1

        END IF

      END DO

    END DO

    fem3d_nnodes_loaded = id_l
    efv3d_nelemboundaries = ib

    !--------------------------------------------------------

  END IF

  !--------------------------------------------------------------

  CALL init_elemstiffmat3d(esm3d, ns3d, le3d, es3d)
  CALL init_elemexforcevec3d(efv3d, ns3d, le3d, es3d, efv3d_nelemboundaries)
  CALL init_fem3d                              &
       (fem3d, ns3d, le3d, es3d, esm3d, efv3d, &
        fem3d_nnodes_loaded)

  ALLOCATE( efv3d_table_ie(efv3d_nelemboundaries) )
  efv3d_table_ie = 0
  ALLOCATE( efv3d_table_ma(efv3d_nelemboundaries) )
  efv3d_table_ma = 0
  ALLOCATE( efv3d_t(3, efv3d_nelemboundaries) )
  efv3d_t = 0.0D0

  CALL get_fem3d_ndofs(fem3d, fem3d_ndofs)
  ALLOCATE( fem3d_id_loaded(fem3d_nnodes_loaded) )
  fem3d_id_loaded = 0
  ALLOCATE( fem3d_f_loaded(3, fem3d_nnodes_loaded) )
  fem3d_f_loaded = 0.0D0

  !--------------------------------------------------------------

  ! Tensile deformation
  IF( (prob .EQ. 1 ) .OR. ( prob .EQ. 2 ) ) THEN

    !--------------------------------------------------------

    DO id = 1, ns3d_n

      IF( DABS( ns3d_x(1, id)-rm3d_x_start(1) ) .LT. EPSILON(1.0D0) ) THEN

        idof = 3*(id-1)+1

        ns3d_u(idof) = 0.0D0
        ns3d_bc(idof) = 1

        IF( DABS( ns3d_x(2, id)-rm3d_x_center(2) ) .LT. EPSILON(1.0D0) ) THEN

          idof = 3*(id-1)+2

          ns3d_u(idof) = 0.0D0
          ns3d_bc(idof) = 1

        END IF

        IF( DABS( ns3d_x(3, id)-rm3d_x_center(3) ) .LT. EPSILON(1.0D0) ) THEN

          idof = 3*(id-1)+3

          ns3d_u(idof) = 0.0D0
          ns3d_bc(idof) = 1

        END IF

      END IF

    END DO

  !--------------------------------------------------------

  END IF

  !--------------------------------------------------------------

  id_l = 0


  ! Tensile deformation
  IF( prob .EQ. 1 ) THEN

    !--------------------------------------------------------

    DO id = 1, ns3d_n

      IF( DABS( ns3d_x(1, id)-rm3d_x_end(1) ) .LT. EPSILON(1.0D0) ) THEN

        id_l = id_l+1

        fem3d_id_loaded(id_l) = id
        fem3d_f_loaded(1, id_l) = 2.5D6

        IF( DABS( ns3d_x(2, id)-rm3d_x_center(2) ) .LT. EPSILON(1.0D0) ) THEN

          fem3d_f_loaded(1, id_l) = 5.0D6

        END IF

        IF( DABS( ns3d_x(3, id)-rm3d_x_center(3) ) .LT. EPSILON(1.0D0) ) THEN

          fem3d_f_loaded(1, id_l) = 5.0D6

        END IF

        IF( DABS( ns3d_x(2, id)-rm3d_x_center(2) ) .LT. EPSILON(1.0D0) ) THEN

          IF( DABS( ns3d_x(3, id)-rm3d_x_center(3) ) .LT. EPSILON(1.0D0) ) THEN

            fem3d_f_loaded(1, id_l) = 1.0D7

          END IF

        END IF

      END IF

    END DO


  ! Tensile deformation
  ELSE IF( prob .EQ. 2 ) THEN

    !--------------------------------------------------------

    ib = 0

    DO ie = 1, es3d_n

      DO ma = 1, le3d_nboundaries

        na = le3d_table_na(1, ma)
        id = es3d_connectivity(na, ie)
        x_local(1, 1) = ns3d_x(1, id)
        x_local(2, 1) = ns3d_x(2, id)
        x_local(3, 1) = ns3d_x(3, id)

        na = le3d_table_na(2, ma)
        id = es3d_connectivity(na,ie)
        x_local(1, 2) = ns3d_x(1, id)
        x_local(2, 2) = ns3d_x(2, id)
        x_local(3, 2) = ns3d_x(3, id)

        na = le3d_table_na(3, ma)
        id = es3d_connectivity(na, ie)
        x_local(1, 3) = ns3d_x(1, id)
        x_local(2, 3) = ns3d_x(2, id)
        x_local(3, 3) = ns3d_x(3, id)

        na = le3d_table_na(4, ma)
        id = es3d_connectivity(na, ie)
        x_local(1, 4) = ns3d_x(1, id)
        x_local(2, 4) = ns3d_x(2, id)
        x_local(3, 4) = ns3d_x(3, id)

        IF( ( DABS( x_local(1, 1)-rm3d_x_end(1) ) .LT. EPSILON(1.0D0) ) .AND.  &
           ( DABS( x_local(1, 2)-rm3d_x_end(1) ) .LT. EPSILON(1.0D0) ) .AND.  &
           ( DABS( x_local(1, 3)-rm3d_x_end(1) ) .LT. EPSILON(1.0D0) ) .AND.  &
           ( DABS( x_local(1, 4)-rm3d_x_end(1) ) .LT. EPSILON(1.0D0) ) ) THEN

          ib = ib+1

          efv3d_table_ie(ib) = ie
          efv3d_table_ma(ib) = ma
          efv3d_t(1, ib) = -4.0D7
          efv3d_t(2, ib) =  0.0D0
          efv3d_t(3, ib) =  0.0D0

        END IF

      END DO

    END DO

  END IF

  !--------------------------------------------------------

  CALL set_fem3d_f_loaded                        &
        (fem3d, fem3d_id_loaded, fem3d_f_loaded)
  CALL set_elemexforcevec3d_t                     &
        (efv3d, efv3d_table_ie, efv3d_ma, efv3d_t)

!--------------------------------------------------------------------

  OPEN(10, FILE = 'mesh.dat')

  WRITE(10, '(A)') '!NODE'

  DO id = 1, ns3d_n

    WRITE( 10,'( I8, 3(A, E17.8) )' )            &
           id, ( ',', ns3d_x(i, id), i = 1, 3  )

  END DO

  WRITE( 10, '(A, 3(A, I3) )' )                 &
         '!ELEMENT', ', ', le3d_nboundaries,    &
          ', ', le3d_nnodes, ', ', 2

  DO ie = 1, es3d_n

    WRITE( 10, '( I8, 27(A, I8) )' )             &
           ie, ( ',', es3d_connectivity(na, ie), &
                na = 1, le3d_nnodes )


  END DO

  WRITE(10,'(A)') '!END'

  CLOSE(10)

  !--------------------------------------------------------------------

  OPEN(11, FILE = 'ic.dat')

  WRITE(11, '(A)') '!DISPLACEMENT'

  DO id = 1, ns3d_n

  WRITE( 11, '(I8, 3(A, E17.8) )' )                   &
        id, ( ', ', ns3d_u( 3*(id-1)+i ), i = 1, 3 )

  END DO

  WRITE(11, '(A)') '!END'

  CLOSE(11)

  !--------------------------------------------------------------------

  OPEN(12, FILE = 'bc.dat')

  WRITE(12, '(A)') '!DISPLACEMENT'

  DO id = 1, ns3d_n

    WRITE( 12, '(I8, 3(A, I8) )' )                       &
    id, ( ', ', ns3d_bc( 3*(id-1)+i ), i = 1, 3 )

  END DO

  WRITE(12, '(A)') '!END'

  CLOSE(12)

  !--------------------------------------------------------------------

  WRITE(13, '(A)') '!F_LOADED'

  DO id_l = 1, fem3d_nnodes_loaded

  WRITE( 13, '(I8, 3(A, E17.8) )' )                  &
        fem3d_id_loaded(id_l),                      &
        ( ', ', fem3d_f_loaded(i, id_l), i = 1, 3 )

  END DO

  WRITE(13, '(A)') '!TRACTION'

  DO ib = 1, efv3d_nelemboundaries

    WRITE( 13, '(I8, 2(A, I8), 3(A, E17.8) )' ) &
           ib, ',', efv3d_table_ie(ib),         &
           ',', efv3d_table_ma(ib),             &
           ( ',', efv3d_t(i, ib), i = 1, 3 )

  END DO

  WRITE(13, '(A)') '!END'

  CLOSE(13)

  !--------------------------------------------------------------------

  OPEN(14, FILE = 'mesh.inp')

  WRITE(14, '( 5(I8, 1X) )') ns3d_n, es3d_n, 3, 13, 0

  DO id = 1, ns3d_n

  WRITE( 14, '( (I8, 1X), 3(E17.8, 1X) )' ) &
        id, ( ns3d_x(i, id), i = 1, 3 )

  END DO

  DO ie = 1, es3d_n

    WRITE( 14, '( 2(I8, 1X), (A5, 1X), 27(I8, 1X) )' )        &
           ie, 1, '  hex',                                    &
           ( es3d_connectivity(na, ie), na = 1, le3d_nnodes )

  END DO

  WRITE(14, '( 4(I8, 1X) )') 1, 3
  WRITE(14, '(A)') 'DISPLACEMENT, m'

  DO id = 1, ns3d_n

    WRITE( 14, '( (I8, 1X), 3(E17.8, 1X) )' )     &
          id, ( ns3d_u( 3*(id-1)+i ), i = 1, 3 )

  END DO

  WRITE(14, '( 14I8 )') 1, 1
  WRITE(14, '( (A, 1X) )') 'VOLUME, m3'

  DO ie = 1, es3d_n
      WRITE( 14, '( (I8, 1X), (E17.8, 1X) )' ) &
            ie, es3d_volume(ie)
  END DO

  CLOSE(14)

  !--------------------------------------------------------------------

  DEALLOCATE( ns3d_x )
  DEALLOCATE( ns3d_u )

  DEALLOCATE( le3d_table_na )

  DEALLOCATE( es3d_volume )
  DEALLOCATE( es3d_connectivity )

  DEALLOCATE( efv3d_table_ie )
  DEALLOCATE( efv3d_table_ma )
  DEALLOCATE( efv3d_t )

  DEALLOCATE( fem3d_id_loaded )
  DEALLOCATE( fem3d_f_loaded )

  DEALLOCATE( x_local )

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
  CALL del_elemstiffmat3d(esm3d)
  CALL del_elemexforcevec3d(efv3d)
  CALL del_fem3d(fem3d)
  CALL del_rectmesher3d(rm3d)

  !--------------------------------------------------------------------

  DEALLOCATE( ns3d )
  DEALLOCATE( le3d )
  DEALLOCATE( es3d )
  DEALLOCATE( esm3d )
  DEALLOCATE( efv3d )
  DEALLOCATE( fem3d )
  DEALLOCATE( rm3d )

  !--------------------------------------------------------------------

  RETURN

  !####################################################################
  END SUBROUTINE finish_appli
  !####################################################################


!####################################################################
END MODULE mod_appli
