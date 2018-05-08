MODULE mod_appli
!####################################################################

USE mod_nodes3d
USE mod_localelement3d
USE mod_elements3d
USE mod_elemstiffmat3d
USE mod_elemexforcevec3d
USE mod_fem3d

!--------------------------------------------------------------------

IMPLICIT NONE

!--------------------------------------------------------------------

TYPE(struct_nodes3d), POINTER          :: ns3d
TYPE(struct_localelement3d), POINTER   :: le3d
TYPE(struct_elements3d), POINTER       :: es3d
TYPE(struct_elemstiffmat3d), POINTER   :: esm3d
TYPE(struct_elemexforcevec3d), POINTER :: efv3d
TYPE(struct_fem3d), POINTER            :: fem3d

!--------------------------------------------------------------------

CONTAINS

! Start appli
!####################################################################
SUBROUTINE start_appli()
!####################################################################

  INTEGER :: ns3d_n
  INTEGER, ALLOCATABLE :: ns3d_bc(:)
  INTEGER :: le3d_nboundaries
  INTEGER :: le3d_nnodes
  INTEGER :: le3d_nqps
  INTEGER :: es3d_n
  INTEGER, ALLOCATABLE :: es3d_connectivity(:, :)
  INTEGER :: fem3d_nnodes_loaded
  INTEGER, ALLOCATABLE :: fem3d_id_loaded(:)
  INTEGER :: efv3d_nelemboundaries
  INTEGER, ALLOCATABLE :: efv3d_table_ie(:)
  INTEGER, ALLOCATABLE :: efv3d_table_ma(:)
  INTEGER :: fem3d_ndofs
  INTEGER :: i
  INTEGER :: id
  INTEGER :: id_l
  INTEGER :: na
  INTEGER :: ie
  INTEGER :: ib
  INTEGER :: idof_g
  INTEGER :: idof_l
  INTEGER :: number

  REAL(8), ALLOCATABLE :: ns3d_x(:, :)
  REAL(8), ALLOCATABLE :: ns3d_u(:)
  REAL(8), ALLOCATABLE :: esm3d_e(:)
  REAL(8), ALLOCATABLE :: esm3d_nu(:)
  REAL(8), ALLOCATABLE :: efv3d_rho(:)
  REAL(8), ALLOCATABLE :: efv3d_t(:, :)
  REAL(8), ALLOCATABLE :: fem3d_f_loaded(:, :)
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

  !--------------------------------------------------------------------

  OPEN(10, FILE = 'mesh.dat')

  READ(10, *) dataname

  ns3d_n = 0

  DO

    READ(10, *) dataname

    ns3d_n = ns3d_n+1

    IF( dataname .EQ. '!' ) THEN

      EXIT

    END IF

  END DO

  ns3d_n = ns3d_n-1

  es3d_n = 0

  DO

    READ(10, *) dataname

    es3d_n = es3d_n+1

    IF( dataname .EQ. '!' ) THEN

      EXIT

    END IF

  END DO

  es3d_n = es3d_n-1

  CLOSE(10)

  !--------------------------------------------------------------

  OPEN(13, FILE = 'param_fea.dat')

  READ(13, *) dataname
  READ(13, *) dataname
  READ(13, *) dataname
  READ(13, *) dataname
  READ(13, *) dataname
  READ(13, *) dataname
  READ(13, *) dataname
  READ(13, *) dataname
  READ(13, *) dataname
  READ(13, *) dataname

  READ(13, *) dataname

  fem3d_nnodes_loaded = 0

  DO

    READ(13, *) dataname

    fem3d_nnodes_loaded = fem3d_nnodes_loaded+1

    IF( dataname .EQ. '!' ) THEN

      EXIT

    END IF

    END DO

    fem3d_nnodes_loaded = fem3d_nnodes_loaded-1


    efv3d_nelemboundaries = 0

    DO

      READ(13, *) dataname

      efv3d_nelemboundaries = efv3d_nelemboundaries+1

      IF( dataname .EQ. '!' ) THEN

        EXIT

      END IF

    END DO

    efv3d_nelemboundaries = efv3d_nelemboundaries-1

  CLOSE(13)

  !--------------------------------------------------------------------

  OPEN(10, FILE = 'mesh.dat')

  READ(10, *) dataname

  CALL init_nodes3d(ns3d, ns3d_n)

  ALLOCATE( ns3d_x(3, ns3d_n) )

  DO id = 1, ns3d_n

  READ(10, *) number, ( ns3d_x(i, id), i = 1, 3 )

  END DO

  CALL set_nodes3d_x(ns3d, ns3d_x)

  READ(10, *) dataname, le3d_nboundaries, le3d_nnodes, le3d_nqps

  CALL init_localelement3d                              &
     (le3d, le3d_nboundaries, le3d_nnodes, le3d_nqps)
  CALL init_elements3d(es3d, ns3d, le3d, es3d_n)

  ALLOCATE( es3d_connectivity(le3d_nnodes, es3d_n) )

  DO ie = 1, es3d_n

    READ(10, *) number, ( es3d_connectivity(na, ie), &
                       na = 1, le3d_nnodes )

  END DO

  CALL set_elements3d_connectivity(es3d, es3d_connectivity)

  CLOSE(10)

  !--------------------------------------------------------------

  CALL init_elemstiffmat3d(esm3d, ns3d, le3d, es3d)
  CALL init_elemexforcevec3d(efv3d, ns3d, le3d, es3d, efv3d_nelemboundaries)
  CALL init_fem3d                                 &
        (fem3d, ns3d, le3d, es3d, esm3d, efv3d, fem3d_nnodes_loaded)

  CALL get_fem3d_ndofs(fem3d, fem3d_ndofs)

  !--------------------------------------------------------------

  OPEN(11, FILE = 'ic.dat')

  READ(11, *) dataname

  ALLOCATE( ns3d_u(3*ns3d_n) )

  DO id = 1, ns3d_n

  READ(11, *) number, ( ns3d_u( 3*(id-1)+i ), i = 1, 3 )

  END DO

  CALL set_nodes3d_u(ns3d, ns3d_u)

  READ(11, *) dataname

  CLOSE(11)

  !--------------------------------------------------------------

  OPEN(12, FILE = 'bc.dat')

  READ(12, *) dataname

  ALLOCATE( ns3d_bc(3*ns3d_n) )

  DO id = 1, ns3d_n

    READ(12, *) number, ( ns3d_bc( 3*(id-1)+i ), i = 1, 3 )

  END DO

  CALL set_nodes3d_bc(ns3d, ns3d_bc)

  READ(12, *) dataname

  CLOSE(12)

  !--------------------------------------------------------------------

  OPEN(13, FILE = 'param_fea.dat')

  READ(13, *) dataname
  READ(13, *) dataname
  READ(13, *) dataname
  READ(13, *) e
  READ(13, *) dataname
  READ(13, *) nu
  READ(13, *) dataname
  READ(13, *) rho
  READ(13, *) dataname
  READ(13, *) g

  !--------------------------------------------------------------

  ALLOCATE( esm3d_e(es3d_n) )
  ALLOCATE( esm3d_nu(es3d_n) )

  ALLOCATE( efv3d_rho(es3d_n) )

  DO ie = 1, es3d_n

    esm3d_e(ie)  = e
    esm3d_nu(ie) = nu

    efv3d_rho(ie) = rho

  END DO

  CALL set_elemstiffmat3d_e_nu(esm3d, esm3d_e, esm3d_nu)

  CALL set_elemexforcevec3d_rho(efv3d, efv3d_rho)
  CALL set_elemexforcevec3d_g(efv3d, g)

  !--------------------------------------------------------------

  READ(13, *) dataname

  ALLOCATE( fem3d_id_loaded(fem3d_nnodes_loaded) )
  ALLOCATE( fem3d_f_loaded(3, fem3d_nnodes_loaded) )

  DO idof_l = 1, fem3d_nnodes_loaded

    READ(13, *) fem3d_id_loaded(idof_l),                &
             ( fem3d_f_loaded(i, idof_l), i = 1, 3 )

  END DO

  CALL set_fem3d_f_loaded                       &
     (fem3d, fem3d_id_loaded, fem3d_f_loaded)

  READ(13, *) dataname

  ALLOCATE( efv3d_table_ie(efv3d_nelemboundaries) )
  ALLOCATE( efv3d_table_ma(efv3d_nelemboundaries) )
  ALLOCATE( efv3d_t(3, efv3d_nelemboundaries) )

  DO ib = 1, efv3d_nelemboundaries

    READ(13, *) number,                                 &
                efv3d_table_ie(ib), efv3d_table_ma(ib), &
                ( efv3d_t(i, ib), i = 1, 3 )

  END DO

  CALL set_elemexforcevec3d_t                           &
       (efv3d, efv3d_table_ie, efv3d_table_ma, efv3d_t)

  READ(13, *) dataname

  CLOSE(13)

  !--------------------------------------------------------------------

  DEALLOCATE( ns3d_x )
  DEALLOCATE( ns3d_u )
  DEALLOCATE( ns3d_bc )

  DEALLOCATE( es3d_connectivity )

  DEALLOCATE( esm3d_e  )
  DEALLOCATE( esm3d_nu )

  DEALLOCATE( efv3d_rho )
  DEALLOCATE( efv3d_table_ie )
  DEALLOCATE( efv3d_table_ma )
  DEALLOCATE( efv3d_t )

  DEALLOCATE( fem3d_id_loaded )
  DEALLOCATE( fem3d_f_loaded )

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
  INTEGER :: le3d_nnodes
  INTEGER :: es3d_n
  INTEGER, ALLOCATABLE :: es3d_connectivity(:, :)
  INTEGER :: i
  INTEGER :: id
  INTEGER :: na
  INTEGER :: ie

  REAL(8), ALLOCATABLE :: ns3d_x(:, :)
  REAL(8), ALLOCATABLE :: ns3d_u(:)
  REAL(8), ALLOCATABLE :: esm3d_evec(:, :)
  REAL(8), ALLOCATABLE :: esm3d_svec(:, :), esm3d_s_mises(:)

  !--------------------------------------------------------------------

  CALL cal_elements3d(es3d)

  CALL cal_fem3d(fem3d)

  !--------------------------------------------------------------------

  CALL get_nodes3d_n(ns3d, ns3d_n)
  ALLOCATE( ns3d_x(3, ns3d_n) )
  CALL get_nodes3d_x(ns3d, ns3d_x)
  ALLOCATE( ns3d_u(3*ns3d_n) )
  CALL get_nodes3d_u(ns3d, ns3d_u)

  CALL get_localelement3d_nnodes(le3d, le3d_nnodes)

  CALL get_elements3d_n(es3d, es3d_n)
  ALLOCATE( es3d_connectivity(le3d_nnodes, es3d_n) )
  CALL get_elements3d_connectivity(es3d, es3d_connectivity)

  ALLOCATE( esm3d_evec(6, es3d_n) )
  ALLOCATE( esm3d_svec(6, es3d_n) )
  ALLOCATE( esm3d_s_mises(es3d_n) )
  CALL get_elemstiffmat3d_evec_svec                   &
     (esm3d, esm3d_evec, esm3d_svec, esm3d_s_mises)

  !--------------------------------------------------------------------

  OPEN(14, FILE = 'result.inp')

  WRITE(14, '( 5(I8, 1X) )') ns3d_n, es3d_n, 3, 13, 0

  DO id = 1, ns3d_n

    WRITE(14, '( (I8, 1X), 3(E17.8, 1X) )') &
        id, ( ns3d_x(i, id), i = 1, 3 )

  END DO

  DO ie = 1, es3d_n

    WRITE(14, '( 2(I8, 1X), (A5, 1X), 27(I8, 1X) )')         &
       ie, 1, '  hex',                                    &
       ( es3d_connectivity(na, ie),  na = 1, le3d_nnodes)

  END DO

  WRITE(14, '( 4(I8, 1X) )') 1, 3
  WRITE(14, '(A)') 'DISPLACEMENT, m'

  DO id = 1, ns3d_n

    WRITE(14, '( (I8, 1X), 3(E17.8, 1X) )')      &
       id, ( ns3d_u( 3*(id-1)+i ), i = 1, 3 )

  END DO

  WRITE(14, '( 14I8 )') 13, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
  WRITE(14, '( (A, 1X) )') 'STRAIN_11, unit_unknown'
  WRITE(14, '( (A, 1X) )') 'STRAIN_22, unit_unknown'
  WRITE(14, '( (A, 1X) )') 'STRAIN_33, unit_unknown'
  WRITE(14, '( (A, 1X) )') 'STRAIN_12, unit_unknown'
  WRITE(14, '( (A, 1X) )') 'STRAIN_23, unit_unknown'
  WRITE(14, '( (A, 1X) )') 'STRAIN_31, unit_unknown'
  WRITE(14, '( (A, 1X) )') 'STRESS_11, Pa'
  WRITE(14, '( (A, 1X) )') 'STRESS_22, Pa'
  WRITE(14, '( (A, 1X) )') 'STRESS_33, Pa'
  WRITE(14, '( (A, 1X) )') 'STRESS_12, Pa'
  WRITE(14, '( (A, 1X) )') 'STRESS_23, Pa'
  WRITE(14, '( (A, 1X) )') 'STRESS_31, Pa'
  WRITE(14, '( (A, 1X) )') 'STRESS_MISES, Pa'

  DO ie = 1, es3d_n

    WRITE(14, '( (I8, 1X), 13(E17.8, 1X) )')                     &
          ie, ( esm3d_evec(i, ie), i = 1, 6 ),                   &
            ( esm3d_svec(i, ie), i = 1, 6 ), esm3d_s_mises(ie)

  END DO

  CLOSE(14)

  !--------------------------------------------------------------------

  DEALLOCATE( ns3d_x )
  DEALLOCATE( ns3d_u )

  DEALLOCATE( es3d_connectivity )

  DEALLOCATE( esm3d_evec )
  DEALLOCATE( esm3d_svec )
  DEALLOCATE( esm3d_s_mises )

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE run_appli
!####################################################################


! Finish appli
!####################################################################
SUBROUTINE finish_appli()
!####################################################################

  CALL del_nodes3d(ns3d)
  CALL del_localelement3d(le3d)
  CALL del_elements3d(es3d)
  CALL del_elemstiffmat3d(esm3d)
  CALL del_elemexforcevec3d(efv3d)
  CALL del_fem3d(fem3d)

  !--------------------------------------------------------------------

  DEALLOCATE( ns3d )
  DEALLOCATE( le3d )
  DEALLOCATE( es3d )
  DEALLOCATE( esm3d )
  DEALLOCATE( efv3d )
  DEALLOCATE( fem3d )

  !--------------------------------------------------------------------

  RETURN

  !####################################################################
  END SUBROUTINE finish_appli
  !####################################################################

!####################################################################
END MODULE mod_appli
