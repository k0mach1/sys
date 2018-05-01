MODULE mod_elemexforcevec3d
!####################################################################

USE mod_nodes3d
USE mod_localelement3d
USE mod_elements3d

!--------------------------------------------------------------------

IMPLICIT NONE

!--------------------------------------------------------------------

TYPE :: struct_elemexforcevec3d

  !--------------------------------------------------------

  PRIVATE

  !--------------------------------------------------------

  TYPE(struct_nodes3d), POINTER        :: ns3d => NULL()
  TYPE(struct_localelement3d), POINTER :: le3d => NULL()
  TYPE(struct_elements3d), POINTER     :: es3d => NULL()

  !--------------------------------------------------------
  !
  ! f(:, :)
  ! Element external force vector
  !
  !--------------------------------------------------------
  !
  ! nelemboundaries
  ! The total number of element boundaries
  !
  ! table_ie(:)
  ! Table of element boundary no. and element no.
  !
  ! table_ma(:)
  ! Table of element boundary no. and
  ! boundary no. in a local element
  !
  ! t(:, :)
  ! Traction vector
  ! tx, ty, tz
  !
  !--------------------------------------------------------
  !
  ! rho(:)
  ! Density
  !
  ! g
  ! Gravitational acceleration
  !
  !--------------------------------------------------------

  INTEGER :: nelemboundaries
  INTEGER, ALLOCATABLE :: table_ie(:)
  INTEGER, ALLOCATABLE :: table_ma(:)

  REAL(8), ALLOCATABLE :: f(:, :)
  REAL(8), ALLOCATABLE :: t(:, :)
  REAL(8), ALLOCATABLE :: rho(:)
  REAL(8) :: g

  !--------------------------------------------------------

END TYPE struct_elemexforcevec3d

!--------------------------------------------------------------------

CONTAINS


! Get element external force vector
!####################################################################
SUBROUTINE get_elemexforcevec3d_f(efv3d, f)
!####################################################################

  TYPE(struct_elemexforcevec3d), INTENT(IN) :: efv3d

  REAL(8), INTENT(OUT) :: f(:, :)

  !--------------------------------------------------------------------

  f = efv3d%f

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE get_elemexforcevec3d_f
!####################################################################

! Get the total number of element boundaries
!####################################################################
SUBROUTINE get_elemexforcevec3d_nelemboundaries(efv3d, nelemboundaries)
!####################################################################

  TYPE(struct_elemexforcevec3d), INTENT(IN) :: efv3d

  INTEGER, INTENT(OUT) :: nelemboundaries

  !--------------------------------------------------------------------

  nelemboundaries = efv3d%nelemboundaries

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE get_elemexforcevec3d_nelemboundaries
!####################################################################


! Set traction vector
!####################################################################
SUBROUTINE set_elemexforcevec3d_t(efv3d, table_ie, table_ma, t)
!####################################################################

  TYPE(struct_elemexforcevec3d), INTENT(INOUT) :: efv3d

  INTEGER, INTENT(IN) :: table_ie(:)
  INTEGER, INTENT(IN) :: table_ma(:)
  REAL(8), INTENT(IN) :: t(:, :)

  !--------------------------------------------------------------------

  efv3d%table_ie = table_ie
  efv3d%table_ma = table_ma
  efv3d%t = t

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE set_elemexforcevec3d_t
!####################################################################


! Get traction vector
!####################################################################
SUBROUTINE get_elemexforcevec3d_t(efv3d, table_ie, table_ma, t)
!####################################################################

  TYPE(struct_elemexforcevec3d), INTENT(IN) :: efv3d

  INTEGER, INTENT(OUT) :: table_ie(:)
  INTEGER, INTENT(OUT) :: table_ma(:)
  REAL(8), INTENT(OUT) :: t(:, :)

  !--------------------------------------------------------------------

  table_ie = efv3d%table_ie
  table_ma = efv3d%table_ma
  t        = efv3d%t

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE get_elemexforcevec3d_t
!####################################################################

! Set density
!####################################################################
SUBROUTINE set_elemexforcevec3d_rho(efv3d, rho)
!####################################################################

  TYPE(struct_elemexforcevec3d), INTENT(INOUT) :: efv3d

  REAL(8), INTENT(IN) :: rho(:)

  !--------------------------------------------------------------------

  efv3d%rho = rho

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE set_elemexforcevec3d_rho
!####################################################################


! Get density
!####################################################################
SUBROUTINE get_elemexforcevec3d_rho(efv3d, rho)
!####################################################################

  TYPE(struct_elemexforcevec3d), INTENT(IN) :: efv3d

  REAL(8), INTENT(OUT) :: rho(:)

  !--------------------------------------------------------------------

  rho = efv3d%rho

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE get_elemexforcevec3d_rho
!####################################################################


! Set gravitational acceleration
!####################################################################
SUBROUTINE set_elemexforcevec3d_g(efv3d, g)
!####################################################################

  TYPE(struct_elemexforcevec3d), INTENT(INOUT) :: efv3d

  REAL(8), INTENT(IN) :: g

  !--------------------------------------------------------------------

  efv3d%g = g

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE set_elemexforcevec3d_g
!####################################################################


! Get gravitational acceleration
!####################################################################
SUBROUTINE get_elemexforcevec3d_g(efv3d, g)
!####################################################################

  TYPE(struct_elemexforcevec3d), INTENT(IN) :: efv3d

  REAL(8), INTENT(OUT) :: g

  !--------------------------------------------------------------------

  g = efv3d%g

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE get_elemexforcevec3d_g
!####################################################################


!####################################################################
SUBROUTINE init_elemexforcevec3d(efv3d, ns3d, le3d, es3d, nelemboundaries)
!####################################################################

  TYPE(struct_elemexforcevec3d), INTENT(INOUT) :: efv3d

  TYPE(struct_nodes3d), TARGET, INTENT(IN)        :: ns3d
  TYPE(struct_localelement3d), TARGET, INTENT(IN) :: le3d
  TYPE(struct_elements3d), TARGET, INTENT(IN)     :: es3d

  !--------------------------------------------------------------------

  INTEGER, INTENT(IN) :: nelemboundaries
  INTEGER :: le3d_nnodes
  INTEGER :: es3d_n

  !--------------------------------------------------------------------

  efv3d%ns3d => ns3d
  efv3d%le3d => le3d
  efv3d%es3d => es3d

  !--------------------------------------------------------------------

  CALL get_localelement3d_nnodes(efv3d%le3d, le3d_nnodes)

  CALL get_elements3d_n(efv3d%es3d, es3d_n)

  !--------------------------------------------------------------------

  ALLOCATE( efv3d%f(3*le3d_nnodes, es3d_n) )

  efv3d%f = 0.0D0

  !--------------------------------------------------------------

  efv3d%nelemboundaries = nelemboundaries

  ALLOCATE( efv3d%table_ie(nelemboundaries) )

  efv3d%table_ie = 0

  ALLOCATE( efv3d%table_ma(nelemboundaries) )

  efv3d%table_ma = 0

  ALLOCATE( efv3d%t(3, nelemboundaries) )

  efv3d%t = 0.0D0

  !--------------------------------------------------------------

  ALLOCATE( efv3d%rho(es3d_n) )

  efv3d%rho = 0.0D0

  efv3d%g = 0.0D0

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE init_elemexforcevec3d
!####################################################################

!####################################################################
SUBROUTINE cal_elemexforcevec3d(efv3d)
!####################################################################

  TYPE(struct_elemexforcevec3d), INTENT(INOUT) :: efv3d

  !--------------------------------------------------------------------

    INTEGER :: ns3d_n
    INTEGER :: le3d_nboundaries
    INTEGER :: le3d_nnodes
    INTEGER :: le3d_nnodes_boundary
    INTEGER, ALLOCATABLE :: le3d_table_na(:, :)
    INTEGER :: le3d_nqps
    INTEGER :: es3d_n
    INTEGER, ALLOCATABLE :: es3d_connectivity(:, :)
    INTEGER :: nqps_tot
    INTEGER :: i, k
    INTEGER :: id
    INTEGER :: ma
    INTEGER :: na, nb
    INTEGER :: naa
    INTEGER :: ie
    INTEGER :: ib
    INTEGER :: isize
    INTEGER :: jsize1, jsize2, jsize3
    INTEGER :: ijk

    REAL(8), ALLOCATABLE :: ns3d_x(:, :)
    REAL(8), ALLOCATABLE :: le3d_xi_qp(:, :)
    REAL(8), ALLOCATABLE :: le3d_w_qp(:, :)
    REAL(8), ALLOCATABLE :: le3d_n_qp(:, :)
    REAL(8), ALLOCATABLE :: le3d_dndxi_qp(:, :, :)
    REAL(8), ALLOCATABLE :: x_local(:, :)
    REAL(8) :: w_xi, w_eta, w_zeta
    REAL(8) :: g1(3), g2(3), g3(3)
    REAL(8) :: x31(3), x42(3)
    REAL(8) :: area, area_inv
    REAL(8) :: area_nx, area_ny, area_nz
    REAL(8) :: det_j
    REAL(8), ALLOCATABLE :: n(:)
    REAL(8) :: w_w_w_det_j
    REAL(8), ALLOCATABLE :: nmat(:, :)
    REAL(8) :: bvec(3)

  !--------------------------------------------------------------------

    CALL get_nodes3d_n(efv3d%ns3d, ns3d_n)
    ALLOCATE( ns3d_x(3, ns3d_n) )
    CALL get_nodes3d_x(efv3d%ns3d, ns3d_x)

    CALL get_localelement3d_nboundaries(efv3d%le3d, le3d_nboundaries)
    CALL get_localelement3d_nnodes(efv3d%le3d, le3d_nnodes)
    CALL get_localelement3d_nnodes_boundary &
         (efv3d%le3d, le3d_nnodes_boundary)
    ALLOCATE( le3d_table_na(le3d_nnodes_boundary, le3d_nboundaries) )
    CALL get_localelement3d_table_na(efv3d%le3d, le3d_table_na)
    CALL get_localelement3d_nqps(efv3d%le3d, le3d_nqps)
    nqps_tot = le3d_nqps*le3d_nqps*le3d_nqps
    ALLOCATE( le3d_xi_qp(3, nqps_tot) )
    ALLOCATE( le3d_w_qp(3, nqps_tot) )
    CALL get_localelement3d_xi_w_qp(efv3d%le3d, le3d_xi_qp, le3d_w_qp)
    ALLOCATE( le3d_n_qp(le3d_nnodes, nqps_tot) )
    ALLOCATE( le3d_dndxi_qp(3, le3d_nnodes, nqps_tot) )
    CALL get_localelement3d_n_qp(efv3d%le3d, le3d_n_qp, le3d_dndxi_qp)

    CALL get_elements3d_n(efv3d%es3d, es3d_n)
    ALLOCATE( es3d_connectivity(le3d_nnodes, es3d_n) )
    CALL get_elements3d_connectivity(efv3d%es3d, es3d_connectivity)

    ALLOCATE( x_local(3, le3d_nnodes) )
    ALLOCATE( n(le3d_nnodes_boundary) )
    ALLOCATE( nmat(3, 3*le3d_nnodes) )

  !--------------------------------------------------------------------

    efv3d%f = 0.0D0

    DO ib = 1, efv3d%nelemboundaries

      !--------------------------------------------------------

      ie = efv3d%table_ie(ib)
      ma = efv3d%table_ma(ib)

      DO naa = 1, le3d_nnodes_boundary

        na = le3d_table_na(naa, ma)
        id = es3d_connectivity(na, ie)

        DO i = 1, 3

          x_local(i, naa) = ns3d_x(i, id)

        END DO

      END DO

      IF( le3d_nboundaries .EQ. 6 ) THEN

        x31(1) = x_local(1, 3)-x_local(1, 1)
        x31(2) = x_local(2, 3)-x_local(2, 1)
        x31(3) = x_local(3, 3)-x_local(3, 1)

        x42(1) = x_local(1, 4)-x_local(1, 2)
        x42(2) = x_local(2, 4)-x_local(2, 2)
        x42(3) = x_local(3, 4)-x_local(3, 2)

        area_nx = 0.5D0*( x31(2)*x42(3)-x42(2)*x31(3) )
        area_ny = 0.5D0*( x31(3)*x42(1)-x42(3)*x31(1) )
        area_nz = 0.5D0*( x31(1)*x42(2)-x42(1)*x31(2) )

        area = DSQRT( area_nx*area_nx   &
                    +area_ny*area_ny   &
                    +area_nz*area_nz )

        det_j = 0.25D0*area

        IF( le3d_nnodes .EQ. 8 ) THEN

          n(1) = 1.0D0
          n(2) = 1.0D0
          n(3) = 1.0D0
          n(4) = 1.0D0

        END IF

     END IF

    DO naa = 1, le3d_nnodes_boundary

      na = le3d_table_na(naa, ma)

      isize = 3*(na-1)+1
      efv3d%f(isize, ie)                               &
      = efv3d%f(isize, ie)+n(naa)*efv3d%t(1, ib)*det_j

      isize = 3*(na-1)+2
      efv3d%f(isize, ie)                               &
      = efv3d%f(isize, ie)+n(naa)*efv3d%t(2, ib)*det_j

      isize = 3*(na-1)+3
      efv3d%f(isize, ie)                               &
      = efv3d%f(isize, ie)+n(naa)*efv3d%t(3, ib)*det_j

     END DO

    END DO

!--------------------------------------------------------------------

  DO ie = 1, es3d_n

    !--------------------------------------------------------

    DO na = 1, le3d_nnodes

      id = es3d_connectivity(na, ie)

      DO i = 1, 3

        x_local(i, na) = ns3d_x(i, id)

      END DO

    END DO

    !--------------------------------------------------------

    DO ijk = 1, nqps_tot

      !--------------------------------------------------

      ! Covariant basis vector
      DO i = 1, 3

        g1(i) = 0.0D0
        g2(i) = 0.0D0
        g3(i) = 0.0D0

        DO na = 1, le3d_nnodes

          g1(i) = g1(i)+le3d_dndxi_qp(1, na, ijk)*x_local(i, na)
          g2(i) = g2(i)+le3d_dndxi_qp(2, na, ijk)*x_local(i, na)
          g3(i) = g3(i)+le3d_dndxi_qp(3, na, ijk)*x_local(i, na)

        END DO

      END DO

      !--------------------------------------------------

      ! Jacobian
      det_j = g1(1)*( g2(2)*g3(3)-g2(3)*g3(2) ) &
             +g1(2)*( g2(3)*g3(1)-g2(1)*g3(3) ) &
             +g1(3)*( g2(1)*g3(2)-g2(2)*g3(1) )

      w_w_w_det_j                                             &
      = le3d_w_qp(1, ijk)*le3d_w_qp(2, ijk)*le3d_w_qp(3, ijk) &
        *det_j

      !--------------------------------------------------

      ! N matrix

      nmat = 0.0D0

      DO nb = 1, le3d_nnodes

        jsize1 = 3*(nb-1)+1
        jsize2 = 3*(nb-1)+2
        jsize3 = 3*(nb-1)+3

        nmat(1, jsize1) = le3d_n_qp(nb, ijk)
        nmat(2, jsize2) = le3d_n_qp(nb, ijk)
        nmat(3, jsize3) = le3d_n_qp(nb, ijk)

      END DO

      !--------------------------------------------------

      bvec(1) = 0.0D0
      bvec(2) = 0.0D0
      bvec(3) = efv3d%g

      !--------------------------------------------------

      DO isize = 1, 3*le3d_nnodes

        DO k = 1, 3

          efv3d%f(isize, ie)                      &
          = efv3d%f(isize, ie)                    &
          + w_w_w_det_j                           &
          *efv3d%rho(ie)*nmat(k, isize)*bvec(k)

        END DO

      END DO

      !--------------------------------------------------

    END DO

    !----------------------------------------------------

  END DO

!--------------------------------------------------------------------

  DEALLOCATE( ns3d_x )

  DEALLOCATE( le3d_table_na )
  DEALLOCATE( le3d_xi_qp )
  DEALLOCATE( le3d_w_qp )
  DEALLOCATE( le3d_n_qp )
  DEALLOCATE( le3d_dndxi_qp )

  DEALLOCATE( es3d_connectivity )

  DEALLOCATE( x_local )
  DEALLOCATE( n )
  DEALLOCATE( nmat )

!--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE cal_elemexforcevec3d
!####################################################################

!####################################################################
SUBROUTINE del_elemexforcevec3d(efv3d)
!####################################################################

  TYPE(struct_elemexforcevec3d), INTENT(INOUT) :: efv3d

  !--------------------------------------------------------------------

  NULLIFY( efv3d%ns3d )
  NULLIFY( efv3d%le3d )
  NULLIFY( efv3d%es3d )

  !--------------------------------------------------------------------

  DEALLOCATE( efv3d%f )

  !--------------------------------------------------------------

  DEALLOCATE( efv3d%table_ie )
  DEALLOCATE( efv3d%table_ma )
  DEALLOCATE( efv3d%t )


  !--------------------------------------------------------------

  DEALLOCATE( efv3d%rho )

  efv3d%g = 0.0D0

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE del_elemexforcevec3d
!####################################################################


!####################################################################
END MODULE mod_elemexforcevec3d
