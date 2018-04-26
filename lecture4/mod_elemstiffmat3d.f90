MODULE mod_elemstiffmat3d
!####################################################################

USE mod_nodes3d
USE mod_localelement3d
USE mod_elements3d

!--------------------------------------------------------------------

IMPLICIT NONE

!--------------------------------------------------------------------

TYPE :: struct_elemstiffmat3d

  !--------------------------------------------------------

  PRIVATE

  !--------------------------------------------------------

  TYPE(struct_nodes3d), POINTER        :: ns3d => NULL()
  TYPE(struct_localelement3d), POINTER :: le3d => NULL()
  TYPE(struct_elements3d), POINTER     :: es3d => NULL()

  !--------------------------------------------------------
  !
  ! k(:, :, :)
  ! Element stiffness matrix
  !
  !--------------------------------------------------------
  !
  ! e(:)
  ! Young's modulus
  !
  ! nu(:)
  ! Poisson's ratio
  !
  !--------------------------------------------------------
  !
  ! evec(:, :)
  ! Infinitesimal strain
  !
  ! svec(:, :)
  ! Stress
  !
  ! s_mises(:)
  ! Mises stress
  !
  !--------------------------------------------------------

  REAL(8), ALLOCATABLE :: k(:, :, :)
  REAL(8), ALLOCATABLE :: e(:)
  REAL(8), ALLOCATABLE :: nu(:)
  REAL(8), ALLOCATABLE :: evec(:, :)
  REAL(8), ALLOCATABLE :: svec(:, :)
  REAL(8), ALLOCATABLE :: s_mises(:)

  !--------------------------------------------------------

END TYPE

CONTAINS


! Get element stiffness matrix
!####################################################################
SUBROUTINE get_elemstiffmat3d_k(esm3d, k)
!####################################################################

  TYPE(struct_elemstiffmat3d), INTENT(IN) :: esm3d

  REAL(8), INTENT(OUT) :: k(:, :, :)

!--------------------------------------------------------------------

  k = esm3d%k

!--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE get_elemstiffmat3d_k
!####################################################################


! Set Young's modulus and Poisson's ratio
!####################################################################
SUBROUTINE set_elemstiffmat3d_e_nu(esm3d, e, nu)
!####################################################################

  TYPE(struct_elemstiffmat3d), INTENT(INOUT) :: esm3d

  REAL(8), INTENT(IN) :: e(:)
  REAL(8), INTENT(IN) :: nu(:)

!--------------------------------------------------------------------

  esm3d%e  = e
  esm3d%nu = nu

!--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE set_elemstiffmat3d_e_nu
!####################################################################


! Get Young's modulus and Poisson's ratio
!####################################################################
SUBROUTINE get_elemstiffmat3d_e(esm3d, e, nu)
!####################################################################

  TYPE(struct_elemstiffmat3d), INTENT(IN) :: esm3d

  REAL(8), INTENT(OUT) :: e(:)
  REAL(8), INTENT(OUT) :: nu(:)

!--------------------------------------------------------------------

  e  = esm3d%e
  nu = esm3d%nu

!--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE get_elemstiffmat3d_e
!####################################################################


! Get infinitesimal strain and stress
!####################################################################
SUBROUTINE get_elemstiffmat3d_evec_svec(esm3d, evec, svec, s_mises)
!####################################################################

  TYPE(struct_elemstiffmat3d), INTENT(IN) :: esm3d

  REAL(8), INTENT(OUT) :: evec(:, :)
  REAL(8), INTENT(OUT) :: svec(:, :)
  REAL(8), INTENT(OUT) :: s_mises(:)

!--------------------------------------------------------------------

  evec = esm3d%evec
  svec = esm3d%svec

  s_mises = esm3d%s_mises

!--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE get_elemstiffmat3d_evec_svec
!####################################################################

!####################################################################
SUBROUTINE init_elemstiffmat3d(esm3d, ns3d, le3d, es3d)
!####################################################################

  TYPE(struct_elemstiffmat3d), INTENT(INOUT) :: esm3d

  TYPE(struct_nodes3d), TARGET, INTENT(IN)        :: ns3d
  TYPE(struct_localelement3d), TARGET, INTENT(IN) :: le3d
  TYPE(struct_elements3d), TARGET, INTENT(IN)     :: es3d

!--------------------------------------------------------------------

  INTEGER :: le3d_nnodes
  INTEGER :: es3d_n

!--------------------------------------------------------------------

  esm3d%ns3d => ns3d
  esm3d%le3d => le3d
  esm3d%es3d => es3d

!--------------------------------------------------------------------

  CALL get_localelement3d_nnodes(esm3d%le3d, le3d_nnodes)

  CALL get_elements3d_n(esm3d%es3d, es3d_n)

!--------------------------------------------------------------------

  ALLOCATE( esm3d%k(3*le3d_nnodes, 3*le3d_nnodes, es3d_n) )

  esm3d%k = 0.0D0

  !--------------------------------------------------------------

  ALLOCATE( esm3d%e(es3d_n)  )
  ALLOCATE( esm3d%nu(es3d_n) )

  esm3d%e  = 0.0D0
  esm3d%nu = 0.0D0

  !--------------------------------------------------------------

  ALLOCATE( esm3d%evec(6, es3d_n) )

  esm3d%evec = 0.0D0

  ALLOCATE( esm3d%svec(6, es3d_n) )

  esm3d%svec = 0.0D0

  ALLOCATE( esm3d%s_mises(es3d_n) )

  esm3d%s_mises = 0.0D0

!--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE init_elemstiffmat3d
!####################################################################


!####################################################################
SUBROUTINE cal_elemstiffmat3d(esm3d)
!####################################################################

  TYPE(struct_elemstiffmat3d), INTENT(INOUT) :: esm3d

!--------------------------------------------------------------------

  INTEGER :: ns3d_n
  INTEGER :: le3d_nnodes
  INTEGER :: le3d_nqps
  INTEGER :: es3d_n
  INTEGER, ALLOCATABLE :: es3d_connectivity(:, :)
  INTEGER :: nqps_tot
  INTEGER :: i, j, k
  INTEGER :: id
  INTEGER :: na, nb
  INTEGER :: ie
  INTEGER :: idof
  INTEGER :: isize, jsize
  INTEGER :: jsize1, jsize2, jsize3
  INTEGER :: ijk

  REAL(8), ALLOCATABLE :: ns3d_x(:, :)
  REAL(8), ALLOCATABLE :: ns3d_u(:)
  REAL(8), ALLOCATABLE :: le3d_xi_qp(:, :)
  REAL(8), ALLOCATABLE :: le3d_w_qp(:, :)
  REAL(8), ALLOCATABLE :: le3d_n_qp(:, :)
  REAL(8), ALLOCATABLE :: le3d_dndxi_qp(:, :, :)
  REAL(8), ALLOCATABLE :: x_local(:, :)
  REAL(8), ALLOCATABLE :: u_local(:)
  REAL(8) :: nqps_tot_inv
  REAL(8) :: g1(3), g2(3), g3(3)
  REAL(8) :: det_j, det_j_inv
  REAL(8) :: w_w_w_det_j
  REAL(8) :: cg1(3), cg2(3), cg3(3)
  REAL(8), ALLOCATABLE :: dndx(:, :)
  REAL(8), ALLOCATABLE :: bmat(:, :)
  REAL(8) :: evec(6)
  REAL(8) :: lambda, mu
  REAL(8) :: dmat(6, 6)
  REAL(8), ALLOCATABLE :: cmat(:, :)
  REAL(8) :: svec(6)

!--------------------------------------------------------------------

  CALL get_nodes3d_n(esm3d%ns3d, ns3d_n)
  ALLOCATE( ns3d_x(3, ns3d_n) )
  CALL get_nodes3d_x(esm3d%ns3d, ns3d_x)
  ALLOCATE( ns3d_u(3*ns3d_n) )
  CALL get_nodes3d_u(esm3d%ns3d, ns3d_u)

  CALL get_localelement3d_nnodes(esm3d%le3d, le3d_nnodes)
  CALL get_localelement3d_nqps(esm3d%le3d, le3d_nqps)
  nqps_tot = le3d_nqps*le3d_nqps*le3d_nqps
  nqps_tot_inv = 1.0D0/DFLOAT( nqps_tot )
  ALLOCATE( le3d_xi_qp(3, nqps_tot) )
  ALLOCATE( le3d_w_qp(3, nqps_tot) )
  CALL get_localelement3d_xi_w_qp(esm3d%le3d, le3d_xi_qp, le3d_w_qp)
  ALLOCATE( le3d_n_qp(le3d_nnodes, nqps_tot) )
  ALLOCATE( le3d_dndxi_qp(3, le3d_nnodes, nqps_tot) )
  CALL get_localelement3d_n_qp(esm3d%le3d, le3d_n_qp, le3d_dndxi_qp)

  CALL get_elements3d_n(esm3d%es3d, es3d_n)
  ALLOCATE( es3d_connectivity(le3d_nnodes, es3d_n) )
  CALL get_elements3d_connectivity(esm3d%es3d, es3d_connectivity)

  ALLOCATE( x_local(3, le3d_nnodes) )
  ALLOCATE( u_local(3*le3d_nnodes) )
  ALLOCATE( dndx(3, le3d_nnodes) )
  ALLOCATE( bmat(6, 3*le3d_nnodes) )
  ALLOCATE( cmat(6, 3*le3d_nnodes) )

!--------------------------------------------------------------------

  esm3d%k = 0.0D0

  DO ie = 1, es3d_n

    !--------------------------------------------------------

    DO na = 1, le3d_nnodes

      id = es3d_connectivity(na, ie)

      DO i = 1, 3

        idof  = 3*(id-1)+i
        isize = 3*(na-1)+i

        x_local(i, na) = ns3d_x(i, id)
        u_local(isize) = ns3d_u(idof)

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

      det_j_inv = 1.0D0/det_j

      w_w_w_det_j                                             &
      = le3d_w_qp(1, ijk)*le3d_w_qp(2, ijk)*le3d_w_qp(3, ijk) &
      *det_j

      !--------------------------------------------------

      ! Contravariant basis vector
      cg1(1) = det_j_inv                    &
             *( g2(2)*g3(3)-g2(3)*g3(2) )
      cg1(2) = det_j_inv                    &
               *( g2(3)*g3(1)-g2(1)*g3(3) )
      cg1(3) = det_j_inv                    &
               *( g2(1)*g3(2)-g2(2)*g3(1) )
      cg2(1) = det_j_inv                    &
               *( g3(2)*g1(3)-g3(3)*g1(2) )
      cg2(2) = det_j_inv                    &
               *( g3(3)*g1(1)-g3(1)*g1(3) )
      cg2(3) = det_j_inv                    &
               *( g3(1)*g1(2)-g3(2)*g1(1) )
      cg3(1) = det_j_inv                    &
               *( g1(2)*g2(3)-g1(3)*g2(2) )
      cg3(2) = det_j_inv                    &
               *( g1(3)*g2(1)-g1(1)*g2(3) )
      cg3(3) = det_j_inv                    &
               *( g1(1)*g2(2)-g1(2)*g2(1) )

      !--------------------------------------------------

      DO na = 1, le3d_nnodes

        dndx(1, na)                        &
        = cg1(1)*le3d_dndxi_qp(1, na, ijk) &
        +cg2(1)*le3d_dndxi_qp(2, na, ijk) &
        +cg3(1)*le3d_dndxi_qp(3, na, ijk)
        dndx(2, na)                        &
        = cg1(2)*le3d_dndxi_qp(1, na, ijk) &
        +cg2(2)*le3d_dndxi_qp(2, na, ijk) &
        +cg3(2)*le3d_dndxi_qp(3, na, ijk)
        dndx(3, na)                        &
        = cg1(3)*le3d_dndxi_qp(1, na, ijk) &
        +cg2(3)*le3d_dndxi_qp(2, na, ijk) &
        +cg3(3)*le3d_dndxi_qp(3, na, ijk)

      END DO

      !--------------------------------------------------

      ! B matrix

      bmat = 0.0D0

      DO nb = 1, le3d_nnodes

        jsize1 = 3*(nb-1)+1
        jsize2 = 3*(nb-1)+2
        jsize3 = 3*(nb-1)+3


        bmat(1, jsize1) = dndx(1, nb)
        bmat(4, jsize1) = dndx(2, nb)
        bmat(6, jsize1) = dndx(3, nb)
        bmat(2, jsize2) = dndx(2, nb)
        bmat(4, jsize2) = dndx(1, nb)
        bmat(5, jsize2) = dndx(3, nb)
        bmat(3, jsize3) = dndx(3, nb)
        bmat(5, jsize3) = dndx(2, nb)
        bmat(6, jsize3) = dndx(1, nb)

      END DO

      !--------------------------------------------------

      evec = 0.0D0

      DO i = 1, 6

        DO jsize = 1, 3*le3d_nnodes

          evec(i) = evec(i)+bmat(i, jsize)*u_local(jsize)

        END DO

      END DO

      !------------------------------------------------

      lambda &
      = ( esm3d%e(ie)*esm3d%nu(ie) )                             &
        /( ( 1.0D0+esm3d%nu(ie) )*( 1.0D0-2.0D0*esm3d%nu(ie) ) )
      mu = esm3d%e(ie)/( 2.0D0*( 1.0D0+esm3d%nu(ie) ) )

      !------------------------------------------------

      ! D matrix

      dmat = 0.0D0

      dmat(1, 1) = lambda+2.0D0*mu
      dmat(2, 2) = lambda+2.0D0*mu
      dmat(3, 3) = lambda+2.0D0*mu
      dmat(1, 2) = lambda
      dmat(1, 3) = lambda
      dmat(2, 1) = lambda
      dmat(2, 3) = lambda
      dmat(3, 1) = lambda
      dmat(3, 2) = lambda
      dmat(4, 4) = mu
      dmat(5, 5) = mu
      dmat(6, 6) = mu

      !--------------------------------------------------

      DO i = 1, 6

        DO jsize = 1, 3*le3d_nnodes

          cmat(i, jsize) = 0.0D0

          DO k = 1, 6

            cmat(i, jsize) = cmat(i, jsize)+dmat(i, k)*bmat(k, jsize)

          END DO

        END DO

      END DO

      DO isize = 1, 3*le3d_nnodes

        DO jsize = 1, 3*le3d_nnodes

          DO k = 1, 6

            esm3d%k(isize, jsize, ie)                   &
            = esm3d%k(isize, jsize, ie)                 &
              +w_w_w_det_j*bmat(k, isize)*cmat(k, jsize)

          END DO

        END DO

      END DO

      !--------------------------------------------------

      svec = 0.0D0

      DO i = 1, 6

        DO j = 1, 6

          svec(i) = svec(i)+dmat(i, j)*evec(j)

        END DO

      END DO

      !--------------------------------------------------

      DO i = 1, 6

        esm3d%evec(i, ie) = esm3d%evec(i, ie)+evec(i)
        esm3d%svec(i, ie) = esm3d%svec(i, ie)+svec(i)

      END DO

      !--------------------------------------------------------

      DO I = 1, 6

        esm3d%evec(i, ie) = nqps_tot_inv*esm3d%evec(i, ie)
        esm3d%svec(i, ie) = nqps_tot_inv*esm3d%svec(i, ie)

      END DO

      esm3d%s_mises(ie)                                           &
      = DSQRT( 0.5D0*( ( esm3d%svec(1, ie)-esm3d%svec(2, ie) )**2 &
                      +( esm3d%svec(2, ie)-esm3d%svec(3, ie) )**2 &
                      +( esm3d%svec(3, ie)-esm3d%svec(1, ie) )**2 &
                      +6.0D0                                      &
                       *( esm3d%svec(4, ie)**2                    &
                         +esm3d%svec(5, ie)**2                    &
                         +esm3d%svec(6, ie)**2 ) ) )

      !--------------------------------------------------------

    END DO

!--------------------------------------------------------------------

  DEALLOCATE( ns3d_x )
  DEALLOCATE( ns3d_u )

  DEALLOCATE( le3d_xi_qp )
  DEALLOCATE( le3d_w_qp )
  DEALLOCATE( le3d_n_qp )
  DEALLOCATE( le3d_dndxi_qp )

  DEALLOCATE( es3d_connectivity )

  DEALLOCATE( x_local )
  DEALLOCATE( u_local )
  DEALLOCATE( dndx )
  DEALLOCATE( bmat )
  DEALLOCATE( cmat )

!--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE cal_elemstiffmat3d
!####################################################################


!####################################################################
SUBROUTINE del_elemstiffmat3d(esm3d)
!####################################################################

  TYPE(struct_elemstiffmat3d), INTENT(INOUT) :: esm3d

!--------------------------------------------------------------------

  NULLIFY( esm3d%ns3d )
  NULLIFY( esm3d%le3d )
  NULLIFY( esm3d%es3d )

!--------------------------------------------------------------------

  DEALLOCATE( esm3d%k )

  !--------------------------------------------------------------

  DEALLOCATE( esm3d%e  )
  DEALLOCATE( esm3d%nu )

  !--------------------------------------------------------------

  DEALLOCATE( esm3d%evec )
  DEALLOCATE( esm3d%svec )
  DEALLOCATE( esm3d%s_mises )

!--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE del_elemstiffmat3d
!####################################################################


!####################################################################
END MODULE mod_elemstiffmat3d
