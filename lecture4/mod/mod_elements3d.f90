MODULE mod_elements3d
!####################################################################

USE mod_nodes3d
USE mod_localelement3d

!--------------------------------------------------------------------

IMPLICIT NONE

!--------------------------------------------------------------------

TYPE :: struct_elements3d

!--------------------------------------------------------

PRIVATE

!--------------------------------------------------------

TYPE(struct_nodes3d), POINTER        :: ns3d => NULL()
TYPE(struct_localelement3d), POINTER :: le3d => NULL()

!--------------------------------------------------------
!
! n
! The total number of elements
!
! connectivity(:. :)
! Connectivity between element no. and node no.
!
! volume(:)
! Element volume
!
! max_volume, max_ie_volume
! Maximum element volume and the element no.
!
! min_volume_min, min_ie_volume
! Minimum element volume and the element no.
!
! sum_volume
! The sum of element volumes
!
!--------------------------------------------------------

INTEGER :: n
INTEGER, ALLOCATABLE :: connectivity(:, :)
INTEGER :: ie_max_volume, ie_min_volume

REAL(8), ALLOCATABLE :: volume(:)
REAL(8) :: max_volume, min_volume
REAL(8) :: sum_volume

!--------------------------------------------------------

END TYPE struct_elements3d

!--------------------------------------------------------------------

CONTAINS


! Set the total number of elements
!####################################################################
SUBROUTINE set_elements3d_n(es3d, n)
!####################################################################

  TYPE(struct_elements3d), INTENT(INOUT) :: es3d

  INTEGER, INTENT(IN) :: n

!--------------------------------------------------------------------

  es3d%n = n

!--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE set_elements3d_n
!####################################################################

! Get the total number of elements
!####################################################################
SUBROUTINE get_elements3d_n(es3d, n)
!####################################################################

  TYPE(struct_elements3d), INTENT(IN) :: es3d

  INTEGER, INTENT(OUT) :: n

!--------------------------------------------------------------------

  n = es3d%n

!--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE get_elements3d_n
!####################################################################

! Set connectivity between element no. and node no.
!####################################################################
SUBROUTINE set_elements3d_connectivity(es3d, connectivity)
!####################################################################

  TYPE(struct_elements3d), INTENT(INOUT) :: es3d

  INTEGER, INTENT(IN) :: connectivity(:, :)

!--------------------------------------------------------------------

  es3d%connectivity = connectivity

!--------------------------------------------------------------------

RETURN

!####################################################################
END SUBROUTINE set_elements3d_connectivity
!####################################################################


! Get connectivity between element no. and node no.
!####################################################################
SUBROUTINE get_elements3d_connectivity(es3d, connectivity)
!####################################################################

  TYPE(struct_elements3d), INTENT(IN) :: es3d

  INTEGER, INTENT(OUT) :: connectivity(:, :)

!--------------------------------------------------------------------

  connectivity = es3d%connectivity

!--------------------------------------------------------------------

RETURN

!####################################################################
END SUBROUTINE get_elements3d_connectivity
!####################################################################


! Get element volume
!####################################################################

SUBROUTINE get_elements3d_volume       &
           (es3d, volume,              &
            max_volume, ie_max_volume, &
            min_volume, ie_min_volume, &
            sum_volume)

!####################################################################

  TYPE(struct_elements3d), INTENT(IN) :: es3d

  REAL(8), INTENT(OUT) :: volume(:)
  REAL(8), INTENT(OUT) :: max_volume
  INTEGER, INTENT(OUT) :: ie_max_volume
  REAL(8), INTENT(OUT) :: min_volume
  INTEGER, INTENT(OUT) :: ie_min_volume
  REAL(8), INTENT(OUT) :: sum_volume

!--------------------------------------------------------------------

  volume = es3d%volume

  max_volume = es3d%max_volume
  ie_max_volume = es3d%ie_max_volume

  min_volume = es3d%min_volume
  ie_min_volume = es3d%ie_min_volume

  sum_volume = es3d%sum_volume

!--------------------------------------------------------------------

RETURN

!####################################################################
END SUBROUTINE get_elements3d_volume
!####################################################################

! Initialize elements3d
!####################################################################
SUBROUTINE init_elements3d(es3d, ns3d, le3d, n)
!####################################################################

  TYPE(struct_elements3d), INTENT(INOUT) :: es3d

  TYPE(struct_nodes3d), TARGET, INTENT(IN)        :: ns3d
  TYPE(struct_localelement3d), TARGET, INTENT(IN) :: le3d

  INTEGER, INTENT(IN) :: n

!--------------------------------------------------------------------

  INTEGER :: le3d_nnodes

!--------------------------------------------------------------------

  es3d%ns3d => ns3d
  es3d%le3d => le3d

!--------------------------------------------------------------------

  CALL get_localelement3d_nnodes(es3d%le3d, le3d_nnodes)

!--------------------------------------------------------------------

  es3d%n = n

!--------------------------------------------------------------

  ALLOCATE( es3d%connectivity(le3d_nnodes, n) )

  es3d%connectivity = 0

!--------------------------------------------------------------

  ALLOCATE( es3d%volume(n) )

  es3d%volume = 0.0D0

!--------------------------------------------------------------

  es3d%max_volume = 0.0D0
  es3d%ie_max_volume = 0

  es3d%min_volume = 0.0D0
  es3d%ie_min_volume = 0

!--------------------------------------------------------------

  es3d%sum_volume = 0.0D0

!--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE init_elements3d
!####################################################################


! Calculate elements3d
!####################################################################
SUBROUTINE cal_elements3d(es3d)
!####################################################################

TYPE(struct_elements3d), INTENT(INOUT) :: es3d

  INTEGER :: ns3d_n
  INTEGER :: le3d_nnodes
  INTEGER :: le3d_nqps
  INTEGER :: nqps_tot
  INTEGER :: i
  INTEGER :: id
  INTEGER :: na
  INTEGER :: ie
  INTEGER :: ijk

  REAL(8), ALLOCATABLE :: ns3d_x(:, :)
  REAL(8), ALLOCATABLE :: le3d_xi_qp(:, :)
  REAL(8), ALLOCATABLE :: le3d_w_qp(:, :)
  REAL(8), ALLOCATABLE :: le3d_n_qp(:, :)
  REAL(8), ALLOCATABLE :: le3d_dndxi_qp(:, :, :)
  REAL(8), ALLOCATABLE :: x_local(:, :)
  REAL(8) :: w_xi, w_eta, w_zeta
  REAL(8) :: g1(3), g2(3), g3(3)
  REAL(8) :: det_j
  REAL(8) :: w_w_w_det_j

!--------------------------------------------------------------------

  CALL get_nodes3d_n(es3d%ns3d, ns3d_n)
  ALLOCATE( ns3d_x(3, ns3d_n) )
  CALL get_nodes3d_x(es3d%ns3d, ns3d_x)

  CALL get_localelement3d_nnodes(es3d%le3d, le3d_nnodes)
  CALL get_localelement3d_nqps(es3d%le3d, le3d_nqps)
  nqps_tot = le3d_nqps*le3d_nqps*le3d_nqps
  ALLOCATE( le3d_xi_qp(3, nqps_tot) )
  ALLOCATE( le3d_w_qp(3, nqps_tot) )
  CALL get_localelement3d_xi_w_qp(es3d%le3d, le3d_xi_qp, le3d_w_qp)
  ALLOCATE( le3d_n_qp(le3d_nnodes, nqps_tot) )
  ALLOCATE( le3d_dndxi_qp(3, le3d_nnodes, nqps_tot) )
  CALL get_localelement3d_n_qp(es3d%le3d, le3d_n_qp, le3d_dndxi_qp)

  ALLOCATE( x_local(3, le3d_nnodes) )

!--------------------------------------------------------------------

  DO ie = 1, es3d%n

    !--------------------------------------------------------

    es3d%volume(ie) = 0.0D0

    !--------------------------------------------------------

    DO na = 1, le3d_nnodes

      id = es3d%connectivity(na, ie)

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

      es3d%volume(ie) = es3d%volume(ie)+w_w_w_det_j

      !--------------------------------------------------

    END DO

  !--------------------------------------------------------
  END DO

!--------------------------------------------------------------------

  es3d%max_volume = MAXVAL( es3d%volume )

  es3d%ie_max_volume = 0

  DO ie = 1, es3d%n

   IF( es3d%volume(ie) .EQ. es3d%max_volume ) THEN

     es3d%ie_max_volume = ie

   END IF

  END DO

!--------------------------------------------------------------

  es3d%min_volume = MINVAL( es3d%volume )

  es3d%ie_min_volume = 0

  DO ie = 1, es3d%n

    IF( es3d%volume(ie) .EQ. es3d%min_volume ) THEN

      es3d%ie_min_volume = ie

    END IF

  END DO

!--------------------------------------------------------------

  es3d%sum_volume = SUM( es3d%volume )

!--------------------------------------------------------------------

  DEALLOCATE( ns3d_x )

  DEALLOCATE( le3d_xi_qp )
  DEALLOCATE( le3d_w_qp )
  DEALLOCATE( le3d_n_qp )
  DEALLOCATE( le3d_dndxi_qp )
  
  DEALLOCATE( x_local )

  RETURN

!####################################################################
END SUBROUTINE cal_elements3d
!####################################################################

      ! Delete elements3d
!####################################################################
SUBROUTINE del_elements3d(es3d)
!####################################################################

  TYPE(struct_elements3d), INTENT(INOUT) :: es3d

!--------------------------------------------------------------------

  IF ( es3d%n .EQ. 0 ) THEN

    RETURN

  END IF

!--------------------------------------------------------------------

  NULLIFY( es3d%ns3d )
  NULLIFY( es3d%le3d )

!--------------------------------------------------------------------

  es3d%n = 0

  !--------------------------------------------------------------

  DEALLOCATE( es3d%connectivity )

  !--------------------------------------------------------------

  DEALLOCATE( es3d%volume )

  !--------------------------------------------------------------

  es3d%max_volume = 0.0D0
  es3d%ie_max_volume = 0

  es3d%min_volume = 0.0D0
  es3d%ie_min_volume = 0

  !--------------------------------------------------------------

  es3d%sum_volume = 0.0D0

!--------------------------------------------------------------------

  RETURN

!####################################################################
  END SUBROUTINE del_elements3d
!####################################################################

!####################################################################
END MODULE mod_elements3d
