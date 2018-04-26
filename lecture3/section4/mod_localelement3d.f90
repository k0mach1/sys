MODULE mod_localelement3d
!####################################################################

IMPLICIT NONE

!--------------------------------------------------------------------

TYPE :: struct_localelement3d

  !--------------------------------------------------------

  PRIVATE

  !--------------------------------------------------------
  !
  ! nboundaries
  ! The total number of local element boundaries
  !
  ! nedges
  ! The total number of local element edges
  !
  ! volume
  ! Local element volume
  !
  ! area_boundary
  ! Local element boundary area
  !
  ! nnodes
  ! The total number of local nodes
  !
  ! nnodes_boundary
  ! The total number of nodes on a local element boundary
  !
  ! nedges_boundary
  ! The total number of edges on a local element boundary
  !
  ! nnodes_edge
  ! The total number of nodes on a local element edge
  !
  ! xi(:, :)
  ! Cartesian coordinates of a local node
  ! xi, eta, zeta
  !
  ! table_na(:, :)
  ! Table of local element boundary no. and local node no.
  !
  !--------------------------------------------------------
  !
  ! nqps
  ! The total number of quadrature points
  !
  ! xi_qp(:, :)
  ! Cartesian coordinates of a quadrature point
  ! xi_qp,  eta_qp, zeta_qp
  !
  ! w_qp(:, :)
  ! Weight of a quadrature point
  ! w_qp_xi, w_qp_eta, w_qp_zeta
  !
  ! n_qp(:, :)
  ! Shape function at a quadrature point
  ! N_qp
  !
  ! dndxi_qp(:, :, :)
  ! Partial derivatives of a shape function
  ! at a quadrature point
  ! (dN/dxi)_qp, (dN/deta)_qp, (dN/dzeta)_qp
  !
  !--------------------------------------------------------

  INTEGER :: nboundaries
  INTEGER :: nedges
  INTEGER :: nedges_boundary
  INTEGER :: nnodes
  INTEGER :: nnodes_boundary
  INTEGER :: nnodes_edge
  INTEGER, ALLOCATABLE :: table_na(:, :)
  INTEGER :: nqps

  REAL(8) :: volume
  REAL(8) :: area_boundary
  REAL(8), ALLOCATABLE :: xi(:, :)
  REAL(8), ALLOCATABLE :: xi_qp(:, :)
  REAL(8), ALLOCATABLE :: w_qp(:, :)
  REAL(8), ALLOCATABLE :: n_qp(:, :)
  REAL(8), ALLOCATABLE :: dndxi_qp(:, :, :)

  !--------------------------------------------------------

END TYPE struct_localelement3d

!--------------------------------------------------------------------

PRIVATE :: init_localelement3d_quadrature

!--------------------------------------------------------------------

CONTAINS


! Set the total number of local element boundaries
!####################################################################
SUBROUTINE set_localelement3d_nboundaries(le3d, nboundaries)
!####################################################################

  TYPE(struct_localelement3d), INTENT(INOUT) :: le3d

  INTEGER, INTENT(IN) :: nboundaries

  !--------------------------------------------------------------------

  le3d%nboundaries = nboundaries

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE set_localelement3d_nboundaries
!####################################################################


! Get the total number of local element boundaries
!####################################################################
SUBROUTINE get_localelement3d_nboundaries(le3d, nboundaries)
!####################################################################

  TYPE(struct_localelement3d), INTENT(IN) :: le3d

  INTEGER, INTENT(OUT) :: nboundaries

  !--------------------------------------------------------------------

  nboundaries = le3d%nboundaries

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE get_localelement3d_nboundaries
!####################################################################


! Get the total number of local element edges
!####################################################################
SUBROUTINE get_localelement3d_nedges(le3d, nedges)
!####################################################################

  TYPE(struct_localelement3d), INTENT(IN) :: le3d

  INTEGER, INTENT(OUT) :: nedges

  !--------------------------------------------------------------------

  nedges = le3d%nedges

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE get_localelement3d_nedges
!####################################################################


! Get local element volume
!####################################################################
SUBROUTINE get_localelement3d_volume(le3d, volume)
!####################################################################

  TYPE(struct_localelement3d), INTENT(IN) :: le3d

  REAL(8), INTENT(OUT) :: volume

  !--------------------------------------------------------------------

  volume = le3d%volume

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE get_localelement3d_volume
!####################################################################


! Get local element boundary area
!####################################################################
SUBROUTINE get_localelement3d_area_boundary(le3d, area_boundary)
!####################################################################

  TYPE(struct_localelement3d), INTENT(IN) :: le3d

  REAL(8), INTENT(OUT) :: area_boundary

  !--------------------------------------------------------------------

  area_boundary = le3d%area_boundary

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE get_localelement3d_area_boundary
!####################################################################


! Set the total number of nodes in a local element
!####################################################################
SUBROUTINE set_localelement3d_nnodes(le3d, nnodes)
!####################################################################

  TYPE(struct_localelement3d), INTENT(INOUT) :: le3d

  INTEGER, INTENT(IN) :: nnodes

  !--------------------------------------------------------------------

  le3d%nnodes = nnodes

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE set_localelement3d_nnodes
!####################################################################


! Get the total number of nodes in a local element
!####################################################################
SUBROUTINE get_localelement3d_nnodes(le3d, nnodes)
!####################################################################

  TYPE(struct_localelement3d), INTENT(IN) :: le3d

  INTEGER, INTENT(OUT) :: nnodes

  !--------------------------------------------------------------------

  nnodes = le3d%nnodes

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE get_localelement3d_nnodes
!####################################################################


! Get the total number of nodes on a local element boundary
!####################################################################
SUBROUTINE get_localelement3d_nnodes_boundary(le3d, nnodes_boundary)
!####################################################################

  TYPE(struct_localelement3d), INTENT(IN) :: le3d

  INTEGER, INTENT(OUT) :: nnodes_boundary

  !--------------------------------------------------------------------

  nnodes_boundary = le3d%nnodes_boundary

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE get_localelement3d_nnodes_boundary
!####################################################################


! Get the total number of edges on a local element boundary
!####################################################################
SUBROUTINE get_localelement3d_nedges_boundary(le3d, nedges_boundary)
!####################################################################

  TYPE(struct_localelement3d), INTENT(IN) :: le3d

  INTEGER, INTENT(OUT) :: nedges_boundary

  !--------------------------------------------------------------------

  nedges_boundary = le3d%nedges_boundary

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE get_localelement3d_nedges_boundary
!####################################################################


! Get the total number of nodes on a local element edge
!####################################################################
SUBROUTINE get_localelement3d_nnodes_edge(le3d, nnodes_edge)
!####################################################################

  TYPE(struct_localelement3d), INTENT(IN) :: le3d

  INTEGER, INTENT(OUT) :: nnodes_edge

  !--------------------------------------------------------------------

  nnodes_edge = le3d%nnodes_edge

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE get_localelement3d_nnodes_edge
!####################################################################

! Get Cartesian coordinates of a local node
!####################################################################
SUBROUTINE get_localelement3d_xi(le3d, xi)
!####################################################################

  TYPE(struct_localelement3d), INTENT(IN) :: le3d

  REAL(8), INTENT(OUT) :: xi(:, :)

  !--------------------------------------------------------------------

  xi = le3d%xi

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE get_localelement3d_xi
!####################################################################


! Get table of local element boundary no. and local node no.
!####################################################################
SUBROUTINE get_localelement3d_table_na(le3d, table_na)
!####################################################################

  TYPE(struct_localelement3d), INTENT(IN) :: le3d

  INTEGER, INTENT(OUT) :: table_na(:, :)

  !--------------------------------------------------------------------

  table_na = le3d%table_na

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE get_localelement3d_table_na
!####################################################################


! Set the total number of quadrature points
!####################################################################
SUBROUTINE set_localelement3d_nqps(le3d, nqps)
!####################################################################

  TYPE(struct_localelement3d), INTENT(INOUT) :: le3d

  INTEGER, INTENT(IN) :: nqps

  !--------------------------------------------------------------------

  le3d%nqps = nqps

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE set_localelement3d_nqps
!####################################################################


! Get the total number of quadrature points
!####################################################################
SUBROUTINE get_localelement3d_nqps(le3d, nqps)
!####################################################################

  TYPE(struct_localelement3d), INTENT(IN) :: le3d

  INTEGER, INTENT(OUT) :: nqps

  !--------------------------------------------------------------------

  nqps = le3d%nqps

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE get_localelement3d_nqps
!####################################################################


! Get Cartesian coordinates and weight of a quadrature point
!####################################################################
SUBROUTINE get_localelement3d_xi_w_qp(le3d, xi_qp, w_qp)
!####################################################################

  TYPE(struct_localelement3d), INTENT(IN) :: le3d

  REAL(8), INTENT(OUT) :: xi_qp(:, :)
  REAL(8), INTENT(OUT) :: w_qp(:, :)

  !--------------------------------------------------------------------

  xi_qp = le3d%xi_qp
  w_qp  = le3d%w_qp

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE get_localelement3d_xi_w_qp
!####################################################################


! Get shape function at a quadrature point
!####################################################################
SUBROUTINE get_localelement3d_n_qp(le3d, n_qp, dndxi_qp)
!####################################################################

  TYPE(struct_localelement3d), INTENT(IN) :: le3d

  REAL(8), INTENT(OUT) :: n_qp(:, :)
  REAL(8), INTENT(OUT) :: dndxi_qp(:, :, :)

  !--------------------------------------------------------------------

  n_qp = le3d%n_qp
  dndxi_qp = le3d%dndxi_qp

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE get_localelement3d_n_qp
!####################################################################

! Initialize localelement3d
!####################################################################
SUBROUTINE init_localelement3d(le3d, nboundaries, nnodes, nqps)
!####################################################################

  TYPE(struct_localelement3d), INTENT(INOUT) :: le3d

  INTEGER, INTENT(IN) :: nboundaries
  INTEGER, INTENT(IN) :: nnodes
  INTEGER, INTENT(IN) :: nqps

  !--------------------------------------------------------------------

  INTEGER :: nnodes_boundary
  INTEGER :: nnodes_edge

  !--------------------------------------------------------------------

  le3d%nboundaries = nboundaries

  !--------------------------------------------------------------

  ! Hexahedral element
  IF( nboundaries .EQ. 6 ) THEN

    le3d%nedges = 12

    le3d%nedges_boundary = 4

    le3d%volume = 8.0D0

    le3d%area_boundary = 4.0D0

  END IF

  !--------------------------------------------------------------

  le3d%nnodes = nnodes

  !--------------------------------------------------------------

  ALLOCATE( le3d%xi(3, nnodes)   )

  ! Hexahedral element
  IF( nboundaries .EQ. 6 ) THEN

    ! Linear element
    IF( nnodes .EQ. 8 ) THEN

      ! xi
      le3d%xi(1, 1)   = -1.0D0
      le3d%xi(1, 2)   =  1.0D0
      le3d%xi(1, 3)   =  1.0D0
      le3d%xi(1, 4)   = -1.0D0
      le3d%xi(1, 5)   = -1.0D0
      le3d%xi(1, 6)   =  1.0D0
      le3d%xi(1, 7)   =  1.0D0
      le3d%xi(1, 8)   = -1.0D0
      ! eta
      le3d%xi(2, 1)  = -1.0D0
      le3d%xi(2, 2)  = -1.0D0
      le3d%xi(2, 3)  =  1.0D0
      le3d%xi(2, 4)  =  1.0D0
      le3d%xi(2, 5)  = -1.0D0
      le3d%xi(2, 6)  = -1.0D0
      le3d%xi(2, 7)  =  1.0D0
      le3d%xi(2, 8)  =  1.0D0
      ! zeta
      le3d%xi(3, 1) = -1.0D0
      le3d%xi(3, 2) = -1.0D0
      le3d%xi(3, 3) = -1.0D0
      le3d%xi(3, 4) = -1.0D0
      le3d%xi(3, 5) =  1.0D0
      le3d%xi(3, 6) =  1.0D0
      le3d%xi(3, 7) =  1.0D0
      le3d%xi(3, 8) =  1.0D0

    END IF

  END IF

  !--------------------------------------------------------------

  ! Hexahedral element
  IF( nboundaries .EQ. 6 ) THEN

    ! Linear element
    IF( nnodes .EQ. 8 ) THEN

      nnodes_boundary = 4
      nnodes_edge = 2

    END IF

  END IF

  le3d%nnodes_boundary = nnodes_boundary
  le3d%nnodes_edge = nnodes_edge

  !--------------------------------------------------------------

  ALLOCATE( le3d%table_na(nnodes_boundary, nboundaries) )

  ! Hexahedral element
  IF( nboundaries .EQ. 6) THEN

    ! ma = 1
    le3d%table_na(1, 1) = 4
    le3d%table_na(2, 1) = 3
    le3d%table_na(3, 1) = 2
    le3d%table_na(4, 1) = 1
    ! ma = 2
    le3d%table_na(1, 2) = 5
    le3d%table_na(2, 2) = 6
    le3d%table_na(3, 2) = 7
    le3d%table_na(4, 2) = 8
    ! ma = 3
    le3d%table_na(1, 3) = 1
    le3d%table_na(2, 3) = 2
    le3d%table_na(3, 3) = 6
    le3d%table_na(4, 3) = 5
    ! ma = 4
    le3d%table_na(1, 4) = 2
    le3d%table_na(2, 4) = 3
    le3d%table_na(3, 4) = 7
    le3d%table_na(4, 4) = 6
    ! ma = 5
    le3d%table_na(1, 5) = 3
    le3d%table_na(2, 5) = 4
    le3d%table_na(3, 5) = 8
    le3d%table_na(4, 5) = 7
    ! ma = 6
    le3d%table_na(1, 6) = 4
    le3d%table_na(2, 6) = 1
    le3d%table_na(3, 6) = 5
    le3d%table_na(4, 6) = 8

  END IF

  !--------------------------------------------------------------------

  CALL init_localelement3d_quadrature(le3d, nqps)

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE init_localelement3d
!####################################################################


! Initialize localelement3d (Gaussian quadrature)
!####################################################################
SUBROUTINE init_localelement3d_quadrature(le3d, nqps)
!####################################################################

  TYPE(struct_localelement3d), INTENT(INOUT) :: le3d

  INTEGER, INTENT(IN) :: nqps

  !--------------------------------------------------------------------

  INTEGER :: na
  INTEGER :: i, j, k
  INTEGER :: nqps_tot
  INTEGER :: ijk

  REAL(8) :: coord(7, 7)
  REAL(8) :: weight(7, 7)
  REAL(8) :: xi, eta, zeta
  REAL(8) :: n_xi, n_eta, n_zeta
  REAL(8) :: dn_xi, dn_eta, dn_zeta

  !--------------------------------------------------------------------
  ! Coordinate and weight of Gaussian quadrature points

  ! Hexahedral element
  IF( le3d%nboundaries .EQ. 6 ) THEN
    !--------------------------------------------------------

    ! 1 point
    coord(1, 1) =  0.0D0
    ! 2 points
    coord(1, 2) = -0.577350269189626D0
    coord(2, 2) =  0.577350269189626D0
    ! 3 points
    coord(1, 3) = -0.774596669241483D0
    coord(2, 3) =  0.0D0
    coord(3, 3) =  0.774596669241483D0
    ! 4 points
    coord(1, 4) = -0.861136311594053D0
    coord(2, 4) = -0.339981043584856D0
    coord(3, 4) =  0.339981043584856D0
    coord(4, 4) =  0.861136311594053D0
    ! 5 points
    coord(1, 5) = -0.906179845938664D0
    coord(2, 5) = -0.538469310105683D0
    coord(3, 5) =  0.0D0
    coord(4, 5) =  0.538469310105683D0
    coord(5, 5) =  0.906179845938664D0
    ! 6 points
    coord(1, 6) = -0.932469514203152D0
    coord(2, 6) = -0.661209386466265D0
    coord(3, 6) = -0.238619186083197D0
    coord(4, 6) =  0.238619186083197D0
    coord(5, 6) =  0.661209386466265D0
    coord(6, 6) =  0.932469514203152D0
    ! 7 points
    coord(1, 7) = -0.949107912342758D0
    coord(2, 7) = -0.741531185599394D0
    coord(3, 7) = -0.405845151377397D0
    coord(4, 7) =  0.0D0
    coord(5, 7) =  0.405845151377397D0
    coord(6, 7) =  0.741531185599394D0
    coord(7, 7) =  0.949107912342758D0

    !--------------------------------------------------------

    ! 1 point
    weight(1, 1) = 2.0D0
    ! 2 points
    weight(1, 2) = 1.0D0
    weight(2, 2) = 1.0D0
    ! 3 points
    weight(1, 3) = 0.555555555555556D0
    weight(2, 3) = 0.888888888888889D0
    weight(3, 3) = 0.555555555555556D0
    ! 4 points
    weight(1, 4) = 0.3478548451D0
    weight(2, 4) = 0.6521451548D0
    weight(3, 4) = 0.6521451548D0
    weight(4, 4) = 0.3478548451D0
    ! 5 points
    weight(1, 5) = 0.236926885056189D0
    weight(2, 5) = 0.478628670499366D0
    weight(3, 5) = 0.568888888888889D0
    weight(4, 5) = 0.478628670499366D0
    weight(5, 5) = 0.236926885056189D0
    ! 6 points
    weight(1, 6) = 0.171324492379170D0
    weight(2, 6) = 0.360761573048139D0
    weight(3, 6) = 0.467913934572691D0
    weight(4, 6) = 0.467913934572691D0
    weight(5, 6) = 0.360761573048139D0
    weight(6, 6) = 0.171324492379170D0
    ! 7 points
    weight(1, 7) = 0.129484966168870D0
    weight(2, 7) = 0.279705391489277D0
    weight(3, 7) = 0.381830050505119D0
    weight(4, 7) = 0.417959183673469D0
    weight(5, 7) = 0.381830050505119D0
    weight(6, 7) = 0.279705391489277D0
    weight(7, 7) = 0.129484966168870D0

    !--------------------------------------------------------

  END IF

  !--------------------------------------------------------------------

  le3d%nqps = nqps

  ALLOCATE( le3d%xi_qp(3, nqps*nqps*nqps) )
  ALLOCATE( le3d%w_qp(3, nqps*nqps*nqps) )

  !--------------------------------------------------------------
  ! Hexahedral element
  IF( le3d%nboundaries .EQ. 6 ) THEN
    !--------------------------------------------------------
    ijk = 0

    DO k = 1, nqps

      DO j = 1, nqps

        DO i = 1, nqps

          ijk = ijk+1

          le3d%xi_qp(1, ijk) = coord(i, nqps)
          le3d%xi_qp(2, ijk) = coord(j, nqps)
          le3d%xi_qp(3, ijk) = coord(k, nqps)
          le3d%w_qp(1, ijk) = weight(i, nqps)
          le3d%w_qp(2, ijk) = weight(j, nqps)
          le3d%w_qp(3, ijk) = weight(k, nqps)

        END DO

      END DO

    END DO

  END IF

  nqps_tot = le3d%nqps*le3d%nqps*le3d%nqps

  ALLOCATE( le3d%n_qp(le3d%nnodes, nqps_tot) )
  ALLOCATE( le3d%dndxi_qp(3, le3d%nnodes, nqps_tot) )

  !--------------------------------------------------------------
  ! Linear element
  IF( le3d%nnodes .EQ. 8 ) THEN
  !--------------------------------------------------------
    DO ijk = 1, nqps_tot
      xi   = le3d%xi_qp(1, ijk)
      eta  = le3d%xi_qp(2, ijk)
      zeta = le3d%xi_qp(3, ijk)

      DO na = 1, 8

        n_xi   = 0.5D0*( 1.0D0+le3d%xi(1, na)*xi   )
        n_eta  = 0.5D0*( 1.0D0+le3d%xi(2, na)*eta  )
        n_zeta = 0.5D0*( 1.0D0+le3d%xi(3, na)*zeta )

        dn_xi   = 0.5D0*le3d%xi(1, na)
        dn_eta  = 0.5D0*le3d%xi(2, na)
        dn_zeta = 0.5D0*le3d%xi(3, na)

        le3d%n_qp(na, ijk) = n_xi*n_eta*n_zeta

        le3d%dndxi_qp(1, na, ijk) = dn_xi*n_eta *n_zeta
        le3d%dndxi_qp(2, na, ijk) = n_xi *dn_eta*n_zeta
        le3d%dndxi_qp(3, na, ijk) = n_xi *n_eta *dn_zeta

      END DO

    END DO

  END IF

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE init_localelement3d_quadrature
!####################################################################


! Delete localelement3d
!####################################################################
SUBROUTINE del_localelement3d(le3d)
!####################################################################

  TYPE(struct_localelement3d), INTENT(INOUT) :: le3d

!--------------------------------------------------------------------

  IF( le3d%nboundaries .EQ. 0 ) THEN

    RETURN

  END IF

!--------------------------------------------------------------------

  le3d%nboundaries = 0
  le3d%nedges = 0

  !--------------------------------------------------------------

  le3d%volume = 0.0D0
  le3d%area_boundary = 0.0D0

  !--------------------------------------------------------------

  le3d%nnodes = 0
  le3d%nnodes_boundary = 0
  le3d%nedges_boundary = 0
  le3d%nnodes_edge = 0

  DEALLOCATE( le3d%xi )

  !--------------------------------------------------------------

  DEALLOCATE( le3d%table_na )

  !--------------------------------------------------------------

  le3d%nqps = 0

  DEALLOCATE( le3d%xi_qp )
  DEALLOCATE( le3d%w_qp )


  DEALLOCATE( le3d%n_qp )
  DEALLOCATE( le3d%dndxi_qp )

!--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE del_localelement3d
!####################################################################


!####################################################################
END MODULE mod_localelement3d
