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

INTEGER :: nboundaries
INTEGER :: nedges
INTEGER :: nedges_boundary
INTEGER :: nnodes
INTEGER :: nnodes_boundary
INTEGER :: nnodes_edge
INTEGER, ALLOCATABLE :: table_na(:, :)

REAL(8) :: volume
REAL(8) :: area_boundary
REAL(8), ALLOCATABLE :: xi(:, :)

!--------------------------------------------------------

END TYPE struct_localelement3d

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
SUBROUTINE get_localelement3d_nnodes_boundary &
(le3d, nnodes_boundary)
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
SUBROUTINE get_localelement3d_nedges_boundary &
(le3d, nedges_boundary)
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


! Initialize localelement3d
!####################################################################
SUBROUTINE init_localelement3d &
(le3d, nboundaries, nnodes)
!####################################################################

  TYPE(struct_localelement3d), INTENT(INOUT) :: le3d

  INTEGER, INTENT(IN) :: nboundaries
  INTEGER, INTENT(IN) :: nnodes

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
    le3d%table_na(1, 1) =4
    le3d%table_na(2, 1) =3
    le3d%table_na(3, 1) =2
    le3d%table_na(4, 1) =1
    ! ma = 2
    le3d%table_na(1, 2) =5
    le3d%table_na(2, 2) =6
    le3d%table_na(3, 2) =7
    le3d%table_na(4, 2) =8
    ! ma = 3
    le3d%table_na(1, 3) =1
    le3d%table_na(2, 3) =2
    le3d%table_na(3, 3) =6
    le3d%table_na(4, 3) =5
    ! ma = 4
    le3d%table_na(1, 4) =2
    le3d%table_na(2, 4) =3
    le3d%table_na(3, 4) =7
    le3d%table_na(4, 4) =6
    ! ma = 5
    le3d%table_na(1, 5) =3
    le3d%table_na(2, 5) =4
    le3d%table_na(3, 5) =8
    le3d%table_na(4, 5) =7
    ! ma = 6
    le3d%table_na(1, 6) =4
    le3d%table_na(2, 6) =1
    le3d%table_na(3, 6) =5

    le3d%table_na(4, 6) = 8

  END IF

!--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE init_localelement3d
!####################################################################

! Delete localelement3d
!####################################################################
SUBROUTINE del_localelement3d(le3d)
!####################################################################

  TYPE(struct_localelement3d), INTENT(INOUT) :: le3d

!--------------------------------------------------------------------

  le3d%nboundaries = 0
  le3d%nedges = 0

!--------------------------------------------------------------

  le3d%volume = 0.0D0
  IF( le3d%nboundaries .EQ. 0 ) THEN
    RETURN
  END IF

  le3d%area_boundary = 0.0D0

!--------------------------------------------------------------

  le3d%nnodes = 0
  le3d%nnodes_boundary = 0
  le3d%nedges_boundary = 0
  le3d%nnodes_edge = 0

  DEALLOCATE( le3d%xi )

!--------------------------------------------------------------

  DEALLOCATE( le3d%table_na )

!--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE del_localelement3d
!####################################################################

!####################################################################
END MODULE mod_localelement3d
