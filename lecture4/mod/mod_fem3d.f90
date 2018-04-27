MODULE mod_fem3d
!####################################################################

USE mod_nodes3d
USE mod_localelement3d
USE mod_elements3d
USE mod_elemstiffmat3d
USE mod_elemexforcevec3d

!--------------------------------------------------------------------

IMPLICIT NONE

!--------------------------------------------------------------------

TYPE :: struct_fem3d

  !--------------------------------------------------------

  PRIVATE

  !--------------------------------------------------------

  TYPE(struct_nodes3d), POINTER          :: ns3d  => NULL()
  TYPE(struct_localelement3d), POINTER   :: le3d  => NULL()
  TYPE(struct_elements3d), POINTER       :: es3d  => NULL()
  TYPE(struct_elemstiffmat3d), POINTER   :: esm3d => NULL()
  TYPE(struct_elemexforcevec3d), POINTER :: efv3d => NULL()

  !--------------------------------------------------------
  !
  ! ndofs
  ! The total number of DOFs
  !
  ! k(:, :)
  ! Stiffness matrix
  !
  ! f(:)
  ! External force vector
  !
  !--------------------------------------------------------
  !
  ! nnodes_loaded
  ! The total number of nodes with an equivalent nodal force
  !
  ! id_loaded(:)
  ! Node no. with an equivalent nodal force
  !
  ! f_loaded(:, :)
  ! Equivalent nodal force
  !
  !--------------------------------------------------------

  INTEGER :: ndofs
  INTEGER :: nnodes_loaded
  INTEGER, ALLOCATABLE :: id_loaded(:)

  REAL(8), ALLOCATABLE :: k(:, :)
  REAL(8), ALLOCATABLE :: f(:)
  REAL(8), ALLOCATABLE :: f_loaded(:, :)

  !--------------------------------------------------------

END TYPE struct_fem3d

!--------------------------------------------------------------------

PRIVATE :: cal_fem3d_stiffmat
PRIVATE :: cal_fem3d_rhs
PRIVATE :: cal_fem3d_dirichletbc
PRIVATE :: cal_fem3d_linearsolver

!--------------------------------------------------------------------

CONTAINS


! Get the total number of DOFs
!####################################################################
SUBROUTINE get_fem3d_ndofs(fem3d, ndofs)
!####################################################################

  TYPE(struct_fem3d), INTENT(IN) :: fem3d

  INTEGER, INTENT(OUT) :: ndofs

  !--------------------------------------------------------------------

  ndofs = fem3d%ndofs

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE get_fem3d_ndofs
!####################################################################


! Get stiffness matrix
!####################################################################
SUBROUTINE get_fem3d_k(fem3d, k)
!####################################################################

  TYPE(struct_fem3d), INTENT(IN) :: fem3d

  INTEGER, INTENT(OUT) :: k(:, :)

  !--------------------------------------------------------------------

  k = fem3d%k

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE get_fem3d_k
!####################################################################


! Get external force vector
!####################################################################
SUBROUTINE get_fem3d_f(fem3d, f)
!####################################################################

  TYPE(struct_fem3d), INTENT(IN) :: fem3d

  INTEGER, INTENT(OUT) :: f(:)

  !--------------------------------------------------------------------

  f = fem3d%f

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE get_fem3d_f
!####################################################################


! Set equivalent nodal force
!####################################################################
SUBROUTINE set_fem3d_f_loaded(fem3d, id_loaded, f_loaded)
!####################################################################

  TYPE(struct_fem3d), INTENT(INOUT) :: fem3d

  INTEGER, INTENT(IN) :: id_loaded(:)
  REAL(8), INTENT(IN) :: f_loaded(:, :)

  !--------------------------------------------------------------------

  fem3d%id_loaded = id_loaded
  fem3d%f_loaded  = f_loaded

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE set_fem3d_f_loaded
!####################################################################


! Get equivalent nodal force
!####################################################################
SUBROUTINE get_fem3d_f_loaded(fem3d, id_loaded, f_loaded)
!####################################################################

  TYPE(struct_fem3d), INTENT(IN) :: fem3d

  INTEGER, INTENT(OUT) :: id_loaded(:)
  REAL(8), INTENT(OUT) :: f_loaded(:, :)

  !--------------------------------------------------------------------

  id_loaded = fem3d%id_loaded
  f_loaded  = fem3d%f_loaded

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE get_fem3d_f_loaded
!####################################################################


! Initialize fem3d
!####################################################################
SUBROUTINE init_fem3d                              &
           (fem3d, ns3d, le3d, es3d, esm3d, efv3d, &
            nnodes_loaded)
!####################################################################

  TYPE(struct_fem3d), INTENT(INOUT) :: fem3d

  TYPE(struct_nodes3d), TARGET, INTENT(IN)
  TYPE(struct_localelement3d), TARGET, INTENT(IN)
  TYPE(struct_elements3d), TARGET, INTENT(IN)
  TYPE(struct_elemstiffmat3d), TARGET, INTENT(IN)
  TYPE(struct_elemexforcevec3d), TARGET, INTENT(IN)

  INTEGER, INTENT(IN) :: nnodes_loaded

  !--------------------------------------------------------------------

  INTEGER :: ns3d_n

  !--------------------------------------------------------------------

  fem3d%ns3d => ns3d   :: ns3d
  fem3d%le3d => le3d   :: le3d
  fem3d%es3d => es3d   :: es3d
  fem3d%esm3d => esm3d :: esm3d
  fem3d%efv3d => efv3d :: efv3d

  !--------------------------------------------------------------------

  CALL get_nodes3d_n(fem3d%ns3d, ns3d_n)

  !--------------------------------------------------------------------

  fem3d%ndofs = 3*ns3d_n

  !--------------------------------------------------------------

  ALLOCATE( fem3d%k(fem3d%ndofs, fem3d%ndofs) )

  fem3d%k = 0.0D0

  ALLOCATE( fem3d%f(fem3d%ndofs) )

  fem3d%f = 0.0D0

  !--------------------------------------------------------------

  fem3d%nnodes_loaded = nnodes_loaded

  ALLOCATE( fem3d%id_loaded(nnodes_loaded) )

  fem3d%id_loaded = 0

  ALLOCATE( fem3d%f_loaded(3, nnodes_loaded) )

  fem3d%f_loaded = 0.0D0

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE init_fem3d
!####################################################################


! Calculate fem3d
!####################################################################
SUBROUTINE cal_fem3d(fem3d)
!####################################################################

  TYPE(struct_fem3d), INTENT(INOUT) :: fem3d

  !--------------------------------------------------------------------

  ! Stiffness matrix
  CALL cal_fem3d_stiffmat(fem3d)

  !--------------------------------------------------------------------

  ! RHS vector
  CALL cal_fem3d_rhs(fem3d)

  !--------------------------------------------------------------------

  ! Dirichlet boundary condition
  CALL cal_fem3d_dirichletbc(fem3d)

  !--------------------------------------------------------------------

  ! Linear solver
  CALL cal_fem3d_linearsolver(fem3d)

  !--------------------------------------------------------------------

  ! Strain and stress
  CALL cal_elemstiffmat3d(fem3d%esm3d)

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE cal_fem3d
!####################################################################


! Calculate fem3d (stiffness matrix)
!####################################################################
SUBROUTINE cal_fem3d_stiffmat(fem3d)
!####################################################################

  TYPE(struct_fem3d), INTENT(INOUT) :: fem3d

  !--------------------------------------------------------------------

  INTEGER :: le3d_nnodes
  INTEGER :: es3d_n
  INTEGER, ALLOCATABLE :: es3d_connectivity(:, :)
  INTEGER :: i, j
  INTEGER :: id, jd
  INTEGER :: na, nb
  INTEGER :: ie
  INTEGER :: idof, jdof
  INTEGER :: isize, jsize

  REAL(8), ALLOCATABLE :: esm3d_k(:, :, :)
  REAL(8), ALLOCATABLE :: efv3d_f(:, :)

  !--------------------------------------------------------------------

  ! Element stiffness matrix
  CALL cal_elemstiffmat3d(fem3d%esm3d)

  !--------------------------------------------------------------------

  CALL get_localelement3d_nnodes(fem3d%le3d, le3d_nnodes)

  CALL get_elements3d_n(fem3d%es3d, es3d_n)
  ALLOCATE( es3d_connectivity(le3d_nnodes, es3d_n) )
  CALL get_elements3d_connectivity(fem3d%es3d, es3d_connectivity)

  ALLOCATE( esm3d_k(3*le3d_nnodes, 3*le3d_nnodes, es3d_n) )
  CALL get_elemstiffmat3d_k(fem3d%esm3d, esm3d_k)

  ! Stiffness matrix

  fem3d%k = 0.0D0

  DO ie = 1, es3d_n

    DO na = 1, le3d_nnodes

      id = es3d_connectivity(na, ie)

      DO i = 1, 3

        idof  = 3*(id-1)+i
        isize = 3*(na-1)+i

        DO nb = 1, le3d_nnodes

          jd = es3d_connectivity(nb, ie)

          DO j = 1, 3

            jdof  = 3*(jd-1)+j
            jsize = 3*(nb-1)+j

            fem3d%k(idof, jdof)                             &
            = fem3d%k(idof, jdof)+esm3d_k(isize, jsize, ie)

          END DO

        END DO

      END DO

    END DO

  END DO

  !--------------------------------------------------------------------

  DEALLOCATE( es3d_connectivity )

  DEALLOCATE( esm3d_k )

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE cal_fem3d_stiffmat
!####################################################################


! Calculate fem3d (RHS vector)
!####################################################################
SUBROUTINE cal_fem3d_rhs(fem3d)
!####################################################################

  TYPE(struct_fem3d), INTENT(INOUT) :: fem3d

  !--------------------------------------------------------------------

  INTEGER :: le3d_nnodes
  INTEGER :: es3d_n
  INTEGER, ALLOCATABLE :: es3d_connectivity(:, :)
  INTEGER :: i
  INTEGER :: id
  INTEGER :: id_l
  INTEGER :: na
  INTEGER :: ie
  INTEGER :: idof
  INTEGER :: isize

  REAL(8), ALLOCATABLE :: efv3d_f(:, :)

  !--------------------------------------------------------------------

  ! Element external force vector
  CALL cal_elemexforcevec3d(fem3d%efv3d)

  !--------------------------------------------------------------------

  CALL get_localelement3d_nnodes(fem3d%le3d, le3d_nnodes)

  CALL get_elements3d_n(fem3d%es3d, es3d_n)
  ALLOCATE( es3d_connectivity(le3d_nnodes, es3d_n) )
  CALL get_elements3d_connectivity(fem3d%es3d, es3d_connectivity)

  ALLOCATE( efv3d_f(3*le3d_nnodes, es3d_n) )
  CALL get_elemexforcevec3d_f(fem3d%efv3d, efv3d_f)

  !--------------------------------------------------------------

  ! External force vector

  fem3d%f = 0.0D0

  DO id_l = 1, fem3d%nnodes_loaded

    id = fem3d%id_loaded(id_l)

    DO i = 1, 3

      idof = 3*(id-1)+i

      fem3d%f(idof) = fem3d%f_loaded(i, id_l)

    END DO

  END DO

  DO ie = 1, es3d_n

    DO na = 1, le3d_nnodes

      id = es3d_connectivity(na, ie)

      DO i = 1, 3

        idof  = 3*(id-1)+i
        isize = 3*(na-1)+i

        fem3d%f(idof) = fem3d%f(idof)+efv3d_f(isize, ie)

      END DO

    END DO

  END DO

  !--------------------------------------------------------------------

  DEALLOCATE( es3d_connectivity )

  DEALLOCATE( efv3d_f )

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE cal_fem3d_rhs
!####################################################################


! Calculate fem3d (Dirichlet boundary condition)
!####################################################################
SUBROUTINE cal_fem3d_dirichletbc(fem3d)
!####################################################################

  TYPE(struct_fem3d), INTENT(INOUT) :: fem3d

  !--------------------------------------------------------------------

  INTEGER :: ns3d_n
  INTEGER, ALLOCATABLE :: ns3d_bc(:)
  INTEGER :: idof, jdof
  INTEGER :: idof_g
  INTEGER :: ndofs_given
  INTEGER, ALLOCATABLE :: idof_given(:)

  REAL(8), ALLOCATABLE :: ns3d_u(:)
  REAL(8) :: u_given

  !--------------------------------------------------------------------

  CALL get_nodes3d_n(fem3d%ns3d, ns3d_n)
  ALLOCATE( ns3d_u(fem3d%ndofs) )
  CALL get_nodes3d_u(fem3d%ns3d, ns3d_u)
  ALLOCATE( ns3d_bc(fem3d%ndofs) )
  CALL get_nodes3d_bc(fem3d%ns3d, ns3d_bc)

  !--------------------------------------------------------------------

  idof_g = 0

  DO idof = 1, fem3d%ndofs

    IF( ns3d_bc(idof) .EQ. 1 ) THEN

      idof_g = idof_g+1

    END IF

  END DO

  ndofs_given = idof_g

  !--------------------------------------------------------------

  ALLOCATE( idof_given(ndofs_given) )

  !--------------------------------------------------------------

  idof_g = 0

  DO idof = 1, fem3d%ndofs

    IF( ns3d_bc(idof) .EQ. 1 ) THEN

    idof_g = idof_g+1
    idof_given(idof_g) = idof

    END IF

  END DO

  !--------------------------------------------------------------------

  ! Dirichlet boundary conditions

  DO idof_g = 1, ndofs_given

    jdof = idof_given(idof_g)

    DO idof = 1, fem3d%ndofs

      fem3d%f(idof) = fem3d%f(idof)-fem3d%k(idof, jdof)*ns3d_u(jdof)

    END DO

  END DO

  DO idof_g = 1, ndofs_given

    idof = idof_given(idof_g)

    fem3d%f(idof) = ns3d_u(idof)

  END DO

  DO idof_g = 1, ndofs_given

    jdof = idof_given(idof_g)

    DO idof = 1, fem3d%ndofs

      fem3d%k(idof, jdof) = 0.0D0
      fem3d%k(jdof, idof) = 0.0D0

    END DO

    fem3d%k(jdof, jdof) = 1.0D0

  END DO

  !--------------------------------------------------------------------

  DEALLOCATE( ns3d_u )
  DEALLOCATE( ns3d_bc )

  DEALLOCATE( idof_given )

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE cal_fem3d_dirichletbc
!####################################################################


! Calculate fem3d (linear solver)
!####################################################################
SUBROUTINE cal_fem3d_linearsolver(fem3d)
!####################################################################

  TYPE(struct_fem3d), INTENT(INOUT) :: fem3d

  !--------------------------------------------------------------------

  INTEGER :: ipiv(fem3d%ndofs)
  INTEGER :: info

  !--------------------------------------------------------------------

  ! Linear solver

  ipiv = 0

  CALL set_nodes3d_u(fem3d%ns3d, fem3d%f)
  CALL DGESV                                        &
       (fem3d%ndofs, 1, fem3d%k, fem3d%ndofs, ipiv, &
        fem3d%f, fem3d%ndofs, info)

  CALL set_nodes3d_u(fem3d%ns3d, fem3d%f)

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE cal_fem3d_linearsolver
!####################################################################


! Delete fem3d
!####################################################################
SUBROUTINE del_fem3d(fem3d)
!####################################################################

  TYPE(struct_fem3d), INTENT(INOUT) :: fem3d

  !--------------------------------------------------------------------

  IF( fem3d%ndofs .EQ. 0 ) THEN

    RETURN

  END IF

  !--------------------------------------------------------------------

  NULLIFY( fem3d%ns3d )
  NULLIFY( fem3d%le3d )
  NULLIFY( fem3d%es3d )
  NULLIFY( fem3d%esm3d )
  NULLIFY( fem3d%efv3d )

  !--------------------------------------------------------------------

  fem3d%ndofs = 0

  !--------------------------------------------------------------

  DEALLOCATE( fem3d%k )
  DEALLOCATE( fem3d%f )

  !--------------------------------------------------------------------

  IF( fem3d%nnodes_loaded .EQ. 0 ) THEN

    RETURN

  END IF

  !--------------------------------------------------------------------

  fem3d%nnodes_loaded = 0

  !--------------------------------------------------------------

  DEALLOCATE( fem3d%id_loaded )
  DEALLOCATE( fem3d%f_loaded )

  !--------------------------------------------------------------------

  RETURN

!####################################################################
END SUBROUTINE del_fem3d
!####################################################################


!####################################################################
END MODULE mod_fem3d
