!!  Copyright (C)  Stichting Deltares, 2012-2024.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.
module m_sgmres
    !! The Generalized Minimal Residual (GMRES) method is an iterative solver for nonsymmetric linear systems, and its
    !! implementation often relies on BLAS and LAPACK routines for efficient performance.

    use m_waq_precision
    use timers

    implicit none

    private
    public :: sgmres, initialize_gmres

contains


    !> Solver for Generalized Minimal Residual (GMRES)
    !! The solver preconditions with the gmres_pre_conditioner routine either:
    !!  - none
    !!  - upper triangular matrix
    !!  - lower triangular matrix
    !!  - both (this is the default preconditioner)
    !!  - constructs an orthonormal set of approximation vectors (Krylov space)
    !!  - if no convergence at end of Krilov space, solver restarts
    !!  - if no convergence at maxiter the solver stops
    subroutine sgmres (ntrace, rhs, sol, restrt, work, &
            ldw, hess, ldh, maxit, tol, &
            fast_solver_arr_size, amat, imat, diag, idiag, &
            klay, ioptpc, num_boundary_conditions, triwrk, iexseg, &
            lurep, litrep)

        use m_logger_helper, only: stop_with_error

        integer(kind = int_wp), intent(in) :: ntrace                       !< Dimension of the matrix
        real(kind = dp), intent(in) :: rhs(ntrace)                  !< right-hand side (1 substance)
        real(kind = dp), intent(inout) :: sol(ntrace)                  !< on entry: initial guess / on exit: solution
        integer(kind = int_wp), intent(in) :: restrt                       !< size of Krylov space, restrt < ntrace !
        real(kind = dp), intent(inout) :: work(ntrace, restrt + 5)     !< workspace
        integer(kind = int_wp), intent(in) :: ldw                          !< leading dimension >= max(1,ntrace  ) (probably superfluous lp)
        real(kind = dp), intent(in) :: hess(restrt + 1, restrt + 2) !< hessenberg matrix
        integer(kind = int_wp), intent(in) :: ldh                          !< leading dimension >= max(1,restrt+1) (probably superfluous lp)
        integer(kind = int_wp), intent(in) :: maxit                        !< maximum number of iterations
        real(kind = dp), intent(in) :: tol                          !< convergence criterion
        integer(kind = int_wp), intent(in) :: fast_solver_arr_size                        !< number of off-diagonal entries of matrix a (format from lp)
        real(kind = dp), intent(in) :: amat(fast_solver_arr_size)                  !< off-diagonal entries of matrix a (format from lp)
        integer(kind = int_wp), intent(in) :: imat(fast_solver_arr_size)                  !< pointer table off-diagonal entries
        real(kind = dp), intent(in) :: diag   (ntrace)              !< diagonal entries of matrix a
        integer(kind = int_wp), intent(in) :: idiag  (0:ntrace)            !< position of the diagonals in amat
        integer(kind = int_wp), intent(in) :: klay                         !< number of layers
        integer(kind = int_wp), intent(in) :: ioptpc                       !< option for preconditioner
        integer(kind = int_wp), intent(in) :: num_boundary_conditions                        !< number of open boundaries
        real(kind = dp), intent(inout) :: triwrk (klay * 6)            !< workspace for tridiagonal solution vertical
        integer(kind = int_wp), intent(in) :: iexseg (ntrace)              !< 0 for explicit volumes
        integer(kind = int_wp), intent(in) :: lurep                        !< Unit number report file
        logical, intent(in) :: litrep                       !< Perform report on iteration process if TRUE

        ! Local constants
        REAL(kind = dp) :: SMALL, SMALL2

        ! SMALL MUST always be larger then SMALL2 !!!!!!!!!!!
        PARAMETER (SMALL = 1.0E-7, SMALL2 = 1.0E-25)

        INTEGER(kind = int_wp) :: i, j, k, iter, av, cs, &
                &          sn, r, s, v, w, y, &
                &          i2, ierr, imax, iloop
        real(kind = dp) :: aa, bb, bnrm2, rnorm, resid, rmax

        logical   first
        data      first  /.true./
        integer(kind = int_wp) :: ithandl = 0

        if (timon) call timstrt ("sgmres", ithandl)

        ! sloppy way of output
        iter = 0
        ierr = 0
        if (first) then
            first = .false.
            if (litrep) then
                write(lurep, *) '   ITER       TOL  OPTION:   ', maxit, tol, ioptpc
                write(lurep, *) '   CYCLE    RESID  '
            end if
        end if

        ! Test the input parameters.
        if (ntrace < 0) then
            ierr = -1
        else if (ldw < max(1, ntrace)) then
            ierr = -2
        else if (maxit <= 0) then
            ierr = -3
        else if (ldh < restrt + 1) then
            ierr = -4
        endif
        if (ierr /= 0) goto 9999


        !     Alias workspace columns.
        r = 1
        s = r + 1
        w = s + 1
        y = w
        av = y + 1
        v = av + 1


        !     Store the Givens parameters in matrix H.
        cs = restrt + 1
        sn = cs + 1

        !   Adapt initial guess if necessary
        do iloop = 1, ntrace
            if (isnan(rhs(iloop))) then
                write (lurep, '(''ERROR: NaN in RHS of segment:'', i10)') iloop
                call stop_with_error()
            endif
        enddo
        bnrm2 = sqrt(sum(rhs * rhs))
        if (bnrm2 < small) sol = 0.0

        !     Set initial residual (AV is temporary workspace here).
        work(:, av) = rhs
        IF (sqrt(sum(sol * sol)) /= 0.0D+00) THEN
            CALL multiply_matrices (ntrace, fast_solver_arr_size, -1.0D+00, amat, imat, &
                    &                 DIAG, IDIAG, sol, 1.0D+00, WORK(1, AV))
        ENDIF

        CALL gmres_pre_conditioner (ntrace, WORK(1, R), WORK(1, AV), &
                &              fast_solver_arr_size, amat, imat, DIAG, IDIAG, &
                &              KLAY, IOPTPC, num_boundary_conditions, TRIWRK, iexseg)

        IF (BNRM2 == 0.0D+00) BNRM2 = 1.0D+00

        RESID = sqrt(sum(WORK(:, R) * work(:, r))) / BNRM2
        IF (LITREP) THEN
            WRITE (LUREP, '(''        Cycle   Residue       Norm-B '')')
            WRITE (LUREP, '(''GMRES'',I7,2E13.5)') 0, RESID, BNRM2
        ENDIF
        !jvb
        I = 0
        !jvb
        IF (BNRM2 < SMALL2) THEN
            IF (LITREP) THEN
                WRITE (LUREP, '(''NORM RHS < '',E12.4,'' HAS CONVERGED '')') SMALL2
            ENDIF
            GOTO 70
        ENDIF
        IF (RESID < TOL) GOTO 70



        !     Main GMRES iteration loop
        10 CONTINUE

        I = 0

        !        Construct the first column of V.

        work(:, v) = work(:, r)
        rnorm = sqrt(sum(work(:, v) * work(:, v)))
        work(:, v) = work(:, v) / rnorm

        !        Initialize S to the elementary vector E1 scaled by RNORM.

        WORK(1, S) = RNORM
        DO K = 2, ntrace
            WORK(K, S) = 0.0D+00
        end do

        30    CONTINUE

        I = I + 1
        ITER = ITER + 1

        CALL multiply_matrices (ntrace, fast_solver_arr_size, 1.0D+00, amat, imat, &
                &                    DIAG, IDIAG, WORK(1, V + I - 1), 0.0D+00, &
                &                    WORK(1, AV))
        CALL gmres_pre_conditioner (ntrace, WORK(1, W), WORK(1, AV), &
                &                    fast_solver_arr_size, amat, imat, DIAG, IDIAG, &
                &                    KLAY, IOPTPC, num_boundary_conditions, TRIWRK, iexseg)

        !           Construct I-th column of H orthnormal to the previous I-1 columns.

        CALL BASIS (I, ntrace, hess(1, I), WORK(1, V), LDW, WORK(1, W))

        !--         Each apply_plane_rotation is a multiplication with a plane rotation such that  --c
        !--         [ c  s ][h(k,i)  ] = [ g ]                                     --c
        !--         [-s  c ][h(k+1,i)] = [ 0 ]                                     --c

        DO K = 1, I - 1
            CALL apply_plane_rotation (1, hess(K, I), LDH, hess(K + 1, I), &
                    &                     LDH, hess(K, CS), hess(K, SN))
        end do


        ! Construct the I-th rot.matrix, and apply it to H so that H(I+1,I)=0.

        AA = hess(I, I)
        BB = hess(I + 1, I)
        CALL SROTG (AA, BB, hess(I, CS), hess(I, SN))
        CALL apply_plane_rotation  (1, hess(I, I), LDH, hess(I + 1, I), &
                &                   LDH, hess(I, CS), hess(I, SN))


        !           Apply the I-th rotation matrix to [ S(I), S(I+1) ]'. This
        !           gives an approximation of the residual norm. If less than
        !           tolerance, update the approximation vector sol and quit.

        CALL apply_plane_rotation (1, WORK(I, S), LDW, &
                &                  WORK(I + 1, S), LDW, hess(I, CS), &
                &                  hess(I, SN))

        RESID = ABS(WORK(I + 1, S)) / BNRM2

        IF (LITREP) THEN
            WRITE (LUREP, '(''GMRES'',I7,E13.5)') ITER, RESID
        ENDIF
        IF (RESID <= TOL) THEN
            CALL UPDATS (I, ntrace, sol, hess, &
                    &                       LDH, WORK(1, Y), WORK(1, S), &
                    &                       WORK(1, V), LDW)
            GO TO 70
            !KHT           GO TO 999
        ENDIF
        IF (ITER==MAXIT) GO TO 50
        IF (I<RESTRT)   GO TO 30

        50    CONTINUE

        !        Compute current solution vector sol

        CALL UPDATS (RESTRT, ntrace, sol, hess, LDH, &
                &                 WORK(1, Y), WORK(1, S), &
                &                 WORK(1, V), LDW)


        !        Compute residual vector R, find norm, then check for tolerance.
        !        (AV is temporary workspace here.)

        work(:, av) = rhs
        CALL multiply_matrices (ntrace, fast_solver_arr_size, -1.0D+00, amat, imat, &
                &                 DIAG, IDIAG, sol, 1.0D+00, WORK(1, AV))
        CALL gmres_pre_conditioner (ntrace, WORK(1, R), WORK(1, AV), &
                &                 fast_solver_arr_size, amat, imat, DIAG, IDIAG, &
                &                 KLAY, IOPTPC, num_boundary_conditions, TRIWRK, iexseg)
        work(i + 1, s) = sqrt(sum(work(:, r) * work(:, r)))
        RESID = WORK(I + 1, S) / BNRM2
        IF (RESID <= TOL) GO TO 70
        IF (ITER == MAXIT) GO TO 60

        !        Restart.

        IF (LITREP) THEN
            WRITE (LUREP, *) 'GMRES RESTARTING', RESID
        ENDIF
        GO TO 10

        60 CONTINUE


        !     Iteration fails.

        IERR = 1
        goto 9999

        70 CONTINUE

        !     Iteration successful; return.

        !     Filter solution for small negative values

        DO I2 = 1, ntrace
            IF (sol(I2) < 0.0D+00 .AND. sol(I2) > -TOL * 10) THEN
                sol(I2) = 0.0D+00
            ENDIF
        ENDDO

        work(:, av) = rhs
        CALL multiply_matrices (ntrace, fast_solver_arr_size, -1.0D+00, amat, imat, &
                &              DIAG, IDIAG, sol, 1.0D+00, WORK(1, AV))
        CALL gmres_pre_conditioner (ntrace, WORK(1, R), WORK(1, AV), &
                &          fast_solver_arr_size, amat, imat, DIAG, IDIAG, &
                &          KLAY, IOPTPC, num_boundary_conditions, TRIWRK, iexseg)
        work(i + 1, s) = sqrt(sum(work(:, r) * work(:, r)))
        RESID = WORK(I + 1, S) / BNRM2

        IF (LITREP) THEN
            WRITE (LUREP, '(''Cycles,T.U.P.E.RES,T.S.P.E.RES,BNRM2'',                               &
                    &         I8,3E13.5)')                                                                     &
                    &         ITER, WORK(I + 1, S), RESID, BNRM2
        ENDIF

        !    Check for true preconditioned residual and effects of the filter

        IF (RESID > TOL) THEN
            IF (LITREP) THEN
                WRITE(LUREP, *) ' AFTER FILTERING THE RESIDUAL EXCEEDS TOL ', RESID
            ENDIF
        ENDIF

        9999 if (timon) call timstop (ithandl)

        !        if on error, stop

        if (ierr > 0) then
            write (*, *) 'ERROR in GMRES', IERR
            write (LUrep, *) ' ERROR in GMRES 1', IERR
            write (LUrep, *) ' Solver did not reach convergence'
            write (LUrep, *) ' maximum contribution in error:', resid
            if (.not. litrep) WRITE (LUrep, *) ' Switch ITERATION REPORT to on to see details'
            write (LUrep, *) ' Reduce the output time step to 1 time step close to point of failure'
            write (lurep, *) ' Possible causes in decreasing frequency of likelyness:'
            write (lurep, *) ' 1. NaNs in the solution of water quality'
            write (lurep, *) '    inspect total mass in monitoring file for substance(s) on NaNs'
            write (lurep, *) ' 2. Inconsistency in drying and flooding of hydrodynamics'
            write (lurep, *) '    exact cause will be difficult to identify, cell nr. may help'
            write (lurep, *) ' 3. Normal lack of convergence, e.g. strongly diffusive problem'
            write (LUrep, *) '    possible solutions: increase TOLERANCE'
            write (LUrep, *) '                        decrease timestep'
            write (LUrep, *) '                        increase MAXITER'
            write (lurep, *) ' 4. other: exact cause will be difficult to identify, cell nr. may help'
            call stop_with_error()
        elseif (iERR < 0) then
            write (*, *) 'ERROR in GMRES', IERR
            write (LUrep, *) ' ERROR in GMRES 1', IERR
            write (LUrep, *) ' solver entered with wrong parameters'
            write (LUrep, *) ' consult the WAQ helpdesk'
        ENDIF

        RETURN
    end subroutine sgmres


    !> Update the GMRES iterated solution approximation.
    subroutine updats(i, n, x, h, ldh, y, s, v, ldv)

        integer(kind = int_wp) :: n, i, ldh, ldv
        real(kind = dp) :: x(*), y(i), s(i), h(ldh, *), v(ldv, *)
        integer(kind = int_wp) :: ithandl = 0

        if (timon) call timstrt ("updats", ithandl)

        ! Solve H*Y = S for upper triangualar H.
        y = s
        call strsv('UPPER', 'NOTRANS', 'NONUNIT', i, h, ldh, y, 1)

        ! Compute current solution vector X = X + V*Y.
        call sgemv('NOTRANS', n, i, 1.0d+00, v, ldv, y, 1, 1.0d+00, x, 1)

        if (timon) call timstop (ithandl)
    end subroutine updats

    !> Construct the I-th column of the upper Hessenberg matrix H
    !! using the Modified Gram-Schmidt process on V and W.
    subroutine basis (i, n, h, v, ldv, w)
        integer(kind = int_wp) :: i, n, ldv
        real(kind = dp) :: h(i + 1), w(n), v(n, i + 1)

        real(kind = dp) :: hscal, aux
        integer(kind = int_wp) :: k
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("basis", ithandl)

        do k = 1, i
            h(k) = sum(w * v(:, k))
            w = w - h(k) * v(:, k)
        end do

        ! re-orthogonalisation
        do k = 1, i
            aux = sum (w * v(:, k))
            h(k) = h(k) + aux
            w = w - aux * v(:, k)
        end do

        h(i + 1) = sqrt(sum(w * w))
        hscal = 1.0d+00 / max(h(i + 1), 1.0d-12)
        v(:, i + 1) = w * hscal

        if (timon) call timstop (ithandl)
    end subroutine basis

    !> Applies a plane rotation.
    !! Jack Dongarra, linpack, 3/11/78.
    subroutine apply_plane_rotation(vector_size, vector_x, increment_x, vector_y, increment_y, cosine_angle, sine_angle)
        real(kind = dp) :: vector_x(1), vector_y(1), stemp, cosine_angle, sine_angle
        integer(kind = int_wp) :: i, increment_x, increment_y, ix, iy, vector_size
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("apply_plane_rotation", ithandl)

        if(vector_size > 0) then
            if(increment_x == 1 .and. increment_y == 1) then
                do i = 1, vector_size
                    stemp = cosine_angle * vector_x(i) + sine_angle * vector_y(i)
                    vector_y(i) = cosine_angle * vector_y(i) - sine_angle * vector_x(i)
                    vector_x(i) = stemp
                end do
            else
                ix = 1
                iy = 1
                if(increment_x<0) ix = 1 + (1 - vector_size) * increment_x
                if(increment_y<0) iy = 1 + (1 - vector_size) * increment_y
                do i = 1, vector_size
                    stemp = cosine_angle * vector_x(ix) + sine_angle * vector_y(iy)
                    vector_y(iy) = cosine_angle * vector_y(iy) - sine_angle * vector_x(ix)
                    vector_x(ix) = stemp
                    ix = ix + increment_x
                    iy = iy + increment_y
                end do
            end if
        endif
        if (timon) call timstop (ithandl)
    end subroutine apply_plane_rotation

    !! Preconditioner for GMRES solver
    subroutine gmres_pre_conditioner(ntrace, x, rhs, fast_solver_arr_size, amat, &
            imat, diag, idiag, num_layers, ioptpc, &
            num_boundary_conditions, triwrk, iexseg)

        use m_logger_helper, only: stop_with_error

        integer(kind = int_wp), intent(in) :: ntrace               ! dimension of matrix (length of diagonal)
        real(kind = dp), intent(out) :: x     (ntrace)     ! the solution of Mx = y
        real(kind = dp), intent(inout) :: rhs   (ntrace)     ! right hand side of P-solve only
        ! this vector may be changed on exit!!
        integer(kind = int_wp), intent(in) :: fast_solver_arr_size                ! number of off-diagonal entries matrix
        real(kind = dp), intent(in) :: amat  (fast_solver_arr_size)     ! off-diagonal entries matrix LP format
        integer(kind = int_wp), intent(in) :: imat  (fast_solver_arr_size)     ! collumn nrs of off-diagonal entries matrix
        real(kind = dp), intent(in) :: diag  (ntrace)     ! diagonal entries of matrix
        integer(kind = int_wp), intent(in) :: idiag (0:ntrace)     ! start of rows in amat
        integer(kind = int_wp), intent(in) :: num_layers                ! number of layers in the vertical
        integer(kind = int_wp), intent(in) :: ioptpc               ! = 0 no preconditioning
        ! = 1 L-GS preconditioning
        ! = 2 U-GS preconditioning
        ! = 3 SSOR preconditioning
        integer(kind = int_wp), intent(in) :: num_boundary_conditions                ! number of open boundaries
        real(kind = dp), intent(inout) :: triwrk(num_layers)     ! work array for vertical double sweep
        integer(kind = int_wp), intent(in) :: iexseg(ntrace)     ! 0 for explicit volumes

        !        local variables

        integer(kind = int_wp) :: num_cells                ! nr of volumes
        integer(kind = int_wp) :: nsegl                ! nr of volumes per layer
        integer(kind = int_wp) :: iadd                 ! 0 for 2DH, 2 for 3D
        integer(kind = int_wp) :: cell_i                 ! this volume
        integer(kind = int_wp) :: jcol                 ! collumn counter for off-diagonals
        integer(kind = int_wp) :: ilow, ihigh          ! loop boundaries

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("gmres_pre_conditioner", ithandl)

        if (num_layers == 1) then
            iadd = 0
        else
            iadd = 2
        endif

        num_cells = ntrace - num_boundary_conditions
        nsegl = num_cells / num_layers
        if (nsegl * num_layers /= num_cells) then
            write(*, *) 'ERROR in gmres_pre_conditioner'
            call stop_with_error()
        endif

        if (ioptpc == 0) then

            x = rhs

        else if(ioptpc == 1) then

            call solve_lower_triangular_matrix (ntrace, num_cells, num_layers, nsegl, fast_solver_arr_size, &
                    amat, imat, diag, idiag, x, &
                    rhs, triwrk, iadd, iexseg)

        else if(ioptpc == 2) then

            call solve_upper_triangular_matrix (ntrace, num_layers, nsegl, fast_solver_arr_size, amat, &
                    imat, diag, idiag, x, rhs, &
                    triwrk, iadd, iexseg)

        else if(ioptpc == 3) then

            !            SSOR (Symmetric Successive Over Relaxation)

            !            M = (D - L) "D^-1" (D - U)

            call solve_lower_triangular_matrix (ntrace, num_cells, num_layers, nsegl, fast_solver_arr_size, &
                    amat, imat, diag, idiag, x, &
                    rhs, triwrk, iadd, iexseg)

            !        THe "D^{-1}" part, note that due to the b.c entries this is
            !        a rather peculiar piece of code

            if (num_layers == 1) then

                !              diagonal element is scalar

                do cell_i = 1, ntrace

                    rhs(cell_i) = diag(cell_i) * x(cell_i)

                    !              extra "b.c" entries

                    ilow = idiag(cell_i - 1) + 1
                    ihigh = idiag(cell_i)
                    do jcol = ilow + iadd, ihigh
                        if (imat(jcol) > num_cells) then
                            rhs(cell_i) = rhs(cell_i) + amat(jcol) * x(imat(jcol))
                        endif
                    enddo

                enddo

            else

                !              diagonal element is tridiagonal K x K matrix
                !              but we can simply loop over the num_cells (=N-num_boundary_conditions) segments
                !              There has been a bug in this section already from the start.
                !              the first layer has no layer above and the last layer has
                !              no layer below.

                do cell_i = 1, num_cells
                    ilow = idiag(cell_i - 1) + 1
                    rhs(cell_i) = diag(cell_i) * x(cell_i)
                    if (imat(ilow) > 0) rhs(cell_i) = rhs(cell_i) + amat(ilow) * x(imat(ilow))
                    if (imat(ilow + 1) > 0) rhs(cell_i) = rhs(cell_i) + amat(ilow + 1) * x(imat(ilow + 1))

                    !              extra "b.c." entries

                    if (iexseg(cell_i) == 0) cycle
                    ihigh = idiag(cell_i)
                    do jcol = ilow + iadd, ihigh
                        if (imat(jcol) > num_cells) then
                            rhs(cell_i) = rhs(cell_i) + amat(jcol) * x(imat(jcol))
                        endif
                    enddo
                enddo
                do cell_i = num_cells + 1, ntrace
                    rhs(cell_i) = diag(cell_i) * x(cell_i)
                enddo

            endif

            call solve_upper_triangular_matrix (ntrace, num_layers, nsegl, fast_solver_arr_size, amat, &
                    imat, diag, idiag, x, rhs, &
                    triwrk, iadd, iexseg)

        else
            write(*, *) ' This option for Pre-Conditioning '
            write(*, *) ' is not implemented :   ABORT     '
            call stop_with_error()
        endif

        if (timon) call timstop (ithandl)
    end subroutine gmres_pre_conditioner


    !! solve with lower triangular matrix:
    subroutine solve_upper_triangular_matrix(ntrace, num_layers, nsegl, fast_solver_arr_size, amat, &
            imat, diag, idiag, x, rhs, &
            triwrk, iadd, iexseg)

        ! Let A = (D - L - U) , solve (D-L) x = y
        ! [ How about (I - L/D) x = y ?? ]

        integer(kind = int_wp), intent(in) :: ntrace               ! dimension of matrix (length of diagonal)
        integer(kind = int_wp), intent(in) :: num_layers                ! number of layers in the vertical
        integer(kind = int_wp), intent(in) :: nsegl                ! number of volumes per layer
        integer(kind = int_wp), intent(in) :: fast_solver_arr_size           ! number of off-diagonal entries matrix
        real(kind = dp), intent(in) :: amat  (fast_solver_arr_size)     ! off-diagonal entries matrix
        integer(kind = int_wp), intent(in) :: imat  (fast_solver_arr_size) ! collumn nrs of off-diagonal entries matrix
        real(kind = dp), intent(in) :: diag  (ntrace)     ! diagonal entries of matrix
        integer(kind = int_wp), intent(in) :: idiag (0:ntrace)     ! start of rows in amat
        real(kind = dp), intent(out) :: x     (ntrace)     ! x = M^{-1} y
        real(kind = dp), intent(in) :: rhs   (ntrace)     ! right hand side of this iteration only
        real(kind = dp), intent(inout) :: triwrk(num_layers)     ! work array for vertical double sweep
        integer(kind = int_wp), intent(in) :: iadd                 ! offset for vertical off-diagonals
        integer(kind = int_wp), intent(in) :: iexseg(ntrace)     ! = 0 if volume is fully explicit

        ! local variables
        real(kind = dp) :: pivot                ! multiplier in double sweep vertical
        integer(kind = int_wp) :: isegl                ! this volume of one layer
        integer(kind = int_wp) :: cell_i                 ! this volume
        integer(kind = int_wp) :: ilay                 ! this layer
        integer(kind = int_wp) :: jcol                 ! collumn counter for off-diagonals
        integer(kind = int_wp) :: ilow, ihigh          ! loop boundaries

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("solve_upper_triangular_matrix", ithandl)

        ! First copy rhs into x

        x = rhs

        ! loop over all interior grid points in top layer.
        do isegl = nsegl, 1, -1

            do ilay = 1, num_layers

                cell_i = isegl + (ilay - 1) * nsegl
                if (iexseg(cell_i) == 0) cycle
                ilow = idiag(cell_i - 1) + 1
                ihigh = idiag(cell_i)
                do jcol = ilow + iadd, ihigh
                    if (imat(jcol) > cell_i) x(cell_i) = x(cell_i) - amat(jcol) * x(imat(jcol))
                enddo
            enddo

            if (num_layers == 1) then

                x(isegl) = x(isegl) / diag(isegl)

            else

                ! direct tridiagonal solver expanded in this code
                pivot = diag(isegl)
                x(isegl) = x(isegl) / pivot
                ilow = idiag(isegl - 1) + 2
                cell_i = isegl
                triwrk(1) = amat(ilow) / pivot

                do ilay = 2, num_layers
                    cell_i = cell_i + nsegl
                    pivot = diag(cell_i) - amat(idiag(cell_i - 1) + 1) * triwrk(ilay - 1)
                    x(cell_i) = (x   (cell_i) - amat(idiag(cell_i - 1) + 1) * x(cell_i - nsegl)) / pivot
                    triwrk(ilay) = amat(idiag(cell_i - 1) + 2) / pivot
                enddo

                do ilay = num_layers - 1, 1, -1
                    x(isegl + (ilay - 1) * nsegl) = x(isegl + (ilay - 1) * nsegl) - triwrk(ilay) * x(isegl + ilay * nsegl)
                enddo

            endif
        enddo

        if (timon) call timstop (ithandl)

    end subroutine solve_upper_triangular_matrix


    !! solve with lower triangular matrix:
    subroutine solve_lower_triangular_matrix (ntrace, num_cells, num_layers, nsegl, fast_solver_arr_size, &
            amat, imat, diag, idiag, x, &
            rhs, triwrk, iadd, iexseg)

        ! Let A = (D - L - U) , solve (D-L) x = y
        ! [ How about (I - L/D) x = y ?? ]

        integer(kind = int_wp), intent(in) :: ntrace               ! dimension of matrix (length of diagonal)
        integer(kind = int_wp), intent(in) :: num_cells                ! number of volumes
        integer(kind = int_wp), intent(in) :: num_layers                ! number of layers in the vertical
        integer(kind = int_wp), intent(in) :: nsegl                ! number of volumes per layer
        integer(kind = int_wp), intent(in) :: fast_solver_arr_size         ! number of off-diagonal entries matrix
        real(kind = dp), intent(in) :: amat  (fast_solver_arr_size)     ! off-diagonal entries matrix
        integer(kind = int_wp), intent(in) :: imat  (fast_solver_arr_size) ! collumn nrs of off-diagonal entries matrix
        real(kind = dp), intent(in) :: diag  (ntrace)     ! off-diagonal entries matrix
        integer(kind = int_wp), intent(in) :: idiag (0:ntrace)     ! start of row in amat
        real(kind = dp), intent(out) :: x     (ntrace)     ! x = M^{-1} y
        real(kind = dp), intent(in) :: rhs   (ntrace)     ! right hand side of this iteration only
        real(kind = dp), intent(inout) :: triwrk(num_layers)     ! work array for vertical double sweep
        integer(kind = int_wp), intent(in) :: iadd                 ! offset for vertical off-diagonals
        integer(kind = int_wp), intent(in) :: iexseg(ntrace)     ! = 0 if volume is fully explicit

        !        local variables

        real(kind = dp) :: pivot                ! multiplier in double sweep vertical
        integer(kind = int_wp) :: isegl                ! this volume of one layer
        integer(kind = int_wp) :: cell_i                 ! this volume
        integer(kind = int_wp) :: ilay                 ! this layer
        integer(kind = int_wp) :: jcol                 ! collumn counter for off-diagonals
        integer(kind = int_wp) :: ilow, ihigh          ! loop boundaries

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("solve_lower_triangular_matrix", ithandl)

        !        First copy rhs into x

        x = rhs

        !        loop over all interior grid points in top layer.

        do isegl = 1, nsegl

            do ilay = 1, num_layers

                cell_i = isegl + (ilay - 1) * nsegl
                if (iexseg(cell_i) == 0) cycle
                ilow = idiag(cell_i - 1) + 1
                ihigh = idiag(cell_i)
                do jcol = ilow + iadd, ihigh
                    if (imat(jcol) > 0 .and. (imat(jcol) < cell_i .or. imat(jcol) > num_cells)) &
                            x(cell_i) = x(cell_i) - amat(jcol) * x(imat(jcol))
                enddo
            enddo

            if (num_layers == 1) then

                x(isegl) = x(isegl) / diag(isegl)

            else

                !           direct tridiagonal solver expanded in this code

                pivot = diag(isegl)
                x(isegl) = x(isegl) / pivot
                ilow = idiag(isegl - 1) + 2
                cell_i = isegl
                triwrk(1) = amat(ilow) / pivot
                do ilay = 2, num_layers
                    cell_i = cell_i + nsegl
                    pivot = diag(cell_i) - amat(idiag(cell_i - 1) + 1) * triwrk(ilay - 1)
                    x(cell_i) = (x   (cell_i) - amat(idiag(cell_i - 1) + 1) * x(cell_i - nsegl)) / pivot
                    triwrk(ilay) = amat(idiag(cell_i - 1) + 2) / pivot
                enddo
                do ilay = num_layers - 1, 1, -1
                    x(isegl + (ilay - 1) * nsegl) = x(isegl + (ilay - 1) * nsegl) - triwrk(ilay) * x(isegl + ilay * nsegl)
                enddo

            endif
        enddo

        if (timon) call timstop (ithandl)

    end subroutine solve_lower_triangular_matrix

    !! Matrix-vector multiply: y = beta y + alpha A x A is square matrix
    subroutine multiply_matrices(ntrace, fast_solver_arr_size, alpha, amat, imat, &
            &                    diag, idiag, xvec, beta, yvec)

        integer(kind = int_wp), intent(in) :: ntrace           ! Dimension of the matrix
        integer(kind = int_wp), intent(in) :: fast_solver_arr_size            ! Dimension of the off-diagonal entries
        real(kind = dp), intent(in) :: alpha            ! Coefficient to multiply Ax with
        real(kind = dp), intent(in) :: amat  (fast_solver_arr_size)  ! Off diagonal entries of A in LP format
        integer(kind = int_wp), intent(in) :: imat  (fast_solver_arr_size)  ! Pointer table off-diagonal entries
        real(kind = dp), intent(in) :: diag  (ntrace) ! diagonal of the matrix
        integer(kind = int_wp), intent(in) :: idiag (0:ntrace) ! position of the diagonals in amat
        real(kind = dp), intent(in) :: xvec  (ntrace) ! vector to multiply amat with
        real(kind = dp), intent(in) :: beta             ! Coefficient to multiply yvec with
        real(kind = dp), intent(inout) :: yvec  (ntrace) ! yvec = beta*yvec + alpha*A*xvec
        integer(kind = int_wp) :: i, j             ! Help variables for loop processing

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("multiply_matrices", ithandl)

        yvec = yvec * beta / alpha

        ! loop over rows of A
        do i = 1, ntrace

            ! loop over non-zero entries of row(i) and multiply
            do j = idiag(i - 1) + 1, idiag(i)
                if (imat(j) > 0) yvec(i) = yvec(i) + amat(j) * xvec(imat(j))
            enddo

            ! add diagonal element
            yvec(i) = yvec(i) + diag(i) * xvec(i)

        enddo

        yvec = yvec * alpha

        if (timon) call timstop (ithandl)

    end subroutine multiply_matrices


    !> Initialise numerical parameters for Generalized Minimal Residual (GMRES) method
    !! from user input-parameters (if found) or from the default values (if not user-defined).
    !! The numerical parameters assigned are:
    !!    * Preconditioner: 0 = none, 1 = GS (Lower), 2 = GS (Upper), 3 = (default) Symmetric-> SSOR (Lower and Upper)
    !!    * Maximum number of iterations: default = 100
    !!    * Relative tolerance: default = 1.D-7
    !!    * Row scaling: 0 = no, 1 = (default) yes
    !!    * Generate iteration report: true or false
    subroutine initialize_gmres(lunrep, num_constants, coname, cons, preconditioner, &
            max_iterations, rel_tolerance, row_scaling, report_iterations, num_cells, &
            num_exchanges_z_dir, num_exchanges, num_fast_solver_vectors, matrix_size, &
            num_layers, integration_type, integration_option)

        use m_string_utils

        integer(kind = int_wp), intent(in) :: lunrep             ! Unit number report file
        integer(kind = int_wp), intent(in) :: num_constants             ! Number of constants used
        character(20), intent(in) :: coname(num_constants)     ! Constant names
        real(kind = real_wp), intent(in) :: cons  (num_constants)     ! Model constants
        integer(kind = int_wp), intent(out) :: preconditioner     ! Preconditioner switch:
        ! 0 = none
        ! 1 = GS (L)
        ! 2 = GS (U)
        ! 3 = SSOR
        integer(kind = int_wp), intent(out) :: max_iterations     ! Maximum number of iterations
        real(kind = dp), intent(out) :: rel_tolerance      ! Relative tolerance
        integer(kind = int_wp), intent(out) :: row_scaling        ! Row scaling switch [0 = no, 1 =yes]
        logical, intent(out) :: report_iterations  ! Switch on reporting iterarions
        integer(kind = int_wp), intent(in) :: num_cells              ! Number of cells or computational volumes
        integer(kind = int_wp), intent(in) :: num_exchanges_z_dir    ! Number of exchange surfaces in 3rd direction
        integer(kind = int_wp), intent(in) :: num_exchanges          ! total number of exchange surfaces
        integer(kind = int_wp), intent(in) :: num_fast_solver_vectors              ! vector_count
        integer(kind = int_wp), intent(in) :: matrix_size        ! size of matrix with off-diagonals
        integer(kind = int_wp), intent(out) :: num_layers              ! number of layers
        integer(kind = int_wp), intent(in) :: integration_type   ! integration type
        integer(kind = int_wp), intent(in) :: integration_option ! integration option

        !     Local declarations
        integer(kind = int_wp) :: ierr                          ! Error count
        integer(kind = int_wp) :: default_preconditioner = 3    ! Default preconditioner switch
        integer(kind = int_wp) :: default_max_iterations = 100  ! Default maximum number of iterations
        integer(kind = int_wp) :: default_row_scaling = 1       ! Default value for row scaling
        real(kind = dp) :: default_rel_tolerance = 1.D-7 ! Default value for relative tolerance
        integer(kind = int_wp) :: default_report_iterations = 0 ! Default value for iteration report
        integer(kind = int_wp) :: idef, itrep                   ! Auxiliary variables
        character(20) defnam                                    ! Auxiliary string
        integer(kind = int_wp) :: threads_count                 ! Number of available threads

        !     The WAQ-timer
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwqf5", ithandl)

        ! look for unstructured setting, this is misuse of num_layers, fractim depends also on this
        if (btest(integration_option, 15)) then
            num_layers = 1
        end if

        ! Some initialisations
        ierr = 0
        write (lunrep, *)
        write (lunrep, 2000)

        ! Preconditioner switch
        write (lunrep, *)
        defnam = 'swprecond'
        idef = index_in_array(defnam, coname)
        if (idef > 0) then
            preconditioner = nint(cons(idef))
            write (lunrep, 2010)
        else
            preconditioner = default_preconditioner
            write (lunrep, 2020)
        end if
        ! Check value
        select case (preconditioner)
        case (0)   ; write (lunrep, 2030) preconditioner
        case (1)   ; write (lunrep, 2040) preconditioner
        case (2)   ; write (lunrep, 2050) preconditioner
        case (3)   ; write (lunrep, 2060) preconditioner
        case default ; ierr = ierr + 1
        write (lunrep, 2070) preconditioner
        end select

        ! Maximum number of iterations
        write (lunrep, *)
        defnam = 'maxiter'
        idef = index_in_array(defnam, coname)
        if (idef > 0) then
            max_iterations = nint(cons(idef))
            write (lunrep, 2080)
        else
            max_iterations = default_max_iterations
            write (lunrep, 2090)
        end if
        ! Check value
        if (max_iterations > 0) then
            write (lunrep, 2100) max_iterations
        else
            ierr = ierr + 1
            write (lunrep, 2110) max_iterations
        end if

        ! Number of vectors
        write (lunrep, 2260) num_fast_solver_vectors

        ! Relative tolerance
        write (lunrep, *)
        defnam = 'tolerance'
        idef = index_in_array(defnam, coname)
        if (idef > 0) then
            rel_tolerance = cons(idef)
            write (lunrep, 2120)
        else
            rel_tolerance = default_rel_tolerance
            write (lunrep, 2130)
        end if
        ! Check value
        if (rel_tolerance  > 0.0) then
            write (lunrep, 2140) rel_tolerance
        else
            ierr = ierr + 1
            write (lunrep, 2150) rel_tolerance
        end if

        ! Look for the row scaling switch
        write (lunrep, *)
        defnam = 'swscale'
        idef = index_in_array(defnam, coname)
        if (idef > 0) then
            row_scaling = nint(cons(idef))
            write (lunrep, 2160)
        else
            row_scaling = default_row_scaling
            write (lunrep, 2170)
        end if
        ! Check value
        select case (row_scaling)
        case (0)   ; write (lunrep, 2180) row_scaling
        case (1)   ; write (lunrep, 2190) row_scaling
        case default ; ierr = ierr + 1
        write (lunrep, 2200) row_scaling
        end select

        ! Iteration report flag
        write (lunrep, *)
        defnam = 'iteration report'
        idef = index_in_array(defnam, coname)
        if (idef > 0) then
            itrep = nint(cons(idef))
            write (lunrep, 2210)
        else
            itrep = default_report_iterations
            write (lunrep, 2220)
        end if
        !     Check value
        select case (itrep)
        case (0)   ; write (lunrep, 2230) itrep
        report_iterations = .false.
        case (1)   ; write (lunrep, 2240) itrep
        report_iterations = .true.
        case default ; ierr = ierr + 1
        write (lunrep, 2250) itrep
        end select

        !     Close timer and return
        if (timon) call timstop (ithandl)
        return

        !     Formats
        2000 format(' Initialising numerical options for method 15...18')
        2010 format(' Preconditioner switch found in input')
        2020 format(' Preconditioner switch not found, using default')
        2030 format(' switch = ', I1, ', corrections based on previous operand')
        2040 format(' switch = ', I1, ', corrections based on lower triangular')
        2050 format(' switch = ', I1, ', corrections based on upper triangular')
        2060 format(' switch = ', I1, ', corrections based on lower and upper ')
        2070 format(' ERROR switch =', I6, ' out of range [0-3]')
        2080 format(' Maximum number of iterations found in input')
        2090 format(' Maximum number of iterations not found, using default')
        2100 format(' Maximum number of iterations set to :', I6)
        2110 format(' ERROR maximum number out of range :', I10)
        2120 format(' Relative tolerance found in input')
        2130 format(' Relative tolerance not found, using default')
        2140 format(' Relative tolerance set to :', E10.3)
        2150 format(' ERROR Relative tolerance out of range :', E10.3)
        2160 format(' Scaling switch found in input')
        2170 format(' Scaling switch not found, using default')
        2180 format(' switch = ', I1, ', scaling is switched off')
        2190 format(' switch = ', I1, ', scaling is switched on')
        2200 format(' ERROR switch =', I6, ' out of range [0-1]')
        2210 format(' Iteration report switch found in input')
        2220 format(' Iteration report switch not found, using default')
        2230 format(' switch = ', I1, ', iteration report is switched off')
        2240 format(' switch = ', I1, ', iteration report is switched on')
        2250 format(' ERROR switch =', I6, ' out of range [0-1]')
        2260 format(' Maximum number of vectors is:', I6)

    end subroutine initialize_gmres

    subroutine strsv (uplo, trans, diag, n, a, lda, x, incx)

        use m_string_manipulation, only: is_same_letter

        integer(kind = int_wp) :: incx, lda, n
        character(len = 1) :: diag, trans, uplo
        real(kind = dp) :: a(lda, *), x(*)

        !       STRSV  solves one of the systems of equations
        !
        !          A*x = b,   or   A'*x = b,
        !
        !       where b and x are n element vectors and A is an n by n unit, or
        !       non-unit, upper or lower triangular matrix.
        !
        !       No test for singularity or near-singularity is included in this
        !       routine. Such tests must be performed before calling this routine.
        !
        !       Parameters
        !       ==========
        !
        !       UPLO   - character(len=1).
        !                On entry, UPLO specifies whether the matrix is an upper or
        !                lower triangular matrix as follows:
        !
        !                   UPLO = 'U' or 'u'   A is an upper triangular matrix.
        !
        !                   UPLO = 'L' or 'l'   A is a lower triangular matrix.
        !
        !                Unchanged on exit.
        !
        !       TRANS  - character(len=1).
        !                On entry, TRANS specifies the equations to be solved as
        !                follows:
        !
        !                   TRANS = 'N' or 'n'   A*x = b.
        !
        !                   TRANS = 'T' or 't'   A'*x = b.
        !
        !                   TRANS = 'C' or 'c'   A'*x = b.
        !
        !                Unchanged on exit.
        !
        !       DIAG   - character(len=1).
        !                On entry, DIAG specifies whether or not A is unit
        !                triangular as follows:
        !
        !                   DIAG = 'U' or 'u'   A is assumed to be unit triangular.
        !
        !                   DIAG = 'N' or 'n'   A is not assumed to be unit
        !                                       triangular.
        !
        !                Unchanged on exit.
        !
        !       N      - INTEGER.
        !                On entry, N specifies the order of the matrix A.
        !                N must be at least zero.
        !                Unchanged on exit.
        !
        !       A      - REAL             array of DIMENSION ( LDA, n ).
        !                Before entry with  UPLO = 'U' or 'u', the leading n by n
        !                upper triangular part of the array A must contain the upper
        !                triangular matrix and the strictly lower triangular part of
        !                A is not referenced.
        !                Before entry with UPLO = 'L' or 'l', the leading n by n
        !                lower triangular part of the array A must contain the lower
        !                triangular matrix and the strictly upper triangular part of
        !                A is not referenced.
        !                Note that when  DIAG = 'U' or 'u', the diagonal elements of
        !                A are not referenced either, but are assumed to be unity.
        !                Unchanged on exit.
        !
        !       LDA    - INTEGER.
        !                On entry, LDA specifies the first dimension of A as declared
        !                in the calling (sub) program. LDA must be at least
        !                max( 1, n ).
        !                Unchanged on exit.
        !
        !       X      - REAL             array of dimension at least
        !                ( 1 + ( n - 1 )*abs( INCX ) ).
        !                Before entry, the incremented array X must contain the n
        !                element right-hand side vector b. On exit, X is overwritten
        !                with the solution vector x.
        !
        !       INCX   - INTEGER.
        !                On entry, INCX specifies the increment for the elements of
        !                X. INCX must not be zero.
        !                Unchanged on exit.
        !
        !
        !       Level 2 Blas routine.
        !
        !       -- Written on 22-October-1986.
        !          Jack Dongarra, Argonne National Lab.
        !          Jeremy Du Croz, Nag Central Office.
        !          Sven Hammarling, Nag Central Office.
        !          Richard Hanson, Sandia National Labs.
        !
        !

        real(kind = dp) :: zero
        parameter        (zero = 0.0e+0)
        real(kind = dp) :: temp
        integer(kind = int_wp) :: i, info, ix, j, jx, kx
        logical            nounit
        intrinsic max
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("strsv", ithandl)

        info = 0
        if (.not.is_same_letter(uplo, 'U').AND. &
                .NOT.is_same_letter(UPLO, 'L'))THEN
            INFO = 1
        else if(.not.is_same_letter(trans, 'N').AND. &
                .NOT.is_same_letter(TRANS, 'T').AND. &
                        .NOT.is_same_letter(TRANS, 'C'))THEN
            INFO = 2
        ELSE IF(.NOT.is_same_letter(DIAG, 'U').AND. &
                .NOT.is_same_letter(DIAG, 'N'))then
            info = 3
        else if(n<0)then
            info = 4
        else if(lda<max(1, n))then
            info = 6
        else if(incx==0)then
            info = 8
        end if
        if(info/=0)then
            call xerbla('STRSV ', info)
            if (timon) call timstop (ithandl)
            return
        end if
        !
        !          Quick return if possible.
        !
        if(n==0) then
            if (timon) call timstop (ithandl)
            return
        endif
        !
        nounit = is_same_letter(diag, 'N')
        !
        !          Set up the start point in X if the increment is not unity. This
        !          will be  ( N - 1 )*INCX  too small for descending loops.
        !
        if(incx<=0)then
            kx = 1 - (n - 1) * incx
        else if(incx/=1)then
            kx = 1
        end if
        !
        !          Start the operations. In this version the elements of A are
        !          accessed sequentially with one pass through A.
        !
        if(is_same_letter(trans, 'N'))then
            !
            !             Form  x := inv( A )*x.
            !
            if(is_same_letter(UPLO, 'U'))then
                if(incx==1)then
                    do j = n, 1, -1
                        if(x(j)/=zero)then
                            if(nounit) &
                                    x(j) = x(j) / a(j, j)
                            temp = x(j)
                            do i = j - 1, 1, -1
                                x(i) = x(i) - temp * a(i, j)
                            end do
                        end if
                    end do
                else
                    jx = kx + (n - 1) * incx
                    do j = n, 1, -1
                        if(x(jx)/=zero)then
                            if(nounit) &
                                    x(jx) = x(jx) / a(j, j)
                            temp = x(jx)
                            ix = jx
                            do i = j - 1, 1, -1
                                ix = ix - incx
                                x(ix) = x(ix) - temp * a(i, j)
                            end do
                        end if
                        jx = jx - incx
                    end do
                end if
            else
                if(incx==1)then
                    do j = 1, n
                        if(x(j)/=zero)then
                            if(nounit) &
                                    x(j) = x(j) / a(j, j)
                            temp = x(j)
                            do i = j + 1, n
                                x(i) = x(i) - temp * a(i, j)
                            end do
                        end if
                    end do
                else
                    jx = kx
                    do j = 1, n
                        if(x(jx)/=zero)then
                            if(nounit) &
                                    x(jx) = x(jx) / a(j, j)
                            temp = x(jx)
                            ix = jx
                            do i = j + 1, n
                                ix = ix + incx
                                x(ix) = x(ix) - temp * a(i, j)
                            end do
                        end if
                        jx = jx + incx
                    end do
                end if
            end if
        else
            !
            !             Form  x := inv( A' )*x.
            !
            if(is_same_letter(UPLO, 'U'))then
                if(incx==1)then
                    do j = 1, n
                        temp = x(j)
                        do i = 1, j - 1
                            temp = temp - a(i, j) * x(i)
                        end do
                        if(nounit) &
                                temp = temp / a(j, j)
                        x(j) = temp
                    end do
                else
                    jx = kx
                    do j = 1, n
                        temp = x(jx)
                        ix = kx
                        do i = 1, j - 1
                            temp = temp - a(i, j) * x(ix)
                            ix = ix + incx
                        end do
                        if(nounit) &
                                temp = temp / a(j, j)
                        x(jx) = temp
                        jx = jx + incx
                    end do
                end if
            else
                if(incx==1)then
                    do j = n, 1, -1
                        temp = x(j)
                        do i = n, j + 1, -1
                            temp = temp - a(i, j) * x(i)
                        end do
                        if(nounit) &
                                temp = temp / a(j, j)
                        x(j) = temp
                    end do
                else
                    kx = kx + (n - 1) * incx
                    jx = kx
                    do j = n, 1, -1
                        temp = x(jx)
                        ix = kx
                        do i = n, j + 1, -1
                            temp = temp - a(i, j) * x(ix)
                            ix = ix - incx
                        end do
                        if(nounit) &
                                temp = temp / a(j, j)
                        x(jx) = temp
                        jx = jx - incx
                    end do
                end if
            end if
        end if

        if (timon) call timstop (ithandl)
    end subroutine strsv

    subroutine sgemv (trans, m, n, alpha, a, lda, x, incx, &
            beta, y, incy)
        use m_string_manipulation, only: is_same_letter

        real(kind = dp) :: alpha, beta
        integer(kind = int_wp) :: incx, incy, lda, m, n
        character(len = 1) :: trans
        real(kind = dp) :: a(lda, *), x(*), y(*)
        ! SGEMV  performs one of the matrix-vector operations
        !
        !          y := alpha*A*x + beta*y,   or   y := alpha*A'*x + beta*y,
        !
        !       where alpha and beta are scalars, x and y are vectors and A is an
        !       m by n matrix.
        !
        !       Parameters
        !       ==========
        !
        !       TRANS  - character(len=1).
        !                On entry, TRANS specifies the operation to be performed as
        !                follows:
        !
        !                   TRANS = 'N' or 'n'   y := alpha*A*x + beta*y.
        !
        !                   TRANS = 'T' or 't'   y := alpha*A'*x + beta*y.
        !
        !                   TRANS = 'C' or 'c'   y := alpha*A'*x + beta*y.
        !
        !                Unchanged on exit.
        !
        !       M      - INTEGER.
        !                On entry, M specifies the number of rows of the matrix A.
        !                M must be at least zero.
        !                Unchanged on exit.
        !
        !       N      - INTEGER.
        !                On entry, N specifies the number of columns of the matrix A.
        !                N must be at least zero.
        !                Unchanged on exit.
        !
        !       ALPHA  - REAL            .
        !                On entry, ALPHA specifies the scalar alpha.
        !                Unchanged on exit.
        !
        !       A      - REAL             array of DIMENSION ( LDA, n ).
        !                Before entry, the leading m by n part of the array A must
        !                contain the matrix of coefficients.
        !                Unchanged on exit.
        !
        !       LDA    - INTEGER.
        !                On entry, LDA specifies the first dimension of A as declared
        !                in the calling (sub) program. LDA must be at least
        !                max( 1, m ).
        !                Unchanged on exit.
        !
        !       X      - REAL             array of DIMENSION at least
        !                ( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = 'N' or 'n'
        !                and at least
        !                ( 1 + ( m - 1 )*abs( INCX ) ) otherwise.
        !                Before entry, the incremented array X must contain the
        !                vector x.
        !                Unchanged on exit.
        !
        !       INCX   - INTEGER.
        !                On entry, INCX specifies the increment for the elements of
        !                X. INCX must not be zero.
        !                Unchanged on exit.
        !
        !       BETA   - REAL            .
        !                On entry, BETA specifies the scalar beta. When BETA is
        !                supplied as zero then Y need not be set on input.
        !                Unchanged on exit.
        !
        !       Y      - REAL             array of DIMENSION at least
        !                ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = 'N' or 'n'
        !                and at least
        !                ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.
        !                Before entry with BETA non-zero, the incremented array Y
        !                must contain the vector y. On exit, Y is overwritten by the
        !                updated vector y.
        !
        !       INCY   - INTEGER.
        !                On entry, INCY specifies the increment for the elements of
        !                Y. INCY must not be zero.
        !                Unchanged on exit.
        !
        !
        !       Level 2 Blas routine.

        REAL(kind = dp) :: ONE, ZERO
        PARAMETER        (ONE = 1.0E+0, ZERO = 0.0E+0)

        REAL(kind = dp) :: TEMP
        INTEGER(kind = int_wp) :: I, INFO, IX, IY, J, JX, JY, KX, KY, LENX, LENY

        INTRINSIC MAX
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("sgemv", ithandl)

        INFO = 0
        IF     (.NOT.is_same_letter(TRANS, 'N').AND. &
                .NOT.is_same_letter(TRANS, 'T').AND. &
                        .NOT.is_same_letter(TRANS, 'C'))THEN
            INFO = 1
        ELSE IF(M<0)THEN
            INFO = 2
        ELSE IF(N<0)THEN
            INFO = 3
        ELSE IF(LDA<MAX(1, M))THEN
            INFO = 6
        ELSE IF(INCX==0)THEN
            INFO = 8
        ELSE IF(INCY==0)THEN
            INFO = 11
        END IF
        IF(INFO/=0)THEN
            CALL XERBLA('SGEMV ', INFO)
            goto 9999  !   RETURN
        END IF
        !          Quick return if possible.
        IF((M==0).OR.(N==0).OR. &
                ((ALPHA==ZERO).AND.(BETA==ONE))) &
                goto 9999  !   RETURN
        !          Set  LENX  and  LENY, the lengths of the vectors x and y, and set
        !          up the start points in  X  and  Y.
        IF(is_same_letter(TRANS, 'N'))THEN
            LENX = N
            LENY = M
        ELSE
            LENX = M
            LENY = N
        END IF
        IF(INCX>0)THEN
            KX = 1
        ELSE
            KX = 1 - (LENX - 1) * INCX
        END IF
        IF(INCY>0)THEN
            KY = 1
        ELSE
            KY = 1 - (LENY - 1) * INCY
        END IF
        !          Start the operations. In this version the elements of A are
        !          accessed sequentially with one pass through A.
        !
        !          First form  y := beta*y.
        IF(BETA/=ONE)THEN
            IF(INCY==1)THEN
                IF(BETA==ZERO)THEN
                    DO I = 1, LENY
                        Y(I) = ZERO
                    end do
                ELSE
                    DO I = 1, LENY
                        Y(I) = BETA * Y(I)
                    end do
                END IF
            ELSE
                IY = KY
                IF(BETA==ZERO)THEN
                    DO I = 1, LENY
                        Y(IY) = ZERO
                        IY = IY + INCY
                    end do
                ELSE
                    DO I = 1, LENY
                        Y(IY) = BETA * Y(IY)
                        IY = IY + INCY
                    end do
                END IF
            END IF
        END IF
        IF(ALPHA==ZERO) &
                goto 9999  !   RETURN
        IF(is_same_letter(TRANS, 'N'))THEN
            !             Form  y := alpha*A*x + y.
            JX = KX
            IF(INCY==1)THEN
                DO J = 1, N
                    IF(X(JX)/=ZERO)THEN
                        TEMP = ALPHA * X(JX)
                        DO I = 1, M
                            Y(I) = Y(I) + TEMP * A(I, J)
                        end do
                    END IF
                    JX = JX + INCX
                end do
            ELSE
                DO J = 1, N
                    IF(X(JX)/=ZERO)THEN
                        TEMP = ALPHA * X(JX)
                        IY = KY
                        DO  I = 1, M
                            Y(IY) = Y(IY) + TEMP * A(I, J)
                            IY = IY + INCY
                        end do
                    END IF
                    JX = JX + INCX
                end do
            END IF
        ELSE
            !             Form  y := alpha*A'*x + y.
            JY = KY
            IF(INCX==1)THEN
                DO J = 1, N
                    TEMP = ZERO
                    DO I = 1, M
                        TEMP = TEMP + A(I, J) * X(I)
                    end do
                    Y(JY) = Y(JY) + ALPHA * TEMP
                    JY = JY + INCY
                end do
            ELSE
                DO J = 1, N
                    TEMP = ZERO
                    IX = KX
                    DO I = 1, M
                        TEMP = TEMP + A(I, J) * X(IX)
                        IX = IX + INCX
                    end do
                    Y(JY) = Y(JY) + ALPHA * TEMP
                    JY = JY + INCY
                end do
            END IF
        END IF

        9999 if (timon) call timstop (ithandl)
    END subroutine sgemv

    !! XERBLA  is an error handler for the Level 2 BLAS routines.
    subroutine xerbla (srname, info)
        use m_logger_helper, only: stop_with_error
        integer(kind = int_wp) :: info
        character(len = 6)        srname
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("xerbla", ithandl)

        ! It is called by the Level 2 BLAS routines if an input parameter is invalid.
        !
        ! Installers should consider modifying the STOP statement in order to call system-specific exception-handling
        ! facilities.
        !
        !
        !       SRNAME - character(len=6).
        !                On entry, SRNAME specifies the name of the routine which
        !                called XERBLA.
        !
        !       INFO: On entry, INFO specifies the position of the invalid
        !                parameter in the parameter-list of the calling routine.
        !
        !
        !       Auxiliary routine for Level 2 Blas.

        write (*, 99999) srname, info
        call stop_with_error()

        99999 FORMAT (' ** On entry to ', A6, ' parameter number ', I2, ' had an illegal value')

        if (timon) call timstop (ithandl)
    end subroutine xerbla

    subroutine srotg (sa, sb, c, s)
        ! construct givens plane rotation.
        ! jack dongarra, linpack, 3/11/78.

        real(kind = dp) :: sa, sb, c, s, roe, scale, r, z, one
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("srotg", ithandl)

        roe = sb
        if(abs(sa) > abs(sb)) roe = sa
        scale = abs(sa) + abs(sb)
        if(scale /= 0.0) go to 10
        c = 1.0
        s = 0.0
        r = 0.0
        go to 20
        10 r = scale * sqrt((sa / scale)**2 + (sb / scale)**2)
        one = 1.0
        r = sign(one, roe) * r
        c = sa / r
        s = sb / r
        20 z = s
        if(abs(c) > 0.0 .and. abs(c) <= s) z = 1.0 / c
        sa = r
        sb = z
        if (timon) call timstop (ithandl)
    end subroutine srotg
end module m_sgmres
