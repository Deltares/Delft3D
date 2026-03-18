!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------
!
!
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

module m_petsc
#include <petsc/finclude/petscksp.h>
   use iso_c_binding, only: c_int32_t, c_int64_t, c_double
   use precision, only: dp
   use petsc
   PetscInt :: numrows ! number of rows in this domain
   integer :: numallrows ! number of rows of whole system
   integer, dimension(:), allocatable :: rowtoelem ! local row to local element list, dim(numrows)

   ! CRS matrices for PETSc/MatCreateMPIAIJWithSplitArrays
   PetscInt :: numdia ! number of non-zero entries in diagonal block
   real(kind=dp), dimension(:), allocatable :: adia ! non-zero matrix entries, diagonal block
   PetscInt, dimension(:), allocatable :: idia, jdia ! column indices and row pointers of off-diagonal block

   integer :: numoff ! number of non-zero entries in off-diagonal block
   PetscScalar, dimension(:), allocatable :: aoff ! non-zero matrix entries, diagonal block
   PetscInt, dimension(:), allocatable :: ioff, joff ! column indices and row pointers of off-diagonal block

   PetscInt, dimension(:), allocatable :: joffsav ! store of joff

   integer, dimension(:), allocatable :: guusidxdia ! index in ccr or bbr array, >0: ccr, <0: bbr, diagonal block, dim(numdia)
   integer, dimension(:), allocatable :: guusidxoff ! index in ccr or bbr array, >0: ccr, <0: bbr, off-diagonal block, dim(numoff)

   integer :: numzerorows ! number of zero rows
   integer, dimension(:), allocatable :: izerorow ! zero-rows in matrix (kfs=0)

   real(kind=dp), dimension(:), allocatable :: rhs_val ! values in vector rhs
   real(kind=dp), dimension(:), allocatable :: sol_val ! values in vector sol
   real(kind=dp), dimension(:), allocatable :: res_val ! values in vector res
   Vec :: res ! residual vector
   Vec :: rhs ! right-hand side vector
   Vec :: sol ! solution vector
   Mat :: Amat ! PETSc-type matrix (will include dry nodes, set to zero)
   KSP :: Solver ! Solver for the equation Amat * sol = rhs
   logical :: isKSPCreated = .false. ! A flag to determine whether KSP is created

   ! preconditioner
   PC :: Preconditioner
   KSP :: SubSolver
   PC :: SubPrec
   PCType :: PreconditioningType

   PetscErrorCode, parameter :: PETSC_OK = 0
end module m_petsc

submodule(m_solve_petsc) m_solve_petsc_
   use iso_c_binding, only: c_int32_t, c_int64_t, c_double
   use precision, only: dp
   implicit none

contains

   !> Initialize PETSc
   module subroutine startpetsc()
#ifdef HAVE_PETSC
      use m_petsc, only: PETSC_OK, petsc_comm_world, petscinitialize, petsc_null_character, petscpopsignalhandler
      use mpi, only: mpi_comm_dup
      use m_flowparameters, only: Icgsolver
      use m_partitioninfo, only: DFM_COMM_DFMWORLD, jampi

      PetscErrorCode :: ierr = PETSC_OK

      if (icgsolver == 6) then
         if (jampi > 0) then
            call mpi_comm_dup(DFM_COMM_DFMWORLD, PETSC_COMM_WORLD, ierr)
         end if
         call PetscInitialize(PETSC_NULL_CHARACTER, ierr)
         call PetscPopSignalHandler(ierr) ! Switch off signal catching in PETSC.
         call PetscLogDefaultBegin(ierr)
      end if
#endif

      return
   end subroutine startpetsc

   !> Clean up PETSc
   module subroutine stoppetsc()
#ifdef HAVE_PETSC
      use mpi, only: mpi_comm_free
      use m_petsc, only: PETSC_OK, petscfinalize, petsc_comm_world, petscinitialized
      use m_flowparameters, only: Icgsolver
      use m_partitioninfo, only: jampi

      PetscErrorCode :: ierr = PETSC_OK

      if (Icgsolver == 6) then
         call killSolverPETSC()
         call PetscFinalize(ierr)
         if (jampi > 0) then
            call mpi_comm_free(PETSC_COMM_WORLD, ierr)
         end if
      end if
#endif
      return
   end subroutine stoppetsc

   !> Allocate arrays for PETSc matrix construction,
   !> and get sparsity pattern in RCS format
   module subroutine ini_petsc(Ndx, ierror)
      use m_reduce, only: nocg, noel, nogauss, ndn, row
      use m_partitioninfo, only: get_global_numbers, iglobal, numcells, jampi, my_rank, ndomains, numghost_sall, ighostlist_sall
      use petsc
      use m_petsc, only: PETSC_OK, numrows, numallrows, numdia, numoff, rowtoelem, jdia, idia, adia, joff, ioff, aoff, joffsav, guusidxdia, guusidxoff, izerorow, rhs_val, sol_val, res_val, rhs, sol, res
      use MessageHandling, only: mess, level_error
      use stdlib_sorting, only: sort_index

      integer, intent(in) :: Ndx !< number of cells
      integer, intent(out) :: ierror !< error (1) or not (0)

      integer, dimension(:), allocatable :: mask
      integer, dimension(:), allocatable :: inonzerodia, inonzerooff ! number of nonzeros in diagonal and off-diagonal block, respectively

      integer, dimension(:), allocatable :: idx, idum ! for sorting
      integer :: istart, iend, num

      integer :: i, irow, j, n
      integer :: ndn_glob ! global cell number
      integer :: ndn_glob_first ! global cell number of first active cell

      PetscInt, parameter :: singletonBlocks = 1
      PetscErrorCode :: ierr = PETSC_OK

      ierror = 1

      ! Make global numbering; the first call fails in debug mode when nocg = 0 and hence nogauss = len(noel)
      if (nocg > 0) then
         call get_global_numbers(nocg, noel(nogauss + 1:nogauss + nocg), iglobal, numcells, 0)
      else
         call get_global_numbers(nocg, noel, iglobal, numcells, 0)
      end if

      if (jampi == 1) then
         ! the number of cells in this domain
         numrows = numcells(my_rank)

         ! the total number of rows
         numallrows = sum(numcells(0:ndomains - 1))
      else
         numrows = nocg
         numallrows = nocg
      end if

      allocate (mask(Ndx))
      allocate (inonzerodia(numrows))
      allocate (inonzerooff(numrows))

      ! mark active cells
      mask = 0
      do n = nogauss + 1, nogauss + nocg
         mask(noel(n)) = 1
      end do

      ! unmark all ghost cells
      do i = 1, numghost_sall
         mask(ighostlist_sall(i)) = 0
      end do

      ! count nonzero elements
      irow = 0
      ndn_glob_first = 0
      numdia = 0
      numoff = 0
      do n = nogauss + 1, nogauss + nocg
         ndn = noel(n) ! cell number
         if (mask(ndn) == 1) then ! active cells only
            irow = irow + 1
            ndn_glob = iglobal(ndn) ! global cell number

            ! check global cell numbering (safety)
            if (ndn_glob_first == 0) then
               ndn_glob_first = ndn_glob
            else
               if (ndn_glob /= ndn_glob_first + irow - 1) then
                  call mess(LEVEL_ERROR, 'ini_petsc: global cell numbering error')
                  goto 1234
               end if
            end if

            ! diagonal element
            numdia = numdia + 1

            ! count non-zero row entries for this row
            do i = 1, row(ndn)%l
               j = row(ndn)%j(i)
               if (iglobal(j) == 0) then
                  cycle
               end if
               if (mask(j) == 1) then ! in diagonal block
                  numdia = numdia + 1
               else ! in off-diagonal block
                  numoff = numoff + 1
               end if
            end do

         end if
      end do

      ! allocate module variables
      if (allocated(rowtoelem)) then
         deallocate (rowtoelem)
      end if
      if (allocated(jdia)) then
         deallocate (jdia)
      end if
      if (allocated(idia)) then
         deallocate (idia)
      end if
      if (allocated(adia)) then
         deallocate (adia)
      end if

      if (allocated(joff)) then
         deallocate (joff)
      end if
      if (allocated(ioff)) then
         deallocate (ioff)
      end if
      if (allocated(aoff)) then
         deallocate (aoff)
      end if

      if (allocated(joffsav)) then
         deallocate (joffsav)
      end if

      if (allocated(guusidxdia)) then
         deallocate (guusidxdia)
      end if
      if (allocated(guusidxoff)) then
         deallocate (guusidxoff)
      end if

      if (allocated(izerorow)) then
         deallocate (izerorow)
      end if

      if (allocated(rhs_val)) then
         deallocate (rhs_val)
      end if
      if (allocated(sol_val)) then
         deallocate (sol_val)
      end if
      if (allocated(res_val)) then
         deallocate (res_val)
      end if
      allocate (rowtoelem(numrows))

      allocate (jdia(numdia))
      allocate (idia(numrows + 1))
      allocate (adia(numdia))

      allocate (joff(max(numoff, 1)))
      allocate (ioff(numrows + 1))
      allocate (aoff(max(numoff, 1)))

      allocate (joffsav(max(numoff, 1)))

      allocate (guusidxdia(numdia))
      allocate (guusidxoff(numoff))

      allocate (izerorow(numrows))

      allocate (rhs_val(1:numrows))
      allocate (sol_val(1:numrows))
      allocate (res_val(1:numrows))

      ! make the RCS index arrays
      irow = 0
      numdia = 0
      numoff = 0
      idia = 0
      ioff = 0
      idia(1) = 1
      ioff(1) = 1
      guusidxdia = 0
      guusidxoff = 0
      do n = nogauss + 1, nogauss + nocg
         ndn = noel(n)
         if (mask(ndn) == 1) then
            irow = irow + 1 ! global cell number

            rowtoelem(irow) = ndn

            ! diagonal element
            numdia = numdia + 1
            jdia(numdia) = iglobal(ndn)
            guusidxdia(numdia) = -ndn

            if (iglobal(ndn) == 0) then
               write (6, *) '--> iglobal=0', my_rank, ndn
            end if

            ! count non-zero row entries for this row
            do i = 1, row(ndn)%l
               j = row(ndn)%j(i)
               if (iglobal(j) == 0) then
                  cycle
               end if
               if (mask(j) == 1) then ! in diagonal block
                  numdia = numdia + 1
                  jdia(numdia) = iglobal(j)
                  guusidxdia(numdia) = row(ndn)%a(i)
               else ! ghost cell: in off-diagonal block
                  numoff = numoff + 1
                  joff(numoff) = iglobal(j)
                  guusidxoff(numoff) = row(ndn)%a(i)
               end if
            end do

            ! end if
            idia(irow + 1) = numdia + 1
            ioff(irow + 1) = numoff + 1
         end if
      end do

      inonzerodia = idia(2:numrows + 1) - idia(1:numrows)
      if (numoff > 0) then
         inonzerooff = ioff(2:numrows + 1) - ioff(1:numrows)
      else
         inonzerooff = 0
      end if

      ! sort the row indices
      num = max(maxval(inonzerodia), maxval(inonzerooff))
      allocate (idx(num))
      allocate (idum(num))

      do n = 1, numrows
         istart = idia(n)
         iend = idia(n + 1) - 1
         num = iend - istart + 1
         if (num > 0) then
            call sort_index(jdia(istart:iend), idx(1:num))

            idum(1:num) = guusidxdia(istart:iend)
            guusidxdia(istart:iend) = idum(idx(1:num))
         end if
      end do

      do n = 1, numrows
         istart = ioff(n)
         iend = ioff(n + 1) - 1
         num = iend - istart + 1
         if (num > 0) then
            call sort_index(joff(istart:iend), idx(1:num))

            idum(1:num) = guusidxoff(istart:iend)
            guusidxoff(istart:iend) = idum(idx(1:num))
         end if
      end do

      ! make indices zero-based
      idia = idia - 1
      jdia = jdia - 1
      ioff = ioff - 1
      joff = joff - 1

      ! diagonal row-indices need to be local
      if (jampi == 1 .and. numrows > 0) then
         jdia = jdia - iglobal(rowtoelem(1)) + 1
      end if

      ! store
      joffsav = joff

      ! create vectors
      rhs_val = 0.0_dp
      sol_val = 0.0_dp
      res_val = 0.0_dp
      if (ierr == PETSC_OK) then
         call VecCreateMPIWithArray(PETSC_COMM_WORLD, singletonBlocks, numrows, PETSC_DECIDE, rhs_val, rhs, ierr)
      end if
      if (ierr == PETSC_OK) then
         call VecCreateMPIWithArray(PETSC_COMM_WORLD, singletonBlocks, numrows, PETSC_DECIDE, sol_val, sol, ierr)
      end if
      if (ierr == PETSC_OK) then
         call VecCreateMPIWithArray(PETSC_COMM_WORLD, singletonBlocks, numrows, PETSC_DECIDE, res_val, res, ierr)
      end if
      if (ierr == PETSC_OK) then
         call VecAssemblyBegin(rhs, ierr)
      end if
      if (ierr == PETSC_OK) then
         call VecAssemblyBegin(sol, ierr)
      end if
      if (ierr == PETSC_OK) then
         call VecAssemblyBegin(res, ierr)
      end if

      if (ierr == PETSC_OK) then
         call VecAssemblyEnd(rhs, ierr)
      end if
      if (ierr == PETSC_OK) then
         call VecAssemblyEnd(sol, ierr)
      end if
      if (ierr == PETSC_OK) then
         call VecAssemblyEnd(res, ierr)
      end if

      if (ierr == PETSC_OK) then
         ierror = 0
      end if

1234  continue

      ! deallocate local variables
      if (allocated(mask)) then
         deallocate (mask)
      end if
      if (allocated(inonzerodia)) then
         deallocate (inonzerodia)
      end if
      if (allocated(inonzerooff)) then
         deallocate (inonzerooff)
      end if
      if (allocated(idx)) then
         deallocate (idx)
      end if
      if (allocated(idum)) then
         deallocate (idum)
      end if

      return
   end subroutine ini_petsc

   !> Fill the PETSc matrix entries with values from bbr and ccr
   subroutine setPETSCmatrixEntries()
      use m_reduce, only: bbr, ccr
      use m_partitioninfo, only: iglobal
      use m_petsc, only: numzerorows, izerorow, adia, aoff, numdia, guusidxdia, numrows, ioff, guusidxoff
      use MessageHandling, only: mess, level_error
      use m_flowgeom, only: kfs

      integer :: i, n

      integer :: irow, istart, iend

      logical :: Lstop

      ! count zero rows
      numzerorows = 0
      izerorow = 0
      adia = 0.0_dp
      aoff = 0.0_dp

      Lstop = .false.

      ! fill matrix entries
      do n = 1, numdia
         i = guusidxdia(n)
         if (i < 0) then ! diagonal entry in diagonal block
            if (kfs(-i) > 0) then ! nonzero row
               adia(n) = bbr(-i)
            else ! zero row
               numzerorows = numzerorows + 1
               izerorow(numzerorows) = iglobal(-i) - 1 ! global row number, zero based
!               adia(n) = 1d0
               adia(n) = bbr(-i)
            end if ! if ( kfs(-i) > 0 )
         else ! off-diagonal entry in diagonal block
            adia(n) = ccr(i)
         end if
      end do

      do irow = 1, numrows
         istart = ioff(irow) + 1 ! ioff is zeros-based
         iend = ioff(irow + 1)
         do n = istart, iend
            i = guusidxoff(n)
            if (i <= 0) then
               ! should not happen
               write (6, *) 'irow=', irow, 'istart=', istart, 'iend=', iend, 'numrows=', numrows, 'n=', n, 'i=', i
               call mess(LEVEL_ERROR, 'conjugategradientPETSC: numbering error')
            else
               aoff(n) = ccr(i)
            end if
         end do
      end do
   end subroutine setPETSCmatrixEntries

   !> Configure the preconditioner for the PETSc KSP solver
   subroutine createPETSCPreconditioner(iprecnd)
      use petsc, only: kspgetpc, pcsettype, pcasmsetoverlap, kspsetup, pcasmgetsubksp, pcasmrestoresubksp, petsc_null_integer, tKSP, kspsetreusepreconditioner, petsc_false
      use m_petsc, only: PETSC_OK, Solver, Preconditioner, SubSolver, SubPrec
      use MessageHandling, only: mess, level_error

      integer, intent(in) :: iprecnd !< preconditioner type, 0:default, 1: none, 2:incomplete Cholesky, 3:Cholesky, 4:GAMG (doesn't work)

      integer :: jasucces

      PetscErrorCode :: ierr = PETSC_OK
      KSP, pointer, dimension(:) :: sub_solvers
      character(len=8) :: preconditioning_type

      jasucces = 0

      ! Ensure preconditioner will be recomputed with new matrix values
      call KSPSetReusePreconditioner(Solver, PETSC_FALSE, ierr)
      if (ierr /= PETSC_OK) then
         goto 1234
      end if

      call KSPGetPC(Solver, Preconditioner, ierr)
      if (ierr /= PETSC_OK) then
         goto 1234
      end if

      ! Configure the preconditioner type
      if (iprecnd == 0) then
         ! Use default preconditioner, just set up with current matrix
         call KSPSetUp(Solver, ierr)
      else if (iprecnd == 1) then
         ! No preconditioner
         call PCSetType(Preconditioner, 'none', ierr)
         if (ierr /= PETSC_OK) then
            goto 1234
         end if
         call KSPSetUp(Solver, ierr)
      else if (iprecnd == 2 .or. iprecnd == 3) then
         ! Incomplete Cholesky with ASM (2) or Cholesky with ASM (3)
         if (iprecnd == 2) then
            preconditioning_type = 'icc'
         else
            preconditioning_type = 'cholesky'
         end if
         call PCSetType(Preconditioner, 'asm', ierr)
         if (ierr /= PETSC_OK) then
            goto 1234
         end if
         call PCASMSetOverlap(Preconditioner, 2, ierr)
         if (ierr /= PETSC_OK) then
            goto 1234
         end if
         call KSPSetUp(Solver, ierr)
         if (ierr /= PETSC_OK) then
            goto 1234
         end if
         call PCASMGetSubKSP(Preconditioner, PETSC_NULL_INTEGER, PETSC_NULL_INTEGER, sub_solvers, ierr)
         if (ierr /= PETSC_OK) then
            goto 1234
         end if
         SubSolver = sub_solvers(1)
         call PCASMRestoreSubKSP(Preconditioner, PETSC_NULL_INTEGER, PETSC_NULL_INTEGER, sub_solvers, ierr)
         if (ierr /= PETSC_OK) then
            goto 1234
         end if
         call KSPGetPC(SubSolver, SubPrec, ierr)
         if (ierr /= PETSC_OK) then
            goto 1234
         end if
         call PCSetType(SubPrec, trim(preconditioning_type), ierr)
      else if (iprecnd == 4) then
         call PCSetType(Preconditioner, 'gamg', ierr)
         if (ierr /= PETSC_OK) then
            goto 1234
         end if
         call KSPSetUp(Solver, ierr)
      else
         call mess(LEVEL_ERROR, 'conjugategradientPETSC: unsupported preconditioner')
         return
      end if

1234  continue

      if (ierr /= PETSC_OK) then
         call mess(LEVEL_ERROR, 'createPETSCPreconditioner: error')
      end if
   end subroutine createPETSCPreconditioner

   !> Compose the global matrix and solver for PETSc.
   !> It is assumed that the global cell numbers iglobal, dim(Ndx) are available
   !> NO GLOBAL RENUMBERING, so the matrix may contain zero rows
   module subroutine preparePETSCsolver(japipe)
      use petsc, only: petsc_default_real, matcreateseqaijwitharrays, petsc_comm_world, matcreatempiaijwithsplitarrays, petsc_determine, matassemblybegin, mat_final_assembly, matassemblyend, kspcreate, kspsetoperators, kspsettype, kspsetinitialguessnonzero, petsc_true, kspsettolerances
      use m_reduce, only: dp
      use m_partitioninfo, only: ndomains
      use m_petsc, only: PETSC_OK, joff, joffsav, adia, aoff, numrows, idia, jdia, Amat, ioff, Solver, isKSPCreated

      integer, intent(in) :: japipe !< use pipelined CG (1) or not (0)

      integer :: jasucces

      PetscErrorCode :: ierr = PETSC_OK
      PetscInt, parameter :: maxits = 4000
      real(kind=dp), parameter :: RelTol = 1.0e-14_dp
      real(kind=dp), parameter :: AbsTol = 1.0e-14_dp
      real(kind=dp), parameter :: dTol = PETSC_DEFAULT_REAL

      jasucces = 0

      ! Restore joff with stored values
      joff = joffsav

      ! Set ridiculous values so that it will be detected if the correct values are not
      ! filled in before use
      adia = 123.4
      aoff = 432.1

      ! the following will destroy joff
      if (ndomains == 1) then
         if (ierr == PETSC_OK) then
            call MatCreateSeqAIJWithArrays(PETSC_COMM_WORLD, numrows, numrows, idia, jdia, adia, Amat, ierr)
         end if
      else
         if (ierr == PETSC_OK) then
            call MatCreateMPIAIJWithSplitArrays(PETSC_COMM_WORLD, numrows, numrows, PETSC_DETERMINE, PETSC_DETERMINE, idia, jdia, adia, ioff, joff, aoff, Amat, ierr)
         end if
      end if

      if (ierr == PETSC_OK) then
         call MatAssemblyBegin(Amat, MAT_FINAL_ASSEMBLY, ierr)
      end if
      if (ierr == PETSC_OK) then
         call MatAssemblyEnd(Amat, MAT_FINAL_ASSEMBLY, ierr)
      end if
      if (ierr /= PETSC_OK) then
         print *, 'conjugategradientPETSC: PETSC_ERROR (1)'
      end if
      if (ierr /= PETSC_OK) then
         go to 1234
      end if

      if (ierr == PETSC_OK) then
         call KSPCreate(PETSC_COMM_WORLD, Solver, ierr)
         isKSPCreated = .true.
      end if
      if (ierr == PETSC_OK) then
         call KSPSetOperators(Solver, Amat, Amat, ierr)
      end if
      if (ierr == PETSC_OK) then
         if (japipe /= 1) then
            call KSPSetType(Solver, 'cg', ierr)
         else
            call KSPSetType(Solver, 'pipecg', ierr)
         end if
      end if
      if (ierr == PETSC_OK) then
         call KSPSetInitialGuessNonzero(Solver, PETSC_TRUE, ierr)
      end if
      if (ierr == PETSC_OK) then
         call KSPSetTolerances(Solver, RelTol, AbsTol, dTol, maxits, ierr)
      end if

      ! Soheil: for imaginairy matrix entries use KSPCGSetType(Solver, ... )

1234  continue

   end subroutine preparePETSCsolver

   !> Solve the linear system with PETSc KSP solver
   module subroutine conjugategradientPETSC(s1, ndx, its, jacompprecond, iprecond)
      use petsc, only: kspsolve, kspgetconvergedreason, KSP_DIVERGED_INDEFINITE_PC, KSP_DIVERGED_NANORINF, kspgetiterationnumber, kspgetresidualnorm, &
         eKSPConvergedReason, KSPGetConvergedReasonString
      use m_reduce, only: dp, nogauss, nocg, ndn, noel, ddr
      use m_partitioninfo, only: iglobal, my_rank
      use m_petsc, only: PETSC_OK, rhs, rhs_val, rowtoelem, sol, sol_val, Solver
      use MessageHandling, only: mess, level_info, level_warn, level_error, level_debug
      use m_flowgeom, only: kfs
      use m_flowtimes, only: dts ! for logging
      use m_flowparameters, only: jalogsolverconvergence

      integer, intent(in) :: ndx
      real(kind=dp), dimension(Ndx), intent(inout) :: s1
      integer, intent(out) :: its
      integer, intent(in) :: jacompprecond !< compute preconditioner (1) or not (0)
      integer, intent(in) :: iprecond !< preconditioner type

      real(kind=dp) :: rnorm ! residual norm

      integer :: i, n, jasucces

      PetscErrorCode :: ierr
      KSPConvergedReason :: Reason
      character(len=100) :: message
      character(len=100) :: reason_string

      jasucces = 0
      ierr = PETSC_OK

      its = 0

      ! fill matrix
      call setPETSCmatrixEntries()

      if (jacompprecond == 1) then
         ! compute preconditioner
         call createPETSCPreconditioner(iprecond)
      end if

      ! fill vector rhs
      i = 0
      rhs_val = 0.0_dp
      do n = nogauss + 1, nogauss + nocg
         ndn = noel(n)
         if (iglobal(ndn) > 0) then
            i = iglobal(ndn) - iglobal(rowtoelem(1)) + 1
            rhs_val(i) = ddr(ndn)
         end if
      end do

      ! fill vector sol
      sol_val = 0.0_dp
      do n = nogauss + 1, nogauss + nocg
         ndn = noel(n)
         if (iglobal(ndn) > 0) then
            i = iglobal(ndn) - iglobal(rowtoelem(1)) + 1
            sol_val(i) = s1(ndn)
         end if
      end do

      ! solve system
      call KSPSolve(Solver, rhs, sol, ierr)
      if (ierr /= PETSC_OK) then
         go to 1234
      end if

      call KSPGetConvergedReason(Solver, Reason, ierr)
      if (ierr /= PETSC_OK) then
         go to 1234
      end if

      reason%v = -9

      ! check for convergence
      if (Reason%v == KSP_DIVERGED_INDEFINITE_PC%v) then
         if (my_rank == 0) then
            call mess(LEVEL_WARN, 'Divergence because of indefinite preconditioner')
         end if
      else if (Reason%v == KSP_DIVERGED_NANORINF%v) then
         call mess(LEVEL_WARN, 'PETSc solver diverged. Divergence reason: a not a number or infinity was detected in a vector during the computation. &
            The simulation became numerically unstable, generating invalid values (NaN/Infinity), which caused the model to crash. &
            Review the model input and inspect the output results to identify unrealistic values or sources of instability.')
      else if (Reason%v < 0) then
         call KSPGetConvergedReasonString(Solver, reason_string, ierr)
         call mess(LEVEL_WARN, 'PETSc solver diverged. Divergence reason: ', reason_string, '. &
            Review the model input and inspect the output results to identify unrealistic values or sources of instability.')
         ! see http://www.mcs.anl.gov/petsc/petsc-current/docs/manualpages/KSP/KSPConvergedReason.html for reason            
      else
         call KSPGetIterationNumber(Solver, its, ierr)
         ! compute residual
         call KSPGetResidualNorm(Solver, rnorm, ierr)
         !
         if (ierr == PETSC_OK .and. my_rank == 0) then
            if (jalogsolverconvergence == 1) then
               write (message, '(a,i0,a,g11.4,a,f8.4)') 'Solver converged in ', its, ' iterations, res=', rnorm, ' dt = ', dts
               call mess(LEVEL_INFO, message)
            end if
         end if
         jasucces = 1
      end if
      if (ierr /= PETSC_OK) then
         call mess(LEVEL_ERROR, 'conjugategradientPETSC: PETSC_ERROR (after solve)')
      end if
      if (ierr /= PETSC_OK) then
         go to 1234
      end if

      ! fill vector sol
      do n = nogauss + 1, nogauss + nocg
         ndn = noel(n)
         if (iglobal(ndn) > 0 .and. kfs(ndn) > 0) then
            i = iglobal(ndn) - iglobal(rowtoelem(1)) + 1
            s1(ndn) = sol_val(i)
         end if
      end do

1234  continue

      ! mark fail by setting number of iterations to -999
      if (jasucces /= 1) then
         its = -999
         call mess(LEVEL_DEBUG, 'conjugategradientPETSC: error.')
      end if

   end subroutine conjugategradientPETSC

   subroutine killSolverPETSC()
      use petsc, only: kspdestroy
      use m_petsc, only: PETSC_OK, isKSPCreated, Solver

      PetscErrorCode :: ierr

      ierr = PETSC_OK
      if (isKSPCreated) then
         call KSPDestroy(Solver, ierr)
      end if
   end subroutine killSolverPETSC
end submodule m_solve_petsc_
