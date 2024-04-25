!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2015-2024.                                
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

   module m_parms
#ifdef HAVE_PARMS
#include "../third_party_open/pARMS/pARMS_3.2/include/fparms.h"
   parms_Map                                   :: map          ! map (represents how data are distributed across processors, see [1])
   parms_Mat                                   :: mat          ! matrix
   parms_PC                                    :: pc           ! preconditioner
   parms_Solver                                :: ksp          ! solver
   
   double precision, dimension(:), allocatable :: sol          ! solution vector
   double precision, dimension(:), allocatable :: rhs          ! right-hand side vector
   
   integer                                     :: numloc       ! number of local rows (elements) in matrix
   integer                                     :: nummat       ! number of matrix entries
   double precision, dimension(:), allocatable :: amat         ! matrix entries, dim(nummat)
   integer,          dimension(:), allocatable :: imat, jmat   ! column indices and row pointers of matrix, dim(numloc+1) and dim(nummat) respectively
   integer,          dimension(:), allocatable :: guusidx      ! index in guus matrix-array
   
   integer                                     :: numzerorows  ! number of zero rows
!   integer,          dimension(:), allocatable :: izerorow     ! zero-rows in matrix (kfs=0)
   integer,          dimension(:), allocatable :: irows        ! rows to be inserted in subsequent matrix construction
   
   integer,          dimension(:), allocatable :: rowtoelem    ! local row to element (flownode), dim(numloc)
   integer,          dimension(:), allocatable :: irowglob     ! local row to global row, dim(numloc)
   
   integer                                     :: iparmsstat=1 ! status
   integer, parameter                          :: IPARMS_OK=0
   integer, parameter                          :: IPARMS_empty=1
   integer, parameter                          :: IPARMS_error=2
   
   
!  References
!  [1] "pARMS: A Package for the Parallel Iterative Solution of General Large Sparse Linear System, Reference Manual", November 10, 2010
#endif
end module m_parms


! initialize parms solver: allocate arrays and set sparsity pattern
subroutine iniparms(ierr)
   use m_reduce
   use m_partitioninfo
   use m_parms
   use mpi
   use unstruc_messages
   use m_flowgeom, only: Ndx
   use m_flowparameters
   implicit none
   
   integer,               intent(out) :: ierr  ! error (1) or not (0)
#ifdef HAVE_PARMS   
   integer, dimension(:), allocatable :: mask
   integer, dimension(:), allocatable :: p2nodes
   integer, dimension(:), allocatable :: part
   
   integer                            :: irow, idxmat
   integer                            :: i, j, n
   
   character(len=100)                 :: param
   
   double precision, parameter        :: a_init = 1d0 ! initial value for matrix entries
   
#ifdef HAVE_PARMS   
   call deallocparms()
#endif
   
   ierr = 1
   
!  make global numbering
   call get_global_numbers(nocg,noel(nogauss+1:nogauss+nocg), iglobal, numcells, 0)
   
!  set number of local rows
   numloc = numcells(my_rank)
   
!  allocate local variables
   allocate(mask(Ndx))
   allocate(p2nodes(max(ndomains,1)+1))
   allocate(part(numloc))
   
!  compute start-pointers for the subdomains
   p2nodes(1) = 1
   do i=1,max(ndomains,1)
      p2nodes(i+1) = p2nodes(i)+numcells(i-1)   ! numcells is zero-based
   end do
   
!  set subdomain number (part) for the cells in this subdomain
   part = my_rank + 1
   
!  generate CRS indices 
!  mark active cells
   mask = 0
   do n=nogauss+1,nogauss+nocg
      mask(noel(n)) = 1
   end do

   !  unmark all ghost cells
   if ( jaoverlap.eq.0 ) then
      do i=1,numghost_sall
         mask(ighostlist_sall(i)) = 0
      end do
   else
      do i=1,numghost_snonoverlap
         mask(ighostlist_snonoverlap(i)) = 0
      end do
   end if
   
!  count nonzero elements
   nummat = 0
   numloc   = 0
   do n=nogauss+1,nogauss+nocg
      ndn=noel(n)
      if ( mask(ndn).eq.1 ) then
         numloc = numloc+1
         
!        diagonal element
         nummat = nummat+1

!        off-diagonal elements
         do i=1,row(ndn)%l
            j = row(ndn)%j(i)
            if ( iglobal(j).eq.0 ) cycle
            nummat = nummat+1
         end do
         
      end if
   end do
!  for safety, check if number of local rows equals the number of local cells
   if ( numloc.ne.numcells(my_rank) ) then
      call mess(LEVEL_ERROR, 'iniparms: global cell numbering error')
      goto 1234
   end if

!  (re)allocate module variables
   allocate(rowtoelem(numloc))
   allocate(irowglob(numloc))
   allocate(imat(numloc+1))
   allocate(jmat(nummat))
   allocate(amat(nummat))
   allocate(guusidx(nummat))
!   allocate(izerorow(numloc))
   allocate(irows(numloc))
   
   allocate(rhs(numloc))
   rhs = 0d0
   allocate(sol(numloc))
   sol = 0d0

!  generate RCS index arrays (1-based)
   irow = 0
   idxmat = 1
   imat(1) = idxmat
   do n=nogauss+1,nogauss+nocg
      ndn = noel(n)
      if ( mask(ndn).eq.1 ) then
         irow = irow+1
         
         rowtoelem(irow) = ndn
         
!        set global row of local row
         irowglob(irow) = iglobal(ndn)

!        diagonal element
         jmat(idxmat) = iglobal(ndn)
         guusidx(idxmat) = -ndn
         idxmat = idxmat+1
         
!        off-diagonal elements
         do i=1,row(ndn)%l
            j = row(ndn)%j(i)
            if ( iglobal(j).eq.0 ) cycle

            jmat(idxmat) = iglobal(j)
            guusidx(idxmat) = row(ndn)%a(i)
            idxmat = idxmat+1
         end do
         
         imat(irow+1) = idxmat
      end if
   end do
   
!  set matrix entries to initial value
   amat = a_init
   
!  create PARMS objects   
   iparmsstat = IPARMS_ERROR

!  create map object
   if ( jampi.eq.1 ) then
      call parms_MapCreateFromDist(map, p2nodes, part, MPI_COMM_WORLD, 1, 1, NONINTERLACED, ierr)
      if ( ierr.ne.0 ) goto 1234
   else
      call parms_MapCreateFromLocal(map, numloc, 1, ierr)
      if ( ierr.ne.0 ) goto 1234
   end if
   
!  create matrix object
   call parms_MatCreate(mat, map, ierr)
   if ( ierr.ne.0 ) goto 1234
   
!  create preconditioner
   call parms_pccreate(pc, mat, ierr)
   if ( ierr.ne.0 ) goto 1234
   
   
!  set preconditioner parameters
   select case ( iparms(IPARMS_ILUTYPE) )
      case ( 1 )
         call parms_pcsetilutype(pc, PCILU0, ierr)
      case ( 2 )
         call parms_pcsetilutype(pc, PCILUK, ierr)
      case ( 3 )
         call parms_pcsetilutype(pc, PCILUT, ierr)
      case ( 4 )
         call parms_pcsetilutype(pc, PCARMS, ierr)
      case ( 0 )
!        default settings
    end select
    
    if ( dparms(IPARMS_DTOL).gt.0d0 ) then
       call parms_pcsettol(pc, dparms(IPARMS_DTOL), ierr)
       if ( ierr.ne.0 ) goto 1234
    end if
    
    if ( iparms(IPARMS_NLEVEL).gt.0 ) then
       call parms_pcsetNlevels(pc, iparms(IPARMS_NLEVEL), ierr)
       if ( ierr.ne.0 ) goto 1234
    end if
    
!  create solver
   call parms_solvercreate(ksp, mat, pc, ierr)
   if ( ierr.ne.0 ) goto 1234
   
!  set tolerance
   write(param, '(e15.5)') epscg
   call parms_solversetparam(ksp,DTOL,trim(param),ierr)
   
!  for subsequent constructions of matrix: insert all rows
   do i=1,numloc
      irows(i)=iglobal(rowtoelem(i))
   end do
   
!  reset matrix to be re-used
   call parms_matreset(mat, SAME_NONZERO_STRUCTURE, ierr)
   if ( ierr.ne.0 ) goto 1234
   
   iparmsstat = IPARMS_OK
   ierr = 0
1234 continue

   if ( ierr.ne.0 ) then
      call mess(LEVEL_ERROR, 'iniparm: error')
   end if

!  deallocate
   if ( allocated(mask)    ) deallocate(mask)
   if ( allocated(p2nodes) ) deallocate(p2nodes)
   if ( allocated(part)    ) deallocate(part)
   
#endif

   return
end subroutine iniparms


!> compose the global matrix and solve with parms
!>  it is assumed that the global cell numbers iglobal, dim(Ndx) are available
!>  NO GLOBAL RENUMBERING, so the matrix may contain zero rows
subroutine conjugategradient_parms(s1,Ndx,its)
   use m_parms
   use m_reduce
   use m_partitioninfo
   use m_flowgeom, only: kfs, ba
   use m_flowtimes, only: dti
   use unstruc_messages
   implicit none
   
   integer,                          intent(in)    :: Ndx   !< number of unknowns (flownodes)
   double precision, dimension(Ndx), intent(inout) :: s1    !< water level
   integer,                          intent(out)   :: its   !< number of iterations
   
   double precision                                :: res   ! residual norm
   
   integer                                         :: i, j, n
   
   integer                                         :: ierr  ! error (1) or not (0)
   
   character(len=100)                              :: message
   
   integer, save                                   :: japrecond = 1
   
   ierr = 1
   its = 0
   
#ifdef HAVE_PARMS   
   numzerorows = 0
!   izerorow = 0
   amat = 0d0
   
!  fill matrix entries
   do n=1,nummat
      i = guusidx(n)
      if ( i.lt.0 ) then   ! diagonal entry
         if ( kfs(-i).ne.0 ) then ! nonzero row
            amat(n) = bbr(-i)
         else  ! zero row
            numzerorows=numzerorows+1
!            izerorow(numzerorows) = iglobal(-i)
            amat(n) =dti * ba(-i)
         end if
      else  ! off-diagonal entry
         amat(n) = ccr(i)
      end if
   end do
   
!   call parms_MatResetRowValues(mat, numloc, irows, imat, jmat, amat, ierr)
   call parms_MatSetValues(mat, numloc, irows, imat, jmat, amat, INSERT, ierr)
   if ( ierr.ne.0 ) goto 1234
   call parms_MatSetup(mat, ierr)
   if ( ierr.ne.0 ) goto 1234
   
!  fill right-hand side and set initial values
   rhs = 0d0
   do i=1,numloc
      n = rowtoelem(i)
      rhs(i) = ddr(n)
      sol(i) = s1(n)
   end do
   
!!  alternate preconditioner   
!   if ( japrecond.eq.1 ) then
!!     setup preconditioner
      call parms_pcsetup(pc, ierr)
!      if ( ierr.ne.0 ) goto 1234
!      japrecond = 0
!   else
!      japrecond = 1
!   end if
   
!  solve system
   call parms_solverapply(ksp, rhs, sol, ierr)
   if ( ierr.ne.0 ) goto 1234
   
!  get residual norm
   call parms_solvergetresidualnorm2(ksp,rhs,sol,res,ierr)
   if ( ierr.ne.0 ) goto 1234
   
!  get number of iterations
   call parms_solvergetits(ksp, its, ierr)
   if ( ierr.ne.0 ) goto 1234
   
!  print residual norm   
   if ( my_rank.eq.0 ) then
   write(message,*) 'Solver converged in ',its,' iterations, res=', res
   call mess(LEVEL_INFO, message)
   end if
   
!  reset matrix to be re-used
   call parms_matreset(mat, SAME_NONZERO_STRUCTURE, ierr)
!   if ( ierr.ne.0 ) goto 1234
   
!  copy solution vector to water-levels
   do i=1,numloc
      n = rowtoelem(i)
      if ( kfs(n).ne.0 ) then
         s1(n) = sol(i)
      end if
   end do
   
   ierr = 0
1234 continue

   if ( ierr.ne.0 ) then
      call mess(LEVEL_ERROR, 'parms error')
   end if
   
#endif
   
   return
end subroutine conjugategradient_parms


! stop parms
subroutine deallocparms()
#ifdef HAVE_PARMS
   use m_parms
   use m_partitioninfo
   implicit none
   
   integer :: ierr
   
   if ( iparmsstat.eq.IPARMS_OK ) then
      call parms_MapFree(map, ierr)
      call parms_matfree(mat, ierr)
      call parms_pcfree(pc, ierr)
      call parms_solverfree(ksp, ierr)
   end if
   
   if ( allocated(imat)      ) deallocate(imat)
   if ( allocated(jmat)      ) deallocate(jmat)
   if ( allocated(amat)      ) deallocate(amat)
   if ( allocated(guusidx)   ) deallocate(guusidx)
!   if ( allocated(izerorow)  ) deallocate(izerorow)
   if ( allocated(irows)     ) deallocate(irows)
   if ( allocated(rowtoelem) ) deallocate(rowtoelem)
   if ( allocated(irowglob)  ) deallocate(irowglob)
   if ( allocated(sol)       ) deallocate(sol)
   if ( allocated(rhs)       ) deallocate(rhs)
   
   iparmsstat = IPARMS_EMPTY
   
   return
#endif   
end subroutine deallocparms
