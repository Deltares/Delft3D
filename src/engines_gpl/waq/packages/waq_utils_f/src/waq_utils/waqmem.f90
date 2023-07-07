!!  Copyright (C)  Stichting Deltares, 2012-2023.
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

MODULE waqmem

!     Deltares Software Centre

!     Function            : This is the new memory allocation area.
!                           It is like a huge unnamed common block.
!                           This invites to include it in every routine and make unstructured
!                           use of everything everywhere
!                           Delwaq group should strictly maintain the policy to only include it
!                           in the dlwqn$ routines and have the remainder of the source code
!                           driven by its parameter lists.
!                           The allocation should be reserved to the routines dhmmja, dhmmra and
!                           dhmmca where also the total memory demand is printed

!     Created             : January 2010 by Leo Postma

!      wasteloads

   integer, pointer     :: iwstkind(:)      ! steers flow-concentration processing
   integer                 ftype   (50)     ! copy of filtype in delwaq2

!      general solvers

   real(4), allocatable :: surface (:)      ! horizontal surface
   real(8), allocatable :: volume0 (:)      ! begin volume of a time step
   real(8), allocatable :: volume1 (:)      ! end   volume of a time step
   real(4), allocatable :: mixlen  (:)      ! standard mixing flow m3/s
   real(4), allocatable :: wdrawal (:)      ! withdrawal term
   integer, allocatable :: rowpnt  (:)      ! start of each row in the matrix (0:n)-array
   integer, allocatable :: fmat    (:)      ! pointer from(iq) in matrix
   integer, allocatable :: tmat    (:)      ! pointer to  (iq) in matrix
   integer, pointer     :: iexseg  (:,:)    ! zero if volume is explicit
   integer, pointer     :: iknmkv  (:,:)    ! time variable feature array (for drying/flooding)
   integer, allocatable :: isegcol (:)      ! pointer from segment to top of column

!      solver  6, 7 and 10 only

   real(4), allocatable :: rhs     (:,:)    ! delmat right hand side

!      solver 11, 12, 13, 14 and 24 only

   real(8), allocatable :: arhs    (:,:)    ! right hand side vertically implicit schemes
   real(8), allocatable :: adiag   (:,:)    ! diagonal filled with volumes vertically implicit schemes
   real(8), allocatable :: acodia  (:,:)    ! workarray under codiagonal vertical transport
   real(8), allocatable :: bcodia  (:,:)    ! workarray upper codiagonal vertical transport

!      solver 15 and 16 only

   real(8), allocatable :: gm_rhs  (:,:)    ! gmres right hand side
   real(8), allocatable :: gm_sol  (:,:)    ! gmres solution
   real(8), allocatable :: gm_work (:,:)    ! gmres workspace
   real(8), allocatable :: gm_hess (:,:)    ! gmres Hessenberg matrix
   real(8), allocatable :: gm_amat (:,:)    ! gmres off-diagonal entries of matrix
   real(8), allocatable :: gm_diag (:,:)    ! gmres diagonal entries of matrix
   real(8), allocatable :: gm_diac (:,:)    ! gmres unscaled copy of diagonal entries
   real(8), allocatable :: gm_trid (:,:)    ! gmres tridiagonal matrix vertical

!      solver 19 and 20 only

   integer, allocatable :: kadu    (:,:)
   integer, allocatable :: kadv    (:,:)
   integer, allocatable :: kcu     (:)
   real(4), allocatable :: r11     (:,:,:)
   real(4), allocatable :: dfluxx  (:,:,:)
   real(4), allocatable :: dfluxy  (:,:,:)
   real(4), allocatable :: s0      ( : )
   real(4), allocatable :: s1      ( : )
   real(4), allocatable :: dps     ( : )
   real(4), allocatable :: gsqs    ( : )
   real(4), allocatable :: sigdif  (:)
   real(4), allocatable :: sigmol  (:)
   real(4), allocatable :: guu     ( : )
   real(4), allocatable :: gvv     ( : )
   real(4), allocatable :: hu      ( : )
   real(4), allocatable :: hv      ( : )
   real(4), allocatable :: thick   (:)
   real(4), allocatable :: sig     ( : )
   real(4), allocatable :: dicuv   (:,:)
   real(4), allocatable :: dicww   (:,:)
   real(4), allocatable :: sour    (:,:,:)
   real(4), allocatable :: sink    (:,:,:)
   real(4), allocatable :: areau   (:,:)
   real(4), allocatable :: areav   (:,:)
   real(4), allocatable :: rscale  (:,:)

!      if regular grid is provided (for future incorporation PART)

   integer, allocatable :: cellpnt (:)      ! backpointer from noseg to mnmaxk
   integer, allocatable :: flowpnt (:)      ! backpointer from noq to 3*mnmaxk - mnmax
   real(4), allocatable :: cell_x  (:,:)    ! x-values at the corner points of the grid
   real(4), allocatable :: cell_y  (:,:)    ! y-values at the corner points of the grid

!      solver 21 and 22 only

   real(4), allocatable :: theta   (:,:)    ! theta per exchange per processor
   real(4), allocatable :: thetaseg(:,:)    ! theta per volume per processor
   real(4), allocatable :: flowtot (:,:)    ! flow per processor
   real(4), allocatable :: disptot (:,:)    ! dispersion per processor
   real(4), allocatable :: flux    (:,:)    ! flux corrections
   real(4), allocatable :: lim     (:,:)    ! limiter
   real(4), allocatable :: maxi    (:,:)
   real(4), allocatable :: mini    (:,:)
   real(4), allocatable :: l1      (:,:)
   real(4), allocatable :: l2      (:,:)
   real(4), allocatable :: m1      (:,:)
   real(4), allocatable :: m2      (:,:)
   real(4), allocatable :: n1      (:,:)
   real(4), allocatable :: n2      (:,:)

!      solver 24 only

   real(8), allocatable :: dwork   (:,:)    ! work array self adjusting step
   real(8), allocatable :: volint  (:)      ! interpolation array for volumes
   real(8), allocatable :: dconc2  (:,:)    ! first guess array concentrations
   integer, allocatable :: ibas    (:)      ! administrative arrays for the self
   integer, allocatable :: ibaf    (:)      ! adjusting time step procedure
   integer, allocatable :: iords   (:)      ! id.
   integer, allocatable :: iordf   (:)      ! id.
   integer, allocatable :: nvert   (:,:)    ! id.
   integer, allocatable :: ivert   (:)      ! id.

   contains

   subroutine waqmem_deallocate

      implicit none

!      wasteloads

       if (associated(iwstkind)) deallocate(iwstkind)

!      general solvers

       if (allocated(volume0)) deallocate(volume0)
       if (allocated(volume1)) deallocate(volume1)
       if (allocated(mixlen))  deallocate(mixlen)
       if (allocated(rowpnt))  deallocate(rowpnt)
       if (allocated(fmat))    deallocate(fmat)
       if (allocated(tmat))    deallocate(tmat)
       if (associated(iexseg))  deallocate(iexseg)
       if (associated(iknmkv))  deallocate(iknmkv)
       if (allocated(isegcol)) deallocate(isegcol)

!      solver  6, 7 and 10 only

       if (allocated(rhs)) deallocate(rhs)

!      solver 15 and 16 only

       if (allocated(gm_rhs))  deallocate(gm_rhs)
       if (allocated(gm_sol))  deallocate(gm_sol)
       if (allocated(gm_work)) deallocate(gm_work)
       if (allocated(gm_hess)) deallocate(gm_hess)
       if (allocated(gm_amat)) deallocate(gm_amat)
       if (allocated(gm_diag)) deallocate(gm_diag)
       if (allocated(gm_diac)) deallocate(gm_diac)
       if (allocated(gm_trid)) deallocate(gm_trid)

!      solver 19 and 20 only

       if (allocated(kadu)) deallocate(kadu)
       if (allocated(kadv)) deallocate(kadv)
       if (allocated(kcu)) deallocate(kcu)
       if (allocated(r11)) deallocate(r11)
       if (allocated(dfluxx)) deallocate(dfluxx)
       if (allocated(dfluxy)) deallocate(dfluxy)
       if (allocated(s0)) deallocate(s0)
       if (allocated(s1)) deallocate(s1)
       if (allocated(dps)) deallocate(dps)
       if (allocated(gsqs)) deallocate(gsqs)
       if (allocated(sigdif)) deallocate(sigdif)
       if (allocated(sigmol)) deallocate(sigmol)
       if (allocated(guu)) deallocate(guu)
       if (allocated(gvv)) deallocate(gvv)
       if (allocated(hu)) deallocate(hu)
       if (allocated(hv)) deallocate(hv)
       if (allocated(thick)) deallocate(thick)
       if (allocated(sig)) deallocate(sig)
       if (allocated(dicuv)) deallocate(dicuv)
       if (allocated(dicww)) deallocate(dicww)
       if (allocated(sour)) deallocate(sour)
       if (allocated(sink)) deallocate(sink)
       if (allocated(areau)) deallocate(areau)
       if (allocated(areav)) deallocate(areav)
       if (allocated(rscale)) deallocate(rscale)

!      solver 21 only

       if (allocated(theta)) deallocate(theta)
       if (allocated(thetaseg)) deallocate(thetaseg)
       if (allocated(flowtot)) deallocate(flowtot)
       if (allocated(disptot)) deallocate(disptot)
       if (allocated(flux)) deallocate(flux)
       if (allocated(lim)) deallocate(lim)
       if (allocated(maxi)) deallocate(maxi)
       if (allocated(mini)) deallocate(mini)
       if (allocated(l1)) deallocate(l1)
       if (allocated(l2)) deallocate(l2)
       if (allocated(m1)) deallocate(m1)
       if (allocated(m2)) deallocate(m2)
       if (allocated(n1)) deallocate(n1)
       if (allocated(n2)) deallocate(n2)

   end subroutine waqmem_deallocate


END MODULE waqmem
