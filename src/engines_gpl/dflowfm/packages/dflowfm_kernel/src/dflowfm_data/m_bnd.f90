!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
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

   ! todo: MERGE THIS WITH UNSTRUC_BOUNDARIES
module m_bnd   !< boundary-type module
implicit none
   integer, parameter :: NAMLEN = 128

   type bndtype
      character(len=NAMLEN)                                 :: name     !< boundary-type name
      integer                                               :: N        !< number of boundary points
      double precision, dimension(:),   allocatable         :: x        !< inner node x-coordinates
      double precision, dimension(:),   allocatable         :: y        !< inner node y-coordinates
      double precision, dimension(:),   allocatable         :: sigma    !< sigma-values
      double precision, dimension(:),   allocatable         :: zminmax  !< zmin and zmax
      double precision, dimension(:),   allocatable         :: z        !< boundary condition values
      double precision, dimension(:,:), allocatable         :: xy2      !< outer-node (x,y)-coordinates
      integer,          dimension(:),   allocatable         :: kd       !< boundary points
      integer,          dimension(:,:), allocatable         :: k        !< index array, see e.g. kbnd
      double precision, dimension(:),   allocatable         :: tht      !< Thatcher-Harleman outflow time
      double precision, dimension(:),   allocatable         :: thz      !< Thatcher-Harleman concentration
   end type bndtype

   contains

!> deallocate boundary-type
   subroutine dealloc_bnd(bnd)
      implicit none

      type(bndtype), intent(inout) :: bnd       !< boundary data

      if ( allocated(bnd%x)     ) deallocate(bnd%x)
      if ( allocated(bnd%y)     ) deallocate(bnd%y)
      if ( allocated(bnd%sigma) ) deallocate(bnd%sigma )
      if ( allocated(bnd%zminmax)) deallocate(bnd%zminmax )
      if ( allocated(bnd%z)     ) deallocate(bnd%z)
      if ( allocated(bnd%xy2)   ) deallocate(bnd%xy2)
      if ( allocated(bnd%kd)    ) deallocate(bnd%kd)
      if ( allocated(bnd%k)     ) deallocate(bnd%k)

      return
   end subroutine dealloc_bnd

!> (re)allocate boundary-type
   subroutine alloc_bnd(N, kmx, bnd)
      implicit none

      integer,       intent(in)    :: N         !< number of boundary points
      integer,       intent(in)    :: kmx       !< maximum number of layers
      type(bndtype), intent(inout) :: bnd       !< boundary data

      call dealloc_bnd(bnd)

      if ( N.gt.0 ) then
         allocate(bnd%x(N))
         allocate(bnd%y(N))

         allocate(bnd%xy2(2,N))
         allocate(bnd%kd(N))
         allocate(bnd%k(5,N))

         bnd%x   = 0d0
         bnd%y   = 0d0
         bnd%xy2 = 0d0
         bnd%kd  = 0
         bnd%k   = 0

         if ( kmx.gt.0 ) then
            allocate(bnd%sigma(kmx*N))
            allocate(bnd%zminmax(2*N))
            allocate(bnd%z(kmx*N))
            bnd%sigma = 0d0
            bnd%z     = 0d0
         else
            allocate(bnd%z(N))
            bnd%z = 0d0
         end if
      end if

      return
   end subroutine alloc_bnd

   !> deallocate bndtr
   subroutine dealloc_bndarr(bndarr)
      implicit none

      type(bndtype), dimension(:), allocatable, intent(inout) :: bndarr

      integer :: i, numbnd


      if ( allocated(bndarr) ) then
         numbnd = ubound(bndarr,1)
         do i=1,numbnd
            call dealloc_bnd(bndarr(i))
         end do
         deallocate(bndarr)
      end if

      return
   end subroutine dealloc_bndarr

   end module m_bnd
