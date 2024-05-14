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
module test_lateral
   use ftnunit
   use precision, only: hp
   use stdlib_kinds, only: dp
   use dfm_error, only: DFM_NOERR, DFM_GENERICERROR
   use m_lateral

   implicit none

   real(hp), parameter :: tolerance = 1.0e-10_hp

   contains
!
!
!==============================================================================
!   
subroutine tests_lateral
   call test( test_get_lateral_volume_per_layer, 'Test computation of water volume per layer in laterals.')
end subroutine tests_lateral
!
!==============================================================================
!> Test computation of water volume per layer for laterals.
!> This test assumes a model of dimension (nx,ny,nz) = (3,3,3).
!> In the last node, the model is shallow meaning it has only 2 active layers.
!> The model contains 2 laterals. 
subroutine test_get_lateral_volume_per_layer
   use m_flow, only: vol1, kbot, ktop, kmxn, kmx
   
   real(kind=dp), allocatable, dimension(:,:)   :: lateral_volume_per_layer  !< Water volume per layer in laterals, dimension = (number_of_layer,number_of_lateral) = (kmx,numlatsg)
   
   integer :: ierr                     !< error flag
   integer :: ndx, ndkx
   integer :: i_cell
   
   ! specify number of computational cells, ndx
   ndx = 9
   
   ! initialize number of active layers for each node
   allocate(kmxn(ndx),stat=ierr)
   kmxn = (/3,3,3,3,3,3,3,3,2/) ! the last cell is assumed shallow and contains only two layers

   ! initialize water volume per cell, vol1
   kmx = 3
   ndkx = ndx * (kmx + 1) - 1 ! one cell is shallow and contains only two layers
   allocate(vol1(ndkx),stat=ierr)
   vol1(ndx+1:) = 1d0 ! only volume per cell, per layer is needed; the first ndx elements contain 2D volume (i.e. total volume over all layers) and are not needed for the function tested here, hence not set.
   
   ! initialize kbot and ktop
   allocate(kbot(ndx),stat=ierr)
   allocate(ktop(ndx),stat=ierr)
   kbot(1) = ndx + 1
   ktop(1) = kbot(1) + kmxn(1) - 1
   do i_cell = 2,ndx
      kbot(i_cell) = ktop(i_cell-1) + 1
      ktop(i_cell) = kbot(i_cell) + kmxn(i_cell) - 1
   enddo
   
   ! initialize laterals administration
   numlatsg = 2
   allocate(n1latsg(numlatsg),stat=ierr)
   allocate(n2latsg(numlatsg),stat=ierr)
   allocate(nnlat(nlatnd),stat=ierr)
   nnlat = (/1,2,8,9/)
   n1latsg = (/1,3/)
   n2latsg = (/2,4/)
   
   allocate(lateral_volume_per_layer(kmx, numlatsg),stat=ierr)
   call get_lateral_volume_per_layer(lateral_volume_per_layer)
   call assert_comparable(lateral_volume_per_layer(1,1), 2d0, tolerance, "get_lateral_volume_per_layer(1,1) output lateral_volume_per_layer is not correct" )
   call assert_comparable(lateral_volume_per_layer(2,1), 2d0, tolerance, "get_lateral_volume_per_layer(2,1) output lateral_volume_per_layer is not correct" )
   call assert_comparable(lateral_volume_per_layer(3,1), 2d0, tolerance, "get_lateral_volume_per_layer(3,1) output lateral_volume_per_layer is not correct" )
   call assert_comparable(lateral_volume_per_layer(1,2), 1d0, tolerance, "get_lateral_volume_per_layer(1,2) output lateral_volume_per_layer is not correct" )
   call assert_comparable(lateral_volume_per_layer(2,2), 2d0, tolerance, "get_lateral_volume_per_layer(2,2) output lateral_volume_per_layer is not correct" )
   call assert_comparable(lateral_volume_per_layer(3,2), 2d0, tolerance, "get_lateral_volume_per_layer(3,2) output lateral_volume_per_layer is not correct" )

   deallocate(kmxn)
   deallocate(vol1)
   deallocate(kbot)
   deallocate(ktop)
   deallocate(n1latsg)
   deallocate(n2latsg)
   deallocate(nnlat)
   deallocate(lateral_volume_per_layer)

end subroutine test_get_lateral_volume_per_layer

end module test_lateral
