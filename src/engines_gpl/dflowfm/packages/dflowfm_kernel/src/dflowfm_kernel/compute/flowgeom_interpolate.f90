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

module m_flowgeom_interpolate
   use precision, only: dp
   use m_flowgeom, only: ndx, lnx, ln, nd, acl, wcL

   implicit none
   private

   public :: node_to_link_vector
   public :: node_to_link_scalar
   public :: link_to_node_vector
   public :: link_to_node_scalar

contains

   !> Interpolate vector components from flow nodes (cell centres) to flow links.
   subroutine node_to_link_vector(xcomp_node, ycomp_node, xcomp_link, ycomp_link, number_of_links)
      real(kind=dp), intent(in) :: xcomp_node(:)  !< x-component of vector at flow nodes
      real(kind=dp), intent(in) :: ycomp_node(:)  !< y-component of vector at flow nodes
      real(kind=dp), intent(out) :: xcomp_link(:) !< x-component of vector at flow links
      real(kind=dp), intent(out) :: ycomp_link(:) !< y-component of vector at flow links
      integer, intent(in) :: number_of_links  !< number of flow links

      integer :: i_link, k1, k2

      do concurrent (i_link = 1:number_of_links)
         k1 = ln(1, i_link)
         k2 = ln(2, i_link)
         xcomp_link(i_link) = acl(i_link) * xcomp_node(k1) + (1.0_dp - acl(i_link)) * xcomp_node(k2)
         ycomp_link(i_link) = acl(i_link) * ycomp_node(k1) + (1.0_dp - acl(i_link)) * ycomp_node(k2)
      end do
   end subroutine node_to_link_vector

   !> Interpolate scalar from flow nodes (cell centres) to flow links.
   subroutine node_to_link_scalar(scalar_node, scalar_link, number_of_links)
      real(kind=dp), intent(in) :: scalar_node(:)  !< x-component of vector at flow nodes
      real(kind=dp), intent(out) :: scalar_link(:) !< x-component of vector at flow links
      integer, intent(in) :: number_of_links  !< number of flow links

      integer :: i_link, k1, k2

      do concurrent (i_link = 1:number_of_links)
         k1 = ln(1, i_link)
         k2 = ln(2, i_link)
         scalar_link(i_link) = acl(i_link) * scalar_node(k1) + (1.0_dp - acl(i_link)) * scalar_node(k2)
      end do
   end subroutine node_to_link_scalar

   !> Interpolate vector components from flow links to flow nodes (cell centres).
   subroutine link_to_node_vector(xcomp_link, ycomp_link, xcomp_node, ycomp_node, number_of_nodes)
      real(kind=dp), intent(in) :: xcomp_link(:)  !< x-component of vector at flow links
      real(kind=dp), intent(in) :: ycomp_link(:)  !< y-component of vector at flow links
      real(kind=dp), intent(out) :: xcomp_node(:) !< x-component of vector at flow nodes
      real(kind=dp), intent(out) :: ycomp_node(:) !< y-component of vector at flow nodes
      integer, intent(in) :: number_of_nodes  !< number of flow nodes

      integer :: i_node, LL, LLL, k

      xcomp_node = 0.0_dp
      ycomp_node = 0.0_dp

      do concurrent(i_node = 1:number_of_nodes)
         do LL = 1, nd(i_node)%lnx
            LLL = abs(nd(i_node)%ln(LL))
            k = 1
            if (nd(i_node)%ln(LL) > 0) then
               k = 2
            end if
            xcomp_node(i_node) = xcomp_node(i_node) + xcomp_link(LLL) * wcL(k, LLL)
            ycomp_node(i_node) = ycomp_node(i_node) + ycomp_link(LLL) * wcL(k, LLL)
         end do
      end do
   end subroutine link_to_node_vector

   !> Interpolate scalar value from flow links to flow nodes (cell centres).
   subroutine link_to_node_scalar(scalar_link, scalar_node, number_of_nodes)
      real(kind=dp), intent(in) :: scalar_link(:)  !< scalar values at flow links
      real(kind=dp), intent(out) :: scalar_node(:) !< scalar values at flow nodes
      integer, intent(in) :: number_of_nodes  !< number of flow nodes

      integer :: i_node, LL, LLL, k

      scalar_node = 0.0_dp

      do concurrent(i_node = 1:number_of_nodes)
         do LL = 1, nd(i_node)%lnx
            LLL = abs(nd(i_node)%ln(LL))
            k = 1
            if (nd(i_node)%ln(LL) > 0) then
               k = 2
            end if
            scalar_node(i_node) = scalar_node(i_node) + scalar_link(LLL) * wcL(k, LLL)
         end do
      end do
   end subroutine link_to_node_scalar

end module m_flowgeom_interpolate