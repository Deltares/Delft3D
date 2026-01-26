!----AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
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
!> Stores values related to circumcenter method
module m_circumcenter_method
   use precision, only: dp

   implicit none
   private
   public extract_circumcenter_method

   integer, parameter, public :: INTERNAL_NETLINKS_EDGE = 1 ! current default - to be deprecated (WO 22.01.2025)
   integer, parameter, public :: INTERNAL_NETLINKS_LOOP = 2
   integer, parameter, public :: ALL_NETLINKS_LOOP = 3

   character(len=128), public :: md_circumcenter_method = 'internalNetlinksEdge' !< Circumcenter method ('internalNetlinksEdge', 'internalNetlinksLoop' or 'allNetlinksLoop')
   integer, public :: circumcenter_method !< Circumcenter computation method (INTERNAL_NETLINKS_EDGE=iterate over each internal netlink; INTERNAL_NETLINKS_LOOP=iterate over each internal netlink loop; ALL_NETLINKS_LOOP=iterate over each netlink loop)
   real(kind=dp), public :: circumcenter_tolerance = 1e-3_dp !< Tolerance for convergence of circumcenter [m]

contains

   !> Extract the circumcenter method from the user provided string.
   function extract_circumcenter_method(circumcenter_method_string, circumcenter_method_read) result(circumcenter_method_)
      use MessageHandling, only: mess, LEVEL_ERROR, LEVEL_WARN
      use string_module, only: str_tolower

      character(len=*), intent(in) :: circumcenter_method_string !< Description of the circumcenter method
      logical, optional, intent(in) :: circumcenter_method_read !< Whether circumcenter_method_string was read

      integer :: circumcenter_method_ !< Local function variable for circumcenter method

      select case (trim(str_tolower(circumcenter_method_string)))
      case ('internalnetlinksedge')
         circumcenter_method_ = INTERNAL_NETLINKS_EDGE
         if (present(circumcenter_method_read)) then
            if (circumcenter_method_read) then
               call mess(LEVEL_WARN, '"[geometry] circumcenterMethod = internalNetlinksEdge" will be deprecated and removed in the future. Please update this in your model. "circumcenterMethod = internalNetlinksLoop" is the improved current implementation that uses only internal net links. "circumcenterMethod = allNetlinksLoop" is a stricter implementation that also considers net links on the boundary of the grid. The new options may require updating your grid.')
            end if
         end if
      case ('internalnetlinksloop')
         circumcenter_method_ = INTERNAL_NETLINKS_LOOP
      case ('allnetlinksloop')
         circumcenter_method_ = ALL_NETLINKS_LOOP
      case default
         call mess(LEVEL_ERROR, 'Did not recognise circumcenterMethod '//trim(circumcenter_method_string)//'. It must be internalNetlinksEdge, internalNetlinksLoop or allNetlinksLoop.')
      end select
   end function extract_circumcenter_method

end module m_circumcenter_method
