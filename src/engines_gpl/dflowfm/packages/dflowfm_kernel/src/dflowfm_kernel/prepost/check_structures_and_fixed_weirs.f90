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

!> check if structures on flowlinks are unique
subroutine check_structures_and_fixed_weirs()
   use m_flowgeom, only: Lnx
   use fm_external_forcings_data, only: ncgensg, kcgen, L1cgensg, L2cgensg, cgen_ids
   use m_fixedweirs, only: nfxw, lnfxw
   use unstruc_messages
   implicit none

   integer, dimension(:), allocatable :: links_used_by_structures
   integer, dimension(:), allocatable :: links_used_by_weirs

   integer :: flow_link, fixed_weir, general_structure, k
   integer :: nummulti
   integer :: numweir
   integer, parameter :: FREE = 0

   allocate (links_used_by_structures(Lnx))
   links_used_by_structures = FREE
   allocate (links_used_by_weirs(Lnx))
   links_used_by_weirs = FREE

   do fixed_weir = 1, nfxw
      flow_link = lnfxw(fixed_weir)
      links_used_by_weirs(flow_link) = fixed_weir
   end do

   nummulti = 0
   numweir = 0
!  loop over structures
   do general_structure = ncgensg, 1, -1
!     loop over flowlinks of structure
      do k = L1cgensg(general_structure), L2cgensg(general_structure)
         flow_link = kcgen(3, k)

         if (links_used_by_structures(flow_link) == FREE) then
            links_used_by_structures(flow_link) = general_structure
         else
            nummulti = nummulti + 1
            write (msgbuf, "('Flowlink ', I0, ' found in general structure ', A, ' already claimed by general structure ', A, '.')") &
               flow_link, trim(cgen_ids(general_structure)), trim(cgen_ids(links_used_by_structures(flow_link)))
            call mess(LEVEL_WARN, trim(msgbuf))
         end if

         if (links_used_by_weirs(flow_link) /= FREE) then
            numweir = numweir + 1
            write (msgbuf, &
                   "('Flowlink ',I0,' found in general structure ', A,' is also used by a fixed weir. It may lead to a wrong solution.')") &
               flow_link, trim(cgen_ids(general_structure))
            call mess(LEVEL_WARN, trim(msgbuf))
         end if
      end do
   end do

   if (nummulti > 0) then
      call mess(LEVEL_ERROR, 'multiple general structures defined on one or more flowlink(s), see preceding message(s).')
   end if

!  deallocate
   if (allocated(links_used_by_structures)) deallocate (links_used_by_structures)
   if (allocated(links_used_by_weirs)) deallocate (links_used_by_weirs)

   return
end subroutine check_structures_and_fixed_weirs
