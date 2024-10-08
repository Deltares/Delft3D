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

!> module containing the modelled temperature modification process
module m_tempermode
   use m_logger_helper
   use m_waq_precision

   implicit none

contains

   !> modelled temperature modification process
   subroutine tmode(process_space_real, fl, ipoint, increm, num_cells, &
                    noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
                    num_exchanges_z_dir, num_exchanges_bottom_dir)

      ! arguments
      real(kind=real_wp), dimension(*), intent(inout) :: process_space_real ! input-output array space to be adressed with IPOINT/INCREM
      real(kind=real_wp), dimension(*), intent(inout) :: fl ! flux array
      integer(kind=int_wp), dimension(*), intent(in) :: ipoint ! start index input-output parameters in the process_space_real array (segment or exchange number 1)
      integer(kind=int_wp), dimension(*), intent(in) :: increm ! increment for each segment-exchange for the parameters in the process_space_real array
      integer(kind=int_wp), intent(in) :: num_cells ! number of segments
      integer(kind=int_wp), intent(in) :: noflux ! total number of fluxes (increment in FL array)
      integer(kind=int_wp), dimension(4, *), intent(in) :: iexpnt ! exchange pointer table
      integer(kind=int_wp), dimension(*), intent(in) :: iknmrk ! segment features array
      integer(kind=int_wp), intent(in) :: num_exchanges_u_dir ! number of exchanges in first direction
      integer(kind=int_wp), intent(in) :: num_exchanges_v_dir ! number of exchanges in second direction
      integer(kind=int_wp), intent(in) :: num_exchanges_z_dir ! number of exchanges in third direction
      integer(kind=int_wp), intent(in) :: num_exchanges_bottom_dir ! number of exchanges in fourth direction

      ! local declarations
      real(kind=real_wp) :: mtemp !    1 in  modelled temperature                                           [oC]
      real(kind=real_wp) :: tmpnat !   2 in  natural temperature of ambient water                           [oC]
      integer(kind=int_wp) :: iswtmp ! 3 in  switch if modelled temperature is total or excess temeperature [-]
      real(kind=real_wp) :: mintemp !  4 in  minimum total temperature to be used by processes              [oC]
      real(kind=real_wp) :: maxtemp !  5 in  minimum total temperature to be used by processes              [oC]
      real(kind=real_wp) :: ttemp !    6 out total temperature to be used by processes                      [oC]
      real(kind=real_wp) :: etemp !    7 out excess temperature to be used by processes                     [oC]

      integer(kind=int_wp) :: ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8
      integer(kind=int_wp) :: iflux, iseg, ikmrk2

      ip1 = ipoint(1)
      ip2 = ipoint(2)
      ip3 = ipoint(3)
      ip4 = ipoint(4)
      ip5 = ipoint(5)
      ip6 = ipoint(6)
      ip7 = ipoint(7)
      ip8 = ipoint(8)

      do iseg = 1, num_cells
         mtemp = process_space_real(ip1)
         tmpnat = process_space_real(ip2)
         iswtmp = nint(process_space_real(ip3))
         mintemp = process_space_real(ip4)
         maxtemp = process_space_real(ip5)

         ! What is the meaning of modelled temperatures (one or two may be modelled)
         if (iswtmp == 0) then
            ! User defines total and natural, excess is calculated
            ttemp = mtemp
            etemp = ttemp - tmpnat
         elseif (iswtmp == 1) then
            ! User defines excess and natural, total is calculated
            etemp = mtemp
            ttemp = etemp + tmpnat
         elseif (iswtmp == 2) then
            ! User defines excess and total, nothing is calculated
            ttemp = tmpnat
            etemp = mtemp
         else
            call write_error_message('SwitchTemp has no valid value <0,1,2> in TMODE')
         end if

         ! Output flux, temp, surtemp, heat exchage and temperature increase due to radiation
         process_space_real(ip6) = min(max(ttemp,mintemp),maxtemp)
         process_space_real(ip7) = etemp
         process_space_real(ip8) = tmpnat + 1

         ip1 = ip1 + increm(1)
         ip2 = ip2 + increm(2)
         ip3 = ip3 + increm(3)
         ip4 = ip4 + increm(4)
         ip5 = ip5 + increm(5)
         ip6 = ip6 + increm(6)
         ip7 = ip7 + increm(7)
         ip8 = ip8 + increm(8)
      end do
   end

end module m_tempermode
