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
module m_caltem
   use m_waq_precision

   implicit none

contains

   subroutine caltem(process_space_real, fl, ipoint, increm, num_cells, &
                     noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
                     num_exchanges_z_dir, num_exchanges_bottom_dir)
      !>\file
      !>       Calculation of temperature

      !
      !     Description of the module :
      !
      !        General water quality module for DELWAQ:
      !        pick up temperature from flow model and apply a change
      !
      !        AVERAGED MODELS
      !
      ! Name    T   L I/O  Description                              Units
      ! ----    --- -  -   -------------------                      ----
      ! TEMPF   R   1  I   Temperature from flow model (degC)
      ! DTEMP   R   1  I   Change to be applied (degC)
      ! TEMP    R   1  O   Result (degC)

      !     Logical Units : -

      !     Modules called : -

      !     Name     Type   Library
      !     ------   -----  ------------

      implicit real(A - H, J - Z)
      implicit integer(I)

      real(kind=real_wp) :: process_space_real(*), FL(*)
      integer(kind=int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                              IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir
      !
      IP1 = IPOINT(1)
      IP2 = IPOINT(2)
      IP3 = IPOINT(3)
      !
      do ISEG = 1, num_cells

         if (btest(IKNMRK(ISEG), 0)) then
            !
            TEMPF = process_space_real(IP1)
            DTEMP = process_space_real(IP2)

            TEMP = TEMPF + DTEMP
            TEMP = min(TEMP, 100.)
            TEMP = max(TEMP, 0.)

            process_space_real(IP3) = TEMP
            !
         end if
         !
         IP1 = IP1 + INCREM(1)
         IP2 = IP2 + INCREM(2)
         IP3 = IP3 + INCREM(3)
         !
      end do
      !

      return
   end

end module m_caltem
