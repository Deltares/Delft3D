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
module m_dmvol
   use m_waq_precision

   implicit none

contains

   subroutine dmvol(process_space_real, fl, ipoint, increm, num_cells, &
                    noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
                    num_exchanges_z_dir, num_exchanges_bottom_dir)
      use m_extract_waq_attribute

      !>\file
      !>       Volume of dry matter in a segment

      !
      !     Description of the module :
      !
      !        General water quality module for DELWAQ:
      !
      ! Name    T   L I/O   Description                                    Uni
      ! ----    --- -  -    -------------------                            ---

      !     Logical Units : -

      !     Modules called : -

      !     Name     Type   Library

      !     ------   -----  ------------

      implicit real(A - H, J - Z)
      implicit integer(I)

      real(kind=real_wp) :: process_space_real(*), FL(*)
      integer(kind=int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                              IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir

      parameter(RHOWAT=1000000.)

      IP1 = IPOINT(1)
      IP2 = IPOINT(2)
      IP3 = IPOINT(3)
      IP4 = IPOINT(4)
      IP5 = IPOINT(5)
      IP6 = IPOINT(6)
      IP7 = IPOINT(7)
      IP8 = IPOINT(8)
      IP9 = IPOINT(9)
      IP10 = IPOINT(10)
      !
      IFLUX = 0
      do ISEG = 1, num_cells
         call extract_waq_attribute(1, IKNMRK(ISEG), IKMRK1)

         if (btest(IKNMRK(ISEG), 0)) then

            Surf = process_space_real(IP1)
            Volume = process_space_real(IP2)
            TIM = process_space_real(IP3)
            POM = process_space_real(IP4)
            RhoIM = process_space_real(IP5)
            RhoOM = process_space_real(IP6)

            VolDM = (TIM / RhoIM + POM / RhoOM)
            VolDM = min(1.0, VolDM)
            VolDM = max(0.0, VolDM)
            Poros = 1.0 - VolDM
            Poros = min(0.999, Poros)
            Poros = max(0.02, Poros)
            VolDM = VolDM * Volume
            Rho = (TIM + POM + Poros * RHOWAT)

            if (IKMRK1 == 3) then
               ActTh = VOLUME / SURF
            else
               ActTh = 0.0
            end if

            process_space_real(IP7) = Poros
            process_space_real(IP8) = Rho
            process_space_real(IP9) = VolDM
            process_space_real(IP10) = ActTh

         else
            process_space_real(IP7) = 1.0
            process_space_real(IP8) = RHOWAT
            process_space_real(IP9) = 0.0
            process_space_real(IP10) = 0.0
         end if
         !
         IFLUX = IFLUX + NOFLUX
         IP1 = IP1 + INCREM(1)
         IP2 = IP2 + INCREM(2)
         IP3 = IP3 + INCREM(3)
         IP4 = IP4 + INCREM(4)
         IP5 = IP5 + INCREM(5)
         IP6 = IP6 + INCREM(6)
         IP7 = IP7 + INCREM(7)
         IP8 = IP8 + INCREM(8)
         IP9 = IP9 + INCREM(9)
         IP10 = IP10 + INCREM(10)
         !
      end do
      !
      return
      !
   end

end module m_dmvol
