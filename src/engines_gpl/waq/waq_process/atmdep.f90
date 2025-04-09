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
module m_atmdep
   use m_waq_precision

   implicit none

contains

   subroutine atmdep(process_space_real, fl, ipoint, increm, num_cells, &
                     noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
                     num_exchanges_z_dir, num_exchanges_bottom_dir)
      use m_extract_waq_attribute

      !>\file
      !>       Atmosferic deposition and diffuse input of IMx, N, P, Org_us and Metals

      !
      !     Description of the module :
      !
      !        General water quality module for DELWAQ:
      !        Atmosferic deposition
      !
      ! Name    T   L I/O   Description                                    Units
      ! ----    --- -  -    -------------------                            -----
      ! ZFL     REAL        Zero'th oreder flux         M/m3/s
      ! DEPTH   R*4 1 I     depth                                          [m]
      ! SW1                 load option 0=all, 1=top, 2=bottom segments    (-)
      ! SW2                 maximise withdrawel to mass 0=no, 1=yes        (-)
      !     Logical Units : -

      !     Modules called : -

      !     Name     Type   Library
      !     ------   -----  ------------

      real(kind=real_wp) :: process_space_real(*), FL(*)
      integer(kind=int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                              IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir
      integer(kind=int_wp) :: IP1, IP2, IP3, IP4, IP5, IP6, IFLUX, ISEG, IKMRK1, &
                              IKMRK2, ISW1, ISW2
      real(kind=real_wp) :: zfl, depth, conc, delt

      IP1 = IPOINT(1)
      IP2 = IPOINT(2)
      IP3 = IPOINT(3)
      IP4 = IPOINT(4)
      IP5 = IPOINT(5)
      IP6 = IPOINT(6)
      !
      IFLUX = 0
      do ISEG = 1, num_cells
         call extract_waq_attribute(1, IKNMRK(ISEG), IKMRK1)
         if (IKMRK1 == 1) then

            ZFL = process_space_real(IP1)
            DEPTH = process_space_real(IP2)
            CONC = process_space_real(IP3)
            ISW1 = nint(process_space_real(IP4))
            ISW2 = nint(process_space_real(IP5))
            DELT = process_space_real(IP6)

            call extract_waq_attribute(2, IKNMRK(ISEG), IKMRK2)
            if ((ISW1 == 0) .or. & ! option load in all segments
                (IKMRK2 == 0) .or. & ! segment with surface and bottom always a load
                (IKMRK2 == 1 .and. ISW1 == 1) .or. & ! top segment and option top segment
                (IKMRK2 == 3 .and. ISW1 == 2)) then ! bottom segment and option bottom segment
               !

               !*******************************************************************************
               !**** FLUX equals input divided by depth , M/m2/d * 1/m = M/m3/d
               !***********************************************************************

               ZFL = ZFL / DEPTH

               if (ISW2 == 1) then
                  ! maximise a withdrawel to the amount availeble in one timestep
                  if (ZFL < 0.0) then
                     ZFL = max(ZFL, -CONC / DELT)
                  end if
               end if

               FL(1 + IFLUX) = ZFL

            end if
         end if
         !
         IFLUX = IFLUX + NOFLUX
         IP1 = IP1 + INCREM(1)
         IP2 = IP2 + INCREM(2)
         IP3 = IP3 + INCREM(3)
         IP4 = IP4 + INCREM(4)
         IP5 = IP5 + INCREM(5)
         IP6 = IP6 + INCREM(6)
         !
      end do
      !
      return
      !
   end

end module m_atmdep
