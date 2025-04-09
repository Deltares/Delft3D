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
module m_ddepth
   use m_waq_precision

   implicit none

contains

   subroutine ddepth(process_space_real, fl, ipoint, increm, num_cells, &
                     noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
                     num_exchanges_z_dir, num_exchanges_bottom_dir)
      use m_logger_helper, only: write_error_message
      use m_extract_waq_attribute

      !>\file
      !>       Dynamic calculation of the depth as volume / surf

      !
      !     Description of the module :
      !
      !        General water quality module for DELWAQ:
      !        DEPTH CALCULATION FROM HORIZONTAL SURFACE AREA OF A SEGMENT
      !
      ! Name    T   L I/O   Description                                    Units
      ! ----    --- -  -    -------------------                             ----
      ! DEPTH   R*4 1 O depth of the water column                            [m]
      ! SURF    R*4 1 I surface area of segment                             [m2]
      ! VOLUME  R*4 1 I volume of segment                                   [m3]

      !     Logical Units : -

      !     Modules called : -

      !     Name     Type   Library
      !     ------   -----  ------------

      implicit real(A - H, J - Z)
      implicit integer(I)

      real(kind=real_wp) :: process_space_real(*), FL(*)
      integer(kind=int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                              IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir
      character(55) message

      message = 'SURF in DDEPTH zero at segment:'
      IP1 = IPOINT(1)
      IP2 = IPOINT(2)
      IP3 = IPOINT(3)
      !
      IFLUX = 0
      do ISEG = 1, num_cells
         call extract_waq_attribute(3, IKNMRK(ISEG), IKMRK3)
         if (IKMRK3 == 1 .or. IKMRK3 == 3) then
            !
            VOLUME = process_space_real(IP1)
            SURF = process_space_real(IP2)

            if (SURF < 1e-30) then
               write (message(32:55), '(i9,1x,e14.6)') iseg, surf
               call write_error_message(message)
            end if

            !***********************************************************************
            !**** Calculate DEPTH - minimum: "TINY" to avoid divisions by zero if
            !     the volume happens to be zero
            !***********************************************************************
            !
            DEPTH = max(tiny(1.0), VOLUME / SURF)
            !
            process_space_real(IP3) = DEPTH
            !
         end if
         !
         IFLUX = IFLUX + NOFLUX
         IP1 = IP1 + INCREM(1)
         IP2 = IP2 + INCREM(2)
         IP3 = IP3 + INCREM(3)
         !
      end do
      !
      return
      !
   end

end module m_ddepth
