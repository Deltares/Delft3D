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
module m_depave
   use m_waq_precision

   implicit none

contains

   subroutine depave(process_space_real, fl, ipoint, increm, num_cells, &
                     noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
                     num_exchanges_z_dir, num_exchanges_bottom_dir)
      use m_logger_helper, only: stop_with_error, get_log_unit_number
      !>\file
      !>       Average depth for a Bloom time step (typically a day)

      !
      !     Description of the module :
      !
      !     Logical Units : -

      !     Modules called : -

      !     Name     Type   Library
      !     ------   -----  ------------

      real(kind=real_wp) :: process_space_real(*), FL(*)
      integer(kind=int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                              IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir

      integer(kind=int_wp) :: LUNREP

      integer(kind=int_wp) :: IP1, IP2, IP3, IP4, IP5, IP6
      real(kind=real_wp) :: DEPTH, ADEPTH
      integer(kind=int_wp) :: TELLER, NAVERA, NSWITS, ISEG
      logical FIRST
      save FIRST
      data FIRST/.true./
      save TELLER
      data TELLER/0/

      IP1 = IPOINT(1)
      IP2 = IPOINT(2)
      IP3 = IPOINT(3)
      IP4 = IPOINT(4)
      IP5 = IPOINT(5)
      IP6 = IPOINT(6)

      !     Check whether certain input parameters are independent of X

      if (FIRST) then
         FIRST = .false.
         if ((INCREM(1) > 0) .or. &
             (INCREM(2) > 0)) then
            call get_log_unit_number(LUNREP)
            write (LUNREP, *) &
               ' DEPAVE: INPUT parameters function(x) not ALLOWED'
            write (*, *) &
               ' DEPAVE: INPUT parameters function(x) not ALLOWED'
            call stop_with_error()
         end if
      end if

      !     Retrieve switch for averaging and nr. of steps to be averaged

      NSWITS = nint(process_space_real(IP1))
      NAVERA = nint(process_space_real(IP2))

      !     Add 1 to counter and check for period

      TELLER = TELLER + 1
      if (TELLER > NAVERA) TELLER = TELLER - NAVERA

      !     Loop over segments

      do ISEG = 1, num_cells

         if (btest(IKNMRK(ISEG), 0)) then

            DEPTH = process_space_real(IP3)
            ADEPTH = process_space_real(IP4)
            process_space_real(IP6) = ADEPTH

            if (NSWITS == 0) then

               !                 No averaging: copy depth to average depth

               process_space_real(IP5) = DEPTH

            else

               !                 Averaging: FANCY FORMULA!!!!!

               process_space_real(IP5) = (ADEPTH * real(TELLER - 1) + DEPTH) &
                                         / real(TELLER)
            end if
         end if
         !
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

end module m_depave
