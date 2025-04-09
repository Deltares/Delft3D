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
module m_tfalg
   use m_waq_precision

   implicit none

contains

   subroutine tfalg(process_space_real, fl, ipoint, increm, num_cells, &
                    noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
                    num_exchanges_z_dir, num_exchanges_bottom_dir)
      !>\file
      !>       Temperature functions for algae growth and mortality

      !
      !     Description of the module :
      !
      ! Name    T   L I/O   Description                                   Unit
      ! ----    --- -  -    -------------------                            ---
      ! TEMP    R*4 1 I ambient temperature                                 [x
      ! TEMP20  R*4 1 L ambient temperature - stand. temp (20)              [x
      ! TCG1    R*4 1 I temp. coeff. for growth processes diatoms            [
      ! TCM1    R*4 1 I temp. coeff. for mortality processes green s         [
      ! TFUNG1  R*4 1 L temp. function for growth processes green            [
      ! TFUNM1  R*4 1 L temp. function for mortality processes green         [

      !     Logical Units : -

      !     Modules called : -

      !     Name     Type   Library
      !     ------   -----  ------------

      implicit real(A - H, J - Z)
      implicit integer(I)

      real(kind=real_wp) :: process_space_real(*), FL(*)
      integer(kind=int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                              IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir

      logical TMPOPT
      !
      IN1 = INCREM(1)
      IN2 = INCREM(2)
      IN3 = INCREM(3)
      IN4 = INCREM(4)
      IN5 = INCREM(5)
      !
      IP1 = IPOINT(1)
      IP2 = IPOINT(2)
      IP3 = IPOINT(3)
      IP4 = IPOINT(4)
      IP5 = IPOINT(5)
      !
      if (IN1 == 0 .and. IN2 == 0 .and. IN3 == 0) then
         TEMP = process_space_real(IP1)
         TCG = process_space_real(IP2)
         TCM = process_space_real(IP3)
         TEMP20 = TEMP - 20.
         TFG = TCG**TEMP20
         TFM = TCM**TEMP20
         TMPOPT = .false.
      else
         TMPOPT = .true.
      end if
      !
      do ISEG = 1, num_cells

         if (btest(IKNMRK(ISEG), 0)) then
            !
            if (TMPOPT) then
               TEMP = process_space_real(IP1)
               TCG = process_space_real(IP2)
               TCM = process_space_real(IP3)
               TEMP20 = TEMP - 20.
               !     Algal temp. functions for growth (G) and mortality (M) processes
               TFG = TCG**TEMP20
               TFM = TCM**TEMP20
            end if

            !     Uitvoer limiterende factoren
            process_space_real(IP4) = TFG
            process_space_real(IP5) = TFM
            !
         end if
         !
         IP1 = IP1 + IN1
         IP2 = IP2 + IN2
         IP3 = IP3 + IN3
         IP4 = IP4 + IN4
         IP5 = IP5 + IN5
         !
      end do
      !
      return
   end

end module m_tfalg
