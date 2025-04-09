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
module m_watage
   use m_waq_precision

   implicit none

contains

   subroutine watage(process_space_real, fl, ipoint, increm, num_cells, &
                     noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
                     num_exchanges_z_dir, num_exchanges_bottom_dir)
      use m_logger_helper

      !>\file
      !>       Age of water through the tracer substances

      !
      !     Description of the module :
      !
      ! Name    T   L I/O   Description                                    Units
      ! ----    --- -  -    -------------------                             ----
      ! AGE     R*4 1 O average age of the water
      ! CONCWA  R*4 1 I fraction of specific water ( conservative )
      ! CONCTR  R*4 1 I concentration tracer ( 1st order decay )
      ! DECAYR  R*4 1 I decay rate tracer
      ! FDECAY  R*4 1 O flux first order decay on tracer

      !     Logical Units : -

      !     Modules called : -

      !     Name     Type   Library
      !     ------   -----  ------------

      implicit none

      real(kind=real_wp) :: process_space_real(*), FL(*)
      integer(kind=int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                              IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir

      integer(kind=int_wp) :: IP1, IP2, IP3, IP4, IFLUX, ISEG
      real(kind=real_wp) :: CONCWA, CONCTR, DECAYR, ARGUM, AGE, FDECAY

      IP1 = IPOINT(1)
      IP2 = IPOINT(2)
      IP3 = IPOINT(3)
      IP4 = IPOINT(4)
      !
      IFLUX = 0
      do ISEG = 1, num_cells

         if (btest(IKNMRK(ISEG), 0)) then
            !
            CONCWA = process_space_real(IP1)
            CONCTR = process_space_real(IP2)
            DECAYR = process_space_real(IP3)
            !
            if (DECAYR < 1e-20) call write_error_message('RCDECTR in WATAGE zero')

            !     Calculate age
            !
            if (CONCWA <= 1.0e-20) then
               AGE = -999.
            elseif (CONCTR <= 1.0e-20) then
               AGE = -999.
            elseif (CONCTR > CONCWA) then
               AGE = -999.
            else
               ARGUM = CONCTR / CONCWA
               if (ARGUM < 1e-20) then
                  AGE = -999.
               elseif (abs(ARGUM - 1.0) > 1.0e-3) then
                  AGE = -log(ARGUM) / DECAYR
               else
                  AGE = -((ARGUM - 1.0) - (ARGUM - 1.0)**2 / 2.0) / DECAYR
               end if
            end if
            !
            !     Calculate decay
            !
            FDECAY = DECAYR * CONCTR
            !
            !     Output
            !
            process_space_real(IP4) = AGE
            FL(1 + IFLUX) = FDECAY
            !
         end if
         !
         IFLUX = IFLUX + NOFLUX
         IP1 = IP1 + INCREM(1)
         IP2 = IP2 + INCREM(2)
         IP3 = IP3 + INCREM(3)
         IP4 = IP4 + INCREM(4)
         !
      end do
      !
      return
   end

end module m_watage
