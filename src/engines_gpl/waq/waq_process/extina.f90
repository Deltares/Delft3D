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
module m_extina
   use m_waq_precision

   implicit none

contains

   subroutine extina(process_space_real, fl, ipoint, increm, num_cells, &
                     noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
                     num_exchanges_z_dir, num_exchanges_bottom_dir)
      !>\file
      !>       Extinction of light by algae and POC

      real(kind=real_wp) :: process_space_real(*), FL(*)
      integer(kind=int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                              IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir
      !
      !     Local declarations
      !
      integer(kind=int_wp) :: NALG, ISWFIX, NIPALG, IFLUX, ISEG, &
                              IALG, IP, IFIX
      real(kind=dp) :: EXTALG, EXTCF, BIOMAS, DEPTH, SDMIX
      !
      NALG = nint(process_space_real(IPOINT(1)))
      ISWFIX = nint(process_space_real(IPOINT(2)))
      if (ISWFIX == 1) then
         NIPALG = 4
      else
         NIPALG = 2
      end if
      IFLUX = 0

      do ISEG = 1, num_cells

         if (btest(IKNMRK(ISEG), 0)) then

            EXTALG = 0.0
            DEPTH = process_space_real(IPOINT(3) + (ISEG - 1) * INCREM(3))
            !
            !     Loop over algae

            do IALG = 1, NALG

               IP = 3 + IALG
               EXTCF = process_space_real(IPOINT(IP) + (ISEG - 1) * INCREM(IP))

               IP = 3 + NALG + IALG
               BIOMAS = process_space_real(IPOINT(IP) + (ISEG - 1) * INCREM(IP))

               if (ISWFIX == 1) then
                  IP = 3 + 2 * NALG + IALG
                  IFIX = nint(process_space_real(IPOINT(IP) + (ISEG - 1) * INCREM(IP)))
                  if (IFIX < 0) then

                     ! Rooted algae, inlclude only if sdmix positive

                     IP = 3 + 3 * NALG + IALG
                     SDMIX = process_space_real(IPOINT(IP) + (ISEG - 1) * INCREM(IP))
                     if (SDMIX > 1e-10) then
                        BIOMAS = BIOMAS / DEPTH
                     else
                        BIOMAS = 0.0
                     end if
                  end if
               end if

               if (BIOMAS > 0.0) &
                  EXTALG = EXTALG + BIOMAS * EXTCF

            end do

            IP = 3 + NIPALG * NALG + 1
            process_space_real(IPOINT(IP) + (ISEG - 1) * INCREM(IP)) = EXTALG

         end if
         !
         IFLUX = IFLUX + NOFLUX
         !
      end do
      !
      return

   end

end module m_extina
