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
module m_radmac
   use m_waq_precision

   implicit none

contains

   subroutine RADMAC(process_space_real, FL, IPOINT, INCREM, num_cells, &
                     NOFLUX, IEXPNT, IKNMRK, num_exchanges_u_dir, num_exchanges_v_dir, &
                     num_exchanges_z_dir, num_exchanges_bottom_dir)
      use m_logger_helper
      use m_extract_waq_attribute

      !
      !*******************************************************************************
      !
      implicit none
      !
      !     Type    Name         I/O Description
      !
      real(kind=real_wp) :: process_space_real(*) !I/O Process Manager System Array, window of routine to process library
      real(kind=real_wp) :: FL(*) ! O  Array of fluxes made by this process in mass/volume/time
      integer(kind=int_wp) :: IPOINT(9) ! I  Array of pointers in process_space_real to get and store the data
      integer(kind=int_wp) :: INCREM(9) ! I  Increments in IPOINT for segment loop, 0=constant, 1=spatially varying
      integer(kind=int_wp) :: num_cells ! I  Number of computational elements in the whole model schematisation
      integer(kind=int_wp) :: NOFLUX ! I  Number of fluxes, increment in the FL array
      integer(kind=int_wp) :: IEXPNT ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      integer(kind=int_wp) :: IKNMRK(*) ! I  Active-Inactive, Surface-water-bottom, see manual for use
      integer(kind=int_wp) :: num_exchanges_u_dir ! I  Nr of exchanges in 1st direction, only horizontal dir if irregular mesh
      integer(kind=int_wp) :: num_exchanges_v_dir ! I  Nr of exchanges in 2nd direction, num_exchanges_u_dir+num_exchanges_v_dir gives hor. dir. reg. grid
      integer(kind=int_wp) :: num_exchanges_z_dir ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer(kind=int_wp) :: num_exchanges_bottom_dir ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
      integer(kind=int_wp) :: IPNT(9) !    Local work array for the pointering
      integer(kind=int_wp) :: ISEG !    Local loop counter for computational element loop
      !
      !*******************************************************************************
      !
      !     Type    Name         I/O Description                                        Unit
      !
      real(kind=real_wp) :: ACTRAD, SATRAD, FRAD, RADTOP, HACT, TOTDEP, LOCDEP, DEPTH, &
                            EXT, ZM, Z1, DZ
      integer(kind=int_wp) :: IKMRK1, IKMRK2, ITOPSEG

      integer(kind=int_wp) :: LUNREP
      integer(kind=int_wp), save :: NR_MSG = 0

      !*******************************************************************************
      !
      IPNT = IPOINT
      !
      do ISEG = 1, num_cells

         call extract_waq_attribute(1, IKNMRK(ISEG), IKMRK1)
         if (IKMRK1 == 1) then
            call extract_waq_attribute(2, IKNMRK(ISEG), IKMRK2)
            if ((IKMRK2 == 0) .or. (IKMRK2 == 3)) then

               !         Access conditions from the cell where the top of the plant is
               ITOPSEG = nint(process_space_real(IPNT(1)))
               if (ITOPSEG <= 0) then
                  call get_log_unit_number(LUNREP)
                  write (LUNREP, *) 'RADMAC: top segment missing - needed for light intensity at tip of plant'
                  write (LUNREP, *) '   ISEG    =', ISEG
                  write (LUNREP, *) '   ITOPSEG =', ITOPSEG
                  stop 'Problem in RADMAC'
               end if

               !         Calculate light intensity at the tip of the plant
               RADTOP = process_space_real(IPOINT(2) + (ITOPSEG - 1) * INCREM(2))
               HACT = process_space_real(IPNT(3))
               TOTDEP = process_space_real(IPNT(4))
               LOCDEP = process_space_real(IPOINT(5) + (ITOPSEG - 1) * INCREM(5))
               DEPTH = process_space_real(IPOINT(6) + (ITOPSEG - 1) * INCREM(6))
               EXT = process_space_real(IPOINT(7) + (ITOPSEG - 1) * INCREM(7))
               ZM = TOTDEP - HACT
               Z1 = LOCDEP - DEPTH
               DZ = ZM - Z1

               if (DZ < 0.0 .or. DZ > DEPTH) then
                  NR_MSG = NR_MSG + 1
                  if (NR_MSG <= 25) then
                     call get_log_unit_number(LUNREP)
                     write (LUNREP, *) 'RADMAC: depth out of range'
                     write (LUNREP, *) '   ISEG  =', ISEG
                     write (LUNREP, *) '   ITOPS =', ITOPSEG
                     write (LUNREP, *) '   HACT  =', HACT
                     write (LUNREP, *) '   TOTDEP=', TOTDEP
                     write (LUNREP, *) '   LOCDEP=', LOCDEP
                     write (LUNREP, *) '   DEPTH =', DEPTH
                     write (LUNREP, *) '   ZM=', ZM
                     write (LUNREP, *) '   Z1=', Z1
                     write (LUNREP, *) '   DZ=', DZ
                     if (NR_MSG == 25) then
                        write (LUNREP, *) 'RADMAC: 25 messages written'
                        write (LUNREP, *) 'RADMAC: further messages will be suppressed'
                     end if
                  end if
               end if
               ACTRAD = RADTOP * exp(-EXT * DZ)

               !         Calculate and store light efficiency
               SATRAD = process_space_real(IPNT(8))

               FRAD = min(1.0, ACTRAD / SATRAD)
               process_space_real(IPNT(9)) = FRAD
               !
            end if
         end if
         !
         IPNT = IPNT + INCREM
         !
      end do
      !
      return
      !
   end

end module m_radmac
