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
module m_grzmac
   use m_waq_precision

   implicit none

contains

   subroutine GRZMAC(process_space_real, FL, IPOINT, INCREM, num_cells, &
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
      integer(kind=int_wp) :: IPOINT(14) ! I  Array of pointers in process_space_real to get and store the data
      integer(kind=int_wp) :: INCREM(14) ! I  Increments in IPOINT for segment loop, 0=constant, 1=spatially varying
      integer(kind=int_wp) :: num_cells ! I  Number of computational elements in the whole model schematisation
      integer(kind=int_wp) :: NOFLUX ! I  Number of fluxes, increment in the FL array
      integer(kind=int_wp) :: IEXPNT ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      integer(kind=int_wp) :: IKNMRK(*) ! I  Active-Inactive, Surface-water-bottom, see manual for use
      integer(kind=int_wp) :: num_exchanges_u_dir ! I  Nr of exchanges in 1st direction, only horizontal dir if irregular mesh
      integer(kind=int_wp) :: num_exchanges_v_dir ! I  Nr of exchanges in 2nd direction, num_exchanges_u_dir+num_exchanges_v_dir gives hor. dir. reg. grid
      integer(kind=int_wp) :: num_exchanges_z_dir ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer(kind=int_wp) :: num_exchanges_bottom_dir ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
      integer(kind=int_wp) :: IPNT(14) !    Local work array for the pointering
      integer(kind=int_wp) :: ISEG !    Local loop counter for computational element loop
      !
      !*******************************************************************************
      !
      !     Type    Name         I/O Description                                        Unit
      !
      real(kind=real_wp) :: EM ! I  macrophyt emerged                                  (gC/m2)
      real(kind=real_wp) :: SM ! I  macrophyt submerged                                (gC/m2)
      real(kind=real_wp) :: RH ! I  macrophyt rhizome                                  (gC/m2)
      real(kind=real_wp) :: NRH ! I  nitrogen content macrophyt rhizome                 (gN/m2)
      real(kind=real_wp) :: PRH ! I  phosphorus content macrophyt rhizome               (gP/m2)
      real(kind=real_wp) :: K0GrzEM ! I  zeroth-order grazing flux macrophyte EM            (gC/m2/d)
      real(kind=real_wp) :: K1GrzEM ! I  first order grazing rate macrophyte EM             (1/d)
      real(kind=real_wp) :: K0GrzSM ! I  zeroth-order grazing flux macrophyte SM            (gC/m2/d)
      real(kind=real_wp) :: K1GrzSM ! I  first order grazing rate macrophyte SM             (1/d)
      real(kind=real_wp) :: K0GrzRH ! I  zeroth-order grazing flux macrophyte RH            (gC/m2/d)
      real(kind=real_wp) :: K1GrzRH ! I  first order grazing rate macrophyte RH             (1/d)
      real(kind=real_wp) :: Volume ! I  volume of computational cell                       (m3)
      real(kind=real_wp) :: Depth ! I  depth of segment                                   (m)
      real(kind=real_wp) :: DELT ! I  timestep for processes                             (d)
      real(kind=real_wp) :: dGrazeEM ! F  grazing flux macrophyte EM                         (gC/m3/d)
      real(kind=real_wp) :: dGrazeSM ! F  grazing flux macrophyte SM                         (gC/m3/d)
      real(kind=real_wp) :: dGrazeRH ! F  grazing flux macrophyte RH                         (gC/m3/d)
      real(kind=real_wp) :: dGrzNRH ! F  grazing flux macrophyte NRH                        (gC/m3/d)
      real(kind=real_wp) :: dGrzPRH ! F  grazing flux macrophyte PRH                        (gC/m3/d)
      integer(kind=int_wp) :: IdGrazeEM !    Pointer to the grazing flux macrophyte EM
      integer(kind=int_wp) :: IdGrazeSM !    Pointer to the grazing flux macrophyte SM
      integer(kind=int_wp) :: IdGrazeRH !    Pointer to the grazing flux macrophyte RH
      integer(kind=int_wp) :: IdGrzNRH !    Pointer to the grazing flux macrophyte NRH
      integer(kind=int_wp) :: IdGrzPRH !    Pointer to the grazing flux macrophyte PRH
      real(kind=real_wp) :: SURF ! L  surface area                                       (m2)
      integer(kind=int_wp) :: IKMRK1

      integer(kind=int_wp), save :: NR_MSG = 0
      integer(kind=int_wp) :: LUNREP
      !
      !*******************************************************************************
      !
      IPNT = IPOINT
      IdGrazeEM = 1
      IdGrazeSM = 2
      IdGrazeRH = 3
      IdGrzNRH = 4
      IdGrzPRH = 5
      !
      do ISEG = 1, num_cells

         call extract_waq_attribute(1, IKNMRK(ISEG), IKMRK1)
         if (IKMRK1 == 1) then

            !
            EM = process_space_real(IPNT(1))
            SM = process_space_real(IPNT(2))
            RH = process_space_real(IPNT(3))
            NRH = process_space_real(IPNT(4))
            PRH = process_space_real(IPNT(5))
            K0GrzEM = process_space_real(IPNT(6))
            K1GrzEM = process_space_real(IPNT(7))
            K0GrzSM = process_space_real(IPNT(8))
            K1GrzSM = process_space_real(IPNT(9))
            K0GrzRH = process_space_real(IPNT(10))
            K1GrzRH = process_space_real(IPNT(11))
            Volume = process_space_real(IPNT(12))
            Depth = process_space_real(IPNT(13))
            DELT = process_space_real(IPNT(14))
            !
            !   *****     Insert your code here  *****
            !
            ! check input

            if (DEPTH < 1e-20) then
               NR_MSG = NR_MSG + 1
               call get_log_unit_number(LUNREP)
               if (NR_MSG <= 25) then
                  write (LUNREP, *) 'GRZMAC: WARNING - depth zero or negative'
                  write (LUNREP, *) '   Segment:', ISEG
                  write (LUNREP, *) '   Depth:', DEPTH
                  if (NR_MSG == 25) then
                     write (LUNREP, *) 'GRZMAC: 25 warnings written - further warnings suppressed'
                  end if
               end if
            end if

            SURF = VOLUME / max(DEPTH, 1.0e-20)
            if (SURF < 1e-20) then
               NR_MSG = NR_MSG + 1
               call get_log_unit_number(LUNREP)
               if (NR_MSG <= 25) then
                  write (LUNREP, *) 'GRZMAC: WARNING - surface zero or negative'
                  write (LUNREP, *) '   Segment:', ISEG
                  write (LUNREP, *) '   Surface:', SURF
                  if (NR_MSG == 25) then
                     write (LUNREP, *) 'GRZMAC: 25 warnings written - further warnings suppressed'
                  end if
               end if
            end if

            ! graze on emerged macrophyte

            dGrazeEM = K0GrzEM + EM * K1GrzEM
            if (EM > dGrazeEM * DELT) then
               dGrazeEM = dGrazeEM / DEPTH
            else
               dGrazeEM = 0.0
            end if

            ! graze on submerged macrophyte

            dGrazeSM = K0GrzSM + SM * K1GrzSM
            if (SM > dGrazeSM * DELT) then
               dGrazeSM = dGrazeSM / DEPTH
            else
               dGrazeSM = 0.0
            end if

            ! graze on rhizome macrophyte

            dGrazeRH = K0GrzRH + RH * K1GrzRH
            if (RH > dGrazeRH * DELT) then
               dGrazeRH = dGrazeRH / DEPTH
            else
               dGrazeRH = 0.0
            end if

            ! the nitrogen content of rhizome

            if (RH > 1e-20) then
               dGrzNRH = dGrazeRH * NRH / RH
            else
               dGrzNRH = 0.0
            end if

            ! the phosphorus content of rhizome

            if (RH > 1e-20) then
               dGrzPRH = dGrazeRH * PRH / RH
            else
               dGrzPRH = 0.0
            end if

         end if

         !
         !   *****     End of your code       *****
         !
         FL(IdGrazeEM) = dGrazeEM
         FL(IdGrazeSM) = dGrazeSM
         FL(IdGrazeRH) = dGrazeRH
         FL(IdGrzNRH) = dGrzNRH
         FL(IdGrzPRH) = dGrzPRH
         !
         IdGrazeEM = IdGrazeEM + NOFLUX
         IdGrazeSM = IdGrazeSM + NOFLUX
         IdGrazeRH = IdGrazeRH + NOFLUX
         IdGrzNRH = IdGrzNRH + NOFLUX
         IdGrzPRH = IdGrzPRH + NOFLUX
         IPNT = IPNT + INCREM
         !
      end do
      !
      return
   end

end module m_grzmac
