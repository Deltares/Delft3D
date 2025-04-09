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
module m_macdis
   use m_waq_precision

   implicit none

contains

   subroutine MACDIS(process_space_real, FL, IPOINT, INCREM, num_cells, &
                     NOFLUX, IEXPNT, IKNMRK, num_exchanges_u_dir, num_exchanges_v_dir, &
                     num_exchanges_z_dir, num_exchanges_bottom_dir)
      use m_logger_helper, only: stop_with_error, get_log_unit_number
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
      integer(kind=int_wp) :: IEXPNT(4, *) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      integer(kind=int_wp) :: IKNMRK(*) ! I  Active-Inactive, Surface-water-bottom, see manual for use
      integer(kind=int_wp) :: num_exchanges_u_dir ! I  Nr of exchanges in 1st direction, only horizontal dir if irregular mesh
      integer(kind=int_wp) :: num_exchanges_v_dir ! I  Nr of exchanges in 2nd direction, num_exchanges_u_dir+num_exchanges_v_dir gives hor. dir. reg. grid
      integer(kind=int_wp) :: num_exchanges_z_dir ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer(kind=int_wp) :: num_exchanges_bottom_dir ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
      integer(kind=int_wp) :: IPNT(14) !    Local work array for the pointering
      integer(kind=int_wp) :: ISEG !    Local loop counter for computational element loop
      !*******************************************************************************
      !
      !     Type    Name         I/O Description                                        Unit
      !
      real(kind=real_wp) :: Surf ! I  surface of segment                            (m2)
      real(kind=real_wp) :: Depth ! I  depth of segment                               (m)
      real(kind=real_wp) :: TotalDepth ! I  total depth water column                       (m)
      real(kind=real_wp) :: LocalDepth ! I  depth from water surface to bottom of segment  (m)
      real(kind=real_wp) :: SM ! I  macrophyte submerged                       (gC/m2)
      real(kind=real_wp) :: smmax ! I  Maximum biomass macrophyte submerged       (gC/m2)
      real(kind=real_wp) :: SwDisSM ! I  macrophyt distr. function (1: lin, 2:Exp)      (-)
      real(kind=real_wp) :: Hmax ! I  Maxmimum Length Macrophytes                    (m)
      real(kind=real_wp) :: Ffac ! I  Form factor lin: F = M(mean)/(M/Hmax)          (-)
      real(kind=real_wp) :: BmLaySM ! O  Biomass Layer macrophyt submerged 01       (gC/m2)
      real(kind=real_wp) :: Hact ! O  Actual Length Macrophytes                      (m)
      real(kind=real_wp) :: Z2 !    Height Bottom Segment from Bottom              (m)
      real(kind=real_wp) :: Z1 !    Height Top Segment from Bottom                 (m)
      real(kind=real_wp) :: Z2a !    Height Bottom Segment from Bottom              (m)
      real(kind=real_wp) :: Z1a !    Height Top Segment from Bottom                 (m)
      real(kind=real_wp) :: Hactd !    Actual Length Macrophytes - relative to top    (-)
      real(kind=real_wp) :: Z2ad !    Height Bottom Segment from Bottom - relative   (-)
      real(kind=real_wp) :: Z1ad !    Height Top Segment from Bottom - relative      (-)
      real(kind=real_wp) :: absHmax !    Absolute maxmimum Length Macrophytes           (m)
      integer(kind=int_wp) :: IKMRK1
      integer IKMRK2
      real(kind=real_wp) :: FrBmLay !    Fraction BM per layer                          (-)
      real(kind=real_wp) :: Zm !    Watersurface to top Macropyte                  (-)
      real(kind=real_wp) :: A !    Lineair factor A (Ax + B)                      (-)
      real(kind=real_wp) :: B !    Lineair factor B (Ax + B)                      (-)
      real(kind=real_wp) :: OriginalDepth
      !          INTEGER IQ           !    Loop counter
      !          INTEGER Ifrom       !    From Segment
      !          INTEGER Ito         !    From Segment
      !          LOGICAL First

      integer(kind=int_wp) :: LUNREP

      integer IBotSeg ! Bottom Segment for Macrophyte
      !     INTEGER ITopSeg     ! Top    Segment for Macrophyte
      !*******************************************************************************

      IPNT = IPOINT
      !     Loop over segments
      do ISEG = 1, num_cells

         !        Check on active segments
         call extract_waq_attribute(1, IKNMRK(ISEG), IKMRK1)
         if (IKMRK1 == 1) then

            Surf = process_space_real(IPNT(1))
            Depth = process_space_real(IPNT(2))
            TotalDepth = process_space_real(IPNT(3))
            LocalDepth = process_space_real(IPNT(4))
            SwDisSM = process_space_real(IPNT(6))
            Hmax = process_space_real(IPNT(7))
            Ffac = process_space_real(IPNT(8))
            IBotSeg = nint(process_space_real(IPNT(9)))
            smmax = process_space_real(IPNT(10))

            ! get biomass from bottom segment

            !SM          = max( 1.0e-10, process_space_real(IPOINT(5)+(IBOTSEG-1)*INCREM(5)) )
            SM = process_space_real(IPOINT(5) + (IBOTSEG - 1) * INCREM(5))

            !           Limit the maximum height of the plants to the water depth
            absHmax = min(abs(Hmax), TotalDepth)

            ! actual height is fraction of maximum height

            if (smmax > 1e-20) then
               Hact = min(absHmax * SM / smmax, TotalDepth - 0.001)
               Hact = max(Hact, 0.01)
            else
               Hact = 0.01
            end if
            Hactd = 1.0 ! Represents the entire length of the plants

            !
            ! Hmax > 0: the macrophytes grow from the bottom upwards
            ! Hmax < 0: the macrophytes grow from the surface downwards
            !
            OriginalDepth = TotalDepth
            if (hmax < 0.0) then
               TotalDepth = Hact
            end if

            Zm = TotalDepth - Hact
            Z1 = LocalDepth - Depth
            Z2 = LocalDepth
            Z1a = TotalDepth - LocalDepth
            Z2a = TotalDepth - LocalDepth + Depth

            Z1ad = Z1a / Hact
            Z2ad = Z2a / Hact

            !           Switch = 1:  linear Biomass distribution
            if (SwDisSM == 1) then

               !              Check Ffac: 0,1 or 2

               if (Ffac < 0 .or. Ffac > 2) then
                  call get_log_unit_number(lunrep)
                  write (lunrep, *) 'MACDIS: Illegal option for Macrophyte form factor - should be between 0 and 2'
                  write (lunrep, *) '   Value now: ', ffac
                  write (lunrep, *) '   Input error (linear biomass distribution)'
                  write (lunrep, *) 'Input error in process MACDIS'
                  write (*, *) 'Input error in process MACDIS'
                  call stop_with_error()
               end if

               A = (SM / Hact) * (2 - (2 * Ffac)) / Hact
               B = (SM / Hact) * (Ffac * (Zm + TotalDepth) - 2 * Zm) / Hact

               !              Macrophyte is not in segment:
               if (Zm > Z2) then
                  BmLaySM = 0
                  !                 Macropyhte is completely in segment:
               elseif (Zm < Z1) then
                  BmLaySM = (A / 2) * (Z2**2 - Z1**2) + B * (Z2 - Z1)
                  !                 Macropyhte is partialy in segment: TIP !!!!
               else
                  BmlaySM = (A / 2) * (Z2**2 - Zm**2) + B * (Z2 - Zm)
                  !                 For the segment IBotSeg, current segment is ITopSeg!!!
                  process_space_real(IPOINT(14) + (IBotSeg - 1) * INCREM(14)) = ISEG
               end if

               !              Switch = 2:  Exponential Biomass distribution

            elseif (SwDisSM == 2) then

               if (Ffac <= 0 .or. Ffac > 50.0) then
                  call get_log_unit_number(lunrep)
                  write (lunrep, *) 'MACDIS: Incorrect value for Macrophyte form factor - ', &
                     'should be positive and lower than or equal to 50'
                  write (lunrep, *) '   Value now: ', ffac
                  write (lunrep, *) '   Input error (exponential biomass distribution)'
                  write (lunrep, *) 'Input error in process MACDIS'
                  write (*, *) 'Input error in process MACDIS'
                  call stop_with_error()
               end if

               A = SM / Hactd / ((exp(Ffac * Hactd) - 1.0) / Ffac - Hactd)
               !              Macrophyte is not in segment:
               if (Hact < Z1a) then
                  BmLaySM = 0
                  !              Macrophyte is completely in segment:
               elseif (Hact > Z2a) then
                  BmLaySM = A * ((exp(Ffac * Z2ad) - exp(Ffac * Z1ad)) / Ffac - (Z2ad - Z1ad))
                  !              Macrophyte is partially in segment: TIP !!!
               else
                  BmLaySM = A * ((exp(Ffac * Hactd) - exp(Ffac * Z1ad)) / Ffac - (Hactd - Z1ad))
                  !                 For the segment IBotSeg, current segment is ITopSeg!!!
                  process_space_real(IPOINT(14) + (IBotSeg - 1) * INCREM(14)) = ISEG
               end if

            end if

            !
            ! One complication: we need to set the top segment explicitly
            ! if we start at the top
            !
            if (Hmax < 0.0) then
               call extract_waq_attribute(2, IKNMRK(ISEG), IKMRK2)
               if (IKMRK2 == 0 .or. IKMRK2 == 1) then
                  process_space_real(IPOINT(14) + (IBotSeg - 1) * INCREM(14)) = ISEG
               end if
            end if

            if (SM > 0) then
               FrBmLay = BmLaySm / SM
            else
               if (iseg == IBotseg) then
                  FrBmLay = 1.0
               else
                  FrBmLay = 0.0
               end if
            end if

            !           Return Outputparameters to delwaq

            process_space_real(IPNT(11)) = FrBmLay
            process_space_real(IPNT(12)) = BmLaySM / Depth
            if (Hmax > 0.0) then
               process_space_real(IPNT(13)) = Hact
            else
               process_space_real(IPNT(13)) = OriginalDepth
            end if

         end if

         IPNT = IPNT + INCREM

      end do
      !
      return
   end

end module m_macdis
