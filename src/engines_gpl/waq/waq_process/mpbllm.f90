!!  Copyright(C) Stichting Deltares, 2012-2024.
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
module m_mpbllm
   use m_waq_precision

   implicit none

contains

   subroutine MPBLLM(process_space_real, FL, IPOINT, INCREM, num_cells, &
                     NOFLUX, IEXPNT, IKNMRK, num_exchanges_u_dir, num_exchanges_v_dir, &
                     num_exchanges_z_dir, num_exchanges_bottom_dir)
      !     ***********************************************************************
      !          +----------------------------------------+
      !          |    D E L F T   H Y D R A U L I C S     |
      !          +----------------------------------------+
      !     ***********************************************************************
      !
      !          Function     : Calculation of the light limitation function
      !
      !          Project      : Implementatie pilot GEM (T2087)
      !          Formulations : NIOO-CEMO Yerseke
      !          Programmer   : M. Bokhorst
      !          Date         : 09-04-97           Version : 1.0
      !
      !          History :
      !
      !          Date    Programmer      Description
      !          ------  --------------  ------------------------------------------
      !          090497  M. Bokhorst     First version
      !          040399  A. Blauw        Formulation completed (see TRM)
      !          050399  J. vGils        Optional S1 mode implemented
      !                                  Explicit declaration
      !          110399  J. vGils        Error in ICLIM computation corrected
      !                                  (note: GEM documentation is not correct!!)
      !          110399  J. vGils        C-limitation removed from loop over Z
      !          110399  J. vGils        Error in time integration removed
      !          111103  Jan van Beek    2003 implementation
      !     ***********************************************************************

      use m_extract_waq_attribute
      use m_logger_helper, only: get_log_unit_number, write_error_message_with_values

      implicit none

      !          arguments

      real(kind=real_wp) :: process_space_real(*) ! in/out input-output array space to be adressed with IPOINT/INCREM
      real(kind=real_wp) :: FL(*) ! in/out flux array
      integer(kind=int_wp) :: IPOINT(*) ! in     start index input-output parameters in the process_space_real array (segment or exchange number 1)
      integer(kind=int_wp) :: INCREM(*) ! in     increment for each segment-exchange for the input-output parameters in the process_space_real array
      integer(kind=int_wp) :: num_cells ! in     number of segments
      integer(kind=int_wp) :: NOFLUX ! in     total number of fluxes (increment in FL array)
      integer(kind=int_wp) :: IEXPNT(4, *) ! in     exchange pointer table
      integer(kind=int_wp) :: IKNMRK(*) ! in     segment features array
      integer(kind=int_wp) :: num_exchanges_u_dir ! in     number of exchanges in first direction
      integer(kind=int_wp) :: num_exchanges_v_dir ! in     number of exchanges in second direction
      integer(kind=int_wp) :: num_exchanges_z_dir ! in     number of exchanges in third direction
      integer(kind=int_wp) :: num_exchanges_bottom_dir ! in     number of exchanges in fourth direction

      !          from process_space_real array

      real(kind=real_wp) :: RADSURF !  1 in  , irradiation at the water surface            (W/m2)
      real(kind=real_wp) :: RADTOP !  2 in  , irradiation at the segment upper-boundary   (W/m2)
      real(kind=real_wp) :: EXTVL !  3 in  , VL extinction coefficient                    (1/m)
      real(kind=real_wp) :: A_ENH !  4 in  , enhancement factor in radiation calculation    (-)
      real(kind=real_wp) :: FPAR !  5 in  , fraction Photosynthetic Active Radiance        (-)
      real(kind=real_wp) :: PM !  6 in  , MPB maximum photosynthesis           (gC/(gChl)/d)
      real(kind=real_wp) :: RADSAT !  7 in  , MPB saturation radiation                    (W/m2)
      integer(kind=int_wp) :: SWEMERSION !  8 in  , switch indicating submersion(0) or emersion(1) (-)
      real(kind=real_wp) :: MIGRDEPTH1 !  9 in  , MPB migration depth 1                          (m)
      real(kind=real_wp) :: MIGRDEPTH2 ! 10 in  , MPB migration depth 2                          (m)
      real(kind=real_wp) :: DEPTH ! 11 in  , depth of segment                               (m)
      real(kind=real_wp) :: LOCSEDDEPT ! 12 in  , Sediment layer depth to bottom of segment      (m)
      integer(kind=int_wp) :: I_NRDZ ! 13 in  , Nr. of integration intervals over depth        (-)
      integer(kind=int_wp) :: ITIME ! 14 in  , DELWAQ time                                  (scu)
      integer(kind=int_wp) :: IDT ! 15 in  , DELWAQ timestep                              (scu)
      integer(kind=int_wp) :: ITSTRT ! 16 in  , DELWAQ start time                            (scu)
      integer(kind=int_wp) :: AUXSYS ! 17 in  , ratio between days and system clock        (scu/d)
      logical :: S1_BOTTOM ! 18 in  , switch for S1 bottom approach (.true.) or DELWAQ-G approach (.false.)
      real(kind=real_wp) :: RADBOT ! 19 in  , irradiation at the segment lower-boundary   (W/m2)
      real(kind=real_wp) :: EXTVLS1 ! 20 in  , VL extinction coefficient in the sediment    (1/m)
      real(kind=real_wp) :: ZSED ! 21 in  , Depth of microfytobenthos layer                (m)
      real(kind=real_wp) :: WS1 ! 22 i/o , Workspace array 1                              (-)
      real(kind=real_wp) :: WS2 ! 23 i/o , Workspace array 2                              (-)
      real(kind=real_wp) :: WS3 ! 24 i/o , Workspace array 3                              (-)
      real(kind=real_wp) :: WS4 ! 25 i/o , Workspace array 4                              (-)
      real(kind=real_wp) :: FLT ! 26 out , MPB light limitation                           (-)
      real(kind=real_wp) :: FLTS1 ! 27 out , MPB light limitation in sediment layer 1       (-)

      !          local

      real(kind=real_wp), parameter :: PI = 3.1415927 ! pi
      integer(kind=int_wp) :: ISEG ! loop counter segment loop
      integer(kind=int_wp) :: IZ ! loop counter integration layers
      integer(kind=int_wp) :: IKMRK1 ! first feature inactive(0)-active(1)-bottom(2) segment
      integer(kind=int_wp) :: IKMRK2 ! second feature 2D(0)-surface(1)-middle(2)-bottom(3) segment
      integer(kind=int_wp), parameter :: NO_POINTER = 30 ! number of input output variables in process_space_real array
      integer(kind=int_wp) :: IP(NO_POINTER) ! index pointer in process_space_real array updated for each segment
      real(kind=real_wp) :: ACTDEP ! actual depth
      real(kind=real_wp) :: ACTLIM ! limitation at actual radiance
      real(kind=real_wp) :: ACTRAD ! radiance at actual depth
      real(kind=real_wp) :: CUMLIM ! cummulative limitation
      real(kind=real_wp) :: DZ ! depth of integration layers
      real(kind=real_wp) :: FRACSURF ! fraction of migrating MPB to reach surface
      real(kind=real_wp) :: LIMSURF ! limitation with RADSURF
      real(kind=real_wp) :: RELZ ! relative Z in migration dpeth
      real(kind=real_wp) :: Z ! Z in total sediment layer

      integer(kind=int_wp) :: ISTEP
      real(kind=real_wp) :: RTIME
      real(kind=real_wp) :: RDT
      real(kind=real_wp) :: RTSTRT

      !          initialise pointers for process_space_real and FL array

      IP = IPOINT(1:NO_POINTER)

      !          loop over the segments

      do ISEG = 1, num_cells

         call extract_waq_attribute(1, IKNMRK(ISEG), IKMRK1)
         call extract_waq_attribute(2, IKNMRK(ISEG), IKMRK2)

         RADSURF = process_space_real(IP(1))
         RADTOP = process_space_real(IP(2))
         EXTVL = process_space_real(IP(3))
         A_ENH = process_space_real(IP(4))
         FPAR = process_space_real(IP(5))
         RADSAT = process_space_real(IP(6))
         SWEMERSION = nint(process_space_real(IP(7)))
         MIGRDEPTH1 = process_space_real(IP(8))
         MIGRDEPTH2 = process_space_real(IP(9))
         DEPTH = process_space_real(IP(10))
         LOCSEDDEPT = process_space_real(IP(11))
         I_NRDZ = nint(process_space_real(IP(12)))
         RTIME = process_space_real(IP(13))
         RDT = process_space_real(IP(14))
         RTSTRT = process_space_real(IP(15))
         AUXSYS = nint(process_space_real(IP(16)))
         S1_BOTTOM = nint(process_space_real(IP(17))) == 1
         RADBOT = process_space_real(IP(18))
         EXTVLS1 = process_space_real(IP(19))
         ZSED = process_space_real(IP(20))
         WS1 = process_space_real(IP(21))
         WS2 = process_space_real(IP(22))
         WS3 = process_space_real(IP(23))
         WS4 = process_space_real(IP(24))

         ISTEP = nint((RTIME - RTSTRT) / RDT)
         IDT = nint(RDT)
         ITSTRT = nint(RTSTRT)
         ITIME = ITSTRT + ISTEP * IDT

         !             check proces parameters

         if (I_NRDZ <= 0) call write_error_message_with_values('I_NRDZ', real(I_NRDZ), ISEG, 'MPBLLM')

         !             scale all radiance to PAR, radsurf with enhancement since it is used as top of sediment layer radiation

         RADSURF = RADSURF * FPAR * A_ENH
         RADTOP = RADTOP * FPAR
         RADBOT = RADBOT * FPAR

         !             Active water segments and bottom segments

         !              IF ( IKMRK1.EQ.1 .OR. IKMRK1.EQ.2 ) THEN

         !                for top layer thicker then euphotic depth all production in euphotic zone, so intergate only over ZSED

         if (IKMRK1 == 2 .and. abs(DEPTH - LOCSEDDEPT) < 1.e-20 .and. DEPTH > ZSED) then
            DZ = ZSED / I_NRDZ
         else
            DZ = DEPTH / I_NRDZ
         end if

         CUMLIM = 0.0

         !                Bereken totale lichthoeveelheid en lichtlimitatie per laagje

         LIMSURF = 1.0 - exp(-RADSURF / RADSAT)
         do IZ = 1, I_NRDZ
            if (IZ == 1) then
               ACTDEP = 0.5 * DZ
            else
               ACTDEP = ACTDEP + DZ
            end if

            !                   bereken de fractie algen die naar het sediment oppervlak zijn gemigreerd

            if (IKMRK1 == 2 .and. SWEMERSION == 1) then
               if (MIGRDEPTH2 <= 1e-20) then
                  FRACSURF = 0.0
               else
                  Z = LOCSEDDEPT - DEPTH + ACTDEP
                  RELZ = min(1.0, (max(0.0, (Z - MIGRDEPTH1) / (MIGRDEPTH2 - MIGRDEPTH1))))
                  FRACSURF = 0.5 * cos(PI * RELZ) + 0.5
               end if
            else
               FRACSURF = 0.0
            end if

            ACTRAD = RADTOP * exp(-EXTVL * ACTDEP)
            ACTLIM = 1.0 - exp(-ACTRAD / RADSAT)

            CUMLIM = CUMLIM + FRACSURF * LIMSURF + (1.0 - FRACSURF) * ACTLIM

         end do

         !                gemiddelde lichtlimitatie

         CUMLIM = CUMLIM / I_NRDZ

         !                Integratie over de dag

         if (mod(ITIME - ITSTRT, AUXSYS) < IDT) then
            if (ITIME == ITSTRT) then
               WS2 = CUMLIM
            else
               WS2 = WS1 / AUXSYS
            end if
            WS1 = 0.0
            process_space_real(IP(23)) = WS2
         end if

         FLT = WS2
         WS1 = WS1 + CUMLIM * IDT

         process_space_real(IP(21)) = WS1
         process_space_real(IP(25)) = FLT

         !              ENDIF

         !             S1_BOTTOM

         if (S1_BOTTOM .and. (IKMRK2 == 0 .or. IKMRK2 == 3)) then

            DZ = ZSED / I_NRDZ
            CUMLIM = 0.0

            !                Bereken totale lichthoeveelheid en lichtlimitatie per laagje

            LIMSURF = 1.0 - exp(-RADBOT / RADSAT)
            do IZ = 1, I_NRDZ
               if (IZ == 1) then
                  ACTDEP = 0.5 * DZ
               else
                  ACTDEP = ACTDEP + DZ
               end if

               !                   bereken de fractie algen die naar het sediment oppervlak zijn gemigreerd

               if (SWEMERSION == 1) then
                  if (MIGRDEPTH2 <= 1e-20) then
                     FRACSURF = 0.0
                  else
                     RELZ = min(1.0, (max(0.0, (ACTDEP - MIGRDEPTH1) / (MIGRDEPTH2 - MIGRDEPTH1))))
                     FRACSURF = 0.5 * cos(PI * RELZ) + 0.5
                  end if
               else
                  FRACSURF = 0.0
               end if

               ACTRAD = RADBOT * exp(-EXTVLS1 * ACTDEP)
               ACTLIM = 1.0 - exp(-ACTRAD / RADSAT)

               CUMLIM = CUMLIM + FRACSURF * LIMSURF + (1.0 - FRACSURF) * ACTLIM
            end do

            !                gemiddelde lichtlimitatie

            CUMLIM = CUMLIM / I_NRDZ

            !                Integratie over de dag

            if (mod(ITIME - ITSTRT, AUXSYS) < IDT) then
               if (ITIME == ITSTRT) then
                  WS4 = CUMLIM
               else
                  WS4 = WS3 / AUXSYS
               end if
               WS3 = 0.0
               process_space_real(IP(24)) = WS4
            end if

            FLTS1 = WS4
            WS3 = WS3 + CUMLIM * IDT

            process_space_real(IP(23)) = WS3
            process_space_real(IP(26)) = FLTS1

         end if

         !             update pointering in process_space_real array

         IP = IP + INCREM(1:NO_POINTER)

      end do

      return
   end

end module m_mpbllm
