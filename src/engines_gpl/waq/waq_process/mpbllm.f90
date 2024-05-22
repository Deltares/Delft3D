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


    SUBROUTINE MPBLLM (PMSA, FL, IPOINT, INCREM, NOSEG, &
            NOFLUX, IEXPNT, IKNMRK, NOQ1, NOQ2, &
            NOQ3, NOQ4)
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

        use m_evaluate_waq_attribute
        use m_logger_helper, only : get_log_unit_number, write_error_message_with_values

        IMPLICIT NONE

        !          arguments

        REAL(kind = real_wp) :: PMSA(*)            ! in/out input-output array space to be adressed with IPOINT/INCREM
        REAL(kind = real_wp) :: FL(*)              ! in/out flux array
        INTEGER(kind = int_wp) :: IPOINT(*)          ! in     start index input-output parameters in the PMSA array (segment or exchange number 1)
        INTEGER(kind = int_wp) :: INCREM(*)          ! in     increment for each segment-exchange for the input-output parameters in the PMSA array
        INTEGER(kind = int_wp) :: NOSEG              ! in     number of segments
        INTEGER(kind = int_wp) :: NOFLUX             ! in     total number of fluxes (increment in FL array)
        INTEGER(kind = int_wp) :: IEXPNT(4, *)        ! in     exchange pointer table
        INTEGER(kind = int_wp) :: IKNMRK(*)          ! in     segment features array
        INTEGER(kind = int_wp) :: NOQ1               ! in     number of exchanges in first direction
        INTEGER(kind = int_wp) :: NOQ2               ! in     number of exchanges in second direction
        INTEGER(kind = int_wp) :: NOQ3               ! in     number of exchanges in third direction
        INTEGER(kind = int_wp) :: NOQ4               ! in     number of exchanges in fourth direction

        !          from PMSA array

        REAL(kind = real_wp) :: RADSURF            !  1 in  , irradiation at the water surface            (W/m2)
        REAL(kind = real_wp) :: RADTOP             !  2 in  , irradiation at the segment upper-boundary   (W/m2)
        REAL(kind = real_wp) :: EXTVL              !  3 in  , VL extinction coefficient                    (1/m)
        REAL(kind = real_wp) :: A_ENH              !  4 in  , enhancement factor in radiation calculation    (-)
        REAL(kind = real_wp) :: FPAR               !  5 in  , fraction Photosynthetic Active Radiance        (-)
        REAL(kind = real_wp) :: PM                 !  6 in  , MPB maximum photosynthesis           (gC/(gChl)/d)
        REAL(kind = real_wp) :: RADSAT             !  7 in  , MPB saturation radiation                    (W/m2)
        INTEGER(kind = int_wp) :: SWEMERSION         !  8 in  , switch indicating submersion(0) or emersion(1) (-)
        REAL(kind = real_wp) :: MIGRDEPTH1         !  9 in  , MPB migration depth 1                          (m)
        REAL(kind = real_wp) :: MIGRDEPTH2         ! 10 in  , MPB migration depth 2                          (m)
        REAL(kind = real_wp) :: DEPTH              ! 11 in  , depth of segment                               (m)
        REAL(kind = real_wp) :: LOCSEDDEPT         ! 12 in  , Sediment layer depth to bottom of segment      (m)
        INTEGER(kind = int_wp) :: I_NRDZ             ! 13 in  , Nr. of integration intervals over depth        (-)
        INTEGER(kind = int_wp) :: ITIME              ! 14 in  , DELWAQ time                                  (scu)
        INTEGER(kind = int_wp) :: IDT                ! 15 in  , DELWAQ timestep                              (scu)
        INTEGER(kind = int_wp) :: ITSTRT             ! 16 in  , DELWAQ start time                            (scu)
        INTEGER(kind = int_wp) :: AUXSYS             ! 17 in  , ratio between days and system clock        (scu/d)
        LOGICAL :: S1_BOTTOM          ! 18 in  , switch for S1 bottom approach (.true.) or DELWAQ-G approach (.false.)
        REAL(kind = real_wp) :: RADBOT             ! 19 in  , irradiation at the segment lower-boundary   (W/m2)
        REAL(kind = real_wp) :: EXTVLS1            ! 20 in  , VL extinction coefficient in the sediment    (1/m)
        REAL(kind = real_wp) :: ZSED               ! 21 in  , Depth of microfytobenthos layer                (m)
        REAL(kind = real_wp) :: WS1                ! 22 i/o , Workspace array 1                              (-)
        REAL(kind = real_wp) :: WS2                ! 23 i/o , Workspace array 2                              (-)
        REAL(kind = real_wp) :: WS3                ! 24 i/o , Workspace array 3                              (-)
        REAL(kind = real_wp) :: WS4                ! 25 i/o , Workspace array 4                              (-)
        REAL(kind = real_wp) :: FLT                ! 26 out , MPB light limitation                           (-)
        REAL(kind = real_wp) :: FLTS1              ! 27 out , MPB light limitation in sediment layer 1       (-)

        !          local

        REAL(kind = real_wp), PARAMETER :: PI = 3.1415927 ! pi
        INTEGER(kind = int_wp) :: ISEG               ! loop counter segment loop
        INTEGER(kind = int_wp) :: IZ                 ! loop counter integration layers
        INTEGER(kind = int_wp) :: IKMRK1             ! first feature inactive(0)-active(1)-bottom(2) segment
        INTEGER(kind = int_wp) :: IKMRK2             ! second feature 2D(0)-surface(1)-middle(2)-bottom(3) segment
        INTEGER(kind = int_wp), parameter :: NO_POINTER = 30    ! number of input output variables in PMSA array
        INTEGER(kind = int_wp) :: IP(NO_POINTER)     ! index pointer in PMSA array updated for each segment
        REAL(kind = real_wp) :: ACTDEP             ! actual depth
        REAL(kind = real_wp) :: ACTLIM             ! limitation at actual radiance
        REAL(kind = real_wp) :: ACTRAD             ! radiance at actual depth
        REAL(kind = real_wp) :: CUMLIM             ! cummulative limitation
        REAL(kind = real_wp) :: DZ                 ! depth of integration layers
        REAL(kind = real_wp) :: FRACSURF           ! fraction of migrating MPB to reach surface
        REAL(kind = real_wp) :: LIMSURF            ! limitation with RADSURF
        REAL(kind = real_wp) :: RELZ               ! relative Z in migration dpeth
        REAL(kind = real_wp) :: Z                  ! Z in total sediment layer

        INTEGER(kind = int_wp) :: ISTEP
        REAL(kind = real_wp) :: RTIME
        REAL(kind = real_wp) :: RDT
        REAL(kind = real_wp) :: RTSTRT

        !          initialise pointers for PMSA and FL array

        IP = IPOINT(1:NO_POINTER)

        !          loop over the segments

        DO ISEG = 1, NOSEG

            CALL extract_waq_attribute(1, IKNMRK(ISEG), IKMRK1)
            CALL extract_waq_attribute(2, IKNMRK(ISEG), IKMRK2)

            RADSURF = PMSA(IP(1))
            RADTOP = PMSA(IP(2))
            EXTVL = PMSA(IP(3))
            A_ENH = PMSA(IP(4))
            FPAR = PMSA(IP(5))
            RADSAT = PMSA(IP(6))
            SWEMERSION = NINT(PMSA(IP(7)))
            MIGRDEPTH1 = PMSA(IP(8))
            MIGRDEPTH2 = PMSA(IP(9))
            DEPTH = PMSA(IP(10))
            LOCSEDDEPT = PMSA(IP(11))
            I_NRDZ = NINT(PMSA(IP(12)))
            RTIME = PMSA(IP(13))
            RDT = PMSA(IP(14))
            RTSTRT = PMSA(IP(15))
            AUXSYS = NINT(PMSA(IP(16)))
            S1_BOTTOM = NINT(PMSA(IP(17))) == 1
            RADBOT = PMSA(IP(18))
            EXTVLS1 = PMSA(IP(19))
            ZSED = PMSA(IP(20))
            WS1 = PMSA(IP(21))
            WS2 = PMSA(IP(22))
            WS3 = PMSA(IP(23))
            WS4 = PMSA(IP(24))

            ISTEP = NINT((RTIME - RTSTRT) / RDT)
            IDT = NINT(RDT)
            ITSTRT = NINT(RTSTRT)
            ITIME = ITSTRT + ISTEP * IDT

            !             check proces parameters

            IF (I_NRDZ<=0) CALL write_error_message_with_values('I_NRDZ', real(I_NRDZ), ISEG, 'MPBLLM')

            !             scale all radiance to PAR, radsurf with enhancement since it is used as top of sediment layer radiation

            RADSURF = RADSURF * FPAR * A_ENH
            RADTOP = RADTOP * FPAR
            RADBOT = RADBOT * FPAR

            !             Active water segments and bottom segments

            !              IF ( IKMRK1.EQ.1 .OR. IKMRK1.EQ.2 ) THEN

            !                for top layer thicker then euphotic depth all production in euphotic zone, so intergate only over ZSED

            IF (IKMRK1 == 2 .AND. ABS(DEPTH - LOCSEDDEPT) < 1.E-20 .AND. DEPTH > ZSED) THEN
                DZ = ZSED / I_NRDZ
            ELSE
                DZ = DEPTH / I_NRDZ
            ENDIF

            CUMLIM = 0.0

            !                Bereken totale lichthoeveelheid en lichtlimitatie per laagje

            LIMSURF = 1.0 - EXP(- RADSURF / RADSAT)
            DO IZ = 1, I_NRDZ
                IF (IZ == 1) THEN
                    ACTDEP = 0.5 * DZ
                ELSE
                    ACTDEP = ACTDEP + DZ
                ENDIF

                !                   bereken de fractie algen die naar het sediment oppervlak zijn gemigreerd

                IF (IKMRK1 == 2 .AND. SWEMERSION == 1) THEN
                    IF (MIGRDEPTH2 <= 1E-20) THEN
                        FRACSURF = 0.0
                    ELSE
                        Z = LOCSEDDEPT - DEPTH + ACTDEP
                        RELZ = MIN(1.0, (MAX(0.0, (Z - MIGRDEPTH1) / (MIGRDEPTH2 - MIGRDEPTH1))))
                        FRACSURF = 0.5 * COS(PI * RELZ) + 0.5
                    ENDIF
                ELSE
                    FRACSURF = 0.0
                ENDIF

                ACTRAD = RADTOP * EXP (-EXTVL * ACTDEP)
                ACTLIM = 1.0 - EXP(- ACTRAD / RADSAT)

                CUMLIM = CUMLIM + FRACSURF * LIMSURF + (1.0 - FRACSURF) * ACTLIM

            ENDDO

            !                gemiddelde lichtlimitatie

            CUMLIM = CUMLIM / I_NRDZ

            !                Integratie over de dag

            IF   (MOD(ITIME - ITSTRT, AUXSYS) < IDT)   THEN
                IF (ITIME == ITSTRT) THEN
                    WS2 = CUMLIM
                ELSE
                    WS2 = WS1 / AUXSYS
                ENDIF
                WS1 = 0.0
                PMSA(IP(23)) = WS2
            ENDIF

            FLT = WS2
            WS1 = WS1 + CUMLIM * IDT

            PMSA(IP(21)) = WS1
            PMSA(IP(25)) = FLT

            !              ENDIF

            !             S1_BOTTOM

            IF (S1_BOTTOM .AND. (IKMRK2 == 0 .OR. IKMRK2 == 3)) THEN

                DZ = ZSED / I_NRDZ
                CUMLIM = 0.0

                !                Bereken totale lichthoeveelheid en lichtlimitatie per laagje

                LIMSURF = 1.0 - EXP(- RADBOT / RADSAT)
                DO IZ = 1, I_NRDZ
                    IF (IZ == 1) THEN
                        ACTDEP = 0.5 * DZ
                    ELSE
                        ACTDEP = ACTDEP + DZ
                    ENDIF

                    !                   bereken de fractie algen die naar het sediment oppervlak zijn gemigreerd

                    IF (SWEMERSION == 1) THEN
                        IF (MIGRDEPTH2 <= 1E-20) THEN
                            FRACSURF = 0.0
                        ELSE
                            RELZ = MIN(1.0, (MAX(0.0, (ACTDEP - MIGRDEPTH1) / (MIGRDEPTH2 - MIGRDEPTH1))))
                            FRACSURF = 0.5 * COS(PI * RELZ) + 0.5
                        ENDIF
                    ELSE
                        FRACSURF = 0.0
                    ENDIF

                    ACTRAD = RADBOT * EXP (-EXTVLS1 * ACTDEP)
                    ACTLIM = 1.0 - EXP(- ACTRAD / RADSAT)

                    CUMLIM = CUMLIM + FRACSURF * LIMSURF + (1.0 - FRACSURF) * ACTLIM
                ENDDO

                !                gemiddelde lichtlimitatie

                CUMLIM = CUMLIM / I_NRDZ

                !                Integratie over de dag

                IF   (MOD(ITIME - ITSTRT, AUXSYS) < IDT)   THEN
                    IF (ITIME == ITSTRT) THEN
                        WS4 = CUMLIM
                    ELSE
                        WS4 = WS3 / AUXSYS
                    ENDIF
                    WS3 = 0.0
                    PMSA(IP(24)) = WS4
                ENDIF

                FLTS1 = WS4
                WS3 = WS3 + CUMLIM * IDT

                PMSA(IP(23)) = WS3
                PMSA(IP(26)) = FLTS1

            ENDIF

            !             update pointering in PMSA array

            IP = IP + INCREM(1:NO_POINTER)

        end do

        RETURN
    END

end module m_mpbllm
