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
module m_mpbnlm
    use m_waq_precision

    implicit none

contains


    SUBROUTINE MPBNLM (process_space_real, FL, IPOINT, INCREM, num_cells, &
            NOFLUX, IEXPNT, IKNMRK, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_extract_waq_attribute

        !     ***********************************************************************
        !          +----------------------------------------+
        !          |    D E L F T   H Y D R A U L I C S     |
        !          +----------------------------------------+
        !     ***********************************************************************
        !
        !          Function : MPB nutrient limitation function
        !
        !     ***********************************************************************

        IMPLICIT NONE

        !          arguments

        REAL(kind = real_wp) :: process_space_real(*)            ! in/out input-output array space to be adressed with IPOINT/INCREM
        REAL(kind = real_wp) :: FL(*)              ! in/out flux array
        INTEGER(kind = int_wp) :: IPOINT(*)          ! in     start index input-output parameters in the process_space_real array (segment or exchange number 1)
        INTEGER(kind = int_wp) :: INCREM(*)          ! in     increment for each segment-exchange for the input-output parameters in the process_space_real array
        INTEGER(kind = int_wp) :: num_cells              ! in     number of segments
        INTEGER(kind = int_wp) :: NOFLUX             ! in     total number of fluxes (increment in FL array)
        INTEGER(kind = int_wp) :: IEXPNT(4, *)        ! in     exchange pointer table
        INTEGER(kind = int_wp) :: IKNMRK(*)          ! in     segment features array
        INTEGER(kind = int_wp) :: num_exchanges_u_dir               ! in     number of exchanges in first direction
        INTEGER(kind = int_wp) :: num_exchanges_v_dir               ! in     number of exchanges in second direction
        INTEGER(kind = int_wp) :: num_exchanges_z_dir               ! in     number of exchanges in third direction
        INTEGER(kind = int_wp) :: num_exchanges_bottom_dir               ! in     number of exchanges in fourth direction

        !          from process_space_real array

        REAL(kind = real_wp) :: CAM                !  1 in  Ammonium (NH4)                             (gN/m3)
        REAL(kind = real_wp) :: CNI                !  2 in  Nitrate (NO3)                              (gN/m3)
        REAL(kind = real_wp) :: CPHO               !  3 in  Ortho-Phosphate (PO4)                      (gP/m3)
        REAL(kind = real_wp) :: CSI                !  4 in  dissolved Silica (Si)                     (gSi/m3)
        REAL(kind = real_wp) :: KDIN               !  5 in  MPB1 half saturation constant N            (gN/m3)
        REAL(kind = real_wp) :: KPHO               !  6 in  MPB1 half saturation constant P            (gP/m3)
        REAL(kind = real_wp) :: KSI                !  7 in  MPB1 half saturation constant Si          (gSi/m3)
        LOGICAL :: S1_BOTTOM          !  8 in  switch for MPB model (0=segment,1=S1)          (-)
        REAL(kind = real_wp) :: CAMS1              !  9 in  Ammonium concentration in the bottom       (gN/m3)
        REAL(kind = real_wp) :: CNIS1              ! 10 in  Nitrate concentration in layer S1          (gN/m3)
        REAL(kind = real_wp) :: CPHOS1             ! 11 in  Phosphate concentration in the bottom      (gP/m3)
        REAL(kind = real_wp) :: CSIS1              ! 12 in  Silicium concentration in layer S1        (gSi/m3)
        REAL(kind = real_wp) :: FN                 ! 13 out MPB nitrogen limitation                        (-)
        REAL(kind = real_wp) :: FPHO               ! 14 out MPB phosphate limitation                       (-)
        REAL(kind = real_wp) :: FSI                ! 15 out MPB silicate limitation                        (-)
        REAL(kind = real_wp) :: FNUT               ! 16 out MPB nutrient limitation                        (-)
        REAL(kind = real_wp) :: FNS1               ! 17 out MPB nitrogen limitation S1                     (-)
        REAL(kind = real_wp) :: FPHOS1             ! 18 out MPB phosphate limitation S1                    (-)
        REAL(kind = real_wp) :: FSIS1              ! 19 out MPB silicate limitation S1                     (-)
        REAL(kind = real_wp) :: FNUTS1             ! 20 out MPB nutrient limitation S1                     (-)
        REAL(kind = real_wp) :: AMOPRF             ! Preference factor ammomium over nitrate (DYNAMO)      (-)

        !          local declarations

        INTEGER(kind = int_wp) :: ISEG               ! loop counter segment loop
        INTEGER(kind = int_wp) :: IKMRK1             ! first feature inactive(0)-active(1)-bottom(2) segment
        INTEGER(kind = int_wp) :: IKMRK2             ! second feature 2D(0)-surface(1)-middle(2)-bottom(3) segment
        INTEGER(kind = int_wp), parameter :: NO_POINTER = 21    ! number of input output variables in process_space_real array
        INTEGER(kind = int_wp) :: IP(NO_POINTER)     ! index pointer in process_space_real array updated for each segment
        REAL(kind = real_wp) :: CNN                ! Weigthed nitrogen concentration (a la DYNAMO)     (gN/m3)
        REAL(kind = real_wp) :: CNNS1              ! Weigthed nitrogen concentration, bottom           (gN/m3)

        !          initialise pointers for process_space_real and FL array

        IP = IPOINT(1:NO_POINTER)

        !          loop over the segments

        DO ISEG = 1, num_cells

            CALL extract_waq_attribute(1, IKNMRK(ISEG), IKMRK1)
            CALL extract_waq_attribute(2, IKNMRK(ISEG), IKMRK2)

            CAM = MAX(process_space_real(IP(1)), 0.0)
            CNI = MAX(process_space_real(IP(2)), 0.0)
            CPHO = MAX(process_space_real(IP(3)), 0.0)
            CSI = MAX(process_space_real(IP(4)), 0.0)
            KDIN = process_space_real(IP(5))
            KPHO = process_space_real(IP(6))
            KSI = process_space_real(IP(7))
            S1_BOTTOM = NINT(process_space_real(IP(8))) == 1
            CAMS1 = MAX(process_space_real(IP(9)), 0.0)
            CNIS1 = MAX(process_space_real(IP(10)), 0.0)
            CPHOS1 = MAX(process_space_real(IP(11)), 0.0)
            CSIS1 = MAX(process_space_real(IP(12)), 0.0)
            AMOPRF = process_space_real(IP(13))

            CNN = CAM + CNI / AMOPRF
            CNNS1 = CAMS1 + CNIS1 / AMOPRF

            !             water en delwaq-g bodem

            IF ((IKMRK1==1) .OR. (IKMRK1==2)) THEN

                FN = CNN / (KDIN + CNN)
                FPHO = CPHO / (KPHO + CPHO)
                IF (KSI < 1E-20) THEN
                    FSI = 1.0
                ELSE
                    FSI = CSI / (KSI + CSI)
                ENDIF
                FNUT = MAX(0.0, MIN(FN, FPHO, FSI))

            ELSE

                FN = 0.0
                FPHO = 0.0
                FSI = 0.0
                FNUT = 0.0

            ENDIF

            !             s1 bodem

            IF (S1_BOTTOM .AND. (IKMRK2==0 .OR. IKMRK2==3)) THEN

                FNS1 = CNNS1 / (KDIN + CNNS1)
                FPHOS1 = CPHOS1 / (KPHO + CPHOS1)
                IF (KSI < 1E-20) THEN
                    FSIS1 = 1.0
                ELSE
                    FSIS1 = CSIS1 / (KSI + CSIS1)
                ENDIF
                FNUTS1 = MAX(0.0, MIN(FNS1, FPHOS1, FSIS1))

            ELSE

                FNS1 = 0.0
                FPHOS1 = 0.0
                FSIS1 = 0.0
                FNUTS1 = 0.0

            ENDIF

            process_space_real(IP(14)) = FN
            process_space_real(IP(15)) = FPHO
            process_space_real(IP(16)) = FSI
            process_space_real(IP(17)) = FNUT
            process_space_real(IP(18)) = FNS1
            process_space_real(IP(19)) = FPHOS1
            process_space_real(IP(20)) = FSIS1
            process_space_real(IP(21)) = FNUTS1

            !             update pointering in process_space_real

            IP = IP + INCREM(1:NO_POINTER)

        end do

        RETURN
    END

end module m_mpbnlm
