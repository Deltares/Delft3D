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
module m_somsed
    use m_waq_precision

    implicit none

contains


    subroutine somsed (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_extract_waq_attribute

        !>\file
        !>       Total of all sedimenting substances

        !
        !     Description of the module :
        !
        ! Name    T   L I/O   Description                                    Units
        ! ----    --- -  -    -------------------                            -----
        ! DMCFy   R*4 1 I  conversion factor for gX->dry matter subst y   [gDM/gX]
        ! FLXy    R*4 1 I  sedimentation flux substance y                [gX/m3/d]
        ! TDMSED  R*4 1 O  total dry matter sedimentation flux          [gDM/m2/d]
        ! TIMSED  R*4 1 O  total inorganic mattter sedimentation flux   [gDM/m2/d]

        !     Logical Units : -
        !     Modules called : -
        !     Name     Type   Library

        !     ------   -----  ------------

        IMPLICIT NONE

        REAL(kind = real_wp) :: process_space_real  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(40), INCREM(40), num_cells, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir

        INTEGER(kind = int_wp) :: IP(40)
        REAL(kind = real_wp) :: FLX1, FLX2, FLX3, FLX1S2, FLX2S2, FLX3S2, &
                FLPOC, FLPOM, FLALGC, &
                FLALGM, DMCF1, DMCF2, DMCF3, TIMSED, TDMSED, POCSED, &
                C1, C2, C3, V1, V2, V3, CTOT, &
                FLPOC1, FLPOC2, FLPOC3, FLPOC4, DMPOC1, DMPOC2, DMPOC3, &
                DMPOC4, CPTOT, &
                CP1, VP1, &
                CP2, VP2, &
                CP3, VP3, &
                CP4, VP4
        INTEGER(kind = int_wp) :: IFLUX, ISEG, IKMRK2, IQ, IVAN, INAAR
        INTEGER(kind = int_wp) :: IKMRKN, IKMRKV

        IP = IPOINT
        !
        IFLUX = 0
        DO ISEG = 1, num_cells
            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                CALL extract_waq_attribute(2, IKNMRK(ISEG), IKMRK2)
                IF ((IKMRK2==0).OR.(IKMRK2==3)) THEN
                    !

                    FLX1 = process_space_real(IP(1))
                    FLX2 = process_space_real(IP(2))
                    FLX3 = process_space_real(IP(3))
                    FLX1S2 = process_space_real(IP(4))
                    FLX2S2 = process_space_real(IP(5))
                    FLX3S2 = process_space_real(IP(6))
                    FLPOC1 = process_space_real(IP(7))
                    FLPOC2 = process_space_real(IP(8))
                    FLPOC3 = process_space_real(IP(9))
                    FLPOC4 = process_space_real(IP(10))
                    FLALGC = process_space_real(IP(11))
                    FLALGM = process_space_real(IP(12))
                    DMCF1 = process_space_real(IP(13))
                    DMCF2 = process_space_real(IP(14))
                    DMCF3 = process_space_real(IP(15))
                    DMPOC1 = process_space_real(IP(16))
                    DMPOC2 = process_space_real(IP(17))
                    DMPOC3 = process_space_real(IP(18))
                    DMPOC4 = process_space_real(IP(19))

                    !*******************************************************************************
                    !**** Calculations connected to the sedimentation
                    !***********************************************************************

                    !    Calculate som sedimentation of dry matter
                    TIMSED = (FLX1 + FLX1S2) * DMCF1 + &
                            (FLX2 + FLX2S2) * DMCF2 + &
                            (FLX3 + FLX3S2) * DMCF3
                    FLPOC = FLPOC1 + FLPOC2 + FLPOC3 + FLPOC4
                    FLPOM = FLPOC1 * DMPOC1 + FLPOC2 * DMPOC2 + FLPOC3 * DMPOC3 &
                            + FLPOC4 * DMPOC4

                    TDMSED = TIMSED + FLPOM + FLALGM

                    POCSED = FLPOC + FLALGC

                    process_space_real (IP(34)) = TDMSED
                    process_space_real (IP(35)) = TIMSED
                    process_space_real (IP(36)) = POCSED
                    process_space_real (IP(37)) = FLPOC
                    process_space_real (IP(38)) = FLPOM

                ENDIF
            ENDIF
            IFLUX = IFLUX + NOFLUX
            IP = IP + INCREM
            !
        end do
        !

        !.....Exchangeloop over de horizontale richting
        IP = IPOINT
        DO IQ = 1, num_exchanges_u_dir + num_exchanges_v_dir
            process_space_real(IP(39)) = 0.0
            process_space_real(IP(40)) = 0.0
            IP = IP + INCREM
        end do

        !.....Exchangeloop over de verticale richting
        DO IQ = num_exchanges_u_dir + num_exchanges_v_dir + 1, num_exchanges_u_dir + num_exchanges_v_dir + num_exchanges_z_dir

            process_space_real(IP(39)) = 0.0
            process_space_real(IP(40)) = 0.0
            IVAN = IEXPNT(1, IQ)
            INAAR = IEXPNT(2, IQ)

            !        Zoek eerste kenmerk van- en naar-segmenten

            IF (IVAN > 0 .AND. INAAR > 0) THEN
                CALL extract_waq_attribute(1, IKNMRK(IVAN), IKMRKV)
                CALL extract_waq_attribute(1, IKNMRK(INAAR), IKMRKN)
                IF (IKMRKV==1.AND.IKMRKN==1) THEN

                    !            Water-water uitwisseling

                    C1 = process_space_real(IPOINT(20) + (IVAN - 1) * INCREM(20))
                    C2 = process_space_real(IPOINT(21) + (IVAN - 1) * INCREM(21))
                    C3 = process_space_real(IPOINT(22) + (IVAN - 1) * INCREM(22))
                    CP1 = process_space_real(IPOINT(23) + (IVAN - 1) * INCREM(23))
                    CP2 = process_space_real(IPOINT(24) + (IVAN - 1) * INCREM(24))
                    CP3 = process_space_real(IPOINT(25) + (IVAN - 1) * INCREM(25))
                    CP4 = process_space_real(IPOINT(26) + (IVAN - 1) * INCREM(26))
                    V1 = process_space_real(IP(27))
                    V2 = process_space_real(IP(28))
                    V3 = process_space_real(IP(29))
                    VP1 = process_space_real(IP(30))
                    VP2 = process_space_real(IP(31))
                    VP3 = process_space_real(IP(32))
                    VP4 = process_space_real(IP(33))
                    CTOT = C1 + C2 + C3
                    CPTOT = CP1 + CP2 + CP3 + CP4
                    IF (CTOT > 0.0) &
                            process_space_real(IP(39)) = (C1 * V1 + C2 * V2 + C3 * V3) / CTOT
                    IF (CPTOT > 0.0) &
                            process_space_real(IP(40)) = (CP1 * VP1 + CP2 * VP2 + CP3 * VP3 + CP4 * VP4) / CPTOT
                ENDIF
            ENDIF

            IP = IP + INCREM

        end do

        RETURN
        !
    END

end module m_somsed
