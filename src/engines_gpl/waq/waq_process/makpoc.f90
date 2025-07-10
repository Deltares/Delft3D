!!  Copyright (C)  Stichting Deltares, 2012-2025.
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
module m_makpoc
    use m_waq_precision

    implicit none

contains


    subroutine makpoc (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_logger_helper, only : stop_with_error, get_log_unit_number

        !>\file
        !>       Derive OOC from IM-fractions and percentage POM in IMx

        !
        !     Description of the module :
        !
        ! Name    T   L I/O   Description                                    Uni
        ! ----    --- -  -    -------------------                            ---
        ! IMx     R*4 1 I  conversion factor for gX->dry matter substy [gDM/gm3]
        ! FrCx    R*4 1 I  percentage OM in sediment x                 [gOC/gDM]
        ! POC(x)  R*4 1 I  particulate carbon content                   [gOC/m3]
        ! OCPOM   R*4 1 I  Dry weight of Organic Carbonin Part.Org.Mat [gDM/gOC]

        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library

        !     ------   -----  ------------

        IMPLICIT NONE

        REAL(kind = real_wp) :: process_space_real  (*), FL    (*)
        INTEGER(kind = int_wp) :: IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, ISEG
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, LUNREP, &
                IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir
        !
        !     Local
        !
        REAL(kind = real_wp) :: IM1, IM2, IM3
        REAL(kind = real_wp) :: FRC1, FRC2, FRC3
        REAL(kind = real_wp) :: OCPOM
        REAL(kind = real_wp) :: poc, POC1, POC2, POC3
        !
        IP1 = IPOINT(1)
        IP2 = IPOINT(2)
        IP3 = IPOINT(3)
        IP4 = IPOINT(4)
        IP5 = IPOINT(5)
        IP6 = IPOINT(6)
        IP7 = IPOINT(7)
        IP8 = IPOINT(8)
        !
        DO ISEG = 1, num_cells

            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                !
                IM1 = MAX(0.0, process_space_real(IP1))
                IM2 = MAX(0.0, process_space_real(IP2))
                IM3 = MAX(0.0, process_space_real(IP3))
                FRC1 = process_space_real(IP4)
                FRC2 = process_space_real(IP5)
                FRC3 = process_space_real(IP6)
                OCPOM = process_space_real(IP7)

                !***********************************************************************
                !**** Calculations connected to the POC calculation
                !***********************************************************************

                !     Calculate amount POC to be made
                IF (OCPOM * FRC1 < 1.0D0) THEN
                    POC1 = FRC1 * IM1 / (1 - OCPOM * FRC1)
                ELSE
                    CALL get_log_unit_number(LUNREP)
                    WRITE(LUNREP, *) 'ERROR in MAKPOC'
                    WRITE(LUNREP, *) 'Segment:', ISEG
                    WRITE(LUNREP, *) 'fctr   :', OCPOM
                    WRITE(LUNREP, *) 'fcsed1 :', FRC1
                    WRITE(LUNREP, *) 'fctr * fcsed1 must be less than 1.00'
                    CALL stop_with_error()
                END IF

                IF (OCPOM * FRC2 < 1.0D0) THEN
                    POC2 = FRC2 * IM2 / (1 - OCPOM * FRC2)
                ELSE
                    CALL get_log_unit_number(LUNREP)
                    WRITE(LUNREP, *) 'ERROR in MAKPOC'
                    WRITE(LUNREP, *) 'Segment:', ISEG
                    WRITE(LUNREP, *) 'fctr   :', OCPOM
                    WRITE(LUNREP, *) 'fcsed2 :', FRC2
                    WRITE(LUNREP, *) 'fctr * fcsed2 must be less than 1.00'
                    CALL stop_with_error()
                END IF

                IF (OCPOM * FRC3 < 1.0D0) THEN
                    POC3 = FRC3 * IM3 / (1 - OCPOM * FRC3)
                ELSE
                    CALL get_log_unit_number(LUNREP)
                    WRITE(LUNREP, *) 'ERROR in MAKPOC'
                    WRITE(LUNREP, *) 'Segment:', ISEG
                    WRITE(LUNREP, *) 'fctr   :', OCPOM
                    WRITE(LUNREP, *) 'fcsed3 :', FRC3
                    WRITE(LUNREP, *) 'fctr * fcsed3 must be less than 1'
                    CALL stop_with_error()
                END IF

                !     Total POC
                POC = POC1 + POC2 + POC3

                process_space_real (IP8) = POC
                !
            ENDIF
            !
            IP1 = IP1 + INCREM (1)
            IP2 = IP2 + INCREM (2)
            IP3 = IP3 + INCREM (3)
            IP4 = IP4 + INCREM (4)
            IP5 = IP5 + INCREM (5)
            IP6 = IP6 + INCREM (6)
            IP7 = IP7 + INCREM (7)
            IP8 = IP8 + INCREM (8)
            !
        end do
        !
        RETURN
        !
    END

end module m_makpoc
