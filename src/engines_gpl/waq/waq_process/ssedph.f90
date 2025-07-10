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
module m_ssedph
    use m_waq_precision

    implicit none

contains


    subroutine ssedph (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_extract_waq_attribute

        !>\file
        !>       Sum of sedimentation flux of algae Dynamo - Bloom - GEM

        !
        !     Description of the module :
        !
        ! Name    T   L I/O   Description                                    Units
        ! ----    --- -  -    -------------------                            -----

        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library

        !     ------   -----  ------------

        IMPLICIT REAL (A-H, J-Z)

        REAL(kind = real_wp) :: process_space_real  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir

        INTEGER(kind = int_wp) :: IFLUX, ISEG, IKMRK1, IKMRK2, IN, IP, IP2, &
                IALG, IQ, IVAN, INAAR, IKMRKN, IKMRKV
        REAL(kind = real_wp) :: DEPTH, SEDCAR, SEDDM, SEDNIT, SEDPHO, SEDSIL, SEDSPE, &
                CTODRY, NCRAT, PCRAT, SCRAT, TOTFLX, TOTCON, CONSPE, &
                VELSPE
        !
        !     Local
        !
        INTEGER(kind = int_wp) :: NALG
        integer(kind = int_wp), parameter :: vx_index = 6 ! The one velocity array starts AFTER the segment-based output
        !
        NALG = NINT(process_space_real(IPOINT(1)))
        IFLUX = 0
        IP2 = IPOINT(2)

        DO ISEG = 1, num_cells
            CALL extract_waq_attribute(1, IKNMRK(ISEG), IKMRK1)
            IF (IKMRK1==1) THEN
                CALL extract_waq_attribute(2, IKNMRK(ISEG), IKMRK2)
                IF ((IKMRK2==0).OR.(IKMRK2==3)) THEN
                    !
                    DEPTH = process_space_real(IP2)
                    SEDCAR = 0.0
                    SEDDM = 0.0
                    SEDNIT = 0.0
                    SEDPHO = 0.0
                    SEDSIL = 0.0
                    DO IALG = 1, NALG

                        IN = 2 + 0 * NALG + IALG
                        SEDSPE = process_space_real(IPOINT(IN) + (ISEG - 1) * INCREM(IN))
                        IN = 2 + 1 * NALG + IALG
                        CTODRY = process_space_real(IPOINT(IN) + (ISEG - 1) * INCREM(IN))

                        SEDCAR = SEDCAR + SEDSPE
                        SEDDM = SEDDM + SEDSPE * CTODRY

                        IN = 2 + 2 * NALG + IALG
                        NCRAT = process_space_real(IPOINT(IN) + (ISEG - 1) * INCREM(IN))
                        IN = 2 + 3 * NALG + IALG
                        PCRAT = process_space_real(IPOINT(IN) + (ISEG - 1) * INCREM(IN))
                        IN = 2 + 4 * NALG + IALG
                        SCRAT = process_space_real(IPOINT(IN) + (ISEG - 1) * INCREM(IN))

                        SEDNIT = SEDNIT + SEDSPE * NCRAT
                        SEDPHO = SEDPHO + SEDSPE * PCRAT
                        SEDSIL = SEDSIL + SEDSPE * SCRAT

                        !              ENDIF

                    end do

                    IP = IPOINT(2 + 7 * NALG + 1) + (ISEG - 1) * INCREM(2 + 7 * NALG + 1)
                    process_space_real (IP) = SEDCAR
                    IP = IPOINT(2 + 7 * NALG + 2) + (ISEG - 1) * INCREM(2 + 7 * NALG + 2)
                    process_space_real (IP) = SEDDM
                    IP = IPOINT(2 + 7 * NALG + 3) + (ISEG - 1) * INCREM(2 + 7 * NALG + 3)
                    process_space_real (IP) = SEDNIT
                    IP = IPOINT(2 + 7 * NALG + 4) + (ISEG - 1) * INCREM(2 + 7 * NALG + 4)
                    process_space_real (IP) = SEDPHO
                    IP = IPOINT(2 + 7 * NALG + 5) + (ISEG - 1) * INCREM(2 + 7 * NALG + 5)
                    process_space_real (IP) = SEDSIL

                    !         NO LONGER Define fluxes only for Bloom (NALG .GT. 6)

                    IF (DEPTH > 0.0) THEN
                        FL(IFLUX + 1) = SEDCAR / DEPTH
                        FL(IFLUX + 2) = SEDNIT / DEPTH
                        FL(IFLUX + 3) = SEDPHO / DEPTH
                        FL(IFLUX + 4) = SEDSIL / DEPTH
                    ELSE
                        FL(IFLUX + 1) = 0.0
                        FL(IFLUX + 2) = 0.0
                        FL(IFLUX + 3) = 0.0
                        FL(IFLUX + 4) = 0.0
                    ENDIF
                    !          ENDIF

                ENDIF
            ENDIF
            IFLUX = IFLUX + NOFLUX
            IP2 = IP2 + INCREM(2)
            !
        end do
        !
        !.....Exchangeloop over de horizontale richting ter initialisatie
        DO IQ = 1, num_exchanges_u_dir + num_exchanges_v_dir + num_exchanges_z_dir

            IP = IPOINT(2 + 7 * NALG + vx_index) + (IQ - 1) * INCREM(2 + 7 * NALG + vx_index)
            process_space_real (IP) = 0.0

        end do

        !.....Exchangeloop over de verticale richting
        DO IQ = num_exchanges_u_dir + num_exchanges_v_dir + 1, num_exchanges_u_dir + num_exchanges_v_dir + num_exchanges_z_dir + num_exchanges_bottom_dir

            IVAN = IEXPNT(1, IQ)
            INAAR = IEXPNT(2, IQ)

            !        Zoek eerste kenmerk van- en naar-segmenten

            IF (IVAN>0 .AND. INAAR>0) THEN
                CALL extract_waq_attribute(1, IKNMRK(IVAN), IKMRKV)
                CALL extract_waq_attribute(1, IKNMRK(INAAR), IKMRKN)
                IF (IKMRKV==1.AND.IKMRKN==1 .OR. &
                        IKMRKV==1.AND.IKMRKN==3) THEN

                    !            Water-water uitwisseling

                    TOTFLX = 0.0
                    TOTCON = 0.0
                    DO IALG = 1, NALG
                        IP = IPOINT(2 + 5 * NALG + IALG) + (IVAN - 1) * INCREM(2 + 5 * NALG + IALG)
                        CONSPE = process_space_real(IP)
                        IP = IPOINT(2 + 6 * NALG + IALG) + (IQ - 1) * INCREM(2 + 6 * NALG + IALG)
                        VELSPE = process_space_real(IP)
                        TOTFLX = TOTFLX + CONSPE * VELSPE
                        TOTCON = TOTCON + CONSPE
                    end do
                    IP = IPOINT(2 + 7 * NALG + vx_index) + (IQ - 1) * INCREM(2 + 7 * NALG + vx_index)
                    IF (TOTCON > 0.0) THEN
                        process_space_real(IP) = TOTFLX / TOTCON
                    ELSE
                        process_space_real(IP) = 0.0
                    ENDIF
                ENDIF
            ENDIF

        end do
        RETURN
        !
    END

end module m_ssedph
