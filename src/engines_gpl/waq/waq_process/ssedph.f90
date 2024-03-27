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
module m_ssedph
    use m_waq_precision

    implicit none

contains


    subroutine ssedph (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        use m_evaluate_waq_attribute

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

        REAL(kind = real_wp) :: PMSA  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), NOSEG, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), NOQ1, NOQ2, NOQ3, NOQ4

        INTEGER(kind = int_wp) :: IFLUX, ISEG, IKMRK1, IKMRK2, IN, IP, IP2, &
                IALG, IQ, IVAN, INAAR, IKMRKN, IKMRKV
        REAL(kind = real_wp) :: DEPTH, SEDCAR, SEDDM, SEDNIT, SEDPHO, SEDSIL, SEDSPE, &
                CTODRY, NCRAT, PCRAT, SCRAT, TOTFLX, TOTCON, CONSPE, &
                VELSPE
        !
        !     Local
        !
        INTEGER(kind = int_wp) :: NALG
        !
        NALG = NINT(PMSA(IPOINT(1)))
        IFLUX = 0
        IP2 = IPOINT(2)

        DO ISEG = 1, NOSEG
            CALL evaluate_waq_attribute(1, IKNMRK(ISEG), IKMRK1)
            IF (IKMRK1==1) THEN
                CALL evaluate_waq_attribute(2, IKNMRK(ISEG), IKMRK2)
                IF ((IKMRK2==0).OR.(IKMRK2==3)) THEN
                    !
                    DEPTH = PMSA(IP2)
                    SEDCAR = 0.0
                    SEDDM = 0.0
                    SEDNIT = 0.0
                    SEDPHO = 0.0
                    SEDSIL = 0.0
                    DO IALG = 1, NALG

                        IN = 2 + 0 * NALG + IALG
                        SEDSPE = PMSA(IPOINT(IN) + (ISEG - 1) * INCREM(IN))
                        IN = 2 + 1 * NALG + IALG
                        CTODRY = PMSA(IPOINT(IN) + (ISEG - 1) * INCREM(IN))

                        SEDCAR = SEDCAR + SEDSPE
                        SEDDM = SEDDM + SEDSPE * CTODRY

                        IN = 2 + 2 * NALG + IALG
                        NCRAT = PMSA(IPOINT(IN) + (ISEG - 1) * INCREM(IN))
                        IN = 2 + 3 * NALG + IALG
                        PCRAT = PMSA(IPOINT(IN) + (ISEG - 1) * INCREM(IN))
                        IN = 2 + 4 * NALG + IALG
                        SCRAT = PMSA(IPOINT(IN) + (ISEG - 1) * INCREM(IN))

                        SEDNIT = SEDNIT + SEDSPE * NCRAT
                        SEDPHO = SEDPHO + SEDSPE * PCRAT
                        SEDSIL = SEDSIL + SEDSPE * SCRAT

                        !              ENDIF

                    end do

                    IP = IPOINT(2 + 7 * NALG + 1) + (ISEG - 1) * INCREM(2 + 7 * NALG + 1)
                    PMSA (IP) = SEDCAR
                    IP = IPOINT(2 + 7 * NALG + 2) + (ISEG - 1) * INCREM(2 + 7 * NALG + 2)
                    PMSA (IP) = SEDDM

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
        DO IQ = 1, NOQ1 + NOQ2 + NOQ3

            IP = IPOINT(2 + 7 * NALG + 3) + (IQ - 1) * INCREM(2 + 7 * NALG + 3)
            PMSA (IP) = 0.0

        end do

        !.....Exchangeloop over de verticale richting
        DO IQ = NOQ1 + NOQ2 + 1, NOQ1 + NOQ2 + NOQ3 + NOQ4

            IVAN = IEXPNT(1, IQ)
            INAAR = IEXPNT(2, IQ)

            !        Zoek eerste kenmerk van- en naar-segmenten

            IF (IVAN>0 .AND. INAAR>0) THEN
                CALL evaluate_waq_attribute(1, IKNMRK(IVAN), IKMRKV)
                CALL evaluate_waq_attribute(1, IKNMRK(INAAR), IKMRKN)
                IF (IKMRKV==1.AND.IKMRKN==1 .OR. &
                        IKMRKV==1.AND.IKMRKN==3) THEN

                    !            Water-water uitwisseling

                    TOTFLX = 0.0
                    TOTCON = 0.0
                    DO IALG = 1, NALG
                        IP = IPOINT(2 + 5 * NALG + IALG) + (IVAN - 1) * INCREM(2 + 5 * NALG + IALG)
                        CONSPE = PMSA(IP)
                        IP = IPOINT(2 + 6 * NALG + IALG) + (IQ - 1) * INCREM(2 + 6 * NALG + IALG)
                        VELSPE = PMSA(IP)
                        TOTFLX = TOTFLX + CONSPE * VELSPE
                        TOTCON = TOTCON + CONSPE
                    end do
                    IP = IPOINT(2 + 7 * NALG + 3) + (IQ - 1) * INCREM(2 + 7 * NALG + 3)
                    IF (TOTCON > 0.0) THEN
                        PMSA(IP) = TOTFLX / TOTCON
                    ELSE
                        PMSA(IP) = 0.0
                    ENDIF
                ENDIF
            ENDIF

        end do
        RETURN
        !
    END

end module m_ssedph
