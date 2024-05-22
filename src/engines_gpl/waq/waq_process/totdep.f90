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
module m_totdep
    use m_waq_precision

    implicit none

contains


    subroutine totdep (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        !>\file
        !>       Total depth water column

        !
        !     Description of the module :
        !
        ! Name    T   L I/O   Description                                  Units
        ! ----    --- -  -    -------------------                          -----
        !
        ! DEPTH               segment diepte
        ! TDEPTH              totale diepte ( van surf tot bottom )
        ! LDEPTH              locale diepte ( = van surf tot onderkant segment )

        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        use m_advtra
        use m_evaluate_waq_attribute
        USE BottomSet     !  Module with definition of the waterbottom segments

        IMPLICIT NONE

        REAL(kind = real_wp) :: PMSA  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), NOSEG, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), NOQ1, NOQ2, NOQ3, NOQ4

        REAL(kind = real_wp) :: DEPTH              ! 1  in  depth of segment                                     (m)
        REAL(kind = real_wp) :: SURF               ! 2  in horizontal surface area                              (m2)
        REAL(kind = real_wp) :: TOTALDEPTH         ! 3  out total depth water column                             (m)
        REAL(kind = real_wp) :: LOCALDEPTH         ! 4  out depth from water surface to bottom of segment        (m)
        REAL(kind = real_wp) :: LOCSEDDEPT         ! 5  out Sediment layer depth to bottom of segment            (m)
        REAL(kind = real_wp) :: TOTSEDDEPT         ! 2  out Sediment layer depth to bottom of sediment column    (m)

        INTEGER(kind = int_wp) :: IP1, IP2, IP3, IP4, IP5
        INTEGER(kind = int_wp) :: IN1, IN2, IN3, IN4, IN5
        INTEGER(kind = int_wp) :: IKMRK, ISEG, IFROM, ITO
        INTEGER(kind = int_wp) :: IK                 ! loop counter bottom columns
        INTEGER(kind = int_wp) :: IQ                 ! loop counter exchanges
        INTEGER(kind = int_wp) :: IWA1               ! index first water exchange
        INTEGER(kind = int_wp) :: IWA2               ! index last water exchange
        INTEGER(kind = int_wp) :: ITOP               ! index first bottom exhange
        INTEGER(kind = int_wp) :: IBOT               ! index last bottom exhange
        INTEGER(kind = int_wp) :: IBODEM             ! segment number bottom segment
        INTEGER(kind = int_wp) :: IWATER             ! segment number water segment
        REAL(kind = real_wp) :: CUMTOTDEPTH        ! cummulative in averaging totaldepth
        REAL(kind = real_wp) :: TOTSURF            ! cummulative surf in averaging totaldepth


        !     initialise bottom if necessary

        CALL MAKKO2 (IEXPNT, IKNMRK, NOQ1, NOQ2, NOQ3, &
                NOQ4)

        IP1 = IPOINT(1)
        IP2 = IPOINT(2)
        IP3 = IPOINT(3)
        IP4 = IPOINT(4)
        IP5 = IPOINT(5)

        IN1 = INCREM(1)
        IN2 = INCREM(2)
        IN3 = INCREM(3)
        IN4 = INCREM(4)
        IN5 = INCREM(5)

        !.....Zet de totale en lokale diepte initieel op de diepte
        !.....voor actieve watersegmenten, anders 0
        !.....zet sediment dikte to onderkant segment op 0
        DO ISEG = 1, NOSEG

            PMSA(IP3) = PMSA(IP1)
            PMSA(IP4) = PMSA(IP1)
            PMSA(IP5) = PMSA(IP1)

            IP1 = IP1 + IN1
            IP3 = IP3 + IN3
            IP4 = IP4 + IN4
            IP5 = IP5 + IN5

        end do

        IP1 = IPOINT(1)
        IP3 = IPOINT(3)
        IP4 = IPOINT(4)
        IP5 = IPOINT(5)

        !.....Exchange-loop over de derde richting
        DO IQ = NOQ1 + NOQ2 + 1, NOQ1 + NOQ2 + NOQ3

            IFROM = IEXPNT(1, IQ)
            ITO = IEXPNT(2, IQ)

            IF (IFROM>0 .AND. ITO>0) THEN

                CALL extract_waq_attribute(2, IKNMRK(IFROM), IKMRK)
                IF ((IKMRK==0).OR.(IKMRK==1)) THEN

                    PMSA (IP3 + (IFROM - 1) * IN3) = &
                            PMSA (IP1 + (IFROM - 1) * IN1)
                    PMSA (IP4 + (IFROM - 1) * IN4) = &
                            PMSA (IP1 + (IFROM - 1) * IN1)
                    PMSA (IP5 + (IFROM - 1) * IN5) = &
                            PMSA (IP4 + (IFROM - 1) * IN4)

                    PMSA (IP3 + (ITO - 1) * IN3) = &
                            PMSA (IP1 + (IFROM - 1) * IN1) + &
                                    PMSA (IP1 + (ITO - 1) * IN1)
                    PMSA (IP4 + (ITO - 1) * IN4) = &
                            PMSA (IP1 + (IFROM - 1) * IN1) + &
                                    PMSA (IP1 + (ITO - 1) * IN1)
                    PMSA (IP5 + (ITO - 1) * IN5) = &
                            PMSA (IP4 + (ITO - 1) * IN4)

                ELSE

                    PMSA (IP3 + (ITO - 1) * IN3) = &
                            PMSA (IP3 + (IFROM - 1) * IN3) + &
                                    PMSA (IP1 + (ITO - 1) * IN1)
                    PMSA (IP4 + (ITO - 1) * IN4) = &
                            PMSA (IP4 + (IFROM - 1) * IN4) + &
                                    PMSA (IP1 + (ITO - 1) * IN1)
                    PMSA (IP5 + (ITO - 1) * IN5) = &
                            PMSA (IP4 + (ITO - 1) * IN4)

                ENDIF
                !           ENDIF
            ENDIF

        end do


        !.....Exchange-loop over de derde richting
        DO IQ = NOQ1 + NOQ2 + NOQ3, NOQ1 + NOQ2 + 1, -1

            IFROM = IEXPNT(1, IQ)
            ITO = IEXPNT(2, IQ)

            !........Berekende totale dieptes voor de onderste laag segmenten
            !        toekennen aan de bovenliggende segmenten

            IF (IFROM>0 .AND. ITO>0) then
                CALL extract_waq_attribute(1, IKNMRK(ITO), IKMRK)
                IF (IKMRK == 1) THEN

                    PMSA (IP3 + (IFROM - 1) * IN3) = &
                            PMSA (IP3 + (ITO - 1) * IN3)
                ENDIF
            ENDIF
        end do

        !     loop over the sediment columns, set sediment depth

        IP1 = IPOINT(1)
        IP2 = IPOINT(2)
        IP3 = IPOINT(3)
        IP4 = IPOINT(4)
        IP5 = IPOINT(5)

        DO IK = 1, Coll%current_size

            IWA1 = Coll%set(IK)%fstwatsed
            IWA2 = Coll%set(IK)%lstwatsed
            ITOP = Coll%set(IK)%topsedsed
            IBOT = Coll%set(IK)%botsedsed

            ! make average totaldepth water

            CUMTOTDEPTH = 0.0
            TOTSURF = 0.0
            DO IQ = IWA1, IWA2
                IWATER = IEXPNT(1, IQ)
                TOTALDEPTH = PMSA(IP3 + (IWATER - 1) * IN3)
                SURF = PMSA(IP2 + (IWATER - 1) * IN2)
                CUMTOTDEPTH = CUMTOTDEPTH + TOTALDEPTH * SURF
                TOTSURF = TOTSURF + SURF
            ENDDO
            IF (TOTSURF > 1E-20) THEN
                TOTALDEPTH = CUMTOTDEPTH / TOTSURF
            ELSE
                TOTALDEPTH = 0.0
            ENDIF

            ! accumulate depth within bottom

            LOCALDEPTH = TOTALDEPTH
            LOCSEDDEPT = 0.0
            DO IQ = ITOP, IBOT
                IBODEM = IEXPNT(1, IQ)
                DEPTH = PMSA(IP1 + (IBODEM - 1) * IN1)
                LOCALDEPTH = LOCALDEPTH + DEPTH
                LOCSEDDEPT = LOCSEDDEPT + DEPTH
                PMSA(IP4 + (IBODEM - 1) * IN4) = LOCALDEPTH
                PMSA(IP5 + (IBODEM - 1) * IN5) = LOCSEDDEPT
            ENDDO

            ! final is total copy back in the column

            TOTSEDDEPT = LOCSEDDEPT
            DO IQ = ITOP, IBOT
                IBODEM = IEXPNT(1, IQ)
                PMSA(IP3 + (IBODEM - 1) * IN3) = TOTSEDDEPT
            ENDDO

        ENDDO

        RETURN
    END

end module m_totdep
