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
module m_trase2
    use m_waq_precision

    implicit none

contains


    subroutine trase2 (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        use m_extract_waq_attribute

        !>\file
        !>       Total of transport in sediment for 66 substances

        !
        !     Description of the module :
        !
        !        Total of TRAnsport processes in the SEDiment
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

        !     from PMSA array

        INTEGER(kind = int_wp) :: SWEMERSION         ! 3  in  switch indicating submersion(0) or emersion (1)
        INTEGER(kind = int_wp) :: XTRDIF             ! 4  in  extra diffusion factor in sediment during emersion (-)

        INTEGER(kind = int_wp) :: IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, IP9, IP10, IP11, IP12
        INTEGER(kind = int_wp) :: IN1, IN2, IN3, IN4, IN5, IN6, IN7, IN8, IN9, IN10, IN11, IN12
        REAL(kind = real_wp) :: FRDISU, FRDOCU, FRDISD, FRDOCD, &
                FRPAR, VRESU, VSEDI, VBURI, &
                VBTUR, VBIRR, FRDIS, VSEEP
        LOGICAL  NEWBOT
        INTEGER(kind = int_wp) :: IKMRKN, IKMRKV, IVAN, INAAR, IQ

        IP1 = IPOINT(1)
        IP2 = IPOINT(2)
        IP3 = IPOINT(3)
        IP4 = IPOINT(4)
        IP5 = IPOINT(5)
        IP6 = IPOINT(6)
        IP7 = IPOINT(7)
        IP8 = IPOINT(8)
        IP9 = IPOINT(9)
        IP10 = IPOINT(10)
        IP11 = IPOINT(11)
        IP12 = IPOINT(12)
        !
        IN1 = INCREM(1)
        IN2 = INCREM(2)
        IN3 = INCREM(3)
        IN4 = INCREM(4)
        IN5 = INCREM(5)
        IN6 = INCREM(6)
        IN7 = INCREM(7)
        IN8 = INCREM(8)
        IN9 = INCREM(9)
        IN10 = INCREM(10)
        IN11 = INCREM(11)
        IN12 = INCREM(12)

        !.....Segmentloop om op nul te zetten
        !      DO 9000 ISEG = 1,NOSEG
        ! 9000 CONTINUE

        !.....Exchangeloop over de horizontale richtingen om 0 te zetten
        !.....en over de vertical richting om te initialiseren
        DO IQ = 1, NOQ1 + NOQ2 + NOQ3
            PMSA(IP11) = 0.0
            PMSA(IP12) = 0.0
            IP5 = IP5 + IN5
            IP6 = IP6 + IN6
            IP7 = IP7 + IN7
            IP8 = IP8 + IN8
            IP9 = IP9 + IN9
            IP10 = IP10 + IN10
            IP11 = IP11 + IN11
            IP12 = IP12 + IN12
        end do

        !.....Exchangeloop over de verticale richting

        DO IQ = NOQ1 + NOQ2 + NOQ3 + 1, NOQ1 + NOQ2 + NOQ3 + NOQ4

            IVAN = IEXPNT(1, IQ)
            INAAR = IEXPNT(2, IQ)

            !        Zoek eerste kenmerk van- en naar-segmenten
            IF (IVAN > 0) THEN
                CALL extract_waq_attribute(1, IKNMRK(IVAN), IKMRKV)
            ELSE
                IKMRKV = -1
            ENDIF
            IF (INAAR > 0) THEN
                CALL extract_waq_attribute(1, IKNMRK(INAAR), IKMRKN)
            ELSE
                IKMRKN = -1
            ENDIF

            !        extra diffusion during emersion

            XTRDIF = 1.0
            IF (IVAN > 0) THEN
                SWEMERSION = NINT(PMSA(IP3 + (IVAN - 1) * IN3))
                IF (SWEMERSION == 1) THEN
                    XTRDIF = PMSA(IP4 + (IVAN - 1) * IN4)
                ENDIF
            ENDIF

            NEWBOT = .FALSE.

            IF ((IKMRKV==1 .AND. IKMRKN==3)  .OR. &
                    (IKMRKV==0 .AND. IKMRKN==3)) THEN

                !.....WATER-SEDIMENT INTERFACE

                FRDISD = PMSA(IP1 + (INAAR - 1) * IN1)
                FRDISU = PMSA(IP1 + (IVAN - 1) * IN1)
                FRDOCD = PMSA(IP2 + (INAAR - 1) * IN2)
                FRDOCU = PMSA(IP2 + (IVAN - 1) * IN2)

                NEWBOT = .TRUE.

            ENDIF

            IF ((IKMRKV==3 .AND. IKMRKN==3)) THEN

                !.....SEDIMENT-SEDIMENT INTERFACE

                FRDISU = PMSA(IP1 + (IVAN - 1) * IN1)
                FRDISD = PMSA(IP1 + (INAAR - 1) * IN1)
                FRDOCU = PMSA(IP2 + (IVAN - 1) * IN2)
                FRDOCD = PMSA(IP2 + (INAAR - 1) * IN2)

                NEWBOT = .TRUE.

            ENDIF

            IF (IKMRKV==3 .AND. IKMRKN==-1) THEN

                !.....DEEP SEDIMENT BOUNDARY

                FRDISU = PMSA(IP1 + (IVAN - 1) * IN1)
                FRDISD = FRDISU
                FRDOCU = PMSA(IP2 + (IVAN - 1) * IN2)
                FRDOCD = FRDOCU

                NEWBOT = .TRUE.

            ENDIF

            !        Delwaq-G exchange?

            IF (NEWBOT) THEN

                VRESU = PMSA(IP5)
                VSEDI = PMSA(IP6)
                VBURI = PMSA(IP7)
                VBTUR = PMSA(IP8)
                VBIRR = PMSA(IP9)
                VSEEP = PMSA(IP10)

                VBIRR = VBIRR * XTRDIF

                !            Upward advection

                FRDIS = FRDISD + FRDOCD
                FRPAR = 1.0 - FRDIS
                PMSA(IP11) = (VRESU + MIN(VBTUR, 0.0)) * FRPAR &
                        + (MIN(VBIRR, 0.0) + MIN(VSEEP, 0.0)) * FRDIS

                !            Downward advection

                FRDIS = FRDISU + FRDOCU
                FRPAR = 1.0 - FRDIS
                PMSA(IP12) = (VSEDI + VBURI + MAX(VBTUR, 0.0)) * FRPAR &
                        + (MAX(VBIRR, 0.0) + MAX(VSEEP, 0.0)) * FRDIS
            ENDIF

            IP5 = IP5 + IN5
            IP6 = IP6 + IN6
            IP7 = IP7 + IN7
            IP8 = IP8 + IN8
            IP9 = IP9 + IN9
            IP10 = IP10 + IN10
            IP11 = IP11 + IN11
            IP12 = IP12 + IN12

        end do

        RETURN
    END

end module m_trase2
