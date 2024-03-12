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
module m_trsoxy
    use m_waq_precision

    implicit none

contains


    SUBROUTINE TRSOXY (PMSA, FL, IPOINT, INCREM, NOSEG, &
            NOFLUX, IEXPNT, IKNMRK, NOQ1, NOQ2, &
            NOQ3, NOQ4)

        !***********************************************************************
        !
        !     Function : Extra rearation flux towards sediment Drying and Flooding
        !
        !***********************************************************************

        use m_advtra
        USE BottomSet     !  Module with definition of the waterbottom segments

        IMPLICIT NONE

        !     arguments

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

        !     from PMSA array

        INTEGER(kind = int_wp) :: SWEMERSION         ! 1  in  switch indicating submersion(0) or emersion (1)
        REAL(kind = real_wp) :: OXY                ! 2  in  dissolved oxygen concentration
        REAL(kind = real_wp) :: OXYSAT             ! 3  in  dissolved oxygen saturation concentration
        REAL(kind = real_wp) :: DEPTH              ! 4  in  depth of a segment
        REAL(kind = real_wp) :: AUXSYS             ! 5  in  auxsys conversion from system timer to day
        REAL(kind = real_wp) :: VDOWN              ! 6  in  downward velocity
        REAL(kind = real_wp) :: CORFLX             ! 7  out correction flux

        !     local decalrations

        INTEGER(kind = int_wp) :: IP1, IP2, IP3, IP4, IP5 ! index pointer in PMSA array
        INTEGER(kind = int_wp) :: IP6, IP7             ! index pointer in PMSA array
        INTEGER(kind = int_wp) :: IN1, IN2, IN3, IN4, IN5 ! increment in PMSA array
        INTEGER(kind = int_wp) :: IN6, IN7             ! increment in PMSA array
        INTEGER(kind = int_wp) :: ISEG                ! loop counter segment loop
        INTEGER(kind = int_wp) :: IK                  ! loop counter bottom columns
        INTEGER(kind = int_wp) :: IQ                  ! loop counter exchanges
        INTEGER(kind = int_wp) :: IWA1                ! index first water exchange
        INTEGER(kind = int_wp) :: IWA2                ! index last water exchange
        INTEGER(kind = int_wp) :: IVAN                ! index from segment in exchange
        INTEGER(kind = int_wp) :: INAAR               ! index to segment in exchange

        !     initialise bottom if necessary

        CALL MAKKO2 (IEXPNT, IKNMRK, NOQ1, NOQ2, NOQ3, &
                NOQ4)

        IP1 = IPOINT(1)
        IP2 = IPOINT(2)
        IP3 = IPOINT(3)
        IP4 = IPOINT(4)
        IP5 = IPOINT(5)
        IP6 = IPOINT(6)
        IP7 = IPOINT(7)
        !
        IN1 = INCREM(1)
        IN2 = INCREM(2)
        IN3 = INCREM(3)
        IN4 = INCREM(4)
        IN5 = INCREM(5)
        IN6 = INCREM(6)
        IN7 = INCREM(7)

        !     zero the output

        DO ISEG = 1, NOSEG
            PMSA(IP7) = 0.0
            IP7 = IP7 + IN7
        ENDDO
        IP7 = IPOINT(7)

        !     Loop over kolommen

        DO IK = 1, Coll%cursize

            !        Select first column of exchanges for DOWNWARD advection, sediment water exchanges only

            IWA1 = Coll%set(IK)%fstwatsed
            IWA2 = Coll%set(IK)%lstwatsed

            DO IQ = IWA1, IWA2

                IVAN = IEXPNT(1, IQ)
                INAAR = IEXPNT(2, IQ)

                SWEMERSION = NINT(PMSA(IP1 + (IVAN - 1) * IN1))

                IF (SWEMERSION == 1) THEN

                    OXY = PMSA(IP2 + (IVAN - 1) * IN2)
                    OXYSAT = PMSA(IP3 + (IVAN - 1) * IN3)
                    DEPTH = PMSA(IP4 + (INAAR - 1) * IN4)
                    AUXSYS = PMSA(IP5 + (IVAN - 1) * IN5)
                    VDOWN = PMSA(IP6 + (IQ - 1) * IN6)

                    !              coorection flux is equal to saturated velocity flux minus actual velocity flux, scaled for time and volume

                    CORFLX = VDOWN * (OXYSAT - OXY) * AUXSYS / DEPTH

                    PMSA(IP7 + (INAAR - 1) * IN7) = PMSA(IP7 + (INAAR - 1) * IN7) + CORFLX
                    FL(1 + (INAAR - 1) * NOFLUX) = FL(1 + (INAAR - 1) * NOFLUX) + CORFLX

                ENDIF

            ENDDO

        ENDDO

        RETURN
    END

end module m_trsoxy
