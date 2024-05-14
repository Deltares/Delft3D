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
module m_clcrad
    use m_waq_precision

    implicit none

contains


    SUBROUTINE CLCRAD (PMSA, FL, IPOINT, INCREM, NOSEG, &
            NOFLUX, IEXPNT, IKNMRK, NOQ1, NOQ2, &
            NOQ3, NOQ4)

        !
        !     Function : Calculates the radiation at the surface and at the bottom of the
        !                active water segments
        !

        use m_logger, only : terminate_execution, get_log_unit_number
        use m_evaluate_waq_attribute
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

        REAL(kind = real_wp) :: EXTVL              ! 1  in  total extinction coefficient visible light   (1/m)
        REAL(kind = real_wp) :: DEPTH              ! 2  in  depth of segment                               (m)
        REAL(kind = real_wp) :: RADSURF            ! 3  in  irradiation at the water surface            (W/m2)
        REAL(kind = real_wp) :: A_ENH              ! 4  in  enhancement factor in radiation calculation    (-)
        REAL(kind = real_wp) :: SURF               ! 5  in  horizontal surface                            (m2)
        INTEGER(kind = int_wp) :: SWEMERSION         ! 6  in  switch indicating submersion(0) or emersion (1)(-)
        REAL(kind = real_wp) :: RADBOT             ! 7  loc/out 9 irradiation at the segment lower-boundary   (W/m2)

        !     local decalrations

        INTEGER(kind = int_wp) :: IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, IP9, IP10
        INTEGER(kind = int_wp) :: IP11 ! index pointers in PMSA array
        INTEGER(kind = int_wp) :: IN1, IN2, IN3, IN4, IN5, IN6, IN7, IN8, IN9, IN10
        INTEGER(kind = int_wp) :: IN11 ! increments in PMSA array
        INTEGER(kind = int_wp) :: LUNREP         ! report file
        INTEGER(kind = int_wp) :: ISEG           ! loop counter segment loop
        INTEGER(kind = int_wp) :: IKMRK1         ! first feature inactive(0)-active(1)-bottom(2) segment
        INTEGER(kind = int_wp) :: IK1VN          ! first feature inactive(0)-active(1)-bottom(2) VAN segment
        INTEGER(kind = int_wp) :: IK1NR          ! first feature inactive(0)-active(1)-bottom(2) NAAR segment
        INTEGER(kind = int_wp) :: IK2VN          ! second feature 2D(0)-surface(1)-middle(2)-bottom(3) VAN segment
        INTEGER(kind = int_wp) :: IK2NR          ! second feature 2D(0)-surface(1)-middle(2)-bottom(3) NAAR segment
        INTEGER(kind = int_wp) :: IK             ! loop counter bottom columns
        INTEGER(kind = int_wp) :: IQ             ! loop counter exchanges
        INTEGER(kind = int_wp) :: IVAN           ! segment number from
        INTEGER(kind = int_wp) :: INAAR          ! segment number to
        INTEGER(kind = int_wp) :: IWA1           ! index first water exchange
        INTEGER(kind = int_wp) :: IWA2           ! index last water exchange
        INTEGER(kind = int_wp) :: ITOP           ! index first bottom exhange
        INTEGER(kind = int_wp) :: IBOT           ! index last bottom exhange
        INTEGER(kind = int_wp) :: IWATER         ! segment number water segment
        INTEGER(kind = int_wp) :: IBODEM         ! segment number bottom segment
        REAL(kind = real_wp) :: RADTOP         ! radiation at top
        REAL(kind = real_wp) :: TOTSURF        ! cummulated surface area
        REAL(kind = real_wp) :: REFLEC         ! Reflected fraction of incident sunlight

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

        IF (IP7/=IP9) THEN
            CALL get_log_unit_number(LUNREP)
            WRITE(LUNREP, *) 'Error in CLCRAD: Rad/RadDay/Rad_uv should be an input too!'
            WRITE(LUNREP, *) 'Use the correct proc_def!'
            WRITE(*, *) 'Error in CLCRAD: Rad/RadDay/Rad_uv should be an input too!'
            WRITE(*, *) 'Use the correct proc_def!'
            CALL terminate_execution(1)
        END IF

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


        !.....2DH mode

        IF (NOQ3==0) THEN

            DO ISEG = 1, NOSEG

                CALL evaluate_waq_attribute(1, IKNMRK(ISEG), IKMRK1)

                !........Segment is inactief
                IF      (IKMRK1 == 0) THEN

                    !          RadTop = RadSurf corrected for reflection
                    PMSA(IP9) = PMSA(IP3) * (1. - PMSA(IP8))

                    !          RadBot    = RadSurf corrected for reflection
                    PMSA(IP10) = PMSA(IP3) * (1. - PMSA(IP8))

                    !          atten
                    PMSA(IP11) = 0.0

                    !........Segment is actief watersegment
                ELSE IF (IKMRK1 == 1) THEN

                    !          RadTop    = RadSurf corrected for reflection
                    PMSA(IP9) = PMSA(IP3) * (1. - PMSA(IP8))

                    !          RadBot    = RadSurf   * (1 - reflection) * EXP( -ExtVl    *Depth     )
                    PMSA(IP10) = PMSA(IP3) * (1. - PMSA(IP8)) * EXP(-PMSA(IP1) * PMSA(IP2))

                    !          atten    = ExtVl    *Depth
                    PMSA(IP11) = PMSA(IP1) * PMSA(IP2)

                    !........Segment is actief bodemsegment
                ELSE IF (IKMRK1 == 3) THEN

                    !          RadTop    = 0.0
                    PMSA(IP9) = 0.0

                    !          RadBot    = 0.0
                    PMSA(IP10) = 0.0

                    !          attenuation    = 0.0
                    PMSA(IP11) = 0.0

                ENDIF

                IP1 = IP1 + IN1
                IP2 = IP2 + IN2
                IP3 = IP3 + IN3
                IP4 = IP4 + IN4
                IP5 = IP5 + IN5
                IP6 = IP6 + IN6
                IP8 = IP8 + IN8
                IP9 = IP9 + IN9
                IP10 = IP10 + IN10
                IP11 = IP11 + IN11

            end do

        ELSE

            !.....3D MODE

            DO IQ = NOQ1 + NOQ2 + 1, NOQ1 + NOQ2 + NOQ3

                IVAN = IEXPNT(1, IQ)
                INAAR = IEXPNT(2, IQ)

                IF (IVAN > 0 .AND. INAAR > 0) THEN
                    CALL evaluate_waq_attribute(1, IKNMRK(IVAN), IK1VN)
                    CALL evaluate_waq_attribute(1, IKNMRK(INAAR), IK1NR)
                    CALL evaluate_waq_attribute(2, IKNMRK(IVAN), IK2VN)
                    CALL evaluate_waq_attribute(2, IKNMRK(INAAR), IK2NR)

                    !...........Van segment = inactief
                    IF (IK1VN == 0) THEN

                        !              RadTop = RadSurf corrected for reflection
                        RADTOP = PMSA(IP3 + (IVAN - 1) * IN3) &
                                * (1. - PMSA(IP8 + (IVAN - 1) * IN8))
                        PMSA(IP9 + (IVAN - 1) * IN9) = RADTOP
                        !              RadBot = RadTOP
                        PMSA(IP10 + (IVAN - 1) * IN10) = RADTOP

                        !              atten
                        PMSA(IP11 + (IVAN - 1) * IN11) = 0.0

                        !...........Van segment = actief water segment
                    ELSE IF (IK1VN == 1) THEN

                        !..............Van segment = water segment met surface
                        IF (IK2VN == 1) THEN

                            EXTVL = PMSA(IP1 + (IVAN - 1) * IN1)
                            DEPTH = PMSA(IP2 + (IVAN - 1) * IN2)
                            RADTOP = PMSA(IP3 + (IVAN - 1) * IN3)
                            REFLEC = PMSA(IP8 + (IVAN - 1) * IN8)

                            RADTOP = RADTOP * (1. - REFLEC)
                            RADBOT = RADTOP * EXP(-EXTVL * DEPTH)

                            PMSA(IP9 + (IVAN - 1) * IN9) = RADTOP
                            PMSA(IP9 + (INAAR - 1) * IN9) = RADBOT
                            PMSA(IP10 + (IVAN - 1) * IN10) = RADBOT
                            PMSA(IP11 + (IVAN - 1) * IN11) = EXTVL * DEPTH

                        ENDIF

                        !..............Van segment = water segment zonder surface of bodem
                        IF (IK2VN == 2) THEN

                            EXTVL = PMSA(IP1 + (IVAN - 1) * IN1)
                            DEPTH = PMSA(IP2 + (IVAN - 1) * IN2)
                            RADTOP = PMSA(IP9 + (IVAN - 1) * IN9)

                            RADBOT = RADTOP * EXP(-EXTVL * DEPTH)

                            PMSA(IP9 + (INAAR - 1) * IN9) = RADBOT
                            PMSA(IP10 + (IVAN - 1) * IN10) = RADBOT
                            PMSA(IP11 + (IVAN - 1) * IN11) = EXTVL * DEPTH

                        ENDIF

                    ENDIF

                    !...........Naar segment = inactief
                    IF (IK1NR == 0) THEN

                        !              RadTop = RadSurf
                        PMSA(IP9 + (INAAR - 1) * IN9) = PMSA(IP3 + (INAAR - 1) * IN3)

                        !              RadBot = Radsurf
                        PMSA(IP10 + (INAAR - 1) * IN10) = PMSA(IP3 + (INAAR - 1) * IN3)

                        PMSA(IP11 + (INAAR - 1) * IN11) = 0.0

                        !...........Naar segment = actief water segment
                    ELSE IF (IK1NR == 1) THEN

                        !...........Naar segment = water segment met bodem
                        IF (IK2NR == 3) THEN

                            EXTVL = PMSA(IP1 + (INAAR - 1) * IN1)
                            DEPTH = PMSA(IP2 + (INAAR - 1) * IN2)
                            RADTOP = PMSA(IP7 + (INAAR - 1) * IN7)

                            RADBOT = RADTOP * EXP(-EXTVL * DEPTH)

                            PMSA(IP10 + (INAAR - 1) * IN10) = RADBOT
                            PMSA(IP11 + (INAAR - 1) * IN11) = EXTVL * DEPTH

                        ENDIF

                    ENDIF
                ENDIF

            end do

        ENDIF

        !     the sediment columns

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

        DO IK = 1, Coll%current_size

            IWA1 = Coll%set(IK)%fstwatsed
            IWA2 = Coll%set(IK)%lstwatsed
            ITOP = Coll%set(IK)%topsedsed
            IBOT = Coll%set(IK)%botsedsed

            !         average RAD at water-sediment interface

            RADTOP = 0.0
            TOTSURF = 0.0
            DO IQ = IWA1, IWA2
                IWATER = IEXPNT(1, IQ)
                IBODEM = IEXPNT(2, IQ)
                RADSURF = PMSA(IP3 + (IWATER - 1) * IN3)
                SURF = PMSA(IP5 + (IWATER - 1) * IN5)
                SWEMERSION = NINT(PMSA(IP6 + (IWATER - 1) * IN6))
                RADBOT = PMSA(IP10 + (IWATER - 1) * IN10)
                IF (SWEMERSION == 1) THEN
                    RADTOP = RADTOP + RADSURF * SURF
                ELSE
                    RADTOP = RADTOP + RADBOT * SURF
                ENDIF
                TOTSURF = TOTSURF + SURF
            ENDDO
            A_ENH = PMSA(IP4 + (IWATER - 1) * IN4)
            RADTOP = RADTOP * A_ENH / TOTSURF

            !         extinction over the layers of the column

            DO IQ = ITOP, IBOT
                IBODEM = IEXPNT(1, IQ)
                EXTVL = PMSA(IP1 + (IBODEM - 1) * IN1)
                DEPTH = PMSA(IP2 + (IBODEM - 1) * IN2)
                IF (RADTOP < 1.E-10) THEN
                    RADBOT = 0.0
                ELSE
                    RADBOT = RADTOP * EXP(-EXTVL * DEPTH)
                ENDIF
                PMSA(IP9 + (IBODEM - 1) * IN9) = RADTOP
                PMSA(IP10 + (IBODEM - 1) * IN10) = RADBOT
                RADTOP = RADBOT
            ENDDO

        ENDDO

        RETURN
    END

end module m_clcrad
