!!  Copyright (C)  Stichting Deltares, 2012-2026.
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
module m_sedim
    use m_waq_precision

    implicit none

contains


    subroutine sedim  (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        !>\file
        !>       Sedimentation routine used for IMx

        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !        SEDIMENTATION FORMULATIONS
        !        MODULE VALID FOR IM, IM2, IM3
        !
        ! Name    T   L I/O   Description                                    Units
        ! ----    --- -  -    -------------------                            -----
        ! CONC    R*4 1 I  concentration sedimenting material water        [gX/m3]
        ! DEPTH   R*4 1 I  DELWAQ depth                                        [m]
        ! FL (1)  R*4 1 O  sedimentation flux (water->mixinglayer)       [gX/m3/d]
        ! MINDEP  R*4 1 I  minimal depth for sedimentation                     [m]
        ! PSED    R*4 1 L  sedimentaion probability (0 - 1)                    [-]
        ! POTSED  R*4 1 L  potential sedimentation flux                  [gX/m2/d]
        ! TAU     R*4 1 I  calculated sheerstress                        [kg/m/s2]
        ! TAUVEL  R*4 1 I  total velocity calcualted from tau                [m/s]
        ! TCRSED  R*4 1 I  critical sheerstress sedimentation            [kg/m/s2]
        ! VCRSED  R*4 1 I  critical velocity sedimentation                   [m/s]
        ! VSED    R*4 1 O  first order sedimentaion rate (calculated)        [m/d]
        ! ZERSED  R*4 1 I  zeroth order sedimentation flux               [gX/m2/d]

        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        use m_extract_waq_attribute
        USE BottomSet     !  Module with definition of the waterbottom segments

        REAL(kind = real_wp) :: process_space_real  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir

        REAL(kind = real_wp) :: MINDEP, MINDE2, DEPTH, DEPTH2

        REAL(kind = real_wp) :: PSEDMIN

        real(kind = real_wp), parameter :: seconds_per_day = 86400.0

        integer :: ip1,  ip2,  ip3,  ip4,  ip5,  ip6,  ip7,  ip8,  ip9,  ip10, &
                   ip11, ip12, ip13, ip14, ip15, ip16, ip17, ip18, ip19, ip20, ip21, &
                   in1,  in2,  in3,  in4,  in5,  in6,  in7,  in8,  in9,  in10, &
                   in11, in12, in13, in14, in15, in16, in17, in18, in19, in20, in21
        integer :: ivan, inaar, ip, ipv, ipn, ipq, iq, iseg, iflux, ikmrkv, ikmrkn, iwa1, iwa2, ikmrk1, ikmrk2, ik, iwater
        real(kind = real_wp) :: conc, zersed, vsed, tau, tcrsed, delt, psed, alpha, p, pmax, maxsed, potsed
        real(kind = real_wp) :: flowrate, volume, surf

        IP1 = IPOINT(1)       ! Concentration inorganic matter
        IP2 = IPOINT(2)       ! Zeroth-order flux -- at all useful?
        IP3 = IPOINT(3)       ! Sedimentation velocity
        IP4 = IPOINT(4)       ! Bottom shear stress
        IP5 = IPOINT(5)       ! Critical shear stress for sedimentation
        IP6 = IPOINT(6)       ! Depth of the segments
        IP7 = IPOINT(7)       ! Time step (for limiting the deposition flux)
        IP8 = IPOINT(8)       ! Minimum depth for sedimentation/resuspension -- obsolete!
        IP9 = IPOINT(9)       ! Fraction going directly to layer S2
        IP10 = IPOINT(10)     ! Fraction total inorganic matter (TIM) in layer S2
        IP11 = IPOINT(11)     ! Maximum allowable fraction (TIM) in layer S2
        IP12 = IPOINT(12)     ! Minimum sedimentation probability (force sedimentation) -- useful?
        IP13 = IPOINT(13)     ! Volume of the segments
        IP14 = IPOINT(14)     ! Workspace (input), used for computing maximum allowed flux
        IP15 = IPOINT(15)     ! (exchange) sedimentation velocity per exchange
        IP16 = IPOINT(16)     ! (exchange) Flow rate, used for estimating the influx
        IP17 = IPOINT(17)     ! (output) Sedimentation probability (used for adsorbed substances)
        IP18 = IPOINT(18)     ! (output) Sedimentation flux to layer S1
        IP19 = IPOINT(19)     ! (output) Sedimentation flux to layer S2
        IP20 = IPOINT(20)     ! Workspace (output), storing maximum allowed flux
        IP21 = IPOINT(21)     ! (additional velocity) Sedimentation velocity

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
        IN13 = INCREM(13)
        IN14 = INCREM(14)
        IN15 = INCREM(15)
        IN16 = INCREM(16)
        IN17 = INCREM(17)
        IN18 = INCREM(18)
        IN19 = INCREM(19)
        IN20 = INCREM(20)
        IN21 = INCREM(21)

        !
        ! Determine the mass influx to be expected for the cells near the bottom
        !
        ! Note: this does not work properly for floating material!
        !
        !

        do iseg = 1,num_cells
            ip = ipoint(14) + (iseg - 1) * increm(14)
            process_space_real(ip) = 0.0
        enddo

        !
        ! Limit the estimation to the vertical exchanges
        !
        DO IQ = num_exchanges_u_dir + num_exchanges_v_dir +1, num_exchanges_u_dir + num_exchanges_v_dir + num_exchanges_z_dir

            IVAN     = IEXPNT(1, IQ)
            INAAR    = IEXPNT(2, IQ)

            ipv      = ipoint(14) + (ivan  - 1) * increm(14)
            ipn      = ipoint(14) + (inaar - 1) * increm(14)
            ipq      = ipoint(16) + (iq    - 1) * increm(16)
            flowrate = process_space_real(ipq)

            IKMRKV = 1
            IKMRKN = 1
            IF (IVAN > 0 ) THEN
                CALL extract_waq_attribute(1, IKNMRK(IVAN), IKMRKV)
                IF ( IKMRKV /= 0 ) THEN
                    CALL extract_waq_attribute(2, IKNMRK(IVAN), IKMRKV)
                 ENDIF
            ENDIF
            IF (INAAR > 0 ) THEN
                CALL extract_waq_attribute(1, IKNMRK(INAAR), IKMRKN)
                IF ( IKMRKN /= 0 ) THEN
                    CALL extract_waq_attribute(2, IKNMRK(INAAR), IKMRKN)
                ENDIF
            ENDIF

            !
            ! Note: including the horizontal exchanges fails, unclear why.
            ! Hence commented out.
            ! It may still be useful (when repaired) to refine the estimate of
            ! how much sediment can be deposited per timestep.
            !
            ! Handle "from" segment
            !
            !IF ( (IKMRKV == 0 .OR. IKMRKV == 3 ) ) THEN
            !    if ( inaar > 0 ) then
            !        ip1 = ipoint(1) + (inaar-1) * increm(1)
            !    else
            !        ! Use the "to" segment as a proxy
            !        ip1 = ipoint(1) + (ivan-1) * increm(1)
            !    endif
            !
            !    conc = process_space_real(ip1)
            !
            !    process_space_real(ipv) = process_space_real(ipv) + max( 0.0, -flowrate * max( 0.0, conc )  )
            !
            !ENDIF

            !
            ! Handle "to" segment
            !
            !IF ( (IKMRKN == 0 .OR. IKMRKN == 3 ) ) THEN
            !    if ( ivan > 0 ) then
            !        ip1 = ipoint(1) + (ivan-1) * increm(1)
            !    else
            !        ip1 = ipoint(1) + (inaar-1) * increm(1)
            !    endif
            !
            !    conc = process_space_real(ip1)
            !
            !    process_space_real(ipn) = process_space_real(ipn) + max( 0.0, flowrate * max( 0.0, conc ) )
            !
            !ENDIF

            ! If the exchange is a vertical exchange, also include the settling from the cell above

            if ( iq > num_exchanges_u_dir + num_exchanges_v_dir ) then
                if ( ivan > 0 .and. inaar > 0 ) then
                    ip1    = ipoint(1)  + (ivan-1) * increm(1)
                    ip3    = ipoint(3)  + (ivan-1) * increm(3)
                    ip6    = ipoint(6)  + (ivan-1) * increm(6)
                    ip13   = ipoint(13) + (ivan-1) * increm(13)
                    conc   = process_space_real(ip1)
                    vsed   = process_space_real(ip3) / seconds_per_day   ! Conversion from m/day to m/s
                    depth  = process_space_real(ip6)
                    volume = process_space_real(ip13)
                    if ( depth > 0.0 ) then
                        surf   = volume / depth
                    else
                        surf   = 0.0
                    endif
!!                    process_space_real(ipn) = process_space_real(ipn) + max( 0.0, -vsed * surf * max( 0.0, conc ) )
                    process_space_real(ipn) = max( 0.0, -vsed * surf * max( 0.0, conc ) )

                    ip1    = ipoint(1)  + (inaar-1) * increm(1)
                    ip3    = ipoint(3)  + (inaar-1) * increm(3)
                    ip6    = ipoint(6)  + (inaar-1) * increm(6)
                    ip13   = ipoint(13) + (inaar-1) * increm(13)
                    conc   = process_space_real(ip1)
                    vsed   = process_space_real(ip3) / seconds_per_day   ! Conversion from m/day to m/s
                    depth  = process_space_real(ip6)
                    volume = process_space_real(ip13)
                    if ( depth > 0.0 ) then
                        surf   = volume / depth
                    else
                        surf   = 0.0
                    endif
!!                    process_space_real(ipv) = process_space_real(ipv) + max( 0.0, vsed * surf * max( 0.0, conc ) )
                    process_space_real(ipv) = max( 0.0, vsed * surf * max( 0.0, conc ) )

                endif
            endif
        end do

        !
        ! Properly scale the maximum flux
        !
        do iseg = 1,num_cells
            ip  = ipoint(14) + (iseg - 1) * increm(14)
            ipv = ipoint(13) + (iseg - 1) * increm(13)
            volume = process_space_real(ip13)

            if ( volume > 0.0 ) then
                process_space_real(ip) = seconds_per_day * process_space_real(ip) / volume
            else
                process_space_real(ip) = 0.0
            endif
        enddo

        IFLUX = 0
        IP1   = IPOINT(1)
        IP3   = IPOINT(3)
        IP6   = IPOINT(6)
        IP13  = IPOINT(13)
        DO ISEG = 1, num_cells

            !     zero output

            process_space_real (IP17) = 0.0
            process_space_real (IP18) = 0.0
            process_space_real (IP19) = 0.0

            !     sedimentation towards the bottom

            CALL extract_waq_attribute(1, IKNMRK(ISEG), IKMRK1)
            IF (IKMRK1==1) THEN
                CALL extract_waq_attribute(2, IKNMRK(ISEG), IKMRK2)
                IF ((IKMRK2==0).OR.(IKMRK2==3)) THEN
                    !
                    CONC = MAX (0.0, process_space_real(IP1))
                    ZERSED = process_space_real(IP2)
                    VSED = MAX (0.0, process_space_real(IP3))    ! Avoid inadvertent source if VSED negative (Delft3D-35562)
                    TAU = process_space_real(IP4)
                    TCRSED = process_space_real(IP5)
                    DEPTH = process_space_real(IP6)
                    DELT = process_space_real(IP7)
                    MINDEP = process_space_real(IP8)
                    ALPHA = process_space_real(IP9)
                    P = process_space_real(IP10)
                    PMAX = process_space_real(IP11)
                    PSEDMIN = process_space_real(IP12)

                    !***********************************************************************
                    !**** Processes connected to the SEDIMENTATION
                    !***********************************************************************

                    !     if fraction IM1 in second layer P > PMAX then ALPHA = 0 meaning no sedimentations towards S2

                    IF (P >= PMAX) THEN
                        ALPHA = 0.0
                    ENDIF

                    !     Calculate sedimenation probability

                    IF (TAU == -1.0) THEN
                        PSED = 1.0
                    ELSEIF (TCRSED < 1E-20)  THEN
                        PSED = 0.0
                    ELSE
                        !         vergelijking met critische schuifspanning
                        PSED = MAX (0.0, (1.0 - TAU / TCRSED))
                    ENDIF
                    PSED = MAX(PSEDMIN, PSED)

                    !     Calculate potential sedimentation fluxes
                    !     No sedimentation when depth below min depth

                    IF (DEPTH < MINDEP) THEN
                        MAXSED = 0.0
                        FL(1 + IFLUX) = 0.0
                        FL(2 + IFLUX) = 0.0
                    ELSE
                        POTSED = ZERSED + (VSED * CONC) * PSED

                        !        limit sedimentation to available mass (M/L2/DAY)
                        !!MAXSED = MIN (POTSED, CONC / DELT * DEPTH)
                        MAXSED = MIN (POTSED, (process_space_real(ip14) + conc / delt) * depth )

                        !        convert sedimentation to flux in M/L3/DAY
                        FL(1 + IFLUX) = MAXSED * (1. - ALPHA) / DEPTH
                        FL(2 + IFLUX) = MAXSED * ALPHA / DEPTH
                    ENDIF

                    !     Output of calculated sedimentation rate
                    process_space_real (IP17) = PSED
                    process_space_real (IP18) = MAXSED * (1. - ALPHA)
                    process_space_real (IP19) = MAXSED * ALPHA
                    !
                ENDIF
            ENDIF
            !
            IFLUX = IFLUX + NOFLUX
            IP1 = IP1 + IN1
            IP2 = IP2 + IN2
            IP3 = IP3 + IN3
            IP4 = IP4 + IN4
            IP5 = IP5 + IN5
            IP6 = IP6 + IN6
            IP7 = IP7 + IN7
            IP8 = IP8 + IN8
            IP9 = IP9 + IN9
            IP10 = IP10 + IN10
            IP11 = IP11 + IN11
            IP12 = IP12 + IN12
            IP14 = IP14 + IN14
            IP15 = IP15 + IN15
            IP16 = IP16 + IN16
            IP17 = IP17 + IN17
            IP18 = IP18 + IN18
            IP19 = IP19 + IN19
        end do
        !
        IP1 = IPOINT(1)
        IP6 = IPOINT(6)
        IP8 = IPOINT(8)
        IP18 = IPOINT(18)

        !.....Exchangeloop over de horizontale richting
        DO IQ = 1, num_exchanges_u_dir + num_exchanges_v_dir

            process_space_real(IP21) = 0.0

            IP21 = IP21 + IN21

        end do

        IP15 = IP15 + (num_exchanges_u_dir + num_exchanges_v_dir) * IN15

        !.....Exchangeloop over de verticale richting
        DO IQ = num_exchanges_u_dir + num_exchanges_v_dir + 1, num_exchanges_u_dir + num_exchanges_v_dir + num_exchanges_z_dir + num_exchanges_bottom_dir

            IVAN = IEXPNT(1, IQ)
            INAAR = IEXPNT(2, IQ)

            IF (IVAN > 0 .AND. INAAR > 0) THEN

                !           Zoek eerste kenmerk van- en naar-segmenten

                CALL extract_waq_attribute(1, IKNMRK(IVAN), IKMRKV)
                CALL extract_waq_attribute(1, IKNMRK(INAAR), IKMRKN)
                IF (IKMRKV==1.AND.IKMRKN==3) THEN

                    !               Bodem-water uitwisseling: NUL FLUX OM OOK OUDE PDF's
                    !                                         TE KUNNEN GEBRUIKEN
                    !               Snelheid behoeft niet gezet (gebeurt in TRASED)

                    !               MAXSED = process_space_real (IP11+(IVAN-1)*IN11)
                    !               CONC   = MAX (1E-20, process_space_real(IP1+(IVAN-1)*IN1) )
                    !               process_space_real(IP17) = MAXSED/86400./CONC
                    FL (1 + (IVAN - 1) * NOFLUX) = 0.0

                ELSEIF (IKMRKV==1.AND.IKMRKN==1) THEN

                    !               Water-water uitwisseling
                    !rs             merk op: sedimentatie tussen waterlagen: geen taucr correctie,
                    !rs             alleen conversie van 1/d naar 1/s. Ten overvloede:
                    !rs             scu (s) en aux-timer (d) liggen dus vast!

                    DEPTH = process_space_real(IP6 + (IVAN - 1) * IN6)
                    DEPTH2 = process_space_real(IP6 + (INAAR - 1) * IN6)
                    MINDEP = process_space_real(IP8 + (IVAN - 1) * IN8)
                    MINDE2 = process_space_real(IP8 + (INAAR - 1) * IN8)
                    IF (DEPTH > MINDEP .AND. DEPTH2 > MINDE2) THEN
                        process_space_real(IP21) = process_space_real(IP15) / seconds_per_day
                    ELSE
                        process_space_real(IP21) = 0.0
                    ENDIF
                ELSE
                    process_space_real(IP21) = 0.0
                ENDIF

            ENDIF

            IP15 = IP15 + IN15
            IP21 = IP21 + IN21

        end do

        !     Handle velocity to the delwaq-g bottom

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
        IP13 = IPOINT(13)
        IP14 = IPOINT(14)
        IP15 = IPOINT(15)
        IP16 = IPOINT(16)
        IP17 = IPOINT(17)
        IP18 = IPOINT(18)
        IP19 = IPOINT(19)
        IP20 = IPOINT(20)
        IP21 = IPOINT(21)

        DO IK = 1, Coll%current_size

            IWA1 = Coll%set(IK)%fstwatsed
            IWA2 = Coll%set(IK)%lstwatsed

            DO IQ = IWA1, IWA2
                IWATER = IEXPNT(1, IQ)

                CONC = MAX (0.0, process_space_real(IP1 + (IWATER - 1) * IN1))
                ZERSED = process_space_real(IP2 + (IWATER - 1) * IN2)
                VSED = MAX (0.0, process_space_real(IP3 + (IWATER - 1) * IN3))
                TAU = process_space_real(IP4 + (IWATER - 1) * IN4)
                TCRSED = process_space_real(IP5 + (IWATER - 1) * IN5)
                DEPTH = process_space_real(IP6 + (IWATER - 1) * IN6)
                DELT = process_space_real(IP7 + (IWATER - 1) * IN7)
                MINDEP = process_space_real(IP8 + (IWATER - 1) * IN8)

                !           Calculate sedimenation probability

                IF (TAU == -1.0) THEN
                    PSED = 1.0
                ELSEIF (TCRSED < 1E-20)  THEN
                    PSED = 0.0
                ELSE
                    !               vergelijking met critische schuifspanning
                    PSED = MAX (0.0, (1.0 - TAU / TCRSED))
                ENDIF

                !           Bereken de potentiele sedimentatie fluxen
                !           Geen sedimentatie onder een minimale diepte

                IF (DEPTH < MINDEP) THEN
                    MAXSED = 0.0
                ELSE
                    POTSED = ZERSED + (VSED * CONC) * PSED

                    !              sedimenteer maximaal de aanwezige hoeveelheid (M/L2/DAY)
                    MAXSED = MIN (POTSED, CONC / DELT * DEPTH)

                ENDIF

                IF (CONC > 1.E-10) THEN
                    process_space_real(IP21 + (IQ - 1) * IN21) = MAXSED / 86400. / CONC
                ENDIF

            ENDDO

        ENDDO
        !
        RETURN
    END

end module m_sedim
