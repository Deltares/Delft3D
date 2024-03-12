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
module m_d40blo
    use m_waq_precision
    use m_set_effi

    implicit none

contains


    subroutine d40blo (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        !>\file
        !>       BLOOM II algae module

        !
        !     Description of the module :
        !
        ! Name    T   L I/O   Description                                   Units
        ! ----    --- -  -    -------------------                            ----

        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        ! 3DL
        use m_bloom_3dl
        use m_blinit
        use m_iblbal
        use m_blprim
        use m_bloutc
        use m_blmort
        use m_blinpu
        use m_blfile
        use m_blclmort
        use m_srstop
        use m_monsys
        use m_dhnoseg
        use m_dhnolay
        use m_evaluate_waq_attribute
        use      bloom_data_3dl
        use      bloom_data_vtrans
        ! END3DL

        implicit none

        real(kind = real_wp) :: pmsa  (*), fl    (*)
        integer(kind = int_wp) :: ipoint(*), increm(*), noseg, noflux
        integer(kind = int_wp) :: iexpnt(4, *), iknmrk(*), iq, ifrom, ito, noq1, noq2, noq3, noq4

        !
        !     Local (species groups arrays are now dimensioned as species/types arrays)
        !
        !     Name    Type  Length   I/O  Description

        !     ALGDM   R     1             Dry matter in algae (gDM/m3)
        !     ALGTYP  R     0:20,NTYP     Algae type properties
        !     AMMONI  R     1             Ammonium concentration (gN/m3)
        !     BIOMAS  R     NTYP          Species biomass (gC/m3)
        !     BLDEP   R     1             Bloomdepth (DEPTH averaged over BLSTEP)
        !     BLSTEP  R*4   1             Time step Bloom (days)
        !     CHLORO  R     1             Total chlorophyl in algae (mg/m3)
        !     CGROUP  R     NGRO          Algae species group biomass (gC/m3)
        !     CL      R     1             Chlorinity (gCl/m3)
        !     DEPTHW  R     1             Depth (m)
        !     DAYLEN  R     1             Day length (h)
        !     DELTAT  R     1             Time step DELWAQ (d)
        !     DEAT4   R*4   1             ??$Necessity to transfer?$
        !     EXTALG  R     1             Extinction by algae (1/m)
        !     EXTTOT  R     1             Total extinction (1/m)
        !     FAUT    R     NTYP          Fraction autolysis per species (-)
        !     FDET    R     NTYP          Fraction detritus per species (-)
        !     FL(IFPROD)    NTYP_A        Primary production per type (g/m3/d)
        !     FL(IFMORT)    NTYP_A        Mortality per type (g/m3/d)
        !     FL(IFAUTO)    4             Autolysis fluxes per nutrient (g/m3/d)
        !                                 (C, N, P, Si)
        !     FL(IFDETR)    4             Detritus production per nutrient
        !                                 (C, N, P, Si)  (g/m3/d)
        !     FL(IFOOXP)    4             OOx production
        !                                 (C, N, P, Si)  (g/m3/d)
        !     FL(IFUPTA)    5             Uptake of nutrients
        !                                 (CO2, NH4, NO3, PO4, SiOx)  (g/m3/d)
        !     FRAMMO  R     1             Fraction of NH4 in N-Uptake (-)
        !     FBOD5   R     1             BOD5/BODinf in algae (-)
        !                                 an history output timestep
        !     ID      I     1             Week number (-)
        !     IFIX    I     NTYP          Flag indicating fixed (attached, immobile algae)
        !     ISWVTR  I     1             Switch if 3DL is to be used
        !     LIMFAC  R     6             Limiting factors (-)
        !     MRTM1   I     NTYP          Mortality parameter
        !     MRTM2   I     NTYP          Mortality parameter
        !     MRTB1   I     NTYP          Mortality parameter
        !     MRTB2   I     NTYP          Mortality parameter
        !     NTYP_A  I     1             Actual number of types
        !     NTYP_M  I     1             Limit number of types
        !     NGRO_A  I     1             Actual number of groups
        !     NGRO_M  I     1             Limit number of groups
        !     NSET    I     1             Counter for subroutine SETABC of BLOOM II
        !     NITRAT  R     1             Nitrate (gN/m3)
        !     NUPTAK  R     1             N-Uptake (gN/m3/d)
        !     PHOSPH  R     1             Phosphate (gP/m3)
        !     RATGRO  R     NGRO          Effective growth rate per group (1/d)
        !     RATMOR  R     NGRO          Effective mortality per group (1/d)
        !     RADIAT  R     1             Irradiation (W/m2)
        !     SILICA  R     1             Silicate (gSi/m3)
        !     SWCLIM  I     1             Carbon limitation switch (0 inactive, 1 active)
        !     TIMMUL  R     1             Time step multiplyer Bloom call (-)
        !     TEMPER  R     1             Temperature (degrees C)
        !     TOTNUT  R     4             C, N, P, Si in algae (gX/m3)
        !     NUTCON  I*4   8             Nutrients involved in active nutrient constraints
        !     FLXCON  I*4   8             Uptake fluxes involved in active nutrient constra
        !     SWBLSA  I     1             Switch for BLOOM stand alone (0=no, 1=yes)     (-)
        !     TotNin  R     1             Total nitrogen for BLOOM stand alone        (g/m3)
        !     TotPin  R     1             Total phosphorous for BLOOM stand alone     (g/m3)
        !     TotSIin R     1             Total silicium for BLOOM stand alone        (g/m3)
        integer(kind = int_wp) :: ntyp_m, nipfix, nipvar, noutlim, nunucom, nopfix
        parameter (ntyp_m = 30)
        !     NIPFIX      Nr of input items independent of BLOOM types, preceding BLOOM types input
        !     NIPVAR      Nr of input items for BLOOM types
        parameter (nipfix = 40, nipvar = 27)
        parameter (nopfix = 29)
        parameter (nunucom = 8)
        parameter (noutlim = nunucom + 2 + 2 * ntyp_m)
        real(kind = real_wp) :: biomas(ntyp_m), faut  (ntyp_m), fdet  (ntyp_m), &
                algtyp(0:20, ntyp_m), mrtm1(ntyp_m), mrtm2(ntyp_m), &
                mrtb1(ntyp_m), mrtb2(ntyp_m), cgroup(ntyp_m)
        integer(kind = int_wp) :: ifix(ntyp_m)
        real(kind = real_wp) :: ratgro(ntyp_m), ratmor(ntyp_m)
        logical  lmixo, lfixn, lcarb
        integer(kind = int_wp) :: ntyp_a, ngro_a, &
                nset, id
        real(kind = real_wp) :: timmul, temper, radiat, depthw, depth, daylen, &
                ammoni, nitrat, phosph, silica, deltat, blstep, &
                exttot, deat4, nuptak, frammo, fbod5, extalg, &
                chloro, totnut(4), algdm, &
                thrnh4, thrno3, thrpo4, thrsi, rcresp, tcresp, &
                bldep, cl, tic, co2, co2lim, effin, &
                ppmco2, detn, detp, rdcnt, sdmixn, volume
        real(kind = real_wp) :: limfac(6)
        integer(kind = int_wp) :: ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8, ip9, &
                ip10, ip11, ip12, ip13, ip14, ip15, ip16, ip17, ip18, &
                ip19, ip20, ip21, ip22, ip23, ip24, ip25, ip26, ip27, &
                ip28, ip29, ip30, ip31, ip32
        integer(kind = int_wp) :: io(nopfix)
        integer(kind = int_wp) :: nosegw, nolay, nosegl, ikmrk1, ikmrk2
        integer(kind = int_wp) :: ipo17, ipo18, ipo19
        integer(kind = int_wp) :: ino17, ino18, ino19
        integer(kind = int_wp) :: init, iflux, iseg, ialg, ioff, ip, igro
        integer(kind = int_wp) :: ifauto, ifdetr, ifooxp, ifupta, ifprod, ifmort
        integer(kind = int_wp) :: iswvtr
        integer(kind = int_wp) :: swbloomout
        integer(kind = int_wp) :: swclim
        integer(kind = int_wp) :: lunrep
        character cdummy
        real(kind = dp) :: org_availn(ntyp_m)
        integer(kind = int_wp) :: nutcon(nunucom), flxcon(nunucom), con2out(nunucom)
        real(kind = real_wp) :: outlim(noutlim)
        real(kind = real_wp) :: TotNin, TotPin, TotSIin
        integer(kind = int_wp) :: swblsa

        !     Former D09 input
        integer(kind = int_wp) :: SWBlSolInt           ! Switch for solar irradiation as total radiation (0) or PAR (1)
        integer(kind = int_wp) :: SWBlObject           ! Switch for objective growth (1) or biomass (0)
        real(kind = real_wp) :: BlTemLim             ! Minimum temperature for growth
        real(kind = real_wp) :: BlBasMor             ! Base mortality when temperature is below minimum temperature for growth
        integer(kind = int_wp) :: SWBlGroChk           ! Switch to use extra constraints on growth rates
        real(kind = real_wp) :: BlBioBas             ! Base biomass level per group
        integer(kind = int_wp) :: SWBlMorChk           ! Switch to use extra mortality constraints
        real(kind = real_wp) :: BlTopLev             ! Top level of mortality constraints
        integer(kind = int_wp) :: SWBlOutput           ! Switch to BLOOM debug output (possible to set this on (1) or off(0) per segment and in time)

        !     JVB much more variables needs to be saved, for the time being all
        !
        !     SAVE     INIT,RDCNT,ID
        save
        !
        data     init   / 1 /
        data     nset   / 0 /
        !
        if (init == 1) then
            init = 0

            !        Open autonomous I/O files Bloom
            call getmlu(lunrep)
            call blfile(lunrep)

            timmul = pmsa(ipoint(1))
            deltat = pmsa(ipoint(19))
            blstep = timmul * deltat
            rdcnt = - blstep
            id = 0
            swclim = nint(pmsa(ipoint(28)))
            if (increm(28)/=0) call blstopinit(lunrep, 'SWCLim')
            lcarb = .false.
            if (swclim>0) lcarb = .true.
            swblsa = pmsa(ipoint(29))
            if (increm(29)/=0) call blstopinit(lunrep, 'SWBlSA')

            !        Former D09 input
            SWBlSolInt = nint(pmsa(ipoint(33)))
            if (increm(33)/=0) call blstopinit(lunrep, 'SWBlSolInt')
            SWBlObject = nint(pmsa(ipoint(34)))
            if (increm(34)/=0) call blstopinit(lunrep, 'SWBlObject')
            BlTemLim = pmsa(ipoint(35))
            if (increm(35)/=0) call blstopinit(lunrep, 'BlTemLim')
            BlBasMor = pmsa(ipoint(36))
            if (increm(36)/=0) call blstopinit(lunrep, 'BlBasMor')
            SWBlGroChk = nint(pmsa(ipoint(37)))
            if (increm(37)/=0) call blstopinit(lunrep, 'SWBlGroChk')
            BlBioBas = pmsa(ipoint(38))
            if (increm(38)/=0) call blstopinit(lunrep, 'BlBioBas')
            SWBlMorChk = nint(pmsa(ipoint(39)))
            if (increm(39)/=0) call blstopinit(lunrep, 'SWBlMorChk')
            BlTopLev = pmsa(ipoint(40))
            if (increm(40)/=0) call blstopinit(lunrep, 'BlTopLev')

            !        Copy algae type properties for input
            DO IALG = 1, NTYP_M
                !          BLOOMALG
                IP = NIPFIX + IALG
                IP = IPOINT(IP)
                !
                !          Hier ook voor ulva van (g) naar (g/m3) lijkt me niet wordt
                !          hier alleen naar negatieve waarde gekeken
                !
                ALGTYP(0, IALG) = PMSA(IP)
                !          SPECALG
                IP = NIPFIX + 1 * NTYP_M + IALG
                IF (INCREM(IP)/=0) CALL BLSTOP('SpecAlg', IALG)
                IP = IPOINT(IP)
                ALGTYP(1, IALG) = NINT(PMSA(IP))
                !          FAUTALG
                IP = NIPFIX + 2 * NTYP_M + IALG
                IF (INCREM(IP)/=0) CALL BLSTOP('FrAutAlg', IALG)
                IP = IPOINT(IP)
                ALGTYP(15, IALG) = PMSA(IP)
                !          EXTVLALG
                IP = NIPFIX + 4 * NTYP_M + IALG
                IF (INCREM(IP)/=0) CALL BLSTOP('ExtVlAlg', IALG)
                IP = IPOINT(IP)
                ALGTYP(2, IALG) = PMSA(IP)
                !          DMCFALG
                IP = NIPFIX + 5 * NTYP_M + IALG
                IF (INCREM(IP)/=0) CALL BLSTOP('DMCFAlg', IALG)
                IP = IPOINT(IP)
                ALGTYP(3, IALG) = PMSA(IP)
                !          NCRALG
                IP = NIPFIX + 6 * NTYP_M + IALG
                IF (INCREM(IP)/=0) CALL BLSTOP('NCRAlg', IALG)
                IP = IPOINT(IP)
                ALGTYP(4, IALG) = PMSA(IP)
                !          PCRALG
                IP = NIPFIX + 7 * NTYP_M + IALG
                IF (INCREM(IP)/=0) CALL BLSTOP('PCRAlg', IALG)
                IP = IPOINT(IP)
                ALGTYP(5, IALG) = PMSA(IP)
                !          SCRALG
                IP = NIPFIX + 8 * NTYP_M + IALG
                IF (INCREM(IP)/=0) CALL BLSTOP('SCRAlg', IALG)
                IP = IPOINT(IP)
                ALGTYP(6, IALG) = PMSA(IP)
                !          XNCRALG
                IP = NIPFIX + 9 * NTYP_M + IALG
                IF (INCREM(IP)/=0) CALL BLSTOP('XNCRAlg', IALG)
                IP = IPOINT(IP)
                ALGTYP(16, IALG) = PMSA(IP)
                !          XPCRALG
                IP = NIPFIX + 10 * NTYP_M + IALG
                IF (INCREM(IP)/=0) CALL BLSTOP('XPCRAlg', IALG)
                IP = IPOINT(IP)
                ALGTYP(17, IALG) = PMSA(IP)
                !          FNCRALG
                IP = NIPFIX + 11 * NTYP_M + IALG
                IF (INCREM(IP)/=0) CALL BLSTOP('FNCRAlg', IALG)
                IP = IPOINT(IP)
                ALGTYP(18, IALG) = PMSA(IP)
                !          CHLACALG
                IP = NIPFIX + 12 * NTYP_M + IALG
                IF (INCREM(IP)/=0) CALL BLSTOP('ChlaCAlg', IALG)
                IP = IPOINT(IP)
                ALGTYP(7, IALG) = PMSA(IP)
                !          PPMAXALG
                IP = NIPFIX + 13 * NTYP_M + IALG
                IF (INCREM(IP)/=0) CALL BLSTOP('PPMaxAlg', IALG)
                IP = IPOINT(IP)
                ALGTYP(8, IALG) = PMSA(IP)
                !          TCPMXALG
                IP = NIPFIX + 14 * NTYP_M + IALG
                IF (INCREM(IP)/=0) CALL BLSTOP('TcPMxAlg', IALG)
                IP = IPOINT(IP)
                ALGTYP(9, IALG) = PMSA(IP)
                !          TFPMXALG
                IP = NIPFIX + 15 * NTYP_M + IALG
                IF (INCREM(IP)/=0) CALL BLSTOP('TFPMxAlg', IALG)
                IP = IPOINT(IP)
                ALGTYP(10, IALG) = PMSA(IP)
                !          MORT0ALG
                IP = NIPFIX + 16 * NTYP_M + IALG
                IF (INCREM(IP)/=0) CALL BLSTOP('Mort0Alg', IALG)
                IP = IPOINT(IP)
                ALGTYP(11, IALG) = PMSA(IP)
                !          TCMRTALG
                IP = NIPFIX + 17 * NTYP_M + IALG
                IF (INCREM(IP)/=0) CALL BLSTOP('TcMrtAlg', IALG)
                IP = IPOINT(IP)
                ALGTYP(12, IALG) = PMSA(IP)
                !          MRESPALG
                IP = NIPFIX + 18 * NTYP_M + IALG
                IF (INCREM(IP)/=0) CALL BLSTOP('MRespAlg', IALG)
                IP = IPOINT(IP)
                ALGTYP(13, IALG) = PMSA(IP)
                !          TCRSPALG
                IP = NIPFIX + 19 * NTYP_M + IALG
                IF (INCREM(IP)/=0) CALL BLSTOP('TcRspAlg', IALG)
                IP = IPOINT(IP)
                ALGTYP(14, IALG) = PMSA(IP)
                !          SDMIXALG
                IP = NIPFIX + 20 * NTYP_M + IALG
                IP = IPOINT(IP)
                ALGTYP(19, IALG) = PMSA(IP)
                !          MRTEXALG
                IP = NIPFIX + 21 * NTYP_M + IALG
                IF (INCREM(IP)/=0) CALL BLSTOP('MrtExAlg', IALG)
                IP = IPOINT(IP)
                ALGTYP(20, IALG) = PMSA(IP)
                !          FIXALG
                IP = NIPFIX + 25 * NTYP_M + IALG
                IF (INCREM(IP)/=0) CALL BLSTOP('FixAlg', IALG)
                IP = IPOINT(IP)
                IFIX(IALG) = NINT(PMSA(IP))
            end do

            !     Read BLOOM-input and set some parameters
            !     JvG 11102013 set NUNUCO dependent of LCARB

            call blinpu (ntyp_m, ntyp_a, ngro_a, algtyp, lmixo, lfixn, lcarb, nunucom, nutcon, flxcon, con2out, &
                    swblsolint, swblobject, bltemlim, blbasmor, swblgrochk, blbiobas, swblmorchk, bltoplev)
            IF (NTYP_A>NTYP_M) GOTO 901

            !     set common CBLBAL communication with balance routines

            CALL IBLBAL (NTYP_M, NTYP_A, ALGTYP, IPOINT(NIPFIX + 1))

            !     Initialize BLOOM (Unit conversions and filling of A-matrix)

            CALL BLINIT
            !     Check on availability of efficiency tracer
            IOFF = NIPFIX + 26 * NTYP_M
            IP = IPOINT(IOFF + 1) + (ISEG - 1) * INCREM(IOFF + 1)
            EFFIN = PMSA(IP + 1)
            ACTIVE_EFFT = EFFIN >= 0.0

            IF (ACTIVE_EFFT) THEN
                ACTIVE_3DL = .FALSE.
            ELSE
                IF (NOQ3 > 0) THEN
                    CALL DHNOSEG(NOSEGW)
                    CALL DHNOLAY(NOLAY)
                    NOLAY = MAX (1, NOLAY)
                    NOSEGW = MAX (1, NOSEGW)
                    NOSEGL = NOSEGW / NOLAY
                    IF (NOSEGL * NOLAY /= NOSEG .AND. .NOT. FM_VTRANS) THEN
                        CALL GETMLU(LUNREP)
                        WRITE(LUNREP, *) ' WARNING unstructured 3D application'
                        WRITE(LUNREP, *) ' BLOOM 3D light approach not possible'
                        ACTIVE_3DL = .FALSE.
                        NOSEG_3DL = NOSEG            ! number of segments, copy of NOSEG
                        NOSEGL_3DL = 1000000000       ! number of segments per layer
                        NOLAY_3DL = 1                ! number of layers
                        NGRO_3DL = NGRO_A           ! number of BLOOM algae groups, copy of NGRO_A
                        NOLAY = 1
                    ELSE
                        ACTIVE_3DL = .TRUE.
                        IF (.NOT. ACTIVE_3DL) THEN ! to trick something in the debugger, (don't) use the variables here
                            NOSEG_3DL = 0                ! number of segments, copy of NOSEG
                            NOSEGL_3DL = 0                ! number of segments per layer
                            NOLAY_3DL = 0                ! number of layers
                            NGRO_3DL = 0                ! number of BLOOM algae groups, copy of NGRO_A
                            ISEG_3DL = 0                ! actual segment for which bloom is called
                            ILAY_3DL = 0                ! actual layer for which bloom is called
                            ACTIVE_3DL = .FALSE.          ! switch indicating if 3DL functionality is active
                            EFFIC_3DL = 0.0
                        ENDIF
                    ENDIF
                ELSE
                    NOSEGW = NOSEG
                    NOSEGL = NOSEG
                    ACTIVE_3DL = .FALSE.
                    NOLAY = 1
                ENDIF
                CALL INIT_3DL(NOSEG, NOSEGW, NOSEGL, NOLAY, NGRO_A, NTYP_A)
                ALLOCATE(IFIX_3DL(NTYP_A))
                IFIX_3DL = IFIX
            ENDIF

            !     Return after initialization

            RETURN

        ENDIF

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
        IP22 = IPOINT(22)
        IP23 = IPOINT(23)
        IP24 = IPOINT(24)
        IP25 = IPOINT(25)
        IP26 = IPOINT(26)
        IP27 = IPOINT(27)
        IP28 = IPOINT(28)
        IP30 = IPOINT(30)
        IP31 = IPOINT(31)
        IP32 = IPOINT(32)

        DO IP = 1, NOPFIX
            IO(IP) = IPOINT(NIPFIX + NIPVAR * NTYP_M + IP)
        ENDDO
        !
        ISWVTR = NINT(PMSA(IPOINT(24)))
        ! 3DL
        IF (ACTIVE_3DL .AND. ISWVTR == 0) THEN
            CALL GETMLU(LUNREP)
            WRITE(LUNREP, *) ' WARNING vertical distribution not active'
            WRITE(LUNREP, *) ' BLOOM 3D light approach not possible'
            ACTIVE_3DL = .FALSE.
        ENDIF
        ! END3DL

        TIMMUL = PMSA(IP1)
        DELTAT = PMSA(IP19)
        BLSTEP = TIMMUL * DELTAT
        RDCNT = RDCNT + BLSTEP
        IF ((AINT(RDCNT / 7.) + 1)/=ID) THEN
            ID = AINT(RDCNT / 7.) + 1
            IF (ID>52) THEN
                ID = ID - 52
                RDCNT = RDCNT - 52. * 7.
            ENDIF
        ENDIF

        !     First segment loop set efficiencies
        !     This loop is only needed for the 3DL Efficiencies, not in 2D or with the light tracer
        ! 3DL
        IF (.NOT.ACTIVE_EFFT) THEN
            DO ISEG = 1, NOSEG
                CALL evaluate_waq_attribute(1, IKNMRK(ISEG), IKMRK1)

                IF (BTEST(IKNMRK(ISEG), 0)) THEN
                    CALL evaluate_waq_attribute(2, IKNMRK(ISEG), IKMRK2)
                    ISEG_3DL = ISEG
                    ILAY_3DL = (ISEG - 1) / NOSEGL_3DL + 1
                    EXTTOT = PMSA(IP2)
                    EXTALG = PMSA(IP3)
                    TEMPER = PMSA(IP4)
                    RADIAT = PMSA(IP5) * 60.48
                    IF (IKMRK1 == 3) THEN
                        RADIAT = 0.0                    ! WAQ-G bodem geen groei
                        CALL BL_NO_AUTOLYSE(ORG_AVAILN) ! WAQ-G bodem geen autolyse
                    ENDIF
                    DEPTH = PMSA(IP6)
                    BLDEP = PMSA(IP7)
                    DAYLEN = PMSA(IP8) * 24.
                    DEPTHW = DEPTH
                    IF (BLDEP>0.) DEPTHW = BLDEP
                    CL = PMSA(IP22)

                    DO IALG = 1, NTYP_A

                        !jvb              set SDMIX for all types, time/space dependent
                        IOFF = NIPFIX + 20 * NTYP_M + IALG
                        IP = IPOINT(IOFF) + (ISEG - 1) * INCREM(IOFF)
                        SDMIXN = PMSA(IP)
                        CALL BLSSDM(IALG, SDMIXN)
                        !jvb
                        IF (IFIX(IALG)<0) THEN
                            !
                            !                    No PP for fixed ulva in non bottom segment, unless sdmix is set positive for this segment
                            !
                            IF (SDMIXN < -1.E-10) THEN
                                IF ((IKMRK2==1).OR.(IKMRK2==2)) THEN
                                    CALL BLSPPM(IALG, 0.0)
                                ENDIF
                            ELSEIF (SDMIXN < 1.E-10) THEN
                                CALL BLSPPM(IALG, 0.0)
                            ENDIF
                        ENDIF

                        IOFF = NIPFIX + NTYP_M * 22
                        IP = IPOINT(IOFF + IALG) + (ISEG - 1) * INCREM(IOFF + IALG)
                        MRTM2 (IALG) = PMSA(IP)
                        IOFF = NIPFIX + NTYP_M * 23
                        IP = IPOINT(IOFF + IALG) + (ISEG - 1) * INCREM(IOFF + IALG)
                        MRTB1 (IALG) = PMSA(IP)
                        IOFF = NIPFIX + NTYP_M * 24
                        IP = IPOINT(IOFF + IALG) + (ISEG - 1) * INCREM(IOFF + IALG)
                        MRTB2 (IALG) = PMSA(IP)
                    ENDDO
                    CALL BLCLST (MRTM1, MRTM2, MRTB1, MRTB2, NTYP_A, CL)

                    CALL SET_EFFI(TEMPER, RADIAT, EXTTOT, DEPTHW, DAYLEN, &
                            ID)

                    IF (IKMRK1 == 3) THEN
                        CALL BL_RESTORE_AUTOLYSE(ORG_AVAILN) ! WAQ-G restore autolyse
                    ENDIF
                    CALL BLCLRS (MRTM1, NTYP_A)
                    !nt2           reset PPMAX anyhow
                    DO IALG = 1, NTYP_A
                        CALL BLSPPM(IALG, ALGTYP(8, IALG))
                    ENDDO
                ENDIF
                IP2 = IP2 + INCREM(2)
                IP3 = IP3 + INCREM(3)
                IP4 = IP4 + INCREM(4)
                IP5 = IP5 + INCREM(5)
                IP6 = IP6 + INCREM(6)
                IP7 = IP7 + INCREM(7)
                IP8 = IP8 + INCREM(8)
                IP22 = IP22 + INCREM(22)
                IP25 = IP25 + INCREM(25)
                IP26 = IP26 + INCREM(26)
                IP27 = IP27 + INCREM(27)
                IP28 = IP28 + INCREM(28)
                IP30 = IP30 + INCREM(30)
                IP31 = IP31 + INCREM(31)
                IP32 = IP32 + INCREM(32)
            ENDDO
        ENDIF
        ! END3DL

        IP2 = IPOINT(2)
        IP3 = IPOINT(3)
        IP4 = IPOINT(4)
        IP5 = IPOINT(5)
        IP6 = IPOINT(6)
        IP7 = IPOINT(7)
        IP8 = IPOINT(8)
        IP22 = IPOINT(22)
        IP25 = IPOINT(25)
        IP26 = IPOINT(26)
        IP27 = IPOINT(27)
        IP28 = IPOINT(28)
        IP30 = IPOINT(30)
        IP31 = IPOINT(31)
        IP32 = IPOINT(32)
        !     Second segment loop, actual bloom call

        IFLUX = 0
        DO ISEG = 1, NOSEG
            CALL evaluate_waq_attribute(1, IKNMRK(ISEG), IKMRK1)
            IF (IKMRK1==1 .OR. IKMRK1==3) THEN
                CALL evaluate_waq_attribute(2, IKNMRK(ISEG), IKMRK2)
                !
                ! 3DL
                IF (.NOT.ACTIVE_EFFT) THEN
                    ISEG_3DL = ISEG
                    ILAY_3DL = (ISEG - 1) / NOSEGL_3DL + 1
                ENDIF
                ! END3DL
                !
                TIMMUL = PMSA(IP1)
                EXTTOT = PMSA(IP2)
                EXTALG = PMSA(IP3)
                TEMPER = PMSA(IP4)

                !     Conversion from standard Delwaq 4.0 [W/m2] to [J/cm2/week] for Bloom

                RADIAT = PMSA(IP5) * 60.48
                IF (IKMRK1 == 3) THEN
                    RADIAT = 0.0                    ! WAQ-G bodem geen groei
                    CALL BL_NO_AUTOLYSE(ORG_AVAILN) ! WAQ-G bodem no autolyse
                ENDIF
                DEPTH = PMSA(IP6)
                BLDEP = PMSA(IP7)
                !     Replace DEPTHW with BLDEP if BLDEP > 0.0
                DEPTHW = DEPTH
                IF (BLDEP>0.) DEPTHW = BLDEP

                !     Conversion from standard Delwaq 4.0 [d] to [h] for Bloom

                DAYLEN = PMSA(IP8) * 24.
                IF (DAYLEN>24.) GOTO 903
                TotNin = PMSA(IP30)
                TotPin = PMSA(IP31)
                TotSIin = PMSA(IP32)
                AMMONI = PMSA(IP9)
                NITRAT = PMSA(IP10)
                PHOSPH = PMSA(IP11)
                SILICA = PMSA(IP12)
                THRNH4 = PMSA(IP13)
                THRNO3 = PMSA(IP14)
                THRPO4 = PMSA(IP15)
                THRSI = PMSA(IP16)
                DETN = PMSA(IP17)
                DETP = PMSA(IP18)
                DELTAT = PMSA(IP19)
                SWBLOOMOUT = NINT(PMSA(IP20))
                CALL BLOUTC(SWBLOOMOUT)
                CL = PMSA(IP22)
                VOLUME = PMSA(IP23)
                TIC = MAX(0.0, PMSA(IP25))
                CO2 = MAX(0.0, PMSA(IP26))

                !     SUBTRACT THRESHOLDS FROM DISSOLVED CONCENTRATION, NOT BELOW ZERO,
                !     BUT BELOW ZERO IF ORIGINAL CONCENTRATION BELOW ZERO
                AMMONI = MAX(MIN(AMMONI, 0.0), AMMONI - THRNH4)
                NITRAT = MAX(MIN(NITRAT, 0.0), NITRAT - THRNO3)
                PHOSPH = MAX(MIN(PHOSPH, 0.0), PHOSPH - THRPO4)
                SILICA = MAX(MIN(SILICA, 0.0), SILICA - THRSI)

                DO IALG = 1, NTYP_A

                    !jvb     set SDMIX for all types, time/space dependent
                    !        SDMIXALG
                    IOFF = NIPFIX + 20 * NTYP_M + IALG
                    IP = IPOINT(IOFF) + (ISEG - 1) * INCREM(IOFF)
                    SDMIXN = PMSA(IP)
                    CALL BLSSDM(IALG, SDMIXN)
                    !jvb
                    !         scale ulva from (g/m2) to (g/m3)
                    !
                    IOFF = NIPFIX
                    IP = IPOINT(IOFF + IALG) + (ISEG - 1) * INCREM(IOFF + IALG)
                    IF (IFIX(IALG)<0) THEN
                        BIOMAS(IALG) = PMSA(IP) / DEPTH
                        !
                        !            No PP for fixed ulva in non bottom segment, unless sdmix is set positive for this segment
                        !
                        IF (SDMIXN < -1.E-10) THEN
                            IF ((IKMRK2==1).OR.(IKMRK2==2)) THEN
                                CALL BLSPPM(IALG, 0.0)
                            ENDIF
                        ELSEIF (SDMIXN < 1.E-10) THEN
                            CALL BLSPPM(IALG, 0.0)
                        ENDIF
                    ELSE
                        BIOMAS(IALG) = PMSA(IP)
                    ENDIF
                    !jvb
                    IOFF = NIPFIX + NTYP_M * 2
                    IP = IPOINT(IOFF + IALG) + (ISEG - 1) * INCREM(IOFF + IALG)
                    FAUT  (IALG) = PMSA(IP)
                    IOFF = NIPFIX + NTYP_M * 3
                    IP = IPOINT(IOFF + IALG) + (ISEG - 1) * INCREM(IOFF + IALG)
                    FDET  (IALG) = PMSA(IP)
                    IOFF = NIPFIX + NTYP_M * 22
                    IP = IPOINT(IOFF + IALG) + (ISEG - 1) * INCREM(IOFF + IALG)
                    MRTM2 (IALG) = PMSA(IP)
                    IOFF = NIPFIX + NTYP_M * 23
                    IP = IPOINT(IOFF + IALG) + (ISEG - 1) * INCREM(IOFF + IALG)
                    MRTB1 (IALG) = PMSA(IP)
                    IOFF = NIPFIX + NTYP_M * 24
                    IP = IPOINT(IOFF + IALG) + (ISEG - 1) * INCREM(IOFF + IALG)
                    MRTB2 (IALG) = PMSA(IP)

                    IF (IKMRK1 == 3) THEN ! WAQG bodem no autolyses
                        FDET(IALG) = FDET(IALG) + FAUT(IALG)
                        FAUT(IALG) = 0.0
                    ENDIF

                end do

                IOFF = NIPFIX + 26 * NTYP_M
                DO IGRO = 1, NGRO_A
                    IP = IPOINT(IOFF + IGRO) + (ISEG - 1) * INCREM(IOFF + IGRO)
                    EFFIN = PMSA(IP)
                    CALL BLSAEF(IGRO, EFFIN)
                ENDDO

                IFAUTO = IFLUX + 1
                IFDETR = IFLUX + 5
                IFOOXP = IFLUX + 9
                IFUPTA = IFLUX + 13
                IFPROD = IFLUX + 23
                IFMORT = IFLUX + 23 + NTYP_M

                !     Salinity dependend mortality
                !     Adapt mortality rates

                CALL BLCLST (MRTM1, MRTM2, MRTB1, MRTB2, NTYP_A, CL)

                !     Compute mortality

                CALL BLMORT (BIOMAS, TEMPER, FAUT, &
                        FDET, FL(IFAUTO), FL(IFDETR), &
                        FL(IFOOXP), FL(IFMORT), DEAT4, &
                        BLSTEP, LMIXO, LFIXN, &
                        LCARB, NUTCON, FLXCON)

                !     Compute primary production and nutrient uptake

                CALL BLPRIM (BIOMAS, AMMONI, NITRAT, &
                        PHOSPH, SILICA, DETN, &
                        DETP, CO2, &
                        TIC, FL(IFMORT), &
                        FL(IFDETR), BLSTEP, EXTTOT, &
                        EXTALG, TEMPER, RADIAT, &
                        DEPTHW, DAYLEN, ID, &
                        NSET, DEAT4, &
                        TOTNUT, CHLORO, FL(IFPROD), &
                        FL(IFUPTA), LIMFAC, NUPTAK, &
                        FRAMMO, FBOD5, RATGRO, &
                        RATMOR, ALGDM, ISEG, &
                        CGROUP, LMIXO, LFIXN, &
                        LCARB, NUTCON, FLXCON, &
                        NOUTLIM, OUTLIM, NUNUCOM, &
                        NTYP_M, CON2OUT, SWBLSA, &
                        TotNin, TotPin, TotSIin)

                !     Copy C-uptake flux to seperate flux for Oxygen

                IF (NINT(PMSA(IP21))==0) &
                        FL(IFUPTA + 9) = FL(IFUPTA)

                !     Salinity dependend mortality
                !     Reset mortality rates

                CALL BLCLRS (MRTM1, NTYP_A)

                !nt2  reset PPMAX anyhow
                DO IALG = 1, NTYP_A
                    CALL BLSPPM(IALG, ALGTYP(8, IALG))
                ENDDO

                IF (IKMRK1 == 3) THEN
                    CALL BL_RESTORE_AUTOLYSE(ORG_AVAILN) ! WAQ-G restore autolyse
                ENDIF

                PMSA(IO(1)) = NUPTAK
                PMSA(IO(2)) = FRAMMO
                PMSA(IO(3)) = TOTNUT(1)
                PMSA(IO(4)) = TOTNUT(2)
                PMSA(IO(5)) = TOTNUT(3)
                PMSA(IO(6)) = TOTNUT(4)
                PMSA(IO(7)) = ALGDM
                PMSA(IO(8)) = FBOD5
                PMSA(IO(9)) = CHLORO
                PMSA(IO(10)) = CHLORO
                PMSA(IO(11)) = LIMFAC(1)
                PMSA(IO(12)) = LIMFAC(2)
                PMSA(IO(13)) = LIMFAC(3)
                PMSA(IO(14)) = LIMFAC(4)
                PMSA(IO(15)) = LIMFAC(5)
                PMSA(IO(16)) = LIMFAC(6)
                PMSA(IO(17)) = FL(IFUPTA) * DEPTHW

                !     RECONSTRUCT RESPIRATION FLUXES
                PMSA(IO(18)) = 0.0
                DO IGRO = 1, NTYP_A
                    RCRESP = ALGTYP(13, IGRO)
                    TCRESP = ALGTYP(14, IGRO)
                    PMSA(IO(18)) = PMSA(IO(18)) &
                            + RCRESP * TCRESP**TEMPER * BIOMAS(IGRO)
                ENDDO
                PMSA(IO(18)) = PMSA(IO(18)) * DEPTHW
                PMSA(IO(19)) = FL(IFUPTA + 7) * DEPTHW

                !     New limitation factors (nutrients + light)
                DO IP = 1, NUNUCOM + 2
                    PMSA(IO(19 + IP)) = OUTLIM(IP)
                ENDDO

                !     Growth rate of all groups
                IOFF = NIPFIX + NIPVAR * NTYP_M + NOPFIX
                DO IGRO = 1, NTYP_A
                    IP = IPOINT(IOFF + IGRO) + (ISEG - 1) * INCREM(IOFF + IGRO)
                    PMSA(IP) = RATGRO(IGRO)
                ENDDO

                !     Mortality rate of all groups
                IOFF = NIPFIX + NIPVAR * NTYP_M + NOPFIX + NTYP_M
                DO IGRO = 1, NTYP_A
                    IP = IPOINT(IOFF + IGRO) + (ISEG - 1) * INCREM(IOFF + IGRO)
                    PMSA(IP) = RATMOR(IGRO)
                ENDDO

                !     Biomass of all groups
                IOFF = NIPFIX + NIPVAR * NTYP_M + NOPFIX + 2 * NTYP_M
                DO IGRO = 1, NTYP_A
                    IP = IPOINT(IOFF + IGRO) + (ISEG - 1) * INCREM(IOFF + IGRO)
                    PMSA(IP) = CGROUP(IGRO)
                ENDDO

                !     Growth limitation of all groups
                IOFF = NIPFIX + NIPVAR * NTYP_M + NOPFIX + 3 * NTYP_M
                DO IGRO = 1, NTYP_A
                    IP = IPOINT(IOFF + IGRO) + (ISEG - 1) * INCREM(IOFF + IGRO)
                    PMSA(IP) = OUTLIM(NUNUCOM + 2 + IGRO)
                ENDDO

                !     Mort limitation of all groups
                IOFF = NIPFIX + NIPVAR * NTYP_M + NOPFIX + 4 * NTYP_M
                DO IGRO = 1, NTYP_A
                    IP = IPOINT(IOFF + IGRO) + (ISEG - 1) * INCREM(IOFF + IGRO)
                    PMSA(IP) = OUTLIM(NUNUCOM + 2 + NTYP_M + IGRO)
                ENDDO

            ENDIF
            !
            IFLUX = IFLUX + NOFLUX
            !
            IP1 = IP1 + INCREM(1)
            IP2 = IP2 + INCREM(2)
            IP3 = IP3 + INCREM(3)
            IP4 = IP4 + INCREM(4)
            IP5 = IP5 + INCREM(5)
            IP6 = IP6 + INCREM(6)
            IP7 = IP7 + INCREM(7)
            IP8 = IP8 + INCREM(8)
            IP9 = IP9 + INCREM(9)
            IP10 = IP10 + INCREM(10)
            IP11 = IP11 + INCREM(11)
            IP12 = IP12 + INCREM(12)
            IP13 = IP13 + INCREM(13)
            IP14 = IP14 + INCREM(14)
            IP15 = IP15 + INCREM(15)
            IP16 = IP16 + INCREM(16)
            IP17 = IP17 + INCREM(17)
            IP18 = IP18 + INCREM(18)
            IP19 = IP19 + INCREM(19)
            IP20 = IP20 + INCREM(20)
            IP21 = IP21 + INCREM(21)
            IP22 = IP22 + INCREM(22)
            IP23 = IP23 + INCREM(23)
            IP24 = IP24 + INCREM(24)
            IP25 = IP25 + INCREM(25)
            IP26 = IP26 + INCREM(26)
            IP27 = IP27 + INCREM(27)
            IP28 = IP28 + INCREM(28)
            IP30 = IP30 + INCREM(30)
            IP31 = IP31 + INCREM(31)
            IP32 = IP32 + INCREM(32)

            DO IP = 1, NOPFIX
                IO(IP) = IO(IP) + INCREM(NIPFIX + NIPVAR * NTYP_M + IP)
            ENDDO
            !
        end do

        ! cummulate output per square metre over the depth towards lowest layer

        ipo17 = ipoint(nipfix + nipvar * ntyp_m + 17)
        ino17 = increm(nipfix + nipvar * ntyp_m + 17)
        ipo18 = ipoint(nipfix + nipvar * ntyp_m + 18)
        ino18 = increm(nipfix + nipvar * ntyp_m + 18)
        ipo19 = ipoint(nipfix + nipvar * ntyp_m + 19)
        ino19 = increm(nipfix + nipvar * ntyp_m + 19)

        do iq = noq1 + noq2 + 1, noq1 + noq2 + noq3
            ifrom = iexpnt(1, iq)
            ito = iexpnt(2, iq)
            if (ifrom>0 .and. ito>0) then
                pmsa(ipo17 + (ito - 1) * ino17) = pmsa(ipo17 + (ifrom - 1) * ino17) + pmsa(ipo17 + (ito - 1) * ino17)
                pmsa(ipo18 + (ito - 1) * ino18) = pmsa(ipo18 + (ifrom - 1) * ino18) + pmsa(ipo18 + (ito - 1) * ino18)
                pmsa(ipo19 + (ito - 1) * ino19) = pmsa(ipo19 + (ifrom - 1) * ino19) + pmsa(ipo19 + (ito - 1) * ino19)
            endif
        enddo

        ! set the accumulated value for every layer

        do iq = noq1 + noq2 + noq3, noq1 + noq2 + 1, -1
            ifrom = iexpnt(1, iq)
            ito = iexpnt(2, iq)
            if (ifrom>0 .and. ito>0) then
                pmsa(ipo17 + (ifrom - 1) * ino17) = pmsa(ipo17 + (ito - 1) * ino17)
                pmsa(ipo18 + (ifrom - 1) * ino18) = pmsa(ipo18 + (ito - 1) * ino18)
                pmsa(ipo19 + (ifrom - 1) * ino19) = pmsa(ipo19 + (ito - 1) * ino19)
            endif
        enddo

        return
        !
        901 call getmlu(lunrep)
        write (lunrep, *) 'ERROR D40BLO: DIMENSION NTYP_M TOO SMALL'
        write (*, *) 'ERROR D40BLO: DIMENSION NTYP_M TOO SMALL'
        call srstop (1)
        ! 902 STOP 'ERROR D40BLO: DIMENSION NGRO_M TOO SMALL'
        903 call getmlu(lunrep)
        write (lunrep, *) 'ERROR D40BLO: DAYLEN > 1.0 DAY'
        write (*, *) 'ERROR D40BLO: DAYLEN > 1.0 DAY'
        call srstop (1)
    end subroutine d40blo

    subroutine blstopinit(lunrep, inputname)
        use m_srstop

        integer(kind = int_wp) :: lunrep
        character(*) inputname

        write(lunrep, *) 'ERROR in bloom: ', inputname, ' must be a constant!'
        write(*, *) 'ERROR in bloom: ', inputname, ' must be a constant!'

        call srstop(1)

        return

    end subroutine blstopinit


    subroutine blstop(mes, i)
        use m_srstop
        use m_monsys

        character(*) mes
        integer(kind = int_wp) :: lunrep, i

        call getmlu(lunrep)
        write(lunrep, *) 'ERROR in bloom: '
        write(lunrep, *) 'Characteristic ', mes, ' for algae type ', i, ' must be a constant!'

        write(*, *) 'ERROR in bloom: '
        write(*, *) 'Characteristic ', mes, ' for algae type ', i, ' must be a constant!'

        call srstop(1)

        return

    end subroutine blstop


    subroutine bl_no_autolyse(org_availn)

        use bloom_data_dim
        use bloom_data_phyt

        implicit none

        real(kind = dp) :: org_availn(mt)

        org_availn(1:mt) = availn(1:mt)
        availn = 0.0

    end subroutine bl_no_autolyse


    subroutine bl_restore_autolyse(org_availn)

        use bloom_data_dim
        use bloom_data_phyt

        implicit none

        real(kind = dp) :: org_availn(mt)

        availn(1:mt) = org_availn(1:mt)

    end subroutine bl_restore_autolyse

end module m_d40blo
