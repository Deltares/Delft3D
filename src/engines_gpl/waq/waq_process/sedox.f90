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
module m_sedox
    use m_waq_precision

    implicit none

contains


    subroutine sedox  (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        use m_extract_waq_attribute

        !>\file
        !>       Sediment Oxygen Demand (SOD)

        !
        !     Description of the module :
        !
        ! Name    T   L I/O   Description                                    Units
        ! ----    --- -  -    -------------------                            -----
        ! ZFL     REAL        Zero'th order flux                        [gO2/m2/d]
        ! ZFLAUT  REAL        Zero'th order flux (autonomous)           [gO2/m2/d]
        ! DEPTH   R*4 1 I     Depth                                            [m]
        ! SOD     R*4 1 I     oxygen demand in sediment                   [gO2/m2]
        ! RCSOD   R*4 1 I     decay reaction rate SOD at 20 xC               [1/d]
        ! TCSOD   R*4 1 I     decay temperature coefficient SOD                [-]
        ! TEMP    R*4 1 I     ambient water temperature                        [C]
        ! DSOD    R*4 1 O     sediment oxygen demand flux               [gO2/m3/d]
        ! GASBEL  Log 1 L     Calculate methane bubbles if true
        ! BODEM   Log 1 L     Calculate SOD if true
        ! x1(6)   R*4 6 O     See below : output of methane bubble routine.
        ! diagen  R*4 1 L     = total diagenesis, as oxygen demand      [gO2/m2/d]
        ! dep     R*4 1 I     = DEPTH
        ! hsed    R*4 1 I     Active layer of sediment                         [m]
        ! kapc20  R*4 1 I     See SODCH4 routine - in the comment it is called KAPC
        ! thetak  R*4 1 I     See SODCH4 routine
        ! edwcsd  R*4 1 I     See SODCH4 routine
        ! diamb   R*4 1 I     See SODCH4 routine
        ! xox     R*4 1 I     Oxygen concentration in water column        [gO2/m3]
        ! kappad  R*4 1 I     See SODCH4 routine
        ! OXY     R*4 1 I     Oxygen concentration in water column        [gO2/m3]
        ! COXSOD  R*4 1 I     critical oxygen concentration for SOD       [gO2/m3]
        ! OOXSOD  R*4 1 I     optimum oxygen concentration for SOD        [gO2/m3]
        !
        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        REAL(kind = real_wp) :: PMSA  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), NOSEG, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), NOQ1, NOQ2, NOQ3, NOQ4

        REAL(kind = real_wp) :: x1real(6), kapc20, kappad, &
                OXY, COXSOD, OOXSOD, TCSOD, TEMP, TFSOD, O2FUNC, &
                ZFL, DEPTH, SOD, RCSOD, VOL, DSOD, DOXSOD, &
                DMINER, DIAGEN, HSED, THETAK, EDWCSD, DIAMB, XOX, &
                DEP, ZFLAUT
        LOGICAL  TFACT, GASBEL, BODEM, OFACT
        INTEGER(kind = int_wp) :: IFLUX, ISEG, IKMRK1, IKMRK2
        INTEGER(kind = int_wp) :: IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, IP9, &
                IP10, IP11, IP12, IP13, IP14, IP15, IP16, IP17, IP18, &
                IP19, IP20, IP21, IP22, IP23, IP24, IP25, IP26, IP27, &
                IP28, IP29, IP30, IP31, IP32, IP33
        INTEGER(kind = int_wp) :: IN1, IN2, IN3, IN4, IN5, IN6, IN7, IN8, IN9, &
                IN10, IN11, IN12, IN13, IN14, IN15, IN16, IN17, IN18, &
                IN19, IN20, IN21, IN22, IN23, IN24, IN25, IN26, IN27, &
                IN28, IN29, IN30, IN31, IN32, IN33

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
        IP29 = IPOINT(29)
        IP30 = IPOINT(30)
        IP31 = IPOINT(31)
        IP32 = IPOINT(32)
        IP33 = IPOINT(33)

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
        IN22 = INCREM(22)
        IN23 = INCREM(23)
        IN24 = INCREM(24)
        IN25 = INCREM(25)
        IN26 = INCREM(26)
        IN27 = INCREM(27)
        IN28 = INCREM(28)
        IN29 = INCREM(29)
        IN30 = INCREM(30)
        IN31 = INCREM(31)
        IN32 = INCREM(32)
        IN33 = INCREM(33)

        IF (IN6==0 .AND. IN7==0) THEN
            TCSOD = PMSA(IP6)
            TEMP = PMSA(IP7)

            TFSOD = TCSOD ** (TEMP - 20.)

            TFACT = .FALSE.

        ELSE
            TFACT = .TRUE.
        ENDIF
        IF (IN15==0 .AND. IN22==0 .AND. IN23==0) THEN

            OXY = PMSA(IP15)
            COXSOD = PMSA(IP22)
            OOXSOD = PMSA(IP23)

            !       Zuurstoffunctie

            IF (COXSOD < OOXSOD - 0.01) THEN
                IF (OXY <= COXSOD) THEN
                    O2FUNC = 0.0
                ELSEIF (OXY >= OOXSOD) THEN
                    O2FUNC = 1.0
                ELSE
                    O2FUNC = (OXY - COXSOD) / (OOXSOD - COXSOD)
                ENDIF
            ELSE
                O2FUNC = 1.0
            ENDIF

            OFACT = .FALSE.

        ELSE
            OFACT = .TRUE.
        ENDIF
        !
        IFLUX = 0
        DO ISEG = 1, NOSEG
            PMSA (IP24) = 0.0
            PMSA (IP25) = 0.0
            PMSA (IP26) = 0.0
            PMSA (IP27) = 0.0
            PMSA (IP28) = 0.0
            PMSA (IP29) = 0.0
            PMSA (IP30) = 0.0
            PMSA (IP31) = 0.0
            PMSA (IP32) = 0.0
            PMSA (IP33) = 0.0

            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                CALL extract_waq_attribute(2, IKNMRK(ISEG), IKMRK2)

                !           Alleen bij vaktype met een bodem...
                BODEM = .FALSE.
                IF ((IKMRK2==0).OR.(IKMRK2==3)) THEN
                    BODEM = .TRUE.
                ENDIF

                ZFLAUT = PMSA(IP1)
                ZFL = PMSA(IP2)
                DEPTH = PMSA(IP3)
                SOD = PMSA(IP4)
                RCSOD = PMSA(IP5)
                VOL = PMSA(IP8)

                !           Indien diepte of volume bijna of < 0, bereken dan niks.
                !           Dit is tevens beveiliging tegen delen door 0.
                IF (DEPTH<1.E-15) BODEM = .FALSE.
                IF (VOL<1.E-15)   BODEM = .FALSE.

                IF (BODEM) THEN

                    IF (TFACT) THEN
                        TCSOD = PMSA(IP6)
                        TEMP = PMSA(IP7)
                        TFSOD = TCSOD ** (TEMP - 20.)
                    ENDIF


                    !****************************************************************************
                    !**** FLUX equainput divided by depth , M/m2/d * 1/m = M/m3/d + decay SOD
                    !********************************************************************

                    DSOD = ZFL / DEPTH + SOD * RCSOD * TFSOD / DEPTH

                    DOXSOD = (ZFL + ZFLAUT) / DEPTH + SOD * RCSOD * TFSOD / DEPTH

                    !              beveiliging(en) tegen deling door nul in sodch4
                    IF (DOXSOD<1.E-15) DOXSOD = 1.E-15
                    DMINER = 2.67 * (PMSA(IP17) + PMSA(IP18) + &
                            PMSA(IP19) + PMSA(IP20))

                    !              Indien nodig DOXSOD corrigeren voor methaanbellen
                    !              DOXSOD is het effect op zuurstof, dat dus kleiner wordt
                    !              als er methaan bubbels ontsnappen.

                    GASBEL = .FALSE.
                    IF (INT(PMSA(IP9)) == 1) GASBEL = .TRUE.

                    IF (GASBEL) THEN

                        !   Te doen :
                        !   - mag ik dsod gebruiken voor diagen?
                        !   - declaraties !!!!!
                        !   - Is de temperatuurcorrectie in deze routine niet dubbelop ???
                        !   - Correctie voor mineralisatie DetC enOOC in S1 en S2:
                        !     nog heel goed controleren of deze vlieger wel opgaat zo !!
                        !     (groot risico dubbeltellingen).
                        !
                        ! -- Opgelet: ook de zuurstofvraag van DetC en OOC in S1 en S2 moet worden
                        !    gecorrigeerd voor de vorming van gasbellen. Dit gebeurt door de (eventuele)
                        !    fluxen 2.67*dMin[DetC|OOC][S1|S2] op te tellen bij diagen.
                        !    Let op: dMin(etc) is in gC/m3/d, vandaar de omrekening naar gO2.
                        !    Dit betekent dat er mogelijk een produktie van O2 wordt berekend,
                        !    namelijk als de mineralisatiefluxen al tot methaanbelvorming leidt.
                        !    Op die manier wordt de zuurstofvraag van DetC en OOC in het sediment
                        !    gecorrigeerd voor de belvorming.
                        !
                        diagen = DOXSOD * DEPTH + DMINER * DEPTH

                        temp = PMSA(IP7)
                        hsed = PMSA(IP10)
                        kapc20 = PMSA(IP11)
                        thetak = PMSA(IP12)
                        edwcsd = PMSA(IP13)
                        diamb = PMSA(IP14)
                        xox = PMSA(IP15)
                        kappad = PMSA(IP16)

                        !                  OPGELET Hier wordt de totale diepte gebruikt indien
                        !                  gelaagde schematisatie...

                        dep = PMSA(IP21)
                        IF (dep < DEPTH) THEN
                            dep = DEPTH
                        ENDIF

                        CALL SODCH4 (diagen, hsed, kapc20, thetak, temp, dep, &
                                edwcsd, diamb, xox, kappad, x1real)

                        !                 x1real(1) = O2-consumptie                    (gO2/m2/d)
                        !                 x1real(2) = dCH4    = methaanproduktie (als O2) (gO2/m2/d)
                        !                 x1real(3) = DfCH4b  = O2 vraag door diffusie CH4 uit bellen na
                        !                 x1real(4) = DfCH4d  = O2 vraag door direkte CH4 diffusie naar
                        !                 x1real(5) = hSaCH4  = diepte vanaf waar CH4 verzadigd (m)
                        !                 x1real(6) = hAerob  = aerobe diepte (m)
                        !
                        !           Overschrijf de waarde van DOXSOD met de voor methaan gecorrigeerde
                        !           waarde.
                        !           De zuurstofvraag DMINER wel weer aftrekken van DOXSOD !!!!!
                        !           (anders dubbeltelling met BotMin !!!!)

                        DOXSOD = (x1real(1) + x1real(3) + x1real(4)) / DEPTH &
                                - DMINER

                        PMSA (IP25) = x1real(2)
                        PMSA (IP26) = x1real(3)
                        PMSA (IP27) = x1real(4)
                        PMSA (IP28) = x1real(5)
                        PMSA (IP29) = x1real(6)
                        PMSA (IP30) = x1real(2) / DEPTH

                    ELSE

                        !                  Zuurstoffunctie

                        IF (OFACT) THEN
                            OXY = PMSA(IP15)
                            COXSOD = PMSA(IP22)
                            OOXSOD = PMSA(IP23)

                            IF (COXSOD < OOXSOD - 0.01) THEN
                                IF (OXY <= COXSOD) THEN
                                    O2FUNC = 0.0
                                ELSEIF (OXY >= OOXSOD) THEN
                                    O2FUNC = 1.0
                                ELSE
                                    O2FUNC = (OXY - COXSOD) / (OOXSOD - COXSOD)
                                ENDIF
                            ELSE
                                O2FUNC = 1.0
                            ENDIF
                        ENDIF

                        DSOD = DSOD * O2FUNC
                        DOXSOD = DOXSOD * O2FUNC
                    ENDIF

                    !    Dit ook doen indien geen gasbellen gewenst
                    PMSA (IP24) = (DOXSOD + DMINER) * DEPTH
                    PMSA (IP31) = DOXSOD
                    PMSA (IP32) = DMINER
                    PMSA (IP33) = DSOD

                    FL(1 + IFLUX) = DSOD
                    FL(2 + IFLUX) = DOXSOD

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
            IP13 = IP13 + IN13
            IP14 = IP14 + IN14
            IP15 = IP15 + IN15
            IP16 = IP16 + IN16
            IP17 = IP17 + IN17
            IP18 = IP18 + IN18
            IP19 = IP19 + IN19
            IP20 = IP20 + IN20
            IP21 = IP21 + IN21
            IP22 = IP22 + IN22
            IP23 = IP23 + IN23
            IP24 = IP24 + IN24
            IP25 = IP25 + IN25
            IP26 = IP26 + IN26
            IP27 = IP27 + IN27
            IP28 = IP28 + IN28
            IP29 = IP29 + IN29
            IP30 = IP30 + IN30
            IP31 = IP31 + IN31
            IP32 = IP32 + IN32
            IP33 = IP33 + IN33
            !
        end do
        !
        RETURN
    END
    !
    !--- Hieronder de routine van Nico ('sodcard.for'), de naam heb ik
    !    gewijzigd in SODCH4 (rs11dec96)
    !
    subroutine sodCH4(diagen, hsed, kapc20, thetak, temp, dep, &
            edwcsd, diamb, xox, kappad, x1real)
        ! INPUT:
        ! DIAGEN (decay organic materials in sediment (GRAMS O2/M2/DAY))
        ! HSED   (active depth sediment (mostly 0.1 meter))
        ! KAPC   (constant in formula's (See orpheus report)) ->1.6
        ! thetak (temperat constant (See orpheus report)) ->1.079
        ! temp   (temperature in sediment (degrees C)
        ! dep    (thickness of water column (meters)
        ! edwcsd (diffusion coeff) -> 0.00025
        ! diamb  (diameter of methan bubbles in cm -> 1.0
        ! xox    (oxygen in water column (grams/m3)
        ! kappad (transfer coefficient (m/day) -> .003
        ! OUTPUT:
        ! x1(1)  oxygen transfer (g O2/m2/day)
        ! x1(2)  gas formation   (g O2/m2/day)
        ! x1(3)  diffusion from gas bubbles  (g O2/m2/day)
        ! x1(4)  diffusion dissolved methane  (g O2/m2/day)
        ! x1(5)  methane sat depth   (m)
        ! x1(6)  aerobic depth  (m)
        ! total consumption O2 = x1(1) + x1(3) + x1(4) (g O2/m2/day)
        !
        ! RS 20dec96 : all variables in the argument list of this routine must be
        ! single precision because of delwaq.
        !
        !
        implicit double precision (a-z)
        dimension x1(6)
        !
        ! RS: input variables must be single precision
        !     output is converted to single precision
        !
        real :: diagen, hsed, kapc20, thetak, temp, dep, &
                edwcsd, diamb, xox, kappad, x1real(6)

        !**************************************************************
        !diagenese + flux aan sod
        !**************************************************************
        !diagenese snelheid gedeeld door de dikte van het sediment
        stp20 = temp - 20.0
        diagv = diagen / hsed
        !        temperature correct kappas
        kappac = kapc20 * thetak**stp20
        !        methane saturation
        ch4ssd = 99. * (1. + (dep + hsed / 2.) / 10.) * 0.9759**stp20
        dowc = xox
        !        to prevent numerical difficulty with diagenesis computation
        if(dowc<1.e-3)  dowc = 1.e-3
        !        initial sod estimate
        sodi = 1.
        delsod = 0.01
        !        max carbonaceous sod if all methane transferred to
        !        sediment-water column interface were oxidized
        !     *******************************************************
        csodmx = dsqrt(2. * kappad * ch4ssd * diagen)
        !     *******************************************************
        !     write(not,1330)  csodmx,edwcsd,ch4ssd,diagv
        !1330 format(' csodmx,edwcsd,ch4ssd,diagv',4e11.4)
        if(csodmx>diagen)  csodmx = diagen
        !
        !        iterate on total sod
        110 continue
        xc = kappac * dowc / sodi
        !     *************************************************************
        sechxc = 2. / (dexp(xc) + dexp(-xc))
        !     *************************************************************
        csod = csodmx * (1. - sechxc)
        !     ***************************************************************
        !        check convergence
        delta = csod - sodi
        !     write(not,1333)  nsod,csodmx,csod,tsod,delta
        !1333 format(' nosd,effjt,csod,tsod,delta'/5e10.3)
        if(abs(delta)<=delsod)  go to 120
        !        did not converge - new estimate and try again
        sodi = sodi + delta / 2.
        go to 110
        !
        !             converged - compute remaining fluxes
        !        depth of methane saturation
        120  x1(1) = csod
        !     lch4s = dsqrt(2.0 * edwcsd*ch4ssd/diagv)
        lch4s = dsqrt(2.0 * kappad * ch4ssd * hsed * hsed / diagen)
        x1(5) = lch4s
        !        methane gas diffusive flux (g o2/m**2-day)
        jch4d = dsqrt(2. * kappad * ch4ssd * diagen) * sechxc
        !        ch4 production only if saturation depth < sediment depth
        if(lch4s<=hsed)  then
            !          methane saturation therefore methane gas (l/m**2-day)
            !          (gm c/5.33 gm o2 * 22.4 l/12 gm c ==> 0.3502)
            jch4g = 0.3502 * diagv * (hsed - lch4s)
            !          compute methane bubble transfer
            !          methane transfer from bubble to water column (gm o2/m**2-day)
            jbt = 0.0961 * jch4g * dep**0.6667 / diamb
            !          methane gas bubble transfer (l/m**2-day)
            jbtf = 0.3502 * jbt / jch4g
            !          if bubble transfer > ch4(gas) then
            !          set bubble transfer equal to total ch4(gas)
            if(jbtf>1.) then
                jbtf = 1.
                jbt = jch4g / 0.3502
            endif
        else
            jch4g = 0.
            jch4d = 0.
            jbtf = 0.0
            jbt = 0.0
            x1(1) = diagen
        endif
        x1(2) = jch4g / 0.3502
        x1(3) = jbt
        x1(4) = jch4d
        laero = edwcsd * dowc / csod
        x1(6) = laero
        !
        ! RS : conversion of double to single precision for delwaq variable
        !
        x1real(1) = real(x1(1))
        x1real(2) = real(x1(2))
        x1real(3) = real(x1(3))
        x1real(4) = real(x1(4))
        x1real(5) = real(x1(5))
        x1real(6) = real(x1(6))

        return
    end

end module m_sedox
