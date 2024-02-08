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
module m_debgrz
   use m_waq_precision

   implicit none

   contains


      SUBROUTINE DEBGRZ  (PMSA , FL , IPOINT , INCREM , NOSEG , NOFLUX , &
                IEXPNT, IKNMRK, IPODIM, NOQ1, NOQ2, NOQ3, NOQ4)
      use m_monsys
      use m_evaluate_waq_attribute

      !**********************************************************************
      !     +----------------------------------------+
      !     |    D E L F T   H Y D R A U L I C S     |
      !     |    WAter Resources and Environment     |
      !     +----------------------------------------+
      !
      !***********************************************************************
      !
      !     Project : Hogere trofische niveaus met DEB
      !     Author  : Tineke Troost
      !     Date    : 19122014             Version : 0.01
      !
      !     History :
      !
      !     Date    Author          Description
      !     ------  --------------  -----------------------------------
      !     040116  Jeroen Wijsman  Created STORG
      !     141222  Tineke Troost   Transformed STORG into DEBGRZ
      !
      !***********************************************************************
      !
      !     Description of the module :
      !
      !        General routine for the dynamics of a grazer based on DEB theory.
      !        The modeled grazers can either form a cohort of equal organisms of
      !        increasing length (isomorphs) or a simplified population of individuals
      !        with an overall fixed size distribution (V1 morphs).
      !        Furthermore, the organisms can either be non-mobile or passively
      !        transported with the water.
      !        The organism can consume various (pelagic and benthic) food types,
      !        including dynamo and bloom algae and various detritus fractions
      !        (DetX, POX and DetXS1). The consumer has a specific preference
      !        for each food type.
      !
      ! Name              Description                                  Units
      ! ----           --- -  -    -------------------                           ----
      ! DELT             x timestep for processes                         (d)
      ! Volume           x volume of computational cell                  (m3)
      ! Temp             x ambient water temperature                     (oC)
      ! Depth            x depth of segment                               (m)
      ! TotalDepth       x total depth water column                       (m)
      ! TIM              x total inorganic matter                    (gDM/m3)
      ! SWDetTyp         x use DetX (0) or POXi for GEM (1)               (-)
      ! SwitchV1         x use ISO-morphs (0) or V1-morphs (1)            (-)
      ! BENTHS           x Use 0 [pelagic] or 1 [benthic] for grazers     (-)
      ! Vtot             x structural biomass grazer pop.    (gC/m3 or gC/m2)
      ! Etot             x energy storage grazer pop.        (gC/m3 or gC/m2)
      ! Rtot             x reproductional storage grazer pop.(gC/m3 or gC/m2)
      ! Dens             x number of grazer individuals        (#/m3 or #/m2)
      ! Length           x Individual Length                        (gWW m-2)
      ! Dummy var        x
      ! Vb               x volume at birth                              (cm3)
      ! Vp               x volume at start of reproductive stage        (cm3)
      ! shape            x shape coefficient                              (-)
      ! Em               x Maximum storage density                    (J/cm3)
      ! Eg               x Volume-specific costs for growth           (J/cm3)
      ! Pm               x rate constant basal respiration grazers      (J/d)
      ! JXm              x Maximum surface area-spec.ingestion rate (J/cm2/d)
      ! kappaI           x Ingestion efficiency (pseudofaeces production) (-)
      ! kappaA               x Assimilation efficiency                        (-)
      ! kappa            x fraction of util.energy spent on maint&growth  (-)
      ! kappaR           x fraction of repro.energy spent on              (-)
      ! Ta               x Arrhenius temperature                          (K)
      ! Tah              x Arr temp for rate of decrease at upper boundary(K)
      ! Tal              x Arr temp for rate of decrease at lower boundary(K)
      ! Th               x Upper boundary of tolerance range              (K)
      ! Tl               x Lower boundary of tolerance range              (K)
      ! GSIupr           x Minimum GSI for spawning                       (-)
      ! GSIlwr           x minimum GSI while spawning                     (-)
      ! DoSpawn          x indication of spawning                         (-)
      ! rSpawn           x Spawning rate                                  (-)
      ! MinSTmp          x Minimum temperature for spawning              (oC)
      ! Xk               x Halfrate const food uptake Sup fdr         (gC/m3)
      ! Yk               x Halfrate const TIM                         (gC/m3)
      ! rMor_ref         x reference mortality rate grazers              (/d)
      ! cMor             x length-dep coefficient mortality rate         (/d)
      ! rHrv_ref         x reference  harvesting rate grazers            (/d)
      ! cHrv             x length-dep coefficient harvesting rate        (/d)
      ! c_J_gC           x conversion factor from J into gC            (gC/J)
      ! c_cm3_C          x conversion factor from cm3 into gC        (gC/cm3)
      ! c_AFW_C          x conversion factor from gAFDW into gC    (gC/gAFDW)
      ! c_WW_gC          x conversion factor from gWW into gC        (gC/gWW)
      ! TC               x C:C ratio grazers                          (gC/gC)
      ! TN               x N:C ratio grazers                          (gN/gC)
      ! TP               x P:C ratio grazers                          (gP/gC)
      ! TSi              x Si:C ratio grazers                        (gSi/gC)
      ! FrDetBot         x fraction of detritus into sediment or water    (-)
      ! SFSusp           x Rel importance suspension vs deposit feeding   (-)
      ! PrDet            x Preference of grazers for DetC or POC1         (-)
      ! PrDetS1          x Preference of grazers for DetCS1               (-)
      ! FFDet            x Faecal fraction of detritus                    (-)
      ! FFDetS1          x Faecal fraction of bottom detritus             (-)
      ! DetC             x Detritus Carbon  (DetC)                    (gC/m3)
      ! DetN             x Detritus Nitrogen (DetN)                   (gN/m3)
      ! DetP             x Detritus Phosphorus (DetP)                 (gP/m3)
      ! DetSi            x Detritus Silica (DetSi)                   (gSi/m3)
      ! POC1               POC1 (fast decaying fraction)              (gC/m3)
      ! PON1               PON1 (fast decaying fraction)              (gN/m3)
      ! POP1               POP1 (fast decaying fraction)              (gP/m3)
      ! POSi1              dummy fast detritus silicium              (gSi/m3)
      ! DetCS1           x DetC in layer S1                              (gC)
      ! DetNS1           x DetN in layer S1                              (gN)
      ! DetPS1           x DetP in layer S1                              (gP)
      ! DetSiS1          x DetSi in layer S1                            (gSi)
      ! SMOtot           x Shell matrix organic tissue       (gC/m3 or gC/m2)
      ! SMItot           x Shell matrix inorganic tissue    (gC/m3 or gDM/m2)
      ! EgSMO              x Vol-spec growth costs for org shell matrix (J/cm3)
      ! EgSMI              x Vol-spec growth costs for inorg shell matr (J/cm3)
      ! FrGSMO           x fraction of growth flux to shell matrix        (-)
      ! FrRespSMO        x fraction of resp flux to shell matrix          (-)
      ! FrSMOSMI         x fraction of shell matrix flux to calcification (-)
      ! cSO_cm3_gC       x conversion factor org shell cm3 into gC   (gC/cm3)
      ! cSI_cm3_gC       x conversion factor inorg shell cm3 into gC (gC/cm3)

      REAL(kind=real_wp) ::PMSA  (*)  , FL  (*)
      INTEGER(kind=int_wp) ::IPOINT(*)  , INCREM(*), NOSEG , NOFLUX, &
                 IEXPNT(4,*), IKNMRK(*), IKMRK2, IPODIM, NOQ1,NOQ2,NOQ3,NOQ4

      INTEGER(kind=int_wp), PARAMETER  ::NO_POINTER = 358 +22 ! number of input output variables in PMSA array =56+3*ntotnut+7*(nfood-2)+21 output vars
      INTEGER(kind=int_wp) ::IP(NO_POINTER)       ! index pointer in PMSA array updated for each segment

      INTEGER(kind=int_wp) ::I,IFOOD,IFLUX,ISEG,IKMRK1
      INTEGER(kind=int_wp) ::NFOOD = 40 +2        ! 30 BLOOM algae + 2 DYNAMO algae + 8 dummy food sources + DetCS1 + Detritus = 40+2
      INTEGER(kind=int_wp) ::NTOTNUT = 4          ! Carbon, Nitrogen, Phosphorus and Silica
      INTEGER(kind=int_wp) ::BENTHS

      ! From PMSA array
      REAL(kind=real_wp) ::DELT , Volume, temp, TotalDepth, Depth, TIM, GEM, &
                 Length_ini, Vtot, Etot, Rtot, Dens_ini, SMOtot, SMItot, &
                 E_L3, rMor_ref, cMor, rHrv_ref, cHrv, TC, TN, TP, TSi, &
                 Ta, Tal, Tah, Th, Tl,  Vb, Vp, shape, Dummy, &
                 Em_L3, Eg_L3, Pm_L3, JXm_L2, PAm_L2, kappa, kappaR, &
                 Xk, Yk, Xk_B, XK_S, Suspension, FrDetBot, EgSMO, EgSMI, &
                 conv_J_gC, conv_cm3_gC, conv_gWW_gC, conv_gAFDW_gC, &
                 cSO_cm3_gC, cSI_cm3_gC, FrGSMO, FrRespSMO, FrSMOSMI

      REAL(kind=real_wp) ::CFOOD   (42),  CCFOOD (42),  Pref   (42),  &
                  NCFOOD  (42),  PCFOOD (42),  SICFOOD(42), &
                  dFil    (42),  FFFOOD (42)
      INTEGER(kind=int_wp) ::BENFOOD (42),  SwitchV1
      INTEGER(kind=int_wp) ::lunrep

      REAL(kind=real_wp) ::POM(4), DETRIT(4), DETS1(4), DETBIO(4)

      REAL(kind=real_wp) ::Area, Dens_m2, Dens,  &
                  E_scaled, V, E, R, V_m2, E_m2, R_m2, &
                  Food, dDef, dNDef, dPDef, dSiDef, &
                  dMor, dNMor, dPMor, Pra, rMor, rHrv, &
                  dRes, dNRes, dPRes, FoodPel, FoodBen, &
                  f_B, f_S, f_max, kappaI, kappaA, FF, FFBen, FFPel, dUpte, &
                  UptakeC, NuptakeC, PuptakeC, LimUptake, &
                  Filtr, NFiltr, PFiltr, SiFiltr, &
                  GSI, GSI_upper, GSI_lower, MinSpTemp, DoSpawn, &
                  dSpw, dNSpw, dPSpw, rSpawn, fadult, fjuv, &
                  Pa, Pc, Pr, Pg, Pm, Prj, Pjj, Pja, Pv, Pomm, Pca, &
                  Vd, Length, Onethird, kappa_G, kT, minFood

      REAL(kind=real_wp) ::Cin, Cuit, Cbal, Nin, Nuit, Pin, Puit, &
                  Siin, Siuit, Nbal, Pbal, Sibal, &
                  Dens_out, WW_ind, Biomass, TotBiomass, &
                  AFDW, TotAFDW, WW, TotWW, &
                  NettGr, GrossGr, Spawn, Harvest, NatMort



      LOGICAL,save :: INIT = .true.

      if (INIT) then
         INIT  = .false.
         IP = IPOINT(1:NO_POINTER)
         IFLUX = 0

         conv_cm3_gC = PMSA( IP(44))
         conv_J_gC   = PMSA( IP(43))
         Eg_L3       = PMSA( IP(20))
         if (conv_cm3_gC > conv_J_gC*Eg_L3) then
            call getmlu( lunrep )
            write (lunrep,*) 'warning: conv_cm3_gC larger than costs for growth,'
            write (lunrep,*) 'therefore adjusted to conv_J_gC*Eg_L3'
            write (lunrep,*) 'this implies kappa_G=0 (no overhead costs for growth)'
         endif

         !benthics can only occur in the bottom layer, they are put to zero in other layers
         BENTHS      =                   NINT(PMSA( IP( 9)))
         DO ISEG = 1, NOSEG
            CALL evaluate_waq_attribute(2,IKNMRK(ISEG),IKMRK2)
            IF ( (IKMRK2 == 1 .OR. IKMRK2 == 2) .AND. BENTHS==1 ) then
               Depth = PMSA( IP( 4))
               Vtot  = PMSA( IP(10))
               Etot  = PMSA( IP(11))
               Rtot  = PMSA( IP(12))
               FL (18 + IFLUX  ) = (Vtot) /depth                                     !mortality (Vtot [gC/m3/d])
               FL (21 + IFLUX  ) = (Etot) /depth                                     !mortality (Etot [gC/m3/d])
               FL (23 + IFLUX  ) = (Rtot) /depth                                     !mortality (Rtot [gC/m3/d])
            endif
            IFLUX = IFLUX + NOFLUX
         enddo
      endif

      !********************************************************************
      ! INITIALISATIONS AND CONVERSIONS
      !********************************************************************
      ! initialise pointers for PMSA and FL array
      IP = IPOINT(1:NO_POINTER)
      BENTHS = NINT(PMSA( IP( 9)))

      IFLUX = 0
      DO ISEG = 1, NOSEG
         CALL evaluate_waq_attribute(1,IKNMRK(ISEG),IKMRK1)
         ! !if cell is active
         IF (IKMRK1==1) THEN
            CALL evaluate_waq_attribute(2,IKNMRK(ISEG),IKMRK2)
            ! pelagics can occur everywhere, but benthic grazers can only exist in bottom layer
            IF ( (IKMRK2 == 1 .OR. IKMRK2 == 2 ) .AND. BENTHS==1 ) then
               ! skip calculations
               GOTO 8900
            ENDIF

            ! Read input from first part of the PSMA
            DELT        =      PMSA( IP( 1))
            Volume      =      PMSA( IP( 2))
            Temp        =      PMSA( IP( 3))
            Depth       =      PMSA( IP( 4))
            TotalDepth  =      PMSA( IP( 5))
            TIM         =      PMSA( IP( 6))
            GEM         =      PMSA( IP( 7))
            SwitchV1    = NINT(PMSA( IP( 8)))
            BENTHS      = NINT(PMSA( IP( 9)))
            Vtot        =      PMSA( IP(10))
            Etot        =      PMSA( IP(11))
            Rtot        =      PMSA( IP(12))
            Dens_ini    =      PMSA( IP(13))
            Length_ini  =      PMSA( IP(14))
            Dummy       =      PMSA( IP(15))
            Vb          =      PMSA( IP(16))
            Vp          =      PMSA( IP(17))
            shape       =      PMSA( IP(18))
            Em_L3       =      PMSA( IP(19))
            Eg_L3       =      PMSA( IP(20))
            Pm_L3       =      PMSA( IP(21))
            JXm_L2      =      PMSA( IP(22))
            kappaI      =      PMSA( IP(23))
            kappaA      =      PMSA( IP(24))
            kappa       =      PMSA( IP(25))
            kappaR      =      PMSA( IP(26))
            Ta          =      PMSA( IP(27))
            Tah         =      PMSA( IP(28))
            Tal         =      PMSA( IP(29))
            Th          =      PMSA( IP(30))
            Tl          =      PMSA( IP(31))
            GSI_upper   =      PMSA( IP(32))
            GSI_lower   =      PMSA( IP(33))
            DoSpawn     =      PMSA( IP(34))
            rSpawn      =      PMSA( IP(35))
            MinSpTemp   =      PMSA( IP(36))
            Xk          =      PMSA( IP(37))
            Yk          =      PMSA( IP(38))
            rMor_ref    =      PMSA( IP(39))
            cMor        =      PMSA( IP(40))
            rHrv_ref    =      PMSA( IP(41))
            cHrv        =      PMSA( IP(42))
            conv_J_gC   =      PMSA( IP(43))
            conv_cm3_gC =      PMSA( IP(44))
            conv_gAFDW_gC=     PMSA( IP(45))
            conv_gWW_gC =      PMSA( IP(46))
            TC          =      PMSA( IP(47))
            TN          =      PMSA( IP(48))
            TP          =      PMSA( IP(49))
            TSi         =      PMSA( IP(50))
            FrDetBot    =      PMSA( IP(51))
            Suspension  =      PMSA( IP(52))
            Pref(1)     =      PMSA( IP(53))
            Pref(2)     =      PMSA( IP(54))
            FFFood(1)   =      PMSA( IP(55))
            FFFood(2)   =      PMSA( IP(56))
            MinFood     =      PMSA( IP(57))
            SMOtot      =      PMSA( IP(58))
            SMItot      =      PMSA( IP(59))
            EgSMO       =      PMSA( IP(60))
            EgSMI       =      PMSA( IP(61))
            FrGSMO      =      PMSA( IP(62))
            FrRespSMO   =      PMSA( IP(63))
            FrSMOSMI    =      PMSA( IP(64))
            cSO_cm3_gC  =      PMSA( IP(65))
            cSI_cm3_gC  =      PMSA( IP(66))

            DO I=1,NTOTNUT
               DETRIT(I)   = MAX(0.,PMSA(IP(66 + I              ))  )
               POM(I)      = MAX(0.,PMSA(IP(66 + I +     NTOTNUT))  )
               DETS1(I)    = MAX(0.,PMSA(IP(66 + I + 2 * NTOTNUT))  )
               DETBIO(I)   = MAX(0.,DETRIT(I)*(1.0-GEM) + POM(I)*GEM)
            ENDDO

            DO IFOOD=3,NFOOD
               CFOOD(IFOOD)  = MAX(0.,PMSA( IP(76 +               IFOOD)))
               CCFOOD(IFOOD) = 1.
               NCFOOD(IFOOD) =        PMSA( IP(76 +   (NFOOD-2) + IFOOD))
               PCFOOD(IFOOD) =        PMSA( IP(76 + 2*(NFOOD-2) + IFOOD))
               SiCFOOD(IFOOD)=        PMSA( IP(76 + 3*(NFOOD-2) + IFOOD))
               Pref(IFOOD)   =        PMSA( IP(76 + 4*(NFOOD-2) + IFOOD))
               BenFood(IFOOD)=  NINT (PMSA( IP(76 + 5*(NFOOD-2) + IFOOD)))
               FFFood(IFOOD) =        PMSA( IP(76 + 6*(NFOOD-2) + IFOOD))
            ENDDO

            ! Add Detbio and DetS1 to the food array's
            ! DetBIO is pelagic detritus
            CFOOD   (1) = DETBIO(1)
            CCFOOD  (1) = 1.
            BenFood (1) = NINT(0.)

            if (DETBIO(1) > tiny(DETBIO(1))) then
               NCFOOD(1)   = DETBIO(2) / DETBIO(1)
               PCFOOD(1)   = DETBIO(3) / DETBIO(1)
               SiCFOOD(1)  = DETBIO(4) / DETBIO(1)
            else
               NCFOOD(1)   = 0.
               PCFOOD(1)   = 0.
               SiCFOOD(1)  = 0.
            endif

            ! DetS1 is a benthic detritus
            CFOOD   (2) = DETS1(1)
            CCFOOD  (2) = 1.
            BenFood (2) = NINT(1.)

            if (DETS1(1) > tiny(DETS1(1))) then
               NCFOOD(2)   = DETS1(2) / DETS1(1)
               PCFOOD(2)   = DETS1(3) / DETS1(1)
               SiCFOOD(2)  = DETS1(4) / DETS1(1)
            else
               NCFOOD(2)   = 0.
               PCFOOD(2)   = 0.
               SiCFOOD(2)  = 0.
            endif

            Onethird = 1./3.
            Area = VOLUME / DEPTH

            ! costs for growth set a maximum to the energy content of structural material and thus to the conversion
            ! coefficient conv_cm3_gC
            ! this may affect the overhead costs for growth (kappa_G)
            conv_cm3_gC = min(conv_cm3_gC,conv_J_gC*Eg_L3)

            !if cell has grazers (volume)
            IF (Vtot > tiny(Vtot))  THEN
               Etot=max(0.,Etot)
               Rtot=max(0.,Rtot)

                ! convert benthic and pelagic grazer components to units /m2,
               if (SwitchV1==1) then
                  Vd = (shape*Length_ini)**3                                 !Vd is reference volume (cm3)
                  Dens = max(((Vtot/conv_cm3_gC)/Vd),tiny(Dens))             !Density derived from Vtot(unit dep on BENTHS)
               else
                  Dens = max(Dens_ini,tiny(Dens))                            !Density is dynamic variable
               endif
               IF (BENTHS==0) then                                         !pelagics (=active substance)
                  Dens_m2 = Dens * Depth                                     !Density pelagics converted from #/m3 to #/m2
                  V_m2 = Vtot * Depth                                        !V_m2 is population structural biomass (gC/m2)
                  E_m2 = Etot * Depth                                        !E_m2 is population energy biomass (gC/m2)
                  R_m2 = Rtot * Depth                                        !R_m2 is population gonadal biomass (gC/m2)
               ELSE                                                          !benthics (=inactive substance)
                  Dens_m2 = Dens                                             !Density benthics converted from #/cell to #/m2
                  V_m2 = Vtot                                                !V_m2 is population structural biomass (gC/m2)
                  E_m2 = Etot                                                !E_m2 is population energy biomass (gC/m2)
                  R_m2 = Rtot                                                !R_m2 is population gonadal biomass (gC/m2)
               ENDIF

               V = max( V_m2 / (Dens_m2 * conv_cm3_gC), tiny(V) )            !V is individual volume (cm3/ind)
               E = max( E_m2 / (Dens_m2 * conv_J_gC), tiny(E) )              !E is individual energy (J/ind)
               R = max( R_m2 / (Dens_m2 * conv_J_gC), tiny(R) )              !R is individual gonads (J/ind)

               if (SwitchV1==1) then
                  V=Vd                                                  !to avoid numerical artefacts at small biomasses
               endif

               Length = (V**Onethird)/shape                                  !Length is derived from individual V
               E_scaled = E /( Em_L3 * V )                                   !E_scaled is derived from E and V

               ! convert benthic FOOD components to units gC m-2, do not convert pelagic components: unit stays gC m-3
               do IFOOD = 1,NFOOD
                  if (Benfood(ifood)==1) then
                     CFOOD(IFOOD)=max(CFOOD(IFOOD) / AREA , 0.)
                  endif
               enddo

               ! Temperature dependent rates
               kT = exp(Ta/(20.+273.)- Ta/(Temp +273.))*  &
                   (1.+ exp(Tal/(20.+273.)-Tal/Tl)+exp(Tah/Th-Tah/(20.+273.)))  &
                   /(1.+ exp(Tal/(Temp+273.)-Tal/Tl)+exp(Tah/Th-Tah/(Temp+273.)))

      !********************************************************************
      ! UPTAKE: FILTRATION, INGESTION and ASSIMILATION per INDIVIDUAL
      !********************************************************************

      ! effective food concentrations (gC/m3), and their faecal (=indigestible) fractions (FF)
               FoodPel = 0.
               FoodBen = 0.
               FFPel = 0.
               FFBen = 0.
               do IFOOD = 1,NFOOD
                  if (Benfood(ifood)==1) then
                     CFood(IFOOD) = Pref(IFOOD) * CFood(IFOOD)
                     FoodBen = FoodBen + CFood(IFOOD)
                     FFBen = FFBen + FFFood(IFOOD) * CFood(IFOOD)
                  else
                     CFood(IFOOD) = Pref(IFOOD) * CFood(IFOOD)
                     FoodPel = FoodPel + CFood(IFOOD)
                     FFPel = FFPel + FFFood(IFOOD) * CFood(IFOOD)
                  endif
               enddo
               FFBen = FFBen/(FoodBen+tiny(FoodBen))
               FFPel = FFPel/(FoodPel+tiny(FoodPel))
               FF = (1.-Suspension)*FFBen + Suspension*FFPel

               ! Calculate scaled functional respons FoodPel (-)
               ! Half-saturation constant same for suspended and bottom food
               Xk_S = Xk
               Xk_B = Xk

               ! No assimilation and uptake when depth < 5 cm (to prevent uptake at dryfalling mudflats)
               if (TotalDepth < 0.05) then
                  f_S = 0.
                  f_B = 0.
               else
                  !f_S = (FoodPel / (FoodPel +  Xk_S *(1. + TIM/Yk)))
                  !f_B = (FoodBen / (FoodBen +  Xk_B *(1. + TIM/Yk)))
                  f_S = ((FoodPel-minFood) / ( (FoodPel-minFood) +  Xk_S *(1. + TIM/Yk)))  ! ZZ: Modify half-saturation relationship with a minimum food threshold.
                  f_B = ((FoodBen-minFood) / ( (FoodBen-minFood) +  Xk_B *(1. + TIM/Yk)))  ! ZZ: Modify half-saturation relationship with a minimum food threshold.
               endif
               ! to avoid negative values or values larger than 1, e.g. due to negative TIM
               f_S=max(f_S,0.)
               f_B=max(f_B,0.)
               f_S=min(f_S,1.)
               f_B=min(f_B,1.)

               ! Commented out: do we need this code? See issue DELWAQ-597.
               ! if too little food (Food<Xk/1000) then diapause
               !              f_max = max(f_S,f_B)
               !              if (f_max .lt. Dummy) then
               !                  kT=0.
               !              endif


               ! Calculate the energy ingestion rates (J/ind/d) and filtration rates (gC/ind/d)
               ! Filtration rates are determined from ingestion rates by conversion into units of gC,
               ! by correction for pseudofaeces losses (1/kappaI),
               ! and for the faecal fraction of food (FF), which fraction is low in energy and thus does not increase
               ! the ingested energy, but does add to the ingested carbon.

               Filtr   = 0.
               NFiltr  = 0.
               PFiltr  = 0.
               SiFiltr = 0.

               do IFOOD=1,NFOOD
                  if (Benfood(ifood)==1) then                                          ! Deposit feeding
                     dUpte = (1.-Suspension)*(CFood(IFOOD)/(FoodBen+tiny(FoodBen))) &
                          * f_B * kT * (V**(2.*Onethird)) * JXm_L2                      !energy ingestion rate J/ind/d
                     dFil(IFOOD) = dUpte * (conv_J_gC/(1.-FFBen))*(1./kappaI)            !filtration rate gC/ind/d

                  else                                                                   ! Suspension feeding
                     dUpte = Suspension * ( CFood(IFOOD) / (FoodPel+tiny(FoodPel))) &
                          * f_S * kT * (V**(2.*Onethird)) * JXm_L2                      !energy ingestion rate J/ind/d
                     dFil(IFOOD) = dUpte * (conv_J_gC/(1.-FFPel))*(1./kappaI)            !filtration rate gC/ind/d
                  endif

                  dFil(IFOOD) = min(dFil(IFOOD), &
     &                           ( (CFood(IFOOD)-minFood)/(DELT*Dens_m2/DEPTH)))                    !ZZ: to impose a minimum food conc threshold (mainly a threshold for large time steps).

                  dFil(IFOOD) = max(dFil(IFOOD),0.)                                      !to avoid negative food uptake

                  Filtr   = Filtr   + dFil(IFOOD) * CCFOOD(IFOOD)                        !(gC/ind/d)
                  NFiltr  = NFiltr  + dFil(IFOOD) * NCFOOD(IFOOD)                        !(gN/ind/d)
                  PFiltr  = PFiltr  + dFil(IFOOD) * PCFOOD(IFOOD)                        !(gP/ind/d)
                  SiFiltr = SiFiltr + dFil(IFOOD) * SiCFOOD(IFOOD)                       !(gSi/ind/d)
               enddo

               !********************************************************************
               ! DEFAECATION per INDIVIDUAL
               !********************************************************************
               ! From the ingested material, a fraction is lost due to (lack of) assimilation efficiency (kappaA)
               ! leading to faeces production and/or due to (lack of) ingestion efficiency (kappaI) leading to
               ! pseudofaeces production Also, the faecal food fraction (FF) is not assimilated; it is assumed to
               ! consist of carbon fibres only, and to be low in energy.
               ! Furthermore the assimilated material has to match the N/C and P/C ratio of the grazer

               if (Filtr > 0.) then
                  UptakeC   = Filtr  *(kappaI*kappaA) * (1.-FF)                          ! Cuptake in (gC/ind/d)
                  NuptakeC =  NFiltr *(kappaI*kappaA) / TN                               ! Nuptake in carbon equivalents (gC/ind/d)
                  PuptakeC =  PFiltr *(kappaI*kappaA) / TP                               ! Puptake in carbon equivalents (gC/ind/d)
                  LimUptake = min(NuptakeC, PuptakeC, UptakeC)                           ! limiting uptake in carbon equivalents (gC/ind/d)

                  ! Pseudofaeces, efficiency losses, and excess nutrients are all released as Faeces
                  ! All uptake of silicate is lost by defecation
                  dDef  =  Filtr  - LimUptake                                            !(gC/ind/d)
                  dNDef = (NFiltr/ TN - LimUptake) * TN                                  !(gN/ind/d)
                  dPDef = (PFiltr/ TP - LimUptake) * TP                                  !(gP/ind/d)
                  dSiDef  = SiFiltr                                                      ! Si loss by def in carbon equivalents (gC d-1)
               else ! no food uptake, so no stoichiometric losses
                  dDef = 0.
                  dNDef = 0.
                  dPDef = 0.
                  dSiDef = 0.
                  LimUptake = 0.
               endif

               ! The remaining material is assimilated into the energy reserves
               Pa  =  LimUptake / conv_J_gC                                              !(J/ind/d)

               !********************************************************************
               ! ENERGY RESERVE DYNAMICS per INDIVIDUAL
               !********************************************************************
               ! volume specific and theoretically maximum uptake rate
               ! this is the maximum rate with which energy can be obtained from the energy reserves
               ! and (being a theoretical maximum) it is not dependent on the actual algae uptake Pa
               PAm_L2 = JXm_L2 * kappaA * kT

               E_L3 = (E/(V+tiny(V)))
               Pc = ((Eg_L3 / Em_L3) * PAm_L2 * V**(2.*Onethird) + Pm_L3*V*kT) * &
                   (E_L3 /(kappa*E_L3 + Eg_L3))                                        !(J/ind/d)

               Pc = min(Pc, E/DELT)
               Pc = max(Pc, 0.)

               !********************************************************************
               ! MAINTENANCE per INDIVIDUAL
               !********************************************************************
               ! Respiration is only due to basal respiration, not to activity or stress.
               ! Respiration of nutrients is related to the carbon respiration with ratios TN and TP
               Pm = Pm_L3 * V * kT                                                       !(J/ind/d)


               !********************************************************************
               ! GROWTH per INDIVIDUAL
               !********************************************************************
               Pg = kappa * Pc - Pm                                                      !(J/ind/d)

               ! when growing, energy will be put in the new tissue and some will be lost due to overhead costs
               ! if too little energy catabolized to pay maintenance, the organisms will shrink
               ! in that case, the overhead costs are assumed to be proportional to those for growth

               kappa_G=conv_cm3_gC/(conv_J_gC*Eg_L3)

               if (Pg > 0.) then
                  Pv = kappa_G*Pg                                                          !(J/ind/d)
               else
                  Pv = (1.+ (1.-kappa_G))*Pg
                  Pm = Pm + abs((1.-kappa_G) * Pg)

                  Pv = max(Pv,(-V/DELT)*(conv_cm3_gC/conv_J_gC))
                  Pg = max(Pg, (((-V/DELT)/(1.+ (1.-kappa_G)))*(conv_cm3_gC/conv_J_gC)))
                  Pm = min(Pm,(kappa*Pc + (V/DELT)*(conv_cm3_gC/conv_J_gC)))
               endif

               !********************************************************************
               ! MATURITY and REPRODUCTION per INDIVIDUAL
               !********************************************************************
               !  ISO-morphs only produce gonads if they are larger than Vp and if GSI > G_upper
               !  V1-morphs produce gonads with a fraction related to the ratio of V and Vp
               !  some adjustments were made with respect to original equations to make sure all catabolized energy is
               ! being used

               if (SwitchV1==1) then
                  fjuv= Vp/(Vp+V)                                                 !juvenile fraction of the population
                  fadult= 1.-Vp/(Vp+V)                                            !adult fraction of the population
                  GSI = (R*conv_J_gC)/(V*fadult*conv_cm3_gC + &
                    E*fadult*conv_J_gC + R*conv_J_gC)
               elseif (V < Vp) then
                  fjuv=1.
                  fadult=0.
                  GSI = 0.
               else
                  fjuv=0.
                  fadult=1.
                  GSI = (R*conv_J_gC)/(V*fadult*conv_cm3_gC + &
                    E*fadult*conv_J_gC + R*conv_J_gC)
               endif

               Pjj = ((1.-kappa)/(kappa+tiny(kappa))) * Pm_L3 * (V*fjuv) * kT    !maturity maintenance juveniles (J/ind/d)
               Pjj = min(Pjj, ((1.-kappa) * Pc * fjuv ))
               Prj  = max(0., ((1.-kappa) * Pc * fjuv - Pjj))                    !remainder goes to maturity development
               Pja = ((1.-kappa)/(kappa+tiny(kappa)))* Pm_L3 * (Vp*fadult) * kT  !maturity maintenance adults (J/ind/d)
               Pra  = (1.-kappa) * Pc * fadult - Pja                             !remainder goes to reproduction flux

               ! if too little energy for juv mat maint and dev, these processes simply stop
               ! but if too little energy for adult mat maint, costs are paid by R with additional overhead costs
               ! proportional to kappaR:
               if (Pra > 0.) then
                  Pr = kappaR*Pra                                                          !(J/ind/d)
               else
                  Pr = (1.+(1.-kappaR))*Pra
                  Pja = Pja + abs((1.-kappaR) * Pra)

                  Pr = max(Pr,(-R/DELT))
                  Pra = max(Pra, (((-R/DELT)/(1.+(1.-kappaR)))))
                  Pja = min(Pja,((1.-kappa)*Pc*fadult + (R/DELT)))
               endif

               ! if conditions are suitable, spawning will start
               if (GSI > GSI_upper) then
                  DoSpawn = 1.
               endif

               ! spawning continues as long as conditions remain suitable
               if (DoSpawn > 0) then
                  if (( GSI > GSI_lower .AND. Temp > MinSpTemp ) .AND. R > 0 ) then

                     dSpw = (rSpawn * R + max(Pr,0.))                                    !(J/ind/d)
                     dSpw = min(dSpw,(R/DELT+min(Pr,0.)))
                  else
                     dSpw = 0.
                  endif

                  if ( GSI < GSI_lower ) then
                     DoSpawn = 0.
                  endif
               else
                  dSpw = 0.
               endif

               PMSA(IP(34)) = DoSpawn                                                    !update pmsa
               dNSpw = dSpw * TN
               dPSpw = dSpw * TP

               !********************************************************************
               ! RESPIRATION per INDIVIDUAL
               !********************************************************************

               !last two terms refer to overhead costs of growth and reproduction
               dRes =Pm+Pja+Pjj+Prj+(1.-kappa_G)*max(Pg,0.)+(1.-kappaR)*max(Pra,0.)      !(J/ind/d)

               dNRes = dRes * TN
               dPRes = dRes * TP

               !********************************************************************
               ! SHELL formation fluxes per INDIVIDUAL
               !********************************************************************

               Pomm =  max(0., (Pv   * FrGSMO))         ! energy flux to organic shell matrix [J/ind/d])
               Pca  =  max(0., (Pomm * FrSMOSMI))       ! energy flux to calcification of shell matrix [J/ind/d])

               !********************************************************************
               ! MORTALITY
               !********************************************************************
               ! Natural mortality and harvesting (only former comes back into the system as detritus)
               ! These added fractions cannot be larger than one (minus the material used for maintenance, at Pv<0)
               rMor  = rMor_ref * (Length**cMor) * kt
               rMor  = min(rMor,(1.+(min(Pv,0.)*conv_J_gC)/(V*conv_cm3_gC)))
               rHrv  = rHrv_ref * (Length**cHrv)
               rHrv  = min(rHrv,(1.-rMor+(min(Pv,0.)*conv_J_gC)/(V*conv_cm3_gC)))

               dMor  = rMor *(V*conv_cm3_gC+(E+R)*conv_J_gC)                             !(gC/ind/d)
               dNMor = dMor * TN
               dPMor = dMor * TP

               !********************************************************************
               ! End of Statements
               !********************************************************************
               ! Fluxes in units of gX/ind/d converted to gX/m3/d for WAQ
               !
               FL ( 1 + IFLUX  ) = dMor*FrDetBot*Dens_m2  /DEPTH                      !mortality to detritus sediment [gC/m3/d]
               FL ( 2 + IFLUX  ) = dNMor*FrDetBot*Dens_m2 /DEPTH                      !mortality to detritus sediment [gN/m3/d]
               FL ( 3 + IFLUX  ) = dPMor*FrDetBot*Dens_m2 /DEPTH                      !mortality to detritus sediment [gP/m3/d]
               FL ( 4 + IFLUX  ) = dMor*(1.-FrDetBot)*Dens_m2  /DEPTH                 !mortality to detritus [gC/m3/d]
               FL ( 5 + IFLUX  ) = dNMor*(1.-FrDetBot)*Dens_m2 /DEPTH                 !mortality to detritus [gN/m3/d]
               FL ( 6 + IFLUX  ) = dPMor*(1.-FrDetBot)*Dens_m2 /DEPTH                 !mortality to detritus [gP/m3/d]
               FL ( 7 + IFLUX  ) = dRes * conv_J_gC*Dens_m2 /DEPTH                    !respiration [gC/m3/d]
               FL ( 8 + IFLUX  ) = dNRes* conv_J_gC*Dens_m2 /DEPTH                    !respiration [gN/m3/d]
               FL ( 9 + IFLUX  ) = dPRes* conv_J_gC*Dens_m2 /DEPTH                    !respiration [gP/m3/d]
               FL (10 + IFLUX  ) = dDef*Dens_m2  /DEPTH                               !defecation [gC/m3/d]
               FL (11 + IFLUX  ) = dNDef*Dens_m2 /DEPTH                               !defecation [gN/m3/d]
               FL (12 + IFLUX  ) = dPDef*Dens_m2 /DEPTH                               !defecation [gP/m3/d]
               FL (13 + IFLUX  ) = dSiDef*Dens_m2 /DEPTH                              !defecation [gSi/m3/d]
               FL (14 + IFLUX  ) = dSpw * conv_J_gC*Dens_m2  /DEPTH                   !spawning to detritus [gC/m3/d]
               FL (15 + IFLUX  ) = dNSpw* conv_J_gC*Dens_m2 /DEPTH                    !spawning to detritus [gN/m3/d]
               FL (16 + IFLUX  ) = dPSpw* conv_J_gC*Dens_m2 /DEPTH                    !spawning to detritus [gP/m3/d]
               FL (17 + IFLUX  ) = ((Pv-Pomm)*conv_J_gC) *Dens_m2 /DEPTH              !growth    (Vtot [gC/m3/d])
               FL (18 + IFLUX  ) = (rMor+rHrv)*V*conv_cm3_gC *Dens_m2 /DEPTH          !mortality (Vtot [gC/m3/d])
               FL (19 + IFLUX  ) = (Pa * conv_J_gC )*Dens_m2 / DEPTH                  !anabolic  (Etot [gC/m3/d])
               FL (20 + IFLUX  ) = (Pc * conv_J_gC )*Dens_m2 / DEPTH                  !catabolic (Etot [gC/m3/d])
               FL (21 + IFLUX  ) = (rMor+rHrv)* E * conv_J_gC *Dens_m2 / DEPTH        !mortality (Etot [gC/m3/d])
               FL (22 + IFLUX  ) = (Pr*conv_J_gC) *Dens_m2 / DEPTH                    !growth    (Rtot [gC/m3/d])
               FL (23 + IFLUX  ) = (rMor+rHrv)* R * conv_J_gC *Dens_m2 / DEPTH        !mortality (Rtot [gC/m3/d])
               if (SwitchV1==0) then                                                !if iso-morph
               ! iso-morphs only grow in V, not in density,
                  FL (24 + IFLUX  ) = 0.                                              !growth    (Dens [#/m3/d])
               else                                                                   !if V1-morph
                  ! V1-morphs have a constant (individual) V, the population V only grows through increase in density
                  FL (24 + IFLUX  ) =((Pv*conv_J_gC/conv_cm3_gC)/V)*Dens_m2 /Depth      !growth    (Dens [#/m3/d])
               endif
               FL (25 + IFLUX  ) = min(1.,(rMor + rHrv))*Dens_m2 /Depth               !mortality (Dens [#/m3/d])
               ! shell fluxes (if taken into account) remain part of the structural volume
               ! hence, their (explicit) presence does thus not change any other fluxes
               FL (26 + IFLUX  ) =  ((Pomm-Pca)/EgSMO)*cSO_cm3_gC * Dens_m2 / DEPTH   ! change in organic shell matrix [gC/m3/d])
               FL (27 + IFLUX  ) =  (Pca/EgSMI) * cSI_cm3_gC * Dens_m2 / DEPTH       ! calcification of shell matrix [gC/m3/d])


               FL (28 + IFLUX  ) = (0.+ GEM)*dFil(1) * Dens_m2 /DEPTH                 !uptake    [gC/m3/d]
               FL (29 + IFLUX  ) = (1.- GEM)*dFil(1) * Dens_m2 /DEPTH
               FL (30 + IFLUX  ) =           dFil(2) * Dens_m2 /DEPTH
               FL (31 + IFLUX  ) = (0.+ GEM)*dFil(1) * NCFOOD(1) * Dens_m2 /DEPTH
               FL (32 + IFLUX  ) = (1.- GEM)*dFil(1) * NCFOOD(1) * Dens_m2 /DEPTH
               FL (33 + IFLUX  ) =           dFil(2) * NCFOOD(2) * Dens_m2 /DEPTH
               FL (34 + IFLUX  ) = (0.+ GEM)*dFil(1) * PCFOOD(1) * Dens_m2 /DEPTH
               FL (35 + IFLUX  ) = (1.- GEM)*dFil(1) * PCFOOD(1) * Dens_m2 /DEPTH
               FL (36 + IFLUX  ) =           dFil(2) * PCFOOD(2) * Dens_m2 /DEPTH
               FL (37 + IFLUX  ) = (0.+ GEM)*dFil(1) * SiCFOOD(1)* Dens_m2 /DEPTH
               FL (38 + IFLUX  ) = (1.- GEM)*dFil(1) * SiCFOOD(1)* Dens_m2 /DEPTH
               FL (39 + IFLUX  ) =           dFil(2) * SiCFOOD(2)* Dens_m2 /DEPTH

               do IFOOD=3,NFOOD
                     FL(37 + IFOOD + IFLUX) = dFil(IFOOD) * Dens_m2 /DEPTH
               enddo

               ! Check on budgets: Nbal, Pbal and Sibal should be zero (unless material is harvested!!).
               ! shell fluxes (if taken into account) remain part of the structural volume
               ! hence, their (explicit) presence does thus not change the calculation of the total mass budgets
               Cin   = Filtr * Dens_m2/DEPTH
               Cuit  = FL(1+IFlux)+ FL(4+IFLUX)+FL(7+IFLUX)+FL(10+IFLUX)+FL(14+IFLUX)
               Nin   = NFiltr *  Dens_m2/DEPTH
               Nuit  = FL(2+IFlux)+ FL(5+IFLUX)+FL(8+IFLUX)+FL(11+IFLUX)+FL(15+IFLUX)
               Pin   = PFiltr *  Dens_m2/DEPTH
               Puit  = FL(3+IFlux)+ FL(6+IFLUX)+FL(9+IFLUX)+FL(12+IFLUX)+FL(16+IFLUX)
               Siin  = SiFiltr *  Dens_m2/DEPTH
               Siuit = FL(13+IFLUX)

               Cbal  = Cin - Cuit &
                      -( FL(17+IFLUX)- FL(18+IFLUX) ) &
                      -( FL(19+IFLUX)- FL(20+IFLUX)- FL(21+IFLUX)) &
                      -( FL(22+IFLUX)- FL(23+IFLUX)- FL(14+IFLUX) )

               Nbal  = Nin - Nuit &
                      -( FL(17+IFLUX)- FL(18+IFLUX) )*TN &
                      -( FL(19+IFLUX)- FL(20+IFLUX)- FL(21+IFLUX))*TN &
                      -( FL(22+IFLUX)- FL(23+IFLUX)- FL(14+IFLUX))*TN

               Pbal  = Pin - Puit &
                      -( FL(17+IFLUX)- FL(18+IFLUX) )*TP &
                      -( FL(19+IFLUX)- FL(20+IFLUX)- FL(21+IFLUX))*TP &
                      -( FL(22+IFLUX)- FL(23+IFLUX)- FL(14+IFLUX))*TP


               Sibal = Siin - Siuit
               Food  = FoodPel+FoodBen
               NatMort=rMor*(V_m2+E_m2+R_m2)/conv_gWW_gC*Area                            !(gWW d-1)
               Harvest=rHrv*(V_m2+E_m2+R_m2)/conv_gWW_gC*Area                            !(gWW d-1)
               Spawn =  dSpw * conv_J_gC *  Dens_m2 / DEPTH                              !(gC d-1)
               TotBiomass= (V_m2 + E_m2 + R_m2) * Area                                   !(gC/cell)
               Biomass= (V_m2 + E_m2 + R_m2)                                             !(gC/m2)
               TotAFDW= TotBiomass / conv_gAFDW_gC                                       !(gAFDW/cell)
               AFDW= Biomass / conv_gAFDW_gC                                             !(gAFDW/m2)
               TotWW=   TotBiomass / conv_gWW_gC                                         !(gWW/cell)
               WW=   (Biomass / conv_gWW_gC) / Area                                      !(gWW/m2)
               WW_ind=   ((Biomass / conv_gWW_gC) / Area) / Dens_m2                      !(gWW/ind)
               GrossGr= Pa * conv_J_gC * Dens_m2                                         !(gC/m2/d)
               NettGr= GrossGr - dRes* conv_J_gC * Dens_m2                               !(gC/m2/d)
               IF (BENTHS==0) then                                                     !pelagics (=active substance)
                  Dens_out = Dens_m2/Depth                                               !(#/m3)
                  Biomass= Biomass / Depth                                               !(gC/m3)
                  AFDW= AFDW / Depth                                                     !(gAFDW/m3)
                  WW=   WW/Depth                                                         !(gWW/m3)
               ELSE                                                                      !benthics (=inactive substance)
                  Dens_out = Dens_m2
               ENDIF

               PMSA(IP(359)) = TotBiomass     !gC
               PMSA(IP(360)) = Biomass        !gC/m2 or gC/m3
               PMSA(IP(361)) = TotAFDW        !gAFDW
               PMSA(IP(362)) = AFDW           !gAFDW/m2 or gAFDW/m3
               PMSA(IP(363)) = TotWW          !gWW
               PMSA(IP(364)) = WW             !gWW/m2 or gWW/m3
               PMSA(IP(365)) = WW_ind         !gWW/ind
               PMSA(IP(366)) = V              !cm3/ind
               PMSA(IP(367)) = E              !J/ind
               PMSA(IP(368)) = R              !J/ind
               PMSA(IP(369)) = Length         !cm/ind
               PMSA(IP(370)) = GSI            !-
               PMSA(IP(371)) = E_scaled       !-
               PMSA(IP(372)) = Harvest        !gC/m2/d
               PMSA(IP(373)) = Spawn          !gC/m2/d
               PMSA(IP(374)) = GrossGr        !gC/m2/d
               PMSA(IP(375)) = NettGr         !gC/m2/d
               PMSA(IP(376)) = Dens_out       !#/m2
               PMSA(IP(377)) = Cbal           !gC/m3
               PMSA(IP(378)) = Nbal           !gN/m3
               PMSA(IP(379)) = Pbal           !gP/m3

            ENDIF   ! (Vtot > 0)
         ENDIF      ! (IKMRK1 == 1)

         ! update pointering in PMSA and FL array
 8900    CONTINUE
         IFLUX = IFLUX + NOFLUX
         IP    = IP    + INCREM(1:NO_POINTER)

      end do

      RETURN
      END

end module m_debgrz
