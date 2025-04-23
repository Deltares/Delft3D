!----- AGPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
!
!  This program is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

 module RunoffFormulations

 use Messages

 implicit none

 contains


    Subroutine RunoffFactorFormulation (VNow, NetRain, RunoffFactor, RateInfiltration, &
                                        Idebug, TimestepSize, TotUit, Vinf, FixArs12253)

    real    Vnow, NetRain, RunoffFactor,RateInfiltration
    integer TimestepSize, Idebug
    real    TotUit, Vinf
    logical FixArs12253

    integer NrsMin, NLTims, itime
    real    VNetr, Vinf2, Vuit, dt, &
            RunoffFactor2, VOutNetR, LnAfVrTr, Temp


      ! Compute runoff as formulated in NWRW model: q=c.h with c in (1/min), h=mm
      !
      ! Input data:
      !    VNow  = initial volume  (m3)
      !    NetRain = net rainfall
      !    RunoffFactor = Runoff factor c
      !    RateInfiltration = infiltration rate
      !    NLTims= nr. of minutes
      !    Vinit = initial volume
      !    Vinit = initial volume
      ! Output:
      !    TotUit = Total outflow (m3)
      !    VInf   = infiltration  (m3)

       VINF2  = 0.0

       NrsMin = 60
       NLTims = TimestepSize / NrsMin

!      write(*,*) ' RunoffFactorFormulation NlTims=', NlTims
!      write(*,*) ' RunoffFactorFormulation VNow', VNow

       if (RunoffFactor .gt. 1 .or. RunoffFactor .lt. 0) &
          call ErrMsgStandard (972, 0, ' Runoff coefficient values should be between 0 and 1', ' ')

       Temp = -Log (1.-(Min(RunoffFactor,0.999999)))
       LnAfvrtr = 1.- RunoffFactor/Max(0.000001,Temp)

       ! Inloop riool en rest in m3 per tijdstap
       !  loop over de minuten in de tijdstap
       !   veronderstel NTRAIN etc homogeen verdeeld

!      write(*,*) ' RunoffFactorFormulation LnAfvrtr FixArs12253=', LnAfvrtr, FixArs12253
       TOTUIT = 0.0
       IF (NLTIMS .GT. 1) THEN
          VNETR  = NetRain / NLTIMS
          VINF   = 0.0
          VINF2  = RateInfiltration / NLTIMS
!         write(*,*) ' RunoffFactorFormulation VNetR Vinf Vinf2', VNetr, Vinf, Vinf2
          DO ITime=1,NLTIMS
             VOutNetR = 0.0
!            write(*,*) ' RunoffFactorFormulation ITime', ITime
             If (FixArs12253) VOutNetR = LnAfVrTr * VNetR
!            write(*,*) ' RunoffFactorFormulation VoutNetR', VOutNetR
             VINF2= MIN (VINF2, VNOW+VNETR-VOutNetR)
!            write(*,*) ' RunoffFactorFormulation VInf2', VInf2
!            write(*,*) ' RunoffFactorFormulation RunoffFactor', RunoffFactor
             if (FixArs12253) then
                VUIT = (VNOW-VINF2) * RunoffFactor + VOutNetR
             else
                VUIT = (VNOW+VNETR-VINF2) * RunoffFactor
             endif
!            write(*,*) ' RunoffFactorFormulation VUit', VUit
             VNOW = VNOW + VNETR - VINF2 - VUIT
             ! totals
             VINF = VINF + VINF2
             TOTUIT = TOTUIT + VUIT
!            write(*,*) ' RunoffFactorFormulation VNow VInf TotUit', VNow, VInf, TotUit
          ENDDO
       ELSE
          ! tijdstapgrootte is kleiner dan 1 minuut, gebruik afvrtr2
          dt = (1.0 * TimestepSize/ NrsMin)
          RunoffFactor2 = (1. - (1.-RunoffFactor) ** dt) / dt
          VNETR  = NetRain
          VINF   = MIN (RateInfiltration, VNOW+VNETR)
          VUIT = (VNOW+VNETR-VINF) * RunoffFactor2 * TimestepSize / NrsMin

          VNOW = VNOW + VNETR - VINF - VUIT
          TOTUIT = TOTUIT + VUIT
       ENDIF
!      write(*,*) ' Exit RunoffFactorFormulation'
       IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' RunoffFactor ',RunoffFactor
       IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Infiltration rate ',RateInfiltration
       IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Infiltrated volume',Vinf2
       IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' NLTIMS  VNOW VNETR VUIT TOTUIT',NLTIMS,VNOW,VNETR,VUIT,TOTUIT

    End subroutine RunoffFactorFormulation


  Subroutine HellingaDeZeeuwFormule (Q, Area, Alfa, BergCoef, DeltaH, DeltaQ, DeltaT)
    ! *********************************************************************
    ! *** Last update:  March 1995
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    Deze subroutine bevat de Hellinga-deZeeuw formule
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  Area     = area in m2
    ! ***  alfa     = alfa in 1/s
    ! ***  bergcoef = bergingscoefficient
    ! ***  deltaH   = peilverschil in m
    ! ***  deltaQ   = Qin huidige tijdstap
    ! ***  deltaT   = tijdstapgrootte
    ! ***  Q        = berekend debiet (output)  NB omdat met area vermenigvuldigd wordt is het een VOLUME !!!
    ! *********************************************************************

     double precision Q, Area, Alfa, BergCoef, DeltaH, DeltaQ
     Integer DeltaT
     double precision temp1, temp2, temp3
!
!     Q = ( Area * Alfa * BergCoef * DeltaH - DeltaQ) / Alfa *  &
!                                        (1.0 -EXP(-Alfa * DeltaT) ) + DeltaQ * DeltaT
      temp1 = ( Area * Alfa * BergCoef * DeltaH - DeltaQ) / Alfa
      temp2 = (1.0 -EXP(-1.0 * Alfa * dble(DeltaT)) )
      temp3 = DeltaQ * DeltaT
      Q = (temp1 * temp2) + temp3

   Return
  END subroutine HellingaDeZeeuwFormule




  Subroutine ErnstFormule (Q, Area, Weerstand, DeltaH, DeltaT, Idebug)
    ! *********************************************************************
    ! *** Last update:  March 1995
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    Deze subroutine bevat de Hellinga-deZeeuw formule
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  Area      = area in m2
    ! ***  Weerstand = weerstand in dagen
    ! ***  deltaH    = peilverschil in m
    ! ***  deltaT    = tijdstapgrootte
    ! ***  Q         = berekend VOLUME (output)
    ! *********************************************************************

     Double precision  Q, Area, Weerstand, DeltaH
     Integer DeltaT, Idebug, D_IfReal, idum
     Double precision  Vormfactor, Flux, Volume
     Integer NrSecondsPerDay

     NrSecondsPerDay = 86400
!    Vormfactor = 0.74    ! Simgro default; Ernst factor tussen 0.65 en 0.85 afhankelijk van slootafstand e.d.
     Vormfactor = 1.00    ! dit moet de gebruiker maar in de op te geven Weerstand stoppen

! ARS 11029: Add Check on DeltaH=0, and Weerstand=0
     idum = D_IfReal (DeltaH, 0.0D0, 1D-8)
     if (Idum .eq. 0) then
        Flux = 0.0
     else
        Weerstand = Max (Weerstand, 0.0001d0)
        Flux = DeltaH / (Weerstand * Vormfactor) / NrSecondsPerDay     ! in m/s
     Endif
     Volume = Flux * Area * DeltaT   ! in m3

     Q = Volume
     if (idebug .ne. 0) write(Idebug,'(A,4E12.5,I7)') ' Ernst Formula result', Q, Area, Weerstand, DeltaH, DeltaT

   Return
  END subroutine ErnstFormule


   Subroutine GreenRoofBalance  (ThetaInit, ThetaFinal, ThetaWiltingPoint, ThetaFieldCapacity, ThetaSat, &
                                  KSat, Kp, CropFact, Rain, Epot, Eact, TotalRunoff, Idebug)

    real    ThetaInit, ThetaTemp, ThetaFinal, ThetaWiltingPoint, ThetaFieldCapacity, ThetaSat, KSat, Kp, CropFact, Rain, Epot, Eact
    real    TotalRunoff, SurfaceRunoff
    real    Percolation
    integer Idebug

    ThetaTemp = ThetaInit + Rain
    Call GreenRoofEact (ThetaTemp, ThetaWiltingPoint, ThetaFieldCapacity, CropFact, Epot, Eact, Idebug)
    ThetaTemp = ThetaInit + Rain - Eact
    Call GreenRoofPerc (ThetaTemp, ThetaFieldCapacity, ThetaSat, Kp, KSat, Percolation, idebug)
    ThetaFinal = ThetaInit + Rain - Eact - Percolation
    SurfaceRunoff = 0.0
    SurfaceRunoff = max (0.0, ThetaFinal - ThetaSat)
    TotalRunoff = Percolation + SurfaceRunoff
    ! Feb 2011: do not forget to update ThetaFinal for surface runoff!
    ThetaFinal  = min (ThetaFinal, ThetaSat)

    if (idebug .gt.0) then
       write(Idebug,*) ' GreenRoofBalance '
       write(Idebug,*) ' ThetaInit = ', ThetaInit
       write(Idebug,*) ' ThetaWiltingPoint  = ', ThetaWiltingPoint
       write(Idebug,*) ' ThetaFieldCapacity = ', ThetaFieldCapacity
       write(Idebug,*) ' ThetaSat           = ', ThetaSat
       write(Idebug,*) ' Eact               = ', Eact
       write(Idebug,*) ' Percolation        = ', Percolation
       write(Idebug,*) ' Surface Runoff     = ', SurfaceRunoff
       write(Idebug,*) ' ThetaFinal         = ', ThetaFinal
       write(Idebug,*) ' TotalRunoff        = ', TotalRunoff
    endif

   Return
  END subroutine GreenRoofBalance


   Subroutine GreenRoofInit   (ThetaInit, ThetaWiltingPoint, ThetaFieldCapacity, ThetaSat, &
                               InputInitialPercentage, InputWiltingPointPercentage, InputFieldCapacityPercentage, &
                               InputSatPercentage, GreenRoofThickness, idebug)

    real    ThetaInit, ThetaWiltingPoint, ThetaFieldCapacity, ThetaSat
    real    InputInitialPercentage, InputWiltingPointPercentage, InputFieldCapacityPercentage, InputSatPercentage, GreenRoofThickness
    integer Idebug

    ThetaInit          = InputInitialPercentage * GreenRoofThickness / 100.
    ThetaWiltingPoint  = InputWiltingPointPercentage * GreenRoofThickness / 100.
    ThetaFieldCapacity = InputFieldCapacityPercentage * GreenRoofThickness / 100.
    ThetaSat           = InputSatPercentage * GreenRoofThickness / 100.
    if (idebug .gt.0) then
       write(Idebug,*) ' GreenRoofInit '
       write(Idebug,*) ' ThetaInit          = ', ThetaInit, InputInitialPercentage
       write(Idebug,*) ' ThetaWiltingPoint  = ', ThetaWiltingPoint
       write(Idebug,*) ' ThetaFieldCapacity = ', ThetaFieldCapacity
       write(Idebug,*) ' ThetaSat           = ', ThetaSat
    endif


   Return
  END subroutine GreenRoofInit


   Subroutine GreenRoofEact            (ThetaInit, ThetaWiltingPoint, ThetaFieldCapacity, CropFact, Epot, Eact, Idebug)

    real    ThetaInit, ThetaWiltingPoint, ThetaFieldCapacity, CropFact, Epot, Eact
    real    EpotGreenRoof, ThetaRelative
    integer Idebug

    EpotGreenRoof = CropFact * Epot
    ThetaRelative = min (1.0, (ThetaInit-ThetaWiltingPoint) / (ThetaFieldCapacity-ThetaWiltingPoint) )
    ThetaRelative = max (0.0, ThetaRelative)

    Eact = EpotGreenRoof * ThetaRelative * Exp (1-ThetaRelative)

    if (idebug .gt.0) then
       write(Idebug,*) ' GreenRoofEact '
       write(Idebug,*) ' Epot          = ', Epot
       write(Idebug,*) ' CropFact      = ', CropFact
       write(Idebug,*) ' EpotGreenRoof = ', EpotGreenRoof
       write(Idebug,*) ' ThetaWiltingPoint  = ', ThetaWiltingPoint
       write(Idebug,*) ' ThetaFieldCapacity = ', ThetaFieldCapacity
!      write(Idebug,*) ' Eact               = ', Eact
    endif

   Return
  END subroutine GreenRoofEAct


  Subroutine GreenRoofPerc (ThetaInit, ThetaFieldCapacity, ThetaSat, Kp, KSat, Percolation, idebug)

    real    ThetaInit, ThetaFieldCapacity, ThetaSat, Kp, KSat
    real    Percolation, ThetaExcess, ThetaMaxExcess
    integer Idebug

    if (ThetaInit .lt. ThetaFieldCapacity) then
       Percolation = 0
    else
       ThetaExcess = ThetaInit - ThetaFieldCapacity
       ThetaMaxExcess = ThetaSat - ThetaFieldCapacity
       Percolation = min (ThetaExcess, (Kp + (KSat-Kp)* ThetaExcess/ThetaMaxExcess) * ThetaExcess/ThetaMaxExcess )
       Percolation = max (0.0, Percolation)
    endif

    if (idebug .gt.0) then
       write(Idebug,*) ' GreenRoofPerc '
       write(Idebug,*) ' ThetaInit     = ', ThetaInit
       write(Idebug,*) ' ThetaFieldCap = ', ThetaFieldCapacity
       write(Idebug,*) ' ThetaExcess   = ', ThetaExcess
       write(Idebug,*) ' ThetaMaxExcess= ', ThetaMaxExcess
       write(Idebug,*) ' Kp            = ', Kp
       write(Idebug,*) ' KSat          = ', KSat
       write(Idebug,*) ' Percolation   = ', Percolation
    endif

   Return
  END subroutine GreenRoofPerc



  Subroutine NAMCalculations (Precipitation, PotEvap,  BaseStorageMax, itmstp, &
                              SurfStorageMax, RootStorageMax, Tof, Tif,Tg, cqof, ckif, ck12, ofsmin, beta, ckbf, &
                              SurfStorageInitial, RootStorageInitial,  InterflowStorage1Initial, InterflowStorage2Initial, OverlandStorageInitial, BaseStorageInitial, &
                              SurfStorageFinal, RootStorageFinal, InterflowStorage1Final, InterflowStorage2Final, OverlandStorageFinal, BaseStorageFinal, &
                              SurfEvap, RootEvap, QOF, QIF, QBF, runoff, DL, G, PNReturnValue, &
                              InflowInterflowRsv, InflowInterflowRsv2, InflowOverlandflowRsv, Idebug, &
                              Houtside, Gwl0, GwlBf0, GwlBf1, GwlFl1, GwlInitial, GwlFinal, SpecificYield, cklow, ckinf, CapRis, MaxCapRis, Qlow, Qinf, Pumpflow, Fraction)


    double precision Precipitation, PotEvap
    double precision BaseStorageMax, SurfStorageMax, RootStorageMax, Tof, Tif,Tg, cqof, ckif, ck12, ofsmin, beta, ckbf
    double precision SurfStorageInitial, RootStorageInitial,  InterflowStorage1Initial, InterflowStorage2Initial, OverlandStorageInitial, BaseStorageInitial
    double precision SurfStorageFinal, RootStorageFinal, InterflowStorage1Final, InterflowStorage2Final, OverlandStorageFinal, BaseStorageFinal
    double precision SurfEvap, RootEvap, QOF, QIF, QBF, Runoff, InflowInterflowRsv, InflowInterflowRsv2, InflowOverlandflowRsv

    double precision SurfStorage, RootStorage, OverlandStorage, InterflowStorage1, InterflowStorage2, BaseStorage, BaseStorage0
    double precision RRel, PNReturnValue, PN, A, G, DL, TR, QIF1TO2
    integer Idebug, itmstp

! addition May 2015
    double precision SpecificYield, CapRis, MaxCapRis, alfa, cklow, ckinf, fraction
    double precision BaseStorageMax0, BaseStorageMax1, BaseFlowMax, QLow, Qinf
    double precision GwlInitial, GwlFinal, Gwl, Gwl0, GwlBf0, GwlBf1, GwlFl1, Houtside
    double precision DeltaRootStorage, DeltaBaseStorage, Pumpflow, Change, TempStorage, TempStorageQbf, TempStorageQlow

        if (idebug .gt.0) then
           write(Idebug,*) ' Input in NAMCalculations'
           write(Idebug,*) '  Timestep                 = ', itmstp
           write(Idebug,*) '  Precipitation            = ', Precipitation
           write(Idebug,*) '  PotEvap                  = ', PotEvap
           write(Idebug,*) '  BaseStorageMax           = ', BaseStorageMax
           write(Idebug,*) '  SurfStorageMax           = ', SurfStorageMax
           write(Idebug,*) '  RootStorageMax           = ', RootStorageMax
           write(Idebug,*) '  Tof                      = ', Tof
           write(Idebug,*) '  Tif                      = ', Tif
           write(Idebug,*) '  Tg                       = ', Tg
           write(Idebug,*) '  cqof                     = ', cqof
           write(Idebug,*) '  ck12                     = ', ck12
           write(Idebug,*) '  ofsmin                   = ', ofsmin
           write(Idebug,*) '  beta                     = ', beta
           write(Idebug,*) '  ckbf                     = ', ckbf
           write(Idebug,*) '  SurfStorageInitial       = ', SurfStorageInitial
           write(Idebug,*) '  RootStorageInitial       = ', RootStorageInitial
           write(Idebug,*) '  InterflowStorage1Initial = ', InterflowStorage1Initial
           write(Idebug,*) '  InterflowStorage2Initial = ', InterflowStorage2Initial
           write(Idebug,*) '  OverlandStorageInitial   = ', OverlandStorageInitial
           write(Idebug,*) '  BaseStorageInitial       = ', BaseStorageInitial
           write(Idebug,*) '  Houtside (mbs)           = ', Houtside
           write(Idebug,*) '  Gwl0 (mbs)               = ', Gwl0
           write(Idebug,*) '  GwlBf0 (mbs)             = ', GwlBf0
           write(Idebug,*) '  GwlBf1 (mbs)             = ', GwlBf1
           write(Idebug,*) '  GwlFl1 (mbs)             = ', GwlFl1
           write(Idebug,*) '  GwlInitial (mbs)         = ', GwlInitial
           write(Idebug,*) '  SpecificYield            = ', SpecificYield
           write(Idebug,*) '  cklow                    = ', cklow
           write(Idebug,*) '  ckinf                    = ', ckinf
           write(Idebug,*) '  Pumpflow                 = ', Pumpflow
           write(Idebug,*) '  Fraction                 = ', fraction
           write(Idebug,*) '  MaxCapRis                = ', MaxCapRis
          endif

! initialise local variables
       OverlandStorage   = OverlandStorageInitial
       SurfStorage       = SurfStorageInitial
       RootStorage       = RootStorageInitial
       InterflowStorage1 = InterflowStorage1Initial
       InterflowStorage2 = InterflowStorage2Initial
       BaseStorage       = BaseStorageInitial
       Gwl               = GwlInitial

       if (idebug .ne. 0) write(Idebug,*) ' NAMCalculations'
! computations

        RRel= RootStorage / RootStorageMax

! === Surface Zone Routine ============================================
! --- Surface Storage -------------------------------------------------
        SurfStorage = Precipitation + SurfStorage
        SurfEvap    = MIN (Potevap, SurfStorage)
        SurfStorage = SurfStorage - SurfEvap

! --- Interflow -------------------------------------------------------
        A=SurfStorage/CKIF
        Call NAMFluxes (A,RRel,TIF,QIF)
        InflowInterflowRsv = QIF

! --- Surface Excess Water --------------------------------------------
        SurfStorage = SurfStorage - QIF
        if ( SurfStorage .gt. SurfStorageMax ) then
           PN          = SurfStorage - SurfStorageMax
           SurfStorage = SurfStorageMax
        else
           PN = 0.D0
        Endif
        PNReturnValue = PN

! --- Overland Flow ---------------------------------------------------
        A = CQOF * PN
        Call NAMFluxes (A,RRel,TOF,QOF)
        PN = PN - QOF
        if (idebug .gt.0) then
           write(Idebug,*) ' Overland Storage step1 = ', OverlandStorage
           write(Idebug,*) ' Overland Flow step1    = ', QOF
        endif
        InflowOverlandflowRsv = QOF

! === Root Zone Routine =================================================
! --- Groundwater Recharge ----------------------------------------------
        Call NAMFluxes (PN,RRel,TG,G)
        DL = PN - G

! --- Root Zone Evapotranspiration --------------------------------------
        A = Potevap - SurfStorage
        Tr = 0.D0
        Call NAMFluxes (A,RRel,Tr,RootEvap)
        if ( SurfEvap .lt. Potevap )  then
            RootEvap = min (PotEvap-SurfEvap, RRel * (Potevap-SurfStorage) )
        else
            RootEvap=0.D0
        endif
! May 2015 add CapRis
! Dec 2015: first update RootStorage to check free space in RootStorage
        RootStorage = RootStorageInitial + DL - RootEvap
        If (BaseStorage .le. 0) then
           CapRis = 0.D0
        ElseIf (RootStorage .ge. RootStorageMax) then
           CapRis = 0.D0
        Else
           alfa = -1.D0 * (1.5D0 + 0.45D0 * GwlFl1)
           ! empirical formula is in mm/day, so multiply with fraction (timestepsize/daysize)
           ! make sure gwl used >=0  so below or at surface
           if (Gwl .le. 0) then
              CapRis = 0.D0
           else
              CapRis = fraction * Sqrt(1.D0 - RRel) * (Gwl / GwlFl1) ** alfa
           endif
           ! and limit to MaxCapRis mm/day (default value MaxCapRis=5 mm/day in NL)
           ! MaxCapRis also limited by RootStorage
           CapRis = min (MaxCapRis * Fraction, CapRis, max(0.D0, RootStorageMax - RootStorage) )
        Endif
        RootStorage = RootStorageInitial + DL - RootEvap + CapRis

! May 2015 check RootStorage>RootStorageMax, adjust DL and G, or CapRis
        If (RootStorage .gt. RootStorageMax) then
           if (idebug .gt.0) write(Idebug,*) '  DL                         ',DL
           if (idebug .gt.0) write(Idebug,*) '  RootEvap                   ',RootEvap
           if (idebug .gt.0) write(Idebug,*) '  CapRis                     ',CapRis
           if (idebug .gt.0) write(Idebug,*) '  Correction for RootStorage ',RootStorage
           if (DL .gt. 0) then
              Change = RootStorage - RootStorageMax
              Change = min (DL, Change)
              DL = DL - Change
              G = G + Change
              RootStorage = RootStorage - Change
           endif
!          If (RootStorage .gt. RootStorageMax) then
!            Capris Already limited by RootStorageMx-Rootstorage, so should not be needed here anymore
!             if (CapRis .gt. 0) then
!                Change = RootStorage - RootStorageMax
!                Change = min (CapRis, Change)
!                CapRis = CapRis - Change
!                RootStorage = RootStorage - Change
!             Endif
           if (RootStorage .gt. RootStorageMax) then
!              write(*,*) ' Error: RootStorage > Max! ', RootStorage, RootStorageMax
              if (idebug .gt.0) write(Idebug,*) ' Error: RootStorage > Max! ', RootStorage, RootStorageMax
           endif
!          Endif
        Endif

! === Runoff Routine ====================================================
!
! --- Baseflow Routing --------------------------------------------------
! May 2015: include CapRis, Pumpflow (both going out of Gw reservoir)
        BaseStorage = BaseStorage + G - CapRis - Pumpflow
        Gwl = Gwl0 - (BaseStorage / SpecificYield / 1000.D0)
        Gwl = min (Gwl, Gwl0)
        Gwl = max (Gwl, 0D0)

! May 2015: check on max. Basestorage (taking into account max. baseflow)
!       BaseStorageMax = Gwl0 * SpecificYield * 1000.D0  ! passed via parameter list
!       BaseFlowMax = BaseStorageMax/CKBF + BaseStorageMax/CKLow
! alternative
         BaseStorageMax0= ( min(Houtside,gwlBf0) - gwl) * SpecificYield * 1000.D0
         BaseStorageMax1= ( min(Houtside,gwlBf1) - gwl) * SpecificYield * 1000.D0
         BaseFlowMax = max (0.D0, BaseStorageMax0/CKBF) + max (0.D0, BaseStorageMax1/CKLow)  ! 5 Jan 2016, each term should be >=0
!        if (Houtside .lt. gwl) then
!           TempStorage = (gwl - Houtside) * SpecificYield * 1000.D0
!           BaseFlowMax = TempStorage / CkInf * -1.D0 ! infiltration from sw
!        endif
! end alternative
        BaseStorageMax = BaseStorageMax + BaseFlowMax
        if (idebug .gt.0) write(Idebug,*) '  BaseFlowMax',BaseFlowMax
        if (idebug .gt.0) write(Idebug,*) '  BaseStorageMax',BaseStorageMax
        if (BaseStorage .gt. BaseStorageMax) then
           DeltaBaseStorage = BaseStorage - BaseStorageMax
           if (idebug .gt.0) write(Idebug,*) '  Correction for DeltaBaseStorage ',DeltaBaseStorage
           if (Pumpflow .lt. 0) then
              ! reduce inflow into gw by pumping
              if (idebug .gt.0) write(Idebug,*) '  Correction for pumping, old value ',PumpFlow
              if (idebug .gt.0) write(Idebug,*) '  Correction DeltabaseStorage ',DeltaBaseStorage
              Change = min (-1.D0 * Pumpflow, DeltaBaseStorage)
              Pumpflow = Pumpflow + Change
              DeltaBaseStorage = DeltaBaseStorage - Change
              BaseStorage = BaseStorage - Change
              if (idebug .gt.0) write(Idebug,*) '  Correction for pumping, new value ',PumpFlow
              if (idebug .gt.0) write(Idebug,*) '  Remaining DeltabaseStorage ',DeltaBaseStorage
           endif
           if (DeltaBaseStorage .gt. 0D0) then
             if (idebug .gt.0) write(Idebug,*) '  Correction for G, old value ',G
             G = G - DeltaBaseStorage
             if (idebug .gt.0) write(Idebug,*) '  Correction for G, new value ',G
             If (PNReturnValue .gt. 0) then
                ! already excess rain, too much G is added to overland flow
                 if (idebug .gt.0) write(Idebug,*) '  Increase Overland flow QOF old value=', QOF
                 QOF = QOF + DeltaBaseStorage
                 InflowOverlandflowRsv = QOF
                 DeltaBaseStorage = 0.0
                 if (idebug .gt.0) write(Idebug,*) '  Increase Overland flow QOF new value=', QOF
             else
                ! first fill up upper zone storage, if still too much add to overland flow
                if (RootStorage .lt. RootStorageMax) then
                    if (idebug .gt.0) write(Idebug,*) '  Increase Rootstorage'
                    if (idebug .gt.0) write(Idebug,*) '    Old value =', RootStorage
                    DeltaRootStorage = min (RootStorageMax-RootStorage, DeltaBaseStorage)
                    RootStorage = Rootstorage + DeltaRootStorage
                    if (idebug .gt.0) write(Idebug,*) '    New value =', RootStorage
                    ! to be added: additional interflow InflowInterflowRsv ? (QIF increase)
                else
                    DeltaRootStorage = 0.D0
                endif
                if (idebug .gt.0) write(Idebug,*) '  Remaining increase Overland flow QOF old value=', QOF
                QOF = QOF + (DeltaBaseStorage - DeltaRootStorage)
                InflowOverlandflowRsv = QOF
                if (idebug .gt.0) write(Idebug,*) '  Increase Overland flow QOF new value=', QOF
             endif
           endif
           BaseStorage = BaseStorageMax
        endif
! two components of baseflow, CKBF and CKLow
! to be added: check on gwl levels and outside water levels, check for infiltration
        Gwl = Gwl0 - (BaseStorage / SpecificYield / 1000.D0)
        Gwl = min (Gwl, Gwl0)
        Gwl = max (Gwl, 0D0)
        QBF  = 0.0D0
        QLow = 0.0D0
        QInf = 0.0D0
        if (idebug .gt.0) write(Idebug,*) '  BaseStorage =', BaseStorage
        if (idebug .gt.0) write(Idebug,*) '  GWL =', GWL
! Qbf, Qlow, Qinf
        if (HOutside .lt. gwl) then
           TempStorage = (gwl - Houtside) * SpecificYield * 1000.D0
           Qinf = TempStorage / CKInf
           if (idebug .gt.0) write(Idebug,*) ' TempStorage Qinf= ', TempStorage
           ! Qinf should be limited by available storage (BaseStoragemax)
           Qinf = min (Qinf, BaseStorageMax - BaseStorage)
        else   ! Houtside >= gwl in meters below surface, dus geen infiltratie maar drainage
           if (HOutside .gt. Gwlbf0) then
               TempStorage = (gwlbf0 - gwl) * SpecificYield * 1000.D0
           else
               TempStorage = (Houtside - gwl) * SpecificYield * 1000.D0
           endif
           if (idebug .gt.0) write(Idebug,*) ' TempStorage Qbf = ', TempStorage
           TempStorageQbf = TempStorage
           if (TempStorage .gt. 0) Qbf = TempStorage / CKbf
           if (HOutside .gt. Gwlbf1) then
               TempStorage = (gwlbf1 - gwl) * SpecificYield * 1000.D0
           else
               TempStorage = (Houtside - gwl) * SpecificYield * 1000.D0
           endif
           if (idebug .gt.0) write(Idebug,*) ' TempStorage Qlow= ', TempStorage
           if (TempStorage .gt. 0) Qlow = TempStorage / CKlow
           TempStorageQlow = TempStorage
        endif
        BaseStorage0 = BaseStorage
        BaseStorage = BaseStorage - QBF - QLOW + Qinf
! check dat qbf + qlow < basestorage + inflow, (ckbf>=1, cklow>=1, maar ckbf+cklow kan samen er wel meer uittrekken dan er aan basestorage is )
! reduce Qlow
        TempStorage = (gwl0 - gwlbf1) * SpecificYield*1000.D0       ! storage below gwlbf1
        if (idebug .gt.0) write(Idebug,*) ' TempStorage gwlbf1-gwl0 ', TempStorage
        If (BaseStorage .lt. TempStorage .and. (Qbf + Qlow .ge. BaseStorage0 - TempStorage) ) then
           QLow = max(0.0D0, min (Qlow, BaseStorage0 - TempStorage - Qbf + Qinf))
           if (idebug .gt.0) write(Idebug,*) ' adjusted Qlow to ', Qlow
           BaseStorage = BaseStorage0 - QBF - QLOW + Qinf
        endif

! check that gw storage >=0, otherwise reduce abstractions (pumping, baseflow, CapRis)
        if (BaseStorage .lt. 0) then
           if (pumpflow .gt. 0) then
               ! reduce abstraction by pumping
               Change = min (pumpflow, -1.D0 * BaseStorage)
               pumpflow = pumpflow - Change
               BaseStorage = BaseStorage + Change
           endif
           if (BaseStorage .lt. 0) then
               ! reduce baseflow
               Change = min (QBF, -1.D0 * BaseStorage)
               QBF = QBF - Change
               BaseStorage = BaseStorage + Change
           endif
           if (BaseStorage .lt. 0) then
               ! reduce CapRis
               Change = min (CapRis, -1.D0 * BaseStorage)
               CapRis = CapRis - Change
               BaseStorage = BaseStorage + Change
           endif
           if (BaseStorage .lt. 0) then
              if (idebug .gt.0) write(Idebug,*) ' Error: BaseStorage negative! ', BaseStorage
              ! assume numerical error if basestorage only very little negative
              If (BaseStorage .gt. -1.0D-5) BaseStorage = 0.0D0
           endif
        endif

! --- Interflow Routing -------------------------------------------------
! Sept 2015
        InterflowStorage1 = InterflowStorage1 + QIF
! end Sept 2015
        QIF1to2           = InterflowStorage1 * CK12
        InflowInterflowRsv2 = QIF1to2
! adjusted GP Sept2011 for QIF term
        InterflowStorage1 = InterflowStorage1 - QIF1to2
        InterflowStorage2 = InterflowStorage2 + QIF1to2
        QIF               = InterflowStorage2 * CK12
        InterflowStorage2 = InterflowStorage2 - QIF


! --- Overland flow Routing ---------------------------------------------
        OverlandStorage = OverlandStorage + QOF
        if (idebug .gt.0) then
           write(Idebug,*) ' Overland Storage step2 = ', OverlandStorage
           write(Idebug,*) ' OFSMIN (mm)            = ', OFSMin
           write(Idebug,*) ' Beta                   = ', Beta
        endif
        if (OverlandStorage .lt. OFSmin) then
           QOF = 1.D0
        else
           QOF = (OverlandStorage / OFSmin) ** (-Beta)
        endif
        ! Jan 2016 correction for CK12*QOF, in NAM CK12 in hours,here Ck12 converted to 1/hours (1/timesteps)
        QOF = min(OverlandStorage, (1.D0 / QOF) * CK12 * OverlandStorage)   ! not more than OverlandStorage, 16 Dec 2015
        OverlandStorage = OverlandStorage - QOF
        if (idebug .gt.0) then
           write(Idebug,*) ' Overland Flow step2    = ', QOF
        endif

! === Results ===========================================================
        runoff=QIF+QOF+QBF+QLOW-Qinf                   !in mm


! store final storages
       OverlandStorageFinal   = OverlandStorage
       SurfStorageFinal       = SurfStorage
       RootStorageFinal       = RootStorage
       InterflowStorage1Final = InterflowStorage1
       InterflowStorage2Final = InterflowStorage2
       BaseStorageFinal       = BaseStorage
       GwlFinal = Gwl0 - (BaseStorageFinal / SpecificYield / 1000.D0)

    if (idebug .gt.0) then
       write(Idebug,*) ' Precipitation       = ', Precipitation
       write(Idebug,*) ' PotEvap             = ', PotEvap
       write(Idebug,*) ' OverlandStorageInit = ', OverlandStorageInitial
       write(Idebug,*) ' SurfStorageInitial  = ', SurfStorageInitial
       write(Idebug,*) ' RootStorageInitial  = ', RootStorageInitial
       write(Idebug,*) ' Interflow1_Initial  = ', InterflowStorage1Initial
       write(Idebug,*) ' Interflow2_Initial  = ', InterflowStorage2Initial
       write(Idebug,*) ' BaseStorageInitial  = ', BaseStorageInitial
       write(Idebug,*) ' BaseStorageMax      = ', BaseStorageMax
       write(Idebug,*) ' GwlInitial          = ', GwlInitial
       write(Idebug,*) ' Gwlbf0              = ', Gwlbf0
       write(Idebug,*) ' Gwlbf1              = ', Gwlbf1
       write(Idebug,*) ' Gwlfl1              = ', Gwlfl1
       write(Idebug,*) ' Gwl0                = ', Gwl0
       write(Idebug,*) ' Houtside            = ', Houtside
       write(Idebug,*) ' SpecificYield       = ', SpecificYield
       write(Idebug,*) ' SurfEvap            = ', SurfEvap
       write(Idebug,*) ' RootEvap            = ', RootEvap
       write(Idebug,*) ' GW-recharge         = ', G
       write(Idebug,*) ' Rootzone-DL         = ', DL
       write(Idebug,*) ' CapRis              = ', CapRis
       write(Idebug,*) '  Pumpflow           = ', Pumpflow
       write(Idebug,*) ' BaseFlow            = ', QBF,QLOW, Qbf+Qlow
       write(Idebug,*) ' Infiltration from sw= ', QInf
       write(Idebug,*) ' InflowInterflowRsv  = ', InflowInterflowRsv
       write(Idebug,*) ' InflowInterflowRsv2 = ', InflowInterflowRsv2, QIF1to2
       write(Idebug,*) ' InterFlow           = ', QIF
       write(Idebug,*) ' InflowOverlandRsv   = ', InflowOverlandflowRsv
       write(Idebug,*) ' Overland Flow       = ', QOF
       write(Idebug,*) ' OverlandStorageFinal= ', OverlandStorageFinal
       write(Idebug,*) ' SurfStorageFinal    = ', SurfStorageFinal
       write(Idebug,*) ' RootStorageFinal    = ', RootStorageFinal
       write(Idebug,*) ' Interflow1_Final    = ', InterflowStorage1Final
       write(Idebug,*) ' Interflow2_Final    = ', InterflowStorage2Final
       write(Idebug,*) ' BaseStorageFinal    = ', BaseStorageFinal
       write(Idebug,*) ' GwlFinal            = ', GwlFinal
    endif

   Return
  END subroutine NAMCalculations


  Subroutine NAMFluxes (A,RRel,Tr,Q)
  Double Precision A,RRel,Tr,Q

  if (RRel .gt. Tr) then
      Q=A*(RRel-Tr)/(1.D0-Tr)
  else
      Q=0.D0
  endif

   Return
  END subroutine NAMFluxes

! === NAM Variables Description=======================================
! ===    Note: all input data are converted to mm, mm/timestep, timesteps for easy computation ===
! === NAM Variables Description=======================================
!       SurfStorageMax - Capacity of Surface Storage (mm)
!       RootStorageMax - Capacity of Lower Storage Zone (mm)
!       Tr - Threshold (general for subroutine calling)
!       TOF - Threshold Value for Overland Flow
!       TIF - Threshold Value for Interflow
!       TG - Threshold Value for Groundwater Discharge
!       CQOF - Overland Flow Runoff Coefficient
!       CKIF - Interflow Time Constant (day)
!       CK12 - Interflow Runoff Constant (day^-1)
!       OFmin - Upper Limit Overland Flow (mm/h)
!       OFSmin - Upper Limit Overland Flow Storage (mm)
!       Beta - Manning Over Land Flow Parameter
!       CKBF - Baseflow Time Constant (day)
!       Area - Catchment Area (km2)

!       SurfStorage - Storage of Upper Zone (mm)
!       RootStorage - Storage of Root Zone (mm)
!       InterflowStorage1 - Initial Content on First Linear Interflow Reservoir (mm/h)
!       InterflowStorage2 - Initial Content on Second Linear Interflow Reservoir (mm/h)
!       BaseStorage - Initial Content on Baseflow Reservoir (mm/h)
!       OverlandStorage - Initial content on Overland Flow Reservoir (mm/h)
!       RRel - Relation between Initial and Capacity Storage Root Zone
!       SurfEvap - Evapotranspiration on Upper Zone (mm)
!       RootEvap - Evapotranspiration from the Root Zone (mm)
!       QOF - Overland Flow(mm/day)
!       QIF - Interflow (mm/day)
!       QIF1to2 - Transition between the 2 Linear Interflow Reservoir (mm/h)
!       QBF - Baseflow (mm/day)
!       PN - Excess of Water on Upper Zone (mm)
!       G - Groundwater Recharge (mm)
!       DL - Part of PN that is kept on the Root Zone (mm)
!       A - Auxiliary Variable for Fluxes Subroutine (mm)


  Subroutine LGSI_FlowHighToLowSubArea (LGSIInterflow, Hdiff, C )

  double precision LGSIInterflow, HDiff, C

  ! to be implemented
  LGSIInterflow = Hdiff / max (0.001d0, C)

  Return
  END subroutine LGSI_FlowHighToLowSubArea


 End module RunoffFormulations
