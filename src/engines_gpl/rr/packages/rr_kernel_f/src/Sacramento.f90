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

 !======================================================================
!
!     Company name         : Deltares
!                            Rotterdamseweg 185
!                            2628 HD Delft
!                            The Netherlands
!
!======================================================================
!
!     Module: Sacramento: subroutines Samini and Landsc
!
!
!======================================================================
!
!     Programmers      : Burnash and Ferral / Henk Ogink / Johan Crebas
!
!======================================================================

!
  integer function SegmentUH (dro, munit, nunit, juhsz) result (errStatus)

!     create UH-components for juhsz time steps from nunit
!     given UH-components, by interpolation

!
!     nunit   :  number of UH-components
!     juhsz   :  number of time steps

      INTEGER nunit, munit, juhsz

!     real (4) DRO(121),tmpu(150)
      real (4) DRO(501),tmpu(550)
!
      integer i,j,k, l
      real    stpu, suh

      errStatus = 0
!
! number of UH-components nunit

      do 25 i=1,munit
        k=munit-i+1
        if(dro(k).gt.0.) goto 30
   25 continue
   30 nunit=k
!
! UH-time step
      if(juhsz.eq.0) juhsz=1
!
!     create components for juhsz time steps from nunit
!     given components by interpolation

      do i = 1, nunit
        j = nunit - i + 1
        dro(j+1) = dro (j)
      enddo
      nunit = nunit + 2
      dro(1) = 0.
      dro(nunit) = 0.

      k=1
      tmpu(1) = dro (1)
      do j=2,nunit
        stpu=(dro(j)-dro(j-1))/float(juhsz)
        k=k+1
        do l=k,k+juhsz-2
          tmpu(l)=tmpu(l-1)+stpu
        enddo
        k=k+juhsz-1
        tmpu(k) = dro(j)
      enddo

      nunit=k-2

      do j=1,nunit
        dro(j)=tmpu(j+1)
      enddo

      suh = 0.
      do j=1,nunit
        suh=suh+dro(j)
      enddo

      do 536 j=1,nunit
  536   dro(j)=dro(j)/suh

      errStatus = 0

      END function


 integer function Samini(idoor,ReadAdimCInRestartFile,FirstCall,LZFSM,LZFPM, &
                         UZTWC,UZFWC,LZTWC,LZFSC,LZFPC, &
                         LZSK,LZPK,RSERV,PCTIM,ADIMP,SARVA,SIDE, &
                         STOR,ADIMC,SID,SAVED,PERCM,QQ,QD,QS,QI) result (errStatus)

! SamIni COMPUTES SOME INITIAL VALUES OF LAND SUBROUTINE AND
! ADJUSTS BASEFLOW CAPACITIES AND CONTENTS
!
! idoor > 0 : restart, no initialisation for some parameters
! FirstCall : when Samini is called in loop i = 1, n, FirstCall = True when i = 1
! ReadAdimCInRestartFile: when AdimC is Read In RestartFile, ReadAdimCInRestartFile = True

      integer idoor
      REAL    LZFSM,LZFPM,LZTWC,LZFSC,LZFPC,LZSK,LZPK,QQ(501) ,QD(501),QS(501),QI(501)    ! 121
      REAL    UZTWC, UZFWC
      REAL    RSERV, PCTIM, ADIMP, SARVA, SIDE, STOR, ADIMC, SID, SAVED, PERCM

      logical ReadAdimCInRestartFile,FirstCall

      ! local variables
      integer i


      errStatus = 0

        SID=1./(1.+SIDE)
      SAVED=RSERV*(LZFPM+LZFSM)
      LZFSM=LZFSM*(1.+SIDE)
      LZFPM=LZFPM*(1.+SIDE)
      PERCM=LZFSM*LZSK+LZFPM*LZPK

      if (idoor.eq.0 .and. FirstCall) then
!       should not be adjusted if this is a restart run!
        LZFSC=LZFSC*(1.+SIDE)
        LZFPC=LZFPC*(1.+SIDE)
      endif

      if (.not. ReadAdimCInRestartFile) ADIMC=UZTWC+LZTWC

      STOR=(UZTWC+UZFWC+LZTWC+LZFSC+LZFPC)*(1.-PCTIM-ADIMP)+ADIMC*ADIMP

!     INITIALISE SURFACE RUNOFF

      if(idoor.eq.0 .and. FirstCall) then
        DO 30 I=1,501  ! 121
           QQ(i) =0
           QD(i) =0
           QS(i) =0
           QI(i) =0
   30 Continue
      else
! restart run
! (in calling routine)
!     if(idoor.eq.1.and. FirstCall) &
!          open(33,file='samlnd.uhc',status='old',form='unformatted')
!       read(33) qq
!        idoor=2
      endif

      IF(SARVA-PCTIM) 50,50,40
   40 SARVA=PCTIM

   50 CONTINUE

      END function

 integer function LANDSC(FLOSF,      &
                         FRACT,      &
                         UZTWM,UZFWM,LZTWM,LZFSM,LZFPM,   &
                         UZTWC,UZFWC,LZTWC,LZFSC,LZFPC,   &
                         DCUZ,DCLZS,DCLZP,ZPERC,REXP,PFREE ,  &
                         PCTIM,ADIMP,SARVA,SSOUT,  &
                         DRO,PM,PT1,PT2,NUNIT,    &
                         PLIQ,EDMND,QF,     &
                         STOR,ADIMC,SID,SAVED,PERCM,EUSED,FLOBF, &
                         QQ,      &
                         FLOIN, FLOBS, ROIMP, SSOUTACT) result (errStatus)

!  Landsc computes streamflow components
!  including of time distribution of surface runoff

      INTEGER  NUNIT
!     REAL (4)   DRO(121), QQ(121)
      REAL (4)   DRO(501), QQ(501)
      REAL (4)   UZTWM, UZFWM, UZTWC, UZFWC, LZTWC,LZFSC,LZFPC,LZTWM,LZFPM,LZFSM

      real       DCUZ, DCLZS, DCLZP, ZPERC, REXP, PFREE, PCTIM, ADIMP, SARVA, SSOUT, PM, PT1, PT2, PLIQ, EDMND, QF
      real       STOR, ADIMC, SID, SAVED, PERCM, EUSED, FLOBF, FLOIN, FLOBS, FLOSF, FRACT, ROIMP, SSOUTACT

!              UZTWC - UPPER ZONE TENSION WATER CONTENTS
!              UZFWC - UPPER ZONE FREE WATER CONTENTS
!              LZTWC - LOWER ZONE TENSION WATER CONTENTS
!              LZFSC - LOWER ZONE SUPPLEMENTAL FREE WATER CONTENTS
!              LZFPC - LOWER ZONE PRIMARY FREE WATER CONTENTS
!              UZTWM - UPPER ZONE TENSION WATER CAPACITY
!              UZFWM - UPPER ZONE FREE WATER CAPACITY
!              LZTWM - LOWER ZONE TENSION WATER CAPACITY
!              LZFSM - LOWER ZONE SUPPLEMENTAL FREE WATER CAPACITY
!              LZFPM - LOWER ZONE PRIMARY FREE WATER CAPACITY
!              FRACT - FRACTION OF A DAY FOR WHICH COMPUTATIONS ARE TO Be made
!               annual data,  fract = 365
!               monthly data, fract = 30
!               daily data,   fract = 1
!               hourly data,  fract = 24
!              SAVED - THE DEPTH OF LOWER ZONE FREE WATER NOT AVAILABLE for
!                        TRANSFER TO TENSION - MM   SAVED=RSERV*(LZFPM+Lzfsm)
!              ZPERC - PERCOLATION MULTIPLIER, WET TO DRY SITUATION
!              PCTIM - FRACTION OF BASIN COVERED BY IMPERVIOUS SURFACE
!                        AND ADJOINING STREAM CHANNELS
!              REXP  - EXPONENT AFFECTING RATE OF CHANGE OF PERCOLATION
!                        BETWEEN WET AND DRY CONDITIONS
!              DCUZ  - FRACTION OF UZFWC LOST TO INTERFLOW IN ONE DAY
!              DCLZS - FRACTION OF LZFSC LOST TO BASEFLOW IN ONE DAY
!              DCLZP - FRACTION OF LZFPC LOST TO BASEFLOW IN ONE DAY
!              PFREE - PERCOLATION FRACTION DIRECTLY LOADED TO LOWER ZONe
!                      FREE WATER
!              EDMND - DEMAND FOR EVAPORATION DURING THE COMPUTATION PERiod
!            * EUSED - USE OF WATER BY EVAPOTRANSPIRATION DURING THE PERiod
!              PLIQ  - LIQUID PRECIPITATION AND SNOWMELT DURING PERIOD
!              PERCM - SATURATED PERCOLATION RATE. PERCM=DCLZP*LZFPM+DCLzs*lzfsm
!            * FLOSF - SURFACE RUNOFF COMPUTED - MM
!            * FLOIN - INTERFLOW COMPUTED - MM
!            * FLOBF - BASE FLOW COMPUTED - MM
!              ADIMP - ADDITIONAL PERCENT IMPERVIOUS WHEN ALL TENSION
!                      WATER STORAGE HAS BEEN FILLED
!              ADIMC - CONTENTS OF AREA WHICH WHEN SATURATED PRODUCES DIrect ro


!     *  VALUES GENERATED IN SUBROUTINE

            ! local variables
      integer itime, Ninc, inc, J4, J5, J6
      real    A, B, E1, E2, E3, E4, E5, EDUZ, DEL, DELA, PAV, ADJ, dinc, pinc, &
              duz, DLZP, DLZS, RATIO, RED, ADDRO, BF, PERC, CHECK, SPERC, REST, HPL, RATLP, RATLS, PERCS


!Error codes

!errStatus .eq. 1: 'Error in Landsc.f90, UZTWM = 0'
!errStatus .eq. 2: 'Error in Landsc.f90, UZFWM = 0'
!errStatus .eq. 3: 'Error in Landsc.f90, PT2 = 0'
!errStatus .eq. 4: 'Error in Landsc.f90, PAV = 0'
!errStatus .eq. 5: 'Error in Landsc.f90, LZTWM = 0'
!errStatus .eq. 6: 'Warning: in Landsc.f90, Percolation set to 0'


!     * * * * * * * * * * * * * * * * *
!     * DETERMINE EVAPORATION LOSSES  *
!     * * * * * * * * * * * * * * * * *

!     COMPUTE TRANSPIRATION LOSS FROM UPPER ZONE TENSION WATER

      errstatus = 0

      E2=0.
      if (UZTWM.eq.0) then
        errStatus = 1
!    'Error in Landsc, UZTWM = 0'
        return
      endif
      E1=EDMND * UZTWC/UZTWM
      RED=EDMND-E1

      UZTWC=UZTWC-E1
      IF(UZTWC) 100,155,155
  100 EDUZ=E1
      E1=E1 + UZTWC
      UZTWC=0.

!     COMPUTE TRANSPIRATION LOSS FROM UPPER ZONE FREE WATER

      EDUZ=EDUZ-E1
      IF(UZFWC-EDUZ) 130,140,140
 130  E2=UZFWC
      UZFWC=0.
      GO TO 170
  140 E2=EDUZ
      UZFWC=UZFWC-E2

!     IF UPPER ZONE FREE WATER RATIO EXCEEDS UPPER ZONE TENSION
!       CONTENT RATIO, TRANSFER FREE WATER INTO TENSION

 155  CONTINUE
      if (UZFWM.eq.0) then
        errStatus = 2
        return
      endif
      A=UZTWC/UZTWM
      B=UZFWC/UZFWM
      IF(A-B)160,170,170
 160  A=(UZTWC+UZFWC)/(UZTWM+UZFWM)
      UZTWC=UZTWM*A
      UZFWC=UZFWM*A

  170 RED=EDMND-E1

!     COMPUTE EVAPORATION LOSS FROM ADIMP AREA
!     call logwrite('COMPUTE EVAPORATION LOSS FROM ADIMP AREA')

      E5=E1+RED*(ADIMC-E1-UZTWC)/(UZTWM+LZTWM)
      ADIMC=ADIMC-E5
      IF(ADIMC) 211,212,212
  211 E5=E5+ADIMC
      ADIMC=0.
  212 E5=E5*ADIMP

!     COMPUTE TRANSPIRATION LOSS FROM LOWER ZONE TENSION
!     call logwrite('COMPUTE TRANSPIRATION LOSS FROM LZT')

      E3=(RED-E2)*LZTWC/(UZTWM+LZTWM)
      LZTWC=LZTWC-E3
      IF(LZTWC)180,185,185
 180  E3=E3+LZTWC
      LZTWC=0.

!     RESUPPLY LOWER ZONE TENSION WATER FROM LOWER ZONE FREE IF MORE
!       WATER AVAILABLE THERE.

 185  CONTINUE
      A=LZTWC/LZTWM
      B=(LZFPC+LZFSC-SAVED+LZTWC)/(LZFPM+LZFSM-SAVED+LZTWM)
      IF(A-B)190,210,210
 190  DEL=(B-A)*LZTWM

!     TRANSFER WATER FROM LOWER ZONE SECONDARY FREE WATER TO LOWER ZONE tension
!     call logwrite('TRANSFER WATER FROM LZSFW TO LZ')

      DELA=LZFSC+LZFPC
      IF(DEL .GT. DELA) DEL=DELA
      LZTWC=LZTWC+DEL
      LZFSC=LZFSC-DEL
      IF(LZFSC)200,210,210

!     TRANSFER PRIMARY FREE WATER IF SECONDARY FREE WATER INADEQUATE
!     call logwrite('TRANSFER WATER IF SFW INADEQUATE')

 200  LZFPC=LZFPC+LZFSC
      LZFSC=0.
 210  CONTINUE

!     * * * * * * * * *
!     * ADD RAINFALL  *
!     * * * * * * * * *

!     RUNOFF FROM IMPERVIOUS OR WATER-COVERED AREA
!     call logwrite('RUNOFF FROM IMPERVIOUS OR WATER-COVERED AREA')

      ROIMP=PLIQ*PCTIM

!     REDUCE RAIN BY AMOUNT OF UPPER ZONE TENSION WATER DEFICIENCY.
!     FILL UPPER ZONE TENSION WATER AS MUCH AS RAIN PERMITS
!     PAV = Peff

      PAV=PLIQ+UZTWC-UZTWM
      IF(PAV)220,230,230
!     PAV<0 goto 220, if PAV = 0 the goto 230, if PAV > 0 then goto 230
 220  UZTWC=UZTWC+PLIQ
      PAV=0.
      GO TO 240
 230  UZTWC=UZTWM

!     FILL ADIMC AND INITIALIZE BASE-,SURFACE- AND INTERFLOW

  240 ADIMC=ADIMC+PLIQ-PAV
      FLOBF=0.
      FLOSF=0.
      FLOIN=0.

!     DETERMINE NUMBER OF INCREMENTS

!     IF RAINFALL EXCEEDS THRESHOLDS(PT1,2) THEN RAINFALL IS LUMPED
!     IN LESS THAN THE TOTAL PERIOD TO SIMULATE INTENSITY VARIATION
!     EFFECT

!     lumping depends on thresholds
      IF(PAV-PT1) 246,246,242
  242 IF(PAV-PT2) 243,244,244
  246 ITIME=2
      ADJ=1.
      GOTO 390
  243 if (PT2.eq.0) then
        errStatus = 3
        return
      endif
      ADJ=.5*SQRT(PAV/PT2)

      GOTO 245
  244 if (PAV.eq.0) then
        errStatus = 4
        return
      endif
      ADJ=1. - .5*PT2/PAV

  245 ITIME=1
!     end lumping

  390 NINC=1.+PM*(UZFWC*FRACT*ADJ+PAV)
      DINC=NINC
      DINC=1./DINC
      PINC=PAV*DINC
      DINC=DINC*FRACT*ADJ

!     ADJUSTMENT OF FREE-WATER STORAGE DEPLETION COEFFICIENTS FROM 1/DAY
!     TO 1/DELTA T

      DUZ =1.-(1.-DCUZ )**DINC
      DLZP=1.-(1.-DCLZP)**DINC
      DLZS=1.-(1.-DCLZS)**DINC

!     BEGIN DRAINAGE AND PERCOLATION LOOP
!     call logwrite('BEGIN DRAINAGE AND PERCOLATION LOOP')

      DO 385 INC=1,NINC
      PAV=PINC

!     RUNOFF FROM ADIMP AREA (FIRST STEP)

      if (LZTWM.eq.0) then
        errStatus = 5
        return
      endif
      RATIO=(ADIMC-UZTWC)/LZTWM
      ADDRO=PINC*RATIO*RATIO

!     COMPUTE BASEFLOW FROM LOWER ZONE

      BF=LZFPC*DLZP
      FLOBF=FLOBF+BF
      LZFPC=LZFPC-BF
      BF=LZFSC*DLZS
      LZFSC=LZFSC-BF
      FLOBF=FLOBF+BF

!     COMPUTE PERCOLATION
!     NO PERCOLATION COMPUTATIONS IF NO WATER AVAILABLE TO PERCOLATE

      IF(PINC+UZFWC-.01 )380,380,250

!     COMPUTE PERCOLATION FROM UPPER ZONE TO LOWER

 250  PERC=PERCM*DINC
      if (UZFWM.eq.0) then
        errStatus = 2
        return
      endif
      PERC=PERC*UZFWC/UZFWM

!     MODIFICATION FOR FASTER PERCOLATION INTO MOISTURE-DEFICIENT SOIL
!       IN LOWER LAYER

!     Check value of exponent
      If ((1.-(LZFPC+LZFSC+LZTWC)/(LZFPM+LZFSM+LZTWM)).GT.0) then
         PERC=PERC*(1.+(ZPERC*(1.-(LZFPC+LZFSC+LZTWC)/(LZFPM+LZFSM+LZTWM))**REXP))
      Else
        errStatus = 6
!       'Warning: Percolation set to ',PERC
      END IF

      IF(PERC-UZFWC)270,270,260
 260  PERC=UZFWC
      UZFWC=0
      GOTO 300
 270  UZFWC=UZFWC-PERC
      CHECK=LZFPC+LZFSC+LZTWC+PERC-LZFPM-LZFSM-LZTWM
      IF(CHECK)290,290,280
 280  PERC=PERC-CHECK
      UZFWC=UZFWC+CHECK

!     COMPUTE INTERFLOW
!     call logwrite('COMPUTE INTERFLOW')

 290  DEL=DUZ*UZFWC
      FLOIN=FLOIN+DEL
      UZFWC=UZFWC-DEL

!     DISTRIBUTE PERCOLATED WATER

  300 CONTINUE
      SPERC=PERC
      PERC=PERC*(1.-PFREE)
      IF(PERC-LZTWM+LZTWC)310,310,320
 310  LZTWC=LZTWC+PERC
      PERC=0.
      GOTO 321
 320  PERC=PERC-LZTWM+LZTWC
      LZTWC=LZTWM

!     DISTRIBUTE PERIOD PERCOLATION IN EXCESS OF LOWER ZONE TENSION
!     REQUIREMENT

 321  PERC=PERC+SPERC*PFREE
      IF(PERC)340,340,323
  323 REST=PERC+LZFSC+LZFPC-LZFSM-LZFPM
      IF(REST)505,506,506
  506 LZTWC=LZTWC+REST
      LZFSC=LZFSM
      LZFPC=LZFPM
      GOTO 340
  505 HPL=LZFPM/(LZFPM+LZFSM)
      RATLP=1.-LZFPC/LZFPM
      RATLS=1.-LZFSC/LZFSM
      PERCS=PERC*(1.-HPL*2.*RATLP/(RATLP+RATLS))
      LZFSC=LZFSC+PERCS
      IF(LZFSC-LZFSM)330,330,322
 322  PERCS=PERCS-LZFSC+LZFSM
      LZFSC=LZFSM
 330  CONTINUE
      LZFPC=LZFPC+(PERC-PERCS)
 340  IF(PAV)380,380,350
 350  CONTINUE

!     DISTRIBUTE PERIOD RAIN IN EXCESS OF UPPER ZONE TENSION REQUIREMENT
!       TO UPPER ZONE FREE WATER AND SURFACE RUNOFF.

!     CHECK WHETHER RESIDUAL RAIN EXCEEDS AVAILABLE UPPER LEVEL FREE
!       WATER CAPACITY.

      IF(PAV-UZFWM+UZFWC)360,360,370
 360  UZFWC=UZFWC+PAV
      GO TO 380
 370  PAV=PAV-UZFWM+UZFWC
      UZFWC=UZFWM

!     COMPUTE SURFACE RUNOFF AND RUNOFF FROM ADIMP AREA (FINAL STEP)
!     call logwrite('COMPUTE SURFACE RUNOFF AND RUNOFF')

      FLOSF=FLOSF+PAV
      ADDRO=ADDRO+PAV*(1.-ADDRO/PINC)
 380  CONTINUE
      ADIMC=ADIMC+PINC-ADDRO
      ROIMP=ROIMP+ADDRO*ADIMP
 385  CONTINUE
      IF(ITIME-2) 396,395,395
  396 ITIME=ITIME+1
      ADJ=1.-ADJ
      PAV=0.
      GOTO 390
  395 CONTINUE

!     END OF DRAINAGE AND PERCOLATION LOOP

!     ROUTE SURFACE,IMPERVIOUS(=IMP+ADIMP) AND INTERFLOW
!     call logwrite('ROUTE SURFACE,IMPERVIOUS AND INTERFLOW')

      FLOSF=FLOSF*(1.-PCTIM-ADIMP)
      FLOIN=FLOIN*(1.-PCTIM-ADIMP)
      QQ(1)=FLOSF+ROIMP+FLOIN
      FLOSF=0.
      DO 66 J5=1,NUNIT
   66 FLOSF=FLOSF+QQ(J5)*DRO(J5)
!     write(6,'(11f7.3)') (qq(jj),jj=1,10),flosf
      DO 67 J4=2,NUNIT
      J6=NUNIT+2-J4
      J5=J6-1
   67 QQ(J6)=QQ(J5)
      FLOBF=FLOBF*(1.-PCTIM-ADIMP)
      FLOBS=FLOBF*(1-SID)
      FLOBF=FLOBF*SID

!     COMPUTE CHANNEL INFLOW
!     call logwrite('COMPUTE CHANNEL INFLOW')

      E4=EDMND*SARVA
      QF=FLOBF+FLOSF-SSOUT
! add GP
      SSOUTACT = SSOUT
! end add GP
      IF(QF) 71,72,72
   71 CONTINUE
! add GP; geval QF < 0
      SSOUTACT = SSOUT + QF
! end add GP
      QF=0.
   72 QF=QF-E4
      IF(QF) 75,76,76
   75 E4=E4+QF
      QF=0.
   76 CONTINUE

!     SUMMING UP
!     call logwrite('SUMMING UP')
      EUSED=(E1+E2+E3)*(1.-PCTIM-ADIMP)+E4+E5
      STOR=STOR+PLIQ-QF-EUSED-FLOBS
      END function

 integer function LANDSC2(FLOSF, FloDirect,FloSurf, FloInter, &
                         FRACT,      &
                         UZTWM,UZFWM,LZTWM,LZFSM,LZFPM,   &
                         UZTWC,UZFWC,LZTWC,LZFSC,LZFPC,   &
                         DCUZ,DCLZS,DCLZP,ZPERC,REXP,PFREE ,  &
                         PCTIM,ADIMP,SARVA,SSOUT,  &
                         DRO,PM,PT1,PT2,NUNIT,    &
                         PLIQ,EDMND,QF,     &
                         STOR,ADIMC,SID,SAVED,PERCM,EUSED,FLOBF, &
                         QQ, QD, QS, QI, &
                         FLOIN, FLOBS, ROIMP, SSOUTACT, PercAct) result (errStatus)

!  Landsc computes streamflow components
!  including of time distribution of surface runoff
! LANDSC2 is a copy of the original LANDSC function, with the following adjustments:
!  - additional output variables are passed to the higher level, allowing a more complete water balance to be given in the output
!    this relates to
!     - 21 surface runoff
!     - 22 interflow
!     - 23, 24, 25: routed paved direct runoff, routed unpaved surface runoff, routed interflow
!     - 26 percolation
!     - etc
!  - separation of routed direct paved impervious runoff, unpaved surface runoff and interflow


      INTEGER  NUNIT
!     REAL (4)   DRO(121), QQ(121)
      REAL (4)   DRO(501), QQ(501)
      Real (4)   QD(501), QS(501), QI(501)
      REAL (4)   UZTWM, UZFWM, UZTWC, UZFWC, LZTWC,LZFSC,LZFPC,LZTWM,LZFPM,LZFSM

      real       DCUZ, DCLZS, DCLZP, ZPERC, REXP, PFREE, PCTIM, ADIMP, SARVA, SSOUT, PM, PT1, PT2, PLIQ, EDMND, QF
      real       STOR, ADIMC, SID, SAVED, PERCM, EUSED, FLOBF, FLOIN, FLOBS, FLOSF, &
                 FloDirect,FloSurf, FloInter, FRACT, ROIMP, SSOUTACT, PercAct

!              UZTWC - UPPER ZONE TENSION WATER CONTENTS
!              UZFWC - UPPER ZONE FREE WATER CONTENTS
!              LZTWC - LOWER ZONE TENSION WATER CONTENTS
!              LZFSC - LOWER ZONE SUPPLEMENTAL FREE WATER CONTENTS
!              LZFPC - LOWER ZONE PRIMARY FREE WATER CONTENTS
!              UZTWM - UPPER ZONE TENSION WATER CAPACITY
!              UZFWM - UPPER ZONE FREE WATER CAPACITY
!              LZTWM - LOWER ZONE TENSION WATER CAPACITY
!              LZFSM - LOWER ZONE SUPPLEMENTAL FREE WATER CAPACITY
!              LZFPM - LOWER ZONE PRIMARY FREE WATER CAPACITY
!              FRACT - FRACTION OF A DAY FOR WHICH COMPUTATIONS ARE TO Be made
!               annual data,  fract = 365
!               monthly data, fract = 30
!               daily data,   fract = 1
!               hourly data,  fract = 24
!              SAVED - THE DEPTH OF LOWER ZONE FREE WATER NOT AVAILABLE for
!                        TRANSFER TO TENSION - MM   SAVED=RSERV*(LZFPM+Lzfsm)
!              ZPERC - PERCOLATION MULTIPLIER, WET TO DRY SITUATION
!              PCTIM - FRACTION OF BASIN COVERED BY IMPERVIOUS SURFACE
!                        AND ADJOINING STREAM CHANNELS
!              REXP  - EXPONENT AFFECTING RATE OF CHANGE OF PERCOLATION
!                        BETWEEN WET AND DRY CONDITIONS
!              DCUZ  - FRACTION OF UZFWC LOST TO INTERFLOW IN ONE DAY
!              DCLZS - FRACTION OF LZFSC LOST TO BASEFLOW IN ONE DAY
!              DCLZP - FRACTION OF LZFPC LOST TO BASEFLOW IN ONE DAY
!              PFREE - PERCOLATION FRACTION DIRECTLY LOADED TO LOWER ZONe
!                      FREE WATER
!              EDMND - DEMAND FOR EVAPORATION DURING THE COMPUTATION PERiod
!            * EUSED - USE OF WATER BY EVAPOTRANSPIRATION DURING THE PERiod
!              PLIQ  - LIQUID PRECIPITATION AND SNOWMELT DURING PERIOD
!              PERCM - SATURATED PERCOLATION RATE. PERCM=DCLZP*LZFPM+DCLzs*lzfsm
!            * FLOSF - SURFACE RUNOFF COMPUTED - MM
!            * FLOIN - INTERFLOW COMPUTED - MM
!            * FLOBF - BASE FLOW COMPUTED - MM
!              ADIMP - ADDITIONAL PERCENT IMPERVIOUS WHEN ALL TENSION
!                      WATER STORAGE HAS BEEN FILLED
!              ADIMC - CONTENTS OF AREA WHICH WHEN SATURATED PRODUCES DIrect ro


!     *  VALUES GENERATED IN SUBROUTINE

            ! local variables
      integer itime, Ninc, inc, J4, J5, J6
      real    A, B, E1, E2, E3, E4, E5, EDUZ, DEL, DELA, PAV, ADJ, dinc, pinc, &
              duz, DLZP, DLZS, RATIO, RED, ADDRO, BF, PERC, CHECK, SPERC, REST, HPL, RATLP, RATLS, PERCS


!Error codes

!errStatus .eq. 1: 'Error in Landsc.f90, UZTWM = 0'
!errStatus .eq. 2: 'Error in Landsc.f90, UZFWM = 0'
!errStatus .eq. 3: 'Error in Landsc.f90, PT2 = 0'
!errStatus .eq. 4: 'Error in Landsc.f90, PAV = 0'
!errStatus .eq. 5: 'Error in Landsc.f90, LZTWM = 0'
!errStatus .eq. 6: 'Warning: in Landsc.f90, Percolation set to 0'

      PercAct=0.  ! GP make sure SPERC is initialised in case of UZFWC <= 0.01
      SPERC = 0.

!     * * * * * * * * * * * * * * * * *
!     * DETERMINE EVAPORATION LOSSES  *
!     * * * * * * * * * * * * * * * * *

!     COMPUTE TRANSPIRATION LOSS FROM UPPER ZONE TENSION WATER

      errstatus = 0

      E2=0.
      if (UZTWM.eq.0) then
        errStatus = 1
!    'Error in Landsc, UZTWM = 0'
        return
      endif
      E1=EDMND * UZTWC/UZTWM
      RED=EDMND-E1

      UZTWC=UZTWC-E1
      IF(UZTWC) 100,155,155
  100 EDUZ=E1
      E1=E1 + UZTWC
      UZTWC=0.

!     COMPUTE TRANSPIRATION LOSS FROM UPPER ZONE FREE WATER

      EDUZ=EDUZ-E1
      IF(UZFWC-EDUZ) 130,140,140
 130  E2=UZFWC
      UZFWC=0.
      GO TO 170
  140 E2=EDUZ
      UZFWC=UZFWC-E2

!     IF UPPER ZONE FREE WATER RATIO EXCEEDS UPPER ZONE TENSION
!       CONTENT RATIO, TRANSFER FREE WATER INTO TENSION

 155  CONTINUE
      if (UZFWM.eq.0) then
        errStatus = 2
        return
      endif
      A=UZTWC/UZTWM
      B=UZFWC/UZFWM
      IF(A-B)160,170,170
 160  A=(UZTWC+UZFWC)/(UZTWM+UZFWM)
      UZTWC=UZTWM*A
      UZFWC=UZFWM*A

  170 RED=EDMND-E1

!     COMPUTE EVAPORATION LOSS FROM ADIMP AREA
!     call logwrite('COMPUTE EVAPORATION LOSS FROM ADIMP AREA')

      E5=E1+RED*(ADIMC-E1-UZTWC)/(UZTWM+LZTWM)
      ADIMC=ADIMC-E5
      IF(ADIMC) 211,212,212
  211 E5=E5+ADIMC
      ADIMC=0.
  212 E5=E5*ADIMP

!     COMPUTE TRANSPIRATION LOSS FROM LOWER ZONE TENSION
!     call logwrite('COMPUTE TRANSPIRATION LOSS FROM LZT')

      E3=(RED-E2)*LZTWC/(UZTWM+LZTWM)
      LZTWC=LZTWC-E3
      IF(LZTWC)180,185,185
 180  E3=E3+LZTWC
      LZTWC=0.

!     RESUPPLY LOWER ZONE TENSION WATER FROM LOWER ZONE FREE IF MORE
!       WATER AVAILABLE THERE.

 185  CONTINUE
      A=LZTWC/LZTWM
      B=(LZFPC+LZFSC-SAVED+LZTWC)/(LZFPM+LZFSM-SAVED+LZTWM)
      IF(A-B)190,210,210
 190  DEL=(B-A)*LZTWM

!     TRANSFER WATER FROM LOWER ZONE SECONDARY FREE WATER TO LOWER ZONE tension
!     call logwrite('TRANSFER WATER FROM LZSFW TO LZ')

      DELA=LZFSC+LZFPC
      IF(DEL .GT. DELA) DEL=DELA
      LZTWC=LZTWC+DEL
      LZFSC=LZFSC-DEL
      IF(LZFSC)200,210,210

!     TRANSFER PRIMARY FREE WATER IF SECONDARY FREE WATER INADEQUATE
!     call logwrite('TRANSFER WATER IF SFW INADEQUATE')

 200  LZFPC=LZFPC+LZFSC
      LZFSC=0.
 210  CONTINUE

!     * * * * * * * * *
!     * ADD RAINFALL  *
!     * * * * * * * * *

!     RUNOFF FROM IMPERVIOUS OR WATER-COVERED AREA
!     call logwrite('RUNOFF FROM IMPERVIOUS OR WATER-COVERED AREA')

      ROIMP=PLIQ*PCTIM

!     REDUCE RAIN BY AMOUNT OF UPPER ZONE TENSION WATER DEFICIENCY.
!     FILL UPPER ZONE TENSION WATER AS MUCH AS RAIN PERMITS
!     PAV = Peff

      PAV=PLIQ+UZTWC-UZTWM
      IF(PAV)220,230,230
!     PAV<0 goto 220, if PAV = 0 the goto 230, if PAV > 0 then goto 230
 220  UZTWC=UZTWC+PLIQ
      PAV=0.
      GO TO 240
 230  UZTWC=UZTWM

!     FILL ADIMC AND INITIALIZE BASE-,SURFACE- AND INTERFLOW

  240 ADIMC=ADIMC+PLIQ-PAV
      FLOBF=0.
      FLOSF=0.
      FLOIN=0.

!     DETERMINE NUMBER OF INCREMENTS

!     IF RAINFALL EXCEEDS THRESHOLDS(PT1,2) THEN RAINFALL IS LUMPED
!     IN LESS THAN THE TOTAL PERIOD TO SIMULATE INTENSITY VARIATION
!     EFFECT

!     lumping depends on thresholds
      IF(PAV-PT1) 246,246,242
  242 IF(PAV-PT2) 243,244,244
  246 ITIME=2
      ADJ=1.
      GOTO 390
  243 if (PT2.eq.0) then
        errStatus = 3
        return
      endif
      ADJ=.5*SQRT(PAV/PT2)

      GOTO 245
  244 if (PAV.eq.0) then
        errStatus = 4
        return
      endif
      ADJ=1. - .5*PT2/PAV

  245 ITIME=1
!     end lumping

  390 NINC=1.+PM*(UZFWC*FRACT*ADJ+PAV)
      DINC=NINC
      DINC=1./DINC
      PINC=PAV*DINC
      DINC=DINC*FRACT*ADJ

!     ADJUSTMENT OF FREE-WATER STORAGE DEPLETION COEFFICIENTS FROM 1/DAY
!     TO 1/DELTA T

      DUZ =1.-(1.-DCUZ )**DINC
      DLZP=1.-(1.-DCLZP)**DINC
      DLZS=1.-(1.-DCLZS)**DINC

!     BEGIN DRAINAGE AND PERCOLATION LOOP
!     call logwrite('BEGIN DRAINAGE AND PERCOLATION LOOP')

      DO 385 INC=1,NINC
      PAV=PINC

!     RUNOFF FROM ADIMP AREA (FIRST STEP)

      if (LZTWM.eq.0) then
        errStatus = 5
        return
      endif
      RATIO=(ADIMC-UZTWC)/LZTWM
      ADDRO=PINC*RATIO*RATIO

!     COMPUTE BASEFLOW FROM LOWER ZONE

      BF=LZFPC*DLZP
      FLOBF=FLOBF+BF
      LZFPC=LZFPC-BF
      BF=LZFSC*DLZS
      LZFSC=LZFSC-BF
      FLOBF=FLOBF+BF

!     COMPUTE PERCOLATION
!     NO PERCOLATION COMPUTATIONS IF NO WATER AVAILABLE TO PERCOLATE

      IF(PINC+UZFWC-.01 )380,380,250

!     COMPUTE PERCOLATION FROM UPPER ZONE TO LOWER

 250  PERC=PERCM*DINC
      if (UZFWM.eq.0) then
        errStatus = 2
        return
      endif
      PERC=PERC*UZFWC/UZFWM

!     MODIFICATION FOR FASTER PERCOLATION INTO MOISTURE-DEFICIENT SOIL
!       IN LOWER LAYER

!     Check value of exponent
      If ((1.-(LZFPC+LZFSC+LZTWC)/(LZFPM+LZFSM+LZTWM)).GT.0) then
         PERC=PERC*(1.+(ZPERC*(1.-(LZFPC+LZFSC+LZTWC)/(LZFPM+LZFSM+LZTWM))**REXP))
      Else
        errStatus = 6
!       'Warning: Percolation set to ',PERC
      END IF

      IF(PERC-UZFWC)270,270,260
 260  PERC=UZFWC
      UZFWC=0
      GOTO 300
 270  UZFWC=UZFWC-PERC
      CHECK=LZFPC+LZFSC+LZTWC+PERC-LZFPM-LZFSM-LZTWM
      IF(CHECK)290,290,280
 280  PERC=PERC-CHECK
      UZFWC=UZFWC+CHECK

!     COMPUTE INTERFLOW
!     call logwrite('COMPUTE INTERFLOW')

 290  DEL=DUZ*UZFWC
      FLOIN=FLOIN+DEL
      UZFWC=UZFWC-DEL

!     DISTRIBUTE PERCOLATED WATER

  300 CONTINUE
      SPERC=PERC
      PercAct = PercAct + SPerc
!     write(*,*) ' lbl300 PercAct ', PercAct
      PERC=PERC*(1.-PFREE)
      IF(PERC-LZTWM+LZTWC)310,310,320
 310  LZTWC=LZTWC+PERC
      PERC=0.
      GOTO 321
 320  PERC=PERC-LZTWM+LZTWC
      LZTWC=LZTWM

!     DISTRIBUTE PERIOD PERCOLATION IN EXCESS OF LOWER ZONE TENSION
!     REQUIREMENT

 321  PERC=PERC+SPERC*PFREE
      IF(PERC)340,340,323
  323 REST=PERC+LZFSC+LZFPC-LZFSM-LZFPM
      IF(REST)505,506,506
  506 LZTWC=LZTWC+REST
      LZFSC=LZFSM
      LZFPC=LZFPM
      GOTO 340
  505 HPL=LZFPM/(LZFPM+LZFSM)
      RATLP=1.-LZFPC/LZFPM
      RATLS=1.-LZFSC/LZFSM
      PERCS=PERC*(1.-HPL*2.*RATLP/(RATLP+RATLS))
      LZFSC=LZFSC+PERCS
      IF(LZFSC-LZFSM)330,330,322
 322  PERCS=PERCS-LZFSC+LZFSM
      LZFSC=LZFSM
 330  CONTINUE
      LZFPC=LZFPC+(PERC-PERCS)
 340  IF(PAV)380,380,350
 350  CONTINUE

!     DISTRIBUTE PERIOD RAIN IN EXCESS OF UPPER ZONE TENSION REQUIREMENT
!       TO UPPER ZONE FREE WATER AND SURFACE RUNOFF.

!     CHECK WHETHER RESIDUAL RAIN EXCEEDS AVAILABLE UPPER LEVEL FREE
!       WATER CAPACITY.

      IF(PAV-UZFWM+UZFWC)360,360,370
 360  UZFWC=UZFWC+PAV
      GO TO 380
 370  PAV=PAV-UZFWM+UZFWC
      UZFWC=UZFWM

!     COMPUTE SURFACE RUNOFF AND RUNOFF FROM ADIMP AREA (FINAL STEP)
!     call logwrite('COMPUTE SURFACE RUNOFF AND RUNOFF')

      FLOSF=FLOSF+PAV
      ADDRO=ADDRO+PAV*(1.-ADDRO/PINC)
 380  CONTINUE
      ADIMC=ADIMC+PINC-ADDRO
      ROIMP=ROIMP+ADDRO*ADIMP
 385  CONTINUE
      IF(ITIME-2) 396,395,395
  396 ITIME=ITIME+1
      ADJ=1.-ADJ
      PAV=0.
      GOTO 390
  395 CONTINUE

!     END OF DRAINAGE AND PERCOLATION LOOP

!     ROUTE SURFACE,IMPERVIOUS(=IMP+ADIMP) AND INTERFLOW
!     call logwrite('ROUTE SURFACE,IMPERVIOUS AND INTERFLOW')

      FLOSF=FLOSF*(1.-PCTIM-ADIMP)
      FLOIN=FLOIN*(1.-PCTIM-ADIMP)
      QQ(1)=FLOSF+ROIMP+FLOIN
!GP   separate direct, surface and interflow
      QD(1)=ROIMP
      QS(1)=FLOSF
      QI(1)=FLOIN
!     write(*,*) ' QI (1) FLOIN', QI(1), FLOIN
!GP   rout total direct+surface+interflow via Clark UH
      FLOSF=0.
      DO 66 J5=1,NUNIT
   66 FLOSF=FLOSF+QQ(J5)*DRO(J5)
!     write(6,'(11f7.3)') (qq(jj),jj=1,10),flosf
      DO 67 J4=2,NUNIT
      J6=NUNIT+2-J4
      J5=J6-1
   67 QQ(J6)=QQ(J5)
!GP same for direct, surface and interflow
     ! direct
      FloDirect = 0.
      DO 661 J5=1,NUNIT
  661 FloDirect=FloDirect+QD(J5)*DRO(J5)
      DO 671 J4=2,NUNIT
      J6=NUNIT+2-J4
      J5=J6-1
  671 QD(J6)=QD(J5)
     ! surface
      FloSurf = 0.
      DO 662 J5=1,NUNIT
  662 FloSurf = FloSurf+QS(J5)*DRO(J5)
      DO 672 J4=2,NUNIT
      J6=NUNIT+2-J4
      J5=J6-1
  672 QS(J6)=QS(J5)
     ! interflow
      FloInter = 0.
      DO 663 J5=1,NUNIT
!       write(*,*) ' FloInter, QI(j5), Dro(j5)'
!       write(*,*) FloInter, QI(j5), Dro(j5)
663   FloInter = FloInter+QI(J5)*DRO(J5)
!     write(*,*) FloInter
      DO 673 J4=2,NUNIT
      J6=NUNIT+2-J4
      J5=J6-1
  673 QI(J6)=QI(J5)
!     write(*,'(A,11f7.3)') 'Qi',(qi(jj),jj=1,10)
!     write(*,*) 'Floin FloInter', Floin, FloInter
! end GP

      FLOBF=FLOBF*(1.-PCTIM-ADIMP)
      FLOBS=FLOBF*(1-SID)
      FLOBF=FLOBF*SID

!     COMPUTE CHANNEL INFLOW
!     call logwrite('COMPUTE CHANNEL INFLOW')

      E4=EDMND*SARVA
      QF=FLOBF+FLOSF-SSOUT
! add GP
      SSOUTACT = SSOUT
! end add GP
      IF(QF) 71,72,72
   71 CONTINUE
! add GP; geval QF < 0
      SSOUTACT = SSOUT + QF
! end add GP
      QF=0.
   72 QF=QF-E4
      IF(QF) 75,76,76
   75 E4=E4+QF
      QF=0.
   76 CONTINUE

!     SUMMING UP
!     call logwrite('SUMMING UP')
      EUSED=(E1+E2+E3)*(1.-PCTIM-ADIMP)+E4+E5
      STOR=STOR+PLIQ-QF-EUSED-FLOBS
!     write(*,*) ' on exit PercAct ', PercAct
      END function
