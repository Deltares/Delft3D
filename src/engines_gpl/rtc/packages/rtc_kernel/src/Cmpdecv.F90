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

      Function CMPDECV  (IDEBUG, IOUT1, IDUM, IflRtnRtc) result(RetVal)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  RTC  version 1.0.                   Date: June 1997
! *********************************************************************
! *** Last update: June   1997       By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Compute values of decision variables
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  IOUT1  = file unit number of output file with messages
! ***  IDUM   = tijdstap nummer
! ***           let op dat    idum-timeshift>=1.
! *********************************************************************

! *** to be added: interpolation table operation !

      Use ParameterModule
      Use LocationDataModule
      Use DecisionModule
      Use ReservoirModule
      Use NewTables_rtc
      Use MeasureModule
      Use OtherData
      Use Readlib_rtc
      Use DH_Alloc
      Use ExternalDLLModule

      IMPLICIT NONE

      Integer :: RetVal

      Integer DoOperation, DoOperationInitial

      INTEGER i, IT, IPARA, ISBK, I3B, irr, ID3D, IPRE, IEXT, ISobWQ, JPara, imeas, jmeas, iwq, iparwq
      INTEGER ISBKStart, I3BStart, IPREStart, IEXTStart, ISobWQStart, IParStart, ID3DStart
      INTEGER IPAR, IPAR2, ITeller
      INTEGER IDEBUG, IOUT1, IflRtnRtc, IDUM, WeekDy, NTotal
!      INTEGER INMAT, IOUTMAT, ISIGIN, ISIGOUT
      Double Precision RHLP, CMPRHLP, CFMULT, CFADD
      Integer   DataToMatlab, DataFromMatlab

! reservoir index, initial level, expected inflow
! resulting flows (Results: bottom gate, turbine, spillway)
! 3 rule curve levels (Result4-6: flood control, target, firm)
      INTEGER Irsv, ilink
      Double Precision MaxAllowedLvl,InitLevel, ExpInflow, Result(MaxTypeGates*MaxSameGates), HFlood,HTarget, HFirm,&
                       DesiredQCons(MaxTypeGates*MaxSameGates), MaxFlowOutletLinks(MaxTypeGates*MaxSameGates)

      CHARACTER*1             Comma, quote, dblQuote
!      CHARACTER*CharIdLength STRING
!
      Integer Rownr, TableNr
      logical DateTimeOutsideTable, success

      type (Date) currentDate
      type (Time) currentTime
!
      integer ::  maxsobek, maxrr, maxrain, maxd3d, maxlocwq, maxparwq, maxsetpoints

      integer           :: error, returncode
      integer, external :: rtc_perf_function

      integer, save :: SobekFirst = 1


      RetVal = 0
      Comma = ','
      Quote = ''''
      DblQuote = '"'
!
      IF (IDEBUG .GT. 0) WRITE (IDEBUG,1)
    1 FORMAT (' CMPDECV')

     currentDate%year   = iYear
     currentDate%month  = iMo
     currentDate%day    = iDay
     currentTime%hour   = iHour
     currentTime%minute = iMin
     currentTime%second = iSec

!      ISIGIN  = 201
!      ISIGOUT = 202
!      INMAT   = 203
!      IOUTMAT = 204

! *********************************************************************
! *** Store/shift decision parameter values of previous timesteps
! *********************************************************************
!
      ITeller = Min (NTims, 10)
      DO IPARA=1,NPARA
         DO IT=2-MxTmShift(6),2,-1
            DCVVAL (IPARA,IT) = DCVVAL(IPARA,IT-1)
         ENDDO
         DCVVAL (IPARA,1) = 0.
         IF (IDEBUG .GT. 0) WRITE(IDEBUG,*)  'Idecv',IPARA,(DCVVAL(IPARA,IT),IT=1,ITeller)
      ENDDO
!
! *********************************************************************
! *** Give input to Matlab
! *** For time being: output data to Matlab RTC_MAT.DAT
! ***                 request input from Matlab in file MAT_RTC.DAT
! ***                  input from Matlab                 MAT_RTC.DAT
! ***                  signal files Signal.RTC and Signal.MAT
! *********************************************************************
!
#if (defined(USE_MATLAB))
      If (MATUSE) Then
         if (MatlabCommunicationOldStyle) then
            ! NO LONGER SUPPORTED
            RetVal = -999
         else
            RetVal = DataToMatlab (IDEBUG, IDUM, IOUT1, IflRtnRtc)
         endif
         if (RetVal .ne. 0) return
      ENDIF
#endif
!
! *********************************************************************
! *** Bepaal waarden beslisparameters
! *********************************************************************
! *** NB time shift in array TISSBK is 0 of negatief;
! ***    tijdindex NTIMS   = huidige tijdstap
! ***    tijdindex NTIMS-1 = een tijdstap terug
! ***    tijdindex NTIMS-2 = twee tijdstappen terug etc.
! ***      in eerste tijdstap nog geen vorige tijdstap waarde bekend;
! ***      neem dan waarde van huidige tijdstap!
! *********************************************************************
!
! *********************************************************************
! ** Predefined variables
! *********************************************************************
     DcvVal(1 ,1) =  IYear
     DcvVal(2 ,1) =  IMo
     DcvVal(3 ,1) =  IDay
     DcvVal(4 ,1) =  IHour
     DcvVal(5 ,1) =  IMin
     DcvVal(6 ,1) =  ISec
     DcvVal(7 ,1) =  sngl ( dble(IYear * 10000.) + dble(IMo * 100.) + dble(IDay) )
     DcvVal(8 ,1) =  10000 * Ihour + 100 * IMin + Isec
     DcvVal(9 ,1) =  sngl ( dble(IYear * 10000.) + dble(IMo * 100.) + dble(IDay) + &
                             dble(0.01 * float(Ihour) + 0.0001 * float(IMin) + 0.000001 * Float(ISec)) )
     IF (IDEBUG .GT. 0) THEN
        WRITE(IDEBUG,*) ' IYEAR IMO IDAY IHOUR IMIN ISEC'
        WRITE(IDEBUG,*) IYEAR, IMO, IDAY, IHOUR, IMIN, ISEC
        WRITE(IDEBUG,*) ' DCVVAL(9,1) ', DCVVAL(9,1)
     ENDIF
     DcvVal(10,1) =  Weekdy (IYear, IMo, IDay)
     DcvVal(11,1) =  RtmSiz


! other decision variables; initialize to zero, then act depending on decision parameter type

     DO IPARA=NParI+1,NPARA
        DCVVAL(IPARA,1) = 0
     Enddo

!    Use Simulation sequence
!    old, assuming input order:    Do IPARA=NParI+1,NPARA
     DO JPARA=NParI+1,NPARA
       IPara=Simseq(Jpara)

! *********************************************************************
! ** PARA type decision parameters: linear combination of observed values
! *********************************************************************
       If (Idebug .gt. 0) write(Idebug,*) ' Handle nr ',JPara, ' Parameter nr from input list ',IPara
       If (ParTyp(IPARA) .eq. 'PARA') then
! ** Sobek-Flow and 1D2D variabelen
         DO ISBK=1,PARDIM(IPARA,1)
            RHLP = CMPRHLP ('Sobek ', IXSBHIS, VARSBK, TISSBK, CFSBK, &
                             ALRSBK,  IDEBUG,   IDUM, NSBK,  NDECV,   &
                             NSPAR, NPARS, NTIMS, IPARA, ISBK)
            DCVVAL(IPARA,1) = DCVVAL(IPARA,1) + RHLP
            IF (IDEBUG .GT. 0) WRITE(IDEBUG,*)  ' IDECV',IPARA, DCVVAL(IPARA,1)
         ENDDO
! ** 3B variabelen
         DO I3B=1,PARDIM(IPARA,2)
            RHLP = CMPRHLP ('3B    ',IX3BHIS, VAR3B , TIS3B , CF3B, &
                             ALRS3B,  IDEBUG,   IDUM, ND3B,  NDECV, &
                             N3PAR, NPAR3, NTIMS, IPARA, I3B)
            DCVVAL(IPARA,1) = DCVVAL(IPARA,1) + RHLP
            IF (IDEBUG .GT. 0) WRITE(IDEBUG,*)  ' Idecv',IPARA, DCVVAL(IPARA,1)
         ENDDO
! ** precipitation variabelen
         DO IPRE=1,PARDIM(IPARA,3)
            RHLP = CMPRHLP ('Rain  ',IXPRHIS, VARPRE, TISPRE, CFPRE,  &
                             ALRSPR,  IDEBUG,   IDUM, NPRE,  NDECV,   &
                             NPPAR, NPARP, NTIMS, IPARA, IPRE)
            DCVVAL(IPARA,1) = DCVVAL(IPARA,1) + RHLP
            IF (IDEBUG .GT. 0) WRITE(IDEBUG,*)  ' Idecv',IPARA, DCVVAL(IPARA,1)
         ENDDO
! ** externe variabelen
         DO IEXT=1,PARDIM(IPARA,4)
            RHLP = CMPRHLP ('Extern',IXEXHIS, VAREXT, TISEXT, CFEXT, &
                             ALRSEX,  IDEBUG,   IDUM, NEXT+NEXTH,  NDECV,  &
                             NEPAR, NPARE, NTIMS, IPARA, IEXT)
            DCVVAL(IPARA,1) = DCVVAL(IPARA,1) + RHLP
            IF (IDEBUG .GT. 0) WRITE(IDEBUG,*)  ' Idecv',IPARA, DCVVAL(IPARA,1)
         ENDDO
! ** WQ variabelen
         DO ISobWQ =1,PARDIM(IPARA,5)
            RHLP = CMPRHLP ('WQ    ', IXWQHIS, VARWQ , TISWQ , CFWQ , &
                             ALRSWQ,  IDEBUG,   IDUM, NSWQ,  NDECV,   &
                             NQPAR, NPARQ, NTIMS, IPARA, ISobWQ)
            DCVVAL(IPARA,1) = DCVVAL(IPARA,1) + RHLP
            IF (IDEBUG .GT. 0) WRITE(IDEBUG,*)  ' IDECV',IPARA, DCVVAL(IPARA,1)
         ENDDO
! ** D3DFlow variabelen
         DO ID3D=1,PARDIM(IPARA,9)
            RHLP = CMPRHLP ('3D    ',IX3DHIS, VAR3D , TIS3D , CF3D, &
                             ALRS3D,  IDEBUG,   IDUM, ND3D,  NDECV, &
                             N3DPAR, NPAR3D, NTIMS, IPARA, ID3D)
            DCVVAL(IPARA,1) = DCVVAL(IPARA,1) + RHLP
            IF (IDEBUG .GT. 0) WRITE(IDEBUG,*)  ' Idecv',IPARA, DCVVAL(IPARA,1)
         ENDDO

! ** beslisparameters van vorige tijdstappen
         DO IPAR=1,PARDIM(IPARA,6)
            IPAR2 = IXDPAR(IPARA,IPAR)
            IT    = MIN (NTIMS, 1-TISPAR(IPARA,IPAR))
! Als nog geen vorige tijdstap geweest, neem dan huidige waarde
! Dit om consistent te zijn met de manier waarop Sobek, 3B resultaten
! afgehandeld worden.
! Enige opmerking: op deze plek zou verwezen kunnen worden naar nog niet berekende beslisparameters.
! In dat geval zijn die dus nul, omdat dat de initialisatie is.

            IF (IT .GT. IDUM+UseInitValues) IT=IDUM+UseInitValues
            CFMULT = CFPAR(IPARA,IPAR,1)
            CFADD  = CFPAR(IPARA,IPAR,2)
            RHLP   = CFMULT * DCVVAL(IPAR2,IT) + CFADD
            DCVVAL(IPARA,1) = DCVVAL(IPARA,1) + RHLP
            IF (IDEBUG .GT. 0) THEN
               WRITE(IDEBUG,*) ' Add decispar   ',PARPAR(IPARA,IPAR)(1:len_trim(ParPar(Ipara,IPar)))
               WRITE(IDEBUG,*) '  index         ',IPAR2
               WRITE(IDEBUG,*) '  tijdindex     ',TISPAR(IPARA,IPAR),IT
               WRITE(IDEBUG,*) '  coefficienten ',CFMULT, CFADD
               WRITE(IDEBUG,*) '  From DcVval   ',DCVVAL(IPAR2,IT)
               WRITE(IDEBUG,*) '  RHLP       =  ',RHLP
               WRITE(IDEBUG,*) ' Idecv',IPARA, DCVVAL(IPARA,1)
            ENDIF
         ENDDO
! *********************************************************************
! ** reservoir beslisparameters
! *********************************************************************

       ElseIf (ParTyp(IPARA) .eq. 'RSVP') then
           IRSV = RSVIndx(IPara)
           IPAR = IXDPAR(IPARA,1)
           InitLevel = DCVVal(IPAR,1)     ! level at beginning current timestep
           IPAR = IXDPAR(IPARA,2)
           ExpInflow = DCVVal(IPAR,1)     ! current expected inflow
           IPAR = IXDPAR(IPARA,3)
           MaxAllowedLvl = DCVVal(IPAR,1) ! current maximum level
           DesiredQcons = 0
           do ilink=1,NrOutletLinks(irsv)
              IPAR = IXDPAR(IPARA,3+ilink)
              DesiredQcons (ilink) = DCVVal(IPAR,1)     ! current demand
           enddo
           do ilink=1,NrOutletLinks(irsv)
              IPAR = IXDPAR(IPARA,3+NrOutletLinks(irsv)+ilink)
              MaxFlowOutletLinks (ilink) = DCVVal(IPAR,1)     ! current max.flow
           enddo
           ! compute release through 1. bottom gate, 2.turbine, 3. spillway
           If (Idebug .gt. 0) write(idebug,*) ' call RibasimReservoir for irsv, itmstp=', irsv, idum
           RetVal = RibasimReservoir (Irsv, NPar3RsvP, MaxAllowedLvl, InitLevel, ExpInflow, &
                                  DesiredQcons, MaxFlowOutletLinks, &
                                  RtcTableHandle, Idebug, IOut1, ItmSiz, &
                                  IYear, IMo  ,  IDay , IHour, IMin , ISec , &
                                  Result, HFlood, HTarget, HFirm)
           Do i=1,NrRsvOutputPar(ipara)
              DCVVAL(IPARA+i-1,1)   = Result(i)
           Enddo
           DCVVAL(IPARA+NrRsvOutputPar(ipara)  ,1) = HFlood
           DCVVAL(IPARA+NrRsvOutputPar(ipara)+1,1) = HTarget
           DCVVAL(IPARA+NrRsvOutputPar(ipara)+2,1) = HFirm
           If (Idebug .gt. 0) write(idebug,*) ' exit RibasimReservoir with error=',RetVal
           If (RetVal .ne. 0) then
              call write_error_message_rtc (RetVal, 0,' Reservoir simulation ',' ',IOUT1)
              Return
           Endif

! *********************************************************************
! ** PAR2 type decision parameters (using functions)
! ** Beslisparameters als functie van andere beslisparameters
! *********************************************************************

       ElseIf (ParTyp(IPARA) .eq. 'PAR2') then
         NTotal = 0
         Do i=1,6
            NTotal = NTotal + Pardim(IPara,i)
         Enddo
         NTotal = NTotal + Pardim(IPara,8) + Pardim(IPara,9)

!        Initialise counters
         ISbkStart = 0
         I3BStart  = 0
         IPreStart = 0
         IExtStart = 0
         ISobWQStart = 0
         IPARStart = 0
         ID3DStart = 0

!        Set first term
         DcvVal(IPara,1) = 0.0

         i = 0
         Do I=1, NTotal
            rhlp = 0
            If (ParOrder(ipara,i,1) .eq. 1) then
                ISbkStart = ParOrder(ipara,i,2)
                RHLP = CMPRHLP ('Sobek ', IXSBHIS, VARSBK, TISSBK, CFSBK, &
                                 ALRSBK,  IDEBUG,   IDUM, NSBK,  NDECV,   &
                                 NSPAR, NPARS, NTIMS, IPARA, ISBKStart)
            ElseIf (ParOrder(ipara,i,1) .eq. 2) then
                I3BStart = ParOrder(ipara,i,2)
                RHLP = CMPRHLP ('3B    ',IX3BHIS, VAR3B , TIS3B , CF3B, &
                                ALRS3B,  IDEBUG,   IDUM, ND3B,  NDECV, &
                                N3PAR, NPAR3, NTIMS, IPARA, I3BStart)
            ElseIf (ParOrder(ipara,i,1) .eq. 3) then
                IPreStart = ParOrder(ipara,i,2)
                RHLP = CMPRHLP ('Rain  ',IXPRHIS, VARPRE, TISPRE, CFPRE,  &
                                ALRSPR,  IDEBUG,   IDUM, NPRE,  NDECV,   &
                                NPPAR, NPARP, NTIMS, IPARA, IPREStart)
            ElseIf (ParOrder(ipara,i,1) .eq. 4) then
                IExtStart = ParOrder(ipara,i,2)
                RHLP = CMPRHLP ('Extern',IXEXHIS, VAREXT, TISEXT, CFEXT, &
                                ALRSEX,  IDEBUG,   IDUM, NEXT+NEXTH,  NDECV,  &
                                NEPAR, NPARE, NTIMS, IPARA, IEXTStart)
            ElseIf (ParOrder(ipara,i,1) .eq. 5) then
                ISobWQStart = ParOrder(ipara,i,2)
                RHLP = CMPRHLP ('WQ    ', IXWQHIS, VARWQ , TISWQ , CFWQ , &
                                ALRSWQ,  IDEBUG,   IDUM, NSWQ,  NDECV,   &
                                NQPAR, NPARQ, NTIMS, IPARA, ISobWQStart)
            ElseIf (ParOrder(ipara,i,1) .eq. 6) then
                IParStart = ParOrder(ipara,i,2)
                IPAR  = IParStart
                IPAR2 = IXDPAR(IPARA,IPAR)
                IT    = MIN (NTIMS, 1-TISPAR(IPARA,IPAR))
                IF (IT .GT. IDUM+UseInitValues) IT=IDUM+UseInitValues
                CFMULT = CFPAR(IPARA,IPAR,1)
                CFADD  = CFPAR(IPARA,IPAR,2)
                RHLP   = CFMULT * DCVVAL(IPAR2,IT) + CFADD
                IF (IDEBUG .GT. 0) THEN
                  WRITE(IDEBUG,*) ' Add decispar   ',PARPAR(IPARA,IPAR)(1:len_trim(ParPar(Ipara,IPar)))
                  WRITE(IDEBUG,*) '  index         ',IPAR2
                  WRITE(IDEBUG,*) '  tijdindex     ',TISPAR(IPARA,IPAR),IT
                  WRITE(IDEBUG,*) '  coefficienten ',CFMULT, CFADD
                  WRITE(IDEBUG,*) '  From DcVval   ',DCVVAL(IPAR2,IT)
                  WRITE(IDEBUG,*) '  RHLP       =  ',RHLP
                  WRITE(IDEBUG,*) ' Idecv',IPARA, DCVVAL(IPARA,1)
                ENDIF
            ElseIf (ParOrder(ipara,i,1) .eq. 9) then
                ID3DStart = ParOrder(ipara,i,2)
                RHLP = CMPRHLP ('3D    ',IX3DHIS, VAR3D , TIS3D , CF3D, &
                                ALRS3D,  IDEBUG,   IDUM, ND3D,  NDECV, &
                                N3DPAR, NPAR3D, NTIMS, IPARA, ID3DStart)
            Endif
            continue
!           Set first term
            if (i .eq. 1) then
               RetVal = DoOperationInitial (IPara, RHlp, Iout1)
            else
               RetVal = DoOperation (IPara, RHlp, Iout1)
            endif
            If (RetVal .ne. 0) Return
            IF (IDEBUG .GT. 0) WRITE(IDEBUG,*)  ' IDECV',IPARA, DCVVAL(IPARA,1)
         Enddo

! voer operatie uit: middeling
         IF (ParOper(Ipara) .eq. 7)  DCVVAL(IPARA,1) = DCVVAL(IPARA,1) / NTotal

! *********************************************************************
! ** PAR3 type decision parameters (using time tables)
! *********************************************************************

       ElseIf (ParTyp(IPARA) .eq. 'PAR3') then
! find current value from time table
         RowNr = -1
         TableNr = Par3TableIndex(Ipara)
         Rhlp = GetNewValue(RTCTableHandle, TableNr, 1, RowNr, CurrentDate, CurrentTime, &
                            Idebug, iout1, DateTimeOutsideTable, .true.)
         DCVVAL(IPARA,1) = RHLP
        Endif

      ENDDO

! *********************************************************************
! ** Matlab variabelen: alleen als NTOTPAR=0; beslisparameter is nl. impliciet gedefinieerd
! ** Request output from Matlab
! ** Give signal and wait
! ** Get data from Matlab (measure-id=structure-id)
! *********************************************************************

!      Write(*,*) ' In CmpDecV voor DataFromMatlab'
#if (defined(USE_MATLAB))
      If (MatUse) Then
         RetVal = DataFromMatlab (IDEBUG, IOUT1, IDUM, IflRtnRtc)
      Endif
#endif
!      Write(*,*) ' In CmpDecV na DataFromMatlab'

      If (dll_handle .ne. 0) then
         !
         ! prepare call to DLL
         !
         call set_pointers( alrsbk(:,:,ntims), alrs3b(:,:,ntims), alrswq(:,:,:), &
                            alrspr(:,:,ntims), alrs3d(:,:,ntims), dcvval(:,1), paraid )
         ! call to dll_function

         if (dll_test .eq. -1) then
            ! temporary: use eval_measures from Arjen
            if (idebug .gt. 0) write(idebug,*) ' Call eval_measures'
            call eval_measures( sobekfirst, int(rtc_timold), &
                                1000000 * ihour + 10000 * imin + 100 * isec, &
                                itmsiz, id_sbr, id_slc, id_d3b, id_swq, id_pre, measidmatlab )
         else
           ! call to the dll_function
            if (idebug .gt. 0) then
               write(idebug,*) ' Call dll routine', dll_function
               write(idebug,*) ' first allocate arrays for dll communication'
            endif
!           write(*,*) ' first allocate arrays for dll communication'
            success = .true.
            success = success .and. DH_AllocInit(NSBK, dllSobekh, -999.999D0)
            success = success .and. DH_AllocInit(NSBK, dllSobekq, -999.999D0)
            success = success .and. DH_AllocInit(NSBK, dllSobeksa, -999.999D0)
            success = success .and. DH_AllocInit(NSBK, dllSobekwd, -999.999D0)
            success = success .and. DH_AllocInit(NSBK, dllSobekcl, -999.999D0)
            success = success .and. DH_AllocInit(NSBK, dllSobekcw, -999.999D0)
            success = success .and. DH_AllocInit(NSBK, dllSobekgl, -999.999D0)
            success = success .and. DH_AllocInit(NSBK, dllSobekgo, -999.999D0)
            success = success .and. DH_AllocInit(NSBK, dllSobekfa, -999.999D0)
            success = success .and. DH_AllocInit(NSBK, dllSobekqs, -999.999D0)
            success = success .and. DH_AllocInit(NSBK, dllSobekvs, -999.999D0)
            success = success .and. DH_AllocInit(NSBK, dllSobekhu, -999.999D0)
            success = success .and. DH_AllocInit(NSBK, dllSobekhd, -999.999D0)
            success = success .and. DH_AllocInit(NSBK, dllSobekdh, -999.999D0)
            success = success .and. DH_AllocInit(NSBK, dllSobekpd, -999.999D0)
            success = success .and. DH_AllocInit(NSBK, dllSobekpc, -999.999D0)
            success = success .and. DH_AllocInit(NSBK, dllSobek1d2dbl, -999.999D0)
            success = success .and. DH_AllocInit(NSBK, dllSobek1d2du , -999.999D0)
            success = success .and. DH_AllocInit(NSBK, dllSobek1d2dv , -999.999D0)
            success = success .and. DH_AllocInit(NSBK, dllSobek1d2dc , -999.999D0)
            success = success .and. DH_AllocInit(NSBK, dllSobekvr, -999.999D0)
            success = success .and. DH_AllocInit(ND3B, dllRRH , -999.999D0)
            success = success .and. DH_AllocInit(NPRE, dllRAINH , -999.999D0)
            success = success .and. DH_AllocInit(ND3D, dlld3dh , -999.999D0)
            success = success .and. DH_AllocInit(ND3D, dlld3dsal , -999.999D0)
            success = success .and. DH_AllocInit(Nswq, nparq, dllsobekwq , -999.999D0)
            success = success .and. DH_AllocInit(NRSbkDllMeasures, DllSobekC , -999.999D0)
            success = success .and. DH_AllocInit(NRSbkDllMeasures, DllSobekS , -999.999D0)
            success = success .and. DH_AllocInit(NSBK, DllIDSBR , ' ')
            success = success .and. DH_AllocInit(ND3B, DllIDD3B , ' ')
            success = success .and. DH_AllocInit(NPRE, DllIDPRE , ' ')
            success = success .and. DH_AllocInit(ND3D, DllIDD3D , ' ')
            success = success .and. DH_AllocInit(Nswq, DllLocIDWQ , ' ')
            success = success .and. DH_AllocInit(NParq, DllParIDWQ , ' ')
            success = success .and. DH_AllocInit(NrSbkDllMeasures, DllMEASID , ' ')
            if (.not. success) then
               call write_error_message_rtc (958, 0, 'RTC', ' Error allocating arrays for dll call', IOUT1)
            endif
            if (idebug .gt. 0) write(idebug,*) ' fill arrays for dll communication with proper data'
!           write(*,*) ' fill arrays for dll communication with proper data'
            Do isbk=1,Nsbk
               dllSobekh (isbk) = Sobekh (isbk)
               dllSobekq (isbk) = Sobekq (isbk)
               dllSobeksa(isbk) = Sobeksa(isbk)
               dllSobekwd(isbk) = Sobekwd(isbk)
               dllSobekcl(isbk) = Sobekcl(isbk)
               dllSobekcw(isbk) = Sobekcw(isbk)
               dllSobekgl(isbk) = Sobekgl(isbk)
               dllSobekgo(isbk) = Sobekgo(isbk)
               dllSobekfa(isbk) = Sobekfa(isbk)
               dllSobekqs(isbk) = Sobekqs(isbk)
               dllSobekvs(isbk) = Sobekvs(isbk)
               dllSobekhu(isbk) = Sobekhu(isbk)
               dllSobekhd(isbk) = Sobekhd(isbk)
               dllSobekdh(isbk) = Sobekdh(isbk)
               dllSobekpd(isbk) = Sobekpd(isbk)
               dllSobekpc(isbk) = Sobekpc(isbk)
               dllSobek1d2dBl(isbk) = Sobek1d2dBl(isbk)
               dllSobek1d2dU (isbk) = Sobek1d2dU (isbk)
               dllSobek1d2dV (isbk) = Sobek1d2dV (isbk)
               dllSobek1d2dC (isbk) = Sobek1d2dC (isbk)
               dllSobekvr(isbk) = Sobekvr(isbk)
               dllIdSbr (isbk) = Id_sbr (isbk)
            enddo
            Do irr=1,Nd3b
               dllRRh (irr) = rrh(irr)
               dllIdD3B(irr) = Id_d3b (irr)
            enddo
            Do ipre=1,NPRE
               dllRainh (ipre) = rainh(ipre)
               dllIdPre (ipre) = Id_Pre (ipre)
            enddo
            Do id3d=1,Nd3d
               dlld3dh (id3d)  = d3dh (id3d)
               dlld3dsal(id3d) = d3dsal (id3d)
               dllidd3d (id3d) = Id_d3d (id3d)
            enddo
            Do iwq =1,Nswq
               Do iparwq =1,Nparq
                  dllsobekwq (iwq,iparwq) = sobekwq(iwq,iparwq)
               enddo
               dlllocidwq(iwq) = id_swq(iwq)
            enddo
            Do iparwq =1,Nparq
               dllparidwq (iparwq) = idparq(iparwq)
            enddo

            jmeas = 0
            Do imeas=1,NsMeas
               if (measidmatlab(imeas) .ne. '' .and. measty(imeas) .eq. 11) then
                  jmeas = jmeas + 1
                  dllmeasid (jmeas) = measidmatlab(jmeas)
               endif
            enddo
            maxsobek = nsbk
            maxrr = nd3b
            maxrain = npre
            maxlocwq = nswq
            maxparwq = nparq
            maxd3d  = nd3d
            maxsetpoints = nrSbkDllmeasures
            if (idebug .gt. 0) write(idebug,*) ' now call function'
!            write(*,*) ' now call function'
            error = rtc_perf_function (dll_handle, dll_function, &
                                  sobekfirst, int(rtc_timold), &
                                  1000000 * ihour + 10000 * imin + 100 * isec, itmsiz, &
                                  maxsobek, &
                                  dllsobekh,  dllsobekq, dllsobeksa, dllsobekwd, &
                                  dllsobekcl, dllsobekcw, dllsobekgl, dllsobekgo, dllsobekfa, dllsobekqs, &
                                  dllsobekvs, dllsobekhu, dllsobekhd, dllsobekdh, dllsobekpd, dllsobekpc, &
                                  dllsobek1d2dbl, dllsobek1d2du, dllsobek1d2dv, dllsobek1d2dc, dllsobekvr, &
                                  maxrr, dllrrh, &
                                  maxrain, dllrainh, &
                                  maxd3d, dlld3dh, dlld3dsal, &
                                  maxlocwq, maxparwq, dllsobekwq, &
                                  maxsetpoints, dllsobekc, dllsobeks, returncode, &
                                  dllidsbr, dllidd3b, dllidpre, dllidd3d, dlllocidwq, dllparidwq, dllmeasid )
!!   pm RR setpoints, wq info id_swq
!                                 NrRRDllmeasures, .., rrslowon, rrslowoff, rrshighon, rrshighoff, &
            if (returncode .ne. -999) then
                write(iout1,*) ' error calling dll ', returncode
                call write_error_message_rtc (958, 0, ' Error calling dll', ' Error calling dll', IOUT1)
            else
                if (idebug .gt. 0) then
                   write(idebug,*) ' Returned from dll Sobeks array'
                   Do i=1,maxsetpoints
                      write(idebug,*)    i, DllSobeks(i)
                   enddo
                endif
                ! extract the setpoints and put them in the dll decision parameter results
                jpara = 1
                i = 0
         101    if (ParaId(jpara)(1:3) .ne. 'Dll') then
                   if (idebug .gt. 0) write(Idebug,*) ' skip jpara', jpara
                   jpara = jpara + 1
                else
                   i = i+1
                   if (idebug .gt. 0) write(Idebug,*) ' adjust DcVVal', jpara, i, DllSobeks(i)
                   DcvVal(jpara,1) = DllSobekS(i)
                   jpara = jpara + 1
                endif
                if (idebug .gt. 0) then
                    write(Idebug,*) ' last i, NrSbkDllMeasures, new Jpara, NPara'
                    write(Idebug,*) i, NrSbkDllMeasures, Jpara, NPara
                endif
                if (i .lt. NrSbkDllMeasures .and. jpara .le. Npara) goto 101
             endif
             Deallocate (Dllsobekh, dllsobekq, dllsobeksa, dllsobekwd)
             Deallocate (Dllsobekcl, dllsobekcw, dllsobekgl, dllsobekgo)
             Deallocate (Dllsobekqs, dllsobekvs, dllsobekhu, dllsobekhd)
             Deallocate (Dllsobekfa, dllsobekdh, dllsobekpd, dllsobekpc)
             Deallocate (Dllsobek1d2dbl, dllsobek1d2du, dllsobek1d2dv, dllsobek1d2dc)
             Deallocate (Dllsobekvr)
             Deallocate (Dllsobekc, dllSobeks)
             Deallocate (Dllrrh, dllrainh)
             Deallocate (Dlld3dh, dlld3dsal)
             Deallocate (Dllsobekwq)
             Deallocate (Dllidsbr, dllidd3b, dllidpre, dllidd3d, dlllocidwq, dllparidwq, dllmeasid)
         endif

         ! put SobekFirst to zero
         SobekFirst = 0
      Endif

! *********************************************************************
! *** Debug
! *********************************************************************

      IF (IDEBUG .GT. 0) THEN
          WRITE(IDEBUG,*)  ' Decision variable id.             value '
          DO IPARA=1,NPARA
             WRITE(IDEBUG,*)  IPARA, PARAID(IPARA)(1:len_trim(ParaId(Ipara))), DCVVAL(IPARA,1)
          ENDDO
      ENDIF

! *********************************************************************
! *** Put to csv file if desired
! *********************************************************************

      IF (UseTCN .and. WriteCsvFile .ne. '') then
          Open (IOutCsv,File=WriteCsvFile(1:len_trim(WriteCsvFile)), Status='Unknown')
          Write(iOutCsv,'(A)') ' "id", "description", "value" '
          DO IPARA=12,NPARA    ! skip first fixed set, only put user specified parameters (start from 12 to skip date, time etc)
             IF (ParaId(ipara)(1:6) .NE. 'Matlab' .and. ParaId(Ipara)(1:3) .NE. 'Dll' .and. ParaId(Ipara)(1:4) .ne. 'TCN_') THEN
                 WRITE(IoutCsv,'(A1,A,A1,A1,1X,A1,A,A1,A1,F10.3)') DblQuote, PARAID(IPARA)(1:len_trim(ParaId(Ipara))), DblQuote, Comma, &
                                                             DblQuote, PARADescr(IPARA)(1:len_trim(ParaDescr(Ipara))), DblQuote, Comma, DCVVAL(IPARA,1)
             ENDif
          ENDDO
          Close (IOutCsv)
      ENDIF

      RETURN
      END Function CmpDecV



      Function DoOperation (IPara, RHlp, Iout1)  result(RetVal)

      Use ParameterModule
      Use LocationDataModule
      Use DecisionModule
      Use ReservoirModule
      Use NewTables_rtc
      Use MeasureModule
      Use OtherData

      IMPLICIT NONE

      Integer :: RetVal


      Integer                     :: IPara, ITable, IDummy, Iout1
      Double Precision            :: RHlp, RVal
      CHARACTER(len=CharIdLength) :: STRING

      RetVal = 0
! voer operatie uit
      IF (PAROPER(IPARA) .EQ. 1) THEN
         DCVVAL(IPARA,1) = DCVVAL(IPARA,1) + RHLP
      ELSEIF (PAROPER(IPARA) .EQ. 2) THEN
         DCVVAL(IPARA,1) = DCVVAL(IPARA,1) - RHLP
      ELSEIF (PAROPER(IPARA) .EQ. 3) THEN
         DCVVAL(IPARA,1) = DCVVAL(IPARA,1) * RHLP
      ELSEIF (PAROPER(IPARA) .EQ. 4) THEN
         IF (ABS(RHLP) .LE. .0000001) THEN
            STRING = PARAID(IPARA)
            call write_error_message_rtc (936, 0, ' Cmpdecv', STRING, IOUT1)
!           code 936 = een Warning
            IF (RHLP .GE. 0) THEN
               RHLP = 0.000001
            ELSE
               RHLP = -0.000001
            ENDIF
         ENDIF
         DCVVAL(IPARA,1) = DCVVAL(IPARA,1) / RHLP
      ELSEIF (PAROPER(IPARA) .EQ. 5) THEN
         DCVVAL(IPARA,1) = MAX ( DCVVAL(IPARA,1), RHLP)
      ELSEIF (PAROPER(IPARA) .EQ. 6) THEN
         DCVVAL(IPARA,1) = MIN ( DCVVAL(IPARA,1), RHLP)
      ELSEIF (PAROPER(IPARA) .EQ. 7) THEN
         DCVVAL(IPARA,1) = DCVVAL(IPARA,1) + RHLP
      ELSEIF (PAROPER(IPARA) .EQ. 8) THEN
         DCVVAL(IPARA,1) = DCVVAL(IPARA,1) ** RHLP
      ELSEIF (PAROPER(IPARA) .EQ. 32) THEN
!        do Interpolation, using DCVVal(Ipara,1) as input
         RVAL = DCVVAL(IPARA,1)
         ITable = IxInterpTable(Ipara,1)
         IDUMMY = 1
         CALL INTERP_double (NrInterpolationPoints(ITable), InterpTableInput(1,ITable),InterpTableOutput(1,ITable), RVAL,RHLP,IDUMMY)
         DCVVAL(IPARA,1) = RHLP
      ELSE
         String = 'Compute Decision Variable ' // (ParaId(Ipara)(1:len_trim(ParaId(ipara))))
         call write_error_message_rtc (994, 0, String, ' Operation only allowed using 1 argument', IOUT1)
         RetVal = 994
      ENDIF

      Return
      End Function DoOperation



      Function DoOperationInitial (IPara, RHlp, Iout1)  result(RetVal)

      Use ParameterModule
      Use LocationDataModule
      Use DecisionModule
      Use ReservoirModule
      Use NewTables_rtc
      Use MeasureModule
      Use OtherData

      IMPLICIT NONE

      Integer :: RetVal


      Integer                     :: IPara, Iout1
      Double Precision            :: RHlp
      CHARACTER(len=CharIdLength) :: STRING

		! The intrinsic functions sinD, cosD, tanD, asinD, acosD and atanD are Intel Extensions
		! They are not supported by other compilers.
		
#ifndef __INTEL_COMPILER
      double precision    :: radians
		double precision    :: pi
		pi = 4.0d0 * atan(1.0d0)
#endif
      RetVal = 0
! voer operatie uit
      IF (PAROPER(IPARA) .LE. 8) THEN
         DCVVAL(IPARA,1) = RHLP
      ELSEIF (PAROPER(IPARA) .EQ. 9) THEN
         DCVVAL(IPARA,1) = Sin (RHlp)       ! sinus (radialen)
      ELSEIF (PAROPER(IPARA) .EQ. 10) THEN
#ifdef __INTEL_COMPILER
         DCVVAL(IPARA,1) = SinD (RHlp)      ! sinus (graden)
#else
         radians = rHlp * pi / 180.0d0
			DCVVAL(IPARA,1) = sin(radians)
#endif
      ELSEIF (PAROPER(IPARA) .EQ. 11) THEN
         DCVVAL(IPARA,1) = Cos (RHlp)       ! cosinus (radialen)
      ELSEIF (PAROPER(IPARA) .EQ. 12) THEN
#ifdef __INTEL_COMPILER
         DCVVAL(IPARA,1) = CosD (RHlp)      ! cosinus (graden)
#else
         radians = rHlp * pi / 180.0d0
			DCVVAL(IPARA,1) = cos(radians)
#endif
      ELSEIF (PAROPER(IPARA) .EQ. 13) THEN
         DCVVAL(IPARA,1) = Tan (RHlp)       ! tangens (radialen)
      ELSEIF (PAROPER(IPARA) .EQ. 14) THEN
#ifdef __INTEL_COMPILER
         DCVVAL(IPARA,1) = TanD (RHlp)      ! tangens (graden)
#else
         radians = rHlp * pi / 180.0d0
			DCVVAL(IPARA,1) = tan(radians)
#endif
      ELSEIF (PAROPER(IPARA) .EQ. 15) THEN
         If (abs(Rhlp) .gt. 1) Then
              String = 'Compute decision parameter ' // (ParaId(Ipara)(1:len_trim(ParaId(ipara))))
              call write_error_message_rtc (994, 0, String, ' Argument outside allowed range', IOUT1)
              RetVal = 994
              Return
         Endif
         DCVVAL(IPARA,1) = ASin (RHlp)     ! arcsinus (result in radialen)
      ELSEIF (PAROPER(IPARA) .EQ. 16) THEN
         If (abs(Rhlp) .gt. 1) Then
              String = 'Compute decision parameter ' // (ParaId(Ipara)(1:len_trim(ParaId(ipara))))
              call write_error_message_rtc (994, 0, String, ' Argument outside allowed range', IOUT1)
              RetVal = 994
              Return
         Endif
#ifdef __INTEL_COMPILER
         DCVVAL(IPARA,1) = ASinD (RHlp)    ! arcsinus (result in graden)
#else
         DCVVAL(IPARA,1) = asin(RHlp) * 180.0d0 / pi    ! arcsinus (result in graden)
#endif
      ELSEIF (PAROPER(IPARA) .EQ. 17) THEN
         If (abs(Rhlp) .gt. 1) Then
              String = 'Compute decision parameter ' // (ParaId(Ipara)(1:len_trim(ParaId(ipara))))
              call write_error_message_rtc (994, 0, String, ' Argument outside allowed range', IOUT1)
              RetVal = 994
              Return
         Endif
         DCVVAL(IPARA,1) = ACos (RHlp)     ! arccosinus (result in radialen)
      ELSEIF (PAROPER(IPARA) .EQ. 18) THEN
         If (abs(Rhlp) .gt. 1) Then
              String = 'Compute decision parameter ' // (ParaId(Ipara)(1:len_trim(ParaId(ipara))))
              call write_error_message_rtc (994, 0, String, ' Argument outside allowed range', IOUT1)
              RetVal = 994
              Return
         Endif
#ifdef __INTEL_COMPILER
         DCVVAL(IPARA,1) = ACosD (RHlp)    ! arccosinus (result in graden)
#else
         DCVVAL(IPARA,1) = acos(RHlp) * 180.0d0 / pi    ! arcsinus (result in graden)
#endif
      ELSEIF (PAROPER(IPARA) .EQ. 19) THEN
         DCVVAL(IPARA,1) = ATan (RHlp)     ! arctangens (result in radialen)
      ELSEIF (PAROPER(IPARA) .EQ. 20) THEN
#ifdef __INTEL_COMPILER
         DCVVAL(IPARA,1) = ATanD (RHlp)    ! arctangens (result in graden)
#else
         DCVVAL(IPARA,1) = atan(RHlp) * 180.0d0 / pi    ! arcsinus (result in graden)
#endif
      ELSEIF (PAROPER(IPARA) .EQ. 21) THEN
         DCVVAL(IPARA,1) = Floor (Rhlp)      ! largest integer <= Rhlp
      ELSEIF (PAROPER(IPARA) .EQ. 22) THEN
         DCVVAL(IPARA,1) = Ceiling (RHlp)       ! smallest integer >= Rhelp
      ELSEIF (PAROPER(IPARA) .EQ. 23) THEN
         DCVVAL(IPARA,1) = Nint (RHlp)       ! nearest integer
      ELSEIF (PAROPER(IPARA) .EQ. 24) THEN
         DCVVAL(IPARA,1) = Rhlp * Rhlp       ! square
      ELSEIF (PAROPER(IPARA) .EQ. 25) THEN
         If (Rhlp .lt. 0.0) Then
              String = 'Compute decision parameter ' // (ParaId(Ipara)(1:len_trim(ParaId(ipara))))
              call write_error_message_rtc (994, 0, String, ' Argument outside allowed range', IOUT1)
              RetVal = 994
              Return
         Endif
         DCVVAL(IPARA,1) = Sqrt (RHlp)       ! square root
      ELSEIF (PAROPER(IPARA) .EQ. 26) THEN
         DCVVAL(IPARA,1) = Exp (RHlp)        ! e tot de macht rhlp
      ELSEIF (PAROPER(IPARA) .EQ. 27) THEN
         If (Rhlp .le. 0.0) Then
              String = 'Compute decision parameter ' // (ParaId(Ipara)(1:len_trim(ParaId(ipara))))
              call write_error_message_rtc (994, 0, String, ' Argument outside allowed range', IOUT1)
              RetVal = 994
              Return
         Endif
         DCVVAL(IPARA,1) = Log (Rhlp)        ! natural log
      ELSEIF (PAROPER(IPARA) .EQ. 28) THEN
         If (Rhlp .le. 0.0) Then
              String = 'Compute decision parameter ' // (ParaId(Ipara)(1:len_trim(ParaId(ipara))))
              call write_error_message_rtc (994, 0, String, ' Argument outside allowed range', IOUT1)
              RetVal = 994
              Return
         Endif
         DCVVAL(IPARA,1) = Log10 (RHlp)      !10log
      ELSEIF (PAROPER(IPARA) .EQ. 29) THEN
         DCVVAL(IPARA,1) = sinH (RHlp)      ! sinus hyperbolicus
      ELSEIF (PAROPER(IPARA) .EQ. 30) THEN
         DCVVAL(IPARA,1) = cosH (RHlp)      ! cosinus hyperbolicus
      ELSEIF (PAROPER(IPARA) .EQ. 31) THEN
         DCVVAL(IPARA,1) = tanH (RHlp)      ! tangens hyperbolicus
      ELSEIF (PAROPER(IPARA) .eq. 32) THEN  ! Interpolation
         DCVVAL(IPARA,1) = Rhlp
      ELSE
         String = 'Compute decision parameter ' // (ParaId(Ipara)(1:len_trim(ParaId(ipara))))
         call write_error_message_rtc (994, 0, String, ' unknown operation', IOUT1)
         RetVal = 994
      ENDIF

      Return
      End Function DoOperationInitial

