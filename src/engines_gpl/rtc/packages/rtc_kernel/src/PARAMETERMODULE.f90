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

   Module Parametermodule


! *** Parameters used for RTC module, now put in a module

! *** -------------------------------
! ***  ND3B = max. aantal D3B nodes (knopen)
! ***  NSBK = max. aantal Sobek-lokaties
! ***  N3D  = max. aantal D3DFlow lokaties
! ***  NPRE = max. aantal precipitation-lokaties
! ***  NEXT = max. aantal externe-lokaties wind
! ***  NEXTH = max. aantal externe-lokaties gekoppeld aan externe HIS files)
! ***  NEXTHDataset = max. aantal externe-HIS datasets
! ***  NTimHis = max. aantal tijdstappen in externe-HIS datasets
! ***  NSWQ = max. aantal WQ-lokaties
! ***  NDECV = max. aantal decision varables (beslisparameters)
! ***  NSMES = max. number of SOBEK-CF/SF and Delft3D-FLOW measures
! ***  N3MES = max. aantal measures 3B       (maatregelen)
! ***  N3LOC = max. aantal lokaties 3B mbt maatregelen
! ***  NSCV  = max. number of check values of SOBEK-CF/SF and Delft3D-FLOW measures
! ***  NSMES_SBK = max. number of measures SOBEK-CF/SF
! ***  NSCV_SBK = max. number of check values of SOBEK-CF/SF measures
! ***  NSMES_D3D = max. number of measures Delft3D-FLOW
! ***  NSCV_D3D = max. number of check values of Delft3D-FLOW measures
! ***
! ***  NSPAR = max. aantal sobek lokaties per beslisparameter
! ***  N3DPAR = max. aantal D3DFlow lokaties per beslisparameter
! ***  N3PAR = max. aantal 3B lokaties per beslisparameter
! ***  NPPAR = max. aantal precipitation lokaties per beslisparameter
! ***  NEPAR = max. aantal externe lokaties per beslisparameter
! ***  NQPAR = max. aantal WQ lokaties per beslisparameter
! ***  NDPAR = max. aantal decision parameters per beslisparameter
! ***  NTPAR = max. aantal totaal afhankelijkheden (NsPAR+N3Par + ... +NdPar), 7 termen
! ***  NRPAR = max. aantal rsv input decision parameters (=3+2*MaxNrOutletLinks(MaxTypeGates*MaxSameGates)),
!                                         i.e. max.lvl, initial level and expected inflow)
!                                              maximum flow per outlet link
!                                              downstream demand per outlet link
! ***  NTIMSH= min. waarde timeshift (0=geen, -1=1 tijdstap terug, etc)
! ***  NTIMS = max. totaal aantal tijdstappen te onthouden (=-NTIMSH+1)
! ***  NTIMH = max. time horizon
! ***
! ***  NPAR3 = max. aantal parameters in 3B-HIS file
! ***  NPARS = max. aantal parameters in Sobek-HIS file
! ***  NPARP = max. aantal parameters in Precipitation-HIS file
! ***  NPARE = max. aantal parameters in External-HIS file
! ***  NPARQ = max. aantal parameters in WQ HIS file
! ***  NPAR3D= max. aantal parameters in D3DHIS file
!
! ***  NTIM  = max. aantal tijdstappen in bui file
! ***  NTIMW = max. aantal tijdstappen in wind file
! ***  NSTAT = max. aantal meteostations
! ***  NEVNT = max. aantal buien
! ***  NLOCHIS = max. aantal lokaties in HIS file voor dummy array ResRead

      Implicit None

      Integer  ND3B, NSBK, N3D, NPRE, NEXT, NExtH, NExtHDataset, NTimHis, NSWQ, NDECV, &
               NSPAR, N3DPar, N3PAR, NPPAR, NEPAR, NQPAR, NDPAR, NTPAR, NRPAR,&
               NSMES, N3MES, N3LOC, N3Mat, NSCV, &
               NPARS, NPAR3, NPAR3D, NPARP, NPARE, NPARQ, &
               NTIMSH, NTIMS, NTIMH, NTIM, NEVNT, NSTAT, &
               NSMES_SBK, NSCV_SBK, NSMES_D3D, NSCV_D3D
      Integer  IdControlModule
      Integer  NLocHis

      Integer  RTCTableHandle, RTCLanguageHandle

! *** Actual values used for koppelingsmodule RTC  (NDAT.CMN)
! *** ----------------------------------
! ***  ND3BID  = aantal 3B-id's (lokaties) in HIS output 3B
! ***  NSOBEK  = aantal Sobek-lokaties  in HIS output Sobek
! ***  ND3D    = aantal D3DFlow-lokaties  in HIS output D3DFlow
! ***  NPRECP  = aantal precipitation-lokaties in HIS output Precipitation
! ***  NEXTD   = aantal externe-lokaties gekoppeld aan Wind
! ***  NEXTHD  = aantal externe-lokaties gekoppeld aan external HIS input file
! ***     (totaal in uitvoer externe lokaties dus NEXTD+NEXTHD)
! ***  NSOBWQ  = aantal WQ-lokaties in HIS output WQ
! ***  NPARI   = aantal interne slisparameters (predefined)
! ***  NPARA   = aantal beslisparameters totaal (NPAR1+NPAR2+NPAR3rsvp)
! ***  NPAR1   = aantal beslisparameters type 1 = afh. datalokaties + beslispar.
! ***  NPAR2   = aantal beslisparameters type 2 = afh. beslisparameters + operatie
! ***  NPAR3Para  = aantal beslisparameters type 3  = Sobek time tables
! ***  NPAR3rsvp  = aantal beslisparameters type 3b = RSVP beslisparameters
! ***  NSMEAS_SBK  = number of SOBEK-CF/SF measures
! ***  NSMEAS_D3D  = number of Delft3D-FLOW measures
! ***  NSMEAS  = aantal maatregelen SOBEK-CF/SF en Delft3D-FLOW
! ***  NSMSID_SBK  = number of different IDs in SOBEK-CF/SF measures
! ***  NSMSID_D3D  = number of different IDs in Delf3D-FLOW measures
! ***  NSMSID  = aantal verschillende Sobek-id's in Sobek-maatregelfile
! ***  N3MEAS  = aantal maatregelen 3B
! ***  N3MLOC  = aantal lokaties voor maatregelen 3B
! ***  N3BMS   = aantal unieke lokaties met maatregelen 3B
! ***  LOWSPRI = laagste prioriteit Sobekmaatregelen
! ***  LOW3PRI = laagste prioriteit 3Bmaatregelen
! ***  NWIND   = aantal wind-lokaties in HIS output wind
! ***  NCSTAT  = aantal stations in bui file


      INTEGER  ND3BID, NSOBEK, ND3D, NPRECP, NEXTD, NextHD, NSobWQ, &
               NParI, NPARA, NPAR1, NPAR2, NPar3Para, NPAR3RSVP, &
               NSMEAS, NSMSID, N3MEAS, N3MLOC, N3BMS, N3MatLoc, &
               NWIND , NCSTAT, LOWSPRI, LOW3PRI, NParaHis, &
               NSMEAS_SBK, NSMEAS_D3D, NSMSID_SBK, NSMSID_D3D

      integer, parameter    :: CharIdLength = 256


   Contains


      Subroutine SetFixedParameters
!     Zet vaste dimensies

      NParI=   11

!     NSPAR=   50 ! was fixed at 50, now by scanning the input file Decispar.Rtc.
!     N3PAR=   50
!     NPPAR=   50
!     NEPAR=   50
!     NQPAR=   50
!     NDPAR=   50
!     NRPAR=   50 ! should be: 3 + 2*3*15       (2*MxTypeGtes * MmeGtes)
      NSCV =   1  ! was 10, now determined by checking the Sbk-measure file
      NPARS=   21 ! was NPARS= 4; =16 for River-RTC, ARS 13422, aug 2004, =20 for RTC-1D2D June 2006
                  ! January 2010: =21 to add Flow-reachsegment velocity as parameter 21
      NPAR3=    4 ! ARS 11612 Waternood-GGOR: June 2003:
                  !           RR ow-levels and  RR-gw levels in on-line communication
      NPAR3D   = 9999
      NPARQ=   500              ! may be overwritten from INI file using key NPARQ
      NLocHis  = 9999           ! WAS 9999, problem in call ReadDioPlt stack overflow
                                ! may be overwritten from INI file using key NLOCHIS
      NTimHis  = 365*50*24      ! 50 jaar, uurbasis data in externe HIS file
                                ! may be overwritten from INI file using key NTIMHIS
      NParaHis =7


      Return
      End Subroutine SetFixedParameters


      Subroutine SetOtherParameters

!     Zet variabele dimensies, op basis van de invoer van de gebruiker
!     NB Na lezen Ini file reeds gezet: NTIMSH, NTIMS, NTIMH

!     Zet max. aantal parameters in HIS uitvoer Precipitation en Wind (Extern)
      NParP = NTimH
      NParE = NTimH


!     Scan overige datafiles (lokaties, beslisparameters, maatregelen, bui en wind file)
!     Lokaties 3B          Nd3b,   Nd3bid
!     Lokaties Sbk         Nsbk    NSobek
!     Lokaties neerslag    Npre,   NPrecp
!     Lokaties extern wind NExt    NExtd
!                     HIS  NExtH   NExtHd
!     Beslisparameters     NDecV,  NPARA, NPar1, NPar2
!     Maatregelen 3B       N3MES,  N3Meas   (MLST records)
!                          N3Loc , N3MLoc   (3BML records)
!                          N3MAT , N3MatLoc (MATL records)
!                          N3BMs  (aantal unieke 3B id's)
!     Maatregelen Sobek    NSMES,  NSMeas, NSmId (unieke Sobek id's)
!     Buidata              NTIM ,
!                          NStat, NcStat
!                          NEvnt, NEvent
!     Winddata             NWind

!     Overige data (LowSPri etc.) wordt later ingevuld, niet relevant voor array dimensionering

!     ND3B  =  180
!     NSBK  =  250
!     NPRE  =   25
!     NEXT  =   25
!     NEXTH =   25
!     NDECV=  100
!     NSMES=  200
!     N3MES=   20
!     N3LOC=  150
!      NSTAT=   10
!      NEVNT=  200
!      NTIM = 2400
!      NTIMW = 2400

      Return
      End Subroutine SetOtherParameters

   End Module ParameterModule
