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

   Module OtherData

      Implicit None


! ***  Other data  (voorheen in Other.Cmn)
! ***  NEVENT  = aantal gebeurtenissen
! ***  NSTEP   = aantal tijdstappen per update vanuit 3B/Sobek (=1)
! ***  LASTTM  = laatste tijdstap nr
! ***  DELTAT  = tijdstapgrootte
! ***  DELTA2  = gecorrigeerde tijdstapgrootte
! ***  RTC_TIMOLD  = tijdstip dat wordt weggeschreven in Signal file RTC-proces
! ***  RTC_TIMNEW  = tijdstip waarop wordt gecheckt in Signal files andere processen (Sobek en/of 3B)
! ***  TMFILE  = tijdstip start 3B (=TIMNEW-DELTAT)
! ***  RTMSIZ  = tijdstipgrootte in seconden (real)
! ***  ITMSIZ  = tijdstipgrootte in seconden (integer)
! ***  IYEAR   = jaar, gerelateerd aan RTC_TIMNEW
! ***  IMO     = maand
! ***  IDAY    = dag
! ***  IHOUR   = uur
! ***  IMIN    = minuut
! ***  ISEC    = seconde (integer)
! ***  RSEC    = seconde (float)
! ***  IFYEAR  = jaar, gerelateerd aan TMFILE
! ***  IFMO    = maand
! ***  IFDAY   = dag
! ***  IFHOUR  = uur
! ***  IFMIN   = minuut
! ***  IFSEC   = seconde (integer)
! ***  RFSEC   = seconde (float)
! ***  IDHR    = delta t (geheel aantal uren)
! ***  IDMIN   = delta t (geheel aantal minuten)
! ***  IDSEC   = delta t (geheel aantal seconden)
! ***  RDSEC   = delta t (mogelijk fractioneel aantal seconden)
! ***
! ***  VERSION = versienummer
! ***  DATE    = datum
! ***  USE3B   = logical die aangeeft of met of zonder 3B gedraaid wordt
! ***  USESBK  = logical die aangeeft of met of zonder Sobek gedraaid wordt
! ***  USE3D   = logical die aangeeft of met of zonder D3DFlow gedraaid wordt
! ***  USEPRE  = logical die aangeeft of met of zonder Precipitation voorspelling gedraaid wordt
! ***  USEEXT  = logical die aangeeft of met of zonder External data gedraaid wordt
! ***  USEWQ   = logical die aangeeft of met of zonder WQ data gedraaid wordt
! ***  SOBEKMODE  = logical die aangeeft of in SOBEK mode gedraaid wordt
! ***  CoupledToDelft3D = logical die aangeeft of Delft3D online meedraait
! ***  D3DACTIVE  = logical die aangeeft of comminicatie met Delft3D al actief is
! ***  TIMF3B  = logical die aangeeft of tijdstapinfo vanuit 3B komt of niet (=uit BUI of RKS file)
! ***  WRTHIS  = logical die aangeeft of RTC overall HIS files moet schrijven voor reeks
! ***  IOPTS   = optie of Sobek HIS of ASCII invoer heeft
! ***  IOPT3   = optie of 3B HIS of ASCII invoer heeft
! ***  IOPTP   = optie of Precipitation HIS of ASCII invoer heeft
! ***  IOPTE   = optie of Externe data  HIS of ASCII invoer heeft
! ***            1 ASCII; 2=HIS format voor Sobek, 3B, Precipitation en Externe files
! ***  IOPTWQ  = optie of WQ data  HIS of ASCII invoer heeft
! ***
! ***
! ***  MATDBG = optie om Matlab in Debugmode te draaien; dwz met pause statement; 1=met pause statement, 0=zonder pause statement
! ***  MATEXE = full name of Matlab executable
! ***  MATDIR = directory Matlab M-file
! ***  MATFIL = filenaam Matlab M-file
! ***  MATUSE = MATLAB measures are used  (true or false)
! ***
! ***  SetSequenceDecisionParameters = yes = set by RTC
! ***                                  no  = set by user in input file (backwards compatible)
! ***  WindUseTableModule = yes = use NewTables_rtc for wind tables
! ***                       no  = do not, = old method
! ***  ReduceWindTable    = yes = reduce length of Wind Table if possible (default)
! ***                       no  = do not

      INTEGER      NEVENT, NSTEP, LASTTM, IYEAR, IMO, IDAY, IHOUR, IMIN, ISEC, ITMSIZ, &
                   IFYEAR, IFMO, IFDAY, IFHOUR, IFMIN, IFSEC, IDHR,  IDMIN, IDSEC
      INTEGER      IOPTS, IOPT3, IOPTP, IOPTE, IOPTWQ
      Double Precision DELTAT, DELTA2, RTC_TIMOLD, RTC_TIMNEW, TMFILE, &
                       RTMSIZ, RSEC, RDSEC, RFSEC
      CHARACTER*12 VERSION, DATEVersion
      LOGICAL      USE3B, USESBK, USE3D, USEPRE, USEEXT, USEWQ, TIMF3B, WRTHIS, MATUSE
      LOGICAL      SOBEKMODE, CoupledToDelft3D, D3DACTIVE, &
                   SetSequenceDecisionParameters, WindUseTableModule, ReduceWindTable

!     DebugFromTimestep, DebugToTimestep om debug periodes aan te geven
!     OutputTimestep = om de zoveel tijdstappen uitvoer (ARS 7688)
      Integer      DebugFromTimestep, DebugToTimestep
      Integer      DebugFromTimestep2, DebugToTimestep2
      Integer      OutputTimestep
      Integer      UseInitValues

      CHARACTER*256 MATEXE
      CHARACTER*99 MATFIL, MATDIR
!  MatRR    = true : pass RR data to Matlab
!  MatRain  = true : pass Rainfall data to Matlab
      Logical      MatRR, MatRain, MatRainPredict, MatWindPredict, MatPredictMulti, MatWq
      INTEGER      MATDBG, MatlabNrWqPar
      CHARACTER*256, Pointer, Save ::  MatlabWqParId(:)
      Integer, Pointer, Save ::  MatlabWq(:)
      Logical      MatlabCommunicationOldStyle
      Integer      MaxCountGeneral, MaxCountBiLcLocations, MaxCountStructures, &
                   MaxCountNodes, MaxCountReachSegments, MaxCountMeasLocations, &
                   MaxCountRRLocations, MaxCountRRSetpoints, MaxCountCFSetpoints, &
                   MaxCount1D2DLocations
      Integer      ToMatlab1D2DH, ToMatlab1D2DWD, ToMatlab1D2DBL, ToMatlab1D2DU, ToMatlab1D2DV, ToMatlab1D2DC

! Oct 2017  coupling with external exe (e.g. TCN)
!           reading/writing csv files with data, and calling exe/batch for communication with TCN
      Logical        UseTCN
      CHARACTER*256  RunCommand
      CHARACTER*256  WriteCsvFile, ReadCsvFile
      Integer        IOutCsv, IInCsv
      CHARACTER*256, allocatable, Save ::  CsvReadId(:)
      Real         , allocatable, Save ::  CsvReadValue(:)

! ***  Other data (Predict.cmn)
!
! ***  USEP    = neerslagvoorspelling ja/nee
! ***  USEW    = windvoorspelling ja/nee
! ***  NTIMHP  = time horizon precipitation
! ***  NTIMHW  = time horizon wind
! ***  IMODEP  = mode neerslag voorspelling
! ***            0 = perfecte voorspelling
! ***            1 = nog niet geimplementeerd
! ***            2 = nog niet geimplementeerd
! ***  IMODEW  = mode wind voorspelling
! ***            0 = perfecte voorspelling
! ***            1 = nog niet geimplementeerd
! ***            2 = nog niet geimplementeerd
! ***  TIMSTB  = tijdstip start bui
! ***  TIMENB  = tijdstip eind bui
! ***  TIMEND  = eindtijdstip volgens control of 3B-bui

      INTEGER            IMODEP, IMODEW, NTIMHP, NTIMHW

      Double Precision   TIMSTB, TIMENB, TIMEND

      LOGICAL            USEP, USEW

! ***  IDATE = Date as used in communication with Flow (YYYYMMDD)
! ***  ITIME = Time as used in communication with Flow (HHMMSS)
      INTEGER IDATE, ITIME


! *** Communication to Matlab with or WithoutCString (SobekC, RRC)

      LOGICAL            WithoutSobekCString

! *** Variable to pass active language read from INI file to LanguageModule_rtc
      Integer            ActiveLanguage

      Logical            OnMatlabErrorQuit

      Logical            RTCSelfCrash

   End Module OtherData
