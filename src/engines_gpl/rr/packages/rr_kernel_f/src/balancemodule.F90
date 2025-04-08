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

module Balance

  !use
  use Conf_fil
  use Conf_Arr
  use Network
  use Link
  use NWRW    ! voor PlvBal results
  use Output  ! voor de WriteT0string

  use dio_plt_rw, prop_file_unused => prop_file

  implicit none

  Integer, parameter :: MaxSeries = 41, MaxBalTerms=30, MaxNwrwTerms=6

  ! variables

  Integer             NBalSeries
  Integer             IoBal
  Real                Balance_error, Bal_error2, Bal_error3, simulated_period, MaxBalanceError
  Double Precision    Bal3B(MaxBalTerms), TotalArea, PlvBalEvents(MaxNwrwTerms), PlvBalHistory(MaxNwrwTerms)

  type(DioPltType)  :: DataSetHisRRBalance      ! output dataset HIS balance file
  type(DioPltType)  :: DataSetHisNWRWSystem     ! output dataset HIS NWRW balance, only to be used if NWRWContinuous flag is on

  !Bal3B 1  = totaal area (ha)
  !      2  = totale neerslag (m3)
  !      3  = verdamping paved area (m3)
  !      4  = verdamping unpaved area (m3)
  !      5  = verdamping +kasgebruik greenhouse area (m3)
  !      6  = verdamping open water (m3)
  !      7  = netto kwel onverhard gebied (m3)
  !      8  = netto kwel open water  (m3)
  !      9  = storage change paved area (m3)
  !      10 = storage change unpaved area (m3)
  !      11 = storage change greenhouse area (m3)
  !      12 = storage change open water area (m3)
  !      13 = total outflow at boundaries (m3)
  !      14 = total inflow at boundaries (m3)
  !      15 = DWA op verhard gebied (m3)
  !      16 = netto industriele lozing (m3)
  !      17 = storage change bij RWZI's (m3)
  !      18 = act.Evap Sacramento (m3)
  !      19 = Losses (Side, Ssout) Sacramento en LGSI en Wagmod_Seep (m3)
  !      20 = Storage Change Reservoirs Sacramento (m3)
  !      21 = Storage Change Surface runoff Unit Hydrograph Sacramento (m3)
  !      22 = Transfer to NWRW nodes (m3)
  !      23 = Storage change RR routing links (m3)
  !      24 = Total evap cel
  !      25 = Netto kwel+DWF cel
  !      26 = Storage change cel
  !      27 = Total outflow cel
  !      28 = Total inflow RRRunoff nodes
  !      29 = Storage change RRRunoff nodes
  !      30 = Net evap HBV


contains



   Subroutine WriteHdrBalanceFile

!*********************************************************************
!***                D E L F T         H Y D R A U L I C S
!***
!***              WATER RESOURCES AND ENVIRONMENT DIVISION
!*********************************************************************
!*** Program :  DELFT-3B version 2.00                Date: Nov   1996
!*** Module  :
!*********************************************************************
!*** Created    : March  1999                     By : Geert Prinsen
!*********************************************************************
!*********************************************************************
!*** Brief description:
!*** ------------------
!***   Write Header info in 3B-balance file
!*********************************************************************
!*** Input/output parameters:     none.
!*** ------------------------
!*********************************************************************
!*** ADRESS     : Deltares
!***              P.O.BOX 177,
!***              2600 MH DELFT, THE NETHERLANDS
!**********************************************************************



  IMPLICIT NONE !!!!!!!!!


  Character(Len=CharIdLength) String

        Call OpenFl (IoBal,ConfFil_get_NAMFIL(85),1,2)

        if (ncnode .gt. 0) then
            STRING = ConfFil_get_NAMFIL(13)
!cgp12sept  string = string(len_trim(string) - 12:) ! laatste 12 karakters van de string pakken
!cgp12sept  string = string(scan(string, '\') + 1:)! op zoek naar de backslash
!           string = string(:scan(string, '.') - 1) ! op zoek naar de punt
            WRITE (IOBAL,42) string

            STRING = ConfFil_get_NAMFIL(14)
            WRITE (IOBAL,421) string

            IF (nckas .gt. 0) then
                STRING = ConfFil_get_NAMFIL(17)
                WRITE (IOBAL,422) string
                STRING = ConfFil_get_NAMFIL(18)
                WRITE (IOBAL,423) string
                STRING = ConfFil_get_NAMFIL(12)
                WRITE (IOBAL,424) string
            ENDIF

            IF (ncovhg .gt. 0) then
                STRING = ConfFil_get_NAMFIL(19)
!12sept         string = string(len_trim(string) - 12:) ! laatste 12 karakters van de string pakken
!12sept         string = string(scan(string, '\') + 1:)! op zoek naar de backslash
                WRITE (IOBAL,425) string
                STRING = ConfFil_get_NAMFIL(20)
                WRITE (IOBAL,426) string
            ENDIF

            IF (ncow .gt. 0) then
                STRING = ConfFil_get_NAMFIL(40)
                WRITE (IOBAL,427) string
            ENDIF

            WRITE (IOBAL,428) timeSettings%timestepSize

   42       FORMAT (' Results 3B calculation',//,' Rainfall file  ',20X,': ',A)
  421       FORMAT (' Evaporation file ',18X,': ',A)
  422       FORMAT (' Greenhouse Initialisation file ',4X,': ',A)
  423       FORMAT (' Greenhouse Water Use file      ',4X,': ',A)
  424       FORMAT (' Greenhouse classification file ',4X,': ',A)
  425       FORMAT (' Crop factor file ',18X,': ',A)
  426       FORMAT (' Storage coefficient file       ',4X,': ',A)
  427       FORMAT (' Crop factor file open water    ',4X,': ',A)
  428       FORMAT (' Timestep size (s)',18X,': ',I8)
        endif

        MaxBalanceError = 0.0

    Return

  END subroutine WriteHdrBalancefile



   Subroutine WriteCheckBalance (ITimestep)

! Writes each timestep to IoBalance file, current balance error
      implicit none

      Integer i, ITimestep

       ! Total inflow
        Balance_error = Bal3B(2) + Bal3B(7) + Bal3b(8) + Bal3B(14) + Bal3B(15)+ Bal3B(16) + Bal3B(25) + Bal3B(28)
        Bal_error2 = Balance_error
        Bal_error3 = Balance_error
       ! minus total outflow
        DO i=3,6
           Balance_error = Balance_error - BAL3B(i)
        enddo
        DO i=9,13
           Balance_error = Balance_error - BAL3B(i)
        enddo
        Balance_error = Balance_error - BAL3B(17)    ! storage changes RWZI
        DO i=18,21        ! Sacramento outflows and storage change
           Balance_error = Balance_error - BAL3B(i)
        enddo
        Balance_error = Balance_error - BAL3B(23)    ! Storage change RR routing links
        Balance_error = Balance_error - BAL3B(22)    ! Transfer to NWRW
        ! nieuwe cel: uitgaande termen
        Balance_error = Balance_error - BAL3B(24)    ! evap
        Balance_error = Balance_error - BAL3B(26)    ! storage change
!       Balance_error = Balance_error - BAL3B(27)    ! outflow cell

        Balance_error = Balance_error - BAL3B(29)    ! storage changes RRRunoff
        Balance_error = Balance_error - BAL3B(30)    ! RRRUnoff HBV/LGSI/Wagmod evap

        if (bal_error2 .ne. 0) then
            Bal_error2 = Balance_error * 100. / Bal_error2
        else
            bal_error2 = 0.0
        endif
!       write(iobal,*) ' tijdstap en Balance_error', itimestep, balance_error

  END subroutine WriteCheckBalance





   Subroutine WriteBalance (Ievent, accum)

! Write Balance output at end of event/series of event

   implicit none

   Integer IEvent, i
   Logical accum


        if (Accum) then
          WRITE (iobal,41) simulated_period, Ievent
   41     FORMAT(' Simulated period (hours)           : ',F11.2, ' for number of Events=',I6)
        else
          WRITE (iobal,401) simulated_period, Ievent
  401     FORMAT(' Simulated period (hours)           : ',F11.2, ' for Event number =',I6)
        Endif

!balans poldermodel
        IF (ncnode-ncpluv .gt. 0) THEN
            ! bal_error2 relative tov inflow
           Balance_error = Bal3B(2) + Bal3B(7) + Bal3b(8) + Bal3B(14) + Bal3B(15)+ Bal3B(16) + Bal3B(25) + Bal3B(28)
           Bal_error2 = Balance_error
            ! bal_error2 relative tov outflow: all evaporation terms + lossflow + boundary outflow
           Bal_error3 = Bal3B(3) + Bal3B(4) + Bal3b(5) + Bal3B(6) + &
                         Bal3B(30)+ Bal3B(24) + Bal3B(18)+ Bal3B(19) + Bal3B(13)
           DO i=3,6
              Balance_error = Balance_error - BAL3B(i)
           enddo
           DO i=9,13
              Balance_error = Balance_error - BAL3B(i)
           enddo
           Balance_error = Balance_error - BAL3B(17)    ! storage changes RWZI
           DO i=18,21        ! Sacramento outflows and storage change and LGSI losses
              Balance_error = Balance_error - BAL3B(i)
           enddo
           Balance_error = Balance_error - BAL3B(23)    ! Storage change RR routing links
           Balance_error = Balance_error - BAL3B(22)    ! Transfer to NWRW
           ! nieuwe cel: uitgaande termen
           Balance_error = Balance_error - BAL3B(24)    ! evap
           Balance_error = Balance_error - BAL3B(26)    ! storage change
!          Balance_error = Balance_error - BAL3B(27)    ! outflow cell

           Balance_error = Balance_error - BAL3B(29)    ! RRRUnoff storage change
           Balance_error = Balance_error - BAL3B(30)    ! RRRUnoff HBV evap
           if (bal_error2 .ne. 0) then
               Bal_error2 = Balance_error * 100. / Bal_error2
           else
               bal_error2 = 0.0
           endif
           if (bal_error3 .ne. 0) then
               Bal_error3 = Balance_error * 100. / Bal_error3
           else
               bal_error3 = 0.0
           endif
! ARS9591
! Write output, depending on existence of node types
           WRITE (iobal,4110) (BAL3B(I),I=1,2)
 4110      FORMAT(//,' Summary results Sobek-RR Rural model   ',//, &
                 ' Total area (ha)                    :',F15.2, /,  &
                 ' Total rainfall (m3)                :',F15.2)
! evaporation and other abstractrions
           if (ncvhg .gt. 0) then
             WRITE (iobal,4111) BAL3B(3)
 4111        FORMAT(' Total evaporation paved area (m3)  :',F15.2)
           endif
           if (ncovhg .gt. 0) then
             WRITE (iobal,4112) BAL3B(4)
 4112        FORMAT(' Total evaporation unpaved area (m3):',F15.2)
           Endif
           if (nckas .gt. 0) then
             WRITE (iobal,4113) BAL3B(5)
 4113        FORMAT(' Total water use greenhouses (m3)   :',F15.2)
           Endif
           if (ncOw+ncOwRain .gt. 0)  then
             WRITE (iobal,4114) BAL3B(6)
 4114        FORMAT(' Total evaporation open water (m3)  :',F15.2)
           Endif
           if (ncSacr .gt. 0) then
             WRITE (iobal,4115) BAL3B(18)
 4115        FORMAT(' Total evaporation Sacramento (m3)  :',F15.2)
           Endif
           if (ncSacr .gt. 0 .or. NcRrRunoff .gt. 0) then
             WRITE (iobal,41151) Bal3b(19)
 41151       FORMAT(' Loss flows Sac/SCS/LGSI/Wag/NAM(m3):',F15.2)
           Endif
! discharges
           if (ncvhg .gt. 0) then
             WRITE (iobal,4121) BAL3B(15)
 4121        FORMAT(' Total DWA paved area (m3)          :',F15.2)
           Endif
           if (ncindus .gt. 0) then
             WRITE (iobal,4122) BAL3B(16)
 4122        FORMAT(' Net discharge industry (m3)        :',F15.2)
           Endif
           if (ncovhg .gt. 0) then
             WRITE (iobal,4123) BAL3B(7)
 4123        FORMAT(' Net seepage unpaved area (m3)      :',F15.2)
           Endif
           if (ncOw .gt. 0)  then
             WRITE (iobal,4124) BAL3B(8)
 4124        FORMAT(' Net seepage open water (m3)        :',F15.2)
           Endif
! storage changes
           if (ncvhg .gt. 0) then
             WRITE (iobal,4131) BAL3B(9)
 4131        FORMAT(' Storage change paved area (m3)     :',F15.2)
           Endif
           if (ncovhg .gt. 0) then
             WRITE (iobal,4132) BAL3B(10)
 4132        FORMAT(' Storage change unpaved area (m3)   :',F15.2)
           Endif
           if (nckas .gt. 0) then
             WRITE (iobal,4133) BAL3B(11)
 4133        FORMAT(' Storage change greenhouses (m3)    :',F15.2)
           Endif
           if (ncOw .gt. 0)  then
             WRITE (iobal,4134) BAL3B(12)
 4134        FORMAT(' Storage change open water (m3)     :',F15.2)
           Endif
           if (ncRwzi .gt. 0)  then
             WRITE (iobal,4135) BAL3B(17)
 4135        FORMAT(' Storage change WWTP (m3)           :',F15.2)
           Endif
           if (ncSacr .gt. 0)  then
             WRITE (iobal,4136) BAL3B(20)
 4136        FORMAT(' Storage change Sacr.reservoirs (m3):',F15.2)
           Endif
           if (ncSacr .gt. 0 .or. NcRRRunoff .gt. 0)  then
             WRITE (iobal,41363) BAL3B(21)
 41363       FORMAT(' Storage ch.Sacr/LGSI/Wag.routng(m3):',F15.2)
           Endif
           if (ncRRRunoff .gt. 0)  then
             WRITE (iobal,41360) BAL3B(28), Bal3B(30), Bal3B(29)
41360        FORMAT(' External inflow RRRunoff nodes (m3):',F15.2, /,&
                    ' Total evap. RRRunoff-HBV-Wagmod(m3):',F15.2, /,&
                    ' Storage change RRRunoff nodes (m3) :',F15.2)
           Endif
! storage change routing links
           if (RoutingLinkExists)  then
             WRITE (iobal,41361) BAL3B(23)
41361        FORMAT(' Storage change RR routing links(m3):',F15.2)
           Endif
! New Urban Cell
           if (ncCell .gt. 0)  then
             WRITE (iobal,41362) BAL3B(24), Bal3B(25), Bal3B(26) !, Bal3B(27)
 41362       FORMAT(' Total evaporation at cells (m3)    :',F15.2, /, &
                    ' Total seepage&DWF at cells (m3)    :',F15.2, /, &
                    ' Total storage change at cells (m3) :',F15.2)
!                    , /, &
!                    ' Total outflow at cells (m3)        :',F15.2)
           Endif
! outflow
           if (ncBoun .gt. 0)  then
             WRITE (iobal,4137) BAL3B(13), Bal3B(14)
 4137        FORMAT(' Total outflow at boundaries (m3)   :',F15.2, /,  &
                    ' Total inflow at boundaries (m3)    :',F15.2)
           Endif
! NWRW
           if (ncPluv .gt. 0)  then
             WRITE (iobal,4138) BAL3B(22)
 4138        FORMAT(' Total transfer to RR-Urban   (m3)  :',F15.2)
           endif
! balance error
           Write(iobal,4140) Balance_Error, Bal_error2, Bal_error3
 4140      FORMAT(' Balance error (m3)',17X,':',F15.2,' (',F7.4,'% of inflow;', F7.4,'% of outflow)')
!End ARS9591
! ARS 8841 give error/warning if balance error too large
             If (Abs(Bal_error2) .gt. LargeBalanceErrorPercentage .and. &
                  Abs(Bal_error3) .gt. LargeBalanceErrorPercentage) then
                call ErrMsgStandard (910, 910, ' WriteBalance',' WriteBalance')
             Endif
        ENDIF

!balans Pluvius inloopmodel
        IF (ncpluv .gt. 0) then
            Balance_error = PLVBAL(2) - PLVBAL(3) - PLVBAL(4) - PLVBAL(5) - PLVBAL(6) - PLVBAL(7)
            Balance_error = Balance_error + BAL3B(22)    ! Transfer to NWRW
            if (PLVBAL(2) .ne. 0) then
                Bal_error2 = Balance_error * 100. / PLVBAL(2)
            else
                bal_error2 = 0.0
            endif

            If (NcNode - NcPluv .gt. 0) then
              WRITE (iobal,411) (PLVBAL(I),I=1,10), Bal3B(22), Balance_Error, Bal_error2
  411         FORMAT(//,' Summary results Sobek-RR Urban model ',//, &
                    ' Total area (m2)                    :',F13.2, /,  &
                    ' Total rainfall (m3)                :',F13.2, /,  &
                    ' Total evaporation (m3)             :',F13.2, /,  &
                    ' Total infiltration depressions (m3):',F13.2, /,  &
                    ' Total infiltration from runoff (m3):',F13.2, /,  &
                    ' Total storage change (m3)          :',F13.2, /,  &
                    ' Total inflow sewer excl. DWD (m3)  :',F13.2, /,  &
                    ' Total DWA people (m3)              :',F13.2, /,  &
                    ' Total DWA companies (m3)           :',F13.2, /,  &
                    ' Total inflow sewer (m3)            :',F13.2, /,  &
                    ' Total transfer from RR-Rural(m3)   :',F13.2, /,  &
                    ' Balance error (m3)',17X,':',F13.2,' (',F7.4,'%)')
            else
              WRITE (iobal,412) (PLVBAL(I),I=1,10), Balance_Error, Bal_error2
  412         FORMAT(//,' Summary results Sobek-RR Urban model ',//, &
                    ' Total area (m2)                    :',F13.2, /,  &
                    ' Total rainfall (m3)                :',F13.2, /,  &
                    ' Total evaporation (m3)             :',F13.2, /,  &
                    ' Total infiltration depressions (m3):',F13.2, /,  &
                    ' Total infiltration from runoff (m3):',F13.2, /,  &
                    ' Total storage change (m3)          :',F13.2, /,  &
                    ' Total inflow sewer excl. DWD (m3)  :',F13.2, /,  &
                    ' Total DWA people (m3)              :',F13.2, /,  &
                    ' Total DWA companies (m3)           :',F13.2, /,  &
                    ' Total inflow sewer (m3)            :',F13.2, /,  &
                    ' Balance error (m3)',17X,':',F13.2,' (',F7.4,'%)')
            endif
! ARS 8841 give error/warning if balance error too large
            If (Abs(Bal_error2) .gt. LargeBalanceErrorPercentage) then
               call ErrMsgStandard (910, 910, ' WriteBalance',' WriteBalance')
            Endif
        ENDIF
! ARS 13649: add maximum absolute balance error during simulation
        WRITE (iobal,413) MaxBalanceError
  413   FORMAT(' Maximum balance error in simulation:',F15.2)

        Call CloseGP (iobal)

  END subroutine WriteBalance


   Subroutine WriteHdrBalanceHisFile (NwrwContinuous)

!*********************************************************************
!***                D E L F T         H Y D R A U L I C S
!***
!***              WATER RESOURCES AND ENVIRONMENT DIVISION
!*********************************************************************
!*** Program :  DELFT-3B version 2.00                Date: Nov   1996
!*** Module  :
!*********************************************************************
!*** Created    : March  1999                     By : Geert Prinsen
!*********************************************************************
!*********************************************************************
!*** Brief description:
!*** ------------------
!***   Write Header info in 3B-balance HIS file
!***   HIS file is only written if 1 event is simulated
!*********************************************************************

! for netCdf
  use netCdfData
  use readlib
  use NetCdf


  IMPLICIT NONE

  Logical   NwrwContinuous  ! =true if DataSetHisNwrwSystem should be generated

  Character(len=160) String

  Integer       IYear, IMo, IDay, IHour, IMin, ISec, TimeSeriesVar
  Integer       TmSize
  Logical       Success

! DIO
  Character(Len=DioMaxStreamLen)  :: outName   ! name of output dataset
  character(len=HisRunIdSize), dimension(HisRunIdDim) :: runId         ! (HIS) runid
  Character(Len=DioMaxParLen), dimension(MaxSeries)   :: HisIdSeries   ! all possible variable(s) in dataset
  Character(Len=DioMaxParLen), dimension(MaxSeries)   :: LongHisIdSeries   ! all possible long variable(s) in dataset
  Character(Len=DioMaxParLen), pointer, dimension(:)  :: ParNames      ! actual variable(s) in dataset
  Character(Len=DioMaxParLen), pointer, dimension(:)  :: LongParNames  ! long actual variable(s) in dataset
  Character(Len=DioMaxLocLen), dimension(1)           :: HisLocation   ! locations(s) in dataset

! NetCdf
  Integer                             jpar, ipos1
! Character(Len=40)                :: DateString
  Character(Len=40), dimension(1)  :: locationid
  integer                             nitem, refdate, reftime
! Character(Len=40)                   Id
  Character(Len=40)                   VariableName, LongVariableName
  Character(len=8)                    Unit

! create empty dataset (no name, var. type unknown)
  DataSetHisRRBalance = DioPltCreate('NoName', Dio_PLT_Unknown)

  TimeSeriesVar = -1

! Parameter and location id's
        HisIdSeries (1)  = 'Rainfall (+ext.irrigation supply)'
        HisIdSeries (2)  = 'Evap.Paved'
        HisIdSeries (3)  = 'Evap.+Irr.Loss.Unpaved'
        HisIdSeries (4)  = 'Use Greenhouses'
        HisIdSeries (5)  = 'Evap.OpenWater'
        HisIdSeries (6)  = 'Evap.Sacramento'
        HisIdSeries (7)  = 'NetLoss Sacr/LGSI/Wagmod'
        HisIdSeries (8)  = 'DWF Paved'
        HisIdSeries (9)  = 'Net Disch. Industry'
        HisIdSeries (10) = 'Net Seepage Unpaved'
        HisIdSeries (11) = 'Net Seepage OpenWater'
        HisIdSeries (12) = 'Storage Paved'
        HisIdSeries (13) = 'Storage Unpaved'
        HisIdSeries (14) = 'Storage Greenhouses'
        HisIdSeries (15) = 'Storage OpenWater'
        HisIdSeries (16) = 'Storage WWTP '
        HisIdSeries (17) = 'Storage Sacramento'
        HisIdSeries (18) = 'Storage Routing Sacramento/LGSI'
        HisIdSeries (19) = 'Storage RR routing links'
        HisIdSeries (20) = 'Cell evaporation'
        HisIdSeries (21) = 'Cell seepage&DWF'
        HisIdSeries (22) = 'Storage change cell'
        HisIdSeries (23) = 'Outflow cell'
        HisIdSeries (24) = 'Boundaries out'
        HisIdSeries (25) = 'Boundaries in'
        HisIdSeries (26) = 'External inflow RRRunoff'
        HisIdSeries (27) = 'Evaporation RRRunoff-HBV-Wagmod-Walrus'
        HisIdSeries (28) = 'Storage change RRRunoff'
        HisIdSeries (29) = 'Balance error RR Rural'
        HisIdSeries (30) = 'Rainfall NWRW'
        HisIdSeries (31) = 'Evaporation NWRW'
        HisIdSeries (32) = 'Infiltr.storage NWRW'
        HisIdSeries (33) = 'Infiltr.runoff NWRW'
        HisIdSeries (34) = 'Storage NWRW'
        HisIdSeries (35) = 'RWF NWRW'
        HisIdSeries (36) = 'DWF people NWRW'
        HisIdSeries (37) = 'DWF companies NWRW'
        HisIdSeries (38) = 'RWF+DWF NWRW'
        HisIdSeries (39) = 'Balance error NWRW'
        HisIdSeries (40) = 'Total balance error'
        HisIdSeries (41) = 'Transfer from RR-Rural to Urban'

        HisLocation  = 'Total RR system'

        LongHisIdSeries (1)  = 'Rainfall plus external irrigation supply'
        LongHisIdSeries (2)  = 'Evaporation Paved'
        LongHisIdSeries (3)  = 'Evaporation plus irrigation losses Unpaved'
        LongHisIdSeries (4)  = 'Water Use Greenhouses'
        LongHisIdSeries (5)  = 'Evaporation OpenWater'
        LongHisIdSeries (6)  = 'Evaporation Sacramento'
        LongHisIdSeries (7)  = 'NetLoss Sacramento/LGSI/Wagmod/Walrus'
        LongHisIdSeries (8)  = 'Dry Weather Flow Paved'
        LongHisIdSeries (9)  = 'Net Discharge Industry'
        LongHisIdSeries (10) = 'Net Seepage Unpaved'
        LongHisIdSeries (11) = 'Net Seepage OpenWater'
        LongHisIdSeries (12) = 'Storage Paved'
        LongHisIdSeries (13) = 'Storage Unpaved'
        LongHisIdSeries (14) = 'Storage Greenhouses'
        LongHisIdSeries (15) = 'Storage OpenWater'
        LongHisIdSeries (16) = 'Storage Waste Water Treatment Plants'
        LongHisIdSeries (17) = 'Storage Sacramento'
        LongHisIdSeries (18) = 'Storage Routing Sacramento/LGSI'
        LongHisIdSeries (19) = 'Storage RR routing links'
        LongHisIdSeries (20) = 'Cell evaporation'
        LongHisIdSeries (21) = 'Cell seepage plus dry weather flow'
        LongHisIdSeries (22) = 'Storage change cell'
        LongHisIdSeries (23) = 'Outflow cell'
        LongHisIdSeries (24) = 'Boundaries out'
        LongHisIdSeries (25) = 'Boundaries in'
        LongHisIdSeries (26) = 'External inflow RRRunoff'
        LongHisIdSeries (27) = 'Evaporation RRRunoff-HBV-Wagmod-Walrus'
        LongHisIdSeries (28) = 'Storage change RRRunoff'
        LongHisIdSeries (29) = 'Balance error RR Rural'
        LongHisIdSeries (30) = 'Rainfall NWRW'
        LongHisIdSeries (31) = 'Evaporation NWRW'
        LongHisIdSeries (32) = 'Infiltration from storage NWRW'
        LongHisIdSeries (33) = 'Infiltration from runoff NWRW'
        LongHisIdSeries (34) = 'Storage NWRW'
        LongHisIdSeries (35) = 'Storm water flow NWRW'
        LongHisIdSeries (36) = 'Dry Weather flow people NWRW'
        LongHisIdSeries (37) = 'Dry Weather flow companies NWRW'
        LongHisIdSeries (38) = 'Storm water flow plus dry weather flow NWRW'
        LongHisIdSeries (39) = 'Balance error NWRW'
        LongHisIdSeries (40) = 'Total balance error'
        LongHisIdSeries (41) = 'Transfer from RR-Rural to Urban'

! Construct T0 string
        iYear = ConfArr_get_IYEAR()
        iMo   = ConfArr_get_iMonth()
        iDay  = ConfArr_get_IDAY()
        iHour = ConfArr_get_IHour()
        iMin  = ConfArr_get_IMinute()
        iSec  = ConfArr_get_ISecond()
        TmSize= timeSettings%timestepSize
! include T0 string
        String = ' '
        Call WriteT0String (String(1:160), IYear,Imo,Iday,Ihour,Imin,Isec,TmSize)

        outName = Conffil_get_Namfil(107)
        runId  = ' '
        runId(1) = CASENM(1:)
        runId(3) = 'TITLE: RR Water balance             '
        RunId(4) = String(121:)

! Determine nr of series in file, depending on schematisation with/without NWRW
        NBalSeries = MaxSeries-1
        If (NcPluv .eq. 0) NBalSeries = 29
        If (NcPluv .eq. NcNode) NBalSeries = 10
        If (UrbanRuralConnected) NBalSeries=MaxSeries

! set ParNames
        success = Dh_AllocInit (NBalSeries, ParNames, ' ')
        success = Dh_AllocInit (NBalSeries, LongParNames, ' ')
!        Allocate ( ParNames(NBalSeries), Stat=Allocation_Error )
        If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', &
                                                ' WriteHdrBalanceHisFile')
        If (NBalSeries .ge. 29) then
           ParNames = HisIdSeries(1:NBalSeries)
           LongParNames = LongHisIdSeries(1:NBalSeries)
        ElseIf (NBalSeries .eq. 10) then
           ParNames = HisIdSeries(30:39)
           LongParNames = LongHisIdSeries(30:39)
        Endif

        DataSetHisRRBalance = DioPltDefine(outName, runId, Dio_Plt_Real, ParNames, HisLocation)

! NetCdf output
        if (GenerateNetCdfOutput) then
           ! NetCdf file name is now constructed from his outputfilename, replace .his with .nc
           TimestepInNWRWSys = 0
           NetCdfOutputFileName = Conffil_get_Namfil(107)
           Ipos1 = FndFrst('.his', NetCdfOutputFileName, .false.)
           Ipos1 = max (ipos1, FndFrst('.His', NetCdfOutputFileName(1:), .false.) )
           Ipos1 = max (ipos1, FndFrst('.HIS', NetCdfOutputFileName(1:), .false.) )
           if (ipos1 .le. 0) then
              call ErrMsgStandard(972, 0, ' Error in setting Netcdf output file name for balance output',' BalanceModule_WriteHdrBalanceHisFile')
           endif
           NetCdfOutputFileName(ipos1:) = ''
           NetCdfOutputFileName(ipos1:) = '.nc'
           ! create NetCdf file
           INetCdfFile(BalanceNetCdfFileNr) = nc_create (NetCdfOutputFileName)

           nitem = 1  ! 1 location
           locationid(1:) = HisLocation(1)(1:40)
           refdate = iYear * 10000 + Imo * 100 + IDay
           reftime = ihour * 10000 + Imin * 100 + Isec
           call nc_prepare(iNetCdfFile(BalanceNetCdfFileNr), nitem, '', loc_dimid(BalanceNetCdfFileNr), time_dimid(BalanceNetCdfFileNr), refdate, reftime, time_varid(BalanceNetCdfFileNr),locationid, .false.)
           id_locdim = loc_dimid(BalanceNetCdfFileNr)
           id_timedim = time_dimid(BalanceNetCdfFileNr)
           id_strlendim = 40
!          write(*,*) ' Nc dimensions '
!          write(*,*) ' id_locdim', id_locdim
!          write(*,*) ' id_timedim', id_timedim
!          write(*,*) ' id_strlendim', id_strlendim

           unit = 'm3'
           Do jpar=1,NBalSeries
              VariableName = Parnames(jpar)(1:40)
              LongVariableName = LongParnames(jpar)(1:40)
              ! Variable name may only contain letters, cijfers and underscores (so no ()*&^%$#@!?><=+-;:.,space)
              VariableName = NetCdfName(VariableName)
              id_vars(BalanceNetCdfFileNr,jpar) = HisSetupVariable (INetCdfFile(BalanceNetCdfFileNr), VariableName(1:len_trim(VariableName)), nf90_double, (/ id_locdim, id_timedim /), &
                                                    VariableName(1:len_trim(VariableName)), LongVariableName(1:len_trim(LongVariableName)),unit, TimeSeries=TimeSeriesVar)
!              ierr = nf90_put_att(ncid, id_vars(jpar),   'cf_role', 'timeseries_id')
!              Call NetCdfCheck('NC_Prepare - After put_att cf_role',ierr)
           Enddo
           ierr = nf90_enddef(INetCdfFile(BalanceNetCdfFileNr))
           Call NetCdfCheck(' BalanceModule after Enddef',ierr)
        endif

        Deallocate (ParNames)
        Deallocate (LongParNames)

! Dataset for NWRW system
        If (NwrwContinuous) then
           DataSetHisNWRWSystem= DioPltCreate('NoName', Dio_PLT_Unknown)

! Parameter and location id's
           HisIdSeries (1) = 'Total inflow sewer'
           HisIdSeries (2) = 'RWF sewer inflow'
           HisIdSeries (3) = 'DWF sewer inflow'
           HisIdSeries (4) = 'Total inflow sewer excluding events'
           HisIdSeries (5) = 'RWF sewer inflow excluding events'
           HisIdSeries (6) = 'DWF sewer inflow excluding events'
           LongHisIdSeries (1) = 'Total inflow sewer'
           LongHisIdSeries (2) = 'Storm water sewer inflow'
           LongHisIdSeries (3) = 'Dry weather sewer inflow'
           LongHisIdSeries (4) = 'Total inflow sewer excluding events'
           LongHisIdSeries (5) = 'Storm water flow sewer inflow excluding events'
           LongHisIdSeries (6) = 'Dry weather flow sewer inflow excluding events'
           HisLocation  = 'Total NWRW system'
! Construct T0 string
           iYear = ConfArr_get_IYEAR()
           iMo   = ConfArr_get_iMonth()
           iDay  = ConfArr_get_IDAY()
           iHour = ConfArr_get_IHour()
           iMin  = ConfArr_get_IMinute()
           iSec  = ConfArr_get_ISecond()
           TmSize= timeSettings%timestepSize
! include T0 string
           String = ' '
           Call WriteT0String (String(1:160), IYear,Imo,Iday,Ihour,Imin,Isec,TmSize)
           outName = Conffil_get_Namfil(116)
           runId  = ' '
           runId(1) = CASENM(1:)
           runId(3) = 'TITLE: RR Water balance             '
           RunId(4) = String(121:)

           success = Dh_AllocInit (MaxNwrwTerms, ParNames, ' ')
           success = Dh_AllocInit (MaxNwrwTerms, LongParNames, ' ')
!           Allocate ( ParNames(MaxNwrwTerms), Stat=Allocation_Error )
           If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', &
                                                   ' WriteHdrBalanceHisFile')
           ParNames = HisIdSeries(1:MaxNwrwTerms)
           LongParNames = LongHisIdSeries(1:MaxNwrwTerms)
           DataSetHisNWRWSystem = DioPltDefine(outName, runId, Dio_Plt_Real, ParNames, HisLocation)
! NetCdf output
           if (GenerateNetCdfOutput) then
               ! NetCdf file name is now constructed from his outputfilename, replace .his with .nc
               NetCdfOutputFileName = Conffil_get_Namfil(116)
               Ipos1 = FndFrst('.his', NetCdfOutputFileName, .false.)
               Ipos1 = max (ipos1, FndFrst('.His', NetCdfOutputFileName(1:), .false.) )
               Ipos1 = max (ipos1, FndFrst('.HIS', NetCdfOutputFileName(1:), .false.) )
               if (ipos1 .le. 0) then
                  call ErrMsgStandard(972, 0, ' Error in setting Netcdf output file name for balance output',' BalanceModule_WriteHdrBalanceHisFile')
               endif
               NetCdfOutputFileName(ipos1:) = ''
               NetCdfOutputFileName(ipos1:) = '.nc'
               ! create NetCdf file
               INetCdfFile(NWRWSysNetCdfFileNr) = nc_create (NetCdfOutputFileName)

               nitem = 1  ! 1 location
               locationid(1:) = HisLocation(1)(1:40)
               refdate = iYear * 10000 + Imo * 100 + IDay
               reftime = ihour * 10000 + Imin * 100 + Isec
               call nc_prepare(iNetCdfFile(NWRWSysNetCdfFileNr), nitem, '', loc_dimid(NWRWSysNetCdfFileNr), time_dimid(NWRWSysNetCdfFileNr), refdate, reftime, time_varid(NWRWSysNetCdfFileNr),locationid, .true.)
               id_locdim = loc_dimid(NWRWSysNetCdfFileNr)
               id_timedim = time_dimid(NWRWSysNetCdfFileNr)
               id_strlendim = 40
!              write(*,*) ' Nc dimensions '
!              write(*,*) ' id_locdim', id_locdim
!              write(*,*) ' id_timedim', id_timedim
!              write(*,*) ' id_strlendim', id_strlendim

               unit = ' m3'
               Do jpar=1,6
                  VariableName = Parnames(jpar)(1:40)
                  LongVariableName = LongParnames(jpar)(1:40)
                  ! Variable name may only contain letters, cijfers and underscores (so no ()*&^%$#@!?><=+-;:.,space)
                  VariableName = NetCdfName(VariableName)
                  id_vars(NWRWSysNetCdfFileNr,jpar) = HisSetupVariable (INetCdfFile(NWRWSysNetCdfFileNr), VariableName(1:len_trim(VariableName)), nf90_double, (/ id_locdim, id_timedim /), &
                                                        VariableName(1:len_trim(VariableName)), LongVariableName(1:len_trim(LongVariableName)),unit, TimeSeries=TimeSeriesVar)
!                 ierr = nf90_put_att(ncid, id_vars(jpar),   'cf_role', 'timeseries_id')
!                 Call NetCdfCheck('NC_Prepare - After put_att cf_role',ierr)
               Enddo
               ierr = nf90_enddef(INetCdfFile(NWRWSysNetCdfFileNr))
               Call NetCdfCheck(' BalanceModule after Enddef NWRWSys',ierr)

           endif
!
           Deallocate (ParNames)
           Deallocate (LongParNames)
        Endif

    Return

  END subroutine WriteHdrBalanceHisFile




  Subroutine Write3BBalanceHisfile (Itmstp, LastTm)

! writes output 3B Balance HIS file, only for simulation of 1 event
! Itmstp = timestep

! for netCdf
  use netCdfData
  use readlib
  use NetCdf

  use ParallelData, only : JulianStartSimulation, JulianNowSimulation, IDateAct, iTimeAct

  implicit none

   Double precision Julian

   Integer  Itmstp, Lasttm

   Logical OutputNow
   Integer Ihelp, i, itmstp1
   Logical Success
   Real    Balance_Error, Plv_Balance_error, Total_error
   Real, Pointer :: DioResult(:,:)

! NetCdf
   Character(Len=20)  :: DateString
   Character(Len=10)  :: CurrentDate, CurrentTime
   Integer               IYear, IMo, IDay, IHour, IMin, ISec

   OutputNow = .false.
   ihelp = ITmstp / OutputAtTimestep
   if (itmstp .eq. 0) then
      OutputNow=.true.
   elseif (ihelp*OutputAtTimestep .eq. Itmstp) then
      OutputNow=.true.
   elseif (itmstp .eq. lasttm) then
      OutputNow=.true.
   endif

   Balance_error = 0.0
   Plv_Balance_error = 0.0
   Total_error = 0.0

! Determine Balance_errors
! RR polder + Sacramento
   If (NcNode .gt. NcPluv) then
     Balance_error = Bal3B(2) + Bal3B(7) + Bal3b(8) + Bal3B(14) + Bal3B(15)+ Bal3B(16) + Bal3B(25) + Bal3B(28)
     DO i=3,6
       Balance_error = Balance_error - BAL3B(i)
     enddo
     DO i=9,13
        Balance_error = Balance_error - BAL3B(i)
     enddo
     Balance_error = Balance_error - BAL3B(17)    ! storage changes RWZI
     DO i=18,21        ! Sacramento outflows and storage change
        Balance_error = Balance_error - BAL3B(i)
     enddo
     Balance_error = Balance_error - BAL3B(23)  ! storage change routing links
     Balance_error = Balance_error - BAL3B(24)  ! seepage/DWF cells
     Balance_error = Balance_error - BAL3B(26)  ! storage change cells
!     Balance_error = Balance_error - BAL3B(27)  ! outflow cells
     Balance_error = Balance_error - BAL3B(29)  ! storage change RR Runoff
     Balance_error = Balance_error - BAL3B(30)  ! evap RRRunoff (HBV)
   Endif
! NWRW
   If (NcPluv .gt. 0) Plv_Balance_error = PLVBAL(2) - PLVBAL(3) - PLVBAL(4) - PLVBAL(5) - PLVBAL(6) - PLVBAL(7)
! Total
   Total_error = Balance_error + Plv_Balance_error

   if (OutputNow) then
       success = DH_AllocInit (NBalSeries, 1, DioResult, 0E0)
!      Allocate ( DioResult(NBalSeries,1), Stat=Allocation_Error )
      If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', &
                                              ' Write3BBalanceHisFile')
      If (NBalSeries .ge. 26) then
          DioResult( 1,1)  = Sngl (Bal3B( 2))
          DioResult( 2,1)  = Sngl (Bal3B( 3))
          DioResult( 3,1)  = Sngl (Bal3B( 4))
          DioResult( 4,1)  = Sngl (Bal3B( 5))
          DioResult( 5,1)  = Sngl (Bal3B( 6))
          DioResult( 6,1)  = Sngl (Bal3B(18))
          DioResult( 7,1)  = Sngl (Bal3B(19))
          DioResult( 8,1)  = Sngl (Bal3B(15))
          DioResult( 9,1)  = Sngl (Bal3B(16))
          DioResult(10,1)  = Sngl (Bal3B( 7))
          DioResult(11,1)  = Sngl (Bal3B( 8))
          DioResult(12,1)  = Sngl (Bal3B( 9))
          DioResult(13,1)  = Sngl (Bal3B(10))
          DioResult(14,1)  = Sngl (Bal3B(11))
          DioResult(15,1)  = Sngl (Bal3B(12))
          DioResult(16,1)  = Sngl (Bal3B(17))
          DioResult(17,1)  = Sngl (Bal3B(20))
          DioResult(18,1)  = Sngl (Bal3B(21))
          DioResult(19,1)  = Sngl (Bal3B(23))
          DioResult(20,1)  = Sngl (Bal3B(24))
          DioResult(21,1)  = Sngl (Bal3B(25))
          DioResult(22,1)  = Sngl (Bal3B(26))
          DioResult(23,1)  = Sngl (Bal3B(27))
          DioResult(24,1)  = Sngl (Bal3B(13))
          DioResult(25,1)  = Sngl (Bal3B(14))
          DioResult(26,1)  = Sngl (Bal3B(28))
          DioResult(27,1)  = Sngl (Bal3B(30))
          DioResult(28,1)  = Sngl (Bal3B(29))
          DioResult(29,1)  = Balance_Error
      Endif
      If (NBalSeries .ge. 35) then
          DioResult(30,1)  = Sngl (PlvBal(2))
          DioResult(31,1)  = Sngl (PlvBal(3))
          DioResult(32,1)  = Sngl (PlvBal(4))
          DioResult(33,1)  = Sngl (PlvBal(5))
          DioResult(34,1)  = Sngl (PlvBal(6))
          DioResult(35,1)  = Sngl (PlvBal(7))
          DioResult(36,1)  = Sngl (PlvBal(8))
          DioResult(37,1)  = Sngl (PlvBal(9))
          DioResult(38,1)  = Sngl (PlvBal(10))
          DioResult(39,1)  = Plv_Balance_Error
          DioResult(40,1)  = Total_Error
          if (NBalSeries .eq. 41) DioResult(41,1) = Sngl(Bal3B(22))   ! ARS 15475
      Elseif (NBalSeries .eq. 29) then
!          nothing special
      ElseIf (NBalSeries .eq. 10) then
          DioResult(1,1)  = Sngl (PlvBal(2))
          DioResult(2,1)  = Sngl (PlvBal(3))
          DioResult(3,1)  = Sngl (PlvBal(4))
          DioResult(4,1)  = Sngl (PlvBal(5))
          DioResult(5,1)  = Sngl (PlvBal(6))
          DioResult(6,1)  = Sngl (PlvBal(7))
          DioResult(7,1)  = Sngl (PlvBal(8))
          DioResult(8,1)  = Sngl (PlvBal(9))
          DioResult(9,1)  = Sngl (PlvBal(10))
          DioResult(10,1)  = Plv_Balance_Error
      Endif
      if (GenerateHisOutput) Call DioPltPut (DataSetHisRRBalance, Itmstp, DioResult )
      if (GenerateNetCdfOutput) then
          itmstp1 = ihelp + 1
         ! output at timestep itmstep,
          iYear = ConfArr_get_IYEAR()
          iMo   = ConfArr_get_iMonth()
          iDay  = ConfArr_get_IDAY()
          iHour = ConfArr_get_IHour()
          iMin  = ConfArr_get_IMinute()
          iSec  = ConfArr_get_ISecond()
          Write(CurrentDate,'(A4,A1,A2,A1,A2)') IntCh4(IYear), '-', IntCh2(IMo), '-', IntCh2(IDay)
          Write(CurrentTime,'(A2,A1,A2,A1,A2)') IntCh2(iHour),':',IntCh2(iMin),':', IntCh2(iSec)
          DateString = CurrentDate(1:10)//' '//CurrentTime(1:8)
!         write(*,*) ' DateString ',DateString(1:19)
!         write(*,*) ' Nc dimensions '
!         write(*,*) ' id_locdim', id_locdim
!         write(*,*) ' id_timedim', id_timedim
!         write(*,*) ' id_strlendim', id_strlendim
!         write(*,*) ' Current Timestep BalanceModule', itmstp1
!         write(*,*) ' JulianNowSimulation  ', JulianNowSimulation
!         write(*,*) ' JulianStartSimulation', JulianStartSimulation
! adjust JulianTime for NetCdfOutput
          IDateAct = ConfArr_get_IYEAR()*10000 + ConfArr_get_iMonth()* 100 + ConfArr_get_IDAY()
          ITimeAct = ConfArr_get_iHour()*10000 + ConfArr_get_iMinute()* 100 + ConfArr_get_iSecond()
          JulianNowSimulation = Julian(IdateAct, ITimeAct)
          if (itmstp .ge. 1) JulianNowSimulation = JulianNowSimulation + timeSettings%timestepSize /86400.D0
! end
!         write(*,*) ' JulianNowSimulation again ', JulianNowSimulation
!         DDays in seconds, minutes, hours or days
          if (trim(NetCdfTimestep) .eq. 'Seconds') then
             DDays = (JulianNowSimulation - JulianStartSimulation) * 86400.D0
          elseif (trim(NetCdfTimestep) .eq. 'Minutes') then
             DDays = (JulianNowSimulation - JulianStartSimulation) * 1440.D0
          elseif (trim(NetCdfTimestep) .eq. 'Hours') then
             DDays = (JulianNowSimulation - JulianStartSimulation) * 24.D0
          elseif (trim(NetCdfTimestep) .eq. 'Days') then
             DDays = (JulianNowSimulation - JulianStartSimulation) * 1.D0
          endif
          IDays = Nint(DDays)
!         write(*,*) ' Ddays            ', DDays
!         ierr = nf90_put_var(INetCdfFile(BalanceNetCdfFileNr), time_varid(BalanceNetCdfFileNr), DDays, (/ itmstp1 /))
          ierr = nf90_put_var(INetCdfFile(BalanceNetCdfFileNr), time_varid(BalanceNetCdfFileNr), IDays, (/ itmstp1 /))
          Call NetCdfCheck(' BalanceModule after Putvar  DDays',ierr)
          if (ierr .ne. nf90_noerr) then
!            write(*,*) ' time_varid', time_varid(BalanceNetCdfFileNr)
!            write(*,*) ' Error Putting timestep Ierr =', ierr, trim(nf90_strerror(ierr)), Ddays
             call ErrMsgStandard (999, 3, ' Error putting timestep data to NetCdf file RR-balance ', '')
          endif
! write output variables to NetCdf
          Do i=1,NBalSeries
!            write(*,*) ' Put variable ',i, id_vars(BalanceNetCdfFileNr,i), ' value        ',DioResult(i,1)
             ierr = nf90_put_var(INetCdfFile(BalanceNetCdfFileNr), id_vars(BalanceNetCdfFileNr,i), dble(DioResult(i,1)), (/ 1, itmstp1 /))
             Call NetCdfCheck(' BalanceModule after Putvar DioResult',ierr)
             if (ierr .ne. 0)  call ErrMsgStandard (999, 3, ' Error putting variable data to NetCdf file RR-balance ', '')
          Enddo
      Endif

       if (itmstp .eq. lastTm) ierr = nf90_close(iNetCdfFile(BalanceNetCdfFileNr))

      Deallocate ( DioResult)
   Endif
   MaxBalanceError = max (abs(Total_Error), MaxBalanceError)

   Return
   End subroutine Write3BBalanceHisFile



  Subroutine WriteNWRWSystemBalance (Itmstp, LastTm)

! writes output NWRW system balance, yearly output only

! for netCdf
  use netCdfData
  use readlib
  use NetCdf
  use ParallelData, only : JulianStartSimulation, JulianNowSimulation, IDateAct, iTimeAct

   implicit none

   Integer  Itmstp, LastTm, i
   Integer  IYear, IMo, IDay, IHour, IMin, ISec
   double precision  Julian

   Logical Success
   Real, Pointer :: DioResult(:,:)

       Success = DH_AllocInit (6,1, DioResult, 0E0)
!      Allocate ( DioResult(6,1), Stat=Allocation_Error )
      If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', &
                                              ' WriteNWRWSystemBalance')
          DioResult(6,1)  = Sngl (PlvBal(9)+PlvBal(8)-PlvBalHistory(3)-PlvBalEvents(3))
          DioResult(5,1)  = Sngl (PlvBal(7)-PlvBalHistory(2)-PlvBalEvents(2))
          DioResult(4,1)  = Sngl (PlvBal(10)-PlvBalHistory(1)-PlvBalEvents(1))
          DioResult(3,1)  = Sngl (PlvBal(9)+PlvBal(8)-PlvBalHistory(3))
          DioResult(2,1)  = Sngl (PlvBal(7)-PlvBalHistory(2))
          DioResult(1,1)  = Sngl (PlvBal(10)-PlvBalHistory(1))
          if (GenerateHisOutput) Call DioPltPut (DataSetHisNWRWSystem, Itmstp, DioResult )
          if (GenerateNetCdfOutput) then
              TimestepInNWRWSys = TimestepInNWRWSys + 1
             ! output at timestep itmstep,
              iYear = ConfArr_get_IYEAR()
              iMo   = ConfArr_get_iMonth()
              iDay  = ConfArr_get_IDAY()
              iHour = ConfArr_get_IHour()
              iMin  = ConfArr_get_IMinute()
              iSec  = ConfArr_get_ISecond()
    !          Write(CurrentDate,'(A4,A1,A2,A1,A2)') IntCh4(IYear), '-', IntCh2(IMo), '-', IntCh2(IDay)
    !          Write(CurrentTime,'(A2,A1,A2,A1,A2)') IntCh2(iHour),':',IntCh2(iMin),':', IntCh2(iSec)
    !          DateString = CurrentDate(1:10)//' '//CurrentTime(1:8)
    !         write(*,*) ' DateString ',DateString(1:19)
    !         write(*,*) ' Nc dimensions '
    !         write(*,*) ' id_locdim', id_locdim
    !         write(*,*) ' id_timedim', id_timedim
    !         write(*,*) ' id_strlendim', id_strlendim
    !         write(*,*) ' Current Timestep BalanceModule', itmstp1
    !         write(*,*) ' JulianNowSimulation  ', JulianNowSimulation
    !         write(*,*) ' JulianStartSimulation', JulianStartSimulation
    !         JulianTime for NetCdfOutput
              IDateAct = ConfArr_get_IYEAR()*10000 + ConfArr_get_iMonth()* 100 + ConfArr_get_IDAY()
              ITimeAct = ConfArr_get_iHour()*10000 + ConfArr_get_iMinute()* 100 + ConfArr_get_iSecond()
              JulianNowSimulation = Julian(IdateAct, ITimeAct)
              if (itmstp .ge. 1) JulianNowSimulation = JulianNowSimulation + timeSettings%timestepSize /86400.D0
    !         write(*,*) ' JulianNowSimulation again ', JulianNowSimulation
!             DDays in seconds, minutes, hours or days
              if (trim(NetCdfTimestep) .eq. 'Seconds') then
                 DDays = (JulianNowSimulation - JulianStartSimulation) * 86400.D0
              elseif (trim(NetCdfTimestep) .eq. 'Minutes') then
                 DDays = (JulianNowSimulation - JulianStartSimulation) * 1440.D0
              elseif (trim(NetCdfTimestep) .eq. 'Hours') then
                 DDays = (JulianNowSimulation - JulianStartSimulation) * 24.D0
              elseif (trim(NetCdfTimestep) .eq. 'Days') then
                 DDays = (JulianNowSimulation - JulianStartSimulation) * 1.D0
              endif
              IDays = Nint(DDays)
    !         write(*,*) ' Ddays            ', DDays
    !         write(*,*) ' NWRWSys timestep ', TimestepInNWRWSys
!              ierr = nf90_put_var(INetCdfFile(NWRWSysNetCdfFileNr), time_varid(NWRWSysNetCdfFileNr), DDays, (/ TimestepInNWRWSys /))
              ierr = nf90_put_var(INetCdfFile(NWRWSysNetCdfFileNr), time_varid(NWRWSysNetCdfFileNr), IDays, (/ TimestepInNWRWSys /))
              Call NetCdfCheck(' BalanceModule after Putvar  NWRWSys DDays',ierr)
              if (ierr .ne. nf90_noerr) then
    !            write(*,*) ' time_varid', time_varid(NWRWSysNetCdfFileNr)
    !            write(*,*) ' Error Putting timestep Ierr =', ierr, trim(nf90_strerror(ierr)), Ddays
                 call ErrMsgStandard (999, 3, ' Error putting timestep data to NetCdf file NWRW System balance ', '')
              endif
              Do i=1,6
    !            write(*,*) ' Put variable ',i, id_vars(NWRWSysNetCdfFileNr,i), ' value        ',DioResult(i,1)
                 ierr = nf90_put_var(INetCdfFile(NWRWSysNetCdfFileNr), id_vars(NWRWSysNetCdfFileNr,i), dble(DioResult(i,1)), (/ 1, TimestepInNWRWSys /))
                 Call NetCdfCheck(' BalanceModule after Putvar DioResult',ierr)
                 if (ierr .ne. 0)  call ErrMsgStandard (999, 3, ' Error putting variable data to NetCdf file NWRW System Balance ', '')
              Enddo
              if (itmstp .eq. lastTm) ierr = nf90_close(iNetCdfFile(NWRWSysNetCdfFileNr))
          endif
          Deallocate ( DioResult)
          PlvBalEvents(3)  = 0
          PlvBalEvents(2)  = 0
          PlvBalEvents(1)  = 0
          PlvBalHistory(3) = PlvBal(8) + PlvBal(9)
          PlvBalHistory(2) = PlvBal(7)
          PlvBalHistory(1) = PlvBal(10)
   Return
   End subroutine WriteNWRWSystemBalance

    !> If success, function returns Values array of length ElementCount
    !! for balance data per node elementset on specific quantity handle
    function RR_GetNodeBalanceDataByIntId(QuantityHandle, ElementCount, Values) result(success)

    use RR_open_mi_support
    use wl_open_mi_support

    implicit none

    ! return value
    logical  :: success

    ! arguments
    integer,          intent(in)      :: QuantityHandle   !< quant. handle
    integer,          intent(in)      :: ElementCount     !< #elems in balance per node elementset
    double precision, intent(out), &
            dimension(1:ElementCount) :: Values           !< values in balance per node elemenset

    ! locals

    Values  = 0
    success = .true.

    select case (QuantityHandle)
    ! Subdivide for various variables
    case(RRiTotalInAtNode_m3)
    !RR Total inflow on nodes in m3/s
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NNOD > 0) then
                Values(1:NNOD) = RSLMAP8_bal(1, 1:NNOD, 1)
            else
                success = .false.
            endif
    case(RRiTotalInViaLinks_m3)
    !RR Total inflow via links in m3/s
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NNOD > 0) then
                Values(1:NNOD) = RSLMAP8_bal(2, 1:NNOD, 1)
            else
                success = .false.
            endif
    case (RRiTotalOutAtNode_m3)
    !RR Total outflow on nodes in m3/s
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NNOD > 0) then
                Values(1:NNOD) = RSLMAP8_bal(3, 1:NNOD, 1)
            else
                success = .false.
            endif
    case(RRiTotalOutViaLinks_m3)
    !RR Total outflow via links in m3/s
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NNOD > 0) then
                Values(1:NNOD) = RSLMAP8_bal(4, 1:NNOD, 1)
            else
                success = .false.
            endif
    case(RRiDeltaStorage_m3)
    !RR storage difference in m3/s
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NNOD > 0) then
                Values(1:NNOD) = RSLMAP8_bal(5, 1:NNOD, 1)
            else
                success = .false.
            endif
    case(RRiBalanceError_m3)
    !RR balance error in m3/s
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NNOD > 0) then
                Values(1:NNOD) = RSLMAP8_bal(6, 1:NNOD, 1)
            else
                success = .false.
            endif
    case(RRiCumInAtNode_m3)
    !RR Cumulative inflow at nodes in m3/s
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NNOD > 0) then
                Values(1:NNOD) = RSLMAP8_bal(7, 1:NNOD, 1)
            else
                success = .false.
            endif
    case(RRiCumInViaLinks_m3)
    !RR Cumulative inflow via links in m3/s
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NNOD > 0) then
                Values(1:NNOD) = RSLMAP8_bal(8, 1:NNOD, 1)
            else
                success = .false.
            endif
    case(RRiCumOutAtNode_m3)
    !RR Cumulative outflow at nodes in m3/s
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NNOD > 0) then
                Values(1:NNOD) = RSLMAP8_bal(9, 1:NNOD, 1)
            else
                success = .false.
            endif
    case(RRiCumOutViaLinks_m3)
    !RR Cumulative outflow via links in m3/s
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NNOD > 0) then
                Values(1:NNOD) = RSLMAP8_bal(10, 1:NNOD, 1)
            else
                success = .false.
            endif
    case(RRiCumDeltaStorage_m3)
    !RR Cumulative storage difference in m3/s
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NNOD > 0) then
                Values(1:NNOD) = RSLMAP8_bal(11, 1:NNOD, 1)
            else
                success = .false.
            endif
    case(RRiCumBalanceError_m3)
    !RR Cumulative balance error
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NNOD > 0) then
                Values(1:NNOD) = RSLMAP8_bal(12, 1:NNOD, 1)
            else
                success = .false.
            endif
    case default
    ! Something is wrong
        success = .false.
    end select

    end function

    !> If success, function returns Values array of length ElementCount
    !! for balance data of whole model elementset on specific quantity handle
    function RR_GetBalanceDataByIntId(QuantityHandle, ElementCount, Values) result(success)

    use RR_open_mi_support
    use wl_open_mi_support

    implicit none

    ! return value
    logical  :: success

    ! arguments
    integer,          intent(in)      :: QuantityHandle   !< quant. handle
    integer,          intent(in)      :: ElementCount     !< #elems in balance elementset, should be 1
    double precision, intent(out)     :: Values           !< values in balance elemenset

    ! locals

    Values  = 0
    success = .true.

    select case (QuantityHandle)
    ! Subdivide for various variables
    case(RRiRainfall)
    !RR Total Rainfall in m3
        ! If a stack overflow occurs due to this array operation, use a do loop
        Values = Bal3B(2)
    case(RRiEvaporationPaved)
    !RR Total evaporation on paved in m3
        ! If a stack overflow occurs due to this array operation, use a do loop
        Values = Bal3B(3)
    case (RRiEvaporationUnpaved)
    !RR Total evaporation on unpaved in m3
        ! If a stack overflow occurs due to this array operation, use a do loop
        Values = Bal3B(4)
    case(RRiWaterUse)
    !RR Total use of Greenhouses in m3
        ! If a stack overflow occurs due to this array operation, use a do loop
        Values = Bal3B(5)
    case(RRiDWFPaved)
    !RR Total DWF on paved in m3
        ! If a stack overflow occurs due to this array operation, use a do loop
        Values = Bal3B(15)
    case(RRiNetSeepageUnpaved)
    !RR Total net seepage on unpaved in m3
        ! If a stack overflow occurs due to this array operation, use a do loop
        Values = Bal3B(7)
    case(RRiStoragePaved)
    !RR Total storage change on paved in m3
        ! If a stack overflow occurs due to this array operation, use a do loop
        Values = Bal3B(9)
    case(RRiStorageUnpaved)
    !RR Total storage change on unpaved in m3
        ! If a stack overflow occurs due to this array operation, use a do loop
        Values = Bal3B(10)
    case(RRiStorageGreenhouses)
    !RR Total storage change on greenhouses in m3
        ! If a stack overflow occurs due to this array operation, use a do loop
        Values = Bal3B(11)
    case(RRiStorageWWTP)
    !RR Total storage on wwtp
        ! If a stack overflow occurs due to this array operation, use a do loop
        Values = Bal3B(17)
    case(RRiBoundariesOut)
    !RR Total outflow on boundaries in m3
        ! If a stack overflow occurs due to this array operation, use a do loop
        Values = Bal3B(13)
    case(RRiBoundariesIn)
    !RR Total inflow on boundaries
        ! If a stack overflow occurs due to this array operation, use a do loop
        Values = Bal3B(14)
    case(RRiExternalInflowRRRunoff)
    !RR Total external inflow on runoff nodes in m3
        ! If a stack overflow occurs due to this array operation, use a do loop
        Values = Bal3B(28)
    case(RRiStorageChangeRRRunoff)
    !RR Total change in storage on runoff nodes in m3
        ! If a stack overflow occurs due to this array operation, use a do loop
        Values = Bal3B(29)
    case(RRiBalanceError_m3)
    !RR Total balance error in m3
        ! If a stack overflow occurs due to this array operation, use a do loop
        Values = Balance_error
    case default
    ! Something is wrong
        success = .false.
    end select

    end function

end Module Balance
