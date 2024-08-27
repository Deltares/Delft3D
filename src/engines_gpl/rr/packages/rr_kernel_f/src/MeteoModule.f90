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

 ! Last changed
! by:               $Author:: Schrier           $
! at:               $Modtime:: 18-08-97 5:14p   $
!
! current revision: $Revision:: 7               $



module RR_Meteo

  use Conf_arr
  use Conf_Fil
  use Network
  use DH_Alloc
  use ReadLib
  use NewTables
!
  use netcdf
  use nctimeseries

  !use Messages

  implicit none

  Integer        RainfallNcid, EvapNcid, TemperatureNcid
  INTEGER        PrecipitationTimesteps, PrecipitationSeriesNr, NrPrecipitationStations, PrecipitationTimeNr
  INTEGER        EvaporationTimesteps, EvaporationSeriesNr, NrEvaporationStations, EvaporationTimeNr
  INTEGER        TemperatureTimesteps, TemperatureSeriesNr, NrTemperatureStations, TemperatureTimeNr
  Double Precision PrecipJulianStartDate, PrecipJulianEndDate
  Double Precision EvapJulianStartDate, EvapJulianEndDate
  Double Precision TemperatureJulianStartDate, TemperatureJulianEndDate

  real(kind=8), dimension(:), allocatable :: date_arr
  real(kind=8), dimension(:), allocatable :: data_arr
  Character(Len=CharIdLength)             :: PrecipitationTimeUnitString, EvaporationTimeUnitString, TemperatureTimeUnitString


  REAL, Pointer, SAVE ::  EVAP(:)
  REAL, Pointer, SAVE ::  RAIN(:), Runoff(:), InputTemperature(:), AAFNodeRainfall(:)
  ! *** NODMET      = meteostation
  ! *** NODEVAP     = evap station (if also from rainfall file)
  ! *** NODFXS / FXG= FXS / FXG station (if also from rainfall file)
  ! *** AAFNodeRainfall = Area Adjustment Factor onde node rainfall
  Integer, Pointer, save :: NODMET(:), NodRunoff(:), NodTemperature(:), NODEVAP(:)
  Integer, Pointer, save :: NODFXS(:), NodFXG(:)
  Integer, Pointer, save :: EvapTable(:)
  Integer, Pointer, save :: EvapTimestep(:)

  Integer iMet, ncStat, NcStatRunoff, NcStatTemperature
  Integer NrSecsRai, NrSecsEvap, NrSecsRunoff, nrSecsTemperature

! ivm ARS 2978 een variabele EvaporationYear om aan te geven hoe met initialisatie om te gaan (welk jaar te nemen?)
  Integer EvaporationYear


  Character(Len=CharIdLength), Pointer, save :: namMet(:), NamEvap(:), NamRunoff(:), NamTemperature(:)
  Character(Len=CharIdLength), Pointer, save :: namFXS(:), NamFXG(:)
  Character(Len=CharIdLength), Pointer :: StationName(:)
  Character(Len=CharIdLength), Pointer :: StationNameRunoff(:)
  Character(Len=CharIdLength), Pointer :: StationNameTemperature(:)

contains


  Subroutine Meteo_confAr1

    implicit none
    ! variables
    Integer ncNode, nMet, fileHandle
    Logical Success

    !meteostation

    ncNode = Network_get_nrNodes()
    ncMet = Network_get_nrMeteo()

    NMET = MAX (1, NCMET )

    fileHandle = ConfFil_get_iOut1()
    If ( (NMET .GT. 0) .and. (fileHandle .gt. 0) ) then
      Write(fileHandle, *) ' Meteostations         =',NMET
    endif

    !*** Meteo data
   ! NB RAIN, EVAP met dimensie NMET

    Success = DH_AllocInit (NMet, Rain, Evap, 0E0)
    Success = Success .and. DH_AllocInit (NMet, Runoff, 0E0)
    Success = Success .and. DH_AllocInit (NMet, InputTemperature, 0E0)
    Success = Success .and. DH_AllocInit (NcNode, AAFNodeRainfall, 1E0)  ! default value 1.0 area adjustment factor JIRA 20542
    Success = Success .and. DH_AllocInit (NcNode, NodMet, 0)
    Success = Success .and. DH_AllocInit (NcNode, NodEvap, 0)
    Success = Success .and. DH_AllocInit (NcNode, NodFXS , 0)
    Success = Success .and. DH_AllocInit (NcNode, NodFXG , 0)
    Success = Success .and. DH_AllocInit (NcNode, NodRunoff, 0)
    Success = Success .and. DH_AllocInit (NcNode, NodTemperature, 0)
    Success = Success .and. DH_AllocInit (NcNode, NamMet, '')
    Success = Success .and. DH_AllocInit (NcNode, NamEvap, '')
    Success = Success .and. DH_AllocInit (NcNode, NamFXS , '')
    Success = Success .and. DH_AllocInit (NcNode, NamFXG , '')
    Success = Success .and. DH_AllocInit (NcNode, NamRunoff, '')
    Success = Success .and. DH_AllocInit (NcNode, NamTemperature, '')
    If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', &
                                           ' Meteo_ConfAr1')

  End subroutine Meteo_confAr1



  subroutine Meteo_readBinInput (DummyRunoffCompOption, DummyNCRunoff, MeteoNetcdfInput)

    implicit none

  ! variables
    Integer DummyNcRunoff
    Integer DummyRunoffCompOption(DummyNcRunoff)
    Logical MeteoNetcdfInput

    Integer inRain, teller, dummy
    Character(Len=CharIdLength), Pointer:: string(:)
    Integer ncNode, Ncstru
    Logical endFil, success

    ncNode = Network_get_NrNodes()
    ncStru = Network_get_nrStruc()
    ncMet = Network_get_nrMeteo()
    ncBoun = Network_get_nrBoun()
    ncRwzi = Network_get_nrRwzi()
    ncIndus = Network_get_nrIndus()
    ! body

    call Meteo_convertName2Id (MeteoNetcdfInput)
    nodRunoff = 1
    if (ncRRRunoffExternal .gt. 0) call Meteo_convertName2IdRunoff(DummyRunoffCompOption, DummyNCRunoff, MeteoNetCdfInput)
    nodTemperature = 1
    if (NcRRRunoffHBV .gt. 0) call Meteo_convertName2IdTemperature(DummyRunoffCompOption, DummyNcRunoff, MeteoNetCdfInput)

    ! from file 13 (rainfall file) read nevent after
    ! 2 integers are read
    CALL OPENFL(INRAIN, ConfFil_get_namFil(13),1,1)
    CALL SKPCOM (INRAIN, ENDFIL, 'ODS ')
    READ(INRAIN,*) dummy
    CALL SKPCOM (INRAIN, ENDFIL, 'ODS ')
    READ(INRAIN,*) dummy
    CALL SKPCOM (INRAIN, ENDFIL, 'ODS ')

    Success = DH_AllocInit (NCMet, String, ' ')
!   allocate(string(ncMet), Stat=Allocation_Error )
    If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Meteo_ReadBinInput')

    if (.not. MeteoNetCdfInput) then
       do teller = 1, ncMet
         READ(INRAIN,'(A)') string(teller)
       end do
       close(inRain)
    endif

  End subroutine Meteo_readBinInput





  SUBROUTINE RDEVAP (nrStations, makelogfile, IEvent, EvapTmstp)
    ! *********************************************************************
    ! *** Last update: March  1995       By : Geert Prinsen
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***   Read evaporation file
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  IDEBUG = file unit number of debug file
    ! ***  IOUT1  = file unit number of output file with messages
    ! ***  EvapTmstp = Evaporation timestep (used to be DayInEvent, but from NetCdf other than daily timesteps are possible)
    ! *********************************************************************

    INTEGER       iCount, iECode, i, nrStations, ncMet, iDeflt, iDebug, IEvent, EvapTmstp
    CHARACTER(Len=CharIdLength) STRING
    Integer       makelogfile, NrSecs ! IMonth, IDay

    String = ' '
    ncMet = Network_get_nrMeteo()
    iDeflt = Network_get_Default()
    iDebug = ConfFil_get_iDebug()

    IF (iDebug /= 0) WRITE (IDEBUG,1)
  1 FORMAT (' RDEVAP')
    ICOUNT=0

    ! *********************************************************************
    ! *** read data, convert from mm/day to m/s.
    ! *********************************************************************

! Always from already read EvapData array
       if (idebug .ne. 0) write(Idebug,*) ' RdEvap Ievent EvapTmstp', IEvent, EvapTmstp
       DO I=1,nrStations
          EVAP(I) = max(0.0, Evapdata(Ievent,i, EvapTmstp))  !8-11-2004: Evap should be >= 0
       ENDDO

    NrSecs = ConfArr_get_nrSEvap()
    DO I=1,nrStations
       EVAP(I) = EVAP(I) * MM2M/NRSecs
    ENDDO

    if (ncMet .gt. nrStations) then
      if (makelogfile .gt. 0)  call ErrMsgStandard(974, 0, 'More meteostations for rainfall than evaporation', '')
      do I = nrStations + 1, ncMet
         EVAP(I) = EVAP(1)
      enddo
    endif

    GOTO 999

    ! *********************************************************************
    ! *** Error during reading of file
    ! *********************************************************************

150 CONTINUE
    call ErrMsgStandard (902, IECODE, '  Rdevap', STRING)

    ! *********************************************************************
    ! *** end of file
    ! *********************************************************************

 30 CONTINUE
    call ErrMsgStandard (911, IECODE, '  Rdevap', STRING)

999 CONTINUE

    RETURN
  END subroutine rdevap


  SUBROUTINE RdEvap2 (NrEvapStations)
    ! *********************************************************************
    ! *** Last update:  January 2006
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    Find evaporation values from time table; Evaporation file format2
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  NrEvapStations = number of evaporation stations
    ! *********************************************************************

    INTEGER Imeteo, rowNr, TabelNr, Idebug, Iout1, NrEvapStations, FirstIndx
    REAL    EvapValue

    type (Date) currentDate
    type (Time) currentTime
    Logical DateTimeOutsideTable

    IOut1  = ConfFil_get_IOut1()
    Idebug = ConfFil_get_IDEBUG()

    currentDate%year = ConfArr_get_IYear()
    currentDate%month = ConfArr_get_iMonth()
    currentDate%day = ConfArr_get_iDay()
    currentTime%hour = ConfArr_get_iHour()
    currentTime%minute = ConfArr_get_iMinute()
    currentTime%second = 0

    FirstIndx = NrEvapStations+1
    Do iMeteo = 1, NrEvapStations
      TabelNr = EvapTable (imeteo)
      If (TabelNr .gt. 0) then
         RowNr = -1
         EvapValue = GetNewValue(TableHandle, TabelNr, 1, RowNr, CurrentDate, CurrentTime, &
                                 Idebug, iout1, DateTimeOutsideTable, .true.)
         if (idebug .ne. 0) WRITE (IDEBUG,*) ' nieuwe methode imeteo, rowNr EvapValue', imeteo, RowNr, EvapValue
         Evap(Imeteo) = max(0.0, EvapValue * mm2m / EvapTimestep(imeteo))  !Evap should be >= 0  and in m/s
         FirstIndx = min (FirstIndx, imeteo)
      Else
         Evap(Imeteo) = -999.99
      Endif
    Enddo
    If (FirstIndx .gt. NrEvapStations)  call ErrMsgStandard (972, 0, ' No correct first evaporation station', ' ')

    Do iMeteo = 1, NcMet
       if (Evap(Imeteo) .le.  -999.) Evap(Imeteo) = Evap(FirstIndx)
    Enddo
!   fill complete array, missing stations with Evap(1)
    If (NcMet .gt. NrEvapStations) then
       Do iMeteo = NrEvapStations+1, NcMet
          Evap(Imeteo) = Evap(FirstIndx)
       Enddo
    EndIf

    RETURN
  END subroutine RdEvap2



  SUBROUTINE RDRAIN (IEvent, BuiTmstp)
    ! *********************************************************************
    ! *** Last update: March  1995       By : Geert Prinsen
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***   Read rainfall file
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  IDEBUG = file unit number of debug file
    ! ***  Ievent = bui nummer
    ! ***  BuiTmstp = tijdstap in bui
    ! *********************************************************************

    INTEGER       iECode, I, iDebug, Ievent, BuiTmstp, Nrsecs
    CHARACTER(Len=CharIdLength) STRING

    String = ' '
    iDebug = ConfFil_get_iDebug()
    IF (iDebug /= 0) WRITE (IDEBUG,1)
  1 FORMAT (' RDRAIN')

! Always from already read Buidata array
      DO I=1, Network_get_nrMeteo()
         RAIN(I) = max(0.0, BuiData (Ievent, i, BuiTmstp))   ! 8-11-2004: Rain should be >= 0
      ENDDO

! Convert from mm/timestep to m/s.
    NrSecs = ConfArr_get_nrSRai()
    RAIN = RAIN * MM2M / NrSecs

    IF (iDebug /= 0) WRITE(IDEBUG,*) ' Rainfall read in m/s', &
                                         (RAIN(I),I=1, Network_get_nrMeteo())
    GOTO 999

    ! *********************************************************************
    ! *** Error during reading of file
    ! *********************************************************************

150 CONTINUE
    call ErrMsgStandard (902, IECODE, '  Rdrain', STRING)

    ! *********************************************************************
    ! *** end of file
    ! *********************************************************************

 30 CONTINUE
    call ErrMsgStandard (911, IECODE, '  Rdrain', STRING)

999 CONTINUE
    RETURN
  END subroutine RdRain



  real function Meteo_getRain(stationCode)

    integer stationCode

    Meteo_getRain = RAIN(stationCode)
  end function Meteo_getRain



  real function Meteo_getEvap(stationCode)

    integer stationCode

    Meteo_getEvap = EVAP(stationCode)
  end function Meteo_getEvap



  subroutine Meteo_convertName2Id (MeteoNetCdfInput)
    ! open rainfall-file
    ! search for name of meteostation
    ! ID = number of the line the name occurs in
    ! ID's are stored in the NODMET-array

    ! variables
    Logical        MeteoNetcdfInput
    Logical        endFil
    Character(Len=CharIdLength), Pointer :: namstat(:)
    Integer      , Pointer :: Lenstat(:)
    Character(CharIdLength)  namnodmet, NodeId
    Integer        inRain, nrStations
    Integer        ilen1, ilen2, ikind, teller, teller2
    Integer        dummy, ncNode, icount
    Character(CharIdLength) string  !, deletespaces
    Logical        found, success
    Integer        RetVal

    ! body

    RetVal = 0
    ncNode = Network_get_nrNodes()
! UNST-5103
    If (MeteoNetCdfInput) then
       ! todo get ncid
!      success = success .and. GetNcTimeDimension(Rainfallncid, 'precipitation', PrecipitationTimesteps, PrecipitationTimeNr, PrecipitationTimeUnitString)
!      success = success .and. GetNcSeriesid (Rainfallncid, 'precipitation', PrecipitationSeriesNr, PrecipitationTimeNr)
!      nrStations = 1 ! TODO from Netcdf
!      success = success .and. GetNcLocationDimension (Rainfallncid, 'precipitation', PrecipitationSeriesNr, NrStations)

       NrStations = NrPrecipitationStations
       success = DH_AllocInit (NrStations, StationName, NamStat, '')
       success = success .and. DH_AllocInit (NrStations, LenStat, 0)

       success = success .and. GetNcLocationIds (Rainfallncid, 'precipitation', PrecipitationSeriesId, PrecipitationSeriesNr, NrStations, StationName)
       do teller = 1, nrStations
         do teller2 = 1, len(StationName(teller))
            if (StationName(teller)(teller2:teller2) .eq. '' .or. StationName(teller)(teller2:teller2) .eq. '' &
                      .or. StationName(teller)(teller2:teller2) .eq. char(0)) then
                StationName(teller)(teller2:teller2) = ' '
            end if
         end do
       end do
       do teller = 1, nrStations
         Namstat(teller) = stationName(teller)
         call upperc (namstat(teller))
       end do
    else
       call openfl(inRain, ConfFil_get_namFil(13),1,1)
       rewind(inRain)
       call skpcom(inRain, endFil, 'ODS ')
       read(inRain, *) dummy
       read(inRain, '(A17)') string
       call upperc(string)
       string = deleteSpaces(string)

       if (string(1:15) == "*AANTALSTATIONS") then
         read(inRain, *) nrStations
         success = DH_AllocInit (NrStations, StationName, NamStat, '')
         success = success .and. DH_AllocInit (NrStations, LenStat, 0)
         If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Meteo_ConvertName2Id')
         call skpcom(inRain, endFil, 'ODS ')

         do teller = 1, nrStations
           read(inRain, *) stationName(teller)  ! namen van stations
           Lenstat(teller) = max (1, len_trim (stationName(teller)) )
           Namstat(teller) = stationName(teller)
           call upperc (namstat(teller))
         end do

       else
         ! foutmelding
         call ErrMsgStandard (972, 0, ' Meteofile lacks stationnames.',  '(Hint, add: "* aantal stations" and stationnames)')
       endif
       close(inRain)
    endif
! end UNST-5103

!   Write (*,*) ' MeteoStations id from NetCdf or bui file (in upper case)'
!   Do teller = 1, nrStations
!      Write(*,*)  teller, NamStat(teller)(1:len_trim(NamStat(teller)))
!   Enddo

!   Write (*,*) ' MeteoStations id (in upper case) required'
!   Do teller = 1, ncNode
!      Write(*,*)  teller, NamMet(teller) (1:Len_trim(NamMet(teller)))
!   Enddo

      ! wijs de waarde van nodmet toe op basis van nammet
      if (nrStations .eq. 1) then
        do teller=1,ncNode
           NodMet(teller) = 1
        enddo
        call ErrMsgStandard (975, 0, ' ', ' ')
      else
        icount = 0
        do teller = 1, ncNode
! not for structures, boundaries, RWZI's, industry, RRconnection and bifurcation nodes
          ikind = EiNode(teller, 3)
          if (ikind .ne. 5 .and. ikind .ne. 6 .and. ikind .ne. 14 .and. ikind .ne. 15 .and. ikind .ne. 30  .and. ikind .ne. 32) then
            ilen1 = max (1, len_trim (namMet(teller)) )
            namnodmet=''
            namnodmet = namMet (teller) (1:ilen1)
            call upperc (namnodmet(1:ilen1))
            teller2 = 0
            found = .false.
            do while (.not. found .and. teller2 .lt. nrStations)
              teller2 = teller2 + 1
              ilen2 = max (1, len_trim (namstat(teller2)) )
              if (namnodmet(1:ilen1) .eq. namstat(teller2)(1:ilen2) )  then
                  nodMet(teller) = teller2
                  found = .true.
              endif
            enddo
            if (.not. found) then
              icount = icount + 1
              NodeId = Id_Nod(teller)
              if (icount .le. 10) call ErrMsgStandard (914, 0, NodeId(1:len_trim(NodeId)) , namMet(teller))
              nodMet(teller) = 1
              ! the first meteostation is taken instead of the meteostation which could not be found
            endif
          end if
        enddo
        if (icount .gt. 10) call ErrMsgStandard (914, 0, ' Many more meteostations not found in rainfallfile. ', '')
        if (NCRRRunoffExternal .gt. 0 .and. icount .gt. 0) call ErrMsgStandard (9140, 0, ' ', '')
      endif


      ! Zelfde, maar nu voor NodEvap op basis van NAMEVAP (mits gedefinieerd)
      RetVal = 0
      if (nrStations .eq. 1) then
        do teller=1,ncNode
! default nul ipv 1
           if (NamEvap(teller) .ne. '') NodEvap(teller) = 0
           if (NamFXS (teller) .ne. '') NodFXS (teller) = 0
           if (NamFXG (teller) .ne. '') NodFXG (teller) = 0
        enddo
        call ErrMsgStandard (975, 0, ' ', ' ')
      else
        icount = 0
        do teller = 1, ncNode
! not for structures, boundaries, RWZI's, industry, RRconnection and bifurcation nodes
           ! NodEvap
          ikind = EiNode(teller, 3)
          if (ikind .ne. 5 .and. ikind .ne. 6 .and. ikind .ne. 14 .and. ikind .ne. 15 .and. ikind .ne. 30  .and. ikind .ne. 32) then
            ilen1 = max (1, len_trim (namEvap(teller)) )
            namnodmet=''
            namnodmet = namEvap (teller) (1:ilen1)
            call upperc (namnodmet(1:ilen1))
            teller2 = 0
            found = .false.
            do while (.not. found .and. teller2 .lt. nrStations .and. namnodmet .ne. '')
              teller2 = teller2 + 1
              if (namnodmet .eq. namstat(teller2) )  then
                  nodEvap(teller) = teller2
                  found = .true.
              endif
            enddo
            if (.not. found .and. NamEvap(teller) .ne. '') then
              RetVal = 99
              icount = icount + 1
              NodeId = Id_Nod(teller)
              if (icount .le. 10) call ErrMsgStandard (914, 0, NodeId(1:len_trim(NodeId)) , namEvap(teller))
              nodEvap(teller) = 1
              ! the first meteostation is taken instead of the meteostation which could not be found
              call ErrMsgStandard (977, 0, ' For Node ', NodeId(1:len_trim(NodeId)))
              call ErrMsgStandard (977, 0, ' Specified evaporation station does not exist in BUI file. Correct your input, station:', NamEvap(teller))
            endif
           ! NodFXS
           ! same for FXS (Walrus)
            ilen1 = max (1, len_trim (namFXS(teller)) )
            namnodmet=''
            namnodmet = namFXS (teller) (1:ilen1)
            call upperc (namnodmet(1:ilen1))
            teller2 = 0
            found = .false.
            do while (.not. found .and. teller2 .lt. nrStations .and. namnodmet .ne. '')
              teller2 = teller2 + 1
              if (namnodmet .eq. namstat(teller2) )  then
                  nodFXS (teller) = teller2
                  found = .true.
              endif
            enddo
            if (.not. found .and. NamFXS (teller) .ne. '') then
              RetVal = 99
              icount = icount + 1
              NodeId = Id_Nod(teller)
              if (icount .le. 10) call ErrMsgStandard (914, 0, NodeId(1:len_trim(NodeId)) , namFXS (teller))
              nodFXS (teller) = 0
              call ErrMsgStandard (977, 0, ' For Node ', NodeId(1:len_trim(NodeId)))
              call ErrMsgStandard (977, 0, ' Specified FXS station does not exist in BUI file. Correct your input, station: ', NamFXS(teller))
              ! leave NodFxS at zero and use zero values
            endif
           ! NodFXG
           ! same for FXG (Walrus)
            ilen1 = max (1, len_trim (namFXG(teller)) )
            namnodmet=''
            namnodmet = namFXG (teller) (1:ilen1)
            call upperc (namnodmet(1:ilen1))
            teller2 = 0
            found = .false.
            do while (.not. found .and. teller2 .lt. nrStations .and. namnodmet .ne. '')
              teller2 = teller2 + 1
              if (namnodmet .eq. namstat(teller2) )  then
                  nodFXG (teller) = teller2
                  found = .true.
              endif
            enddo
            if (.not. found .and. NamFXG (teller) .ne. '') then
              RetVal = 99
              icount = icount + 1
              NodeId = Id_Nod(teller)
              if (icount .le. 10) call ErrMsgStandard (914, 0, NodeId(1:len_trim(NodeId)) , namFXG (teller))
              nodFXG (teller) = 0
              call ErrMsgStandard (977, 0, ' For Node ', NodeId(1:len_trim(NodeId)))
              call ErrMsgStandard (977, 0, ' Specified FXG station does not exist in BUI file. Correct your input, station: ', NamFXG(teller))
              ! leave NodFxG at zero and use zero values
            endif

          end if
        enddo
        if (icount .gt. 10) call ErrMsgStandard (914, 0, ' Many more evaporation stations not found in rainfallfile. ', '')
        if (NCRRRunoffExternal .gt. 0 .and. icount .gt. 0) call ErrMsgStandard (9140, 0, ' ', '')
      endif

      If (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Some Walrus evaporation, FXS and/or FXG stations missing in rainfall file', '')

  end subroutine Meteo_convertName2Id





! Similar routine as Meteo_convertName2ID, but now for runoff stations

  subroutine Meteo_convertName2IdRunoff (DummyRunoffCompOption, DummyNcRunoff, MeteoNetCdfInput)


    ! open runoff file
    ! search for name of runoffstation
    ! ID = number of the line the name occurs in
    ! ID's are stored in the NODRunoff-array

    ! variables
    Integer DummyNcRunoff
    Integer DummyRunoffCompOption(DummyNcRunoff)
    Logical MeteoNetcdfInput

    Logical endFil
    Character(Len=CharIdLength), Pointer :: namstatRunoff(:)
    Integer      , Pointer :: LenstatRunoff(:)
    Character(CharIdLength)  namnodrunoff, NodeId
    Integer        inRunoff, nrStationsRunoff
    Integer        ilen1, ikind, teller, teller2, IRRRunoff
    Integer        dummy, ncNode, icount
    Character(CharIdLength) string  !, deletespaces
    Logical        found, success

    ! body

    ncNode = Network_get_nrNodes()

! not yet for Runoff timeseries, only for Rainfall/Evap/Temp
! UNST-5103
!    If (MeteoNetCdfInput) then
!       nrStationsRunoff =
!       success = DH_AllocInit (NrStationsRunoff, StationNameRunoff, NamStatRunoff, '')
!       success = success .and. DH_AllocInit (NrStationsRunoff, LenStatRunoff, 0)
!
!    else

       call openfl(inRunoff, ConfFil_get_namFil(80),1,1)
       rewind(inRunoff)
       call skpcom(inRunoff, endFil, 'ODS ')
       read(inRunoff, *) dummy
       read(inRunoff, '(A17)') string
       call upperc(string)
       string = deleteSpaces(string)

       if (string(1:15) == "*AANTALSTATIONS" .or. string(1:16) == "*AANTAL STATIONS") then
         read(inRunoff, *) nrStationsRunoff
         success = DH_AllocInit (NrStationsRunoff, StationNameRunoff, NamStatRunoff, '')
         success = success .and. DH_AllocInit (NrStationsRunoff, LenStatRunoff, 0)
         If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Meteo_ConvertName2IdRunoff')
         call skpcom(inRunoff, endFil, 'ODS ')

         do teller = 1, nrStationsRunoff
           read(inRunoff, *) stationNameRunoff(teller)  ! namen van stations
           LenstatRunoff(teller) = max (1, len_trim (stationNameRunoff(teller)) )
           NamstatRunoff(teller) = stationNameRunoff(teller)
           call upperc (namstatRunoff(teller))
         end do

       else
         ! foutmelding
         call ErrMsgStandard (972, 0, ' Runofffile lacks stationnames.',  '(Hint, add: "* aantal stations" and stationnames)')
       endif
       close(inRunoff)
!    endif
! end UNST-5103

      ! wijs de waarde van nodmet toe op basis van nammet
      if (nrStationsRunoff .eq. 1) then
        do teller=1,ncNode
           NodRunoff(teller) = 1
        enddo
        call ErrMsgStandard (975, 0, ' ', ' ')
      else
        icount = 0
        do teller = 1, ncNode
          ikind = EiNode(teller, 3)
          iRRRunoff = EiNode(teller, 2)
!         External runoff timeserie = 18
          if (ikind .eq. 18) then
           if (DummyRunoffCompOption(iRRRunoff) .eq. 0) then
            ilen1 = max (1, len_trim (namRunoff(teller)) )
            namnodRunoff=''
            namnodRunoff = namRunoff (teller) (1:ilen1)
            call upperc (namnodRunoff(1:ilen1))
            teller2 = 0
            found = .false.
            do while (.not. found .and. teller2 .lt. nrStationsRunoff)
              teller2 = teller2 + 1
              if (namnodRunoff .eq. namstatRunoff(teller2) )  then
                  nodRunoff(teller) = teller2
                  found = .true.
              endif
            enddo
            if (.not. found) then
              icount = icount + 1
              NodeId = Id_Nod(teller)
              if (icount .le. 10) call ErrMsgStandard (9141, 0, NodeId(1:len_trim(NodeId)) , namRunoff(teller))
              nodRunoff(teller) = 1
              ! the first runoffstation is taken instead of the meteostation which could not be found
            endif
           endif
          endif
        enddo
        if (icount .gt. 10) call ErrMsgStandard (9141, 0, ' Many more runoffstations not found in runofffile. ', '')
      endif


  end subroutine Meteo_convertName2IdRunoff


! Similar routine as Meteo_convertName2ID, but now for Temperature stations

  subroutine Meteo_convertName2IdTemperature (DummyRunoffCompOption, DummyNcRunoff, MeteoNetCdfInput)

    ! open Temperature file
    ! search for name of Temperaturestation
    ! ID = number of the line the name occurs in
    ! ID's are stored in the NODTemperature-array

    ! variables
    Logical MeteoNetcdfInput
    Integer DummyNcRunoff
    Integer DummyRunoffCompOption(DummyNcRunoff)
    Logical endFil
    Character(Len=CharIdLength), Pointer :: namstatTemperature(:)
    Integer      , Pointer :: LenstatTemperature(:)
    Character(CharIdLength)  namnodTemperature, NodeId
    Integer        inTemperature, nrStationsTemperature
    Integer        ilen1, ikind, teller, teller2, IRRRunoff
    Integer        dummy, ncNode, icount
    Character(CharIdLength) string  !, deletespaces
    Logical        found, success

    ! body

    ncNode = Network_get_nrNodes()

! UNST-5103
    If (MeteoNetCdfInput) then

       NrStationsTemperature = NrTemperatureStations
       success = DH_AllocInit (NrStationsTemperature, StationNameTemperature, NamStatTemperature, '')
       success = success .and. DH_AllocInit (NrStationsTemperature, LenStatTemperature, 0)

       success = success .and. GetNcLocationIds (Rainfallncid, 'temperature', TemperatureSeriesId, TemperatureSeriesNr, NrStationsTemperature, StationNameTemperature)
       do teller = 1, nrStationsTemperature
         do teller2 = 1, len(StationNameTemperature(teller))
            if (StationNameTemperature(teller)(teller2:teller2) .eq. '' .or. StationNameTemperature(teller)(teller2:teller2) .eq. '' &
                      .or. StationNameTemperature(teller)(teller2:teller2) .eq. char(0)) then
                StationNameTemperature(teller)(teller2:teller2) = ' '
            end if
         end do
       end do
       do teller = 1, nrStationsTemperature
         NamstatTemperature(teller) = stationNameTemperature(teller)
         call upperc (namstatTemperature(teller))
       end do

    else
       call openfl(inTemperature, ConfFil_get_namFil(79),1,1)
       rewind(inTemperature)
       call skpcom(inTemperature, endFil, 'ODS ')
       read(inTemperature, *) dummy
       read(inTemperature, '(A17)') string
       call upperc(string)
       string = deleteSpaces(string)

       if (string(1:15) == "*AANTALSTATIONS" .or. string(1:16) == "*AANTAL STATIONS") then
         read(inTemperature, *) nrStationsTemperature
         success = DH_AllocInit (NrStationsTemperature, StationNameTemperature, NamStatTemperature, '')
         success = success .and. DH_AllocInit (NrStationsTemperature, LenStatTemperature, 0)
         If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Meteo_ConvertName2IdTemperature')
         call skpcom(inTemperature, endFil, 'ODS ')

         do teller = 1, nrStationsTemperature
           read(inTemperature, *) stationNameTemperature(teller)  ! namen van stations
           LenstatTemperature(teller) = max (1, len_trim (stationNameTemperature(teller)) )
           NamstatTemperature(teller) = stationNameTemperature(teller)
           call upperc (namstatTemperature(teller))
         end do

       else
         ! foutmelding
         call ErrMsgStandard (972, 0, ' Temperaturefile lacks stationnames.',  '(Hint, add: "* aantal stations" and stationnames)')
       endif
       close(inTemperature)
    endif
! end UNST-5103

!    Write (*,*) ' Temperature Stations id from NetCdf or tmp file (in upper case)'
!    Do teller = 1, nrStationsTemperature
!       Write(*,*)  teller, NamStatTemperature(teller) (1:len_trim(NamStatTemperature(teller)))
!    Enddo

!    Write (*,*) ' TemperatureStations id (in upper case) required'
!    Do teller = 1, ncNode
!       Write(*,*)  teller, NamTemperature(teller)(1:len_trim(NamTemperature(teller)))
!    Enddo


      ! wijs de waarde van nodmet toe op basis van nammet
      if (nrStationsTemperature .eq. 1) then
        do teller=1,ncNode
           NodTemperature(teller) = 1
        enddo
        call ErrMsgStandard (975, 0, ' ', ' ')
      else
        icount = 0
        do teller = 1, ncNode
          ikind = EiNode(teller, 3)
          iRRRunoff = EiNode(teller, 2)
!         if (ikind .eq. 18 .and. DummyRunoffCompOption(iRRRunoff) .eq. 1) then
!         HBV = 19
          if (ikind .eq. 19) then
           if (DummyRunoffCompOption(iRRRunoff) .eq. 1) then
            ilen1 = max (1, len_trim (namTemperature(teller)) )
            namnodTemperature=''
            namnodTemperature = namTemperature (teller) (1:ilen1)
            call upperc (namnodTemperature(1:ilen1))
            teller2 = 0
            found = .false.
            do while (.not. found .and. teller2 .lt. nrStationsTemperature)
              teller2 = teller2 + 1
              if (namnodTemperature .eq. namstatTemperature(teller2) )  then
                  nodTemperature(teller) = teller2
                  found = .true.
              endif
            enddo
            if (.not. found) then
              icount = icount + 1
              NodeId = Id_Nod(teller)
              if (icount .le. 10) call ErrMsgStandard (9142, 0, NodeId(1:len_trim(NodeId)) , namTemperature(teller))
              nodTemperature(teller) = 1
              ! the first Temperaturestation is taken instead of the meteostation which could not be found
            endif
           endif
          endif
        enddo
        if (icount .gt. 10) call ErrMsgStandard (9142, 0, ' Many more Temperaturestations not found in Temperaturefile. ', '')
      endif

  end subroutine Meteo_convertName2IdTemperature



  Subroutine ReadEvapFile (INEVAP, nrStations, EvapFormat2)
    ! *******op************************************************************
    ! *** Last update: March  2000       By : Sobek-parallel
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***   Read evaporation data for all events; store in array EvapData
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  IDEBUG = file unit number of debug file
    ! ***  INEVAP = file unit number of input file
    ! ***  IOUT1  = file unit number of output file with messages
    ! *********************************************************************

    INTEGER        INEVAP, iCount, iECode, i, nrStations, ncMet, iDeflt, iDebug, Iout1
    LOGICAL        ENDFIL, LeapYear, YearNul, Doorgaan, EvapFormat2, Allow, Found
    CHARACTER(Len=1000)         STRING
    CHARACTER(Len=CharIdLength) TableName, CDum
    Integer        iYear, idum, Idum1, Idum2, Idum3, NrTested, NrWrong
    Integer        Ievent, DayInEvent, IMeteo, FixedTimestep, Isuccess
    Real           FixedTimestepSize, RDum

! table
    Integer        NrColumns, TableNr
    Logical        success, occurs, TabYesNo, Err969
! end table

    String = ' '
    ncMet = Network_get_nrMeteo()
    iDeflt = Network_get_Default()
    iDebug = ConfFil_get_iDebug()
    IOut1  = ConfFil_get_IOut1()
    YearNul = .false.

    IF (iDebug /= 0) WRITE (IDEBUG,1) IDeflt
  1 FORMAT (' ReadEvapFile  IDeflt=',I3)

    ! *********************************************************************
    ! *** read data
    ! *********************************************************************

    Allow = .false.
    Found = .false.

    If (EvapFormat2) then
!      New format, time table
       Success = Success .and. DH_AllocInit (NcMet, EvapTable, 0)
       Success = Success .and. DH_AllocInit (NcMet, EvapTimestep, 0)
       Endfil = .false.
       Call SkpCom (InEvap, Endfil, 'ODS')
       Do while (.not. endfil)
         READ(InEvap,'(A1000)',END=211,ERR=151,IOSTAT=IECODE) STRING
         If (String(1:4) .eq. 'EVAP') then
            backspace(InEvap)
            success = GetRecord(InEvap, 'EVAP', Endfil, Idebug, Iout1)    ! get record van keyword EVAP to evap
            IF (.not. success) goto 211
            IF (Endfil) GOTO 211
            Success = GetTableName (TabYesNo, TableName, ' id ', Iout1)     ! get table name via keyword ' id ', TabYesNo=TBLE found
            IF (.not. success) goto 211
            ISuccess = GetVAR2 (STRING,' eq ',3,' ReadEvap',' Read Evaporation file',IOUT1, &
                                CDUM, RDUM, FixedTimestep, ALLOW, FOUND, IflRtn)
            if (ISuccess .gt. 0) call ErrMsgStandard(972, 0, ' Error reading Evaporation data ', ' Error getting timestep')
            if (Abs (FixedTimestep -1) .gt. 1E-6) then
               call ErrMsgStandard (969, 0, ' At present only fixed timestepsize in evaporation table supported', TableName)
               Err969 = .true.
            else
               ISuccess = GetVAR2 (STRING,' ts ',2,' ReadEvap',' Read evaporation file',IOUT1, &
                                   CDUM, FixedTimestepSize, IDUM, ALLOW, FOUND, IflRtn)
               if (ISuccess .gt. 0) call ErrMsgStandard(972, 0, ' Error reading Evaporation data ', ' Error getting timestep')
            Endif
            If (TabYesNo .and. TableName .ne. '') Then
!              Er is een tabel gedefinieerd, met een niet-lege naam
               NrColumns = 1
               Imeteo = FindString (NcMet, StationName, TableName, NcMet, CaseSensitive)
               Occurs = (Imeteo .gt. 0)
               if (Imeteo .gt. 0) then
                  if (EvapTable(imeteo) .gt. 0) then
                    call SetMessage(LEVEL_ERROR, 'Evaporation table definition '//Tablename(1:len_trim(TableName))//' double in Evaporation datafile')
                  endif
               endif
!              Verwerken evaporation time table
               if (occurs .and. NrColumns .gt. 0) then
!                 Get table with name TableName, 1 column data fields, result in global arrays; tabel nummer is TableNr
                  Success = GetTable (TableHandle, TableName, NrColumns, TableNr, idebug, Iout1)
                  IF (.not. success) goto 211
                  if (idebug .ne. 0) write(idebugLunRR,*) ' Read evap table ',TableName(1:32), ' Table index ', TableNr
                  EvapTable(imeteo) = TableNr
                  EvapTimestep(iMeteo) = FixedTimestepSize
               endif
            Endif
         Endif
       Enddo
 151   Continue
       call ErrMsgStandard (902, IECODE, '  ReadEvapFile', STRING)
 211   Continue
       If (Err969) call ErrMsgStandard (972, 0, ' Only fixed timestepsize evaporation supported', ' ')

       Err969 = .false.
       NrWrong = 0
       NrTested= 0
       Do iMeteo = 1, ncMet
          NrTested = NrTested + 1
          if (EvapTable(imeteo) .eq. 0)  Then
             nrWrong = NrWrong + 1
             Err969 = .true.
             call ErrMsgStandard (969, 0, ' Some MeteoStations not present in evaporation file', StationName(imeteo))
          Endif
       Enddo
       If (NrTested .eq. NrWrong) then
          call ErrMsgStandard (972, 0, ' All meteoStations not present in evaporation file', StationName(imeteo))
       Endif

    Else
!      Old format, always daily values in mm/day
       Do IEvent=1,NEvent
         if (idebug .ne. 0) then
            write(Idebug,*) ' Event - Duration in days ReadEvapFile', Ievent, EventDuration(Ievent,5)
            write(IDEBUG,*) ' IDeflt ', IDeflt
         Endif
         ICOUNT=0
         If (IDefLt .eq. 1) then
            Rewind(InEvap)
            Call SplFil (Inevap, ' Evaporation file')
         Endif
         Do i=1,6
            EvStrt(i) = EventStartDateTime(Ievent,i)
         Enddo
         String = ' '
         String = ' Evaporation file'
         Read(Inevap,*,End=11,Err=11) idum
         Goto 12
      11 Continue
         if (Yearnul) then
            Rewind(Inevap)
            CALL SKPCOM (INEVAP, ENDFIL, 'ODS ')
            if (Endfil) goto 30
            Read(Inevap,*,End=30) idum
         endif
      12 Continue
         Backspace(Inevap)

         Call SplFl3(InEvap,IEvent, EvaporationYear, String, YearNul)
         Do DayInEvent=1, EventDuration(Ievent,5)
           Doorgaan = .true.
           Do While (Doorgaan)
              CALL SKPCOM (INEVAP, ENDFIL, 'ODS ')
              STRING = ' evaporation file'
              IF (ENDFIL .AND. IDEFLT .EQ. 0) THEN
                If (YearNul) then
                  REWIND(INEVAP)
                  Doorgaan = .true.
                Else
                  call ErrMsgStandard (911, 0, '  ReadEvapFile', STRING)
                Endif
              ELSE
!               allow rewinding of evaporation file only at and of year!
                Doorgaan = .false.
                IF (ENDFIL .AND. IDEFLT .EQ. 1 .AND. &
                      idum2 .eq. 12 .and. idum3 .eq. 31 .and. icount .eq. 0) THEN
                  ICOUNT = ICOUNT +1
                  REWIND(INEVAP)
                  Doorgaan = .true.
                  If (YearNul) Icount = 0  ! ARS 15794
                ENDIF
              ENDIF
           Enddo

           READ(INEVAP,*,END=30,ERR=150,IOSTAT=IECODE)  IDUM1, IDUM2, IDUM3, (EVAP(I),I=1,nrStations)
           IF (iDebug /= 0) WRITE(IDEBUG,*) ' Evaporation read in mm/day', (EVAP(I),I=1,nrStations)

!    ARS 569: 29 feb always in the file; check on date always
!             means that 3 out of 4 years 29 feb has to be skipped;
           if (Idum2 .eq. 2 .and. idum3 .eq. 29) then
!            IYear = year read from evaporation file; either startyear event or Checkyear
             if (Ideflt .eq. 0 .or. EvaporationYear .eq. -1) then
!               IYear = EventStartDateTime(IEvent,1)
!    nav ARS 8845 continue bui van 96 jaar, niet Iyear=Startyear maar iYear=ReadYear
                IYear = Idum1
             elseIf (EvaporationYear .ne. -1) then
                IYear = EvaporationYear
             endif
             LeapYear = .false.
             If (IYear/4   *  4 .eq. IYear) LeapYear = .true.
             if (IYear/100 *100 .eq. IYear) LeapYear = .false.
             if (IYear/400 *400 .eq. IYear) LeapYear = .true.
             if (.NOT. LeapYear) &
               Read (InEvap,*,END=30,ERR=150,IOSTAT=IECODE) IDUM1,IDUM2,IDUM3, (EVAP(I),I=1,nrStations)
           endif
!    End check ARS 569
           DO I=1,nrStations
              EvapData(Ievent, i, DayInEvent) = Evap(i)
           ENDDO

           if (ncMet .gt. nrStations) then
             do I = nrStations + 1, ncMet
               EVAP(I) = EVAP(1)
             enddo
           endif
         Enddo
!    bij event mogelijk een dag teveel gelezen, backspace nodig als volgende event direct begint
!    nav. ARS 11511 extra backspace (want in ReadRainfallfile wordt NrDaysInEvent op ...+2 gezet , ipv ...+1
         BackSpace(Inevap)
       Enddo

       if (idebug .ne. 0) then
          DO IEvent=1,NEvent
            write(Idebug,*) ' EvapData station1 for Event',IEvent
            write(Idebug,*) (EvapData(Ievent,1,DayInEvent), DayInEvent=1,EventDuration(Ievent,5))
          ENDDO
       Endif
    Endif

    Return

    ! *********************************************************************
    ! *** Error during reading of file
    ! *********************************************************************

150 CONTINUE
    call ErrMsgStandard (902, IECODE, '  ReadEvapFile', STRING)

    ! *********************************************************************
    ! *** end of file
    ! *********************************************************************

 30 CONTINUE
    call ErrMsgStandard (911, IECODE, '  ReadEvapFile', STRING)

999 CONTINUE

    RETURN
  END subroutine ReadEvapFile


      Subroutine ReadRainfallFile (IDEBUG, IN, IOUT1, ICALL)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  Sobek-RR parallell version                  March 2000
! *********************************************************************
! *** Last update: June  1997        By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Inlezen bui data
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  IN     = file unit number of input file
! ***  IOUT1  = file unit number of output file with messages
! ***  ICall  = calling parameter
! ***            1 = call to determine number of events, max. nr. timesteps
! ***            2 = call to actually read the rainfall data
! *********************************************************************


      INTEGER      FRSTTM, LASTT, IDEBUG, IN,IOUT1,IECODE, &
                   IDEFLT, IDUM, IT, I, IEVENT, ITMSTP, ICall,&
                   ISecStart, ISecDuration   !, NrsecsRai
      Real         Rdum
      LOGICAL      ENDFIL
      CHARACTER(len=1) QUOTE
      CHARACTER(Len=CharIdLength) DummyName
      CHARACTER(len=500) STRING

      QUOTE = ''''
      iDebug = ConfFil_get_iDebug()
      if (idebug .ne. 0) WRITE (IDEBUG,1)
    1 FORMAT (' ReadRainfallFile')

! *********************************************************************
! *** skip header of file
! *********************************************************************

      Rewind(in)
      CALL SKPCOM (IN, ENDFIL, '3B')
      IF (ENDFIL)  call ErrMsgStandard (911, 0, 'Rdrain', ' Precipitation file')

! *********************************************************************
! *** Read bui file
! *********************************************************************

      CALL SKPCOM (IN, ENDFIL, '3B')
      READ(IN,*,END=991,ERR=991,IOSTAT=IECODE) IDEFLT
      CALL SKPCOM (IN, ENDFIL, '3B')
      READ(IN,*,END=991,ERR=981,IOSTAT=IECODE) NCSTAT
      CALL SKPCOM (IN, ENDFIL, '3B')

      DO IDUM=1,NCSTAT
        READ(IN,'(A)',END=991,IOSTAT=IECODE) STRING
        IF  (STRING(1:1) .NE. QUOTE) THEN
           call SetMessage(LEVEL_WARN, 'Rainfall file in old format?')
           GOTO 982
        ENDIF
        read(string,*) DummyName
      ENDDO

      GOTO 9900
  981 CONTINUE
      BACKSPACE(IN)
      GOTO 9900
  982 CONTINUE
 9900 CONTINUE
! nr. events, rainfall timestep size
      CALL SKPCOM (IN, ENDFIL, '3B')
      NRSecsRAI = 3600
      READ(IN,*,END=991,ERR=990,IOSTAT=IECODE) NEVENT, NRSecsRAI
      if (idebug .ne. 0) WRITE (IDEBUG,*) ' IDeflt    :',IDeflt
      if (idebug .ne. 0) WRITE (IDEBUG,*) ' Nr. events:',NEVENT,NRSecsRAI
      GOTO 9901
  990 CONTINUE
      BACKSPACE(IN)
 9901 CONTINUE

      If (Icall .eq. 1) then
         MaxTimesteps = 0
         MaxTimestepsEvap = 0
         MaxDays      = 0
      Endif

! *********************************************************************
! *** Read date/time record for each event
! *********************************************************************

      DO IEVENT=1,NEVENT

        CALL SKPCOM (IN, ENDFIL, '3B')
        IF (ENDFIL) call ErrMsgStandard (911, 0, '  Rdrain', STRING)

        STRING = ' rainfall file'
        READ(IN,*,END=991,ERR=150,IOSTAT=IECODE) &
                       (EventStartDateTime(IEVENT,IT),IT=1,6), &
                       (EventDuration(IEVENT,I),I=1,4)
        if (idebug .ne. 0) THEN
           WRITE(IDEBUG,*) ' Start   ',(EventStartDateTime(IEVENT,I),I=1,6)
           WRITE(IDEBUG,*) ' Duration',(EventDuration(IEVENT,I),I=1,4)
        ENDIF

! ***   Bepaal aantal tijdstappen per event

        FRSTTM = 1
! ARS 8845: bepaal aantal tijdstappen eerst via een real, dan omzetten naar integer
        Rdum   = EventDuration(IEVENT,1)*86400.E0  + EventDuration(IEVENT,2)*3600. + &
                 EventDuration(IEVENT,3)*60.     + EventDuration(IEVENT,4)
        LASTT  = Rdum / NRSecsRAI

! set number of rainfall timesteps, days, maximum over all events
        Rdum   = Float(LASTT) * Float(NRSecsRAI) / Float(NRSDAY)
! March 2005 and updated Nov 2008 Taiwan: now calculate precisely how many days are needed from EVP file
        EventDuration(Ievent,5) = Floor(Rdum)
        If (EventDuration(Ievent,5) .lt. Rdum) EventDuration(Ievent,5) = EventDuration(Ievent,5) + 1
        ISecStart = EventstartDateTime(Ievent,4)*3600 + EventstartDateTime(Ievent,5)*60 + EventstartDateTime(Ievent,6)
        ISecDuration = LastT * NrSecsRai - (Floor(Rdum)*NrsDay)
        If (ISecStart+ISecDuration .gt. NrsDay) then
!          event duration is not a whole nr. of days, so add 1 additional daily evaporation value is needed
           EventDuration(Ievent,5) = EventDuration(Ievent,5) + 1
        Elseif (EventStartDateTime(ievent,4) .ne. 0 .and. EventStartDateTime(ievent,4) .ne. 24 .and. EventDuration(ievent,5) .eq. RDum) then
!          event lastst whole number of days, but does not start at midnight
           EventDuration(Ievent,5) = EventDuration(Ievent,5) + 1
        Endif
! end March 2005

        EventDuration(Ievent,6) = LASTT +1

        If (Icall .eq. 1) then
          MaxDays      = Max (EventDuration(Ievent,5), MaxDays     )
          MaxTimesteps = Max (EventDuration(Ievent,6), MaxTimesteps)
          MaxTimestepsEvap = MaxTimesteps
        Endif

        if (idebug .ne. 0) Write(IDEBUG,*) ' Buidata bui ', IEVENT
        Do ITMSTP=FRSTTM, LASTT
          If (Icall .eq. 1) then
            Read(IN,*,END=991,ERR=150,IOSTAT=IECODE)  Rdum
          Else
            Read(IN,*,END=991,ERR=150,IOSTAT=IECODE) (BuiData(Ievent,I,ITMSTP),I=1,NCSTAT)
            if (idebug .ne. 0) Write(Idebug,*)   (BuiData(Ievent,I,ITMSTP),I=1,NCSTAT)
          Endif
        Enddo
        if (idebug .ne. 0) then
           write(Idebug,*) ' Event - Duration in days ', Ievent, EventDuration(Ievent,5)
           write(Idebug,*) ' Event - in Rainfall Timesteps ', Ievent, EventDuration(Ievent,6)
        Endif
      Enddo
!     write(*,*) ' MeteoModule maxTimesteps=', MaxTimesteps

      goto 999
!
! *********************************************************************
! *** Error during reading of file
! *********************************************************************
!
  991 CONTINUE
      call ErrMsgStandard (911, IECODE, 'Rdrain', ' neerslag file')
  150 CONTINUE
      call ErrMsgStandard (902, IECODE, 'Rdrain', ' neerslag file')

! *********************************************************************
! *** end of file
! *********************************************************************
!
  999 CONTINUE
      Close(in)

      RETURN
      End Subroutine ReadRainfallFile



      Subroutine ReadRunoffFile (IDEBUG, IN, IOUT1, ICALL)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  Sobek-RR parallell version                  March 2000
! *********************************************************************
! *** Last update: Nov 2007                          By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Inlezen runoff data
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  IN     = file unit number of input file
! ***  IOUT1  = file unit number of output file with messages
! ***  ICall  = calling parameter
! ***            1 = call to determine number of events, max. nr. timesteps
! ***            2 = call to actually read the runoff data
! *********************************************************************


      INTEGER      FRSTTM, LASTT, IDEBUG, IN,IOUT1,IECODE, &
                   IDEFLTRunoff, IDUM, IT, I, IEVENT, ITMSTP, ICall,&
                   ISecStart, ISecDuration
      Real         Rdum
      LOGICAL      ENDFIL
      CHARACTER(len=1) QUOTE
      CHARACTER(Len=CharIdLength) DummyName
      CHARACTER(len=500) STRING

      QUOTE = ''''
      iDebug = ConfFil_get_iDebug()
      if (idebug .ne. 0) WRITE (IDEBUG,1)
    1 FORMAT (' ReadRunoffFile')

! *********************************************************************
! *** skip header of file
! *********************************************************************

      Rewind(in)
      CALL SKPCOM (IN, ENDFIL, '3B')
      IF (ENDFIL)  call ErrMsgStandard (911, 0, 'ReadRunoffFile', ' Runoff file')

! *********************************************************************
! *** Read runoff file
! *********************************************************************

      CALL SKPCOM (IN, ENDFIL, '3B')
      READ(IN,*,END=991,ERR=991,IOSTAT=IECODE) IDEFLTRunoff
      CALL SKPCOM (IN, ENDFIL, '3B')
      READ(IN,*,END=991,ERR=991,IOSTAT=IECODE) NCStatRunoff
      CALL SKPCOM (IN, ENDFIL, '3B')

      DO IDUM=1,NCStatRunoff
        READ(IN,'(A)',END=991,IOSTAT=IECODE) STRING
        read(string,*) DummyName
      ENDDO

 9900 CONTINUE
! nr. events, runoff timestep size
      CALL SKPCOM (IN, ENDFIL, '3B')
      NRSecsRunoff = 3600
      READ(IN,*,END=991,ERR=990,IOSTAT=IECODE) NEVENTRunoff, NRSecsRunoff
      if (idebug .ne. 0) WRITE (IDEBUG,*) ' IDefltRunoff     :',IDefltRunoff
      if (idebug .ne. 0) WRITE (IDEBUG,*) ' Nr. events Runoff:',NEVENTRunoff,NRSecsRunoff
      GOTO 9901
  990 CONTINUE
      BACKSPACE(IN)
 9901 CONTINUE

      MaxTimestepsRunoff = 0
      MaxDaysRunoff      = 0

! *********************************************************************
! *** Read date/time record for each event
! *********************************************************************

      DO IEVENT=1,NEVENTRunoff

        CALL SKPCOM (IN, ENDFIL, '3B')
        IF (ENDFIL) call ErrMsgStandard (911, 0, '  ReadRunoffFile', STRING)

        STRING = ' runoff file'
        READ(IN,*,END=991,ERR=150,IOSTAT=IECODE) &
                       (EventStartDateTimeRunoff(IEVENT,IT),IT=1,6), &
                       (EventDurationRunoff(IEVENT,I),I=1,4)
        if (idebug .ne. 0) THEN
           WRITE(IDEBUG,*) ' Start   ',(EventStartDateTimeRunoff(IEVENT,I),I=1,6)
           WRITE(IDEBUG,*) ' Duration',(EventDurationRunoff(IEVENT,I),I=1,4)
        ENDIF

! ***   Bepaal aantal tijdstappen per event

        FRSTTM = 1
! ARS 8845: bepaal aantal tijdstappen eerst via een real, dan omzetten naar integer
        Rdum   = EventDurationRunoff(IEVENT,1)*86400.E0  + EventDurationRunoff(IEVENT,2)*3600.E0 + &
                 EventDurationRunoff(IEVENT,3)*60.E0     + EventDurationRunoff(IEVENT,4)
        LASTT  = Rdum / NRSecsRunoff

! set number of runoff timesteps, days, maximum over all events
        Rdum   = Float(LASTT) * Float(NRSecsRunoff) / Float(NRSDAY)
! March 2005: now calculate precisely how many days are needed from EVP file
        EventDurationRunoff(Ievent,5) = Floor(Rdum)
        If (EventDurationRunoff(Ievent,5) .lt. Rdum) EventDurationRunoff(Ievent,5) = EventDurationRunoff(Ievent,5) + 1
        ISecStart = EventstartDateTimeRunoff(Ievent,4)*3600 + EventstartDateTimeRunoff(Ievent,5)*60 + EventstartDateTimeRunoff(Ievent,6)
        ISecDuration = LastT * NrSecsRunoff - (Floor(Rdum)*NrsDay)
        If (ISecStart+ISecDuration .gt. NrsDay) then
!          event duration is not a whole nr. of days, so add 1 additional daily evaporation value is needed
           EventDurationRunoff(Ievent,5) = EventDurationRunoff(Ievent,5) + 1
        Elseif (EventStartDateTimeRunoff(ievent,4) .ne. 0 .and. EventStartDateTimeRunoff(ievent,4) .ne. 24) then
!          event lastst whole number of days, but does not start at midnight
           EventDurationRunoff(Ievent,5) = EventDurationRunoff(Ievent,5) + 1
        Endif
! end March 2005

        EventDurationRunoff(Ievent,6) = LASTT +1

        If (Icall .eq. 1) then
          MaxDaysRunoff      = Max (EventDurationRunoff(Ievent,5), MaxDaysRunoff     )
          MaxTimestepsRunoff = Max (EventDurationRunoff(Ievent,6), MaxTimestepsRunoff)
        Endif

        if (idebug .ne. 0) Write(IDEBUG,*) ' Runoff event ', IEVENT
        Do ITMSTP=FRSTTM, LASTT
          If (Icall .eq. 1) then
            Read(IN,*,END=991,ERR=150,IOSTAT=IECODE)  Rdum
          Else
            Read(IN,*,END=991,ERR=150,IOSTAT=IECODE) (RunoffData(Ievent,I,ITMSTP),I=1,NCSTATRunoff)
            if (idebug .ne. 0) Write(Idebug,*)   (RunoffData(Ievent,I,ITMSTP),I=1,NCSTATRunoff)
          Endif
        Enddo
        if (idebug .ne. 0) then
           write(Idebug,*) ' Event - RunoffDuration in days ', Ievent, EventDurationRunoff(Ievent,5)
           write(Idebug,*) ' Event - Runoff in Runoff Timesteps ', Ievent, EventDurationRunoff(Ievent,6)
        Endif
      Enddo

      goto 999
!
! *********************************************************************
! *** Error during reading of file
! *********************************************************************
!
  991 CONTINUE
      call ErrMsgStandard (911, IECODE, 'ReadRunoffFile', ' runoff file')
  150 CONTINUE
      call ErrMsgStandard (902, IECODE, 'ReadRunoffFile', ' runoff file')

! *********************************************************************
! *** end of file
! *********************************************************************
!
  999 CONTINUE
      Close(in)

      RETURN
      End Subroutine ReadRunoffFile



  SUBROUTINE RdRunoff (IEvent, RunoffTmstp)
    ! *********************************************************************
    ! *** Last update: November 2007                     By : Geert Prinsen
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***   Read rainfall file
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  IDEBUG = file unit number of debug file
    ! ***  Ievent = bui nummer
    ! *********************************************************************

    INTEGER       iECode, I, iDebug, Ievent, RunoffTmstp
    CHARACTER(Len=CharIdLength) STRING

    String = ' '
    iDebug = ConfFil_get_iDebug()
    IF (iDebug /= 0) WRITE (IDEBUG,1)
  1 FORMAT (' RdRunoff')

! Always from already read Runoffdata array
      DO I=1, NcStatRunoff
         Runoff(I) = max(0.0, Runoffdata (Ievent, i, RunoffTmstp))   ! assumed: Runoff should be >= 0
      ENDDO


    IF (iDebug /= 0) WRITE(IDEBUG,*) ' Runoff read in m3/s', &
                                         (Runoff(I),I=1, NcStatRunoff)
    GOTO 999

    ! *********************************************************************
    ! *** Error during reading of file
    ! *********************************************************************

150 CONTINUE
    call ErrMsgStandard (902, IECODE, '  RdRunoff', STRING)

    ! *********************************************************************
    ! *** end of file
    ! *********************************************************************

 30 CONTINUE
    call ErrMsgStandard (911, IECODE, '  RdRunoff', STRING)

999 CONTINUE
    RETURN
  END subroutine RdRunoff


  Subroutine ConvertBuiTimestepToRunoffTimestep (Ievent, BuiTmstp, RunoffTmstp)

  Integer Ievent, BuiTmstp, RunoffTmstp, T0shift

  ! use DateTimeStartEvent, DateTimeStartEventRunoff
   Integer           DateEventStart
   Integer           TimeEventStart
   Integer           DateEventStartRunoff
   Integer           TimeEventStartRunoff

   Double Precision  Julian
   Double Precision  JulianEventStart, JulianEventStartRunoff

    DateEventStart = EventStartDateTime(Ievent,1)*10000 + EventStartDateTime(Ievent,2)*100 + EventStartDateTime(Ievent,3)
    TimeEventStart = EventStartDateTime(Ievent,4)*10000 + EventStartDateTime(Ievent,5)*100 + EventStartDateTime(Ievent,6)
    JulianEventStart= Julian (DateEventStart  ,TimeEventStart )

    DateEventStartRunoff = EventStartDateTimeRunoff(Ievent,1)*10000 + EventStartDateTimeRunoff(Ievent,2)*100 + EventStartDateTime(Ievent,3)
    TimeEventStartRunoff = EventStartDateTimeRunoff(Ievent,4)*10000 + EventStartDateTimeRunoff(Ievent,5)*100 + EventStartDateTime(Ievent,6)
    JulianEventStartRunoff = Julian (DateEventStartRunoff, TimeEventStartRunoff )

    T0shift = (JulianEventStartRunoff - JulianEventStart) * Float(NrsDay) / NrSecsRunoff

    RunoffTmstp =  T0shift + ceiling ( BuiTmstp * (float(NrSecsRai) / float(NrSecsRunoff)))


  Return
  End subroutine ConvertBuiTimestepToRunoffTimestep



      Subroutine ReadTemperatureFile (IDEBUG, IN, IOUT1, ICALL)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  Sobek-RR parallell version                  March 2000
! *********************************************************************
! *** Last update: Nov 2007                          By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Inlezen Temperature data
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  IN     = file unit number of input file
! ***  IOUT1  = file unit number of output file with messages
! ***  ICall  = calling parameter
! ***            1 = call to determine number of events, max. nr. timesteps
! ***            2 = call to actually read the Temperature data
! *********************************************************************


      INTEGER      FRSTTM, LASTT, IDEBUG, IN,IOUT1,IECODE, &
                   IDEFLTTemperature, IDUM, IT, I, IEVENT, ITMSTP, ICall,&
                   ISecStart, ISecDuration
      Real         Rdum
      LOGICAL      ENDFIL
      CHARACTER(len=1) QUOTE
      CHARACTER(Len=CharIdLength) DummyName
      CHARACTER(len=500) STRING

      QUOTE = ''''
      iDebug = ConfFil_get_iDebug()
      if (idebug .ne. 0) WRITE (IDEBUG,1)
    1 FORMAT (' ReadTemperatureFile')

! *********************************************************************
! *** skip header of file
! *********************************************************************

      Rewind(in)
      CALL SKPCOM (IN, ENDFIL, '3B')
      IF (ENDFIL)  call ErrMsgStandard (911, 0, 'ReadTemperatureFile', ' Temperature file')

! *********************************************************************
! *** Read Temperature file
! *********************************************************************

      CALL SKPCOM (IN, ENDFIL, '3B')
      READ(IN,*,END=991,ERR=991,IOSTAT=IECODE) IDEFLTTemperature
      CALL SKPCOM (IN, ENDFIL, '3B')
      READ(IN,*,END=991,ERR=991,IOSTAT=IECODE) NCStatTemperature
      CALL SKPCOM (IN, ENDFIL, '3B')

      DO IDUM=1,NCStatTemperature
        READ(IN,'(A)',END=991,IOSTAT=IECODE) STRING
        read(string,*) DummyName
      ENDDO

 9900 CONTINUE
! nr. events, Temperature timestep size
      CALL SKPCOM (IN, ENDFIL, '3B')
      NRSecsTemperature = 3600
      READ(IN,*,END=991,ERR=990,IOSTAT=IECODE) NEVENTTemperature, NRSecsTemperature
      if (idebug .ne. 0) WRITE (IDEBUG,*) ' IDefltTemperature     :',IDefltTemperature
      if (idebug .ne. 0) WRITE (IDEBUG,*) ' Nr. events Temperature:',NEVENTTemperature,NRSecsTemperature
      GOTO 9901
  990 CONTINUE
      BACKSPACE(IN)
 9901 CONTINUE

      MaxTimestepsTemperature = 0
      MaxDaysTemperature      = 0

! *********************************************************************
! *** Read date/time record for each event
! *********************************************************************

      DO IEVENT=1,NEVENTTemperature

        CALL SKPCOM (IN, ENDFIL, '3B')
        IF (ENDFIL) call ErrMsgStandard (911, 0, '  ReadTemperatureFile', STRING)

        STRING = ' Temperature file'
        READ(IN,*,END=991,ERR=150,IOSTAT=IECODE) &
                       (EventStartDateTimeTemperature(IEVENT,IT),IT=1,6), &
                       (EventDurationTemperature(IEVENT,I),I=1,4)
        if (idebug .ne. 0) THEN
           WRITE(IDEBUG,*) ' Start   ',(EventStartDateTimeTemperature(IEVENT,I),I=1,6)
           WRITE(IDEBUG,*) ' Duration',(EventDurationTemperature(IEVENT,I),I=1,4)
        ENDIF

! ***   Bepaal aantal tijdstappen per event

        FRSTTM = 1
! ARS 8845: bepaal aantal tijdstappen eerst via een real, dan omzetten naar integer
        Rdum   = EventDurationTemperature(IEVENT,1)*86400.E0  + EventDurationTemperature(IEVENT,2)*3600.E0 + &
                 EventDurationTemperature(IEVENT,3)*60.     + EventDurationTemperature(IEVENT,4)
        LASTT  = Rdum / NRSecsTemperature

! set number of Temperature timesteps, days, maximum over all events
        Rdum   = Float(LASTT) * Float(NRSecsTemperature) / Float(NRSDAY)
! March 2005: now calculate precisely how many days are needed from EVP file
        EventDurationTemperature(Ievent,5) = Floor(Rdum)
        If (EventDurationTemperature(Ievent,5) .lt. Rdum) EventDurationTemperature(Ievent,5) = EventDurationTemperature(Ievent,5) + 1
        ISecStart = EventstartDateTimeTemperature(Ievent,4)*3600 + EventstartDateTimeTemperature(Ievent,5)*60 + EventstartDateTimeTemperature(Ievent,6)
        ISecDuration = LastT * NrSecsTemperature - (Floor(Rdum)*NrsDay)
        If (ISecStart+ISecDuration .gt. NrsDay) then
!          event duration is not a whole nr. of days, so add 1 additional daily evaporation value is needed
           EventDurationTemperature(Ievent,5) = EventDurationTemperature(Ievent,5) + 1
        Elseif (EventStartDateTimeTemperature(ievent,4) .ne. 0 .and. EventStartDateTimeTemperature(ievent,4) .ne. 24) then
!          event lastst whole number of days, but does not start at midnight
           EventDurationTemperature(Ievent,5) = EventDurationTemperature(Ievent,5) + 1
        Endif
! end March 2005

        EventDurationTemperature(Ievent,6) = LASTT +1

        If (Icall .eq. 1) then
          MaxDaysTemperature      = Max (EventDurationTemperature(Ievent,5), MaxDaysTemperature     )
          MaxTimestepsTemperature = Max (EventDurationTemperature(Ievent,6), MaxTimestepsTemperature)
        Endif

        if (idebug .ne. 0) Write(IDEBUG,*) ' Temperature event ', IEVENT
        Do ITMSTP=FRSTTM, LASTT
          If (Icall .eq. 1) then
            Read(IN,*,END=991,ERR=150,IOSTAT=IECODE)  Rdum
          Else
            Read(IN,*,END=991,ERR=150,IOSTAT=IECODE) (TemperatureData(Ievent,I,ITMSTP),I=1,NCSTATTemperature)
            if (idebug .ne. 0) Write(Idebug,*)   (TemperatureData(Ievent,I,ITMSTP),I=1,NCSTATTemperature)
          Endif
        Enddo
        if (idebug .ne. 0) then
           write(Idebug,*) ' Event - TemperatureDuration in days ', Ievent, EventDurationTemperature(Ievent,5)
           write(Idebug,*) ' Event - Temperature in Temperature Timesteps ', Ievent, EventDurationTemperature(Ievent,6)
        Endif
      Enddo

      goto 999
!
! *********************************************************************
! *** Error during reading of file
! *********************************************************************
!
  991 CONTINUE
      call ErrMsgStandard (911, IECODE, 'ReadTemperatureFile', ' Temperature file')
  150 CONTINUE
      call ErrMsgStandard (902, IECODE, 'ReadTemperatureFile', ' Temperature file')

! *********************************************************************
! *** end of file
! *********************************************************************
!
  999 CONTINUE
      Close(in)

      RETURN
      End Subroutine ReadTemperatureFile



  SUBROUTINE RdTemperature (IEvent, TemperatureTmstp)
    ! *********************************************************************
    ! *** Last update: November 2007                     By : Geert Prinsen
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***   Read rainfall file
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  IDEBUG = file unit number of debug file
    ! ***  Ievent = bui nummer
    ! *********************************************************************

    INTEGER       iECode, I, iDebug, Ievent, TemperatureTmstp
    CHARACTER(Len=CharIdLength) STRING

    String = ' '
    iDebug = ConfFil_get_iDebug()
    IF (iDebug /= 0) WRITE (IDEBUG,1)
  1 FORMAT (' RdTemperature')

! Always from already read Temperaturedata array
      DO I=1, NcStatTemperature
         InputTemperature(I) = Temperaturedata (Ievent, i, TemperatureTmstp)
      ENDDO


    IF (iDebug /= 0) WRITE(IDEBUG,*) ' Temperature read in m3/s', &
                                         (InputTemperature(I),I=1, NcStatTemperature)
    GOTO 999

    ! *********************************************************************
    ! *** Error during reading of file
    ! *********************************************************************

150 CONTINUE
    call ErrMsgStandard (902, IECODE, '  RdTemperature', STRING)

    ! *********************************************************************
    ! *** end of file
    ! *********************************************************************

 30 CONTINUE
    call ErrMsgStandard (911, IECODE, '  RdTemperature', STRING)

999 CONTINUE
    RETURN
  END subroutine RdTemperature


      Subroutine SelectedSubsetRainfallData (IDEBUG, Iout1)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  Sobek-RR parallell version                  March 2000
! *********************************************************************
! *** Last update: June  1997        By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Selecteren subset bui data
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  IOUT1  = file unit number of output file with messages
! *********************************************************************


      INTEGER      Iout1, Idebug, FRSTTM, LASTT,  &
                   Ievent, SelectedEvent, Itmstp, IShift, IStat, I
      Logical      found

      Integer           DateSubStart, DateSubEnd, DateEventStart
      Integer           TimeSubStart, TimeSubEnd, TimeEventStart
      Double Precision  Julian
      Double Precision  JulianSubDateStart, JulianSubDateEnd, JulianEventStart, SubSetDuration

      Real              DaysAfterStartEvent, DaysInEvent, DaysInSubset, Rshift, RLASTT

      iDebug = ConfFil_get_iDebug()
      if (idebug .ne. 0) WRITE (IDEBUG,1)
    1 FORMAT (' SelectedSubsetRainfallData')


! *** Check if there is a subset selected

      If (.not. TimeSettings%StartTimeFromEvent ) then

! *********************************************************************
! ***   There is a subset selected by specifications in the INI file
! ***   Check if it is a part of the rainfall data just read, and reorganize the array BUIDATA, EventStartDateTime and Duration.
! *********************************************************************

        DateSubStart = Timesettings%StartYear * 10000 + Timesettings%StartMonth * 100 + Timesettings%StartDay
        TimeSubStart = Timesettings%StartHour * 10000 + Timesettings%StartMinute * 100 + Timesettings%StartSecond
        JulianSubDateStart = Julian (DateSubStart,TimeSubStart)

        DateSubEnd   = Timesettings%EndYear * 10000 + Timesettings%EndMonth * 100 + Timesettings%EndDay
        TimeSubEnd   = Timesettings%EndHour * 10000 + Timesettings%EndMinute * 100 + Timesettings%EndSecond
        JulianSubDateEnd   = Julian (DateSubEnd  ,TimeSubEnd  )

        SubSetDuration = JulianSubDateEnd - JulianSubDateStart
!        DaysinSubSet = INT (JulianSubDateEnd - JulianSubDateStart) + 1
        DaysinSubSet = JulianSubDateEnd - JulianSubDateStart

! *********************************************************************
! *** Check in which event the selected period falls
! *********************************************************************

        Ievent = 1
        found = .false.

        Do While ((.not. found) .and. (Ievent .le. Nevent))

          DateEventStart = EventStartDateTime(Ievent,1)*10000 + EventStartDateTime(Ievent,2)*100 + EventStartDateTime(Ievent,3)
          TimeEventStart = EventStartDateTime(Ievent,4)*10000 + EventStartDateTime(Ievent,5)*100 + EventStartDateTime(Ievent,6)
          JulianEventStart= Julian (DateEventStart  ,TimeEventStart )

          DaysAfterStartEvent = (JulianSubDateStart-JulianEventStart)

!         DaysInEvent  = Float ((EventDuration(Ievent,6)-1)) * NRSecsRai / Float (NrsDay)
          DaysInEvent  = Float ((EventDuration(Ievent,6)  )) * NRSecsRai / Float (NrsDay)

          If ( (DaysAfterStartEvent .ge. 0) .and. ( (DaysInEvent+ (1.0/Float(NrsDay))) .ge. SubSetDuration+DaysAfterStartEvent) ) then
             ! ivm accuracy float/double: test dat beschikbaar aantal dagen in event (float) plus 1 seconde is groter dan de opgegeven subsetduur (als double)
             SelectedEvent = Ievent
             found = .true.
          else
             write(Iout1,*) ' ievent', ievent
             write(iout1,*) ' JulianSubDateStart', JulianSubDateStart
             Write(iout1,*) ' JulianEventStart', JulianEventStart
             write(Iout1,*) ' DaysAfterStartEvent', DaysAfterStartEvent
             write(Iout1,*) ' EventDuration(ievent,6) -1', EventDuration(Ievent,6) -1
             write(Iout1,*) ' NrSecsRai, NrsDay ', NrSecsRai, NrsDay
             write(Iout1,*) ' DaysInEvent', DaysInEvent
             write(Iout1,*) ' SubSetDuration', SubSetDuration
             write(Iout1,*) ' SubSetDuration +DaysAfterStartEvent', SubsetDuration+DaysAfterStartEvent
          Endif
          Ievent = Ievent + 1
        Enddo

        if (idebug .ne. 0) then
            Write(idebug,*) ' JulianSubDateStart', JulianSubDateStart
            Write(idebug,*) ' JulianEventStart', JulianEventStart
            Write(idebug,*) ' DaysInEvent', DaysInEvent
            Write(idebug,*) ' SubSetDuration', SubSetDuration
            Write(idebug,*) ' DaysAfterStartEvent', DaysAfterStartEvent
        Endif

        if (.not. found) then
           call ErrMsgStandard (972, 0, ' Error: Specified simulation period is not a subset of the rainfall datafile', &
                                           ' Meteo_SelectedSubsetRainfallData')
        else
! if subset period found, reset number of events and rainfall data
           Nevent = 1
           Ievent = 1
           RShift = DaysAfterStartEvent * Float (NrsDay) / NrSecsRai
           IShift = Int (DaysAfterStartEvent * Float (NrsDay) / NrSecsRai)
           if (idebug .ne. 0) then
              Write(idebug,*) ' IShift', IShift
              Write(idebug,*) ' RShift', RShift
              Write(idebug,*) ' DaysinSubset', DaysInSubset
           Endif
           If (abs (Rshift-Ishift) .gt. .01) then
              call ErrMsgStandard (977, 0, ' Specified simulation period does not start at beginning of a rainfall timestep', &
                                   ' This may cause undesired results. Check your input data!')
           Endif

           EventStartDateTime(Ievent,1) = TimeSettings%StartYear
           EventStartDateTime(Ievent,2) = TimeSettings%StartMonth
           EventStartDateTime(Ievent,3) = TimeSettings%StartDay
           EventStartDateTime(Ievent,4) = TimeSettings%StartHour
           EventStartDateTime(Ievent,5) = TimeSettings%StartMinute
           EventStartDateTime(Ievent,6) = TimeSettings%StartSecond

           if (idebug .ne. 0) THEN
              WRITE(IDEBUG,*) ' New Start   ',(EventStartDateTime(IEVENT,I),I=1,6)
              WRITE(IDEBUG,*) ' Old Duration',(EventDuration(IEVENT,I),I=1,4)
           ENDIF
           EventDuration     (Ievent,1) = Int (SubSetDuration)
! 24 uur op een dag
           SubSetDuration               = ( SubSetDuration - EventDuration(Ievent,1)) * 24.
           EventDuration     (Ievent,2) = Int ( SubSetDuration )
! 60 minuten in een uur
           SubSetDuration               = ( SubSetDuration - EventDuration(Ievent,2)) * 60.
           EventDuration     (Ievent,3) = Int ( SubSetDuration )
! 60 seconden in een minuut
           SubSetDuration               = ( SubSetDuration - EventDuration(Ievent,3)) * 60.
           EventDuration     (Ievent,4) = NInt ( SubSetDuration )
! check afrondingsfouten
           If (EventDuration (Ievent,4) .eq. 60) then
              EventDuration (Ievent,4) = 0
              EventDuration (Ievent,3) = EventDuration (Ievent,3) + 1
              If (EventDuration (Ievent,3) .eq. 60) then
                 EventDuration (Ievent,3) = 0
                 EventDuration (Ievent,2) = EventDuration (Ievent,2) + 1
                 If (EventDuration (Ievent,2) .eq. 24) then
                     EventDuration (Ievent,2) = 0
                     EventDuration (Ievent,1) = EventDuration (Ievent,1) + 1
                 Endif
              Endif
           Endif
!
           FRSTTM = 1
           RLASTT  = EventDuration(IEVENT,1)*86400.E0  + EventDuration(IEVENT,2)*3600.E0 + &
                    EventDuration(IEVENT,3)*60.E0     + EventDuration(IEVENT,4)*1.0E0
           LASTT  = NINT ( RLASTT  / Float (NRSecsRAI) )
! set number of rainfall timesteps, days, maximum over all events
! +2 ipv +1 ivm de mogelijkeheid van een bui, korter dan 1 dag qua duur, maar vallend over een datumgrens.
! Okt 2006:  maar niet groter dan de tot nu toe gedefinieerde maximum lengte
! April 2010: subset max nr days from evap file <= nr days (eventduration(.,1) +2
           EventDuration(Ievent,5) = Min (LASTT * NRSecsRAI / NRSDAY + 2, EventDuration(ievent,1)+2,EventDuration(Ievent,5))
! Issue evap end of file
           if (EventStartDateTime(Ievent,4) .eq. 0) then
              if (EventDuration(Ievent,5) .gt. 1) EventDuration(Ievent,5) = EventDuration(Ievent,5)-1
           else
              EventDuration(Ievent,5) = Int (JulianSubDateEnd -0.5D0) - Int (JulianSubDateStart -0.5D0) + 1
           endif
!
           EventDuration(Ievent,6) = LASTT +1

           Do ITMSTP=FRSTTM, LASTT
              Do IStat=1,NcStat
                 BuiData(Ievent,IStat,ITMSTP) = Buidata (SelectedEvent, iStat, Itmstp+IShift)
              Enddo
           Enddo

        Endif


        if (idebug .ne. 0) THEN
           WRITE(IDEBUG,*) ' Start   ',(EventStartDateTime(IEVENT,I),I=1,6)
           WRITE(IDEBUG,*) ' Duration',(EventDuration(IEVENT,I),I=1,4)
        ENDIF

! ***   Bepaal aantal tijdstappen per event

        if (idebug .ne. 0) Write(IDEBUG,*) ' Buidata bui ', IEVENT
        Do ITMSTP=FRSTTM, LASTT
            if (idebug .ne. 0) Write(Idebug,*)   (BuiData(Ievent,I,ITMSTP),I=1,NCSTAT)
        Enddo
        if (idebug .ne. 0) then
           write(Idebug,*) ' Event - Duration in days ', Ievent, EventDuration(Ievent,5)
           write(Idebug,*) ' Event - in Rainfall Timesteps ', Ievent, EventDuration(Ievent,6)
        Endif

      Endif


      RETURN
      End Subroutine SelectedSubsetRainfallData



      Subroutine SelectedSubsetRunoffData (IDEBUG)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  Sobek-RR parallell version                  March 2000
! *********************************************************************
! *** Last update: June  1997        By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Selecteren subset runoff data
! ***
! ***   routine based on subroutine SelectedSubsetRainfall data, see above !!
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  IOUT1  = file unit number of output file with messages
! *********************************************************************


      INTEGER      Idebug, FRSTTM, LASTT,  &
                   Ievent, SelectedEvent, Itmstp, IShift, IStat, I
      Logical      found

      Integer           DateSubStart, DateSubEnd, DateEventStart
      Integer           TimeSubStart, TimeSubEnd, TimeEventStart
      Double Precision  Julian
      Double Precision  JulianSubDateStart, JulianSubDateEnd, JulianEventStart, SubSetDuration

      Real              DaysAfterStartEvent, DaysInEvent, DaysInSubset, Rshift, RLASTT

      iDebug = ConfFil_get_iDebug()
      if (idebug .ne. 0) WRITE (IDEBUG,1)
    1 FORMAT (' SelectedSubsetRunoffData')


! *** Check if there is a subset selected

      If (.not. TimeSettings%StartTimeFromEvent ) then

! *********************************************************************
! ***   There is a subset selected by specifications in the INI file
! ***   Check if it is a part of the runoff data just read, and reorganize the array RUNOFFDATA, EventStartDateTimeRunoff and
! ***    EventDurationRunoff.
! *********************************************************************

        DateSubStart = Timesettings%StartYear * 10000 + Timesettings%StartMonth * 100 + Timesettings%StartDay
        TimeSubStart = Timesettings%StartHour * 10000 + Timesettings%StartMinute * 100 + Timesettings%StartSecond
        JulianSubDateStart = Julian (DateSubStart,TimeSubStart)

        DateSubEnd   = Timesettings%EndYear * 10000 + Timesettings%EndMonth * 100 + Timesettings%EndDay
        TimeSubEnd   = Timesettings%EndHour * 10000 + Timesettings%EndMinute * 100 + Timesettings%EndSecond
        JulianSubDateEnd   = Julian (DateSubEnd  ,TimeSubEnd  )

        SubSetDuration = JulianSubDateEnd - JulianSubDateStart
!        DaysinSubSet = INT (JulianSubDateEnd - JulianSubDateStart) + 1
        DaysinSubSet = JulianSubDateEnd - JulianSubDateStart

! *********************************************************************
! *** Check in which event the selected period falls
! *********************************************************************

        Ievent = 1
        found = .false.

        Do While ((.not. found) .and. (Ievent .le. Nevent))

          DateEventStart = EventStartDateTimeRunoff(Ievent,1)*10000 + EventStartDateTimeRunoff(Ievent,2)*100 + &
                              EventStartDateTimeRunoff(Ievent,3)
          TimeEventStart = EventStartDateTimeRunoff(Ievent,4)*10000 + EventStartDateTimeRunoff(Ievent,5)*100 + &
                              EventStartDateTimeRunoff(Ievent,6)
          JulianEventStart= Julian (DateEventStart  ,TimeEventStart )

          DaysAfterStartEvent = (JulianSubDateStart-JulianEventStart)

          DaysInEvent  = Float ((EventDurationRunoff(Ievent,6)-1)) * NRSecsRunoff / Float (NrsDay)

          If ( (DaysAfterStartEvent .ge. 0) .and. ( (DaysInEvent+ (1.0/Float(NrsDay))) .ge. SubSetDuration+DaysAfterStartEvent) ) then
             ! ivm accuracy float/double: test dat beschikbaar aantal dagen in event (float) plus 1 seconde is groter dan de opgegeven subsetduur (als double)
             SelectedEvent = Ievent
             found = .true.
          Endif
          Ievent = Ievent + 1
        Enddo

        if (idebug .ne. 0) then
            Write(idebug,*) ' JulianSubDateStart', JulianSubDateStart
            Write(idebug,*) ' JulianEventStart', JulianEventStart
            Write(idebug,*) ' DaysInEvent', DaysInEvent
            Write(idebug,*) ' SubSetDuration', SubSetDuration
            Write(idebug,*) ' DaysAfterStartEvent', DaysAfterStartEvent
        Endif

        if (.not. found) then
           call ErrMsgStandard (972, 0, ' Error: Specified simulation period is not a subset of the runoff datafile', &
                                           ' Meteo_SelectedSubsetRunoffData')
        else
! if subset period found, reset number of events and rainfall data
           Nevent = 1
           Ievent = 1
           RShift = DaysAfterStartEvent * Float (NrsDay) / NrSecsRunoff
           IShift = Int (DaysAfterStartEvent * Float (NrsDay) / NrSecsRunoff)
           if (idebug .ne. 0) then
              Write(idebug,*) ' IShift', IShift
              Write(idebug,*) ' RShift', RShift
              Write(idebug,*) ' DaysinSubset', DaysInSubset
           Endif
           If (abs (Rshift-Ishift) .gt. .01) then
              call ErrMsgStandard (977, 0, ' Specified simulation period does not start at beginning of a runoff timestep', &
                                   ' This may cause undesired results. Check your input data!')
           Endif

           EventStartDateTimeRunoff(Ievent,1) = TimeSettings%StartYear
           EventStartDateTimeRunoff(Ievent,2) = TimeSettings%StartMonth
           EventStartDateTimeRunoff(Ievent,3) = TimeSettings%StartDay
           EventStartDateTimeRunoff(Ievent,4) = TimeSettings%StartHour
           EventStartDateTimeRunoff(Ievent,5) = TimeSettings%StartMinute
           EventStartDateTimeRunoff(Ievent,6) = TimeSettings%StartSecond

           if (idebug .ne. 0) THEN
              WRITE(IDEBUG,*) ' New Start   ',(EventStartDateTimeRunoff(IEVENT,I),I=1,6)
              WRITE(IDEBUG,*) ' Old Duration',(EventDurationRunoff(IEVENT,I),I=1,4)
           ENDIF
           EventDurationRunoff(Ievent,1) = Int (SubSetDuration)
! 24 uur op een dag
           SubSetDuration                = ( SubSetDuration - EventDurationRunoff(Ievent,1)) * 24.
           EventDurationRunoff(Ievent,2) = Int ( SubSetDuration )
! 60 minuten in een uur
           SubSetDuration                = ( SubSetDuration - EventDurationRunoff(Ievent,2)) * 60.
           EventDurationRunoff(Ievent,3) = Int ( SubSetDuration )
! 60 seconden in een minuut
           SubSetDuration                = ( SubSetDuration - EventDurationRunoff(Ievent,3)) * 60.
           EventDurationRunoff(Ievent,4) = NInt ( SubSetDuration )
! check afrondingsfouten
           If (EventDurationRunoff (Ievent,4) .eq. 60) then
              EventDurationRunoff (Ievent,4) = 0
              EventDurationRunoff (Ievent,3) = EventDurationRunoff (Ievent,3) + 1
              If (EventDurationRunoff (Ievent,3) .eq. 60) then
                 EventDurationRunoff (Ievent,3) = 0
                 EventDurationRunoff (Ievent,2) = EventDurationRunoff (Ievent,2) + 1
                 If (EventDurationRunoff (Ievent,2) .eq. 24) then
                     EventDurationRunoff (Ievent,2) = 0
                     EventDurationRunoff (Ievent,1) = EventDurationRunoff (Ievent,1) + 1
                 Endif
              Endif
           Endif
!
           FRSTTM = 1
           RLASTT  = EventDurationRunoff(IEVENT,1)*86400.E0  + EventDurationRunoff(IEVENT,2)*3600.E0 + &
                    EventDurationRunoff(IEVENT,3)*60.E0     + EventDurationRunoff(IEVENT,4)*1.E0
           LASTT  = NINT ( RLASTT  / Float (NRSecsRunoff) )
! set number of runoff timesteps, days, maximum over all events
! +2 ipv +1 ivm de mogelijkeheid van een bui, korter dan 1 dag qua duur, maar vallend over een datumgrens.
! Okt 2006:  maar niet groter dan de tot nu toe gedefinieerde maximum lengte
           EventDurationRunoff(Ievent,5) = Min (LASTT * NRSecsRunoff / NRSDAY + 2, EventDurationRunoff(Ievent,5))
           EventDurationRunoff(Ievent,6) = LASTT +1

           Do ITMSTP=FRSTTM, LASTT
              Do IStat=1,NcStatRunoff
                 RunoffData(Ievent,IStat,ITMSTP) = Runoffdata (SelectedEvent, iStat, Itmstp+IShift)
              Enddo
           Enddo

        Endif


        if (idebug .ne. 0) THEN
           WRITE(IDEBUG,*) ' Start   ',(EventStartDateTimeRunoff(IEVENT,I),I=1,6)
           WRITE(IDEBUG,*) ' Duration',(EventDurationRunoff(IEVENT,I),I=1,4)
        ENDIF

! ***   Bepaal aantal tijdstappen per event

        if (idebug .ne. 0) Write(IDEBUG,*) ' Runoffdata bui ', IEVENT
        Do ITMSTP=FRSTTM, LASTT
            if (idebug .ne. 0) Write(Idebug,*)   (RunoffData(Ievent,I,ITMSTP),I=1,NCSTAT)
        Enddo
        if (idebug .ne. 0) then
           write(Idebug,*) ' Event - Duration in Runoff days ', Ievent, EventDurationRunoff(Ievent,5)
           write(Idebug,*) ' Event - in Runoff Timesteps ', Ievent, EventDurationRunoff(Ievent,6)
        Endif

      Endif


      RETURN
      End Subroutine SelectedSubsetRunoffData



      Subroutine SelectedSubsetTemperatureData (IDEBUG)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  Sobek-RR parallell version                  March 2000
! *********************************************************************
! *** Last update: June  1997        By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Selecteren subset Temperature data
! ***
! ***   routine based on subroutine SelectedSubsetRainfall data, see above !!
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  IOUT1  = file unit number of output file with messages
! *********************************************************************


      INTEGER      Idebug, FRSTTM, LASTT,  &
                   Ievent, SelectedEvent, Itmstp, IShift, IStat, I
      Logical      found

      Integer           DateSubStart, DateSubEnd, DateEventStart
      Integer           TimeSubStart, TimeSubEnd, TimeEventStart
      Double Precision  Julian
      Double Precision  JulianSubDateStart, JulianSubDateEnd, JulianEventStart, SubSetDuration

      Real              DaysAfterStartEvent, DaysInEvent, DaysInSubset, Rshift, RLASTT

      iDebug = ConfFil_get_iDebug()
      if (idebug .ne. 0) WRITE (IDEBUG,1)
    1 FORMAT (' SelectedSubsetTemperatureData')


! *** Check if there is a subset selected

      If (.not. TimeSettings%StartTimeFromEvent ) then

! *********************************************************************
! ***   There is a subset selected by specifications in the INI file
! ***   Check if it is a part of the Temperature data just read, and reorganize the array TemperatureData,
! ***     EventStartDateTimeTemperature and EventDurationTemperature.
! *********************************************************************

        DateSubStart = Timesettings%StartYear * 10000 + Timesettings%StartMonth * 100 + Timesettings%StartDay
        TimeSubStart = Timesettings%StartHour * 10000 + Timesettings%StartMinute * 100 + Timesettings%StartSecond
        JulianSubDateStart = Julian (DateSubStart,TimeSubStart)

        DateSubEnd   = Timesettings%EndYear * 10000 + Timesettings%EndMonth * 100 + Timesettings%EndDay
        TimeSubEnd   = Timesettings%EndHour * 10000 + Timesettings%EndMinute * 100 + Timesettings%EndSecond
        JulianSubDateEnd   = Julian (DateSubEnd  ,TimeSubEnd  )

        SubSetDuration = JulianSubDateEnd - JulianSubDateStart
        DaysinSubSet = JulianSubDateEnd - JulianSubDateStart

! *********************************************************************
! *** Check in which event the selected period falls
! *********************************************************************

        Ievent = 1
        found = .false.

        Do While ((.not. found) .and. (Ievent .le. Nevent))

          DateEventStart = EventStartDateTimeTemperature(Ievent,1)*10000 + EventStartDateTimeTemperature(Ievent,2)*100 + &
                              EventStartDateTimeTemperature(Ievent,3)
          TimeEventStart = EventStartDateTimeTemperature(Ievent,4)*10000 + EventStartDateTimeTemperature(Ievent,5)*100 + &
                              EventStartDateTimeTemperature(Ievent,6)
          JulianEventStart= Julian (DateEventStart  ,TimeEventStart )

          DaysAfterStartEvent = (JulianSubDateStart-JulianEventStart)

          DaysInEvent  = Float ((EventDurationTemperature(Ievent,6)-1)) * NRSecsTemperature / Float (NrsDay)

          If ( (DaysAfterStartEvent .ge. 0) .and. ( (DaysInEvent+ (1.0/Float(NrsDay))) .ge. SubSetDuration+DaysAfterStartEvent) ) then
             ! ivm accuracy float/double: test dat beschikbaar aantal dagen in event (float) plus 1 seconde is groter dan de opgegeven subsetduur (als double)
             SelectedEvent = Ievent
             found = .true.
          Endif
          Ievent = Ievent + 1
        Enddo

        if (idebug .ne. 0) then
            Write(idebug,*) ' JulianSubDateStart', JulianSubDateStart
            Write(idebug,*) ' JulianEventStart', JulianEventStart
            Write(idebug,*) ' DaysInEvent', DaysInEvent
            Write(idebug,*) ' SubSetDuration', SubSetDuration
            Write(idebug,*) ' DaysAfterStartEvent', DaysAfterStartEvent
        Endif

        if (.not. found) then
           call ErrMsgStandard (972, 0, ' Error: Specified simulation period is not a subset of the Temperature datafile', &
                                           ' Meteo_SelectedSubsetTemperatureData')
        else
! if subset period found, reset number of events and rainfall data
           Nevent = 1
           Ievent = 1
           RShift = DaysAfterStartEvent * Float (NrsDay) / NrSecsTemperature
           IShift = Int (DaysAfterStartEvent * Float (NrsDay) / NrSecsTemperature)
           if (idebug .ne. 0) then
              Write(idebug,*) ' IShift', IShift
              Write(idebug,*) ' RShift', RShift
              Write(idebug,*) ' DaysinSubset', DaysInSubset
           Endif
           If (abs (Rshift-Ishift) .gt. .01) then
              call ErrMsgStandard (977, 0, ' Specified simulation period does not start at beginning of a Temperature timestep', &
                                   ' This may cause undesired results. Check your input data!')
           Endif

           EventStartDateTimeTemperature(Ievent,1) = TimeSettings%StartYear
           EventStartDateTimeTemperature(Ievent,2) = TimeSettings%StartMonth
           EventStartDateTimeTemperature(Ievent,3) = TimeSettings%StartDay
           EventStartDateTimeTemperature(Ievent,4) = TimeSettings%StartHour
           EventStartDateTimeTemperature(Ievent,5) = TimeSettings%StartMinute
           EventStartDateTimeTemperature(Ievent,6) = TimeSettings%StartSecond

           if (idebug .ne. 0) THEN
              WRITE(IDEBUG,*) ' New Start   ',(EventStartDateTimeTemperature(IEVENT,I),I=1,6)
              WRITE(IDEBUG,*) ' Old Duration',(EventDurationTemperature(IEVENT,I),I=1,4)
           ENDIF
           EventDurationTemperature(Ievent,1) = Int (SubSetDuration)
! 24 uur op een dag
           SubSetDuration                = ( SubSetDuration - EventDurationTemperature(Ievent,1)) * 24.
           EventDurationTemperature(Ievent,2) = Int ( SubSetDuration )
! 60 minuten in een uur
           SubSetDuration                = ( SubSetDuration - EventDurationTemperature(Ievent,2)) * 60.
           EventDurationTemperature(Ievent,3) = Int ( SubSetDuration )
! 60 seconden in een minuut
           SubSetDuration                = ( SubSetDuration - EventDurationTemperature(Ievent,3)) * 60.
           EventDurationTemperature(Ievent,4) = NInt ( SubSetDuration )
! check afrondingsfouten
           If (EventDurationTemperature (Ievent,4) .eq. 60) then
              EventDurationTemperature (Ievent,4) = 0
              EventDurationTemperature (Ievent,3) = EventDurationTemperature (Ievent,3) + 1
              If (EventDurationTemperature (Ievent,3) .eq. 60) then
                 EventDurationTemperature (Ievent,3) = 0
                 EventDurationTemperature (Ievent,2) = EventDurationTemperature (Ievent,2) + 1
                 If (EventDurationTemperature (Ievent,2) .eq. 24) then
                     EventDurationTemperature (Ievent,2) = 0
                     EventDurationTemperature (Ievent,1) = EventDurationTemperature (Ievent,1) + 1
                 Endif
              Endif
           Endif
!
           FRSTTM = 1
           RLASTT  = EventDurationTemperature(IEVENT,1)*86400.E0  + EventDurationTemperature(IEVENT,2)*3600.E0 + &
                    EventDurationTemperature(IEVENT,3)*60.E0     + EventDurationTemperature(IEVENT,4)*1.E0
           LASTT  = NINT ( RLASTT  / Float (NRSecsTemperature) )
! set number of Temperature timesteps, days, maximum over all events
! +2 ipv +1 ivm de mogelijkeheid van een bui, korter dan 1 dag qua duur, maar vallend over een datumgrens.
! Okt 2006:  maar niet groter dan de tot nu toe gedefinieerde maximum lengte
           EventDurationTemperature(Ievent,5) = Min (LASTT * NRSecsTemperature / NRSDAY + 2, EventDurationTemperature(Ievent,5))
           EventDurationTemperature(Ievent,6) = LASTT +1

           Do ITMSTP=FRSTTM, LASTT
              Do IStat=1,NcStatTemperature
                 TemperatureData(Ievent,IStat,ITMSTP) = Temperaturedata (SelectedEvent, iStat, Itmstp+IShift)
              Enddo
           Enddo

        Endif


        if (idebug .ne. 0) THEN
           WRITE(IDEBUG,*) ' Start   ',(EventStartDateTimeTemperature(IEVENT,I),I=1,6)
           WRITE(IDEBUG,*) ' Duration',(EventDurationTemperature(IEVENT,I),I=1,4)
        ENDIF

! ***   Bepaal aantal tijdstappen per event

        if (idebug .ne. 0) Write(IDEBUG,*) ' Temperaturedata bui ', IEVENT
        Do ITMSTP=FRSTTM, LASTT
            if (idebug .ne. 0) Write(Idebug,*)   (TemperatureData(Ievent,I,ITMSTP),I=1,NCSTAT)
        Enddo
        if (idebug .ne. 0) then
           write(Idebug,*) ' Event - Duration in Temperature days ', Ievent, EventDurationTemperature(Ievent,5)
           write(Idebug,*) ' Event - in Temperature Timesteps ', Ievent, EventDurationTemperature(Ievent,6)
        Endif

      Endif


      RETURN
      End Subroutine SelectedSubsetTemperatureData




      Subroutine VerifyOutput2CFUserDefPeriod (IDEBUG)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  Sobek-RR parallell version                  March 2000
! *********************************************************************
! *** Last update: June  1997        By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Verify selected output period to CF; should be a subset of the simulation period
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  IOUT1  = file unit number of output file with messages
! *********************************************************************


      INTEGER      Idebug, Ievent
      Logical      found

      Integer           DateSubStart, DateSubEnd, DateEventStart
      Integer           TimeSubStart, TimeSubEnd, TimeEventStart
      Double Precision  Julian
      Double Precision  JulianSubDateStart, JulianSubDateEnd, JulianEventStart, SubSetDuration

      Real              DaysAfterStartEvent, DaysInEvent, DaysInSubset

      iDebug = ConfFil_get_iDebug()
      if (idebug .ne. 0) WRITE (IDEBUG,1)
    1 FORMAT (' VerifyOutput2CFPeriod')


! *** Check if there is a subset selected

      If (TimeSettings%Output2CFUserDefinedPeriod ) then

! *********************************************************************
! ***   The output period to CF is user defined
! ***   Check if it is a part of the simulation period
! *********************************************************************

        DateSubStart = Timesettings%OutputStartYear * 10000 + &
                         Timesettings%OutputStartMonth * 100 + &
                             Timesettings%OutputStartDay
        TimeSubStart = Timesettings%OutputStartHour * 10000 + &
                          Timesettings%OutputStartMinute * 100 + &
                             Timesettings%OutputStartSecond
        JulianSubDateStart = Julian (DateSubStart,TimeSubStart)

        DateSubEnd   = Timesettings%OutputEndYear * 10000 + &
                            Timesettings%OutputEndMonth * 100 + Timesettings%OutputEndDay
        TimeSubEnd   = Timesettings%OutputEndHour * 10000 + &
                            Timesettings%OutputEndMinute * 100 + Timesettings%OutputEndSecond
        JulianSubDateEnd   = Julian (DateSubEnd  ,TimeSubEnd  )

        SubSetDuration = JulianSubDateEnd - JulianSubDateStart
        DaysinSubSet = JulianSubDateEnd - JulianSubDateStart

        TimeSettings%JulianStartOutput = JulianSubDateStart
        TimeSettings%JulianEndOutput   = JulianSubDateEnd

! *********************************************************************
! *** Check in which event the selected period falls
! *********************************************************************

        Ievent = 1
        found = .false.

        Do While ((.not. found) .and. (Ievent .le. Nevent))
          DateEventStart = EventStartDateTime(Ievent,1)*10000 + EventStartDateTime(Ievent,2)*100 + EventStartDateTime(Ievent,3)
          TimeEventStart = EventStartDateTime(Ievent,4)*10000 + EventStartDateTime(Ievent,5)*100 + EventStartDateTime(Ievent,6)
          JulianEventStart= Julian (DateEventStart  ,TimeEventStart )
          DaysAfterStartEvent = (JulianSubDateStart-JulianEventStart)
          DaysInEvent  = Float ((EventDuration(Ievent,6)-1)) * NRSecsRai / Float (NrsDay)
          If ( (DaysAfterStartEvent .ge. 0) .and. ( (DaysInEvent+ (1.0/Float(NrsDay))) .ge. SubSetDuration+DaysAfterStartEvent) ) then
             ! ivm accuracy float/double: test dat beschikbaar aantal dagen in event (float) plus 1 seconde is groter dan de opgegeven subsetduur (als double)
             TimeSettings%outputEvent = Ievent
             found = .true.
          Endif
          Ievent = Ievent + 1
        Enddo

        if (idebug .ne. 0) then
            Write(idebug,*) ' JulianSubDateStart', JulianSubDateStart
            Write(idebug,*) ' JulianEventStart', JulianEventStart
            Write(idebug,*) ' DaysInEvent', DaysInEvent
            Write(idebug,*) ' SubSetDuration', SubSetDuration
            Write(idebug,*) ' DaysAfterStartEvent', DaysAfterStartEvent
        Endif

        if (.not. found) then
           call ErrMsgStandard (972, 0, ' Error: Specified simulation period is not a subset of the rainfall datafile', &
                                           ' Meteo_SelectedSubsetRainfallData')
        else
! if subset period found, set header data for output in RUNOFF.OUT


        Endif


      Endif


      RETURN
      End Subroutine VerifyOutput2CFUserDefPeriod



      Subroutine VerifyRainfallData

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  Sobek-RR parallell version                  March 2000
! *********************************************************************
! *** Last update: June  1997        By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Verify that in series, events are chronological
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  IOUT1  = file unit number of output file with messages
! *********************************************************************


      INTEGER      Idebug, iout1, Ievent
      Logical      errorfound

      Integer           DateEventStart, DateEventStartip1
      Integer           TimeEventStart, TimeEventStartip1
      Double Precision  Julian
      Double Precision  JulianEventStartip1, JulianEventStart

      Real              DaysInEvent

      iDebug = ConfFil_get_iDebug()
      iOut1 = ConfFil_get_iOut1()
      if (idebug .ne. 0) WRITE (IDEBUG,1)
    1 FORMAT (' VerifyRainfallData')

! *********************************************************************
! *** start of event i+1 should be after end of event i
! *********************************************************************

        errorfound = .false.
        Do Ievent = 1, NEvent-1
          DateEventStart = EventStartDateTime(Ievent,1)*10000 + EventStartDateTime(Ievent,2)*100 + EventStartDateTime(Ievent,3)
          TimeEventStart = EventStartDateTime(Ievent,4)*10000 + EventStartDateTime(Ievent,5)*100 + EventStartDateTime(Ievent,6)
          DateEventStartip1 = EventStartDateTime(Ievent+1,1)*10000 + EventStartDateTime(Ievent+1,2)*100 + EventStartDateTime(Ievent+1,3)
          TimeEventStartip1 = EventStartDateTime(Ievent+1,4)*10000 + EventStartDateTime(Ievent+1,5)*100 + EventStartDateTime(Ievent+1,6)
          JulianEventStart= Julian (DateEventStart  ,TimeEventStart )
          JulianEventStartip1= Julian (DateEventStartip1  ,TimeEventStartip1 )
          DaysInEvent  = Float ((EventDuration(Ievent,6)-1)) * NRSecsRai / Float (NrsDay)
          if (JulianEventStart + DaysInEvent .gt. JulianEventStartip1) then
             Write(iout1,*) ' Error for event nr ', iEvent
             errorfound = .true.
          Endif
        Enddo

       if (errorfound) &
         call ErrMsgStandard (972, IEvent, ' Error: Specified Rainfall file not consistent: Starttime of Event i+1 < Endtime of Event i', '')

      RETURN
      End Subroutine VerifyRainfallData



      Subroutine VerifyRainfallAndRunoffAndTemperatureData

      Integer Ievent
      Integer           DateEventStart
      Integer           TimeEventStart
      Integer           DateEventStartRunoff
      Integer           TimeEventStartRunoff, iout1
      Integer           DateEventStartTemperature
      Integer           TimeEventStartTemperature

      Double Precision  Julian
      Double Precision  JulianEventStart, JulianEventStartRunoff, JulianEventStartTemperature

      Real              DaysInEvent, DaysInEventRunoff, DaysInEventTemperature
      Logical           errorfound
      Logical           errorfoundtemp

      errorfound = .false.
      errorfoundtemp = .false.
      iOut1 = ConfFil_get_iOut1()

      Do iEvent =1,NEvent
         DateEventStart = EventStartDateTime(Ievent,1)*10000 + EventStartDateTime(Ievent,2)*100 + EventStartDateTime(Ievent,3)
         TimeEventStart = EventStartDateTime(Ievent,4)*10000 + EventStartDateTime(Ievent,5)*100 + EventStartDateTime(Ievent,6)
         JulianEventStart= Julian (DateEventStart  ,TimeEventStart )
         DaysInEvent  = Float ((EventDuration(Ievent,6)-1)) * NRSecsRai / Float (NrsDay)

         if (NcRRRunoffExternal .gt. 0) then
           DateEventStartRunoff = EventStartDateTimeRunoff(Ievent,1)*10000 + &
                                   EventStartDateTimeRunoff(Ievent,2)*100 + EventStartDateTimeRunoff(Ievent,3)
           TimeEventStartRunoff = EventStartDateTimeRunoff(Ievent,4)*10000 + &
                                   EventStartDateTimeRunoff(Ievent,5)*100 + EventStartDateTimeRunoff(Ievent,6)
           JulianEventStartRunoff = Julian (DateEventStartRunoff, TimeEventStartRunoff )
           DaysInEventRunoff = Float ((EventDurationRunoff(Ievent,6)-1)) * NRSecsRunoff / Float (NrsDay)
         !check for each event that runoff data is present for whole rainfall period
           if ( (JulianEventStart .lt. JulianEventStartRunoff-0.0001) .or.  &
                (JulianEventStart+DaysInEvent .gt. JulianEventStartRunoff + DaysInEventRunoff +0.0001) ) then
              Write(iout1,*) ' Error for event nr ', iEvent
              errorfound = .true.
           endif
         endif

         if (NcRRRunoffHBV .gt. 0) then
           DateEventStartTemperature = EventStartDateTimeTemperature(Ievent,1)*10000 + &
                                        EventStartDateTimeTemperature(Ievent,2)*100 + &
                                         EventStartDateTimeTemperature(Ievent,3)
           TimeEventStartTemperature = EventStartDateTimeTemperature(Ievent,4)*10000 + &
                                        EventStartDateTimeTemperature(Ievent,5)*100 + &
                                         EventStartDateTimeTemperature(Ievent,6)
           JulianEventStartTemperature = Julian (DateEventStartTemperature  ,TimeEventStartTemperature )

           DaysInEventTemperature = Float ((EventDurationTemperature(Ievent,6)-1)) * NRSecsTemperature / Float (NrsDay)

           !check for each event that temperature data is present for whole rainfall period
           if ( (JulianEventStart .lt. JulianEventStartTemperature-0.0001) .or.  &
                (JulianEventStart+DaysInEvent .gt. JulianEventStartTemperature + DaysInEventTemperature+0.0001) ) then
               Write(iout1,*) ' Error for event nr ', iEvent
               errorfoundtemp = .true.
           endif
         endif
      Enddo
      if (errorfound) then
         call ErrMsgStandard (972, IEvent, ' Error: Specified Runoff file does not contain data for whole simulation period', '')
      endif
      if (errorfoundtemp) then
         call ErrMsgStandard (972, IEvent, ' Error: Specified Temperature file does not contain data for whole simulation period', '')
      endif

      RETURN
      End Subroutine VerifyRainfallandRunoffAndTemperatureData


      Subroutine DetermEventStartHisIdxROffOut (Idebug)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  Sobek-RR parallell version                  March 2000
! *********************************************************************
! *** Last update: June  1997        By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Bepalen startindex HIS file runoff.Out bij reeksberekening
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  IOUT1  = file unit number of output file with messages
! *********************************************************************

      INTEGER           Idebug, Ievent

      Integer           DateEventStart1, DateEventStart, HisIndexStart
      Integer           TimeEventStart1, TimeEventStart
      Double Precision  Julian
      Double Precision  JulianEventStart1, JulianEventStart, EventDifference, Even


      iDebug = ConfFil_get_iDebug()
      if (idebug .ne. 0) WRITE (IDEBUG,1)
    1 FORMAT (' DetermEventStartHisIdxROffOut')

! *********************************************************************
! ***   Determine Julian date of first timestep
! *********************************************************************

        Ievent = 1
        DateEventStart1   = EventStartDateTime(Ievent,1)*10000 + EventStartDateTime(Ievent,2)*100 + EventStartDateTime(Ievent,3)
        TimeEventStart1   = EventStartDateTime(Ievent,4)*10000 + EventStartDateTime(Ievent,5)*100 + EventStartDateTime(Ievent,6)
        JulianEventStart1 = Julian (DateEventStart1  ,TimeEventStart1 )
        EventStartDateTime(Ievent,7) = 0

! *********************************************************************
! *** Set index for all events
! *********************************************************************

        Do Ievent=2,Nevent

          DateEventStart = EventStartDateTime(Ievent,1)*10000 + EventStartDateTime(Ievent,2)*100 + EventStartDateTime(Ievent,3)
          TimeEventStart = EventStartDateTime(Ievent,4)*10000 + EventStartDateTime(Ievent,5)*100 + EventStartDateTime(Ievent,6)
          JulianEventStart= Julian (DateEventStart  ,TimeEventStart )
          EventDifference = (JulianEventStart-JulianEventStart1)
          Even            =  EventDifference / Timesettings%TimestepSize
!         if (idebug .ne. 0) write(idebug,*) ' Even ', Even, Even*86400
          HisIndexStart   =  Int ( EventDifference / Timesettings%TimestepSize * Float (NrsDay) )
!         if (idebug .ne. 0) write(idebug,*) ' HisIndexStart 1 ', HisIndexStart
          HisIndexStart   =  NInt ( Even * Float (NrsDay) )

          if (idebug .ne. 0) then
            Write(idebug,*) ' JulianEventStart1', JulianEventStart1
            Write(idebug,*) ' JulianEventStart IEVENT', Ievent, JulianEventStart
            Write(idebug,*) ' EventDifference', EventDifference
            Write(idebug,*) ' HisIndexStart ', HisIndexStart
          Endif
!         Write(*,*) ' HisIndexStart in days from start 1st event', HisIndexStart
          EventStartDateTime(Ievent,7) = HisIndexStart

        Enddo

      RETURN
      End Subroutine DetermEventStartHisIdxROffOut




end module RR_Meteo
