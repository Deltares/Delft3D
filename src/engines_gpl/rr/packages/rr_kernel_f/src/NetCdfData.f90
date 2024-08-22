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

 module NetCdfData


  ! variables

  implicit none

  Logical  GenerateNetCdfOutput ! =true if NetCdf output is to be generated
  Logical  GenerateHISOutput ! =true if HIS output is to be generated
  Logical  MeteoNetCdfInput ! =true if meteo input data is to be read from NetCdf

  Integer, parameter :: NrNetCdfFiles = 50
  Integer, parameter :: MaxSeriesPerFile = 100

  Integer, parameter :: BalanceNetCdfFileNr      = 50
  Integer, parameter :: FixedAreaNetCdfFileNr    = 49
  Integer, parameter :: NWRWAreaNetCdfFileNr     = 48
  Integer, parameter :: RRStrDimNetCdfFileNr     = 47
  Integer, parameter :: BndFloTotNetCdfFileNr    = 46
  Integer, parameter :: NWRWSysNetCdfFileNr      = 45
! Integer, parameter :: RRWLMBalanceNetCdfFileNr = 44

  Character(Len=256)           NetCdfOutputFileName, NetCdfTimestep
  Integer                      IErr
  Integer                      INetCdfFile(NrNetCdfFiles)
  Integer                      Loc_Dimid(NrNetCdfFiles), Time_DimId(NrNetCdfFiles), Time_VarId(NrNetCdfFiles)
  Integer                      id_StrLenDim, id_locdim, id_timedim
  Integer                      ID_Vars(NrNetCdfFiles,MaxSeriesPerFile)

  Integer                      NetCdfMaxNrTimesteps, NetCdfMaxNrDays
  Integer                      TimestepInNWRWSys
  Integer                      NrTimestepsPreviousEventsUntilNow, NrEventInSeries
  Double precision             DDays
  Integer                      IDays


  contains

 subroutine nc_init

    NrTimestepsPreviousEventsUntilNow = 0

 end subroutine nc_init


 function nc_create(ncfile) result (ncid)

    use netcdf
    use readlib
    use globals

    implicit none
    character(len=*), intent(in) :: ncfile   !< name of the new netcdf-file to be created
    integer                      :: ncid     !< Netcdf ID given to this file

    Character(len=999) :: ncfile_lowercase, modelversion
    character(len=8)  :: cdate
    character(len=10) :: ctime
    character(len=5)  :: czone
    integer :: ierr
    integer :: oldfillmode
    ierr = nf90_noerr

!  always use netcdf filename in lower case
    ncfile_lowercase = ncfile(1:)
    call Lowerc(ncfile_lowercase)
    ierr = nf90_create(trim(ncfile_lowercase), 0, ncid)
    Call NetCdfCheck('NC_Create - After nf90_create',ierr)
    ierr = nf90_set_fill(ncid,NF90_NOFILL,oldfillmode)
    Call NetCdfCheck('NC_Create - After nf90_set_fill',ierr)
    if (ierr /= nf90_noerr) then
       return
    endif
    ierr = nf90_put_att(ncid, nf90_global,  'institution', 'Deltares')
    Call NetCdfCheck('NC_Create - After put_att institution',ierr)
    ierr = nf90_put_att(ncid, nf90_global,  'references', 'http://www.deltares.nl')
    Call NetCdfCheck('NC_Create - After put_att references',ierr)
    modelversion = rr_version_string
    ierr = nf90_put_att(ncid, nf90_global,  'source', trim(modelversion))
    Call NetCdfCheck('NC_Create - After put_att source',ierr)
    ierr = nf90_put_att(ncid, nf90_global,  'featureType', 'timeSeries')
    Call NetCdfCheck('NC_Create - After put_att featuretype',ierr)

    call date_and_time(cdate, ctime, czone)
    ierr = nf90_put_att(ncid, nf90_global,  'history', &
        'Created on '//cdate(1:4)//'-'//cdate(5:6)//'-'//cdate(7:8)//'T'//ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//czone(1:5)// &
        ', SOBEK-RR Rainfall-Runoff model')

    Call NetCdfCheck('NC_Create - After put_att history',ierr)
!   ierr = nf90_put_att(ncid, nf90_global,  'simulation_start_date', 'Startdate: '//RefDate(1:4)//'-'//RefDate(6:7)//'-'//RefDate(9:10))

    ierr = nf90_put_att(ncid, nf90_global,  'Conventions', 'CF-1.7')
    Call NetCdfCheck('NC_Create - After put_att Conventions',ierr)
 end function nc_create


subroutine nc_prepare(ncid, numitem, item_prefix, dimid_item, dimid_time, refdate, reftime, varid_time, id, DailyOutputNetCdf, xc,  yc, xc2, yc2, dimid_chr)

    use netcdf

    implicit none
    integer, intent(in)                               ::  ncid       !< ID of the netcdf file
    integer, intent(in)                               ::  numitem    !< number of items (structs, observations,...you name it)
    character(len=*), intent(in)                      ::  item_prefix!< Specific name of this item, ends up in the varnames
    character(len=40), dimension(:)                   ::  id !< Pointer to an array of id's
    integer, intent(in)                               ::  reftime    !< Reference time (used in time unit)
    integer, intent(in)                               ::  refdate    !< Reference date (used in time unit)
    integer, intent(out)                              ::  varid_time !< variable ID assigned to the time variable
    integer, intent(out)                              ::  dimid_item !< Dimension id of the item axis (we need it later when creating variables along these dimensions)
    integer, intent(out)                              ::  dimid_time !< Dimension id of the item axis (we need it later when creating variables along these dimensions)

    double precision, dimension(:), optional ::  xc, yc       !< Pointer to (x,y) coordinates, optional for nodes and 'from' node links
    double precision, dimension(:), optional ::  xc2, yc2     !< Pointer to (x,y) coordinates, optional            for 'to' node links
    integer, optional                        ::  dimid_chr
    character(len = 50)                      ::  time_unit
    logical                                  ::  DailyOutputNetCdf

    integer :: ierr, idLen
    integer :: dimid_chars
    integer :: varid_id, varid_x, varid_y, varid_x2, varid_y2, varid_type

    integer          :: imiss
    double precision :: dmiss

    IdLen = 40
    imiss = -999
    dmiss = -999.99D0


    if (DailyOutputNetCdf) then
!      write(*,*) ' NetCdf file daily timesteps max ',NetCdfMaxNrDays
!      ierr = nf90_def_dim(ncid, 'time', NetCdfMaxNrDays, dimid_time)
       ierr = nf90_def_dim(ncid, 'time', nf90_unlimited, dimid_time)
    else
!      ierr = nf90_def_dim(ncid, 'time', NetCdfMaxNrTimesteps, dimid_time)
       ierr = nf90_def_dim(ncid, 'time', nf90_unlimited, dimid_time)
    endif
    Call NetCdfCheck('NC_Prepare - After def_dim time',ierr)

    ierr = nf90_def_dim(ncid, 'id', numitem, dimid_item)
    Call NetCdfCheck('NC_Prepare - After def_dim id',ierr)
    ierr = nf90_def_dim(ncid, 'id_charlen', IdLen, dimid_chars)
    Call NetCdfCheck('NC_Prepare - After def_dim id_charlen',ierr)

    if (present(dimid_chr)) dimid_chr = dimid_chars

    time_unit = ''
    if (trim(NetCdfTimestep) .eq. 'Seconds') then
      write(time_unit,'(a,i4.4,a1,i2.2,a1,i2.2,a1,i2.2,a1,i2.2,a1,i2.2)') 'seconds since ',    &
           int(refdate/10000),'-',int(mod(refdate,10000)/100),'-',mod(refdate,100),' ',        &
           int(reftime/10000),':',int(mod(reftime,10000)/100),':',mod(reftime,100)
    elseif (trim(NetCdfTimestep) .eq. 'Minutes') then
      write(time_unit,'(a,i4.4,a1,i2.2,a1,i2.2,a1,i2.2,a1,i2.2,a1,i2.2)') 'minutes since ',    &
           int(refdate/10000),'-',int(mod(refdate,10000)/100),'-',mod(refdate,100),' ',        &
           int(reftime/10000),':',int(mod(reftime,10000)/100),':',mod(reftime,100)
    elseif (trim(NetCdfTimestep) .eq. 'Hours') then
      write(time_unit,'(a,i4.4,a1,i2.2,a1,i2.2,a1,i2.2,a1,i2.2,a1,i2.2)') 'hours since ',    &
           int(refdate/10000),'-',int(mod(refdate,10000)/100),'-',mod(refdate,100),' ',        &
           int(reftime/10000),':',int(mod(reftime,10000)/100),':',mod(reftime,100)
    elseif (trim(NetCdfTimestep) .eq. 'Days') then
      write(time_unit,'(a,i4.4,a1,i2.2,a1,i2.2,a1,i2.2,a1,i2.2,a1,i2.2)') 'days since ',    &
           int(refdate/10000),'-',int(mod(refdate,10000)/100),'-',mod(refdate,100),' ',        &
           int(reftime/10000),':',int(mod(reftime,10000)/100),':',mod(reftime,100)
    endif
!   varid_time = HisSetupVariable(ncid,'time',nf90_double,(/ dimid_time /),'time','Time',time_unit)
    varid_time = HisSetupVariable(ncid,'time',nf90_int,(/ dimid_time /),'time','Time',time_unit)

    ! Create 1D-array of location ID's
    varid_id = HisSetupVariable(ncid,'locations',nf90_char,(/ dimid_chars, dimid_item /),'id','locations','-')
! vraag Erik Pelgrim OpenDA: voeg attribuut cf_role weer toe (stond uitgecommentarieerd)
     ierr = nf90_put_att(ncid, varid_id,   'cf_role', 'timeseries_id')
     Call NetCdfCheck('NC_Prepare - After put_att cf_role',ierr)

    ! Per item, location coordinates X and Y, indexed by structure number
    if (present(xc)) then
       if (present(xc2)) then
          varid_x = HisSetupVariable(ncid,'from_x_coordinate',nf90_double,(/ dimid_item /),'x','from_X-coordinate','m',fillDble=dmiss)
          varid_x2 = HisSetupVariable(ncid,'to_x_coordinate',nf90_double,(/ dimid_item /),'x','to_X_coordinate','m',fillDble=dmiss)
       else
          varid_x = HisSetupVariable(ncid,'x_coordinate',nf90_double,(/ dimid_item /),'x','X_coordinate','m',fillDble=dmiss)
       endif
    endif
    if (present(yc)) then
       if (present(yc2)) then
          varid_y = HisSetupVariable(ncid,'from_y_coordinate',nf90_double,(/ dimid_item/),'y','from_Y_coordinate','m',fillDble=dmiss)
          varid_y2 = HisSetupVariable(ncid,'to_y_coordinate',nf90_double,(/ dimid_item /),'y','to_Y_coordinate','m',fillDble=dmiss)
       else
          varid_y = HisSetupVariable(ncid,'y_coordinate',nf90_double,(/ dimid_item /),'y','Y_coordinate','m',fillDble=dmiss)
       endif
    endif

    ! Temporary switch to data-mode to fill in the id's, locations etc
    ierr = nf90_enddef(ncid)
    Call NetCdfCheck('NC_Prepare - After enddef',ierr)

    ! Write location info
    if (numitem>0) then
       ierr = nf90_put_var(ncid, varid_id, id(1:numitem), start=(/1,1/), count = (/IdLen,numitem/))
       Call NetCdfCheck('NC_Prepare - After put_var locationid',ierr)
       if (present(xc)) then
          ierr = nf90_put_var(ncid, varid_x, xc(1:numitem), start=(/1/), count = (/numitem/))
          Call NetCdfCheck('NC_Prepare - After put_var xc',ierr)
       endif
       if (present(yc)) then
          ierr = nf90_put_var(ncid, varid_y, yc(1:numitem), start=(/1/), count = (/numitem/))
          Call NetCdfCheck('NC_Prepare - After put_var yc',ierr)
       endif
       if (present(xc2)) then
          ierr = nf90_put_var(ncid, varid_x2, xc2(1:numitem), start=(/1/), count = (/numitem/))
          Call NetCdfCheck('NC_Prepare - After put_var xc2',ierr)
       endif
       if (present(yc2)) then
          ierr = nf90_put_var(ncid, varid_y2, yc2(1:numitem), start=(/1/), count = (/numitem/))
          Call NetCdfCheck('NC_Prepare - After put_var yc2',ierr)
       endif
    endif
    ierr = nf90_redef(ncid)
    Call NetCdfCheck('NC_Prepare - After redef',ierr)
end subroutine nc_prepare



 function HisSetupVariable(ncid,var_name,var_type,dimensions,std_name,lng_name,units,fillDble, fillInt, TimeSeries, AggregationOption) result (varid)

    use netcdf

    implicit none
    integer, intent(in)               :: ncid         !< ID of the netcdf file, already opened for writing
    character(len=*), intent(in)      :: var_name     !< Variable name in the file
    integer,          intent(in)      :: var_type     !< NetCDF variable type (as defined in the netCDF API)
    character(len=*), intent(in)      :: std_name     !< Standard name attribute
    character(len=*), intent(in)      :: lng_name     !< Long name attribute
    character(len=*), intent(in)      :: units        !< Units attribute
    double precision, intent(in) , optional  :: fillDble  !< Default value representing missing: Double
    integer,          intent(in) , optional  :: fillInt   !< Default value representing missing: Int
    integer,          intent(in) , optional  :: TimeSeries!< Timeseries variable (<>0 or not (=0)
    character (len=*),intent(in) , optional  :: AggregationOption!< Aggregation option (current, average, maximum)
    integer, dimension(:), intent(in) :: dimensions   !< Vector with the number of levels in each dimension
    integer                           :: ierr
    integer                           :: varid
!   Write(*,*) ' HisSetupVariable Defining variable ',var_name
    ierr = nf90_def_var(ncid, trim(var_name), var_type, dimensions, varid)
    Call NetCdfCheck('HisSetupVar - After def_var ',ierr)
!   ierr = nf90_put_att(ncid, varid, 'standard_name', trim(std_name))   ! if present, standard_name should obey CF-convention
    ierr = nf90_put_att(ncid, varid, 'long_name', trim(lng_name))
    Call NetCdfCheck('HisSetupVar - After put_ att ',ierr)
    ierr = nf90_put_att(ncid, varid, 'units',trim(units))
    Call NetCdfCheck('HisSetupVar - After put_ att ',ierr)
    if (present(TimeSeries)) then
       if (TimeSeries .ne. 0) then
          ierr = nf90_put_att(ncid, varid, 'coordinates','locations')
          Call NetCdfCheck('HisSetupVar - After put_ att ',ierr)
       endif
    endif
    if (present(AggregationOption)) then
          ierr = nf90_put_att(ncid, varid, 'aggregation_option',trim(AggregationOption))
          Call NetCdfCheck('HisSetupVar - After put_ att ',ierr)
    endif

    select case (var_type)
    case (nf90_double)
       if (present(fillDble)) ierr = nf90_put_att(ncid, varid, '_FillValue', fillDble)
    case (nf90_int)
       if (present(fillInt)) ierr = nf90_put_att(ncid, varid, '_FillValue', fillInt)
    end select
    Call NetCdfCheck('HisSetupVar - After fill ',ierr)
 end function HisSetupVariable



  subroutine NetCdfcheck(String, status)

    use NetCdf

    integer, intent ( in)           :: status
    Character (len=*), intent ( in) :: String

    if (status /= nf90_noerr) then
       write(*,'(A,A,1X,A)') 'NetCdfCheck: ', String,trim(nf90_strerror(status))
    endif
  end subroutine NetCdfcheck


  function NetCdfName(String)  result (OutputString)

    use readlib

    integer           ilen, i
    Character (len=*) :: String
    Character (len=99) :: OutputString


    OutputString(1:) = String (1:)
    Call RplStr (OutputString, '+','_')
    Call RplStr (OutputString, '-','_')
    Call RplStr (OutputString, '=','_')
    Call RplStr (OutputString, '(','_')
    Call RplStr (OutputString, ')','_')
    Call RplStr (OutputString, '/','_')
    Call RplStr (OutputString, '\','_')
    Call RplStr (OutputString, '!','_')
    Call RplStr (OutputString, '?','_')
    Call RplStr (OutputString, '@','_')
    Call RplStr (OutputString, '#','_')
    Call RplStr (OutputString, '%','_')
    Call RplStr (OutputString, '&','_')
    Call RplStr (OutputString, '*','_')
    Call RplStr (OutputString, ',','_')
    Call RplStr (OutputString, '.','_')
    Call RplStr (OutputString, ';','_')
    Call RplStr (OutputString, ':','_')
    Call RplStr (OutputString, '[','_')
    Call RplStr (OutputString, ']','_')
    Call RplStr (OutputString, '{','_')
    Call RplStr (OutputString, '}','_')
    Call RplStr (OutputString, ' ','_')

! no trailing underscores at the end
 91 continue
    ilen = len_trim(OutputString)
    if (OutputString(ilen:ilen) .eq. '_') then
        OutputString(ilen:ilen) = ''
        goto 91
    endif

! no two double underscores in a row
 92 continue
    ilen = len_trim(OutputString)
    do i=1,ilen-1
       if (OutputString(i:i) .eq. '_' .and. OutputString(i+1:i+1) .eq. '_') then
           OutputString(i:) = OutputString(i+1:)
           goto 92
       endif
    enddo

  end function NetCdfName


end Module NetCdfData




 module nctimeseries

   use netcdf
   use netcdfdata, only : MeteoNetCdfInput
   implicit none

   private

   public :: PrecipitationSeriesId, EvaporationSeriesId, TemperatureSeriesId

   public :: GetNcTimeDimension, GetNcSeriesId, GetNcLocationDimension, GetNCLocationIds, GetNCTimeSeries, GetNCTimeSeriesById, GetNCTimestepSize
   public :: try, OpenNetCdf

   character (len=132)  PrecipitationSeriesId, EvaporationSeriesId, TemperatureSeriesId

contains
   function try(ierr) result (fail)
      logical :: fail
      integer :: ierr
      if (ierr==NF90_NOERR) then
         fail = .False.
      else
         write(*,*) 'NC-ERROR: '//nf90_strerror(ierr)
         fail = .True.
      end if
   end function try


   function OpenNetcdf(MeteoNcid, MeteoFile) result(success)

        implicit none

        logical :: success
        character(Len=*), intent(in) :: MeteoFile
        integer, intent(out)         :: MeteoNcid

        success = .not. try(nf90_open(MeteoFile, NF90_NOWRITE, MeteoNcid))
        if (.not. success) then
           MeteoNcid = -1
           Write(*,*) ' Error in opening Meteo Netcdf file'
        endif

   end function OpenNetCdf


   function GetNCTimeDimension(ncid, varname, varname2, nTim, TimeNr,  TimeUnit) result (success)
      implicit none
      logical                             :: success
      integer,                intent(in)  :: ncid
      character(len=*),       intent(in)  :: varname, varname2
      integer,                intent(in)  :: TimeNr
      integer,                intent(out) :: nTim
      character(len=*),       intent(out) :: TimeUnit
      integer :: ierr
      integer :: varid, tim_dimid

      success = .False.
      nTim = -1
      TimeUnit = ''
      ierr = nf90_inq_varid(ncid, varname2, varid)                            ! find the ID of the variable based on the id specified in the INI file
      if (ierr .ne. NF90_NOERR) ierr = nf90_inq_varid(ncid, varname, varid)   ! if error, find the ID of the variable using the default name
      if (ierr .ne. NF90_NOERR) MeteoNetcdfInput = .false.
      ierr = nf90_inq_dimid(ncid, 'time', tim_dimid)                    ! find the dim ID of the time dimension
      if (ierr .ne. NF90_NOERR) MeteoNetcdfInput = .false.
      ierr = nf90_inquire_dimension(ncid, tim_dimid, len=nTim)          ! find the length of the timeseries dimension
      if (ierr .ne. NF90_NOERR) MeteoNetcdfInput = .false.
      ierr = nf90_get_att(ncid, TimeNr,'units',TimeUnit)                ! Store the unit string of the time variable
      if (ierr .ne. NF90_NOERR) MeteoNetcdfInput = .false.
      success = .true.

   end function GetNcTimeDimension


   function GetNCTimestepSize (ncid, NrSecs, ndx0, ndx1, Date_arr, TimeUnit, TStart, Tend) result (success)

      use time_module
      use m_ec_parameters
      use m_ec_message
      use m_ec_support
      implicit none

      logical                             :: success
      integer,                intent(in)  :: ncid
      integer,                intent(in)  :: ndx0
      integer,                intent(in)  :: ndx1
      real (kind=8),          intent(in)  :: Date_arr(ndx0:ndx1)
      character(len=*),       intent(in)  :: TimeUnit
      integer,                intent(out) :: NrSecs
      real (kind=8),          intent(out) :: TStart, Tend
      integer                             :: ierr, nTim, unit
      integer                             :: varid, tim_dimid
      real                                :: RTimestepSize
      real(kind=8)                        :: ref_date, tzone

      success = .False.
      nTim = -1
      ierr = nf90_inq_dimid(ncid, 'time', tim_dimid)                    ! find the dim ID of the time dimension
      ierr = nf90_inquire_dimension(ncid, tim_dimid, len=nTim)          ! find the length of the timeseries dimension

      RTimestepSize = ( Date_arr(ndx1) - Date_arr(ndx0) ) / (ndx1 - ndx0)

!     TimeUnit = 'minutes since 1970-01-01 00:00:00.0 +0000'

! zie ec_support.f90
      !> Extracts time unit and reference date from a standard time string.
      !! ASCII example: "TIME = 0 hours since 2006-01-01 00:00:00 +00:00"
      !! NetCDF example: "minutes since 1970-01-01 00:00:00.0 +0000"
      !! also ISO_8601 is supported: 2020-11-16T07:47:33Z

       success = ecSupportTimestringToUnitAndRefdate(TimeUnit, unit, ref_date, tzone)
       if (tzone .lt. -12 .or. tzone .gt. 14) tzone = 0.0

!      Unit is day, hour, minute or seconds
! 1Feb2023 CheckBoyan FEWS vraag; -tzone instead of +tzone
       if (unit .eq. ec_second) then
           tStart = ref_date - tzone / 24.0_hp +  Date_arr(ndx0) / 86400.0_hp
           tEnd   = ref_date - tzone / 24.0_hp +  Date_arr(ndx1) / 86400.0_hp
           NrSecs = RTimestepSize
       elseif (unit .eq. ec_minute) then
           tStart = ref_date - tzone / 24.0_hp +  Date_arr(ndx0) / 1440.0_hp
           tEnd   = ref_date - tzone / 24.0_hp +  Date_arr(ndx1) / 1440.0_hp
           NrSecs = RTimestepSize * 60.0_hp
       elseif (unit .eq. ec_hour) then
           tStart = ref_date - tzone / 24.0_hp +  Date_arr(ndx0) / 24.0_hp
           tEnd   = ref_date - tzone / 24.0_hp +  Date_arr(ndx1) / 24.0_hp
           NrSecs = RTimestepSize * 60.0_hp * 24.0_hp
       elseif (unit .eq. ec_day) then
           tStart = ref_date - tzone / 24.0_hp +  Date_arr(ndx0)
           tEnd   = ref_date - tzone / 24.0_hp +  Date_arr(ndx1)
           NrSecs = RTimestepSize * 60.0_hp * 24.0_hp * 60.0_hp
       else
           call setECMessage("ec_support::ecGetTimesteps: Unable to identify the time unit.")
           return
       endif

!      convert Date_arr(1) to a yyyy-mm-dd-hh-mm-ss  StartEventDate
!      convert total period to duration dd-hh-mm-ss  EventDuration
!      determine End date                            EndEventDate

      TStart = TStart + offset_modified_jd
      TEnd   = Tend   + offset_modified_jd

!      write(*,*) ' TimeUnit', TimeUnit
!      write(*,*) ' TStart  ', TStart
!      write(*,*) ' TEnd    ', TEnd
!      write(*,*) ' unit   ', unit
!      write(*,*) ' Tunit sec/min/hour/day'
!      write(*,*)   ec_second, ec_minute, ec_hour, ec_day
!      write(*,*) ' Timestepsize ', RTimestepSize
!      write(*,*) ' nrSecs       ', NrSecs
      success = .true.

   end function GetNcTimeStepSize


   function GetNCSeriesId(ncid, varname, varname2, serienr, timenr) result (success)
      implicit none
      logical                             :: success
      integer,                intent(in)  :: ncid
      character(len=*),       intent(in)  :: varname, varname2
      integer,                intent(out) :: serienr, timenr

      integer :: ierr, nVar, nDim, nTim, nLoc, iVar
      integer :: varid, series_varid, tim_dimid, time_varid  !loc_dimid
      character(len=:), allocatable   :: attstr
      character(len=255) :: naam
      integer :: attlen

      success = .False.
      nTim = -1
      ierr = nf90_inq_varid(ncid, varname2, varid)                            ! find the ID of the variable based on the id specified in the INI file
      if (ierr .ne. NF90_NOERR) ierr = nf90_inq_varid(ncid, varname, varid)   ! if error, find the ID of the variable using the default name
      if (ierr .ne. NF90_NOERR) MeteoNetcdfInput = .false.
      ierr = nf90_inq_dimid(ncid, 'time', tim_dimid)                    ! find the dim ID of the time dimension
      if (ierr .ne. NF90_NOERR) MeteoNetcdfInput = .false.
!     ierr = nf90_inquire_dimension(ncid, loc_dimid, len=nTim)          ! find the length of the location dimension
!     if (ierr .ne. NF90_NOERR) MeteoNetcdfInput = .false.
      ierr = nf90_inquire_dimension(ncid, tim_dimid, len=nLoc)          ! find the length of the timeseries dimension
      if (ierr .ne. NF90_NOERR) MeteoNetcdfInput = .false.

      !   We require: variable ID, Time variable ID, Length of the time dimension, index of the station dimension
      if (try(nf90_inquire(ncid, nDim, nVar))) return
      series_varid = -1
      time_varid = -1
      do iVar = 1, nVar                                                                     ! Inventorize variables
         ierr = nf90_inquire_variable(ncid, iVar, name=naam)
         ierr = nf90_inquire_attribute(ncid, iVar, 'cf_role', len=attlen)
         if (ierr .ne. NF90_NOERR) then
            ! no cf_role attribute
         else
            if (series_varid < 0) then                                                         ! check if it is a series-ID
                ierr = nf90_inquire_attribute(ncid, iVar, 'cf_role', len=attlen)
                if (ierr == NF90_NOERR) then
                    allocate (character(len=attlen)::attstr)
                    attstr(:) = ''
                    ierr = nf90_get_att(ncid, iVar, 'cf_role', attstr)
!                    write(0,*) trim(naam)// ', cf-role = ', trim(attstr)
                    if (trim(attstr)=='timeseries_id') then
                       series_varid = iVar
                    end if
                    deallocate (attstr)
                end if
            end if
         end if

         if (time_varid < 0) then     ! check if it is the time-variable
             ierr = nf90_inquire_attribute(ncid, iVar, 'standard_name', len=attlen)
             if (ierr == NF90_NOERR) then
                 allocate (character(len=attlen)::attstr)
                 attstr(:) = ''
                 ierr = nf90_get_att(ncid, iVar, 'standard_name', attstr)
                 if (attstr=='time') then
                    time_varid = iVar
                    success = .true.
                 end if
                 deallocate (attstr)
             end if
         end if
      end do  ! loop over variables

      serienr = series_varid
      timenr  = time_varid

! in case no CF_role attributes found, fall back:
      if (Series_varid < 0) serienr = varid
      if (time_varid < 0) then
         write(*,*) ' WARNING: NetCdf file not CF-compliant; will search using variable names time and station_id'
         success = .false.
      endif
!     if (time_varid < 0) timenr = tim_dimid
!     find time variable via long_name
      do ivar=1,nVar
          if (time_varid < 0) then
             ierr = nf90_inquire_attribute(ncid, iVar, 'long_name', len=attlen)
             if (ierr == NF90_NOERR) then
                allocate (character(len=attlen)::attstr)
                attstr(:) = ''
                ierr = nf90_get_att(ncid, iVar, 'long_name', attstr)
                if (attstr=='time') then
                   time_varid = iVar
                end if
                deallocate (attstr)
             end if
             timenr  = time_varid
          endif
      enddo

      if (timenr .gt. 0 .and. serienr .gt. 0) MeteoNetCdfInput = .true.     ! desired time series and times available
!     success = .True.

   end function GetNCSeriesId


   function GetNCLocationDimension(ncid, varname, varname2, series_varid, NrStations) result (success)

      implicit none
      logical                             :: success
      integer,                intent(in)  :: ncid
      character(len=*),       intent(in)  :: varname, varname2
      integer,                intent(in)  :: series_varid
      integer,                intent(out) :: NrStations
      integer :: ierr, nVar, nDim, nTim, nLoc, iVar, station_varid
      integer :: varid, tim_dimid, time_varid  !loc_dimid
      integer :: tslen
      character(len=:), allocatable   :: attstr
      character(len=255) :: naam
      integer :: attlen, dimids_tsid(2)

      success = .False.
      nTim = -1
      ierr = nf90_inq_varid(ncid, varname2, varid)                            ! find the ID of the variable based on the id specified in the INI file
      if (ierr .ne. NF90_NOERR) ierr = nf90_inq_varid(ncid, varname, varid)   ! if error, find the ID of the variable using the default name
      if (ierr .ne. NF90_NOERR) MeteoNetcdfInput = .false.
      ierr = nf90_inq_dimid(ncid, 'time', tim_dimid)                    ! find the dim ID of the time dimension
      if (ierr .ne. NF90_NOERR) MeteoNetcdfInput = .false.
!     ierr = nf90_inquire_dimension(ncid, loc_dimid, len=nTim)          ! find the length of the location dimension
!     if (ierr .ne. NF90_NOERR) MeteoNetcdfInput = .false.
      ierr = nf90_inquire_dimension(ncid, tim_dimid, len=nLoc)          ! find the length of the timeseries dimension
      if (ierr .ne. NF90_NOERR) MeteoNetcdfInput = .false.

      !   We require: variable ID, Time variable ID, Length of the time dimension, index of the station dimension
      if (try(nf90_inquire(ncid, nDim, nVar))) return
      time_varid = -1
      iVar = series_varid
      ierr = nf90_inquire_variable(ncid, iVar, name=naam)
      if (ierr .ne. NF90_NOERR) then
          MeteoNetcdfInput = .false.
      endif
      ierr = nf90_inquire_attribute(ncid, iVar, 'cf_role', len=attlen)
      if (ierr .ne. NF90_NOERR) then
          ! try to find the station id parameter and dimension
          ierr = nf90_inq_varid(ncid, 'station_id', station_varid)        ! find the ID of the variable based on the default id
          if (ierr .ne. NF90_NOERR)  MeteoNetcdfInput = .false.
          ierr = nf90_inquire_variable(ncid, station_varid, dimids = dimids_tsid)
          if (ierr .ne. NF90_NOERR)  MeteoNetcdfInput = .false.
          ierr = nf90_inquire_dimension(ncid, dimids_tsid(2), len=nLoc)     ! nr of location id's
          if (ierr .ne. NF90_NOERR)  MeteoNetcdfInput = .false.
      else
          allocate (character(len=attlen)::attstr)
          attstr(:) = ''
          ierr = nf90_get_att(ncid, iVar, 'cf_role', attstr)
!          write(0,*) trim(naam)// ', cf-role = ', trim(attstr)
          if (trim(attstr)=='timeseries_id') then
             dimids_tsid = -1
             ierr = nf90_inquire_variable(ncid, series_varid, dimids = dimids_tsid)
             if (ierr .ne. NF90_NOERR) MeteoNetcdfInput = .false.
             ierr = nf90_inquire_dimension(ncid, dimids_tsid(1), len=tslen)    ! id length
             if (ierr .ne. NF90_NOERR) MeteoNetcdfInput = .false.
             ierr = nf90_inquire_dimension(ncid, dimids_tsid(2), len=nLoc)     ! nr of id's
             if (ierr .ne. NF90_NOERR) MeteoNetcdfInput = .false.
          endif
      end if

      nrStations = -1
      if (nLoc .ge. 0) nrStations = nLoc
      if (nrStations .gt. 0)  MeteoNetCdfInput = .true.     ! desired nr stations found
      Success = .true.

   end function GetNCLocationDimension


   function GetNCLocationIds(ncid, varname, varname2, series_varid, NrStations, Station_ids) result (success)
      implicit none
      logical                             :: success
      integer,                intent(in)  :: ncid
      character(len=*),       intent(in)  :: varname, varname2
      integer,                intent(in)  :: series_varid
      integer,                intent(in)  :: NrStations
      character(len=*),       intent(out) :: Station_ids(NrStations)
      integer :: ierr, nVar, nDim, nTim, nLoc, iVar, iLoc, station_VarId
      integer :: varid, tim_dimid, time_varid  !loc_dimid
      integer :: tslen
      character(len=:), allocatable   :: attstr, tsname
      character(len=255) :: naam
      integer :: attlen, dimids_tsid(2)

      success = .False.
      do iloc = 1, nrStations
         Station_Ids(iloc) = ' '
      enddo
      nTim = -1
      ierr = nf90_inq_varid(ncid, varname2, varid)                            ! find the ID of the variable based on the id specified in the INI file
      if (ierr .ne. NF90_NOERR) ierr = nf90_inq_varid(ncid, varname, varid)   ! if error, find the ID of the variable using the default name
      if (ierr .ne. NF90_NOERR) MeteoNetcdfInput = .false.
      ierr = nf90_inq_dimid(ncid, 'time', tim_dimid)                    ! find the dim ID of the time dimension
      if (ierr .ne. NF90_NOERR) MeteoNetcdfInput = .false.
!     ierr = nf90_inquire_dimension(ncid, loc_dimid, len=nTim)          ! find the length of the location dimension
!     if (ierr .ne. NF90_NOERR) MeteoNetcdfInput = .false.
      ierr = nf90_inquire_dimension(ncid, tim_dimid, len=nLoc)          ! find the length of the timeseries dimension
      if (ierr .ne. NF90_NOERR) MeteoNetcdfInput = .false.

      !   We require: variable ID, Time variable ID, Length of the time dimension, index of the station dimension
      if (try(nf90_inquire(ncid, nDim, nVar))) return
      time_varid = -1
      iVar = series_varid
      ierr = nf90_inquire_variable(ncid, iVar, name=naam)
      if (ierr .ne. NF90_NOERR) MeteoNetcdfInput = .false.
      ierr = nf90_inquire_attribute(ncid, iVar, 'cf_role', len=attlen)
      if (ierr .ne. NF90_NOERR) MeteoNetcdfInput = .false.
      ierr = nf90_inquire_attribute(ncid, iVar, 'cf_role', len=attlen)
      if (ierr .ne. NF90_NOERR) MeteoNetcdfInput = .false.

      if (ierr == NF90_NOERR) then
          allocate (character(len=attlen)::attstr)
          attstr(:) = ''
          ierr = nf90_get_att(ncid, iVar, 'cf_role', attstr)
!          write(0,*) trim(naam)// ', cf-role = ', trim(attstr)
          if (trim(attstr)=='timeseries_id') then
             dimids_tsid = -1
             ierr = nf90_inquire_variable(ncid, series_varid, dimids = dimids_tsid)
             ierr = nf90_inquire_dimension(ncid, dimids_tsid(1), len=tslen)
             ierr = nf90_inquire_dimension(ncid, dimids_tsid(2), len=nLoc)
             allocate (character(len=tslen)::tsname)
             do iloc = 1, nLoc
                tsname(:) = ''
                ierr = nf90_get_var(ncid,series_varid,tsname,start=(/1,iloc/),count=(/tslen,1/))
!               Station_Ids(iloc) (1:) = tsname(1:len_trim(tsname))
                Station_Ids(iloc) (1:) = tsname(1:index(tsname,char(0))-1)
             end do
          end if
          deallocate (attstr)
          Success = .true.
      else
          ! find series with station id's
          ierr = nf90_inq_varid(ncid, 'station_id', station_varid)        ! find the ID of the variable based on the default id
          if (ierr == NF90_NOERR) then
             dimids_tsid = -1
             ierr = nf90_inquire_variable(ncid, station_varid, dimids = dimids_tsid)
             ierr = nf90_inquire_dimension(ncid, dimids_tsid(1), len=tslen)
             ierr = nf90_inquire_dimension(ncid, dimids_tsid(2), len=nLoc)
             allocate (character(len=tslen)::tsname)
             do iloc = 1, nLoc
                tsname(:) = ''
                ierr = nf90_get_var(ncid,station_varid,tsname,start=(/1,iloc/),count=(/tslen,1/))
                Station_Ids(iloc) (1:) = tsname(1:len_trim(tsname))
             end do
          end if
      endif
      if (Station_Ids(1) .ne. ' ') MeteoNetCdfInput = .true.     ! stations found
      Success = .true.

   end function GetNCLocationIds


   function GetNCTimeSeries(ncid, varname, varname2, series_varid, location_index, ndx0, ndx1, time_arr, data_arr) result (success)
      implicit none
      logical                             :: success
      integer,                intent(in)  :: ncid
      character(len=*),       intent(in)  :: varname, varname2
      integer,                intent(in)  :: series_varId
      integer,                intent(in)  :: location_index
      integer,                intent(in)  :: ndx0
      integer,                intent(in)  :: ndx1
      real(kind=8),   dimension(:), intent(out) :: time_arr
      real(kind=8),   dimension(:), intent(out) :: data_arr
      integer :: ierr, nVar, nDim, nTim, nLoc, iVar, iLoc
      integer :: varid, tim_dimid, time_varid  !loc_dimid
      integer :: nn
      character(len=:), allocatable   :: attstr
      character(len=255) :: naam
      integer :: attlen

      success = .False.
      nTim = -1
      ierr = nf90_inq_varid(ncid, varname2, varid)                            ! find the ID of the variable based on the id specified in the INI file
      if (ierr .ne. NF90_NOERR) ierr = nf90_inq_varid(ncid, varname, varid)   ! if error, find the ID of the variable using the default name
      if (ierr .ne. NF90_NOERR) MeteoNetcdfInput = .false.
      ierr = nf90_inq_dimid(ncid, 'time', tim_dimid)                    ! find the dim ID of the time dimension
      if (ierr .ne. NF90_NOERR) MeteoNetcdfInput = .false.
!     ierr = nf90_inquire_dimension(ncid, loc_dimid, len=nTim)          ! find the length of the location dimension
!     if (ierr .ne. NF90_NOERR) MeteoNetcdfInput = .false.
      ierr = nf90_inquire_dimension(ncid, tim_dimid, len=nLoc)          ! find the length of the timeseries dimension
      if (ierr .ne. NF90_NOERR) MeteoNetcdfInput = .false.

      !   We require: variable ID, Time variable ID, Length of the time dimension, index of the station dimension
      if (try(nf90_inquire(ncid, nDim, nVar))) return
      iVar = Series_varid
      iLoc = Location_index
      time_varid = -1

      do iVar = 1, nVar                                                                     ! Inventorize variables
         ierr = nf90_inquire_variable(ncid, iVar, name=naam)
         ierr = nf90_inquire_attribute(ncid, iVar, 'cf_role', len=attlen)
         if (ierr .ne. NF90_NOERR) then
             ierr = nf90_inquire_attribute(ncid, iVar, 'long_name', len=attlen)
             if (ierr == NF90_NOERR) then
                allocate (character(len=attlen)::attstr)
                attstr(:) = ''
                ierr = nf90_get_att(ncid, iVar, 'long_name', attstr)
                if (attstr=='time') then
                   time_varid = iVar
                end if
                deallocate (attstr)
             end if
         elseif (time_varid < 0) then     ! check if it is the time-variable
             ierr = nf90_inquire_attribute(ncid, iVar, 'standard_name', len=attlen)
             if (ierr == NF90_NOERR) then
                 allocate (character(len=attlen)::attstr)
                 attstr(:) = ''
                 ierr = nf90_get_att(ncid, iVar, 'standard_name', attstr)
                 if (attstr=='time') then
                    time_varid = iVar
                 end if
                 deallocate (attstr)
             endif
         endif
      end do  ! loop over variables

      MeteoNetCdfInput = .true.
      if (time_varid .le. 0) MeteoNetCdfInput = .false.

      ! Get the data
      nn = ndx1-ndx0+1
!      write(0,*) 'Getting time, var ID = ', time_varid
      if (try(nf90_get_var(ncid, time_varid, time_arr(1:nn), start=(/ndx0/), count=(/nn/)))) return
!      write(0,*) 'Getting data, var ID = ', varid
      if (try(nf90_get_var(ncid, varid, data_arr(1:nn), start=(/iloc,ndx0/), count=(/1,nn/)))) return
      success = .True.

   end function GetNCTimeSeries


   function GetNCTimeSeriesById(ncid, varname, varname2, locname, ndx0, ndx1, time_arr, data_arr) result (success)
! not yet 'fool-proof' for non CF-compliant NC files

      implicit none
      logical                             :: success
      integer,                intent(in)  :: ncid
      character(len=*),       intent(in)  :: varname, varname2
      character(len=*),       intent(in)  :: locname
      integer,                intent(in)  :: ndx0
      integer,                intent(in)  :: ndx1
      real(kind=8),   dimension(:), intent(out) :: time_arr
      real(kind=8),   dimension(:), intent(out) :: data_arr
      integer :: ierr, nVar, nDim, nTim, nLoc, iVar, iDim, iTim, iLoc
      integer :: varid, series_varid, tim_dimid, time_varid  !loc_dimid
      integer :: tslen, nn
      character(len=:), allocatable   :: attstr, tsname
      character(len=255) :: naam
      integer :: attlen, dimids_tsid(2)

      success = .False.
      nTim = -1
      ierr = nf90_inq_varid(ncid, varname2, varid)                            ! find the ID of the variable based on the id specified in the INI file
      if (ierr .ne. NF90_NOERR) ierr = nf90_inq_varid(ncid, varname, varid)   ! if error, find the ID of the variable using the default name
      if (ierr .ne. NF90_NOERR) MeteoNetcdfInput = .false.
      ierr = nf90_inq_dimid(ncid, 'time', tim_dimid)                    ! find the dim ID of the time dimension
      if (ierr .ne. NF90_NOERR) MeteoNetcdfInput = .false.
!     ierr = nf90_inquire_dimension(ncid, loc_dimid, len=nTim)          ! find the length of the time dimension
!     if (ierr .ne. NF90_NOERR) MeteoNetcdfInput = .false.
      ierr = nf90_inquire_dimension(ncid, tim_dimid, len=nLoc)          ! find the length of the timeseries dimension
      if (ierr .ne. NF90_NOERR) MeteoNetcdfInput = .false.

      !   We require: variable ID, Time variable ID, Length of the time dimension, index of the station dimension
      if (try(nf90_inquire(ncid, nDim, nVar))) return
      series_varid = -1
      time_varid = -1
      do iVar = 1, nVar                                                                     ! Inventorize variables
         ierr = nf90_inquire_variable(ncid, iVar, name=naam)
         ierr = nf90_inquire_attribute(ncid, iVar, 'cf_role', len=attlen)
         if (series_varid < 0) then                                                         ! check if it is a series-ID
             ierr = nf90_inquire_attribute(ncid, iVar, 'cf_role', len=attlen)
             if (ierr == NF90_NOERR) then
                 allocate (character(len=attlen)::attstr)
                 attstr(:) = ''
                 ierr = nf90_get_att(ncid, iVar, 'cf_role', attstr)
!                 write(0,*) trim(naam)// ', cf-role = ', trim(attstr)
                 if (trim(attstr)=='timeseries_id') then
                    series_varid = iVar
                    dimids_tsid = -1
                    ierr = nf90_inquire_variable(ncid, series_varid, dimids = dimids_tsid)
                    ierr = nf90_inquire_dimension(ncid, dimids_tsid(1), len=tslen)
                    ierr = nf90_inquire_dimension(ncid, dimids_tsid(2), len=nLoc)
                    allocate (character(len=tslen)::tsname)
                    do iloc = 1, nLoc
                       tsname(:) = ''
                       ierr = nf90_get_var(ncid,series_varid,tsname,start=(/1,iloc/),count=(/tslen,1/))
                       if (tsname(1:index(tsname,char(0))-1)==locname) then
                          exit
                       end if
                    end do
                    if (iloc>nLoc) then           ! location found, retrieve series
!                       write(0,*) 'Location label not found'
                       return
                    else
!                       write(0,*) 'Location found: index = ',iloc
                    end if
                 end if
                 deallocate (attstr)
             end if
         end if

         if (time_varid < 0) then     ! check if it is the time-variable
             ierr = nf90_inquire_attribute(ncid, iVar, 'standard_name', len=attlen)
             if (ierr == NF90_NOERR) then
                 allocate (character(len=attlen)::attstr)
                 attstr(:) = ''
                 ierr = nf90_get_att(ncid, iVar, 'standard_name', attstr)
                 if (attstr=='time') then
                    time_varid = iVar
                 end if
                 deallocate (attstr)
             end if
         end if
      end do  ! loop over variables

      ! Get the data
      nn = ndx1-ndx0+1
!      write(0,*) 'Getting time, var ID = ', time_varid
      if (try(nf90_get_var(ncid, time_varid, time_arr(1:nn), start=(/ndx0/), count=(/nn/)))) return
!      write(0,*) 'Getting data, var ID = ', varid
      if (try(nf90_get_var(ncid, varid, data_arr(1:nn), start=(/iloc,ndx0/), count=(/1,nn/)))) return
      success = .True.

   end function GetNCTimeSeriesById

end module nctimeseries
