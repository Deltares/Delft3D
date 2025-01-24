module flow_in_datools
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow
!
! Programmer:         S.Hummel
!
! Module:             flow_in_datools (flow running in data tools env.)
!
! Module description: This module facilitates computing a 'time slice'
!                     of a model schematization, instead of a the
!                     full model schematization computation period.
!                     The module corrects the sobeksim 'time' variable,
!                     that is used when retreiving values from the
!                     TABLE array.
!                     It uses the sobeksim 'itim(2)' variable to check
!                     'time' variable is performed for a valid run.
!
! Example / Detailed explanation:
!
!                    The MDA-file contains the computation period
!
!      jan 1st, 00h:00                        jan 1st, 12h:00
!                 |--------------------------------------|
!                    with a DeltaT of 20 minutes (36 steps).
!
!                    The DATools computation splits this for example
!                    into 3 periods of 4 hours:
!
!      jan 1st, 00h:00       04h:00       08h:00       12h:00
!                 |------------|------------|------------|
!
!                    The DATool-wrapper does this by adjusting the
!                    the MDA-file, i.e. by setting the start time
!                    in and number of steps to resp.:
!                            jan 1st, 00h:00,   12 steps
!                            jan 1st, 04h:00,   12 steps
!                            jan 1st, 08h:00,   12 steps
!
!                    In this way, the model can simply stop
!                    at 04h:00, let DATools disturb the computed
!                    state in restart file, restart at 04h:00,
!                    stop at 08h:00, etcetera.
!
!                    If DATools also disturbs boundary conditions,
!                    etc., it inserts the disturbed values into the
!                    TABLE arrays according to the adjusted time frame.
!                    So if ALL time dependent boundary conditions,
!                    discharges, etc., are disturbed by DATools,
!                    the time sliced computation will work perfectly OK
!                    by simply adjusting the time frame in the MDA-file.
!
!                    However, if the MDA-file containing time series
!                    that are not disturbed/inserted by DATools, their
!                    values in the TABLE-array are still set according
!                    to the original time frame. So in that case, a
!                    'time offset' is need for accessing the
!                    TABLE-array. This offset, stored in the
!                    variable da_seconds_from_org_start, is read from
!                    a 'slice start time' file.
!
!                    If the file is present, it will be parsed and its
!                    timing info will be parsed. In that case
!                    the logical function da_running_in_da_tools() will
!                    return .true.
!
!                    If the file not present, the logical function
!                    da_running_in_da_tools() will return .false.
!
!=======================================================================
!
!     module variables
!
!=======================================================================
!
   implicit none
!
!     Hide module variables and private functions
!
   private
!
!     Export the three public functions
!
   public :: da_running_in_da_tools,&
   &da_check_model_start_time,&
   &da_get_seconds_from_org_start
!
!     Name of file containing start of time slice
!
   character(Len=20),&
   &parameter :: startOfTimeSliceFile = './sliceStartTime'
!
!     Sobek representation of start of time slice
!
   integer, dimension(2) :: da_itim
!
!     Number of seconds from the start in the original,
!     non-time-sliced mda-file
!
   integer, parameter :: not_initialized = - 99
   integer, parameter :: not_running     = -111
   integer :: da_seconds_from_org_start = not_initialized
!
!
!     Module log file handle ( unique lun (e.g. 6543): do logging,
!                                                < 0 : no logging )
   integer            :: log_file_handle = 6543
!     Module log file name
   character(len=25),&
   &parameter :: log_file_name = 'flow-in-datools-log.txt'
!
!
!=======================================================================
!
!     module functions / routines
!
!=======================================================================
!
contains
!
!=======================================================================
!     Check if Flow in running in data tools
!=======================================================================
   function da_running_in_da_tools() result(running_in_datools)
!
!     return value: true if running in DA-TOOLS
!
      logical :: running_in_datools
!
!     locals
!
      integer :: year, month, day, hour, minute, second
      integer :: seconds_from_org_in_file
!
!     body
!
      running_in_datools = .false.
      da_itim = 0
!
      if ( da_seconds_from_org_start == not_initialized ) then
!
         da_seconds_from_org_start = not_running
!
         if ( daReadDaToolsTimeStamp(year,month ,day   ,&
         &hour,minute,second,&
         &seconds_from_org_in_file) ) then
!
            da_itim(1) = year * 10000 + month * 100 + day
            da_itim(2) = hour * 1000000 + minute * 10000 + second
!
            da_seconds_from_org_start = seconds_from_org_in_file
!
            if ( log_file_handle > 0 ) then
               open(log_file_handle, file  = log_file_name,&
               &access= 'append')
            endif
         endif
      endif
!
      if ( da_seconds_from_org_start /= not_running ) then
         running_in_datools = .true.
      endif
!
   end function da_running_in_da_tools
!
!
!=======================================================================
!     Adjust actual model time when running in DATools-wrapper
!=======================================================================
   function da_check_model_start_time(itim, juer) result(success)
      implicit none
      include '..\include\errcod.i'
!
!     return value: success = .true. if start time is OK
      logical :: success
!
!     arguments
!
!     time from mda-file
      integer, dimension(2), intent(inout) :: itim
!     log file handle
      integer,               intent(in)    :: juer
!
!     body

      success = .false.

      if ( da_seconds_from_org_start /= not_running ) then
         if ( log_file_handle > 0 ) then
            write(log_file_handle, '(A,I,A,I,A,I,A,I)')&
            &'Check itim == da_itim ',    itim(1), ' ',    itim(2),&
            &' == ', da_itim(1), ' ', da_itim(2)
         endif

         if ( itim(1) == da_itim(1) .and. itim(2) == da_itim(2) ) then
            success = .true.
         else
            if ( log_file_handle > 0 ) then
               write(log_file_handle, '(A)') 'CHECK NOT OK'
            endif

            call ERROR (juer,'Time Slice start time inconsistency',&
            &efltsi , fatal )
         endif
      endif
!
   end function da_check_model_start_time
!
!
!=======================================================================
!     Get the number of seconds from the start in the original,
!                                               non-time-sliced mda-file
!=======================================================================
   function da_get_seconds_from_org_start() result(seconds)
!
!     return value:
!
      integer :: seconds
!
!     body
      seconds = 0
      if ( da_seconds_from_org_start /= not_running ) then
         if ( log_file_handle > 0 ) then
            write(log_file_handle, '(A,I,A,I,A,I,A,I)')&
            &'Seconds from start ', da_seconds_from_org_start
         endif
         seconds = da_seconds_from_org_start
      endif
!
   end function da_get_seconds_from_org_start
!
!
!=======================================================================
!    Check if DATools-wrapper has put time slice file
!=======================================================================
   function daReadDaToolsTimeStamp(&
   &year,month ,day   ,&
   &hour,minute,second,&
   &seconds_from_org_in_file) result(hasBeenRead)
      implicit none
!
!     return value
!
      logical :: hasBeenRead
!
!     arguments
!
      integer, intent(out) :: year, month , day   ,&
      &hour, minute, second
      integer, intent(out) :: seconds_from_org_in_file
!
!     locals
!
      integer          , parameter :: fileHandle = 2143
      integer                      :: ioStatus
      character(Len=80)            :: dummyLine
!
!     body
!     if time slice file is present, parse it. Example of the file:
!        Start of uncertainty time slice (yy mm dd hh mm ss)
!        1995 1 1 20 0 0
!        Start of uncertainty time slice (in seconds from start of original run)
!        72000
!
      hasBeenRead = .false.
      year=0 ; month =0 ; day   =0
      hour=0 ; minute=0 ; second=0
      seconds_from_org_in_file  =0
      open(fileHandle, file=startOfTimeSliceFile,&
      &status='old', iostat=ioStatus)
      if ( ioStatus == 0 ) then
!         skip first line (explanation line)
         read(fileHandle, '(A80)', iostat=ioStatus) dummyLine
!         read line with yy mm dd hh mm ss
         read(fileHandle, * , iostat=ioStatus)&
         &year, month , day   ,&
         &hour, minute, second
!         skip second line (explanation line)
         read(fileHandle, '(A80)', iostat=ioStatus) dummyLine
!         read line with yy mm dd hh mm ss
         read(fileHandle, * , iostat=ioStatus) seconds_from_org_in_file
         if ( ioStatus == 0 ) then
            hasBeenRead = .true.
         endif
      endif
!
   end function daReadDaToolsTimeStamp
!
!
!
end module flow_in_datools
