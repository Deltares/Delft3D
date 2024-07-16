      module flow_in_datools
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow
c
c Programmer:         S.Hummel
c
c Module:             flow_in_datools (flow running in data tools env.)
c
c Module description: This module facilitates computing a 'time slice'
c                     of a model schematization, instead of a the
c                     full model schematization computation period.
c                     The module corrects the sobeksim 'time' variable,
c                     that is used when retreiving values from the
c                     TABLE array.
c                     It uses the sobeksim 'itim(2)' variable to check
c                     'time' variable is performed for a valid run.
c
c Example / Detailed explanation:
c
c                    The MDA-file contains the computation period
c
c      jan 1st, 00h:00                        jan 1st, 12h:00
c                 |--------------------------------------|
c                    with a DeltaT of 20 minutes (36 steps).
c
c                    The DATools computation splits this for example
c                    into 3 periods of 4 hours:
c
c      jan 1st, 00h:00       04h:00       08h:00       12h:00
c                 |------------|------------|------------|
c
c                    The DATool-wrapper does this by adjusting the
c                    the MDA-file, i.e. by setting the start time
c                    in and number of steps to resp.:
c                            jan 1st, 00h:00,   12 steps
c                            jan 1st, 04h:00,   12 steps
c                            jan 1st, 08h:00,   12 steps
c
c                    In this way, the model can simply stop
c                    at 04h:00, let DATools disturb the computed
c                    state in restart file, restart at 04h:00,
c                    stop at 08h:00, etcetera.
c
c                    If DATools also disturbs boundary conditions,
c                    etc., it inserts the disturbed values into the
c                    TABLE arrays according to the adjusted time frame.
c                    So if ALL time dependent boundary conditions,
c                    discharges, etc., are disturbed by DATools,
c                    the time sliced computation will work perfectly OK
c                    by simply adjusting the time frame in the MDA-file.
c                    
c                    However, if the MDA-file containing time series
c                    that are not disturbed/inserted by DATools, their
c                    values in the TABLE-array are still set according
c                    to the original time frame. So in that case, a
c                    'time offset' is need for accessing the
c                    TABLE-array. This offset, stored in the
c                    variable da_seconds_from_org_start, is read from
c                    a 'slice start time' file.
c
c                    If the file is present, it will be parsed and its
c                    timing info will be parsed. In that case
c                    the logical function da_running_in_da_tools() will
c                    return .true.
c
c                    If the file not present, the logical function
c                    da_running_in_da_tools() will return .false.
c                        
c=======================================================================
c
c     module variables
c
c=======================================================================
c     
      implicit none
c     
c     Hide module variables and private functions
c
      private
c     
c     Export the three public functions
c
      public :: da_running_in_da_tools,
     &          da_check_model_start_time, 
     &          da_get_seconds_from_org_start
c     
c     Name of file containing start of time slice
c     
      character(Len=20),
     +         parameter :: startOfTimeSliceFile = './sliceStartTime'
c     
c     Sobek representation of start of time slice
c     
      integer, dimension(2) :: da_itim
c     
c     Number of seconds from the start in the original,
c     non-time-sliced mda-file
c     
      integer, parameter :: not_initialized = - 99
      integer, parameter :: not_running     = -111
      integer :: da_seconds_from_org_start = not_initialized
c     
c     
c     Module log file handle ( unique lun (e.g. 6543): do logging,
c                                                < 0 : no logging )
      integer            :: log_file_handle = 6543
c     Module log file name
      character(len=25), 
     &         parameter :: log_file_name = 'flow-in-datools-log.txt'
c
c
c=======================================================================
c
c     module functions / routines
c
c=======================================================================
c     
      contains
c     
c=======================================================================
c     Check if Flow in running in data tools
c=======================================================================
      function da_running_in_da_tools() result(running_in_datools)
c     
c     return value: true if running in DA-TOOLS
c     
      logical :: running_in_datools
c     
c     locals
c     
      integer :: year, month, day, hour, minute, second
      integer :: seconds_from_org_in_file
c     
c     body
c     
      running_in_datools = .false.
      da_itim = 0
c     
      if ( da_seconds_from_org_start == not_initialized ) then
c
         da_seconds_from_org_start = not_running
c
         if ( daReadDaToolsTimeStamp(year,month ,day   ,
     &                               hour,minute,second,
     &                               seconds_from_org_in_file) ) then
c
            da_itim(1) = year * 10000 + month * 100 + day
            da_itim(2) = hour * 1000000 + minute * 10000 + second
c
            da_seconds_from_org_start = seconds_from_org_in_file
c
            if ( log_file_handle > 0 ) then
               open(log_file_handle, file  = log_file_name,
     +                               access= 'append')
            endif
         endif
      endif
c     
      if ( da_seconds_from_org_start /= not_running ) then
          running_in_datools = .true.
      endif
c     
      end function da_running_in_da_tools
c     
c     
c=======================================================================
c     Adjust actual model time when running in DATools-wrapper
c=======================================================================
      function da_check_model_start_time(itim, juer) result(success)
      implicit none
      include '..\include\errcod.i'
c
c     return value: success = .true. if start time is OK
      logical :: success
c
c     arguments
c
c     time from mda-file
      integer, dimension(2), intent(inout) :: itim
c     log file handle
      integer,               intent(in)    :: juer
c
c     body

      success = .false.

      if ( da_seconds_from_org_start /= not_running ) then
          if ( log_file_handle > 0 ) then
             write(log_file_handle, '(A,I,A,I,A,I,A,I)')
     &           'Check itim == da_itim ',    itim(1), ' ',    itim(2),
     &                             ' == ', da_itim(1), ' ', da_itim(2)
          endif

          if ( itim(1) == da_itim(1) .and. itim(2) == da_itim(2) ) then
             success = .true.
          else
             if ( log_file_handle > 0 ) then
                write(log_file_handle, '(A)') 'CHECK NOT OK'
             endif

             call ERROR (juer,'Time Slice start time inconsistency',
     +                        efltsi , fatal )
          endif
      endif
c
      end function da_check_model_start_time
c     
c
c=======================================================================
c     Get the number of seconds from the start in the original,
c                                               non-time-sliced mda-file
c=======================================================================
      function da_get_seconds_from_org_start() result(seconds)
c
c     return value: 
c
      integer :: seconds
c
c     body
      seconds = 0
      if ( da_seconds_from_org_start /= not_running ) then
          if ( log_file_handle > 0 ) then
             write(log_file_handle, '(A,I,A,I,A,I,A,I)')
     &           'Seconds from start ', da_seconds_from_org_start
          endif
          seconds = da_seconds_from_org_start
      endif
c
      end function da_get_seconds_from_org_start
c
c     
c=======================================================================
c    Check if DATools-wrapper has put time slice file
c=======================================================================
      function daReadDaToolsTimeStamp(
     &                     year,month ,day   ,
     &                     hour,minute,second,
     &                     seconds_from_org_in_file) result(hasBeenRead)
      implicit none
c
c     return value
c
      logical :: hasBeenRead
c
c     arguments
c
      integer, intent(out) :: year, month , day   ,
     &                        hour, minute, second
      integer, intent(out) :: seconds_from_org_in_file
c
c     locals
c
      integer          , parameter :: fileHandle = 2143
      integer                      :: ioStatus
      character(Len=80)            :: dummyLine
c
c     body
c     if time slice file is present, parse it. Example of the file:
c        Start of uncertainty time slice (yy mm dd hh mm ss)
c        1995 1 1 20 0 0
c        Start of uncertainty time slice (in seconds from start of original run)
c        72000
c
      hasBeenRead = .false.
      year=0 ; month =0 ; day   =0
      hour=0 ; minute=0 ; second=0
      seconds_from_org_in_file  =0
      open(fileHandle, file=startOfTimeSliceFile,
     +                 status='old', iostat=ioStatus)
      if ( ioStatus == 0 ) then
c         skip first line (explanation line)
          read(fileHandle, '(A80)', iostat=ioStatus) dummyLine
c         read line with yy mm dd hh mm ss
          read(fileHandle, * , iostat=ioStatus)
     &                           year, month , day   ,
     &                           hour, minute, second
c         skip second line (explanation line)
          read(fileHandle, '(A80)', iostat=ioStatus) dummyLine
c         read line with yy mm dd hh mm ss
          read(fileHandle, * , iostat=ioStatus) seconds_from_org_in_file
          if ( ioStatus == 0 ) then
              hasBeenRead = .true.
          endif
      endif
c
      end function daReadDaToolsTimeStamp
c
c
c
      end module flow_in_datools
