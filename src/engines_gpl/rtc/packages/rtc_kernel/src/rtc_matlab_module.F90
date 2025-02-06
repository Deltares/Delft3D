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

module rtc_matlab_module
!-------------------------------------------------------------------------------
!  $Id: rtc_matlab_module.F90 57584 2018-08-23 07:52:01Z zeekant $
!  $HeadURL: https://repos.deltares.nl/repos/ds/trunk/src/engines/rtc/packages/rtc_kernel/src/rtc_matlab_module.F90 $
!!--module description----------------------------------------------------------
!
! This module implements the communication channel between the RTC module and
! MATLAB via shared memory DelftIO.
!
!!--module declarations---------------------------------------------------------
use dio_plt_rw, only: DioMaxLocLen, DioPltType, Dio_Var_Integer, Dio_Var_Double, DioPltDefine, DioPltGetDataset, DioPltReadValues, DioPltGetValues, DioPltDestroy, DioPltPut
private

public rtc_matlab_type
public rtc_matlab_instance
!
public rtc_matlab_module_info
public start_matlab
public send_inputpar_names
public send_outputpar_names
public send_inputpar_values
public receive_outputpar_values
public stop_matlab
!
type rtc_matlab_type
    private
    integer :: status = -999
    type(DioPltType)              :: M2R_sig
    type(DioPltType)              :: R2M_sig
    type(DioPltType)              :: R2M_in
    type(DioPltType)              :: M2R_out
    type(DioPltType)              :: R2M_out ! only during initialization phase
    character(256)                :: channel
    !
    integer                                            :: NInputVars
    integer                                            :: NOutputVars
    character(len=DioMaxLocLen), dimension(:), pointer :: InputVarName
    character(len=DioMaxLocLen), dimension(:), pointer :: OutputVarName
    double precision           , dimension(:,:), pointer :: InputVarVal
    double precision           , dimension(:,:), pointer :: OutputVarVal
end type rtc_matlab_type
!
type(rtc_matlab_type), save :: rtc_matlab_instance

contains
!
!
!
!==============================================================================
subroutine rtc_matlab_module_info(messages)
    use message_module
    !
    type(message_stack) :: messages
    !
    call addmessage(messages,'$Id: rtc_matlab_module.F90 57584 2018-08-23 07:52:01Z zeekant $')
    call addmessage(messages,'$URL: https://repos.deltares.nl/repos/ds/trunk/src/engines/rtc/packages/rtc_kernel/src/rtc_matlab_module.F90 $')
end subroutine rtc_matlab_module_info
!
!
!
!==============================================================================
function start_matlab(this,rtcfolder,diofolder,userscript,matlabexec,channel,lundia,matdbg) result (istat)
!!--description-----------------------------------------------------------------
!
!    Function: - Start MATLAB and initialize communication
!
!!--declarations----------------------------------------------------------------
    implicit none
    !
    ! Call variables
    !
    type(rtc_matlab_type)    , intent(out)   :: this
    character(*)             , intent(in)    :: rtcfolder
    character(*)             , intent(in)    :: diofolder
    character(*)             , intent(in)    :: userscript
    character(*)             , intent(in)    :: matlabexec
    character(*)             , intent(in)    :: channel
    integer                  , intent(in)    :: lundia !< logical unit for debug/error output
    integer                  , intent(in)    :: matdbg
    integer                                  :: istat
    !
    ! Local variables
    !
    character(256)                            :: userfolder
    character(256)                            :: userfile
    character(256)                            :: matlabdiary
    character(256)                            :: startscript
    character(256)                            :: command
    character(256)                            :: msg
    integer                                   :: unit
    integer                                   :: i
    !
    integer                       :: luntmp
    integer, external             :: newunit
    logical                       :: ext
    character(20), dimension(1)   :: vals
    character(20), dimension(1)   :: locs
!
!! executable statements -------------------------------------------------------
!
    unit = 22 ! f2008_newunit
    !
    istat = 0
    if (matdbg>0) write(lundia,'(A)') 'Entering START_MATLAB'
    !
    ! Split userscript into userfolder and userfile
    !
    i = index(userscript,'/',back=.true.)
    i = max(i,index(userscript,'\',back=.true.))
    if (i/=0) then
       userfolder = userscript(1:i-1)
       userfile = userscript(i+1:)
    else
       userfolder = ''
       userfile = userscript
    endif
    i = index(userfile,'.')
    if (i/=0) then
       userfile = userfile(1:i-1)
    endif
    !
    ! Check if MATLAB exists
    !
    inquire(file=matlabexec, exist=ext)
    if (.not.ext) then
       write(lundia,'(3A)') "The MATLAB executable '",trim(matlabexec),"' does not exist. Check input field MatlabExec."
       istat = -1
       return
    endif
    !
    ! Check if old MATLAB diary file exists
    !
    matlabdiary = 'rtc_matlab_log.txt'
    inquire(file=matlabdiary, exist=ext)
    if (ext) then
       open(unit=unit, file=matlabdiary, status='old', action='read')
       close(unit=unit, status='delete')
    endif
    !
    ! Write MATLAB startup script in CMTWORK directory. This is where MATLAB initially starts.
    !
    startscript = 'start_rtc.m'
    open(unit=unit, file=startscript, action='write', iostat=istat)
    if (istat /= 0) then
       ! write message to message stack
       return
    endif
    write(unit, '(A)') '%MATLAB startup script created by RTC module'
    if (matdbg>0) then
       write(unit, '(3A)') 'diary(''',trim(matlabdiary),''')'
    endif
    write(unit, '(3A)') 'addpath(''',trim(rtcfolder),''') % make sure that generic MATLAB RTC tools can be found'
    write(unit, '(3A)') 'addpath(''',trim(diofolder),''') % make sure that generic MATLAB DelftIO tools can be found'
    write(unit, '(A)')  'rtc_communicator(''errorfile'',''rtc_matlab_error.txt'') % in case of error use this file in the current directory for error message'
    if (userfolder /= '') then
       write(unit, '(3A)') 'cd(''',trim(userfolder),''') % move to directory of user defined script'
    endif
    write(unit, '(5A,I0,A)') 'rtc_communicator(''channel'',''',trim(channel), &
                        ''',''userscript'',''',trim(userfile), &
                        ''',''debug'',',matdbg,') % start MATLAB RTC tool'
    close(unit)
    !
    ! Start MATLAB
    !
    command = trim(matlabexec) // ' -nosplash -minimize -r ' // startscript(1:len_trim(startscript)-2)
#ifdef HAVE_CONFIG_H
    istat = -1
    write(lundia,'(a)') 'Starting MATLAB not implemented for Linux'
#else
    call f2008_execute_command_line(command, lwait=.false., cmdstat=istat, cmdmsg=msg)
#endif
    if (istat<0) then
       ! -1: processor does not support command line execution
       ! -2: processor does not support asynchronous execution
       return
    elseif (istat>0) then
       ! an error condition occurred: display cmdmsg
       return
    endif
    !
    ! Open signal stream from MATLAB to RTC module.
    !
    this%M2R_sig = DioPltGetDataset(trim(channel) // '.MATLAB2RTC.signal.shm')
    this%M2R_sig%ds%curDataIndex = 1 ! Default value of curDataIndex is 0,
                                     ! which results in out of bounds when
                                     ! reading data.
    !
    ! Open signal stream from RTC module to MATLAB.
    !
    vals(1) = 'Value'
    locs(1) = 'Signal'
    this%R2M_sig = DioPltDefine(trim(channel) // '.RTC2MATLAB.signal.shm', Dio_Var_Integer,vals,locs)
    !
    ! Send communication test code to MATLAB.
    !
    call send_signal(this%R2M_sig,123)
    !
    ! Obtain confirmation.
    !
    istat = receive_signal(this%M2R_sig)
    if (matdbg>0) write(lundia,'(A,I0)') 'Receiving from MATLAB confirmation signal: ',istat
    !
    this%channel = channel
    this%status = 0
    istat = 0
end function start_matlab
!
!
!
!==============================================================================
subroutine send_inputpar_names(this,lundia,matdbg)
!!--description-----------------------------------------------------------------
!
!    Function: - Send names of MATLAB input parameters
!
!!--declarations----------------------------------------------------------------
    implicit none
    !
    ! Call variables
    !
    type(rtc_matlab_type)    , intent(inout) :: this
    integer                  , intent(in)    :: lundia !< logical unit for debug/error output
    integer                  , intent(in)    :: matdbg
    !
    ! Local variables
    !
    character(256)                            :: userfolder
    character(256)                            :: userfile
    character(256)                            :: startscript
    character(256)                            :: command
    character(256)                            :: msg
    integer                                   :: unit
    integer                                   :: i
    !
    integer                       :: istat
    character(20), dimension(1)   :: vals
!
!! executable statements -------------------------------------------------------
!
    if (matdbg>0) write(lundia,'(A)') 'Entering SEND_INPUTPAR_NAMES'
    !
    call ProcessMatlabInputVars(this,0,1) ! count number of parameters
    call ProcessMatlabInputVars(this,1,1) ! get parameter names
    !
    ! Send names of variables to be provided by RTC.
    !
    this%R2M_in = DioPltDefine(trim(this%channel) // '.RTC2MATLAB.shm', Dio_Var_Double,vals,rtc_matlab_instance%InputVarName)
    !
    ! Obtain confirmation.
    !
    istat = receive_signal(this%M2R_sig)
    if (matdbg>0) write(lundia,'(A,I0)') 'Receiving from MATLAB confirmation signal: ',istat
end subroutine send_inputpar_names
!
!
!
!==============================================================================
subroutine send_outputpar_names(this,lundia,matdbg)
!!--description-----------------------------------------------------------------
!
!    Function: - Send names of MATLAB output parameters
!
!!--declarations----------------------------------------------------------------
    implicit none
    !
    ! Call variables
    !
    type(rtc_matlab_type)    , intent(inout) :: this
    integer                  , intent(in)    :: lundia !< logical unit for debug/error output
    integer                  , intent(in)    :: matdbg
    !
    ! Local variables
    !
    character(256)                            :: userfolder
    character(256)                            :: userfile
    character(256)                            :: startscript
    character(256)                            :: command
    character(256)                            :: msg
    integer                                   :: unit
    integer                                   :: i
    !
    integer                       :: istat
    character(20), dimension(1)   :: vals
!
!! executable statements -------------------------------------------------------
!
    if (matdbg>0) write(lundia,'(A)') 'Entering SEND_OUTPUTPAR_NAMES'
    !
    call ProcessMatlabOutputVars(this,0,matdbg,lundia) ! count number of parameters
    call ProcessMatlabOutputVars(this,1,matdbg,lundia) ! get parameter names
    !
    ! Send name of user script and names of variables expected from MATLAB.
    !
    this%R2M_out = DioPltDefine(trim(this%channel) // '.RTC2MATLAB.requested.shm', Dio_Var_Double,vals,rtc_matlab_instance%OutputVarName)
    !
    ! Obtain confirmation.
    !
    istat = receive_signal(this%M2R_sig)
    if (matdbg>0) write(lundia,'(A,I0)') 'Receiving from MATLAB confirmation signal: ',istat
    !
    ! Delete R2M_out stream.
    !
    call DioPltDestroy(this%R2M_out)
    !
    ! Open stream for receiving values from MATLAB.
    !
    this%M2R_out = DioPltGetDataset(trim(this%channel) // '.MATLAB2RTC.shm')
    this%M2R_out%ds%curDataIndex = 1 ! Default value of curDataIndex is 0,
                                     ! which results in out of bounds when
                                     ! reading data.
end subroutine send_outputpar_names
!
!
!
!==============================================================================
subroutine send_inputpar_values(this,IDUM,lundia,matdbg)
!!--description-----------------------------------------------------------------
!
!    Function: - Send data values to MATLAB
!                This will trigger the execution of the user defined MATLAB function
!                Obtain values from MATLAB
!
!!--declarations----------------------------------------------------------------
    implicit none
    !
    ! Call variables
    !
    type(rtc_matlab_type)    , intent(inout) :: this
    integer                  , intent(in)    :: IDUM
    integer                  , intent(in)    :: lundia !< logical unit for debug/error output
    integer                  , intent(in)    :: matdbg
    !
    ! Local variables
    !
    ! NONE
!
!! executable statements -------------------------------------------------------
!
    if (matdbg>0) write(lundia,'(A)') 'Entering SEND_INPUTPAR_VALUES'
    !
    call ProcessMatlabInputVars(rtc_matlab_instance,2,IDUM) ! get values from RTC kernel
    !
    ! Send time step signal to MATLAB.
    !
    call send_signal(this%R2M_sig,this%status+1)
    !
    ! Send data values to MATLAB.
    !
    !dioplt_write(RTC_COMM_.R2M_in,InVals(:,t)');
    if (matdbg>0) then
       write(lundia,'(A)') 'Sending data:'
       write(lundia,*) this%InputVarVal
    endif
    call DioPltPut(this%R2M_in,this%InputVarVal)
    if (matdbg>0) write(lundia,'(A)') 'The data has been sent.'
end subroutine send_inputpar_values
!
!
!
!==============================================================================
function receive_outputpar_values(this,lundia,matdbg) result (istat)
!!--description-----------------------------------------------------------------
!
!    Function: - Obtain values from MATLAB
!
!!--declarations----------------------------------------------------------------
    implicit none
    !
    ! Call variables
    !
    type(rtc_matlab_type)    , intent(inout) :: this
    integer                  , intent(in)    :: lundia !< logical unit for debug/error output
    integer                  , intent(in)    :: matdbg
    integer                                  :: istat
    !
    ! Local variables
    !
    ! NONE
!
!! executable statements -------------------------------------------------------
!
    if (matdbg>0) write(lundia,'(A)') 'Entering RECEIVE_OUTPUTPAR_VALUES'
    !
    ! Get status.
    !
    !MatlabStatus = dioplt_read(RTC_COMM_.M2R_sig);
    istat = receive_signal(this%M2R_sig)
    if (matdbg>0) write(lundia,'(A,I0)') 'Receiving from MATLAB confirmation signal: ',istat
    if (istat<0) then
        return
    endif
    !
    ! Receive data values from MATLAB.
    !
    !OutVals(:,t) = dioplt_read(RTC_COMM_.M2R_out);
    this%OutputVarVal = -999.0d0
    if (matdbg>0) write(lundia,'(A)') 'Waiting to receive data from MATLAB ...'
    if (.not.DioPltReadValues(Dio_Var_Double,this%M2R_out)) then
       ! error reading data from data stream
    elseif (.not.DioPltGetValues(this%M2R_out,this%OutputVarVal)) then
       ! error copying data to dvals
    endif
    if (matdbg>0) then
       write(lundia,'(A)') 'The following data had been received:'
       write(lundia,*) this%OutputVarVal
    endif
    !
    this%status = this%status+1
    !
    call ProcessMatlabOutputVars(rtc_matlab_instance,2, matdbg, lundia) ! set values
end function receive_outputpar_values
!
!
!
!==============================================================================
function stop_matlab(this,lundia,matdbg) result (istat)
!!--description-----------------------------------------------------------------
!
!    Function: - Finalize communication and stop MATLAB
!
!!--declarations----------------------------------------------------------------
    implicit none
    !
    ! Call variables
    !
    type(rtc_matlab_type)    , intent(inout) :: this
    integer                  , intent(in)    :: lundia !< logical unit for debug/error output
    integer                  , intent(in)    :: matdbg
    integer                                  :: istat
    !
    ! Local variables
    !
!
!! executable statements -------------------------------------------------------
!
    istat = 0
    if (matdbg>0) write(lundia,'(A)') 'Entering STOP_MATLAB'
    !
    ! Send stop signal to MATLAB.
    !
    call send_signal(this%R2M_sig,-1)
    !
    this%status = -1
end function stop_matlab
!
!
!
#ifndef HAVE_CONFIG_H

subroutine f2008_execute_command_line(command, lwait, cmdstat, cmdmsg)
!!--description-----------------------------------------------------------------
!
!    Function: - Placeholder for FORTRAN 2008 standardized
!                EXECUTE_COMMAND_LINE routine.
!
!!--declarations----------------------------------------------------------------
    use kernel32
    implicit none
    !
    ! Call variables
    !
    character(*)             , intent(in)    :: command
    logical, optional        , intent(in)    :: lwait
    integer, optional        , intent(out)   :: cmdstat
    character(*), optional   , intent(inout) :: cmdmsg
    !
    ! Local variables
    !
    type (T_STARTUPINFO) :: StartupInfo
    type (T_PROCESS_INFORMATION) :: ProcessInfo
    integer ret
!
!! executable statements -------------------------------------------------------
!

    ! Initialize StartupInfo - we won't use any of its fields
    StartupInfo = T_STARTUPINFO(SIZEOF(StartupInfo), &
              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

    ! Create a new process to run the command.
    ret = CreateProcess (NULL,  & ! Application Name
	    command, &  ! Command line
	    NULL, &
	    NULL, &
	    TRUE, & ! InheritHandles
	    0, & ! CreationFlags
	    NULL, & ! Environment variables
	    NULL, & ! Current directory
	    StartupInfo, &
	    ProcessInfo)

    if (ret == 0) then
       ret = GetLastError ()
       if (present(cmdstat)) then
          cmdstat = ret
       else
          write(*,'(A,I0)') 'Create process failed with error ',ret
          ret = 2/0 ! trigger exception
       endif
       if (present(cmdmsg)) write(cmdmsg,'(A,I0)') 'Create process failed with error ',ret
    else
       ! CreateProcess succeeded.  Wait for the process to finish
       !ret = WaitForSingleObject (ProcessInfo%hProcess, INFINITE)
       ! Close handles, otherwise resources are lost
       ret = CloseHandle (ProcessInfo%hThread)
       ret = CloseHandle (ProcessInfo%hProcess)
    end if

end subroutine f2008_execute_command_line
#endif
!
!
!
!==============================================================================
function receive_signal(M2R_sig) result (signal)
!!--description-----------------------------------------------------------------
!
!    Function: - Get signal from MATLAB.
!
!!--declarations----------------------------------------------------------------
    implicit none
    !
    ! Call variables
    !
    type(DioPltType)         , intent(in)    :: M2R_sig
    integer                                  :: signal
    !
    ! Local variables
    !
    integer, dimension(1,1)                  :: ival
!
!! executable statements -------------------------------------------------------
!
    ival(1,1) = -999
    if (.not.DioPltReadValues(Dio_Var_Integer,M2R_sig)) then
       ! error reading data from data stream
    elseif (.not.DioPltGetValues(M2R_sig,ival)) then
       ! error copying data to ival
    endif
    signal = ival(1,1)
end function receive_signal
!
!
!
!==============================================================================
subroutine send_signal(R2M_sig, signal)
!!--description-----------------------------------------------------------------
!
!    Function: - Get signal from MATLAB.
!
!!--declarations----------------------------------------------------------------
    implicit none
    !
    ! Call variables
    !
    type(DioPltType)         , intent(in)    :: R2M_sig
    integer                  , intent(in)    :: signal
    !
    ! Local variables
    !
    integer, dimension(1,1)                  :: ival
!
!! executable statements -------------------------------------------------------
!
    ival(1,1) = signal
    call DioPltPut(R2M_sig,ival)
end subroutine send_signal
!
!
!
!==============================================================================
function Missing (val)  result (retval)
    implicit none

    logical          ::  retval
    double precision ::  val
    real, parameter  ::  MissingMax = -998.999
    real, parameter  ::  MissingMin = -1000.

    retval = .false.
    if (val<=MissingMax .and. val>MissingMin) retval=.true.
end function Missing
!
!
!
!==============================================================================
subroutine ProcessMatlabInputVars(this,IREQ,IDUM)
    use ParameterModule, only: CharIdLength, NSOBEK, NSobWq, ND3BID, NPRECP, NTIMS
    use LocationDataModule, only: ID_SLC, ID_SBR, ID_D3B, ID_SWQ, ID_PRE, ALRSBK, ALRSWQ, ALRS3B, ALRSPR, ResPre, CheckIdsMatlab
    use OtherData, only: RTC_TIMOLD, IHOUR, IMIN, ISEC, ITMSIZ, ToMatlab1D2DH, ToMatlab1D2DWD, ToMatlab1D2DBL, ToMatlab1D2DU, ToMatlab1D2DV, ToMatlab1D2DC, MatRR, MatRain, MatRainPredict, MatWindPredict, MatPredictMulti, MatWq, MatlabNrWqPar, MatlabWq, MatlabWqParId, NTIMHP
    use ReadLib_rtc, only: IntCH8
    !
    IMPLICIT NONE
    !
    integer               :: IDUM
    integer               :: IREQ ! IREQ = 0 : Count number of variables
                                  ! IREQ = 1 : Collect variable names and values
                                  ! IREQ = 2 : Collect variable values only
    type(rtc_matlab_type) :: this
    !
    integer :: i
    integer :: ID
    integer :: I3B
    integer :: IPRE
    integer :: ISBK
    integer :: ISobWq
    integer :: IPar
    integer :: RetVal
    double precision :: RVAL
    character(len=CharIdLength) :: TempString
    character(len=8) :: IparStr
    character(len=DioMaxLocLen), dimension(:), pointer :: VarName
    double precision, dimension(:,:), pointer          :: VarVal
    !
    i = 0
    if (IREQ==1) VarName => this%InputVarName
    if (IREQ==2) VarVal  => this%InputVarVal

    i = i+1
    if (IREQ==1) VarName(i) = 'SobekFirst'
    if (IREQ==2) then
       if (IDUM==1) then
          VarVal(1,i) = 1
       else
          VarVal(1,i) = 0
       endif
    endif

    i = i+1
    if (IREQ==1) VarName(i)  = 'SobekDate'
    if (IREQ==2) VarVal(1,i) = INT(RTC_TIMOLD) ! should be converted to string like '20140516'

    i = i+1
    if (IREQ==1) VarName(i)  = 'SobekTime'
    if (IREQ==2) VarVal(1,i) = 1000000 * IHOUR + 10000 * IMIN + 100 * ISEC ! should be converted to string like '00000000'

    i = i+1
    if (IREQ==1) VarName(i)  = 'SobekCompTimestepSize'
    if (IREQ==2) VarVal(1,i) = ITMSIZ ! time step in seconds; should be converted to string like '00000600', meaning 600 sec

    ! all Sobeksim 1D2D locations locations (<=-5)
    do ISBK = 1,NSOBEK
      if (ID_SLC(ISBK)==-5) then
        TempString = ID_SBR(ISBK)
        ! water level
        if (ToMatlab1D2DH/=0) then
            i = i+1
            if (IREQ==1) VarName(i)  = 'Sobek1D2DH_'//TempString
            if (IREQ==2) VarVal(1,i) = ALRSBK(ISBK,1,NTIMS)
        endif
        ! water depth
        if (ToMatlab1D2DWD/=0) then
            i = i+1
            if (IREQ==1) VarName(i)  = 'Sobek1D2DWD_'//TempString
            if (IREQ==2) VarVal(1,i) = ALRSBK(ISBK,4,NTIMS)
        endif
        ! bed level
        if (ToMatlab1D2DBL/=0) then
            i = i+1
            if (IREQ==1) VarName(i)  = 'Sobek1D2DBL_'//TempString
            if (IREQ==2) VarVal(1,i) = ALRSBK(ISBK,17,NTIMS)
        endif
        ! u-velocity
        if (ToMatlab1D2DU/=0) then
            i = i+1
            if (IREQ==1) VarName(i)  = 'Sobek1D2DU_'//TempString
            if (IREQ==2) VarVal(1,i) = ALRSBK(ISBK,18,NTIMS)
        endif
        ! v-velocity
        if (ToMatlab1D2DV/=0) then
            i = i+1
            if (IREQ==1) VarName(i)  = 'Sobek1D2DV_'//TempString
            if (IREQ==2) VarVal(1,i) = ALRSBK(ISBK,19,NTIMS)
        endif
        ! 2D-flow velocity
        if (ToMatlab1D2DC/=0) then
            i = i+1
            if (IREQ==1) VarName(i)  = 'Sobek1D2DC_'//TempString
            if (IREQ==2) VarVal(1,i) = ALRSBK(ISBK,20,NTIMS)
        endif
      endif
    enddo

    ! all Sobeksim measurement locations (<=-4)
    do ISBK = 1,NSOBEK
      if (ID_SLC(ISBK)==-4) then
        TempString = 'ml' // ID_SBR(ISBK)
        ! water level
        i = i+1
        if (IREQ==1) VarName(i)  = 'SobekH_'//TempString
        if (IREQ==2) VarVal(1,i) = ALRSBK(ISBK,1,NTIMS)
        ! discharge
        i = i+1
        if (IREQ==1) VarName(i)  = 'SobekQ_'//TempString
        if (IREQ==2) VarVal(1,i) = ALRSBK(ISBK,2,NTIMS)
        ! water depth
        i = i+1
        if (IREQ==1) VarName(i)  = 'SobekWD_'//TempString
        if (IREQ==2) VarVal(1,i) = ALRSBK(ISBK,4,NTIMS)
        ! velocity
        i = i+1
        if (IREQ==1) VarName(i)  = 'SobekVR_'//TempString
        if (IREQ==2) VarVal(1,i) = ALRSBK(ISBK,21,NTIMS)
      endif
    enddo

    ! all Sobeksim structures (<=-3)
    ! filter: all non-defined values (e.g. pump capacity for a weir) are -999
    do ISBK = 1,NSOBEK
       if (ID_SLC(ISBK)==-3) then
         TempString = ID_SBR(ISBK)
         ! For backwards compatibility of 2.06 and B&O version,
         ! always include old structure par; write using SobekP_ string
         RVAL  =  ALRSBK(ISBK,3,NTIMS)
         if (Missing (RVAL)) then
            if (.not. Missing(AlRSbk(Isbk,5,NTIMS)) ) then
               ! weir or orifice
               RVAL  =  ALRSBK(ISBK,5,NTIMS)
            endif
            if (.not. Missing(AlRSbk(ISBK,7,NTIMS)) ) then
               ! orifice
               RVAL  =  ALRSBK(ISBK,7,NTIMS)
            elseif (.not. Missing(AlRSbk(Isbk,16,NTIMS)) ) then
               ! pump
               RVAL  =  ALRSBK(ISBK,16,NTIMS)
            endif
         endif
         if (.not. Missing(RVAL) ) then
            i = i+1
            if (IREQ==1) VarName(i)  = 'SobekP_'//TempString
            if (IREQ==2) VarVal(1,i) = RVAL
         endif
         ! crest level
         RVAL  =  ALRSBK(ISBK,5,NTIMS)
         if (.not. Missing(RVAL) ) then
            i = i+1
            if (IREQ==1) VarName(i)  = 'SobekCL_'//TempString
            if (IREQ==2) VarVal(1,i) = RVAL
         endif
         ! crest width
         RVAL  =  ALRSBK(ISBK,6,NTIMS)
         if (.not. Missing(RVAL) ) then
            i = i+1
            if (IREQ==1) VarName(i)  = 'SobekCW_'//TempString
            if (IREQ==2) VarVal(1,i) = RVAL
         endif
         ! gate lower edge
         RVAL  =  ALRSBK(ISBK,7,NTIMS)
         if (.not. Missing(RVAL) ) then
            i = i+1
            if (IREQ==1) VarName(i)  = 'SobekGL_'//TempString
            if (IREQ==2) VarVal(1,i) = RVAL
         endif
         ! gate opening height
         RVAL  =  ALRSBK(ISBK,8,NTIMS)
         if (.not. Missing(RVAL) ) then
            i = i+1
            if (IREQ==1) VarName(i)  = 'SobekGO_'//TempString
            if (IREQ==2) VarVal(1,i) = RVAL
         endif
         ! structure flow area
         RVAL  =  ALRSBK(ISBK,9,NTIMS)
         if (.not. Missing(RVAL) ) then
            i = i+1
            if (IREQ==1) VarName(i)  = 'SobekFA_'//TempString
            if (IREQ==2) VarVal(1,i) = RVAL
         endif
         ! flow at structure
         RVAL  =  ALRSBK(ISBK,10,NTIMS)
         if (.not. Missing(RVAL) ) then
            i = i+1
            if (IREQ==1) VarName(i)  = 'SobekQS_'//TempString
            if (IREQ==2) VarVal(1,i) = RVAL
         endif
         ! velocity at structure
         RVAL  =  ALRSBK(ISBK,11,NTIMS)
         if (.not. Missing(RVAL) ) then
            i = i+1
            if (IREQ==1) VarName(i)  = 'SobekVS_'//TempString
            if (IREQ==2) VarVal(1,i) = RVAL
         endif
         ! water level up
         RVAL  =  ALRSBK(ISBK,12,NTIMS)
         if (.not. Missing(RVAL) ) then
            i = i+1
            if (IREQ==1) VarName(i)  = 'SobekHU_'//TempString
            if (IREQ==2) VarVal(1,i) = RVAL
         endif
         ! water level down
         RVAL  =  ALRSBK(ISBK,13,NTIMS)
         if (.not. Missing(RVAL) ) then
            i = i+1
            if (IREQ==1) VarName(i)  = 'SobekHD_'//TempString
            if (IREQ==2) VarVal(1,i) = RVAL
         endif
         ! head
         RVAL  =  ALRSBK(ISBK,14,NTIMS)
         if (.not. Missing(RVAL) ) then
            i = i+1
            if (IREQ==1) VarName(i)  = 'SobekDH_'//TempString
            if (IREQ==2) VarVal(1,i) = RVAL
         endif
         ! pressure difference
         RVAL  =  ALRSBK(ISBK,15,NTIMS)
         if (.not. Missing(RVAL) ) then
            i = i+1
            if (IREQ==1) VarName(i)  = 'SobekDP_'//TempString
            if (IREQ==2) VarVal(1,i) = RVAL
         endif
         ! pump capacity
         RVAL  =  ALRSBK(ISBK,16,NTIMS)
         if (.not. Missing(RVAL) ) then
            i = i+1
            if (IREQ==1) VarName(i)  = 'SobekPC_'//TempString
            if (IREQ==2) VarVal(1,i) = RVAL
         endif
       endif
    enddo

    ! all Sobeksim reachsegments (<=-2)
    do ISBK = 1,NSOBEK
       if (ID_SLC(ISBK)==-2) then
         TempString = ID_SBR(ISBK)
         ! discharge
         i = i+1
         if (IREQ==1) VarName(i)  = 'SobekQ_'//TempString
         if (IREQ==2) VarVal(1,i) = ALRSBK(ISBK,2,NTIMS)
         ! velocity
         i = i+1
         if (IREQ==1) VarName(i)  = 'SobekVR_'//TempString
         if (IREQ==2) VarVal(1,i) = ALRSBK(ISBK,21,NTIMS)
       endif
    enddo

    ! all Sobeksim nodes (-1)
    do ISBK = 1,NSOBEK
       if (ID_SLC(ISBK)==-1) then
         TempString = ID_SBR(ISBK)
         ! water level
         i = i+1
         if (IREQ==1) VarName(i)  = 'SobekH_'//TempString
         if (IREQ==2) VarVal(1,i) = ALRSBK(ISBK,1,NTIMS)
         ! water depth
         i = i+1
         if (IREQ==1) VarName(i)  = 'SobekWD_'//TempString
         if (IREQ==2) VarVal(1,i) = ALRSBK(ISBK,4,NTIMS)
       endif
    enddo

    ! all Sobeksim bi lc locations
    do ISBK = 1,NSOBEK
       if (ID_SLC(ISBK)>=0) then
         ID = ID_SLC(ISBK) ! ID_SLC is double precision whereas IntCH8 expects integer
         TempString = 'bi' // trim(ID_SBR(ISBK)) // 'lc' //IntCH8(I)
         ! water level
         i = i+1
         if (IREQ==1) VarName(i)  = 'SobekH_'//TempString
         if (IREQ==2) VarVal(1,i) = ALRSBK(ISBK,1,NTIMS)
         ! discharge
         i = i+1
         if (IREQ==1) VarName(i)  = 'SobekQ_'//TempString
         if (IREQ==2) VarVal(1,i) = ALRSBK(ISBK,2,NTIMS)
         ! surface area
         i = i+1
         if (IREQ==1) VarName(i)  = 'SobekSA_'//TempString
         if (IREQ==2) VarVal(1,i) = ALRSBK(ISBK,3,NTIMS)
         ! water depth
         i = i+1
         if (IREQ==1) VarName(i)  = 'SobekWD_'//TempString
         if (IREQ==2) VarVal(1,i) = ALRSBK(ISBK,4,NTIMS)
         ! velocity
         i = i+1
         if (IREQ==1) VarName(i)  = 'SobekVR_'//TempString
         if (IREQ==2) VarVal(1,i) = ALRSBK(ISBK,21,NTIMS)
       endif
    enddo

    ! WQ to Matlab
    if (MatWQ) then
      do ISobwq = 1,NSobWq
        do IPar = 1,MatlabNrWqPar
           i = i+1
           if (IREQ==1) VarName(i)  = 'WQ_Par_'//trim(MatlabWqParId(IPar))//'_Loc_'//ID_SWQ(ISobWq)
           if (IREQ==2) VarVal(1,i) = ALRSWQ(ISobwq,MatlabWq(IPar),NTIMS)
        enddo
      enddo
    endif

    ! RR to Matlab
    if (MatRR) then
      do I3B = 1,ND3BID
        i = i+1
        if (IREQ==1) VarName(i)  = 'RRH_'//ID_D3B(I3B)
        if (IREQ==2) VarVal(1,i) = ALRS3B(I3B,1,NTIMS)
      enddo
    endif

    ! Rain to Matlab
    if (MatRain) then
      do IPRE = 1,NPRECP
        i = i+1
        if (IREQ==1) VarName(i)  = 'RainH_'//ID_PRE(IPRE)
        if (IREQ==2) VarVal(1,i) = ALRSPR(IPRE,1,NTIMS)
      enddo
    endif

    ! Rain Prediction to Matlab
    if (MatRainPredict) then
      do IPRE = 1,NPRECP
         if (MatPredictMulti) then
            ! NOT SUPPORTED - Did this ever work with the old communication?
            ! i = i+1
            ! if (IREQ==1) VarName(i) = 'RainHPredict_'//ID_PRE(IPRE)
            ! if (IREQ==2) VarVal(i)  = (ResPre(Ipre,Ipar), IPar=1,NTimHp)
         else
           do IPar = 1,NTimHp
             i = i+1
             write(IparStr,'(I0)') Ipar
             if (IREQ==1) VarName(i)  = 'RainHPredict_'//trim(ID_PRE(IPRE))//'_'//IparStr
             if (IREQ==2) VarVal(1,i) = ResPre(Ipre,Ipar)
           enddo
         endif
      enddo
    endif

    if (IREQ==0) then
      this%NInputVars = i
      allocate(this%InputVarName(i))
      allocate(this%InputVarVal(1,i))
    elseif (IREQ==1) then
      ! Check all variable names
      do i = 1,this%NInputVars
         RetVal = CheckIdsMatlab(VarName(i),0,.false.)
      enddo
    endif
end subroutine ProcessMatlabInputVars
!
!
!
!==============================================================================
subroutine ProcessMatlabOutputVars(this,IREQ,matdbg,lundia)
    use ParameterModule, only: CharIdLength, NSMEAS, N3MEAS, N3MLOC, N3MATLOC
    use LocationDataModule, only: CheckIdsMatlab
    use DecisionModule, only: DCVVAL
    use MeasureModule, only: MEASTY, MEASIDMatlab, IXMSBP, LCID3B, IXMS3P
    use OtherData, only: WithoutSobekCString
    !
    IMPLICIT NONE
    !
    integer                  , intent(in)    :: lundia !< logical unit for debug/error output
    integer                  , intent(in)    :: matdbg
    integer               :: IREQ ! IREQ = 0 : Count number of variables
                                  ! IREQ = 1 : Collect variable names and values
                                  ! IREQ = 2 : Collect variable values only
    type(rtc_matlab_type) :: this
    !
    integer :: i
    integer :: IMEAS
    integer :: IPARA
    integer :: imatloc
    integer :: RetVal
    logical :: Controlled
    character(len=DioMaxLocLen), dimension(:), pointer :: VarName
    double precision, dimension(:,:), pointer          :: VarVal

    i = 0
    if (IREQ==1) VarName => this%OutputVarName
    if (IREQ==2) VarVal  => this%OutputVarVal

    do IMEAS = 1,NSMEAS
      ! Add measure location to MATLAB communication string and request output
      if (MEASTY(IMEAS)==9) then
        ! optional, controlled by RTC flag yes(1) / no(0)
        if (.not. WithoutSobekCString) then
          i = i+1
          if (IREQ==1) VarName(i) = 'SobekC_'//MEASIDMatlab(IMEAS)
          if (IREQ==2) Controlled = VarVal(1,i)>0
        else
          Controlled = .true.
        endif
        !
        IPARA = IXMSBP(IMEAS)
        ! setpoint of SOBEK controller
        i = i+1
        if (IREQ==1) VarName(i) = 'SobekS_'//MEASIDMatlab(IMEAS)
        if (IREQ==2 .and. Controlled) DCVVAL(IPARA,1) = VarVal(1,i)
      endif
    enddo

    do IMEAS =N3Meas+1,N3Meas+N3MatLoc
      ! optional, controlled by RTC flag yes(1) / no(0)
!     SobekCString not applicable for RR
      Controlled = .true.
      IMatloc = IMeas - N3Meas

      IPARA = IXMS3P(N3MEAS+1+(Imatloc-1)*4)
      ! RR-pump switch on level, low capacity
      i = i+1
      if (matdbg .gt.0) write(lundia,*) 'imeas n3meas n3matloc',imeas,n3meas,n3matloc
      if (matdbg .gt.0) write(lundia,*) 'imatloc ipara i',imatloc, ipara, i
      if (IREQ==1) VarName(i) = 'RRSlowon_'//LcID3B(N3MLoc+IMatloc)
      if (IREQ==2 .and. Controlled) DCVVAL(IPARA,1) = VarVal(1,i)
      if (ireq==2 .and. matdbg .gt.0) write(lundia,*) 'ipara dcvval',ipara,Dcvval(ipara,1)
      ! RR-pump switch off level, low capacity
      i = i+1
      ipara = ipara + 1
      if (IREQ==1) VarName(i) = 'RRSlowoff_'//LcID3B(N3MLoc+IMatloc)
      if (IREQ==2 .and. Controlled) DCVVAL(IPARA,1) = VarVal(1,i)
      if (ireq==2 .and. matdbg .gt.0) write(lundia,*) 'ipara dcvval',ipara,Dcvval(ipara,1)
      ! RR-pump switch on level, high capacity
      i = i+1
      ipara = ipara + 1
      if (IREQ==1) VarName(i) = 'RRShighon_'//LcID3B(N3MLoc+IMatloc)
      if (IREQ==2 .and. Controlled) DCVVAL(IPARA,1) = VarVal(1,i)
      if (ireq==2 .and. matdbg .gt.0) write(lundia,*) 'ipara dcvval',ipara,Dcvval(ipara,1)
      ! RR-pump switch off level, high capacity
      i = i+1
      ipara = ipara + 1
      if (IREQ==1) VarName(i) = 'RRShighoff_'//LcID3B(N3MLoc+IMatloc)
      if (IREQ==2 .and. Controlled) DCVVAL(IPARA,1) = VarVal(1,i)
      if (ireq==2 .and. matdbg .gt.0) write(lundia,*) 'ipara dcvval',ipara,Dcvval(ipara,1)
    enddo

    if (IREQ==0) then
      this%NOutputVars = i
      allocate(this%OutputVarName(i))
      allocate(this%OutputVarVal(1,i))
    elseif (IREQ==1) then
      ! Check all variable names
      do i = 1,this%NOutputVars
         RetVal = CheckIdsMatlab(VarName(i),0,.false.)
      enddo
    endif
end subroutine ProcessMatlabOutputVars

end module rtc_matlab_module
