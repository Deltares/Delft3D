! Module for synchronisation RTC to and from Flow
module SyncRtcFlow
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
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
!  
!  
!!--description-----------------------------------------------------------------
! Organizes the communication between the FLOW
! executable and the RTC executable.
! This module is the interface between DelftIO and
! Delft3D-FLOW.
! Method used: Handled by DelftIO
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
!
  use Dio_Plt_Rw
!
  implicit none
!
! VARIABLES USED BY BOTH RTC AND FLOW SIDE
!
  type(DioPltType)  :: SignalFlowToRtc
  type(DioPltType)  :: SignalRtcToFlow
  type(DioPltType)  :: DataRtcToFlow
  type(DioPltType)  :: DataFlowToRtc
!
  integer, dimension(:,:), pointer :: SignalRValues
  character(len=DioMaxParLen), pointer, dimension(:) :: SignalRPars
  character(len=DioMaxLocLen), pointer, dimension(:) :: SignalRLocs
!
  integer, dimension(1,6) :: SignalXValues
  character*20, dimension(1) :: SignalXPars
  character*20, dimension(6) :: SignalXLocs
!
  data SignalXPars / 'Signal' /
  data SignalXLocs / 'Status', 'Date', 'Time', 'Init1', 'Init2', 'Init3' /
!
  logical :: commRTCtoFLOW = .false.
  logical :: commFLOWtoRTC = .false.
!
! VARIABLES USED BY RTC SIDE
!
  ! Barrier names received from Flow
  character(len=DioMaxParLen), Allocatable, Save :: BarrierNames(:)
!
  ! Array to store results of calculations
  real        , Allocatable, Save :: Valbar(:,:)
!
  ! Array to store data for sending
  real        , Allocatable, Save :: DataValuesOut(:,:)
!
  ! Number of barriers in Flow
  Integer :: numBarriers
!
  ! Locations for sending data
  character*20, dimension(2) :: Locbar
!
  data Locbar / 'Status', 'Value' /
!
! Variables used by Delft3D-FLOW side
!
  character(len=DioMaxParLen), pointer, dimension(:) :: DataPars
  character(len=DioMaxLocLen), pointer, dimension(:) :: DataLocs
  real, dimension(:,:), pointer :: DataValuesIn


contains

!!!
!!! FUNCTIONS RTC SIDE
!!!

! Initialise communication between RTC and Flow
subroutine SyncRtcFlow_Init(n2steps, error, flagFLOWtoRTC, idate, itstart, rdt)

  integer :: n2steps
  logical :: error
  logical :: flagFLOWtoRTC

  integer :: nrpar, nrloc
  integer :: nDummyLocs
  integer :: FlowStatus
  integer :: idate
  integer :: itstart
  double precision :: rdt  ! time step in seconds

  integer :: itest

  ! Stream for receiving barrier names
  type(DioPltType)  :: InfoFlowToRtc
  character(len=DioMaxParLen), pointer, dimension(:) :: InfoPars
  character(len=DioMaxLocLen), pointer, dimension(:) :: DummyLocs


  ! Initialise error
  error = .false.

  ! Init DelftIO
  call DioInit

  ! Signal from Flow to RTC
  ! First to receive number of (half) time steps
  ! Then to receive status (< 0 = Quit), Date (I8) and Time (I6)
  SignalFlowToRtc = DioPltGetDataset('SignalToRtc')
  nrpar = DioPltGetNPar(SignalFlowToRtc)
  nrloc = DioPltGetNLoc(SignalFlowToRtc)

  if (DioPltGet(SignalFlowToRtc, SignalRValues)) then
    n2steps = SignalRValues(1,1)
    idate   = SignalRValues(1,2)
    rdt     = transfer(SignalRValues(1,3:4),rdt)
    commFLOWtoRTC = btest(SignalRValues(1,5),0)
    commRTCtoFLOW = btest(SignalRValues(1,5),1)
    ! Backward compatibility with Delft3D-FLOW version that doesn't pass itstart yet.
    if (nrloc>=6) then
       itstart = SignalRValues(1,6)
    else
       itstart = 0
    endif
    if (commFLOWtoRTC .neqv. flagFLOWtoRTC) then
       n2steps = -3
    endif
  else
    !
    ! Problem with DelftIO coupling. Stop immediately.
    !
    n2steps = -2
    error   = .true.
    return
  endif

  !
  ! Check whether the communication mode requires barrier heights to be sent to FLOW
  !
  if (commRTCtoFLOW) then

    ! Signal from RTC to Flow
    ! Then to send status (< 0 = Quit), Date (I8) and Time (I6)
    SignalRtcToFlow = DioPltDefine('SignalToFlow', Dio_Plt_Integer, SignalXPars, SignalXLocs)

    ! Initialise send values
    SignalXValues(1,1) = 1
    SignalXValues(1,2) = 2
    SignalXValues(1,3) = 3

    if (n2steps <= 0) then
      SignalXValues(1,1) = -1
      ! Send bad status
      call DioPltPut(SignalRtcToFlow, SignalXValues)
      error = .true.
      return
    endif

    ! Send OK status due to time step value
    SignalXValues(1,1) = 1
    call DioPltPut(SignalRtcToFlow, SignalXValues)

    ! Get stream with barrier names
    InfoFlowToRtc = DioPltGetDataset('InfoToRtc')
    numBarriers   = DioPltGetNPar(InfoFlowToRtc)
    nDummyLocs    = DioPltGetNLoc(InfoFlowToRtc)

    if (numBarriers <= 0) then
      SignalXValues(1,1) = -1
      ! Send bad status
      call DioPltPut(SignalRtcToFlow, SignalXValues)
      ! Cleanup Info-stream did its work however we are not satisfied
      call DioPltDestroy(InfoFlowToRtc)
      error = .true.
      return
    endif

    ! Allocate arrays for barrier names and data and store the names
    Allocate ( BarrierNames(numBarriers) )
    Allocate ( Valbar(2, numBarriers) )
    Allocate ( DataValuesOut(numBarriers, 2) )
    InfoPars => DioPltGetPars(InfoFlowToRtc)
    BarrierNames = InfoPars
    Valbar = -999.0

    ! Send OK status due to barrier names
    SignalXValues(1,1) = 1
    call DioPltPut(SignalRtcToFlow, SignalXValues)

    ! Cleanup Info-stream did its work
    call DioPltDestroy(InfoFlowToRtc)

    ! Setup the actual data-stream to Flow
    DataRtcToFlow = DioPltDefine('DataToFlow', Dio_Plt_Real, BarrierNames, Locbar)

    ! Check if flow received the barrier names
    if (DioPltGet(SignalFlowToRtc, SignalRValues)) then
      FlowStatus = SignalRValues(1,1)
    else
      FlowStatus = -1
    endif

    if (FlowStatus < 0) then
      error = .true.
    endif
  elseif (n2steps <= 0) then
    !
    ! Would like to say "stop" to Delft3D-FLOW, but I don't know how.
    !
    error = .true.
    return  
  endif
  
end subroutine SyncRtcFlow_Init


! Sends calculated dat to Flow
subroutine SyncRtcFlow_Send(RtcStatus)

  integer :: RtcStatus
  integer :: i         ! Loop counter

  ! First send status through signal
  SignalXValues(1,1) = RtcStatus
  call DioPltPut(SignalRtcToFlow, SignalXValues)

  ! If RtcStatus < 0, sending data is not necessary, because
  ! Flow will quit
  if (RtcStatus >= 0) then
    ! Convert/copy data
    do i = 1, numBarriers
      DataValuesOut(i, 1) = ValBar(1, i)
      DataValuesOut(i, 2) = ValBar(2, i)
    enddo
    call DioPltPut(DataRtcToFlow, DataValuesOut)
  endif

end subroutine SyncRtcFlow_Send


subroutine SyncRtcFlow_Close

  ! Close open streams
  call DioPltDestroy(SignalFlowToRtc)
  if ( commRTCtoFLOW ) then
      call DioPltDestroy(SignalRtcToFlow)
      call DioPltDestroy(DataRtcToFlow)
  endif
  !
  ! DataFlowToRtc never allocated on RTC side (see inPlt_D3D in rtcmodule)
  !
  !if ( commFLOWtoRTC ) then
  !    call DioPltDestroy(DataFlowToRtc)
  !endif
    
end subroutine SyncRtcFlow_Close

!!!
!!! FUNCTIONS Delft3D-FLOW SIDE
!!!
!==============================================================================
subroutine syncflowrtc_quit
    use precision
! Shut down RTC in case of 'early' error
!
    implicit none
!
!! executable statements -------------------------------------------------------
!
    ! Init DelftIO
    call dioinit
    !
    ! Signal from Flow to RTC
    ! To send the shutdown message
    signalflowtortc = diopltdefine('SignalToRtc', dio_plt_integer, signalxpars, &
                    & signalxlocs)
    ! Send shutdown code to RTC
    signalxvalues(1, 1) = -1
    signalxvalues(1, 2) = 1
    signalxvalues(1, 3) = 2
    !
    call diopltput(signalflowtortc, signalxvalues)
    ! Close stream
    call diopltdestroy(signalflowtortc)
end subroutine syncflowrtc_quit
!
!
!
!==============================================================================
subroutine syncflowrtc_init(error, nambar, nsluv, IdLen, nsteps, &
                          & flagFLOWtoRTC, flagRTCtoFLOW, idate, itstart, dt)
    use precision
! Initialise communication between Flow and RTC
!
    implicit none
!
! Global variables
!
    integer                        ,intent (in)  :: IdLen
    integer                        ,intent (in)  :: idate
    integer                        ,intent (in)  :: itstart
    real(fp)                       ,intent (in)  :: dt
    integer                        ,intent (in)  :: nsluv
    integer                        ,intent (in)  :: nsteps
    logical                        ,intent (in)  :: flagFLOWtoRTC
    logical                        ,intent (in)  :: flagRTCtoFLOW
    logical                        ,intent (out) :: error
    character(IdLen), dimension(nsluv)         :: nambar ! WARNING: both IdLen and nsluv must be passed via parameter list for Intel 9.0
!
! Local variables
!
    integer                         :: ndatapars
    integer                         :: nrloc
    integer                         :: nrpar
    integer                         :: rtcstatus
    double precision                :: dt8
    character(20), dimension(1)     :: dummylocs
    type (dioplttype)               :: infoflowtortc
!
    data dummylocs/'Dummy'/
!
!! executable statements -------------------------------------------------------
!
    ! Init DelftIO
    call dioinit

    ! store RTC-FLOW communication flags in module
    commFLOWtoRTC = flagFLOWtoRTC
    commRTCtoFLOW = flagRTCtoFLOW

    !
    ! Signal from Flow to RTC
    ! First to send number of (half) time steps
    ! Then to send status (< 0 = Quit), Date (I8) and Time (I6)
    signalflowtortc = diopltdefine('SignalToRtc', dio_plt_integer, signalxpars, &
                    & signalxlocs)
    ! Send number of half time steps to RTC
    signalxvalues(1, 1) = nsteps*2
    signalxvalues(1, 2) = idate
    dt8 = dt
    signalxvalues(1, 3:4) = transfer(dt8,nsteps,2)
    signalxvalues(1, 5) = 0
    if (commFLOWtoRTC) signalxvalues(1, 5) = ibset(signalxvalues(1, 5),0)
    if (commRTCtoFLOW) signalxvalues(1, 5) = ibset(signalxvalues(1, 5),1)
    ! Send start time (relative to idate) expressed as multiples of half time steps
    signalxvalues(1, 6) = itstart*2
    !
    call diopltput(signalflowtortc, signalxvalues)
    !
    ! Check whether the communication mode requires barrier heights to be received from RTC
    !
    if (commRTCtoFLOW) then
       !
       ! Signal from RTC to Flow
       ! Then to receive status (< 0 = Quit), Date (I8) and Time (I6)
       ! Status used for shutdown, date and time can be used for checking.
       signalrtctoflow = diopltgetdataset('SignalToFlow')
       nrpar = diopltgetnpar(signalrtctoflow)
       nrloc = diopltgetnloc(signalrtctoflow)
       !
       if (diopltget(signalrtctoflow, signalrvalues)) then
          rtcstatus = signalrvalues(1, 1)
       else
          rtcstatus = -1
       endif
       !
       if (rtcstatus<0) then
          error = .true.
          return
       endif
       !
       ! Setup stream for sending barrier names
       !
       infoflowtortc = diopltdefine('InfoToRtc', dio_plt_integer, nambar,          &
                     & dummylocs)
       ! Check if message came through
       if (diopltget(signalrtctoflow, signalrvalues)) then
          rtcstatus = signalrvalues(1, 1)
       else
          rtcstatus = -1
       endif
       ! Info-stream did its work and not necessary any more
       call diopltdestroy(infoflowtortc)
       !
       if (rtcstatus<0) then
          error = .true.
          return
       endif
       ! Get the actual data-stream to Flow
       datartctoflow = diopltgetdataset('DataToFlow')
       ndatapars = diopltgetnpar(datartctoflow)
       ! Check on right number of parameters
       if (ndatapars==nsluv) then
          error = .false.
          signalxvalues(1, 1) = 1
       else
          error = .true.
          signalxvalues(1, 1) = -1
       endif
       ! Send status to RTC
       call diopltput(signalflowtortc, signalxvalues)
       !
    endif

end subroutine syncflowrtc_init
!
!
!
!==============================================================================
subroutine syncflowrtc_get(rtcstatus, tparget, tnparget)
    use precision
! Routine to get RTC status and the barrier data from RTC.
! If the flow status is < 0, then RTC will quit.
    implicit none
!
! Global variables
!
    integer                           ,intent (in)  :: tnparget  ! Number of parameters to get values for
    integer                                         :: rtcstatus ! Status sent from RTC, < 0 tells Flow to quit.
    real(fp), dimension(2, tnparget), intent (out)  :: tparget   ! Parameter values
!
! Local variables
!
    integer :: i
!
!! executable statements -------------------------------------------------------
!
    ! Get status from RTC
    if (diopltget(signalrtctoflow, signalrvalues)) then
                  ! Loop counter
       rtcstatus = signalrvalues(1, 1)
    else
       rtcstatus = -5
    endif
    !
    ! If RtcStatus < 0, data is not sent, because
    ! Flow will quit on bad status
    if (rtcstatus>=0) then
       if (diopltget(datartctoflow, DataValuesIn)) then
          do i = 1, tnparget
             tparget(1, i) = real(DataValuesIn(i, 1),fp)
             tparget(2, i) = real(DataValuesIn(i, 2),fp)
          enddo
       else
          ! No valid data received
          rtcstatus = -5
       endif
    endif
    !
end subroutine syncflowrtc_get
!
!
!
!==============================================================================
subroutine syncflowrtc_close
    use precision
!
    implicit none
!
!! executable statements -------------------------------------------------------
!
    ! Close open streams
    call diopltdestroy(signalflowtortc)
    if ( commRTCtoFLOW ) then
        call diopltdestroy(signalrtctoflow)
        call diopltdestroy(datartctoflow)
    endif
    if ( commFLOWtoRTC ) then
        call diopltdestroy(dataflowtortc)
    endif
    !
end subroutine syncflowrtc_close
!
!
!
!==============================================================================
subroutine datatortc(timsec, ifirstrtc, tparput,  &
                   & tlocput_names, nlocput, &
                   & tparput_names, nparput, success)
    use precision
    use dio_plt_rw
    !
    implicit none
!
! Global variables
!
    integer             :: ifirstrtc
    integer, intent(in) :: nlocput
    integer, intent(in) :: nparput
    real(fp)                            , intent(in)  :: timsec
    real(fp), dimension(nparput,nlocput), intent(in)  :: tparput
    character(80), dimension(nlocput)   , intent(in)  :: tlocput_names
    character(80), dimension(nparput)   , intent(in)  :: tparput_names
    logical                             , intent(out) :: success
!
! Local variables
!
    integer                                           :: i
    integer                                           :: k
    integer                                           :: istat
    real(sp)               , pointer, dimension(:, :) :: datavaluesToRTC
    character(256)                                    :: filename
    character(DioMaxParLen), pointer, dimension(:)    :: parNames      ! variable(s) in dataset
    character(DioMaxLocLen), pointer, dimension(:)    :: locNames
!
!! executable statements -------------------------------------------------------
!
   success = .false.
   !
   if (ifirstrtc > 0) then
      ifirstrtc = 0
      filename = 'TMP_FLOWtoRTC'
      allocate(parNames(nparput),stat=istat)
      if (istat == 0) allocate(locNames(nlocput), stat=istat)
      if (istat /= 0) goto 99
      !
      locNames = tlocput_names
      parNames = tparput_names
      !
      dataflowtortc = DioPltDefine(trim(filename), Dio_Plt_Real, parNames, locNames)
      deallocate(parNames)
      deallocate(locNames)
      commFLOWtoRTC = .true.
   endif
   !
   allocate (datavaluesToRTC(nparput,nlocput), stat=istat)
   if (istat /= 0) goto 99
   !
   do i = 1,nparput
      do k = 1,nlocput
         datavaluesToRTC(i,k) = real(tparput(i,k),sp)
      enddo
   enddo
   call DioPltPut (dataflowtortc, nint(timsec), datavaluesToRTC )
   success = .true.
   deallocate (datavaluesToRTC)
   return
   !
99 continue
   write(*,*) 'ERROR: Memory allocation error in datatortc'
end subroutine datatortc

end module SyncRtcFlow
