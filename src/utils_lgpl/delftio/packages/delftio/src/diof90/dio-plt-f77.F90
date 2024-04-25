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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!
!!! Dio-PLT-F77: F77 interface to DIO streams, PLT Datasets and Config
!!!
!!! (c) Deltares, mar 2001
!!!
!!! Stef.Hummel@deltares.nl
!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!
!! For each F90 function that has to be accessed by F77, a small
!! interface is available.
!! By defining 'USE_SRW_DLL)' a DLL is provided, that is also
!! used by GF's Delphi-wrappers
!!

!*
!* SRW interface (will become obsolete when SRW-wrapper is converted)
!*

function DioCreateStream(streamType, name, mode) result(streamHandle)
#if defined(USE_SRW_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DioCreateStream
#endif
    use Dio_streams

    ! return value
    integer  :: streamHandle ! streamHandle == 0 : error;
                             ! streamHandle > 0  : indx to arr. in STREAM-mod.

    ! arguments
    integer                      :: streamType  ! type of stream
    character(Len=*),intent(in)  :: name        ! stream name
    character(Len=*),intent(in)  :: mode        ! open mode ('r' | 'w' | 'a'),
                                                ! ( read | write | append )
    ! locals

    type(DioStreamType), pointer          :: stream

    ! body

    streamHandle = DioStreamNewF77Handle()
    if ( streamHandle > 0 ) then
        stream => StreamData(streamHandle)
        stream = DioStreamCreateSynched(streamType, name, mode, .false.)
        stream % id = streamHandle
        if ( .not. stream % opened ) then
            call DioStreamClose(stream)
            call DioStreamReleaseF77Handle(streamHandle)
            streamHandle = 0
        endif
    endif

end function DioCreateStream


function DioCreateStreamSynched(streamType, name, mode) result(streamHandle)
#if defined(USE_SRW_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DioCreateStreamSynched
#endif
    use Dio_streams

    ! return value
    integer  :: streamHandle ! streamHandle == 0 : error;
                             ! streamHandle > 0  : indx to arr. in STREAM-mod.

    ! arguments
    integer                      :: streamType ! type of stream
    character(Len=*),intent(in)  :: name       ! stream name
    character(Len=*),intent(in)  :: mode       ! open mode ('r' | 'w' | 'a'),
                                               ! ( read | write | append )
    ! locals

    type(DioStreamType), pointer :: stream

    ! body

    streamHandle = DioStreamNewF77Handle()
    if ( streamHandle > 0 ) then
        stream => StreamData(streamHandle)
        stream = DioStreamCreateSynched(streamType, name, mode, .true.)
        stream % id = streamHandle
        if ( .not. stream % opened ) then
            call DioStreamClose(stream)
            call DioStreamReleaseF77Handle(streamHandle)
            streamHandle = 0
        endif
    endif

end function DioCreateStreamSynched


subroutine DioCloseStream(streamHandle)
#if defined(USE_SRW_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DioCloseStream
#endif
    use Dio_streams

    ! arguments
    integer                      :: streamHandle ! int streamHandle

     ! locals
    type(DioStreamType), pointer :: stream       ! pointer to stream

    ! body

    if ( DioStreamF77HandleIsValid(streamHandle) ) then
        stream => StreamData(streamHandle)
        call DioStreamClose(stream)
        call DioStreamReleaseF77Handle(streamHandle)
    endif

end subroutine DioCloseStream


function DioDefinePltDataSet(streamHandle, name, varType,&
                                        npar, pars, nloc, locs) result(pltHandle)
#if defined(USE_SRW_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DioDefinePltDataSet
#endif
    use Dio_plt_rw

    ! return value
    integer  :: pltHandle ! pltHandle == 0 : error;
                          ! pltHandle > 0  : index to array in PLT-module.

    ! arguments
    integer            ,intent(in) :: streamHandle   ! stream id
    character(Len=*)   ,intent(in) :: name           ! dataset name
    integer            ,intent(in) :: varType        ! type of var in ds
    integer            ,intent(in) :: nPar           ! #parameters
    character(Len=*),&
        dimension(npar),intent(in) :: pars           ! parameter names
    integer            ,intent(in) :: nLoc           ! #locations
    character(Len=*),&
        dimension(nloc),intent(in) :: locs           ! location names

    ! locals
    type(DioStreamType), pointer   :: stream         ! handle to stream

    ! body

    pltHandle = 0
    stream => DioStreamGetF77Data(streamHandle)
    if ( .not. associated(stream) ) then
        call DioStreamError(403, 'DioDefinePltDataSet: invalid streamHandle')
    else
        if ( .not. stream % opened ) then
            call DioStreamError(404, &
                'DioDefinePltDataSet: stream not open', stream % name)
        else
            pltHandle = DioPltNewF77Handle()
            if ( pltHandle > 0 ) then
                PltDataset(pltHandle) = DioPltDefine(stream, name, varType, pars, locs)
                PltDataset(pltHandle) % ds % id = pltHandle
            endif
        endif
    endif

end function DioDefinePltDataSet


subroutine DioPutPltDataSetReals(pltHandle, timestep, nPar, nLoc, values)
#if defined(USE_SRW_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DioPutPltDataSetReals
#endif

    use Dio_plt_rw

    ! arguments

    integer, intent(in)          :: pltHandle  ! dataset id
    integer, intent(in)          :: nPar       ! #array-items for parameters
    integer, intent(in)          :: nLoc       ! #array-items for locations
    character(Len=*), intent(in) :: timestep   ! current time step
    real, dimension(nPar,nLoc)   :: values     ! values to be put

    ! locals

    type(DioPltType), pointer         :: plt

    ! body

    if ( DioPltF77HandleIsValid(pltHandle) ) then
        plt => PltDataset(pltHandle)
        if ( (npar .ne. plt % header % nPar) .or. &
             (nloc .ne. plt % header % nLoc)      ) then
            call DioStreamError(401, plt % ds % name, &
                ': DioPutPltDataSetReals: wrong sizes for time ', timestep)
        else
            call DioPltPut(plt, values)
        endif
    endif

    return

end subroutine DioPutPltDataSetReals


function DioGetPltDataSetInfo(streamHandle, name, npar, pars,&
                                        nloc, locs, ntim, tims) result(pltHandle)
#if defined(USE_SRW_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DioGetPltDataSetInfo
#endif

    use Dio_plt_rw

    ! return value
    integer  :: pltHandle ! pltHandle == 0 : error;
                          ! pltHandle > 0  : index to array in PLT-module.

    ! arguments
    integer         , intent(in)  :: streamHandle  ! stream id
    character(Len=*), intent(in)  :: name          ! dataset name
    integer         , intent(out) :: nPar          ! #parameters
    character(Len=*),&
        dimension(*), intent(out) :: pars          ! parameter names
    integer         , intent(out) :: nLoc          ! #locations
    character(Len=*),&
        dimension(*), intent(out) :: locs          ! location names
    integer         , intent(out) :: nTim          ! #timestep
    character(Len=*),&
        dimension(*), intent(out) :: tims          ! times

    ! locals
    type(DioPltType), pointer    :: plt            ! f90 dataset handle
    type(DioStreamType), pointer :: stream         ! stream handle

    ! body

    pltHandle = 0
    stream => DioStreamGetF77Data(streamHandle)
    if ( associated(stream) ) then
        if ( .not. stream % opened ) then
            call DioStreamError(404, &
                'DioGetPltDataSetInfo: stream not open', stream % name)
        else
            pltHandle = DioPltNewF77Handle()
        endif
    endif

    if ( pltHandle > 0 ) then
        plt => PltDataset(pltHandle)
        plt = DioPltGetDataset(stream, name)
        plt % ds % id = pltHandle

        nPar = plt % header % nPar
        nLoc = plt % header % nLoc
        if ( nPar .gt. 0 ) then
            pars(1:nPar) = plt % header % pars
        endif
        if ( nLoc .gt. 0) then
            locs(1:nLoc) = plt % header % locs
        endif
        nTim = 1
        tims(1) = '00:00'
    endif

end function DioGetPltDataSetInfo


function DioGetPltDataSetReals(pltHandle, timestep, npar, nloc, values) result(retVal)
#if defined(USE_SRW_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DioGetPltDataSetReals
#endif

    use Dio_plt_rw

    ! return value
    logical                           :: retVal    ! .true.: success

    ! arguments
    integer  , intent(in)             :: pltHandle ! dataset id
    character(Len=*), intent(in)      :: timestep  ! current time step
    integer, intent(in) :: npar
    integer, intent(in) :: nloc
    real, dimension(*), intent(OUT)   :: values    ! values to deliver

    ! locals
    type(DioPltType), pointer         :: plt       ! F90handle to PLT
    integer                           :: par,loc   ! loop counters
    real, dimension(:,:), pointer     :: locValues ! pointer to values

    ! body

    retVal = .false.
    if ( DioPltF77HandleIsValid(pltHandle) ) then
        plt => PltDataset(pltHandle)
        retVal = dioPltGet(plt, locValues)
        if ( retVal ) then
            if ( (npar .lt. plt % header % nPar) .or. &
                 (nloc .lt. plt % header % nLoc)      ) then
                call DioStreamError(401, 'DioGetPltDataSetReals: wrong sizes ',&
                                             plt % ds % name, timestep)
            else
                do par = 1, plt % header % nPar
                    do loc = 1, plt % header % nLoc
                        values( par + (loc - 1 )*nPar) = locValues(par,loc)
                    enddo
                enddo
                retVal = .true.
            endif
        endif
    endif
end function DioGetPltDataSetReals


subroutine DioDestroyPltDataSet(pltHandle)
#if defined(USE_SRW_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DioDestroyPltDataSet
#endif
    use Dio_plt_rw

    ! arguments
    integer  , intent(in)             :: pltHandle     ! dataset id

    ! locals
    type(DioPltType), pointer         :: plt

    ! body
    if ( DioPltF77HandleIsValid(pltHandle) ) then
        plt => PltDataset(pltHandle)
        call DioPltDestroy(plt)
        call DioPltReleaseF77Handle(pltHandle)
    endif

end subroutine DioDestroyPltDataSet


!**
!** ODS Server Interface
!**

!*
!* GENERAL
!*

subroutine DiofGetVersion(version)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofGetVersion
#endif
    use Dio_streams
    character(Len=*) :: version
    call DioGetVersion(version)
end subroutine DiofGetVersion


function DiofGetMaxParLen() result(retVal)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofGetMaxParLen
#endif
    use dio_plt_rw
    integer :: retVal
    retVal = DioMaxParLen
end function DiofGetMaxParLen


function DiofGetMaxLocLen() result(retVal)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofGetMaxLocLen
#endif
    use dio_plt_rw
    integer :: retVal
    retVal = DioMaxLocLen
end function DiofGetMaxLocLen


function DiofGetMaxDescrLen() result(retVal)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofGetMaxDescrLen
#endif
    use dio_plt_rw
    integer :: retVal
    retVal = DioMaxDescrLen
end function DiofGetMaxDescrLen


function DiofGetHisRunIdSize() result(retVal)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofGetHisRunIdSize
#endif
    use dio_plt_rw
    integer :: retVal
    retVal = HisRunIdSize
end function DiofGetHisRunIdSize


function DiofJulian2DioTime(julTime) result(dioTime)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofJulian2DioTime
#endif
    use dio_ds
    include 'dio-time-support.inc'
    character(Len=DioMaxTimLen) :: dioTime ! Dio TimeString
    double precision, intent(in):: julTime ! Julian Date
    dioTime = DioJulian2DioTime(julTime)
end function DiofJulian2DioTime


function DiofTimeString2Julian(dioTime) result(julTime)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofTimeString2Julian
#endif
    use dio_ds
    double precision:: julTime ! Julian Date
    character(Len=*), intent(in) :: dioTime ! Dio TimeString
    julTime = DioDsTimeString2Julian(dioTime)
end function DiofTimeString2Julian


subroutine DiofInit()
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofInit
#endif
    use Dio_streams
    call DioInit()
    call DioUnsetLogErrors()
end subroutine DiofInit


subroutine DiofInitByFile(fileName)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofInitByFile
#endif
    use Dio_streams
    ! arguments
    character(Len=*) :: fileName  ! configuration filename
    call DioInitByFile(fileName)
    call DioUnsetLogErrors()
end subroutine DiofInitByFile


!!
!! Error functions
!!

function DiofGetMaxErrMsgLen() result(maxMsgLen)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofGetMaxErrMsgLen
#endif
    use Dio_streams
    integer :: maxMsgLen
    maxMsgLen = DioGetLastError()
end function DiofGetMaxErrMsgLen


function DiofGetLastError() result(nr)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofGetLastError
#endif
    use Dio_streams
    integer :: nr
    nr = DioGetLastError()
end function DiofGetLastError


subroutine DiofGetLastErrorMsg(msg)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofGetLastErrorMsg
#endif
    use Dio_streams
    character(Len=*) :: msg
    msg = DioGetLastErrorMsg()
end subroutine DiofGetLastErrorMsg


!*
!* Define dataset
!*

function DiofPltDefine_1(name, varType, npar, pars, nloc, locs) result(pltHandle)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofPltDefine_1
#endif
    use Dio_plt_rw

    ! return value
    integer  :: pltHandle ! pltHandle == 0 : error;
                          ! pltHandle > 0  : index to array in PLT-module.

    ! arguments
    character(Len=*)              , intent(in) :: name           ! dataset name
    integer                       , intent(in) :: varType        ! type of var in ds
    integer                       , intent(in) :: nPar           ! #parameters
    character(Len=DioMaxParLen), &
                  dimension(npar) , intent(in) :: pars           ! parameter names
    integer                       , intent(in) :: nLoc           ! #locations
    character(Len=DioMaxLocLen), &
                  dimension(nloc) , intent(in) :: locs           ! location names

    ! body

    pltHandle = DioPltNewF77Handle()
    if ( pltHandle > 0 ) then
        PltDataset(pltHandle) = DioPltDefine(name, varType, pars, locs)
        PltDataset(pltHandle) % ds % id = pltHandle
        if (.not. DioPltOpenedOK(PltDataset(pltHandle)) ) then
             call DioPltReleaseF77Handle(pltHandle)
             pltHandle = 0
        endif
    endif

end function DiofPltDefine_1


function DiofPltDefine_2(name, varType, npar, pars, nloc, locs, startTime) result(pltHandle)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofPltDefine_2
#endif
    use Dio_plt_rw

    ! return value
    integer  :: pltHandle ! pltHandle == 0 : error;
                          ! pltHandle > 0  : index to array in PLT-module.

    ! arguments
    character(Len=*)              , intent(in) :: name      ! dataset name
    integer                       , intent(in) :: varType   ! type of var in ds
    integer                       , intent(in) :: nPar      ! #parameters
    character(Len=DioMaxParLen), &
                  dimension(npar) , intent(in) :: pars      ! parameter names
    integer                       , intent(in) :: nLoc      ! #locations
    character(Len=DioMaxLocLen), &
                  dimension(nloc) , intent(in) :: locs      ! location names
    double precision              , intent(in) :: startTime ! startTime as julian

    ! externals
    integer, external  :: DiofPltDefine_2a  ! actual define function

    ! locals
    double precision   :: endTime           ! endTime as julian

    ! body
    endTime = startTime - 1.0D+00
    pltHandle = DiofPltDefine_2a(name, varType, npar, pars, nloc, locs, startTime, endTime)

end function DiofPltDefine_2


function DiofPltDefine_2a(name, varType, npar, pars, nloc, locs, startTime, endTime) result(pltHandle)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofPltDefine_2a
#endif
    use Dio_plt_rw

    ! return value
    integer  :: pltHandle ! pltHandle == 0 : error;
                          ! pltHandle > 0  : index to array in PLT-module.

    ! arguments
    character(Len=*)              , intent(in) :: name      ! dataset name
    integer                       , intent(in) :: varType   ! type of var in ds
    integer                       , intent(in) :: nPar      ! #parameters
    character(Len=DioMaxParLen), &
                  dimension(npar) , intent(in) :: pars      ! parameter names
    integer                       , intent(in) :: nLoc      ! #locations
    character(Len=DioMaxLocLen), &
                  dimension(nloc) , intent(in) :: locs      ! location names
    double precision              , intent(in) :: startTime ! startTime as julian
    double precision              , intent(in) :: endTime   ! endTime as julian

    ! body

    pltHandle = DioPltNewF77Handle()
    if ( pltHandle > 0 ) then
        if ( endTime > startTime ) then
            PltDataset(pltHandle) = DioPltDefine(name, varType, pars, locs, startTime, endTime)
        else
            PltDataset(pltHandle) = DioPltDefine(name, varType, pars, locs, startTime)
        endif
        PltDataset(pltHandle) % ds % id = pltHandle
            if (.not. DioPltOpenedOK(PltDataset(pltHandle)) ) then
             call DioPltReleaseF77Handle(pltHandle)
             pltHandle = 0
        endif
    endif

end function DiofPltDefine_2a


function DiofPltDefine_3(name, runid, varType, &
                         npar, pars, nloc, locs              ) result(pltHandle)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofPltDefine_3
#endif
    use Dio_plt_rw

    ! return value
    integer  :: pltHandle ! pltHandle == 0 : error;
                          ! pltHandle > 0  : index to array in PLT-module.

    ! arguments
    character(Len=*)              , intent(in) :: name      ! dataset name
    character(Len=HisRunIdSize), &
           dimension(HisRunIdDim) , intent(in) :: runid     ! His RunIDLines
    integer                       , intent(in) :: varType   ! type of var in ds
    integer                       , intent(in) :: nPar      ! #parameters
    character(Len=DioMaxParLen), &
                  dimension(npar) , intent(in) :: pars      ! parameter names
    integer                       , intent(in) :: nLoc      ! #locations
    character(Len=DioMaxLocLen), &
                  dimension(nloc) , intent(in) :: locs      ! location names

    ! body

    pltHandle = DioPltNewF77Handle()
    if ( pltHandle > 0 ) then
        PltDataset(pltHandle) = DioPltDefine(name, runId, varType, pars, locs)
        PltDataset(pltHandle) % ds % id = pltHandle
            if (.not. DioPltOpenedOK(PltDataset(pltHandle)) ) then
             call DioPltReleaseF77Handle(pltHandle)
             pltHandle = 0
        endif
    endif

end function DiofPltDefine_3


function DiofPltDefine_4(name, runid, varType, &
                         npar, pars, nloc, locs, startTime) result(pltHandle)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofPltDefine_4
#endif
    use Dio_plt_rw

    ! return value
    integer  :: pltHandle ! pltHandle == 0 : error;
                          ! pltHandle > 0  : index to array in PLT-module.

    ! arguments
    character(Len=*)              , intent(in) :: name      ! dataset name
    character(Len=HisRunIdSize), &
           dimension(HisRunIdDim) , intent(in) :: runid     ! His RunIDLines
    integer                       , intent(in) :: varType   ! type of var in ds
    integer                       , intent(in) :: nPar      ! #parameters
    character(Len=DioMaxParLen), &
                  dimension(npar) , intent(in) :: pars      ! parameter names
    integer                       , intent(in) :: nLoc      ! #locations
    character(Len=DioMaxLocLen), &
                  dimension(nloc) , intent(in) :: locs      ! location names
    double precision              , intent(in) :: startTime ! startTime as julian
    double precision                           :: endTime   ! endTime as julian

    ! externals
    integer, external  :: DiofPltDefine_4a  ! actual define function

    ! body

    endTime = startTime - 1.0D+00
    pltHandle = DiofPltDefine_4a(name, runid, varType, &
                         npar, pars, nloc, locs, startTime, endTime )

end function DiofPltDefine_4


function DiofPltDefine_4a(name, runid, varType, &
                         npar, pars, nloc, locs, startTime, endTime ) result(pltHandle)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofPltDefine_4a
#endif
    use Dio_plt_rw

    ! return value
    integer  :: pltHandle ! pltHandle == 0 : error;
                          ! pltHandle > 0  : index to array in PLT-module.

    ! arguments
    character(Len=*)              , intent(in) :: name      ! dataset name
    character(Len=HisRunIdSize), &
           dimension(HisRunIdDim) , intent(in) :: runid     ! His RunIDLines
    integer                       , intent(in) :: varType   ! type of var in ds
    integer                       , intent(in) :: nPar      ! #parameters
    character(Len=DioMaxParLen), &
                  dimension(npar) , intent(in) :: pars      ! parameter names
    integer                       , intent(in) :: nLoc      ! #locations
    character(Len=DioMaxLocLen), &
                  dimension(nloc) , intent(in) :: locs      ! location names
    double precision              , intent(in) :: startTime ! startTime as julian
    double precision              , intent(in) :: endTime   ! endTime as julian

    ! body

    pltHandle = DioPltNewF77Handle()
    if ( pltHandle > 0 ) then
        if ( endTime > startTime ) then
            PltDataset(pltHandle) = DioPltDefine(name, runId, varType, pars, locs, startTime, endTime)
        else
            PltDataset(pltHandle) = DioPltDefine(name, runId, varType, pars, locs, startTime)
        endif
        PltDataset(pltHandle) % ds % id = pltHandle
            if (.not. DioPltOpenedOK(PltDataset(pltHandle)) ) then
             call DioPltReleaseF77Handle(pltHandle)
             pltHandle = 0
        endif
    endif

end function DiofPltDefine_4a


function DiofPltDefine_5(name, runid, varType, &
                         npar, pars, nloc, intIds, locs, startTime    ) result(pltHandle)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofPltDefine_5
#endif
    use Dio_plt_rw

    ! return value
    integer  :: pltHandle ! pltHandle == 0 : error;
                          ! pltHandle > 0  : index to array in PLT-module.

    ! arguments
    character(Len=*)              , intent(in) :: name      ! dataset name
    character(Len=HisRunIdSize), &
           dimension(HisRunIdDim) , intent(in) :: runid     ! His RunIDLines
    integer                       , intent(in) :: varType   ! type of var in ds
    integer                       , intent(in) :: nPar      ! #parameters
    character(Len=DioMaxParLen), &
                  dimension(npar) , intent(in) :: pars      ! parameter names
    integer                       , intent(in) :: nLoc      ! #locations
    integer,      dimension(nloc) , intent(in) :: intIds    ! integer loc. ids
    character(Len=DioMaxLocLen), &
                  dimension(nloc) , intent(in) :: locs      ! location names
    double precision              , intent(in) :: startTime ! startTime as julian

    ! externals
    integer, external  :: DiofPltDefine_5a  ! actual define function

    ! locals
    double precision   :: endTime           ! endTime as julian

    ! body

    endTime = startTime - 1.0D+00
    pltHandle = DiofPltDefine_5a(name, runid, varType, &
                         npar, pars, nloc, intIds, locs, startTime, endTime)

end function DiofPltDefine_5


function DiofPltDefine_5a(name, runid, varType, &
                         npar, pars, nloc, intIds, locs, startTime, endTime ) result(pltHandle)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofPltDefine_5a
#endif
    use Dio_plt_rw

    ! return value
    integer  :: pltHandle ! pltHandle == 0 : error;
                          ! pltHandle > 0  : index to array in PLT-module.

    ! arguments
    character(Len=*)              , intent(in) :: name      ! dataset name
    character(Len=HisRunIdSize), &
           dimension(HisRunIdDim) , intent(in) :: runid     ! His RunIDLines
    integer                       , intent(in) :: varType   ! type of var in ds
    integer                       , intent(in) :: nPar      ! #parameters
    character(Len=DioMaxParLen), &
                  dimension(npar) , intent(in) :: pars      ! parameter names
    integer                       , intent(in) :: nLoc      ! #locations
    integer,      dimension(nloc) , intent(in) :: intIds    ! integer loc. ids
    character(Len=DioMaxLocLen), &
                  dimension(nloc) , intent(in) :: locs      ! location names
    double precision              , intent(in) :: startTime ! startTime as julian
    double precision              , intent(in) :: endTime   ! endTime as julian

    ! body

    pltHandle = DioPltNewF77Handle()
    if ( pltHandle > 0 ) then
        if ( endTime > startTime ) then
            PltDataset(pltHandle) = DioPltDefine(name, runId, varType, pars, intIds, locs, startTime, endTime)
        else
            PltDataset(pltHandle) = DioPltDefine(name, runId, varType, pars, intIds, locs, startTime)
        endif
        PltDataset(pltHandle) % ds % id = pltHandle
            if (.not. DioPltOpenedOK(PltDataset(pltHandle)) ) then
             call DioPltReleaseF77Handle(pltHandle)
             pltHandle = 0
        endif
    endif

end function DiofPltDefine_5a


function DiofPltDefine_11(name, varType, npar, pars, nLoc) result(pltHandle)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofPltDefine_11
#endif
    use Dio_plt_rw

    ! return value
    integer  :: pltHandle ! pltHandle == 0 : error;
                          ! pltHandle > 0  : index to array in PLT-module.

    ! arguments
    character(Len=*)              , intent(in) :: name           ! dataset name
    integer                       , intent(in) :: varType        ! type of var in ds
    integer                       , intent(in) :: nPar           ! #parameters
    character(Len=DioMaxParLen), &
                  dimension(npar) , intent(in) :: pars           ! parameter names
    integer                       , intent(in) :: nLoc           ! #locations

    ! body

    pltHandle = DioPltNewF77Handle()
    if ( pltHandle > 0 ) then
        PltDataset(pltHandle) = DioPltDefine(name, varType, pars, nLoc)
        PltDataset(pltHandle) % ds % id = pltHandle
        if (.not. DioPltOpenedOK(PltDataset(pltHandle)) ) then
             call DioPltReleaseF77Handle(pltHandle)
             pltHandle = 0
        endif
    endif

end function DiofPltDefine_11


function DiofPltDefine_12(name, varType, npar, pars, nLoc, startTime) result(pltHandle)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofPltDefine_12
#endif
    use Dio_plt_rw

    ! return value
    integer  :: pltHandle ! pltHandle == 0 : error;
                          ! pltHandle > 0  : index to array in PLT-module.

    ! arguments
    character(Len=*)              , intent(in) :: name      ! dataset name
    integer                       , intent(in) :: varType   ! type of var in ds
    integer                       , intent(in) :: nPar      ! #parameters
    character(Len=DioMaxParLen), &
                  dimension(npar) , intent(in) :: pars      ! parameter names
    integer                       , intent(in) :: nLoc      ! #locations
    double precision              , intent(in) :: startTime ! startTime as julian

    ! externals
    integer, external  :: DiofPltDefine_12a ! actual define function

    ! locals
    double precision   :: endTime           ! endTime as julian

    ! body

    endTime = startTime - 1.0D+00
    pltHandle = DiofPltDefine_12a(name, varType, npar, pars, nLoc, startTime, endTime)

end function DiofPltDefine_12


function DiofPltDefine_12a(name, varType, npar, pars, nLoc, startTime, endTime) result(pltHandle)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofPltDefine_12a
#endif
    use Dio_plt_rw

    ! return value
    integer  :: pltHandle ! pltHandle == 0 : error;
                          ! pltHandle > 0  : index to array in PLT-module.

    ! arguments
    character(Len=*)              , intent(in) :: name      ! dataset name
    integer                       , intent(in) :: varType   ! type of var in ds
    integer                       , intent(in) :: nPar      ! #parameters
    character(Len=DioMaxParLen), &
                  dimension(npar) , intent(in) :: pars      ! parameter names
    integer                       , intent(in) :: nLoc      ! #locations
    double precision              , intent(in) :: startTime ! startTime as julian
    double precision              , intent(in) :: endTime   ! startTime as julian

    ! body

    pltHandle = DioPltNewF77Handle()
    if ( pltHandle > 0 ) then
        if ( endTime > startTime ) then
            PltDataset(pltHandle) = DioPltDefine(name, varType, pars, nLoc, startTime, endTime)
        else
            PltDataset(pltHandle) = DioPltDefine(name, varType, pars, nLoc, startTime)
        endif
        PltDataset(pltHandle) % ds % id = pltHandle
            if (.not. DioPltOpenedOK(PltDataset(pltHandle)) ) then
             call DioPltReleaseF77Handle(pltHandle)
             pltHandle = 0
        endif
    endif

end function DiofPltDefine_12a


function DiofPltDefine_13(name, runid, varType, &
                         npar, pars, nloc         ) result(pltHandle)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofPltDefine_13
#endif
    use Dio_plt_rw

    ! return value
    integer  :: pltHandle ! pltHandle == 0 : error;
                          ! pltHandle > 0  : index to array in PLT-module.

    ! arguments
    character(Len=*)              , intent(in) :: name      ! dataset name
    character(Len=HisRunIdSize), &
           dimension(HisRunIdDim) , intent(in) :: runid     ! His RunIDLines
    integer                       , intent(in) :: varType   ! type of var in ds
    integer                       , intent(in) :: nPar      ! #parameters
    character(Len=DioMaxParLen), &
                  dimension(npar) , intent(in) :: pars      ! parameter names
    integer                       , intent(in) :: nLoc      ! #locations

    ! body

    pltHandle = DioPltNewF77Handle()
    if ( pltHandle > 0 ) then
        PltDataset(pltHandle) = DioPltDefine(name, runId, varType, pars, nLoc)
        PltDataset(pltHandle) % ds % id = pltHandle
            if (.not. DioPltOpenedOK(PltDataset(pltHandle)) ) then
             call DioPltReleaseF77Handle(pltHandle)
             pltHandle = 0
        endif
    endif

end function DiofPltDefine_13


function DiofPltDefine_14(name, runid, varType, &
                         npar, pars, nloc, startTime) result(pltHandle)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofPltDefine_14
#endif
    use Dio_plt_rw

    ! return value
    integer  :: pltHandle ! pltHandle == 0 : error;
                          ! pltHandle > 0  : index to array in PLT-module.

    ! arguments
    character(Len=*)              , intent(in) :: name      ! dataset name
    character(Len=HisRunIdSize), &
           dimension(HisRunIdDim) , intent(in) :: runid     ! His RunIDLines
    integer                       , intent(in) :: varType   ! type of var in ds
    integer                       , intent(in) :: nPar      ! #parameters
    character(Len=DioMaxParLen), &
                  dimension(npar) , intent(in) :: pars      ! parameter names
    integer                       , intent(in) :: nLoc      ! #locations
    double precision              , intent(in) :: startTime ! startTime as julian
    double precision                           :: endTime   ! endTime as julian

    ! externals
    integer, external  :: DiofPltDefine_14a  ! actual define function

    ! body

    endTime = startTime - 1.0D+00
    pltHandle = DiofPltDefine_14a(name, runid, varType, &
                         npar, pars, nloc, startTime, endTime )

end function DiofPltDefine_14


function DiofPltDefine_14a(name, runid, varType, &
                         npar, pars, nloc, startTime, endTime ) result(pltHandle)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofPltDefine_14a
#endif
    use Dio_plt_rw

    ! return value
    integer  :: pltHandle ! pltHandle == 0 : error;
                          ! pltHandle > 0  : index to array in PLT-module.

    ! arguments
    character(Len=*)              , intent(in) :: name      ! dataset name
    character(Len=HisRunIdSize), &
           dimension(HisRunIdDim) , intent(in) :: runid     ! His RunIDLines
    integer                       , intent(in) :: varType   ! type of var in ds
    integer                       , intent(in) :: nPar      ! #parameters
    character(Len=DioMaxParLen), &
                  dimension(npar) , intent(in) :: pars      ! parameter names
    integer                       , intent(in) :: nLoc      ! #locations
    double precision              , intent(in) :: startTime ! startTime as julian
    double precision              , intent(in) :: endTime   ! endTime as julian

    ! body

    pltHandle = DioPltNewF77Handle()
    if ( pltHandle > 0 ) then
        if ( endTime > startTime ) then
            PltDataset(pltHandle) = DioPltDefine(name, runId, varType, pars, nLoc, startTime, endTime)
        else
            PltDataset(pltHandle) = DioPltDefine(name, runId, varType, pars, nLoc, startTime)
        endif
        PltDataset(pltHandle) % ds % id = pltHandle
            if (.not. DioPltOpenedOK(PltDataset(pltHandle)) ) then
             call DioPltReleaseF77Handle(pltHandle)
             pltHandle = 0
        endif
    endif

end function DiofPltDefine_14a


!*
!* Add descriptions
!*

subroutine DiofPltAddDescriptions(pltHandle, descr_type, nDescr, descriptions)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofPltAddDescriptions
#endif

    use Dio_plt_rw

    ! arguments

    integer, intent(in)              :: pltHandle    ! dataset id
    integer, intent(in)              :: descr_type   ! dio_plt_pars or dio_plt_locs
    integer, intent(in)              :: nDescr       ! #descriptions
    character(Len=DioMaxLocLen), &
        dimension(nDescr), intent(in):: descriptions ! parameter or location descriptions

    ! locals

    type(DioPltType), pointer         :: plt

    ! body

    if ( DioPltF77HandleIsValid(pltHandle) ) then
        plt => PltDataset(pltHandle)
        call DioPltAddDescriptions(plt, descr_type, descriptions)
    endif

end subroutine DiofPltAddDescriptions


!*
!* Put Values
!*


subroutine DiofPltPutFloats(pltHandle, timestep, nPar, nLoc, values)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofPltPutFloats
#endif

    use Dio_plt_rw

    ! arguments

    integer, intent(in)         :: pltHandle    ! dataset id
    integer, intent(in)         :: nPar         ! #array-items for parameters
    integer, intent(in)         :: nLoc         ! #array-items for locations
    double precision, intent(in):: timestep     ! current time step
    real, dimension(nPar,nLoc)  :: values       ! values to be put

    ! locals

    type(DioPltType), pointer         :: plt

    ! body

    if ( DioPltF77HandleIsValid(pltHandle) ) then
        plt => PltDataset(pltHandle)
        call DioPltPut(plt, timestep, values)
    endif

end subroutine DiofPltPutFloats


subroutine DiofPltPutNextFloats(pltHandle, nPar, nLoc, values)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofPltPutNextFloats
#endif

    use Dio_plt_rw

    ! arguments

    integer, intent(in)         :: pltHandle    ! dataset id
    integer, intent(in)         :: nPar         ! #array-items for parameters
    integer, intent(in)         :: nLoc         ! #array-items for locations
    real, dimension(nPar,nLoc)  :: values       ! values to be put

    ! locals

    type(DioPltType), pointer         :: plt

    ! body

    if ( DioPltF77HandleIsValid(pltHandle) ) then
        plt => PltDataset(pltHandle)
        call DioPltPut(plt, values)
    endif

end subroutine DiofPltPutNextFloats


!*
!* Get dataset and its info
!*

function DiofPltGetDataset(name) result(pltHandle)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofPltGetDataset
#endif
    use Dio_plt_rw

    ! return value
    integer                    :: pltHandle ! dataset handle

    ! arguments
    character(Len=*)           :: name      ! dataset name

    ! locals
    type(DioPltType), pointer  :: plt       ! f90 dataset handle

    ! body

    pltHandle = DioPltNewF77Handle()
    if ( pltHandle > 0 ) then
        plt => PltDataset(pltHandle)
        plt = DioPltGetDataset(name)
        plt % ds % id = pltHandle
        if ( (.not. DioPltOpenedOK(PltDataset(pltHandle)) ) .or. &
                     plt % header % varType .eq. Dio_PLT_Unknown ) then
             call DioPltReleaseF77Handle(pltHandle)
             pltHandle = 0
        endif
    endif

end function DiofPltGetDataset


function DiofPltGetNPars(pltHandle) result(retVal)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofPltGetNPars
#endif
    use dio_plt_rw

    ! return value
    integer                   :: retVal    ! #parameters

    ! arguments
    integer                   :: pltHandle ! plt LongInt handle

    ! locals
    type(DioPltType), pointer :: plt       ! pointer to PLT

    ! body
    retVal = 0
    if ( DioPltF77HandleIsValid(pltHandle) ) then
        plt => PltDataset(pltHandle)
        retVal = DioPltGetNPar(plt)
    endif

end function DiofPltGetNPars


function DiofPltGetNLocs(pltHandle) result(retVal)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofPltGetNLocs
#endif
    use dio_plt_rw

    ! return value
    integer                   :: retVal    ! #locations

    ! arguments
    integer                   :: pltHandle ! plt LongInt handle

    ! locals
    type(DioPltType), pointer :: plt       ! pointer to PLT

    ! body
    if ( DioPltF77HandleIsValid(pltHandle) ) then
        plt => PltDataset(pltHandle)
        retVal = DioPltGetNLoc(plt)
    endif

end function DiofPltGetNLocs


function DiofPltGetNTimes(pltHandle) result(retVal)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofPltGetNTimes
#endif
    use dio_plt_rw

    ! return value
    integer                   :: retVal    ! #timesteps

    ! arguments
    integer                   :: pltHandle ! plt LongInt handle

    ! locals
    type(DioPltType), pointer :: plt       ! pointer to PLT

    ! body
    if ( DioPltF77HandleIsValid(pltHandle) ) then
        plt => PltDataset(pltHandle)
        retVal = DioPltGetNTimes(plt)
    endif

end function DiofPltGetNTimes


function DiofPltGetPars(pltHandle, nPar, pars) result(retVal)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofPltGetPars
#endif
    use dio_plt_rw

    ! return value
    logical                       :: retVal    ! .true.: success

    ! arguments
    integer,          intent(IN)  :: pltHandle ! plt LongInt handle
    integer,          intent(IN)  :: nPar ! #pars expected
    character(Len=DioMaxParLen), &
        dimension(nPar), intent(OUT) :: pars      ! out: array with par.names

    ! locals
    type(DioPltType), pointer     :: plt       ! pointer to PLT

    ! body
    retVal = .false.
    if ( DioPltF77HandleIsValid(pltHandle) ) then
        plt => PltDataset(pltHandle)
        if (associated(plt % header % pars) ) then
            pars(1:nPar) = plt % header % pars(1:nPar)
            retVal = .true.
        else
            call DioStreamError(405, &
                    'DiofPltGetPars: Par names not available', plt % ds % name)
        endif
    endif
end function DiofPltGetPars


function DiofPltGetLocs(pltHandle, nLoc, locs) result(retVal)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofPltGetLocs
#endif
    use dio_plt_rw

    ! return value
    logical                       :: retVal    ! .true.: success

    ! arguments
    integer,          intent(IN)  :: pltHandle ! plt LongInt handle
    integer,          intent(IN)  :: nLoc      ! #locs expected
    character(Len=DioMaxLocLen), &
        dimension(nLoc), intent(OUT) :: locs   ! out: array with loc.names

    ! locals
    type(DioPltType), pointer     :: plt       ! pointer to PLT
    integer                       :: loc       ! loc counter

    ! body
    retVal = .false.
    if ( DioPltF77HandleIsValid(pltHandle) ) then
        plt => PltDataset(pltHandle)
        if (associated(plt % header % locs) ) then
            locs(1:nLoc) = plt % header % locs(1:nLoc)
            retVal = .true.
        else
            if ( DioPltGetStreamType(plt) .eq. Dio_WQMap_stream ) then
                do loc = 1, plt % header % nLoc
                    write(locs(loc), '(A,I10)') 'Segment', loc
                enddo
                retVal = .true.
            else
                call DioStreamError(406, 'DiofPltGetLocs: Loc names not available', &
                                           plt % ds % name)
            endif
        endif
    endif
end function DiofPltGetLocs


function DiofPltGetDescriptions(pltHandle, descr_type, nDescr, descripts) result(retVal)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofPltGetDescriptions
#endif
    use dio_plt_rw

    ! return value
    logical                       :: retVal         ! .true.: descriptions available

    ! arguments
    integer,          intent(IN)  :: pltHandle      ! plt LongInt handle
    integer,          intent(IN)  :: descr_type     ! type==1: pars, type==2:locs
    integer,          intent(IN)  :: nDescr         ! #descriptions expected
    character(Len=DioMaxDescrLen), &
        dimension(nDescr), intent(OUT) :: descripts ! out: array with descriptions 

    ! locals
    type(DioPltType), pointer     :: plt            ! pointer to PLT

    ! body
    retVal = .false.
    if ( DioPltF77HandleIsValid(pltHandle) ) then
        plt => PltDataset(pltHandle)
        if ( descr_type == dio_plt_pars .and. associated(plt % header % parDescripts) ) then
            descripts(1:nDescr) = plt % header % parDescripts(1:nDescr)
            retVal = .true.
        endif
        if ( descr_type == dio_plt_locs .and. associated(plt % header % locDescripts) ) then
            descripts(1:nDescr) = plt % header % locDescripts(1:nDescr)
            retVal = .true.
        endif
    endif
end function DiofPltGetDescriptions


function DiofPltGetIntIds(pltHandle, nLoc, intIds) result(retVal)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofPltGetIntIds
#endif
    use dio_plt_rw

    ! return value
    logical                       :: retVal    ! .true.: success

    ! arguments
    integer,          intent(IN)  :: pltHandle ! plt LongInt handle
    integer,          intent(IN)  :: nLoc      ! #locs expected
    integer, dimension(nLoc),&
                       intent(OUT):: intIds    ! out: array with loc.ids

    ! locals
    type(DioPltType), pointer     :: plt       ! pointer to PLT
    integer                       :: loc       ! loc counter

    ! body
    retVal = .false.
    if ( DioPltF77HandleIsValid(pltHandle) ) then
        plt => PltDataset(pltHandle)
        if (associated(plt % header % hisIntIds) ) then
            intIds(1:nLoc) = plt % header % hisIntIds(1:nLoc)
            retVal = .true.
        else
            if ( DioPltGetStreamType(plt) .eq. Dio_WQMap_stream ) then
                do loc = 1, plt % header % nLoc
                    intIds(loc) = loc
                enddo
                retVal = .true.
            else
                call DioStreamError(406, 'DiofPltGetIntIds: Int ids not available',&
                                             plt % ds % name)
            endif
        endif
    endif
end function DiofPltGetIntIds


function DiofPltGetTimes(pltHandle, nTim, times) result(retVal)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofPltGetTimes
#endif
    use dio_plt_rw

    ! return value
    logical                       :: retVal    ! .true.: success

    ! arguments
    integer,          intent(IN)  :: pltHandle ! plt LongInt handle
    integer,          intent(IN)  :: nTim      ! #times expected
    double precision, &
        dimension(nTim), intent(OUT) :: times     ! out: array with jul. times

    ! locals
    type(DioPltType), pointer     :: plt       ! pointer to PLT
    integer                       :: nTimInPlt ! #times in PLT
    double precision, &
        dimension(:), pointer     :: locTimes  ! local pointer to julians

    ! body

    retVal = .false.
    if ( DioPltF77HandleIsValid(pltHandle) ) then
        plt => PltDataset(pltHandle)
        nTimInPlt = DioPltGetNTimes(plt)
        if ( nTimInPlt >= nTim ) then
            locTimes => DioPltGetTimes(plt)
            if (associated(locTimes)) then
                times(1:nTim) = locTimes(1:nTim)
                if ( nTimInPlt == nTim ) then
                    retVal = .true.
                else
                    call DioStreamError(409, &
                    'DiofPltGetTimes: More times available then requested', &
                                                            plt % ds % name)
                endif
            else
                if ( nTim .eq. 0 ) then
                    ! 0 requested, 0 found, OK.
                    retVal = .true.
                else
                    call DioStreamError(408, &
                        'DiofPltGetTimes: No times available', plt % ds % name)
                endif
            endif
        else
            call DioStreamError(407, &
                    'DiofPltGetTimes: Not all times delivered', plt % ds % name)
        endif
    endif

end function DiofPltGetTimes


!
! Get one of the header lines (from HIS file, or generated)
!
function DiofPltGetHeaderLine(pltHandle, lineNr, line) result(retVal)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofPltGetHeaderLine
#endif
    use dio_plt_rw

    ! return value
    logical                       :: retVal    ! .true.: success

    ! arguments
    integer,          intent(IN)  :: pltHandle ! plt LongInt handle
    integer, intent(in)           :: lineNr   ! 1 <= linenr <= 4
    character(Len=*), intent(out) :: line     ! resulting line

    ! locals
    type(DioPltType), pointer     :: plt       ! pointer to PLT

    ! body
    retVal = .false.
    if ( DioPltF77HandleIsValid(pltHandle) ) then
        plt => PltDataset(pltHandle)
        retVal = DioPltGetHeaderLine(plt, lineNr, line)
    endif
end function DiofPltGetHeaderLine


!*
!* Get dataset REAL values
!*

function DiofPltGetNextFloats(pltHandle, npar, nloc, timestep, values) result(retVal)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofPltGetNextFloats
#endif

    use Dio_plt_rw

    ! return value
    logical                         :: retVal    ! .true.: success

    ! arguments
    integer  , intent(in)           :: pltHandle  ! dataset id
    integer, intent(in)             :: npar      ! #expected Pars
    integer, intent(in)             :: nloc      ! #expected Locs
    double precision, intent(OUT)   :: timestep  ! current time step
    real, dimension(*), intent(OUT) :: values    ! values to deliver

    ! locals
    type(DioPltType), pointer       :: plt       ! pointer to actual PLT
    integer                         :: par,loc   ! counters
    real, dimension(:,:), pointer   :: locValues ! pointer to values

    ! body

    retVal = .false.
    if ( DioPltF77HandleIsValid(pltHandle) ) then
        plt => PltDataset(pltHandle)
        retVal = DioPltGet(plt, timestep, locValues)
        if ( retVal ) then
            if ( (npar .lt. plt % header % nPar) .or. &
                 (nloc .lt. plt % header % nLoc)      ) then
                call DioStreamError(401, 'DiofPltGetNextFloats: wrong sizes ', plt % ds % name)
                retVal = .false.
            else
                do par = 1, plt % header % nPar
                    do loc = 1, plt % header % nLoc
                        values( par + (loc - 1 )*nPar) = locValues(par,loc)
                    enddo
                enddo
            endif
        endif
    endif
end function DiofPltGetNextFloats


!*
!* Get dataset double values
!*

function DiofPltGetNextDoubles(pltHandle, npar, nloc, timestep, values) result(retVal)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofPltGetNextDoubles
#endif

    use Dio_plt_rw

    ! return value
    logical                         :: retVal    ! .true.: success

    ! arguments
    integer  , intent(in)           :: pltHandle  ! dataset id
    integer, intent(in)             :: npar      ! #expected Pars
    integer, intent(in)             :: nloc      ! #expected Locs
    double precision, intent(OUT)   :: timestep  ! current time step
    double precision, &
          dimension(*), intent(OUT) :: values    ! values to deliver

    ! locals
    type(DioPltType), pointer       :: plt       ! pointer to actual PLT
    integer                         :: par,loc   ! counters
    double precision, &
          dimension(:,:), pointer   :: locValues ! pointer to values

    ! body

    retVal = .false.
    if ( DioPltF77HandleIsValid(pltHandle) ) then
        plt => PltDataset(pltHandle)
        retVal = DioPltGet(plt, timestep, locValues)
        if ( retVal ) then
            if ( (npar .lt. plt % header % nPar) .or. &
                 (nloc .lt. plt % header % nLoc)      ) then
                call DioStreamError(401, 'DiofPltGetNextDoubles: wrong sizes ', plt % ds % name)
                retVal = .false.
            else
                do par = 1, plt % header % nPar
                    do loc = 1, plt % header % nLoc
                        values( par + (loc - 1 )*nPar) = locValues(par,loc)
                    enddo
                enddo
            endif
        endif
    endif
end function DiofPltGetNextDoubles


!
! Function to read a selection of real values from a file
!

function DiofPltGetSelectionFloats(pltHandle, nPar, parIndx, nLoc, locIndx,&
                                     nTim, timIndx, values) result(retVal)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofPltGetSelectionFloats
#endif
    use Dio_plt_rw

    ! return value
    logical                               :: retVal    ! .true.: success

    ! arguments
    integer, intent(in)                   :: pltHandle ! dataset
    integer, intent(in)                   :: nPar, &   ! #indices per dimension
                                             nLoc, &
                                             nTim
    integer, dimension(nPar) , intent(in) :: parIndx   ! indices per dimension
    integer, dimension(nLoc) , intent(in) :: locIndx   ! indices per dimension
    integer, dimension(nTim) , intent(in) :: timIndx   ! indices per dimension
    real, dimension(nPar,nLoc,nTim), &
                               intent(out):: values    ! selected values

    ! locals
    type(DioPltType), pointer             :: plt

    ! body
    retVal = .false.
    if ( DioPltF77HandleIsValid(pltHandle) ) then
        plt => PltDataset(pltHandle)
        retVal = DioPltGetSelectionReals(plt, nPar, parIndx, nLoc, locIndx,&
                                                nTim, timIndx, values)
    endif

end function DiofPltGetSelectionFloats


!
! Function to read a selection of double values from a file
!

function DiofPltGetSelectionDoubles(pltHandle, nPar, parIndx, nLoc, locIndx,&
                                     nTim, timIndx, values) result(retVal)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofPltGetSelectionDoubles
#endif
    use Dio_plt_rw

    ! return value
    logical                               :: retVal    ! .true.: success

    ! arguments   
    integer, intent(in)                   :: pltHandle ! dataset
    integer, intent(in)                   :: nPar, &   ! #indices per dimension
                                             nLoc, &
                                             nTim
    integer, dimension(nPar) , intent(in) :: parIndx   ! indices per dimension
    integer, dimension(nLoc) , intent(in) :: locIndx   ! indices per dimension
    integer, dimension(nTim) , intent(in) :: timIndx   ! indices per dimension
    double precision, dimension(nPar,nLoc,nTim), &
                               intent(out):: values    ! selected values

    ! locals
    type(DioPltType), pointer             :: plt

    ! body
    retVal = .false.
    if ( DioPltF77HandleIsValid(pltHandle) ) then
        plt => PltDataset(pltHandle)
        retVal = DioPltGetSelectionDoubles(plt, nPar, parIndx, nLoc, locIndx,&
                                                  nTim, timIndx, values)
    endif

end function DiofPltGetSelectionDoubles


subroutine DiofPltClose(pltHandle)
#if defined(USE_DIOF_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: DiofPltClose
#endif
    use Dio_plt_rw

    ! arguments
    integer  , intent(in)             :: pltHandle     ! dataset id

    ! locals
    type(DioPltType), pointer         :: plt

    ! body
    if ( DioPltF77HandleIsValid(pltHandle) ) then
        plt => PltDataset(pltHandle)
        call DioPltDestroy(plt)
        call DioPltReleaseF77Handle(pltHandle)
    endif

end subroutine DiofPltClose

