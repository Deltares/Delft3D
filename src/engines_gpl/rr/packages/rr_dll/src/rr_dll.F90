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

 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!
!!! SE_RR.F90: *S*obek*E*ngine API DLL interface on top of RR module
!!!
!!! (c) Deltares, sep 2004
!!!
!!! Stef.Hummel@deltares.nl
!!! Hans.vanPutten@Deltares.nl
!!!
!!! Version 1.01
!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!
!!! Sobek-OpenMI-support
!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module sobek_RR_support

    use wl_open_mi_support

    !
    ! include RR modules
    !
    use rr_open_mi_support
    use rrmodule

    implicit none
    !
    ! return values:
    !
    integer, parameter :: SE_UNDEFINED    = -1

    double precision :: start_time_as_mjd = -1
    double precision :: end_time_as_mjd   = -1

    integer, parameter :: maxStringLen    = 10
    character(Len=maxStringLen) :: RRComponentID  = 'RR'

end module sobek_RR_support


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!
!!! SE_<funct>: *S*obek*E*ngine API DLL functions
!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


function SE_Initialize(model, schematization) result(retVal)

    !DEC$ ATTRIBUTES DLLEXPORT :: SE_Initialize
    use sobek_RR_support
    use rr_dll_version_module
    use globals

    use rrmodule
    
    implicit none
    
    ! return value

    integer                       :: retVal         ! >=0 : Success; <0 : Error

    ! arguments

    character(len=*), intent(in)  :: model          ! model identifier
    character(len=*), intent(in)  :: schematization ! schematization identifier

    ! locals

    integer                    , parameter          :: argCount = 3
    character(Len=oes_path_len), dimension(argCount):: argsToModel
    integer                                         :: modelHandle
    integer                                         :: dummyNumEvents, dummyNumTimesteps
    integer                                         :: finalizeResult
    character(len=oes_path_len)                     :: workingDir
    
    ! body

    retVal = -1

    !
    ! Identification string
    !
    call getfullversionstring_rr_dll(rr_version_string)

    if ( OesModelCoreIsInitialized(model, schematization) ) then
        retVal = 0
        return
    endif
    
    ! body

    call OesDirName(schematization, workingDir)

    if ( trim(model) == trim(RRComponentID) ) then        
        argsToModel(1) = 'D:\RR_FROM_OPENMI.EXE'
        argsToModel(2) = schematization
        argsToModel(3) = trim(workingDir) // '/sobek_3b.rtn'
        retVal = RRCreate(modelHandle, argsToModel)
        if ( retVal == 0 ) then
            retVal = RRInitialize(modelHandle, dummyNumEvents)
            if ( retVal == 0 ) then
                retVal = RRInitializeEvent(modelHandle, 1, dummyNumTimesteps)
            endif
        endif
    else 
        write(oesError,'(A)') 'Incorrect component ID'
    endif

    if ( retVal == 0 ) then
        call OesModelSetCoreInitialized(model, schematization, .true.)
    endif
    
end function SE_Initialize

subroutine SE_GetCurrentTime(componentID, schemID, retVal)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_GetCurrentTime
    use sobek_RR_support

    implicit none

    ! result
    double precision :: retVal   ! Current Model time (Modified Julian)

    ! arguments
    character(Len=*), intent(in) :: componentID  ! RR, RTC, etc.
    character(Len=*), intent(in) :: schemID      ! schem. file (*.fnm)

    ! locals
    integer                      :: modelHandle  ! handle to computation

    ! body

    retVal = SE_UNDEFINED

    modelHandle = OesModelFind(componentID, schemID)
    if ( modelHandle <= 0 ) return

    if ( componentID == RRcomponentID ) then
        retVal = RRGetCurrentTime(modelHandle)
    else
        write(oesError,'(A)') 'Incorrect component ID'
    endif    

end subroutine SE_GetCurrentTime


function SE_GetTimeHorizon(componentID, schemID, startMJD, endMJD) result(retVal)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_GetTimeHorizon
    use sobek_RR_support

    implicit none

    ! result
    integer :: retVal   ! 0: OK

    ! arguments
    character(Len=*), intent(in) :: componentID ! RR, RTC, etc.
    character(Len=*), intent(in) :: schemID     ! schem. file (*.fnm)
    double precision, intent(out):: startMJD    ! Model's start time (Modified Julian)
    double precision, intent(out):: endMJD      ! Model's end time (Modified Julian)

    ! locals
    integer                      :: modelHandle  ! handle to computation

    ! body

    retVal = SE_UNDEFINED

    modelHandle = OesModelFind(componentID, schemID)
    if ( modelHandle <= 0 ) return

    if ( componentID == RRcomponentID ) then
        retVal = RRGetTimeHorizon(modelHandle, startMJD, endMJD)
    else
        write(oesError,'(A)') 'Incorrect component ID'
    endif

end function SE_GetTimeHorizon


function SE_GetDeltaT(componentID, schemID, deltaTAsMJD) result(retVal)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_GetDeltaT
    use sobek_RR_support

    implicit none

    ! result
    integer                      :: retVal       ! Delta T (Modified Julian)

    ! arguments
    character(Len=*), intent(in) :: componentID  ! RR, RTC, etc.
    character(Len=*), intent(in) :: schemID      ! schem. file (*.fnm)
    double precision, intent(out):: deltaTAsMJD  ! time step size as modified julian day

    ! locals
    integer                      :: modelHandle  ! handle to computation

    ! body

    retVal = SE_UNDEFINED

    modelHandle = OesModelFind(componentID, schemID)
    if ( modelHandle <= 0 ) return

    if ( componentID == RRcomponentID ) then
        deltaTAsMJD = RRGetDeltaT(modelHandle)
        retVal = 0
    else
        write(oesError,'(A)') 'Incorrect component ID'
    endif

end function SE_GetDeltaT


subroutine SE_GetInputTime(componentID, schemID, quantityID, elementSetID, retVal)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_GetInputTime
    use sobek_RR_support

    implicit none

    ! result
    double precision :: retVal       ! Current Model time (Modified Julian)

    ! arguments
    character(Len=*), intent(in) :: componentID  ! RR, RTC, etc.
    character(Len=*), intent(in) :: schemID      ! schem. file (*.fnm)
    character(len=*)             :: quantityID   ! quantity identifier
    character(len=*)             :: elementSetID ! elementset identifier

    ! locals
    integer                      :: modelHandle  ! handle to computation

    ! body

    retVal = SE_UNDEFINED

    modelHandle = OesModelFind(componentID, schemID)
    if ( modelHandle <= 0 ) return  

    if ( componentID == RRcomponentID ) then
        retVal = RRGetInputTime(modelHandle)
    else
        write(oesError,'(A)') 'Incorrect component ID'
    endif

end subroutine SE_GetInputTime

function SE_PerformTimeStep(componentID, schemID) result(retVal)
    use sobek_RR_support

    !DEC$ ATTRIBUTES DLLEXPORT :: SE_PerformTimeStep
    implicit none
    integer :: retVal

    ! arguments
    character(Len=*), intent(in) :: componentID  ! RR, RTC, etc.
    character(Len=*), intent(in) :: schemID      ! schem. file (*.fnm)

    ! locals
    integer                      :: modelHandle  ! handle to computation

    ! body

    retVal = SE_UNDEFINED

    modelHandle = OesModelFind(componentID, schemID)
    if ( modelHandle <= 0 ) return

    if ( componentID == RRcomponentID ) then
        RR_ITimestep = RR_ITimestep + 1
        retVal = RRPerformTimeStep(modelHandle,1,RR_ITimestep)
    else
        write(oesError,'(A)') 'Incorrect component ID'
    endif

end function SE_PerformTimeStep

function SE_Finalize(componentID, schemID) result(retVal)
    use sobek_RR_support

    !DEC$ ATTRIBUTES DLLEXPORT :: SE_Finalize
    implicit none

    ! result
    integer     :: retVal           ! retVal == 0 : success

    ! arguments
    character(Len=*), intent(in) :: componentID  ! RR, RTC, etc.
    character(Len=*), intent(in) :: schemID      ! schem. file (*.fnm)
    integer                      :: modelHandle  ! handle to Model computation

    ! arguments
    integer    :: eventCounter = 1 ! Current event

    ! body

    retVal = -99

    modelHandle = OesModelFind(componentID, schemID)
    if ( modelHandle <= 0 ) return

    if ( componentID == RRcomponentID ) then
        retVal = RRFinalizeEvent(modelHandle, eventCounter)
        if ( retVal == 0 ) then
           retVal = RRFinalize(modelHandle)
           call OesDestroy()
        endif
    else
        write(oesError,'(A)') 'Incorrect component ID'
    endif
    
end function SE_Finalize

!> If retVal = 0, function returns Values array of length ElementCount 
!! for specific elementset and quantity combination (character strings)
function SE_GetValues(componentID, schemID, QuantityID,ElementsetID,ElementCount,Values) result(retVal)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_GetValues
    use wl_open_mi_support
    use RRModule
    
    implicit none

    ! result
    integer :: retVal                                !< retVal == 0 : success

    ! arguments
    character(Len=*), intent(in)     :: componentID  !< RR, RTC, etc.
    character(Len=*), intent(in)     :: schemID      !< schem. file (*.fnm)
    character(Len=*), intent(in)     :: QuantityID   !< quant. identifier
    character(Len=*), intent(in)     :: ElementsetID !< elem.set. identifier
    integer         , intent(in)     :: ElementCount !< #elems in elementset
    double precision, intent(out), &
           dimension(1:ElementCount) :: Values       !< values in elemenset

    ! locals
    integer                          :: modelHandle      ! handle to computation
    integer                          :: ElementsetHandle ! handle to elementset
    integer                          :: QuantityHandle   ! handle to quantity

    ! body

    retVal  = OES_UNDEFINED ; Values  = 0

    modelHandle = OesModelFind(trim(componentID)//'-Wrapper', schemID)
    if ( modelHandle <= 0 ) return

    if ( .not. OMI_RR_GetIntHandles(QuantityID, ElementsetID, ElementsetHandle, QuantityHandle) ) then
        retVal = OES_UNDEFINED
        return
    endif

    if ( OMI_RR_GetDataByIntID(QuantityHandle, ElementsetHandle, ElementCount, Values) ) retVal = 0

end function SE_GetValues

!> If retVal = 0, function returns Values array of length ElementCount 
!! for specific elementset and quantity combination (integer handles)
function SE_GetValuesByIntId(componentID, schemID, QuantityIntID,ElementsetIntID,ElementCount,Values) result(retVal)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_GetValuesByIntId
    use wl_open_mi_support
    use RRModule
    
    implicit none

    ! result
    integer :: retVal                                   !< retVal == 0 : success

    ! arguments
    character(Len=*), intent(in)     :: componentID     !< RR, RTC, etc.
    character(Len=*), intent(in)     :: schemID         !< schem. file (*.fnm)
    integer         , intent(in)     :: QuantityIntID   !< quant. identifier
    integer         , intent(in)     :: ElementsetIntID !< elem.set. identifier
    integer         , intent(in)     :: ElementCount    !< #elems in elementset
    double precision, intent(out), &
           dimension(1:ElementCount) :: Values          !< values in elemenset

    ! locals
    integer                          :: modelHandle  ! handle to computation

    ! body

    retVal  = OES_UNDEFINED ; Values  = 0

    modelHandle = OesModelFind(trim(componentID)//'-Wrapper', schemID)
    if ( modelHandle <= 0 ) then
        modelHandle = OesModelFind(trim(componentID), schemID)
        if ( modelHandle <= 0 ) return
    endif

    if ( OMI_RR_GetDataByIntID(QuantityIntID, ElementsetIntID, ElementCount, Values) ) retVal = 0

end function SE_GetValuesByIntId

subroutine SE_SET_EI_GROUPS(groups)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_SET_EI_GROUPS
    use rr_open_mi_support

    implicit none

    ! arguments
    integer, intent(in) :: groups         ! 0 : all groups, bit-indication per group otherwise

    ! TODO: handle bits (0: All, 1: NHI, 2: ..., etc)
    if (groups == 1) then
       ! do nothing for now, empty routine
    endif
    
end subroutine SE_SET_EI_GROUPS



