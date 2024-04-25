module open_mi_dio
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
!
!
!!!
!!! open_mi_dio.F90: OD - module (*O*penMI exchange by means of *D*elftIO)
!!!
!!! (c) Deltares, feb. 2005
!!!
!!! Stef.Hummel@deltares.nl
!!!
!!! File organization:
!!!
!!!   DECLARATIONS
!!!   - Data Type Definitions
!!!   - Overloaded Functions
!!!   - Exchanged Items datastore
!!!
!!!   IMPLEMENTATION
!!!
!!!   - Public Functions:
!!!
!!!     .  Initialization / general functions
!!!     .  Create exchanged items
!!!     .  Put / Get values
!!!
!!!   - Private Functions:
!!!
!!!     .  Add / find exchanged items
!!!
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use dio_plt_rw
    !
    implicit none
!
! Module constants/enumerations
!
    integer, parameter :: odMaxNumExchanges = 10

    real               :: odUndefinedValue       = -999.0    ! missing value indicator
    double precision   :: odUndefinedValueDouble = -999.0D00 ! missing value indicator (double)
!
! Module variables: accept or provide items
!
    integer, parameter :: od_unknown   = 0   ! not known yet
    integer, parameter :: od_providing = 1   ! provided by caller
    integer, parameter :: od_accepting = 2   ! accepted by caller
!
! Overloaded Functions
!
    interface OD_ExchItemCreate
        module procedure OD_ExchItemCreate_WithIDs   ! Called by providing component
        module procedure OD_ExchItemCreate_WithSizes ! Called by providing component of 2d fields
        module procedure OD_ExchItemCreate_NoIDs     ! Called by accepting component
    end interface

    interface OD_Put
        module procedure OD_Put_1D                  ! Put/Get values
        module procedure OD_Put_2D
    end interface

    interface OD_Get
        module procedure OD_Get_1D
        module procedure OD_Get_2D
    end interface
!
! Definition of data types
!
    !
    ! info for each exchanged item
    !
    type t_od_exchange

        character(Len=DioMaxParLen)    :: quantID    ! Quantity Identifier
        character(Len=DioMaxLocLen)    :: elmsetID   ! ElementSet Identifier

        integer                        :: role       ! Providing / Accepting
        character(Len=DioMaxStreamLen) :: exchDsName ! Exchange Dataset Name
        character(Len=DioMaxStreamLen) :: dumpDsName ! Dumpfile name for dataset

        type(DioPltType)               :: exchPlt    ! DelftIO PLT for shared mem. exchange
        type(DioPltType)               :: dumpPlt    ! DelftIO PLT for dumping values

    end type t_od_exchange
    !
    ! stored with all exchanged items
    !
    type t_od_store

        character(Len=DioMaxParLen)      :: myComponentID     ! my Component Identifier
        character(Len=DioMaxParLen)      :: otherComponentID  ! related Component Identifier

        type(t_od_exchange), &
            dimension(odMaxNumExchanges) :: exchanges         ! data exchange exchPlt's
        integer                          :: numExchanges      ! # data exchange exchPlt's

        logical                          :: fake_comm         ! Fake communication?

    end type t_od_store
!
! Module variables
!
    type(t_od_store)  , target  :: odStore                               ! Exchanged items store
    character(Len=250), private :: odLastError   = ''                    ! Last error in OD module

    logical           , private :: module_has_been_initialized = .false. ! Initialize done?
    logical           , public  :: dumpValues = .false.                  ! Dump exchanged values
contains
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! PUBLIC functions, INITIALIZATION / GENERAL 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
! Initialization
!
!==============================================================================
function OD_Init(myComponentID, otherComponentID, fake_comm) result(success)
    !
    ! return value
    logical :: success  ! .true.: all OK
    !
    ! arguments
    character(Len=*), intent(in)  :: mycomponentID    ! my Component Identifier
    character(Len=*), intent(in)  :: otherComponentID ! related Component Identifier
    logical                       :: fake_comm        ! Fake communication?
    !
    ! body: intialize store
    if ( .not. module_has_been_initialized ) then
        odStore % myComponentID    = myComponentID
        odStore % otherComponentID = otherComponentID
        !
        odStore % exchanges % quantID    = ''
        odStore % exchanges % elmsetID   = ''
        odStore % exchanges % role       = od_unknown
        odStore % exchanges % exchDsName = ''
        odStore % exchanges % dumpDsName = ''
        !
        odStore % numExchanges           = 0
        !
        odStore % fake_comm = fake_comm
        !
        module_has_been_initialized = .true.
    endif
    success = .true.
end function OD_Init
!
!
!
! Fake communication or not.
!
!==============================================================================
function DO_FakeComm() result(fake_comm)
    !
    ! return value
    logical :: fake_comm ! Fake communication?
    !
    ! body
    fake_comm = odStore % fake_comm
end function DO_FakeComm
!
!
!
! Get last error
!
!==============================================================================
subroutine DO_LastError(errString)
    !
    ! arguments
    character(Len=*), intent(out) :: errString    ! error string
    !
    ! body
    errString   = odLastError
    odLastError = ''
end subroutine DO_LastError
!
!
!
! Finalize
!
!==============================================================================
subroutine OD_Finalize(myComponentID, otherComponentID)
    !
    ! arguments
    character(Len=*), intent(in) :: myComponentID
    character(Len=*), intent(in) :: otherComponentID
    !
    ! locals
    integer   :: e
    !
    ! body
    if ( myComponentID    == odStore % myComponentID    .and. &
         otherComponentID == odStore % otherComponentID       ) then
        do e = 1, odStore % numExchanges
            call DioPltDestroy(odStore % exchanges(e) % exchPlt)
        enddo
        odStore % numExchanges = 0
    endif
end subroutine OD_Finalize
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! PUBLIC functions, CREATE exchanged items
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
! Create provided exchange item (called by providing component)
!
!==============================================================================
function OD_ExchItemCreate_WithIDs(quantID, elmsetID, elmIDs, role, startTime, dumpDsName) result(success)
    !
    ! return value
    logical :: success  ! .true.: all OK
    !
    ! arguments
    character(Len=*)             , intent(in) :: quantID    ! Quantity Identifier
    character(Len=*)             , intent(in) :: elmsetID   ! ElementSet Identifier
    character(Len=*),dimension(:), intent(in) :: elmIDs     ! Element ID's in elm.set
    integer                      , intent(in) :: role       ! Providing / Accepting
    double precision             , intent(in) :: startTime  ! Start time for Dio PLT
    character(Len=*)             , intent(in), &
                                   optional   :: dumpDsName ! Name of dump file (his file), must be different
                                                            ! from data set name (to be able to distinguish
                                                            ! between them in the dio config)
    !
    ! locals
    type(t_od_exchange), pointer             :: od_exchange ! pointer to exchanged item
    character(Len=DioMaxParLen),dimension(1) :: arrQuant    ! Array representation of qant.
    !
    ! body
    success = .false.
    if ( role /= od_providing ) then
        write(odLastError, '(4A)') 'OPENMI_DIO: ERROR 105: role must be PROVIDING for ', &
                                        trim(quantID), '/', trim(elmsetID)
    else
        if (present(dumpDsName)) then
            od_exchange => odAddExchange(quantID, elmsetID, role, dumpDsName)
        else
            od_exchange => odAddExchange(quantID, elmsetID, role)
        endif
        if ( .not. associated(od_exchange) ) then
            write(odLastError, '(4A)') 'OPENMI_DIO: ERROR 104: could not create exchange item ', &
                                        trim(quantID), '/', trim(elmsetID)
        else
            arrQuant(1) = quantID
            od_exchange % exchPlt = DioPltDefine(od_exchange % exchDsName,      &
                                             dio_plt_real, arrQuant,  elmIDs, startTime )
            if ( DioGetLastError() /= 0 ) then
                write(odLastError,'(A)') 'OPENMI_DIO: ERROR 100: See dio error file'
            else
                success = .true.
                if (dumpValues) then
                    od_exchange % dumpPlt = DioPltDefine(od_exchange % dumpDsName,      &
                                                 dio_plt_real, arrQuant,  elmIDs, startTime )
                endif
            endif
        endif
    endif
end function OD_ExchItemCreate_WithIDs
!
!
! Create provided exchange item, 2d field (called by providing component)
!
!==============================================================================
function OD_ExchItemCreate_WithSizes(quantID, elmsetID, mMax, nMax, role, startTime) result(success)
    !
    ! return value
    logical :: success  ! .true.: all OK
    !
    ! arguments
    character(Len=*)             , intent(in) :: quantID    ! Quantity Identifier
    character(Len=*)             , intent(in) :: elmsetID   ! ElementSet Identifier
    integer                      , intent(in) :: mMax       ! # values m-dir.
    integer                      , intent(in) :: nMax       ! # values n-dir.
    integer                      , intent(in) :: role       ! Providing / Accepting
    double precision             , intent(in) :: startTime  ! Start time for Dio PLT
    !
    ! locals
    type(t_od_exchange), pointer              :: od_exchange ! pointer to exchanged item
    character(Len=DioMaxParLen), dimension(1) :: arrQuant    ! Array representation of qant.
    character(Len=DioMaxLocLen), &
       allocatable, dimension(:)              :: locIds      ! Row/Col loc ids
    integer                                   :: m, n, mn    ! loop counters

    !
    ! body
    allocate(locIds(mMax*nMax))
    do n = 1, nMax
        do m = 1, mMax
           mn = (n-1) * mMax + m
           write(locIds(mn), '(A,I4.4,A,I4.4,A)') 'row,col[', n, ',', m, ']'
        enddo
    enddo
    success = OD_ExchItemCreate_WithIDs(quantID, elmsetID, locIds, role, startTime)
    deallocate(locIds)

end function OD_ExchItemCreate_WithSizes
!
!
!
! Create accepted exchange item (called by accepting component)
!
!==============================================================================
function OD_ExchItemCreate_NoIDs(quantID, elmsetID, role) result(success)
    !
    ! return value
    logical :: success  ! .true.: all OK
    !
    ! arguments
    character(Len=*)             , intent(in) :: quantID  ! Quantity Identifier
    character(Len=*)             , intent(in) :: elmsetID ! ElementSet Identifier
    integer                      , intent(in) :: role     ! Providing / Accepting
    !
    ! locals
    type(t_od_exchange), pointer :: od_exchange
    !
    ! body
    success = .false.
    if ( role /= od_accepting ) then
        write(odLastError, '(4A)') 'OPENMI_DIO: ERROR 105: role must be ACCEPTING for ', &
                                        trim(quantID), '/', trim(elmsetID)
    else
        od_exchange => odAddExchange(quantID, elmsetID, role)
        if ( .not. associated(od_exchange) ) then
            write(odLastError, '(4A)') 'OPENMI_DIO: ERROR 104: could not create exchange item ', &
                                        trim(quantID), '/', trim(elmsetID)
        else
            od_exchange % exchPlt = DioPltGetDataset(od_exchange % exchDsName)
            if ( DioGetLastError() /= 0 ) then
                write(odLastError,'(A)') 'OPENMI_DIO: ERROR 100: See dio error file'
            else
                success = .true.
            endif
        endif
    endif
end function OD_ExchItemCreate_NoIDs
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! PUBLIC functions, PUT / GET values
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
! Put 1-dimensional value-arrays
!
!==============================================================================
function OD_Put_1D(julianTime, quantID, elmsetID, values) result(success)
    !
    ! return value
    logical :: success  ! .true.: all OK
    !
    ! arguments
    double precision  , intent(in) :: julianTime   ! current julianTime
    character(Len=*)  , intent(in) :: quantID      ! Quantity Identifier
    character(Len=*)  , intent(in) :: elmsetID     ! ElementSet Identifier
    real, dimension(:), intent(in) :: values       ! Exch.Item values
    !
    ! locals
    type(t_od_exchange), pointer      :: od_exchange
    real, dimension(:,:), allocatable :: values2D
    !
    ! body
    success = .false.
    od_exchange => odFindExchange(quantID, elmsetID, od_providing)
    if ( .not. associated(od_exchange) ) then
        write(odLastError, '(4A)') &
              'OPENMI_DIO: ERROR 103: could not find outgoing exchange item ', &
                                    trim(quantID), '/', trim(elmsetID)
    else
        allocate(values2D(1,size(values)))
        values2D(1,:) = values(:)
        if ( .not. DO_FakeComm() ) then
            call DioPltPut(od_exchange % exchPlt, julianTime, values2D)
        endif
        if (dumpValues) then
            call DioPltPut(od_exchange % dumpPlt, julianTime, values2D)
        endif
        deallocate(values2D)
        success = .true.
    endif
end function OD_Put_1D
!
!
!
! Put 2-dimensional value-arrays
!
!==============================================================================
function OD_Put_2D(julianTime, quantID, elmsetID, values) result(success)
    !
    ! return value
    logical :: success  ! .true.: all OK
    !
    ! arguments
    double precision    , intent(in) :: julianTime  ! current julianTime
    character(Len=*)    , intent(in) :: quantID     ! Quantity Identifier
    character(Len=*)    , intent(in) :: elmsetID    ! ElementSet Identifier
    real, dimension(:,:), intent(in) :: values      ! Exch.Item values
    !
    ! locals
    type(t_od_exchange) , pointer    :: od_exchange
    !
    ! body
    success = .false.
    od_exchange => odFindExchange(quantID, elmsetID, od_providing)
    if ( .not. associated(od_exchange) ) then
        write(odLastError, '(4A)')                                           &
            'OPENMI_DIO: ERROR 103: could not find outgoing exchange item ', &
                   trim(quantID), '/', trim(elmsetID)
    else
        if ( .not. DO_FakeComm() ) then
            call DioPltPut(od_exchange % exchPlt, julianTime, values)
        endif
        if (dumpValues) then
            call DioPltPut(od_exchange % dumpPlt, julianTime, values)
        endif
        success = .true.
    endif
end function OD_Put_2D
!
!
!
! Get 1-dimensional value-arrays
!
!==============================================================================
function OD_Get_1D(quantID, elmsetID, values) result(success)
    !
    ! return value
    logical :: success  ! .true.: all OK
    !
    ! arguments
    character(Len=*)  , intent(in) :: quantID   ! Quantity Identifier
    character(Len=*)  , intent(in) :: elmsetID  ! ElementSet Identifier
    real, dimension(:), intent(out):: values    ! Exch.Item values
    !
    ! locals
    type(t_od_exchange), pointer  :: od_exchange
    real, dimension(:,:), pointer :: values2D
    integer                       :: numValues
    !
    ! body
    success = .false.
    values = odUndefinedValue
    if ( DO_FakeComm() ) then
        values  = 0.0
        success = .true.
    else
        od_exchange => odFindExchange(quantID, elmsetID, od_accepting)
        if ( .not. associated(od_exchange) ) then
            write(odLastError, '(4A)')                                           &
                'OPENMI_DIO: ERROR 101: could not find incoming exchange item ', &
                                                trim(quantID), '/', trim(elmsetID)
        else
            if ( .not. DioPltGet(od_exchange % exchPlt, values2D) ) then
                    write(odLastError, '(4A)')                                    &
                            'OPENMI_DIO: ERROR 102: Didn''t receive values for ', &
                                            trim(quantID), '/', trim(elmsetID)
            else
                numValues = min(size(values),size(values2D,2))
                values(1:numValues) = values2D(1,1:numValues)
                success = .true.
            endif
        endif
    endif
end function OD_Get_1D
!
!
!
! Get 2-dimensional value-arrays
!
!==============================================================================
function OD_Get_2D(quantID, elmsetID, values) result(success)
    !
    ! return value
    logical :: success  ! .true.: all OK
    !
    ! arguments
    character(Len=*)    , intent(in) :: quantID   ! Quantity Identifier
    character(Len=*)    , intent(in) :: elmsetID  ! ElementSet Identifier
    real, dimension(:,:), intent(out):: values    ! Exch.Item values
    !
    ! locals
    type(t_od_exchange) , pointer :: od_exchange
    real, dimension(:,:), pointer :: values2D
    !
    ! body
    success = .false.
    values  = odUndefinedValue
    if ( DO_FakeComm() ) then
        success = .true.
    else
        od_exchange => odFindExchange(quantID, elmsetID, od_accepting)
        if ( .not. associated(od_exchange) ) then
            write(odLastError, '(4A)')                                                    &
                        'OPENMI_DIO: ERROR 101: could not find incoming exchange item ',  &
                                        trim(quantID), '/', trim(elmsetID)
        else
            if ( .not. DioPltGet(od_exchange % exchPlt, values2D) ) then
                write(odLastError, '(4A)')                                                &
                        'OPENMI_DIO: ERROR 102: Didn''t receive values for ',             &
                                            trim(quantID), '/', trim(elmsetID)
            else
                values = values2D
                success = .true.
            endif
        endif
    endif
end function OD_Get_2D
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! PRIVATE functions
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
! Add exchanged item
!
!==============================================================================
function odAddExchange(quantID, elmsetID, role, dumpDsName) result(od_exchange)
    !
    ! return value
    type(t_od_exchange), pointer  :: od_exchange
    !
    ! arguments
    character(Len=*), intent(in) :: quantID     ! Quantity Identifier
    character(Len=*), intent(in) :: elmsetID    ! ElementSet Identifier
    integer         , intent(in) :: role        ! Providing / Accepting
    character(Len=*), intent(in), &
                      optional   :: dumpDsName  ! Name of dump file (his file), must be different
                                                ! from data set name (to be able to distinguish
                                                ! between them in the dio config)
    !
    ! locals
    character(Len=20)            :: inMemPrefix ! Prefix for in mem stream
    !
    ! body
    nullify(od_exchange)
    if ( odStore % numExchanges < odMaxNumExchanges ) then
        odStore % numExchanges = odStore % numExchanges + 1
        od_exchange => odStore % exchanges(odStore % numExchanges)
        !
        od_exchange % quantID   = quantID
        od_exchange % elmsetID  = elmsetID
        od_exchange % role      = role
        if ( role == od_providing ) then
            inMemPrefix = trim(odStore % myComponentID   ) // '2' // &
                          trim(odStore % otherComponentID)
        else
            inMemPrefix = trim(odStore % otherComponentID) // '2' // &
                          trim(odStore % myComponentID   )
        endif
        od_exchange % exchDsName = trim(inMemPrefix) // '-' // &
                                   trim(quantID)     // '-' // &
                                   trim(elmsetID)    // '.shm'
        if (present(dumpDsName)) then
            od_exchange % dumpDsName = dumpDsName
        else
            !
            ! Not only the extension, but also the base name must be different for the dumpDsName compared to the exchDsName
            od_exchange % dumpDsName = trim(inMemPrefix) // '-' // &
                                       trim(quantID)     // '-' // &
                                       trim(elmsetID)    // '_log.his'
        endif                                         
    endif
end function odAddExchange
!
!
!
! Find exchanged item
!
!==============================================================================
function odFindExchange(quantID, elmsetID, role) result(od_exchange)
    !
    ! return value
    type(t_od_exchange), pointer  :: od_exchange
    !
    ! arguments
    character(Len=*), intent(in) :: quantID   ! Quantity Identifier
    character(Len=*), intent(in) :: elmsetID  ! ElementSet Identifier
    integer         , intent(in) :: role      ! Providing / Accepting
    !
    ! locals
    integer :: e         ! exchange loop counter
    !
    ! body
    nullify(od_exchange)
    do e = 1, odStore % numExchanges
        if ( odStore % exchanges(e) % quantID  == quantID  .and. &
             odStore % exchanges(e) % elmsetID == elmsetID .and. &
             odStore % exchanges(e) % role     == role           ) then
            od_exchange => odStore % exchanges(e)
        endif
    enddo
end function odFindExchange




end module open_mi_dio

