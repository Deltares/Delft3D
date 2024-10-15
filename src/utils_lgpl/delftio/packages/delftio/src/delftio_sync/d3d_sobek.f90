module D3D_Sobek
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
!!! d3d_sobek.F90: D3S - module (*D*elft-*3*D *S*obek)
!!!
!!! (c) Deltares, feb. 2005
!!!
!!! Stef.Hummel@deltares.nl, Herman.Kernkamp@deltares.nl
!!!
!!! File organization:
!!!
!!!   DECLARATIONS
!!!   - Data Type Definitions
!!!   - Overloaded Functions
!!!   - Module variables
!!!
!!!   IMPLEMENTATION
!!!
!!!   - Public Functions:
!!!
!!!     .  Initialization / general functions
!!!         - initialization Delft3D side
!!!         - initialization Sobek side
!!!         - last error, do-coupling, finalize
!!!     .  Put/Get functions delft3d side
!!!         - put water levels
!!!         - get volumes
!!!     .  Put/Get functions sobek side
!!!         - set discharges
!!!         - put volumes (== cumulated discharges)
!!!         - get water levels
!!!
!!!   - Private Functions:
!!!
!!!     .  Parse configuration file
!!!     .  Dump configuration
!!!
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Constants/enumerations
!
    integer, parameter, private :: D3S_ID_LEN   = 80  ! identifier string length
!
! shared mem block identifiers for Delta-T exchange
!
    character(Len=D3S_ID_LEN), parameter :: D3D2SBK_DeltaT_Name     = 'D3D2SBK_DB_DeltaT'
    character(Len=D3S_ID_LEN), parameter :: SBK2D3D_DeltaT_Name     = 'SBK2D3D_DB_DeltaT'
    character(Len=D3S_ID_LEN), parameter :: D3D2SBK_Starttime_Name  = 'D3D2SBK_DB_Starttime'
    character(Len=D3S_ID_LEN), parameter :: SBK2D3D_Starttime_Name  = 'SBK2D3D_DB_Starttime'
    character(Len=D3S_ID_LEN), parameter :: D3D2SBK_Endtime_Name    = 'D3D2SBK_DB_Endtime'
    character(Len=D3S_ID_LEN), parameter :: SBK2D3D_Endtime_Name    = 'SBK2D3D_DB_Endtime'
    !
    ! identifiers for exchanged quantities and elementsets
    !
    character(Len=D3S_ID_LEN), parameter :: quantID_Volume                = 'Volume'
    character(Len=D3S_ID_LEN), parameter :: quantID_Waterlevel            = 'Waterlevel'
    character(Len=D3S_ID_LEN), parameter :: elmsetID_WaterlevelLocations  = 'WaterlevelLocs'
    character(Len=D3S_ID_LEN), parameter :: elmsetID_VolumeLocations      = 'VolumeLocs'
!
! Overloaded functions
!
    interface D3S_Init
        module procedure D3S_Init_Sobek
        module procedure D3S_Init_Delft3D
    end interface
!
! Module variables, Delft3D and Sobek side
!
    logical                   , private :: d3sDoD3dSobek              = .false.                          ! do 1D3D-coupling indeed?
    character(Len=D3S_ID_LEN) , private :: myComponentID              =  '<NN>'                          ! calling component identifier
    double precision          , private :: my_delta_t                 = 0.0D+00                          ! my delta-t (seconds)
    double precision          , private :: other_delta_t              = 0.0D+00                          ! other component's delta-t (seconds)
    integer                   , private :: my_num_time_steps          = 0                                ! my # time steps
    integer                   , private :: other_num_time_steps       = 0                                ! other component's # time steps
    double precision          , private :: my_starttimeAsJulianDay    = 2451544.5D+00                    ! my start time 
                                                                                                         ! Julian day representation (start at 1/1/2005, if start time not set)
    double precision          , private :: other_starttimeAsJulianDay = 2451544.5D+00                    ! other component's starttime 
                                                                                                         ! Julian day representation (start at 1/1/2005, if start time not set)
    double precision          , private :: my_endtimeAsJulianDay      = 2451544.5D+00                    ! my start time 
                                                                                                         ! Julian day representation (start at 1/1/2005, if start time not set)
    double precision          , private :: other_endtimeAsJulianDay   = 2451544.5D+00                    ! other component's starttime 
                                                                                                         ! Julian day representation (start at 1/1/2005, if start time not set)
    double precision          , private :: currentTimeAsJulianDay     = 2451544.5D+00                    ! current time 
                                                                                                         ! Julian day representation (start at 1/1/2005, if start time not set)
    double precision          , private :: deltaT_asJulianDay         = 1.0D+00 / 86400.0D+00 * 60.0D+00 ! delta-t 
                                                                                                         ! Julian day representation (default 60 seconds)
    character(Len=250)        , private :: d3sLastError               = ''                               ! Last error in D3S module
!
! Module variables, Sobek side
!
    double precision, dimension(:), pointer,    private :: outVolumes         ! outgoing volumes for next Put Call
    integer         , private   :: gsLastGetStep = -1 ! currTimeStep at last get-call
    double precision, private   :: d3d_half_delta_t   ! half of D3D time step interval
    integer         , private   :: d3dPutCounter = 0  ! # d3d put-wat.lev. calls
contains
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! PUBLIC functions, INITIALIZATION / GENERAL
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
! Initialization Delft3D side
!
!==============================================================================
function D3S_Init_Delft3D( componentID, runid  , &
                           confFile   ,          &
                           mlb        , mub    , &
                           nlb        , nub    , &
                           xcor       , ycor   , &
                           nto        , nambnd , mnbnd       , &
                           nostat     , namst  , mnstat      , mask, &
                           start_time , delta_T, numTimeSteps) result(success)
    use OPEN_MI_DIO
    use d3d_sobek_conf
!
! return value
!
    logical  :: success  ! .true. : OK
                         ! .false.: NOT OK, error stored in last-error
!
! arguments
!
    character(Len=*)                 , intent(in)  :: componentID    ! 'D3D'
    character(Len=*)                 , intent(in)  :: runid          ! schem.identifier (== runid)
    character(Len=*)                 , intent(in)  :: confFile       ! configuration file name
    integer                          , intent(in)  :: mlb            ! upper/lower boundaries
    integer                          , intent(in)  :: mub            !   for D3D-flow arrays
    integer                          , intent(in)  :: nlb
    integer                          , intent(in)  :: nub
    real, dimension(nlb:nub, mlb:mub), intent(in)  :: xcor           ! D3D-flow domain's X/Y coord.s
    real, dimension(nlb:nub, mlb:mub), intent(in)  :: ycor
    integer                          , intent(in)  :: nto            ! # D3D-flow boundaries
    character(Len=*), dimension(:)   , intent(in)  :: nambnd         ! boundary identifiers
    integer, dimension(:,:)          , intent(in)  :: mnbnd          ! boundary M/N indices
    integer                          , intent(in)  :: nostat         ! #monitoring stations
    character(Len=*), dimension(:)   , intent(in)  :: namst          ! mon.stat. identifiers
    integer, dimension(:,:)          , intent(in)  :: mnstat         ! mon.stat. M/N indices
    integer, dimension(nto)                        :: mask           ! to identify boundaries that are coupled to sobek: mask=1
    double precision                 , intent(in)  :: start_time     ! start time (julian)
    double precision                 , intent(in)  :: delta_T        ! time step interval (seconds)
    integer                          , intent(in)  :: numTimeSteps   ! total #time steps
!
! locals
!
    character(Len=D3S_ID_LEN), dimension(:), pointer :: WaterlevelLocIDs  ! id's of water level
    logical                                          :: fake_comm         ! fake communication ?
!
! body
!
    !
    ! store component / time info, initialize configuration / communication
    !
    myComponentID           = componentID       !  D3D identifier
    my_delta_t              = delta_T           !  D3D time step interval
    my_num_time_steps       = numTimeSteps      !  TODO: communicate #timesteps * deltaT
    my_starttimeAsJulianDay = start_time        !  D3D start time (Julian day representation)

    d3d_half_delta_t        = delta_T / 2.0D+00 !  half of D3D time step interval
    currentTimeAsJulianDay  = start_time
    deltaT_asJulianDay      = delta_T / 86400.0D+00

    my_endtimeAsJulianDay   = my_starttimeAsJulianDay + deltaT_asJulianDay*dble(my_num_time_steps)  !  D3D endtime (Julian day representation)

    success = DSC_Conf_Delft3D(confFile     , &
                               mlb          , mub         , &
                               nlb          , nub         , &
                               xcor         , ycor        , &
                               nto          , nambnd      , mnbnd , &
                               nostat       , namst       , mnstat, &
                               mask         ,d3sDoD3dSobek, fake_comm)

    if ( .not. success ) then
        call DSC_LastError(d3sLastError)
        d3sLastError = 'D3S: Init D3D for ' // trim(runid) // ': ' // trim(d3sLastError)
    else

        if ( d3sDoD3dSobek ) then
            if ( StringsEqual(CaseInsens, componentID ,'D3D-FLOW') ) then

                ! Initialize on line communication

                if ( .not. OD_Init('D3D', 'Sobek', fake_comm) ) then

                    call DO_LastError(d3sLastError)
                    d3sLastError = 'D3S: D3S_Init Delft3D for ' // trim(runid) // ': ' // trim(d3sLastError)

                else

                    ! Create exchanged data sets

                    call DioShmDataBlockPutDouble(D3D2SBK_DeltaT_Name, my_delta_t)
                    call DioShmDataBlockPutDouble(D3D2SBK_Starttime_Name, my_starttimeAsJulianDay)
                    call DioShmDataBlockPutDouble(D3D2SBK_Endtime_Name, my_endtimeAsJulianDay)

                    allocate(WaterlevelLocIDs(DSC_GetNumExchangedElements()))
                    success = DSC_MapD3dWaterlevelLocations(WaterlevelLocIDs)

                    if ( .not. success ) then
                        call DSC_LastError(d3sLastError)
                        d3sLastError = 'D3S: D3S_Init Delft3D for ' // trim(runid) // ': ' // trim(d3sLastError)
                    else

                        success = OD_ExchItemCreate( quantID_Waterlevel, &
                                         elmsetID_WaterlevelLocations, WaterlevelLocIDs, od_providing, &
                                         currentTimeAsJulianDay )

                        if ( success .and. (.not. DO_FakeComm()) ) then

                            success = success .and. &
                                        OD_ExchItemCreate( quantID_Volume, &
                                            elmsetID_VolumeLocations, od_accepting)

                            if (.not. d3sChecktimes(SBK2D3D_DeltaT_Name, 0.50D+0*my_delta_t)) then
                                write(d3sLastError,'(A)') 'D3S: 2*Sobek delta-T <> Delft3D delta-T'
                                success = .false.
                                return
                            endif
                            if (.not. d3sChecktimes(SBK2D3D_Starttime_Name, my_starttimeAsJulianDay)) then
                                write(d3sLastError,'(A)') 'D3S: Sobek starttime <> Delft3D starttime'
                                success = .false.
                                return
                            endif
                            if (.not. d3sChecktimes(SBK2D3D_Endtime_Name, my_endtimeAsJulianDay)) then
                                write(d3sLastError,'(A)') 'D3S: Sobek endtime <> Delft3D endtime'
                                success = .false.
                                return
                            endif
                        endif
                    endif
                    deallocate(WaterlevelLocIDs)
                endif
            endif
        endif
    endif
end function D3S_Init_Delft3D
!
!
!
! Initialization Sobek side
!
!==============================================================================
function D3S_Init_Sobek(  componentID   , schemID       , &
                          confFile      ,                 &
                          numSegments   , segmentIDs    , &
                          numBoundaries , boundaryIDs   , &
                          start_time    , delta_T       , numTimeSteps    ) result(success)
    use OPEN_MI_DIO
    use d3d_sobek_conf
!
! return value
!
    logical  :: success  ! .true. : OK
                         ! .false.: NOT OK, error stored in last-error
!
! arguments
!
    character(Len=*), intent(in) :: componentID   ! componentID identification
    character(Len=*), intent(in) :: schemID       ! schematisation identification
    character(Len=*), intent(in) :: confFile      ! configuration file name

    integer, intent(in)          :: numSegments   ! #segments
    character(Len=*), &
        dimension(:), intent(in) :: segmentIDs    ! segment identifiers

    integer, intent(in)          :: numBoundaries ! #boundaries
    character(Len=*), &
        dimension(:), intent(in) :: boundaryIDs   ! boundary identifiers

    double precision, intent(in) :: start_time    ! start time (julian)
    double precision, intent(in) :: delta_T       ! time step interval (seconds)
    integer         , intent(in) :: numTimeSteps  ! total #time steps
!
! locals
!
    character(Len=D3S_ID_LEN), &
        dimension(:), pointer :: volumeLocIDs     ! element ID's of volume locations
    logical                   :: fake_comm        ! fake communication
!
! body
!
    ! store component / time info, initialize configuration / communication
    !
    myComponentID           = componentID
    my_delta_t              = delta_T
    my_num_time_steps       = numTimeSteps    !  TODO: communicate #timesteps * deltaT
    my_starttimeAsJulianDay = start_time      !  SOBEK start time (Julian day representation)

    my_delta_t              = delta_T
    currentTimeAsJulianDay  = start_time
    deltaT_asJulianDay      = delta_T / 86400.0D+00

    my_endtimeAsJulianDay   = my_starttimeAsJulianDay + deltaT_asJulianDay*dble(my_num_time_steps) ! SOBEK endtime (Julian day representation)

    nullify(outVolumes)

    success = DSC_Conf_Sobek(  confFile      ,                 &
                               numSegments   , segmentIDs  ,   &
                               numBoundaries , boundaryIDs ,   &
                               d3sDoD3dSobek , fake_comm )

    if ( .not. success ) then
        call DSC_LastError(d3sLastError)
        d3sLastError = 'D3S: Init Sobek for ' // trim(schemID) // ': ' // trim(d3sLastError)
    else
        if ( d3sDoD3dSobek ) then
            if ( StringsEqual(CaseInsens, componentID ,'Sobek' ) ) then

                ! Initialize on line communication

                if ( .not. OD_Init('Sobek', 'D3D', fake_comm) ) then

                    call DO_LastError(d3sLastError)
                    d3sLastError = 'D3S: Init Sobek for ' // trim(schemID) // ': ' // trim(d3sLastError)

                else

                    ! Create exchanged data sets

                    call DioShmDataBlockPutDouble(SBK2D3D_DeltaT_Name, my_delta_t)
                    call DioShmDataBlockPutDouble(SBK2D3D_Starttime_Name, my_starttimeAsJulianDay)
                    call DioShmDataBlockPutDouble(SBK2D3D_Endtime_Name, my_endtimeAsJulianDay)

                    allocate(volumeLocIDs(DSC_GetNumExchangedElements()))
                    success = DSC_MapSobekVolumeLocations(segmentIDs, volumeLocIDs)

                    if ( .not. success ) then
                        call DSC_LastError(d3sLastError)
                        d3sLastError = 'D3S: D3S_Init Delft3D for ' // trim(schemID) // ': ' // trim(d3sLastError)
                    else

                        success = OD_ExchItemCreate( quantID_Volume, &
                                       elmsetID_VolumeLocations, volumeLocIDs, od_providing, &
                                       currentTimeAsJulianDay )

                        if ( success .and. (.not. DO_FakeComm()) ) then

                            success = success .and. OD_ExchItemCreate( quantID_Waterlevel, &
                                              elmsetID_WaterlevelLocations, od_accepting)

                            if (.not. d3sChecktimes(D3D2SBK_DeltaT_Name, 2.0D+0*my_delta_t)) then
                                write(d3sLastError,'(A)') 'D3S: 2*Sobek delta-T <> Delft3D delta-T'
                                success = .false.
                                return
                            endif
                            if (.not. d3sChecktimes(D3D2SBK_Starttime_Name, my_starttimeAsJulianDay)) then
                                write(d3sLastError,'(A)') 'D3S: Sobek starttime <> Delft3D starttime'
                                success = .false.
                                return
                            endif
                            if (.not. d3sChecktimes(D3D2SBK_Endtime_Name, my_endtimeAsJulianDay)) then
                                write(d3sLastError,'(A)') 'D3S: Sobek endtime <> Delft3D endtime'
                                success = .false.
                                return
                            endif
                        endif
                    endif
                    deallocate(volumeLocIDs)
                    allocate(outVolumes(numSegments))
                    outVolumes = 0.0D+00
                endif
            endif
        endif
    endif
end function D3S_Init_Sobek
!
!
!
! Get last error
!
!==============================================================================
subroutine D3S_LastError(errString)
!
! arguments
!
    character(Len=*), intent(out)  :: errString    ! error string
!
! body
!
    errString    = d3sLastError
    d3sLastError = ''
end subroutine D3S_LastError
!
!
!
! Do 1D3D-coupling indeed?
!
!==============================================================================
function D3S_DoD3dSobek() result(do1D3D)
!
! return value
!
    logical :: do1D3D
!
! body
!
    do1D3D = d3sDoD3dSobek
end function D3S_DoD3dSobek
!
!
!
! Specify missing value
!
!==============================================================================
subroutine D3S_SetMissingValueDefinition(value)
    use OPEN_MI_DIO
!
! arguments
!
    real :: value
!
! body
!
    odUndefinedValue  = value
end subroutine D3S_SetMissingValueDefinition
!
!
!
! Finalize
!
!==============================================================================
subroutine D3S_Finalize(myComponentID, otherComponentID)
    use OPEN_MI_DIO
    use d3d_sobek_conf
!
! arguments
!
    character(Len=*), intent(in) :: myComponentID
    character(Len=*), intent(in) :: otherComponentID
!
! body
!
    call OD_Finalize(myComponentID, otherComponentID)
    call DSC_Finalize()
    if ( myComponentID == 'Sobek' ) then
        if ( associated(outVolumes) ) deallocate(outVolumes)
    endif
end subroutine D3S_Finalize
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! PUBLIC functions, PUT/GET functions DELFT3D side
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
! Put water levels
!
!==============================================================================
function D3S_PutWaterlevels(currTimeStep, waterlevels, kfs, mlb, mub, nlb, nub ) result(success)
    use OPEN_MI_DIO
    use d3d_sobek_conf
!
! return value
!
    logical  :: success  ! .true. : OK
                         ! .false.: NOT OK, error stored in last-error
!
! arguments
!
    integer                            , intent(in) :: currTimeStep
    integer                            , intent(in) :: mlb
    integer                            , intent(in) :: mub
    integer                            , intent(in) :: nlb
    integer                            , intent(in) :: nub
    integer, dimension(nlb:nub,mlb:mub), intent(in) :: kfs
    real   , dimension(nlb:nub,mlb:mub), intent(in) :: waterlevels
!
! locals
!
    real, dimension(:), pointer :: values
!
! body
!
    success = .true.
    if ( d3sDoD3dSobek ) then
        success = .false.
        if ( d3sCheckTimeStep(currTimeStep) ) then
            allocate(values(DSC_GetNumExchangedElements()))
            values  = odUndefinedValue
            success = DSC_MapD3dWaterlevels(waterlevels, my_delta_t, values, kfs, mlb, mub, nlb, nub)
            if ( success ) then
                d3dPutCounter = d3dPutCounter + 1
                success =  OD_Put(currentTimeAsJulianDay + d3dPutCounter * deltaT_asJulianDay / 2.0, &
                                  quantID_Waterlevel, elmsetID_WaterlevelLocations, values)
            endif
            deallocate(values)
        endif
    endif
end function D3S_PutWaterlevels
!
!
!
! Get discharges on full elementset (set missing values where not available)
!     Use mask to indicate where they were received indeed.
!
!==============================================================================
function D3S_GetAllDischarges(currTimeStep, discharges, mask) result(success)
    use OPEN_MI_DIO
    use d3d_sobek_conf
!
! return value
!
    logical  :: success  ! .true. : OK
                         ! .false.: NOT OK, error stored in last-error
!
! arguments
!
    integer              , intent(in)  :: currTimeStep
    real, dimension(:)   , intent(out) :: discharges
    logical, dimension(:), intent(out) :: mask
!
! locals
!
    real, dimension(:)   , allocatable :: receivedVolumes
    integer                            :: newD3Scoupling    ! 0: SOBEK produces instantaneous discharges (already divided by d3d_half_delta_t)
                                                            ! 1: SOBEK produces accumulated volumes, to be divided by d3d_half_delta_t here by Delft3D
!
! body
!
    discharges = odUndefinedValue
    mask       = .false.
    success = .true.
    if ( d3sDoD3dSobek ) then
        success = .false.
        if ( d3sCheckTimeStep(currTimeStep) ) then
            allocate(receivedVolumes(DSC_GetNumExchangedElements()))
            success = OD_Get(quantID_Volume, elmsetID_VolumeLocations, receivedVolumes )
            if ( success ) then
                !
                ! Map exchanged locations to D3D-disch. boundaries,
                ! Transform volumes to discharges
                !
                success        = DSC_MapD3dVolumes(receivedVolumes, discharges, mask)
                newD3Scoupling = 1
                if (newD3Scoupling == 1) then
                   where ( discharges /= odUndefinedValue ) &
                         discharges = discharges / d3d_half_delta_t
                endif
            endif
            deallocate(receivedVolumes)
        endif
    endif
end function D3S_GetAllDischarges
!
!
!
! Get discharges on elementset,
!     insert received values on the related element(s)
!
!==============================================================================
function D3S_GetDischarges(currTimeStep, discharges) result(success)
!
! return value
!
    logical  :: success  ! .true. : OK
                         ! .false.: NOT OK, error stored in last-error
!
! arguments
!
    integer              , intent(in)  :: currTimeStep
    real, dimension(:)   , intent(out) :: discharges
!
! locals
!
    logical, dimension(:), allocatable :: mask
    real   , dimension(:), allocatable :: values
!
! body
!
    success = .true.
    if ( d3sDoD3dSobek ) then
        success = .false.
        if ( d3sCheckTimeStep(currTimeStep) ) then

            allocate( values(size(discharges)), mask  (size(discharges)) )
            if ( D3S_GetAllDischarges(currTimeStep, values, mask ) ) then
                success = .true.
                where (mask) discharges = values
            endif
            deallocate(values, mask)
        endif
    endif
end function D3S_GetDischarges
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! PUBLIC functions, PUT/GET functions SOBEK side
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
function D3S_PutDischarges(currTimeStep, discharges) result(success)

    use OPEN_MI_DIO
    use d3d_sobek_conf

    ! return value
    logical :: success

    ! arguments
    integer         , intent(in)               :: currTimeStep
    double precision, dimension(:), intent(in) :: discharges

    ! locals

    real, dimension(:), allocatable :: values


    success = .false.
    if ( D3S_CheckTimeStep(currTimeStep) ) then
        success = .true.
        if ( D3S_DoCommunicate(currTimeStep) ) then

            allocate(values(DSC_GetNumExchangedElements()))
            values  = odUndefinedValue
            success = DSC_MapSobekVolumes(discharges, values)

            if ( success ) then
               success =  OD_Put(currentTimeAsJulianDay + currTimeStep * deltaT_asJulianDay, quantID_Volume, elmsetID_VolumeLocations, values)
!               write (204,*) (values(i),i=1,DSC_GetNumDischargeLocs())
            endif
            deallocate(values)

        endif
    endif

end function D3S_PutDischarges


!
! Add ( discharge * delta-t ) to the volumes to be for the current time step
!
!==============================================================================
function D3S_SetDischarges(dt, discharges) result(success)
    use OPEN_MI_DIO
    use d3d_sobek_conf
!
! return value
!
    logical  :: success  ! .true. : OK
                         ! .false.: NOT OK, error stored in last-error
!
! arguments
!
    double precision, intent(in)               :: dt
    double precision, dimension(:), intent(in) :: discharges
!
! body
!
    success = .true.
    if ( d3sDoD3dSobek ) then
        success = .false.
        if ( size(discharges) == size(outVolumes) ) then
            outVolumes = outVolumes + discharges * dt
            success = .true.
        else
            write(d3sLastError,'(A)') 'D3S: SetVolumes, incompatible sizes'
        endif
    endif
end function D3S_SetDischarges
!
!

function D3S_CheckTimeStep(currTimeStep)  result(ok)

    ! return value
    logical :: ok

    ! arguments
    integer           , intent(in)  :: currTimeStep

    ! body

    ok = .false.
    if ( currTimeStep >= 0 .and. currTimeStep <= my_num_time_steps ) then
        ok = .true.
    else
        write(d3sLastError,'(A,I6)') 'D3S: CheckTimeStep, time step out of range ', currTimeStep
    endif

end function D3S_CheckTimeStep


function D3S_DoCommunicate(currTimeStep)  result(doComm)

    ! return value
    logical :: doComm

    ! arguments
    integer           , intent(in)  :: currTimeStep

    ! body

    doComm = .true.  ! NO intelligent 1:N time stepping for Delft3D - Sobek

    !! if ( mod ( currTimeStep - 1, d3s_comm_ratio ) == 0 ) then
    !!     doComm = .true.
    !! endif

end function D3S_DoCommunicate


!
! Put the volumes to be for the current time step
!
!==============================================================================
function D3S_PutVolumes(currTimeStep) result(success)
    use OPEN_MI_DIO
    use d3d_sobek_conf
!
! return value
!
    logical  :: success  ! .true. : OK
                         ! .false.: NOT OK, error stored in last-error
!
! arguments
!
    integer, intent(in) :: currTimeStep
!
! locals
!
    real, dimension(:), allocatable :: values
!
! body
!
    success = .true.
    if ( d3sDoD3dSobek ) then
        success = .false.
        if ( d3sCheckTimeStep(currTimeStep) ) then
            if ( .not. associated(outVolumes) ) then
                write(d3sLastError,'(A)') 'D3S: PutVolumes, no discharges provided'
            else
                allocate(values(DSC_GetNumExchangedElements()))
                values  = odUndefinedValue
                success = DSC_MapSobekVolumes(outVolumes, values)
                if ( .not. success ) then
                    call DSC_LastError(d3sLastError)
                    d3sLastError = 'D3S: PutVolumes: ' // trim(d3sLastError)
                else
                    success =  OD_Put(currentTimeAsJulianDay + currTimeStep * deltaT_asJulianDay, &
                                      quantID_Volume, elmsetID_VolumeLocations, values )
                    if ( .not. success ) then
                        call DO_LastError(d3sLastError)
                        d3sLastError = 'D3S: PutVolumes: ' // trim(d3sLastError)
                    endif
                endif
                deallocate(values)
                outVolumes = 0.0D+00
            endif
        endif
    endif
end function D3S_PutVolumes
!
!
!
! Get water levels on full elementset (set missing values where not available)
!     Use mask to indicate where they were received indeed.
!
!==============================================================================
function D3S_GetAllWaterlevels(currTimeStep, waterlevels, mask) result(success)
    use OPEN_MI_DIO
    use d3d_sobek_conf
!
! return value
!
    logical  :: success  ! .true. : OK
                         ! .false.: NOT OK, error stored in last-error
!
! arguments
!
    integer              , intent(in)  :: currTimeStep
    real   , dimension(:), intent(out) :: waterlevels
    logical, dimension(:), intent(out) :: mask
!
! locals
!
    real, dimension(:), allocatable    :: valuesFromDio
    integer                            :: NumWaterlevelLocs
!
! body
!
    waterlevels = odUndefinedValue
    mask        = .false.
    success     = .true.
    if ( d3sDoD3dSobek ) then
        success = .false.
        if ( d3sCheckTimeStep(currTimeStep) ) then
            if ( currTimeStep /= gsLastGetStep ) then
                NumWaterlevelLocs = DSC_GetNumExchangedElements()
                allocate(valuesFromDio(DSC_GetNumExchangedElements()))
                success = OD_Get(quantID_Waterlevel, elmsetID_WaterlevelLocations, valuesFromDio )
                if ( .not. success ) then
                    call DO_LastError(d3sLastError)
                    d3sLastError = 'D3S: D3S_GetAllWaterlevels: ' // trim(d3sLastError)
                else
                    success = DSC_MapSobekWaterlevels(valuesFromDio, waterlevels, mask)
                    if ( .not. success ) then
                        call DSC_LastError(d3sLastError)
                        d3sLastError = 'D3S: GetAllWaterlevels: ' // trim(d3sLastError)
                    endif
                endif
                    ! TODO received water levels
                deallocate(valuesFromDio)
            else
            endif
        endif
    endif
end function D3S_GetAllWaterlevels
!
!
!
! Get water level on elementset,
!     insert received values on the related element(s)
!
!==============================================================================
function D3S_GetWaterlevels(currTimeStep, waterlevels) result(success)
!
! return value
!
    logical :: success
!
! arguments
!
    integer              , intent(in)  :: currTimeStep
    real   , dimension(:), intent(out) :: waterlevels
!
! locals
!
    real   , dimension(:), allocatable :: values
    logical, dimension(:), allocatable :: mask
!
! body
!
    success = .true.
    if ( d3sDoD3dSobek ) then
        success = .false.
        if ( d3sCheckTimeStep(currTimeStep) ) then
            allocate( values(size(waterlevels)), mask  (size(waterlevels)) )
            if ( D3S_GetAllWaterlevels(currTimeStep, values, mask ) ) then
                success = .true.
                where (mask) waterlevels = values
            endif
            deallocate(values, mask)
        endif
    endif
end function D3S_GetWaterlevels
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! PRIVATE functions
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
! check range of current time step
!
!==============================================================================
function d3sCheckTimeStep(currTimeStep)  result(ok)
!
! return value
!
    logical  :: ok  ! .true. : currTimeStep OK
                    ! .false.: currTimeStep out of range (error stored in last-error)
!
! arguments
!
    integer           , intent(in)  :: currTimeStep
!
! body
!
    ok = .false.
    if ( currTimeStep >= 0 .and. currTimeStep <= my_num_time_steps ) then
        ok = .true.
    else
        write(d3sLastError,'(A,I6)') 'D3S: CheckTimeStep, time step out of range ', currTimeStep
    endif
end function d3sCheckTimeStep
!
!
! Compare time information of two processes
!
!==============================================================================
function d3sCheckTimes(valueShmName, my_value)  result(success)
!
    use dio_shm
!
! return value
!
    logical  :: success  ! .true. : time comparison between two processes OK
                         ! .false.: time comparison between two processes not OK
!
! arguments
!
    character(*)      , intent(in)  :: valueShmName ! Name of shared memory location for other process
    double precision  , intent(in)  :: my_value     ! Time value for active process
!
! locals
!
    double precision    :: diff            ! Difference between the two residuals (double)
    double precision    :: eps             ! Epsilon, allowed missfit between times
    double precision    :: other_value     ! Time value in shared memory for other process
!
! body
!
    success = .false.
    eps = 1.0D-6
    !
    ! For comparison between the start and end times (Julian day representation)
    ! an epsilon of 1e-6 is used. This means a difference in start and end times of 
    ! just less than 1/10 second is allowed (1e-6*86400).
    ! For the time step in seconds the criterium is set at 1e-3 s.
    !
    if (DioShmDataBlockGetDouble(valueShmName, other_value)) then
       if (my_value > 0.0D+0 .and. other_value > 0.0D+0 ) then
          diff = abs(other_value - my_value)
          if (index(valueShmName, 'Delta') > 0) then
             eps = 1.0D-3
          elseif (index(valueShmName, 'time') > 0) then
             eps = 1.0D-6
          endif
          if (diff < eps ) then
             !
             ! Difference between two values is less than eps
             !
             success = .true.
          endif
       endif
    endif
    !
end function d3sCheckTimes


end module D3D_Sobek


