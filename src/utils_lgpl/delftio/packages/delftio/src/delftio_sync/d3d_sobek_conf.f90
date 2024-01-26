module d3d_sobek_conf
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
!!! d3d_sobek_conf.F90: DSC - module (*D*elft-3D *S*obek *C*onfiguration)
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
!!!   - Data store with mapping info
!!!
!!!   IMPLEMENTATION
!!!
!!!   - Public Functions:
!!!
!!!     .  Initialization / general functions
!!!         - initialization Delft3D side
!!!         - initialization Sobek side
!!!         - info on number of exchange items
!!!     .  Mapping functions delft3d side
!!!         - map exchanged water level locations
!!!         - map water levels to exchanged outgoing water levels
!!!         - map exchanged incoming volumes to discharge boundary values
!!!     .  Mapping functions sobek side
!!!         - map exchanged volume locations
!!!         - map exchanged incoming water levels to water level boundary values
!!!         - map reach-segment discharges to exchanged outgoing volumes
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
! Public constants/enumerations
!
    integer, parameter :: DSC_ID_LEN = 80    ! identifier string length
!
! Private constants/enumerations
!
    integer         , parameter, private :: dscIndexInvalid   = -999       ! invalid mapping index
    double precision, parameter, private :: waterlevelInvalid = -999.0     ! invalid water level
!
! location indicator for water level from D3D towards sobek:
!
    integer, parameter, private :: DSC_use_boundary    = 1   ! - wat.lev. at related D3D FLOW boundary
    integer, parameter, private :: DSC_use_mon_station = 2   ! - wat.lev. at D3D monitoring station
    integer, parameter, private :: DSC_use_coordinates = 3   ! - wat.lev. at X/Y point (i.e. M/N point)
!
! Definition of data types
!
    !
    ! D3D-Sobek mapping info:
    !
    type t_dsc_mapping
        character(Len=DSC_ID_LEN)                         :: myComponentID             ! 'Sobek' or 'D3D'
        logical                                           :: dumpMapping               ! produce mapping log file?
        character(Len=256)                                :: dumpFileName              ! mapping log file name
        logical                                           :: dumpCoords                ! dump D3D x/y coordinates?
        logical                                           :: doD3dSobek                ! is D3S active?
        double precision                                  :: d3dToSobekWaterlevelShift ! h-Sobek - h-D3d
        logical                                           :: fakeCommunication         ! only produce his files?
        integer                                           :: numConnections            ! #conn.s in D3S coupling file
        type(t_dsc_ini_connection), dimension(:), pointer :: conn                      ! coupling info per connection
    end type t_dsc_mapping
    !
    ! D3D-Sobek mapping info per connection:
    !
    type t_dsc_ini_connection
        character(Len=DSC_ID_LEN) :: sobekSegmentID     ! reach-seg.ID in config file
        integer                   :: sbkConn2SegIndex   ! sobek's internal index for this reach-seg
        integer                   :: sbkSignMultiplier  ! (1|-1), to make incoming volume positive.
        character(Len=DSC_ID_LEN) :: sobekBoundaryID    ! h-bound. ID in config file
        integer                   :: sbkConn2BoundIndex ! sobek's internal index for this h-bound.
        character(Len=DSC_ID_LEN) :: d3dOpenBoundaryID  ! disch. bound. ID in config file
        integer                   :: d3dConn2BoundIndex ! D3D's internal index for this disch.-bound.
        integer                   :: d3dSignMultiplier  ! (1|-1), to make incoming volume positive.
        integer                   :: d3dtypeWSEN        ! west / south / north / east
        integer                   :: h_source           ! (DSC_use_boundary | DSC_use_mon_station
                                                        !                   | DSC_use_coordinates)
        logical                   :: d3dUseRelax        ! use relaxation time factor?
        real                      :: d3dRelaxationTime  ! relaxation time factor value
        real                      :: d3dprevWaterlevel  ! previous water level
        character(Len=DSC_ID_LEN) :: monStationID       ! mon.stat.ID in config file (optional)
        real                      :: xCoord             ! X/Y coords. in config file (optional)
        real                      :: yCoord
        integer                   :: d3d_M_Start        ! determined from:
        integer                   :: d3d_N_Start        !     boundary           (DSC_use_boundary)
        integer                   :: d3d_M_End          ! or: mon.stat. location (DSC_use_mon_station)
        integer                   :: d3d_N_End          ! or: point's coords     (DSC_use_coordinates)
    end type t_dsc_ini_connection
!
! Module variables
!
    type(t_dsc_mapping), private, target :: dsc_mapping_store      ! All mapping info
    character(Len=250) , private         :: dscLastError = ''      ! Last error in DSC module
    
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
function DSC_Conf_Delft3D( confFile    , &
                           mlb         , mub      , &
                           nlb         , nub      , &
                           xcor        , ycor     , &
                           nto         , nambnd   , mnbnd , &
                           nostat      , namst    , mnstat, mask, &
                           do_d3d_sobek, fake_comm) result(success)
    use Dio_Streams
!
! return value
!
    logical  :: success  ! .true. : OK
                         ! .false.: NOT OK, error stored in last-error
!
! arguments
!
    character(Len=*)                 , intent(in)  :: confFile     ! configuration file name
    integer                          , intent(in)  :: mlb          ! upper/lower boundaries
    integer                          , intent(in)  :: mub          !   for D3D-flow arrays
    integer                          , intent(in)  :: nlb   
    integer                          , intent(in)  :: nub
    real, dimension(nlb:nub, mlb:mub), intent(in)  :: xcor         ! D3D-flow domain's X/Y coord.s
    real, dimension(nlb:nub, mlb:mub), intent(in)  :: ycor
    integer                          , intent(in)  :: nto          ! # D3D-flow boundaries
    character(Len=*), dimension(:)   , intent(in)  :: nambnd       ! boundary identifiers
    integer, dimension(:,:)          , intent(in)  :: mnbnd        ! boundary M/N indices
    integer                          , intent(in)  :: nostat       ! #monitoring stations
    character(Len=*), dimension(:)   , intent(in)  :: namst        ! mon.stat. identifiers
    integer, dimension(:,:)          , intent(in)  :: mnstat       ! mon.stat. M/N indices
    integer, dimension(nto)                        :: mask         ! to identify boundaries that are coupled to sobek: mask=1
    logical                          , intent(out) :: do_d3d_sobek ! do d3d-sobek indeed?
    logical                          , intent(out) :: fake_comm    ! fake communication ?
!
! locals
!
    integer                             :: c,b,s,m,n  ! loop counters
    integer                             :: lun        ! dump file handle
    type(t_dsc_mapping)       , pointer :: mapping    ! pointer to mapping part
    type(t_dsc_ini_connection), pointer :: conn       ! pointer to connection in mapping store
    integer                             :: minc, ninc ! inside model instead of on boundaries
!
! body
!
    mapping => dsc_mapping_store

    mapping % myComponentID = 'D3D'

    ! Parse config file, and determine main flags

    success      = dscParseIniFile(confFile)
    do_d3d_sobek = mapping % doD3dSobek
    fake_comm    = mapping % fakeCommunication

    if ( do_d3d_sobek ) then

        if ( mapping % numConnections <= 0 ) then
            write(dscLastError, '(A)') 'DSC: Conf_Delft3D: No connections defined'
        else

            ! Determine indices for mapping internal values to
            ! the values in the exchanged valuesets.

            do c = 1, mapping % numConnections

                conn => mapping % conn(c)

                do b = 1, nto

                    ! Find D3D boundary for this connection
                    ! Store the internal boundary index.
                    ! Store the water level indices, based on
                    !     the source-location to be used for water level

                    if (StringsEqual(CaseInsens, nambnd(b),         &
                                     conn % d3dOpenBoundaryID) ) then

                        conn % d3dConn2BoundIndex = b
                        conn % d3dtypeWSEN        = mnbnd(7,b)
                        mask(b)                   = 1

                        if (mnbnd(7,b) == 3 .or. mnbnd(7,b) == 4) then
                           conn % d3dSignMultiplier = -1
                        endif

                        if ( conn % h_source == DSC_use_boundary ) then

                            minc = 0 ; ninc = 0
                            if (mnbnd(7,b) == 1) minc =  1
                            if (mnbnd(7,b) == 3) minc = -1
                            if (mnbnd(7,b) == 2) ninc =  1
                            if (mnbnd(7,b) == 4) ninc = -1
                            conn % d3d_M_Start = mnbnd(1,b) + minc
                            conn % d3d_N_Start = mnbnd(2,b) + ninc
                            conn % d3d_M_End   = mnbnd(3,b) + minc
                            conn % d3d_N_End   = mnbnd(4,b) + ninc

                        else if ( conn % h_source == DSC_use_mon_station ) then

                            do s = 1, nostat
                                if (StringsEqual(CaseInsens, namst(s),         &
                                                 conn % monStationID) ) then
                                    conn % d3d_M_Start = mnstat(1,s)
                                    conn % d3d_M_End   = conn % d3d_M_Start
                                    conn % d3d_N_Start = mnstat(2,s)
                                    conn % d3d_N_End   = conn % d3d_N_Start
                                endif
                            enddo

                        else if ( conn % h_source == DSC_use_coordinates ) then
                          ! TODO
                        endif

                    endif

                enddo

            enddo

        endif

    endif

    if ( success .and. mapping % dumpMapping ) then

        open(newunit=lun, file=mapping % dumpFileName)

        write(lun, '(2A)') 'DSC_Conf_Delft3D, conffile: ', trim(confFile)

        if ( mapping % dumpCoords ) then
            write(lun, '(A)') '  x/y'
            do m = mlb, mub
                do n = nlb, nub
                    write(lun, '(2I8,2F12.6)') m, n, xcor(n,m), ycor(n,m)
                enddo
            enddo
        endif

        write(lun, '(A)') '  boundaryIDs'
        do b = 1, nto
            write(lun, '(A,A20,7I8)') '    ', trim(nambnd(b))  , &
                                              mnbnd(1,b)  , &
                                              mnbnd(2,b)  , &
                                              mnbnd(3,b)  , &
                                              mnbnd(4,b)  , &
                                              mnbnd(5,b)  , &
                                              mnbnd(6,b)  , &
                                              mnbnd(7,b)  
        enddo

        write(lun, '(A)') '  namst'
        do s = 1, nostat
            write(lun, '(A,A20,2I8)') '    ', trim(namst(s))  , &
                                              mnstat(1,s)  , &
                                              mnstat(2,s)
        enddo

        call dscDumpConfig(lun)

        close (lun)

    endif

end function DSC_Conf_Delft3D
!
!
!
! Initialization Sobek side
!
!==============================================================================
function DSC_Conf_Sobek(  confFile      ,                      &
                          numSegments   , segmentIDs         , &
                          numBounds     , boundIDs           , &
                          do_d3d_sobek  , fake_comm            ) result(success)
    use Dio_Streams
!
! return value
!
    logical  :: success  ! .true. : OK
                         ! .false.: NOT OK, error stored in last-error
!
! arguments
!
    character(Len=*), intent(in)        :: confFile     ! configuration file name

    integer, intent(in)                 :: numSegments  ! #segments
    character(Len=*), &
        dimension(:), intent(in)        :: segmentIDs   ! segment identifiers

    integer, intent(in)                 :: numBounds    ! #boundaries
    character(Len=*), &
        dimension(:), intent(in)        :: boundIDs     ! boundary identifiers
    logical         , intent(out)       :: do_d3d_sobek ! do d3d-sobek indeed?
    logical         , intent(out)       :: fake_comm    ! fake communication ?
!
! locals
!
    integer                             :: c,b,s        ! loop counters
    integer                             :: lun          ! dump file handle
    type(t_dsc_mapping)       , pointer :: mapping      ! pointer to mapping part
    type(t_dsc_ini_connection), pointer :: conn         ! pointer to connection in mapping store
!
! body
!
    mapping => dsc_mapping_store
    mapping % myComponentID = 'Sobek'
    success      = dscParseIniFile(confFile)
    do_d3d_sobek = mapping % doD3dSobek
    fake_comm    = mapping % fakeCommunication
    if ( do_d3d_sobek ) then
        ! Build mapping
        if ( mapping % numConnections <= 0 ) then
            write(dscLastError, '(A)') 'DSC: Conf_Sobek: No connections defined'
        else

            do c = 1, mapping % numConnections

                conn => mapping % conn(c)

                do s = 1, numSegments
                    if (StringsEqual(CaseInsens, segmentIDs(s),         &
                                     conn % sobekSegmentID) ) then
                        conn % sbkConn2SegIndex   = s
                    endif
                enddo

                do b = 1, numBounds
                    if (StringsEqual(CaseInsens, boundIDs(b),         &
                                     conn % sobekBoundaryID ) ) then
                        conn % sbkConn2BoundIndex = b
                    endif
                enddo

            enddo

        endif
    endif

    if ( success .and. mapping % dumpMapping ) then

        open(newunit=lun, file=mapping % dumpFileName)

        write(lun, '(2A)') 'DSC_Conf_Sobek, conffile: ', trim(confFile)

        write(lun, '(A)') '  segmentIDs'
        do s = 1, numSegments
           write(lun, '(2A)') '    ', trim(segmentIDs(s))
        enddo

        write(lun, '(A)') '  boundaryIDs'
        do b = 1, numBounds
           write(lun, '(2A)') '    ', trim(boundIDs(b))
        enddo
        call dscDumpConfig(lun)
        close (lun)
    endif
end function DSC_Conf_Sobek
!
!
!
! General: Get last error
!
!==============================================================================
subroutine DSC_LastError(errString)
!
! arguments
!
    character(Len=*), intent(out)  :: errString    ! error string
!
! body
!
    errString    = dscLastError
    dscLastError = ''
end subroutine DSC_LastError
!
!
!
! General: Get size of exchanged elementset
!
!==============================================================================
function DSC_GetNumExchangedElements() result(numExchangedElements)
!
! arguments
!
    integer :: numExchangedElements   ! # communicated locations
!
! body
!
    numExchangedElements = dsc_mapping_store % numConnections
end function DSC_GetNumExchangedElements
!
!
!
! General: Finalize
!
!==============================================================================
subroutine DSC_Finalize()
!
! body
!
    if ( dsc_mapping_store % numConnections > 0 ) then
        if ( associated(dsc_mapping_store % conn) ) deallocate(dsc_mapping_store % conn)
    endif
end subroutine DSC_Finalize
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! PUBLIC functions, MAPPING functions DELFT3D side
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
! Map exchanged water level locations
!
!==============================================================================
function DSC_MapD3dWaterlevelLocations(mappedWatLevLocations) result(success)
    use Dio_Ini
!
! return value
!
    logical  :: success  ! .true. : OK
                         ! .false.: NOT OK, error stored in last-error
!
! arguments
!
    character(Len=*), dimension(:) :: mappedWatLevLocations ! mapped values
!
! locals
!
    integer                        :: c                     ! loop counter
    type(t_dsc_mapping), pointer   :: mapping               ! pointer to mapping store
    type(t_dsc_ini_connection), &
                           pointer :: conn                  ! pointer to connection in mapping store
!
! body
!
    success = .false.
    mapping => dsc_mapping_store
    if ( mapping % numConnections <= 0 ) then
        write(dscLastError, '(A)') 'DSC: MapD3dWaterlevelLocations: No connections defined'
    else
        success = .true.
        do c = 1, mapping % numConnections
            conn => mapping % conn(c)
            if ( conn % d3d_M_Start /= dscIndexInvalid .and. &
                 conn % d3d_N_Start /= dscIndexInvalid .and. &
                 conn % d3d_M_End   /= dscIndexInvalid .and. &
                 conn % d3d_N_End   /= dscIndexInvalid       ) then
                    write(mappedWatLevLocations(c), '(a,i0,a,i0)') &
                        'Point ', ( conn % d3d_M_Start + conn % d3d_M_End ) / 2, ',', &
                                  ( conn % d3d_N_Start + conn % d3d_N_End ) / 2
            else
                write(dscLastError, '(A,I8,A)') 'DSC: Connection ', c, ' not mapped'
                success = .false.
            endif
        enddo
    endif
end function DSC_MapD3dWaterlevelLocations
!
!
!
! Map water levels to exchanged outgoing water levels
!
!==============================================================================
function DSC_MapD3dWaterlevels(waterlevels, delta_t, mappedWatLevs, kfs, mlb, mub, nlb, nub ) result(success)
!
! return value
!
    logical :: success
!
! arguments
!
    integer                            , intent(in)  :: mlb
    integer                            , intent(in)  :: mub
    integer                            , intent(in)  :: nlb
    integer                            , intent(in)  :: nub
    integer, dimension(nlb:nub,mlb:mub), intent(in)  :: kfs
    real   , dimension(nlb:nub,mlb:mub), intent(in)  :: waterlevels   ! d3d water levels
    double precision                   , intent(in)  :: delta_t       ! d3d time step interval
    real   , dimension(:)              , intent(out) :: mappedWatLevs ! mapped values
!
! locals
!
    integer                       :: c          ! loop counter
    type(t_dsc_mapping), pointer  :: mapping    ! pointer to mapping store
    type(t_dsc_ini_connection), &
                         pointer  :: conn       ! pointer to connection in mapping store
    real                          :: relaxation ! relaxation factor
    integer                       :: m,n,num    ! indices, #involved points
    integer                       :: mstep      ! interval step for m-loop across boundary
    integer                       :: nstep      ! interval step for n-loop across boundary
!
! body
!
    success = .false.
    mapping => dsc_mapping_store
    if ( mapping % numConnections <= 0 ) then
        write(dscLastError, '(A)') 'DSC: MapD3dWaterlevels: No connections defined'
    else
        success = .true.
        do c = 1, mapping % numConnections

            conn => mapping % conn(c)

            if ( mapping % conn(c) % d3d_M_Start /= dscIndexInvalid .and. &
                 mapping % conn(c) % d3d_N_Start /= dscIndexInvalid       ) then
                mappedWatLevs(c) = 0
                num              = 0
                mstep = 1
                nstep = 1
                if ( iabs(mapping % conn(c) % d3d_M_End) < iabs(mapping % conn(c) % d3d_M_Start) ) then
                   mstep = -1
                endif
                if ( iabs(mapping % conn(c) % d3d_N_End) < iabs(mapping % conn(c) % d3d_N_Start) ) then
                   nstep = -1
                endif
                do m = iabs(mapping % conn(c) % d3d_M_Start),iabs(mapping % conn(c) % d3d_M_End), mstep
                    do n = iabs(mapping % conn(c) % d3d_N_Start),iabs(mapping % conn(c) % d3d_N_End), nstep
                       if (kfs(n, m) /= 0) then 
                          mappedWatLevs(c) = mappedWatLevs(c) + waterlevels(n,m)
                          num = num + 1
                       endif
                    enddo
                enddo
                mappedWatLevs(c) = mappedWatLevs(c) / num

                if ( conn % d3dUseRelax ) then

                    if ( conn % d3dprevWaterlevel == real(waterlevelInvalid) ) &
                        conn % d3dprevWaterlevel = mappedWatLevs(c)

                    relaxation = exp(- sngl(delta_t) / conn % d3dRelaxationTime)
                    mappedWatLevs(c) =                                        &
                          dble(mappedWatLevs(c))   *             relaxation   &
                        + conn % d3dprevWaterlevel * ( 1.0D+00 - relaxation )
                    conn % d3dprevWaterlevel = mappedWatLevs(c)

                endif

                mappedWatLevs(c) = mappedWatLevs(c) + mapping % D3dToSobekWaterlevelShift

            else
                success = .false.
                write(dscLastError, '(A,I8)') 'DSC: MapD3dWaterlevels: Invalid connection index ', c
            endif
        enddo
    endif
end function DSC_MapD3dWaterlevels
!
!
!
! Map exchanged incoming volumes to discharge boundary values
!
!==============================================================================
function DSC_MapD3dVolumes(volumes, mappedVolumes, mask) result(success)
!
! return value
!
    logical  :: success  ! .true. : OK
                         ! .false.: NOT OK, error stored in last-error
!
! arguments
!
    real   , dimension(:) :: volumes        ! received volumes
    real   , dimension(:) :: mappedVolumes  ! mapped values
    logical, dimension(:) :: mask           ! where mapped?
!
! locals
!
    integer                      :: c       ! loop counter
    type(t_dsc_mapping), pointer :: mapping ! pointer to mapping store
!
! body
!
    success   = .false.
    mask      = .false.
    mapping => dsc_mapping_store
    if ( mapping % numConnections <= 0 ) then
        write(dscLastError, '(A)') 'DSC: MapD3dVolumes: No connections defined'
    else
        success = .true.
        do c = 1, mapping % numConnections
            if (mapping % conn(c) % d3dConn2BoundIndex /= dscIndexInvalid ) then

                mask            (mapping % conn(c) % d3dConn2BoundIndex) = .true.
                mappedVolumes(mapping % conn(c) % d3dConn2BoundIndex) = &
                        -1 * & ! Sobek-Out: negative, is positive inflow for D3D
                             mapping % conn(c) % d3dSignMultiplier         &
                           * volumes(c)
            else
                success = .false.
                write(dscLastError, '(A,I8)') 'DSC: MapD3dVolumes: Invalid connection index ', c
            endif
        enddo
    endif
end function DSC_MapD3dVolumes
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! PUBLIC functions, MAPPING functions SOBEK side
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
! Map exchanged volume locations
!
!==============================================================================
function DSC_MapSobekVolumeLocations(segmentIDs, volumeLocIDs) result(success)
!
! return value
!
    logical  :: success  ! .true. : OK
                         ! .false.: NOT OK, error stored in last-error
!
! arguments
!
    character(Len=*), dimension(:) :: segmentIDs   ! sobek locations
    character(Len=*), dimension(:) :: volumeLocIDs ! mapped locations for volumes
!
! locals
!
    integer                        :: c            ! loop counter
    type(t_dsc_mapping), pointer   :: mapping      ! pointer to mapping store
!
! body
!
    success = .false.
    mapping => dsc_mapping_store
    if ( mapping % numConnections <= 0 ) then
        write(dscLastError, '(A)') 'DSC: MapSobekVolumeLocations: No connections defined'
    else
        success = .true.
        do c = 1, mapping % numConnections
            if (mapping % conn(c) % sbkConn2SegIndex /= dscIndexInvalid ) then
                volumeLocIDs(c) = segmentIDs(mapping % conn(c) % sbkConn2SegIndex)
            else
                success = .false.
                write(dscLastError, '(A,I8)') 'DSC: MapSobekVolumeLocations: Invalid connection index ', c
            endif
        enddo
    endif
end function DSC_MapSobekVolumeLocations
!
!
!
! Map reach-segment volumes to exchanged outgoing volumes
!
!==============================================================================
function DSC_MapSobekVolumes(volumes, mappedValues) result(success)
!
! return value
!
    logical  :: success  ! .true. : OK
                         ! .false.: NOT OK, error stored in last-error
!
! arguments
!
    double precision, dimension(:)  :: volumes   ! sobek volumes
    real            , dimension(:)  :: mappedValues ! mapped values
!
! locals
!
    integer                         :: c            ! loop counter
    type(t_dsc_mapping), pointer    :: mapping      ! pointer to mapping store
!
! body
!
    success = .false.
    mapping => dsc_mapping_store
    if ( mapping % numConnections <= 0 ) then
        write(dscLastError, '(A)') 'DSC: MapSobekVolumes: No connections defined'
    else
        success = .true.
        do c = 1, mapping % numConnections
            if (mapping % conn(c) % sbkConn2SegIndex /= dscIndexInvalid ) then
                mappedValues(c) =  volumes(mapping % conn(c) % sbkConn2SegIndex) &
                                 * mapping % conn(c) % sbkSignMultiplier
            else
                success = .false.
                write(dscLastError, '(A,I8)') 'DSC: MapSobekVolumes: Invalid connection index ', c
            endif
        enddo
    endif
end function DSC_MapSobekVolumes
!
!
!
! Map exchanged incoming water levels to water level boundary values
!
!==============================================================================
function DSC_MapSobekWaterlevels(waterlevels, mappedWatLevs, mask) result(success)
!
! return value
!
    logical  :: success  ! .true. : OK
                         ! .false.: NOT OK, error stored in last-error
!
! arguments
!
    real   , dimension(:) :: waterlevels    ! received waterlevels
    real   , dimension(:) :: mappedWatLevs  ! mapped values
    logical, dimension(:) :: mask           ! where mapped?
!
! locals
!
    integer                      :: c       ! loop counter
    type(t_dsc_mapping), pointer :: mapping ! pointer to mapping store
!
! body
!
    success   = .false.
    mask      = .false.
    mapping => dsc_mapping_store
    if ( mapping % numConnections <= 0 ) then
        write(dscLastError, '(A)') 'DSC: MapSobekWaterlevels: No connections defined'
    else
        success = .true.
        do c = 1, mapping % numConnections
            if (mapping % conn(c) % sbkConn2BoundIndex /= dscIndexInvalid ) then
                mappedWatLevs(mapping % conn(c) % sbkConn2BoundIndex) = waterlevels(c)
                mask         (mapping % conn(c) % sbkConn2BoundIndex) = .true.
            else
                success = .false.
                write(dscLastError, '(A,I8)') 'DSC: MapSobekWaterlevels: Invalid connection index ',c
            endif
        enddo
    endif
end function DSC_MapSobekWaterlevels
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! PRIVATE functions
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
! Parse configuration file
!
!==============================================================================
function dscParseIniFile(confFileName) result(success)
    use Dio_Ini
    use open_mi_dio
!
! return value
!
    logical :: success ! .true.: no config file present, or config file parsed successfully.
!
! arguments
!
    character(Len=*), intent(in) :: confFileName       ! configuration file name
!
! locals
!
    type(TDioIniFile)                 :: iniFileHandle ! Handle to config file
    character(Len=DioMaxIniLineLen)   :: connectString ! Connection<n> string
    character(Len=DioMaxIniLineLen)   :: ValString     ! Value string in ini
    type(t_dsc_mapping), pointer      :: mapping       ! pointer to mapping store
    integer                           :: c             ! Connection loop counter
    type(t_dsc_ini_connection),pointer:: conn          ! pointer to connection
    character(Len=10)                 :: intFormat     ! format to write int
    integer                           :: readRes       ! result of read-call
!
! body
!
    success = .false.
    mapping => dsc_mapping_store
    mapping % doD3dSobek                = .false.
    mapping % fakeCommunication         = .false.

    mapping % dumpMapping               = .false.
    mapping % dumpCoords                = .false.
    mapping % dumpFileName              = trim(mapping % myComponentID) // '-d3s-mapping.txt'

    mapping % numConnections            = 0
    mapping % d3dToSobekWaterlevelShift = 0.0D+00
    if ( .not. DioIniFileOpen(iniFileHandle, confFileName, 'r') ) then
        ! Config file not present. Successfull ending, leading to no 1D3D-coupling.
        write(dscLastError, '(3A)') 'DSC: ParseIniFile: Config file "', trim(confFileName), &
                                  & '" not present'
        success = .false.
    else
        ! find main items (check file type, do 1D3D-coupling)
        if ( DioIniFindItem(iniFileHandle, 'General', 'FileType', valString) ) then
            if ( StringsEqual(CaseInsens, valString, 'D3DSobekMapping' ) ) then
                success          = .true.
                mapping % doD3dSobek = .true.
                if ( DioIniFindItem(iniFileHandle, 'D3dSobek coupling', 'DoD3dSobek', valString) ) then
                    mapping % doD3dSobek = dscStringToBool(valString)
                endif
            endif
        endif
        if ( success .and. mapping % doD3dSobek ) then

            ! find / set mapping items

            if ( DioIniFindItem(iniFileHandle, &
                   'D3dSobek coupling', 'DumpMapping', valString) ) then
                   mapping % dumpMapping = dscStringToBool(valString)
            endif

            if ( DioIniFindItem(iniFileHandle, &
                   'D3dSobek coupling', 'DumpFileName', valString) ) then
                mapping % dumpFileName  = trim(mapping % myComponentID) // trim(valString)
            endif

            if ( DioIniFindItem(iniFileHandle, &
                   'D3dSobek coupling', 'DumpCoordinates', valString) ) then
                   mapping % dumpCoords = dscStringToBool(valString)
            endif

            if ( DioIniFindItem(iniFileHandle, &
                   'D3dSobek coupling', 'DumpExchangedValues', valString) ) then
                   dumpValues = dscStringToBool(valString)
            endif

            if ( DioIniFindItem(iniFileHandle, &
                   'D3dSobek coupling', 'D3dToSobekWaterlevelShift', valString) ) then
                   read(valString,*) mapping %  d3dToSobekWaterlevelShift
            endif

            if ( DioIniFindItem(iniFileHandle, &
                   'D3dSobek coupling', 'FakeCommunication', valString) ) then
                   mapping % fakeCommunication = dscStringToBool(valString)
            endif

            ! Find # connections

            if ( .not. DioIniFindItem(iniFileHandle, &
                   'D3dSobek coupling', 'NumberOfConnections', valString) ) then
                success = .false.
            else

                read(valString,* , iostat=readRes) mapping % numConnections

                if ( mapping % numConnections < 1 .or. readRes /= 0 ) then
                    write(dscLastError, '(3A)')                          &
                        'DSC: ParseIniFile: NumberOfConnections=', trim(valString), &
                        ': Invalid value'
                    success = .false.
                else

                    ! Allocate connection info for #connections.
                    ! Initialize to default values.

                    allocate(mapping % conn(mapping % numConnections))

                    ! Sobek-side id's and settings

                    mapping % conn % sobekSegmentID     = ''
                    mapping % conn % sbkConn2SegIndex   = dscIndexInvalid
                    mapping % conn % sbkSignMultiplier  = 1

                    mapping % conn % sobekBoundaryID    = ''
                    mapping % conn % sbkConn2BoundIndex = dscIndexInvalid

                    ! D3D-side id's and settings

                    mapping % conn % d3dOpenBoundaryID  = ''
                    mapping % conn % d3dConn2BoundIndex = dscIndexInvalid
                    mapping % conn % d3dSignMultiplier  = 1
                    mapping % conn % d3dtypeWSEN        = 0

                    mapping % conn % h_source           = DSC_use_boundary

                    mapping % conn % d3dUseRelax        = .false.
                    mapping % conn % d3dRelaxationTime  = -1.0
                    mapping % conn % d3dprevWaterlevel  = waterlevelInvalid

                    mapping % conn % monStationID       = ''
                    mapping % conn % xCoord             = 0.0D+00
                    mapping % conn % yCoord             = 0.0D+00

                    mapping % conn % d3d_M_Start        = dscIndexInvalid
                    mapping % conn % d3d_M_End          = dscIndexInvalid
                    mapping % conn % d3d_N_Start        = dscIndexInvalid
                    mapping % conn % d3d_N_End          = dscIndexInvalid

                endif
            endif
        endif

        if ( success ) then

            do c = 1, mapping % numConnections

                conn => mapping % conn(c)

                ! Set integer format for [Connection<integer-nr>] group

                if ( .not. DioIniMakPosIntFormat(c, intFormat) ) then
                    write(dscLastError, '(A)') 'DSC: INTERNAL ERROR 1'
                    success = .false.
                else

                    ! Process in [Connection<integer-nr>] group

                    intFormat = '(A,' // trim(intFormat) // ')'
                    write(connectString, intFormat) 'Connection', c

                    ! Sobek-side id's and settings

                    if ( .not. DioIniFindItem(iniFileHandle, &
                                                connectString, 'SobekBoundaryID', &
                                                conn % sobekBoundaryID ) ) then
                        write(dscLastError, '(A,I8,A)')          &
                            'DSC: ParseIniFile: connection ', c, &
                            ': SobekBoundaryID not defined'
                        success = .false.
                    endif

                    if ( .not. DioIniFindItem(iniFileHandle, &
                                                connectString, 'SobekSegmentID', &
                                                conn % sobekSegmentID ) ) then
                        write(dscLastError, '(A,I8,A)')          &
                            'DSC: ParseIniFile: connection ', c, &
                            ': sobekSegmentID not defined'
                        success = .false.
                    endif

                    if ( DioIniFindItem(iniFileHandle, connectString, &
                                        'SobekSegmentDirectionOutgoing', valString) ) then
                        if ( dscStringToBool(valString) ) conn % sbkSignMultiplier = -1
                    endif

                    ! D3D-side id's and settings

                    if ( .not. DioIniFindItem(iniFileHandle, &
                                                connectString, 'D3dOpenBoundaryID', &
                                                conn % d3dOpenBoundaryID ) ) then
                        write(dscLastError, '(2A)') 'DSC: ParseIniFile: No D3D OpenBoundary defined for ', &
                                                    trim(connectString)
                        success = .false.
                    endif

                    if ( DioIniFindItem(iniFileHandle, connectString, &
                                        'D3dWaterLevelRelaxationTime', valString) ) then
                        read(valString,*, iostat=readRes) conn % d3dRelaxationTime
                        if ( readRes /= 0 ) then
                            write(dscLastError, '(3A)')                          &
                                'DSC: ParseIniFile: D3dWaterLevelRelaxationTime=', &
                                    trim(valString), ': Invalid value'
                            success = .false.
                        else
                            if ( conn % d3dRelaxationTime > 0.0) then
                                conn % d3dUseRelax = .true.
                            endif
                        endif
                    endif

                    if ( DioIniFindItem(iniFileHandle, connectString,   &
                                        'D3dMonitorStationID',          &
                                        conn % monStationID) ) then
                        conn % h_source = DSC_use_mon_station
                    endif

                    if ( DioIniFindItem(iniFileHandle, connectString,  &
                                        'D3dXCoordinate', valString )  ) then
                        read(valString,*) conn % xCoord
                        if ( DioIniFindItem(iniFileHandle, connectString, &
                                        'D3dYCoordinate', valString     ) ) then
                            read(valString,*) conn % yCoord
                            conn % h_source = DSC_use_coordinates
                        endif
                    endif
                endif
            enddo
        endif
        Call DioIniFileClose(iniFileHandle)
    endif
end function dscParseIniFile
!
!
!
! Determine boolean value from value string in ini-file
!
!==============================================================================
function dscStringToBool(valString) result(bool)
    use Dio_Ini
!
! return value
!
    logical                      :: bool      ! boolean represent. of valString
!
! arguments
!
    character(Len=*), intent(in) :: ValString ! Value string in config file
!
! body
!
    bool = .true.
    if ( valString == '0'                             .or. &
         StringsEqual(CaseInsens, valString, 'F'    ) .or. &
         StringsEqual(CaseInsens, valString, 'False') .or. &
         StringsEqual(CaseInsens, valString, 'N'    ) .or. &
         StringsEqual(CaseInsens, valString, 'No'   )      ) then
        bool = .false.
    endif
end function dscStringToBool
!
!
!
! Dump configuration
!
!==============================================================================
subroutine dscDumpConfig(lun)
    use dio_ini
!
! arguments
!
    integer                             :: lun           ! file handle
!
! locals
!
    type(t_dsc_mapping), pointer        :: mapping       ! pointer to mapping store
    integer                             :: c             ! Connection loop counter
    character(Len=DSC_ID_LEN)           :: hSourceString ! D3D-source for waterlevel
    type(t_dsc_ini_connection), pointer :: conn          ! pointer to connection
!
! body
!
    mapping => dsc_mapping_store
    write(lun, '(A)')       '==='
    write(lun, '(A,L3)')    'DoD3dSobek               : ', mapping % doD3dSobek
    write(lun, '(A,L3)')    'FakeCommunication        : ', mapping % fakeCommunication
    write(lun, '(A,F12.4)') 'D3dToSobekWaterlevelShift: ', mapping % d3dToSobekWaterlevelShift
    write(lun, '(A)')       '==='
    write(lun, '(A,I0)')    'NumberOfConnections        ', mapping % numConnections
    do c = 1, mapping % numConnections
        conn => mapping % conn(c)
        hSourceString = 'BND ' // trim(conn % d3dOpenBoundaryID)
        if ( conn % h_source == DSC_use_mon_station ) then
            hSourceString = 'BND ' // trim(conn % monStationID)
        else if ( conn % h_source == DSC_use_coordinates ) then
            write(hSourceString, '(A,F20.4,A,F20.4)')  'X=', conn % xCoord, &
                                                       ', Y=', conn % yCoord
        endif

        write(lun, '(A)') '   ---'

        write(lun,'(A,I8,A,I8,4A)') &
                   '  Connection', c, ': SDSM =', conn % SbkSignMultiplier,  ', ',&
                   trim(conn % sobekSegmentID), ' => ', trim(conn % d3dOpenBoundaryID)

        write(lun,'(4A)') '                                '                      , &
                   trim(conn % sobekBoundaryID), &
                   ' <= ', trim(hSourceString)

        if ( StringsEqual(CaseInsens, mapping % myComponentID, 'Sobek') ) then
            write(lun,'(A,I8)') '          Sobek Segm.Index'    , conn % sbkConn2SegIndex
            write(lun,'(A,I8)') '          Sobek SignMultiplier', conn % sbkSignMultiplier
            write(lun,'(A,I8)') '          Sobek Bound.Index'   , conn % sbkConn2BoundIndex
        endif

        if ( StringsEqual(CaseInsens, mapping % myComponentID, 'D3D') ) then
            write(lun,'(A,I8)')      '          D3D Bound.Index'    , conn % d3dConn2BoundIndex
            write(lun,'(A,I8)')      '          D3D SignMultiplier' , conn % d3dSignMultiplier
            write(lun,'(A,I8)')      '          D3D Type WSEN'      , conn % d3dtypeWSEN
            write(lun,'(A,I8,A,I8,A,I8,A,I8)') &
                                     '          D3D M/N. Indices'   , conn % d3d_M_Start, &
                                                              ','   , conn % d3d_N_Start, &
                                                              '-'   , conn % d3d_M_End  , &
                                                              ','   , conn % d3d_N_End
            if ( conn % d3dUseRelax ) &
               write(lun,'(A,F10.4)')'          D3D RelaxationTime' , conn % d3dRelaxationTime
        endif
    enddo
    write(lun, '(A)') '==='
end subroutine dscDumpConfig




end module d3d_sobek_conf

