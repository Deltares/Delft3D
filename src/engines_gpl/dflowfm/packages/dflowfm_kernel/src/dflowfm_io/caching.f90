!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

! 
! 

! NOTES:
! - Observation points can be "moving" - these are excluded.
! - How about the global index to these items?
!


!> Manages the caching file - store and retrieve the grid-based information.
module unstruc_caching
    use precision
    use m_observations, only: numobs, xobs, yobs, locTpObs, kobs, lobs
    use m_monitoring_crosssections, only: crs, tcrs, deallocCrossSections
    !use m_crspath, only: tcrspath
    use md5_checksum
    use m_alloc
    use network_data

    implicit none

    logical, private :: cache_success

    character(len=20), dimension(4), private :: section = ['OBSERVATIONS        ', &
                                                           'FIXED WEIRS         ', &
                                                           'CROSS_SECTIONS      ', &
                                                           'DRY_POINTS_AND_AREAS']
    integer, parameter, private :: key_obs = 1
    integer, parameter, private :: key_fixed_weirs = 2
    integer, parameter, private :: key_cross_sections = 3
    integer, parameter, private :: key_dry_points_and_areas = 4

    double precision, dimension(:), allocatable, private :: cache_xobs
    double precision, dimension(:), allocatable, private :: cache_yobs
    double precision, dimension(:), allocatable, private :: cache_xpl_fixed
    double precision, dimension(:), allocatable, private :: cache_ypl_fixed
    double precision, dimension(:), allocatable, private :: cache_dsl_fixed
    integer, dimension(:), allocatable, private          :: cache_locTpObs
    integer, dimension(:), allocatable, private          :: cache_kobs
    integer, dimension(:), allocatable, private          :: cache_lobs
    integer, dimension(:), allocatable, private          :: cache_ilink_fixed
    integer, dimension(:), allocatable, private          :: cache_ipol_fixed
    integer, dimension(:), allocatable, private          :: cache_linklist
    integer, dimension(:), allocatable, private          :: cache_ipol

    type(tcrs), dimension(:), allocatable                :: cache_cross_sections
    
    integer, private                                     :: cached_nump_dry
    integer, private                                     :: cached_nump1d2d_dry
    integer, dimension(:,:), allocatable, private        :: cached_lne_dry
    integer, dimension(:), allocatable, private          :: cached_lnn_dry
    double precision, dimension(:), allocatable, private :: cached_xzw_dry
    double precision, dimension(:), allocatable, private :: cached_yzw_dry
    double precision, dimension(:), allocatable, private :: cached_bottom_area_dry
    double precision, dimension(:), allocatable, private :: cached_xz_dry
    double precision, dimension(:), allocatable, private :: cached_yz_dry
    type (tface), dimension(:), allocatable, private     :: cached_netcell_dry


    character(len=30), parameter, private :: version_string = "D-Flow FM, cache file, 1.0"
    character(len=md5length), private :: md5current

contains
!> Sets ALL (scalar) variables in this module to their default values.
subroutine default_caching()

   cache_success = .false.

   if (allocated(cache_xobs))           deallocate(cache_xobs)
   if (allocated(cache_yobs))           deallocate(cache_yobs)
   if (allocated(cache_xpl_fixed))      deallocate(cache_xpl_fixed)
   if (allocated(cache_ypl_fixed))      deallocate(cache_ypl_fixed)
   if (allocated(cache_dsl_fixed))      deallocate(cache_dsl_fixed)
   if (allocated(cache_locTpObs))       deallocate(cache_locTpObs)
   if (allocated(cache_kobs))           deallocate(cache_kobs)
   if (allocated(cache_lobs))           deallocate(cache_lobs)
   if (allocated(cache_ilink_fixed))    deallocate(cache_ilink_fixed)
   if (allocated(cache_ipol_fixed))     deallocate(cache_ipol_fixed)
   if (allocated(cache_linklist))       deallocate(cache_linklist)
   if (allocated(cache_ipol))           deallocate(cache_ipol)
   if (allocated(cached_lne_dry))       deallocate(cached_lne_dry)
   if (allocated(cached_lnn_dry))       deallocate(cached_lnn_dry)
   if (allocated(cached_xzw_dry))       deallocate(cached_xzw_dry)
   if (allocated(cached_yzw_dry))       deallocate(cached_yzw_dry)
   if (allocated(cached_bottom_area_dry))        deallocate(cached_bottom_area_dry)
   if (allocated(cached_xz_dry))        deallocate(cached_xz_dry)
   if (allocated(cached_yz_dry))        deallocate(cached_yz_dry)
   if (allocated(cached_netcell_dry))   deallocate(cached_netcell_dry)
   
   if (allocated(cache_cross_sections)) call deallocCrossSections(cache_cross_sections)

   md5current = ''

end subroutine default_caching


!> Check that the caching file contained compatible information
logical function cacheRetrieved()
    cacheRetrieved = cache_success
end function cacheRetrieved

!> Load the information from the caching file - if any.
subroutine loadCachingFile( basename, netfile, usecaching )

    use MessageHandling, only: LEVEL_INFO, mess
    
    character(len=*), intent(in   ) :: basename      !< Basename to construct the name of the caching file (typically md_ident).
    character(len=*), intent(in   ) :: netfile       !< Full name of the network file
    integer,          intent(in   ) :: usecaching    !< Use the cache file if possible (1) or not (0)

    integer :: lun
    integer :: ierr
    integer :: number, number_links, number_sections, number_nodes, number_netcells
    character(len=30) :: version_file
    character(len=20) :: key
    character(len=md5length) :: md5checksum
    logical :: okay
    logical :: success
    character(len=256) :: filename

    cache_success = .false.

    if ( usecaching /= 1 ) then
        call mess(LEVEL_INFO,'Not using cache file.')
        return
    endif

    filename = trim(basename) // '.cache'

    call mess(LEVEL_INFO,'Reading cache file: ' // trim(filename))
    open( newunit = lun, file = trim(filename), status = "old", access = "stream", iostat = ierr )

    !
    ! Apparently there is no caching file, so return without further processing
    ! But for writing the caching file later, determine the checksum now
    !
    if ( ierr /= 0 ) then
        call mess(LEVEL_INFO,'Error reading cache file')
        call md5file( netfile, md5current, success )
        return
    endif

    !
    ! Load the version number and the MD5 checksum - useable at all?
    !
    read( lun, iostat = ierr ) version_file, md5checksum

    if ( ierr /= 0 ) then
        close( lun )
        return
    endif

    if ( version_file /= version_string ) then
        !
        ! As there is no history of versions yet, the version in the file
        ! should match exactly
        !
        close( lun )
        return
    endif

    !
    ! Determine the MD5 checksum for the network file - it must match the
    ! checksum in the cache file
    !
    call md5file( netfile, md5current, success )

    if ( md5checksum /= md5current .or. .not. success ) then
        close( lun )
        return
    endif

    !
    ! Load the observation points:
    ! Copy the node numbers when successful
    !
    okay = .true.

    read( lun, iostat = ierr ) key, number

    if ( ierr /= 0 .or. key /= section(key_obs) ) then
        close( lun )
        return
    endif


    call realloc(cache_xobs,     number, keepExisting=.false.)
    call realloc(cache_yobs,     number, keepExisting=.false.)
    call realloc(cache_locTpObs, number, keepExisting=.false.)
    call realloc(cache_kobs,     number, keepExisting=.false.)
    call realloc(cache_lobs,     number, keepExisting=.false.)

    if ( number > 0 ) then
        read( lun, iostat = ierr ) cache_xobs      ; okay = ierr == 0
        read( lun, iostat = ierr ) cache_yobs      ; okay = okay .and. ierr == 0
        read( lun, iostat = ierr ) cache_locTpObs  ; okay = okay .and. ierr == 0
        read( lun, iostat = ierr ) cache_kobs      ; okay = okay .and. ierr == 0
        read( lun, iostat = ierr ) cache_lobs      ; okay = okay .and. ierr == 0
    endif

    if ( .not. okay ) then
        close( lun )
        return
    endif

    !
    ! Load the information on the fixed weirs:
    ! Copy the link numbers when successful
    !
    read( lun, iostat = ierr ) key, number, number_links

    if ( ierr /= 0 .or. key /= section(key_fixed_weirs) ) then
        close( lun )
        return
    endif

    call realloc(cache_xpl_fixed,   number,       keepExisting=.false.)
    call realloc(cache_ypl_fixed,   number,       keepExisting=.false.)
    call realloc(cache_ilink_fixed, number_links, keepExisting=.false.)
    call realloc(cache_ipol_fixed,  number_links, keepExisting=.false.)
    call realloc(cache_dsl_fixed,   number_links, keepExisting=.false.)

    if ( number > 0 ) then
        read( lun, iostat = ierr ) cache_xpl_fixed   ; okay = ierr == 0
        read( lun, iostat = ierr ) cache_ypl_fixed   ; okay = okay .and. ierr == 0
        read( lun, iostat = ierr ) cache_ilink_fixed ; okay = okay .and. ierr == 0
        read( lun, iostat = ierr ) cache_ipol_fixed  ; okay = okay .and. ierr == 0
        read( lun, iostat = ierr ) cache_dsl_fixed   ; okay = okay .and. ierr == 0
    endif

    if ( .not. okay ) then
        close( lun )
        return
    endif

    !
    ! Load the information on the cross-sections:
    ! Copy all information when successful
    !
    read( lun, iostat = ierr ) key, number, number_links

    if ( ierr /= 0 .or. key /= section(key_cross_sections) ) then
        close( lun )
        return
    endif

    allocate( cache_cross_sections(number) )
    allocate( cache_linklist(number_links) )
    allocate( cache_ipol(number_links) )
    call loadCachedSections( lun, cache_linklist, cache_ipol, cache_cross_sections, ierr )
    if ( ierr /= 0 ) then
        close( lun )
        return
    endif

    !
    ! Load the information on grid with deleted dry points and areas:
    !
    read( lun, iostat = ierr ) key, cached_nump_dry, cached_nump1d2d_dry, number_nodes, number_links, number_netcells
    if ( ierr /= 0 .or. key /= section(key_dry_points_and_areas) ) then
        close( lun )
        return
    endif
    allocate( cached_bottom_area_dry(number_nodes) )
    allocate( cached_lne_dry(2,number_links) )
    allocate( cached_lnn_dry(number_links) )
    allocate( cached_xzw_dry(number_nodes) )
    allocate( cached_yzw_dry(number_nodes) )
    allocate( cached_xz_dry(number_nodes) )
    allocate( cached_yz_dry(number_nodes) )
    read( lun, iostat = ierr ) cached_bottom_area_dry, cached_lne_dry, cached_lnn_dry, cached_xz_dry, cached_yz_dry, cached_xzw_dry, cached_yzw_dry ; okay = ierr == 0
    if ( .not. okay ) then
        close( lun )
        return
    endif
    allocate( cached_netcell_dry(number_netcells) )
    call load_netcell( lun, number_netcells, cached_netcell_dry, ierr)
    if ( ierr /= 0 ) then
        close( lun )
        return
    endif

    !
    ! All cached values were loaded, so all is well
    !
    close( lun )
    cache_success = .true.

end subroutine loadCachingFile

!> Load cached netcell from a caching file.
subroutine load_netcell( lun, number_netcell, netcell , ierr)
    integer,                    intent(in   ) :: lun            !< LU-number of the caching file
    integer,                    intent(in   ) :: number_netcell !< Number of netcells
    type (tface), dimension(:), intent(  out) :: netcell        !< (nump1d2d) 1D&2D net cells (nodes and links), a cell with net nodes as vertices.
    integer,                    intent(  out) :: ierr           !< Error code

    integer                              :: i, number_links, number_nodes
    
    do i=1,number_netcell
        read( lun, iostat = ierr ) netcell(i)%n 
        if ( ierr /= 0 ) then
            close( lun )
            exit
        endif
        if (netcell(i)%n > 0) then
            read( lun, iostat = ierr ) number_nodes, number_links
            if ( ierr /= 0 ) then
                close( lun )
                exit
            endif
            allocate(netcell(i)%nod(number_nodes),netcell(i)%lin(number_links))
            read( lun, iostat = ierr ) netcell(i)%nod, netcell(i)%lin
            if ( ierr /= 0 ) then
                close( lun )
                exit
            endif
        endif
    enddo

end subroutine load_netcell

!> Load cached cross sections from a caching file.
subroutine loadCachedSections( lun, linklist, ipol, sections, ierr )
    integer,                  intent(in   ) :: lun       !< LU-number of the caching file
    integer, dimension(:),    intent(  out) :: linklist  !< Cached list of crossed flow links
    integer, dimension(:),    intent(  out) :: ipol      !< Cached polygon administration
    type(tcrs), dimension(:), intent(  out) :: sections  !< Array of cross-sections to be filled
    integer,                  intent(  out) :: ierr      !< Error code

    integer                                 :: i, np, nlink
    logical                                 :: okay

    ! If there is nothing to be cached, do not even try to read (D3DFMIQ-2193)
    if ( size(linklist) == 0 ) then
        ierr = 0
        return
    endif

    read( lun, iostat = ierr ) linklist
    if ( ierr /= 0 ) then
        close( lun )
        return
    endif

    read( lun, iostat = ierr ) ipol
    if ( ierr /= 0 ) then
        close( lun )
        return
    endif

    do i = 1,size(sections)
        read( lun, iostat = ierr ) np, nlink

        sections(i)%path%np  = np
        sections(i)%path%lnx = nlink
        allocate( sections(i)%path%xp(np), sections(i)%path%yp(np),  &
                  sections(i)%path%zp(np), sections(i)%path%indexp(nlink), &
                  sections(i)%path%xk(2,nlink), sections(i)%path%yk(2,nlink), &
                  sections(i)%path%wfp(nlink), &
                  sections(i)%path%iperm(nlink), sections(i)%path%wfk1k2(nlink), &
                  sections(i)%path%sp(nlink), sections(i)%path%ln(nlink) )

        if ( nlink > 0 ) then
            read( lun, iostat = ierr ) sections(i)%path%xp, sections(i)%path%yp,  &
                                       sections(i)%path%zp, sections(i)%path%indexp, &
                                       sections(i)%path%xk, sections(i)%path%yk, &
                                       sections(i)%path%wfp, &
                                       sections(i)%path%iperm, sections(i)%path%wfk1k2, &
                                       sections(i)%path%sp, sections(i)%path%ln
        else
            if ( np > 0 ) then
                read( lun, iostat = ierr ) sections(i)%path%xp, sections(i)%path%yp,  &
                                           sections(i)%path%zp
            endif
        endif
        if ( ierr /= 0 ) then
            close( lun )
            exit
        endif
    enddo
end subroutine loadCachedSections

!> Save the link list of crossed flow links for later storage in the caching file.
subroutine saveLinklist( length, linklist, ipol )
    integer,                  intent(in   ) :: length    !< Length of the list of crossed flow links
    integer, dimension(:),    intent(in   ) :: linklist  !< List of crossed flow links to be saved
    integer, dimension(:),    intent(in   ) :: ipol      !< Polygon administration

    cache_linklist = linklist(1:length)
    cache_ipol     = ipol(1:length)
end subroutine saveLinklist

!> Store the grid-based information in the caching file.
subroutine storeCachingFile( basename, usecaching )
    character(len=*), intent(in   ) :: basename            !< Basename to construct the name of the caching file (typically md_ident).
    integer,          intent(in   ) :: usecaching          !< Write the caching file (1) or not (0) - in accordance with the user setting

    integer :: lun
    integer :: ierr
    character(len=256) :: filename

    cache_success = .false.

    !
    ! If no caching should be used, dispense with writing the caching file
    !
    if ( usecaching /= 1 ) then
        return
    endif

    filename = trim(basename) // '.cache'

    open( newunit = lun, file = trim(filename), access = "stream", status = "old", action = 'read',  iostat = ierr )

    if ( ierr == 0 ) then
        close( lun, status = "delete" )
    endif
    open( newunit = lun, file = trim(filename), access = "stream" )

    !
    ! Store version string and checksum (already determined at start-up)
    !
    write( lun ) version_string, md5current

    !
    ! Store the observation points
    !
    write( lun ) section(key_obs), numobs
    if ( numobs > 0 ) then
        write( lun ) xobs(1:numobs), yobs(1:numobs), locTpObs(1:numobs), kobs(1:numobs), lobs(1:numobs)
    endif

    !
    ! Store the fixed weirs data
    !
    write( lun ) section(key_fixed_weirs), size(cache_xpl_fixed), size(cache_ilink_fixed)

    if ( size(cache_xpl_fixed) > 0 ) then
        write( lun ) cache_xpl_fixed, cache_ypl_fixed, cache_ilink_fixed, cache_ipol_fixed, cache_dsl_fixed
    endif

    !
    ! Store the data for the cross-sections
    !
    if ( .not. allocated(crs) ) then
        allocate( crs(0) )
    endif
    if ( .not. allocated(cache_linklist) ) then
        allocate( cache_linklist(0) )
    endif
    if ( .not. allocated(cache_ipol) ) then
        allocate( cache_ipol(0) )
    endif
    write( lun ) section(key_cross_sections), size(crs)
    call storeSections( lun, crs, cache_linklist, cache_ipol )

    !
    ! Store the data for the dry points and areas
    !
    write( lun ) section(key_dry_points_and_areas), cached_nump_dry, cached_nump1d2d_dry,  size(cached_bottom_area_dry), size(cached_lnn_dry), size(cached_netcell_dry,1)
    write( lun ) cached_bottom_area_dry, cached_lne_dry, cached_lnn_dry, cached_xz_dry, cached_yz_dry, cached_xzw_dry, cached_yzw_dry
    call store_netcell(lun, cached_netcell_dry)
    
    !
    ! We are done, so close the file
    !
    close( lun )

end subroutine storeCachingFile

!> Store netcell to a caching file.
subroutine store_netcell( lun, netcell )
    integer,                    intent(in   ) :: lun     !< LU-number of the caching file
    type (tface), dimension(:), intent(in   ) :: netcell !< Nr. of 2d netcells.

    integer                              :: i, number_netcell, number_links, number_nodes
    number_netcell = size(netcell,1)
    
    do i=1,number_netcell
        write( lun ) netcell(i)%n
        if (netcell(i)%n > 0) then
            number_nodes = size(netcell(i)%nod)
            number_links = size(netcell(i)%lin)
            write( lun ) number_nodes, number_links
            write( lun ) netcell(i)%nod(1:number_nodes), netcell(i)%lin(1:number_links)
        endif
    enddo

end subroutine store_netcell

!> Store cross sections to a caching file.
subroutine storeSections( lun, sections, linklist, ipol )
    integer,                  intent(in   ) :: lun       !< LU-number of the caching file
    type(tcrs), dimension(:), intent(in   ) :: sections  !< Array of cross-sections to be filled
    integer, dimension(:),    intent(in   ) :: linklist  !< List of crossed flow links
    integer, dimension(:),    intent(in   ) :: ipol      !< Polygon administration

    integer                              :: i, np, nlink

    write( lun ) size(linklist)
    write( lun ) linklist
    write( lun ) ipol

    do i = 1,size(sections)
        np    = sections(i)%path%np
        nlink = sections(i)%path%lnx
        write( lun ) sections(i)%path%np, sections(i)%path%lnx

        if ( nlink > 0 ) then
            write( lun ) sections(i)%path%xp(1:np), sections(i)%path%yp(1:np),  &
                         sections(i)%path%zp(1:np), sections(i)%path%indexp(1:nlink), &
                         sections(i)%path%xk(:,1:nlink), sections(i)%path%yk(:,1:nlink), &
                         sections(i)%path%wfp(1:nlink), &
                         sections(i)%path%iperm(1:nlink), sections(i)%path%wfk1k2(1:nlink), &
                         sections(i)%path%sp(1:nlink), sections(i)%path%ln(1:nlink)
        else
            if ( np > 0 ) then
                write( lun ) sections(i)%path%xp(1:np), sections(i)%path%yp(1:np),  &
                             sections(i)%path%zp(1:np)
            endif
        endif
    enddo
end subroutine storeSections

!> Copy the cached network information for observation points.
subroutine copyCachedObservations( success )
    logical, intent(  out) :: success             !< The cached information was compatible if true

    success = .false.
    if ( cache_success ) then
        !
        ! Check the number of observations
        !
        if (.not. allocated(cache_xobs)) then
            return
        else if ( numobs /= size(cache_xobs) ) then
            return
        endif
        !
        ! Check that the coordinates and the type are identical to the cached values
        !
        if ( all( cache_xobs == xobs(1:numobs) ) .and. all( cache_yobs == yobs(1:numobs) ) .and. &
             all( cache_locTpObs == locTpObs(1:numobs) ) ) then
            success        = .true.
            kobs(1:numobs) = cache_kobs
            lobs(1:numobs) = cache_lobs
        endif
    endif
end subroutine copyCachedObservations

!> Copy the cached network information for cross-sections
subroutine copyCachedCrossSections( linklist, ipol, success )
    integer, dimension(:), allocatable, intent(  out) :: linklist            !< Cached list of crossed flow links
    integer, dimension(:), allocatable, intent(  out) :: ipol                !< Polygon administration
    logical,                            intent(  out) :: success             !< The cached information was compatible if true

    integer                :: i, np

    success = .false.

    if ( cache_success ) then
        !
        ! Check the number of observations
        !
        if ( size(crs) /= size(cache_cross_sections) ) then
            return
        endif
        !
        ! Check that the coordinates and the type are identical to the cached values
        ! Note: no check on zp, it seems filled with arbitrary data (at least in 2D?)
        !
        success = .true.
        do i = 1,size(cache_cross_sections)
            np = cache_cross_sections(i)%path%np
            if ( np /= crs(i)%path%np ) then
                success        = .false.
                exit
            endif
            if ( np == 0 ) cycle
            if ( any( cache_cross_sections(i)%path%xp(1:np) /= crs(i)%path%xp(1:np) ) .or. &
                 any( cache_cross_sections(i)%path%yp(1:np) /= crs(i)%path%yp(1:np) ) ) then
                success        = .false.
                exit
            endif
        enddo

        if ( success ) then
            linklist = cache_linklist
            ipol     = cache_ipol

            do i = 1,size(cache_cross_sections)
                ! Rely on automatic (re)allocation)
                crs(i)%path%np     = cache_cross_sections(i)%path%np
                crs(i)%path%lnx    = cache_cross_sections(i)%path%lnx
                crs(i)%path%indexp = cache_cross_sections(i)%path%indexp
                crs(i)%path%xk     = cache_cross_sections(i)%path%xk
                crs(i)%path%yk     = cache_cross_sections(i)%path%yk
                crs(i)%path%wfp    = cache_cross_sections(i)%path%wfp
                crs(i)%path%iperm  = cache_cross_sections(i)%path%iperm
                crs(i)%path%wfk1k2 = cache_cross_sections(i)%path%wfk1k2
                crs(i)%path%sp     = cache_cross_sections(i)%path%sp
                crs(i)%path%ln     = cache_cross_sections(i)%path%ln
            enddo
        endif
    endif
end subroutine copyCachedCrossSections

!> Copy the cached information on fixed weirs.
subroutine copyCachedFixedWeirs( npl, xpl, ypl, number_links, iLink, iPol, dSL, success )
    integer,                        intent(in   ) :: npl                !< Number of points in the polylines making up the weirs
    double precision, dimension(:), intent(in   ) :: xpl                !< X-coordinates of the polyline points for the weirs
    double precision, dimension(:), intent(in   ) :: ypl                !< Y-coordinates of the polyline points for the weirs
    integer,                        intent(  out) :: number_links       !< Number of flow links that was cached
    double precision, dimension(:), intent(  out) :: dSL                !< Intersection distance of each flow link on polyline segments that were cached
    integer, dimension(:),          intent(  out) :: iLink              !< Flow link numbers that were cached
    integer, dimension(:),          intent(  out) :: iPol               !< Intersected polyline segment numbers that were cached
    logical,                        intent(  out) :: success            !< The cached information was compatible if true

    success = .false.
    if ( cache_success ) then
        !
        ! Check the number of coordinate pairs
        !
        if (.not. allocated(cache_xpl_fixed)) then
            return
        else if ( npl /= size(cache_xpl_fixed) ) then
            return
        endif
        !
        ! Check that the coordinates are identical to the cached values
        !
        if ( all( cache_xpl_fixed == xpl(1:npl) ) .and. all( cache_ypl_fixed == ypl(1:npl) ) ) then
            success      = .true.
            number_links = size(cache_iLink_fixed)
            iLink(1:number_links) = cache_iLink_fixed
            iPol(1:number_links)  = cache_iPol_fixed
            dSL(1:number_links)   = cache_dSL_fixed
        endif
    endif
end subroutine copyCachedFixedWeirs

!> cacheFixedWeirs:
!>     The arrays for fixed weirs are partly local - they do not reside in a
!>     module, so explicitly store them when we have the actual data
subroutine cacheFixedWeirs( npl, xpl, ypl, number_links, iLink, iPol, dSL )
    integer,                        intent(in   ) :: npl             !< Number of points in the polylines making up the weirs
    integer,                        intent(in   ) :: number_links    !< Number of flow links that is to be cached
    double precision, dimension(:), intent(in   ) :: xpl             !< X-coordinates of the polyline points for the weirs
    double precision, dimension(:), intent(in   ) :: ypl             !< Y-coordinates of the polyline points for the weirs
    double precision, dimension(:), intent(in   ) :: dSL             !< Intersection distance of each flow link on polyline segments that are to be cached
    integer, dimension(:),          intent(in   ) :: iLink           !< Flow link numbers to be cached
    integer, dimension(:),          intent(in   ) :: iPol            !< Intersected polyline segment number to be cached

    cache_xpl_fixed   = xpl(1:npl)
    cache_ypl_fixed   = ypl(1:npl)
    cache_iLink_fixed = iLink(1:number_links)
    cache_iPol_fixed  = iPol(1:number_links)
    cache_dSL_fixed   = dSL(1:number_links)
end subroutine cacheFixedWeirs

!> Copy grid information, where dry points and areas have been deleted, from cache file: 
subroutine copy_cached_netgeom_without_dry_points_and_areas(nump, nump1d2d, lne, lnn, bottom_area, xz, yz, xzw, yzw, netcell, success)
    integer,                        intent(  out) :: nump         !< Nr. of 2d netcells.
    integer,                        intent(  out) :: nump1d2d     !< nr. of 1D and 2D netcells (2D netcells come first)
    integer, dimension(:,:),        intent(  out) :: lne          !< (2,numl) Edge administration 1=nd1 , 2=nd2, rythm of kn flow nodes between/next to which this net link lies.
    integer, dimension(:),          intent(inout) :: lnn          !< (numl) Nr. of cells in which link participates (ubound for non-dummy values in lne(:,L))
    double precision, dimension(:), intent(inout) :: bottom_area  !< [m2] bottom area, if < 0 use table in node type {"location": "face", "shape": ["ndx"]}
    double precision, dimension(:), intent(  out) :: xz           !< [m/degrees_east] waterlevel point / cell centre, x-coordinate (m) {"location": "face", "shape": ["ndx"]}
    double precision, dimension(:), intent(  out) :: yz           !< [m/degrees_north] waterlevel point / cell centre, y-coordinate (m) {"location": "face", "shape": ["ndx"]}
    double precision, dimension(:), intent(  out) :: xzw          !< [m] x-coordinate, centre of gravity {"shape": ["nump"]}
    double precision, dimension(:), intent(  out) :: yzw          !< [m] y-coordinate, centre of gravity {"shape": ["nump"]}
    type (tface), dimension(:),     intent(inout) :: netcell      !< (nump1d2d) 1D&2D net cells (nodes and links)
    logical,                        intent(  out) :: success      !< The cached information was compatible if true
    
    integer number_nodes, number_links, number_netcells
    
    number_nodes = size(bottom_area)
    number_links = size(lnn)
    number_netcells = size(netcell)

    success = .false.
    if ( cache_success ) then
        if (.not. allocated(cached_netcell_dry)) then
            return
        endif
        if ( size(bottom_area) == size(cached_bottom_area_dry) ) then
            success      = .true.
            nump = cached_nump_dry
            nump1d2d = cached_nump1d2d_dry
            lne = cached_lne_dry(1:2,1:number_links)
            lnn = cached_lnn_dry(1:number_links)
            xzw = cached_xzw_dry(1:number_nodes)
            yzw = cached_yzw_dry(1:number_nodes)
            bottom_area  = cached_bottom_area_dry(1:number_nodes)
            xz  = cached_xz_dry(1:number_nodes)
            yz  = cached_yz_dry(1:number_nodes)
            netcell = cached_netcell_dry(1:number_netcells)
        endif
    endif
end subroutine copy_cached_netgeom_without_dry_points_and_areas

!> Cache grid information, where dry points and areas have been deleted: 
subroutine cache_netgeom_without_dry_points_and_areas( nump, nump1d2d, lne, lnn, bottom_area, xz, yz, xzw, yzw, netcell )
    integer,                        intent(in   ) :: nump         !< Nr. of 2d netcells.
    integer,                        intent(in   ) :: nump1d2d     !< nr. of 1D and 2D netcells (2D netcells come first)
    integer, dimension(:,:),        intent(in   ) :: lne          !< (2,numl) Edge administration 1=nd1 , 2=nd2, rythm of kn flow nodes between/next to which this net link lies.
    integer, dimension(:),          intent(in   ) :: lnn          !< (numl) Nr. of cells in which link participates (ubound for non-dummy values in lne(:,L))
    double precision, dimension(:), intent(in   ) :: bottom_area  !< [m2] bottom area, if < 0 use table in node type {"location": "face", "shape": ["ndx"]}
    double precision, dimension(:), intent(in   ) :: xz           !< [m/degrees_east] waterlevel point / cell centre, x-coordinate (m) {"location": "face", "shape": ["ndx"]}
    double precision, dimension(:), intent(in   ) :: yz           !< [m/degrees_north] waterlevel point / cell centre, y-coordinate (m) {"location": "face", "shape": ["ndx"]}
    double precision, dimension(:), intent(in   ) :: xzw          !< [m] x-coordinate, centre of gravity {"shape": ["nump"]}
    double precision, dimension(:), intent(in   ) :: yzw          !< [m] y-coordinate, centre of gravity {"shape": ["nump"]}
    type (tface), dimension(:),     intent(in   ) :: netcell      !< (nump1d2d) 1D&2D net cells (nodes and links)
    integer number_nodes
    integer number_links
    integer number_netcells
    
    cached_nump_dry = nump
    cached_nump1d2d_dry = nump1d2d
    number_nodes = size(bottom_area)
    number_links = size(lnn)
    number_netcells = size(netcell)
    cached_lne_dry = lne(1:2,1:number_links)
    cached_lnn_dry = lnn(1:number_links)
    cached_xzw_dry = xzw(1:number_nodes)
    cached_yzw_dry = yzw(1:number_nodes)
    cached_bottom_area_dry = bottom_area(1:number_nodes)
    cached_xz_dry = xz(1:number_nodes)
    cached_yz_dry = yz(1:number_nodes)
    cached_netcell_dry = netcell(1:number_netcells)

end subroutine cache_netgeom_without_dry_points_and_areas

end module unstruc_caching
