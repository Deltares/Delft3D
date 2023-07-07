!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2023.
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
    use m_observations, only: numobs, xobs, yobs, locTpObs, kobs
    use m_monitoring_crosssections, only: crs, tcrs, deallocCrossSections
    !use m_crspath, only: tcrspath
    use md5_checksum
    use m_alloc
    use m_longculverts, only: t_longculvert, longculverts

    implicit none

    logical, private :: cache_success

    character(len=20), dimension(10), private :: section = ['OBSERVATIONS        ', &
                                                            'FIXED WEIRS         ', &
                                                            'CROSS_SECTIONS      ', &
                                                            'LONG_CULVERTS       ', &
                                                            '12345678901234567890', &
                                                            '12345678901234567890', &
                                                            '12345678901234567890', &
                                                            '12345678901234567890', &
                                                            '12345678901234567890', &
                                                            '12345678901234567890']
    integer, parameter, private :: key_obs = 1
    integer, parameter, private :: key_fixed_weirs = 2
    integer, parameter, private :: key_cross_sections = 3
    integer, parameter, private :: key_long_culverts = 4

    double precision, dimension(:), allocatable, private :: cache_xobs
    double precision, dimension(:), allocatable, private :: cache_yobs
    double precision, dimension(:), allocatable, private :: cache_xpl_fixed
    double precision, dimension(:), allocatable, private :: cache_ypl_fixed
    double precision, dimension(:), allocatable, private :: cache_dsl_fixed
    integer, dimension(:), allocatable, private          :: cache_locTpObs
    integer, dimension(:), allocatable, private          :: cache_kobs
    integer, dimension(:), allocatable, private          :: cache_ilink_fixed
    integer, dimension(:), allocatable, private          :: cache_ipol_fixed
    integer, dimension(:), allocatable, private          :: cache_linklist
    integer, dimension(:), allocatable, private          :: cache_ipol

    type(tcrs), dimension(:), allocatable                :: cache_cross_sections
    type(t_longculvert), dimension(:), allocatable       :: cache_longculverts


    character(len=30), parameter, private :: version_string = "D-Flow FM, cache file, 1.0"
    character(len=md5length), private :: md5current

contains
!> Sets ALL (scalar) variables in this module to their default values.
subroutine default_caching()

   cache_success = .false.

   if (allocated(cache_xobs))        deallocate(cache_xobs)
   if (allocated(cache_yobs))        deallocate(cache_yobs)
   if (allocated(cache_xpl_fixed))   deallocate(cache_xpl_fixed)
   if (allocated(cache_ypl_fixed))   deallocate(cache_ypl_fixed)
   if (allocated(cache_dsl_fixed))   deallocate(cache_dsl_fixed)
   if (allocated(cache_locTpObs))    deallocate(cache_locTpObs)
   if (allocated(cache_kobs))        deallocate(cache_kobs)
   if (allocated(cache_ilink_fixed)) deallocate(cache_ilink_fixed)
   if (allocated(cache_ipol_fixed))  deallocate(cache_ipol_fixed)
   if (allocated(cache_linklist))    deallocate(cache_linklist)
   if (allocated(cache_ipol))        deallocate(cache_ipol)

   if (allocated(cache_longculverts))   deallocate(cache_longculverts)

   if (allocated(cache_cross_sections)) call deallocCrossSections(cache_cross_sections)

   md5current = ''

end subroutine default_caching


!> Check that the caching file contained compatible information
logical function cacheRetrieved()
    cacheRetrieved = cache_success
end function cacheRetrieved

!> Load the information from the caching file - if any.
subroutine loadCachingFile( basename, netfile, usecaching )
    character(len=*), intent(in   ) :: basename      !< Basename to construct the name of the caching file (typically md_ident).
    character(len=*), intent(in   ) :: netfile       !< Full name of the network file
    integer,          intent(in   ) :: usecaching    !< Use the cache file if possible (1) or not (0)

    integer :: lun
    integer :: ierr
    integer :: number, number_links, number_sections
    character(len=30) :: version_file
    character(len=20) :: key
    character(len=md5length) :: md5checksum
    logical :: okay
    logical :: success
    character(len=256) :: filename

    cache_success = .false.

    if ( usecaching /= 1 ) then
        return
    endif

    filename = trim(basename) // '.cache'

    open( newunit = lun, file = trim(filename), status = "old", access = "stream", iostat = ierr )

    !
    ! Apparently there is no caching file, so return without further processing
    ! But for writing the caching file later, determine the checksum now
    !
    if ( ierr /= 0 ) then
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

    if ( number > 0 ) then
        read( lun, iostat = ierr ) cache_xobs      ; okay = ierr == 0
        read( lun, iostat = ierr ) cache_yobs      ; okay = okay .and. ierr == 0
        read( lun, iostat = ierr ) cache_locTpObs  ; okay = okay .and. ierr == 0
        read( lun, iostat = ierr ) cache_kobs      ; okay = okay .and. ierr == 0
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
    ! Load the information on the long culverts:
    ! Copy all information when successful
    !
    read( lun, iostat = ierr ) key, number

    if ( ierr /= 0 .or. key /= section(key_long_culverts) ) then
        close( lun )
        return
    endif

    allocate( cache_longculverts(number) )
    call loadCachedLongCulverts( lun, cache_longculverts, ierr )
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

!> Load cached long culverts from a caching file.
subroutine loadCachedLongCulverts( lun, longculverts, ierr )
    integer,                           intent(in   ) :: lun          !< LU-number of the caching file
    type(t_longculvert), dimension(:), intent(  out) :: longculverts !< Array of long culverts to be filled
    integer,                           intent(  out) :: ierr         !< Error code

    integer                                 :: i, np, node_up, node_down

    do i = 1,size(longculverts)
        read( lun, iostat = ierr ) np, node_up, node_down

        longculverts(i)%numlinks    = np
        longculverts(i)%flownode_up = node_up
        longculverts(i)%flownode_dn = node_down
        allocate( longculverts(i)%xcoords(np+1), &
                  longculverts(i)%ycoords(np+1), &
                  longculverts(i)%netlinks(np),  &
                  longculverts(i)%flowlinks(np) )

        if ( np > 0 ) then
            read( lun, iostat = ierr ) longculverts(i)%xcoords,  &
                                       longculverts(i)%ycoords,  &
                                       longculverts(i)%netlinks, &
                                       longculverts(i)%flowlinks
        endif
        if ( ierr /= 0 ) then
            close( lun )
            exit
        endif
    enddo
end subroutine loadCachedLongCulverts

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
        write( lun ) xobs(1:numobs), yobs(1:numobs), locTpObs(1:numobs), kobs(1:numobs)
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
    ! Store the data for the long culverts
    !
    !if ( .not. allocated(longculverts) ) then
    !    allocate( longculverts(0) )
    !endif
    !write( lun ) section(key_long_culverts), size(longculverts)
    !call storeLongCulverts( lun, longculverts )

    !
    ! We are done, so close the file
    !
    close( lun )

end subroutine storeCachingFile

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

!> Store long culverts to a caching file.
subroutine storeLongCulverts( lun, longculverts )
    integer,                           intent(in   ) :: lun          !< LU-number of the caching file
    type(t_longculvert), dimension(:), intent(in   ) :: longculverts !< Array of long culverts to be filled

    integer                                          :: i

    do i = 1,size(longculverts)
        write( lun ) longculverts(i)%numlinks, longculverts(i)%flownode_up, longculverts(i)%flownode_dn

        if ( longculverts(i)%numlinks > 0 ) then
            write( lun ) longculverts(i)%xcoords,  &
                         longculverts(i)%ycoords,  &
                         longculverts(i)%netlinks, &
                         longculverts(i)%flowlinks
        endif
    enddo
end subroutine storeLongCulverts

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

!> Copy the cached information on long culverts
subroutine copyCachedLongCulverts( longculverts, success )
    type(t_longculvert), dimension(:), intent(inout) :: longculverts !< Array of long culverts to be filled, partly already filled
    logical,                           intent(  out) :: success      !< The cached information was compatible if true

    integer                                          :: i

    success = .false.
    if ( cache_success ) then
        if ( size(cache_longculverts) == size(longculverts) ) then
            success = .true.
            do i = 1,size(longculverts)
                if ( cache_longculverts(i)%numlinks > 0 .and. longculverts(i)%numlinks > 0 ) then
                    if ( any( cache_longculverts(i)%xcoords /= longculverts(i)%xcoords ) .or. &
                         any( cache_longculverts(i)%ycoords /= longculverts(i)%ycoords ) ) then
                        success = .false.
                    endif
                endif
            enddo
        endif

        if ( success ) then
            do i = 1,size(longculverts)
                longculverts(i)%flownode_up = cache_longculverts(i)%flownode_up
                longculverts(i)%flownode_dn = cache_longculverts(i)%flownode_dn
                if ( cache_longculverts(i)%numlinks > 0 ) then
                    longculverts(i)%netlinks  = cache_longculverts(i)%netlinks
                    longculverts(i)%flowlinks = cache_longculverts(i)%flowlinks
                endif
            enddo
        endif
    endif
end subroutine copyCachedLongCulverts

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

end module unstruc_caching
