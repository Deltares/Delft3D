module meteo_data
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
! see meteo.f90
!
!!--pseudo code and references--------------------------------------------------
!
! Stef.Hummel@deltares.nl
! Herman.Kernkamp@deltares.nl
! Adri.Mourits@deltares.nl
!
!!--declarations----------------------------------------------------------------
   use precision
!
! Module constants
!
   integer,      parameter :: maxnamelen     = 256
   real(fp),     parameter :: nodata_default = -999.0_fp   ! Default missing value in meteo arrays
   !
   ! enumeration for data fields
   !
   integer, parameter :: uniuvp                         =  1    ! 3 values per timestep 1 dim arr
   integer, parameter :: meteo_on_computational_grid    =  2    ! 3 velden per timestep 3 dim array
   integer, parameter :: meteo_on_equidistant_grid      =  3    ! 1 veld per timestep 2 dim array
   integer, parameter :: meteo_on_spiderweb_grid        =  4    ! 3 velden per timestep 3 dim array
   integer, parameter :: meteo_on_curvilinear_grid      =  5    ! 1 veld per timestep 2 dim array
   integer, parameter :: numfiletypes                   =  5
   integer, parameter :: max_nummeteoitems              = 10
   integer, parameter, private :: max_num_meteopointers = 50
   !
   ! enumeration for optional flags
   !
   integer, parameter :: opt_not_defined      = 0
   integer, parameter :: opt_value_specified  = 1
   integer, parameter :: opt_use_from_kernel  = 2
!
! type definitions
!
   type tfield
      ! contains time and field information
      logical                             :: all_nodata     ! Flag to indicate whether all data values are missing
                                                            ! (Only for meteo_on_spiderweb_grid)
      real(hp)                            :: time
      real(hp)                            :: x_spw_eye      ! X-coordinate of cyclone eye (spiderweb centre) at specified TIME
      real(hp)                            :: y_spw_eye      ! Y-coordinate of cyclone eye (spiderweb centre) at specified TIME
      real(hp)                            :: dx             ! x-gridsize
      real(hp)                            :: dy             ! y-gridsize
      real(hp)                            :: p_drop_spw_eye !
      real(hp), dimension(:,:,:), pointer :: arr3d          ! 3-dim array
      real(hp), dimension(:,:),   pointer :: arr2d          ! 2-dim array
      real(hp), dimension(:),     pointer :: arr1d          ! 1-dim array
   end type tfield


   type tgrid
      integer                            :: num_columns       ! Size of meteo grid, related to tmeteoitem%numm
      integer                            :: num_rows       ! Size of meteo grid, related to tmeteoitem%numn
      integer                            :: mcur
      integer                            :: ncur
      integer, dimension(:,:,:), pointer :: mnref
      real(fp)                           :: misval
      real(hp), dimension(:,:) , pointer :: x
      real(hp), dimension(:,:) , pointer :: y
      character(20)                      :: type       ! Spheric or cartesian
      character(maxnamelen)              :: filename   ! File containing data
   end type tgrid


   type tmeteoitem
      integer                       :: filetype         ! Type of meteofile, read from MD-file
      integer                       :: it0              ! See it1
      integer                       :: it1              ! Index old or new fields
      integer                       :: lun              ! Handle to file
      integer                       :: mfirst           ! Mfirst, mlast, nfirst and nlast are the grid indices that result from the  
                                                        ! possible reversing of grid coordinates determined by first_data_value and data_row.
      integer                       :: mlast            ! See mfirst
      integer                       :: n_cols           ! Number of columns used for meteo datafield
      integer                       :: n_quantity       ! Number of quantities prescribed in the file
      integer                       :: n_rows           ! Number of rows used for meteo datafield
      integer                       :: nfirst           ! See mfirst
      integer                       :: nlast            ! See mfirst
      integer                       :: numk
      integer                       :: numm             ! Size of meteo grid, related to tgrid%num_columns
      integer                       :: numn             ! Size of meteo grid, related to tgrid%num_rows
      integer                       :: pref_option      ! Reference pressure options: 0: not defined, 1: value specified, 2: use from kernel
      real(hp)                      :: dx               ! x-gridsize
      real(hp)                      :: dy               ! y-gridsize
      real(fp)                      :: nodata_value     ! Value used for undefined or missing data
      real(hp)                      :: p_conv           ! Conversion factor for air_pressure (to Pa)
      real(hp)                      :: pref             ! Reference value for air_pressure, optionally used by spiderweb: patm = pref - p_drop
      real(hp)                      :: spw_radius       ! Radius of spiderweb (in units specified in spw_rad_unit)
      real(hp)                      :: spw_merge_frac   ! Fraction of radius for merging spw data with background wind
      real(hp)                      :: x_llcorner       ! X-coordinate of lower left corner of lower left cell of grid (in units specified in grid_unit)
                                                        ! (Only for meteo_on_equidistant_grid)
      real(hp)                      :: y_llcorner       ! Y-coordinate of lower left corner of lower left cell of grid (in units specified in grid_unit)
                                                        ! (Only for meteo_on_equidistant_grid)
      real(hp)                      :: x_llcenter       ! X-coordinate of centre of lower left cell of grid (in units specified in grid_unit)
                                                        ! (Only for meteo_on_equidistant_grid)
      real(hp)                      :: y_llcenter       ! Y-coordinate of centre of lower left cell of grid (in units specified in grid_unit)
                                                        ! (Only for meteo_on_equidistant_grid)
      logical                       :: firstcopy        ! Copied field0 after first read?
      logical                       :: initialized      ! Initialized or not?
      logical                       :: mrow             ! Flag for swithcing columns and row when reading the data on the curvilinear grid file
      character(20)                 :: data_row         ! Specifies whether a row of data corresponds to a row or a column on the grid (for the curvilinear grid file)
      character(256)                :: grid_file        ! Separate grid on which the wind can be specified
      character(20)                 :: first_data_value ! Location on the grid to which the first value of the dataset corresponds
      character(maxnamelen)         :: filename         ! File containing data
      character(20)                 :: fileversion      ! Version of meteo input file, to check if the newest file format is used
      character(20)                 :: grid_unit        ! Unit of distances on the grid      
      character(256)                :: meteotype        ! Type of meteo input file: svwp on the FLOW grid, on a separate curvilinear grid,
                                                        ! on a separate equidistant grid or on a spiderweb grid, read from meteo file
      character(60), dimension(3)   :: quantities       ! Quantities to be served by this meteoitem, normally just one. 
                                                        ! Exceptions: meteotypes uniuvp, meteo_on_computational_grid, meteo_on_spiderweb_grid
      character(20)                 :: spw_rad_unit     ! Unit of spiderweb radius, must be meters
      character(100)                :: time_unit        ! Actual unit of time, distilled from time_definition, possibly decorated with spaces and long real by the user
      character(300)                :: time_definition  ! Fixed format: Time unit 'since' reference date and time +- time zone difference, possibly decorated with spaces and long reals by the user
      character(30), dimension(3)   :: units = 'undef'  ! Units of the three quantities
      type(tfield) , dimension(0:1) :: field            ! Old and new fields
      type(tgrid)  , pointer        :: grid
   end type tmeteoitem


   type tmeteoitempointer
      type(tmeteoitem), pointer :: ptr
   end type tmeteoitempointer


   type tflowgrid
      logical                           :: initialized
      integer                           :: num_columns
      integer                           :: num_rows
      integer , dimension(:,:), pointer :: kcs
      real(fp), dimension(:,:), pointer :: xz
      real(fp), dimension(:,:), pointer :: yz
   end type tflowgrid


   type tspiderweb
      logical                             :: active
      real(fp)                            :: time   ! time for which spiderweb arrays are set 
      real(fp), dimension(:,:), pointer   :: spwf   ! merge factor spiderweb
      real(fp), dimension(:,:,:), pointer :: spwarr ! addition of windu(1,n,m), windv(2,n,m) and patm(3,n,m), due to spiderweb
   end type tspiderweb


   type tmeteo
     !
     ! Datastucture for all meteo items (= Meteo structure for one subdomain)
     !
     integer                                                  :: nummeteoitems
     type(tmeteoitempointer), dimension(max_nummeteoitems)    :: item
     type(tflowgrid)                                          :: flowgrid
     type(tspiderweb)                                         :: spiderweb
   end type tmeteo


   type tmeteopointer
       character(maxnamelen) :: subdomname
       type(tmeteo), pointer :: pntr
   end type tmeteopointer
!
! Module parameters
!
   integer                                                     :: num_meteopointers     
   integer                                                     :: message_count         ! Flag used to make sure message on the time zone shift is given only once
   logical, save                                               :: meteodata_initialized = .false.
   logical                                                     :: msferic               ! true: meteo grid is sferical, false: cartesian
   logical                                                     :: meteoint              ! true: interpolate in time, false: block
   real(fp)                                                    :: patm_default          ! Default air pressure (Pa)
   character(500)                                              :: meteomessage          ! (error)messages are set in meteomessage.
                                                                                        ! function getmeteomessage returns the message

   type(tmeteopointer), dimension(max_num_meteopointers),save  :: meteopointers


contains

subroutine init_meteo_data
   implicit none

   num_meteopointers     = 0
   message_count         = 0
   msferic               = .false.
   meteoint              = .true.  
   patm_default          = 101300.0_fp
   meteomessage          = ' '
  
end subroutine init_meteo_data


!subroutine meteosetsferic()
!   implicit none
!   msferic = .true.
!end subroutine meteosetsferic


subroutine nullifyfield( field )
   implicit none
   type(tfield)     :: field
   field%all_nodata     = .false.
   field%dx             = nodata_default
   field%dy             = nodata_default
   field%x_spw_eye      = nodata_default
   field%y_spw_eye      = nodata_default
   field%p_drop_spw_eye = nodata_default
   nullify( field%arr1d )
   nullify( field%arr2d )
   nullify( field%arr3d )
end subroutine nullifyfield


subroutine nullifygrid( grid )
   implicit none
   type(tgrid) :: grid
   grid%num_columns = 0
   grid%num_rows = 0
   grid%mcur = 0
   grid%ncur = 0
   nullify( grid%mnref )
   nullify( grid%x )
   nullify( grid%y )
   grid%type     = ' '
   grid%filename = ' '
   grid%misval   = nodata_default
end subroutine nullifygrid


subroutine allocfield( field, m, n, k )
   implicit none
   type(tfield)     :: field
   integer          :: m
   integer          :: n
   integer          :: k
   integer          :: mm
   integer          :: nn
   integer          :: kk
   allocate( field % arr3d(1:m,1:n,1:k) )
   !
   ! Must be filled immediately
   !
   do kk = 1,k
      do nn = 1,n
         do mm = 1,m
            field%arr3d(mm,nn,kk) = 0
         enddo
      enddo
   enddo
   field%arr2d => field%arr3d(:, :, 1)
   field%arr1d => field%arr3d(1, 1, :)
end subroutine allocfield


subroutine allocgrid( grid, m, n, flowmmax, flownmax )
   implicit none
   type(tgrid)     :: grid
   integer         :: m
   integer         :: n
   integer         :: flowmmax
   integer         :: flownmax
   if (m > 0 .and. n>0) then
      allocate( grid%x(1:m,1:n) )
      allocate( grid%y(1:m,1:n) )
      grid%x = nodata_default
      grid%y = nodata_default
      if (flowmmax>0 .and. flownmax>0) then
         allocate( grid%mnref(1:flowmmax,1:flownmax,2) )
         grid%mnref = 0
      endif
   endif
end subroutine allocgrid


subroutine deallocfield( field )
   implicit none
   type(tfield)     :: field
   if(associated(field % arr3d)) deallocate( field % arr3d )
end subroutine deallocfield


subroutine deallocgrid( grid )
   implicit none
   type(tgrid)     :: grid
   if(associated(grid%mnref)) deallocate(grid%mnref)
   if(associated(grid%x))     deallocate(grid%x)
   if(associated(grid%y))     deallocate(grid%y)
end subroutine deallocgrid


subroutine deallocmeteo(runid)
   implicit none
   character(*), intent(in)  :: runid
   type(tmeteo), pointer     :: meteo     ! all meteo for one subdomain
   integer                   :: i
   integer                   :: istat

   meteodata_initialized = .false.
   call getmeteopointer(runid, meteo)
   if (associated(meteo) )then
       do i = 1, meteo%nummeteoitems
           call deallocfield( meteo%item(i)%ptr%field(0) )
           call deallocfield( meteo%item(i)%ptr%field(1) )
           if (associated(meteo%item(i)%ptr%grid)) then
              call deallocgrid ( meteo%item(i)%ptr%grid )
           endif
       enddo
       if (meteo%flowgrid%initialized) then
          if (associated(meteo%flowgrid%kcs))     deallocate (meteo%flowgrid%kcs, stat=istat)
          if (associated(meteo%flowgrid%xz))      deallocate (meteo%flowgrid%xz, stat=istat)
          if (associated(meteo%flowgrid%yz))      deallocate (meteo%flowgrid%yz, stat=istat)
       endif
       if (meteo%spiderweb%active) then
          if (associated(meteo%spiderweb%spwf))   deallocate (meteo%spiderweb%spwf, stat=istat)
          if (associated(meteo%spiderweb%spwarr)) deallocate (meteo%spiderweb%spwarr, stat=istat)
       endif
       ! Do not deallocate(meteo)
       ! meteo is a pointer to an element in a fixed, static, saved module array
   endif
end subroutine deallocmeteo


function createmeteoitem(runid, meteo, meteoitem) result(success)
   implicit none
   !
   ! Globals
   !
   logical                                :: success
   character(*)             , intent(in)  :: runid
   type(tmeteo)    , pointer              :: meteo     ! all meteo for one subdomain
   type(tmeteoitem), pointer              :: meteoitem
   !
   ! Locals
   !
   !
   ! Executable statements
   !
   call getmeteopointer(runid, meteo)
   if ( meteo%nummeteoitems < max_nummeteoitems ) then
      meteo%nummeteoitems = meteo%nummeteoitems + 1
      allocate(meteo%item(meteo%nummeteoitems)%ptr)
      meteoitem => meteo%item(meteo%nummeteoitems)%ptr
   else
      write(meteomessage,'(a,i4,a)') 'Meteo module can only handle ', max_nummeteoitems, &
                                  &  ' items per subdomain.'
      success = .false.
      return
   endif
   meteoitem%it0 = 0
   meteoitem%it1 = 1
   call nullifyfield( meteoitem%field(0) )
   call nullifyfield( meteoitem%field(1) )
   nullify(meteoitem%grid)
   meteoitem%initialized = .false.
   success               = .true.
end function createmeteoitem


subroutine meteoallocateitem(meteoitem, numm, numn, numk)
   implicit none
   !
   ! arguments
   !
   type(tmeteoitem), pointer    :: meteoitem
   integer          , intent(in):: numm       ! M, -sizes of item grid
   integer, optional, intent(in):: numn       ! N, -sizes of item grid
   integer, optional, intent(in):: numk       ! optional K-size (default 1)
   !
   ! locals
   !
   integer                      :: locnumn    ! optional N-size (default 1)
   integer                      :: locnumk    ! optional K-size (default 1)
   !
   ! body
   !
   ! Determine size in N, K-direction.
   !
   locnumn = 1
   if ( present(numn) ) then
       if ( numn > 0 ) then
           locnumn = numn
       endif
   endif
   locnumk = 1
   if ( present(numk) ) then
       if ( numk > 0 ) then
           locnumk = numk
       endif
   endif
   !
   ! Allocate data structure
   !
   call allocfield( meteoitem%field(0), numm, locnumn, locnumk )
   call allocfield( meteoitem%field(1), numm, locnumn, locnumk )
   meteoitem%numm = numm
   meteoitem%numn = locnumn
   meteoitem%numk = locnumk
end subroutine meteoallocateitem


function meteoallocateitemgrid(runid, meteoitem, m, n, gridfilnam, flowmmax, flownmax) result(success)
   implicit none
   !
   ! arguments
   !
   integer          , intent(out):: m          ! M, -sizes of item grid
   integer          , intent(out):: n          ! M, -sizes of item grid
   character(*)     , intent(in) :: runid
   character(*)     , intent(in) :: gridfilnam
   integer          , intent(in) :: flowmmax
   integer          , intent(in) :: flownmax
   logical                       :: success
   type(tmeteoitem)              :: meteoitem
   !
   ! locals
   !
   type(tgrid), pointer  :: grid
   integer               :: i
   integer               :: j
   integer               :: gridfil
   integer               :: pos
   character(256)        :: rec
   character(10)         :: dum
   logical               :: sferic
   logical               :: kw_found
   real(fp)              :: xymiss
   type(tmeteo), pointer :: meteo
   !
   ! body
   !
   call getmeteopointer(runid, meteo)
   !
   ! Check if there is another meteoItem with the same gridfile
   !
   do i=1,meteo%nummeteoitems
      !
      ! Skip the same meteoItem
      !
      if (  meteo%item(i)%ptr%quantities(1) == meteoitem%quantities(1) .and. &
          & meteo%item(i)%ptr%filetype == meteoitem%filetype                  ) cycle
      if (  associated(meteo%item(i)%ptr%grid) ) then
         if (meteo%item(i)%ptr%grid%filename == gridfilnam) then
            !
            ! Found another meteoItem with the same gridfile; pointering to that one
            !
            meteoitem%grid => meteo%item(i)%ptr%grid
            m       = meteoitem%grid%num_columns
            n       = meteoitem%grid%num_rows
            success = .true.
            return
         endif
      endif
   enddo
   !
   ! Creating a new (Tgrid type) grid inside meteo%item(meteoItem)
   !
   allocate(meteoitem%grid)
   call nullifygrid( meteoitem%grid )
   sferic   = .false.
   kw_found = .false.
   xymiss   = nodata_default
   grid     =>  meteoitem%grid
   success  = openexistingfile_meteo(gridfil,trim(gridfilnam),meteoitem%filetype)
   if (.not. success) return
    !
    ! The following part is copied (and adapted) from file:
    ! Delft3D-FLOW/libsrc/flow/inichk/rdrgf.f90
    !
    ! Read file, check for end of file or error in file:
    ! - The first line always contains comments
    !   sferic is true when the first line contains the keyword Spherical
    ! - Skip comment lines (starting with a '*'), while trying to read the
    !   following keywords: 'Coordinate System'
    !                       'Missing Value'
    !   If 'Coordinate System' is present, it overrules the sferic-specification
    !   in the first line!
    ! - The next line contains the dimensions mc and nc
    !   Parameter npart may also be on this line, but it is neglected
    ! - Read the next line containing three zero's
    !   xori, yori and alfori are not used anymore
    ! - Read x coordinates
    ! - Read y coordinates
    !
    read (gridfil, '(a)', end = 7777, err = 8888) rec
    if (index(rec, 'Spherical')>=1 .or. index(rec, 'SPHERICAL')>=1) then
       sferic = .true.
    endif
10  continue
       kw_found = .false.
       read(gridfil,'(a)',end = 7777,err=8888) rec
       if (rec(1:1) == '*') goto 10
       !
       pos = index(rec,'Coordinate System')
       if (pos >= 1) then
          kw_found = .true.
          if (index(rec(pos+1:),'spherical') >= 1 .or. &
            & index(rec(pos+1:),'Spherical') >= 1 .or. &
            & index(rec(pos+1:),'SPHERICAL') >= 1       ) then
             sferic = .true.
          else
             sferic = .false.
          endif
       endif 
       !
       pos = index(rec,'Missing Value')
       if (pos >= 1) then
          kw_found = .true.
          pos      = index(rec,'=') + 1
          read(rec(pos:),*,err=8888) xymiss
       endif
    if (kw_found) goto 10
    !
    read(rec,*,err=8888)  m,n
    !
    ! read three zero's
    !
    read(gridfil,'(a)',end = 7777,err=8888) rec
    !
    ! Put parameters read in grid-structure
    !
    if (sferic) then
       !call meteosetsferic()
       grid%type = 'sferic'
    else
       grid%type = 'cartesian'
    endif
    grid%misval = xymiss
    call allocgrid( grid, m, n, flowmmax, flownmax )
    grid%num_columns = m
    grid%num_rows = n
    grid%filename = gridfilnam
    !
    ! read XD
    ! read unformatted: The number of digits of xcor may vary
    !
    do j = 1, n
       read (gridfil, *, end = 7777, err = 8888) dum,dum,(grid%x(i, j),i=1, m)
    enddo
    !
    ! read YD
    ! read unformatted: The number of digits of ycor may vary
    !
    do j = 1, n
       read (gridfil, *, end = 7777, err = 8888) dum,dum,(grid%y(i, j),i=1, m)
    enddo
    close (gridfil)
    success = .true.
    return
    !
    ! error handling
    !
  7777 continue
    write(meteomessage,'(2a)') 'Unexpected end of file in ',trim(gridfilnam)
    success = .false.
    return
  8888 continue
    write(meteomessage,'(2a)') 'Error while reading in file ',trim(gridfilnam)
    success = .false.
    return
end function meteoallocateitemgrid


function addnewsubdomainmeteopointer(subdomname, subdommeteo) result(success)
   implicit none
   !
   ! arguments
   !
   logical                :: success
   character(*)           :: subdomname
   type(tmeteo),target    :: subdommeteo
!
!! executable statements -------------------------------------------------------
! 
    if ( num_meteopointers < max_num_meteopointers ) then
        num_meteopointers = num_meteopointers + 1
        meteopointers(num_meteopointers) % subdomname = subdomname
        meteopointers(num_meteopointers) % pntr => subdommeteo
    else
        write(meteomessage,'(a,i4,a)') 'Meteo module can only handle ',max_num_meteopointers, &
                                    &  ' subdomains.'
        success = .false.
        return
    endif
    success = .true.
end function addnewsubdomainmeteopointer

subroutine getmeteopointer(subdomname, meteopointer)
   implicit none
   type(tmeteo), pointer               :: meteopointer
   character(*)         , intent(in)   ::  subdomname
   !
   ! locals
   !
   integer :: isubdom ! subdomain counter
!
!! executable statements -------------------------------------------------------
! 
   nullify(meteopointer)
   do isubdom = 1, num_meteopointers
      if ( subdomname == meteopointers(isubdom) % subdomname ) then
         meteopointer => meteopointers(isubdom) % pntr
         return
      endif
   enddo
end subroutine getmeteopointer

subroutine getmeteoitempointer(runid, id, filetype, meteoitempointer)
   implicit none
   integer                  , intent(in)  ::  filetype
   character(*)             , intent(in)  ::  id
   character(*)             , intent(in)  ::  runid
   type(tmeteoitem), pointer              ::  meteoitempointer
   !
   ! locals
   !
   integer               :: i
   type(tmeteo), pointer :: meteo
!
!! executable statements -------------------------------------------------------
! 
   nullify(meteoitempointer)
   call getmeteopointer(runid, meteo)
   if ( .not. associated(meteo) ) then
      return
   endif
   do i = 1, meteo%nummeteoitems
      if (  meteo%item(i)%ptr%quantities(1) == id      .and. &
          & meteo%item(i)%ptr%filetype      == filetype       ) then
         meteoitempointer => meteo%item(i)%ptr
         return
      endif
   enddo
end subroutine getmeteoitempointer

function getmeteomessage( ) result(retval)
   character(500) :: retval
   retval = meteomessage
   meteomessage = ' '
end function getmeteomessage

subroutine meteoblockint()
   implicit none
   meteoint = .false.
end subroutine meteoblockint

function openexistingfile_meteo(minp, filename, meteotype) result(success)

implicit none
!
! Global variables
!
    integer         :: minp
    integer         :: meteotype
    logical         :: success
    character(*)    :: filename
!
! Local variables
!
    integer :: ierr
!
!! executable statements -------------------------------------------------------
!
    if (len_trim(filename) == 0) then
       write (meteomessage, '(a,i0)') 'While opening meteo file: name is empty, for meteotype = ', meteotype
       success = .false.
       return
    endif
    inquire (file = trim(filename), exist = success)
    if (.not. success) then
       write(meteomessage,'(3a)') 'Meteo file ',trim(filename),' does not exist'
       success = .false.
       return
    endif
    open (newunit=minp, file = trim(filename), action = 'READ', iostat=ierr)
    if (ierr == 0) success = .true.
end function openexistingfile_meteo


end module meteo_data
