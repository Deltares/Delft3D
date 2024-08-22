module meteo
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
! Read time series in five possible formats:
! uniform                     : Delft3D-FLOW format: time, uniform windspeed, direction and pressure
! meteo_on_computational_grid : Space varying wind and pressure on the computational grid: time and fields of patm, windu, windv
!                               on the computational (m,n) grid
! field_on_computational_grid : same as meteo_on_computational_grid but more general
! meteo_on_equidistant_grid   : time and fields on on equidistant grid
! meteo_on_spiderweb_grid     : time and fields of patm, windspeed, wind_from_direction op spiderweb grid
! meteo_on_curvilinear_grid   : time and fields on own curvilinear grid
!
! Main calls from Delft3D-FLOW:
! readmd -> rdmeteo:
!             initmeteo        : allocate meteo structure for this domain
!             addmeteoitem     : allocate and initialized an input quantity
!                                with specified format
!             checkmeteo       : check whether input is available for the complete
!                                time interval
! trisol  -> incmeteo:
!             meteoupdate      : prepare meteo data for the current time
!             getmeteoval      : return meteo data for the current time and position
!                                use optional m and n parameters to speed up in case of meteo_on_curvilinear_grid
!             getspiderval     : same as getmeteoval for spiderweb data
!
! gdp_dealloc:
!             deallocmeteo
!
! Additional calls:
!    getmeteomessage   : returns a string containing a(n) (error) message
!
!!--pseudo code and references--------------------------------------------------
!
! Stef.Hummel@deltares.nl
! Herman.Kernkamp@deltares.nl
! Adri.Mourits@deltares.nl
!
!!--declarations----------------------------------------------------------------
   use precision
   use meteo_data


   real(fp), private :: pi
   real(fp), private :: d2r

contains
!
!
!===============================================================================
function initmeteo(runid) result(success)
   implicit none
   logical               :: success
   type(tmeteo), pointer :: meteopointer  ! All Meteo for one subdomain
   character(*)          :: runid
   !
   ! body
   if (.not. meteodata_initialized) then
      call init_meteo_data
      meteodata_initialized = .true.
   endif
   allocate(meteopointer)
   meteopointer%nummeteoitems        = 0
   meteopointer%spiderweb%active     = .false.
   nullify(meteopointer%spiderweb%spwf)
   nullify(meteopointer%spiderweb%spwarr)
   meteopointer%flowgrid%initialized = .false.
   success                           = addnewsubdomainmeteopointer(runid, meteopointer)
   pi                                = acos(-1.0_fp)
   d2r                               = pi/180.0_fp
end function initmeteo
!
!
!===============================================================================
function gridtometeo(runid, num_rows, num_columns, &
                   & nlb  , nub , mlb , mub, &
                   & kcs  , xz  , yz  ) result(success)
   implicit none
   !
   logical :: success
   !
   character(*)                         , intent(in)  :: runid
   integer                              , intent(in)  :: num_rows
   integer                              , intent(in)  :: num_columns
   integer                              , intent(in)  :: nlb
   integer                              , intent(in)  :: nub
   integer                              , intent(in)  :: mlb
   integer                              , intent(in)  :: mub
   integer,  dimension(nlb:nub, mlb:mub), intent(in)  :: kcs
   real(fp), dimension(nlb:nub, mlb:mub), intent(in)  :: xz
   real(fp), dimension(nlb:nub, mlb:mub), intent(in)  :: yz
   !
   integer               :: m
   integer               :: n
   type(tmeteo), pointer :: meteo  ! All Meteo for one subdomain
   !
   ! always return success = true
   !
   success = .true.
   call getmeteopointer(runid, meteo)
   if ( .not. associated(meteo) ) then
      return
   endif
   if (num_rows==0 .or. num_columns==0) then
      nullify(meteo%flowgrid%kcs)
      nullify(meteo%flowgrid%xz)
      nullify(meteo%flowgrid%yz)
      return
   endif
   meteo%flowgrid%num_rows = num_rows
   meteo%flowgrid%num_columns = num_columns
   allocate(meteo%flowgrid%kcs(num_rows,num_columns))
   allocate(meteo%flowgrid%xz (num_rows,num_columns))
   allocate(meteo%flowgrid%yz (num_rows,num_columns))
   meteo%flowgrid%initialized = .true.
   do m=1, num_columns
      do n=1, num_rows
         meteo%flowgrid%kcs(n,m) = kcs(n,m)
         meteo%flowgrid%xz (n,m) = xz(n,m)
         meteo%flowgrid%yz (n,m) = yz(n,m)
      enddo
   enddo
   if (meteo%spiderweb%active) then
      allocate(meteo%spiderweb%spwf  (  meteo%flowgrid%num_rows,meteo%flowgrid%num_columns))
      allocate(meteo%spiderweb%spwarr(3,meteo%flowgrid%num_rows,meteo%flowgrid%num_columns))
   endif
end function gridtometeo
!
!
!===============================================================================
function addmeteoitem(runid, inputfile, gridsferic, num_columns, num_rows) result(success)
    !
    ! return value
    !
    logical :: success
    !
    ! arguments
    !
    integer      , optional , intent(in) :: num_columns
    integer      , optional , intent(in) :: num_rows
    character(*)            , intent(in) :: inputfile       ! filename for meteo data file
    character(*)            , intent(in) :: runid           ! runid for current domain
    logical      , optional , intent(in) :: gridsferic      ! type of flow/wave grid sferic (.true.) or cartesian (.false.)?
    !
    ! locals
    !
    integer                      :: i0
    integer                      :: i1
    integer                      :: kxr
    integer                      :: mxr
    integer                      :: nxr
    integer                      :: minp
    logical                      :: meteogridsferic    ! type of meteo grid sferic (.true.) or cartesian (.false.)?
    logical, external            :: readmeteoheader
    logical, external            :: checkmeteoheader
    type(tmeteo)    , pointer    :: meteo
    type(tmeteoitem), pointer    :: meteoitem
    !
    ! body
    !
    if ( inputfile == '') then
       meteomessage = 'addmeteoitem: Input file not found'
       success = .false.
       return
    endif
    !
    ! create a new meteo item
    !
    success = createmeteoitem(runid, meteo, meteoitem)
    if (.not. success) return
    !
    ! Open meteofile
    !
    meteoitem%filetype = 0
    success = openexistingfile_meteo(minp, inputfile, meteoitem%filetype)
    if (.not. success) return
    !
    meteoitem%filename = inputfile
    !
    ! Read the header of the file to find out the type of input
    !
    success = readmeteoheader (minp, meteoitem)
    if (.not. success) return
    !
    ! Determine filetype of meteofile
    !
    select case(meteoitem%meteotype)
        case ('uniuvp')
           meteoitem%filetype = 1
        case ('meteo_on_computational_grid')
           meteoitem%filetype = 2
        case ('meteo_on_equidistant_grid')
           meteoitem%filetype = 3
        case ('meteo_on_spiderweb_grid')
           meteoitem%filetype = 4
        case ('meteo_on_curvilinear_grid')
           meteoitem%filetype = 5
        case ('field_on_computational_grid')
           meteoitem%meteotype = 'meteo_on_computational_grid'
           meteoitem%filetype = 2
    end select
    !
    kxr = 3
    mxr = 1
    nxr = 1
    !
    select case (meteoitem%filetype)
    case ( uniuvp )
       kxr = 3
    case ( meteo_on_computational_grid )
       mxr     = num_rows
       nxr     = num_columns
       kxr     = meteoitem%n_quantity
    case ( meteo_on_equidistant_grid )
       mxr = meteoitem%n_cols
       nxr = meteoitem%n_rows
       kxr = 1
    case ( meteo_on_curvilinear_grid )
       success = meteoallocateitemgrid(runid, meteoitem, mxr, nxr, meteoitem%grid_file, num_columns, num_rows)
       if (.not. success) return
       kxr = 1
    case ( meteo_on_spiderweb_grid )
       kxr                    = 3
       !
       ! One extra colum for 360 deg = 0 deg, for interpolation
       !
       meteoitem%n_cols = meteoitem%n_cols + 1
       !
       ! One extra row for the points on radius = 0
       !
       meteoitem%n_rows = meteoitem%n_rows + 1
       !       
       mxr                    = meteoitem%n_cols
       nxr                    = meteoitem%n_rows
       meteo%spiderweb%active = .true.
       meteo%spiderweb%time   = nodata_default
    end select
    !
    ! Check the validity of the parameters in the header of the meteo file
    ! This must be done after function meteoallocateitemgrid
    !
    success = checkmeteoheader(meteoitem)
    if (.not. success) return

    call meteoallocateitem(meteoitem, mxr, nxr, kxr)
    i1                        = meteoitem%it1
    i0                        = meteoitem%it0
    meteoitem%field(i0)%time  = nodata_default
    meteoitem%field(i1)%time  = nodata_default
    meteoitem%field(i0)%dx    = meteoitem%dx
    meteoitem%field(i0)%dy    = meteoitem%dy
    meteoitem%field(i1)%dx    = meteoitem%dx
    meteoitem%field(i1)%dy    = meteoitem%dy
    meteoitem%lun             = minp
    meteoitem%initialized     = .true.
    meteoitem%firstcopy       = .false.
    success                   = .true.
    !
    ! Determine whether meteogrid and flow grid are of the same type: spherical or cartesian
    !
    if (meteoitem%filetype == meteo_on_curvilinear_grid) then
       if (meteoitem%grid%type == 'sferic') then
          meteogridsferic = .true.
          msferic         = .true.
       elseif (meteoitem%grid%type == 'cartesian') then
          meteogridsferic = .false.
       endif
    else
       if (meteoitem%grid_unit == 'degree') then
          meteogridsferic = .true.
          msferic         = .true.
       elseif (meteoitem%grid_unit == 'm') then
          meteogridsferic = .false.
       endif
    endif
    !
    ! Only for space varying meteo input on a separate grid
    !
    if (meteoitem%filetype /= uniuvp .and. meteoitem%filetype /= meteo_on_computational_grid) then
       if (meteogridsferic .neqv. gridsferic) then
          success = .false.
          meteomessage = 'Meteo grid and hydrodynamic grid must be of the same type: Cartesian or spherical'
          return
       endif
    endif
    !
end function addmeteoitem
!
!
!===============================================================================
function checkmeteo(runid, flow_itdate, flow_tzone, tstop) result(success)
   implicit none
   !
   ! Globals
   !
   integer     , intent(in) :: flow_itdate
   real(fp)    , intent(in) :: flow_tzone   
   real(fp)    , intent(in) :: tstop
   logical                  :: success
   character(*), intent(in) :: runid
   !
   ! Locals
   !
   integer                    :: i
   type(tmeteo)    , pointer  :: meteo       ! all meteo for one subdomain
   type(tmeteoitem), pointer  :: meteoitem
   !
   ! Executable statements
   !   
   success = meteoupdate(runid, flow_itdate, flow_tzone, tstop)
   if (.not. success) then
      return
   endif
   !
   ! rewind input files
   ! set times read at nodata_default to force rereading the first fields
   !
   call getmeteopointer(runid, meteo)
   do i = 1, meteo%nummeteoitems
      meteoitem => meteo%item(i)%ptr
      rewind (meteoitem%lun)
      meteoitem%field(meteoitem%it0)%time = nodata_default
      meteoitem%field(meteoitem%it1)%time = nodata_default
   enddo
end function checkmeteo
!
!
!===============================================================================
function meteoupdate(runid, flow_itdate, flow_tzone, time) result(success)
   implicit none
   !
   ! Globals
   !
   integer     , intent(in) :: flow_itdate
   real(fp)    , intent(in) :: flow_tzone   
   real(fp)    , intent(in) :: time
   character(*), intent(in) :: runid
   logical                  :: success
   !
   ! Locals
   !
   integer               :: i
   type(tmeteo), pointer :: meteo  ! all meteo for one subdomain
   !
   ! Executable statements
   !
   success = .true.
   call getmeteopointer(runid, meteo)
   if ( .not. associated(meteo) ) then
      write(meteomessage,'(2a)') 'meteoupdate: Unable to find meteo structure related to runid ',trim(runid)
      success = .false.
      return
   endif
   do i = 1, meteo%nummeteoitems
      success = meteoupdateitem(meteo%item(i)%ptr, flow_itdate, flow_tzone, time)
      if (.not. success) return
   enddo
end function meteoupdate
!
!
!===============================================================================
function meteoupdateitem(meteoitem, flow_itdate, flow_tzone, tim) result(success)
   !
   ! Update information of an item in the meteo module.
   ! If item = Uniform, directly perform transformation
   !
   use meteo_read
   implicit none
   !
   ! return value
   !
   logical :: success
   !
   ! arguments
   !
   integer         , intent(in) :: flow_itdate
   real(fp)        , intent(in) :: flow_tzone   
   real(fp)        , intent(in) :: tim       ! flow time to update to
   type(tmeteoitem), pointer    :: meteoitem
   !
   ! locals
   !
   integer                             :: m
   integer                             :: mx
   integer                             :: n
   integer                             :: grid_width
   integer                             :: kx
   integer                             :: k
   integer                             :: minp
   integer                             :: it1
   logical                             :: all_nodata
   real(fp)                            :: tread
   real(hp), dimension(:),     pointer :: uz     ! 1-dim array
   real(hp), dimension(:,:),   pointer :: vz     ! 2-dim array
   real(hp), dimension(:,:,:), pointer :: wz     ! 3-dim array
   real(fp)                            :: x_spw_eye
   real(fp)                            :: y_spw_eye
   !
   ! body
   !
   if ( .not. meteoitem%initialized ) then
      meteomessage = 'meteoupdateitem: meteoitem not initialized'
      success = .false.
      return
   endif
   
   ! first check if current time is not before meteo time interval.
   ! If not, reset the whole meteo!
   ! alternatively, we can make it more subtle by decreasing meteoitem(it0)%time,
   ! but then we have to rewind the file anyway.
    
   if (comparereal(real(meteoitem%field(meteoitem%it0)%time,fp), tim) == 1) then
   
   !    print *,'WARNING: current time ',tim,'before start of meteo data time interval: ', &
   !        meteoitem%field(meteoitem%it0)%time
   !    print *,'probably because of a change of instance? In any case, we reset the meteo. '  

       rewind (meteoitem%lun)
       meteoitem%field(meteoitem%it0)%time = nodata_default
       meteoitem%field(meteoitem%it1)%time = nodata_default

   endif
   
   
   !
   do
      !
      ! stop reading data when (tim <= it1%time) and (it0 and it1 are not empty)
      ! it%time is hp, comparereal expects fp
      !
      if (  comparereal(tim            , real(meteoitem%field(meteoitem%it1)%time,fp)) /= 1 .and. &
          & comparereal(nodata_default , real(meteoitem%field(meteoitem%it0)%time,fp)) /= 0 .and. &
          & comparereal(nodata_default , real(meteoitem%field(meteoitem%it1)%time,fp)) /= 0        ) exit
      call meteoswapitem(meteoitem)
      it1          = meteoitem%it1
      minp         = meteoitem%lun
      mx           = meteoitem%numm
      grid_width           = meteoitem%numn
      kx           = meteoitem%numk
      !
      ! Read the time field in the meteo file
      ! For uniform wind nothing is done in readtime
      !
      success = readtime(minp, meteoitem, flow_itdate, flow_tzone, tread)
      if (.not. success) then
         return
      endif
      !
      select case (meteoitem%filetype)
         case ( uniuvp )
            !
            uz      => meteoitem%field(it1)%arr1d
            success =  readseries(minp,uz,kx-1,tread)
            if (.not. success) then
               return
            endif
            !
            ! Do not convert from angle/magnitude to u/v here
            ! Time interpolation has to be done first
            !
         case ( meteo_on_computational_grid )
            !
            wz      => meteoitem%field(it1)%arr3d
            success =  read_spv_block(minp, meteoitem, wz, mx, grid_width, kx)
            ! 
            do k = 1,kx
               !
               ! Conversion of pressure to Pa (N/m2). If already Pa, p_conv = 1.0_hp 
               !
               if (meteoitem%quantities(k) == 'air_pressure' .or. meteoitem%quantities(k) == 'patm'.or. meteoitem%quantities(k) == 'p_drop') then
                  do m = 1, size(wz,2)
                     do n = 1, size(wz,1)
                        wz(n,m,k) = wz(n,m,k) * meteoitem%p_conv
                     enddo
                  enddo
               endif 
            end do    
            !
            if (.not. success) then
               return
            endif
            !
         case ( meteo_on_equidistant_grid )
            !
            vz      => meteoitem%field(it1)%arr2d
            success =  read_equidistant_block(minp, meteoitem, vz, mx, grid_width)
            if (.not. success) then
               return
            endif
            !
         case ( meteo_on_curvilinear_grid )
            !
            vz      => meteoitem%field(it1)%arr2d            
            success =  read_curvilinear_block(minp, vz, meteoitem)
            if (.not. success) then
               return
            endif
            !
         case ( meteo_on_spiderweb_grid )
            !
            wz      => meteoitem%field(it1)%arr3d
            success =  read_spiderweb_block(minp, wz, mx, grid_width, meteoitem, x_spw_eye, y_spw_eye, all_nodata)
            if (.not. success) then
               return
            endif
            meteoitem%field(it1)%x_spw_eye  = x_spw_eye
            meteoitem%field(it1)%y_spw_eye  = y_spw_eye            
            meteoitem%field(it1)%all_nodata = all_nodata
            !
      end select
      !
      meteoitem%field(it1)%time  = tread
      !
   enddo
   !
   ! it%time is hp, comparereal expects fp
   !
   if ( comparereal(real(meteoitem%field(meteoitem%it0)%time,fp), tim) == 1 ) then
      write(meteomessage,'(3a,2(g16.8,a))') 'In file ',trim(meteoitem%filename), &
           & ': Start time of forcing data (',meteoitem%field(meteoitem%it0)%time,') should precede start time of simulation (',tim,')'
      success = .false.
   else
      !meteomessage = ' '
      success      = .true.
   endif
end function meteoupdateitem
!
!
!===============================================================================
subroutine meteoswapitem(meteoitem)
   implicit none
   type(tmeteoitem) :: meteoitem
   meteoitem%it0 =     meteoitem%it1
   meteoitem%it1 = 1 - meteoitem%it0
end subroutine meteoswapitem
!
!
!===============================================================================
function setmeteodefault(quantity, gapres) result(success)
   implicit none
!
! Return value
!
   logical :: success
!
! Global variables
!
   real(fp)              , intent(in)  :: gapres    ! Global atmospheric pressure specified and/or read from MD-file in rdproc.f90 for Delft3D-FLOW
   character(*)          , intent(in)  :: quantity
!
! Local variables
!
   integer :: i
   integer :: j
!
!! executable statements -------------------------------------------------------
!
   if (quantity == 'patm') then
      patm_default = gapres
      success = .true.
      !
      ! patm_default is copied to meteoitem%pref when pref_option=use_from_kernel
      ! Normally, setmeteodefault('patm', value) is called after meteoitem%pref has been set
      ! This means that pref has to be reset in all meteoitemscase using this flag
      !
      do i = 1, num_meteopointers
         do j = 1, meteopointers(i)%pntr%nummeteoitems
             if (meteopointers(i)%pntr%item(j)%ptr%pref_option == opt_use_from_kernel) then
                meteopointers(i)%pntr%item(j)%ptr%pref = patm_default
             endif
         enddo
      enddo
   else
      write(meteomessage,'(2a)') 'Setmeteodefault: Incorrect quantity found: ',trim(quantity)
      success = .false.
   endif
   !   
end function setmeteodefault
!
!
!===============================================================================
function getmeteotypes(runid, meteotypes, mtdim) result(success)
   use m_alloc
   implicit none
!
! Return value
!
   logical :: success
!
! Global variables
!
   character(*)                         , intent(in)   :: runid
   character(256), dimension(:), pointer, intent(out)  :: meteotypes
   integer, optional                                   :: mtdim
!
! Local variables
!
   integer                             :: dimmeteotypes
   integer                             :: i
   integer                             :: j
   integer                             :: ierr
   logical                             :: newtype
   character(256)                      :: curtype
   type(tmeteo)              , pointer :: meteo     ! all meteo for one subdomain
!
!! executable statements -------------------------------------------------------
!
   success = .true.
   call getmeteopointer(runid, meteo)
   if ( .not. associated(meteo) ) then
      success = .false.
      return
   endif
   if (present(mtdim)) then
      dimmeteotypes = mtdim
   else
      if (associated(meteotypes)) then
         dimmeteotypes = size(meteotypes)
      else
         dimmeteotypes = 0
      endif
   endif
   ierr = 0
   do i = 1, meteo%nummeteoitems
      curtype = meteo%item(i)%ptr%meteotype
      newtype = .true.
      do j = 1, dimmeteotypes
         if (meteotypes(j) == curtype) then
            newtype = .false.
            exit
         endif
      enddo
      if (newtype) then
         dimmeteotypes = dimmeteotypes + 1
         call reallocP(meteotypes, dimmeteotypes, stat=ierr, keepExisting=.true.)
         meteotypes(dimmeteotypes) = curtype
      endif
   enddo
   if (ierr /= 0) then
      success = .false.
   endif
   if (present(mtdim)) then
      mtdim = dimmeteotypes
   endif
end function getmeteotypes
!
!
!===============================================================================
function getmeteoquantities(runid, meteoquantities) result(success)
   use m_alloc
   implicit none
!
! Return value
!
   logical :: success
!
! Global variables
!
   character(*) , intent(in)  :: runid
   character(60), dimension(:), allocatable, intent(out)  :: meteoquantities
!
! Local variables
!
   integer                             :: dimmeteoquantities
   integer                             :: i
   integer                             :: j
   integer                             :: k
   integer                             :: ierr
   logical                             :: newquant
   character(60)                       :: curquant
   type(tmeteo)              , pointer :: meteo     ! all meteo for one subdomain
   type(tmeteoitem)          , pointer :: meteoitem
!
!! executable statements -------------------------------------------------------
!
   success = .true.
   call getmeteopointer(runid, meteo)
   if ( .not. associated(meteo) ) then
      success = .false.
      return
   endif
   ierr = 0
   do i = 1, meteo%nummeteoitems
      do k = 1, 3
         curquant = meteo%item(i)%ptr%quantities(k)
         if (curquant == ' ') cycle
         newquant = .true.
         if (allocated(meteoquantities)) then
            dimmeteoquantities = size(meteoquantities)
         else
            dimmeteoquantities = 0
         endif
         do j = 1, dimmeteoquantities
            if (meteoquantities(j) == curquant) then
               newquant = .false.
               exit
            endif
         enddo
         if (newquant) then
            call realloc(meteoquantities, size(meteoquantities)+1, stat=ierr, keepExisting=.true.)
            meteoquantities(size(meteoquantities)) = curquant
         endif
      enddo
   enddo
   if (ierr /= 0) then
      success = .false.
   endif
end function getmeteoquantities
!
!
!===============================================================================
function getmeteoval(runid, quantity, time, mfg, nfg, & 
                   & nlb, nub, mlb, mub, qarray, grid_size, gridnoise) result(success)
   !
   ! Function description:
   ! Put quantity values for given time for complete grid in qarray
   ! Spiderweb input is added to possible other types of meteo input
   !
   !
   ! Note that size(qarray) need not to be equal to the size of the
   ! meteo%flowgrid. When running in parallel, qarray is defined for a local 
   ! domain while the meteo information is always stored globally.
   !
   implicit none
!
! Return value
!
   logical :: success
!
! Global variables
!
   integer                                          , intent(in)  :: nlb
   integer                                          , intent(in)  :: nub
   integer                                          , intent(in)  :: mlb
   integer                                          , intent(in)  :: mub
   integer                                          , intent(in)  :: mfg !offset in global domain.
   integer                                          , intent(in)  :: nfg !offset in global domain.
   real(fp)                                         , intent(in)  :: time
   real(fp)    , dimension(nlb:nub, mlb:mub), target, intent(out) :: qarray
   character(*)                                     , intent(in)  :: quantity
   character(*)                                     , intent(in)  :: runid
   integer                                          , intent(in)  :: grid_size
   real(fp), dimension(grid_size,3),optional        , intent(in)  :: gridnoise
!
! Local variables
!
   integer                               :: dir
   integer                               :: i
   integer, dimension(4)                 :: ind
   integer                               :: iq
   integer                               :: k
   integer                               :: mx
   integer                               :: grid_width
   integer                               :: kx
   integer                               :: it1
   integer                               :: it0
   integer                               :: ierr
   integer                               :: m
   integer                               :: m_mg     ! loop variable on meteo grid
   integer                               :: n
   integer                               :: n_mg     ! loop variable on meteo grid
   integer                               :: i1
   integer                               :: j1
   integer                               :: mv
   integer                               :: nv
   logical                               :: nodata0
   logical                               :: nodata1
   real(fp)                              :: alpha
   real(fp)                              :: unival
   real(fp)                              :: t1
   real(fp)                              :: t0
   real(fp)                              :: a1
   real(fp)                              :: a0
   real(fp)                              :: fm
   real(fp)                              :: fm0
   real(fp)                              :: rcycl
   real(fp)                              :: x
   real(hp)                              :: x_hp
   real(hp)                              :: x_mg    ! x-location on meteo grid
   real(fp)                              :: y
   real(hp)                              :: y_hp
   real(hp)                              :: y_mg    ! y-location on meteo grid
   real(fp)                              :: xx
   real(fp)                              :: yy
   real(fp)                              :: x00_eye
   real(fp)                              :: y00_eye
   real(fp)                              :: x01
   real(fp)                              :: y01
   real(fp)                              :: x01_eye
   real(fp)                              :: y01_eye
   real(fp)                              :: dx
   real(fp)                              :: dy
   real(fp)                              :: dx1
   real(fp)                              :: dy1
   real(fp)                              :: x1
   real(fp)                              :: y1
   real(fp)                              :: di1
   real(fp)                              :: dj1
   real(fp)                              :: vv0
   real(fp)                              :: vv1
   real(fp)                              :: wmag
   real(fp)                              :: wdir
   real(fp)                              :: wdir0
   real(fp)                              :: wdir1
   real(fp), dimension(3)                :: z         ! spiderweb wind_speed (1), wind_from_direction (2) and air_pressure (3)
   real(fp), dimension(4)                :: f
   real(fp), dimension(4)                :: u
   real(fp), dimension(4)                :: v
   real(hp), dimension(4)                :: f_hp
   real(hp), dimension(4)                :: u_hp
   real(hp), dimension(4)                :: v_hp
   real(hp), dimension(4)                :: w         ! weighing factors
   real(hp), dimension(:,:,:), pointer   :: v1
   real(hp), dimension(:,:,:), pointer   :: v0        ! 3-dim array
   real(hp), dimension(:)    , pointer   :: u1
   real(hp), dimension(:)    , pointer   :: u0        ! 1-dim array
   real(fp), dimension(:,:), allocatable :: meteo_noise
   character(20)                         :: tex
   type(tmeteo)              , pointer   :: meteo     ! all meteo for one subdomain
   type(tmeteoitem)          , pointer   :: meteoitem
   type(tgrid)               , pointer   :: grid
   type(tspiderweb)          , pointer   :: spw
!
!! executable statements -------------------------------------------------------
!
   call getmeteopointer(runid, meteo)
   if ( .not. associated(meteo) ) then
      success = .false.
      return
   endif
   ierr = 0
   if (quantity == 'patm') then
      !
      ! Set qarray to patm_default for all n and m
      !
      qarray = patm_default
   else
      !
      ! Set qarray to zero for all n and m
      !
      qarray = 0.0_fp
   endif
   if (meteo%spiderweb%active) then
      spw => meteo%spiderweb
   else
      nullify(spw)
   endif
   !
   ! Process all meteo items that have an addition to the current quantity
   !
   do i = 1, meteo%nummeteoitems
      do iq = 1, 3
         if (meteo%item(i)%ptr%quantities(iq) == quantity) then
            meteoitem => meteo%item(i)%ptr
            mx  =  meteoitem%numm
            grid_width  =  meteoitem%numn
            kx  =  meteoitem%numk
            it1 =  meteoitem%it1
            it0 =  meteoitem%it0
            t1  =  meteoitem%field(it1)%time
            t0  =  meteoitem%field(it0)%time
            !
            ! time weight factor
            !
            if (.not. meteoint) then
               a1 = 0.0_fp
            elseif (t1 == t0) then
               a1 = 1.0_fp
            else
               a1 = (time-t0) / (t1-t0)
            endif
            a0 = 1.0_fp - a1

            select case (meteoitem%filetype)
            case ( uniuvp )
               !
               ! arr1d contains wind magnitude and direction only
               ! first interpolate in time, then convert to u/v
               !
               u1  => meteoitem%field(it1)%arr1d
               u0  => meteoitem%field(it0)%arr1d
               wmag  = real(a0,hp)*u0(1) + real(a1,hp)*u1(1)
               wdir0 = real(u0(2),fp)
               wdir1 = real(u1(2),fp)
               !
               ! Be careful with interpolation of angles
               !
               if (comparereal( abs(wdir0-wdir1), 180.0_fp ) == 1) then
                  if (comparereal( wdir0, wdir1 ) == -1) then
                     wdir0 = wdir0 + 360.0_fp
                  else
                     wdir1 = wdir1 + 360.0_fp
                  endif
               endif
               wdir = a0*wdir0 + a1*wdir1
               if (comparereal( wdir, 360.0_fp ) == 1) then
                  wdir = wdir - 360.0_fp
               endif
               !
               ! nautical convention
               !
               wdir   = (270.0_fp-wdir) * d2r
               select case (quantity)
               case ( 'windu' )
                  unival = real(wmag,fp) * cos(wdir)
               case ( 'windv' )
                  unival = real(wmag,fp) * sin(wdir)
               case ( 'patm'  )
                  !
                  ! qarray is already filled with patm_default
                  !
               case default
                  meteomessage = 'Uniuvp can only be used for windu and windv'
                  success = .false.
                  return
               end select
               if (quantity == 'windu' .or. quantity == 'windv') then
                  !
                  ! Fill the whole qarray with unival
                  !
                  qarray = unival
               endif
            case ( meteo_on_computational_grid )
               v1  => meteoitem%field(it1)%arr3d
               v0  => meteoitem%field(it0)%arr3d
               select case (quantity)
               case ( 'windu' )
                  k = 1
               case ( 'windv' )
                  k = 2
               case ( 'patm'  )
                  k = 3
               case ( 'sdu' )
                  k = 1
               case default
                  meteomessage = 'field_on_computational_grid can only be used for windu, windv and patm or uplift_rate_reference_elevation'
                  success = .false.
                  return
               end select
               !
               ! Note: This loop will not fill the boundaries of qarray. 
               !
               do m = 1, meteo%flowgrid%num_columns
                  do n = 1, meteo%flowgrid%num_rows
                     qarray(n,m) = a0*v0(n+nfg-1,m+mfg-1,k) + a1*v1(n+nfg-1,m+mfg-1,k)
                  enddo
               enddo
            case ( meteo_on_equidistant_grid )
               v1  => meteoitem%field(it1)%arr3d
               v0  => meteoitem%field(it0)%arr3d
               !
               ! spatial coordinates
               !
               x01 =  meteoitem%x_llcenter
               y01 =  meteoitem%y_llcenter
               dx1 =  meteoitem%field(it1)%dx
               dy1 =  meteoitem%field(it1)%dy
               do m = 1,meteo%flowgrid%num_columns
                  do n = 1,meteo%flowgrid%num_rows
                     !
                     ! Check on kcs > 0 to avoid work in the halo regions when running parallel
                     !
                     if (meteo%flowgrid%kcs(n,m) > 0) then
                        x1  = (meteo%flowgrid%xz(n,m) - x01) / dx1
                        y1  = (meteo%flowgrid%yz(n,m) - y01) / dy1
                        i1  =  1 + int(x1)
                        j1  =  1 + int(y1)
                        di1 =  1.0_fp + x1 - real(i1,fp)
                        dj1 =  1.0_fp + y1 - real(j1,fp)
                        if (i1 == mx) then
                           i1  = i1 - 1
                           di1 = 1.0_fp
                        endif
                        if (j1 == grid_width) then
                           j1  = j1 - 1
                           dj1 = 1.0_fp
                        endif
                        !
                        ! spatial weight factors
                        !
                        f(1) = (1.0_fp-di1) * (1.0_fp-dj1)
                        f(2) = (       di1) * (1.0_fp-dj1)
                        f(3) = (       di1) * (       dj1)
                        f(4) = (1.0_fp-di1) * (       dj1)
                        if (i1 < 1 .or. i1 > mx-1) then
                           write(tex,'(2f10.1)') x1,y1
                           write(meteomessage,'(2a)') 'Flow point outside meteo grid (x-dir) x,y,:',trim(tex)
                           success = .false.
                           return
                        endif
                        if (j1 < 1 .or. j1 > grid_width-1) then
                           write(tex,'(2f10.1)') x1,y1
                           write(meteomessage,'(2a)') 'Flow point outside meteo grid (y-dir) x,y,:',trim(tex)
                           success = .false.
                           return
                        endif
                        u(1) = v0(i1  , j1  , 1)
                        u(2) = v0(i1+1, j1  , 1)
                        u(3) = v0(i1+1, j1+1, 1)
                        u(4) = v0(i1  , j1+1, 1)
                        v(1) = v1(i1  , j1  , 1)
                        v(2) = v1(i1+1, j1  , 1)
                        v(3) = v1(i1+1, j1+1, 1)
                        v(4) = v1(i1  , j1+1, 1)
                        vv0  = u(1)*f(1) + u(2)*f(2) + u(3)*f(3) + u(4)*f(4)
                        vv1  = v(1)*f(1) + v(2)*f(2) + v(3)*f(3) + v(4)*f(4)
                        qarray(n, m) = a0*vv0 + a1*vv1
                     endif
                  enddo
               enddo
            case ( meteo_on_curvilinear_grid )
               v1    => meteoitem%field(it1)%arr3d
               v0    => meteoitem%field(it0)%arr3d
               grid  => meteoitem%grid
               !
               ! interpolate and add noise to the wind field
               !
               if (present(gridnoise)) then
                  !
                  ! Interpolate towards the meteo grid. That means: loop over all
                  ! meteo grid points; a bounding quadrangle on the coarse grid has to be found;
                  ! (using findnm is not possible since the noisegrid does not contain 
                  ! curvilinaer information (i.e. it has no num_rows,num_columns; it is one-dimensional!))
                  ! Then, bilin5 is called to obtain the interpolation weights
                  ! NOTE: the coarse grid must completely cover the meteogrid.
                  ! The problem boils down to find four noisegrid points, in a quadrangle, in the neighbourhood of
                  ! each meteo gridpoint. This quadrangle should ideally be covering the meteo gridpoint.
                  !
                  allocate(meteo_noise(grid%num_columns, grid%num_rows))
                  meteo_noise = 0.0_fp
                  do m_mg = 1, grid%num_columns
                     do n_mg = 1, grid%num_rows
                        x_mg = grid%x(m_mg,n_mg)
                        y_mg = grid%y(m_mg,n_mg)
                        call find_noisegrid_vertices(x_mg, y_mg, ind, grid_size, &
                                                   & gridnoise(:,2), gridnoise(:,3))
                        do dir=1,4                                                     
                           u_hp(dir) = real(gridnoise(ind(dir),2),hp)
                           v_hp(dir) = real(gridnoise(ind(dir),3),hp)
                        enddo   
                        x_hp = real(x_mg, hp)
                        y_hp = real(y_mg, hp)
                        call bilin5(u_hp, v_hp, x_hp, y_hp, f_hp, ierr)
                        !
                        if (ierr == 1) then
                           meteomessage = 'noisegrid: error in bilin5'
                           success = .false.
                           return
                        endif
                        f    = real(f_hp, fp)
                        w(1) = gridnoise(ind(1),1)  ! noise parameter in SW
                        w(2) = gridnoise(ind(2),1)  ! noise parameter in SE
                        w(3) = gridnoise(ind(3),1)  ! noise parameter in NE
                        w(4) = gridnoise(ind(4),1)  ! noise parameter in NW
                        meteo_noise(m_mg,n_mg) = w(1)*f(1) + w(2)*f(2) + w(3)*f(3) + w(4)*f(4)
                     enddo
                  enddo
                  !
                  ! now add the noise field to the meteofields from file
                  ! at two timelevels, since time interpolation has not occured yet.
                  !
                  v0(:,:,1) = v0(:,:,1) + meteo_noise(:,:) 
                  v1(:,:,1) = v1(:,:,1) + meteo_noise(:,:)
                  !
                  ! here: debug output of noise possible
                  !
                  deallocate(meteo_noise)
               endif
               !
               ! interpolate the wind field towards the D3Dflow-grid
               ! at this stage, the wind noise (if present) has already been added
               ! 
               do m = 1,meteo%flowgrid%num_columns
                  do n = 1,meteo%flowgrid%num_rows
                     if (meteo%flowgrid%kcs(n, m) > 0) then
                        x = meteo%flowgrid%xz(n, m)
                        y = meteo%flowgrid%yz(n, m)
                        !
                        ! findnm requires double precision values
                        !
                        x_hp = real(x,hp)
                        y_hp = real(y,hp)
                        success = findnm(x_hp, y_hp, mv, nv, m, n, grid)
                        if (.not. success) return
                        u(1) = grid%x(mv-1, nv-1)
                        v(1) = grid%y(mv-1, nv-1)
                        u(2) = grid%x(mv  , nv-1)
                        v(2) = grid%y(mv  , nv-1)
                        u(3) = grid%x(mv  , nv)
                        v(3) = grid%y(mv  , nv)
                        u(4) = grid%x(mv-1, nv)
                        v(4) = grid%y(mv-1, nv)
                        !
                        ! Bilin5 requires double precision values
                        !
                        u_hp = real(u,hp)
                        v_hp = real(v,hp)
                        f_hp = real(f,hp)
                        call bilin5(u_hp, v_hp, x_hp, y_hp, f_hp, ierr)
                        if (ierr == 1) then
                           meteomessage = 'curvilinear meteo grid: error in bilin5'
                           success = .false.
                           return
                        endif
                        u    = real(u_hp, fp)
                        v    = real(v_hp, fp)
                        f    = real(f_hp, fp)
                        w(1) = v0(mv-1, nv-1, 1)
                        w(2) = v0(mv  , nv-1, 1)
                        w(3) = v0(mv  , nv  , 1)
                        w(4) = v0(mv-1, nv  , 1)
                        vv0  = w(1)*f(1) + w(2)*f(2) + w(3)*f(3) + w(4)*f(4)
                        w(1) = v1(mv-1, nv-1, 1)
                        w(2) = v1(mv  , nv-1, 1)
                        w(3) = v1(mv  , nv  , 1)
                        w(4) = v1(mv-1, nv  , 1)
                        vv1  = w(1)*f(1) + w(2)*f(2) + w(3)*f(3) + w(4)*f(4)
                        qarray(n, m) = a0*vv0 + a1*vv1
                     endif
                  enddo
               enddo
               
            case ( meteo_on_spiderweb_grid )
               !
               ! Spiderweb arrays (spwf and spwarr) and corresponding time are all together updated
               ! This code is visited for each quantity pressure, windu and windv
               ! Skip updating the arrays when the time is correct
               !
               if (spw%time /= time) then
                  spw%time = time
                  v1       => meteoitem%field(it1)%arr3d
                  v0       => meteoitem%field(it0)%arr3d
                  !
                  ! Spatial coordinates
                  !
                  x01_eye =  meteoitem%field(it1)%x_spw_eye
                  y01_eye =  meteoitem%field(it1)%y_spw_eye
                  nodata1 =  meteoitem%field(it1)%all_nodata
                  dx1     =  meteoitem%field(it1)%dx
                  dy1     =  meteoitem%field(it1)%dy
                  !
                  x00_eye =  meteoitem%field(it0)%x_spw_eye
                  y00_eye =  meteoitem%field(it0)%y_spw_eye
                  nodata0 =  meteoitem%field(it0)%all_nodata
                  !
                  ! Current position of cyclone eye
                  !
                  if (nodata0 .and. x00_eye == nodata_default) then
                     x01   = x01_eye
                     y01   = y01_eye
                  elseif (nodata1 .and. x01_eye == nodata_default) then
                     x01   = x00_eye
                     y01   = y00_eye
                  else
                     x01   = a0*x00_eye + a1*x01_eye
                     y01   = a0*y00_eye + a1*y01_eye
                  endif
                  !
                  if (nodata0 .and. nodata1) then
                     alpha = 0.0_fp
                  elseif (nodata0) then
                     alpha = a1
                     a1 = 1.0_fp
                     a0 = 0.0_fp
                  elseif (nodata1) then
                     alpha = a0
                     a0 = 1.0_fp
                     a1 = 0.0_fp
                  else
                     alpha = 1.0_fp
                  endif
                  !
                  ! Radius of cyclone
                  !
                  rcycl = dy1 * real((grid_width-1),fp)
                  !
                  ! Factor used for merging spiderweb and background winds
                  ! If spw_merge_frac is 0.5 (default), the smooth merging starts
                  ! from 50 percent of spiderweb radius with background winds
                  ! getting larger weight factor towards the edge of the spider web.
                  ! spw_merge_frac is defined in the spw file.
                  !
                  fm0   = 1.0_fp/(1.0_fp - real(meteoitem%spw_merge_frac,fp))
                  !
                  do m = 1,meteo%flowgrid%num_columns
                     do n = 1,meteo%flowgrid%num_rows
                        if (meteo%flowgrid%kcs(n, m) /= 0) then
                           spw%spwf(n, m) = 1.0_fp
                           do k = 1, kx
                              spw%spwarr(k, n, m) = 0.0_fp
                           enddo
                           x  = meteo%flowgrid%xz(n,m)
                           y  = meteo%flowgrid%yz(n,m)
                           xx = x
                           yy = y
                           dx = x - x01
                           dy = y - y01
                           !
                           ! Distance to centre
                           !
                           call distance2(msferic, x, y, x01, y01, yy)
                           if (yy > rcycl) cycle
                           !
                           ! We are located within the radius of the cyclone
                           !
                           if (.not. (dy == 0.0_fp .and. dx == 0.0_fp)) then
                              xx = atan2(dy, dx)
                              !
                              ! Nautical convention
                              !
                              xx = 0.5_fp*pi - xx
                              if (xx < 0.0_fp) then
                                 xx = xx + 2.0_fp*pi
                              endif
                           else
                              xx = 0.0_fp
                           endif
                           !
                           ! Spatial merge function
                           !
                           fm = fm0*(1.0_fp - yy/rcycl)
                           spw%spwf(n, m) = 1.0_fp - (max(0.0_fp, min(1.0_fp,fm)))*alpha
                           x1  = xx / dx1
                           y1  = yy / dy1
                           i1  =  1 + int(x1)
                           j1  =  1 + int(y1)
                           di1 =  1.0_fp + x1 - real(i1,fp)
                           dj1 =  1.0_fp + y1 - real(j1,fp)
                           if (i1 == mx) then
                              i1  = i1 - 1
                              di1 = 1.0_fp
                           endif
                           if (j1 == grid_width) then
                              j1  = j1 - 1
                              dj1 = 1.0_fp
                           endif
                           !
                           ! Weight factors
                           !    
                           f(1) = (1.0_fp-di1) * (1.0_fp-dj1)
                           f(2) = (       di1) * (1.0_fp-dj1)
                           f(3) = (       di1) * (       dj1)
                           f(4) = (1.0_fp-di1) * (       dj1)
                           !
                           ! When flow point outside spiderweb grid:
                           ! skip this point
                           !
                           if (i1 < 1 .or. i1 > mx-1) cycle
                           if (j1 < 1 .or. j1 > grid_width-1) cycle
                           do k = 1,kx
                              u(1) =   v0(i1  , j1  , k)
                              u(2) =   v0(i1+1, j1  , k)
                              u(3) =   v0(i1+1, j1+1, k)
                              u(4) =   v0(i1  , j1+1, k)
                              v(1) =   v1(i1  , j1  , k)
                              v(2) =   v1(i1+1, j1  , k)
                              v(3) =   v1(i1+1, j1+1, k)
                              v(4) =   v1(i1  , j1+1, k)
                              if (k == 2 ) then
                                 !
                                 ! Angle interpolation
                                 !
                                 !
                                 ! regulate requires double precision values
                                 !
                                 u_hp = real(u,hp)
                                 v_hp = real(v,hp)
                                 call regulate(u_hp, v_hp, a0, a1, w)
                                 z(k) = w(1)*f(1) + w(2)*f(2) + w(3)*f(3) + w(4)*f(4)
                                 !
                                 ! Nautical convention
                                 !
                                 z(k) = (270.0_fp-z(k)) * d2r
                              else
                                 vv0  = u(1)*f(1) + u(2)*f(2) + u(3)*f(3) + u(4)*f(4)
                                 vv1  = v(1)*f(1) + v(2)*f(2) + v(3)*f(3) + v(4)*f(4)
                                 z(k) = a0*vv0 + a1*vv1
                              endif
                           enddo
                           !
                           ! windu
                           !
                           spw%spwarr(1, n, m) = z(1) * cos(z(2))
                           !
                           ! windv
                           !
                           spw%spwarr(2, n, m) = z(1) * sin(z(2))
                           !
                           ! patm
                           !
                           spw%spwarr(3, n, m) = z(3)
                        endif
                     enddo
                  enddo
               endif
            end select
         endif
      enddo
   enddo
   !
   if (meteo%spiderweb%active) then
      select case (quantity)
      case ('patm')
         do m = 1,meteo%flowgrid%num_columns
            do n = 1,meteo%flowgrid%num_rows
               if (meteo%flowgrid%kcs(n,m) /= 0) then
                  if (meteoitem%pref_option == opt_not_defined) then
                     qarray(n,m) = qarray(n,m) - (1.0_fp-spw%spwf(n,m))*spw%spwarr(3,n,m)
                  else
                     qarray(n,m) = spw%spwf(n,m)*qarray(n,m) &
                                 & + (1.0_fp-spw%spwf(n,m))*(meteoitem%pref - spw%spwarr(3,n,m))
                  endif
               endif
            enddo
         enddo
      case ('windu')
         do m = 1,meteo%flowgrid%num_columns
            do n = 1,meteo%flowgrid%num_rows
               if (meteo%flowgrid%kcs(n,m) /= 0) then
                  qarray(n,m) = spw%spwf(n,m)*qarray(n,m) &
                              & + (1.0_fp-spw%spwf(n,m))*spw%spwarr(1,n,m)
               endif
            enddo
         enddo
      case ('windv')
         do m = 1,meteo%flowgrid%num_columns
            do n = 1,meteo%flowgrid%num_rows
               if (meteo%flowgrid%kcs(n,m) /= 0) then
                  qarray(n,m) = spw%spwf(n,m)*qarray(n,m) &
                              & + (1.0_fp-spw%spwf(n,m))*spw%spwarr(2,n,m)
               endif
            enddo
         enddo
      end select
   endif
   success = .true.
end function getmeteoval
!
!
!===============================================================================
subroutine regulate(w0, w1, a0, a1, w)
!
! angular interpolation
!
   implicit none
   real(fp)              , intent(in)  :: a0
   real(fp)              , intent(in)  :: a1
   real(hp), dimension(4)              :: w0
   real(hp), dimension(4)              :: w1
   real(hp), dimension(4), intent(out) :: w
!
! local
!
   integer :: k
!
! body
!
   !
   ! Time interpolation
   !
   do k = 1, 4
     call regdir(w0(k), w1(k))
     w(k) = a0*w0(k) + a1*w1(k)
   enddo
   !
   ! The four surrounding points
   !
   call regdir(w(4), w(3))
   call regdir(w(1), w(2))
   call regdir(w(2), w(3))
   call regdir(w(1), w(4))
   call regdir(w(2), w(4))
   call regdir(w(1), w(3))
end subroutine regulate
!
!
!===============================================================================
subroutine regdir(w0, w1)
!
! angle regularisation
!
   implicit none
!
! arguments
!
   real(hp) :: w0
   real(hp) :: w1
!
! external
!
   !
   ! Previous method
   !
   !if (     (w1 - w0) > 180.0_fp ) then
   !   w0 = w0 + 360.0_fp
   !elseif ( (w0 - w1) > 180.0_fp ) then
   !   w1 = w1 + 360.0_fp
   !endif

   if (comparereal( abs(w0-w1) , 180.0_hp ) == 1) then
      if (comparereal( w0, w1 ) == -1) then
         w0 = w0 + 360.0_hp
      else
         w1 = w1 + 360.0_hp
      endif
   endif
end subroutine regdir
!
!
!===============================================================================
function findnm(xp, yp, mv, nv, mpoint, npoint, grid) result(success)
    !
    ! Search mv,nv indices in grid in which the xp,yp point is located
    ! mpoint, npoint indices identify the xp,yp point
    ! mv and nv are stored in the mnref array
    ! The next time point xp,yp (with the same mpoint, npoint indices) has
    ! to be searched, first is checked whether the mnref array already contains
    ! the mv, nv indices
    !
    implicit none
!
! Global variables
!

    type(TGrid), pointer              :: grid   ! Grid in which to locate the x,y point
    integer             , intent(out) :: mv     ! m coordinate in
    integer             , intent(out) :: nv
    integer             , intent(in)  :: mpoint
    integer             , intent(in)  :: npoint
    real(hp)            , intent(in)  :: xp
    real(hp)            , intent(in)  :: yp
    logical                           :: success
!
! Local variables
!
    integer                :: i
    integer                :: j
    integer                :: mlowbound
    integer                :: mupbound
    integer                :: nlowbound
    integer                :: nupbound
    integer                :: inhul
    real(fp), dimension(4) :: xl
    real(fp), dimension(4) :: yl
!
!! executable statements -------------------------------------------------------
!
    success = .true.
    mv = grid%mnref(mpoint, npoint, 1)
    nv = grid%mnref(mpoint, npoint, 2)
    !
    ! if mnref contains indices, return them
    !
    if (mv>0 .and. nv>0) return
    !
    ! mcur, ncur contain the indices of the previous search
    ! first look around these indices
    !
    if (grid%mcur > 0 .and. grid%ncur > 0) then
       mlowbound = max(2,grid%mcur - 1)
       mupbound  = min(grid%num_columns,grid%mcur + 1)
       nlowbound = max(2,grid%ncur - 1)
       nupbound  = min(grid%num_rows,grid%ncur + 1)
       do i = mlowbound,mupbound
          do j = nlowbound,nupbound
             xl(1) = grid%x(i,j)
             xl(2) = grid%x(i-1,j)
             xl(3) = grid%x(i-1,j-1)
             xl(4) = grid%x(i,j-1)
             yl(1) = grid%y(i,j)
             yl(2) = grid%y(i-1,j)
             yl(3) = grid%y(i-1,j-1)
             yl(4) = grid%y(i,j-1)
             if (xl(1) .ne. 0 .and. xl(2) .ne. 0 .and. &
               & xl(3) .ne. 0 .and. xl(4) .ne. 0 )then
                call pinpok(xp, yp, 4, xl, yl, inhul, 4)
                if(inhul .eq. 1) then
                   mv = i
                   nv = j
                   grid%mcur = i
                   grid%ncur = j
                   grid%mnref(mpoint, npoint, 1) = i
                   grid%mnref(mpoint, npoint, 2) = j
                   return
                endif
             endif
          enddo
       enddo
    endif
    !
    ! If the indices are not found by now, the complete grid
    ! must be walked through
    !
    do i = 2,grid%num_columns
       do j = 2,grid%num_rows
          xl(1) = grid%x(i,j)
          xl(2) = grid%x(i-1,j)
          xl(3) = grid%x(i-1,j-1)
          xl(4) = grid%x(i,j-1)
          yl(1) = grid%y(i,j)
          yl(2) = grid%y(i-1,j)
          yl(3) = grid%y(i-1,j-1)
          yl(4) = grid%y(i,j-1)
          if (xl(1) .ne. 0 .and. xl(2) .ne. 0 .and. &
            & xl(3) .ne. 0 .and. xl(4) .ne. 0 )then
             call pinpok(xp, yp, 4, xl, yl, inhul, 4)
             if(inhul .eq. 1) then
                mv = i
                nv = j
                grid%mcur = i
                grid%ncur = j
                grid%mnref(mpoint, npoint, 1) = i
                grid%mnref(mpoint, npoint, 2) = j
                return
             endif
          endif
       enddo
    enddo
    write(meteomessage,'(a,e14.5,a,e14.5)') 'meteo_findnm: m,n not found for point x=',xp,' y=',yp
    success = .false.
end function findnm
!
!
!
!==================================================================================
subroutine find_noisegrid_vertices(x_mg, y_mg, ind, ngrid, &
                                      xgrid, ygrid)
   implicit none
!
! Global variables
!
    integer, dimension(4)       , intent(out) :: ind   ! indices of noise grid to form a quadrangle 
                                                       ! covering x_mg,y_mg
    real(hp)                    , intent(in)  :: x_mg  ! x-coordinate of meteo grid point
    real(hp)                    , intent(in)  :: y_mg  ! y-coordinate of meteo grid point
    integer                     , intent(in)  :: ngrid
    real(fp), dimension(ngrid)  , intent(in)  :: xgrid
    real(fp), dimension(ngrid)  , intent(in)  :: ygrid
!
! local variables
!
   integer     :: i
   logical     :: lwest
   logical     :: lsouth
   real(fp)    :: distsqr
   real(fp)    :: d2_sw
   real(fp)    :: d2_se 
   real(fp)    :: d2_nw
   real(fp)    :: d2_ne
!
!! executable statements -------------------------------------------------------
!
   ind   = 0
   d2_sw = 1.0E20_fp;
   d2_se = 1.0E20_fp;
   d2_ne = 1.0E20_fp;
   d2_nw = 1.0E20_fp;
   !
   ! Look for the nearest coarse grid points southwest, southeast etc
   ! We assume that there is at least one grid point in each direction!   
   !
   do i = 1, ngrid
      distsqr = (x_mg-xgrid(i))**2 + (y_mg-ygrid(i))**2
      lsouth  = (ygrid(i) < y_mg)
      lwest   = (xgrid(i) < x_mg)
      if (lsouth .and. lwest) then           !SW (1)
         if (distsqr < d2_sw ) then
            d2_sw  = distsqr
            ind(1) = i
         endif
      elseif (lsouth .and. .not. lwest) then ! SE(2)
         if (distsqr < d2_se ) then
            d2_se  = distsqr
            ind(2) = i
         endif       
      elseif (.not. lsouth .and. lwest) then ! NW(4)
         if (distsqr < d2_nw ) then
            d2_nw  = distsqr
            ind(4) = i
         endif       
      else                                   ! NE(3)
         if (distsqr < d2_ne ) then
            d2_ne  = distsqr
            ind(3) = i
         endif       
      endif     
   enddo
   if (ind(1)*ind(2)*ind(3)*ind(4) == 0) then
      write(*,*) 'error: noise grid cannot provide a bounding box ',ind
      write(*,*) 'for meteo grid location: ',x_mg,y_mg
      write(*,*) 'coarse grid points:',(xgrid(i),ygrid(i),';',i=1,ngrid)
   endif
end subroutine find_noisegrid_vertices
!
!
!
!===============================================================================
subroutine pinpok(xl, yl, n, x, y, inside, maxhul)
   implicit none
   integer                    , intent(in)  :: n
   integer                    , intent(out) :: inside
   integer                    , intent(in)  :: maxhul
   real(hp)                   , intent(in)  :: xl
   real(hp)                   , intent(in)  :: yl
   real(fp), dimension(maxhul), intent(in)  :: x
   real(fp), dimension(maxhul), intent(in)  :: y

   integer :: i
   integer :: i1
   integer :: i2
   integer :: np
   integer :: right
   real(fp) :: rl
   real(fp) :: rm
   real(fp) :: x1
   real(fp) :: x2
   real(fp) :: y1
   real(fp) :: y2

   if (n .le. 2) then
      inside = 1
   else
      np = 0
 5    continue
      np = np + 1
      if (np .le. n) then
         if ( x(np) .ne. nodata_default) goto 5
      endif
      np = np - 1
      inside = 0
      right  = 0
      i = 0
10    continue
      i1 = mod(i,np) + 1
      i2 = mod(i1,np) + 1
      x1 = x(i1)
      x2 = x(i2)
      y1 = y(i1)
      y2 = y(i2)
      if (xl .ge. min(x1,x2) .and. xl .le. max(x1,x2) ) then
         if (xl .eq. x1 .and. yl .eq. y1 .or. &                     ! between or on line
          & (x1 .eq. x2 .and. &                                     ! in point 1
          &   yl .ge. min(y1,y2) .and. yl .le. max(y1,y2) ) .or. &
          & (yl .eq. y1 .and. y1 .eq. y2)  ) then                   ! on vertical line
            !
            ! on horizontal line
            !
            inside = 1
            return
         else if (x1 .ne. x2) then
            !
            ! skewed line
            !
            rl = ( xl - x1 )  / ( x2 - x1 )
            rm = ( y1 - yl )  + rl * ( y2 - y1 )
            if (rm .eq. 0) then
               !
               ! on skewed line
               !
               inside = 1
               return
            else if (rm .gt. 0.0) then
               !
               ! under skewed line
               !
               if (xl .eq. x1 .or. xl .eq. x2) then
                  if (x1 .gt. xl .or. x2 .gt. xl) then
                     right = right + 1
                  endif
               endif
               inside = 1 - inside
            endif
         endif
      endif
      i = i + 1
      if (i .lt. np) goto 10
      if (mod(right,2) .ne. 0) inside = 1 - inside
   endif
end subroutine pinpok


end module meteo
