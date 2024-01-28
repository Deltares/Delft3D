subroutine wrtdro(lundia    ,error     ,filename  ,itdroc    ,itdrof    , &
                & itdroi    ,ndro      ,xydro     ,sferic    ,irequest  , &
                & fds       ,itdate    ,dtsec     ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
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
!  
!  
!!--description-----------------------------------------------------------------
!
!    Function: Writes the time varying groups (2 & 3) to the
!              NEFIS DRO-DAT file
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use datagroups
    use globaldata
    use netcdf, only: nf90_unlimited
    use wrtarray, only: wrtvar
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                         , pointer :: celidt
    type (datagroup)                , pointer :: group2
    type (datagroup)                , pointer :: group3
    integer                         , pointer :: io_fp
    integer                         , pointer :: io_prec
!
! Global variables
!
    integer                                                             , intent(in)  :: fds      !  File handle of output NEFIS/NetCDF file
    integer                                                             , intent(in)  :: irequest !  REQUESTTYPE_DEFINE: define variables, REQUESTTYPE_WRITE: write variables
    character(*)                                                        , intent(in)  :: filename !  File name
    integer                                                             , intent(in)  :: itdate   !  Description and declaration in exttim.igs
    integer                                                             , intent(in)  :: itdroc !!  Current time counter for the history data file
    integer                                                             , intent(in)  :: itdrof !  Description and declaration in inttim.igs
    integer                                                             , intent(in)  :: itdroi !  Description and declaration in inttim.igs
    integer                                                                           :: lundia !  Description and declaration in inout.igs
    integer                                                                           :: ndro   !  Description and declaration in dimens.igs
    logical                                                             , intent(in)  :: sferic !  Description and declaration in tricom.igs
    logical                                                             , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    real(fp)                                                            , intent(in)  :: dtsec    !  Integration time step [in seconds]
    real(fp)    , dimension(2, ndro)                                                  :: xydro  !  Description and declaration in esm_alloc_real.f90
!
!
! Local variables
!
    integer                                           :: filetype
    integer                                           :: i
    integer                                           :: id
    integer                                           :: istat
    !
    integer                                           :: year
    integer                                           :: month
    integer                                           :: day
    !
    integer                                           :: iddim_ndro
    integer                                           :: iddim_time
    integer                                           :: iddim_2
    !
    integer                                           :: idatt_cal
    !
    integer                                           :: ierror      ! Local errorflag for NEFIS files 
    real(fp)       , dimension(:)      , allocatable  :: rbuff1
    character(16)                                     :: xcoordunit  ! Unit of X coordinate: M or DEGREES_EAST
    character(16)                                     :: ycoordunit  ! Unit of Y coordinate: M or DEGREES_NORTH
    character(16)                                     :: grnam2
    character(16)                                     :: grnam3
    character(256)                                    :: string
!
! Data statements
!
    data grnam2/'dro-info-series'/
    data grnam3/'dro-series'/
!
!
!! executable statements -------------------------------------------------------
!
    call getdatagroup(gdp, FILOUT_DRO, grnam2, group2)
    call getdatagroup(gdp, FILOUT_DRO, grnam3, group3)
    celidt              => group2%celidt
    io_fp               => gdp%gdpostpr%io_fp
    io_prec             => gdp%gdpostpr%io_prec
    !
    filetype = getfiletype(gdp, FILOUT_DRO)
    ierror = 0
    select case (irequest)
    case (REQUESTTYPE_DEFINE)
       !
       if (sferic) then
          xcoordunit = 'degrees_east'
          ycoordunit = 'degrees_north'
       else
          xcoordunit = 'm'
          ycoordunit = 'm'
       endif
       !
       ! Set up the element chracteristics
       !
       iddim_time    = adddim(gdp, lundia, FILOUT_DRO, 'time'    , nf90_unlimited)
       iddim_ndro    = adddim(gdp, lundia, FILOUT_DRO, 'NDRO'    , ndro)
       iddim_2       = adddim(gdp, lundia, FILOUT_DRO, 'length_2', 2)
       !
       idatt_cal = addatt(gdp, lundia, FILOUT_DRO, 'calendar','proleptic_gregorian')
       !
       ! dro-info-series
       !
       if (filetype == FTYPE_NEFIS) then
          call addelm(gdp, lundia, FILOUT_DRO, grnam2, 'ITDROC', ' ', IO_INT4, 0, longname='timestep number (ITDROC*DT*TUNIT := time in sec from ITDATE)')
       else
          year  = itdate / 10000
          month = (itdate - year*10000) / 100
          day   = itdate - year*10000 - month*100
          write(string,'(a,i0.4,a,i0.2,a,i0.2,a)') 'seconds since ', year, '-', month, '-', day,' 00:00:00'
          call addelm(gdp, lundia, FILOUT_DRO, grnam2, 'time'  , 'time', io_fp    , 0, longname='time', unit=trim(string), attribs=(/idatt_cal/) )
       endif
       !
       ! dro-series
       !
       if (filetype == FTYPE_NEFIS) then
          call addelm(gdp, lundia, FILOUT_DRO, grnam3, 'XYDRO', ' ', io_fp   , 2, dimids=(/iddim_2, iddim_ndro/), longname='x- and y-coordinates of drogue tracks', unit=xcoordunit)
       else
          call addelm(gdp, lundia, FILOUT_DRO, grnam3, 'XDRO', ' ', io_fp   , 1, dimids=(/iddim_ndro/), longname='x-coordinates of drogue tracks', unit=xcoordunit)
          call addelm(gdp, lundia, FILOUT_DRO, grnam3, 'YDRO', ' ', io_fp   , 1, dimids=(/iddim_ndro/), longname='y-coordinates of drogue tracks', unit=ycoordunit)
       endif
       !
       group2%grp_dim = iddim_time
       group3%grp_dim = iddim_time
       celidt = 0
       !
    case (REQUESTTYPE_WRITE)
       !
       celidt = celidt + 1
       group3%celidt = celidt
       !
       ! element 'ITDROC' / time
       !
       if (filetype == FTYPE_NEFIS) then
          call wrtvar(fds, filename, filetype, grnam2, celidt, &
                    & gdp, ierror, lundia, itdroc, 'ITDROC')
       else
          call wrtvar(fds, filename, filetype, grnam2, celidt, &
                    & gdp, ierror, lundia, itdroc*dtsec, 'time')
       endif
       if (ierror/= 0) goto 9999
       !
       ! element 'XYDRO'
       !
       if (filetype == FTYPE_NEFIS) then
          call wrtvar(fds, filename, filetype, grnam3, celidt, &
                    & gdp, ierror, lundia, xydro, 'XYDRO')
       else
          allocate(rbuff1(ndro), stat=istat)
          do i = 1,ndro
             rbuff1(i) = xydro(1,i)
          enddo
          call wrtvar(fds, filename, filetype, grnam3, celidt, &
                    & gdp, ierror, lundia, rbuff1, 'XDRO')
          do i = 1,ndro
             rbuff1(i) = xydro(2,i)
          enddo
          call wrtvar(fds, filename, filetype, grnam3, celidt, &
                    & gdp, ierror, lundia, rbuff1, 'YDRO')
          deallocate(rbuff1, stat=istat)
       endif
       if (ierror/= 0) goto 9999
       !
    end select
    !
    ! write error message if error occured and set error = .true.
    !
9999   continue
    if (ierror /= 0) error = .true.
end subroutine wrtdro
