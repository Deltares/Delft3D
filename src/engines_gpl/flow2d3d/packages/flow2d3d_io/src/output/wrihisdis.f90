subroutine wrihisdis(lundia    ,error     ,filename  ,itdate    ,tunit     , &
                   & dt        ,nsrc      ,namsrc    ,irequest  ,fds       , &
                   & gdp       )
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
! Writes the initial Discharge group to HIS-DAT
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use datagroups
    use globaldata
    use dfparall, only: inode, master
    use wrtarray, only: wrtvar
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                         , pointer :: io_prec
!
! Global variables
!
    integer                                                             , intent(in)  :: irequest !  REQUESTTYPE_DEFINE: define variables, REQUESTTYPE_WRITE: write variables
    integer                                                             , intent(in)  :: itdate   !  Description and declaration in exttim.igs
    integer                                                             , intent(in)  :: lundia   !  Description and declaration in inout.igs
    integer                                                             , intent(in)  :: nsrc     !  Description and declaration in dimens.igs        
    real(fp)                                                            , intent(in)  :: dt       !  Description and declaration in esm_alloc_real.f90    
    real(fp)                                                            , intent(in)  :: tunit    !  Description and declaration in exttim.igs
    logical                                                             , intent(out) :: error    !  Description and declaration in tricom.f90
    character(20), dimension(nsrc)                                      , intent(in)  :: namsrc   !  Description and declaration in r-i-ch.igs
    character(*)                                                        , intent(in)  :: filename !  File name
    integer                                                             , intent(in)  :: fds      !  File handle of output NEFIS/NetCDF file
!
! Local variables
!
    integer                                           :: filetype
    integer                                           :: ierror ! Local error flag
    integer       , dimension(2)                      :: ival   ! Local array for writing ITDATE and
    !
    integer                                           :: iddim_nsrc
    integer                                           :: iddim_2
    !
    character(16)                                     :: grnam  ! Data-group name defined for the NEFIS-files group 1 
!
! Data statements
!
    data grnam/'his-dis-const'/
!
!! executable statements -------------------------------------------------------
!
    io_prec             => gdp%gdpostpr%io_prec
    ierror = 0
    filetype = getfiletype(gdp, FILOUT_HIS)
    select case (irequest)
    case (REQUESTTYPE_DEFINE)
       !
       ! Set up the element chracteristics
       !
       iddim_nsrc    = adddim(gdp, lundia, FILOUT_HIS, 'NSRC', nsrc)
       iddim_2       = adddim(gdp, lundia, FILOUT_HIS, 'length_2', 2)
       !
       if (filetype /= FTYPE_NETCDF) then ! don't store duplicates for NetCDF       
          call addelm(gdp, lundia, FILOUT_HIS, grnam, 'ITDATE', ' ', IO_INT4, 1, dimids=(/iddim_2/), longname='Initial date (input) & time (default 00:00:00)', unit='[YYYYMMDD]')
          call addelm(gdp, lundia, FILOUT_HIS, grnam, 'TUNIT', ' ', io_prec , 0, longname='Time scale related to seconds', unit='s')
          call addelm(gdp, lundia, FILOUT_HIS, grnam, 'DT', ' ', io_prec    , 0, longname= 'Time step (DT*TUNIT sec)')
       endif
       call addelm(gdp, lundia, FILOUT_HIS, grnam, 'DISCHARGES', ' ', 20 , 1, dimids=(/iddim_nsrc/), longname='Names identifying discharges') !CHARACTER
       !
    case (REQUESTTYPE_WRITE)
       !
       if (inode == master) then
          if (filetype /= FTYPE_NETCDF) then ! don't store duplicates for NetCDF       
             !
             ! element 'ITDATE'
             !
             ival(1) = itdate
             ival(2) = 000000
             call wrtvar(fds, filename, filetype, grnam, 1, &
                       & gdp, ierror, lundia, ival, 'ITDATE')
             if (ierror/= 0) goto 9999
             !
             ! element 'TUNIT'
             !
             call wrtvar(fds, filename, filetype, grnam, 1, &
                       & gdp, ierror, lundia, tunit, 'TUNIT')
             if (ierror/= 0) goto 9999
             !
             ! element 'DT'
             !
             call wrtvar(fds, filename, filetype, grnam, 1, &
                       & gdp, ierror, lundia, dt, 'DT')
             if (ierror/= 0) goto 9999
          endif
          !
          ! element 'DISCHARGES'
          !
          call wrtvar(fds, filename, filetype, grnam, 1, &
                    & gdp, ierror, lundia, namsrc, 'DISCHARGES')
          if (ierror/= 0) goto 9999
          !
       endif
    end select
    !
    ! write error message if error occured and set error = .true.
    !
9999   continue
    if (ierror /= 0) error = .true.
end subroutine wrihisdis
