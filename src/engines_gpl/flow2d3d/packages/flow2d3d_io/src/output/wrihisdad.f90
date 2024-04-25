subroutine wrihisdad(lundia    ,error     ,filename  ,itdate    , &
                   & tunit     ,dt        ,lsedtot   ,irequest  , &
                   & fds       ,gdp       )
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
! Writes the initial Dredge and Dump group to HIS-DAT
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
    real(fp)      , dimension(:,:) , pointer :: link_percentage
    real(fp)      , dimension(:)   , pointer :: link_distance
    integer                        , pointer :: nadred
    integer                        , pointer :: nadump
    integer                        , pointer :: nasupl
    integer                        , pointer :: nalink
    integer       , dimension(:,:) , pointer :: link_def
    character( 80), dimension(:)   , pointer :: dredge_areas
    character( 80), dimension(:)   , pointer :: dump_areas
    integer                        , pointer :: io_prec
!
! Global variables
!
    integer                                                             , intent(in)  :: irequest !  REQUESTTYPE_DEFINE: define variables, REQUESTTYPE_WRITE: write variables
    integer                                                             , intent(in)  :: itdate   !  Description and declaration in exttim.igs
    integer                                                             , intent(in)  :: lsedtot  !  Description and declaration in exttim.igs
    integer                                                                           :: lundia   !  Description and declaration in inout.igs
    logical                                                             , intent(out) :: error    
    real(fp)                                                            , intent(in)  :: dt       !  Description and declaration in esm_alloc_real.f90
    real(fp)                                                            , intent(in)  :: tunit    !  Description and declaration in exttim.igs
    character(*)                                                        , intent(in)  :: filename !  File name
    integer                                                             , intent(in)  :: fds      !  File handle of output NEFIS/NetCDF file
!
! Local variables
!
    integer                                           :: filetype
    integer                                           :: l
    integer                                           :: k
    integer                                           :: ierror ! Local errorflag for NEFIS files
    integer       , dimension(2)                      :: ival   ! Local array for writing ITDATE and
    character(16)                                     :: grnam
    !
    integer                                           :: iddim_lsedtot
    integer                                           :: iddim_nalink
    integer                                           :: iddim_nsource
    integer                                           :: iddim_ndump
    integer                                           :: iddim_1
    integer                                           :: iddim_2
    !
!
! Data statements
!
    data grnam/'his-dad-const'/
!
!! executable statements -------------------------------------------------------
!
    filetype = getfiletype(gdp, FILOUT_HIS)
    !
    link_percentage   => gdp%gddredge%link_percentage
    link_distance     => gdp%gddredge%link_distance
    nadred            => gdp%gddredge%nadred
    nadump            => gdp%gddredge%nadump
    nasupl            => gdp%gddredge%nasupl
    nalink            => gdp%gddredge%nalink
    link_def          => gdp%gddredge%link_def
    dredge_areas      => gdp%gddredge%dredge_areas
    dump_areas        => gdp%gddredge%dump_areas
    io_prec           => gdp%gdpostpr%io_prec
    !
    ! Initialize local variables
    !
    ierror = 0
    select case (irequest)
    case (REQUESTTYPE_DEFINE)
       !
       ! Set up the element chracteristics
       !
       iddim_nalink  = adddim(gdp, lundia, FILOUT_HIS, 'ndredge_links', nalink)
       iddim_lsedtot = adddim(gdp, lundia, FILOUT_HIS, 'LSEDTOT', lsedtot)
       iddim_nsource = adddim(gdp, lundia, FILOUT_HIS, 'ndredge_src', nadred+nasupl)
       iddim_ndump   = adddim(gdp, lundia, FILOUT_HIS, 'ndredge_dmp', nadump)
       iddim_1       = adddim(gdp, lundia, FILOUT_HIS, 'length_1', 1)
       iddim_2       = adddim(gdp, lundia, FILOUT_HIS, 'length_2', 2)
       !
       if (filetype /= FTYPE_NETCDF) then ! don't store duplicates for NetCDF       
          call addelm(gdp, lundia, FILOUT_HIS, grnam, 'ITDATE', ' ', IO_INT4           , 1, dimids=(/iddim_2/), longname='Initial date (input) & time (default 00:00:00)', unit='[YYYYMMDD]')
          call addelm(gdp, lundia, FILOUT_HIS, grnam, 'TUNIT', ' ', io_prec            , 0, longname='Time scale related to seconds', unit='s')
          call addelm(gdp, lundia, FILOUT_HIS, grnam, 'DT', ' ', io_prec               , 0, longname='Time step (DT*TUNIT sec)')
       endif
       call addelm(gdp, lundia, FILOUT_HIS, grnam, 'DREDGE_AREAS', ' ', 80          , 1, dimids=(/iddim_nsource/), longname='Names identifying dredge areas/dredge polygons') !CHARACTER
       call addelm(gdp, lundia, FILOUT_HIS, grnam, 'DUMP_AREAS', ' ', 80            , 1, dimids=(/iddim_ndump/), longname='Names identifying dump areas/dump polygons') !CHARACTER
       call addelm(gdp, lundia, FILOUT_HIS, grnam, 'LINK_DEF', ' ', IO_INT4         , 2, dimids=(/iddim_nalink, iddim_2/), longname='Actual transports from dredge(1st col) to dump(2nd col) areas')
       call addelm(gdp, lundia, FILOUT_HIS, grnam, 'LINK_PERCENTAGES', ' ', io_prec , 2, dimids=(/iddim_nalink, iddim_lsedtot/), longname='Distribution of dredged material from dredge to dump areas', unit='percent')
       call addelm(gdp, lundia, FILOUT_HIS, grnam, 'LINK_DISTANCE', ' ', io_prec    , 2, dimids=(/iddim_nalink, iddim_1/), longname='Link Distance between dredge and dump areas', unit='m')
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
          endif
          !
          ! element 'DREDGE_AREAS'
          !
          call wrtvar(fds, filename, filetype, grnam, 1, &
                    & gdp, ierror, lundia, dredge_areas, 'DREDGE_AREAS')
          if (ierror/= 0) goto 9999
          !
          ! element 'DUMP_AREAS'
          !
          call wrtvar(fds, filename, filetype, grnam, 1, &
                    & gdp, ierror, lundia, dump_areas, 'DUMP_AREAS')
          if (ierror/= 0) goto 9999
          !
          ! element 'LINK_DEF'
          !
          call wrtvar(fds, filename, filetype, grnam, 1, &
                    & gdp, ierror, lundia, link_def, 'LINK_DEF')
          if (ierror/= 0) goto 9999
          !
          ! element 'LINK_PERCENTAGES'
          !
          call wrtvar(fds, filename, filetype, grnam, 1, &
                    & gdp, ierror, lundia, link_percentage, 'LINK_PERCENTAGES')
          if (ierror/= 0) goto 9999
          !
          ! element 'LINK_DISTANCE'
          !
          call wrtvar(fds, filename, filetype, grnam, 1, &
                    & gdp, ierror, lundia, link_distance, 'LINK_DISTANCE')
          if (ierror/= 0) goto 9999
          !
       endif
    end select
    !
    ! write error message if error occured and set error = .true.
    !
9999 continue
    if (ierror /= 0) error = .true.
end subroutine wrihisdad
