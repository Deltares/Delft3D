subroutine wrthisdad(lundia    ,error     ,filename  ,ithisc    , &
                   & lsedtot   ,irequest  ,fds       ,gdp       )
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
! Writes the time varying Dredge and Dump group to the NEFIS HIS-DAT file
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use datagroups
    use globaldata
    use netcdf, only: nf90_unlimited
    use dfparall, only: inode, master
    use wrtarray, only: wrtvar
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)            , dimension(:,:) , pointer :: link_sum
    real(fp)            , dimension(:,:) , pointer :: voldred
    real(fp)            , dimension(:)   , pointer :: totvoldred
    real(fp)            , dimension(:,:) , pointer :: voldump
    real(fp)            , dimension(:)   , pointer :: totvoldump
    real(fp)            , dimension(:)   , pointer :: tim_dredged
    real(fp)            , dimension(:)   , pointer :: tim_ploughed
    integer                              , pointer :: nadred
    integer                              , pointer :: nadump
    integer                              , pointer :: nasupl
    integer                              , pointer :: nalink
    real(fp)                             , pointer :: tim_accum
    character(24)                        , pointer :: date_time
    integer                              , pointer :: celidt
    type (datagroup)                     , pointer :: group
    integer                              , pointer :: io_prec
!
! Global variables
!
    integer                                                             , intent(in)  :: irequest !  REQUESTTYPE_DEFINE: define variables, REQUESTTYPE_WRITE: write variables
    integer                                                             , intent(in)  :: ithisc
    integer                                                             , intent(in)  :: lsedtot
    integer                                                                           :: lundia   !  Description and declaration in inout.igs
    logical                                                             , intent(out) :: error
    character(*)                                                        , intent(in)  :: filename !  File name
    integer                                                             , intent(in)  :: fds      !  File handle of output NEFIS/NetCDF file
!
! Local variables
!
    real(fp)        , dimension(:)    , allocatable   :: rbuff1
    integer                                           :: filetype
    integer                                           :: istat
    integer                                           :: i
    integer                                           :: l
    integer                                           :: ierror      ! Local errorflag for NEFIS files
    !
    integer                                           :: iddim_time
    integer                                           :: iddim_lsedtot
    integer                                           :: iddim_nalink
    integer                                           :: iddim_nsource
    integer                                           :: iddim_ndump
    !
    integer                                           :: idatt_crd_drd
    integer                                           :: idatt_crd_dmp
    !
    real(fp)                                          :: tfrac
    character(16)                                     :: grnam
!
! Data statements
!
    data grnam/'his-dad-series'/
!
!! executable statements -------------------------------------------------------
!
    call getdatagroup(gdp,FILOUT_HIS,grnam,group)
    celidt            => group%celidt
    filetype = getfiletype(gdp, FILOUT_HIS)
    !
    link_sum          => gdp%gddredge%link_sum
    voldred           => gdp%gddredge%voldred
    totvoldred        => gdp%gddredge%totvoldred
    voldump           => gdp%gddredge%voldump
    totvoldump        => gdp%gddredge%totvoldump
    tim_dredged       => gdp%gddredge%tim_dredged
    tim_ploughed      => gdp%gddredge%tim_ploughed
    nadred            => gdp%gddredge%nadred
    nadump            => gdp%gddredge%nadump
    nasupl            => gdp%gddredge%nasupl
    nalink            => gdp%gddredge%nalink
    tim_accum         => gdp%gddredge%tim_accum
    date_time         => gdp%gdinttim%date_time
    io_prec           => gdp%gdpostpr%io_prec
    !
    ierror = 0
    select case (irequest)
    case (REQUESTTYPE_DEFINE)
       !
       ! Set up the element chracteristics
       !
       iddim_time    = adddim(gdp, lundia, FILOUT_HIS, 'time', nf90_unlimited)
       iddim_nalink  = adddim(gdp, lundia, FILOUT_HIS, 'ndredge_links', nalink)
       iddim_lsedtot = adddim(gdp, lundia, FILOUT_HIS, 'LSEDTOT', lsedtot)
       iddim_nsource = adddim(gdp, lundia, FILOUT_HIS, 'ndredge_src', nadred+nasupl)
       iddim_ndump   = adddim(gdp, lundia, FILOUT_HIS, 'ndredge_dmp', nadump)
       !
       idatt_crd_drd = addatt(gdp, lundia, FILOUT_HIS, 'coordinates', 'DREDGE_AREAS')
       idatt_crd_dmp = addatt(gdp, lundia, FILOUT_HIS, 'coordinates', 'DUMP_AREAS')
       !
       if (filetype /= FTYPE_NETCDF) then ! don't store duplicate times
          call addelm(gdp, lundia, FILOUT_HIS, grnam, 'ITHISC', ' ', IO_INT4        , 0, longname='timestep number (ITHISC*DT*TUNIT := time in sec from ITDATE)')
          call addelm(gdp, lundia, FILOUT_HIS, grnam, 'DATE_TIME', ' ', 24          , 0, longname='Current simulation date and time [YYYY-MM-DD HH:MM:SS.FFFF]') !CHARACTER
       endif
       call addelm(gdp, lundia, FILOUT_HIS, grnam, 'LINK_SUM', ' ', io_prec      , 2, dimids=(/iddim_nalink, iddim_lsedtot/), longname='Cumulative dredged material transported via this link', unit='m3')
       call addelm(gdp, lundia, FILOUT_HIS, grnam, 'DREDGE_VOLUME', ' ', io_prec , 1, dimids=(/iddim_nsource/), longname='Cumulative dredged material for this dredge area', unit='m3', attribs=(/idatt_crd_drd/) )
       call addelm(gdp, lundia, FILOUT_HIS, grnam, 'DUMP_VOLUME', ' ', io_prec   , 1, dimids=(/iddim_ndump/), longname='Cumulative dumped material for this dump area', unit='m3', attribs=(/idatt_crd_dmp/) )
       call addelm(gdp, lundia, FILOUT_HIS, grnam, 'DREDGE_TFRAC', ' ', io_prec  , 1, dimids=(/iddim_nsource/), longname='Time fraction spent dredging', attribs=(/idatt_crd_drd/) )
       call addelm(gdp, lundia, FILOUT_HIS, grnam, 'PLOUGH_TFRAC', ' ', io_prec  , 1, dimids=(/iddim_nsource/), longname='Time fraction spent sploughing', attribs=(/idatt_crd_drd/) )
       !
       group%grp_dim = iddim_time
       celidt = 0
       !
    case (REQUESTTYPE_WRITE)
       !
       celidt = celidt + 1
       !
       if (inode == master) then
          if (filetype /= FTYPE_NETCDF) then ! don't store duplicate times
             !
             ! element 'ITHISC'
             !
             call wrtvar(fds, filename, filetype, grnam, celidt, &
                       & gdp, ierror, lundia, ithisc, 'ITHISC')
             if (ierror/= 0) goto 9999
             !
             ! element 'DATE_TIME'
             !
             call wrtvar(fds, filename, filetype, grnam, celidt, &
                       & gdp, ierror, lundia, date_time, 'DATE_TIME')
             if (ierror/= 0) goto 9999
          endif
          !
          ! element 'LINK_SUM'
          !
          call wrtvar(fds, filename, filetype, grnam, celidt, &
                    & gdp, ierror, lundia, link_sum, 'LINK_SUM')
          if (ierror/= 0) goto 9999
          !
          ! element 'DREDGE_VOLUME'
          !
          call wrtvar(fds, filename, filetype, grnam, celidt, &
                    & gdp, ierror, lundia, totvoldred, 'DREDGE_VOLUME')
          if (ierror/= 0) goto 9999
          !
          ! element 'DUMP_VOLUME'
          !
          call wrtvar(fds, filename, filetype, grnam, celidt, &
                    & gdp, ierror, lundia, totvoldump, 'DUMP_VOLUME')
          if (ierror/= 0) goto 9999
          !
          if (tim_accum > 0.0_fp) then
             tfrac = 1.0_fp/tim_accum
          else
             tfrac = 1.0_fp
          endif
          !
          ! element 'DREDGE_TFRAC'
          !
          allocate(rbuff1(nadred+nasupl), stat=istat)
          do i = 1, nadred+nasupl
             rbuff1(i) = tfrac*tim_dredged(i)
          enddo
          call wrtvar(fds, filename, filetype, grnam, celidt, &
                    & gdp, ierror, lundia, rbuff1, 'DREDGE_TFRAC')
          if (ierror/= 0) goto 9999
          !
          ! element 'PLOUGH_TFRAC'
          !
          do i = 1, nadred+nasupl
             rbuff1(i) = tfrac*tim_ploughed(i)
          enddo
          call wrtvar(fds, filename, filetype, grnam, celidt, &
                    & gdp, ierror, lundia, rbuff1, 'PLOUGH_TFRAC')
          deallocate(rbuff1, stat=istat)
          if (ierror/= 0) goto 9999
       endif
       !
       tim_accum    = 0.0_fp
       tim_dredged  = 0.0_fp
       tim_ploughed = 0.0_fp
       !
    end select
    !
    ! write error message if error occured and set error = .true.
    !
9999   continue
    if (ierror /= 0) error = .true.
end subroutine wrthisdad
