subroutine wrwavh(lundia    ,error     ,filename  ,ithisc    , &
                & nostat    ,zhs       ,ztp       , &
                & zdir      ,zrlabd    ,zuwb      ,irequest  , &
                & fds       ,nostatto  ,nostatgl  ,order_sta , &
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
! Writes the time varying data for waves (6 & 7)
! to the NEFIS HIS-DAT file.
! Output is performed conform the times of history
! file and only in case wave.eq.TRUE.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use datagroups
    use globaldata
    use netcdf, only: nf90_unlimited
    use dfparall, only: inode, master
    use wrtarray, only: wrtvar, wrtarray_n, station
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                         , pointer :: celidt
    type (datagroup)                , pointer :: group4
    type (datagroup)                , pointer :: group5
    integer                         , pointer :: io_prec
!
! Global variables
!
    integer                                                             , intent(in)  :: irequest !  REQUESTTYPE_DEFINE: define variables, REQUESTTYPE_WRITE: write variables
    integer                                                             , intent(in)  :: ithisc
    integer                                                                           :: ithisi   !  Description and declaration in inttim.igs
    integer                                                                           :: itstrt   !  Description and declaration in inttim.igs
    integer                                                                           :: lundia   !  Description and declaration in inout.igs
    integer                                                                           :: nostat   !  Description and declaration in dimens.igs
    logical                                                             , intent(out) :: error
    real(fp), dimension(nostat)                                                       :: zdir
    real(fp), dimension(nostat)                                                       :: zhs
    real(fp), dimension(nostat)                                                       :: zrlabd
    real(fp), dimension(nostat)                                                       :: ztp
    real(fp), dimension(nostat)                                                       :: zuwb
    character(*)                                                        , intent(in)  :: filename !  File name
    integer                                                             , intent(in)  :: fds      !  File handle of output NEFIS/NetCDF file
    integer                                                             , intent(in)  :: nostatgl ! global number of stations (i.e. original number excluding duplicate stations located in the halo regions)
    integer                                                             , intent(in)  :: nostatto ! total number of stations (including "duplicate" stations located in halo regions)
    integer       , dimension(nostat)                                   , intent(in)  :: order_sta
!
! Local variables
!
    integer                                           :: filetype
    integer                                           :: ierror        ! Local error flag for NEFIS files 
    integer                                           :: n 
    !
    integer                                           :: iddim_time
    integer                                           :: iddim_nostat
    !
    integer                                           :: idatt_sta
    !
    character(16)           :: grnam4
    character(16)           :: grnam5
    character(256)          :: errmsg
    character(60)           :: filnam
    character(1024)         :: error_string
!
! Data statements
!
    data grnam4/'his-infwav-serie'/
    data grnam5/'his-wav-series'/
!
!! executable statements -------------------------------------------------------
!
    call getdatagroup(gdp, FILOUT_HIS, grnam4, group4)
    call getdatagroup(gdp, FILOUT_HIS, grnam5, group5)
    celidt  => group4%celidt
    filetype = getfiletype(gdp, FILOUT_HIS)
    io_prec             => gdp%gdpostpr%io_prec
    !
    ierror = 0
    select case (irequest)
    case (REQUESTTYPE_DEFINE)
       !
       ! Set up the element characteristics
       !
       iddim_time    = adddim(gdp, lundia, FILOUT_HIS, 'time', nf90_unlimited)
       iddim_nostat  = adddim(gdp, lundia, FILOUT_HIS, 'NOSTAT', nostatgl)
       !
       idatt_sta = addatt(gdp, lundia, FILOUT_HIS, 'coordinates','NAMST XSTAT YSTAT')
       !
       ! his-infwav-serie
       !
       if (filetype == FTYPE_NEFIS) then
          call addelm(gdp, lundia, FILOUT_HIS, grnam4, 'ITHISW', ' ', IO_INT4, 0, longname='timestep number (ITHISW*DT*TUNIT := time in sec from ITDATE)')
       endif
       !
       ! his-sed-series: stations
       !
       if (nostat>0) then
         call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'ZHS', ' ', io_prec    , 1, dimids=(/iddim_nostat/), longname='Significant wave height at station', unit='m', attribs=(/idatt_sta/) )
         call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'ZTP', ' ', io_prec    , 1, dimids=(/iddim_nostat/), longname='Peak wave period at station', unit='s', attribs=(/idatt_sta/) )
         call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'ZDIR', ' ', io_prec   , 1, dimids=(/iddim_nostat/), longname='Direction waves are coming from at station (CW from North)', unit='arc_degrees', attribs=(/idatt_sta/) )
         call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'ZRLABD', ' ', io_prec , 1, dimids=(/iddim_nostat/), longname='Wave length at station', unit='m', attribs=(/idatt_sta/) )
         call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'ZUWB', ' ', io_prec   , 1, dimids=(/iddim_nostat/), longname='Peak near-bed orbital speed at station (Hs, linear theory)', unit='m/s', attribs=(/idatt_sta/) )
       endif
       !
       group4%grp_dim = iddim_time
       group5%grp_dim = iddim_time
       celidt = 0
       !
    case (REQUESTTYPE_WRITE)
       !
       celidt = celidt + 1
       group5%celidt = celidt
       !
       if (filetype == FTYPE_NEFIS) then
          !
          ! element 'ITHISW'
          !
          call wrtvar(fds, filename, filetype, grnam4, celidt, &
                    & gdp, ierror, lundia, ithisc, 'ITHISW')
          if (ierror/= 0) goto 9999
       endif
       !
       if (nostat > 0) then
          !
          ! element 'ZHS'
          !
          call wrtarray_n(fds, filename, filetype, grnam5, &
                 & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                 & ierror, lundia, zhs, 'ZHS', station)
          if (ierror/= 0) goto 9999
          !
          ! element 'ZTP'
          !
          call wrtarray_n(fds, filename, filetype, grnam5, &
                 & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                 & ierror, lundia, ztp, 'ZTP', station)
          if (ierror/= 0) goto 9999
          !
          ! element 'ZDIR'
          !
          call wrtarray_n(fds, filename, filetype, grnam5, &
                 & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                 & ierror, lundia, zdir, 'ZDIR', station)
          if (ierror/= 0) goto 9999
          !
          ! element 'ZRLABD'
          !
          call wrtarray_n(fds, filename, filetype, grnam5, &
                 & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                 & ierror, lundia, zrlabd, 'ZRLABD', station)
          if (ierror/= 0) goto 9999
          !
          ! element 'ZUWB'
          !
          call wrtarray_n(fds, filename, filetype, grnam5, &
                 & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                 & ierror, lundia, zuwb, 'ZUWB', station)
          if (ierror/= 0) goto 9999
       endif
       !
    end select
    !
    ! write error message if error occured and set error = .true.
    !
9999   continue
    if (ierror /= 0) error = .true.
end subroutine wrwavh
