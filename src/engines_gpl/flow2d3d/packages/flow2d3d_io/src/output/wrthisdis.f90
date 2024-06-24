subroutine wrthisdis(lundia    ,error     ,filename  ,ithisc    ,zmodel    , &
                   & kmax      ,lstsc     ,nsrc      ,mnksrc    ,disch     , &
                   & dps       ,rint      ,s1        ,sig       ,zk        , &
                   & voldis    ,xcor      ,ycor      ,sferic    ,irequest  , &
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
! Writes the time varying Discharge group to the NEFIS HIS-DAT file
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
    real(fp)      , dimension(:)   , pointer :: poscul
    character(24)                  , pointer :: date_time
    integer                        , pointer :: celidt
    type (datagroup)               , pointer :: group
    integer                        , pointer :: io_prec
!
! Global variables
!
    integer                                                             , intent(in)  :: irequest !  REQUESTTYPE_DEFINE: define variables, REQUESTTYPE_WRITE: write variables
    integer                                                             , intent(in)  :: ithisc   ! Description and declaration in tricom.f90
    integer                                                             , intent(in)  :: kmax     
    integer                                                             , intent(in)  :: lstsc    ! Description and declaration in dimens.igs
    integer                                                             , intent(in)  :: lundia   ! Description and declaration in inout.igs
    integer                                                             , intent(in)  :: nsrc     ! Description and declaration in dimens.igs
    integer   , dimension(7, nsrc)                                                    :: mnksrc   ! Description and declaration in r-i-ch.igs
    real(fp)  , dimension(nsrc)                                         , intent(in)  :: disch    ! Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)     , intent(in)  :: dps      ! Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(lstsc, nsrc)                                  , intent(in)  :: rint     ! Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)     , intent(in)  :: s1       ! Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(kmax)                                         , intent(in)  :: sig      ! Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nsrc)                                         , intent(in)  :: voldis   ! Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)     , intent(in)  :: xcor     ! Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)     , intent(in)  :: ycor     ! Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(0:kmax)                                       , intent(in)  :: zk       ! See description and declaration of sig in esm_alloc_real.f90
    logical                                                             , intent(out) :: error    ! Description and declaration in tricom.f90
    logical                                                             , intent(in)  :: zmodel   ! Description and declaration in procs.igs
    logical                                                             , intent(in)  :: sferic   !  Description and declaration in tricom.igs
    character(*)                                                        , intent(in)  :: filename !  File name
    integer                                                             , intent(in)  :: fds      !  File handle of output NEFIS/NetCDF file
!
! Local variables
!
    integer                                           :: filetype
    integer                                           :: i
    integer       , dimension(nsrc)                   :: ibuff          ! Help array to write integers
    integer                                           :: ierror         ! Local error flag for NEFIS files
    integer                                           :: isrc           ! Index number of discharge location 
    integer                                           :: k
    integer                                           :: m
    integer                                           :: n    
    !
    integer                                           :: iddim_time
    integer                                           :: iddim_lstsc
    integer                                           :: iddim_nsrc
    integer                                           :: iddim_2
    !
    real(fp)                                          :: h0             ! Water depth (DPS+S1)     
    real(fp)                                          :: zdown
    real(fp)                                          :: zup
    real(fp)      , dimension(nsrc)                   :: rbuff1         ! Help array to write integers
    character(24) , dimension(1)                      :: datetimearr
    character(256)                                    :: errmsg
    character(16)                                     :: grnam          ! Data-group name defined for the NEFIS-files group 1 
    character(16)                                     :: xcoordunit  ! Unit of X coordinate: M or DEGREES_EAST
    character(16)                                     :: ycoordunit  ! Unit of Y coordinate: M or DEGREES_NORTH
!
! Data statements
!
    data grnam/'his-dis-series'/
!
!! executable statements -------------------------------------------------------
!
    call getdatagroup(gdp,FILOUT_HIS,grnam,group)
    celidt            => group%celidt
    filetype = getfiletype(gdp, FILOUT_HIS)
    !
    poscul           => gdp%gdculver%poscul
    date_time        => gdp%gdinttim%date_time
    io_prec          => gdp%gdpostpr%io_prec
    !
    ierror = 0
    select case (irequest)
    case (REQUESTTYPE_DEFINE)
       !
       ! Set up the element chracteristics
       !
       if (sferic) then
          xcoordunit = 'degrees_east'
          ycoordunit = 'degrees_north'
       else
          xcoordunit = 'm'
          ycoordunit = 'm'
       endif
       !
       ! dimensions
       !
       iddim_time    = adddim(gdp, lundia, FILOUT_HIS, 'time', nf90_unlimited)
       iddim_nsrc    = adddim(gdp, lundia, FILOUT_HIS, 'NSRC', nsrc)
       iddim_lstsc   = adddim(gdp, lundia, FILOUT_HIS, 'LSTSC', lstsc)
       iddim_2       = adddim(gdp, lundia, FILOUT_HIS, 'length_2', 2)
       !
       if (filetype /= FTYPE_NETCDF) then ! don't store duplicate times
          call addelm(gdp, lundia, FILOUT_HIS, grnam, 'ITHISC', ' ', IO_INT4 , 0, longname='timestep number (ITHISC*DT*TUNIT := time in sec from ITDATE)  ')
          call addelm(gdp, lundia, FILOUT_HIS, grnam, 'DATE_TIME', ' ', 24   , 0, longname='Current simulation date and time [YYYY-MM-DD HH:MM:SS.FFFF]') !CHARACTER
       endif
       call addelm(gdp, lundia, FILOUT_HIS, grnam, 'RINT', ' ', io_prec   , 2, dimids=(/iddim_nsrc, iddim_lstsc/), longname='Concentrations of the discharge')
       call addelm(gdp, lundia, FILOUT_HIS, grnam, 'ZQ', ' ', io_prec     , 1, dimids=(/iddim_nsrc/), longname='Momentary discharge', unit='m3/s')
       call addelm(gdp, lundia, FILOUT_HIS, grnam, 'ZQ_SUM', ' ', io_prec , 1, dimids=(/iddim_nsrc/), longname='Cumulative volume of the discharge', unit='m3')
       call addelm(gdp, lundia, FILOUT_HIS, grnam, 'MCOR1', ' ', IO_INT4  , 1, dimids=(/iddim_nsrc/), longname='first M coordinate of discharge')
       call addelm(gdp, lundia, FILOUT_HIS, grnam, 'NCOR1', ' ', IO_INT4  , 1, dimids=(/iddim_nsrc/), longname='first N coordinate of discharge')
       call addelm(gdp, lundia, FILOUT_HIS, grnam, 'KCOR1', ' ', IO_INT4  , 1, dimids=(/iddim_nsrc/), longname='first K coordinate of discharge')
       call addelm(gdp, lundia, FILOUT_HIS, grnam, 'XCOR1', ' ', io_prec  , 1, dimids=(/iddim_nsrc/), longname='first X coordinate of discharge', unit=xcoordunit)
       call addelm(gdp, lundia, FILOUT_HIS, grnam, 'YCOR1', ' ', io_prec  , 1, dimids=(/iddim_nsrc/), longname='first Y coordinate of discharge', unit=ycoordunit)
       call addelm(gdp, lundia, FILOUT_HIS, grnam, 'ZCOR1', ' ', io_prec  , 1, dimids=(/iddim_nsrc/), longname='first Z coordinate of discharge', unit='m')
       call addelm(gdp, lundia, FILOUT_HIS, grnam, 'MCOR2', ' ', IO_INT4  , 1, dimids=(/iddim_nsrc/), longname='second M coordinate of discharge')
       call addelm(gdp, lundia, FILOUT_HIS, grnam, 'NCOR2', ' ', IO_INT4  , 1, dimids=(/iddim_nsrc/), longname='second N coordinate of discharge')
       call addelm(gdp, lundia, FILOUT_HIS, grnam, 'KCOR2', ' ', IO_INT4  , 1, dimids=(/iddim_nsrc/), longname='second K coordinate of discharge')
       call addelm(gdp, lundia, FILOUT_HIS, grnam, 'XCOR2', ' ', io_prec  , 1, dimids=(/iddim_nsrc/), longname='second X coordinate of discharge', unit=xcoordunit)
       call addelm(gdp, lundia, FILOUT_HIS, grnam, 'YCOR2', ' ', io_prec  , 1, dimids=(/iddim_nsrc/), longname='second Y coordinate of discharge', unit=ycoordunit)
       call addelm(gdp, lundia, FILOUT_HIS, grnam, 'ZCOR2', ' ', io_prec  , 1, dimids=(/iddim_nsrc/), longname='second Z coordinate of discharge', unit='m')
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
          ! element 'RINT', only if LSTSC > 0
          !
          if (lstsc .gt. 0) then
             call wrtvar(fds, filename, filetype, grnam, celidt, &
                       & gdp, ierror, lundia, rint, 'RINT')
             if (ierror/= 0) goto 9999
          endif
          !
          ! element 'ZQ'
          !
          call wrtvar(fds, filename, filetype, grnam, celidt, &
                    & gdp, ierror, lundia, disch, 'ZQ')
          if (ierror/= 0) goto 9999
          !
          ! element 'ZQ_SUM'
          !
          call wrtvar(fds, filename, filetype, grnam, celidt, &
                    & gdp, ierror, lundia, voldis, 'ZQ_SUM')
          if (ierror/= 0) goto 9999
          !
          ! element 'MCOR1'
          !
          do isrc = 1, nsrc
             ibuff(isrc) = mnksrc(2, isrc)
          enddo
          call wrtvar(fds, filename, filetype, grnam, celidt, &
                    & gdp, ierror, lundia, ibuff, 'MCOR1')
          if (ierror/= 0) goto 9999
          !
          ! element 'NCOR1'
          !
          do isrc = 1, nsrc
             ibuff(isrc) = mnksrc(1, isrc)
          enddo
          call wrtvar(fds, filename, filetype, grnam, celidt, &
                    & gdp, ierror, lundia, ibuff, 'NCOR1')
          if (ierror/= 0) goto 9999
          !
          ! element 'KCOR1'
          !
          ! Recompute layer number of culvert discharge (KCOR1) for T0 (for POSTPR)
          ! NOTE: IMPORTANT
          !       In case of z-model different way of determining the z-coordinate
          !       (using relative layer thickness; complementary value is used due
          !       to inverted definition of layer indices in the z-model)
          !
          do isrc = 1, nsrc
             m = mnksrc(1, isrc)
             n = mnksrc(2, isrc)
             k = mnksrc(3, isrc)
             !
             h0 = real(dps(n, m),fp) + s1(n, m)
             if (mnksrc(7, isrc) == 4 .or. mnksrc(7, isrc) == 5 ) then
               zup = s1(n, m)
               do k=1,kmax
                  zdown = zup + h0 *sig(k)
                  if (poscul(isrc) < zup .and. poscul(isrc) >= zdown) mnksrc(3, isrc) = k
                  zup = zdown
               enddo
             endif
             !
             ibuff(isrc) = mnksrc(3, isrc)
          enddo
          call wrtvar(fds, filename, filetype, grnam, celidt, &
                    & gdp, ierror, lundia, ibuff, 'KCOR1')
          if (ierror/= 0) goto 9999
          !
          ! element 'XCOR1'
          !
          do isrc = 1, nsrc
              m = mnksrc(1, isrc)
              n = mnksrc(2, isrc)
              rbuff1(isrc) = xcor(n, m)
          enddo
          call wrtvar(fds, filename, filetype, grnam, celidt, &
                    & gdp, ierror, lundia, rbuff1, 'XCOR1')
          if (ierror/= 0) goto 9999
          !
          ! element 'YCOR1'
          !
          do isrc = 1, nsrc
              m = mnksrc(1, isrc)
              n = mnksrc(2, isrc)    
              rbuff1(isrc) = ycor(n, m)
          enddo
          call wrtvar(fds, filename, filetype, grnam, celidt, &
                    & gdp, ierror, lundia, rbuff1, 'YCOR1')
          if (ierror/= 0) goto 9999
          !
          ! element 'ZCOR1'
          !
          ! Compute the exact Z-position of the discharges for ZCOR1
          ! NOTE: IMPORTANT
          !       In case of z-model different way of determining the z-coordinate
          !       (using relative layer thickness; complementary value is used due
          !       to inverted definition of layer indices in the z-model)
          !
          do isrc = 1, nsrc
             m = mnksrc(1, isrc)
             n = mnksrc(2, isrc)
             k = mnksrc(3, isrc)
             !
             h0 = real(dps(n, m),fp) + s1(n, m)
             !
             if (k == 0) then
                rbuff1(isrc) = s1(n, m) - 0.5 * h0
             elseif (zmodel) then
                rbuff1(isrc) = 0.5 * ( zk(k) + zk(k-1) )
             else
                rbuff1(isrc) = s1(n, m) + sig(k) * h0
             endif
             if (mnksrc(7, isrc) == 4 .or. mnksrc(7, isrc) == 5 ) then
                rbuff1(isrc) = poscul(isrc)
             endif
          enddo
          call wrtvar(fds, filename, filetype, grnam, celidt, &
                    & gdp, ierror, lundia, rbuff1, 'ZCOR1')
          if (ierror/= 0) goto 9999
          !
          ! element 'MCOR2'
          !
          do isrc = 1, nsrc
             ibuff(isrc) = mnksrc(5, isrc)
          enddo
          call wrtvar(fds, filename, filetype, grnam, celidt, &
                    & gdp, ierror, lundia, ibuff, 'MCOR2')
          if (ierror/= 0) goto 9999
          !
          ! element 'NCOR2'
          !
          do isrc = 1, nsrc
             ibuff(isrc) = mnksrc(4, isrc)
          enddo
          call wrtvar(fds, filename, filetype, grnam, celidt, &
                    & gdp, ierror, lundia, ibuff, 'NCOR2')
          if (ierror/= 0) goto 9999
          !
          ! element 'KCOR2'
          !
          ! Recompute layer number of culvert discharge (KCOR2) for T0 (for POSTPR)
          ! NOTE: IMPORTANT
          !       In case of z-model different way of determining the z-coordinate
          !       (using relative layer thickness; complementary value is used due
          !       to inverted definition of layer indices in the z-model)
          !
          do isrc = 1, nsrc
             m = mnksrc(4, isrc)
             n = mnksrc(5, isrc)
             k = mnksrc(6, isrc)
             !
             h0 = real(dps(n, m),fp) + s1(n, m)
             if (mnksrc(7, isrc) == 4 .or. mnksrc(7, isrc) == 5 ) then
               zup = s1(n, m)
               do k=1,kmax
                  zdown = zup + h0 *sig(k)
                  if (poscul(isrc) < zup .and. poscul(isrc) >= zdown) mnksrc(6, isrc) = k
                  zup = zdown
               enddo
             endif
             !
             ibuff(isrc) = mnksrc(6, isrc)
          enddo
          call wrtvar(fds, filename, filetype, grnam, celidt, &
                    & gdp, ierror, lundia, ibuff, 'KCOR2')
          if (ierror/= 0) goto 9999
          !
          ! element 'XCOR2'
          !
          do isrc = 1, nsrc
              m = mnksrc(4, isrc)
              n = mnksrc(5, isrc)
              rbuff1(isrc) = xcor(n, m)
          enddo
          call wrtvar(fds, filename, filetype, grnam, celidt, &
                    & gdp, ierror, lundia, rbuff1, 'XCOR2')
          if (ierror/= 0) goto 9999
          !
          ! element 'YCOR2'
          !
          do isrc = 1, nsrc
              m = mnksrc(4, isrc)
              n = mnksrc(5, isrc)    
              rbuff1(isrc) = ycor(n, m)
          enddo
          call wrtvar(fds, filename, filetype, grnam, celidt, &
                    & gdp, ierror, lundia, rbuff1, 'YCOR2')
          if (ierror/= 0) goto 9999
          !
          ! element 'ZCOR2'
          !    
          ! Compute the exact Z-position of the discharges for ZCOR2
          ! NOTE: IMPORTANT
          !       In case of z-model different way of determining the z-coordinate
          !       (using relative layer thickness; complementary value is used due
          !       to inverted definition of layer indices in the z-model)
          !
          do isrc = 1, nsrc
             m = mnksrc(4, isrc)
             n = mnksrc(5, isrc)
             k = mnksrc(6, isrc)
             !
             h0 = real(dps(n, m),fp) + s1(n, m)
             !
             if (k == 0) then
                rbuff1(isrc) = s1(n, m) - 0.5 * h0
             elseif (zmodel) then
                rbuff1(isrc) = 0.5 * ( zk(k) + zk(k-1) )
             else
                rbuff1(isrc) = s1(n, m) + sig(k) * h0
             endif
             if (mnksrc(7, isrc) == 4 .or. mnksrc(7, isrc) == 5 ) then
                rbuff1(isrc) = poscul(isrc)
             endif
          enddo
          call wrtvar(fds, filename, filetype, grnam, celidt, &
                    & gdp, ierror, lundia, rbuff1, 'XCOR2')
          if (ierror/= 0) goto 9999
       endif
       !
    end select
    !
    ! write error message if error occured and set error = .true.
    !
9999   continue
    if (ierror /= 0) error = .true.
end subroutine wrthisdis
