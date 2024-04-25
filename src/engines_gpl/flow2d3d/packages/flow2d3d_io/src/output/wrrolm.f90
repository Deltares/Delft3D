subroutine wrrolm(lundia    ,error     ,filename  ,itmapc    ,nmax      , &
                & mmax      ,nmaxus    ,ewave1    ,eroll1    ,qxkr      , &
                & qykr      ,qxkw      ,qykw      ,fxw       ,fyw       , &
                & wsu       ,wsv       ,guu       ,gvv       ,rbuff1    , &
                & hrms      ,tp        ,teta      ,rlabda    ,uorb      , &
                & irequest  ,fds       ,iarrc     ,mf        ,ml        , &
                & nf        ,nl        ,roller    ,xbeach    ,gdp       )
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
! Writes the time varying data for wave model (WAVE coupling, Roller, or XBeach)
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use datagroups
    use globaldata
    use dfparall, only: inode, master, nproc
    use wrtarray, only: wrtarray_nm, wrtvar
    use netcdf
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                         , pointer :: celidt
    type (datagroup)                , pointer :: group1
    type (datagroup)                , pointer :: group2
    integer                         , pointer :: nmaxgl
    integer                         , pointer :: mmaxgl
    integer                         , pointer :: io_prec
!
! Global variables
!
    integer                                                                           , intent(in)  :: itmapc
    integer                                                                                         :: lundia      !  Description and declaration in inout.igs
    integer                                                                                         :: mmax        !  Description and declaration in esm_alloc_int.f90
    integer                                                                           , intent(in)  :: nmax        !  Description and declaration in esm_alloc_int.f90
    integer                                                                                         :: nmaxus      !  Description and declaration in esm_alloc_int.f90
    logical                                                                           , intent(out) :: error       
    logical                                                                           , intent(in)  :: roller
    logical                                                                           , intent(in)  :: xbeach
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                     , intent(in)  :: eroll1      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                     , intent(in)  :: ewave1      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                     , intent(in)  :: fxw         !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                     , intent(in)  :: fyw         !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                     , intent(in)  :: guu         !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                     , intent(in)  :: gvv         !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                     , intent(in)  :: hrms        !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                     , intent(in)  :: qxkr        !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                     , intent(in)  :: qxkw        !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                     , intent(in)  :: qykr        !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                     , intent(in)  :: qykw        !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                     , intent(in)  :: rlabda      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                     , intent(in)  :: teta        !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                     , intent(in)  :: tp          !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                     , intent(in)  :: uorb        !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                     , intent(in)  :: wsu         !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                     , intent(in)  :: wsv         !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                                   :: rbuff1
    character(60)                                                                     , intent(in)  :: filename    !  File name
    integer                                                                           , intent(in)  :: irequest    !  REQUESTTYPE_DEFINE: define variables, REQUESTTYPE_WRITE: write variables
    integer                                                                           , intent(in)  :: fds         !  File handle of output NEFIS/NetCDF file
    !
    integer    , dimension(4,0:nproc-1)                                               , intent(in)  :: iarrc       ! array containing collected grid indices
    integer    , dimension(0:nproc-1)                                                 , intent(in)  :: mf          ! first index w.r.t. global grid in x-direction
    integer    , dimension(0:nproc-1)                                                 , intent(in)  :: ml          ! last index w.r.t. global grid in x-direction
    integer    , dimension(0:nproc-1)                                                 , intent(in)  :: nf          ! first index w.r.t. global grid in y-direction
    integer    , dimension(0:nproc-1)                                                 , intent(in)  :: nl          ! last index w.r.t. global grid in y-direction
!
! Local variables
!
    integer                                       :: filetype
    integer                                       :: i
    integer                                       :: ierror
    integer                                       :: m
    integer                                       :: n
    integer    , dimension(1)                     :: idummy
    integer                        , external     :: clsnef
    integer                        , external     :: open_datdef
    character(16)                                 :: grnam1
    character(16)                                 :: grnam2
    character(256)                                :: errmsg
    character(1024)                               :: error_string
    
    integer                                       :: iddim_time
    integer                                       :: iddim_n
    integer                                       :: iddim_nc
    integer                                       :: iddim_m
    integer                                       :: iddim_mc
!
! Data statements        
!        
    data grnam1/'map-infrol-serie'/
    data grnam2/'map-rol-series'/
!
!! executable statements -------------------------------------------------------
!
    call getdatagroup(gdp, FILOUT_MAP, grnam1, group1)
    call getdatagroup(gdp, FILOUT_MAP, grnam2, group2)
    celidt              => group1%celidt
    !
    mmaxgl              => gdp%gdparall%mmaxgl
    nmaxgl              => gdp%gdparall%nmaxgl
    io_prec             => gdp%gdpostpr%io_prec
    !
    filetype = getfiletype(gdp, FILOUT_MAP)
    !
    ierror = 0
    select case (irequest)
    case (REQUESTTYPE_DEFINE)
       !
       ! Define dimensions
       !
       iddim_time    = adddim(gdp, lundia, FILOUT_MAP, 'time'   , nf90_unlimited)
       iddim_n       = adddim(gdp, lundia, FILOUT_MAP, 'N'      , nmaxgl        ) ! Number of N-grid points (cell centres)
       iddim_nc      = adddim(gdp, lundia, FILOUT_MAP, 'NC'     , nmaxgl        ) ! Number of N-grid points (corner points)
       iddim_m       = adddim(gdp, lundia, FILOUT_MAP, 'M'      , mmaxgl        ) ! Number of M-grid points (cell centres)
       iddim_mc      = adddim(gdp, lundia, FILOUT_MAP, 'MC'     , mmaxgl        ) ! Number of M-grid points (corner points)
       !
       ! Define elements
       !
       ! map-infrol-serie
       ! 
       if (filetype == FTYPE_NEFIS) then
          call addelm(gdp, lundia, FILOUT_MAP, grnam1, 'ITMAPS', ' ', IO_INT4 , 0, longname='timestep number (ITMAPS*DT*TUNIT := time in sec from ITDATE)')
       endif
       !
       ! map-rol-series
       !
       call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'HS'    , ' ', io_prec , 2, dimids=(/iddim_n, iddim_m/), longname='Significant wave height', unit='m', acl='z')
       if (gdp%gdflwpar%flwoutput%waveqnt) then
          call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'TP'    , ' ', io_prec , 2, dimids=(/iddim_n, iddim_m/) , longname='Peak period', unit='s', acl='z')
          call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'TETA'  , ' ', io_prec , 2, dimids=(/iddim_n, iddim_m/) , longname='Wave direction relative to local grid direction', unit='deg', acl='z')
          call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'LAMBDA', ' ', io_prec , 2, dimids=(/iddim_n, iddim_m/) , longname='Wave length', unit='m', acl='z')
          call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'UORB'  , ' ', io_prec , 2, dimids=(/iddim_n, iddim_m/) , longname='Orbital velocity near bed', unit='m/s', acl='z')
       endif
       if (roller) then
          call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'EWAVE1', ' ', io_prec , 2, dimids=(/iddim_n, iddim_m/) , longname='Short-wave energy', unit='J/m2', acl='z')
          call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'EROLL1', ' ', io_prec , 2, dimids=(/iddim_n, iddim_m/) , longname='Roller energy', unit='J/m2', acl='z')
          call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'QXKR'  , ' ', io_prec , 2, dimids=(/iddim_n, iddim_mc/), longname='Transport velocity of roller energy in ksi direction', unit='m/s', acl='u')
          call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'QYKR'  , ' ', io_prec , 2, dimids=(/iddim_nc, iddim_m/), longname='Transport velocity of roller energy in eta direction', unit='m/s', acl='v')
          call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'QXKW'  , ' ', io_prec , 2, dimids=(/iddim_n, iddim_mc/), longname='Transport velocity of wave energy in ksi direction', unit='m/s', acl='u')
          call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'QYKW'  , ' ', io_prec , 2, dimids=(/iddim_nc, iddim_m/), longname='Transport velocity of wave energy in eta direction', unit='m/s', acl='v')
       endif
       if (roller .or. xbeach) then
          call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'FXW'   , ' ', io_prec , 2, dimids=(/iddim_n, iddim_mc/), longname='Component of wave force in ksi direction', unit='N/m2', acl='u')
          call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'FYW'   , ' ', io_prec , 2, dimids=(/iddim_nc, iddim_m/), longname='Component of wave force in eta direction', unit='N/m2', acl='v')
          call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'WSU'   , ' ', io_prec , 2, dimids=(/iddim_n, iddim_mc/), longname='Component of roller force in ksi direction', unit='N/m2', acl='u')
          call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'WSV'   , ' ', io_prec , 2, dimids=(/iddim_nc, iddim_m/), longname='Component of roller force in eta direction', unit='N/m2', acl='v')
       endif
       !
       group1%grp_dim = iddim_time
       group2%grp_dim = iddim_time
       celidt = 0
       !
    case (REQUESTTYPE_WRITE)
       !
       celidt = celidt + 1
       group2%celidt = celidt
       !
       ! Writing of output on every itmapc
       !
       if (filetype == FTYPE_NEFIS) then
          call wrtvar(fds, filename, filetype, grnam1, celidt, &
                    & gdp, ierror, lundia, itmapc, 'ITMAPS')
          if (ierror/=0) goto 9999
       endif
       !
       ! element 'HS'.  hs = sqrt(2)*hrms
       !
       rbuff1(:,:) = -999.0_fp
       do m = 1, mmax
          do n = 1, nmaxus
             rbuff1(n, m) = sqrt(2.0_fp)*hrms(n,m)
          enddo
       enddo
       call wrtarray_nm(fds, filename, filetype, grnam2, celidt, &
                    & nf, nl, mf, ml, iarrc, gdp, &
                    & ierror, lundia, rbuff1, 'HS')
       if (ierror /= 0) goto 9999
       !
       if (gdp%gdflwpar%flwoutput%waveqnt) then
           call wrtarray_nm(fds, filename, filetype, grnam2, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & ierror, lundia, tp, 'TP')
           if (ierror /= 0) goto 9999
           !
           call wrtarray_nm(fds, filename, filetype, grnam2, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & ierror, lundia, teta, 'TETA')
           if (ierror /= 0) goto 9999
           !
           call wrtarray_nm(fds, filename, filetype, grnam2, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & ierror, lundia, rlabda, 'LAMBDA')
           if (ierror /= 0) goto 9999
           !
           call wrtarray_nm(fds, filename, filetype, grnam2, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & ierror, lundia, uorb, 'UORB')
           if (ierror /= 0) goto 9999
       endif
       !
       if (roller) then
          !
          ! element 'EWAVE1'
          !
          call wrtarray_nm(fds, filename, filetype, grnam2, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, ewave1, 'EWAVE1')
          if (ierror /= 0) goto 9999
          !
          ! element 'EROLL1'
          !
          call wrtarray_nm(fds, filename, filetype, grnam2, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, eroll1, 'EROLL1')
          if (ierror /= 0) goto 9999
          !
          ! element 'QXKR'
          !
          do m = 1, mmax
             do n = 1, nmaxus
                if (abs(guu(n, m))>0.) then
                   rbuff1(n, m) = qxkr(n, m)/guu(n, m)
                endif
             enddo
          enddo
          call wrtarray_nm(fds, filename, filetype, grnam2, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, rbuff1, 'QXKR')
          if (ierror /= 0) goto 9999
          !
          ! element 'QYKR'
          !
          do m = 1, mmax
             do n = 1, nmaxus
                if (abs(gvv(n, m))>0.) then
                   rbuff1(n, m) = qykr(n, m)/gvv(n, m)
                endif
             enddo
          enddo
          call wrtarray_nm(fds, filename, filetype, grnam2, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, rbuff1, 'QYKR')
          if (ierror /= 0) goto 9999
          !
          ! element 'QXKW'
          !
          do m = 1, mmax
             do n = 1, nmaxus
                if (abs(guu(n, m))>0.) then
                   rbuff1(n, m) = qxkw(n, m)/guu(n, m)
                endif
             enddo
          enddo
          call wrtarray_nm(fds, filename, filetype, grnam2, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, rbuff1, 'QXKW')
          if (ierror /= 0) goto 9999
          !
          ! element 'QYKW'
          !
          do m = 1, mmax
             do n = 1, nmaxus
                if (abs(gvv(n, m))>0.) then
                   rbuff1(n, m) = qykw(n, m)/gvv(n, m)
                endif
             enddo
          enddo
          call wrtarray_nm(fds, filename, filetype, grnam2, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, rbuff1, 'QYKW')
          if (ierror /= 0) goto 9999
       endif
       !
       if (roller .or. xbeach) then
          !
          ! element 'FXW'
          !
          call wrtarray_nm(fds, filename, filetype, grnam2, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, fxw, 'FXW')
          if (ierror /= 0) goto 9999
          !
          ! element 'FYW'
          !
          call wrtarray_nm(fds, filename, filetype, grnam2, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, fyw, 'FYW')
          if (ierror /= 0) goto 9999
          !
          ! element 'WSU'
          !
          call wrtarray_nm(fds, filename, filetype, grnam2, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, wsu, 'WSU')
          if (ierror /= 0) goto 9999
          !
          ! element 'WSV'
          !
          call wrtarray_nm(fds, filename, filetype, grnam2, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, wsv, 'WSV')
          if (ierror /= 0) goto 9999
       endif
       !
    end select
    !
    ! write error message if error occured and set error = .true.
    !
9999   continue
    if (ierror /= 0) error = .true.
end subroutine wrrolm
