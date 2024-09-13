subroutine wrxbm(lundia     ,error     ,filename  ,itmapc    ,nmax      , &
                & mmax      ,nmaxus    ,fxw       ,fyw       , &
                & wsu       ,wsv       ,guu       ,gvv       ,rbuff1    , &
                & hrms      ,irequest  ,fds       ,iarrc     ,mf        , &
                & ml        ,nf        ,nl        ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2016.                                
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
!  $Id:  $
!  $HeadURL: $
!!--description-----------------------------------------------------------------
!
! Writes the time varying data for XBeach roller model
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
!
! Global variables
!
    integer                                                                           , intent(in)  :: itmapc
    integer                                                                                         :: lundia      !  Description and declaration in inout.igs
    integer                                                                                         :: mmax        !  Description and declaration in esm_alloc_int.f90
    integer                                                                           , intent(in)  :: nmax        !  Description and declaration in esm_alloc_int.f90
    integer                                                                                         :: nmaxus      !  Description and declaration in esm_alloc_int.f90
    logical                                                                           , intent(out) :: error       
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                     , intent(in)  :: fxw         !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                     , intent(in)  :: fyw         !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                     , intent(in)  :: guu         !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                     , intent(in)  :: gvv         !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                     , intent(in)  :: hrms        !  Description and declaration in esm_alloc_real.f90
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
    integer    , dimension(3,5)                   :: uindex
    integer                        , external     :: neferr
    integer                        , external     :: putelt
    integer                        , external     :: clsnef
    integer                        , external     :: open_datdef
    character(16)                                 :: grnam1
    character(16)                                 :: grnam2
    character(256)                                :: errmsg
    
    integer                                       :: year
    integer                                       :: month
    integer                                       :: day
    !
    integer                                       :: iddim_time
    integer                                       :: iddim_nmax
    integer                                       :: iddim_mmax
    integer                                       :: iddim_nmaxgl
    integer                                       :: iddim_mmaxgl
    !
    integer                                       :: idvar_time
    !
    integer                                       :: idvar_hs    
    integer                                       :: idvar_fxw   
    integer                                       :: idvar_fyw 
    integer                                       :: idvar_wsu 
    integer                                       :: idvar_wsv 
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
    celidt         => group1%celidt
    !
    mmaxgl         => gdp%gdparall%mmaxgl
    nmaxgl         => gdp%gdparall%nmaxgl
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
       iddim_nmax    = adddim(gdp, lundia, FILOUT_MAP, 'NMAX'   , nmaxgl        ) !'Number of N-grid points            '
       iddim_mmax    = adddim(gdp, lundia, FILOUT_MAP, 'MMAX'   , mmaxgl        ) !'Number of M-grid points            '
       !
       ! map-infrol-serie
       !
       if (filetype == FTYPE_NEFIS) then
          call addelm(gdp, lundia, FILOUT_MAP, grnam1, 'ITMAPS', ' ', IO_INT4, 0, longname='timestep number (ITMAPS*DT*TUNIT := time in sec from ITDATE)')
       endif
       !
       ! map-rol-series
       !
       call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'HS', ' ', IO_REAL4 , 2, dimids=(/iddim_nmax,iddim_mmax/), longname='Significant wave height', unit='m')
       call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'FXW', ' ', IO_REAL4, 2, dimids=(/iddim_nmax,iddim_mmax/), longname='Component of wave force in ksi direction', unit='N/m2')
       call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'FYW', ' ', IO_REAL4, 2, dimids=(/iddim_nmax,iddim_mmax/), longname='Component of wave force in eta direction', unit='N/m2')
       call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'WSU', ' ', IO_REAL4, 2, dimids=(/iddim_nmax,iddim_mmax/), longname='Component of roller force in ksi direction', unit='N/m2')
       call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'WSV', ' ', IO_REAL4, 2, dimids=(/iddim_nmax,iddim_mmax/), longname='Component of roller force in eta direction', unit='N/m2')
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
       ! element 'ITMAPS'
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
       !
    end select
    !
    ! write error message if error occured and set error = .true.
    !
9999   continue
    if (ierror /= 0) error = .true.
end subroutine wrxbm
