subroutine wrsedmavg(lundia    ,error     ,filename  ,itmapc    ,mmax      , &
                   & nmaxus    ,lsed      ,lsedtot   ,irequest  ,fds       , &
                   & iarrc     ,mf        ,ml        ,nf        ,nl        , &
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
!    Function: Writes the time varying data for sediment (4 & 5)
!              to the FLOW MAP file
!              Output is performed conform the times of the map
!              file and only in case lsedtot > 0 (lsed may be zero!).
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use netcdf
    use datagroups
    use globaldata
    use wrtarray, only: wrtarray_nml, wrtvar
    use dfparall, only: inode, master, nproc
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp), dimension(:)          , pointer :: dm
    real(fp), dimension(:,:)        , pointer :: sbuuc
    real(fp), dimension(:,:)        , pointer :: sbvvc
    real(fp), dimension(:,:)        , pointer :: ssuuc
    real(fp), dimension(:,:)        , pointer :: ssvvc
    real(hp)                        , pointer :: hydrt
    real(hp)                        , pointer :: hydrt0
    real(hp)                        , pointer :: morft
    real(hp)                        , pointer :: morft0
    real(fp)                        , pointer :: sus
    real(fp)                        , pointer :: bed
    type (moroutputtype)            , pointer :: moroutput
    integer                         , pointer :: celidt
    type (datagroup)                , pointer :: group6
    type (datagroup)                , pointer :: group7
    real(fp)      , dimension(:)    , pointer :: rhosol
    real(fp)      , dimension(:)    , pointer :: cdryb
    integer                         , pointer :: nmaxgl
    integer                         , pointer :: mmaxgl
    integer                         , pointer :: io_prec
!
! Global variables
!
    integer                                                                           , intent(in)  :: lsed        !  Description and declaration in esm_alloc_int.f90
    integer                                                                           , intent(in)  :: lsedtot     !  Description and declaration in esm_alloc_int.f90
    integer                                                                                         :: lundia      !  Description and declaration in inout.igs
    integer                                                                           , intent(in)  :: mmax        !  Description and declaration in esm_alloc_int.f90
    integer                                                                           , intent(in)  :: nmaxus      !  Description and declaration in esm_alloc_int.f90
    integer                                                                           , intent(in)  :: itmapc
    logical                                                                           , intent(out) :: error       !!  Flag=TRUE if an error is encountered
    character(60)                                                                     , intent(in)  :: filename    !!  File name
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
    real(hp)                                      :: moravg
    real(hp)                                      :: dmorft
    real(fp)                                      :: dmorfs
    real(fp)                                      :: rhodt
    integer                                       :: ierror     ! Local error flag
    integer                                       :: filetype
    integer                                       :: i
    integer                                       :: l          ! Help var. 
    integer                                       :: m          ! Help var. 
    integer                                       :: n          ! Help var. 
    integer                                       :: nm         ! Help var. 
    integer, dimension(1)                         :: idummy     ! Help array to write integers
    integer, external                             :: clsnef
    integer, external                             :: open_datdef
    integer, external                             :: neferr
    real(fp)   , dimension(:,:,:)  , allocatable  :: rbuff3
    character(16)                                 :: grnam6
    character(16)                                 :: grnam7
    character(256)                                :: errmsg      ! Character var. containing the errormessage to be written to file. The message depends on the error. 
    character(1024)                               :: error_string
    !
    integer                                       :: iddim_group
    integer                                       :: iddim_n
    integer                                       :: iddim_nc
    integer                                       :: iddim_m
    integer                                       :: iddim_mc
    integer                                       :: iddim_lsed
    integer                                       :: iddim_lsedtot
!
! Data statements
!
    data grnam6/'map-infavg-serie'/
    data grnam7/'map-avg-series'/
!
!! executable statements -------------------------------------------------------
!
    call getdatagroup(gdp, FILOUT_MAP, grnam6, group6)
    call getdatagroup(gdp, FILOUT_MAP, grnam7, group7)
    celidt              => group6%celidt
    !
    dm                  => gdp%gderosed%dm
    sbuuc               => gdp%gderosed%e_sbnc
    sbvvc               => gdp%gderosed%e_sbtc
    ssuuc               => gdp%gderosed%e_ssnc
    ssvvc               => gdp%gderosed%e_sstc
    hydrt               => gdp%gdmorpar%hydrt
    hydrt0              => gdp%gdmorpar%hydrt0
    morft               => gdp%gdmorpar%morft
    morft0              => gdp%gdmorpar%morft0
    sus                 => gdp%gdmorpar%sus
    bed                 => gdp%gdmorpar%bed
    moroutput           => gdp%gdmorpar%moroutput
    rhosol              => gdp%gdsedpar%rhosol
    cdryb               => gdp%gdsedpar%cdryb
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
       if (moroutput%cumavg) then
          iddim_group   = adddim(gdp, lundia, FILOUT_MAP, 'time'   , nf90_unlimited)
       else
          iddim_group   = adddim(gdp, lundia, FILOUT_MAP, 'avgtime', 1)
       endif
       iddim_n       = adddim(gdp, lundia, FILOUT_MAP, 'N'      , nmaxgl        ) ! Number of N-grid points (cell centes)
       iddim_nc      = adddim(gdp, lundia, FILOUT_MAP, 'NC'     , nmaxgl        ) ! Number of N-grid points (corner points)
       iddim_m       = adddim(gdp, lundia, FILOUT_MAP, 'M'      , mmaxgl        ) ! Number of M-grid points (cell centes)
       iddim_mc      = adddim(gdp, lundia, FILOUT_MAP, 'MC'     , mmaxgl        ) ! Number of M-grid points (corner points)
       iddim_lsed    = adddim(gdp, lundia, FILOUT_MAP, 'LSED'   , lsed          ) ! Number of sediment constituents
       iddim_lsedtot = adddim(gdp, lundia, FILOUT_MAP, 'LSEDTOT', lsedtot       ) ! Number of total sediment fractions
       !
       ! map-infavg-serie
       !
       if (filetype == FTYPE_NEFIS) then
          call addelm(gdp, lundia, FILOUT_MAP, grnam6, 'ITAVGS', ' ', IO_INT4, 0, longname='timestep number (ITAVG*DT*TUNIT := time in sec from ITDATE)')
       endif
       call addelm(gdp, lundia, FILOUT_MAP, grnam6, 'MFTAVG', ' ', IO_REAL8, 0, longname='morphological time (days since start of simulation)', unit='days')
       call addelm(gdp, lundia, FILOUT_MAP, grnam6, 'MORAVG', ' ', io_prec , 0, longname='average MORFAC used during averaging period')
       !
       ! map-avg-series
       !
       call addelm(gdp, lundia, FILOUT_MAP, grnam7, 'SBUUA', ' ', io_prec    , 3, dimids=(/iddim_n , iddim_mc, iddim_lsedtot/), longname='Average bed-load transport u-direction (u point)', unit=moroutput%unit_transport_rate, acl='u')
       call addelm(gdp, lundia, FILOUT_MAP, grnam7, 'SBVVA', ' ', io_prec    , 3, dimids=(/iddim_nc, iddim_m , iddim_lsedtot/), longname='Average bed-load transport v-direction (v point)', unit=moroutput%unit_transport_rate, acl='v')
       if (lsed > 0) then
          call addelm(gdp, lundia, FILOUT_MAP, grnam7, 'SSUUA', ' ', io_prec , 3, dimids=(/iddim_n , iddim_mc, iddim_lsed/), longname='Average suspended-load transport u-direction (u point)', unit=moroutput%unit_transport_rate, acl='u')
          call addelm(gdp, lundia, FILOUT_MAP, grnam7, 'SSVVA', ' ', io_prec , 3, dimids=(/iddim_nc, iddim_m , iddim_lsed/), longname='Average suspended-load transport v-direction (v point)', unit=moroutput%unit_transport_rate, acl='v')
       endif
       !
       group6%grp_dim = iddim_group
       group7%grp_dim = iddim_group
       celidt = 0
       !
    case (REQUESTTYPE_WRITE)
       !
       celidt = celidt + 1
       group7%celidt = celidt
       !
       ! Writing of output on every itmapc/time
       !
       if (filetype == FTYPE_NEFIS) then
          call wrtvar(fds, filename, filetype, grnam6, celidt, &
                    & gdp, ierror, lundia, itmapc, 'ITAVGS')
          if (ierror/=0) goto 9999
       endif
       !
       ! element 'MFTAVG'
       !
       call wrtvar(fds, filename, filetype, grnam6, celidt, &
                 & gdp, ierror, lundia, morft, 'MFTAVG')
       if (ierror/=0) goto 9999
       !
       dmorft = morft - morft0
       dmorfs = real(dmorft*86400.0_hp,fp)
       !
       ! element 'MORAVG'
       !
       if (hydrt > hydrt0) then
          moravg = dmorft/(hydrt - hydrt0)
       else
          moravg = 0.0_hp
       endif
       call wrtvar(fds, filename, filetype, grnam6, celidt, &
                 & gdp, ierror, lundia, moravg, 'MORAVG')
       if (ierror/=0) goto 9999
       !
       ! element 'SBUUA'
       !
       allocate( rbuff3(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, lsedtot) )
       if ( dmorft > 0.0_hp ) then
          rbuff3(:, :, :) = -999.0_fp
          do l = 1, lsedtot
             select case(moroutput%transptype)
             case (0)
                rhodt = dmorfs
             case (1)
                rhodt = cdryb(l)*dmorfs
             case (2)
                rhodt = rhosol(l)*dmorfs
             end select
             do m = 1, mmax
                do n = 1, nmaxus
                   call n_and_m_to_nm(n, m, nm, gdp)
                   rbuff3(n, m, l) = sbuuc(nm, l)/rhodt
                enddo
             enddo
          enddo
       else
          rbuff3(:,:,:) = 0.0_fp
       endif
       call wrtarray_nml(fds, filename, filetype, grnam7, celidt, &
                     & nf, nl, mf, ml, iarrc, gdp, lsedtot, &
                     & ierror, lundia, rbuff3, 'SBUUA')
       if (ierror/= 0) goto 9999
       !
       ! element 'SBVVA'
       !
       if ( dmorft > 0.0_hp ) then
          rbuff3(:, :, :) = -999.0_fp
          do l = 1, lsedtot
             select case(moroutput%transptype)
             case (0)
                rhodt = dmorfs
             case (1)
                rhodt = cdryb(l)*dmorfs
             case (2)
                rhodt = rhosol(l)*dmorfs
             end select
             do m = 1, mmax
                do n = 1, nmaxus
                   call n_and_m_to_nm(n, m, nm, gdp)
                   rbuff3(n, m, l) = sbvvc(nm, l)/rhodt
                enddo
             enddo
          enddo
       else
          rbuff3(:,:,:) = 0.0_fp
       endif
       call wrtarray_nml(fds, filename, filetype, grnam7, celidt, &
                     & nf, nl, mf, ml, iarrc, gdp, lsedtot, &
                     & ierror, lundia, rbuff3, 'SBVVA')
       deallocate(rbuff3)
       if (ierror/= 0) goto 9999
       !
       if (lsed > 0) then
          !
          ! element 'SSUUA'
          !
          allocate( rbuff3(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, lsed) )
          if ( dmorft > 0.0_hp ) then
             rbuff3(:, :, :) = -999.0_fp
             do l = 1, lsedtot
                select case(moroutput%transptype)
                case (0)
                   rhodt = dmorfs
                case (1)
                   rhodt = cdryb(l)*dmorfs
                case (2)
                   rhodt = rhosol(l)*dmorfs
                end select
                do m = 1, mmax
                   do n = 1, nmaxus
                      call n_and_m_to_nm(n, m, nm, gdp)
                      rbuff3(n, m, l) = ssuuc(nm, l)/rhodt
                   enddo
                enddo
             enddo
          else
             rbuff3(:,:,:) = 0.0_fp
          endif
          call wrtarray_nml(fds, filename, filetype, grnam7, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, lsedtot, &
                        & ierror, lundia, rbuff3, 'SSUUA')
          if (ierror/= 0) goto 9999
          !
          ! element 'SSVVA'
          !
          if ( dmorft > 0.0_hp ) then
             rbuff3(:, :, :) = -999.0_fp
             do l = 1, lsedtot
                select case(moroutput%transptype)
                case (0)
                   rhodt = dmorfs
                case (1)
                   rhodt = cdryb(l)*dmorfs
                case (2)
                   rhodt = rhosol(l)*dmorfs
                end select
                do m = 1, mmax
                   do n = 1, nmaxus
                      call n_and_m_to_nm(n, m, nm, gdp)
                      rbuff3(n, m, l) = ssvvc(nm, l)/rhodt
                   enddo
                enddo
             enddo
          else
             rbuff3(:,:,:) = 0.0_fp
          endif
          call wrtarray_nml(fds, filename, filetype, grnam7, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, lsedtot, &
                        & ierror, lundia, rbuff3, 'SSVVA')
          deallocate(rbuff3)
          if (ierror/= 0) goto 9999
       endif
       !
    end select
    !
    ! write error message if error occured and set error = .true.
    !
9999   continue
    if (ierror /= 0) error = .true.
end subroutine wrsedmavg
