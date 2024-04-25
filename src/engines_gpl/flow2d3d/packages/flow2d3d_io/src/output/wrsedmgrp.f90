subroutine wrsedmgrp(lundia    ,error     ,filename  ,itmapc    ,mmax      , &
                   & kmax      ,nmaxus    ,lsed      ,lsedtot   , & 
                   & kfsmin    ,kfsmax    ,irequest  ,fds       ,iarrc     , &
                   & mf        ,ml        ,nf        ,nl        ,gdp       )
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
!    Function: Writes the time varying data for bedforms, sediment transport
!              and morphology to the FLOW MAP file
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use dfparall, only: inode, nproc, master
    use datagroups
    use globaldata
    use wrtarray, only: wrtvar
    use netcdf
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    include 'fsm.i'
    include 'tri-dyn.igd'
    integer(pntrsize)               , pointer :: sbuu
    integer(pntrsize)               , pointer :: sbvv
    integer(pntrsize)               , pointer :: ws
    integer(pntrsize)               , pointer :: dps
    integer(pntrsize)               , pointer :: seddif
    logical                         , pointer :: lfbedfrmout
    integer                         , pointer :: celidt
    type (datagroup)                , pointer :: group4
    type (datagroup)                , pointer :: group5
    real(hp)                        , pointer :: morft
    real(fp)                        , pointer :: morfac
    integer                         , pointer :: nmaxgl
    integer                         , pointer :: mmaxgl
    integer                         , pointer :: io_prec
!
! Global variables
!
    integer                                                                           , intent(in)  :: itmapc  !!  Current time counter for the MAP data file
    integer                                                                                         :: kmax    !  Description and declaration in esm_alloc_int.f90
    integer                                                                                         :: lsed    !  Description and declaration in esm_alloc_int.f90
    integer                                                                                         :: lsedtot !  Description and declaration in esm_alloc_int.f90
    integer                                                                                         :: lundia  !  Description and declaration in inout.igs
    integer                                                                                         :: mmax    !  Description and declaration in esm_alloc_int.f90
    integer                                                                                         :: nmaxus  !  Description and declaration in esm_alloc_int.f90
    logical                                                                           , intent(out) :: error   !!  Flag=TRUE if an error is encountered
    character(*)                                                                      , intent(in)  :: filename    !  File name
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: kfsmax      !  Description and declaration in esm_alloc_int.f90
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: kfsmin      !  Description and declaration in esm_alloc_int.f90
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
    integer                                       :: ierror     ! Local error flag
    integer                                       :: filetype
    character(16)                                 :: grnam4
    character(16)                                 :: grnam5
    !
    integer                                       :: iddim_time
    !
    data grnam4/'map-infsed-serie'/
    data grnam5/'map-sed-series'/
!
!! executable statements -------------------------------------------------------
!
    call getdatagroup(gdp, FILOUT_MAP, grnam4, group4)
    call getdatagroup(gdp, FILOUT_MAP, grnam5, group5)
    celidt         => group4%celidt
    !
    mmaxgl         => gdp%gdparall%mmaxgl
    nmaxgl         => gdp%gdparall%nmaxgl
    morft          => gdp%gdmorpar%morft
    morfac         => gdp%gdmorpar%morfac
    lfbedfrmout    => gdp%gdbedformpar%lfbedfrmout
    sbuu           => gdp%gdr_i_ch%sbuu
    sbvv           => gdp%gdr_i_ch%sbvv
    ws             => gdp%gdr_i_ch%ws
    dps            => gdp%gdr_i_ch%dps
    seddif         => gdp%gdr_i_ch%seddif
    io_prec        => gdp%gdpostpr%io_prec
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
       !
       ! map-infsed-serie
       !
       if (filetype == FTYPE_NEFIS) then
          call addelm(gdp, lundia, FILOUT_MAP, grnam4, 'ITMAPS', ' ', IO_INT4    , 0, longname='timestep number (ITMAPC*DT*TUNIT := time in sec from ITDATE)')
       endif
       if (lsedtot > 0) then
          call addelm(gdp, lundia, FILOUT_MAP, grnam4, 'MORFAC', ' ', io_prec , 0, longname='morphological acceleration factor (MORFAC)')
          call addelm(gdp, lundia, FILOUT_MAP, grnam4, 'MORFT' , ' ', IO_REAL8, 0, longname='morphological time (days since start of simulation)', unit='days')
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
       ! Writing of output on every itmapc
       !
       if (filetype == FTYPE_NEFIS) then
          call wrtvar(fds, filename, filetype, grnam4, celidt, &
                    & gdp, ierror, lundia, itmapc, 'ITMAPS')
          if (ierror/=0) goto 9999
       endif
       !
       if (lsedtot > 0) then
          call wrtvar(fds, filename, filetype, grnam4, celidt, &
                    & gdp, ierror, lundia, morfac, 'MORFAC')
          if (ierror/=0) goto 9999
          !
          call wrtvar(fds, filename, filetype, grnam4, celidt, &
                    & gdp, ierror, lundia, morft, 'MORFT')
          if (ierror/=0) goto 9999
       endif
       !
    end select
    !
    ! Add sediment transport and morphology fields
    !
    call wrsedm(lundia    ,error     ,mmax      ,kmax      ,nmaxus    , &
              & lsed      ,lsedtot   ,irequest  ,fds       ,grnam5    , &
              & r(sbuu)   ,r(sbvv)   ,r(ws)     ,d(dps)    ,r(seddif) , &
              & filename  ,gdp       ,filetype  , &
              & mf        ,ml        ,nf        ,nl        , &
              & iarrc     ,kfsmin    ,kfsmax    )
    !
    ! Add bedform fields
    !
    if (lfbedfrmout) then
       call wrsedd(lundia    ,error     ,mmax      ,nmaxus    ,irequest  , &
                 & fds       ,grnam5    ,filename  ,gdp       ,filetype  , &
                 & mf        ,ml        ,nf        ,nl        ,iarrc     )
    endif
    !
    ! write error message if error occured and set error = .true.
    !
9999   continue
    if (ierror /= 0) error = .true.
end subroutine wrsedmgrp
