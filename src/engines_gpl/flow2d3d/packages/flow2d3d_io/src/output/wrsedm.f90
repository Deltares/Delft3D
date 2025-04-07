subroutine wrsedm(lundia    ,error     ,mmax      ,kmax      ,nmaxus    , &
                & lsed      ,lsedtot   ,irequest  ,fds       ,grpnam    , &
                & sbuu      ,sbvv      ,ws        ,dps       ,seddif    , & 
                & filename  ,gdp       ,filetype  , &
                & mf        ,ml        ,nf        ,nl        , &
                & iarrc     ,kfsmin    ,kfsmax    )
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
!    Function: Writes the time varying data for sediment transport to the
!              sediment group on the FLOW MAP file
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use sp_buffer
    use dfparall, only: nproc
    use datagroups
    use globaldata
    use wrtarray, only: wrtarray_nm_ptr, wrtarray_nml_ptr, wrtarray_nm, wrtarray_nml, wrtarray_nmkl, wrtarray_nm_2d
    use dffunctionals
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                              , pointer :: celidt
    type (datagroup)                     , pointer :: group
    integer                              , pointer :: nxx
    integer                              , pointer :: io_prec
    integer  , dimension(:)              , pointer :: smlay
    type (moroutputtype)                 , pointer :: moroutput
    logical                              , pointer :: scour
    logical                              , pointer :: lfsdu
    real(fp), dimension(:)               , pointer :: xx
    real(fp), dimension(:)               , pointer :: rhosol
    real(fp), dimension(:)               , pointer :: cdryb
    real(fp), dimension(:)               , pointer :: dm
    real(fp), dimension(:)               , pointer :: dg
    real(fp), dimension(:)               , pointer :: dgsd
    real(fp), dimension(:,:)             , pointer :: dxx
    real(fp), dimension(:)               , pointer :: dzduu
    real(fp), dimension(:)               , pointer :: dzdvv
    real(fp), dimension(:,:)             , pointer :: fixfac
    real(fp), dimension(:,:)             , pointer :: frac
    real(fp), dimension(:)               , pointer :: sdu_t0
    real(fp), dimension(:)               , pointer :: sdu_tn
    real(fp), dimension(:)               , pointer :: mudfrac
    real(fp), dimension(:)               , pointer :: sandfrac
    real(fp), dimension(:,:)             , pointer :: hidexp
    real(fp), dimension(:,:)             , pointer :: aks
    real(fp), dimension(:,:)             , pointer :: rca
    real(fp), dimension(:,:)             , pointer :: rsedeq
    real(fp), dimension(:,:)             , pointer :: sbcu
    real(fp), dimension(:,:)             , pointer :: sbcv
    real(fp), dimension(:,:)             , pointer :: sbcuu
    real(fp), dimension(:,:)             , pointer :: sbcvv
    real(fp), dimension(:,:)             , pointer :: sbwu
    real(fp), dimension(:,:)             , pointer :: sbwv
    real(fp), dimension(:,:)             , pointer :: sbwuu
    real(fp), dimension(:,:)             , pointer :: sbwvv
    real(fp), dimension(:,:)             , pointer :: ssuu
    real(fp), dimension(:,:)             , pointer :: ssvv
    real(fp), dimension(:,:)             , pointer :: sswu
    real(fp), dimension(:,:)             , pointer :: sswv
    real(fp), dimension(:,:)             , pointer :: sswuu
    real(fp), dimension(:,:)             , pointer :: sswvv
    real(fp), dimension(:,:)             , pointer :: sucor
    real(fp), dimension(:,:)             , pointer :: svcor
    real(fp), dimension(:,:)             , pointer :: sinkse
    real(fp), dimension(:,:)             , pointer :: sourse
    real(fp), dimension(:)               , pointer :: taub
    real(fp), dimension(:,:)             , pointer :: taurat
    real(fp), dimension(:)               , pointer :: ust2
    real(fp), dimension(:)               , pointer :: umod
    real(fp), dimension(:)               , pointer :: uuu
    real(fp), dimension(:)               , pointer :: vvv
    real(fp), dimension(:)               , pointer :: zumod
    integer          , dimension(:)      , pointer :: noutpar
    integer          , dimension(:,:)    , pointer :: ioutpar
    real(fp)         , dimension(:,:)    , pointer :: outpar
    character(256)   , dimension(:,:)    , pointer :: outpar_name
    character(256)   , dimension(:,:)    , pointer :: outpar_longname
    integer                              , pointer :: nmaxgl
    integer                              , pointer :: mmaxgl
!
! Global variables
!
    integer                                                                    , intent(in)  :: irequest !! Action flag: REQUESTTYPE_DEFINE = define, REQUESTTYPE_WRITE = write
    character(16)                                                              , intent(in)  :: grpnam  !!  Group name
    integer                                                                    , intent(in)  :: kmax    !  Description and declaration in esm_alloc_int.f90
    integer                                                                    , intent(in)  :: lsed    !  Description and declaration in esm_alloc_int.f90
    integer                                                                    , intent(in)  :: lsedtot !  Description and declaration in esm_alloc_int.f90
    integer                                                                    , intent(in)  :: lundia  !  Description and declaration in inout.igs
    integer                                                                    , intent(in)  :: mmax    !  Description and declaration in esm_alloc_int.f90
    integer                                                                    , intent(in)  :: nmaxus  !  Description and declaration in esm_alloc_int.f90
    logical                                                                    , intent(out) :: error   !!  Flag=TRUE if an error is encountered
    real(prec), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)            , intent(in)  :: dps     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax, lsed), intent(in)  :: ws      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, lsedtot)     , intent(in)  :: sbuu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, lsedtot)     , intent(in)  :: sbvv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax, lsed), intent(in)  :: seddif  !  Description and declaration in esm_alloc_real.f90
    integer                                                                    , intent(in)  :: fds    

    integer                                                                    , intent(in)  :: filetype
    integer    , dimension(0:nproc-1)                                          , intent(in)  :: mf      ! first index w.r.t. global grid in x-direction
    integer    , dimension(0:nproc-1)                                          , intent(in)  :: ml      ! last index w.r.t. global grid in x-direction
    integer    , dimension(0:nproc-1)                                          , intent(in)  :: nf      ! first index w.r.t. global grid in y-direction
    integer    , dimension(0:nproc-1)                                          , intent(in)  :: nl      ! last index w.r.t. global grid in y-direction
    integer    , dimension(4,0:nproc-1)                                        , intent(in)  :: iarrc   ! array containing collected grid indices
    integer    , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)           , intent(in)  :: kfsmin  ! Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)           , intent(in)  :: kfsmax  ! Description and declaration in esm_alloc_int.f90

!
! Local variables
!
    real(fp)                                      :: rhol
    real(fp)                                      :: tauadd
    integer                                       :: ierror     ! Local error flag
    integer                                       :: i
    integer                                       :: k          ! Help var. 
    integer                                       :: kmaxout    ! number of layers to be written to the (history) output files
    integer                                       :: kmaxout_restr ! number of layers to be written to the (history) output files, 0 excluded
    integer                                       :: l          ! Help var. 
    integer                                       :: m          ! Help var. 
    integer                                       :: n          ! Help var. 
    integer                                       :: nm         ! Help var. 
    integer    , dimension(:)      , allocatable  :: smlay_restr   ! copy of smlay, excluding layer zero
    integer    , dimension(3,5)                   :: uindex
    real(fp)   , dimension(:,:)    , allocatable  :: rbuff2
    real(fp)   , dimension(:,:,:)  , allocatable  :: rbuff3
    character(3)                                  :: sednr
    character(16)                                 :: dxname
    character(256)                                :: errmsg      ! Character var. containing the errormessage to be written to file. The message depends on the error. 
    character(64)                                 :: dxdescr
    !
    integer                                       :: iddim_n
    integer                                       :: iddim_nc
    integer                                       :: iddim_m
    integer                                       :: iddim_mc
    integer                                       :: iddim_kmax
    integer                                       :: iddim_kmaxout
    integer                                       :: iddim_lsed
    integer                                       :: iddim_lsedtot  
    !
    character(256)                                :: filename
    character(256)                                :: string
!
!! executable statements -------------------------------------------------------
!
    call getdatagroup(gdp, FILOUT_MAP, grpnam, group)
    celidt         => group%celidt
    nxx            => gdp%gdmorpar%nxx
    io_prec        => gdp%gdpostpr%io_prec
    smlay          => gdp%gdpostpr%smlay
    moroutput      => gdp%gdmorpar%moroutput
    xx             => gdp%gdmorpar%xx
    rhosol         => gdp%gdsedpar%rhosol
    cdryb          => gdp%gdsedpar%cdryb
    scour          => gdp%gdscour%scour
    dm             => gdp%gderosed%dm
    dg             => gdp%gderosed%dg
    dgsd           => gdp%gderosed%dgsd
    dxx            => gdp%gderosed%dxx
    dzduu          => gdp%gderosed%e_dzdn
    dzdvv          => gdp%gderosed%e_dzdt
    fixfac         => gdp%gderosed%fixfac
    frac           => gdp%gderosed%frac
    mudfrac        => gdp%gderosed%mudfrac
    sandfrac       => gdp%gderosed%sandfrac
    hidexp         => gdp%gderosed%hidexp
    aks            => gdp%gderosed%aks
    rca            => gdp%gderosed%rca
    rsedeq         => gdp%gderosed%rsedeq
    sbcu           => gdp%gderosed%sbcx
    sbcv           => gdp%gderosed%sbcy
    sbcuu          => gdp%gderosed%e_sbcn
    sbcvv          => gdp%gderosed%e_sbct
    sbwu           => gdp%gderosed%sbwx
    sbwv           => gdp%gderosed%sbwy
    sbwuu          => gdp%gderosed%e_sbwn
    sbwvv          => gdp%gderosed%e_sbwt
    ssuu           => gdp%gderosed%e_ssn
    ssvv           => gdp%gderosed%e_sst
    sswu           => gdp%gderosed%sswx
    sswv           => gdp%gderosed%sswy
    sswuu          => gdp%gderosed%e_sswn
    sswvv          => gdp%gderosed%e_sswt
    sucor          => gdp%gderosed%e_scrn
    svcor          => gdp%gderosed%e_scrt
    sinkse         => gdp%gderosed%sinkse
    sourse         => gdp%gderosed%sourse
    taub           => gdp%gderosed%taub
    taurat         => gdp%gderosed%taurat
    ust2           => gdp%gderosed%ust2
    umod           => gdp%gderosed%umod
    uuu            => gdp%gderosed%uuu
    vvv            => gdp%gderosed%vvv
    zumod          => gdp%gderosed%zumod
    noutpar        => gdp%gdtrapar%noutpar
    ioutpar        => gdp%gdtrapar%ioutpar
    outpar         => gdp%gdtrapar%outpar
    outpar_name    => gdp%gdtrapar%outpar_name
    outpar_longname=> gdp%gdtrapar%outpar_longname
    mmaxgl         => gdp%gdparall%mmaxgl
    nmaxgl         => gdp%gdparall%nmaxgl
    lfsdu          => gdp%gdprocs%lfsdu
    sdu_t0         => gdp%gdsdu%sdu_t0
    sdu_tn         => gdp%gdsdu%sdu_tn
    !
    kmaxout = size(smlay)
    if (smlay(1) == 0) then
       kmaxout_restr = kmaxout - 1
       allocate(smlay_restr(kmaxout_restr))
       smlay_restr   = smlay(2:)
    else
       kmaxout_restr = kmaxout
       allocate(smlay_restr(kmaxout_restr))
       smlay_restr   = smlay
    endif
    !
    select case (irequest)
    case (REQUESTTYPE_DEFINE)
       !
       ! Define dimensions
       !
       iddim_n       = adddim(gdp, lundia, FILOUT_MAP, 'N'      , nmaxgl        ) ! Number of N-grid points (cell centres)
       iddim_nc      = adddim(gdp, lundia, FILOUT_MAP, 'NC'     , nmaxgl        ) ! Number of N-grid points (corner points)
       iddim_m       = adddim(gdp, lundia, FILOUT_MAP, 'M'      , mmaxgl        ) ! Number of M-grid points (cell centres)
       iddim_mc      = adddim(gdp, lundia, FILOUT_MAP, 'MC'     , mmaxgl        ) ! Number of M-grid points (corner points)
       iddim_kmax    = adddim(gdp, lundia, FILOUT_MAP, 'KMAX'   , kmax          ) !'Number of layers                   '
       iddim_kmaxout = adddim(gdp, lundia, FILOUT_MAP, 'KMAXOUT', kmaxout       ) !'Number of layers written           '
       iddim_lsed    = adddim(gdp, lundia, FILOUT_MAP, 'LSED'   , lsed          ) !'Number of sediment constituents    '
       iddim_lsedtot = adddim(gdp, lundia, FILOUT_MAP, 'LSEDTOT', lsedtot       ) !'Number of total sediment fractions '
       !
       ! Define elements 
       !
       if (lsed > 0) then
          if (moroutput%ws) then
              call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'WS', ' ', io_prec        , 4, dimids=(/iddim_n, iddim_m, iddim_kmaxout, iddim_lsed/), longname='Settling velocity per layer', unit='m/s', acl='z')
          endif
          if (kmax==1 .and. moroutput%rsedeq) then
             call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'RSEDEQ', ' ', io_prec , 4, dimids=(/iddim_n, iddim_m, iddim_kmax, iddim_lsed/), longname='Equilibrium concentration of sediment (2D only)', unit='kg/m3', acl='z')
          endif
       endif
       if (moroutput%uuuvvv) then
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'UUU', ' ', io_prec       , 2, dimids=(/iddim_n , iddim_mc/), longname='Characteristic velocity u-direction (zeta point)', unit='m/s', acl='u')
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'VVV', ' ', io_prec       , 2, dimids=(/iddim_nc, iddim_m /), longname='Characteristic velocity v-direction (zeta point)', unit='m/s', acl='v')
       endif
       if (moroutput%umod) then
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'UMOD', ' ', io_prec      , 2, dimids=(/iddim_n, iddim_m/), longname='Characteristic velocity magnitude (zeta point)', unit='m/s', acl='z')
       endif
       if (moroutput%zumod) then
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'ZUMOD', ' ', io_prec     , 2, dimids=(/iddim_n, iddim_m/), longname='Height above bed for characteristic velocity (zeta point)', unit='m/s', acl='z')
       endif
       if (moroutput%ustar) then
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'USTAR', ' ', io_prec     , 2, dimids=(/iddim_n, iddim_m/), longname='Bed shear velocity U* (zeta point)', unit='m/s', acl='z')
       endif
       if (moroutput%sbcuv) then
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'SBCU', ' ', io_prec      , 3, dimids=(/iddim_n, iddim_m, iddim_lsedtot/), longname='Bed-load transport u-direction due to currents (zeta point)', unit=moroutput%unit_transport_rate, acl='z')
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'SBCV', ' ', io_prec      , 3, dimids=(/iddim_n, iddim_m, iddim_lsedtot/), longname='Bed-load transport v-direction due to currents (zeta point)', unit=moroutput%unit_transport_rate, acl='z')
       endif
       if (moroutput%sbcuuvv) then
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'SBCUU', ' ', io_prec     , 3, dimids=(/iddim_n , iddim_mc, iddim_lsedtot/), longname='Bed-load transport u-direction due to currents (u point)', unit=moroutput%unit_transport_rate, acl='u')
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'SBCVV', ' ', io_prec     , 3, dimids=(/iddim_nc, iddim_m , iddim_lsedtot/), longname='Bed-load transport v-direction due to currents (v point)', unit=moroutput%unit_transport_rate, acl='v')
       endif
       if (moroutput%sbwuv) then
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'SBWU', ' ', io_prec      , 3, dimids=(/iddim_n, iddim_m, iddim_lsedtot/), longname='Bed-load transport u-direction due to waves (zeta point)', unit=moroutput%unit_transport_rate, acl='z')
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'SBWV', ' ', io_prec      , 3, dimids=(/iddim_n, iddim_m, iddim_lsedtot/), longname='Bed-load transport v-direction due to waves (zeta point)', unit=moroutput%unit_transport_rate, acl='z')
       endif
       if (moroutput%sbwuuvv) then
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'SBWUU', ' ', io_prec     , 3, dimids=(/iddim_n , iddim_mc, iddim_lsedtot/), longname='Bed-load transport u-direction due to waves (u point)', unit=moroutput%unit_transport_rate, acl='u')
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'SBWVV', ' ', io_prec     , 3, dimids=(/iddim_nc, iddim_m , iddim_lsedtot/), longname='Bed-load transport v-direction due to waves (v point)', unit=moroutput%unit_transport_rate, acl='v')
       endif
       if (moroutput%sswuv) then
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'SSWU', ' ', io_prec      , 3, dimids=(/iddim_n, iddim_m, iddim_lsedtot/), longname='Suspended transport u-direction due to waves (zeta point)', unit=moroutput%unit_transport_rate, acl='z')
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'SSWV', ' ', io_prec      , 3, dimids=(/iddim_n, iddim_m, iddim_lsedtot/), longname='Suspended transport v-direction due to waves (zeta point)', unit=moroutput%unit_transport_rate, acl='z')
       endif
       if (moroutput%sswuuvv) then
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'SSWUU', ' ', io_prec     , 3, dimids=(/iddim_n , iddim_mc, iddim_lsedtot/), longname='Suspended transport u-direction due to waves (u point)', unit=moroutput%unit_transport_rate, acl='u')
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'SSWVV', ' ', io_prec     , 3, dimids=(/iddim_nc, iddim_m , iddim_lsedtot/), longname='Suspended transport v-direction due to waves (v point)', unit=moroutput%unit_transport_rate, acl='v')
       endif
       if (lsedtot > 0 .and. moroutput%sbuuvv) then
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'SBUU', ' ', io_prec         , 3, dimids=(/iddim_n , iddim_mc, iddim_lsedtot/), longname='Bed-load transport u-direction (u point)', unit=moroutput%unit_transport_rate, acl='u')
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'SBVV', ' ', io_prec         , 3, dimids=(/iddim_nc, iddim_m , iddim_lsedtot/), longname='Bed-load transport v-direction (v point)', unit=moroutput%unit_transport_rate, acl='v')
       endif
       if (lsed > 0) then
          if (moroutput%seddif) then
             call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'SEDDIF', ' ', io_prec      , 4, dimids=(/iddim_n , iddim_m, iddim_kmaxout, iddim_lsed/), longname='Vertical sediment diffusion (zeta point)', unit='m2/s', acl='z')
          endif
          if (moroutput%ssuuvv) then
             call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'SSUU', ' ', io_prec      , 3, dimids=(/iddim_n , iddim_mc, iddim_lsed/), longname='Suspended-load transport u-direction (u point)', unit=moroutput%unit_transport_rate, acl='u')
             call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'SSVV', ' ', io_prec      , 3, dimids=(/iddim_nc, iddim_m , iddim_lsed/), longname='Suspended-load transport v-direction (v point)', unit=moroutput%unit_transport_rate, acl='v')
          endif
          if (moroutput%suvcor) then
             call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'SUCOR', ' ', io_prec  , 3, dimids=(/iddim_n , iddim_mc, iddim_lsed/), longname='Near-bed transport correction u-direction (u point)', unit=moroutput%unit_transport_rate, acl='u')
             call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'SVCOR', ' ', io_prec  , 3, dimids=(/iddim_nc, iddim_m , iddim_lsed/), longname='Near-bed transport correction v-direction (v point)', unit=moroutput%unit_transport_rate, acl='v')
          endif
          if (moroutput%sourcesink) then
             call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'SOURSE', ' ', io_prec , 3, dimids=(/iddim_n, iddim_m, iddim_lsed/), longname='Source term suspended sediment fractions', unit='kg/(m3 s)', acl='z')
             call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'SINKSE', ' ', io_prec , 3, dimids=(/iddim_n, iddim_m, iddim_lsed/), longname='Sink term suspended sediment fractions', unit='1/s', acl='z')
          endif
          if (moroutput%aks) then
             call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'AKS', ' ', io_prec    , 3, dimids=(/iddim_n, iddim_m, iddim_lsed/), longname='Near-bed reference concentration height', unit='m', acl='z')
          endif
          if (moroutput%rca) then
             call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'RCA', ' ', io_prec       , 3, dimids=(/iddim_n, iddim_m, iddim_lsed/), longname='Near-bed reference concentration of sediment', unit='kg/m3', acl='z')
          endif
       endif
       call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'DPS', ' ', io_prec          , 2, dimids=(/iddim_n, iddim_m/), longname='Bottom depth (zeta point)', unit='m', acl='z')
       if (lfsdu) then       
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'SDU', ' ', io_prec          , 2, dimids=(/iddim_n, iddim_m/), longname='Cumulative bed level change due to subsidence/uplift', unit='m', acl='z')
       endif    
       if (moroutput%dzduuvv) then
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'DZDUU', ' ', io_prec     , 2, dimids=(/iddim_n , iddim_mc/), longname='Bed slope in u-direction (u point)', acl='u')
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'DZDVV', ' ', io_prec     , 2, dimids=(/iddim_nc, iddim_m /), longname='Bed slope in v-direction (v point)', acl='v')
       endif
       if (scour) then
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'TAUADD', ' ', io_prec    , 2, dimids=(/iddim_n, iddim_m/), longname='Extra shear stress due to scour feature', unit='N/m2', acl='z')
       endif
       if (moroutput%taub) then
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'TAUB', ' ', io_prec    , 2, dimids=(/iddim_n, iddim_m/), longname='Bed shear stress used for morphology', unit='N/m2', acl='z')
       endif
       if (moroutput%taurat) then
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'TAURAT', ' ', io_prec    , 3, dimids=(/iddim_n, iddim_m, iddim_lsedtot/), longname='Excess bed shear ratio', acl='z')
       endif
       if (moroutput%dm) then
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'DM', ' ', io_prec        , 2, dimids=(/iddim_n, iddim_m/), longname='Arithmetic mean sediment diameter', unit='m', acl='z')
       endif
       if (moroutput%dg) then
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'DG', ' ', io_prec        , 2, dimids=(/iddim_n, iddim_m/), longname='Geometric mean sediment diameter', unit='m', acl='z')
       endif
       if (moroutput%dgsd) then
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'DGSD', ' ', io_prec      , 2, dimids=(/iddim_n, iddim_m/), longname='Geometric standard deviation of particle size mix', unit='m', acl='z')
       endif
       if (moroutput%percentiles) then
          do l = 1, nxx
             write(dxname,'(A,I2.2)') 'DXX',l
             write(dxdescr,'(A,F4.1,A)') 'Sediment diameter percentile '    , &
                & xx(l)*100.0,' %'
             call addelm(gdp, lundia, FILOUT_MAP, grpnam, dxname, ' ', io_prec   , 2, dimids=(/iddim_n, iddim_m/), longname=dxdescr, unit='m')
          enddo
       endif
       if (moroutput%frac) then
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'FRAC', ' ', io_prec      , 3, dimids=(/iddim_n, iddim_m, iddim_lsedtot/), longname='Availability fraction in top layer', acl='z')
       endif
       if (moroutput%mudfrac) then
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'MUDFRAC', ' ', io_prec   , 2, dimids=(/iddim_n, iddim_m/), longname='Mud fraction in top layer', acl='z')
       endif
       if (moroutput%sandfrac) then
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'SANDFRAC', ' ', io_prec  , 2, dimids=(/iddim_n, iddim_m/), longname='Sand fraction in top layer', acl='z')
       endif
       if (moroutput%fixfac) then
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'FIXFAC', ' ', io_prec    , 3, dimids=(/iddim_n, iddim_m, iddim_lsedtot/), longname='Reduction factor due to limited sediment thickness', acl='z')
       endif
       if (moroutput%hidexp) then
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'HIDEXP', ' ', io_prec    , 3, dimids=(/iddim_n, iddim_m, iddim_lsedtot/), longname='Hiding and exposure factor', acl='z')
       endif
       if (moroutput%sedpar) then
          do l = 1, lsedtot
             write(sednr,'(I3.3)') l
             do k = 1, noutpar(l)
                call addelm(gdp, lundia, FILOUT_MAP, grpnam, trim(outpar_name(k,l))//trim(sednr), ' ', io_prec    , 2, dimids=(/iddim_n, iddim_m/), longname=trim(outpar_longname(k,l))//' for '//trim(gdp%gdsedpar%namsed(l)), acl='z')
             enddo
          enddo
       endif
       !
       ! Add mor fields
       !
       if (lsedtot > 0) then
          call wrmorm(lundia    ,error     ,mmax      ,nmaxus    ,lsedtot   , &
                    & REQUESTTYPE_DEFINE   ,fds       ,grpnam    , &
                    & filename  ,gdp       ,filetype  , &
                    & mf        ,ml        ,nf        ,nl        ,iarrc     )
       endif
       !
       ! Add fluff fields  
       !
       if (lsed > 0) then
          call wrmfluff(lundia    ,error     ,mmax      ,nmaxus    ,lsed      , &
                      & REQUESTTYPE_DEFINE   ,fds       ,grpnam    , &
                      & filename  ,gdp       ,filetype  , &
                      & mf        ,ml        ,nf        ,nl        ,iarrc     )
       endif
       !
       ! Add statistics fields  
       !
       if (lsed > 0) then
          call wrmorst(lundia    ,error     ,mmax      ,nmaxus    ,lsedtot   , &
                     & REQUESTTYPE_DEFINE   ,fds       ,grpnam    , &
                     & filename  ,gdp       ,filetype  , &
                     & mf        ,ml        ,nf        ,nl        ,iarrc     )
       endif
    case (REQUESTTYPE_WRITE)
       !
       ! Write data to file
       !
       uindex (1,1) = celidt
       uindex (2,1) = celidt
       uindex (3,1) = 1 ! increment in time
       !
       if (lsed > 0) then
          !
          ! element 'WS'
          !
          if (moroutput%ws) then
             call wrtarray_nmkl(fds, filename, filetype, grpnam, celidt, &
                           & nf, nl, mf, ml, iarrc, gdp, &
                           & 0, kmax, lsed, ierror, lundia, ws, 'WS', &
                           & smlay, kmaxout, kfsmin, kfsmax)
             if (ierror /= 0) goto 9999
          endif
          !
          if (kmax==1 .and. moroutput%rsedeq) then
             !
             ! element 'RSEDEQ'
             !
             call wrtarray_nmkl(fds, filename, filetype, grpnam, celidt, &
                          & nf, nl, mf, ml, iarrc, gdp, &
                          & 1, kmax, lsed, ierror, lundia, rsedeq, 'RSEDEQ', &
                          & smlay_restr, kmaxout_restr, kfsmin, kfsmax)
             if (ierror /= 0) goto 9999
          endif
       endif
       !
       if (moroutput%uuuvvv) then
          !
          ! element 'UUU'
          !
          call wrtarray_nm_ptr(fds, filename, filetype, grpnam, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, uuu, 'UUU')
          if (ierror /= 0) goto 9999
          !
          ! element 'VVV'
          !
          call wrtarray_nm_ptr(fds, filename, filetype, grpnam, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, vvv, 'VVV')
          if (ierror /= 0) goto 9999
       endif
       !
       if (moroutput%umod) then
          !
          ! element 'UMOD'
          !
          call wrtarray_nm_ptr(fds, filename, filetype, grpnam, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, umod, 'UMOD')
          if (ierror /= 0) goto 9999
       endif
       !
       if (moroutput%zumod) then
          !
          ! element 'ZUMOD'
          !
          call wrtarray_nm_ptr(fds, filename, filetype, grpnam, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, zumod, 'ZUMOD')
          if (ierror /= 0) goto 9999
       endif
       !
       if (moroutput%ustar) then
          !
          ! element 'USTAR'
          !
          allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
          rbuff2(:, :) = -999.0_fp
          do m = 1, mmax
             do n = 1, nmaxus
                call n_and_m_to_nm(n, m, nm, gdp)
                rbuff2(n, m) = sqrt(ust2(nm))
             enddo
          enddo
          call wrtarray_nm(fds, filename, filetype, grpnam, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, rbuff2, 'USTAR')
          deallocate(rbuff2)
          if (ierror /= 0) goto 9999
       endif
       !
       if (moroutput%sbcuv) then
          !
          ! element 'SBCU'
          !
          allocate( rbuff3(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, lsedtot) )
          rbuff3(:, :, :) = -999.0_fp
          if (associated(sbcu)) then
             do l = 1, lsedtot
                select case(moroutput%transptype)
                case (0)
                   rhol = 1.0_fp
                case (1)
                   rhol = cdryb(l)
                case (2)
                   rhol = rhosol(l)
                end select
                do m = 1, mmax
                   do n = 1, nmaxus
                      call n_and_m_to_nm(n, m, nm, gdp)
                      rbuff3(n, m, l) = sbcu(nm,l)/rhol
                   enddo
                enddo
             enddo
          endif
          call wrtarray_nml(fds, filename, filetype, grpnam, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, lsedtot, &
                        & ierror, lundia, rbuff3, 'SBCU')
          if (ierror /= 0) goto 9999
          !
          ! element 'SBCV'
          !
          rbuff3(:, :, :) = -999.0_fp
          if (associated(sbcu)) then
             do l = 1, lsedtot
                select case(moroutput%transptype)
                case (0)
                   rhol = 1.0_fp
                case (1)
                   rhol = cdryb(l)
                case (2)
                   rhol = rhosol(l)
                end select
                do m = 1, mmax
                   do n = 1, nmaxus
                      call n_and_m_to_nm(n, m, nm, gdp)
                      rbuff3(n, m, l) = sbcv(nm,l)/rhol
                   enddo
                enddo
             enddo
          endif
          call wrtarray_nml(fds, filename, filetype, grpnam, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, lsedtot, &
                        & ierror, lundia, rbuff3, 'SBCV')
          deallocate(rbuff3)
          if (ierror /= 0) goto 9999
       endif
       !
       if (moroutput%sbcuuvv) then
          !
          ! element 'SBCUU'
          !
          allocate( rbuff3(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, lsedtot) )
          rbuff3(:, :, :) = -999.0_fp
          if (associated(sbcu)) then
             do l = 1, lsedtot
                select case(moroutput%transptype)
                case (0)
                   rhol = 1.0_fp
                case (1)
                   rhol = cdryb(l)
                case (2)
                   rhol = rhosol(l)
                end select
                do m = 1, mmax
                   do n = 1, nmaxus
                      call n_and_m_to_nm(n, m, nm, gdp)
                      rbuff3(n, m, l) = sbcuu(nm,l)/rhol
                   enddo
                enddo
             enddo
          endif
          call wrtarray_nml(fds, filename, filetype, grpnam, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, lsedtot, &
                        & ierror, lundia, rbuff3, 'SBCUU')
          if (ierror /= 0) goto 9999
          !
          ! element 'SBCVV'
          !
          rbuff3(:, :, :) = -999.0_fp
          if (associated(sbcu)) then
             do l = 1, lsedtot
                select case(moroutput%transptype)
                case (0)
                   rhol = 1.0_fp
                case (1)
                   rhol = cdryb(l)
                case (2)
                   rhol = rhosol(l)
                end select
                do m = 1, mmax
                   do n = 1, nmaxus
                      call n_and_m_to_nm(n, m, nm, gdp)
                      rbuff3(n, m, l) = sbcvv(nm,l)/rhol
                   enddo
                enddo
             enddo
          endif
          call wrtarray_nml(fds, filename, filetype, grpnam, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, lsedtot, &
                        & ierror, lundia, rbuff3, 'SBCVV')
          deallocate(rbuff3)
          if (ierror /= 0) goto 9999
       endif
       !
       if (moroutput%sbwuv) then
          !
          ! element 'SBWU'
          !
          allocate( rbuff3(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, lsedtot) )
          rbuff3(:, :, :) = -999.0_fp
          if (associated(sbcu)) then
             do l = 1, lsedtot
                select case(moroutput%transptype)
                case (0)
                   rhol = 1.0_fp
                case (1)
                   rhol = cdryb(l)
                case (2)
                   rhol = rhosol(l)
                end select
                do m = 1, mmax
                   do n = 1, nmaxus
                      call n_and_m_to_nm(n, m, nm, gdp)
                      rbuff3(n, m, l) = sbwu(nm,l)/rhol
                   enddo
                enddo
             enddo
          endif
          call wrtarray_nml(fds, filename, filetype, grpnam, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, lsedtot, &
                        & ierror, lundia, rbuff3, 'SBWU')
          if (ierror /= 0) goto 9999
          !
          ! element 'SBWV'
          !
          rbuff3(:, :, :) = -999.0_fp
          if (associated(sbcu)) then
             do l = 1, lsedtot
                select case(moroutput%transptype)
                case (0)
                   rhol = 1.0_fp
                case (1)
                   rhol = cdryb(l)
                case (2)
                   rhol = rhosol(l)
                end select
                do m = 1, mmax
                   do n = 1, nmaxus
                      call n_and_m_to_nm(n, m, nm, gdp)
                      rbuff3(n, m, l) = sbwv(nm,l)/rhol
                   enddo
                enddo
             enddo
          endif
          call wrtarray_nml(fds, filename, filetype, grpnam, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, lsedtot, &
                        & ierror, lundia, rbuff3, 'SBWV')
          deallocate(rbuff3)
          if (ierror /= 0) goto 9999
       endif
       !
       if (moroutput%sbwuuvv) then
          !
          ! element 'SBWUU'
          !
          allocate( rbuff3(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, lsedtot) )
          rbuff3(:, :, :) = -999.0_fp
          if (associated(sbcu)) then
             do l = 1, lsedtot
                select case(moroutput%transptype)
                case (0)
                   rhol = 1.0_fp
                case (1)
                   rhol = cdryb(l)
                case (2)
                   rhol = rhosol(l)
                end select
                do m = 1, mmax
                   do n = 1, nmaxus
                      call n_and_m_to_nm(n, m, nm, gdp)
                      rbuff3(n, m, l) = sbwuu(nm,l)/rhol
                   enddo
                enddo
             enddo
          endif
          call wrtarray_nml(fds, filename, filetype, grpnam, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, lsedtot, &
                        & ierror, lundia, rbuff3, 'SBWUU')
          if (ierror /= 0) goto 9999
          !
          ! element 'SBWVV'
          !
          rbuff3(:, :, :) = -999.0_fp
          if (associated(sbcu)) then
             do l = 1, lsedtot
                select case(moroutput%transptype)
                case (0)
                   rhol = 1.0_fp
                case (1)
                   rhol = cdryb(l)
                case (2)
                   rhol = rhosol(l)
                end select
                do m = 1, mmax
                   do n = 1, nmaxus
                      call n_and_m_to_nm(n, m, nm, gdp)
                      rbuff3(n, m, l) = sbwvv(nm,l)/rhol
                   enddo
                enddo
             enddo
          endif
          call wrtarray_nml(fds, filename, filetype, grpnam, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, lsedtot, &
                        & ierror, lundia, rbuff3, 'SBWVV')
          deallocate(rbuff3)
          if (ierror /= 0) goto 9999
       endif
       !
       if (moroutput%sswuv) then
          !
          ! element 'SSWU'
          !
          allocate( rbuff3(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, lsedtot) )
          rbuff3(:, :, :) = -999.0_fp
          if (associated(sbcu)) then
             do l = 1, lsedtot
                select case(moroutput%transptype)
                case (0)
                   rhol = 1.0_fp
                case (1)
                   rhol = cdryb(l)
                case (2)
                   rhol = rhosol(l)
                end select
                do m = 1, mmax
                   do n = 1, nmaxus
                      call n_and_m_to_nm(n, m, nm, gdp)
                      rbuff3(n, m, l) = sswu(nm,l)/rhol
                   enddo
                enddo
             enddo
          endif
          call wrtarray_nml(fds, filename, filetype, grpnam, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, lsedtot, &
                        & ierror, lundia, rbuff3, 'SSWU')
          if (ierror /= 0) goto 9999
          !
          ! element 'SSWV'
          !
          rbuff3(:, :, :) = -999.0_fp
          if (associated(sbcu)) then
             do l = 1, lsedtot
                select case(moroutput%transptype)
                case (0)
                   rhol = 1.0_fp
                case (1)
                   rhol = cdryb(l)
                case (2)
                   rhol = rhosol(l)
                end select
                do m = 1, mmax
                   do n = 1, nmaxus
                      call n_and_m_to_nm(n, m, nm, gdp)
                      rbuff3(n, m, l) = sswv(nm,l)/rhol
                   enddo
                enddo
             enddo
          endif
          call wrtarray_nml(fds, filename, filetype, grpnam, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, lsedtot, &
                        & ierror, lundia, rbuff3, 'SSWV')
          deallocate(rbuff3)
          if (ierror /= 0) goto 9999
       endif
       !
       if (moroutput%sswuuvv) then
          !
          ! element 'SSWUU'
          !
          allocate( rbuff3(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, lsedtot) )
          rbuff3(:, :, :) = -999.0_fp
          if (associated(sbcu)) then
             do l = 1, lsedtot
                select case(moroutput%transptype)
                case (0)
                   rhol = 1.0_fp
                case (1)
                   rhol = cdryb(l)
                case (2)
                   rhol = rhosol(l)
                end select
                do m = 1, mmax
                   do n = 1, nmaxus
                      call n_and_m_to_nm(n, m, nm, gdp)
                      rbuff3(n, m, l) = sswuu(nm,l)/rhol
                   enddo
                enddo
             enddo
          endif
          call wrtarray_nml(fds, filename, filetype, grpnam, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, lsedtot, &
                        & ierror, lundia, rbuff3, 'SSWUU')
          if (ierror /= 0) goto 9999
          !
          ! element 'SSWVV'
          !
          rbuff3(:, :, :) = -999.0_fp
          if (associated(sbcu)) then
             do l = 1, lsedtot
                select case(moroutput%transptype)
                case (0)
                   rhol = 1.0_fp
                case (1)
                   rhol = cdryb(l)
                case (2)
                   rhol = rhosol(l)
                end select
                do m = 1, mmax
                   do n = 1, nmaxus
                      call n_and_m_to_nm(n, m, nm, gdp)
                      rbuff3(n, m, l) = sswvv(nm,l)/rhol
                   enddo
                enddo
             enddo
          endif
          call wrtarray_nml(fds, filename, filetype, grpnam, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, lsedtot, &
                        & ierror, lundia, rbuff3, 'SSWVV')
          deallocate(rbuff3)
          if (ierror /= 0) goto 9999
       endif
       !
       if (lsedtot > 0 .and. moroutput%sbuuvv) then
          !
          ! element 'SBUU'
          !
          allocate( rbuff3(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, lsedtot) )
          rbuff3(:, :, :) = -999.0_fp
          do l = 1, lsedtot
             select case(moroutput%transptype)
             case (0)
                rhol = 1.0_fp
             case (1)
                rhol = cdryb(l)
             case (2)
                rhol = rhosol(l)
             end select
             do m = 1, mmax
                do n = 1, nmaxus
                   rbuff3(n, m, l) = sbuu(n, m, l)/rhol
                enddo
             enddo
          enddo
          call wrtarray_nml(fds, filename, filetype, grpnam, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, lsedtot, &
                        & ierror, lundia, rbuff3, 'SBUU')
          if (ierror /= 0) goto 9999
          !
          ! element 'SBVV'
          !
          rbuff3(:, :, :) = -999.0_fp
          do l = 1, lsedtot
             select case(moroutput%transptype)
             case (0)
                rhol = 1.0_fp
             case (1)
                rhol = cdryb(l)
             case (2)
                rhol = rhosol(l)
             end select
             do m = 1, mmax
                do n = 1, nmaxus
                   rbuff3(n, m, l) = sbvv(n, m, l)/rhol
                enddo
             enddo
          enddo
          call wrtarray_nml(fds, filename, filetype, grpnam, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, lsedtot, &
                        & ierror, lundia, rbuff3, 'SBVV')
          deallocate(rbuff3)
          if (ierror /= 0) goto 9999
       endif
       !
       if (lsed > 0 .and. moroutput%seddif) then
          !
          ! element 'SEDDIF'
          !
          call wrtarray_nmkl(fds, filename, filetype, grpnam, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & 0, kmax, lsed, ierror, lundia, seddif, 'SEDDIF', &
                        & smlay, kmaxout, kfsmin, kfsmax)
          if (ierror /= 0) goto 9999
       endif
       !
       if (lsed > 0 .and. moroutput%ssuuvv) then
          !
          ! element 'SSUU'
          !
          allocate( rbuff3(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, lsed) )
          rbuff3(:, :, :) = -999.0_fp
          if (associated(sbcu)) then
             do l = 1, lsed
                select case(moroutput%transptype)
                case (0)
                   rhol = 1.0_fp
                case (1)
                   rhol = cdryb(l)
                case (2)
                   rhol = rhosol(l)
                end select
                do m = 1, mmax
                   do n = 1, nmaxus
                      call n_and_m_to_nm(n, m, nm, gdp)
                      rbuff3(n, m, l) = ssuu(nm, l)/rhol
                   enddo
                enddo
             enddo
          endif
          call wrtarray_nml(fds, filename, filetype, grpnam, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, lsed, &
                        & ierror, lundia, rbuff3, 'SSUU')
          if (ierror /= 0) goto 9999
          !
          ! element 'SSVV'
          !
          rbuff3(:, :, :) = -999.0_fp
          if (associated(sbcu)) then
             do l = 1, lsed
                select case(moroutput%transptype)
                case (0)
                   rhol = 1.0_fp
                case (1)
                   rhol = cdryb(l)
                case (2)
                   rhol = rhosol(l)
                end select
                do m = 1, mmax
                   do n = 1, nmaxus
                      call n_and_m_to_nm(n, m, nm, gdp)
                      rbuff3(n, m, l) = ssvv(nm, l)/rhol
                   enddo
                enddo
             enddo
          endif
          call wrtarray_nml(fds, filename, filetype, grpnam, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, lsed, &
                        & ierror, lundia, rbuff3, 'SSVV')
          deallocate(rbuff3)
          if (ierror /= 0) goto 9999
          !
          ! element 'SUCOR'
          !
          if (moroutput%suvcor) then
             allocate( rbuff3(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, lsed) )
             rbuff3(:, :, :) = -999.0_fp
             do l = 1, lsed
                select case(moroutput%transptype)
                case (0)
                   rhol = 1.0_fp
                case (1)
                   rhol = cdryb(l)
                case (2)
                   rhol = rhosol(l)
                end select
                do m = 1, mmax
                   do n = 1, nmaxus
                      call n_and_m_to_nm(n, m, nm, gdp)
                      rbuff3(n, m, l) = sucor(nm,l)/rhol
                   enddo
                enddo
             enddo
             call wrtarray_nml(fds, filename, filetype, grpnam, celidt, &
                           & nf, nl, mf, ml, iarrc, gdp, lsed, &
                           & ierror, lundia, rbuff3, 'SUCOR')
             if (ierror /= 0) goto 9999
             !
             ! element 'SVCOR'
             !
             rbuff3(:, :, :) = -999.0_fp
             do l = 1, lsed
                select case(moroutput%transptype)
                case (0)
                   rhol = 1.0_fp
                case (1)
                   rhol = cdryb(l)
                case (2)
                   rhol = rhosol(l)
                end select
                do m = 1, mmax
                   do n = 1, nmaxus
                      call n_and_m_to_nm(n, m, nm, gdp)
                      rbuff3(n, m, l) = svcor(nm,l)/rhol
                   enddo
                enddo
             enddo
             call wrtarray_nml(fds, filename, filetype, grpnam, celidt, &
                           & nf, nl, mf, ml, iarrc, gdp, lsed, &
                           & ierror, lundia, rbuff3, 'SVCOR')
             deallocate(rbuff3)
             if (ierror /= 0) goto 9999
          endif
          !
          ! element 'SOURSE'
          !
          if (moroutput%sourcesink) then
             call wrtarray_nml_ptr(fds, filename, filetype, grpnam, celidt, &
                          & nf, nl, mf, ml, iarrc, gdp, lsed, &
                          & ierror, lundia, sourse, 'SOURSE')
             if (ierror /= 0) goto 9999
             !
             ! element 'SINKSE'
             !
             call wrtarray_nml_ptr(fds, filename, filetype, grpnam, celidt, &
                          & nf, nl, mf, ml, iarrc, gdp, lsed, &
                          & ierror, lundia, sinkse, 'SINKSE')
             if (ierror /= 0) goto 9999
          endif
          !
          ! element 'AKS'
          !
          if (moroutput%aks) then
             call wrtarray_nml(fds, filename, filetype, grpnam, celidt, &
                          & nf, nl, mf, ml, iarrc, gdp, lsed, &
                          & ierror, lundia, aks, 'AKS')
             if (ierror /= 0) goto 9999
          endif
          !
          ! element 'RCA'
          !
          if (moroutput%rca) then
             call wrtarray_nml(fds, filename, filetype, grpnam, celidt, &
                          & nf, nl, mf, ml, iarrc, gdp, lsed, &
                          & ierror, lundia, rca, 'RCA')
             if (ierror /= 0) goto 9999
          endif
       endif
       !
       ! element 'DPS'
       !
       call wrtarray_nm(fds, filename, filetype, grpnam, celidt, &
                    & nf, nl, mf, ml, iarrc, gdp, &
                    & ierror, lundia, dps, 'DPS')
       if (ierror /= 0) goto 9999
       !
       ! element 'SDU' (subsidence/uplift)
       !
       if (lfsdu) then       
          allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
          rbuff2(:, :) = -999.0_fp
          do m = 1, mmax
             do n = 1, nmaxus
                call n_and_m_to_nm(n, m, nm, gdp)
                rbuff2(n, m) = sdu_tn(nm) - sdu_t0(nm)
             enddo
          enddo
          call wrtarray_nm(fds, filename, filetype, grpnam, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, rbuff2, 'SDU')
          deallocate(rbuff2)
          if (ierror/=0) goto 9999
       endif          
       !
       if (moroutput%dzduuvv) then
          !
          ! element 'DZDUU'
          !
          call wrtarray_nm_ptr(fds, filename, filetype, grpnam, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, dzduu, 'DZDUU')
          if (ierror /= 0) goto 9999
          !
          ! element 'DZDVV'
          !
          call wrtarray_nm_ptr(fds, filename, filetype, grpnam, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, dzdvv, 'DZDVV')
          if (ierror /= 0) goto 9999
       endif
       !
       if (scour) then
          !
          ! element 'TAUADD'
          !
          allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
          rbuff2(:, :) = -999.0_fp
          do m = 1, mmax
             do n = 1, nmaxus
                call n_and_m_to_nm(n, m, nm, gdp)
                call shearx(tauadd, nm, gdp)
                rbuff2(n, m) = tauadd
             enddo
          enddo
          call wrtarray_nm(fds, filename, filetype, grpnam, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & ierror, lundia, rbuff2, 'TAUADD')
          deallocate(rbuff2)
          if (ierror /= 0) goto 9999
       endif
       !
       if (moroutput%taub) then
          !
          ! element 'TAUB'
          !
          call wrtarray_nm_ptr(fds, filename, filetype, grpnam, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & ierror, lundia, taub, 'TAUB')
          if (ierror /= 0) goto 9999
       endif
       !
       if (moroutput%taurat) then
          !
          ! element 'TAURAT'
          !
          call wrtarray_nml_ptr(fds, filename, filetype, grpnam, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, lsedtot, &
                        & ierror, lundia, taurat, 'TAURAT')
          if (ierror /= 0) goto 9999
       endif
       !
       ! element 'DM'
       !
       if (moroutput%dm) then
          call wrtarray_nm_2d(fds, filename, filetype, grpnam, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & ierror, lundia, dm, 'DM')
          if (ierror /= 0) goto 9999
       endif
       !
       ! element 'DG'
       !
       if (moroutput%dg) then
          call wrtarray_nm_2d(fds, filename, filetype, grpnam, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & ierror, lundia, dg, 'DG')
          if (ierror /= 0) goto 9999
       endif
       !
       ! element 'DGSD'
       !
       if (moroutput%dgsd) then
          call wrtarray_nm_2d(fds, filename, filetype, grpnam, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & ierror, lundia, dgsd, 'DGSD')
          if (ierror /= 0) goto 9999
       endif
       !
       if (moroutput%percentiles) then
          allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
          do l = 1, nxx
             write(dxname,'(A,I2.2)') 'DXX',l
             rbuff2(:, :) = -999.0_fp
             do m = 1, mmax
                do n = 1, nmaxus
                   call n_and_m_to_nm(n, m, nm, gdp)
                   rbuff2(n, m) = dxx(nm, l)
                enddo
             enddo
             call wrtarray_nm_2d(fds, filename, filetype, grpnam, celidt, &
                           & nf, nl, mf, ml, iarrc, gdp, &
                           & ierror, lundia, rbuff2, dxname)
             if (ierror /= 0) goto 9999
          enddo
          deallocate(rbuff2)
       endif
       !
       if (moroutput%frac) then
          !
          ! element 'FRAC'
          !
          call wrtarray_nml(fds, filename, filetype, grpnam, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, lsedtot, &
                        & ierror, lundia, frac, 'FRAC')
          if (ierror /= 0) goto 9999
       endif
       !
       if (moroutput%mudfrac) then
          !
          ! element 'MUDFRAC'
          !
          call wrtarray_nm_2d(fds, filename, filetype, grpnam, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & ierror, lundia, mudfrac, 'MUDFRAC')
          if (ierror /= 0) goto 9999
       endif
       !
       if (moroutput%sandfrac) then
          !
          ! element 'SANDFRAC'
          !
          call wrtarray_nm_2d(fds, filename, filetype, grpnam, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & ierror, lundia, sandfrac, 'SANDFRAC')
          if (ierror /= 0) goto 9999
       endif       
       !
       if (moroutput%fixfac) then
          !
          ! element 'FIXFAC'
          !
          call wrtarray_nml_ptr(fds, filename, filetype, grpnam, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, lsedtot, &
                        & ierror, lundia, fixfac, 'FIXFAC')
          if (ierror /= 0) goto 9999
       endif
       !
       if (moroutput%hidexp) then
          !
          ! element 'HIDEXP'
          !
          call wrtarray_nml_ptr(fds, filename, filetype, grpnam, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, lsedtot, &
                        & ierror, lundia, hidexp, 'HIDEXP')
          if (ierror /= 0) goto 9999
       endif
       !
       if (moroutput%sedpar) then
          allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
          i = 0
          do l = 1, lsedtot
             write(sednr,'(I3.3)') l
             do k = 1, noutpar(l)
                i = ioutpar(k,l)
                rbuff2(:, :) = -999.0_fp
                do m = 1, mmax
                   do n = 1, nmaxus
                      call n_and_m_to_nm(n, m, nm, gdp)
                      rbuff2(n, m) = outpar(i,nm)
                   enddo
                enddo
                call wrtarray_nm(fds, filename, filetype, grpnam, celidt, &
                              & nf, nl, mf, ml, iarrc, gdp, &
                              & ierror, lundia, rbuff2, trim(outpar_name(k,l))//trim(sednr))
                if (ierror /= 0) goto 9999
             enddo
          enddo
          deallocate(rbuff2)
       endif
       !
       ! Add mor fields
       !
       if (lsedtot > 0) then
          call wrmorm(lundia    ,error     ,mmax      ,nmaxus    ,lsedtot   , &
                    & REQUESTTYPE_WRITE    ,fds       ,grpnam    , &
                    & filename  ,gdp       ,filetype  , &
                    & mf        ,ml        ,nf        ,nl        ,iarrc     )
          if (error) goto 9999
       endif
       !
       ! Add fluff fields
       !
       if (lsed > 0) then
          call wrmfluff(lundia    ,error     ,mmax      ,nmaxus    ,lsed      , &
                      & REQUESTTYPE_WRITE    ,fds       ,grpnam    , &
                      & filename  ,gdp       ,filetype  , &
                      & mf        ,ml        ,nf        ,nl        ,iarrc     )
          if (error) goto 9999
       endif
       !
       ! Add statistics fields  
       !
       if (lsed > 0) then
          call wrmorst(lundia    ,error     ,mmax      ,nmaxus    ,lsedtot   , &
                     & REQUESTTYPE_WRITE   ,fds       ,grpnam    , &
                     & filename  ,gdp       ,filetype  , &
                     & mf        ,ml        ,nf        ,nl        ,iarrc     )
       endif
       !
9999   continue
       if (ierror/= 0) error = .true.
    endselect
    !
    deallocate(smlay_restr)
end subroutine wrsedm
