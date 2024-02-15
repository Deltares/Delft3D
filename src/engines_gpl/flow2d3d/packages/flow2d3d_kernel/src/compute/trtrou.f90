subroutine trtrou(lundia    ,nmax      ,mmax      ,nmaxus    ,kmax      , &
                & cfrou     ,rouflo    ,linit     ,gdis_dp   ,gdis_zet  , &
                & huv       ,kcuv      ,uvdir     ,uvperp    ,sig       , &
                & z0rou     ,deltadir  ,idir      ,gdp       )
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
! Calculate rougness due to trachytopes.
! Routine is called for U/V-direction
! respectively.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    use globaldata
    use string_module
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                    , pointer :: i50
    integer                    , pointer :: i90
    integer                    , pointer :: iarea_avg
    integer                    , pointer :: max_cl
    integer                    , pointer :: nttaru
    integer                    , pointer :: nttarv
    integer                    , pointer :: ntrt
    integer, dimension(:)      , pointer :: itrt_list
    integer , dimension(:,:)   , pointer :: ittaru
    integer , dimension(:,:)   , pointer :: ittarv
    integer , dimension(:,:)   , pointer :: ittdef
    real(fp)                   , pointer :: eps
    real(fp)                   , pointer :: rhow
    real(fp)                   , pointer :: ag
    real(fp)                   , pointer :: z0
    real(fp)                   , pointer :: vonkar
    real(fp)                   , pointer :: vicmol
    real(fp)                   , pointer :: dryflc
    real(fp)                   , pointer :: alf_area_ser
    real(fp)                   , pointer :: trtminh
    real(fp), dimension(:)     , pointer :: fraccu_list
    real(fp), dimension(:,:)   , pointer :: rgcalu
    real(fp), dimension(:,:)   , pointer :: rgcalv
    real(fp), dimension(:)     , pointer :: rttaru
    real(fp), dimension(:)     , pointer :: rttarv
    real(fp), dimension(:,:)   , pointer :: rttdef
    real(fp), dimension(:,:,:) , pointer :: rttfu
    real(fp), dimension(:,:,:) , pointer :: rttfv
    real(fp), dimension(:,:)   , pointer :: vegh2d
    real(fp), dimension(:,:)   , pointer :: vden2d 
    logical                    , pointer :: flsedprop_rqrd
    logical                    , pointer :: spatial_bedform
    logical                    , pointer :: v2dwbl
    logical                    , pointer :: waqol
    !
    real(fp), dimension(:,:)   , pointer :: dxx
    real(fp), dimension(:)     , pointer :: rhosol
    real(fp), dimension(:)     , pointer :: bedformD50
    real(fp), dimension(:)     , pointer :: bedformD90
    real(fp), dimension(:)     , pointer :: rksr
    real(fp), dimension(:)     , pointer :: rksmr
    real(fp), dimension(:)     , pointer :: rksd    
!
! Local parameters
!
    integer , parameter :: ch_type  = 1
    integer , parameter :: kn_type  = 0
    integer , parameter :: area_rgh = 1
    integer , parameter :: line_rgh = 2
    integer , parameter :: pnt_rgh  = 3
    integer , parameter :: spec_rgh = 0
    integer , parameter :: skip_rgh = -999
!
! Global variables
!
    integer                                                            , intent(in)  :: idir   !  Flag for direction, 1=U, 2=V
    integer                                                            , intent(in)  :: kmax
    integer                                                                          :: lundia
    integer                                                            , intent(in)  :: mmax
    integer                                                            , intent(in)  :: nmax
    integer                                                            , intent(in)  :: nmaxus
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)       , intent(in)  :: kcuv
    logical                                                            , intent(in)  :: linit
    real(fp), dimension(kmax)                                          , intent(in)  :: sig
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in)  :: deltadir
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in)  :: gdis_dp
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in)  :: gdis_zet
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in)  :: huv
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in)  :: z0rou
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 3)                 :: cfrou
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax), intent(in)  :: uvdir
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax), intent(in)  :: uvperp
    character(4)                                                                     :: rouflo
!
! Local variables
!
    integer                     :: ifrom
    integer                     :: ilist
    integer                     :: ircod
    integer                     :: ita
    integer                     :: ito
    integer                     :: itrt
    integer                     :: itrt_user
    integer                     :: k
    integer                     :: kmaxx
    integer                     :: m
    integer                     :: mc
    integer                     :: md
    integer                     :: ml
    integer                     :: mu
    integer                     :: n
    integer                     :: nc
    integer                     :: nd
    integer                     :: nm
    integer                     :: nmu
    integer                     :: nl
    integer                     :: nlist
    integer                     :: nu
    integer                     :: num
    integer, dimension(2)       :: numlen
    integer                     :: rgh_geom
    integer                     :: rgh_type
    logical                     :: lfound
    logical                     :: lnew
    real(fp)                    :: a0
    real(fp)                    :: a1
    real(fp)                    :: alfa
    real(fp)                    :: alfam
    real(fp)                    :: betam
    real(fp)                    :: bfh
    real(fp)                    :: bfl
    real(fp)                    :: calca1
    real(fp)                    :: calca2
    real(fp)                    :: cbed
    real(fp)                    :: ch_icode
    real(fp)                    :: ch_lin_ser
    real(fp)                    :: ch_pnt_ser
    real(fp)                    :: ch_sum_par
    real(fp)                    :: ch_sum_ser
    real(fp)                    :: d50
    real(fp)                    :: d90
    real(fp)                    :: densit
    real(fp)                    :: depth
    real(fp)                    :: drag
    real(fp)                    :: dstar
    real(fp)                    :: e1
    real(fp)                    :: f
    real(fp)                    :: fracbu
    real(fp)                    :: fraccu
    real(fp)                    :: fracto
    real(fp)                    :: hk
    real(fp)                    :: kbed
    real(fp)                    :: kn_icode
    real(fp)                    :: kn_sum
    real(fp)                    :: rc0
    real(fp)                    :: rc3
    real(fp)                    :: rcgrn
    real(fp)                    :: rchmin
    real(fp)                    :: rcmain
    real(fp)                    :: relden
    real(fp)                    :: relh2
    real(fp)                    :: rkgrn
    real(fp)                    :: rkmain
    real(fp)                    :: rktra
    real(fp)                    :: rksdu
    real(fp)                    :: rksmru
    real(fp)                    :: rksru
    real(fp)                    :: rleng
    real(fp)                    :: rm0
    real(fp)                    :: rmndrf
    real(fp)                    :: rmup
    real(fp)                    :: t1
    real(fp)                    :: t2
    real(fp)                    :: t3
    real(fp)                    :: t4
    real(fp)                    :: t5
    real(fp)                    :: thetac
    real(fp)                    :: thetag
    real(fp)                    :: thetam
    real(fp)                    :: tsp
    real(fp)                    :: u2dh
    real(fp)                    :: ubsvg2
    real(fp)                    :: ucbsv2
    real(fp)                    :: umod
    real(fp)                    :: uuu
    real(fp)                    :: uv0
    real(fp)                    :: vheigh
    real(fp)                    :: vd2d
    real(fp)                    :: vh2d
    real(fp)                    :: vvv
    real(fp)                    :: vz0
    real(fp)                    :: zcc
    character(12), dimension(2) :: cnum
    character(132)              :: cmsg
    character(256)              :: errmsg      ! Character var. containing the errormessage to be written to file. The message depends on the error. 
    character(1)                :: cdir   
!
    integer                   , pointer :: nttar
    integer , dimension(:,:)  , pointer :: ittar
    real(fp), dimension(:,:)  , pointer :: rgcal
    real(fp), dimension(:,:,:), pointer :: rttf
    real(fp), dimension(:)    , pointer :: rttar
!
!! executable statements -------------------------------------------------------
!
    rhow            => gdp%gdphysco%rhow
    ag              => gdp%gdphysco%ag
    z0              => gdp%gdphysco%z0
    vonkar          => gdp%gdphysco%vonkar
    vicmol          => gdp%gdphysco%vicmol
    eps             => gdp%gdconst%eps
    dryflc          => gdp%gdnumeco%dryflc
    v2dwbl          => gdp%gdnumeco%v2dwbl
    !
    alf_area_ser    => gdp%gdtrachy%alf_area_ser
    trtminh         => gdp%gdtrachy%trtminh
    iarea_avg       => gdp%gdtrachy%iarea_avg
    max_cl          => gdp%gdtrachy%max_cl
    nttaru          => gdp%gdtrachy%nttaru
    nttarv          => gdp%gdtrachy%nttarv
    ntrt            => gdp%gdtrachy%ntrt
    itrt_list       => gdp%gdtrachy%itrt_list
    ittaru          => gdp%gdtrachy%ittaru
    ittarv          => gdp%gdtrachy%ittarv
    ittdef          => gdp%gdtrachy%ittdef
    fraccu_list     => gdp%gdtrachy%fraccu_list
    rgcalu          => gdp%gdtrachy%rgcalu
    rgcalv          => gdp%gdtrachy%rgcalv
    rttaru          => gdp%gdtrachy%rttaru
    rttarv          => gdp%gdtrachy%rttarv
    rttdef          => gdp%gdtrachy%rttdef
    rttfu           => gdp%gdtrachy%rttfu
    rttfv           => gdp%gdtrachy%rttfv
    vegh2d          => gdp%gdtrachy%vegh2d
    vden2d          => gdp%gdtrachy%vden2d
    flsedprop_rqrd  => gdp%gdtrachy%flsedprop_rqrd
    waqol           => gdp%gdwaqpar%waqol
    !
    dxx             => gdp%gderosed%dxx
    rhosol          => gdp%gdsedpar%rhosol
    i50             => gdp%gdmorpar%i50
    i90             => gdp%gdmorpar%i90
    bedformD50      => gdp%gdbedformpar%bedformD50
    bedformD90      => gdp%gdbedformpar%bedformD90
    spatial_bedform => gdp%gdbedformpar%spatial_bedform
    rksr            => gdp%gdbedformpar%rksr
    rksmr           => gdp%gdbedformpar%rksmr
    rksd            => gdp%gdbedformpar%rksd
    !
    ! refer to appropriate arrays depending on direction
    !
    if (idir==1) then
       !
       ! U-direction
       !
       nttar => nttaru
       ittar => ittaru
       rgcal => rgcalu
       rttf  => rttfu
       rttar => rttaru
    else
       !
       ! V-direction
       !
       nttar => nttarv
       ittar => ittarv
       rgcal => rgcalv
       rttf  => rttfv
       rttar => rttarv
    endif
    !
    cdir = char(116+idir)
    !
    ! initialize ...
    !
    ! Reset RTTF to zero
    !
    do k = 1, kmax
       do m = 1, mmax
          do n = 1, nmax
             rttf(n, m, k) = 0.0_fp
          enddo
       enddo
    enddo
    !
    ! Copy CFROU. When in init-mode, make backup of defaults,
    ! (CFROU(..,..,3) = CFROU(..,..,2))
    ! otherwise retrieve backup (CFROU(..,..,2) = CFROU(..,..,3)).
    !
    if (linit) then
       ifrom = 2
       ito   = 3
    else
       ifrom = 3
       ito   = 2
    endif
    do m = 1, mmax
       do n = 1, nmax
          cfrou(n, m, ito) = cfrou(n, m, ifrom)
       enddo
    enddo
    !
    ! Initialize locals
    !
    fracbu     = 0.0_fp
    fracto     = 0.0_fp
    ch_sum_par = 0.0_fp
    ch_sum_ser = 0.0_fp
    kn_sum     = 0.0_fp
    depth      = 0.0_fp
    ch_lin_ser = 0.0_fp
    ch_pnt_ser = 0.0_fp
    ml         = -99999
    nl         = -99999
    lnew       = .false.
    !
    ! Main loop over area data
    !
    do ita = 1, nttar
       !
       ! Get the next point
       !
       nc = ittar(ita, 1)
       mc = ittar(ita, 2)
       !
       ! Check if it is a new cell
       !
       lnew = nc/=nl .or. mc/=ml
       !
       if (lnew) then
          !
          ! Calculate the roughness for the old grid cell
          !
          if (nl>0 .and. ml>0) then
             !
             ! Calculate the roughness for the old grid cell
             !
             call calrou(kn_sum          ,fracto          ,fracbu    ,depth     ,ch_lin_ser, &
                       & cfrou(nl, ml, 2),cfrou(nl, ml, 3),rouflo    , &
                       & iarea_avg       ,ch_sum_par      ,ch_sum_ser,ch_pnt_ser, &
                       & alf_area_ser    )
          endif
          !
          ! Check if the new point was a block separator
          ! or an inactive point.
          ! If so reset last point and go to the next point
          !
          if (nc== - 1 .and. mc== - 1) then
             !
             ! Reset and get next point
             !
             nl = -99999
             ml = -99999
             cycle
          elseif (kcuv(nc, mc)==0) then
             !
             ! Reset and get next point
             ! This must be a separate elseif branch: kcuv(-1,-1) is not allowed to be read
             !
             nl = -99999
             ml = -99999
             cycle
          endif
          !
          ! Initialise new cell
          !
          nl         = nc
          ml         = mc
          nu         = min(nc + 1, nmax)
          mu         = min(mc + 1, mmax)
          nd         = max(nc - 1, 1)
          md         = max(mc - 1, 1)
          fracbu     = 0.0_fp
          fracto     = 0.0_fp
          ch_sum_par = 0.0_fp
          ch_sum_ser = 0.0_fp
          kn_sum     = 0.0_fp
          ch_lin_ser = 0.0_fp
          ch_pnt_ser = 0.0_fp
          depth      = max(trtminh , huv(nc, mc))
          !
          kmaxx = kmax
          !
          if (v2dwbl) then
             do k = kmax, 1, -1
                zcc = (1.0 + sig(k))*depth
                if (zcc>deltadir(nc, mc) .or. zcc>0.5*depth) then
                   kmaxx = k
                   exit
                endif
             enddo
          endif
          !
          if (idir==1) then
             call n_and_m_to_nm(nc, mc, nm, gdp) 
             call n_and_m_to_nm(nc, mu, nmu, gdp) 
             !
             ! U-direction
             !
             if (flsedprop_rqrd) then
                !
                ! get D50, D90, relative density from morphology module
                !
                if (associated(gdp%gderosed%dxx))  then
                   d50 = 0.5_fp*(dxx(nm, i50) + dxx(nmu, i50))
                   d90 = 0.5_fp*(dxx(nm, i90) + dxx(nmu, i90))
                   relden = (rhosol(1)-rhow)/rhow ! assume density equal for all sediment fractions
                else
                   if (spatial_bedform) then
                      d50 = 0.5_fp*(bedformD50(nm) + bedformD50(nmu))
                      d90 = 0.5_fp*(bedformD90(nm) + bedformD90(nmu))
                   else
                      d50 = bedformD50(1)
                      d90 = bedformD90(1)
                   endif
                   relden = 1.65_fp
                endif 
             endif
             !
             ! Average perpendicular velocity
             !
             vvv = 0.25_fp*(  uvperp(nc, mc, kmaxx) + uvperp(nc, mu, kmaxx)  &
                 &          + uvperp(nd, mc, kmaxx) + uvperp(nd, mu, kmaxx))
             !
             ! Riples, Megaripples, Dunes
             !
             rksru  = (rksr (nm) + rksr (nmu))*0.5_fp
             rksmru = (rksmr(nm) + rksmr(nmu))*0.5_fp
             rksdu  = (rksd (nm) + rksd (nmu))*0.5_fp
             if (waqol) then
                !
                ! 2D Vegetation characterics (coming from WAQ)
                !
                vd2d = vden2d(nc,mc) + vden2d(nc,mu)
                vh2d = (vegh2d(nc,mc)*vden2d(nc,mc) + vegh2d(nc,mu)*vden2d(nc,mu)) / max(vd2d,eps)
                vd2d = vd2d * 0.5_fp 
             endif
          else
             call n_and_m_to_nm(nc, mc, nm, gdp) 
             call n_and_m_to_nm(nu, mc, num, gdp) 
             !
             ! V-direction
             !
             if (flsedprop_rqrd) then
                !
                ! get D50, D90, relative density from morphology module
                !
                if (associated(gdp%gderosed%dxx))  then 
                   d50 = 0.5_fp*(dxx(nm, i50) + dxx(num, i50))
                   d90 = 0.5_fp*(dxx(nm, i90) + dxx(num, i90))
                   relden = (rhosol(1)-rhow)/rhow ! assume density equal for all sediment fractions
                else
                   if (spatial_bedform) then
                      d50 = 0.5_fp*(bedformD50(nm) + bedformD50(num))
                      d90 = 0.5_fp*(bedformD90(nm) + bedformD90(num))
                   else
                      d50 = bedformD50(1)
                      d90 = bedformD90(1)
                   endif
                   relden = 1.65_fp
                endif
             endif
             !
             ! Average perpendicular velocity
             !
             vvv = 0.25_fp*(  uvperp(nc, mc, kmaxx) + uvperp(nc, md, kmaxx)  &
                 &          + uvperp(nu, mc, kmaxx) + uvperp(nu, md, kmaxx))
             !
             ! Riples, Megaripples, Dunes
             !
             rksru  = (rksr (nm) + rksr (num))*0.5_fp
             rksmru = (rksmr(nm) + rksmr(num))*0.5_fp
             rksdu  = (rksd (nm) + rksd (num))*0.5_fp
             if (waqol) then
                !
                ! 2D Vegetation characterics (coming from WAQ)
                !
                vd2d = vden2d(nc,mc) + vden2d(nu,mc)
                vh2d = (vegh2d(nc,mc)*vden2d(nc,mc) + vegh2d(nu,mc)*vden2d(nu,mc)) / max(vd2d,eps)
                vd2d = vd2d * 0.5_fp
             endif
          endif
          !
          ! Depth-average velocity (similar as in TAUBOT)
          !
          uuu  = uvdir(nc, mc, kmaxx)
          umod = sqrt(uuu**2 + vvv**2)
          if (kmax==1) then
             u2dh = umod
          else
             u2dh = (umod/depth*((depth + z0rou(nc, mc))         &
                  &              *log(1.0_fp + depth/max(z0rou(nc, mc),1.0e-20_fp)) &
                  &              - depth)                         ) &
                  & /log(1.0_fp + (1.0_fp + sig(kmaxx))*depth/max(z0rou(nc, mc),1.0e-20_fp))
          endif
       endif
       !
       ! Get the trachytope and roughness characteristic
       !
       itrt_list(1)   = ittar(ita, 3)
       fraccu_list(1) = rttar(ita)
       nlist          = 1
       !
       ilist=1
       do while (ilist<=nlist)
          itrt_user = itrt_list(ilist)
          fraccu    = fraccu_list(ilist)
          !
          lfound = .false.
          do itrt = 1, ntrt
             if (ittdef(itrt, 1)==itrt_user) then
                lfound = .true.
                exit
             endif
          enddo
          if (.not.lfound) then
             call prterr(lundia    ,'J001'    ,'TRTROU: Trachytope not found.' )
             call d3stop(1, gdp)
          endif
          !
          ircod = ittdef(itrt, 2)
          !
          if (ircod==2) then
             !
             ! Combination of area roughnesses: expand
             !
             nlist = nlist+1
             if (nlist>max_cl) then
                write (errmsg, '(a,i6,a,i4,a,a,a)') 'TRTROU: Maximum recursion depth reached for line ', &
                    & ita, ' containing roughness code ', ittar(ita, 3), ' in ', cdir,'-direction.'
                call prterr(lundia    ,'J001'    ,errmsg )
                call d3stop(1, gdp)
             endif
             itrt_list(ilist)   = nint(rttdef(itrt, 1))
             itrt_list(nlist)   = nint(rttdef(itrt, 2))
             fraccu_list(ilist) = fraccu*rttdef(itrt, 3)
             fraccu_list(nlist) = fraccu*rttdef(itrt, 4)
          else
             !
             ! Other roughness formulation: fill in itrt
             ! this is the effective internal trachytope
             ! number where the formula known to the user
             ! as trachytope type itrt_user is stored.
             !
             itrt_list(ilist) = itrt
             ilist            = ilist+1
          endif
       enddo
       !
       ! Calculate the roughnes according to the given types
       !
       do ilist = 1, nlist
          itrt   = itrt_list(ilist)
          fraccu = fraccu_list(ilist)
          ircod  = ittdef(itrt, 2)
          !
          rgh_type = kn_type
          rgh_geom = spec_rgh
          if (ircod==1) then
             !
             ! Water free area
             !
             fracbu = fracbu + fraccu
          elseif (ircod==2) then
             !
             ! Combination of area roughnesses: parsed above
             ! So, the program should never come here.
             !
          elseif (ircod==51) then
             !
             ! Constant White-Colebrook / Nikuradse value
             !
             kn_icode = rttdef(itrt, 1)
             rgh_geom = area_rgh
          elseif (ircod==52) then
             !
             ! Constant Chezy value
             !
             ch_icode = rttdef(itrt, 1)
             rgh_type = ch_type
             rgh_geom = area_rgh
          elseif (ircod==53) then
             !
             ! Constant Manning value
             !
             ch_icode = (depth**(1.0_fp/6.0_fp))/rttdef(itrt, 1)
             rgh_type = ch_type
             rgh_geom = area_rgh
          elseif (ircod==54) then
             !
             ! Constant Z0 value
             !
             kn_icode = 30.0_fp*rttdef(itrt, 1)
             rgh_geom = area_rgh
          elseif (ircod==101) then
             !
             ! WAQUA roughness predictor
             !         (Van Rijn simplified)
             !
             alfam    = rttdef(itrt, 1)
             betam    = rttdef(itrt, 2)
             rkmain   = alfam*depth**0.7_fp*(1.0_fp - exp( - betam*depth**( -0.3_fp)))
             kn_icode = max(rkmain, 0.00001_fp)
             rgh_geom = area_rgh
          elseif (ircod==102) then
             !
             ! power law: C = A * H**B
             !
             ch_icode = rttdef(itrt, 1)*depth**rttdef(itrt, 2)
             rgh_type = ch_type
             rgh_geom = area_rgh
          elseif (ircod==103) then
             !
             ! Van Rijn roughness predictor
             !
             dstar = d50*(ag*relden/vicmol**2)**(1.0_fp/3.0_fp)
             !
             if (dstar<=4.0_fp) then
                thetac = 0.24_fp/dstar
             elseif (dstar<=10.0_fp) then
                thetac = 0.14_fp*dstar**( - 0.64_fp)
             elseif (dstar<=20.0_fp) then
                thetac = 0.04_fp*dstar**( - 0.10_fp)
             elseif (dstar<=150.0_fp) then
                thetac = 0.013_fp*dstar**(0.29_fp)
             else
                thetac = 0.055_fp
             endif
             !
             ucbsv2 = ag*relden*d50*thetac
             !
             rkgrn  = 3.0_fp*d90
             rcgrn  = 18.0_fp*log10(12.0_fp*depth/rkgrn)
             ubsvg2 = ag*u2dh**2/rcgrn**2
             tsp    = (ubsvg2 - ucbsv2)/ucbsv2
             !
             if (tsp<=0.0_fp .or. tsp>=25.0_fp) then
                bfh = 0.0_fp
             else
                t1  = 0.11_fp*depth
                t2  = (d50/depth)**(0.3_fp)
                t3  = 1.0_fp - exp( - 0.5_fp*tsp)
                t4  = 25.0_fp - tsp
                bfh = t1*t2*t3*t4
             endif
             !
             bfl      = max(7.3_fp*depth,1e-6_fp)
             !
             t1       = 1.0_fp - exp(-25.0_fp*bfh/bfl)
             kn_icode = rkgrn + 1.1_fp*bfh*t1
             rgh_geom = area_rgh
          elseif (ircod==104) then
             !
             ! Struiksma roughness predictor
             !
             calca1 = rttdef(itrt, 1)
             calca2 = rttdef(itrt, 2)
             thetac = rttdef(itrt, 3)
             thetam = rttdef(itrt, 4)
             rchmin = rttdef(itrt, 5)
             !
             rcgrn = calca1*log10(calca2*depth/d90)
             !
             thetag = u2dh**2/(rcgrn**2*relden*d50)
             if (u2dh<=0.0_fp) then
                thetag = 0.001_fp
             elseif (thetag<=0.0_fp) then
                write (cnum(1), '(i12)') nc
                call remove_leading_spaces(cnum(1)   ,numlen(1) )
                write (cnum(2), '(i12)') mc
                call remove_leading_spaces(cnum(2)   ,numlen(2) )
                cmsg = cnum(1)(1:numlen(1)) // ', ' // cnum(2)(1:numlen(2))
                call prterr(lundia    ,'J015'    ,cmsg      )
                call d3stop(1, gdp)
                !
                thetag = 0.001_fp
             endif
             !
             if (thetag>=1.0_fp) then
                write (cnum(1), '(i12)') nc
                call remove_leading_spaces(cnum(1)   ,numlen(1) )
                write (cnum(2), '(i12)') mc
                call remove_leading_spaces(cnum(2)   ,numlen(2) )
                cmsg = cnum(1)(1:numlen(1)) // ', ' // cnum(2)(1:numlen(2))
                call prterr(lundia    ,'J016'    ,cmsg      )
                call d3stop(1, gdp)
                !
                thetag = 0.999_fp
             endif
             !
             if (thetag<=thetac) then
                rcmain = rcgrn
             else
                t1     = thetac*(thetag - thetac)
                t2     = thetag - (thetam**2/thetac)
                t3     = ((rcgrn/rchmin)**2) - 1.0_fp
                t4     = thetag*(thetam - thetac)**2
                rmndrf = 1.0_fp - (t1*t2*t3/t4)
                rcmain = rcgrn/sqrt(rmndrf)
             endif
             !
             ch_icode = rcmain
             rgh_type = ch_type
             rgh_geom = area_rgh
          elseif (ircod==105) then
             !
             ! Quadratic combination of bedform heights (Van Rijn 2004)
             !
             rkmain   = sqrt(rksru**2 + rksmru**2 + rksdu**2)
             kn_icode = min(rkmain,0.5_fp*depth)
             rgh_geom = area_rgh
          elseif (ircod==106) then
             !
             ! Linear addition of bedform heights
             !
             rkmain   = rksru + rksmru + rksdu
             kn_icode = min(rkmain,0.5_fp*depth)
             rgh_geom = area_rgh
          elseif (ircod==151 .or. ircod==152) then
             !
             ! Vegetation roughness WAQUA / Van Barneveld
             !
             vheigh = rttdef(itrt, 1)
             densit = rttdef(itrt, 2)
             if (ircod==152) then
                drag = rttdef(itrt, 3)
                kbed = rttdef(itrt, 4)
                cbed = 18.0_fp*log10(12.0_fp*depth/kbed)
             else
                drag = 1.65_fp
                cbed = 1.0e5_fp
             endif
             !
             if (vheigh>=depth) then
                !
                ! For flow through vegetation only
                !
                if (ircod==152) then
                   rc0  = sqrt(1.0_fp/((drag*densit*vheigh)/(2.0_fp*ag)+1.0_fp/(cbed*cbed)))
                else
                   rc0  = sqrt((2.0_fp*ag)/(drag*densit*depth))
                endif
             else
                !
                ! For flow through and over vegetation
                !
                if (ircod==152) then
                   uv0  = sqrt(vheigh/((drag*densit*vheigh)/(2.0_fp*ag)+1.0_fp/(cbed*cbed)))
                   !
                   alfa = 0.0227_fp*vheigh**0.7_fp
                else
                   uv0  = sqrt((2.0_fp*ag)/(drag*densit))
                   !
                   alfa = 0.01_fp*sqrt(depth*vheigh)
                endif
                alfa = max(alfa, 0.001_fp)
                a0   = (densit*drag)/(2.0_fp*alfa)
                !
                t1   = sqrt(2.0_fp*a0)
                rc3  = (2.0_fp*ag*(depth - vheigh))                   &
                     & /(alfa*t1*(exp(vheigh*t1) + exp( - vheigh*t1)))
                t2   = rc3*exp(vheigh*t1)
                t3   = sqrt(t2 + (uv0*uv0))
                t4   = sqrt(rc3 + (uv0*uv0))
                e1   = (t1*t2)/(2.0_fp*t3)
                a1   = (1.0_fp + sqrt(1.0_fp + ((4.0_fp*(e1**2)*(vonkar**2)*(depth - vheigh))/ag)))&
                     & /((2.0_fp*(e1**2)*(vonkar**2))/ag)
                a1   = min(a1,vheigh)
                t5   = depth - (vheigh - a1)
                f    = (vonkar*t3)/sqrt(ag*t5)
                vz0  = a1*exp( - f)
                rc0  = 1.0_fp/(depth**1.5_fp)                                          &
                     & *((2.0_fp/t1)*(t3 - t4) + (uv0/t1)*log(((t3 - uv0)*(t4 + uv0))  &
                     & /((t3 + uv0)*max((t4 - uv0), eps_fp)))                          &
                     & + (sqrt(ag*t5)/vonkar)                                          &
                     & *((t5*log(t5/vz0)) - (a1*log(a1/vz0)) - (depth - vheigh)))
             endif
             !
             if (ircod==152) then
                ch_icode = rc0
                rgh_type = ch_type
             else
                rktra = (12.0_fp*depth)/10.0_fp**(rc0/18.0_fp)
                !
                ! 0.50 is a kind of bed rougness under trachytopes
                !
                kn_icode = max(rktra, 0.50_fp)
             endif
             rgh_geom = area_rgh
          elseif (ircod==153 .or. ircod==154) then
             !
             ! Baptist vegetation formulation
             !
             vheigh = rttdef(itrt, 1)
             densit = rttdef(itrt, 2)
             if (vheigh < 0.0_fp) then
                vheigh = vh2d
                densit = vd2d
             endif
             drag   = rttdef(itrt, 3)
             cbed   = rttdef(itrt, 4)
             !
             rgh_type = ch_type
             rgh_geom = area_rgh
             !
             if (vheigh < eps) then
                rgh_geom = skip_rgh
             elseif (ircod==153) then
                if (depth>vheigh) then
                   ch_icode = 1.0_fp/sqrt(1.0_fp/(cbed*cbed) + &
                            &          (drag*densit*vheigh)/(2.0_fp*ag)) &
                            & + sqrt(ag)*log(depth/vheigh)/vonkar
                else
                   ch_icode = 1.0_fp/sqrt(1.0_fp/(cbed*cbed) + &
                            &          (drag*densit*depth)/(2.0_fp*ag))
                endif
             else
                hk     = max(1.0_fp,depth/vheigh)
                ch_icode = cbed + sqrt(ag)/vonkar*log(hk)* &
                         & sqrt(1.0_fp+(drag*densit*vheigh*cbed**2)/(2.0_fp*ag))
                rttf(nc, mc, 1) = rttf(nc, mc, 1) + fraccu * &
                         & drag*densit/hk*(cbed*cbed)/(ch_icode*ch_icode)
             endif
             !
             !rgh_type = ch_type
             !rgh_geom = area_rgh
          elseif (ircod==201) then
             !
             ! Get coefficients for hedges
             !
             vheigh = rttdef(itrt, 1)
             densit = rttdef(itrt, 2)
             !
             ! Calculate hedges
             !
             rleng = gdis_zet(nc, mc)
             if (vheigh>depth) then
                !
                ! For flow through vegetation only
                !
                rmup = 1.0_fp + 0.175_fp*densit*(depth/vheigh - 2.0_fp)
             else
                !
                ! For flow through and over vegetation
                !
                rmup = 1.0_fp - 0.175_fp*densit*vheigh/depth
             endif
             rmup   = min(0.999_fp, max(0.001_fp, rmup))
             ch_icode = sqrt((2.0_fp*ag*rleng*rmup**2)/(fraccu*depth*(1.0_fp - rmup**2)))
             !
             rgh_type = ch_type
             rgh_geom = line_rgh
          elseif (ircod==202) then
             !
             ! Get coefficients for hedges
             !
             vheigh = rttdef(itrt, 1)
             densit = rttdef(itrt, 2)
             drag   = rttdef(itrt, 3)
             rm0    = rttdef(itrt, 4)
             !
             ! Calculate hedges
             !
             rleng = gdis_zet(nc, mc)
             if (vheigh>depth) then
                !
                ! For flow through vegetation only
                !
                ch_icode = sqrt((2.0_fp*ag*rleng)/(fraccu*drag*densit*depth))
             else
                !
                ! For flow through and over vegetation
                !
                relh2 = ((depth-vheigh)/depth)**2
                ch_icode = sqrt((2.0_fp*ag*rleng)/(fraccu*depth))* &
                         & (vheigh/(depth*sqrt(drag*densit)) + &
                         &  rm0*sqrt(relh2/(1.0_fp-relh2)))
             endif
             !
             rgh_type = ch_type
             rgh_geom = line_rgh
          elseif (ircod==251) then
             !
             ! Get coefficients for trees
             !
             vheigh = rttdef(itrt, 1)
             drag   = rttdef(itrt, 2)
             densit = fraccu
             !
             ! Calculate trees
             !
             vheigh   = min(vheigh,depth)
             ch_icode = sqrt((2.0_fp*ag)/(drag*densit*vheigh))
             !
             rgh_type = ch_type
             rgh_geom = pnt_rgh
          else
             !
             ! Specified roughness type not implemented
             !
             call prterr(lundia    ,'J001'    ,'TRTROU: Specified roughness type not implemented.'   )
             call d3stop(1, gdp)
          endif
          !
          if (rgh_geom == area_rgh) then
             if (rgh_type == ch_type) then
                kn_icode = (12.0_fp*depth)/10.0_fp**(ch_icode/18.0_fp)
             elseif (rgh_type == kn_type) then
                ch_icode = 18.0_fp*log10(12.0_fp*depth/kn_icode)
             endif
             fracto     = fracto + fraccu
             kn_sum     = kn_sum + fraccu*kn_icode
             ch_sum_par = ch_sum_par + fraccu*ch_icode
             ch_sum_ser = ch_sum_ser + fraccu/(ch_icode**2)
          elseif (rgh_geom == line_rgh) then
             ch_lin_ser = ch_lin_ser + 1.0_fp/(ch_icode**2)
          elseif (rgh_geom == pnt_rgh) then
             ch_pnt_ser = ch_pnt_ser + 1.0_fp/(ch_icode**2)
          endif
       enddo
    enddo
    !
    ! Process the last pending cell
    !
    if (nttar>0 .and. nc>0 .and. mc>0) then
       depth = max(dryflc , huv(nc, mc))
       call calrou(kn_sum    ,fracto    ,fracbu    ,depth     ,ch_lin_ser, &
                 & cfrou(nc, mc, 2)     ,cfrou(nc, mc, 3)     ,rouflo    , &
                 & iarea_avg ,ch_sum_par,ch_sum_ser,ch_pnt_ser, &
                 & alf_area_ser         )
    endif
    !
    ! Multiply roughness with calibration factor
    !
    do m = 1, mmax
       do n = 1, nmaxus
          if (cfrou(n, m, 2)>0.0) then
             cfrou(n, m, 2) = cfrou(n, m, 2)*rgcal(n, m)
          endif
       enddo
    enddo
end subroutine trtrou
