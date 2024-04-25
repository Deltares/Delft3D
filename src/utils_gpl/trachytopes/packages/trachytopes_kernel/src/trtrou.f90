module m_trtrou
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
!-------------------------------------------------------------------------------
!
! functions and subroutines
!
implicit none 

public trtrou
public chktra
public chktrt

contains
    
subroutine trtrou(lundia    ,kmax      ,nmmax   , &
                & cfrou     ,rouflo    ,linit     ,gdis_zet  , &
                & huv       ,kcuv      ,sig       , &
                & z0rou     ,jdir      ,waqol     ,gdtrachy  , & 
                & umod      ,nmlb      ,nmub      ,nmlbc     , nmubc    , & 
                & rhow      ,ag        ,vonkar    ,vicmol    , & 
                & eps       ,dryflc    ,spatial_bedform      ,bedformD50,bedformD90, & 
                & rksr      ,rksmr     ,rksd      ,error, & 
                & assoc_dxx ,nxx       ,lsedtot   ,dxx       ,i50       ,i90,       &
                & rhosol        )
              
!!--description-----------------------------------------------------------------
!
! Calculate rougness due to trachytopes.
! Routine is called for U/V-direction (structured), net link direction (unstructured)
! respectively.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use precision_basics, only: comparereal
    use mathconsts
    use trachytopes_data_module
    use m_calrou
    use message_module
    !
    implicit none
    !
    type (trachy_type)          , target :: gdtrachy
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                    , pointer :: iarea_avg
    integer                    , pointer :: ntrt
    integer                    , pointer :: max_cl
    integer                    , pointer :: nttaru
    integer                    , pointer :: ntrtcrs
    integer                    , pointer :: ntrtobs
    integer, dimension(:)      , pointer :: itrt_list
    integer , dimension(:,:)   , pointer :: ittaru
    integer , dimension(:,:)   , pointer :: ittdef
    integer , dimension(:,:)   , pointer :: ittlin
    real(fp)                   , pointer :: alf_area_ser
    real(fp)                   , pointer :: trtminh
    real(fp), dimension(:)     , pointer :: fraccu_list
    real(fp), dimension(:,:)   , pointer :: rttdef
    real(fp), dimension(:)     , pointer :: rgcalu
    real(fp), dimension(:,:)   , pointer :: rttfu
    real(fp), dimension(:)     , pointer :: rttaru
    real(fp), dimension(:)     , pointer :: rttacLin
    real(fp), dimension(:)     , pointer :: blu_trt
    real(fp), dimension(:)     , pointer :: zsu_prev
    real(fp), dimension(:)     , pointer :: vegh2d
    real(fp), dimension(:)     , pointer :: vden2d 
    logical                    , pointer :: flsedprop_rqrd
    !
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
    integer                                                            , intent(in)  :: jdir          !< Flag for direction, 1=U, 2=V
    integer                                                            , intent(in)  :: kmax
    integer                                                                          :: lundia
    integer                                                            , intent(in)  :: nmmax
    integer                                                            , intent(in)  :: nmlb          !< start space index (of edges)
    integer                                                            , intent(in)  :: nmub          !< end space index   (of edges)
    integer                                                            , intent(in)  :: nmlbc         !< start space index (flow nodes)
    integer                                                            , intent(in)  :: nmubc         !< end space index   (flow nodes)
    integer, dimension(nmlb:nmub)                                                    :: kcuv
    logical                                                            , intent(in)  :: linit
    real(fp), dimension(kmax)                                          , intent(in)  :: sig
    !real(fp), dimension(nmlb:nmub)                                     , intent(in)  :: gdis_dp  !(not used) 
    real(fp), dimension(nmlb:nmub)                                     , intent(in)  :: gdis_zet
    real(fp), dimension(nmlb:nmub)                                                   :: huv           !< water depth at u or v point 
    real(fp), dimension(nmlbc:nmubc)                                                 :: z0rou
    real(fp), dimension(nmlb:nmub, 3)                                                :: cfrou
!    real(fp), dimension(nmlb:nmub)              :: uvdir    (not used) 
!    real(fp), dimension(nmlb:nmub), intent(in)  :: uvperp   (not used) 
    real(fp), dimension(nmlbc:nmubc)                                                 :: umod
    character(4)                                                                     :: rouflo
    logical                                                                          :: waqol
    real(fp)                                                            , intent(in) :: eps
    real(fp)                                                            , intent(in) :: rhow
    real(fp)                                                            , intent(in) :: ag
    !real(fp)                                                            , intent(in) :: z0     ! not used 
    real(fp)                                                            , intent(in) :: vonkar
    real(fp)                                                            , intent(in) :: vicmol
    real(fp)                                                            , intent(in) :: dryflc
    logical                                                                          :: assoc_dxx
    integer                                                                          :: nxx           ! cannot be optional
    real(fp), dimension(nmlbc:nmubc, nxx) , optional                    , intent(in) :: dxx           !< sediment diameter corresponding to percentile xx (mud excluded)
    integer                             , optional                                   :: i50
    integer                             , optional                                   :: i90
    integer                                                                          :: lsedtot       ! dito
    real(fp), dimension(lsedtot)        , optional                                   :: rhosol
    logical                                                                          :: spatial_bedform
    real(fp), dimension(nmlbc:nmubc)                                                 :: bedformD50    !< 50-percentile of sediment diameters
    real(fp), dimension(nmlbc:nmubc)                                                 :: bedformD90    !< 90-percentile of sediment diameters
    real(fp), dimension(nmlbc:nmubc)                                                 :: rksr          !< Ripple roughness height in zeta point
    real(fp), dimension(nmlbc:nmubc)                                                 :: rksmr         !< Mega-ripple roughness height in zeta point
    real(fp), dimension(nmlbc:nmubc)                                                 :: rksd          !< Dune roughness height in zeta point
    logical                                                             ,intent(out) :: error
    !
    character(20)                                                                    :: tmpstr
    !
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
    integer                     :: mc
    integer                     :: ml
    integer                     :: nc
    integer                     :: nm     ! L   (DFlow-FM) notation
    integer                     :: nm1    ! k1  (DFlow-FM) notation
    integer                     :: nm2    ! k2  (DFlow-FM) notation
    integer                     :: nml
    integer                     :: nl
    integer                     :: nlist
    integer                     :: rgh_geom
    integer                     :: rgh_type
    logical                     :: lfound
    logical                     :: lnew
    logical                     :: ebb_condition
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
    real(fp)                    :: densitfoliage 
    real(fp)                    :: depth
    real(fp)                    :: drag
    real(fp)                    :: dragfoliage  
    real(fp)                    :: dstar
    real(fp)                    :: e1
    real(fp)                    :: expchistem
    real(fp)                    :: expchifoliage
    real(fp)                    :: f
    real(fp)                    :: fracbu
    real(fp)                    :: fraccu
    real(fp)                    :: fracto
    real(fp)                    :: hk
    real(fp)                    :: kbed
    real(fp)                    :: kn_icode
    real(fp)                    :: kn_sum
    real(fp)                    :: phi
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
    real(fp)                    :: uchistem
    real(fp)                    :: uchifoliage
    real(fp)                    :: umag
!    real(fp)                    :: uuu
    real(fp)                    :: uv0
    real(fp)                    :: vheigh
    real(fp)                    :: vd2d
    real(fp)                    :: vh2d
    real(fp)                    :: vz0
    real(fp)                    :: zstemp
    real(fp)                    :: z0rouL
    character(12), dimension(2) :: cnum
    character(132)              :: cmsg
    character(256)              :: errmsg
    
!
!
!! executable statements -------------------------------------------------------
!
    !rhow            => gdp%gdphysco%rhow
    !ag              => gdp%gdphysco%ag
    !z0              => gdp%gdphysco%z0
    !vonkar          => gdp%gdphysco%vonkar
    !vicmol          => gdp%gdphysco%vicmol
    !eps             => gdp%gdconst%eps
    !dryflc          => gdp%gdnumeco%dryflc
    !
    ! Pointers to general part 
    !
    alf_area_ser    => gdtrachy%gen%alf_area_ser
    iarea_avg       => gdtrachy%gen%iarea_avg
    max_cl          => gdtrachy%gen%max_cl
    itrt_list       => gdtrachy%gen%itrt_list
    fraccu_list     => gdtrachy%gen%fraccu_list
    ntrtcrs         => gdtrachy%gen%ntrtcrs
    ntrtobs         => gdtrachy%gen%ntrtobs
    rttdef          => gdtrachy%gen%rttdef
    trtminh         => gdtrachy%gen%trtminh
    vegh2d          => gdtrachy%gen%vegh2d
    vden2d          => gdtrachy%gen%vden2d
    !
    ! Pointers to directional part 
    !
    nttaru           => gdtrachy%dir(jdir)%nttaru
    rgcalu           => gdtrachy%dir(jdir)%rgcalu
    ittaru           => gdtrachy%dir(jdir)%ittaru
    ittlin           => gdtrachy%dir(jdir)%lin
    rttaru           => gdtrachy%dir(jdir)%rttaru
    rttfu            => gdtrachy%dir(jdir)%rttfu
    rttacLin         => gdtrachy%dir(jdir)%acLin
    blu_trt          => gdtrachy%dir(jdir)%blu_trt
    zsu_prev         => gdtrachy%dir(jdir)%zsu_prev
    !
    !
    ntrt            => gdtrachy%gen%ntrt
    ittdef          => gdtrachy%gen%ittdef
    flsedprop_rqrd  => gdtrachy%gen%flsedprop_rqrd
    !waqol           => gdp%gdwaqpar%waqol
    !
    !i50             => gdp%gdmorpar%i50
    !i90             => gdp%gdmorpar%i90
    !rhosol          => gdp%gdsedpar%rhosol
    !spatial_bedform => gdp%gdbedformpar%spatial_bedform
    !bedformD50      => gdp%gdbedformpar%bedformD50
    !bedformD90      => gdp%gdbedformpar%bedformD90
    !rksr            => gdp%gdbedformpar%rksr
    !rksmr           => gdp%gdbedformpar%rksmr
    !rksd            => gdp%gdbedformpar%rksd
    !
    !
    ! initialize variables
    error      = .false.
    !
    ! Reset RTTFU to zero
    !
    do k = 1, kmax
       do nm = 1, nmmax
          rttfu(nm, k) = 0.0_fp
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
    do nm = 1, nmmax
        cfrou(nm, ito) = cfrou(nm, ifrom)
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
    ebb_condition  = .true.
    ch_lin_ser = 0.0_fp
    ch_pnt_ser = 0.0_fp
    ml         = TRACHY_UNDEFINED
    nl         = TRACHY_UNDEFINED
    nml        = TRACHY_UNDEFINED
    !call n_and_m_to_nm(nl, ml, nml, gdp)   !WOtemp 
    lnew       = .false.
    !
    ! Update discharge dependent trachytope coefficients
    !
    call update_rttdef(gdtrachy%gen%rttdef, ntrtcrs, gdtrachy%gen%crs, gdtrachy%gen%table_q, &
                       gdtrachy%gen%rttdef_q, gdtrachy%gen%rttdef_q_cross, gdtrachy%gen%rttdef_q_slope)
    !
    ! Update water level dependent trachytope coefficients 
    !
    call update_rttdef(gdtrachy%gen%rttdef, ntrtobs, gdtrachy%gen%obs, gdtrachy%gen%table_zs, &
                       gdtrachy%gen%rttdef_zs, gdtrachy%gen%rttdef_zs_cross, gdtrachy%gen%rttdef_zs_slope)

    !
    ! Main loop over area data
    !
    do ita = 1, nttaru
       !
       ! Get the next point
       !
       nc = ittaru(ita, 1)
       mc = ittaru(ita, 2)
       nm = ittaru(ita, 4)
       !call n_and_m_to_nm(nc, mc, nmc, gdp) 
       !
       ! Check if it is a new cell
       !
       lnew = nm/=nml ! nc/=nl .or. mc/=ml
       !
       if (lnew) then
          !
          ! Calculate the roughness for the old grid cell
          !
          if (nml>0) then !(nl>0 .and. ml>0) then
             !
             ! Calculate the roughness for the old grid cell
             !
             call calrou(kn_sum          ,fracto          ,fracbu    ,depth     ,ch_lin_ser, &
                       & cfrou(nml, 2)   ,cfrou(nml, 3)   ,rouflo    , &
                       & iarea_avg       ,ch_sum_par      ,ch_sum_ser,ch_pnt_ser, &
                       & alf_area_ser    )
          endif
          !
          ! Check if the new point was a block separator
          ! or an inactive point.
          ! If so reset last point and go to the next point
          !
          if (nm == TRACHY_NOT_IN_SUBDOMAIN) then ! (nc== - 1 .and. mc== - 1) then
             !
             ! Reset and get next point
             !
             nl = TRACHY_UNDEFINED
             ml = TRACHY_UNDEFINED
             nml = TRACHY_UNDEFINED
             cycle
          elseif (nm == TRACHY_MISSING_VALUE) then 
             ! 
             ! nm is set ouside trachytopes module
             !   
             cycle
          elseif (kcuv(nm)==0) then
             !
             ! Reset and get next point
             ! This must be a separate elseif branch: kcuv(-1,-1) is not allowed to be read
             !
             nl = TRACHY_UNDEFINED
             ml = TRACHY_UNDEFINED
             nml = TRACHY_UNDEFINED
             cycle
          endif
          !
          ! Initialise new cell
          !
          nl         = nc
          ml         = mc
          nml        = nm
          fracbu     = 0.0_fp
          fracto     = 0.0_fp
          ch_sum_par = 0.0_fp
          ch_sum_ser = 0.0_fp
          kn_sum     = 0.0_fp
          ch_lin_ser = 0.0_fp
          ch_pnt_ser = 0.0_fp
          depth      = max(trtminh , huv(nm))
          zstemp     = blu_trt(nm) + huv(nm)
          if (zstemp < zsu_prev(nm)) then
             ebb_condition = .true.
          else 
             ebb_condition = .false.
          end if 
          zsu_prev(nm) = zstemp   ! set previous value
          !
          !if (jdir==1) then
!             call n_and_m_to_nm(nc, mu, nmu, gdp) 
             nm1 = ittlin(1,nm)
             nm2 = ittlin(2,nm)
             if (nm1 == 0 .and. nm2 == 0) cycle
             !
             ! U-direction
             !
             if (flsedprop_rqrd) then
                !
                ! get D50, D90, relative density from morphology module
                !
                if (assoc_dxx)  then
                   d50 = rttacLin(nm)*dxx(nm1, i50) + (1-rttacLin(nm))*dxx(nm2, i50)
                   d90 = rttacLin(nm)*dxx(nm1, i90) + (1-rttacLin(nm))*dxx(nm2, i90)
                   relden = (rhosol(1)-rhow)/rhow ! assume density equal for all sediment fractions
                else
                   if (spatial_bedform) then
                      d50 = rttacLin(nm)*bedformD50(nm1) + (1-rttacLin(nm))*bedformD50(nm2)
                      d90 = rttacLin(nm)*bedformD90(nm1) + (1-rttacLin(nm))*bedformD90(nm2)
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
             !vvv = 0.25_fp*(  uvperp(nc, mc, kmax) + uvperp(nc, mu, kmax)  &
             !    &          + uvperp(nd, mc, kmax) + uvperp(nd, mu, kmax))
             !
             ! Ripples, Megaripples, Dunes
             !
             rksru  = rttacLin(nm)*rksr(nm1)  + (1d0-rttacLin(nm))*rksr(nm2)
             rksmru = rttacLin(nm)*rksmr(nm1) + (1d0-rttacLin(nm))*rksmr(nm2)
             rksdu  = rttacLin(nm)*rksd(nm1)  + (1d0-rttacLin(nm))*rksd(nm2)
             !
             if (waqol) then
                !
                ! 2D Vegetation characterics (coming from WAQ)
                !
                vd2d = rttacLin(nm)*vden2d(nm1) + (1-rttacLin(nm))*vden2d(nm2)
                vh2d = (rttacLin(nm)*vegh2d(nm1)*vden2d(nm1) + (1-rttacLin(nm))*vegh2d(nm2)*vden2d(nm2)) / max(vd2d,eps)
             endif
          !endif
          !
          ! Depth-average velocity (similar as in TAUBOT)
          !
          umag = rttacLin(nm)*umod(nm1) + (1d0-rttacLin(nm))*umod(nm2)
          if (kmax==0) then
             u2dh = umag
          else
             z0rouL = rttacLin(nm)*z0rou(nm1)  + (1d0-rttacLin(nm))*z0rou(nm2)
             u2dh = (umag/depth*((depth + z0rouL)         &
                  &              *log(1.0_fp + depth/max(z0rouL,1.0e-5_fp)) &
                  &              - depth)                         ) &
                  & /log(1.0_fp + (1.0_fp + sig(kmax))*depth/max(z0rouL,1.0e-5_fp))
          endif
       endif
       !
       ! Get the trachytope and roughness characteristic
       !
       itrt_list(1)   = ittaru(ita, 3)
       fraccu_list(1) = rttaru(ita)
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
             !call prterr(lundia    ,'J001'    ,'TRTROU: Trachytope not found.' )
             !call d3stop(1, gdp)
             write (tmpstr, '(i12)') itrt_user
             errmsg = 'Trachytopes: TRTROU: Trachytope definition '// trim(tmpstr) //' not found.'
             call write_error(errmsg, unit=lundia)
             error = .true.
             return
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
                !call prterr(lundia    ,'J001'    ,'TRTROU: Maximum recursion depth.' )
                !call d3stop(1, gdp)
                write (errmsg, '(a,i6,a,i4,a)') 'TRTROU: Maximum recursion depth reached for line ', &
                    & ita, ' containing roughness code ', ittaru(ita, 3), '.'
                call write_error(errmsg, unit=lundia)
                write (errmsg, '(a,i3,a)') 'TRTROU: Adjust composite roughness definition or increase recursion limit (now currently TrtMxR = ', max_cl, ')'
                call write_error(errmsg, unit=lundia)
                error = .true.
                return
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
       ! Calculate the roughness according to the given types
       !
       do ilist = 1, nlist
          itrt   = itrt_list(ilist)
          fraccu = fraccu_list(ilist)
          ircod  = ittdef(itrt, 2)
          !
          rgh_type = kn_type
          rgh_geom = spec_rgh
          if (comparereal(fraccu,0.0_fp)==0) then
             cycle
          elseif (ircod==1) then
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
          elseif (ircod==61) then
             !
             ! Ebb-flood constant White-Colebrook / Nikuradse value
             !
             if (ebb_condition) then 
                kn_icode = rttdef(itrt, 1)
             else
                kn_icode = rttdef(itrt, 2)
             end if 
             rgh_geom = area_rgh
          elseif (ircod==62) then
             !
             ! Ebb-flood constant Chezy value
             !
             if (ebb_condition) then 
                ch_icode = rttdef(itrt, 1)
             else                 
                ch_icode = rttdef(itrt, 2)
             end if
             rgh_type = ch_type
             rgh_geom = area_rgh
          elseif (ircod==63) then
             !
             ! Ebb-flood constant Manning value
             !
             if (ebb_condition) then 
                ch_icode = (depth**(1.0_fp/6.0_fp))/rttdef(itrt, 1)
             else    
                ch_icode = (depth**(1.0_fp/6.0_fp))/rttdef(itrt, 2)
             end if                 
             rgh_type = ch_type
             rgh_geom = area_rgh
          elseif (ircod==64) then
             !
             ! Ebb-flood constant Z0 value
             !
             if (ebb_condition) then 
                 kn_icode = 30.0_fp*rttdef(itrt, 1)
             else 
                 kn_icode = 30.0_fp*rttdef(itrt, 2)
             end if                  
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
             if (thetag<=0.0) then
                write (cnum(1), '(i12)') nc
                !call noextspaces(cnum(1)   ,numlen(1) )
                write (cnum(2), '(i12)') mc
                !call noextspaces(cnum(2)   ,numlen(2) )
                cmsg = trim(cnum(1)) // ', ' // trim(cnum(2))
                !call prterr(lundia    ,'J015'    ,cmsg      )
                !call d3stop(1, gdp)
                !
                thetag = 0.001_fp
             endif
             !
             if (thetag>=1.0_fp) then
                write (cnum(1), '(i12)') nc
                !call noextspaces(cnum(1)   ,numlen(1) )
                write (cnum(2), '(i12)') mc
                !call noextspaces(cnum(2)   ,numlen(2) )
                cmsg = trim(cnum(1)) // ', ' // trim(cnum(2))
                !call prterr(lundia    ,'J016'    ,cmsg      )
                !call d3stop(1, gdp)
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
                !call n_and_m_to_nm(nc, mc, nmc, gdp)
                rttfu(nm, 1) = rttfu(nm, 1) + fraccu * &
                         & drag*densit/hk*(cbed*cbed)/(ch_icode*ch_icode)
             endif
             !
             rgh_type = ch_type
             rgh_geom = area_rgh
          elseif (ircod==155) then
             !
             ! Vaestilae & Jaervelae (2017) formula
             !
             ! input parameters
             vheigh         = rttdef(itrt, 1)
             densit         = rttdef(itrt, 2)
             drag           = rttdef(itrt, 3)
             uchistem       = rttdef(itrt, 4)
             expchistem     = rttdef(itrt, 5)
             densitfoliage  = rttdef(itrt, 6)
             dragfoliage    = rttdef(itrt, 7)
             uchifoliage    = rttdef(itrt, 8)
             expchifoliage  = rttdef(itrt, 9)
             cbed           = rttdef(itrt, 10)
             
             ! Relative vegetation height
             hk     = max(1.0_fp, depth/vheigh)
             
             ! Calculate roughness 
             if (umag > 0) then 
                ! Phi is a function of uc (flow velocity in vegetation layer), but
                ! uc depends on phi. We approximate uc=u2dh
                ! Dimensionless vegetation parameter with uc = u2dh
                phi = drag*densit*(umag/uchistem)**expchistem + &
                    & densitfoliage*dragfoliage*(u2dh/uchifoliage)**expchifoliage
                    
                ! Effective bed friction 
                ch_icode = cbed + 1.0_fp/sqrt(1.0_fp + phi*cbed*cbed/(2.0_fp*ag)) * &
                         & sqrt(ag)*log(hk)/vonkar
                                    
                ! Lambda 
                rttfu(nm, 1) = rttfu(nm, 1) + fraccu * &
                         & phi / depth * (cbed*cbed)/(ch_icode*ch_icode)
                
             else
                 ! zero umag will through dividebyzero error (since expchi are expected to be negative)
                 ! so for zero velocities, use cbed instead
                 ch_icode = cbed
             endif
             rgh_type = ch_type
             rgh_geom = area_rgh
        elseif (ircod==156) then
             !
             ! Jaervelae (2014) formula
             !
             ! input parameters
             vheigh         = rttdef(itrt, 1)
             densit         = rttdef(itrt, 2)
             drag           = rttdef(itrt, 3)
             uchistem       = rttdef(itrt, 4)
             expchistem     = rttdef(itrt, 5)
             cbed           = rttdef(itrt, 6)
             
             ! Relative vegetation height
             hk     = max(1.0_fp,depth/vheigh)
             
             ! Calculate roughness
             if (umag > 0) then 
                ! Phi is a function of uc (flow velocity in vegetation layer), but
                ! uc depends on phi. We approximate uc=u2dh
                ! Dimensionless vegetation parameter with uc = u2dh
                phi = drag*densit*(umag/uchistem)**expchistem
                
                ! Effective bed friction 
                ch_icode = cbed + 1.0_fp/sqrt(1.0_fp + phi*cbed*cbed/(2.0_fp*ag)) * &
                         & sqrt(ag)*log(hk)/vonkar
                
                ! Lambda 
                rttfu(nm, 1) = rttfu(nm, 1) + fraccu * &
                         & phi / depth * (cbed*cbed)/(ch_icode*ch_icode) 
             else
                 ! zero umag will through dividebyzero error (since expchi are expected to be negative)
                 ! so for zero velocities, use cbed instead
                 ch_icode = cbed
             endif
             rgh_type = ch_type
             rgh_geom = area_rgh
          elseif (ircod==201) then
             !
             ! Get coefficients for hedges
             !
             vheigh = rttdef(itrt, 1)
             densit = rttdef(itrt, 2)
             !
             ! Calculate hedges
             !
             rleng = gdis_zet(nm)
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
             rleng = gdis_zet(nm)
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
             errmsg = 'Trachytopes: TRTROU: Specified roughness type not implemented.'
             call write_error(errmsg, unit=lundia)
             error = .true.
             return
             !call prterr(lundia    ,'J001'    ,'TRTROU: Specified roughness type not implemented.'   )
             !call d3stop(1, gdp)
          endif
          !
          if (rgh_geom == area_rgh) then
             if (rgh_type == ch_type) then
                kn_icode = (12.0_fp*depth)/10.0_fp**(ch_icode/18.0_fp)
             elseif (rgh_type == kn_type) then
                ch_icode = white_coolebrook(depth, kn_icode, iarea_avg)
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
    if (nttaru>0 .and. nm>0) then ! nc>0 .and. mc>0) then
       depth = max(dryflc , huv(nm))
      ! call n_and_m_to_nm(nc, mc, nmc, gdp)
       call calrou(kn_sum    ,fracto    ,fracbu    ,depth     ,ch_lin_ser, &
                 & cfrou(nm, 2)     ,cfrou(nm, 3)     ,rouflo    , &
                 & iarea_avg ,ch_sum_par,ch_sum_ser,ch_pnt_ser, &
                 & alf_area_ser         )
    endif
    !
    ! Multiply roughness with calibration factor
    !
    do nm = 1, nmmax
       if (cfrou(nm, 2)>0.0) then
          cfrou(nm, 2) = cfrou(nm, 2)*rgcalu(nm)
       endif
    enddo 
    
    ! debugging 
    ! write (debugfilename, '(a,I8.8, a)') 'cftrt.',numcalls,'.txt'
    ! open (newunit=luntmp, file=debugfilename, action='write')
    ! do nm = 1, nmmax
    !     write(luntmp,'(I5,1PE15.7)') nm, cfrou(nm, 2)
    ! end do     
    ! numcalls = numcalls + 1
    ! close (luntmp)

end subroutine trtrou

!> helper routine to update trachytope definitions in case of discharge of water level dependent roughnesses
subroutine update_rttdef(rttdef, total, crs_obs, table_q_zs, rttdef_q_zs, cross, slope)
    use precision              , only : fp
    use trachytopes_data_module, only : trachy_crs_obs_name
    real(kind=fp),             intent(inout)         :: rttdef(:,:)                 !< trachytope definitions
    real(kind=fp),             intent(in   )         :: table_q_zs(:)               !< discharge / waterlevel array
    real(kind=fp),             intent(in   )         :: rttdef_q_zs(:,:)            !< coeffient table for trachytopes
    real(kind=fp),             intent(in   )         :: cross(:,:)                  !< cross of coeffient table
    real(kind=fp),             intent(in   )         :: slope(:,:)                  !< slope of coeffient table
    type(trachy_crs_obs_name), intent(in   ), target :: crs_obs(:)                  !< struct with name and indexes
    integer,                   intent(in   )         :: total                       !< total number of definitions

    integer          :: itrt
    integer          :: m
    integer          :: mropar
    integer, pointer :: ntrt_qzs
    integer, pointer :: idx_start
    integer, pointer :: idx_end
    integer, pointer :: nropars

    do itrt = 1, total
        ! pointer to discharge/wl dependent trachytope pointer index
        ntrt_qzs => crs_obs(itrt)%itrt
        ! pointer to start and end index of discharge/wl dependent trachytope pointer table index
        idx_start => crs_obs(itrt)%idx_start
        idx_end   => crs_obs(itrt)%idx_end
        ! pointer to number of parameters in discharge/wl dependent trachytope pointer table index
        nropars => crs_obs(itrt)%nropars
        !
        if (crs_obs(itrt)%val < table_q_zs(idx_start)) then 
            ! value through cross-section/observation is smaller than first value in table,
            ! so take first set of values from the table 
            do mropar = 1, nropars
                rttdef(ntrt_qzs, mropar) = rttdef_q_zs(idx_start, mropar)
            end do
        elseif (crs_obs(itrt)%val >= table_q_zs(idx_end)) then 
            ! value through cross-section/observation is larger than or equal to last value in table,
            ! so take last set of values from the table 
            do mropar = 1, nropars
                rttdef(ntrt_qzs, mropar) = rttdef_q_zs(idx_end, mropar)
            end do
        else
            ! value through cross-section/observation lies in the range of values in the table,
            ! so find interval
            do m = crs_obs(itrt)%idx_start, crs_obs(itrt)%idx_end-1
                if (crs_obs(itrt)%val < table_q_zs(m+1)) then
                    ! m is at the right position now
                    exit
                end if
            end do
            ! compute values = slope*val + cross
            do mropar = 1, nropars
                rttdef(ntrt_qzs, mropar) = cross(m, mropar) + crs_obs(itrt)%val * slope(m, mropar)
            end do
        end if
    end do
end subroutine update_rttdef

subroutine chktrt(lundia    ,error     ,griddim   , & 
                & gdtrachy  ,flnmD50   ,flnmD90   ,lfbedfrmrou, sedim, ddbval)
!!--description-----------------------------------------------------------------
! - Checks if trachytope definitions are valid and
! if used trachytopes are available in trachytope
! definitions
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use trachytopes_data_module
    use grid_dimens_module, only: griddimtype
    use message_module
    !
    implicit none
    !
    type(trachy_type),target :: gdtrachy
    type (griddimtype), target , intent(in)  :: griddim
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                    , pointer :: nttaru
    integer                    , pointer :: ntrt
    integer                    , pointer :: nmlb
    integer                    , pointer :: nmub
    integer                    , pointer :: mmax
    integer                    , pointer :: nmax
    integer                    , pointer :: nodir
    integer , dimension(:,:)   , pointer :: ittaru
    integer , dimension(:,:)   , pointer :: ittdef
    real(fp), dimension(:)     , pointer :: rgcalu
    logical                    , pointer :: flsedprop_rqrd
!
! Global variables
!
    integer                                                                   :: lundia
    integer                                                                   :: ddbval
!    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: kcu
!    integer, dimension(griddim%nmlb:griddim%nmub)               , intent(in)  :: kcu
!    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: kcv
!    integer, dimension(griddim%nmlb:griddim%nmub)               , intent(in)  :: kcv
    logical                                                                   :: error
    character(256)                                              , intent(in)  :: flnmD50
    character(256)                                              , intent(in)  :: flnmD90
    logical                                                     , intent(in)  :: lfbedfrmrou
    logical                                                                   :: sedim
!
! Local variables
!
    integer        :: i
    integer        :: id
    integer        :: jdir
    integer        :: nm
    integer        :: nmmax
    integer        :: nmaxddb
    character(1)   :: cdir = ''
    character(12)  :: cnum
    character(256) :: errmsg
    
!
!! executable statements -------------------------------------------------------
!
    ittdef        => gdtrachy%gen%ittdef
    flsedprop_rqrd=> gdtrachy%gen%flsedprop_rqrd
    ntrt          => gdtrachy%gen%ntrt
    nodir          => gdtrachy%gen%nodir
    !flnmD50       => gdbedformpar%flnmD50
    !flnmD90       => gdbedformpar%flnmD90

    nmlb           => griddim%nmlb   
    nmub           => griddim%nmub   
    nmax           => griddim%nmax
    mmax           => griddim%mmax
    
    nmaxddb = nmax + 2*ddbval
    
    nmmax = nmaxddb*(mmax + 2*ddbval)

    !
    ! Initialise flags
    !
    flsedprop_rqrd = .false.
    !
    ! Check trachytope definitions
    !
    error = .false.
    !
    ! Check on negative values
    !
    do i = 1, ntrt
       if (ittdef(i, 1) <= 0) then
          cnum = ' '
          write (cnum, '(i12)') ittdef(i, 1)
          !call noextspaces(cnum      ,numlen    )
          !call prterr(lundia    ,'J008'    ,trim(cnum))
          errmsg = 'Trachytopes: Trachytope number <= 0:' // trim(cnum)
          call write_error(errmsg, unit=lundia)
          error = .true.
       endif
       if (ittdef(i, 2) <= 0) then
          cnum = ' '
          write (cnum, '(i12)') ittdef(i, 2)
          !call noextspaces(cnum      ,numlen    )
          !call prterr(lundia    ,'J009'    ,trim(cnum)       )
          errmsg = 'Trachytopes: Trachytope roughness description <= 0: ' // trim(cnum)
          call write_error(errmsg, unit=lundia)
          error = .true.
       endif
       !
       ! Check on double definitions
       !
       do id = 1, i - 1
          if (ittdef(i, 1) == ittdef(id, 1)) then
             cnum = ' '
             write (cnum, '(i12)') ittdef(i, 1)
             !call noextspaces(cnum      ,numlen    )
             !call prterr(lundia    ,'J012'    ,trim(cnum)       )
             errmsg = 'Trachytopes: Trachytope number '// trim(cnum) // ' defined more than once.'
             call write_error(errmsg, unit=lundia)
             error = .true.
          endif
       enddo
    enddo
    !
    ! Check if used trachytopes have been defined
    !
    cdir = ''
    do jdir = 1,nodir
        ittaru        => gdtrachy%dir(jdir)%ittaru
        rgcalu        => gdtrachy%dir(jdir)%rgcalu
        nttaru        => gdtrachy%dir(jdir)%nttaru        
        if (nodir > 1) then
            cdir = char(116+jdir)
        endif
        call chktra(lundia    ,error     ,nmax      ,mmax      ,ittdef    , &
                  & ntrt      ,ittaru    ,nttaru    ,cdir  ,nmmax     , &   !u or v
                  & flsedprop_rqrd       ,lfbedfrmrou      )
    enddo 

    if (flsedprop_rqrd) then
       !
       if (sedim) then
          !
          ! Use D50, D90, rhosol of simulated sediment
          !
          !call prterr(lundia    ,'G051'    , &
          !  & 'Alluvial roughness predictor uses properties of simulated sediment fractions')
             errmsg = '*** MESSAGE ' // 'Alluvial roughness predictor uses properties of simulated sediment fractions'
             call write_error(errmsg, unit=lundia)
       else
          !
          ! Use BedformD50 and BedformD90
          !
          !call prterr(lundia    ,'G051'    , &
          !  & 'Alluvial roughness predictor uses the following sediment properties')
          errmsg = '*** MESSAGE ' // 'Alluvial roughness predictor uses the following sediment properties'
          call write_error(errmsg, unit=lundia)
          write(lundia,'(12x,2a)') 'D50 (keyword BdfD50) :', trim(flnmD50)
          write(lundia,'(12x,2a)') 'D90 (keyword BdfD90) :', trim(flnmD90)
       endif
       !
    endif
    !
    ! Check on negative values in RGCALU and RGCALV
    !
    do jdir = 1, nodir
       rgcalu => gdtrachy%dir(jdir)%rgcalu
       !
       if (nodir .eq. 1) then 
             errmsg = 'Trachytopes: Negative value(s) ' //                     &
                & 'roughness calibration.'
       else
             errmsg = 'Trachytopes: Negative value(s) ' //                     &
                & 'roughness calibration in '//char(84+jdir) //'-direction.'   ! U or V
       end if
       do nm = 1, nmmax
          if (gdtrachy%dir(jdir)%kcu_trt(nm) > 0 .and. gdtrachy%dir(jdir)%rgcalu(nm)<=0.0) then
             !call prterr(lundia    ,'J013'    ,'U'       )
             call write_error(errmsg, unit=lundia)
             error = .true.
             goto 220
          endif
       enddo
    enddo
220 continue
end subroutine chktrt

                
subroutine chktra(lundia    ,error     ,nmax      ,mmax      ,ittdef    , &
                & ntrt      ,ittaru    ,nttaru    ,cdir      ,nmmax     , &
                & flsedprop_rqrd       ,lfbedfrmrou)
!!--description-----------------------------------------------------------------
! - Checks used trachytopes are available in
! trachytope definitions.
! Called for U/V-directions
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use message_module
    use trachytopes_data_module, only: TRACHY_MISSING_VALUE
    !use globaldata
    !
    implicit none
    !
    !type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
!
! Global variables
!
    integer                                    :: lundia
    integer                      , intent(in)  :: mmax
    integer                      , intent(in)  :: nmax
    integer                      , intent(in)  :: nmmax
    integer                      , intent(in)  :: ntrt
    integer                      , intent(in)  :: nttaru
    integer, dimension(ntrt, 2)  , intent(in)  :: ittdef
    integer, dimension(nttaru, 4), intent(in)  :: ittaru
    logical                      , intent(out) :: error
    logical                                    :: flsedprop_rqrd
    logical                                    :: lfbedfrmrou
    character(1)                               :: cdir
!
! Local variables
!
    integer                     :: i
    integer                     :: itd
    integer                     :: itt
    integer                     :: nmsgnm
    integer                     :: nmsgtd
    logical                     :: lfound
    character(12), dimension(2) :: cnum
    character(132)              :: cmsg
!
!! executable statements -------------------------------------------------------
!
    ! Set message counters
    !
    nmsgnm = 0
    nmsgtd = 0
    !
    ! Perform the checks.
    !
    do itt = 1, nttaru
       !
       ! Skip block separator
       !
!       if (ittaru(itt, 1)== - 1 .and. ittaru(itt, 2)== - 1 .and. ittaru(itt, 3) &
       if (ittaru(itt, 4)== - 77777 .and. ittaru(itt, 3) &
         & == - 1) then
          goto 100
       endif
       !
       ! Check NM values
       !
!       if (ittaru(itt, 1)<=0 .or. ittaru(itt, 1)>nmax .or. ittaru(itt, 2)       &
!         & <=0 .or. ittaru(itt, 2)>mmax) then
       if ((ittaru(itt, 4)<=0 .or. ittaru(itt, 4)>nmmax) .and. ittaru(itt, 4) /= TRACHY_MISSING_VALUE) then !WO-improve 
          !
          ! NM out of range, produce message
          !
          nmsgnm = nmsgnm + 1
          error  = .true.
          !
          ! Break off the check if 10 messages are exceeded
          !
          if (nmsgnm>10) then
             !call prterr(lundia    ,'J010'    ,cdir      )
             if (cdir == '') then
                 cmsg = 'Trachytopes: Too many errors.'
             else             
                 cmsg = 'Trachytopes: Too many NM-Errors in '// cdir //'-direction.'
             endif    
             call write_error(cmsg, unit=lundia)
             error = .true. 
             exit
          endif
          write (cnum(1), '(i12)') ittaru(itt, 4)
          if (cdir == '') then
               cmsg = 'Trachytopes: TRTROU: ' // &
                   & 'Net link out of Range: (' // trim(cnum(1)) //')'
          else 
               cmsg = 'Trachytopes: TRTROU: ' // 'Trachytopes in ' // cdir // '-Direction, ' //                 &
                   & 'NM out of Range: (' // trim(cnum(1)) //')'
          endif
          !call prterr(lundia    ,'J001'    ,cmsg      )
          call write_error(cmsg, unit=lundia)
       endif
       !
       ! Check if trachytope has been defined
       !
       lfound = .false.
       do itd = 1, ntrt
          if (ittaru(itt, 3)==ittdef(itd, 1)) then
             ! alternative without searching
             ! ittaru(itt,5) = itd
             !
             ! Check if grain sizes are required.
             !
             if (ittdef(itd, 2)==103 .or. ittdef(itd, 2)==104) then
                flsedprop_rqrd = .true.
             endif
             if (ittdef(itd, 2)==105 .or. ittdef(itd, 2)==106) then
                lfbedfrmrou = .true.
             endif
             lfound = .true.
             exit
          endif
       enddo
       if (.not.lfound) then
          !
          ! Check if this trachytope was used before.
          ! If so go to next item
          !
          do i = 1, itd - 1
             if (ittaru(itt, 3)==ittaru(i, 3)) goto 100
          enddo
          !
          ! Trachytope was not used before, produce message
          !
          nmsgtd = nmsgtd + 1
          error  = .true.
          !
          ! Break off the check if 10 messages are exceeded
          !
          if (nmsgtd==10) then
             !call prterr(lundia    ,'J011'    ,cdir      )
             if (cdir == '') then
                cmsg = 'Trachytopes: Too many definition errors.'
             else
                cmsg = 'Trachytopes: Too many definition errors in '//cdir//'-direction.'
             endif
             call write_error(cmsg, unit=lundia)          
             error = .true. 
             exit
          endif
          write (cnum(1), '(i12)') ittaru(itt, 3)
          !call noextspaces(cnum(1)   ,numlen(1) )
          if (cdir == '') then
             cmsg = 'Trachytopes: TRTROU: ' // 'Trachytope not defined, Number: ' // trim(cnum(1))
          else
             cmsg = 'Trachytopes: TRTROU: ' // 'Trachytope in ' // cdir // '-Direction ' //                   &
                  & 'not defined, Number: ' // trim(cnum(1))
          endif
          !call prterr(lundia    ,'J001'    ,cmsg      )
          call write_error(cmsg, unit=lundia)          
       endif
  100  continue
    enddo
end subroutine chktra

end module m_trtrou
