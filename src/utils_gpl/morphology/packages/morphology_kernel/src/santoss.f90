!> \page morphology_lib Library of sediment transport formulations
!> \section formula_santoss SANTOSS practical sand transport model
!!
!! See: \ref santoss
!!

!> \anchor santoss
!! Main routine  of the SANTOSS practical sand transport model, version 2.08
!! Computations of the orbital motions in the nearshore morphodynamical model
!! using the formula of Abreu et al. (2010)
!!
subroutine santoss(h, d50, d90, hrms, tp, uorb, teta, uuu, vvv, umod, zumod, &
                 & ag, vicmol, rhosol, rhowat, sw_effects, as_effects, &
                 & pl_effects, sl_effects, dzduu ,dzdvv , i2d3d, &
                 & sbcu, sbcv, sbwu, sbwv, sswu, sswv , &
                 & uwc, uwt, rh, ksw, ksc, ucrepr, utrepr, fcwc, fcwt, &
                 & screpr, strepr, pc, pt, occ, otc, ott, oct, tc, tt, &
                 & phicx, phitx, qsu, sk, as, &
                 & error, message)
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
!!--declarations----------------------------------------------------------------
    use precision
    use morphology_data_module
    use mathconsts, only: sqrt2, degrad, raddeg
    use ieee_arithmetic, only: ieee_is_nan

    implicit none
!
! arguments
!
    real(fp)                        , intent(in)    :: h          !< water depth [m]
    real(fp)                        , intent(in)    :: d50        !< sediment diameter of fraction [m]
    real(fp)                        , intent(in)    :: d90        !< 90-percentile diameter of local sediment mixture [m]
    real(fp)                        , intent(in)    :: hrms       !< wave height [m]
    real(fp)                        , intent(in)    :: tp         !< wave period [s]
    real(fp)                        , intent(in)    :: uorb       !< orbital velocity at the bed [m/s]
    real(fp)                        , intent(in)    :: teta       !< angle between wave dir and local grid orientation [deg]
    real(fp)                        , intent(in)    :: uuu        !< m component of characteristic velocity [m/s]
    real(fp)                        , intent(in)    :: vvv        !< n component of characteristic velocity [m/s]
    real(fp)                        , intent(in)    :: umod       !< magnitude of characteristic velocity [m/s]
    real(fp)                        , intent(in)    :: zumod      !< height above bed of characteristic velocity [m]
    real(fp)                        , intent(in)    :: ag         !< gravitational acceleration [m/s2]
    real(fp)                        , intent(in)    :: vicmol     !< molecular viscosity of water [m2/s]
    real(fp)                        , intent(in)    :: rhosol     !< solid sediment density [kg/m3]
    real(fp)                        , intent(in)    :: rhowat     !< local water density [kg/m3]
!
    integer                         , intent(in)    :: sw_effects !< surface wave effects 1 for on 0 for off
    integer                         , intent(in)    :: as_effects !< wave asymmetry effects 1 for on 0 for off
    integer                         , intent(in)    :: pl_effects !< phase lag effects 1 for on 0 for off
    integer                         , intent(in)    :: sl_effects !< slope effect oaaja in critical shear stress 1 for on 0 for off
!
    integer                         , intent(in)    :: i2d3d      !< switch 2D/3D (2 = 2D, 3 = 3D)
    real(fp)                        , intent(in)    :: dzduu      !< slope in m direction [m/m]
    real(fp)                        , intent(in)    :: dzdvv      !< slope in n direction [m/m]
!
    real(fp)                        , intent(out)   :: sbcu       !< bed load due to currents, m component [m3/m/s]
    real(fp)                        , intent(out)   :: sbcv       !< bed load due to currents, n component [m3/m/s]
    real(fp)                        , intent(out)   :: sbwu       !< bed load due to waves, m component [m3/m/s]
    real(fp)                        , intent(out)   :: sbwv       !< bed load due to waves, n component [m3/m/s]
    real(fp)                        , intent(out)   :: sswu       !< susp load due to waves, m component [m3/m/s]
    real(fp)                        , intent(out)   :: sswv       !< susp load due to waves, n component [m3/m/s]
!
    real(fp)                        , intent(out)   :: uwc        !< orbital velocity at crest [m/s]
    real(fp)                        , intent(out)   :: uwt        !< orbital velocity in trough [m/s]
    real(fp)                        , intent(out)   :: rh         !< ripple height [m]
    real(fp)                        , intent(out)   :: ksw        !< wave roughness height [m]
    real(fp)                        , intent(out)   :: ksc        !< current roughness height [m]
    real(fp)                        , intent(out)   :: ucrepr     !< representative velocity at crest [m/s]
    real(fp)                        , intent(out)   :: utrepr     !< representative velocity in trough [m/s]
    real(fp)                        , intent(out)   :: fcwc       !< friction factor at crest [-]
    real(fp)                        , intent(out)   :: fcwt       !< friction factor in trough [-]
    real(fp)                        , intent(out)   :: screpr     !< representative shear stress at crest [-]
    real(fp)                        , intent(out)   :: strepr     !< representative shear stress in trough [-]
    real(fp)                        , intent(out)   :: pc         !< phase lag parameter at crest [-]
    real(fp)                        , intent(out)   :: pt         !< phase lag parameter in trough [-]
    real(fp)                        , intent(out)   :: occ        !< load entrained and transported during crest period [-]
    real(fp)                        , intent(out)   :: otc        !< load entrained during trough period and transported during crest period [-]
    real(fp)                        , intent(out)   :: ott        !< load entrained and transported during trough period [-]
    real(fp)                        , intent(out)   :: oct        !< load entrained during crest period and transported during trough period [-]
    real(fp)                        , intent(out)   :: tc         !< wave period at crest [s]
    real(fp)                        , intent(out)   :: tt         !< wave period in trough [s]
    real(fp)                        , intent(out)   :: phicx      !< dimensionless sediment transport at crest [-]
    real(fp)                        , intent(out)   :: phitx      !< dimensionless sediment transport in trough [-]
    real(fp)                        , intent(out)   :: qsu        !< volumetric transport rate [m2/s]
    real(fp)                        , intent(out)   :: sk         !< skewness [-]
    real(fp)                        , intent(out)   :: as         !< asymmetry [-]
!
    character(len=256)              , intent(out)   :: message    !< error message
    logical                         , intent(out)   :: error      !< flag indicating whether an error occurred
!

!
! local variables for input parameters
!
      integer                  :: nm
      real(fp)                 :: timsec
      real(fp)                 :: tanphi     ! tangens of angle of natural talud
      real(fp)                 :: u
      real(fp)                 :: utot
      real(fp)                 :: v
!
! local variables
!
      logical                  :: includes_waves
      integer                  :: i
      integer                  :: istat
      integer                  :: j
      integer                  :: nt
      integer                  :: as_effects_
      integer                  :: sw_effects_
      real(fp)                 :: ang        ! angle between net current direction and positive orbital velocity [deg]
      real(fp)                 :: alphas
      real(fp)                 :: alphar
      real(fp)                 :: aw
      real(fp)                 :: alpha
      real(fp)                 :: ang_cur
      real(fp)                 :: ang_ubot
      real(fp)                 :: a1
      real(fp)                 :: a2
      real(fp)                 :: a3
      real(fp)                 :: b
      real(fp)                 :: c1
      real(fp)                 :: delwblt
      real(fp)                 :: delta
      real(fp)                 :: dzbds_c
      real(fp)                 :: dzbds_t
      real(fp)                 :: dstar      ! dimensionless grain-size [-]
      real(fp)                 :: dstars     ! dimensionless grain size associated with representative grain size d50s [-]
      real(fp)                 :: d50s       ! representative grain size [m]
      real(fp)                 :: fc
      real(fp)                 :: fw
      real(fp)                 :: fcw
      real(fp)                 :: fsl_cr_c
      real(fp)                 :: fsl_cr_t
      real(fp)                 :: hs         ! significant wave height  [m]
      real(fp)                 :: k
      real(fp)                 :: k0
      real(fp)                 :: k1
      real(fp)                 :: km
      real(fp)                 :: l
      real(fp)                 :: l0
      real(fp)                 :: m1
      real(fp)                 :: n1
      real(fp)                 :: ocr
      real(fp)                 :: oc
      real(fp)                 :: ot
      real(fp)                 :: omega
      real(fp)                 :: pcr       ! critical phase lag parameter [-]
      real(fp)                 :: phi_ab
      real(fp)                 :: phicy
      real(fp)                 :: phity
      real(fp)                 :: qsx
      real(fp)                 :: qsy
      real(fp)                 :: qsv
      real(fp)                 :: r
      real(fp)                 :: rl
      real(fp)                 :: r_ab
      real(fp)                 :: sfltc
      real(fp)                 :: sfltt
      real(fp)                 :: sflt
      real(fp)                 :: scr
      real(fp)                 :: scr_c
      real(fp)                 :: scr_t
      real(fp)                 :: sc
      real(fp)                 :: s1
      real(fp)                 :: st
      real(fp)                 :: swc
      real(fp)                 :: swt
      real(fp)                 :: scx
      real(fp)                 :: scy
      real(fp)                 :: stx
      real(fp)                 :: sty
      real(fp)                 :: sc_sflt
      real(fp)                 :: st_sflt
      real(fp)                 :: swt_sflt
      real(fp)                 :: swcrepr
      real(fp)                 :: swtrepr
      real(fp)                 :: swc_sflt
      real(fp)                 :: scxrepr
      real(fp)                 :: scyrepr
      real(fp)                 :: stxrepr
      real(fp)                 :: styrepr
      real(fp)                 :: screpr_v
      real(fp)                 :: screpr_u
      real(fp)                 :: strepr_v
      real(fp)                 :: strepr_u
      real(fp)                 :: tcu
      real(fp)                 :: tcd
      real(fp)                 :: ttu
      real(fp)                 :: ttd
      real(fp), dimension(:), allocatable :: tw
      real(fp)                 :: theta
      real(fp)                 :: ustar
      real(fp)                 :: unet      ! magnitude of current velocity [umod] at reference level zumod [m/s]
      real(fp)                 :: urms      ! rootmeansquare orbital velocity [m/s]
      real(fp)                 :: uw
      real(fp)                 :: uwcrepr
      real(fp)                 :: uwtrepr
      real(fp), dimension(:), allocatable :: uorb_time_serie
      real(fp)                 :: ur
      real(fp)                 :: ucxrepr
      real(fp)                 :: ucyrepr
      real(fp)                 :: utxrepr
      real(fp)                 :: utyrepr
      real(fp)                 :: ucx
      real(fp)                 :: ucy
      real(fp)                 :: utx
      real(fp)                 :: uty
      real(fp)                 :: uc
      real(fp)                 :: ut
      real(fp)                 :: unet_delwblt
      real(fp)                 :: uwmax
      real(fp)                 :: uwmin
      real(fp)                 :: wss
      
      integer :: choice_a, choice_b, choice_c, choice_d, choice_e
!
!! executable statements -------------------------------------------------------
!
    nt = 200 ! must equal length of tw, uorb_time_serie, and ub_ts, ab_ts in santoss_orb
    allocate(tw             (nt), STAT = istat)
    allocate(uorb_time_serie(nt), STAT = istat)
    !
    uwcrepr =  0.0_fp
    uwtrepr =  0.0_fp
    error = .false.
    
    ! local copies of these flags since we may modify them
    sw_effects_ = sw_effects
    as_effects_ = as_effects
!
! set some parameters.
!
    hs      = sqrt2*hrms              ! significant wave height [m]
    pcr     = 1.0_fp
    tanphi  = tan(30.0_fp*degrad)     ! fixed the angle of repose of the natural talud to 30 degrees
!
!   determine the rms orbital velocity
!
!   uorb is computed in orbvel (roller) or setwav (SWAN); based on hrms, for a sine wave orbital time-series with uorb as amplitude corresponds to sqrt(2)*urms
    urms  = 0.5_fp*sqrt2*uorb
!
!   if there is no current and no waves the sand transport is zero
!
    if (comparereal(urms,0.0_fp) == 0 .and. comparereal(umod,0.0_fp) == 0) then
        ! no waves and no currents
        qsu = 0.0_fp
        qsv = 0.0_fp
    else
!
!       current strength and angle between current and wave propagation direction
!
        unet = sqrt(uuu**2+vvv**2)
        ang_cur = atan2(vvv,uuu)*raddeg
        ang_ubot=teta

        if (ang_cur >= ang_ubot) then
            ang = ang_cur-ang_ubot
        else 
            ang = ang_cur-ang_ubot+360.0_fp
        endif
!
        if (hs > 0.0_fp) then
            ! include wave effect
!           
!           calculate velocity and acceleration parameters for the wave 
!           approximation of Abreu et al. (2010) by Reussink et al. (2012)
!           
            call santoss_rrr12(ag, hs, tp, h, sk, as, phi_ab, r_ab, ur, km)
!           
!           calculate wave time serie by Abreu et al. (2010)
!           
            call santoss_abreu(hrms, km, h, r_ab, phi_ab, urms, tp, nt, tw, uorb_time_serie)
!           
!           check wether it is current alone
!           
            do i = 1, nt
                uwmax = max(uwmax,uorb_time_serie(i))
                uwmin = min(uwmin,uorb_time_serie(i))
            enddo
            
            if (comparereal(uwmax,0.0_fp) == 0 .and. comparereal(uwmin,0.0_fp) == 0) then
                includes_waves = .false.
            else
                includes_waves = .true.
            endif
        else
            ! currents, but no waves
            uorb_time_serie = 0.0_fp
            tw = 0.0_fp
            includes_waves = .false.
        endif

        if (.not. includes_waves) then
            ! currents, but no wave effect
            uw      =  0.0_fp
            aw      =  0.0_fp
            uwt     =  0.0_fp
            uwc     =  0.0_fp
            uwcrepr =  0.0_fp
            uwtrepr =  0.0_fp
            !
            ucxrepr =  uwcrepr + unet*cos(ang*degrad) ! x-component representative crest velocity   (wave+current)
            ucyrepr =            unet*sin(ang*degrad) ! y-component representative crest velocity   (wave+current)
            ucrepr=sqrt(ucxrepr**2+ucyrepr**2)        ! representative crest velocity   (wave+current)
            !
            utxrepr = -uwtrepr + unet*cos(ang*degrad) ! x-component representative trough velocity  (wave+current)
            utyrepr =            unet*sin(ang*degrad) ! y-component representative trough velocity  (wave+current)
            utrepr=sqrt(utxrepr**2+utyrepr**2)        ! representative trough velocity  (wave+current)
            !
            r = 0.5_fp
            b = 0.5_fp
        else
            ! include wave effect
!
!           calculate orbital periods and velocities
!           for this first call set unet = 0.
!
            call santoss_orb(nt, as_effects, tw, uorb_time_serie, 0.0_fp, ang, tp, &
                     & rhowat, h, hs, aw, uw, uwc, uwt, uwcrepr, uwtrepr, &
                     & tc, tcu, tcd, tt, ttu, ttd, uc, ut, ucx, utx, ucy, uty, &
                     & ucxrepr, utxrepr, ucrepr, utrepr, b)
        endif
        delta = (rhosol-rhowat)/rhowat
!
!       calculate ripple height and length
!
        call santoss_ripple(d50, uwc, uwt, delta, ag, aw, rh, rl)
!
!       calculate shields parameter
!
!       first for the case of only a current near the bed
        if (.not. includes_waves) then
            ! currents, but no wave effect
            call santoss_bsscurrent(i2d3d, ag, h, d50, d90, delta, unet, ang, &
                     & zumod, rh, rl, unet_delwblt, delwblt, sc, scx, scy)
            screpr  = sc
            scxrepr = scx
            scyrepr = scy
            strepr  = 0.0_fp
            stxrepr = 0.0_fp
            styrepr = 0.0_fp
            st      = 0.0_fp
            stx     = 0.0_fp
            sty     = 0.0_fp
            swc     = 0.0_fp
            swt     = 0.0_fp
            tc      = tp
            tcu     = 0.0_fp
            tcd     = 0.0_fp
            tt      = 0.0_fp
            ttu     = 0.0_fp
            ttd     = 0.0_fp
            sw_effects_ = 0
            as_effects_ = 0
        else
            ! include wave effect
!           define general friction factor (wave + current) and net velocity at edge boundary layer
!           using regular waves: so one friction factor and unet_delwblt for whole time serie.
!           first call to santoss_bss1 using aw, uw, uwc and uwt based on santoss_orb call with unet_delwblt=0
            call santoss_bss1(i2d3d, ag, h, d50, d90, delta, aw, uw, &
                     & unet, zumod, rh, rl, uwc, uwt, ang, uc, ut, &
                     & theta, ksw, ksc, fc, fw, fcw, unet_delwblt, alpha, delwblt)

!           final definition of (representative) velocities and partial periods
            call santoss_orb(nt, as_effects, tw, uorb_time_serie, unet_delwblt, ang, tp, &
                     & rhowat, h, hs, aw, uw, uwc, uwt, uwcrepr, uwtrepr, &
                     & tc, tcu, tcd, tt, ttu, ttd, uc, ut, ucx, utx, ucy, uty, &
                     & ucxrepr, utxrepr, ucrepr, utrepr, b)
            r = uwc/(uwc+uwt)

!           final call to santoss_bss1 using aw, uw, uwc and uwt based on santoss_orb call with unet_delwblt/=0
            call santoss_bss1(i2d3d, ag, h, d50, d90, delta, aw, uw, &
                     & unet_delwblt, delwblt, rh, rl, uwc, uwt, ang, uc, ut, &
                     & theta, ksw, ksc, fc, fw, fcw, unet_delwblt, alpha, delwblt)

            ucxrepr = uwcrepr+unet_delwblt*cos(ang*degrad)     !x-component ucrest [m/s]
            ucyrepr = unet_delwblt*sin(ang*degrad)             !y-component ucrest [m/s]
            utxrepr = -uwtrepr+unet_delwblt*cos(ang*degrad)    !x-component utrough [m/s]
            utyrepr = unet_delwblt*sin(ang*degrad)             !y-component utrough [m/s]

            ! Shields based on maximum velocities: uwc, uwt, uc, ut
            call santoss_bss2(sw_effects_, as_effects_, ag, h, rhowat, rhosol, delta, &
                     & d50, d90, b, r, tp, uw, aw, uwc, uwt, uc, ut, unet_delwblt, &
                     & ang, delwblt, alpha, ksw, ksc, fw, fcw, tc, tt, tcu, tcd, &
                     & ttu, ttd, fc, sc, st, swc, swt, scx, scy, stx, sty, fcwc, fcwt)
            ! Shields based on representative velocities: uwcrepr, uwtrepr, ucrepr, utrepr
            call santoss_bss2(sw_effects_, as_effects_, ag, h, rhowat, rhosol, delta, &
                     & d50, d90, b, r, tp, uw, aw, uwcrepr, uwtrepr, ucrepr, utrepr, unet_delwblt, &
                     & ang, delwblt, alpha, ksw, ksc, fw, fcw, tc, tt, tcu, tcd, &
                     & ttu, ttd, fc, screpr, strepr, swcrepr, swtrepr, scxrepr, scyrepr, stxrepr, styrepr, fcwc, fcwt)
        endif
!
!       slope effect factors:
!          - fsl_cr_c and fsl_cr_t for correction on critical Shields stress
!            eqs. (3.27) and (3.28), Veen (2014)
!
        if (sl_effects==1.0_fp) then
            screpr_u = -scyrepr*sin(teta*degrad)+scxrepr*cos(teta*degrad)
            screpr_v =  scxrepr*sin(teta*degrad)+scyrepr*cos(teta*degrad)
            strepr_u = -styrepr*sin(teta*degrad)+stxrepr*cos(teta*degrad)
            strepr_v =  stxrepr*sin(teta*degrad)+styrepr*cos(teta*degrad)

            if (comparereal(screpr,0.0_fp)==0) then
                dzbds_c = 0.0_fp
            else
                dzbds_c = (screpr_u*-dzduu + screpr_v*-dzdvv)/screpr
            endif
            if (comparereal(strepr,0.0_fp)==0) then
                dzbds_t = 0.0_fp
            else
                dzbds_t = (strepr_u*-dzduu + strepr_v*-dzdvv)/strepr
            endif
            fsl_cr_c = sin(atan(tanphi)+atan(dzbds_c))/sin(atan(tanphi))
            fsl_cr_t = sin(atan(tanphi)+atan(dzbds_t))/sin(atan(tanphi))
        else
            fsl_cr_c = 1.0_fp
            fsl_cr_t = 1.0_fp
        endif

!       correction of the transport due to longitudinal and transverse slope in Delft3D
!
!       critical bed shear stress [-] (Soulsby, 1997, dynamics of marine sands)
        dstar = (delta*ag/vicmol**2)**(1.0_fp/3.0_fp)*d50
        scr = 0.3_fp/(1.0_fp+1.2_fp*dstar)+0.055_fp*(1.0_fp-exp(-0.02_fp*dstar))

!       effect of slope on critical shields stress
        scr_c = fsl_cr_c*scr
        scr_t = fsl_cr_t*scr

!       sheet-flow layer thickness [m]
        sc_sflt = sc
        st_sflt = st
        swc_sflt = swc
        swt_sflt = swt

!       calcuate max sheet flow layer thickness at crest and trough
        call santoss_sfltd99(d50, sc_sflt, st_sflt, swc_sflt, &
                 & swt_sflt, unet_delwblt, sfltc, sfltt)
        sflt = max(sfltc,sfltt)

!
!       fall velocity suspended sand [m/s] (Soulsby, 1997, dynamics of marine sands)
!
        d50s = 0.8_fp*d50
        dstars = (delta*ag/vicmol**2)**(1.0_fp/3.0_fp)*d50s
        wss=vicmol/d50s*(sqrt(10.36_fp**2+1.049_fp*dstars**3)-10.36_fp)

!
!       calculate transport rates
!
!       calibration constants SANTOSS report and ICCE paper
        n1 = 1.20_fp            ! power to the shields number    
        m1 = 10.9711_fp         ! multiplication factor 
        alphas = 8.2_fp         ! phase lag coefficient sheet-flow cases
        alphar = 8.2_fp         ! phase lag coefficient ripple cases   
        call santoss_core(pl_effects, sw_effects_, ag, d50, h, hs, rhosol, rhowat, &
                 & delta, tp, r, b, tc, tt, tcu, tcd, ttu, ttd, sfltc, sfltt, &
                 & wss, rh, scr_c, scr_t, screpr, strepr, scxrepr, scyrepr, stxrepr, styrepr, &
                 & uwc, uwt, n1, m1, alphas, alphar, pcr, &
                 & pc, pt, oc, occ, oct, ot, ott, otc, phicx, phitx, &
                 & phicy, phity, qsx, qsy)
        qsu = -sin(teta*degrad)*qsy+cos(teta*degrad)*qsx
        qsv =  sin(teta*degrad)*qsx+cos(teta*degrad)*qsy
    endif

    ! check if qsu and qsv have a value
    if (ieee_is_nan(qsu) .or. ieee_is_nan(qsv)) then
        error = .true.
        message = 'qsu or qsv ieee_is_nan in SANTOSS formula'
    endif
!
!   set the transport rates
!
    sbcu  = qsu*rhosol
    sbcv  = qsv*rhosol
    sbwu  = 0.0_fp
    sbwv  = 0.0_fp
    sswu  = 0.0_fp
    sswv  = 0.0_fp

    !
    deallocate(tw             , STAT = istat)
    deallocate(uorb_time_serie, STAT = istat)
end subroutine santoss
