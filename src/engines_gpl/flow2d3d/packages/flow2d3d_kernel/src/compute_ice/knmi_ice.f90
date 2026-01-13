subroutine knmi_ice (lundia ,tm   ,fwind     ,cloud     ,sboltz    , &
              & anglat ,tair      ,rhum      ,precip    , &
              & zi     ,zs        ,tempw     ,zw        , &
              & tn     ,exch0     ,nm        , &
              & snwat  ,msnage    )
!
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2013.                                     
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
!  $Id: knmi_ice.f90 64423 2019-07-24 11:30:09Z goede $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20190705_ice_modelling/src/engines_gpl/flow2d3d/packages/kernel/src/compute_ice/knmi_ice.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Performs for one computational cell one time step (of 12 hrs)
!              of the KNMI method
! 
! Program:  IJS.F
! Purpose:  Simulation of the temperature and ice cover of inland
!           surface waters.
! Author:   H.R.A.Wessels, KNMI De Bilt
! Version:  1989, Dec. 29
! Modified: 1990, Dec. 31 : error in conversion dewpoint>e (3.1)
!           1991, Feb. 11 : error in evap./snowmelt-loop at label 241
!           1991, Feb. 21 : improved option wind-clearings (3.5)
!           1991, Apr. 15 : for humidity flag also 3 or 4 allowed (2)
!                           also skipping of mcnt datalines (1,2,3.2)
!           1991, Sep. 19 : improved summer turbidity & albedo (3.4.1)
!                           screen print morning water temp. if >4 deg
!           1991, Nov. 11 : also mtown=2-99 for urban effect (3.4.3)
!           1992, Dec. 32 : nh not allowed greater than nn (3.4.1)
!           1997, Jan. 16 : error (small effect) in Ts (3.4.2)
! Reference:See component description
!
! Method:   The program reads from and writes to the same file with
!           meteorological data (see description in the 'Users Manual).
!           In this file the last three record items are reserved
!           for output values: temperature, ice thickness and snow
!           thickness. Of course separate output files will have to be
!           generated for different stations and years. The program can
!           accomodate various boundary conditions:
!           - water depth,
!           - water with internal heat source; e.g. in towns,
!           - wind induced clearings, retarding initial ice formation,
!           - daily snow cover removal for small ice tracks,
!           - only removal of snow layers thicker than 5 cm., which
!             is the Netherlands practice for organised skating tours.
!           These variables are specified in the first record, together
!           with starting values of e.g. water temperature.
!           The snow cover history is followed in separate arrays for
!           equivalent water content and snow age, where the array index
!           (of the snow layers) increases backward in time.
!           Most variables have - apart from prefixes- their usual
!           meteorological meaning:
!             mm=month, dd=day, hh=hour, ff=wind velocity, n=cloud(1/8)
!             ta=air temperature, rh=rel.humidity, e=vapour pressure
!             prec=precipitation, rd=radiation, ev=evaporation
!           The surface condition is expressed by mx, with the value
!           0=ice+snow, 1=clear ice, 2=wet ice, 3=open water
!           The thickness/depth of ice, snow and water is zi, zs, zw
!           (in mm., but zw in m.)
!
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    use timers
    !
    implicit none
    !
    integer    :: lundia, nm
    real(fp)   :: tm, fwind, tair, rhum, cloud, anglat, precip, sboltz, &
                & tempw, zi, zs, zw, tn, exch0
    integer, dimension(15)  :: msnage
    real(fp), dimension(15) :: snwat
!
! Local variables
!
      integer    ::   mark,mmark,mmm,mdd,mhh,mff,mta,mrh,n,mww,mprec,mtw,mtw0, &
     &     mzi,mzs,j,jj,mday,nn,nh,mzw,mzs0,mx,mtime,mslow, &
     &     mprint,mtown,mremov,mclear,mlat,mios,ntm
     real(fp)   :: qzi,ta,tw,tn0,te,pptf,ts,twet,sntot,snloss, &
     &     ppb,ppy,rdglob,rdlong,rdnet,rdhelp,emiss,albedo,wcond, &
     &     e,es,ev,rprec,sollen,soldcl,solelv,exch,exhelp
!     -- pptf= freezing temperature, ppb= constant Stephan-Boltzmann,
!     -- ppy=radians/degrees
!
!! executable statements -------------------------------------------------------
!
    !
    ! Conversion from (input) reals to integers:
    !
    mday  = int(1.0_fp + tm/24.0_fp)  - 5  !! correctie i.v.m. KNMI dagbepaling
    ntm   = nint(tm)
    mhh   = mod(ntm,24)
    if (mhh .eq. 0) then
       mday = mday - 1
       mhh  = 24
    endif    
    mzi   = nint(1000.0_fp * zi)       ! mzi is in mm
    mzs   = nint(1000.0_fp * zs)       ! mzs is in mm
    mtw   = nint(10.0_fp * tempw)      ! mtw in in tens of degrees
    mlat  = nint(anglat)
    mta   = nint(10.0_fp * tair)       ! mta in in tens of degrees
    mrh   = nint(rhum)
    mprec = nint(precip * 86400.0_fp * 100.0_fp)  ! [precip]= m/s --> mm/day
    mff   = nint(fwind)
    n     = nint(100.0_fp * cloud)     ! cloud is in fractions and n in %
    !
    ! Check input
    !
    IF (nm .EQ. 41) THEN
     write (lundia,'(1x,''KNMI wind='',i3,''; air temp='',i4,''; rhum='',i4, &
           &  ''; cloud='',i2,''; precip='',i4,''; temper='',i4)') &
           &  mff,mta,mrh,n,mprec,mtw
     write (lundia,'(1x,''(check data) mday='',i6,''; ntm='',i6,''; mhh='',i4, &
           &  ''; mzi='',i2,''; mzs='',i4)') &
           &  mday,ntm,mhh,mzi,mzs
    ENDIF
    !
    ! Initialisation
    !
    pptf = 0.0_fp
    ppb  = sboltz
    ppy  = 0.0174527_fp
    !
    mtown = 1     ! initialisation of ?? (see first line of KNMI input)
    mremov = 0    ! initialisation of removal of snow (see first line of KNMI input)
    mclear = 0    ! initialisation of ?? (see first line of KNMI input)
    mark = 9      ! initialisation of ?? (see first line of KNMI input)
    mzw = 2       ! initialisation of ?? (see first line of KNMI input)
    !
    IF (mzw .LT. 1) mzw=1
!      IF (mtown .EQ. 1) mtown=16
!      IF (mtown .EQ. 0) mtown=1
!
!     -- Use latitude 52 N as default.
      if ((mlat .EQ. 0) .OR. (ABS(mlat) .GT. 90)) mlat=52
      !  tn=0.6  ! now input parameter
      tw=mtw/10.0
      !  exch0=4.0  ! now input parameter
      qzi=1.0*mzi
!
!     -----------------------------------------------------------------
!     -- Start of main loop, for each time step of 12 hours.
!     -- Note that mtw,mzi,mzs0 are now read as dummmy variables.
!
! 101 jcnt=jcnt+1
      mtw0=mtw
      mios=-2
!     READ (8,10, ERR=999, IOSTAT=mios, REC=jcnt)
!    *        mark,mmm,mdd,mhh,mff,mta,mrh,n,mww,mprec,mtw,mzi,mzs0
      mmark=mark
      IF (mmark .LT. 5) mmark=mmark+5
!     IF ((mcnt .NE. 0) .AND. (jcnt .EQ. mcnt+2)) THEN
!       tw=mtw/10.0
!       qzi=1.0*mzi
!       mzs=mzs0
!       snwat(8)=0.125*mzs
!       msnage(8)=8
!       GOTO 101
!     ENDIF
!
!       ---------------------------------------------------------------
!*             3.Computations for each 12 hourly observation.
!*             3.1.Preparations:
!     -- Call wet bulb; test on 'rain'; correct timestep for new ice.
!
      ta=mta/10.0
      e=mrh*0.06107*10.0**(7.6*ta/(ta+242.0))
      IF (mmark .EQ. 9) e=6.107*10.0**(0.76*mrh/(mrh/10.+242.0))
      tn0=tn
      tn=TWET(ta,e,lundia)
      ! if (nm.eq.41) write (lundia,*) 'part 3', tn,tn0,ta,e,mmark,mrh
!
!     -- If not already specified on input, precipitation at warm
!     -- wet bulb temperatures is treated as 'rain' (minus sign):
      rprec=mprec/10.0
      IF (((tn .GT. 0.5) .AND. (tn0 .GT. 0.5)) .OR. (tn+tn0 .GT. 2)) &
     &        rprec=-ABS(rprec)
!
!     -- For thin ice sheets the time step has to be reduced:
      mslow=1
      IF ((tw .LT. 1.0) .AND. (qzi .LT. 50.0)) mslow=4
!
!*             3.2.Recapitulation of snow cover; ageing of old layers.
!     -- Ageing of old layers, shifting of them in case of fresh snow.
!     -- Computation of new snow thickness and aequivalent water.
!
      DO 201 j=1,15
      IF ((qzi .LE. 0.0) .OR. (mzs .LE. 0)) THEN
        snwat(j)=0.0
        msnage(j)=0
        ENDIF
      IF (msnage(j) .GT. 0) msnage(j)=msnage(j)+1
  201 CONTINUE
      IF ((qzi .GT. 0.0) .AND. (rprec .GT. 0.0)) THEN
        snwat(15)=snwat(15)+snwat(14)
        msnage(15)=msnage(14)
        DO 203 j=14,2,-1
        snwat(j)=snwat(j-1)
        msnage(j)=msnage(j-1)
  203   CONTINUE
        snwat(1)=rprec
        msnage(1)=1
        ENDIF
      mzs=0
      sntot=0.0
      DO 205 j=1,15
      mzs=mzs+nint(1000.0*snwat(j)/(90.0+6.0*msnage(j)))
      sntot=sntot+snwat(j)
  205 CONTINUE
      snloss=0.0
      IF (mzs .LE. 0) mzs=0
!
!*             3.3.Analysis of surface conditions(mx)
!
      do 299 jj=1,mslow
      mtime=12/mslow
      mx=0
      IF (qzi .LE. 0.0) THEN
        mx=3
        ELSE
        IF (mzs .LE. 0) THEN
          mx=1
          IF (tn .GE. 0.0) mx=2
          ENDIF
        ENDIF
!
!*              3.4.Main computation.
!*              3.4.1.Radiation and surface properties
!     -- Computation of global, long wave and net radiation:
!
!     mday=30*mmm+mdd-30       ! mday specified on input (via TM)
      nh=MOD(n,10)
      IF (nh .GT. 8) nh=8
      nn=INT(n/10)
      IF (nn .GT. 8) nn=8
      IF (nh .GT. nn) nh=nn
      sollen=(mday+279.1+1.9*SIN(mday*ppy))*ppy
      soldcl=ATAN(0.398*SIN(sollen))
      rdhelp=1353.0*(1.0+0.01675*COS(mday*ppy))/(1.0-0.01675**2.)
      rdglob=0.0
      ! if (nm.eq.41) write (lundia,*) 'part 3b', sollen,soldcl,rdhelp,mday,mhh
!     -- Estimate daily solar flux from hourly values:
      DO 209, j=-11,12
      solelv=COS((15*j+2.47*SIN(2.0*sollen)-1.9*SIN(mday*ppy))*ppy)
      solelv=SIN(mlat*ppy)*SIN(soldcl)&
     &                        -COS(mlat*ppy)*COS(soldcl)*solelv
!     -- The following turbidity function () may require local version:
!     (in 1992-1993 used (0.60+0.16*solelv)
      IF (solelv .GT. 0.0) rdglob= &
     &              rdglob+rdhelp*solelv*(0.45+0.40*solelv)
!     ! if (nm.eq.41) write (lundia,*) 'part 4', soldcl,rdhelp,solelv,rdglob
  209 CONTINUE
!     -- The solar flux is concentrated in the noon time step.
      rdhelp=rdglob/24.0
      IF (nn .EQ. nh) THEN
        rdglob=rdglob/12.0*(1.0-0.0114*nh*nn)
        ELSE
        rdglob=rdglob/12.0*(1.0-0.0114*(nh+1)*nn)
        ENDIF
      IF ((mhh .EQ. 24) .OR. (rdglob .LT. 0)) rdglob=0.0
      IF (mx .EQ. 0) THEN
        emiss= 0.9
        wcond=(qzi+mzs)/(qzi/2.1+ mzs/(2.0*sntot/mzs))
        albedo=0.95-0.025*msnage(1)
        IF (albedo .LT. 0.30) albedo=0.30
        ELSE
        emiss=0.95
        albedo=0.30
        wcond=2.1
        IF (mx .GE. 2) THEN
          IF (solelv .LT. 0.05) solelv=0.05
          IF (solelv .LT. 0.5) solelv=0.5
          albedo=0.22/solelv-0.05
!          albedo=0.11/solelv
!          albedo=0.06+(1.-FLOAT(nn/8)**2.)*(albedo-0.06)
          wcond=999.0
          IF (mx .EQ. 3) wcond=0.6
          ENDIF
        ENDIF
      rdlong=(0.76+0.004*ta)*ppb*(ta+273.0)**4.0+(2.25*nn+5.25*nh)
      rdnet=(1.0-albedo)*rdglob-emiss*(ppb*(tn+273.0)**4.0-rdlong)
      ! if (nm.eq.41) write (lundia,*) 'part 5', rdlong,rdnet, &
      !       & albedo,rdglob,emiss,ppb,tn, nn,nh 
!
!*             3.4.2.Exchange coefficient and surface temperature.
!     -- Iterative determination of both related quantities
!
      DO 211 j=1,20
      te=tn+rdnet/exch0
      IF ((mx .GE. 2) .OR. (qzi .EQ. 0.0)) THEN
        ts=tw
        ELSE
        ts=(te-pptf)/(1.0+1000.0*wcond/(exch0*(qzi+mzs)))
        IF (mx .EQ. 1) &
     &  ts=ts-0.75*(1.0-albedo)*rdglob/(exch0+1000.0*wcond/(qzi+mzs))
        IF ((mx .EQ. 0) .AND. (sntot .GT. qzi/10.0)) &
     &    ts=pptf+(te-pptf)/(1+0.002*sntot/exch0)
        ENDIF
      IF ((ts .GT. pptf) .AND. (qzi .GT. 0)) ts=pptf
      IF ((ts .LT. ta-0.5) .OR. (mff .GT. 6.5)) THEN
        exch=4.0+2.5*mff
        ELSE
        exch=4.0
        exhelp=1.0-10.0*(ta-ts)/mff**2.0
        IF (exhelp .GT. 1000) exhelp=1000.0
        IF (exhelp .GT. 0.001) exch=exch+2.5*mff*SQRT(exhelp)
        ENDIF
      ! if (nm.eq.41) write (lundia,*) 'part 5a', j,exch,exch0,exhelp
      ! if (nm.eq.41) write (lundia,*) 'RRR', ts,tw,ta,mff,pptf,qzi
      IF (ABS(exch-exch0) .LT. 0.1) goto 213
      exch0=exch
      ! if (nm.eq.41) write (lundia,*) 'part 6', exch
  211 CONTINUE
!
!*             3.4.3.Watertemperature and/or ice thickness
!     -- (the various branches are commmented between the lines)
!
  213 te=tn+rdnet/exch
      IF (mx .EQ. 3) THEN
        tw=tw+exch*mtime*(te-tw)/(1172*mzw)
        tw=tw+mtime*mtown/mzw*3.34/(1172*mzw)
        ENDIF
      ! if (nm.eq.41) write (lundia,*) 'part 6a', mtime,rdnet,tn, te
      ! if (nm.eq.41) write (lundia,*) 'part 6b', mx,tw,exch,mtime,te,mzw
      IF ((tw .LT. pptf) .AND. (mx .EQ. 3)) THEN
!
!       -- For first ice redistribute as latent heat of freezing:
        qzi=13.89*mzw*(pptf-tw)
        tw=pptf
        ENDIF
      ! if (nm.eq.41) write (lundia,*) 'part 7', qzi,tw
!
!     -- Account for heat in rain (with temperature of wet bulb):
      IF ((mx .EQ. 2) .AND. (rprec .LT. 0)) &
     &               qzi=qzi+(tn-pptf)*rprec/mslow/79.150
      IF ((qzi.GT.0.0).AND.(.NOT.((tw.EQ.pptf).AND.(mx.EQ.3)))) THEN
        IF ((mx .EQ. 0) .AND. (te .GT. 0)) THEN
!
!         -- Melting of snow:
          snloss=snloss-mtime*(te-pptf)/(1.0/exch)/90.0
          ELSE
!
!         -- Ice growth or melt:
          qzi=qzi-mtime*(te-pptf)/(1/exch+(qzi+mzs)/1000./wcond)/83.25
          ! if (nm.eq.41) write (lundia,*) 'part 8', qzi,mtime,pptf, &
          !     & exch,mzs,wcond 
          IF (qzi .GT. 1000*mzw) qzi=1000.0*mzw
          ENDIF
!
!       -- Heat loss to ground; estimated extra heat loss inside towns
        qzi=qzi-mtown*0.04*mtime/mzw
!
!       -- Evaporation of ice or snow:
        es=6.107*10.0**(9.5*ts/(ts+266.0))
        ev=mtime*(exch-4.0)*(es-e)/743.925
        IF (ev .LT. 0.0) ev=0.0
        IF (mx .NE. 0) qzi=qzi-ev
        ENDIF
      ! if (nm.eq.41) write (lundia,*) 'part 9', qzi,ev,es,e
!
!     -- Very thin ice (<3 mm) is usually destroyed:
      IF (qzi .LT. 3.0) THEN
        qzi=0.0
        mzs=0
        ENDIF
!
!     -- Update of snow cover to account for melting or evaporation loss
      IF (mx .EQ. 0) snloss=snloss-ev*0.9
      DO 241 j=1,15
      IF ((snwat(j) .NE. 0.0) .AND. (snloss .LT. 0.0)) THEN
        IF (snloss+snwat(j) .GE. 0.0) THEN
          snwat(j)=snwat(j)+snloss
          snloss=0.0
          GOTO 241
          ENDIF
        snloss=snloss+snwat(j)
        mzs=mzs-NINT(1000.0*snwat(j)/(90.0+6.0*msnage(j)))
        sntot=sntot-snwat(j)
        snwat(j)=0.0
        ENDIF
  241 CONTINUE
      IF (snloss .LT. 0.0) mzs=0
      IF (snloss .LT. 0.0) sntot=0.0
!
!*             3.5.Extra options, e.g. snow removal.
!
!     -- A rough criterium for wind induced clearings, taking into
!     -- account some effect of water depth (which is usually related
!     -- to fetch) and a dependence of ice strength on temperature.
!     -- This option simulates later freezing and earlier break-up.
!     -- The purpose is to properly compute qzi after re-freezing.
      IF (mclear .GT. 0) THEN
        IF (mx .EQ. 3) THEN
          IF (qzi .LT. (10.0+2.0*mff*mzw**.3)/mslow) qzi=0.0
          ELSE
          IF (qzi .LT. (5.+ 3.*(mff*mzw)**.5)*(1-SQRT(ABS(ts)))) &
     &       qzi=0.0
          ENDIF
        ENDIF
      IF (qzi .EQ. 0.0) mzs=0
!
!     -- Snow removal operations (not at nighttime or on ice<46 mm):
      IF ((mhh .EQ. 0) .OR. (qzi .LT. 46.0)) GOTO 299  ! (was mhh=24)
      IF ((mremov .GT. 0) .AND. (mzs .GT. 50)) mzs=0
      IF (mremov .GT. 1) mzs=0
  299 CONTINUE
!
!     -----------------------------------------------------------------
!     --  output is in qzi, tw, mzs
!
      zi  = 0.001_fp * qzi
      zs  = 0.001_fp * mzs
      tempw = tw
       if (nm.eq.41) then
          write (lundia,*) 'part 10: ', zi,zs,tw
       endif
      mzi = nint(qzi)
      mtw = nint(10.0_fp * tw)
      ! IF (nm .EQ. 41) THEN
      !     IF (mprint .NE. 0) &
      !  &     write (lundia, '(a,4i4)') 'ijs output=',mx,mtw,mzi,mzs
      ! ENDIF
!
!  noted that temperature of water may be chenged (zee tw), but there is no
!  coupling to the water system

end subroutine knmi_ice
!----------------------------------------------------------------------
! Function: twet
! Purpose:  computation of wet- or ice-bulb temperature from
!           temperature and vapour pressure
!
! Method:   iteration, starting from a first guess zt2
!
function twet(ptt,pee,lundia)
!!--declarations----------------------------------------------------------------
    use precision
    !
    implicit none
!
! Function result
    real(fp) :: twet
!
! Global variables
!
    real(fp)                      , intent(in) :: ptt
    real(fp)                      , intent(in) :: pee
    integer                       , intent(in) :: lundia
!
! Local variables
!
    real(fp)  :: zt, zt2, zes, zes2
    integer iter
!
!! executable statements
!-------------------------------------------------------
!
    zes2=6.107*10.0**(7.6*ptt/(ptt+242.0))
    zt2=ptt-(1-pee/zes2)/0.19
    ! write (lundia,*)'zt2-orig=',zt2, ptt,pee,zes2
    iter = 0
 10 zt=zt2
    if (zt .LT. 0.0) then
      zes=6.107*10.0**(9.5*zt/(zt+266.0))
      zt2=zt-(zes-pee+0.57*(zt-ptt))/(6154.*zes2/(zt+273.)**2.+0.57)
    else
      zes=6.107*10.0**(7.6*zt/(zt+242.0))
      zt2=zt-(zes-pee+0.66*(zt-ptt))/(5419.*zes2/(zt+273.)**2.+0.66)
    endif
    ! write (lundia,*) iter, zt2,zt
    iter = iter + 1
    ! iF (ABS(zt-zt2) .gt. 0.05) goto 10
    iF (ABS(zt-zt2) .gt. 0.05 .and. iter .lt. 10) goto 10
    twet=zt2
    if (iter .eq. 10) then
        ! write (lundia,*) 'no convergence in TWET', zt,zt2,ptt,pee,zes2
        ! twet=zt2
    ! else
        ! write (lundia,*) 'number of iterations in TWET', iter,zt,zt2,ptt,pee,zes2
    endif
end function twet
