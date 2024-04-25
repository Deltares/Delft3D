!!  Copyright (C)  Stichting Deltares, 2012-2024.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

module partwq_mod
!
!  module declarations
!
!  data definition module(s)
!
use m_stop_exit
use m_waq_precision               ! single/double precision
use timers
use m_densty
!
!  module procedure(s)
!
use grid_search_mod         ! explicit interface
!
implicit none               ! force explicit typing
!
contains
      subroutine partwq ( lgrid  , nmax   , conc   , volume , area   , &
                          npart  , mpart  , wpart  , radius , nodye  , &
                          npwndw , nopart , idelt  , velo   , wvelo  , &
                          const  , nocons , ptlay  , lun2   , nosubs , &
                          nolay  , lgrid2 , mmax   , xb     , yb     , &
                          t0cf   , acf    , nwaste , mwaste , kpart  , &
                          mapsub , layt   , mnmaxk       )
!
!
!                   Deltares
!
!                        d e l p a r    v3.60
!
!
!     system administration : m. zeeuw
!
!
!     created               : 25 may 1994, by a. markus for sizewell
!
!     modified              : march 1999 by rj vos for 3d-temperature model
!                             in this case parameters pblay and gamma are not used
!                             number of constants per outfall/intake increased
!                             to 10 (see ncload)
!
!     function              : calculates the processes that change
!                             the mass of individual particles
!
!     note                  : the user is responsible for the contents
!                             of this routine !
!                             the 3d version:
!                             2 layer option works with kpart, zpart always positive
!                             **** subst icvdf must be excess temperature ****
!                             temperature may not be lower than 1.0e-10!!
!                             26/7/1996: mapfile according to delwaq
!                             ie layers are not substances but segments
!
!
!     subroutines called    : stop_exit.
!
!
!     functions   called    : none.
!
!
!     parameters            :
!
!     name    kind     length     funct.  description
!     ====    ====     ======     ======  ===========
!     acf     real      nocont    input   a-coefficient for each load for buoy
!     angle   real      mnmax2    input   angle of the grid cells
!     area    real      mnmax2    input   surface area of grid cells
!     conc    real nosubs*mnmaxk  input   concentration in two layers
!     const   real      nocons    input   user-defined constants
!     decays  real      nosubs    input   decay coefficient per substance
!     dectmp  real      nosubs    input   decay coefficient temperature
!     dx      real      mnmax2    input   dx of grid cells
!     dy      real      mnmax2    input   dy of grid cells
!     idelt   integer     1       input   model time step
!     kpart   integer   nopart    input   k-value of particles
!     layt    integer     1       input   number of layers in the hydrodynamic database
!     lgrid   integer  nmax*mmax  input   active grid layout of the area
!     lgrid2  integer  nmax*mmax  input   total grid layout of the area
!     lun2    integer     1       input   unit number of report file
!     mapsub  integer     1       input   index array substances
!     mmax    integer     1       input   dimension of lgrid
!     mnmax2  integer     1       input   dimension of one layer
!     mnmaxk  integer     1       input   dimension of volume
!     mpart   integer   nopart    input   m-value of particles
!     mwaste  integer nocont+nodye input m-value of the waste location
!     nmax    integer     1       input   dimension of lgrid
!     nocont  integer     1       input   number of continuous releases
!     nocons  integer     1       input   (max.) number of constants
!     nodye  integer     1       input   number of dye releases
!     nolay   integer     1       input   number of layers
!     nopart  integer     1       input   number of active particles
!     nosubc  integer     1       input   leading dimension of conc.array
!     nosubs  integer     1       input   number of substances
!     npart   integer   nopart    input   n-value of particles
!     npwndw  integer     1       input   start of active nopart number
!     nwaste  integer nocont+nodye input n-value of the waste location
!     ptlay   real        1       input   relative thickness upper layer
!     radius  real  nocont+nodye input   radius of the load
!     t0cf    real      nocont    input   t0-coefficient for each load for buoy.
!     velo    real      mnmaxk    input   absolute velocity in cells
!     volume  real      mnmaxk    input   volumes of the grid cells
!     wpart   real nosubs*nopart  in/out  weight of the particles
!     wvelo   real        1       input   wind velocity
!     xb      real      mnmax2    input   x-values bottom points
!     yb      real      mnmax2    input   y-values bottom points
!     zpart   real      nopart    input   vertical coordinate of particles
!     ----    -----    ------     ------- -----------
!     a0      real        1       local   prefactor
!     cp      real        1       local   specific heat of water
!     densmn  real        1       local   critical density difference for stratification
!     drho    real        1       local   density diff. at outlet
!     dfac    real        1       local   dilution factor
!     dfact   real        1       local   decay factor
!     dxby    real        1       local   characteristic buoyancy length
!     fdivt   real        1       local   4 devided by 3
!     first   logical     1       local   switch
!     fwind   real        1       local   wind influence
!     gamma   real        1       local   empirical coeffient
!     graph   real        1       local   graphitational speed [m/s^2]       (nosubs+1,*)
!     hatm    real        1       local   heat exchange (j)
!     hcapac  real        1       local   heat conversion factor
!     i       integer     1       local   index pointer
!     ic      integer     1       local   grid cell number
!     icount  integer     1       local   counter
!     icvdf   integer     1       local   substance numer for excess temp.
!     ierror  integer     1       local   error logger
!     intake  integer   maxld     local   grid cell intake number
!     inocns  integer     1       local   number of constants requested
!     itime   integer     1       local   entry counter
!     mmloc   integer     1       local   intake grid pointer (second index)
!     maxld   integer     1       local   maximal number of intakes/outlets
!     ncload  integer     1       local   number of constants per outfall/intake
!     nmloc   integer     1       local   intake grid pointer (first  index)
!     pblay   real        1       local   relative thickness lower layer
!     qflow   real      maxld     local   inflow of intake (= flow outlet)
!     rhol    real        1       local   density of release
!     rhow    real        1       local   density of water
!     rhowc   real        1       local   calculated density of water
!     rodivf  real        1       local   1 devided by 4
!     rodivt  real        1       local   1 devided by 3
!     salamb  real        1       local   ambient salinity
!     subnam  char.* 6    1       local   name of this program/file
!     sumdc   real        1       local   summed decays
!     sumwp   real        1       local   summed particle weight
!     t0      real        1       local   initial time step buoyant spreading
!     tex     real        1       local   excess temp. after dilution
!     ttemp   real        1       local   total water temp. in each cell
!     tmpamb  real        1       local   ambient temperature
!     tmpexc  real     maxld      local   excess temperature outlet
!     tmpeff  real        1       local   max. excess temperature
!     tpycn   real        1       local   thickness pycnocline
!     twater  real        1       local   background water temperature
!     twopi   real        1       local   2 pi [rad]
!     wexch   real        1       local   heat exchange coeff. (w/m^3/k)
!     xmloc   real        1       local   x-in-cell of selection
!     xnloc   real        1       local   x-value in the national grid
!     ymloc   real        1       local   y-in-cell of selection
!     ynloc   real        1       local   y-value in the national grid
!
!
!     save values between invocations
!
      save
!
!     declarations
!
      character(len=6) :: subnam = 'partwq'
      logical          :: first   = .true.
!
!     parameters
!
      real(sp), parameter :: p1  = 0.00158   , p2     = 0.018  , p3     =  1.12 ,      &
                             p4     = 0.049  , p5     = 4.48   , p6     =  2.05 ,      &
                             p7     = 3.5    , small  = 1.0e-10, graph  =  9.81 ,      &
                             fdivt  = 4.0/3.0, rodivt = 1.0/3.0, rodivf =1.0/4.0,      &
                             twopi  = 6.2831853

      integer(int_wp ), pointer, dimension(:)     :: npart , mpart
      integer(int_wp ), pointer, dimension(:,:)   :: lgrid , lgrid2
      real   (sp), pointer, dimension(:)     :: volume, area  , velo  , radius , const , xb , yb
      real   (dp), pointer, dimension(:)     :: wvelo
      real   (sp), pointer, dimension(:,:)   :: conc
      real   (sp), pointer, dimension(:,:)   :: wpart

      integer(int_wp ), pointer, dimension(:)     :: kpart    , mapsub
      integer(int_wp ), pointer, dimension(:)     :: nwaste   , mwaste
      real   (sp), pointer, dimension(:)     :: t0cf     , acf

      integer(int_wp ), parameter        :: maxld = 50
      integer(int_wp ), dimension(maxld) :: intake
      real   (sp), dimension(maxld) :: qflow    , tmpexc   , cintk
!
!     local scalars
!
      integer(int_wp ) ::  i      , ic     , icount , idelt  , iposv  , k      , min
      integer(int_wp ) ::  icvdf  , il     , inocns , ipos   , ipos0  , layloc , layt
      integer(int_wp ) ::  lun2   , j      , ierror , il2    , iseg   , itime  , mmloc
      integer(int_wp ) ::  nmloc  , nodye , mmax   , mnmax2 , mnmaxk , ncload
      integer(int_wp ) ::  nloads , nocons , nolay  , nmax   , nosubs , nopart , noptk
      integer(int_wp ) ::  npwndw

      real   (sp) ::  a0     , aby    , drho   , hcapac , r0
      real   (sp) ::  cp     , densmn , dfac   , dxby   , gamma  , pblay
      real   (sp) ::  ctot   , dectmp , dfact  , exp    , flrate , fwind  , hatm
      real   (sp) ::  ptlay  , reduc  , sumdc  , sumwp  , t0     , r10    , r20
      real   (sp) ::  r30    , r40    , r50    , rhol   , rhowc  , rhow   , salamb
      real   (sp) ::  tmpamb , tmpeff , tpycn  , xnloc  , ynloc  , tex    , velosi
      real   (sp) ::  xmloc  , ymloc  , tmastk , ttemp  , wexch
      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "partwq", ithndl )
!
!     conventions regarding the vertical position of the particles:
!     1. a value of kpart(..) between of 2 means the particle
!        is in the lower layer
!     1. a value of kpart(..) between of 1 means the particle
!        is in the upper layer
!     3. the pycnocline is represented by z-coordinate 0.0
!
!     the purpose of this routine is to adjust the mass of the
!     particles (represented by the relative weight in wpart(..))
!     according to specific processes
!
!     do not change any other parameters !
!
!     in this case the excess temperature is calculated after
!     heat exchange over the water surface
!
!     no determination of the radius of the wasteload !!
!
!     reading of input parameters from file
!
      icvdf = mapsub(1)
      if(icvdf <= 0) goto 9999
!
      if (first) then
        first = .false.
!
!       check number of concentrations
!
        inocns = 11
        if (inocns  >  nocons) then
          write(lun2, 99001) subnam, inocns, nocons
          call stop_exit(1)
        endif
!
!       saving values for further calculations
!
        cp     = const( 1)
        rhow   = const( 2)
        salamb = const( 3)
        tmpamb = const( 4)
        tmpeff = const( 5)
        dfac   = const( 6)
        tpycn  = const( 7)
        gamma  = const( 8)
        pblay  = const( 9)
        if (nolay  == 1 .and. pblay /= (0.0) ) then
          write(lun2, *)  ' Partwq for one layer requires pblay = 0.0 '
          call stop_exit(1)
        endif
        if (layt    >  1 .and. pblay /= (0.0) ) then
          write(lun2, *)  ' Error:  ', &
                          ' partwq for 3d-hydrod. requires pblay = 0.0 '
          write(*   , *)  ' Error:  ', &
                          ' partwq for 3d-hydrod. requires pblay = 0.0 '
          call stop_exit(1)
        endif
!
        densmn = const(10)
        dxby   = const(11)
!
        ipos0  = 12
        ncload = 10
!
        nloads = int(const(ipos0) + 0.5)
        if(nloads > maxld) then
           write(lun2, * ) ' Error: maximum of ',maxld,' intakes '
          call stop_exit(1)
        endif
!
        write(lun2,*) '  '
!
        mnmax2 = mnmaxk/layt
        if(layt*mnmax2 /= mnmaxk) then
           write(lun2, * ) ' Error in partwq: mnmax2*layt  /=  mnmaxk'
           call stop_exit(1)
        elseif(mnmax2 /= mmax*nmax) then
           write(lun2,*  ) ' Error in partwq: mnmax2  /=  mmax*nmax '
           write(lun2,*  ) ' nmax = ',nmax,' mmax = ',mmax
           write(lun2,*  ) ' mnmax2=',mnmax2
           call stop_exit(1)
        endif
!
        if(layt==1) then
           if(nolay==1) then
           write(lun2,*) ' You are using the 1 layer temperature model!'
           elseif(nolay==2) then
           write(lun2,*) ' You are using the 2 layer temperature model!'
           else
           write(lun2, * ) ' Error: only one or two layers allowed in 2d'
           call stop_exit(1)
           endif
        else
           write(lun2, * ) ' This is the 3-d temperature model '
           write(*   , * ) ' This is the 3-d temperature model '
        endif
!
        write(lun2,*) '  **** Subst ',icvdf,' must be excess temp **** '
        write(lun2,*) '  '
        write(lun2, 99011) ' Specific heat:          ',cp    ,' j/kg/c'
        write(lun2, 99011) ' Density of water:       ',rhow  ,' kg/m^3'
        write(lun2, 99011) ' Ambient salinity:       ',salamb,' g/l'
        write(lun2, 99011) ' Ambient temperature:    ',tmpamb,' c'
        write(lun2, 99011) ' Max. excess temperature:',tmpeff,' c'
        write(lun2, 99011) ' Dilution factor max. t: ',dfac  ,' [-]'
        write(lun2, *    ) ' -----------------------------------------'
        write(lun2, *    ) ' Constants between dashed lines are only '
        write(lun2, *    ) ' used for 2-layer model: '
        write(lun2, 99011) ' Thickness pycnocline:   ',tpycn ,' m'
        write(lun2, 99011) ' Empirical coeff. gamma: ',gamma ,' [-]'
        write(lun2, 99011) ' Rel.thickness bot.layer:',pblay ,' [-]'
        write(lun2, 99011)                                        &
           ' Critical density difference (stratified or not?):',  &
           densmn ,' kg/m^3'
        write(lun2, 99011) ' Character. length buoyancy:',dxby,' m'
        write(lun2, *    ) ' -----------------------------------------'
        write(lun2, 99013) ' No. of intakes/outlets :',nloads
!
        inocns = ipos0 + ncload*nloads
        if (inocns  >  nocons) then
          write(lun2, 99001) subnam, inocns, nocons
          call stop_exit(1)
        endif
!
        do 10 il = 1, nloads
           ipos = ipos0 + (il-1)*ncload
           tmpexc(il) = const(ipos + 1)
           qflow(il)  = const(ipos + 2)
           xnloc      = const(ipos + 3)
           ynloc      = const(ipos + 4)
!
!.. 29 - 3- '99
!.. extra constant in version 3.60: the full 3d temperature model
!.. z of intake need not be specified: uniform extraction of heat from the layer
!                                      is assumed
!
           layloc     = int(const(ipos + 5))
           r10        = const(ipos + 6)
           r20        = const(ipos + 7)
           r30        = const(ipos + 8)
           r40        = const(ipos + 9)
           r50        = const(ipos +10)
!
           if(layloc <1.or.layloc > layt) then
              write(lun2,*) ' Error: the intake layer is out of range'
              write(*   ,*) ' Error: the intake layer is out of range'
           endif
!
!       calculate n,m value of the intake
!
           call  part07 ( lgrid  , lgrid2 , nmax   , mmax   , xb     ,   &
                          yb     , xnloc  , ynloc  , nmloc  , mmloc  ,   &
                          xmloc  , ymloc  , ierror )
           intake(il) = lgrid(nmloc, mmloc)
           if (intake(il)  < 1) then
              write(lun2, *) ' Warning: intake no. ',il,' is inactive !'
           endif
           write(lun2, 99013) ' Outlet/intake no.   :',intake(il)
           write(lun2, 99011) ' Excess temp. outlet :',tmpexc(il),' c'
           write(lun2, 99011) ' Flow of intake(outl):',qflow(il),'m3/s'
           write(lun2, 99011) ' Intake x-coordinate.:',xnloc
           write(lun2, 99011) ' Intake y-coordinate.:',ynloc
           write(lun2, 99012) ' Intake (n,m)        :',nmloc , mmloc
           write(lun2, 99011) ' Intake layer number :',layloc
           write(lun2, 99011) ' Radius at velocity < 10 cm/s:',r10 ,' m'
           write(lun2, 99011) ' Radius at 10cm/s< v <20 cm/s:',r20 ,' m'
           write(lun2, 99011) ' Radius at 20cm/s< v <30 cm/s:',r30 ,' m'
           write(lun2, 99011) ' Radius at 30cm/s< v <40 cm/s:',r40 ,' m'
           write(lun2, 99011) ' Radius at velocity > 40 cm/s:',r50 ,' m'
!
!       calculation of t0 for buoyancy model of quist (only used for b < 0)
!       const(13) = t0
!       assumptions:
!       -density difference calculated for temperature only, salinity of
!        ambient water is assumed
!       -internal froude number assumed to be 1.0
!       -q-outlet = q-intake (const(10)
!       -graph = 9.81
!       -for the chararcteristic length delta-x we substitute dxby
!       -input cont.rel are in order of loads specified in list of const.
!       -acf(il) and drho are time-independent
!
           rhowc = densty (salamb, tmpamb)
           rhol  = densty (salamb, tmpamb + tmpexc(il))
           drho  = abs(rhol - rhowc)
           a0    = graph * qflow(il) * drho / twopi / rhowc
!
!          initialize array for the a-coefficient now (only once)
!
           if (a0 == 0.0 .or. dxby  <=  0.0) then
             acf(il) = 50.0
           else
             a0  = a0 ** rodivt
             acf(il) = ((a0 / fdivt) ** rodivf) * dxby
           endif
           aby = acf(il)
!
!          echo values
!
        write(lun2, 99011) ' Calculated density:     ',rhowc ,' kg/m^3'
        write(lun2, 99011) ' Density diff. at outlet:',drho  ,' kg/m^3'
        write(lun2, 99011) ' A for buoyancy(b=-0.25):',aby,' m2/s^1.25'
!
           write(lun2, *) '   '
!
10      continue
!
!
!       calculation of the initial diluted excess temperature
!       volumetric heat capacity (density is not calculated as
!       function of temperature and salinity for the simple reason
!       of computational efficiency)
!
!
        tex    = tmpeff / dfac
        hcapac = cp * rhow
        itime = 1
        ipos = ipos0 - ncload
!
        do 40 il = 1,nloads
           il2    = nodye + il
           ipos   = ipos   + ncload
           iseg   = lgrid(nwaste(il2),mwaste(il2))
           velosi = velo(iseg)
           if ( velosi  <=  0.10 ) then
              radius(il2) = const(ipos+5)
           else if ( velosi  <=  0.20 ) then
              radius(il2) = const(ipos+6)
           else if ( velosi  <=  0.30 ) then
              radius(il2) = const(ipos+7)
           else if ( velosi  <=  0.40 ) then
              radius(il2) = const(ipos+8)
           else
              radius(il2) = const(ipos+9)
           endif
!
!          initialize array for the t0-coefficient now (each time-step)
!
           r0    = radius (il2)
!..recalculation of a0 (to prevent storage)
           rhol  = densty (salamb, tmpamb + tmpexc(il))
           drho  = abs(rhol - rhowc)
           a0    = graph * qflow(il) * drho / twopi / rhowc
           if(r0 <= (0.0).or.dxby <= (0.0)) then
              t0  = 3600.0
           else
              t0  = (r0 ** fdivt) / (fdivt * a0)
           endif
           t0cf(il) = t0
!
           write(lun2, 99014) ' Outlet no. :',il,' segment : ',iseg
           write(lun2, 99012) ' Outlet (n,m)   :',  &
              nwaste(il2),mwaste(il2)
           write(lun2, 99011) ' Velocity at outlet     :',velosi,' m/s '
           write(lun2, 99011) ' Radius   at outlet     :',r0    ,' m   '
           write(lun2, 99011) ' T0 for buoyancy        :',t0    ,' sec '
   40   continue
!
        goto 9999
!
      endif
!
!.. determine the radii and the buoyancy coefficient for this time-step
!.. using a block function (only for the continuous releases)
!
      ipos = ipos0 - ncload
      do 50 il = 1,nloads
         il2    = nodye + il
         ipos   = ipos   + ncload
         iseg   = lgrid(nwaste(il2),mwaste(il2))
         velosi = velo(iseg)
         if ( velosi  <=  0.10 ) then
            radius(il2) = const(ipos+5)
         else if ( velosi  <=  0.20 ) then
            radius(il2) = const(ipos+6)
         else if ( velosi  <=  0.30 ) then
            radius(il2) = const(ipos+7)
         else if ( velosi  <=  0.40 ) then
            radius(il2) = const(ipos+8)
         else
            radius(il2) = const(ipos+9)
         endif
!
!          initialize array for the t0-coefficient now (each time-step)
!
         r0    = radius (il2)
!
!..recalculation of a0 (to prevent storage)
!
         rhol  = densty (salamb, tmpamb + tmpexc(il))
         drho  = abs(rhol - rhowc)
         a0    = graph * qflow(il) * drho / twopi / rhowc
         if(r0 <= (0.0).or.dxby <= (0.0)) then
            t0  = 3600.0
         else
            t0  = (r0 ** fdivt) / (fdivt * a0)
         endif
         t0cf(il) = t0
!
         write(lun2, 99014) ' Outlet no. :',il,' segment : ',iseg
         write(lun2, 99012) ' Outlet (n,m)   :',nwaste(il2),mwaste(il2)
         write(lun2, 99011) ' Velocity at outlet     :',velosi,' m/s '
         write(lun2, 99011) ' Radius   at outlet     :',r0    ,' m   '
         write(lun2, 99011) ' T0 for buoyancy        :',t0    ,' sec '
   50 continue
!
!     loop over active particles
!
      icount = 0
      sumdc  = 0.0
      sumwp  = 0.0
      itime  = itime + 1
!
      do 100, i = npwndw, nopart
!
!       only calculation of heat exchange in the upper layer
!
        if (kpart(i) == 1) then

          ic = lgrid(npart(i), mpart(i))
!
!         active cell's only
!
          if (ic  >  0) then
!
!           for zero conc no division with conc
!
!           note..top layer iseg = ic
!
            if (conc(icvdf, ic)  >  small) then
!
!             calculation of the total water temperature in each grid
!             cell the conc array is expressed in excess temperature.
!
              ttemp  = conc(icvdf, ic) + tmpamb
!
!             check if the excess temperature is below boundary of
!             dilution
!
              ttemp  = min(ttemp, tex + tmpamb)
!
!             heat exchange coefficient (w/m^2/c)
!
              fwind  = 0.75 * (p7 + p6 * wvelo(ic) )
              wexch  =  p5 + p4 * ttemp + fwind *  &
                       (p3 + p2 * ttemp + p1 * ttemp**2)
!
!             heat transport over surface (j/s)
!
              hatm   =  wexch * area(ic) * (ttemp - tmpamb)
!
!             calculation of the decay coefficient (1/s)
!
              dectmp  = hatm /  &
                       (conc(icvdf, ic) * volume(ic) * hcapac * ptlay)
              dfact  = exp(-dectmp * idelt)
              icount = icount  + 1
              sumdc  = sumdc   + dectmp
!
!             calculation of the decay coefficient (-)
!
              wpart(1, i) = wpart(1, i) * dfact
              sumwp       = sumwp       + wpart(1, i)
!
            endif
          endif
        endif
  100 continue
!
!       similar loop for intake points
!
      noptk  = 0
      tmastk = 0
      do 330 il = 1,nloads
        iposv = intake(il) + mnmax2*(layt-1)
        if(volume(iposv) > (1.0)) then
           flrate     = qflow(il)  * idelt  / volume(iposv)
           reduc      = exp( -flrate )
        else
           reduc = 1.0
           write(lun2,*) ' Warning: volume intake segment < 1.0 m3 '
           write(lun2,*) ' Reduction factor set to 1.0: no heat intake'
        endif
        cintk(il)  = 0.0
        write( lun2 , '(a,2es15.7)' )  &
        'Volume of intake, reduction factor:', volume(iposv), reduc
        do 320 i = npwndw, nopart
          ic = lgrid(npart(i), mpart(i))
          if ( ic     == intake(il)   ) then
            noptk  = noptk  + 1
            tmastk = tmastk + wpart(1,i)
            cintk(il) = cintk(il)  + wpart(1,i) * ( 1.0 - reduc  )
!
!.. intake affects all substances: phyiscal elimination of matter !!
!
            do 310 k = 1,nosubs
               wpart(k,i)  = wpart(k,i)  * reduc
  310       continue
          endif
  320   continue
  330 continue
!
!     some extra print statements : useful information sent to output
!
      if (icount  >  0) then
        sumdc = sumdc / icount
        sumwp = sumwp / icount
        write(lun2, 99023) itime , icount, nopart
        write(lun2, 99025) itime , sumdc
        write(lun2, 99027) itime , sumwp
      else
        write(lun2, 99021) nopart
      endif
!
      ctot   = 0.0
      do 510 j = 1,nloads
         ctot   = ctot   + cintk(j)
  510 continue
      write(lun2, 99031) itime , noptk  , ctot   , tmastk
!
!     end of subroutine
!
 9999 if ( timon ) call timstop ( ithndl )
      return
!
!     formats
!
99001 format (' Error in ', a,  &
              ', : number of constants requested :', i4, &
              ' number available :', i4, '.')
99002 format (' Error in ', a, &
              ', : number of space in constant-array requested :', i4, &
              ' array space available :', i4, '.')
99011 format(a,es15.7,a)
99012 format(a,'(',i4,';',i4,')')
99013 format(a,i6)
99014 format(a,i6,a,i6)
99021 format(/,' Zero particles in upper layer : no heat loss ',/    &
              ,' total no. of particles = ',i6)
99023 format(/,' Step = ',i5,' no. particles in upper layer = ',i6,/ &
              ,' total no. of particles = ',i6)
99025 format(/,' Step = ',i5,' average decay   = ',e17.10,' (1/s)')
99027 format(/,' Step = ',i5,' average weight  = ',e17.10)
99031 format(/,' Time ',i10,' total mass taken in:',i10,2es15.7)
!
      end subroutine
end module
