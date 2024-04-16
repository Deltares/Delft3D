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

module larvae_mod

!
!  data definition module(s)
!
use m_stop_exit
use m_waq_precision               ! single/double precision
use timers
use m_t_forcing
!
!  module procedure(s)
!
use larvm2_mod              ! explicit interface
!
implicit none

    contains
      subroutine larvae ( lunrep   , itime    , idelt    , nmax     , mmax     ,    &
                          layt     , nosegl   , nolay    , mnmaxk   , lgrid    ,    &
                          lgrid2   , lgrid3   , nopart   , npwndw   , nosubs   ,    &
                          npart    , mpart    , kpart    , xpart    , ypart    ,    &
                          zpart    , wpart    , iptime   , wsettl   , locdep   ,    &
                          nocons   , const    , conc     , xa       , ya       )

      ! function  : calculates larval development and behaviour

      ! arguments :

      integer(int_wp ), intent(in)    :: lunrep              ! report file
      integer(int_wp ), intent(in)    :: itime               ! time in seconds
      integer(int_wp ), intent(in)    :: idelt               ! time step size in seconds
      integer(int_wp ), intent(in)    :: nmax                ! first grid dimension
      integer(int_wp ), intent(in)    :: mmax                ! second grid dimension
      integer(int_wp ), intent(in)    :: layt                ! number of layers of hydr. database
      integer(int_wp ), intent(in)    :: nosegl              ! number segments per layer
      integer(int_wp ), intent(in)    :: nolay               ! number of layers in calculation
      integer(int_wp ), intent(in)    :: mnmaxk              ! total number of active grid cells
      integer(int_wp ), pointer       :: lgrid ( : , : )     ! grid with active grid numbers, negatives for open boundaries
      integer(int_wp ), pointer       :: lgrid2( : , : )     ! total grid
      integer(int_wp ), pointer       :: lgrid3( : , : )     ! original grid (conc array)
      integer(int_wp ), intent(in)    :: nopart              ! total number of particles
      integer(int_wp ), intent(in)    :: npwndw              ! first active particle
      integer(int_wp ), intent(in)    :: nosubs              ! number of substances per particle
      integer(int_wp ), pointer       :: npart ( : )         ! first  grid index of the particles
      integer(int_wp ), pointer       :: mpart ( : )         ! second grid index of the particles
      integer(int_wp ), pointer       :: kpart ( : )         ! third grid index of the particles
      real   (sp), pointer       :: xpart ( : )         ! x-value (0.0-1.0) first  direction within grid cell
      real   (sp), pointer       :: ypart ( : )         ! y-value (0.0-1.0) second direction within grid cell
      real   (sp), pointer       :: zpart ( : )         ! z-value (0.0-1.0) third  direction within grid cell
      real   (sp), pointer       :: wpart ( : , :)      ! weight factors of the subs per particle
      integer(int_wp ), pointer       :: iptime( : )         ! particle age in seconds
      real   (sp), pointer       :: wsettl( : )         ! settling per particle
      real   (sp), pointer       :: locdep( : , : )     ! depth per layer
      integer(int_wp ), intent(in)    :: nocons              ! number of constants
      real   (sp), pointer       :: const ( : )         ! user-defined constants
      real   (sp), pointer       :: conc  ( : , : )     ! concentration array in transport grid
      real   (sp), pointer       :: xa    ( : )         ! x-coordiante in real world
      real   (sp), pointer       :: ya    ( : )         ! y-coordinate in real world

      ! from input (const)

      integer, save              :: nstage              ! number of stages
      integer, save              :: iday = 0            ! for csv output
      integer, save              :: ncum = 0            ! for csv output
      character(len=256)              :: filcsv
      integer, save              :: luncsv
      real(sp), allocatable,save :: astage(:)           ! a coefficient in stage development (-)
      real(sp), allocatable,save :: bstage(:)           ! b coefficient in stage development (-)
      integer , allocatable,save :: btype(:)            ! behaviour type
      real(sp), allocatable,save :: mort1(:)            ! base mortality begin stage
      real(sp), allocatable,save :: mort2(:)            ! base mortality end stage
      real(sp), allocatable,save :: tcmort(:)           ! temperature coefficient mortality
      real(sp), allocatable,save :: buoy1(:)            ! buoyancy begin stage
      real(sp), allocatable,save :: buoy2(:)            ! buoyancy end stage
      real(sp), allocatable,save :: vzact1(:)           ! active vertical velocity begin stage
      real(sp), allocatable,save :: vzact2(:)           ! active vertical velocity end stage
      real(sp), allocatable,save :: ztop1(:)            ! depth top layer begin stage
      real(sp), allocatable,save :: ztop2(:)            ! depth top layer end stage
      real(sp), allocatable,save :: zbot1(:)            ! depth bot layer begin stage
      real(sp), allocatable,save :: zbot2(:)            ! depth bot layer end stage
      real(sp), allocatable,save :: phase(:)            ! phase in diurnal behaviour
      integer, save              :: istage_nursery      ! stage from where larvae can settle in nursery area
      integer, save              :: layer_release       ! layer where eggs are released
      integer, save              :: it_start_m2         ! start time m2 output
      integer, save              :: it_stop_m2          ! stop time m2 output
      integer, save              :: idt_m2              ! timestep m2 output
      integer, save              :: idt_csv             ! timestep csv output, 0 = not, times as m2
      integer, save              :: iniday              ! release of initial condition, not used here

      ! local :

      real(sp), allocatable,save :: phase_diurn(:)      ! phase in diurnal behaviour
      integer(int_wp )                :: ipart               ! particle index
      real   (sp)                :: stage               ! stage development
      real   (sp)                :: fstage              ! fraction of current stage
      integer(int_wp )                :: istage              ! integer stage development
      integer(int_wp )                :: istage_t            ! index stage development
      integer(int_wp )                :: istag0              ! previous stage
      real   (sp)                :: stime               ! time since last stage change
      real   (sp)                :: stemp               ! average temperature since last stage change
      real   (sp)                :: dtemp               ! average temp day
      real   (sp)                :: ddepth              ! average depth day
      real   (sp)                :: sdepth              ! average depth since last stage change
      real   (sp)                :: a                   ! a coefficient in development (-)
      real   (sp)                :: b                   ! b coefficient in development (-)
      real   (sp)                :: temp                ! temperature
      real   (sp)                :: delt                ! timestep in days
      real   (sp)                :: day                 ! time in days
      real   (sp)                :: duration            ! duration of current stage
      logical, save              :: is_nursery          ! if arived at nursery area
      real   (sp)                :: vz                  ! vz
      real   (sp)                :: vzact               ! vzact
      real   (sp)                :: buoy                ! buoy
      real   (sp)                :: mort                ! mortality
      real   (sp)                :: ztop                ! ztop
      real   (sp)                :: zbot                ! zbot
      integer                    :: m                   ! m
      integer                    :: n                   ! n
      integer                    :: k                   ! k
      integer                    :: iseg                ! iseg
      integer                    :: isegl               ! isegl
      integer                    :: isegb               ! isegb
      real   (sp)                :: z                   ! z
      real   (sp)                :: zdepth              ! z relative to water surface
      real   (sp)                :: zlevel              ! z relative to bottom
      real   (sp)                :: laydep              ! laydep
      real   (sp)                :: totdep              ! totdep
      logical, save              :: l_csv               ! csv output
      logical                    :: l_csv_now           ! csv output at this timestep

      integer                    :: behaviour_type      ! actual behaviour type

      integer, parameter         :: behaviour_none     = 0 ! behaviour type none
      integer, parameter         :: behaviour_bottom   = 1 ! behaviour type bottom
      integer, parameter         :: behaviour_midlow   = 2 ! behaviour type midlow
      integer, parameter         :: behaviour_midtop   = 3 ! behaviour type midtop
      integer, parameter         :: behaviour_pelagic  = 4 ! behaviour type pelagic
      integer, parameter         :: behaviour_demersal = 5 ! behaviour type demersal
      integer, parameter         :: behaviour_diurnal  = 6 ! behaviour type diurnal
      integer, parameter         :: behaviour_herring  = 7 ! behaviour type herring
      integer, parameter         :: behaviour_stst_dem = 8 ! behaviour type stst_dem
      integer, parameter         :: behaviour_stst_pel = 9 ! behaviour type stst_pel
      real   , parameter         :: pi = 3.141592654
      real   , parameter         :: twopi = pi*2.0
      integer, parameter         :: nfix               = 8 ! fixed number of constants
      integer, parameter         :: nvar               =15 ! variable number of constants per stage

      integer, save              :: ifirst = 1
      logical, save              :: l_larvae
      integer(4),save            :: ithndl = 0             ! handle to time this subroutine

      if ( ifirst .eq. 1 ) then
         ifirst = 0
         if ( nocons .eq. 0 ) then
            write(lunrep,*) ' no constants, no larvae model activated'
            l_larvae = .false.
            return
         endif
         nstage         = nint(const(1))
         if ( nstage .le. 0 ) then
            write(lunrep,*) ' no stages, no larvae model activated'
            l_larvae = .false.
            return
         endif
         if ( nocons .ne. nfix+nstage*nvar ) then
            write(lunrep,*) ' no of constants inconsistent with stages, no larvae model activated'
            l_larvae = .false.
            return
         endif
         l_larvae       = .true.
         istage_nursery = nint(const(2))
         layer_release  = nint(const(3))
         it_start_m2    = nint(const(4)*86400.)
         it_stop_m2     = nint(const(5)*86400.)
         idt_m2         = nint(const(6))
         idt_csv        = nint(const(7))
         iniday         = nint(const(8))
         if ( idt_csv .le. 0 ) then
            l_csv = .false.
         else
            l_csv = .true.
         endif

         allocate(astage(nstage))
         allocate(bstage(nstage))
         allocate(btype(nstage))
         allocate(mort1(nstage))
         allocate(mort2(nstage))
         allocate(tcmort(nstage))
         allocate(buoy1(nstage))
         allocate(buoy2(nstage))
         allocate(vzact1(nstage))
         allocate(vzact2(nstage))
         allocate(ztop1(nstage))
         allocate(ztop2(nstage))
         allocate(zbot1(nstage))
         allocate(zbot2(nstage))
         allocate(phase(nstage))
         allocate(phase_diurn(nstage))

         do istage = 1, nstage
            astage(istage) = const(nfix+(istage-1)*nvar+ 1)
            bstage(istage) = const(nfix+(istage-1)*nvar+ 2)
            btype(istage)  = const(nfix+(istage-1)*nvar+ 3)
            mort1(istage)  = const(nfix+(istage-1)*nvar+ 4)
            mort2(istage)  = const(nfix+(istage-1)*nvar+ 5)
            tcmort(istage) = const(nfix+(istage-1)*nvar+ 6)
            buoy1(istage)  =-const(nfix+(istage-1)*nvar+ 7)/86400.
            buoy2(istage)  =-const(nfix+(istage-1)*nvar+ 8)/86400.
            vzact1(istage) =-const(nfix+(istage-1)*nvar+ 9)/86400.
            vzact2(istage) =-const(nfix+(istage-1)*nvar+10)/86400.
            ztop1(istage)  = const(nfix+(istage-1)*nvar+11)
            ztop2(istage)  = const(nfix+(istage-1)*nvar+12)
            zbot1(istage)  = const(nfix+(istage-1)*nvar+13)
            zbot2(istage)  = const(nfix+(istage-1)*nvar+14)
            phase(istage)  = const(nfix+(istage-1)*nvar+15)
         enddo

         ! set some stuff hard coded for the moment

         is_nursery     = .true.

      endif

      ! exit if no larvae model

      if ( .not. l_larvae ) return
      if ( timon ) call timstrt( "larvae", ithndl )

      ! global forcing

      delt     = idelt/86400.
      day      = itime/86400.
      do istage = 1, nstage
         phase_diurn(istage) = sin((day-phase(istage))*twopi)/2.+.5
      enddo

      ! output larvae per m2

      if ( itime .ge. it_start_m2 .and. itime .le. it_stop_m2 .and. mod(itime-it_start_m2,idt_m2) .lt. idelt ) then
         call larvm2 ( lunrep   , itime    , nosegl   , nolay    , nosubs   ,    &
                       conc     )
      endif

      ! every day output position to csv file

      l_csv_now = .false.
      if ( l_csv ) then
         if ( itime .ge. it_start_m2 .and. itime .le. it_stop_m2 .and. mod(itime-it_start_m2,idt_csv) .lt. idelt ) then
            l_csv_now = .true.
            iday = itime/idt_csv
            write(filcsv,'(a,i5.5,a)') 'position_day_',iday,'.csv'
            open(newunit=luncsv,file=filcsv)
         endif
         ncum = ncum + 1
      endif

      do ipart = npwndw, nopart

         ! position and forcing

         m      = mpart(ipart)
         n      = npart(ipart)
         k      = min(kpart(ipart),nolay)
         z      = zpart(ipart)
         iseg   = lgrid3(n,m)
         if (iseg .gt. 0 ) then
         isegl  = iseg + (k-1)*nosegl
         isegb  = iseg + (nolay-1)*nosegl

         laydep = locdep(lgrid2(n,m),k)
         if( k .ne. 1 ) laydep = laydep - locdep(lgrid2(n,m),k-1)
         totdep = locdep(lgrid2(n,m),nolay)
         zlevel = locdep(lgrid2(n,m),nolay) - locdep(lgrid2(n,m),k) + (1.-z)*laydep
         zdepth = locdep(lgrid2(n,m),k) - (1.-z)*laydep

         temp     = t_forcing(itime,isegl)

         ! stage development

         stage  = wpart(2,ipart)
         stime  = wpart(3,ipart)
         stemp  = wpart(4,ipart)
         if ( nosubs .ge. 4+nstage+1 ) dtemp  = wpart(4+nstage+1,ipart)
         if ( nosubs .ge. 4+nstage+2 ) ddepth = wpart(4+nstage+2,ipart)
         if ( nosubs .ge. 4+nstage+3 ) sdepth = wpart(4+nstage+3,ipart)
         istage = stage
         istage = max(istage,0)
         istag0 = istage

         stemp  = (stemp*stime + temp*delt)/(stime+delt)
         if ( nosubs .ge. 4+nstage+1 ) dtemp  = dtemp + temp
         if ( nosubs .ge. 4+nstage+2 ) ddepth = ddepth + zdepth
         if ( nosubs .ge. 4+nstage+3 ) sdepth = (sdepth*stime + zdepth*delt)/(stime+delt)
         stime  = stime+delt

         if ( istage .eq. 0 ) then
            if ( stime .gt. -(stage+delt) ) then
               stage  = 1.0
               istage = 1
               fstage = 0.0
               stime  = 0.0
               kpart(ipart) = layer_release
               zpart(ipart) = 0.5
            endif
         else
            a = astage(istage)
            b = bstage(istage)
            duration = exp(a+b*stemp)
            if ( duration .lt. stime ) then
               istage = istage + 1
               istage = min(istage,5)
               stime  = 0.0
            endif
            fstage = stime/duration
            stage  = istage + fstage
         endif
         wpart(2,ipart) = stage
         wpart(3,ipart) = stime
         wpart(4,ipart) = stemp

         ! mortality

         if ( istage .gt. 0 ) then
            mort = mort1(istage) + fstage*(mort2(istage)-mort1(istage))
            if ( abs(tcmort(istage)) .gt. 1.e-20 ) then
               mort = mort*exp(tcmort(istage)*temp)
            endif
            wpart(1,ipart) = wpart(1,ipart) - mort*wpart(1,ipart)*delt
         endif

         ! if there is substances per fraction set weight according to stage

         do istage_t = 1, nstage
            if ( istage_t+4 .le. nosubs ) then
               if ( istage_t .eq. istage ) then
                  wpart(istage_t+4,ipart) = wpart(1,ipart)
               else
                  wpart(istage_t+4,ipart) = 0.0
               endif
            endif
         enddo

         ! deactivate particles in nursery stage who have arrived in nursery area

         if ( istage .ge. istage_nursery .and. is_nursery ) then
            kpart(ipart) = nolay +1
         elseif ( istage .gt. 0 ) then

            ! set layer and/or settling velocity according to stage and type of behaviour

            behaviour_type = btype(istage)
            select case ( behaviour_type )
               case ( behaviour_none )
                  wsettl(ipart) = 0.0
               case ( behaviour_bottom )
                  wsettl(ipart) = 0.0
                  kpart(ipart) = nolay + 1
                  zpart(ipart) = 0.5
               case ( behaviour_midlow )
                  wsettl(ipart) = 0.0
                  kpart(ipart) = nolay
                  zpart(ipart) = 0.5
               case ( behaviour_midtop )
                  wsettl(ipart) = 0.0
                  kpart(ipart) = 1
                  zpart(ipart) = 0.5
               case ( behaviour_pelagic )
                  wsettl(ipart) = buoy1(istage) + fstage*(buoy2(istage)-buoy1(istage))
               case ( behaviour_demersal )
                  ztop  = ztop1(istage)  + fstage*(ztop2(istage)-ztop1(istage))
                  zbot  = zbot1(istage)  + fstage*(zbot2(istage)-zbot1(istage))
                  buoy  = buoy1(istage)  + fstage*(buoy2(istage)-buoy1(istage))
                  vzact = vzact1(istage) + fstage*(vzact2(istage)-vzact1(istage))
                  if ( zlevel .gt. ztop ) then
                     vz = buoy
                  elseif ( zlevel .lt. zbot ) then
                     vz = vzact
                  else
                     vz = buoy + phase_diurn(istage)*(vzact-buoy)
                  endif
                  wsettl(ipart) = vz
               case ( behaviour_diurnal )
                  ztop  = ztop1(istage)  + fstage*(ztop2(istage)-ztop1(istage))
                  zbot  = zbot1(istage)  + fstage*(zbot2(istage)-zbot1(istage))
                  buoy  = buoy1(istage)  + fstage*(buoy2(istage)-buoy1(istage))
                  vzact = vzact1(istage) + fstage*(vzact2(istage)-vzact1(istage))
                  if ( zdepth .lt. ztop ) then
                     vz = buoy
                  elseif ( zlevel .lt. zbot ) then
                     vz = vzact
                  else
                     vz = buoy + phase_diurn(istage)*(vzact-buoy)
                  endif
                  wsettl(ipart) = vz
               case ( behaviour_herring )
                  wsettl(ipart) = 0.0
               case ( behaviour_stst_dem )
                  wsettl(ipart) = 0.0
               case ( behaviour_stst_pel )
                  wsettl(ipart) = 0.0
               case default
                  write(lunrep,*) ' error, larval behaviour type not defined'
                  call stop_exit(1)
            end select

         endif

         ! every day output position to csv file

         if ( l_csv_now ) then
            ddepth = ddepth/ncum
            dtemp  = dtemp/ncum
            write(luncsv,'(f10.2,'','',f10.2,5('','',f10.4))') xa(ipart),ya(ipart),zdepth,sdepth,ddepth,stemp,dtemp
            ddepth = 0.0
            dtemp  = 0.0
         endif
         if ( nosubs .ge. 4+nstage+1 ) wpart(4+nstage+1,ipart) = dtemp
         if ( nosubs .ge. 4+nstage+2 ) wpart(4+nstage+2,ipart) = ddepth
         if ( nosubs .ge. 4+nstage+3 ) wpart(4+nstage+3,ipart) = sdepth

         else
            ! inactive

         if ( l_csv_now ) then
            zdepth = -999.
            sdepth = -999.
            ddepth = -999.
            stemp  = -999.
            dtemp  = -999.
            write(luncsv,'(f10.2,'','',f10.2,5('','',f10.4))') -999.,-999.,zdepth,sdepth,ddepth,stemp,dtemp
         endif

         endif

         !

      enddo


      ! every day output position to csv file

      if ( l_csv_now ) then
         close(luncsv)
         ncum = 0
      endif

      continue

      if ( timon ) call timstop ( ithndl )
      return
      end subroutine
end module
