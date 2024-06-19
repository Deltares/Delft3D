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

module parths_mod
!
!  module declarations
!
!
!  data definition module(s)
!
use m_stop_exit
use m_part11
use m_waq_precision            ! single and double precision
use m_part_modeltypes         ! part model definitions
use timers
use openfl_mod
!
!  module procedure(s)
!
use dlpr12_mod           ! explicit interface
use grid_search_mod      ! explicit interface
use typos
!
implicit none            ! force explicit typing
!
contains
      subroutine parths(lun1     , lun2     , title    , subst    , mmax     ,  &
                        lgrid2   , nmax     , volume   , area     , npart    ,  &
                        mpart    , xpart    , ypart    , wpart    , nopart   ,  &
                        itime    , idelt    , xa       , npwndw   , lgrid    ,  &
                        ya       , xb       , yb       , pg       , pblay    ,  &
                        modtyp   , nolay    , nosubs   , conc     , chismp   ,  &
                        chispl   , nosta    , nmstat   , xstat    , ystat    ,  &
                        nstat    , mstat    , nplsta   , mplsta   , ihstrt   ,  &
                        ihstop   , ihstep   , ihplot   , finam    , kpart    ,  &
                        mnmax2   , noseglp  , nfract   , use_settling   , mstick   ,  &
                        elt_names, elt_types, elt_dims , elt_bytes, rbuffr   ,  &
                        zpart    , za       , locdep   , dps      , tcktot   ,  &
                        lgrid3   )
!
!     WRITING HISTORY FILE (*.his)
!            (per time step)
!
!
!     note                  : for 3d all layers are plotted for one (x,y) observation point
!
!
!     logical unit numbers  : lun1 - history file
!                             lun2 - output log file
!
!
!     subroutines called    : part11 - converts model coordinates to
!                                      national grid and plot coordinates,
!                             dlwq12 - writes history file (lun1+nefis)
!                             openfl - opens a binary/unformatted file
!
!
!     functions   called    : none.
!
!
!     parameters            :
!
!     name    kind     length     funct.  description
!     ====    ====     ======     ======  ===========
!     amap    real  nolay*nosubs* in/out  plot grid to be dumped
!                    nmap*mmap
!     area    real      mnmaxk    input   surface areas of the lgrid2 cells
!     angle   real        *       input   angles in the computational grid
!     chismp  real  noslx *nostmx in/out  time history for total grid
!     chispl  real  noslx *nostmx in/out  time history for plot grid
!     dx      real        *       input   grid-distances
!     dy      real        *       input   grid-distances
!     finam   char*256    1       input   name of the history file
!     idelt   integer     1       input   simulation time step
!     ihplot  integer   nosta     input   if 1 then from plot window
!     ihstrt  integer     1       input   start time history file
!     ihstep  integer     1       input   time-step history file
!     ihstop  integer     1       input   stop time history file
!     itime   integer     1       input   simulation time
!     kpart   integer   nopart    input   k-values of particles
!     lgrid   integer  nmax*mmax  input   active grid numbers
!     lgrid2  integer  nmax*mmax  input   model grid layout (total)
!     lun1    integer     1       input   unit number 1 (history)
!     lun2    integer     1       input   unit number 2 (logging)
!     use_settling  logical     1       input   if .true. then settling substances
!                                         in an extra bed layer
!     mapsub  integer   nosubs    input   map of substances on numbers
!     modtyp  integer     1       input   model type
!     mplsta  integer     1       in/out  sec   dir. plot history station
!     mstat   integer     1       in/out  sec   dir. map history station
!     nfract  integer     1       input   number of oil fractions
!     nplsta  integer     1       in/out  first dir. plot history station
!     nstat   integer     1       in/out  first dir. map history station
!     mmap    integer     1       input   dimension of amap
!     mpart   integer   nopart    input   m-values of particles
!     mstick  integer   nosubs    input   sticking material if mstick > 0
!     nmap    integer     1       input   dimension of amap
!     nmax    integer     1       input   dimension of lgrid2
!     nolay   integer     1       input   actual number of layers
!     nopart  integer     1       input   nr of particles
!     nosubc  integer     1       input   leading dimension conc-array
!     nosubs  integer     1       input   actual number of substances
!     npart   integer   nopart    input   n-values of particles
!     npwndw  integer     1       in/out  start of active nopart number
!     pblay   real        1       input   relative thickness lower layer
!     subst   char*20 nolays*nosubs input substance name and unit specs
!     surf    real        1       input   surface of a plot grid cell
!     title   char*40     4       input   model and run titles
!     window  real        4       input   plot grid window
!     wpart   real nosubs* nopart input   weights of the particles
!     xa      real      nopart    output  national coordinates of parts
!     xb      real      mnmaxk    input   x-values of bottom points
!     xpart   real      nopart    input   x-values of particles
!     ya      real      nopart    output  national coordinates of parts
!     yb      real      mnmaxk    input   y-values of bottom points
!     ypart   real      nopart    input   y-values of particles
!     zpart   real      nopart    input   vertical coord. particles
!     ----    ----     ------     ------  -----------
!     fvolum  real        1       local   help var. for volume
!     first   logical     1       local   switch
!     hisfil  logical     1       local   if true then history plot
!     i       integer     1       local   help variable
!     i1      integer     1       local   help variable 1
!     i2      integer     1       local   help variable 2
!     ilay    integer     1       local   layer index
!     ipos    integer     1       local   pointer in layers
!     isub    integer     1       local   substance index
!     ix      integer     1       local   grid pointer for x dir.
!     iy      integer     1       local   grid pointer for y dir.
!     noerr   integer     1       local   help var. for no. of errors
!     nosubt  integer     1       local   nosubs * nolay
!     thickn  real        2       local   ptlay and pblay
!     windw1  real        1       local   help var for window (speed)
!     windw3  real        1       local   help var for window (speed)
!     xpf     real        1       local   delta x/cell for amap
!     ypf     real        1       local   delta y/cell for amap
!     zparti  real        1       local   help var. for zpart (speed)
!     zstep   real        1       local   linear step/layer over depth
!
!     save values between invocations: specified precisely those needed!!
!
      save           first , lplgr , nosubt, windw1, windw3,  &
                     xpf   , ypf   , thickn
!
!     declarations
!
      type(PlotGrid)                   pg                     !< first plot grid information
      character(len=*), pointer, dimension(:) :: nmstat
      character(len=*), pointer, dimension(:) :: subst
      character( 40), intent(in   ) :: title (4)              !< model titles
      real     ( sp)                :: window(4)              !  plot window
      character(len=256)                      :: finam
      logical                                 :: ihflag, hisfil
      logical                                 :: use_settling
!
!     declare putget help var's
!
!     dimensioning
!
      character(len=16), pointer, dimension(:) ::  elt_names, elt_types
!
      integer(int_wp ), pointer, dimension(:)       :: ihplot
      integer(int_wp ), pointer, dimension(:)       :: nplsta, mplsta
      integer(int_wp ), pointer, dimension(:)       :: nstat , mstat
      integer(int_wp ), pointer, dimension(:,:)     :: elt_dims
      integer(int_wp ), pointer, dimension(:)       :: elt_bytes
      integer(int_wp ), pointer, dimension(:)       :: mstick
      integer(int_wp ), pointer, dimension(:)       :: npart , mpart , kpart
      integer(int_wp ), pointer, dimension(:,:)     :: lgrid , lgrid2, lgrid3
      real   (sp), pointer, dimension(:)       :: xa    , ya    , xb     , yb
      real   (sp), pointer, dimension(:)       :: xpart , ypart , volume , area
      real   (sp), pointer, dimension(:)       :: xstat , ystat
      real   (sp), pointer, dimension(:,:,:)   :: chismp
      real   (sp), pointer, dimension(:,:,:)   :: chispl
      real   (sp), pointer, dimension(:)       :: zpart , za
      real   (sp), pointer, dimension(:,:)     :: conc
      real   (sp), pointer, dimension(:,:)     :: wpart
      real   (sp), pointer, dimension(:,:)     :: locdep
      real   (sp), pointer, dimension(:)       :: tcktot
      real   (sp), pointer, dimension(:)       :: dps
      real   (sp), pointer, dimension(:)       :: rbuffr
      real   (sp), dimension(2)       :: thickn

      real   (sp), pointer, dimension(:,:,:,:) :: amap
!
      logical :: first = .true.
      logical :: lplgr = .false.
!
!     local scalars
!
      integer(int_wp ) :: i1        ,i2        ,idelt     ,ierror    ,ihstep    ,ihstop
      integer(int_wp ) :: ipos      ,iseg      ,ist2      ,istat     ,isub
      integer(int_wp ) :: jsub      ,lun1      ,lun2      ,mmap      ,mmax      ,mmloc
      integer(int_wp ) :: mnmax2    ,noseglp   ,modtyp    ,nfract    ,ihstrt    ,ilay
      integer(int_wp ) :: nmap      ,nmax      ,nmloc     ,noerr     ,nolay     ,nopart
      integer(int_wp ) :: nosubs    ,nosubt    ,npwndw    ,itime     ,ix        ,iy      ,nosta
      real   (sp) :: depthl    ,fvolum    ,pblay     ,surf      ,windw1
      real   (sp) :: windw3    ,xmloc     ,xnloc     ,xpf
      real   (sp) :: ymloc     ,ynloc     ,ypf
      integer(kind=int_wp), save :: ithndl = 0            ! handle to time this subroutine

!     For the time being

      mmap      = pg%mmap
      nmap      = pg%nmap
      window(1) = pg%xlow
      window(2) = pg%xhigh
      window(3) = pg%ylow
      window(4) = pg%yhigh
      surf      = pg%surf
      amap     => pg%amap
!
!     determine if history file must be produced
!
      hisfil = .true.
      if ( nosta                      <=  0      .or. &
           ihstep                     < 1        .or. &
           itime                      < ihstrt   .or. &
           itime - idelt              >=  ihstop .or. &
           mod(itime-ihstrt, ihstep)  >=  idelt ) then
          hisfil = .false.
      endif
!
      if ( .not. hisfil ) then
          return
      endif

      if ( timon ) then
          call timstrt( "parths", ithndl )
      endif

!     start of creation of history file

      if (first) then
          first = .false.
          windw1    = window(1)
          windw3    = window(3)
          xpf       = (window(2) - windw1) / mmap
          ypf       = (window(4) - windw3) / nmap
          thickn(1) = 1.0 - pblay
          thickn(2) = pblay
          nosubt    = (nosubs + 1) * nolay
          write ( lun2, * ) ' Writing to new history file:', finam(1:len_trim(finam))
          call openfl ( lun1, finam, 1 )

!         map file stations:

          noerr = 0
          nstat = 0
          do istat = 1, nosta
              xnloc = xstat(istat)
              ynloc = ystat(istat)
              call part07 ( lgrid  , lgrid2 , nmax   , mmax   , xb     , &
                            yb     , xnloc  , ynloc  , nmloc  , mmloc  , &
                            xmloc  , ymloc  , ierror )

!             write some statistics and update output arrays

              if ( ierror == 0 ) then
                  nstat(istat) = nmloc
                  mstat(istat) = mmloc
              else                       !   location invalid
                  noerr = noerr + 1
                  nstat(istat)      = -1
                  mstat(istat)      = -1
                  chismp(:,:,istat) = -999.0
                  nplsta(istat)     = -1
                  mplsta(istat)     = -1
                  chispl(:,:,istat) = -999.0
              endif
          enddo
          if ( noerr /= 0 ) then
              write (lun2, 99005) noerr
          endif

!         plo file stations:

          do istat = 1, nosta
              if ( nstat(istat) .eq. -1 ) cycle
              xnloc = xstat(istat)
              ynloc = ystat(istat)
              ix = int((xnloc - windw1) / xpf) + 1
              iy = int((ynloc - windw3) / ypf) + 1
              if ( ix  >  0 .and. ix  <=  mmap .and.                 &
                   iy  >  0 .and. iy  <=  nmap        ) then
                  i2 = lgrid( nstat(istat), mstat(istat) )
                  if (i2  < 2) then        !    location invalid
                      write (lun2, 99004) istat, xnloc, ynloc
                      nplsta(istat)     = -1
                      mplsta(istat)     = -1
                      chispl(:,:,istat) = -999.0
                  else
                      nplsta(istat) = ix
                      mplsta(istat) = iy
                      lplgr = .true.
                      do ist2 = 1, istat - 1
                          if ( ix == nplsta(ist2) .and.                 &
                               iy == mplsta(ist2)        ) then
                              write(lun2, 99002)    ! two observation points in one cell
                          endif
                      enddo
                  endif
              else             !            initialize missing values
                  nplsta(istat)     = -1
                  mplsta(istat)     = -1
                  chispl(:,:,istat) = -999.0
              endif
          enddo
      endif
!
!     initialize chismp with concentration
!
      do istat = 1, nosta
          if ( nstat(istat) .eq. -1 ) then
              cycle
          endif
          i2   = lgrid3(nstat(istat), mstat(istat))
          do ilay = 1, nolay
              iseg = i2 + (ilay - 1)*noseglp
              do isub = 1, nosubs
                  if (modtyp /= model_two_layer_temp) then
                      chismp(isub , ilay , istat) = conc(isub  , iseg)
                  else
                      ipos = (isub-1)*nolay  + ilay
                      chismp(isub , ilay , istat) = conc(ipos  , i2  )
                  endif
              enddo
              chismp(nosubs + 1 , ilay , istat) = conc(nosubs + 2  , iseg)
          enddo
      enddo
!
      if ( lplgr ) then
!
!       compute particle coordinate
!
          call part11(lgrid , xb    , yb    , nmax  , npart , mpart , &
                      xpart , ypart , xa    , ya    , nopart, npwndw, &
                      lgrid2, kpart , zpart , za    , locdep, dps   , &
                      nolay , mmax  , tcktot)

!
!   zero plot grid for all substances   (which ones after part13 ????)
!
          amap =  0.0   ! whole array assignment
!
!       initialize some variables for within the loop
!
          do i1 = npwndw, nopart
!
!         determine if pointers fit in the grid
!
              ix = int((xa(i1) - windw1) / xpf) + 1
              if (ix  >  0 .and. ix  <=  mmap) then
                  iy = int((ya(i1) - windw3) / ypf) + 1
                  if (iy  >  0 .and. iy  <=  nmap) then
                      i2 = lgrid(npart(i1), mpart(i1))
                      if (i2  >  1) then
                          if (area(i2)  /=  0.0) then
!
!                             determine the appropriate layer
!
                              ilay = kpart(i1)
!
                              if(modtyp == model_two_layer_temp) then
                                  depthl = volume(i2)/area(i2)
                                  fvolum = surf * thickn(ilay) * depthl
                              else
                                  iseg   = (ilay-1)*mnmax2 + i2
                                  if ( use_settling .and. ilay == nolay ) then
                                      fvolum = surf
                                  else
                                      depthl = volume(iseg)/area(i2)
                                      fvolum = surf * depthl
                                  endif
                              endif

                              if (fvolum  /=  0.0) then
!
!                                 put concentration in it's appropriate layer
!
                                  do isub = 1, nosubs
!
!.. for floating oil or for deposited substances surf is required (per m2)
!.. also for sticking substances
!
                                      if (modtyp == model_oil .and. isub <(3*nfract)) then
!.. oil module
                                          jsub = mod(isub,3)
                                          if ((2*(jsub)/2) /= jsub) then
                                              fvolum = surf
                                          elseif ( mstick(isub) < 0 ) then
                                              fvolum = surf
                                          endif
                                      elseif ( use_settling .and. ilay == nolay ) then
                                          fvolum = surf
                                      elseif( mstick(isub) < 0 ) then
                                          fvolum = surf
                                      endif
                                      amap(isub , ilay , iy, ix) = amap(isub , ilay , iy, ix) +  &
                                                                   wpart(isub, i1)/fvolum
                                  enddo
! also count number of particles
                                  amap(nosubs + 1, ilay, iy, ix) = amap(nosubs + 1, ilay, iy, ix) + 1

                              endif
                          endif
                      endif
                  endif
              endif
          enddo
      endif
!
      do istat = 1, nosta
          if ( nplsta(istat)  < 1 .or. mplsta(istat) < 1  .or. .not. lplgr) then
!
!         initialize chispl(1, istat) with missing values
!
              chispl(:,:,istat) = -999.0  ! whole array assignment
!
!         this version: overwrite chispl with chismp if loc. outside
!                       plot window
!
              ihplot(istat) = 0
              do isub = 1, nosubs+1
                  do ilay = 1, nolay
                     chispl(isub, ilay, istat) = chismp(isub, ilay, istat)
                  enddo
              enddo
          else
              ihplot(istat) = 1
              do isub = 1, nosubs+1
                  do ilay = 1, nolay
                      chispl(isub, ilay ,istat) = amap(isub, ilay, mplsta(istat), nplsta(istat))

                  enddo
              enddo
          endif
      enddo
!
!     write history file, use delwaq facilities for this
!
      call dlpr12 ( lun1   , lun2      , ihplot    , chispl    , itime     , &
                    idelt  , ihstrt    , ihstop    , ihstep    , nmstat    , &
                    subst  , title     , nosta     , nosubs+1  , nolay     , &
                    ihflag , elt_names , elt_types , elt_dims  , elt_bytes , &
                    rbuffr)
!
      if ( .not. ihflag ) then
!
!       time-step not written ??  : error
!
          write (lun2, 99006)
          call stop_exit(1)
      endif
!
!     end of subroutine
!
      if ( timon ) then
          call timstop ( ithndl )
      endif
      return
!
!     error formats
!
99002 format('  Warning : Two observation points fall within the', /,     &
             '              resolution of one cell of the zoom window.')
99004 format('  Warning : Observation point', i3, ' at (x,y): (',         &
                            f9.2,',',f9.2, ') not on active grid cell.')
99005 format('  Total number of observation point warnings = ', i3 )
99006 format('  Error: History time-step is not written to file!')
99010 format('  Error: Area = 0 for active segment ',i6)
!
      end subroutine
end module
