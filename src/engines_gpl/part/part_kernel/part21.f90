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

module part21_mod
!
!  module declarations
!
!  data definition module(s)
!
use m_stop_exit
use m_part11
use m_waq_precision           ! single/double precision
      use timers
!
!  module procedure(s)
!
use grid_search_mod     ! explicit interface
      use typos
!
implicit none           ! force explicit typing
!
contains
      subroutine part21 ( lun2   , lgrid  , lgrid2 , xb     , yb     , &
                          area   , volume , nmax   , mmax   , nolay  , &
                          nosubs , nopart , npart  , mpart  , kpart  , &
                          xpart  , ypart  , zpart  , wpart  , npwndw , &
                          pg     , amap   , xa     , ya     , za     , &
                          atotal , apeak  , adepth , imap   , nplay  , &
                          wsettl , irfac  , anfac  , lsettl , locdep , &
                          tcktot , dps   )
!
!
!     CALCULATES (CONC. DEPENDENT) SETTING VELOCITIES
!                     (per time step)
!
!     created       : december 2000, by l. postma
!
!     modified      : june 2001 : by antoon koster
!                     settling out (set w=0 in bottom layer) only
!                     if sed-erosion process active
!
!     note          : just the grid is produced, no file is written
!                     no checking on timings is conducted
!
!     logical unit numbers  : lun2 - output log file
!
!     subroutines called    : part11 - converts model coordinates to
!                                      national grid and plot coordinates,
!
!
!     parameters     :
!
!     name    kind length        funct.  description
!     ====    ==== ======        ======  ===========
!     lun2    integer    1        input   unit number 2 (logging)
!     lgrid   integer nmax*mmax   input   active grid numbers
!     lgrid2  integer nmax*mmax   input   model grid layout (total)
!     xb      real  mnmaxk        input   x-values of bottom points
!     yb      real  mnmaxk        input   y-values of bottom points
!     area    real  mnmaxk        input   surface areas of the lgrid2 cells
!     volume  real  mnmaxk        input   volumes of the lgrid2 cells
!     nmax    integer    1        input   first dimension of lgrid2
!     nmax    integer    1        input   second dimension of lgrid2
!     nolay   integer    1        input   actual number of layers
!     nosubs  integer    1        input   actual number of substances
!     nopart  integer    1        input   nr of particles
!     npart   integer  nopart     input   n-values of particles
!     mpart   integer  nopart     input   m-values of particles
!     kpart   integer  nopart     input   k-values of particles
!     xpart   real  nopart        input   x-values of particles
!     ypart   real  nopart        input   y-values of particles
!     wpart   real   nosubs* nopart    input   weights of the particles
!     npwndw  integer    1        in/out  start of active nopart number
!     nmap    integer    1        input   dimension of amap
!     mmap    integer    1        input   dimension of amap
!     window  real    4        input   plot grid window
!     amap real nolay*nosubs*nmap*mmap output  plot grid to be filled
!     xa      real  nopart        output  national coordinates of parts
!     ya      real  nopart        output  national coordinates of parts
!     adepth  real    nolay*nosubs  output  depth for max mass
!     apeak   real    nolay*nosubs  output  max   mass per subst/per layer
!     atotal  real    nolay*nosubs  output  total per mass per subst/per layer
!     imap    integer  nopart*3   output  index of a particle in plotgrid
!                                         imap(.,1) = layer
!                                         imap(.,2) = x
!                                         imap(.,3) = y
!     nplay   integer   nolay     local   particle counter per layer
!     itime   integer    1        input   simulation time
!     wsettl  real   nopart       output  conc. dep. settling velocity
!     irfac   integer    1        input   refinement factor plot grid
!     anfac   real       1        input   power for c in c dept. vz
!
!
!     save values between invocations: specified precisely those needed!!
!
      save      first , windw1, windw3, xpf   , ypf, surf
!
!     declarations
!
      type(PlotGrid)                   pg    !< plot grid information
      logical :: first = .true.
      logical :: lsettl
!
!     integer arrays
!
      integer(int_wp ),dimension(:)        :: npart , mpart , kpart
      integer(int_wp ),dimension(:)        :: nplay
      integer(int_wp ),dimension(:,:)      :: imap
      integer(int_wp ),dimension(:,:)      :: lgrid , lgrid2
!
!     real arrays
!
      real   (sp),dimension(4)        :: window
      real   (sp),dimension(:)        :: dps
      real   (sp),dimension(:)        :: tcktot
      real   (sp),dimension(:)        :: volume
      real   (sp),dimension(:)        :: wsettl
      real   (sp),dimension(:)        :: xa    , ya    , za
      real   (sp),dimension(:)        :: xb    , yb    , area
      real   (sp),dimension(:)        :: xpart , ypart , zpart
      real   (sp),dimension(:,:)      :: adepth, apeak
      real   (sp),dimension(:,:)      :: atotal
      real   (sp),dimension(:,:)      :: locdep
      real   (sp),dimension(:,:)      :: wpart
      real   (sp),dimension(:,:,:,:)  :: amap
!
!     local scalars
!
      integer(int_wp ) ::  i1    , i2     , ilay   , isub   , ix     , iy     , lun2
      integer(int_wp ) ::  irfac , mmap   , mmax   , nmap   , mmapl  , nmapl  , nodry , nosubt
      integer(int_wp ) ::  nout  , npwndw , ntdry  , nmax   , nolay  , nopart , nosubs
!
      real   (sp) ::  aa    , ac     , am     , depthl , fvolum , surf
      real   (sp) ::  anfac , conc   , vv     , windw1 , windw3 , xpf    , ypf
      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "part21", ithndl )

!     For the time being

      mmap      = pg%mmap
      nmap      = pg%nmap
      window(1) = pg%xlow
      window(2) = pg%xhigh
      window(3) = pg%ylow
      window(4) = pg%yhigh
      surf      = pg%surf
!
      if ( nopart  <=  0 ) goto 9999
      nosubt = nosubs * nolay
      nmapl = nmap*irfac
      mmapl = mmap*irfac
!
!     compute particle coordinates
!
      call part11                                               &
       ( lgrid  , xb     , yb     , nmax   , npart  , mpart,    &
         xpart  , ypart  , xa     , ya     , nopart , npwndw ,  &
         lgrid2 , kpart  , zpart  , za     , locdep , dps    ,  &
         nolay  , mmax   , tcktot)
!
! initialize some variables for within the loop
!
      if (nopart  <=  100) then
         write(lun2,'(///(6x,a))')                                &
            'Calculation of conc. dependent settling velocities', &
            '(on refined zoom grid)'
      endif
!
      if ( first ) then
         first =  .false.
         windw1 =  window(1)
         windw3 =  window(3)
         xpf = (window(2) - windw1) / mmapl
         ypf = (window(4) - windw3) / nmapl
         surf = xpf*ypf
         write(lun2,'(10x,a/12x,a,es15.7,a,es15.7)')   &
           'Resolution for refined zoom grid:',      &
           ' dx = ',xpf,'; dy=',ypf
!
      endif
!
!     zero plot grid and accumulation/statistics arrays for all substances

       amap   =  0.0 ! whole array assignment
       adepth =  0.0 ! whole array assignment
       atotal =  0.0 ! whole array assignment
       apeak  =  0.0 ! whole array assignment
       wsettl =  1.0 ! whole array assignment
!
      do 10 ilay = 1, nolay
         nplay(ilay) = 0
   10 continue
!
      nout  = 0
      nodry = 0
      ntdry = 0
      do 30 i1 = npwndw, nopart
!        check for particles in permanent dry cells
         i2 = lgrid(npart(i1), mpart(i1))
         if ( i2  <=  1 ) then
           nodry = nodry + 1
           imap(i1,1) = -1  ! permanent dry cells
           goto 30
         endif
!        check for out of plot grid
         ix = int ((xa(i1) - windw1) / xpf) + 1
         iy = int ((ya(i1) - windw3) / ypf) + 1
         if ( ix  <=  0   .or. ix  >  mmapl .or.      &
              iy  <=  0   .or. iy  >  nmapl   ) then
             nout = nout + 1
             imap(i1,1) = -2  ! outside grid
             goto 30
         endif
!        check for running dry of complement during simulation
         aa = area  ( i2 )
!        vv = volume( i2 , 1 )
         vv = volume( i2 )
         if ( aa  <=  0.0 .or. vv  <=  0.0  ) then
            ntdry = ntdry + 1
            imap(i1,1) = -3  ! drying/flooding cell
            goto 30
         endif
!        check for inappropriate layer
         ilay = kpart(i1)
         if ( ilay  <=  0 .or. ilay  >  nolay ) then
            write(lun2,*) ' il, ilay: ', i1, ilay
            write(lun2,*) ' Error: ilay out of range in part21'
            call stop_exit(1)
         endif
!
!        particle has settled out
!        modified (01/06/29):
!                  only in case of sed-ero process active
!                  that means: nolay = extra bed layer
!
         nplay(ilay) = nplay(ilay) + 1
         if (lsettl) then
            if ( ilay == nolay ) then
               imap(i1,1) = -4  ! settled out
               goto 30
            endif
         endif
!        hopefully also some normal particles exist
!        ipp   = ix + (iy-1)*nmapl + (ilay-1)*nmapl*mmapl
!        imap(i1) = ipp
         imap(i1,1) = ilay
         imap(i1,2) = ix
         imap(i1,3) = iy
!        put mass in it's appropriate cell and layer
!        depthl = volume( i2 , ilay ) / area(i2)
         depthl = volume(i2 + (ilay-1)*nmax*mmax) / area(i2)
         fvolum = surf * depthl
         do 20 isub = 1, nosubs
            am = wpart(isub, i1)
            ac = am/fvolum
            atotal(ilay,isub) = atotal(ilay,isub) + am
!           amap  (ipp  ,isub) = amap  (ipp  ,isub) + ac
            amap(isub,ilay,iy,ix) = amap(isub,ilay,iy,ix) + ac
            if ( amap(isub,ilay,iy,ix)  >  apeak(isub,ilay) ) then
               apeak (isub,ilay) = amap(isub,ilay,iy,ix)
               adepth(isub,ilay) = depthl
            endif
   20    continue
   30 continue
!
      if (nout /= 0 .or.  nodry  /=  0 .or. ntdry  /=  0)   &
         write ( lun2 , 155 )
      if ( nout   /=  0 ) write ( lun2 , 160 ) nout
      if ( nodry  /=  0 ) write ( lun2 , 170 ) nodry
      if ( ntdry  /=  0 ) write ( lun2 , 180 ) ntdry
      if (nout /= 0 .or.  nodry  /=  0 .or. ntdry  /=  0)   &
         write ( lun2 , 190 )
!
!     making settling velocities
!
!     test data (< 100 particles)
      if (nopart  <=  100) then
        write(lun2,'(//18x,a)') 'Settling (c**n term)        conc. '
      endif
!
      do 70 i1 = 1,npwndw
         wsettl(i1) = 0.0
   70 continue
      do 90 i1 = npwndw, nopart
!        ipp = imap(i1)
         ilay = imap(i1,1)
         ix   = imap(i1,2)
         iy   = imap(i1,3)
         if ( ilay  <=  0 ) then
            wsettl(i1) = 0.0
         else
            conc = 0.0
            do 80 isub = 1, nosubs
!              conc = conc + amap(ipp,isub)
               conc = conc + amap(isub,ilay,iy,ix)
   80       continue
            wsettl(i1) = conc**anfac
            if (nopart  <=  100) then
               write(lun2,'(10x,i4,2e20.5)') i1, wsettl(i1), conc
            endif
         endif
   90 continue
!
 9999 if ( timon ) call timstop ( ithndl )
      return
!
!     formats
!
  155 format      &
       (//6x,'Warning: from calc. conc. dependent settling velocities')
  160 format      &
       (15x,'Number of particles outside of the plotgrid: ',i8)
  170 format      &
       (15x,'Number of particles at permanent dry points: ',i8)
  180 format      &
       (15x,'Number of particles at temporary dry points: ',i8)
  190 format      &
      (15x,'For all those particles settling velocity was set to zero!'/  &
       15x,'enlarging the zoom grid may solve this problem')

      end subroutine
end module
