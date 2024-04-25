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

module part18_mod
!
!  module declarations
!
!  data definition module(s)
!
use m_stop_exit
use m_waq_precision               ! single/double precision
      use timers
!
!  module procedure(s)
!
use partfl_mod              ! explicit interface
use random_generator
!
implicit none               ! force explicit typing
!
contains
      subroutine part18 ( lgrid  , velo   , conc   , flres  , volume , &
                          area   , mnmaxk , npart  , mpart  , wpart  , &
                          zpart  , nopart , idelt  , nolay  , npwndw , &
                          vdiff  , pblay  , ptlay  , const  , nocons , &
                          lun2   , nosubs , layt   , kpart  , icvdf  , &
                          wvelo  , alpha  , nosubc , icvdf2    )
!
!
!     function              : calculates the exchange between the
!                             two layers and exchanges the particles
!                             as part of the 3d version
!
!     note                  : kpart is used to indicate the layers
!                             in the two layer model goes volume with dim. mnmax2
!                             and is mnmaxk = mnmax2 (layt must be 1)
!                             and goes conc with mnmaxk*nolay, with nolay=2
!
!                             only for the two-layer model (modtyp = model_two_layer_temp)
!                             for the two layer model, one layer is used for storage
!                             the total number of substances is nosubs*nolay
!
!
!     subroutines called    : partfl, stop_exit
!
!
!     functions   called    : vdisp.
!
!
!     parameters            :
!
!     name    kind     length     funct.  description
!     ====    ====     ======     ======  ===========
!     alpha   real        1       input   scaling factor vertical diffusivity
!     area    real      mnmaxk    input   surface area of grid cells
!     conc    real nosubs*mnmaxk*nolay input   concentration in two layers
!     const   real      nocons    input   user-defined constants
!     flres   real  nosubs*mnmaxk in/out  residue of mass to be exchanged
!     icvdf   integer     1       input   subst that is excess temp. top layer
!     icvdf2  integer     1       input   subst that is excess temp. bot layer
!     idelt   integer     1       input   model time step
!     itime   integer     1       input   time in calculation
!     kpart   integer   nopart    input   k-value of particles
!     layt    integer     1       input   number of hydr. layers (must be 1)
!     lgrid   integer  nmax*mmax  input   active grid layout of the area
!     lun2    integer     1       input   unit number of report file
!     mnmaxk  integer     1       input   dimension of volume
!     mpart   integer   nopart    input   m-value of particles
!     nmax    integer     1       input   dimension of lgrid
!     nocons  integer     1       input   number of constants
!     nolay   integer     1       input   number of layers (must be 2)
!     nopam   integer     1       input   number of parameters
!     nopart  integer     1       input   number of active particles
!     nosubc  integer     1       input   leading dimension cocn. array
!                                         (nosubc = nosubs*nolay for 2-layer model)
!     nosubs  integer     1       input   number of substances
!     npart   integer   nopart    input   n-value of particles
!     npwndw  integer     1       input   start of active nopart number
!     param   real    nopa*mnmaxk input   user-defined parameters
!     pblay   real        1       input   relative thickness lower layer
!     ptlay   real        1       input   relative thickness upper layer
!     vdiff   real      mnmaxk    in/out  vertical diffusion - work array
!     velo    real      mnmaxk    input   absolute velocity in cells
!     volume  real      mnmaxk    input   volumes of the grid cells
!     wpart   real nosubs*nopart  input   weight of the particles
!     wvelo   real        1       input   wind velocity
!     zpart   real      nopart    in/out  vertical coordinate of particles
!     ----    ----     ------     ------  -----------
!     first   logical     1       local   true at first entry
!     flux    real        1       local   vertical flux exchange
!     i       integer     1       local   help variable
!     ic      integer     1       local   help variable
!     j       integer     1       local   help variable
!     nopam   integer     1       local   number of parameters
!     ntwarn  integer     1       local   total number of warnings
!     tflux   real        1       local   total vertical flux exchange
!     vol1    real        1       local   volume of a particle in layer 1
!     vol2    real        1       local   volume of a particle in layer 2
!
!
!     save values between invocations
!
      save
!
!     dimensioning
!
      integer(int_wp ),dimension(:)    :: npart , mpart , kpart
      integer(int_wp ),dimension(:,:)  :: lgrid
!
!     dimensioning
!
      real   (sp),dimension(:)    :: vdiff , velo  , volume , area , const , zpart
      real   (dp),dimension(:)    :: wvelo
      real   (sp),dimension(:,:)  :: conc
      real   (sp),dimension(:,:)  :: flres
      real   (sp),dimension(:,:)  :: wpart
!
      logical  ::   first =  .true.
      real(dp) ::   rseed = 0.5d+00
!
!     local scalars
!
      integer(int_wp ) ::  i     , ic    , icvdf  , icvdf2 , idelt , layt  , lun2
      integer(int_wp ) ::  mnmaxk, nocons, nofl1  , nofl2  , nolay , nopart
      integer(int_wp ) ::  nosubc, nosubs, npwndw
      real   (sp) ::  alpha , arand , flux   , pblay  , ptlay
      real   (sp) ::  tflux , vol1  , vol2
      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "part18", ithndl )
!
!     call the user-defined routine partfl:
!     the purpose of this routine is to calculate for each segment
!     the exchange volume per square meter per second
!     this is done on basis of substance no. 1 concentrations !!!!
!
      if(nolay /= 2) then
         write(lun2,*) ' Illegal call to stratification model '
         write(lun2,*) ' Number of layers is not equal to 2 ! '
         call stop_exit(1)
      endif
!
!3d
      if ( layt /= 1 ) then
         write (*,*) ' Hydrodynamics should be based on 1 layer for part18'
         write( lun2,*) ' Hydrodynamics should be based on 1 layer for part18'
         call stop_exit(1)
      endif
!3d
!
      call partfl ( mnmaxk , conc   , volume , area   , velo   ,     &
                    vdiff  , const  , nocons , pblay  , nosubs ,     &
                    icvdf  , nolay  , alpha  , wvelo  , lun2   ,     &
                    nosubc , icvdf2 , lgrid  )
!
!     if first initialise several values
!
      if (first) then
        first = .false.
!
        ptlay = 1.0 - pblay
!
!       initialise overflow array
!
        flres = 0.0  ! whole array assignment
!
        goto 9999
      endif
!
!     calculate the number of particles that should go to the other
!     layer:
!     1. the volume per square meter per second to be transported
!        has been determined
!     2. in the next loop, the net amount of mass to be flipped is
!        determined by implicit calculations. positive is downward.
!     3. the flipping that follows flips not randomly, but takes the
!        newest particles first ! there is still no solution for this
!        effect implemented. the effect is partly minimized because
!        the older (smaller) particles may fill the gaps.
!        it flips only in the direction of net flux.
!
      do 30, i = 1 , mnmaxk
!
! -------- check whether the segment is dry or not
!
        if (velo(i)  /=  0.0) then
!         icvdf2 = icvdf + 1
          if (conc(icvdf,i)  /=  0.0 .or. conc(icvdf2,i)  &
                                 /=  0.0 ) then
!
! -------- implicit vertical transport
!
            if ( vdiff(i)  >  0.0 ) then
               vol1        = ptlay    * volume(i)
               vol2        = pblay    * volume(i)
               tflux       = vdiff(i) * area(i)*idelt
               flux        = tflux   &
                             / (1.0 + tflux / vol1 + tflux / vol2)
               flres(1, i) = flres(1, i) +  &
                       flux * (conc(icvdf, i) - conc(icvdf2, i))
            else
               flres(1, i) = 0.0
            endif
          endif
        endif
   30 continue
!
!     flip the particles, starting with the largest
!     only for 2 layers
!     the z coordinate is reflected also
!
      nofl1  = 0
      nofl2  = 0
      do 200, i = nopart , npwndw , -1
        ic     = lgrid(npart(i),mpart(i))
        if (ic  >  0) then
          if (kpart(i)  == 1  ) then
            if (flres(1,ic)  >  0.0) then
              kpart(i)     =  2
              zpart(i)     =  1.0 - zpart(i)
              flres(1, ic) =  flres(1, ic) - wpart(1, i)
              nofl1  = nofl1  + 1
            endif
          else
            if (flres(1,ic)  < 0.0) then
              kpart(i)     =  1
              zpart(i)     =  1.0 - zpart(i)
              flres(1, ic) =  flres(1, ic) + wpart(1, i)
              nofl2  = nofl2  + 1
            endif
          endif
        endif
  200 continue
!
!     now the special case of non-stratified segments:
!     flip the particles at random
!     the chance that a particle should move to the other layer is
!     larger if the layer is thinner
!
      do 300, i = nopart , npwndw , -1
        ic     = lgrid(npart(i),mpart(i))
        if (ic  >  0) then
          if (vdiff(ic)  < 0.0) then
            arand  = rnd(rseed)
            if (arand  <=  pblay) then
              if ( kpart(i)  == 1   ) then
                 kpart(i) = 2
                 zpart(i) = 1.0 - zpart(i)
              endif
            else
              if ( kpart(i)  == 2   ) then
                 kpart(i) = 1
                 zpart(i) = 1.0 - zpart(i)
              endif
            endif
          endif
        endif
  300 continue
      write(lun2,*) 'Number of particles flipped: ',nofl1,nofl2
!
!     end of procedure
!
 9999 if ( timon ) call timstop ( ithndl )
      return
!
      end subroutine
end module
