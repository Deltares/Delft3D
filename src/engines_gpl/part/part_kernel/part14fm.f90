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

module part14fm_mod

   contains
      subroutine part14fm ( itime  , idelt  , nodye  , nocont , ictime ,    &
                            ictmax , nwaste , mwaste , xwaste , ywaste ,    &
                            zwaste , aconc  , rem    , npart  , ndprt  ,    &
                            mpart  , xpart  , ypart  , zpart  , wpart  ,    &
                            laypart, hpart  ,                               &
                            iptime , nopart , pblay  , radius , nrowswaste, &
                            xpolwaste       , ypolwaste       ,             &
                            ftime  , tmassu , nosubs ,                      &
                            ncheck , t0buoy , modtyp , abuoy  , t0cf   ,    &
                            acf    , lun2   , layt   , tcktot ,    &
                            zmodel , laytop , laybot , nplay  , laywaste ,  &
                            nolay  , linear , track  ,    &
                            nmconr , spart  , rhopart, noconsp, const)

!       Deltares Software Centre

!>\file
!>         Adds mass for continuous releases per time step
!>
!>         The routine has been derived from part14

      use m_waq_precision          ! single/double precision
      use timers
      use grid_search_mod
      use spec_feat_par
      use m_sferic, only: jsferic
      use m_sferic_part, only: ptref
      use geometry_module, only: Cart3Dtospher, sphertocart3D
      use mathconsts, only: raddeg_hp, pi
      use physicalconsts, only: earth_radius
      use random_generator
      use m_part_modeltypes
      implicit none

!     Arguments

!     kind            function         name                    description

      integer  ( int_wp ), intent(in   ) :: nocont                !< nr of continuous loads
      integer  ( int_wp ), intent(in   ) :: nodye                 !< nr of dye release points
      integer  ( int_wp ), intent(in   ) :: nosubs                !< nr of substances
      integer  ( int_wp ), intent(in   ) :: layt                  !< number of hydr. layer
      integer  ( int_wp ), intent(in   ) :: itime                 !< actual time
      integer  ( int_wp ), intent(in   ) :: idelt                 !< time step size
      integer  ( int_wp ), intent(in   ) :: ictime (nocont,*)     !< array of breakpoint times
      integer  ( int_wp ), intent(in   ) :: ictmax (nocont)       !< nr of breakpoints per load
      integer  ( int_wp ), intent(in   ) :: nwaste (nodye+nocont) !< n-values of waste locations
      integer  ( int_wp ), intent(in   ) :: mwaste (nodye+nocont) !< m-values of waste locations
      real     ( real_wp), intent(in   ) :: xwaste (nodye+nocont) !< x-values of waste locations
      real     ( real_wp), intent(in   ) :: ywaste (nodye+nocont) !< y-values of waste locations
      real     ( real_wp), intent(in   ) :: zwaste (nodye+nocont) !< z-values of waste locations
      real     ( real_wp), intent(in   ) :: aconc  (nocont+nodye,nosubs)    !< mass per particle
      real     ( real_wp), intent(inout) :: rem    (nocont)       !< remainder of mass to be released
      integer  ( int_wp ), intent(  out) :: npart  (*)            !< n-values particles
      integer  ( int_wp ), intent(in   ) :: ndprt  (nodye+nocont) !< no. particles per waste entry
      integer  ( int_wp ), intent(  out) :: mpart  (*)            !< m-values particles
      real     ( dp), intent(  out) :: xpart  (*)            !< x-in-cell of particles
      real     ( dp), intent(  out) :: ypart  (*)            !< y-in-cell of particles
      real     ( dp), intent(  out) :: zpart  (*)            !< z-in-cell of particles
      real     ( real_wp), intent(  out) :: wpart  (nosubs,*)     !< weight of the particles
      integer  ( int_wp ), intent(  out) :: laypart  (*)          !< layer in which the particles are found
      real     ( dp), intent(  out) :: hpart  (*)            !< position within the layer for the particles
      integer  ( int_wp ), intent(  out) :: iptime (*)            !< particle age
      integer  ( int_wp ), intent(inout) :: nopart                !< number of active particles
      real     ( real_wp), intent(in   ) :: pblay                 !< relative thickness lower layer
      real     ( real_wp), intent(in   ) :: radius (nodye+nocont) !< help var. radius (speed)
      real     ( sp), pointer       :: xpolwaste(:,:)        !< x-coordinates of waste polygon
      real     ( sp), pointer       :: ypolwaste(:,:)        !< y-coordinates of waste polygon
      integer  ( int_wp ), pointer       :: nrowswaste(:)         !< length of waste polygon
      real     ( real_wp), intent(in   ) :: ftime  (nocont,*)     !< time matrix for wasteloads (mass/s)
      real     ( real_wp), intent(in   ) :: tmassu (nocont)       !< total unit masses cont releases
      integer  ( int_wp ), intent(  out) :: ncheck (nocont)       !< check number of particles per load
      real     ( real_wp), intent(  out) :: t0buoy (*)            !< t0 for particles for buoyancy spreading
      integer  ( int_wp ), intent(in   ) :: modtyp                !< for model type 2 temperature
      real     ( real_wp), intent(  out) :: abuoy  (*)            !< 2*sqrt(a*dt) particles-buoyancy spreading
      real     ( real_wp), intent(in   ) :: t0cf   (nocont)       !< coefficients for loads
      real     ( real_wp), intent(in   ) :: acf    (nocont)       !< coefficients for loads
      integer  ( int_wp ), intent(in   ) :: lun2                  !< output report unit number
      real     ( real_wp), intent(in   ) :: tcktot (layt)         !< thickness hydrod.layer
      logical       , intent(in   ) :: zmodel
      integer  ( int_wp ), intent(in   ) :: laytop(:,:)           !< highest active layer in z-layer model
      integer  ( int_wp ), intent(in   ) :: laybot(:,:)           !< highest active layer in z-layer model
      integer  ( int_wp )                :: nplay  (layt)         !< work array that could as well remain inside
      integer  ( int_wp ), intent(in   ) :: laywaste (nodye+nocont) !< k-values of wasteload points
      integer  ( int_wp ), intent(in   ) :: nolay                 !< number of comp. layer
      integer  ( int_wp ), intent(in   ) :: linear (nocont)       !< 1 = linear interpolated loads
      real     ( real_wp), intent(inout) :: track  (10,*)         !< track array for all particles
      character( 20), intent(in   ) :: nmconr (nocont)       !< names of the continuous loads
      real     ( real_wp), intent(  in)  :: spart  (nosubs,*)     !< size of the particles
      real     ( real_wp), intent(inout) :: rhopart  (nosubs,*)   !< density of the particles
      integer  ( int_wp ), intent(in   ) :: noconsp               !< number of constants
      real     ( real_wp), intent(in   ) :: const(*)              !< constant values

      save

!     Locals

      logical     :: first  =  .true.
      logical        lcircl            ! this thing is always false !
      real   (dp) :: rseed             ! seed of the random number generator
      integer(int_wp ) :: ic                ! loop variable continuous loads
      integer(int_wp ) :: id                ! loop variable to identify time locations
      integer(int_wp ) :: ids, ide          ! interval numbers of start and stop time
      real   (real_wp) :: fac1s, fac2s      ! interpolation factors start location time interval
      real   (real_wp) :: fac1e, fac2e      ! interpolation factors end   location time interval
      real   (dp) :: aconu             ! mass per particle of this wasteload (constant !)
      integer(int_wp ) :: ipb               ! help variable for particle number
      real   (dp) :: rest              ! help variable remainder for this wasteload
      real   (dp) :: avcon             ! average mass/s load over a time interval
      real   (dp) :: amass             ! avcon multiplied with its duration
      real   (dp) :: dts               ! (mass/particle) / (mass/s) gives s/particle for spreaded release
      real   (dp) :: rpnul             ! same is dts, but for the rest from last time => time first particle
      integer(int_wp ) :: nopnow            ! number of particles to be released for this continuos load
      integer(int_wp ) :: ibegin, iend      ! number of first and last particle of the batch of this load
      integer(int_wp ) :: ie                ! ic + nodye, entry number in combined arrays
      integer(int_wp ) :: i, ipart          ! loop/help variables for particles
      integer(int_wp ) :: ntot              ! help variables for particles
      integer(int_wp ) :: nulay             ! help variables for the actual layer in a particle loop
      integer(int_wp ) :: mwasth, nwasth    ! help variables for n and m of wastelocation
      real   (real_wp) :: xwasth, ywasth    ! help variables for x and y of wastelocation within (n,m)
      real   (real_wp) :: zwasth            ! help variables for z within the layer
      real   (real_wp) :: radiuh            ! help variable for the radius0
      real   (dp) :: dpangle, dxp, dyp, dradius, xx, yy
      integer(int_wp ) :: ilay  , isub      ! loop variables layers and substances

      integer(int_wp ) :: np                ! Number of particles to add

      integer(4), save :: ithndl = 0   ! handle to time this subroutine

      if ( timon ) call timstrt( "part14", ithndl )

!     at first zero the remainder

      if ( first ) then
         first  = .false.
         lcircl = .false.
         rem    = 0.0          ! zero remainder array for all continuous loads
         ncheck = 0            ! zero check array for number of particles per load
         rseed  = 0.5d+00      ! seed for random generator
      endif

!     loop over the continuous loads

      do ic = 1, nocont
         write ( lun2, 1000 ) nmconr(ic)
         ie = ic + nodye
         if ( nwaste(ie) .eq. 0 ) then
            write ( lun2, 1030 )
            cycle
         endif

!       loop over the number of break points to find start and stop moment
!       NB: unlike Delwaq, these times may be located in different intervals

         do id = 1, ictmax(ic)
            if ( itime       .ge. ictime(ic,id) ) ids = id
            if ( itime+idelt .gt. ictime(ic,id) ) ide = id
         enddo
         if ( ide .lt. ids ) ide = ids

         fac1s  = float(itime      -ictime(ic,ids)) / float(ictime(ic,ids+1)-ictime(ic,ids))
         fac2s  = 1.0 - fac1s
         fac1e  = float(itime+idelt-ictime(ic,ide)) / float(ictime(ic,ide+1)-ictime(ic,ide))
         fac2e  = 1.0 - fac1e

         aconu  =  tmassu(ic) / ndprt(ie)
         if ( aconu .lt. 1.d-10 ) cycle

!       determine mass by integration of the linear function

         ipb  = nopart + 1
         rest = rem(ic)

         do 40 id = ids, ide
            avcon = ftime(ic,id)
            if ( ids .eq. ide ) then    !    both points in the same interval
               if ( linear(ic) .eq. 1 ) then
                  avcon = ( ( fac2s*ftime(ic,id) + fac1s*ftime(ic,id+1) ) +  &
                            ( fac2e*ftime(ic,id) + fac1e*ftime(ic,id+1) ) )  &
                                                                 / 2.0
               endif
               amass = avcon * idelt
            endif
            if ( ids .ne. ide .and. id .eq. ids ) then   !    first interval
               if ( linear(ic) .eq. 1 ) then
                  avcon = ( ( fac2s*ftime(ic,id) + fac1s*ftime(ic,id+1) ) +  &
                                                         ftime(ic,id+1)   )  &
                                                                  / 2.0
               endif
               amass = avcon * ( ictime(ic,id+1) - itime )
            endif
            if ( id .ne. ids .and. id .ne. ide ) then    !    somewhere in the middle
               if ( linear(ic) .eq. 1 ) then
                  avcon = (         ftime(ic,id) +       ftime(ic,id+1) )    &
                                                                  / 2.0
               endif
               amass = avcon * ( ictime(ic,id+1) - ictime(ic,id) )
            endif
            if ( ids .ne. ide .and. id .eq. ide ) then   !    last interval
               if ( linear(ic) .eq. 1 ) then
                  avcon = ( (       ftime(ic,id)                        ) +  &
                            ( fac2e*ftime(ic,id) + fac1e*ftime(ic,id+1) ) )  &
                                                                  / 2.0
               endif
               amass = avcon * ( itime+idelt     - ictime(ic,id) )
            endif

!     Koster 11 July 2001 : check on zero load added

            if ( amass+rest .ge. 1.d-10 .and. avcon .gt. 1.d-10) then
               dts   = aconu/avcon            ! time span per particle
!              rpnul = rest /aconu * dts
               rpnul = rest /avcon            ! time span first particle
               rest  = rest + amass
               np    = min( ndprt(ie) - ncheck(ic), 1 + int(rest/aconu-1.d-10) )
               do i = 1 , np
                  iptime(ipb) = int(rpnul)
                  ipb         = ipb + 1
                  rpnul       = rpnul - dts
                  rest        = rest  - aconu
               enddo
            else
               rest = rest + amass
            endif

!     end loop across time intervals

   40    continue
!     store remainder (always around zero or negative)
         rem(ic) = rest
         nopnow  = ipb - nopart - 1

!     insert the particles at the end (is already done for iptime(ipb)

         ibegin = nopart + 1
         iend   = nopart + nopnow
         nopart = iend
         ncheck(ic) = ncheck(ic) + nopnow

!!       write( lun2, * ) 'tmassu(ic), aconu, nopnow, rem(ic)', &
!!                         tmassu(ic), aconu, nopnow, rem(ic)

         write ( lun2, 1010 ) tmassu(ic), aconu, nopnow*aconu, rem(ic)
         write ( lun2, 1020 ) ndprt (ie), nopnow, ncheck(ic)

         nwasth = nwaste(ie)
         mwasth = mwaste(ie)
         xwasth = xwaste(ie)
         ywasth = ywaste(ie)
         zwasth = zwaste(ie)
         radiuh = radius(ie)
!.. layer distribution

         if ( laywaste (ie) .eq. 0 ) then              !.. uniform
            ntot  = 0
            do ilay = 1, layt
               nplay(ilay) = int(nopnow*tcktot(ilay))
               ntot = ntot + nplay(ilay)
            enddo
            nplay(1) =  nplay(1) + nopnow - ntot     !.. round off in layer 1
            if ( nplay(1) .lt. 0 ) stop 'Neg. dye release in top layer '
         else                                        !.. for the current layer only
            nplay = 0
            nplay( laywaste(ie) ) = nopnow
         endif

         nulay = 1
         ipart = 0

         do i = ibegin, iend

!         give particles gridcell and coordinates of the continuous load

            npart (i)  = nwasth
            mpart (i)  = mwasth
            xpart (i)  = xwasth
            ypart (i)  = ywasth
            laypart(i) = laywaste(ie)
            if ( modtyp .eq. model_two_layer_temp ) then
               t0buoy(i) = t0cf (ic)                    ! could be taken out of the particle loop
               abuoy (i) = 2.0*sqrt(acf(ic)*idelt)      ! even complete out of this routine
            else
               t0buoy(i) = 0.0
               abuoy (i) = 0.0
            endif


!     horizontal distribution (spreaded in a circle if required - this is at present copied for part09fm

         nulay = 1

            if (radiuh.ne.-999.0) then
!              spread the particles over a circle
! this is the code to deal with sferical models (if needed) te get the distances correct
               dpangle =2.0D0 * pi * rnd(rseed)
               dradius = rnd(rseed) * radiuh !noteradius is in m. need to convert to degrees.
               dxp = cos(dpangle) * dradius
               dyp = sin(dpangle) * dradius
               xpart(i) = xwasth + dxp !radius(iload)/2. * rnd(rseed)
               ypart(i) = ywasth + dyp !radius(iload)/2. * rnd(rseed)
              ! trpart(ipart) = iwtime(iload)
               if (jsferic == 1) then
                  dradius = atan2(dradius,earth_radius)*raddeg_hp !in degrees
                  dxp = cos(dpangle) * dradius
                  dyp = sin(dpangle) * dradius
! the distance is expressed in degrees (to make a circle for spherical models,
                 call Cart3Dtospher(dble(xwasth), dble(ywasth), dble(zwasth) ,xx,yy,ptref)
                 xx = xx + dxp
                 yy = yy + dyp
                 call sphertocart3D(xx,yy,xpart(i),ypart(i),zpart(i))
               endif

            else
               radiuh = 0
            end if


!         give the particles a layer number
            do
               ipart = ipart + 1
               if ( ipart .gt. nplay(nulay) ) then         ! next layer
                  ipart = 0
                  nulay = nulay + 1
                  if ( nulay .gt. nolay ) then
                     nulay = nolay
                     exit
                  endif
               else
                  exit
               endif
            enddo

            if (zmodel) then
               laypart(i) = min(laybot(npart(i), mpart(i)), max(nulay,laytop(npart(i), mpart(i))))
            else
               laypart(i) = nulay
            endif

!           give the particles a z-value within the layer
            if ( modtyp .eq. model_two_layer_temp ) then     ! .. two layer model use a pointe discharge (as in v3.00)
               write(*,*) 'Stratified two layer model not supported for unstructured grids'
               error stop
            elseif ( modtyp .eq. model_oil .and. laypart(i) .eq. 1 ) then   !   for one layer models (2dh),
               hpart(i) = 0.0_dp               !      the release will be in the user-defined location
            else                               !      release randomly distributed over the vertical
               hpart(i) = rnd(rseed)
            endif

!         initialize rest of variables for new particle

            do isub = 1, nosubs
               wpart (isub, i) = aconc(ie, isub)
               if (modtyp .eq. model_prob_dens_settling) then
                  rhopart(isub, i) = pldensity(isub)
               endif
            enddo

            track(1,i) = mpart(i)              !       store information required in initial part
            track(2,i) = npart(i)              !       of nefis file for particle tracking
            track(3,i) = laywaste(ie)               !       (see routine wrttrk)
            track(4,i) = xpart(i)
            track(5,i) = ypart(i)
            track(6,i) = zpart(i)
            track(7,i) = laypart(i)
            track(8,i) = hpart(i)
            track(9,i) = itime
            track(10,i) = ic + nodye

!     end loop across the added particles for this continuous load

         enddo

!     write test data

         if ( iend .gt. ibegin .and. iend .le. 20 ) then
            write ( lun2, '(5x,a,(10i10))' )                     &
                   ' Release times (rel.) [iptime in part14]:',  &
                    ( iptime(i) , i = ibegin, iend )
         endif

!     end of loop across continuous loads

      enddo

!     end of subroutine

      if ( timon ) call timstop ( ithndl )
      return

!     formats

 1000 format(6x,'Continuous release ',a)
 1010 format(10x,'Total m3 of water to be added for this run : ',es15.7/   &
             10x,'m3 of water per particle                   : ',es15.7/   &
             10x,'Actual m3 of water added this step         : ',es15.7/   &
             10x,'Actual rest m3 of water (round off)        : ',es15.7)
 1020 format(10x,'Total number of particles to be added      : ',i12/     &
             10x,'Actual number of particles added this step : ',i12/     &
             10x,'Total number of particles added until now  : ',i12)
 1030 format( 10x,'Warning: this release is outside active area !' )

      end subroutine
end module
