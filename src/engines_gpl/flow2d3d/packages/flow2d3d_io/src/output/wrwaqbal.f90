!> \file
!! computes and writes mass balance information per Delwaq time step
!! writes flows of loads and withdrawals to an intermediate file
      subroutine wrwaqbal ( noseg     , noq12  , noq    , vol     , vol2   , &
     &                      flow      , horsurf, ifrmto , nmax    , mmax   , &
     &                      kmax      , isaggr , nolay  , nsrc    , mnksrc , &
     &                      discumwaq , loads  , nowalk , iwlk    , itim   , &
     &                      idt       , nobrk  , zmodel , mode    , naccum , &
     &                      lunsrctmp , lunwlk , dvol   , mtimstep, lundia )
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
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
      use precision
      implicit none
!
!           Global variables
!
      integer(4) noseg                     !<  Number of active WAQ segments
      integer(4) noq12                     !<  Number of horizontal WAQ exchanges
      integer(4) noq                       !<  Total number of WAQ exchanges
      real   (4) vol    (0:noseg)          !<  WAQ volumes at start of time step
      real   (4) vol2   (0:noseg)          !<  WAQ volumes at end of time step
      real   (4) flow   (0:noq  )          !<  Flow between WAQ volumes
      real   (4) horsurf(  noseg)          !<  Horizontal surfaces of WAQ cells
      integer(4) ifrmto(4,noq)             !<  From-To flow pointer table
      integer(4) nmax                      !<  dimension  first index in 2d arrays
      integer(4) mmax                      !<  dimension second index in 2d arrays
      integer(4) kmax                      !<  Number of layers
      integer(4) isaggr(nmax,mmax,kmax)    !<  grid aggregation pointer
      integer(4) nolay                     !<  number of WAQ layers
      integer(4) nsrc                      !<  Number of loads
      integer(4) mnksrc(7,nsrc)            !<  locations and type
      real  (fp) discumwaq(nsrc)           !<  accumulated flows of loads m3/s*naccum
      real   (4) loads (nsrc)              !<  the last flows in m3/s
      integer(4) nowalk                    !<  number of walking discharges
      integer(4) iwlk(nsrc)                !<  their number in the list of dicharges
      integer(4) itim                      !<  time of writing
      integer(4) idt                       !<  Times step between vol1 and vol2
      integer(4) nobrk                     !<  number of time steps in wasteload file
      logical    zmodel                    !<  true if z-model feature is used
      integer(4) mode                      !<  zero at init, 2 at finalisation
      integer(4) naccum                    !<  number of fluxes accumulated
      integer(4) lunsrctmp, lunwlk         !<  file unit number to an output file
      real   (4) dvol   (0:noseg)          !<  help array outflows CFL criterion
      real   (fp) mtimstep                 !<  maximum time step size
      integer(4) lundia                    !<  unit nr diagnostic output file
!
!           Local variables
!
      integer(4) iseg , iq               !   Loop counter over segments and exchanges
      integer(4) ifrom, ito              !   from and to pointer help variables
      integer(4) nosegl, isegl           !   Number of WAQ segments per layer
      real   (4) aflow, aflw2, sum       !   help variable for the actual m3
      real   (4) avol , dvolmx, dvoltot  !   maximum volume and maximum relative error
      integer(4) imxseg                  !   segment nr of max relative error
      real   (8) dvol1                   !   help variables
      integer(4) i, il, itop             !   loop counters and help variable
      integer(4) m, n, k                 !   the m & m - etjes
      logical    changed                 !   are the wasteloads changed ?
!
!! executable statements -------------------------------------------------------
!
!           general

      itop  = 1
      if ( zmodel ) itop = kmax

!           make the theoretical new volumes from the old ones and the flows

      nosegl = noseg/nolay
      dvol = 0.0
      do iq = 1,noq
         ifrom = ifrmto(1,iq)
         ito   = ifrmto(2,iq)
         aflow = flow(iq)*idt
         if ( ifrom .gt. 0 ) then
            vol(ifrom) = vol(ifrom) - aflow
            if ( flow(iq) .gt. 0.0 ) dvol(ifrom) = dvol(ifrom) + flow(iq)
         endif
         if ( ito   .gt. 0 ) then
            vol(ito  ) = vol(ito  ) + aflow
            if ( flow(iq) .lt. 0.0 ) dvol(ito  ) = dvol(ito  ) - flow(iq)
         endif
      enddo

!           add the known loads to the volumes and detect changes

      changed = .false.
      if ( mode .eq. 0 ) changed = .true.
      do i = 1, nsrc
         if (mnksrc(3,i) == -1) cycle ! awkward disabling of discharges outside partition when running parallel
         if (mnksrc(7,i) == 4 .or. mnksrc(7,i) == 5 .or. mnksrc(7,i) == 8) cycle ! skip e,d and f culverts, output is not correct
         m = mnksrc(1,i)
         n = mnksrc(2,i)
         k = mnksrc(3,i)
         discumwaq(i) = discumwaq(i)/naccum
         aflow     = discumwaq(i)
         if ( abs(aflow-loads(i)) .gt. 1.0E-10 ) changed = .true.
         if ( k .ne. 0 ) then
            iseg = isaggr(n,m,k)
            if ( mnksrc(7,i) .le. 1 ) then   !  no inlet-outlet and no culverts
               vol(iseg) = vol(iseg) + aflow * idt
            else
               vol(iseg) = vol(iseg) - aflow * idt
            endif
         else
            if ( zmodel ) changed = .true.   !  because of changed waterlevels
         endif
         if ( mnksrc(7,i) .le. 1 ) cycle   !  no inlet-outlet and no culverts
         m = mnksrc(4,i)
         n = mnksrc(5,i)
         k = mnksrc(6,i)
         if ( k .ne. 0 ) then
            iseg = isaggr(n,m,k)
            vol(iseg) = vol(iseg) + aflow * idt
         endif
      enddo

!           detect the flows of depth averaged loads
!           from the closure error at their locations
!           write temporary file if loads have changed

      if ( changed .and. ( nsrc .ne. 0 ) ) then
         nobrk = nobrk + 1
         write ( lunsrctmp , '(i10,A)' ) itim,'          ; breakpoint time'
      endif
      do i = 1, nsrc
         if (mnksrc(3,i) == -1) cycle ! awkward disabling of discharges outside partition when running parallel
         if (mnksrc(7,i) == 4 .or. mnksrc(7,i) == 5 .or. mnksrc(7,i) == 8) cycle ! skip e,d and f culverts, output is not correct
         m = mnksrc(1,i)
         n = mnksrc(2,i)
         k = mnksrc(3,i)
         aflow = discumwaq(i)                   ! m3/s
         if ( k .ne. 0 ) then
            if ( changed .or. mode .eq. 2 ) then
               iseg = isaggr(n,m,k)
               il = (iseg-1)/nosegl+1
               if ( mnksrc(7,i) .le. 1 ) then   !  no inlet-outlet and no culverts
                  write ( lunsrctmp , '(3I8,E15.6)' ) i,iseg,il, aflow
               else
                  write ( lunsrctmp , '(3I8,E15.6)' ) i,iseg,il,-aflow
               endif
            endif
         else
            iseg = isaggr(n,m,itop)
            sum  = 0.0
            do il = 1 , nolay                    !  m3 / timestep
               sum  = sum + vol2(iseg) - vol(iseg)
               iseg = iseg + nosegl
            enddo
            if ( abs(sum) .lt. 1.0E-28 ) sum = 1.0
            iseg = isaggr(n,m,itop)
            do il = 1 , nolay
               aflw2 = ( (vol2(iseg)-vol(iseg)) / sum ) * aflow       ! m3 / sec
               if ( mnksrc(7,i) .gt. 1 ) aflw2 = -aflw2   !  inlet-outlet or culvert
               vol(iseg) = vol(iseg) + aflw2 * idt
               if ( changed .or. mode .eq. 2 ) then
                  write ( lunsrctmp , '(3I8,E15.6)' ) i,iseg,il,aflw2
               endif
               iseg = iseg + nosegl
            enddo
         endif
      enddo
      do i = 1, nsrc
         if (mnksrc(7,i) .le. 1) cycle   !  no inlet-outlet and no culverts
         if (mnksrc(7,i) == 4 .or. mnksrc(7,i) == 5 .or. mnksrc(7,i) == 8) cycle ! skip e,d and f culverts, output is not correct
         m = mnksrc(4,i)
         n = mnksrc(5,i)
         k = mnksrc(6,i)
         aflow = discumwaq(i)
         if ( k .ne. 0 ) then
            if ( changed .or. mode .eq. 2 ) then
               iseg = isaggr(n,m,k)
               il = (iseg-1)/nosegl+1
               write ( lunsrctmp , '(3I8,E15.6)' ) i,iseg,il, aflow
            endif
         else
            iseg = isaggr(n,m,itop)
            sum  = 0.0
            do il = 1 , nolay
               sum  = sum + vol2(iseg) - vol(iseg)
               iseg = iseg + nosegl
            enddo
            if ( abs(sum) .lt. 1.0e-28 ) sum = 1.0
            iseg = isaggr(n,m,itop)
            do il = 1 , nolay
               aflw2 = ( (vol2(iseg)-vol(iseg)) / sum ) * aflow         !  m3 / sec
               vol(iseg) = vol(iseg) + aflw2 * idt
               if ( changed .or. mode .eq. 2 ) then
                  write ( lunsrctmp , '(3I8,E15.6)' ) i,iseg,il,aflw2
               endif
               iseg = iseg + nosegl
            enddo
         endif
      enddo
      loads     = discumwaq
      discumwaq = 0.0

!

      if ( nowalk .gt. 0 ) then
         m = mnksrc(4,iwlk(1))
         n = mnksrc(5,iwlk(1))
         k = mnksrc(6,iwlk(1))
         iseg = isaggr(n,m,k)
         write ( lunwlk , '(i12/4i12)' ) itim, iseg, m, n, k
         do i = 2,nowalk
            m = mnksrc(4,iwlk(i))
            n = mnksrc(5,iwlk(i))
            k = mnksrc(6,iwlk(i))
            iseg = isaggr(n,m,k)
            write ( lunwlk , '(   4I12)' )    iseg, m, n, k
         enddo
      endif

!         determine closure error per time step

      dvolmx  = 0.0
      dvoltot = 0.0
      do iseg = noseg,1,-1                                            ! from bottom to surface
         dvol1 = vol2(iseg) - vol(iseg)                               ! difference of theoretical volume
         dvoltot = dvoltot + dvol1
         avol  = max( vol(iseg), vol2(iseg) )                         ! and practical volume
         if ( avol .gt. 1.0E-25 ) then
            if ( abs(dvol1) .gt. horsurf(iseg)*1.0E-3 ) then          ! substantial
               isegl = iseg - nosegl                                  ! segment 1 layer higher
               if ( isegl .gt. 0 ) then
                  do iq = noq12+1,noq
                     ifrom = ifrmto(1,iq)                             ! seek the corresponding flow
                     ito   = ifrmto(2,iq)
                     if ( ifrom .eq. isegl .and. ito .eq. iseg ) then
                        flow( iq  ) = flow( iq  ) + dvol1/idt         ! correct the flow
                        vol(iseg ) = vol(iseg ) + dvol1               ! and the volumes
                        vol(isegl) = vol(isegl) - dvol1
                        dvol1 = 0.0                                   ! dvol1 accounted for
                        exit
                     endif
                  enddo
               endif
               if ( dvol1 .ne. 0.0 ) then                             ! if not: message
                  write( lundia, '(A,I8,A,E15.6,A,E15.6,A)' )            &
     &                    "Closure error in.", iseg," is ",              &
     &                    dvol1," m3 or ",                               &
     &                    dvol1/horsurf(iseg)*1000.0," mm."
               endif
            endif
            if ( abs(dvol1/avol) .gt. dvolmx ) then                   ! find the maximum
               dvolmx = abs(dvol1/avol)
               imxseg = iseg
            endif
         endif
      enddo                                                           ! print it
      write( lundia,'(A,I10,A,E15.6,A,E15.6,A,I8,A)' )                   &
     &       "WAQ input written at itime ", itim+idt,                    &
     &       ". Closure error: ", dvoltot,                               &
     &       ", ", dvolmx, "(",imxseg, "): Total[m3], Max.Rel.(segment)"

!         determine CFL maximum time step size for explicit advection

      dvoltot = 0.0
      do iseg = 1,noseg
         if ( dvol(iseg) .gt. 1.0e-20 ) then
            avol = vol(iseg)/dvol(iseg)
            if ( avol .lt. dvoltot .or. dvoltot .eq. 0.0 ) then
               dvoltot = avol
               imxseg  = iseg
            endif
         endif
      enddo
      write( lundia,'(39X,A,F15.2,A,I8)' )                               &
     &         "Maximum timestep: ", dvoltot,                            &
     &       " s in computational element: ",imxseg, "."
      if ( mtimstep .lt. 0 ) then
         mtimstep = dvoltot
      else
         mtimstep = min ( real(mtimstep,4), dvoltot )
      endif
      write( lundia,'(//,A,F15.2)' )                                     &
     &               "Maximum timestep whole simulation: ", mtimstep

      vol = vol2
      end subroutine wrwaqbal
