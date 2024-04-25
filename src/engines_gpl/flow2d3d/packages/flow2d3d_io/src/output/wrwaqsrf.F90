      subroutine wrwaqsrf ( nmax   , mmax   , kmax   , nlb    , nub    , &
     &                      mlb    , mub    , gsqs   , guv    , gvu    , &
     &                      guu    , gvv    , xcor   , ycor   , xz     , &
     &                      yz     , depth  , chezu  , chezv  , chez   , &
     &                      noseg  , noq1   , noq2   , noq3   , nobnd  , &
     &                      aggre  , isaggr , iqaggr , ilaggr , ifrmto , &
     &                      horsurf, itop   , filnam )
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
      integer(4) nmax                       !!  Dimension of first index in 2d arrays
      integer(4) mmax                       !!  Dimension of second index in 2d arrays
      integer(4) kmax                       !!  number of layers
      integer(4) nlb                        !!  Lower bound of all n dimensions
      integer(4) nub                        !!  Upper bound of all n dimensions
      integer(4) mlb                        !!  Lower bound of all m dimensions
      integer(4) mub                        !!  Upper bound of all m dimensions
      real(fp) gsqs ( nlb:nub , mlb:mub )   !!  Array with horizontal surfaces
      real(fp) guv                          !!  distance between zeta points over v points
      real(fp) gvu                          !!  distance between zeta points over u points
      real(fp) guu  ( nlb:nub , mlb:mub )   !!  Array with distances in dir 1
      real(fp) gvv  ( nlb:nub , mlb:mub )   !!  Array with distances in dir 2
      real(fp) xcor                         !!  X-coordinates of depth points
      real(fp) ycor                         !!  Y-coordinates of depth points
      real(fp) xz                           !!  X-coordinates of zeta points
      real(fp) yz                           !!  Y-coordinates of zeta points
      real(hp) depth( nlb:nub , mlb:mub )   !!  Array with a values
      real(fp) chezu( nlb:nub , mlb:mub, 3) !!  Chezy in u points
      real(fp) chezv( nlb:nub , mlb:mub, 3) !!  Chezy in u points
      logical chez                          !!  If true there is a chezy value
      integer(4) noseg                      !!  number of segments
      integer(4) noq1                       !!  number of exchanges in 1st direction
      integer(4) noq2                       !!  number of exchanges in 2nd direction
      integer(4) noq3                       !!  number of exchanges in 3rd direction
      integer(4) nobnd                      !!  number of open boundaries
      integer(4) aggre                      !!  1 if aggregation, 0 is condensation
      integer(4) isaggr(nmax,mmax,kmax)     !!  horizontal grid aggregation pointer
      integer(4) iqaggr(nmax,mmax,kmax,3)   !!  exchanges aggregation pointer
      integer(4) ilaggr(kmax)               !!  vertical layer aggregation array
      integer(4) ifrmto(4,*)                !!  from- to exchange pointer array
      real   (4) horsurf(noseg)             !!  horizontal surface
      integer(4) itop                       !!  1 for sigma model, kmax for z-model
      character( *) filnam                  !!  Filename without extension
!
!           Local variables
!
      integer(4) i, j, k, iseg
      integer(4) nolay                      !!  number of layers in the model
      integer(4) nosegl, nobndl, noq3l
      real   (4), pointer :: surf(:), cc(:), dd(:)
      integer(4) lunout
      integer(4) istat                      !!  allocate return status
!
!! executable statements -------------------------------------------------------
!

      nolay  = ilaggr(kmax)
      nosegl = noseg / nolay
      nobndl = nobnd / nolay

!
                    allocate ( surf(-nobndl:nosegl)        , stat=istat)
      if (istat==0) allocate ( cc  (-nobndl:nmax*mmax*2+1) , stat=istat)
      if (istat==0) allocate ( dd  (-nobndl:nmax*mmax*2+1) , stat=istat)  ! also space for lengthes
      if (istat/=0) then
         write(*,*) '*** ERROR: wrwaqsrf: memory allocation error'
         return
      endif
!     
! write horizontal surface area and weighted depth
!
      cc   = 0.0
      dd   = 0.0
      surf = 0.0
      do j = 1,mmax
         do i = 1,nmax
            iseg = isaggr(i,j,itop)
            surf(iseg) = surf(iseg) + gsqs(i,j)
            dd  (iseg) = dd  (iseg) + gsqs(i,j)*depth(i,j)
            if ( iseg .gt. 0 ) cc(iseg) = cc(iseg) + gsqs(i,j)
         enddo
      enddo
      do k = -nobndl,nosegl
         if ( surf(k) .lt. 1.0e-30 ) surf(k) = 1.0
         dd(k) = dd(k) / surf(k)       ! weighed average of depth
      enddo
      do k = 1, nosegl
         if ( cc(k) .lt. 1.0e-30 ) cc(k) = 1.0
         do i = 0 , ilaggr(kmax)-1
            horsurf(k+i*nosegl) = cc(k)
         enddo
      enddo
!
#ifdef HAVE_FC_FORM_BINARY
      open  ( newunit = lunout , file=trim(filnam)//'srfold', form='binary' )
#else
      open  ( newunit = lunout , file=trim(filnam)//'srfold', form = 'unformatted', access='stream')
#endif
      write ( lunout ) nmax, mmax, nosegl, nosegl, nosegl, 0.0
      write ( lunout ) cc  (1:nosegl)
      close ( lunout )
      
#ifdef HAVE_FC_FORM_BINARY
      open  ( newunit = lunout , file=trim(filnam)//'srf', form='binary' )
#else
      open  ( newunit = lunout , file=trim(filnam)//'srf', form = 'unformatted', access='stream')
#endif
      write ( lunout ) 0.0
      write ( lunout ) (cc  (1:nosegl), i=1, nolay)
      close ( lunout )
!
! write depth at cell centra (depth at zeta point (dps))
!      
#ifdef HAVE_FC_FORM_BINARY
      open  ( newunit = lunout , file=trim(filnam)//'dps', form='binary' )
#else
      open  ( newunit = lunout , file=trim(filnam)//'dps', form = 'unformatted', access='stream')
#endif
      write ( lunout ) nmax, mmax, nosegl, nosegl, nosegl, 0.0
      write ( lunout ) dd  (1:nosegl)
      close ( lunout )
!                   write chezy values in u and v points
      if ( chez ) then
         cc = 0.0
         dd = 0.0
         do j = 1,mmax
            do i = 1,nmax
               iseg = isaggr(i,j,itop)
               cc(iseg) = cc(iseg) + chezu(i,j,2)*gsqs (i,j)
               dd(iseg) = dd(iseg) + chezv(i,j,2)*gsqs (i,j)
            enddo
         enddo
         do k = 1,nosegl
            cc(k) = cc(k) / surf(k)
            dd(k) = dd(k) / surf(k)
         enddo
#ifdef HAVE_FC_FORM_BINARY
      open  ( newunit = lunout , file=trim(filnam)//'chz', form='binary' )
#else
      open  ( newunit = lunout , file=trim(filnam)//'chz', form = 'unformatted', access='stream')
#endif
         write ( lunout ) kmax, nmax, mmax, noseg, noq1, noq2, noq3
         write ( lunout ) cc(1:nosegl), dd(1:nosegl)
         close ( lunout )
      endif
!                   write the width of the cross-sectional areas
      dd = 0.0
      do j = 1,mmax
         do i = 1,nmax
            iseg = iqaggr(i,j,itop,1)
            if ( iseg .gt. 0 ) cc(iseg) = cc(iseg) + guu(i,j)
            iseg = iqaggr(i,j,itop,2)-noq1
            if ( iseg .gt. 0 ) dd(iseg) = dd(iseg) + gvv(i,j)
         enddo
      enddo
#ifdef HAVE_FC_FORM_BINARY
      open  ( newunit = lunout , file=trim(filnam)//'wdt', form='binary' )
#else
      open  ( newunit = lunout , file=trim(filnam)//'wdt', form = 'unformatted', access='stream')
#endif
      write ( lunout ) nmax, mmax, nosegl, noq1, noq2, nolay
      write ( lunout ) cc  (1:noq1/nolay), dd(1:noq2/nolay)
      close ( lunout )

!           Now the lengthes between the segment midpoints

!           Open file
#ifdef HAVE_FC_FORM_BINARY
      open  ( newunit = lunout , file=trim(filnam)//'len', form='binary' )
#else
      open  ( newunit = lunout , file=trim(filnam)//'len', form = 'unformatted', access='stream')
#endif
      write ( lunout ) noq1+noq2+noq3        ! number of exchanges
!
      if ( aggre .le. 0 ) then      ! no aggregation, contraction possible
         call wrwaqlennoag (nmax   , mmax   , kmax   , nlb    , nub    ,  &
     &                      mlb    , mub    , guv    , gvu    , noq1   ,  &
     &                      noq2   , dd     , iqaggr , ilaggr , itop   ,  &
     &                      lunout )
      else                          ! aggregation, curvilinear grid
         call wrwaqlenaggr (nmax   , mmax   , kmax   , nlb    , nub    ,  &
     &                      mlb    , mub    , xcor   , ycor   , guu    ,  &
     &                      gvv    , gsqs   , xz     , yz     , nosegl ,  &
     &                      noq1   , noq2   , nobndl , surf   , dd     ,  &
     &                      isaggr , iqaggr , ilaggr , itop   , ifrmto ,  &
     &                      lunout )
!                no routine is written yet for polar voordinates
      endif
!           Write dummy lengthes in third direction for all layers
      if ( nolay .ne. 1 ) then
         noq3l = noq3 / (ilaggr(kmax)-1)
         dd    = 1.0
         do k = 1,ilaggr(kmax)-1
            write ( lunout ) (dd(i),i=1,2*noq3l)
         enddo
      endif
      close ( lunout )
!
      deallocate ( surf )
      deallocate ( cc )
      deallocate ( dd )
!           Modify ilaggr to facilitate determination vertical diffusion
!               at end ilaggr should contain te flow layer numbers of
!               half of the waq layers in aggregate condition
!               ilaggr(kmax) remains maximum waq layer number.
!               note that ilaggre is the same for z-layer and sigma's
      j = 1
      do k = 2, kmax
         if ( ilaggr(k) .ne. ilaggr(k-1) ) then
            if ( j .ne. 1 ) ilaggr(ilaggr(k-1)) = (k+j)/2
            j = k
         endif
      enddo
      ilaggr(ilaggr(kmax)) = kmax

      end subroutine wrwaqsrf
