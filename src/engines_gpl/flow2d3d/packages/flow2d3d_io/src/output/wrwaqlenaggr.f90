      subroutine wrwaqlenaggr ( nmax   , mmax   , kmax   , nlb    , nub    ,  &
     &                          mlb    , mub    , xcor   , ycor   , guu    ,  &
     &                          gvv    , gsqs   , xz     , yz     , nosegl ,  &
     &                          noq1   , noq2   , nobndl , surf   , len    ,  &
     &                          isaggr , iqaggr , ilaggr , itop   , ifrmto ,  &
     &                          lunout )
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
!
!      routine writes from- and to- lengthes with aggregation
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
      use precision
      implicit none
!
      integer(4) nmax                     !!  Dimension of first index in 2d arrays
      integer(4) mmax                     !!  Dimension of second index in 2d arrays
      integer(4) kmax                     !!  number of layers
      integer(4) nlb                      !!  Lower bound of all n dimensions
      integer(4) nub                      !!  Upper bound of all n dimensions
      integer(4) mlb                      !!  Lower bound of all m dimensions
      integer(4) mub                      !!  Upper bound of all m dimensions
      real(fp) xcor ( nlb:nub , mlb:mub ) !!  X-coordinates of depth points
      real(fp) ycor ( nlb:nub , mlb:mub ) !!  Y-coordinates of depth points
      real(fp) guu  ( nlb:nub , mlb:mub ) !!  Array with distances in dir 1
      real(fp) gvv  ( nlb:nub , mlb:mub ) !!  Array with distances in dir 2
      real(fp) gsqs ( nlb:nub , mlb:mub ) !!  Array with distances in dir 2
      real(fp) xz   ( nlb:nub , mlb:mub ) !!  X-coordinates of zeta points
      real(fp) yz   ( nlb:nub , mlb:mub ) !!  Y-coordinates of zeta points
      integer(4) nosegl                   !!  number of computational elements per layer
      integer(4) noq1                     !!  number of exchanges in 1st direction
      integer(4) noq2                     !!  number of exchanges in 2nd direction
      integer(4) nobndl                   !!  number of boundaries per layer
      real   (4) surf(-nobndl:nosegl)     !!  Horizontal surface areas
      real   (4) len( * )                 !!  Volatile work arrays
      integer(4) isaggr(nmax,mmax,kmax)   !!  horizontal grid aggregation pointer
      integer(4) iqaggr(nmax,mmax,kmax,3) !!  horizontal grid aggregation pointer
      integer(4) ilaggr(kmax)             !!  vertical layer aggregation array
      integer(4) itop                     !!  1 for sigma, kmax for z-model
      integer(4) ifrmto(4,*)              !!  from- to exchange pointer array
      integer(4) lunout                   !!  unit number to write to
!
!           Local variables
!
      integer(4) i, j, k, iq , mmm
      integer(4) noq1l, noq2l
      integer(4) istat               !!  allocate return status
      real(fp)  dx, dy
      real(fp), pointer :: midx(:), midy(:)
      real(fp), pointer :: arex(:), arey(:), sum(:)
!
!! executable statements -------------------------------------------------------
!
!           Dimensions per layer
      noq1l  = noq1  / ilaggr(kmax)
      noq2l  = noq2  / ilaggr(kmax)
      mmm = max(noq1l,noq2l)
!
                    allocate(midx(-nobndl:nosegl) , stat=istat)
      if (istat==0) allocate(midy( -nobndl:nosegl), stat=istat)
      if (istat==0) allocate(arex(0:mmm)          , stat=istat)
      if (istat==0) allocate(arey(0:mmm)          , stat=istat)
      if (istat==0) allocate(sum (0:mmm)          , stat=istat)
      if (istat/=0) then
         write(*,*) '*** ERROR: wrwaqlenaggr: memory allocation error'
         return
      endif
!                     make gravitypoints of aggregated cells
      midx = 0.0
      midy = 0.0
      do j = 1,mmax
         do i = 1,nmax
            midx(isaggr(i,j,itop)) = midx(isaggr(i,j,itop)) +            &
     &                                            gsqs(i,j) * xz(i,j)
            midy(isaggr(i,j,itop)) = midy(isaggr(i,j,itop)) +            &
     &                                            gsqs(i,j) * yz(i,j)
         enddo
      enddo
      do k = -nobndl,nosegl
         midx(k) = midx(k) / surf(k)       ! gravity x - weighed average
         midy(k) = midy(k) / surf(k)       ! gravity y - weighed average
      enddo
!                     make gravity points of exchange surfaces in first direction
      arex = 0.0
      arey = 0.0
      sum  = 0.0
      do j = 1,mmax
         do i = 2,nmax
            iq = iqaggr(i,j,itop,1)
            arex(iq) = arex(iq) + guu(i,j)*(xcor(i,j)+xcor(i-1,j))/2.0
            arey(iq) = arey(iq) + guu(i,j)*(ycor(i,j)+ycor(i-1,j))/2.0
            sum (iq) = sum (iq) + guu(i,j)
         enddo
      enddo

!           Make from- and to- lengthes in first direction
      do i = 1,noq1l
!                     make weighed gravity points
         if ( sum(i) .lt. 1.0e-20 ) sum(i) = 1.0
         arex(i) = arex(i) / sum(i)
         arey(i) = arey(i) / sum(i)
!                     determine from- to distances
         dx = midx(ifrmto(1,i))-arex(i)
         dy = midy(ifrmto(1,i))-arey(i)
         len(2*i-1) = sqrt(dx*dx + dy*dy)
         dx = midx(ifrmto(2,i))-arex(i)
         dy = midy(ifrmto(2,i))-arey(i)
         len(2*i  ) = sqrt(dx*dx + dy*dy)
      enddo

!           Write lengthes in first direction for all layers
      do k = 1,kmax
         write ( lunout ) (len(i),i=1,2*noq1l)
      enddo

!           Make gravity points of exchange surfaces in second direction
      arex = 0.0
      arey = 0.0
      sum  = 0.0
      do j = 2,mmax
         do i = 1,nmax
            iq = iqaggr(i,j,itop,2)
            if ( iq .gt. 0 ) iq = iq - noq1
            arex(iq) = arex(iq) + gvv(i,j)*(xcor(i,j)+xcor(i,j-1))/2.0
            arey(iq) = arey(iq) + gvv(i,j)*(ycor(i,j)+ycor(i,j-1))/2.0
            sum (iq) = sum (iq) + gvv(i,j)
         enddo
      enddo
!           Make from- and to- lengthes in second direction
      do i = 1,noq2l
!                     make weighed gravity points
         if ( sum(i) .lt. 1.0e-20 ) sum(i) = 1.0
         arex(i) = arex(i) / sum(i)
         arey(i) = arey(i) / sum(i)
!                     determine from- to distances
         dx = midx(ifrmto(1,i+noq1))-arex(i)
         dy = midy(ifrmto(1,i+noq1))-arey(i)
         len(2*i-1) = sqrt(dx*dx + dy*dy)
         dx = midx(ifrmto(2,i+noq1))-arex(i)
         dy = midy(ifrmto(2,i+noq1))-arey(i)
         len(2*i  ) = sqrt(dx*dx + dy*dy)
      enddo

!           Write lengthes in second direction for all layers
      do k = 1,kmax
         write ( lunout ) (len(i),i=1,2*noq2l)
      enddo
!           Free the locally allocated on-off arrays
      deallocate( midx )
      deallocate( midy )
      deallocate( arex )
      deallocate( arey )
      deallocate( sum  )
!
      end subroutine wrwaqlenaggr
