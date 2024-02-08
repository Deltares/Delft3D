      subroutine wrwaqlennoag ( nmax   , mmax   , kmax   , nlb    , nub    ,  &
     &                          mlb    , mub    , guv    , gvu    , noq1   ,  &
     &                          noq2   , len    , iqaggr , ilaggr , itop   ,  &
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
!      routine writes from- and to- lengthes without aggregation
!              the routine uses the aggregation pointer to support
!              compaction to active elements and exchanges only
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
      real(fp) guv  ( nlb:nub , mlb:mub ) !!  Array with distances in dir 1
      real(fp) gvu  ( nlb:nub , mlb:mub ) !!  Array with distances in dir 2
      integer(4) noq1                     !!  number of exchanges in 1st direction
      integer(4) noq2                     !!  number of exchanges in 2nd direction
      real   (4) len(-1:nmax*mmax*2)      !!  Volatile work arrays
      integer(4) iqaggr(nmax,mmax,kmax,3) !!  horizontal grid aggregation pointer
      integer(4) ilaggr(kmax)             !!  vertical layer aggregation array
      integer(4) itop                     !!  1 for sigma, kmax for z-model
      integer(4) lunout                   !!  unit number to write to
!
!           Local variables
!
      integer(4) i, j, k, iq
      integer(4) noq1l, noq2l
!
!! executable statements -------------------------------------------------------
!
!           Dimensions per layer
      noq1l = noq1 / ilaggr(kmax)
      noq2l = noq2 / ilaggr(kmax)

!           First layer lengthes in first direction
      len = 0.0
      do j = 1,mmax
         do i = 1,nmax-1
            iq = 2*iqaggr(i,j,itop,1)
            if ( iq .ne. 0 ) then
               len(iq-1) = guv(i  ,j)/2.0
               len(iq  ) = guv(i+1,j)/2.0
            endif
         enddo
      enddo

!           Write lengthes in first direction for all layers
      do k = 1,ilaggr(kmax)
         write ( lunout ) (len(i),i=1,2*noq1l)
      enddo

!           First layer lengthes in second direction
      len = 0.0
      do j = 1,mmax-1
         do i = 1,nmax
            iq = 2*iqaggr(i,j,itop,2)
            if ( iq .gt. 0 ) then
               iq = iq-2*noq1
               len(iq-1) = gvu(i,j  )/2.0
               len(iq  ) = gvu(i,j+1)/2.0
            endif
         enddo
      enddo

!           Write lengthes in second direction for all layers
      do k = 1,ilaggr(kmax)
         write ( lunout ) (len(i),i=1,2*noq2l)
      enddo
!
      end subroutine wrwaqlennoag
