      subroutine wrwaqnpnt ( mmax  , nmax  , kmax  , nmmax , nobnd ,     &
     &                       lgrid , ilaggr, ifrmto, isaggr, iqaggr,     &
     &                       zmodel, noseg , noq1  , noq2  , noq3  )
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
!                 Makes from to pointer with full matrix, no aggregation
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
      implicit none
!
      integer(4) nmax                      !!  dimension  first index in 2d arrays
      integer(4) mmax                      !!  dimension second index in 2d arrays
      integer(4) kmax                      !!  number of layers
      integer(4) nmmax                     !!  nmax*mmax
      integer(4) nobnd                     !!  number of open boundaries
      integer(4) lgrid ( nmax, mmax )      !!  active grid table
      integer(4) ilaggr( kmax )            !!  vertical layer aggregation array
      integer(4) ifrmto( 4, nmmax*kmax*3 ) !!  from to pointer table
      integer(4) isaggr(nmax*mmax*kmax)    !!  grid aggregation pointer
      integer(4) iqaggr( 3*nmax*mmax*kmax) !!  flow aggregation pointer
      logical    zmodel                    !!  true if z-model feature is used
      integer(4) noseg                     !!  number of segments
      integer(4) noq1                      !!  number of exchanges in 1st direction
      integer(4) noq2                      !!  number of exchanges in 2nd direction
      integer(4) noq3                      !!  number of exchanges in 3rd direction
!
      integer(4) i, j, k, n, ipiv          !!  loop variables and z-layer pivot
      integer(4) ioff, ibnd, iq, iseg      !!  help variables
      integer(4) ifrm, ito, ifrm1, itop1   !!  help variables
      integer(4) nolay                     !!  nr of layers for waq
      integer(4) nmmaxk                    !!  total number of flow cells
      integer(4) nobndl                    !!  number of open boundaries per layer
      logical    ldiff                     !!  if true, different layers
!
!! executable statements -------------------------------------------------------
!
      nolay  = ilaggr(kmax)
      nmmaxk = nmmax*   kmax
      noseg  = nmmax*  nolay
      noq1   = noseg
      noq2   = noseg
      noq3   = nmmax*( nolay - 1 )
      nobndl = nobnd
      nobnd  = nobndl*nolay
!
      n = 0
      do k = 1, nolay
         ioff = (k-1)*nmmax
         ibnd = (k-1)*nobndl
         do j = 1,mmax
            do i = 1,nmax
               ifrm  = lgrid( i, j )
               ito   = 0
               ifrm1 = 0
               itop1 = 0
               if ( i .lt. nmax   ) ito   = lgrid( i+1, j )
               if ( i .gt.   1    ) ifrm1 = lgrid( i-1, j )
               if ( i .lt. nmax-1 ) itop1 = lgrid( i+2, j )
               if ( ifrm  .gt. 0 ) ifrm  = ifrm  + ioff
               if ( ifrm  .lt. 0 ) ifrm  = ifrm  - ibnd
               if ( ito   .gt. 0 ) ito   = ito   + ioff
               if ( ito   .lt. 0 ) ito   = ito   - ibnd
               if ( ifrm1 .gt. 0 ) ifrm1 = ifrm1 + ioff
               if ( ifrm1 .lt. 0 ) ifrm1 = ifrm1 - ibnd
               if ( itop1 .gt. 0 ) itop1 = itop1 + ioff
               if ( itop1 .lt. 0 ) itop1 = itop1 - ibnd
               if ( ifrm .lt. 0 .and. ito .lt. 0 ) then
                  ifrm = 0
                  ito  = 0
               endif
               n = n + 1
               ifrmto(1,n) = ifrm
               ifrmto(2,n) = ito
               ifrmto(3,n) = ifrm1
               ifrmto(4,n) = itop1
            enddo
         enddo
      enddo
!            write the 'from' 'to' pointer table second direction
      do k = 1,nolay
         ioff = (k-1)*nmmax
         ibnd = (k-1)*nobndl
         do j = 1,mmax
            do i = 1,nmax
               ifrm  = lgrid( i, j )
               ito   = 0
               ifrm1 = 0
               itop1 = 0
               if ( j .lt. mmax   ) ito   = lgrid( i, j+1 )
               if ( j .gt.   1    ) ifrm1 = lgrid( i, j-1 )
               if ( j .lt. mmax-1 ) itop1 = lgrid( i, j+2 )
               if ( ifrm  .gt. 0 ) ifrm  = ifrm  + ioff
               if ( ifrm  .lt. 0 ) ifrm  = ifrm  - ibnd
               if ( ito   .gt. 0 ) ito   = ito   + ioff
               if ( ito   .lt. 0 ) ito   = ito   - ibnd
               if ( ifrm1 .gt. 0 ) ifrm1 = ifrm1 + ioff
               if ( ifrm1 .lt. 0 ) ifrm1 = ifrm1 - ibnd
               if ( itop1 .gt. 0 ) itop1 = itop1 + ioff
               if ( itop1 .lt. 0 ) itop1 = itop1 - ibnd
               if ( ifrm .lt. 0 .and. ito .lt. 0 ) then
                  ifrm = 0
                  ito  = 0
               endif
               n = n + 1
               ifrmto(1,n) = ifrm
               ifrmto(2,n) = ito
               ifrmto(3,n) = ifrm1
               ifrmto(4,n) = itop1
            enddo
         enddo
      enddo
!            write the 'from' 'to' pointer table third direction
      do k = 1,nolay-1
         ioff = (k-1)*nmmax
         do j = 1,mmax
            do i = 1,nmax
               ifrm  = lgrid( i, j )
               ito   = 0
               ifrm1 = 0
               itop1 = 0
               if ( ifrm .gt. 0 ) then
                  ifrm  = ifrm + ioff
                  if ( k .lt. nolay   ) ito   = ifrm + nmmax
                  if ( k .ne.    1    ) ifrm1 = ifrm - nmmax
                  if ( k .lt. nolay-1 ) itop1 = ito  + nmmax
               else
                  ifrm = 0
               endif
               n = n + 1
               ifrmto(1,n) = ifrm
               ifrmto(2,n) = ito
               ifrmto(3,n) = ifrm1
               ifrmto(4,n) = itop1
            enddo
         enddo
      enddo

!     fill segment aggregation

      iseg = 1
      do k = 1, kmax
         ipiv = ilaggr(k) - 1
         if ( zmodel ) ipiv = ilaggr(kmax-k+1) - 1
         do j = 1,mmax
            do i = 1,nmax
               n = lgrid( i, j )
               if ( n .eq. 0 ) isaggr(iseg) = 0
               if ( n .lt. 0 ) isaggr(iseg) = n - ipiv*nobndl
               if ( n .gt. 0 ) isaggr(iseg) = n + ipiv*nmmax
               iseg = iseg + 1
            enddo
         enddo
      enddo

!            fill all exchanges

      iq = 0
      do k = 1, kmax               !   k is the flow layer numbering
         if ( zmodel ) then        !   kmax-k+1 is the waq layer number for z-model
            ipiv  = ilaggr(kmax-k+1) - 1
            ioff  = nmmax          !   z-model is interface on the upper side
            if ( k .ne. kmax ) then
               ldiff = .false.
               if ( ilaggr(kmax-k) .ne. ilaggr(kmax-k+1) ) ldiff=.true.
            endif
         else                      !   k is the waq layer number for sigma-model
            ipiv  = ilaggr(k) - 1
            ioff  = 0              !   sigma-model is interface on the lower side
            if ( k .ne. kmax ) then
               ldiff = .false.
               if ( ilaggr(     k) .ne. ilaggr(k     +1) ) ldiff=.true.
            endif
         endif
         do j = 1, nmmax
            iq = iq + 1
            iqaggr( iq         ) = j + ipiv*nmmax
            iqaggr( iq+nmmaxk  ) = j + ipiv*nmmax + noseg
            if ( k .eq. kmax ) cycle      ! no vertical last layer
            if ( ldiff ) then
               iqaggr( iq+nmmaxk*2 ) = j + ipiv*nmmax-ioff + noseg*2
            else
               iqaggr( iq+nmmaxk*2 ) = 0       ! in between aggregated layers
            endif
         enddo
      enddo
!
      nobnd  = nobnd*ilaggr(kmax)
!
      end subroutine wrwaqnpnt
