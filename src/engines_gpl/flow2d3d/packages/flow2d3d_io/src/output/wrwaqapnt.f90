      subroutine wrwaqapnt ( mmax  , nmax  , kmax  , mnmax , lgrid ,     &
     &                       iapnt , ilaggr, noseg , noq1  , noq2  ,     &
     &                       noq3  , nobnd , isaggr, iqaggr, ifrmto,     &
     &                       aggre , zmodel)
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
! Writes the time varying data for waves (6 & 7)
! to the NEFIS HIS-DAT file.
! Output is performed conform the times of history
! file and only in case wave.eq.TRUE.
!
!
!     Deltares           SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     Created : Sept. 1996 by Jan van Beek as koppnt.f for coupling
!                                             with SIMONA through DIDO
!
!     Function            : Makes pointers in grid
!
!     Subroutines called  : -
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
!
!     Declaration of arguments:
!
      implicit none
!
!     KIND    NAME   LENGTH           FUNCT.  DESCRIPTION
!     -----   ----   ------           ------- -----------
      integer mmax                  ! INPUT   X,U direction, second in LGRID
      integer nmax                  ! INPUT   Y,V direction, first in LGRID
      integer kmax                  ! INPUT   Number of FLOW layers
      integer mnmax                 ! INPUT   Number of FLOW cells for one layer
      integer lgrid (nmax,mmax)     ! INPUT   Grid table
      integer iapnt (0:mnmax  )     ! INPUT   Pointer to active segments
      integer ilaggr(kmax)          ! INPUT   Layer aggregation pointer
      integer noseg                 ! OUTPUT  Number of DELWAQ cells aggregated
      integer noq1                  ! OUTPUT  Number of N-exchanges
      integer noq2                  ! OUTPUT  Number of M-exchanges
      integer noq3                  ! OUTPUT  Number of K-exchanges
      integer nobnd                 ! OUTPUT  Number of boundaries
      integer isaggr(  mnmax*kmax)  ! grid aggregation pointer
      integer iqaggr(3*mnmax*kmax)  ! flow aggregation pointer
      integer ifrmto(4,mnmax*kmax*3)! OUTPUT  Delwaq pointers
      integer aggre                 ! INPUT   0 means no-aggregation active cells only
                                    !         1 means aggregation
      logical zmodel                ! true if z-model feature is used
!
!     Local declaration
!
      integer nosegl              ! largest segment number of top layer
      integer nobndl              ! largest boundary number of top layer
      integer noq1l, noq2l        ! number of waq exchanges per layer
      integer notot               ! total number of hydrodynamic grid cells
      integer nolay               ! number of active layers in the result
      integer ip, ip0,ip1,ip2,ip3,ip4 ! help variables
      integer i, k, iq            ! loop variables
      integer n, m, ipiv, iloff   ! help variables
!
!! executable statements -------------------------------------------------------
!
!                                   compute highest active segment and open boundary
      nosegl = noseg
      nobndl = nobnd
      notot  = mnmax*kmax
!                                    modification of Jans routine, requires ilaggr(1) = 1
      nolay  = ilaggr( kmax )      ! and kmax = 1 for one layer
      noseg  = nolay*nosegl
      nobnd  = nolay*nobndl
!                                   modification of Jans routine, similar code in subroutine
!         fill the active exchanges in N-direction

      call wrwaqmakpnt ( mnmax  , nosegl , nobndl , nolay  , 1      ,    &
     &                   lgrid  , iapnt  , aggre  , noq1   , iqaggr ,    &
     &                   ifrmto )

!         follow the same procedure in M-direction

      i = notot + 1
      call wrwaqmakpnt ( mnmax  , nosegl , nobndl , nolay  , nmax   ,    &
     &                   lgrid  , iapnt  , aggre  , noq2   , iqaggr(i),  &
     &                   ifrmto(1,noq1+1) )

!     The third direction

      noq3 = (nolay-1)*nosegl
      do k = 1 , nolay-1
         ip0 = (k-1)*nosegl + noq1 + noq2
         ip1 = (k-1)*nosegl
         ip2 = (k  )*nosegl
         ip3 = (k-2)*nosegl
         ip4 = (k+1)*nosegl
         do iq = 1 , nosegl
            ip = iq + ip0
            ifrmto( 1, ip ) = iq + ip1
            ifrmto( 2, ip ) = iq + ip2
            ifrmto( 3, ip ) = MAX( 0, iq+ip3 )
            ifrmto( 4, ip ) = 0
            if ( k .ne. nolay-1 ) ifrmto( 4, ip ) = iq + ip4
         enddo
      enddo

!     fill segment aggregation and vertical exchanges

      m = 1
      do k = 1, kmax
         ipiv = ilaggr(k) - 1
         if ( zmodel ) ipiv = ilaggr(kmax-k+1) - 1
         do i = 1,mnmax
            n = iapnt( i )
            if ( n .eq. 0 ) isaggr(m) = 0
            if ( n .lt. 0 ) isaggr(m) = n - ipiv*nobndl
            if ( n .gt. 0 ) then
               isaggr(m) = n + ipiv*nosegl
               if ( k .ne. kmax ) then
                  if ( zmodel ) then
                     if ( ilaggr(kmax-k+1) .ne. ilaggr(kmax-k) )         &
     &                  iqaggr( m + notot*2 ) =                          &
     &                               noq1 + noq2 + n + (ipiv-1)*nosegl
                  else
                     if ( ilaggr(k+1     ) .ne. ilaggr(k     ) )         &
     &                  iqaggr( m + notot*2 ) =                          &
     &                               noq1 + noq2 + n +  ipiv   *nosegl
                  endif
               endif
            endif
            m = m + 1
         enddo
      enddo

!            fill all horizontal exchanges

      noq1l  = noq1 / nolay
      noq2l  = noq2 / nolay
      do k = kmax, 1, -1
         ipiv  = ilaggr(k) - 1
         if ( zmodel ) ipiv = ilaggr(kmax-k+1) - 1
         iloff = (k-1)*mnmax
         do i = 1, mnmax
            if ( iqaggr(i        ) .ne. 0 )                              &
     &         iqaggr( iloff + i        ) = iqaggr( i        )           &
     &                                         + ipiv*noq1l
            if ( iqaggr(i + notot) .ne. 0 )                              &
               iqaggr( iloff + i + notot) = iqaggr( i + notot)           &
     &                                         + ipiv*noq2l + noq1
         enddo
      enddo
!
      return
      end subroutine wrwaqapnt
