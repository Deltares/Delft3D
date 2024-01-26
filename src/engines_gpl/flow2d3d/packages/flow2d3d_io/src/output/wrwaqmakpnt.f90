      subroutine wrwaqmakpnt ( mnmax  , nosegl , nobndl , nlay   , inc    ,   &
     &                         lgrid  , ipnt   , aggre  , noq    , ipnn   ,   &
     &                         ipoint )
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
!        subroutine makes an aggregated pointer
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
      implicit none
!
!     KIND    NAME     LENGTH            FUNCT.  DESCRIPTION
!     -----   ----     ------            ------- -----------
      integer mnmax                    ! INPUT   total linear dimension of LGRID
      integer nosegl                   ! INPUT   number of segments per layer
      integer nobndl                   ! INPUT   number of boundaries per layer
      integer nlay                     ! INPUT   number of layers
      integer inc                      ! INPUT   either 1 or nmax direction dependent
      integer lgrid   (mnmax)          ! INPUT   the grid in 1D representation
      integer ipnt    (0:mnmax)        ! INPUT   the aggregation pointer
      integer aggre                    ! INPUT   1 if aggregation horizontally
      integer noq                      ! OUTPUT  total number of exchanges
      integer ipnn    (mnmax)          ! OUTPUT  pointer from grid to exchange
      integer ipoint  (4,mnmax*nlay)   ! OUTPUT  exchange pointer
!
!     Local declaration
!
      integer i, iq, in2          ! loop counters
      integer ip,ip0,ip1,ip2,ip3  ! help variables
      integer ip4, noql           ! help variables
      integer isoff, iqoff, iboff ! help variables
!
!! executable statements -------------------------------------------------------
!
      noq  = 0
      ipnn = 0
      do i = 1, mnmax-inc
         ip0 = lgrid( i   )
         ip1 = ipnt ( ip0 )
         if ( ip1 .eq. 0 ) cycle
         ip2 = ipnt( lgrid( i+inc ) )
         if ( ip2 .eq. 0 .or.  ip1 .eq. ip2 ) cycle
         if ( ip1 .lt. 0 .and. ip2 .lt.   0 ) cycle
!               Look down till we find a segment with another number for the -1 pointer
         ip3 = 0
         do in2 = i-inc , 1 , -inc
            ip = ipnt( lgrid( in2 ) )
            if ( ip .eq. ip1 ) cycle
            if ( ip1 .lt. 0 .and. ip .lt. 0 ) then
               ip3 = 0
            else
               ip3 = ip
            endif
            exit
         enddo
!               Look up till we find a segment with another number for the +1 pointer
         ip4 = 0
         do in2 = i+2*inc , mnmax, inc
            ip = ipnt( lgrid( in2 ) )
            if ( ip .eq. ip2 ) cycle
            if ( ip2 .lt. 0 .and. ip .lt. 0 ) then
               ip4 = 0
            else
               ip4 = ip
            endif
            exit
         enddo
!               Exchange unique ?
         ip = 0
         if ( aggre .eq. 1 ) then
            do iq = 1 , noq
               if ( ipoint(1,iq) .eq. ip1 .and.                          &
     &              ipoint(2,iq) .eq. ip2       ) then
                  ip =  iq
                  exit
               endif
               if ( ipoint(1,iq) .eq. ip2 .and.                          &
     &              ipoint(2,iq) .eq. ip1       ) then
                  ip = -iq
                  exit
               endif
            enddo
         endif
         if ( ip  .eq. 0 ) then
            noq = noq + 1
            ipoint(1,noq) = ip1
            ipoint(2,noq) = ip2
            ipoint(3,noq) = ip3
            ipoint(4,noq) = ip4
            ipnn  ( ip0 ) = noq
         else
            ipnn  ( ip0  ) = ip
         endif
!
      enddo
      noql  = noq
!                  Rest of the layers
      noq   = nlay * noql
      do i = 2 , nlay
         isoff = (i-1)*nosegl
         iqoff = (i-1)*noql
         iboff = (i-1)*nobndl
         do iq = 1 , noql
            ip  = iq + iqoff
            ip1 = ipoint( 1, iq )
            if ( ip1 .gt. 0 ) ipoint( 1, ip ) = ip1 + isoff
            if ( ip1 .lt. 0 ) ipoint( 1, ip ) = ip1 - iboff
            ip2 = ipoint( 2, iq )
            if ( ip2 .gt. 0 ) ipoint( 2, ip ) = ip2 + isoff
            if ( ip2 .lt. 0 ) ipoint( 2, ip ) = ip2 - iboff
            ip3 = ipoint( 3, iq )
            if ( ip3 .gt. 0 ) ipoint( 3, ip ) = ip3 + isoff
            if ( ip3 .lt. 0 ) ipoint( 3, ip ) = ip3 - iboff
            IF ( ip3 .eq. 0 ) ipoint( 3, ip ) = 0
            ip4 = ipoint( 4, iq )
            if ( ip4 .gt. 0 ) ipoint( 4, ip ) = ip4 + isoff
            if ( ip4 .lt. 0 ) ipoint( 4, ip ) = ip4 - iboff
            IF ( ip4 .eq. 0 ) ipoint( 4, ip ) = 0
         enddo
      enddo
!
      return
      end subroutine wrwaqmakpnt
