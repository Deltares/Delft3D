subroutine adjlin ( ival, outside, mmax, nmax, onParbndIsInside )
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
!   Checks how a piece of line of dry points, thin dams, e.g., is located
!   w.r.t. subdomain, and if partly inside, adjust that piece of line to
!   fit within subdomain
!
!!--pseudo code and references--------------------------------------------------
!
!   first, check whether piece of line is completely inside subdomain
!   if not, next check whether it is completely outside subdomain
!      if so, then do nothing
!      otherwise, piece of line is partly inside subdomain
!         this piece of line needs to be cut off to fit within subdomain
!         three types of lines are distinguished:
!
!            line is horizontal
!            or line is vertical
!            or angle of line and x-axis is 45, 135, 225 or 315 degrees
!
!              if angle is 45 or 225 degrees (i.e. if m1<m2 then n1<n2)
!
!                 case 1: start point is left, end point is right
!
!                   if start point is outside subdomain then move it to boundary
!                   else if end point is outside subdomain then move it to boundary
!
!                 case 2: start point is right, end point is left
!
!                   if end point is outside subdomain then move it to boundary
!                   else if start point is outside subdomain then move it to boundary
!
!              if angle is 135 or 315 degrees (i.e. if m1<m2 then n1>n2)
!
!                 case 1: start point is left, end point is right
!
!                   if start point is outside subdomain then move it to boundary
!                   else if end point is outside subdomain then move it to boundary
!
!                 case 2: start point is right, end point is left
!
!                   if end point is outside subdomain then move it to boundary
!                   else if start point is outside subdomain then move it to boundary
!
!   re-set the indices of piece of line and its location w.r.t. subdomain
!
!
!!--declarations----------------------------------------------------------------
    use dfparall
    implicit none
!
! Global variables
!
    integer, dimension(4), intent(inout) :: ival    ! array containing indices of start and end points
    integer, intent(in)                  :: mmax    ! number of gridpoints in the x-direction
    integer, intent(in)                  :: nmax    ! number of gridpoints in the y-direction
    logical, intent(out)                 :: outside ! true if piece of line is fully outside subdomain
                                                    ! false if piece of line is partly or fully inside subdomain
    logical, intent(in)                  :: onParbndIsInside ! true : structure entirely on (internal) partition boundary is marked as being inside
                                                             !        (default)
                                                             ! false: structure entirely on (internal) partition boundary is marked as being outside
                                                             !        needed for openboundaries
!
! Local variables
!
    integer :: m1     ! m-index of start point of piece of line
    integer :: m2     ! m-index of end point of piece of line
    integer :: n1     ! n-index of start point of piece of line
    integer :: n2     ! n-index of end point of piece of line
    integer :: sm     ! =sign(m1-m2), i.e. positive if m1 > m2 otherwise m1 < m2
    integer :: sn     ! =sign(n1-n2), i.e. positive if n1 > n2 otherwise n1 < n2
    logical :: outsdf ! true if piece of line is fully outside subdomain
                      ! false if piece of line is partly or fully inside subdomain
    logical :: outsdp ! true if piece of line is partly outside subdomain
                      ! false if piece of line is fully outside subdomain
!
!! executable statements -------------------------------------------------------
!
    m1 = ival(1)
    n1 = ival(2)
    m2 = ival(3)
    n2 = ival(4)
    !
    ! first, check whether piece of line is completely inside subdomain
    !
    outsdf = min(m1,m2) < 1 .or. min(n1,n2) < 1 .or. max(m1,m2) > mmax .or. max(n1,n2) > nmax
    outsdp = outsdf
    !
    ! if not, next check whether it is completely outside subdomain
    !
    if ( outsdf ) then
       !
       outsdp = min(m1,m2) <= mmax .and. min(n1,n2) <= nmax .and. max(m1,m2) >= 1 .and. max(n1,n2) >= 1
       !
       ! if so, then do nothing
       ! otherwise, piece of line is partly inside subdomain
       !
       if ( outsdp ) then
          !
          ! this piece of line needs to be cut off to fit within subdomain
          !
          outsdf = .false.
          !
          ! three types of lines are distinguished:
          !
          if ( m1==m2 .or. n1==n2 ) then
             !
             ! line is horizontal
             !
             if ( m1 < m2 ) then
                !
                ! start point is left, end point is right
                !
                m1 = max(   1,m1)
                m2 = min(mmax,m2)
             else
                !
                ! start point is right, end point is left
                !
                m1 = min(mmax,m1)
                m2 = max(   1,m2)
             endif
             !
             ! or line is vertical
             !
             if ( n1 < n2 ) then
                !
                ! start point is down, end point is up
                !
                n1 = max(   1,n1)
                n2 = min(nmax,n2)
             else
                !
                ! start point is up, end point is down
                !
                n1 = min(nmax,n1)
                n2 = max(   1,n2)
             endif
          else
             !
             ! or angle of line and x-axis is 45, 135, 225 or 315 degrees
             !
             sm = sign(1,m1-m2)
             sn = sign(1,n1-n2)
             !
             if ( sm==sn ) then
                !
                ! angle is 45 or 225 degrees (i.e. if m1<m2 then n1<n2)
                !
                if ( sm < 0 ) then
                   !
                   ! start point is left, end point is right
                   !
                   ! if start point is outside subdomain then move it to boundary
                   !
                   if ( m1 < 1 .or. n1 < 1 ) then
                      !
                      do
                        m1 = m1 +1
                        n1 = n1 +1
                        if ( m1>=1 .and. n1>=1 ) exit
                      enddo
                   !
                   ! else if end point is outside subdomain then move it to boundary
                   !
                   else if ( m2 > mmax .or. n2 > nmax ) then
                      do
                        m2 = m2 -1
                        n2 = n2 -1
                        if ( m2<=mmax .and. n2<=nmax ) exit
                      enddo
                   endif
                else
                   !
                   ! start point is right, end point is left
                   !
                   ! if end point is outside subdomain then move it to boundary
                   !
                   if ( m2 < 1 .or. n2 < 1 ) then
                      do
                        m2 = m2 +1
                        n2 = n2 +1
                        if ( m2>=1 .and. n2>=1 ) exit
                      enddo
                   !
                   ! else if start point is outside subdomain then move it to boundary
                   !
                   else if ( m1 > mmax .or. n1 > nmax ) then
                      do
                        m1 = m1 -1
                        n1 = n1 -1
                        if ( m1<=mmax .and. n1<=nmax ) exit
                      enddo
                   endif
                endif
             else
                !
                ! angle is 135 or 315 degrees (i.e. if m1<m2 then n1>n2)
                !
                if ( sm < 0 ) then
                   !
                   ! start point is left, end point is right
                   !
                   ! if start point is outside subdomain then move it to boundary
                   !
                   if ( m1 < 1 .or. n1 > nmax ) then
                      do
                        m1 = m1 +1
                        n1 = n1 -1
                        if ( m1>=1 .and. n1<=nmax ) exit
                      enddo
                   !
                   ! else if end point is outside subdomain then move it to boundary
                   !
                   else if ( m2 > mmax .or. n2 < 1 ) then
                      do
                        m2 = m2 -1
                        n2 = n2 +1
                        if ( m2<=mmax .and. n2>=1 ) exit
                      enddo
                   endif
                else
                   !
                   ! start point is right, end point is left
                   !
                   ! if end point is outside subdomain then move it to boundary
                   !
                   if ( m2 < 1 .or. n2 > nmax ) then
                      do
                        m2 = m2 +1
                        n2 = n2 -1
                        if ( m2>=1 .and. n2<=nmax ) exit
                      enddo
                   !
                   ! else if start point is outside subdomain then move it to boundary
                   !
                   else if ( m1 > mmax .or. n1 < 1 ) then
                      do
                        m1 = m1 -1
                        n1 = n1 +1
                        if ( m1<=mmax .and. n1>=1 ) exit
                      enddo
                   endif
                endif
             endif
          endif
       endif
    endif
    !
    ! Default: onParbndIsInside = true:
    !          When this item is exactly on the partition boundary, it is
    !          marked as being inside
    ! Open boundaries: onParbndIsInside = false:
    !          An open boundary exactly on the partition boundary will go
    !          wrong when the active cells (kcs=1) adjecent to the open boundary
    !          are in another partition. In case the active cells are in the
    !          current partition, it will not harm to remove the open boundary, because
    !          it's located in the halo of this partition: data will be copied from the neighbouring partition.
    if (.not.onParbndIsInside) then
       if (inode > 1) then
          ! Check left/lower partion boundaries
          if (idir == 1) then
             ! n=1 is a partition boundary
             if (n1==n2 .and. n1<2) then
                outsdf = .true.
             endif
          else if (idir == 2) then
             ! m=1 is a partition boundary
             if (m1==m2 .and. m1<2) then
                outsdf = .true.
             endif
          endif
       endif
       if (inode < nproc) then
          ! Check right/upper partion boundaries
          if (idir == 1) then
             ! n=nmax is a partition boundary
             if (n1==n2 .and. n1>nmax-1) then
                outsdf = .true.
             endif
          else if (idir == 2) then
             ! m=mmax is a partition boundary
             if (m1==m2 .and. m1>mmax-1) then
                outsdf = .true.
             endif
          endif
       endif
    endif
    !
    ! re-set the indices of piece of line and its location w.r.t. subdomain
    !
    ival(1) = m1
    ival(2) = n1
    ival(3) = m2
    ival(4) = n2
    outside = outsdf
    !
end subroutine adjlin
