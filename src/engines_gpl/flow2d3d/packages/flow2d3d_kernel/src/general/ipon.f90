subroutine ipon(xpoly     ,ypoly     ,n         ,xp        ,yp        , &
              & inout     ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2016.                                
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
!  $Id: ipon.f90 5717 2016-01-12 11:35:24Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/general/ipon.f90 $
!--description----------------------------------------------------------------
!
! Detect whether point (xp,yp) lies inside polygon (x,y) of n points.
! Point n+1 is made equal to point 1.
! inout = -1 :  Outside polygon
! inout =  0 :  On boundary of polygon
! inout =  1 :  Inside polygon
!
!--pseudo code and references-------------------------------------------------
!
! Author: J.A. Roelvink
! Date  : 22 Dec 1988
!
! Method used:
! - Draw a vertical line through (xp,yp)
! - Detect the number of crossings with the polygon below yp: nunder
! - If nunder is even, the point is outside the polygon, else inside
! - The boundary is handled seperately
!
!--declarations---------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                    , pointer :: lundia
!
! Global variables
!
    integer               , intent(out) :: inout
    integer               , intent(in)  :: n
    real(fp)              , intent(in)  :: xp
    real(fp)              , intent(in)  :: yp
    real(fp), dimension(*), intent(in)  :: xpoly
    real(fp), dimension(*), intent(in)  :: ypoly
!
! Local variables
!
    integer :: i
    integer :: istat
    integer :: nunder
    real(fp):: ysn
    real(fp):: xprev
    real(fp):: yprev
    real(fp):: xnext
    real(fp):: ynext
!
! executable statements ------------------------------------------------------
!
    lundia       => gdp%gdinout%lundia
    !
    xnext = xpoly(n) - xp
    ynext = ypoly(n) - yp
    nunder   = 0
    do i = 1, n
       xprev = xnext
       yprev = ynext
       xnext = xpoly(i) - xp
       ynext = ypoly(i) - yp
       !
       if ((xprev<0. .and. xnext>=0.).or.(xnext<0. .and. xprev>=0.)) then
          if (yprev<0. .and. ynext<0.) then
             nunder = nunder + 1
          elseif ((yprev<=0. .and. ynext>=0.) .or.                       &
                & (ynext<=0. .and. yprev>=0.)) then
             ysn = (yprev*xnext - xprev*ynext)/(xnext - xprev)
             if (ysn<0.) then
                nunder = nunder + 1
             elseif (ysn<=0.) then
                !
                ! boundary
                !
                inout = 0
                goto 100
             else
             endif
          else
          endif
       elseif (abs(xprev)<1.0E-8 .and. abs(xnext)<1.0E-8) then
          if ((yprev<=0. .and. ynext>=0.).or.(ynext<=0..and.yprev>=0.)) &
            & then
             !
             ! boundary
             !
             inout = 0
             goto 100
          endif
       else
       endif
    enddo
    if (mod(nunder, 2)==0) then
       !
       ! outside
       !
       inout = -1
    else
       !
       ! inside
       !
       inout = 1
    endif
  100 continue
end subroutine ipon
