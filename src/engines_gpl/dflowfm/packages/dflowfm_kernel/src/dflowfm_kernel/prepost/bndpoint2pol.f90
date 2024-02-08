!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! 
! 

 subroutine bndpoint2pol(m,n)
 use m_polygon
 use m_grid
 implicit none
 integer :: m, n
 double precision :: xce, yce, xbb, ybb
 integer :: mu,nu,md,nd

 if (ijyes(m,n) == 0) then

    mu = m + 1 ; nu = n + 1 ; md = m - 1 ; nd = n - 1

    if (m <= mc .and. n > 1 .and. n < nc+1) then
       if (ijyes(mu,n)==1) then     ! linkerrand
          npl      = npl + 1
          xce      = 0.25d0*( xc(mu,n) + xc(mu,nd) + xc(m,n) + xc(m,nd) )
          xbb      = 0.50d0*( xc(m,n)  + xc(m,nd ) )
          xpl(npl) = 1.1d0*xbb - 0.1d0*xce
          yce      = 0.25d0*( yc(mu,n) + yc(mu,nd) + yc(m,n) + yc(m,nd) )
          ybb      = 0.50d0*( yc(m,n)  + yc(m,nd ) )
          ypl(npl) = 1.1d0*ybb - 0.1d0*yce
       endif
    endif

    if (n <= nc .and. m > 1 .and. m < mc+1) then
       if (ijyes(m,nu)==1) then     ! onderrand
          npl      = npl + 1
          xce      = 0.25d0*( xc(m,nu) + xc(md,nu ) + xc(m,n) + xc(md,n) )
          xbb      = 0.50d0*( xc(m,n)  + xc(md,n) )
          xpl(npl) = 1.1d0*xbb - 0.1d0*xce
          yce      = 0.25d0*( yc(m,nu) + yc(md,nu ) + yc(m,n) + yc(md,n) )
          ybb      = 0.50d0*( yc(m,n)  + yc(md,n) )
          ypl(npl) = 1.1d0*ybb - 0.1d0*yce
       endif
    endif

    if (m >= 2 .and. n > 1 .and. n < nc+1  ) then
       if (ijyes(md,n)==1) then    ! rechterrand
          npl      = npl + 1
          xce      = 0.25d0*( xc(md-1,nd) + xc(md-1,n) + xc(m-1,nd) + xc(m-1,n) )
          xbb      = 0.50d0*( xc(m-1,n)   + xc(m-1,nd) )
          xpl(npl) = 1.1d0*xbb - 0.1d0*xce
          yce      = 0.25d0*( yc(md-1,nd) + yc(md-1,n) + yc(m-1,nd) + yc(m-1,n) )
          ybb      = 0.50d0*( yc(m-1,n)   + yc(m-1,nd) )
          ypl(npl) = 1.1d0*ybb - 0.1d0*yce
       endif
    endif

    if (n >= 2 .and. m > 1 .and. m < mc+1  ) then
       if (ijyes(m,nd)==1) then    ! bovenrand
          npl      = npl + 1
          xce      = 0.25d0*( xc(md,nd-1) + xc(md,n-1) + xc(m,nd-1) + xc(m,n-1) )
          xbb      = 0.50d0*( xc(m,n-1)   + xc(md,n-1) )
          xpl(npl) = 1.1d0*xbb - 0.1d0*xce
          yce      = 0.25d0*( yc(md,nd-1) + yc(md,n-1) + yc(m,nd-1) + yc(m,n-1) )
          ybb      = 0.50d0*( yc(m,n-1)   + yc(md,n-1) )
          ypl(npl) = 1.1d0*ybb - 0.1d0*yce
       endif
    endif

 endif

 end subroutine bndpoint2pol
