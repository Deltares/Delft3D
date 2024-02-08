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

   double precision function dlimitercentral(dc,d2,limtyp)  ! as dlimiter, now for central gradient instead of slope
   implicit none

   double precision, intent(in) :: dc, d2   !< central and right slopes
   integer         , intent(in) :: limtyp   !< first order upwind (0) or MC (>0)

   double precision             :: r, d1
   double precision, parameter  :: dtol=1d-16

   dlimitercentral = 0d0
   if (limtyp == 0)     return
!   if ( d1*d2.lt.dtol ) return
!
!   r = d1/d2    ! d1/d2
!   r = 2d0*r - 1d0

!  compute left slope (assume uniform mesh)
   d1 = 2d0*dc - d2

   if ( d1*d2.lt.dtol ) return

   r = d1/d2    ! d1/d2

   dlimitercentral = d2 * max(0d0, min(2d0*r,0.5d0*(1d0+r),2d0) ) !  Monotonized Central
end function dlimitercentral
