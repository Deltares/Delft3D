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

 subroutine dens_eck( temp, sal, rholoc ,rhods ,rhodt )

   implicit none
   double precision, intent(in)  :: temp, sal
    double precision, intent(out) :: rholoc, rhods, rhodt
   double precision              :: cp0, clam, clam0, cp1, clam1, alph0, cp1ds, cp1dt, cladt, rhom, den
    !
   !! Data statements
    !
    alph0 = 0.698d0
    rhom  = 0.0d0
    !
    !! executable statements -------------------------------------------------------
    !
   den = temp * temp
   cp0   = 5890.0d0 + 38.00d0 * temp - 0.3750d0 * den
   clam  = 1779.5d0 + 11.25d0 * temp - 0.0745d0 * den
   clam0 =    3.8d0 +  0.01d0 * temp
   cp1   = cp0  + 3.0d0 * sal
   clam1 = clam - clam0 * sal
   rholoc   = 1000.0d0 * cp1 / ( alph0 * cp1 + clam1 ) ! - rhom

   den   = ( alph0 * cp1 + clam1)**2
   cp1ds = 3.0d0
   rhods = 1000.0d0 * ( cp1ds * clam1 + cp1 * clam0 ) / den

   cp1dt = 38.00d0 - 0.750d0 * temp
   cladt = 11.25d0 - 0.149d0 * temp - 0.01d0 * sal
   rhodt = 1000.0d0 * ( cp1dt * clam1 - cp1 * cladt ) / den

 end subroutine dens_eck
