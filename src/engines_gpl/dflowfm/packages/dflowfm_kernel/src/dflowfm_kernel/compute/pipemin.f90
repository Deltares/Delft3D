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

subroutine pipemin(hpr, dia, area, width) ! top minus part
use m_sferic
use m_flow, only : slotw1D
implicit none
double precision, intent(in)   :: dia, hpr
double precision, intent(out)  :: area, width

double precision               :: are, dacos, dsqrt, fi, r, sq

r = 0.5*dia
if (hpr< r) then
   area  = 0d0
   width = 0d0
else if (hpr < dia) then
   are   = hpr - r
   fi    = dasin(are/r)
   sq    = dsqrt(hpr*(dia - hpr))
   area  = are*dia - fi*r*r - sq*are
   width = dia - 2*sq
else
   area  = (hpr-r)*dia - 0.5d0*pi*r*r
   width = dia
endif
end subroutine pipemin
