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

!> compute the number of grid layers for a given grow factor, first grid layer height and total grid height
integer function comp_nfac(h_h0, dgrow)
   implicit none

   double precision, intent(in) :: h_h0      !< ratio of first grid layer height w.r.t. total grid height, i.e. h/h0
   double precision, intent(in) :: dgrow     !< grow factor

   if ( abs(dgrow-1d0).gt.1d-8 ) then
!      comp_nfac = floor(0.999d0+ log( (dgrow-1d0)*h_h0 + 1d0 ) / log(dgrow) )
      comp_nfac = floor(log( (dgrow-1d0)*h_h0 + 1d0 ) / log(dgrow) )
   else
      comp_nfac = floor(0.999d0+ h_h0 )
   end if

   return
end function comp_nfac
