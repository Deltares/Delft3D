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

module m_advec
   use m_solver

   integer                                     :: jaoutput   = 1 !< output matrices to file (1) or not (0)

   type(tsolver)                               :: solver_advec

   integer,          dimension(:), allocatable :: iI, jI
   double precision, dimension(:), allocatable :: aI        !< interpolation and projection matrix in CRS format

   integer,          dimension(:), allocatable :: iR, jR
   double precision, dimension(:), allocatable :: aR        !< reconstruction matrix in CRS format

   integer,          dimension(:), allocatable :: iC, jC
   double precision, dimension(:), allocatable :: aC        !< collocated discretization matrix in CRS format

   integer,          dimension(:), allocatable :: iW, jW
   double precision, dimension(:), allocatable :: aW        !< work matrix in RCS format
   integer,          dimension(:), allocatable :: iwork

   double precision, dimension(:,:), allocatable :: dfluxfac   !< flux(L) = dfluxfac(1,L) * something(ln(1,L)) + dfluxfac(2,L) * something(ln(2,L)), positive from ln(1,L) to ln(2,L)
end module
