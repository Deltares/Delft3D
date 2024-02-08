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

module m_solver
   type tsolver
!     matrix
      integer                                       :: numrows               !< number of rows
      integer                                       :: numnonzeros           !< number of non-zero entries
      double precision, dimension(:),   allocatable :: a                     !< matrix entries,   dim(numnonzeros)
      integer,          dimension(:),   allocatable :: ia                    !< matrix pointer,   dim(numrows+1)
      integer,          dimension(:),   allocatable :: ja                    !< matrix row index, dim(numnonzeros)

!     right-hand side
      double precision, dimension(:),   allocatable :: rhs

!     preconditioner
      integer                                       :: numnonzerosprecond    !< number of non-zero entries in preconditioning matrix
      double precision, dimension(:),   allocatable :: alu                   !< precond. matrix entries,   dim(numnonzerosprecond)
      integer,          dimension(:),   allocatable :: ju                    !< precond. matrix pointer,   dim(numrows)
      integer,          dimension(:),   allocatable :: jlu                   !< precond. matrix row index, dim(numnonzerosprecond)

!     work array
      integer                                       :: nwork
      double precision, dimension(:),   allocatable :: work                  !< work array, dimension(nwork)     !< size of work array
      integer,          dimension(:),   allocatable :: jw                    !< nonzero indicater, dim(2*nrows)

!     settings
      integer,          dimension(16)               :: ipar
      double precision, dimension(16)               :: fpar
      double precision                              :: alpha
      integer                                       :: lfil
      double precision                              :: tol
      double precision                              :: eps
      integer                                       :: jabcgstab
   end type tsolver
end module m_solver
