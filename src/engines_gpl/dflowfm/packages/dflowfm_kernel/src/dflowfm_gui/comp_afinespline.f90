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

!> compute the spline to fine-spline matrix A, such that
!>   xf = A x, and
!>   yf = A y,
!>   where x and y are the spline control-point coordinates and
!>   xf and yf are the sample point coordinates
subroutine comp_Afinespline(N, numref, Nr, A, ierror)
   implicit none

   integer,                                    intent(in)    :: N        !< number of spline control points
   integer,                                    intent(in)    :: numref   !< number of additional points between spline control points
   integer,                                    intent(inout) :: Nr       !< array size (in), number of sample points (out)
   double precision, dimension(Nr,N),          intent(out)   :: A        !< spline to fine-spline matrices
   integer,                                    intent(out)   :: ierror   !< no error (0), memory error (2) or other error (1)

   integer                                                   :: j, Nr_in

   double precision, dimension(:), allocatable               :: xloc, yloc, xf, yf

   ierror = 1

   Nr_in = Nr

   if ( N.lt.1 ) goto 1234

!  compute the number of samples
   Nr = N + (N-1)*numref

!  check array size
   if ( Nr_in.lt.Nr ) then
      ierror = 2
      goto 1234
   end if

!  allocate
   allocate(xloc(N), yloc(N), xf(Nr), yf(Nr))

!  compose the matrix
!    note: although the y-coordinate spline is refined, it is not used
   xloc = 0d0
   yloc = 0d0
   do j=1,N
      xloc(j) = 1d0
      call sample_spline(N, xloc, yloc, numref, Nr, xf, yf, ierror)
      if ( ierror.ne.0 ) goto 1234
      A(1:Nr, j) = xf
      xloc(j) = 0d0
   end do

   ierror = 0
 1234 continue

!  deallocate
   if ( allocated(xloc) ) deallocate(xloc)
   if ( allocated(yloc) ) deallocate(yloc)
   if ( allocated(xf)   ) deallocate(xf)
   if ( allocated(yf)   ) deallocate(yf)
   return
end subroutine comp_Afinespline
