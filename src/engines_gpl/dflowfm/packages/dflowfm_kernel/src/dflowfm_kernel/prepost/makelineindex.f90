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

!>  indentify the points in an array
    subroutine makelineindex(num, x, idx)
      use m_missing
      use geometry_module, only: get_startend

      implicit none

      integer,                          intent(inout) :: num !< array size
      double precision, dimension(num), intent(in)    :: x   !< line array
      integer,          dimension(num), intent(out)   :: idx !< idx array

      integer                                         :: nidx, ipoint
      integer                                         :: jstart, jend
      integer                                         :: i


!     default
      idx = 0

!     check for DMISS-only
dolp: do
         do i=1,num
           if ( x(i).ne.DMISS ) exit dolp
         end do

         num = 0
         return
      end do dolp

!     initialize pointer
      ipoint = 1
!     initialize counter
      nidx = 0

!     loop over the sections
      do while ( ipoint.le.num )
!        increase counter
         nidx     = nidx+1

!        get start and end array postions of this section
         call get_startend(num-ipoint+1, x(ipoint:num), x(ipoint:num), jstart, jend, dmiss)
         jstart = ipoint+jstart-1
         jend   = ipoint+jend-1

!        fill index array
         do i=jstart,jend
            if ( x(i).ne.DMISS ) idx(i) = nidx
         end do

!        shift pointer
         ipoint = jend + 1
      end do

      return
    end subroutine
