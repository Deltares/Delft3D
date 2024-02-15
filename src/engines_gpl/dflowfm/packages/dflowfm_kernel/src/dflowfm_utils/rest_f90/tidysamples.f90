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

      SUBROUTINE TIDYSAMPLES(XS,YS,ZS,IPSAM,NS,MXSAM,MYSAM)
      use stdlib_sorting, only: sort_index
      implicit none
      integer :: ns
      double precision :: XS(NS), YS(NS), ZS(NS)   !< sample coordinates
      integer, dimension(NS), intent(out) :: IPSAM !< permutation array (increasing x-coordinate)
      integer,                intent(in)  :: MXSAM, MYSAM   !< structured sample data dimensions (>0) or unstructured (0)
      double precision, allocatable :: xs_copy(:)
!      IF (NS .GT. 1) CALL RSORT3(XS,YS,ZS,NS)

      xs_copy = xs
      call sort_index(xs_copy, IPSAM)

!     remove double/missing samples (non-structured sample data only)
      if ( MXSAM*MYSAM.ne.NS ) then
         CALL READYY(' ',0.3d0)
         IF (NS .GT. 1) CALL RMDOUBLE(XS,YS,ZS,IPSAM,NS)
      end if

      CALL READYY(' ',1d0)

      RETURN
      END
