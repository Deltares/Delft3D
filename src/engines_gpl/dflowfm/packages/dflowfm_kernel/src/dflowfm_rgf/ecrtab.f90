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

      SUBROUTINE ECRTAB(X,MC,NC,MRGF,HALF,mmax,nmax)
      implicit none
      double precision :: af
      double precision :: half
      integer :: i
      integer :: i4
      integer :: j
      integer :: mc
      integer :: mmax
      integer :: mrgf
      integer :: nc
      integer :: nmax
!     SCHRIJFROUTINE RGF-FORMAT
      double precision :: X(MMAX,NMAX)
      do J=1,NC
         AF = HALF + 0.5d0*dble(J)/dble(NC)
         CALL READYY(' ',AF)
         WRITE(MRGF,888) J,(X(I,J),I=1,MC)
      end do
  888 FORMAT(' ETA= ',I4,5ES26.18:/(10X,5ES26.18))
      RETURN
      END SUBROUTINE ECRTAB
