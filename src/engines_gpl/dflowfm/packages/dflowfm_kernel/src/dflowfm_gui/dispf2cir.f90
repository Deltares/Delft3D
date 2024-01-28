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

      SUBROUTINE DISPF2cir(X,Y,N,Rcx,Rcy,NCOL)
      implicit none
      integer :: i
      integer :: n
      integer :: ncol
      integer :: nmax
!     LAAT EENDIMENSIONALE FUNCTIE ZIEN met cirkels
      double precision :: X(N), Y(N), rcx, rcy
      CALL SETCOL(NCOL)
      CALL MOVABS(X(1),Y(1))
      DO I = 2,N
         CALL LNABS(X(I),Y(I))
      ENDDO
      CALL MOVABS(X(1),Y(1))
      if (rcx > 0) CALL fbox( x(1)-rcx,y(1)-rcy,x(1)+rcx,y(1)+rcy )     ! CIR(RCIR)
      DO I = 2,N
         if (rcx > 0) CALL fbox( x(i)-rcx,y(i)-rcy,x(i)+rcx,y(i)+rcy )  ! CIR(RCIR)
      ENDDO

      RETURN
      END
