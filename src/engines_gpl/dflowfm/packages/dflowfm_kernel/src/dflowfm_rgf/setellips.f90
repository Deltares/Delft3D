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

!
      SUBROUTINE SETELLIPS(IELL)
      implicit none
      integer :: iell
      COMMON /ELLIPS/ A,E
      double precision :: A,E

      A = 6378137d0
      E = 0.081819d0

      IF (IELL .EQ. 1) THEN      ! Hayford
         A = 6378388d0
         E = 0.081992d0
      ELSEIF (IELL .EQ. 2) THEN  ! Bessel
         A = 6377397d0
         E = 0.081690d0
      ELSEIF (IELL .EQ. 3) THEN  ! WGS 84
         A = 6378137d0
         E = 0.081819d0
      ELSEIF (IELL .EQ. 4) THEN  ! Clarke 1880
         A = 6378249d0
         E = 0.082478d0
      ELSEIF (IELL .EQ. 5) THEN  ! India 1830
         A = 6377276.345d0
         E = 0.081473d0
      ENDIF
      RETURN
      END SUBROUTINE SETELLIPS
