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

      SUBROUTINE POSITIVEBLOK()
      implicit none
      integer :: mb, nb, mb2, nb2, npt, npt2, nputo, itype
      COMMON /BLOK/ MB(6),NB(6),MB2(6),NB2(6),NPT,NPT2,NPUTO,ITYPE

      integer :: mh, nh, m1, n1, m2, n2, i
      IF (NPT .LE. 1) RETURN

!     IF (ITYPE .EQ. 1) THEN
         IF (MB(2) .LT. MB(1)) THEN
            MH    = MB(1)
            MB(1) = MB(2)
            MB(2) = MH
         ENDIF
         IF (NB(2) .LT. NB(1)) THEN
            NH    = NB(1)
            NB(1) = NB(2)
            NB(2) = NH
         ENDIF
!     ENDIF

      M1 = MB(1)
      N1 = NB(1)
      M2 = MB(2)
      N2 = NB(2)
      DO 10 I = 1,NPT
         M1 = MIN(MB(I),M1)
         N1 = MIN(NB(I),N1)
         M2 = MAX(MB(I),M2)
         N2 = MAX(NB(I),N2)
    10 CONTINUE
      MB(3) = M1
      NB(3) = N1
      MB(4) = M2
      NB(4) = N2
      RETURN
      END subroutine positiveblok
