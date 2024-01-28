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

      SUBROUTINE READARCINFOBLOCK(MINP,D,MC,NC,RMIS)
      USE M_MISSING
      implicit none
      integer :: i
      integer :: j
      integer :: mc
      integer :: minp
      integer :: nc
      double precision :: rmis
      real    :: D(MC,NC)
      double precision, dimension(MC) :: dline
      CHARACTER TEX*16

      DO 10 J = NC,1,-1
         READ(MINP,*,ERR=101,END=100) (D(I,J),I = 1,MC)
   10 CONTINUE
      DO 20 I = 1,MC
         DO 20 J = 1,NC
            IF (D(I,J) .EQ. RMIS) D(I,J) = dmiss
   20 CONTINUE
      call doclose (MINP)
      RETURN

  100 CONTINUE
      CALL EOFERROR(MINP)
  101 CONTINUE
      WRITE(TEX,'(2I8)') I,J
      CALL READERROR('ERROR READING ARC-INFO BLOCK IN COLNR, ROWNR :',TEX,MINP)
      RETURN
      END
