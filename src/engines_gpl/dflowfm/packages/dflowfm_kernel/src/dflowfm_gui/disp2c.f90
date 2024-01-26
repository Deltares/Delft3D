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
      SUBROUTINE DISP2C(X,Y,N,RCIR,NCOL)
      use m_missing
 !     use gridoperations
      implicit none
      integer          :: n, ncol
      double precision :: X(N), Y(N), rcir
      logical          :: inview

      integer          :: i, istart, key, in
!     LAAT EEN TWEEDIMENSIONALE FUNCTIE ZIEN MET CIRKELS

      IF (N .LE. 0) RETURN
      CALL SETCOL(NCOL)

      CALL JGRLINE8(x,y,N)

      if (rcir == 0) return

      IF ( NCOL.NE.0 ) THEN

         in = 0
         DO I = 1,N
            if ( INVIEW(X(i),Y(i)) ) then
               CALL MOVABS(X(I),Y(I))
               CALL CIR(RCIR)
               in = in + 1
               if (in > 5000) exit
            endif
         enddo

         CALL SETCOL(31)
         ISTART = 0
         DO I = 1,N
            IF (X(I) .NE. dmiss) THEN
               IF (ISTART .EQ. 1) THEN
               ELSE
                  CALL MOVABS(X(I),Y(I))
                  CALL CIR(RCIR)
                  ISTART = 1
               ENDIF
            ELSE
               ISTART = 0
            ENDIF
         END DO

      END IF

      RETURN
      END
