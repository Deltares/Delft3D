!----- AGPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------

       FUNCTION IFREAL(VAL1, VAL2, EPS)
!C
!C---- COMPARES TWO REAL POINT NUMBERS
!C
!C     RETURN VALUE: -1 IF VAL1 < VAL2
!C                    0 IF VAL1 = VAL2
!C                   +1 IF VAL1 > VAL2
!C
      Integer Ifreal, IfFlt
      Real Val1, Val2, Eps, Value

      IF (ABS(VAL1) .LT. 1.0  .OR.  ABS(VAL2) .LT. 1.0) THEN
         VALUE = VAL1 - VAL2
      ELSE
         VALUE = VAL1 / VAL2 - 1.0
      ENDIF
!C
!CGP  IF (ABS(VALUE) .LT. EPS) THEN
!CGP  ook eps=0 toestaan
      IF (ABS(VALUE) .LE. EPS) THEN
         IFFLT = 0
      ELSE
         IF (VAL1 .LT. VAL2) THEN
            IFFLT = -1
         ELSE
            IFFLT = 1
         ENDIF
      ENDIF
      IfReal = IfFlt
!C
      Return
      END



      FUNCTION D_IFREAL(VAL1, VAL2, EPS)
!C
!C---- COMPARES TWO Double precision POINT NUMBERS
!C
!C     RETURN VALUE: -1 IF VAL1 < VAL2
!C                    0 IF VAL1 = VAL2
!C                   +1 IF VAL1 > VAL2
!C
      Integer          D_Ifreal, IfFlt
      Double precision Val1, Val2, Eps, Value

      IF (ABS(VAL1) .LT. 1.0  .OR.  ABS(VAL2) .LT. 1.0) THEN
         VALUE = VAL1 - VAL2
      ELSE
         VALUE = VAL1 / VAL2 - 1.0
      ENDIF
!C
!CGP  IF (ABS(VALUE) .LT. EPS) THEN
!CGP  ook eps=0 toestaan
      IF (ABS(VALUE) .LE. EPS) THEN
         IFFLT = 0
      ELSE
         IF (VAL1 .LT. VAL2) THEN
            IFFLT = -1
         ELSE
            IFFLT = 1
         ENDIF
      ENDIF
!     write(*,*) ' D_Ifreal val1 val2 eps result'
!     write(*,*) val1, val2, eps, ifflt
      D_IfReal = IfFlt
!C
      Return
      END
