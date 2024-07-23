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

       SUBROUTINE BubbleSort (NA,A,B,N,Option)

! *********************************************************************
! *** Brief description:
! *** ------------------
! ***    This subroutines performs sorting of an array A in increasing
! ***    order, using Bubble sort (not a very good algorithm though).
! ***    Optionally also array B is included in the sorting alg.
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! *** NA = dimension of integer array A
! *** A  = array A with integer values
! *** B  = array B with values, optionally also to be sorted using array A
! *** N  = number of elements in array A
! *** Option  = yes/no sort B also
! *********************************************************************
! *********************************************************************

      INTEGER   ::  NA, N
      Real      ::  A(NA), B(NA)
      Logical   ::  Option

      INTEGER       J,R,L,K
      Real          X

!
! Een array van lengte 1 of kleiner hoeft niet gesorteerd te worden
      if (NA .le. 1 .or. n .le. 1) goto 999

      L = 2
      R = N
      K = N
  100 IF ( L.GT.R ) THEN
!
         RETURN
      ELSE
         DO 150 J = R , L , -1
            IF ( A(J-1).GT.A(J) ) THEN
               X = A(J-1)
               A(J-1) = A(J)
               A(J) = X
               If (option) then
                 X = B(J-1)
                 B(J-1) = B(J)
                 B(J) = X
               Endif
               K = J
            END IF
  150    CONTINUE
         L = K + 1
         DO 200 J = L , R
            IF ( A(J-1).GT.A(J) ) THEN
               X = A(J-1)
               A(J-1) = A(J)
               A(J) = X
               If (option) then
                  X = B(J-1)
                  B(J-1) = B(J)
                  B(J) = X
               Endif
               K = J
            END IF
  200    CONTINUE
         R = K - 1
         GOTO 100
      END IF

  999 Continue
      Return
      END
