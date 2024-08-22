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

 ! Last changed
! by:               $Author:: Schrier           $
! at:               $Modtime:: 22-07-97 11:53a  $
!
! current revision: $Revision:: 3               $


      SUBROUTINE LOCATE (XX, N, X, J)
!
!  Subroutine from 'Numerical recipes' Fortran  edition.
!  Given an array XX of length N, given value X, return a value J
!  such that X is between XX(J) en XX (J+1)
!  XX must be monotonic, either decreasing or increasin
!  J=0 or J=N indicates X is out of range.


      IMPLICIT REAL             ( A - H )
      IMPLICIT INTEGER          ( I - N )
      IMPLICIT REAL             ( O - Z )

      Integer      N, j, jl, ju, jm
      Real         XX(N)
      LOGICAL      L1, L2
      Real         X

      JL = 0
      JU = N+1
  10  IF (JU-JL .GT. 1) THEN
          JM = (JU+JL)/2
          L1 = XX(N) .GT. XX(1)
          L2 = X .GT. XX(JM)
          IF ( (L1.AND.L2) .OR. (.NOT. (L1 .OR. L2)) ) THEN
              JL = JM
          ELSE
              JU = JM
          ENDIF
          GOTO 10
      ENDIF

      J = JL

    RETURN
    END subroutine Locate


      SUBROUTINE D_LOCATE (XX, N, X, J)
!
!  Subroutine from 'Numerical recipes' Fortran  edition.
!  Given an array XX of length N, given value X, return a value J
!  such that X is between XX(J) en XX (J+1)
!  XX must be monotonic, either decreasing or increasin
!  J=0 or J=N indicates X is out of range.


      IMPLICIT Double precision ( A - H )
      IMPLICIT INTEGER          ( I - N )
      IMPLICIT Double Precision ( O - Z )

      Integer          N, j, jl, ju, jm
      Double precision XX(N)
      LOGICAL          L1, L2
      Double precision X

      JL = 0
      JU = N+1
  10  IF (JU-JL .GT. 1) THEN
          JM = (JU+JL)/2
          L1 = XX(N) .GT. XX(1)
          L2 = X .GT. XX(JM)
          IF ( (L1.AND.L2) .OR. (.NOT. (L1 .OR. L2)) ) THEN
              JL = JM
          ELSE
              JU = JM
          ENDIF
          GOTO 10
      ENDIF

      J = JL

    RETURN
    END subroutine D_Locate
