subroutine SORT4(S,SORTED,SORTEDind)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011.                                     
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
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
!  $Id: My_intersec.f90 
!  $HeadURL:
!!--description-----------------------------------------------------------------
!
!   Function:   GIVEN 4 REAL NUMBERS S(1:4), IT ORDERS THEM IN SORTED(1:4), WITH
!               "1" the lowest and "4" the highest
!
!
!!--declarations----------------------------------------------------------------
!!
!!--declarations----------------------------------------------------------------
!
  use precision
!
  implicit none
!
! local variables 
!
  integer, intent(out)           :: SORTEDind(4)
  integer                        :: ind(4)
  Real(fp), intent(inout)        :: S(4) 
  Real(fp), intent(out)          :: SORTED(4)
  Real(fp)                       :: INTERM(4)
!
! executable statements -------------------------------------------------------
!
  ind(:) =(/ 1,2,3,4/)
  INTERM(:) = S(:)
  if (INTERM(2).gt.INTERM(1)) then
    !ind does not change
  else
    CALL SWAPREAL(INTERM(1),INTERM(2))
    CALL SWAPINT(ind(1),ind(2))
  endIF
  if (INTERM(4).gt.INTERM(3)) then
    !ind does not change
  else
    CALL SWAPREAL(INTERM(3),INTERM(4)) 
    CALL SWAPINT(ind(3),ind(4))
  endIF
  IF (INTERM(1).LT.INTERM(3)) THEN
    SORTED(1)    = INTERM(1)  ! LOWEST ELEMENT FOUND
    SORTEDind(1) = ind(1)
    SORTED(2)    = INTERM(3)
    SORTEDind(2) = ind(3)
  ELSE 
    SORTED(1)    = INTERM(3)  ! LOWEST ELEMENT FOUND
    SORTEDind(1) = ind(3)
    SORTED(2)    = INTERM(1)
    SORTEDind(2) = ind(1)
  ENDIF
  IF (INTERM(4).GT.INTERM(2)) THEN
    SORTED(4)    = INTERM(4)  ! HIGHEST ELEMENT FOUND
    SORTEDind(4) = ind(4)
    SORTED(3)    = INTERM(2)
    SORTEDind(3) = ind(2)
  ELSE 
    SORTED(4)    = INTERM(2)  ! HIGHEST ELEMENT FOUND
    SORTEDind(4) = ind(2)
    SORTED(3)    = INTERM(4)
    SORTEDind(3) = ind(4)
  ENDIF
  if (SORTED(3).LT. SORTED(2)) THEN !SORT MIDDLE VALUES, ELSE ITS ALREADY SORTED
     CALL SWAPREAL(SORTED(3),SORTED(2))
     CALL SWAPINT(SORTEDind(3),SORTEDind(2))
  ENDIF
RETURN
     CONTAINS             ! Internal subprograms   
      SUBROUTINE SWAPREAL(a,b)
        use precision
        implicit none
        REAL(fp), intent(inout)  :: a,b
        REAL(fp)                 :: c
         ! executable statements -------------------------------------------------------
          c=b
          b=a
          a=c
        RETURN
      END SUBROUTINE SWAPREAL
!
      SUBROUTINE SWAPINT(a,b)
        implicit none
        INTEGER, intent(inout)  :: a,b
        INTEGER                 :: c
         ! executable statements -------------------------------------------------------
          c=b
          b=a
          a=c
        RETURN
      END SUBROUTINE SWAPINT
!
END SUBROUTINE SORT4
