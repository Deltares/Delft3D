subroutine ERROR_DGETRF(INFO,contFLUID,mBI,nBI)
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
!  Ndryact: delft3d.support@deltares.nl                                         
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
!   Function:  Handle error on LU factorizion.
!               
!!--declarations----------------------------------------------------------------
!
  implicit none
!
! global variables
!
  integer,intent(IN)                    :: INFO          
  integer,intent(IN)                    :: contFLUID,mBI,nBI    
!
! executable statements -------------------------------------------------------
!
   IF (INFO/=0) THEN
      IF (INFO<0) THEN 
         WRITE(*,'(a,i2,100a)') 'DGETRF error: the ', -INFO,'-th argument has an illegal value. contFLUID,mBI,nBI=',contFLUID,mBI,nBI
         !pause
         stop
      ELSE
         WRITE(*,'(a,i1,a,i1,a,3I6)') 'U(',INFO,',',INFO,') is exactly zero. The factorization &
         has been completed, but the factor U is exactly &
         singular, and division by zero will occur if it &
         is used to solve a system of equations. contFLUID,mBI,nBI=',contFLUID,mBI,nBI
         !PAUSE 
         STOP
      ENDIF
   ENDIF
!
RETURN
END