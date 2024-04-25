subroutine initfmtdis(gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
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
!  
!  
!!--description-----------------------------------------------------------------
!
! NONE
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    character*39, dimension(:) , pointer :: fmtdis
!
! Global variables
!
!
!! executable statements -------------------------------------------------------
!
    fmtdis  => gdp%gdfmtdis%fmtdis
    !
    ! Initialize statics for fmtdis
    !
    fmtdis(1)  = '(a1,i5,                      t125 ,a1)'
    fmtdis(2)  = '(a20,a1,a42,a3,a1,           t125 ,a1)'
    fmtdis(3)  = '(a20,a1,a10,a1,a30,          t125 ,a1)'
    fmtdis(4)  = '(2(a20,a1),                  t125 ,a1)'
    fmtdis(5)  = '(a20,                        t125 ,a1)'
    fmtdis(6)  = '(a20,                        t125 ,a1)'
    fmtdis(7)  = '(a20,a1,a15,a1,              t125 ,a1)'
    fmtdis(8)  = '(a20,i9,                     t125 ,a1)'
    fmtdis(9)  = '(a20,a1, a7,a1,              t125 ,a1)'
    fmtdis(10) = '(a20,a1,a10,a1,              t125 ,a1)'
    fmtdis(11) = '(a20,a1,a36,a1,1x,2(a10,a1), t125 ,a1)'
    fmtdis(12) = '(a20,i6,                     t125 ,a1)'
    fmtdis(13) = '(f16.4,  8g14.6,             t125 ,a1)'
end subroutine initfmtdis
