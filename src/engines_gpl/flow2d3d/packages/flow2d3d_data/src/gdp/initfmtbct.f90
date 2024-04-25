subroutine initfmtbct(gdp       )
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
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    character*37, dimension(:) , pointer :: fmtbct
!
! Global variables
!
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !-----Initialize statics for fmtbct
    !
    fmtbct  => gdp%gdfmtbct%fmtbct
    !
    fmtbct(1)  = '(a1,i5,                     t83  ,a1)'
    fmtbct(2)  = '(a20,a1,a42,a3,a1,          t83  ,a1)'
    fmtbct(3)  = '(2(a20,a1),a40,             t83  ,a1)'
    fmtbct(4)  = '(2(a20,a1),                 t83  ,a1)'
    fmtbct(5)  = '(a20,                       t83  ,a1)'
    fmtbct(6)  = '(a20,                       t83  ,a1)'
    fmtbct(7)  = '(a20,a1,a15,a1,             t83  ,a1)'
    fmtbct(8)  = '(a20,i9,                    t83  ,a1)'
    fmtbct(9)  = '(a20,a1,a7,a1,              t83  ,a1)'
    fmtbct(10) = '(a20,a1,a6,a1,              t83  ,a1)'
    fmtbct(11) = '(a20,a1,a36,a1,1x,2(a10,a1),t83  ,a1)'
    fmtbct(12) = '(a20,i6,                    t83  ,a1)'
    fmtbct(13) = '(f16.4,  2g14.6,            t83  ,a1)'
end subroutine initfmtbct
