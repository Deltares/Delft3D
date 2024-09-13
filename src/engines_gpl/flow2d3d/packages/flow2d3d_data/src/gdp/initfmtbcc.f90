subroutine initfmtbcc(gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2016.                                
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
!  $Id: initfmtbcc.f90 5717 2016-01-12 11:35:24Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/data/src/gdp/initfmtbcc.f90 $
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
    character*37, dimension(:) , pointer :: fmtbcc
!
! Global variables
!
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !-----Initialize statics for fmtbcc
    !
    fmtbcc  => gdp%gdfmtbcc%fmtbcc
    !
    fmtbcc(1) = '(a1,i5,                     t89  ,a1)'
    fmtbcc(2) = '(a20,a1,a63,a3,a1,          t89  ,a1)'
    fmtbcc(3) = '(a20,a1,a10,a1,a40,         t89  ,a1)'
    fmtbcc(4) = '(2(a20,a1),                 t89  ,a1)'
    fmtbcc(5) = '(a20,                       t89  ,a1)'
    fmtbcc(6) = '(a20,                       t89  ,a1)'
    fmtbcc(7) = '(a20,a1,a15,a1,             t89  ,a1)'
    fmtbcc(8) = '(a20,i9,                    t89  ,a1)'
    fmtbcc(9) = '(a20,a1,a7,a1,              t89  ,a1)'
    fmtbcc(10) = '(a20,a1,a6,a1,              t89  ,a1)'
    fmtbcc(11) = '(a20,a1,a36,a1,1x,2(a10,a1),t89  ,a1)'
    fmtbcc(12) = '(a20,i6,                    t89  ,a1)'
    fmtbcc(13) = '(f16.4,  5g14.6,            t89  ,a1)'
end subroutine initfmtbcc
