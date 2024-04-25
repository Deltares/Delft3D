!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
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
subroutine test_01
!
!   Program to test some functions which are available in the util_mf libarray
!
!   util_mf: UTILities for Mor and Flow
!
!   Jan.Mooiman@deltares.nl
!   22 sep 05
!
    implicit none
    logical            :: error
    character(len=256) :: value
    character(len=20)  :: env (2)

    call util_getenv('D3D_HOME', value)
    write(*,*) 'Result D3D_HOME: <',trim(value),'>'

    env(1) = 'D3D_HOME'
    env(2) = 'd3d_home'
    value  = ' ' 
    call util_getenv(env(1), value)
    write(*,*) 'Result D3D_HOME: <',trim(value),'>'

!    call getmp(error, value)
!    write(*,*) 'Result getmp : ',error,'<',trim(value),'>'

end subroutine test_01
