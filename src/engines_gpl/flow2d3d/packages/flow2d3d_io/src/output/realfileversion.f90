function realfileversion(versionstring, lenstring)
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
!    Function: Converts a version number string (format mm.nn.xx.yy)
!              in a real number (format mm.nnxxyy) to allow comparison
!              with other version numbers
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
   use precision
   implicit none
!
! Global parameters
!
   integer      :: lenstring
   real(fp)     :: realfileversion
   character(*) :: versionstring
!
! Local parameters
!
   integer       :: p1             ! Index of first point in string (mm.nn.xx.yy)
   integer       :: p2             ! Index of second point in string (mm.nn.xx.yy)
   integer       :: p3             ! Index of third point in string (mm.nn.xx.yy)
   character(16) :: versionstring1 ! Help string to to remove the third point
!
!! executable statements -------------------------------------------------------
!
   p1 = index(versionstring(      1:lenstring), '.')
   p2 = index(versionstring(   p1+1:lenstring), '.')
   p3 = index(versionstring(p1+p2+1:lenstring), '.')
   
   versionstring1 = ' '
   if (p3 > 0) then
      versionstring1 = versionstring(1:p1+p2-1)//versionstring(p1+p2+1:p3-1)//versionstring(p1+p2+p3+1:lenstring)
   else
      versionstring1 = versionstring(1:p1+p2-1)//versionstring(p1+p2+1:lenstring)
   endif
   read(versionstring1, *) realfileversion
end function realfileversion
