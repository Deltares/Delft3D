module m_combinepaths
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2026.                                
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
   implicit none

   contains
   
!> This function appends the second_name to the path part of first_name, and
!! returns it as a new string. Both first_name and second_name may initially
!! contain absolute, relative or no path information.
!!
!! Examples:
!!
!! first_name             second_name              new_name
!! file1.txt              file2.txt                file2.txt
!! dir\file1.txt          file2.txt                dir\file2.txt
!! dir/file1.txt          file2.txt                dir/file2.txt
!! c:\dir\file1.txt       file2.txt                c:\dir\file2.txt
!! \\agent\dir\file1.txt  file2.txt                \\agent\dir\file2.txt
!! /dir/file1.txt         file2.txt                /dir/file2.txt
!! c:\dir\file1.txt       ..\dir2\file2.txt        c:\dir\..\dir2\file2.txt
!! \\agent\dir\file1.txt  ..\dir2\file2.txt        \\agent\dir\..\dir2\file2.txt
!! /dir/file1.txt         ../dir2/file2.txt        /dir/../dir2/file2.txt
!! c:\dir\file1.txt       d:\dir2\file2.txt        d:\dir2\file2.txt
!! /dir/file1.txt         /dir2/file2.txt          /dir2/file2.txt
!! \\agent\dir\file1.txt  \\agent2\dir2\file2.txt  \\agent2\\dir2\file2.txt
pure function combinepaths (first_name, second_name) result(new_name)
   character(*), intent(in)  :: first_name !< reference file name
   character(*), intent(in)  :: second_name !< specification of new file relative to first_name
   character(len=:), allocatable :: new_name !< combined file name

   integer :: ipos ! position of last path separator in first_name

   new_name = adjustl(second_name)
   if (len_trim(new_name) > 2) then
      if (new_name(2:2) == ':') then
         ! second_name contains an absolute Windows path
         ! don't append it to first_name!
         return
         
      elseif (new_name(1:2) == '\\') then
         ! second_name contains an absolute UNC path
         ! don't append it to first_name!
         return
         
      elseif (new_name(1:1) == '/') then
         ! second_name contains an absolute Linux/UNIX path
         ! don't append it to first_name!
         return
         
      endif
   endif

   ! second_name contains file name or relative path.
   ! first_name may contain absolute, relative or no path.
   ipos = index(first_name, '/', BACK=.true.)
   ipos = max(ipos,index(first_name, char(92), BACK=.true.)) ! char(92)==backslash
   ! ipos = 0: no separator in first_name; 
   ! no path in first_name to add to second_name; return
   if (ipos == 0) return
   
   ! otherwise append the second_name to the path part of first_name
   new_name = first_name(:ipos)//new_name
end function combinepaths

end module m_combinepaths
