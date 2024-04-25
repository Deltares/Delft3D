subroutine regel(string    ,il        ,ir        ,maxvld    ,nveld     , &
               & error     )
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
!    Function: Analyses character string line
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Global variables
!
    integer                   , intent(in)  :: maxvld !!  Max number of items to be read from
    integer                                 :: nveld  !!  Number of fields in character string
                                                      !!  Input character string
    integer, dimension(maxvld), intent(out) :: il     !!  Array containing left positions
    integer, dimension(maxvld), intent(out) :: ir     !!  Array containing left positions
    logical                   , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    character(*)              , intent(in)  :: string
!
!
! Local variables
!
    integer           :: i      ! Counter 
    integer           :: lenstr ! Length of character string 
    character(1)      :: l1     ! Used for determining start and stop positions of fields 
    character(1)      :: l2     ! Used for determining start and stop positions of fields 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !
    !-----initialize length of string to check
    !
    lenstr = len(string)
    !
    !-----first character STRING <> ' '
    !
    if (string(1:1)/=' ') then
       il(1) = 1
       nveld = 1
    else
       nveld = 0
    endif
    !
    !-----check all characters till LENSTR
    !
    do i = 1, lenstr - 1
       l1 = string(i:i)
       l2 = string(i + 1:i + 1)
       !
       !-------New field encountered
       !
       if (l1==' ' .and. l2/=' ') then
          nveld = nveld + 1
          if (nveld>maxvld) then
             error = .true.
             goto 9999
          endif
          il(nveld) = i + 1
       endif
       !
       !-------End of field encountered
       !
       if (l1/=' ' .and. l2==' ') then
          ir(nveld) = i
       endif
    enddo
    !
    !-----last character STRING <> ' '
    !
    if (string(lenstr:lenstr)/=' ') then
       ir(nveld) = lenstr
    endif
    !
 9999 continue
end subroutine regel
