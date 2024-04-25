function evenquotes(string    ,strlen    )
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
!    Function: Counts de single quotes in the specified string
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Global variables
!
    integer, intent(in)            :: strlen
    logical         :: evenquotes
    character(*), intent(in)       :: string
!
!
! Local variables
!
    integer                        :: i
    integer                        :: quotecount
    logical                        :: returnval
    character(1)                   :: quote
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !
    !-----Initialize local variables
    !
    returnval = .false.
    quote = char(39)
    quotecount = 0
    !
    !-----Return true when len(string)=0
    !
    if (strlen==0) then
       returnval = .true.
       goto 100
    endif
    !
    !-----Count quotes
    !
    do i = 1, len(string)
       if (string(i:i)==quote) quotecount = quotecount + 1
    enddo
    !
    !-----Even number of quotes: return true
    !     Odd  number of quotes: return false
    !
    if (mod(quotecount, 2)==0) then
       returnval = .true.
    else
       returnval = .false.
    endif
  100 continue
    evenquotes = returnval
end function evenquotes
