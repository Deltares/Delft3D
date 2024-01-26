subroutine srckey(string    ,istart    ,iend      ,ifound    ,gdp       )
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
!    Function: Search a keywrd in the specified string
! Method used:
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
    character*20, dimension(:) , pointer :: keywrd
!
! Global variables
!
    integer         :: iend
                                   !!  End position to search
    integer, intent(out)           :: ifound
                                   !!  Keywrd nr. found:
                                   !!         0 if none found
                                   !!      9999 if comment line found
                                   !!     -9999 if error occured
                                   !!  1 to MXKWTD if appropriate keyword
                                   !!  found
    integer         :: istart
                                   !!  Start position to search
    character(*)    :: string
                                   !!  Input string
!
!
! Local variables
!
    integer                        :: ihelp                ! elp variable 
    integer                        :: ink
    integer                        :: istartcs             ! tart point in string to search comments 
    integer                        :: ix
    integer                        :: lkeyw
    logical                        :: evenquotes           ! unction counting quotes in a string. he source is in this file. 
    logical                        :: nocomments           ! RUE when all commments are removed 
    character(1)                   :: quote
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    keywrd  => gdp%gdkeywtd%keywrd
    !
    quote = char(39)
    ink = 0
    ifound = 0
    istartcs = istart
    !
    !-----test if
    !
    if (istart>iend) then
       ifound = -9999
       goto 300
    endif
    !
    !-----Strip all comments from the string
    !     Comments are defined as:
    !     - The subset of the string, starting with one of the three
    !       characters ;, # or * up to the end of the string
    !     AND
    !     - The number of single quotes (') in the string before the
    !       comment-starting-character must be even; when the number is odd,
    !       the comment-starting-character is part of the (character-)value
    !       of a keyword and should NOT be interpreted as a comment-
    !       starting-character.
    !
    ! ==>
  100 continue
    nocomments = .true.
    ix = index(string(istartcs:iend), ';')
    if (ix/=0) then
       ihelp = istartcs - 1 + ix
       if (evenquotes(string(istart:ihelp), ihelp)) then
          string(ihelp:iend) = ' '
          iend = ihelp
          ifound = 9999
       else
          !
          !-----------Check for comments behind the found not-comment-starting ";"
          !
          nocomments = .false.
          istartcs = istartcs + ix
       endif
    endif
    ix = index(string(istartcs:iend), '#')
    if (ix/=0) then
       ihelp = istartcs - 1 + ix
       if (evenquotes(string(istart:ihelp), ihelp)) then
          string(ihelp:iend) = ' '
          iend = ihelp
          ifound = 9999
       else
          !
          !-----------Check for comments behind the found not-comment-starting "#"
          !
          nocomments = .false.
          istartcs = istartcs + ix
       endif
    endif
    ix = index(string(istartcs:iend), '*')
    if (ix/=0) then
       ihelp = istartcs - 1 + ix
       if (evenquotes(string(istart:ihelp), ihelp)) then
          string(ihelp:iend) = ' '
          iend = ihelp
          ifound = 9999
       else
          !
          !-----------Check for comments behind the found not-comment-starting "*"
          !
          nocomments = .false.
          istartcs = istartcs + ix
       endif
    endif
    if (.not.nocomments) goto 100
    ! <==
    !
    !-----Convert to lower case
    !
    ix = index(string(istart:iend), quote)
    if (ix>0) then
       call small(string(istart:istart + ix - 1)  ,ix - 1    )
    else
       call small(string(istart:iend)  ,iend - istart + 1    )
    endif
    !
    !-----Start searching (loop over MXKWTD defined in KEYWTD.INC)
    !
    do ink = 1, mxkwtd
       lkeyw = index(keywrd(ink), ' ')
       if (lkeyw==0) lkeyw = len(keywrd(ink)) + 1
       lkeyw = lkeyw - 1
       ix = index(string(istart:iend), keywrd(ink)(1:lkeyw))
       !
       !--------Found then clear string from istart to (including) keyword
       !        and exit loop
       !
       if (ix>0) then
          ifound = ink
          ix = istart + ix + lkeyw - 2
          string(istart:ix) = ' '
          istart = ix
          exit
       endif
    enddo
    !
  300 continue
end subroutine srckey
