subroutine keyinp(string    ,rdpar     )
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
!    Function: Gets a character parameter contents from the
!              specified string
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
    character(*)    :: rdpar
                                   !!  Character string for parameter
                                   !!  contents to be read
    character(*)    :: string
                                   !!  Input string, after reading the para-
                                   !!  meter is replaced with blanks
!
!
! Local variables
!
    integer                        :: ix1
    integer                        :: ix2
    integer                        :: lenchr
    character(1)                   :: quote
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !
    quote = char(39)
    rdpar = ' '
    !
    !-----Read character (STRING) from given string
    !     Search for start quote and when found for end quote
    !     If one of these quotes is missing free formatted reading is
    !     not possible, hence RDPAR will remain blank
    !
    ix1 = index(string, quote)
    if (ix1==0) goto 9999
    if (ix1==len(string)) goto 9999
    !
    ix2 = index(string(ix1 + 1:), quote)
    if (ix2==0) goto 9999
    !
    ix1 = ix1 + 1
    ix2 = ix2 - 1
    !
    !-----Define length of substring to be read from STRING
    !     Add blanks after read part if parameter length larger then
    !     substring to read from
    !
    lenchr = min(len(rdpar), ix2)
    rdpar(:lenchr) = string(ix1:ix1 + lenchr)
    if (lenchr<len(rdpar)) rdpar(lenchr + 1:) = ' '
    !
    !-----After reading clear space between quotes including quotes themself
    !
    string(ix1 - 1:ix1 + ix2 + 1) = ' '
    !
 9999 continue
end subroutine keyinp
