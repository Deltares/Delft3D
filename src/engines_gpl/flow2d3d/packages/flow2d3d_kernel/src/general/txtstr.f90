subroutine txtstr(record    ,ibeg      ,iend      ,ltxt      ,txtvar    , &
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
!    Function: - Searches a text in a string between position
!                ibeg and iend. Blanks are disregarded. The text
!                found will be put in TXTVAR
!              - Determines the length of TXTVAR
!              - IBEG and IEND have as output different values
!                from there input values
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
    integer         :: ibeg
                                   !!  Begin position in the RECORD from
                                   !!  where the search for data/record
                                   !!  is started
    integer         :: iend
                                   !!  Last position in the RECORD when
                                   !!  the searched data/record is finished
    integer         :: ltxt
                                   !!  on input : total length of text
                                   !!             string to search
                                   !!  on output: actual length of text
                                   !!             string read
    logical         :: error
                                   !!  Flag=TRUE if an error is encountered
    character(*), intent(in)       :: record
                                   !!  Record read from either the MD-file
                                   !!  or from the attribute file
    character(*), intent(out)      :: txtvar
                                   !!  Text string to searc in RECORD
!
!
! Local variables
!
    integer                        :: ltotxt               ! Help var. 
    integer                        :: ltxtrd
    character(1)                   :: blank ! Character ASCII value 30 
    character(1)                   :: tab ! Character ASCII value 09 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !
    !-----Initialize
    !
    blank = char(32)
    tab = char(09)
    ltotxt = ltxt
    !
    !-----Calculate part of record with textstring and define TXTVAR
    !     remove all leading blanks/tabs
    !
    ! -->
  110 continue
    if (record(ibeg:ibeg)==blank .or. record(ibeg:ibeg)==tab) then
       ibeg = ibeg + 1
       if (ibeg<iend) goto 110
    endif
    ! <--
    !
    !-----Remove all trailing blanks/tabs
    !
    ! -->
  210 continue
    if (record(iend:iend)==blank .or. record(iend:iend)==tab) then
       iend = iend - 1
       if (iend>=ibeg) goto 210
    endif
    ! <--
    iend = max(iend, ibeg)
    !
    !-----textstring no longer than variable TXTVAR
    !
    ltxt = iend - ibeg + 1
    ltxtrd = max(1, min(ltxt, ltotxt))
    txtvar(1:ltxtrd) = record(ibeg:ibeg + ltxtrd - 1)
end subroutine txtstr
