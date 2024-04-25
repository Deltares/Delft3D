subroutine txtmrk(record    ,lrec      ,ibeg      ,iend      ,error     )
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
!    Function: - Searches a text in a string which begins and
!                ends with #
!              - It returns the positions of these # in the
!                string (= IBEG and IEND)
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
    integer                   :: ibeg   !!  Begin position in the RECORD from where the search for data/record is started
    integer                   :: iend   !!  Last position in the RECORD when the searched data/record is finished
    integer     , intent(in)  :: lrec   !!  Help var. containing the length of RECORD
    logical     , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    character(*), intent(in)  :: record !!  Record read from either the MD-file or from the attribute file
!
!
! Local variables
!
    integer                        :: istart               ! Help var. 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !
    !-----initialize
    !
    istart = ibeg
    !
    !-----startpunt buiten record grote
    !
    if (istart>lrec) then
       error = .true.
       goto 9999
    endif
    !
    !-----calculate part of record with text to search, position of first #
    !
    ibeg = index(record(istart:lrec), '#')
    if (ibeg==0) then
       error = .true.
       goto 9999
    else
       ibeg = istart + ibeg
    endif
    !
    !-----calculate part of record with text to search, position of second #
    !
    iend = index(record(ibeg:lrec), '#')
    if (iend==0) then
       iend = lrec
    else
       iend = ibeg - 1 + iend - 1
    endif
    !
 9999 continue
end subroutine txtmrk
