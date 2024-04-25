subroutine read1c(record    ,lrec      ,ibeg      ,iend      ,cvar      , &
                & lcvar     ,ier       )
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
!    Function: Reads ONE character variable with fixed length
!              from an arbitrary string/record. It is implicitly
!              assumed that the value do not contain any blank(s)
! Method used: - Look for first and second non-blanks in the
!                string. If not found, then IER is set to 0 and
!                the program is returned
!              - The character and its length is determined and
!                stored in CVAR
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Global variables
!
    integer                    :: ibeg   !!  Begin position in the RECORD from where the search for data/record is started
    integer                    :: iend   !!  Last position in the RECORD when the searched data/record is finished
    integer      , intent(out) :: ier    !!  =  0 -> end of record encountered
                                         !!  =  1 -> real value found
                                         !!  = -1 -> length or number of data is larger then specified by the calling routine
    integer      , intent(in)  :: lcvar  !!  Help var. (length of var. cvar to be looked for in the MD-file)
    integer      , intent(in)  :: lrec   !!  Help var. containing the length of RECORD
    character(*) , intent(out) :: cvar   !!  Character variable to read from a record
    character(*) , intent(in)  :: record !!  Record read from either the MD-file or from the attribute file
!
! Local variables
!
    integer                    :: iendb ! Character index end of string which is a blank 
    integer                    :: iendt ! Character index end of string which is a tab 
    character(1)               :: blank ! Character ASCII value 30 
    character(1)               :: tab   ! Character ASCII value 09 
!
!! executable statements -------------------------------------------------------
!
    !
    ! Initialisation
    !
    cvar  = ' '
    blank = char(32)
    tab   = char(09)
    !
    ! Starting point inside the record?
    !
    ier = 1
    if (ibeg > lrec) then
       ier = 0
       goto 999
    endif
    !
    ! Find the first actual character in the line 
    !
    ! -->
   20 continue
    if (record(ibeg:ibeg)==blank .or. record(ibeg:ibeg)==tab) then
       ibeg = ibeg + 1
       !
       ! No string found in the line?
       !
       if (ibeg > lrec) then
          ier = 0
          goto 999
       endif
       goto 20
    ! <--
    endif
    !
    ! Find the following blank or tab in the line, starting from IBEG 
    ! This is the end+1 of the character string
    !
    iendb = index(record(ibeg:lrec), blank)
    if (iendb==0) iendb = lrec
    iendt = index(record(ibeg:lrec), tab)
    if (iendt==0) iendt = lrec
    iend = min(iendb, iendt)
    !
    if (iend/=lrec) then
       iend = iend - 1 + ibeg - 1
    endif
    !
    ! Fill character string from RECORD
    !
    if ((iend - ibeg + 1) > lcvar) then
       ier = -1
    else
       cvar = record(ibeg:iend)
    endif
    !
    !
  999 continue
end subroutine read1c
