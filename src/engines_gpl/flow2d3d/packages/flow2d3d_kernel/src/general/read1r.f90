subroutine read1r(record    ,lrec      ,ibeg      ,iend      ,rvar      , &
                & rdfaul    ,ier       )
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
!    Function: Reads ONE real variable from an arbitrary string/
!              record.
! Method used: - Look for first and second non-blanks in the
!                string. If not found, then IER is set to 0 and
!                the program is returned
!              - If [] found, then if   DEFAULT RVAR=RDFAUL
!                                  else         IER =-1
!                             program is returned
!              - Otherwise read real after setting the format
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Global variables
!
    integer                   :: ibeg   !!  Begin position in the RECORD from
                                        !!  where the search for data/record is started
    integer                   :: iend   !!  Last position in the RECORD when the searched data/record is finished
    integer     , intent(out) :: ier    !!  =  0 -> end of record encountered
                                        !!  =  1 -> real value found
                                        !!  = -1 -> length or number of data is
                                        !!          larger then specified by the calling routine (ERROR)
    integer     , intent(in)  :: lrec   !!  Help var. containing the length of RECORD
    real(fp)    , intent(in)  :: rdfaul !!  Default value when RVAR not found
    real(fp)    , intent(out) :: rvar   !!  Real var. or array to store the value(s) read from a record
    character(*), intent(in)  :: record !!  Record read from either the MD-file or from the attribute file
!
!
! Local variables
!
    integer         :: iendb
    integer         :: iendt
    integer         :: inu
    integer         :: iocond
    character(1)    :: blank
    character(1)    :: tab
    character(7)    :: fmtr
!
!
!! executable statements -------------------------------------------------------
!
    !
    !-----Initialisation
    !
    fmtr = '(Fxx.0)'
    blank = char(32)
    tab = char(09)
    !
    !-----Starting point outside record
    !
    ier = 1
    if (ibeg>lrec) then
       ier = 0
       goto 999
    endif
    !
    !-----Find the first 'non' space/tab
    !     (this is the start of a number)
    !
    ! -->
   20 continue
    if (record(ibeg:ibeg)==blank .or. record(ibeg:ibeg)==tab) then
       ibeg = ibeg + 1
       !
       !---------When no more numbers in the line
       !
       if (ibeg>lrec) then
          ier = 0
          goto 999
       endif
       goto 20
    ! <--
    endif
    !
    !-----Find the first 'non' space/tab
    !     (this is the end+1 of a number)
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
    !-----Read number
    !
    if ((record(ibeg:ibeg)=='[') .or. (record(ibeg:ibeg)==']')) then
       rvar = rdfaul
       ier = -1
    else
       write (fmtr(3:4), '(i2.2)') (iend - ibeg + 1)
       read (record(ibeg:iend), fmtr, iostat = iocond) rvar
       if (iocond/=0) then
          ier = -1
          goto 999
       endif
    endif
    !
    !-----Test whether [ ] has been completely taken in
    !
    if ((record(ibeg:ibeg)=='[') .and. (record(iend:iend)/=']')) then
       inu = iend
       iend = index(record(inu:lrec), ']')
       if (iend==0) then
          iend = lrec
       else
          iend = iend + inu - 1
       endif
    endif
    !
    !
  999 continue
end subroutine read1r
