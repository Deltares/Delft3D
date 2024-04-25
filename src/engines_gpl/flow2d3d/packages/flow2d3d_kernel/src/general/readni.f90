subroutine readni(record    ,lrec      ,ibeg      ,iend      ,nvar      , &
                & iarray    ,idfaul    ,ier       )
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
!    Function: Reads NVAR (specified) integers from a RECORD
!              (string) of variable length
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
    integer                                    :: ibeg   !!  Begin position in the RECORD from
                                                         !!  where the search for data/record is started
    integer                      , intent(in)  :: idfaul !!  Default value when IVAR not found
    integer                                    :: iend   !!  Last position in the RECORD when the searched data/record is finished
    integer                      , intent(out) :: ier    !!  =  0 -> end of record encountered
                                                         !!  =  1 -> real value found
                                                         !!  = -1 -> length or number of data is
                                                         !!          larger then specified by the calling routine (ERROR)
    integer                      , intent(in)  :: lrec   !!  Help var. containing the length of RECORD
    integer                      , intent(in)  :: nvar   !!  Help var.: nr. of data to look for in the MD-file
    integer     , dimension(nvar), intent(out) :: iarray !!  Help array for reading the integers
    character(*)                 , intent(in)  :: record !!  Record read from either the MD-file or from the attribute file
!
!
! Local variables
!
    integer          :: iendb
    integer          :: iendt
    integer          :: inu
    integer          :: iocond
    integer          :: n
    character(1)     :: blank
    character(1)     :: tab
    character(7)     :: fmti
!
!
!! executable statements -------------------------------------------------------
!
    !
    !-----Initialisation
    !
    fmti  = '(Ixx)  '
    ier   = 1
    iend  = ibeg - 1
    blank = char(32)
    tab   = char(09)
    !
    !-----Find values for NVAR array elements
    !
    do n = 1, nvar
       ibeg = iend + 1
       !
       !--------Starting point outside record
       !
       if (ibeg>lrec) then
          ier = 0
          exit
       endif
       !
       !--------Find the first 'non' space/tab
       !        (this is the start of a number)
       !
  210  continue
       if (record(ibeg:ibeg)==blank .or. record(ibeg:ibeg)==tab) then
          ibeg = ibeg + 1
          !
          !--------------When no more numbers in the line
          !
          if (ibeg>lrec) then
             ier = 0
             exit
          endif
          goto 210
       endif
       !
       !--------Find the first 'non' space/tab
       !        (this is the end+1 of a number)
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
       !--------Read number
       !
       if ((record(ibeg:ibeg)=='[') .or. (record(ibeg:ibeg)==']')) then
          iarray(n) = idfaul
          ier = -1
       else
          write (fmti(3:4), '(i2.2)') (iend - ibeg + 1)
          read (record(ibeg:iend), fmti, iostat = iocond) iarray(n)
          if (iocond/=0) then
             ier = -1
             exit
          endif
       endif
       !
       !--------Test whether [ ] has been completely taken in
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
    enddo
end subroutine readni
