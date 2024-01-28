subroutine search2(mcmp      ,keywrd    ,ier       ,rec       )
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
!    Function: Looking for a keyword
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
    integer                    :: ier    !!  Help var. for keyword
                                         !!  IER = > 0 found
                                         !!  IER = -1  not found
    integer       , intent(in) :: mcmp   !!  Logical unit number
    character(*)  , intent(in) :: keywrd !  Description and declaration in keywtd.igs
    character(132)             :: rec    !!  Last read record
!
!
! Local variables
!
    integer       :: iocond
!
!
!! executable statements -------------------------------------------------------
!
    !
    rewind (mcmp)
    !
   10 continue
    read (mcmp, '(a)', iostat = iocond) rec
    if (iocond/=0) then
       ier = -1
       goto 999
    endif
    ier = index(rec, keywrd)
    if (ier>=1) goto 999
    goto 10
    !
    !
  999 continue
end subroutine search2
