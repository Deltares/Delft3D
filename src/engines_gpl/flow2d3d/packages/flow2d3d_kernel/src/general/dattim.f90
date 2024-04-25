subroutine dattim(rundat    )
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
!    Function: Determine system date and time, HARDWARE dpendent
!              routine.
!              This routine works on HP 720
!                                    SUN sparc 2
!                                    IBM RS 6000
!                                    CONVEX 3800
!                                    CRAY
!                                    APOLLO DN 10000
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
    character(20)   :: rundat
                                   !!  Current date and time containing a
                                   !!  combination of DATE and TIME
                                   !!  format: 'yyyy/mm/dd hh:mm:ss '
!
!
! Local variables
!
    integer                        :: imo                  ! Current month 
    character(30)                  :: record ! Hulp text string 
    character(48)                  :: month ! All months of the year 
!
    !
    !
    data month/'Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec'/
!
!! executable statements -------------------------------------------------------
!
    !
    !-----initialize
    !
    rundat = ' '
    record = ' '
    !
    !-----get date and time in file
    !
    call cdate(record    )
    !
    !-----format:  'Dvw Mmm dd hh:mm:ss yyyy'
    !     record = '123456789012345678901234567890'
    !     calculate month and rewrite record to rundat
    !
    imo = (index(month, record(5:7)) + 3)/4
    rundat = record(21:24) // '/  /' // record(9:10) // ' ' // record(12:19)    &
            & // ' '
    write (rundat(6:7), '(i2.2)') imo
    !
    !-----replace blanks for 0
    !
    if (rundat(9:9)==' ') rundat(9:9) = '0'
    if (rundat(12:12)==' ') rundat(12:12) = '0'
    if (rundat(15:15)==' ') rundat(15:15) = '0'
    if (rundat(18:18)==' ') rundat(18:18) = '0'
end subroutine dattim
