subroutine cp_file(filnm1    ,filnm2    ,filtype      ,nuerr         )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
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
! Copy or append file FILNM1 to file FILNM2
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Global variables
!
    integer     , intent(out) :: nuerr
    character(*), intent(in)  :: filnm1
    character(*), intent(in)  :: filnm2
    character(*), intent(in)  :: filtype
!
! Local variables
!
    integer           :: iocond    ! IO status return code
    integer           :: iter      ! indicates the number of attempts to query a file
    integer           :: lf1       ! > 0 Error; < 0 End-Of-File Actual length of string FILNM1
    integer           :: lf2       ! Actual length of string FILNM2
    integer           :: lrec      ! Actual length of string REC132
    integer           :: lunf1     ! Unit number for FILNM1
    integer           :: lunf2     ! Unit number for FILNM2
    integer           :: nr
    integer           :: nrec
    logical           :: ex        ! Flag for existing file
    logical           :: opend1    ! Flag to test if file FILNM1 is already opened
    logical           :: opend2    ! Flag to test if file FILNM2 is already opened
    character(132)    :: rec132
!
!! executable statements -------------------------------------------------------
!
    nuerr = 0
    lf1 = len_trim(filnm1)
    lf2 = len_trim(filnm2)
    !
    ! open the source file
    !
    iocond = -1
    iter = 0
    do while (iocond/=0 .and. iter<10)
       inquire (file = filnm1(:lf1), exist = ex, opened = opend1, number = lunf1, iostat = iocond)
       iter = iter+1
       if (iocond/=0) call cutil_sleep(100)
    enddo
    if (iocond/=0) then
       nuerr = 4
       return
    endif
    if (.not.ex) then
       nuerr = 1
       return
    elseif (opend1) then
       rewind (lunf1)
    else
       open (newunit = lunf1, file = filnm1(:lf1), form = 'formatted', status = 'old')
    endif
    !
    ! open the target file in replace or append mode
    !
    iocond = -1
    iter = 0
    do while (iocond/=0 .and. iter<10)
       inquire (file = filnm2(:lf2), exist = ex, opened = opend2, number = lunf2, iostat = iocond)
       iter = iter+1
       if (iocond/=0) call cutil_sleep(100)
    enddo
    if (iocond/=0) then
       nuerr = 5
       return
    endif
    if (ex .and. opend2) then
       close(lunf2)
    endif
    if (filtype=='append') then
        open (newunit = lunf2, file = filnm2(:lf2), form = 'formatted', position = 'append')
    elseif (filtype=='copy') then
        open (newunit = lunf2, file = filnm2(:lf2), form = 'formatted', status = 'replace')
    else
        nuerr = 2
        return
    endif
    !
    ! copy the lines
    !
    do
        rec132 = ' '
        read (lunf1, '(A)', iostat = iocond) rec132
        if (iocond/=0) then
           if (iocond>0) nuerr = 3
           exit
        endif
        write (lunf2, '(A)') trim(rec132)
    enddo
    !
    ! close both files
    !
    close (lunf1, iostat = iocond)
    close (lunf2, iostat = iocond)
end subroutine cp_file
