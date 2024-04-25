subroutine grdfil(lundia    ,lungrd    ,error     ,filgrd    ,fmttmp    , &
                & flgrd     ,gdp       )
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
!    Function: Reads the COMPUTATIONAL GRID ENCL. from the attri-
!              bute file
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use system_utils, only: exifil
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
!
! Global variables
!
    integer                    :: lundia !  Description and declaration in inout.igs
    integer      , intent(in)  :: lungrd !  Description and declaration in luntmp.igs
    logical      , intent(out) :: flgrd  !  Description and declaration in tmpfil.igs
    logical                    :: error  !!  Flag=TRUE if an error is encountered
    character(*)               :: filgrd !!  Name of the relevant file
    character(11), intent(in)  :: fmttmp !!  Help var. for the attribute file formats (eg. the grid file)
!
!
! Local variables
!
    integer                        :: iocond  ! Help var. for iostat condition 
    integer                        :: lfile   ! Length of file name 
    integer                        :: luntmp  ! Unit number for attribute file 
    integer                        :: m       ! Help var. 
    integer, external              :: newlun
    integer, dimension(2)          :: ival    ! Help array (int.) where the data, recently read from the MD-file, are stored temporarily 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !
    !-----test file existence, if so read
    !
    lfile = len(filgrd)
    !
    if (exifil(filgrd, lundia)) then
       open (newunit=luntmp, file = filgrd(1:lfile), form = fmttmp, status = 'old')
       !
       !--------unformatted file
       !
       if (fmttmp(:2)=='un') then
       !
       !-----------read ival  and write to semi-scratch file
       !           end_of_file ok, error condition not ok
       !
       ! -->
  110     continue
          read (luntmp, iostat = iocond) (ival(m), m = 1, 2)
          if (iocond/=0) then
             if (iocond>0) then
                error = .true.
                call prterr(lundia    ,'G007'    ,filgrd(1:lfile)      )
             !
             endif
             goto 200
          endif
          write (lungrd) (ival(m), m = 1, 2)
          goto 110
       ! <--
       !
       !-----------stop reading file
       !
       !
       !--------freeformatted file
       !
       else
          !
          !-----------skip lines starting with a '*'
          !
          call skipstarlines(luntmp    )
          !
          !-----------read ival  and write to semi-scratch file
          !           end_of_file ok, error condition not ok
          !
          ! -->
  210     continue
          read (luntmp, *, iostat = iocond) (ival(m), m = 1, 2)
          if (iocond/=0) then
             if (iocond>0) then
                error = .true.
                call prterr(lundia    ,'G007'    ,filgrd(1:lfile)      )
             !
             endif
             goto 200
          endif
          write (lungrd) (ival(m), m = 1, 2)
          goto 210
       ! <--
       !
       !-----------stop reading file
       !
       endif
       !
       !--------close file and set flag
       !
  200  continue
       close (luntmp)
       !
       flgrd = .true.
       if (error) flgrd = .false.
    else
       !
       !-----test file existence <NO>
       !
       error = .true.
    endif
end subroutine grdfil
