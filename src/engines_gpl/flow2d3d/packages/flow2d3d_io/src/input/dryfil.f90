subroutine dryfil(lundia    ,lundry    ,error     ,fildry    ,fmttmp    , &
                & fldry     ,gdp       )
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
!   Reads the dam points from the attribute file
!   Flag fldry is set and returned
! OR
!   Reads the 45 degrees cut cells from the attribute file
!   Flag fldry in this routine corresponds to flag fl45 in the calling routine
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use dfparall
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
    integer      , intent(in)  :: lundry !  Description and declaration in luntmp.igs
    logical      , intent(out) :: fldry  !  Description and declaration in tmpfil.igs
    logical                    :: error  !!  Flag=TRUE if an error is encountered
    character(*)               :: fildry !!  Name of the relevant file
    character(11), intent(in)  :: fmttmp !!  Help var. for the attribute file formats (eg. the thin dams file)
!
! Local variables
!
    integer               :: imnd   ! counter to count dry points inside subdomain
    integer               :: iocond ! Help var. for iostat condition 
    integer               :: lfile  ! Length of file name 
    integer               :: luntmp ! Unit number for attribute file 
    integer               :: m      ! Help var. 
    integer, external     :: newlun
    integer, dimension(4) :: ival   ! Help array (int.) where the data, recently read from the MD-file, are stored temporarily 
    logical               :: outsd  ! indicating whether all dry points are outside subdomain (.TRUE.) or not (.FALSE.)
    logical               :: onParbndIsInside
    integer, pointer      :: mfg
    integer, pointer      :: nfg
!
!! executable statements -------------------------------------------------------
!
    mfg => gdp%gdparall%mfg
    nfg => gdp%gdparall%nfg
    onParbndIsInside = .true.
    ! test file existence, if so read
    !
    lfile = len(fildry)
    !
    if (exifil(fildry, lundia)) then
       open (newunit=luntmp, file = fildry(1:lfile), form = fmttmp, status = 'old')
       !
       !--------unformatted file
       !
       if (fmttmp(:2)=='un') then
       !
       !-----------read ival  and write to semi-scratch file
       !           end_of_file ok, error condition not ok
       !
       ! -->
          imnd = 0
  110     continue
          read (luntmp, iostat = iocond) (ival(m), m = 1, 4)
          if (iocond/=0) then
             if (iocond>0) then
                error = .true.
                call prterr(lundia    ,'G007'    ,fildry(1:lfile)      )
             !
             endif
             goto 200
          endif
          ival(1) = ival(1) -mfg +1
          ival(2) = ival(2) -nfg +1
          ival(3) = ival(3) -mfg +1
          ival(4) = ival(4) -nfg +1
          !
          ! check if dry points are fully (.TRUE.) or partly (.FALSE.) outside subdomain
          !
          ! Note: for single domain runs, outsd = .FALSE., i.e. dry points are completely inside domain
          !
          call adjlin (ival, outsd, gdp%d%mmax, gdp%d%nmaxus, onParbndIsInside)
          if ( .not. outsd ) then
             imnd = imnd + 1
             write (lundry) (ival(m), m = 1, 4)
          endif
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
          imnd = 0
  210     continue
          read (luntmp, *, iostat = iocond) (ival(m), m = 1, 4)
          if (iocond/=0) then
             if (iocond>0) then
                error = .true.
                call prterr(lundia    ,'G007'    ,fildry(1:lfile)      )
             !
             endif
             goto 200
          endif
          ival(1) = ival(1) -mfg +1
          ival(2) = ival(2) -nfg +1
          ival(3) = ival(3) -mfg +1
          ival(4) = ival(4) -nfg +1
          !
          ! check if dry points are fully (.TRUE.) or partly (.FALSE.) outside subdomain
          !
          ! Note: for single domain runs, outsd = .FALSE., i.e. dry points are completely inside domain
          !
          call adjlin (ival, outsd, gdp%d%mmax, gdp%d%nmaxus, onParbndIsInside)
          if ( .not. outsd ) then
             imnd = imnd + 1
             write (lundry) (ival(m), m = 1, 4)
          endif
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
       fldry = .true.
       if (error .or. imnd==0) fldry = .false.
    else
       !
       !-----test file existence <NO>
       !
       error = .true.
    endif
end subroutine dryfil
