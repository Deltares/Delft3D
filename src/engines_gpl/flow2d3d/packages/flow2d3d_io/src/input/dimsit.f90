subroutine dimsit(lunmd     ,lundia    ,error     ,nrrec     ,nosite    , &
                & keyw      ,gdp       )
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
!    Function: Reads the dimension for site definitions from
!              the MD-file
!              Site definitions can be - monitoring stations
!                                      - cross-sections
!                                      - drogue tracks
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
    integer , pointer :: itis
!
! Global variables
!
    integer                    :: lundia  ! Description and declaration in inout.igs
    integer                    :: lunmd   ! Description and declaration in inout.igs
    integer                    :: nosite  ! Number of elevation points, can be monitoring stations, cross-sections or drogue track definitions
    integer                    :: nrrec   ! Record counter keeping the track of the last record read
    logical      , intent(out) :: error   ! Flag=TRUE if an error is encountered
    character(6)               :: keyw    ! Key word to define the type of "site"
!
!
! Local variables
!
    integer                 :: iocond  ! Reading condition, should be 0 
    integer                 :: lenc    ! Number of char. to read in string 
    integer                 :: lfile   ! Number of non blank characters of file name 
    integer                 :: lkw     ! Length of keyword (:= 6) 
    integer                 :: luntmp  ! Unit number of FILTMP 
    integer, external       :: newlun
    integer                 :: nlook   ! Nr. of values to look for in a record 
    integer                 :: ntrec   ! Current record counter. It's value is changed to detect if all records in the MD-file have been read 
    logical                 :: found   ! Flag is true if KEYW is found 
    logical                 :: lerror  ! Flag=TRUE if an local error is encountered
    logical                 :: newkw   ! Flag to specify if the keyword to look for is a new keyword 
    character(11)           :: fmtdef  ! Default format of an attribute file = blank 
    character(11)           :: fmttmp  ! Format of FILTMP (UN/FRee formatted) 
    character(12)           :: fildef  ! Default file name = blank 
    character(20)           :: cdef    ! Default value for chulp 
    character(20)           :: chulp   ! Help variable to read character from MD-file 
    character(256)          :: filtmp  ! Attribute file name 
    character(300)          :: mdfrec  ! Record read from the MD-file 300 = 256 + a bit (field, =, ##, etc.) 
!
!
!! executable statements -------------------------------------------------------
!
    !
    itis  => gdp%gdrdpara%itis
    !
    mdfrec = ' '
    fmttmp = ' '
    fildef = ' '
    filtmp = fildef
    fmtdef = 'FRformatted'
    lfile  = 12
    lkw    = 6
    nlook  = 1
    cdef   = ' '
    chulp  = cdef
    lerror = .false.
    newkw  = .true.
    found  = .true.
    !
    ! locate 'Fil"site"' record in case site definition is written in an attribute file
    ! for old files 'Fil"site"' is not found, which leads to filsit = ' '
    !
    ntrec = nrrec
    nlook = 1
    call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
              & 'NO'      )
    !
    if (found) then
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,filtmp    ,fildef    ,lfile     ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          error = .true.
          goto 520
       endif
    endif
    if (filtmp/=fildef) then
       !
       ! Site definition in attribute file? <YES>
       ! locate 'Fmt"site"' record for format definition of input file
       ! and then look for file format (unformatted / freeformatted)
       !
       keyw(1:3) = 'Fmt'
       ntrec = nrrec
       lenc = 11
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,fmttmp    ,fmtdef    ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
          fmttmp = 'FRformatted'
       endif
       call filfmt(lundia    ,keyw      ,fmttmp    ,lerror    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
          fmttmp = 'formatted'
       endif
       !
       ! test file existence
       !
       lfile = len(filtmp)
       !
       if (exifil(filtmp, lundia)) then
          !
          ! open input file
          !
          open (newunit=luntmp, file = filtmp(1:lfile), form = fmttmp, status = 'old')
          if (fmttmp(1:2)=='un') then
          !
          ! unformatted file
          ! read record and add 1 to NOSITE till end of file
          !
          ! -->
  110        continue
             read (luntmp, iostat = iocond) chulp
             !
             ! error while reading IOCOND>0, End-Of-File IOCOND<0
             !
             if (iocond/=0) then
                if (iocond<0) goto 310
                call prterr(lundia    ,'G007'    ,filtmp(:lfile)       )
                !
                error = .true.
                goto 520
             endif
             !
             ! site definition found (non-blanks in record part)
             !
             if (chulp/=cdef) then
                nosite = nosite + 1
                goto 110
             ! <--
             endif
          else
             !
             ! freeformatted file, skip lines starting with a '*'
             !
             call skipstarlines(luntmp    )
             !
             ! read record and add 1 to NOSITE till end of file
             !
             ! -->
  210        continue
             read (luntmp, '(a)', iostat = iocond) chulp
             !
             ! error while reading IOCOND>0, End-Of-File IOCOND<0
             !
             if (iocond/=0) then
                if (iocond<0) goto 310
                call prterr(lundia    ,'G007'    ,filtmp(:lfile)       )
                !
                error = .true.
                goto 520
             endif
             !
             ! site definition found (non-blanks in record part)
             !
             if (chulp/=cdef) then
                nosite = nosite + 1
                goto 210
             ! <--
             endif
          endif
          !
          ! close file
          !
  310     continue
          close (luntmp)
       else
          !
          ! file does not exist !!
          !
          error = .true.
       endif
    else
       !
       ! Site definition in attribute file? <NO>
       !
       !
       ! locate 'Nam"site"' record for name site definition
       !
       chulp = cdef
       keyw(1:3) = 'Nam'
       if (keyw(4:6)=='sta') keyw(4:6) = 'st '
       nlook = 0
       newkw = .true.
       ntrec = nrrec
       call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
                 & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
                 & 'NO'      )
       if (found) then
          !
          ! If found count number of site definitions
          !
          lenc = 20
          ! -->
  510     continue
          call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                    & mdfrec    ,chulp     ,cdef      ,lenc      ,nrrec     , &
                    & ntrec     ,lundia    ,gdp       )
          !
          if (lerror .or. nlook<0) then
             error = .true.
             goto 520
          endif
          if (nlook==999) goto 520
          !
          ! site definition found (non-blanks in record part)
          !
          if (chulp/=cdef) then
             nosite = nosite + 1
             !
             ! locate next 'Nam"site"' record for name site definition
             !
             newkw = .false.
             goto 510
          endif
       ! <--
       !
       endif
    endif
    !
  520 continue
end subroutine dimsit
