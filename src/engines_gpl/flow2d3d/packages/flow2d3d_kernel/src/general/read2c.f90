subroutine read2c(lunmd     ,error     ,keyw      ,newkw     ,nlook     , &
                & record    ,cvar      ,cdefau    ,lenc      ,nrrec     , &
                & ntrec     ,lundia    ,gdp       )
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
!    Function: Reads NLOOK character values with a fixed length
!              from a record of MD-file. NLOOK may equal 0, which
!              implies the nr. or record to be read is undeter-
!              mined on input. The values should be separated by
!              blanks
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer , pointer :: ifis
    integer , pointer :: itis
!
! Global variables
!
    integer                  :: lenc    !!  Help var. (length of var. cvar to be looked for in the MD-file)
    integer                  :: lundia  !  Description and declaration in inout.igs
    integer                  :: lunmd   !  Description and declaration in inout.igs
    integer                  :: nlook   !!  Help var.: nr. of data to look for in the MD-file
    integer                  :: nrrec   !!  Pointer to the record number in the MD-file
    integer                  :: ntrec   !!  Help. var to keep track of NRREC
    logical                  :: error   !!  Flag=TRUE if an error is encountered
    logical                  :: newkw   !!  Logical var. specifying whether a new recnam should be read from the
                                        !!  MD-file or just new data in the continuation line
    character(*), intent(in) :: cdefau
    character(*)             :: cvar    !!  Character variable to read from a record
    character(*)             :: record  !!  Record read from either the MD-file or from the attribute file
    character(6)             :: keyw    !!  Name of record to look for in the MD-file (usually KEYWRD or RECNAM)
!
!
! Local variables
!
    integer     :: ibeg    ! Begin position in the RECORD from where the search for data/record is started 
    integer     :: iend    ! Last position in the RECORD when the searched data/record is finished 
    integer     :: lkw     ! Length of char. str (usually the KEYWRD or RECNAM) 
    integer     :: lrec    ! Help var. containing the length of RECORD 
    logical     :: found   ! If FOUND = TRUE then recnam in the MD-file was found 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    ifis  => gdp%gdrdpara%ifis
    itis  => gdp%gdrdpara%itis
    !
    lrec = len(record)
    lenc = len(cvar)
    cvar = ' '
    lkw = index(keyw, ' ') - 1
    if (lkw<=0) lkw = 6
    found = .false.
    error = .false.
    ibeg = itis
    !
    !-----look for keyword in record
    !
    call search(lunmd     ,error     ,newkw     ,nrrec     ,found     , &
              & ntrec     ,record    ,ibeg      ,keyw      ,lkw       , &
              & 'NO'      )
    !
    !-----if not found then check nlook
    !     nlook > 0, then wrong text, else no text but ok
    !
    if (.not.found) then
       if (nlook>0) then
          !
          ! prterr is removed because it generates too much messages/warnings.
          !
          ! call prterr(lundia    ,'U100'    ,keyw      )
          !
          nlook = -1
          error = .true.
       else
          error = .false.
          nlook = 999
       endif
    else
       !
       !--------calculate part of mdfrec with text to search
       !
       ibeg = ifis
       call txtmrk(record    ,lrec      ,ibeg      ,iend      ,error     )
       !
       !--------read cvar, if blank set to default value
       !
       if (.not.error) then
          if (record(ibeg:iend)/=' ') then
             call txtstr(record    ,ibeg      ,iend      ,lenc      ,cvar      , &
                       & error     )
          else
             cvar = cdefau
          endif
       endif
       if (error) then
          call prterr(lundia    ,'U036'    ,keyw      )
          !
          nlook = -1
       endif
    endif
end subroutine read2c
