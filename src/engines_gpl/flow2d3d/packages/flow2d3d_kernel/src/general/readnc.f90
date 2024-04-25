subroutine readnc(lunmd     ,error     ,keyw      ,newkw     ,nlook     , &
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
!              from ONE record of MD-file.
!              nlook may NOT be 0
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
    integer                                 :: lenc   !!  Help var. (length of var. cvar to be looked for in the MD-file)
    integer                                 :: lundia !  Description and declaration in inout.igs
    integer                                 :: lunmd  !  Description and declaration in inout.igs
    integer                                 :: nlook  !!  Help var.: nr. of data to look for in the MD-file
    integer                                 :: nrrec  !!  Pointer to the record number in the MD-file
    integer                                 :: ntrec  !!  Help. var to keep track of NRREC
    logical                                 :: error  !!  Flag=TRUE if an error is encountered
    logical                                 :: newkw  !!  Logical var. specifying whether a
                                                      !!  new recnam should be read from the
                                                      !!  MD-file or just new data in the
                                                      !!  continuation line
    character(*)               , intent(in) :: cdefau
    character(*)                            :: record !!  Record read from either the MD-file or from the attribute file
    character(*), dimension(*)              :: cvar   !!  Character variable to read from a record
    character(6)                            :: keyw   !!  Name of record to look for in the
                                                      !!  MD-file (usually KEYWRD or RECNAM)
!
!
! Local variables
!
    integer     :: ibeg       ! Begin position in the RECORD from where the search for data/record is started 
    integer     :: iend       ! Last position in the RECORD when the searched data/record is finished 
    integer     :: iendm
    integer     :: iocond
    integer     :: lenc0
    integer     :: lkw        ! Length of char. str (usually the KEYWRD or RECNAM) 
    integer     :: lrec       ! Help var. containing the length of RECORD 
    integer     :: n
    integer     :: nn
    integer     :: nnlook     ! Help var.: nr. of data really read from the MD-file 
    logical     :: found      ! If FOUND = TRUE then recnam in the MD-file was found 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    ifis  => gdp%gdrdpara%ifis
    itis  => gdp%gdrdpara%itis
    !
    lrec   = len(record)
    lkw    = index(keyw, ' ') - 1
    if (lkw<=0) lkw = 6
    found  = .false.
    error  = .false.
    lenc0  = lenc
    nnlook = nlook
    !
    !-----look for keyword in record
    !
    ibeg = itis
    call search(lunmd     ,error     ,newkw     ,nrrec     ,found     , &
              & ntrec     ,record    ,ibeg      ,keyw      ,lkw       , &
              & 'NO'      )
    !
    !-----if not found then check nlook
    !     nlook > 0, then wrong text, else no text but ok
    !
    if (.not.found) then
       call prterr(lundia    ,'U100'    ,keyw      )
       !
       error = .true.
       goto 999
    endif
    !
    !-----read value if ier <> 0 then no value not allowed =>
    !     because txtmrk creates string between ##, the initialisation of
    !     iendm should defined 1 position back
    !
    n = 0
    iendm = ifis - 1 - 1
    !-->
   10 continue
    n      = n + 1
    nnlook = n
    ibeg   = iendm + 2
    !
    !--------calculate part of mdfrec with text to search
    !
    call txtmrk(record    ,lrec      ,ibeg      ,iend      ,error     )
    iendm = iend
    !
    !--------read cvar, if blank set to default value
    !
    if (.not.error) then
       if (record(ibeg:iend)/=' ') then
          call txtstr(record    ,ibeg      ,iend      ,lenc      ,cvar(n)   , &
                    & error     )
          lenc = lenc0
       else
          cvar(n) = cdefau
       endif
    elseif (nlook==1) then
       error = .true.
    else
       error = .false.
       read (lunmd, '(a300)', iostat = iocond) record
       !
       !--------------iocond = 0, read continuation record
       !                          ensure the record is all blanks before = sign
       !                          and reset value of n (=> n < nlook per def.)
       !                     < 0, end of file encountered (not ok)
       !                     > 0, error occurred (not ok)
       !
       if (iocond==0) then
          nrrec = nrrec + 1
          if (record(:itis)/=' ') then
             do nn = n, nlook
                cvar(nn) = cdefau
             enddo
             nnlook = n - 1
             n = nlook
          else
             iendm = ifis - 1 - 1
             n = n - 1
          endif
       else
          error = .true.
       endif
    endif
    !
    if (error) then
       call prterr(lundia    ,'U036'    ,keyw      )
       !
       goto 999
    endif
    if (n<nlook) goto 10
    ! <--
    !
  999 continue
    nlook = nnlook
end subroutine readnc
