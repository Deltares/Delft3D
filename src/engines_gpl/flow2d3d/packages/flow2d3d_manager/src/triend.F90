subroutine triend(runid, gdp)
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
!    Function: Ends the FLOW program in a structured way by:
!              - Counts the errors and warnings in the DIAGNOSTIC
!                file
!              - Shows the counted errors/warnings on screen
!              - Closes the diagnostic file
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use string_module
    use dfparall
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                , pointer :: lundia
    integer                , pointer :: lunprt
    integer                , pointer :: lunscr
    integer                , pointer :: iphisi
    integer , dimension(:) , pointer :: ipmap
!
! Global variables
!
    character(*)              :: runid  !!  Run identification code for the current simulation
!
! Local variables
!
    integer        :: icount  ! help var.; counter 
    integer        :: iocond  ! IO condition
    integer        :: itis
    integer        :: lkw     ! Length of char. str (usually the KEYWRD or RECNAM) 
    integer        :: lrid    ! Lenght of runid 
    integer        :: lridmx  ! LRID max. for prt-file 
    integer        :: merr    ! Total number of errors found in the diagnostic file 
    integer        :: mwarn   ! Total number of warnings found in the diagnostic file
    integer        :: n
    integer        :: nrrec   ! Pointer to the record number in the MD-file 
    integer        :: ntel    ! Total number of errors and warnings found in the diagnostic file 
    integer        :: ntrec   ! Help. var to keep track of NRREC 
    integer        :: reclen  ! Help var.; length of recdia 
    logical        :: found   ! FOUND=TRUE if recnam in the MD-file was found 
    logical        :: lerror  ! Help logical dummy var. 
    logical        :: newkw   ! Logical var. specifying whether a new recnam should be read from the MD-file or just new data in the continuation line 
    logical        :: opend   ! Help logical var. to determine whether each of the output file was opened 
    character(10)  :: date   ! Date to be filled in the header 
    character(20)  :: rundat  ! Current date and time containing a combination of DATE and TIME 
    character(300) :: recdia  ! Help var. to store the records read from the diagnostic file 300 = 256 + a bit (field, =, ##, etc.) 
    character(78)  :: txtfil
    character(9)   :: keyw    ! Name of record to look for in the MD-file (usually KEYWRD or RECNAM) 
    integer        :: itag    ! help variable to synchronise errors/warnings summaries to screen
    integer        :: itrig   ! help variable to synchronise errors/warnings summaries to screen
!
!! executable statements -------------------------------------------------------
!
    lundia  => gdp%gdinout%lundia
    lunprt  => gdp%gdinout%lunprt
    lunscr  => gdp%gdinout%lunscr
    iphisi  => gdp%gdinttim%iphisi
    ipmap   => gdp%gdinttim%ipmap
    !
    txtfil(1:40)  = '----------------------------------------'
    txtfil(41:78) = '--------------------------------------'
    !
    ! define lenght of RUNID
    !
    call remove_leading_spaces(runid     ,lrid      )
    !
    ! if NUERR = 1 then error occurred in TRIPOI part
    !
    if (gdp%errorcode /= 1) then
       !
       ! Writing of CPU-time and performance is replaced by timers
       ! See libsrc/flow_modsrc/timers.f90
       !
    else
       !
       ! Test if LUNDIA is an opened file
       ! NUERR = 1 then error occurred in SYSINI, DECARR or CHKLOC
       ! if occured in SYSINI LUNDIA may not be defined.
       !
       inquire (lundia, opened = opend)
       if (.not.opend) goto 9999
    endif
    !
    ! Date and time
    !
    call dattim(rundat    )
    date(1:4)  = rundat(1:4)
    date(5:5)  = '-'
    date(6:7)  = rundat(6:7)
    date(8:8)  = '-'
    date(9:10) = rundat(9:10)
    !
    ! put end date and time LUNDIA
    !
    write (lundia, '(a)')
    write (lundia, '(80a1)') ('*', n = 1, 80)
    write (lundia, '(a)')   '***'
    write (lundia, '(a)')   '*** FINISHED    Delft3D-FLOW'
    write (lundia, '(2a)')  '***             runid      : ', trim(runid)
    write (lundia, '(4a)')  '***             date, time : ', date, ',', rundat(11:19)
    write (lundia, '(a)')   '***'
    write (lundia, '(80a1)') ('*', n = 1, 80)
    write (lundia, '(a)')
    !
    if (iphisi > 0 .or. ipmap(1) > 0) then
       !
       ! put end date and time LUNPRT
       !
       lridmx = min(lrid, 97)
       write (lunprt, '(a)')
       write (lunprt, '(a,t129,a,t32,a)') '*** End   of flow for model:', '***', &
                                        & runid(:lridmx)
       write (lunprt, '(a,t11,a,a,a,t129,a)') '*** ', date, ' ', rundat(11:19), '***'
    endif
    !
    ! put date and time on screen
    !
    if (.not.parll .or. (parll .and. inode == master)) then
    write (lunscr, *)
    write (lunscr, '(a)') txtfil
    write (lunscr, '(a)')  '  FINISHED    Delft3D-FLOW'
    write (lunscr, '(2a)') '              runid      : ', runid(:lrid)
    write (lunscr, '(4a)') '              date, time : ', date, ',', rundat(11:19)
    endif
    !
    ! count errors in lundia
    !
    itag = 2
    ! NB value is not relevant anyway
    itrig = 0
    if (parll .and. inode>1) then
       !
       ! blocks until triggered by lower ranked proc.
       !
       call dfrecvnb(itrig,1,dfint,inode-1,itag,gdp)
    endif
    if (parll) then
       write(lunscr,'(a,i3)')'SUMMARY FOR PARTITION :',inode
    endif
    ntel  = 0
    !
    keyw  = '*** ERROR'
    nrrec = 1
    ntrec = 1
    itis  = 20
    newkw = .false.
    lkw   = 9
    merr  = 0
    !
    rewind (lundia)
    ! -->
 1000 continue
    call search(lundia    ,lerror    ,newkw     ,nrrec     ,found     , &
              & ntrec     ,recdia    ,itis      ,keyw      ,lkw       , &
              & 'NOPR'    )
    if (found) then
       merr = merr + 1
       ntel = ntel + 1
       if (ntel<=10) then
          call remove_leading_spaces(recdia    ,reclen    )
          if (reclen<=79) then
             write (lunscr, '(a)') recdia(:reclen)
          else
             write (lunscr, '(a)') recdia(1:79)
             icount = 0
 1100        continue
             icount = icount + 1
             if (reclen<=(79 + icount*67)) then
                write (lunscr, '(a3,9X,a)') &
                    & '***', recdia((13 + icount*67):reclen)
             else
                write (lunscr, '(a3,9X,a)') &
                    & '***', recdia((13 + icount*67):(79 + icount *67))
                goto 1100
             endif
          endif
       endif
       goto 1000
    ! <--
    endif
    !
    ! count warnings in lundia
    !
    keyw  = '*** WARNI'
    nrrec = 1
    ntrec = 1
    lkw   = 9
    mwarn = 0
    !
    rewind (lundia)
    ! -->
 2000 continue
    call search(lundia    ,lerror    ,newkw     ,nrrec     ,found     , &
              & ntrec     ,recdia    ,itis      ,keyw      ,lkw       , &
              & 'NOPR'    )
    if (found) then
       mwarn = mwarn + 1
       ntel = ntel + 1
       if (ntel<=10) then
          call remove_leading_spaces(recdia    ,reclen    )
          if (reclen<=79) then
             write (lunscr, '(a)') recdia(:reclen)
          else
             write (lunscr, '(a)') recdia(1:79)
             icount = 0
 2100        continue
             icount = icount + 1
             if (reclen<=(79 + icount*67)) then
                write (lunscr, '(a3,9X,a)') &
                    & '***', recdia((13 + icount*67):reclen)
             else
                write (lunscr, '(a3,9X,a)') &
                    & '***', recdia((13 + icount*67):(79 + icount *67))
                goto 2100
             endif
          endif
       endif
       goto 2000
    ! <--
    endif
    !
    if (ntel>10) then
       write (lunscr, *) ' *** Too many errors and warnings',                   &
                        & ' to write to screen !! '
    endif
    write (lunscr, '(i5,a,i5,a)') merr, ' errors and ', mwarn, ' warnings'
    write (lunscr, '(2a)') 'returning to main program from domain ',            &
                         & runid(1:lrid)
    write (lunscr, '(a)') txtfil
    if (parll .and. inode<nproc) then
       !
       ! sends trigger to higher ranked proc.
       !
       call dfsendnb(itrig,1,dfint,inode+1,itag,gdp)
    endif
    !
    ! read until end of tri-diag file such that the final messages written to
    ! that file (about deleting TMP files) are written at the end and do not
    ! overwrite the file
    !
    iocond = 0
    do while (iocond == 0)
       read (lundia, '(a300)', iostat = iocond) recdia
    enddo
    !
    ! One backspace needed to avoid writing after EOF character
    ! The GNU compiled version crashes on that
    !
    backspace (lundia)
 9999 continue
end subroutine triend
