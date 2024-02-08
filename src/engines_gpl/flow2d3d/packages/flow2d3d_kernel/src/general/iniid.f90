subroutine iniid(error     ,prgnm     ,runid     ,filmd     ,filmrs    , &
               & gdp       )
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
!    Function: - Reads RUN IDentification code from file 'runid'
!              - Opens MD-file, and one of the following :
!                MD-diag.runid, TD-diag.runid or TRI-diag.runid
!              - If error occurred (error = TRUE) then an error mes-
!                sage will be put on the screen
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    use dfparall
    !
    use globaldata
    use string_module
    use system_utils, only: exifil
    use MessageHandling, only: SetMessageHandling
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer , pointer :: lunmd
    integer , pointer :: lundia
    integer , pointer :: lunprt
    integer , pointer :: lunscr
!
! Global variables
!
    logical      , intent(out) :: error  !  Flag=TRUE if an error is encountered
    character(*)               :: filmd  !  File name for MD FLOW file
    character(*)               :: runid  !  Run identification
    character(12), intent(in)  :: filmrs !  File name for DELFT3D_MOR FLOW input file (MD-flow.xxx)
    character(6) , intent(in)  :: prgnm  !  Help var. determining the prog. name currently active
!
! Local variables
!
    integer           :: iocond
    integer           :: lfil
    integer           :: linod             ! Help variable to extend length file name with node number
    integer           :: lrid              ! Help var. to determine the actual length of RUNID 
    integer           :: lunid             ! Unit nr. for the file 'runid' where the runid is specified : 'pc  ' or 'unix' 
    integer           :: luntmp
    integer           :: n
    integer, external :: newlun
    integer           :: nrec
    integer           :: pos               ! Help var. for adjusting runid 
    logical           :: ex                ! Help flag = TRUE when file is found 
    logical           :: found             ! Flag to see if MD file name is found
    logical           :: mdfile_ptr_isnull ! F: Contents of mdfile are already placed in gdp%mdfile_ptr
    logical           :: opend             ! Help flag = TRUE when file is still open (DELFT3D) 
    character(256)    :: errmsg            ! String containing error messages 
    character(256)    :: filtmp            ! Help var. to specify file name 
    character(5)      :: filid             ! Var. containing file name 'runid' 
    character(512)    :: outlin            ! Output line
!
!! executable statements -------------------------------------------------------
!
    lunmd   => gdp%gdinout%lunmd
    lundia  => gdp%gdinout%lundia
    lunprt  => gdp%gdinout%lunprt
    lunscr  => gdp%gdinout%lunscr
    !
    if (associated(gdp%mdfile_ptr)) then
       !
       ! Contents of mdfile are already placed in gdp%mdfile_ptr
       ! Don't add it again
       !
       mdfile_ptr_isnull = .false.
    else
       mdfile_ptr_isnull = .true.
       call tree_create_node(gdp%input_tree, "md-file", gdp%mdfile_ptr)
    endif
    filmd = ' '
    !
    ! In Delft3D version 3.00 and higher the new convention for mdf
    ! files is <runid>.mdf Older versions use md-file.<runid> or for
    ! Morsys md-flow.<runid>
    ! Open the MD file after testing on the actual name (3 options)
    !
    if (filmrs==' ') then
       !
       ! Initialize file to read RUNID
       !
       filid = 'runid'
       !
       ! Read RUN-id if runid = ' '
       !
       if (runid==' ') then
          if (exifil(filid)) then
             open (newunit=lunid, file = filid, form = 'formatted', status = 'old')
             read (lunid, '(a)') runid
             close (lunid)
          else
             call prterr(lunscr, 'G003', 'dummy')
             error = .true.
             goto 9999
          endif
       endif
       !
       ! Common error: runid =  <runid>.mdf
       ! Remove extension in this case
       !
       lrid = index(runid, '.mdf')
       if (lrid /= 0) then
          runid = runid(:lrid-1)
       endif
       !
       ! Define length of RUNID
       !
       call remove_leading_spaces(runid     ,lrid      )
       ! Overall maximum allowed length is 200
       if (lrid>200) then
          call prterr(lunscr    ,'G907'    ,'.mdf, md-file. and md-flow.'   )
       endif
       found = .false.
       !
       ! First look for <runid>.mdf
       !
       lfil = lrid + 4
       filmd(1:lfil) = runid(1:lrid) // '.mdf'
       inquire (file = filmd(1:lfil), exist = ex)
       if (ex) then
          found = .true.
          !
          ! New way of reading md-file: use property read library
          ! Subroutine prop_file must be called before the md-file
          ! is opened for a long time.
          !
          if (mdfile_ptr_isnull) then
             call tree_put_data( gdp%mdfile_ptr, transfer(filmd(1:lfil),node_value), "STRING" )
             call prop_file('ini',filmd(1:lfil),gdp%mdfile_ptr,iocond)
             if (iocond /= 0) then
                write(errmsg,'(i0,a)') iocond,' occured on reading md-file'
                call prterr(lunscr    ,'P004'    ,errmsg   )
                error = .true.
                goto 9999
             endif
          endif
          !
          open (newunit=lunmd, file = filmd(1:lfil), form = 'formatted')
       endif
       !
       ! Second look for md-file.<runid>
       !
       if (.not.found) then
          lfil = lrid + 8
          filmd(1:lfil) = 'md-file.' // runid(1:lrid)
          inquire (file = filmd(1:lfil), exist = ex)
          if (ex) then
             found = .true.
             !
             ! New way of reading md-file: use property read library
             ! Subroutine prop_file must be called before the md-file
             ! is opened for a long time.
             !
             if (mdfile_ptr_isnull) then
                call tree_put_data( gdp%mdfile_ptr, transfer(filmd(1:lfil),node_value), "STRING" )
                call prop_file('ini',filmd(1:lfil),gdp%mdfile_ptr,iocond)
                if (iocond /= 0) then
                   write(errmsg,'(i0,a)') iocond,' occured on reading md-file'
                   call prterr(lunscr    ,'P004'    ,errmsg   )
                   error = .true.
                   goto 9999
                endif
             endif
             !
             open (newunit=lunmd, file = filmd(1:lfil), form = 'formatted')
          endif
       endif
       !
       ! Last look for md-flow.<runid>
       !
       if (.not.found) then
          lfil = lrid + 8
          filmd(1:lfil) = 'md-flow.' // runid(1:lrid)
          inquire (file = filmd(1:lfil), exist = ex)
          if (ex) then
             found = .true.
             !
             ! New way of reading md-file: use property read library
             ! Subroutine prop_file must be called before the md-file
             ! is opened for a long time.
             !
             if (mdfile_ptr_isnull) then
                call tree_put_data( gdp%mdfile_ptr, transfer(filmd(1:lfil),node_value), "STRING" )
                call prop_file('ini',filmd(1:lfil),gdp%mdfile_ptr,iocond)
                if (iocond /= 0) then
                   write(errmsg,'(i0,a)') iocond,' occured on reading md-file'
                   call prterr(lunscr    ,'P004'    ,errmsg   )
                   error = .true.
                   goto 9999
                endif
             endif
             !
             open (newunit=lunmd, file = filmd(1:lfil), form = 'formatted')
          endif
       endif
       !
       ! Nothing found, exit program
       !
       if (.not.found) then
          errmsg = 'MD file for ' // runid(1:lrid)
          call prterr(lunscr, 'G004', errmsg(:12 + lrid))
          error = .true.
          call d3stop (1, gdp)
       endif
    else
       !
       filmd = filmrs
       found = .false.
       !
       ! Check which file name of FLOW is used
       ! Start with .mdf
       !
       lrid = index(filmd, '.mdf')
       if (lrid/=0) then
          lrid = lrid - 1
          if (lrid>len(runid)) then
             error = .true.
             call prterr(lunscr    ,'G907'    ,'.mdf'    )
             goto 9999
          else
             found = .true.
             runid = filmd(1:lrid)
             lfil = lrid + 4
          endif
       endif
       !
       ! Second md-file.
       !
       if (.not.found) then
          lfil = index(filmd, 'md-file.')
          if (lfil/=0) then
             lfil = lfil + 8
             filtmp = filmd(lfil:)
             call remove_leading_spaces(filtmp    ,lrid      )
             if (lrid>len(runid)) then
                error = .true.
                call prterr(lunscr    ,'G907'    ,'md-file.')
                goto 9999
             else
                found = .true.
                runid = filtmp
                lfil = lfil - 1 + lrid
             endif
          endif
       endif
       !
       ! Last md-flow.
       !
       if (.not.found) then
          lfil = index(filmd, 'md-flow.')
          if (lfil/=0) then
             lfil = lfil + 8
             filtmp = filmd(lfil:)
             call remove_leading_spaces(filtmp    ,lrid      )
             if (lrid>len(runid)) then
                error = .true.
                call prterr(lunscr    ,'G907'    ,'md-flow.')
                goto 9999
             else
                found = .true.
                runid = filtmp
                lfil = (lfil - 1) + lrid
             endif
          endif
       endif
       !
       ! No proper file name found
       !
       if (.not.found) then
          error = .true.
          call remove_leading_spaces(filmd     ,lfil      )
          errmsg(1:12) = 'MD file for '
          errmsg(13:lfil + 12) = filmd(1:lfil)
          call prterr(lunscr    ,'G004'    ,errmsg(1:lfil + 12)  )
          goto 9999
       endif
       !
       ! Check file existence
       !
       if (exifil(filmd, lunscr)) then
          inquire (file = filmd(1:lfil), opened = opend)
          if (opend) then
             inquire (file = filmd(1:lfil), number = luntmp)
             close (luntmp)
          endif
          !
          ! New way of reading md-file: use property read library
          ! Subroutine prop_file must be called before the md-file
          ! is opened for a long time.
          !
          if (mdfile_ptr_isnull) then
             call tree_put_data( gdp%mdfile_ptr, transfer(filmd(1:lfil),node_value), "STRING" )
             call prop_file('ini',filmd(1:lfil),gdp%mdfile_ptr,iocond)
             if (iocond /= 0) then
                write(errmsg,'(i0,a)') iocond,' occured on reading md-file'
                call prterr(lunscr    ,'P004'    ,errmsg   )
                error = .true.
                goto 9999
             endif
          endif
          !
          open (newunit=lunmd, file = filmd(1:lfil), form = 'formatted')
       else
          error = .true.
          goto 9999
       endif
    endif
    !
    ! open LUNDIA (depends on value of prgnm = tdatom)
    ! for DELFT3DMOR test if lundia was already in use by tri-diag
    !
    if (prgnm=='tdatom') then
       filtmp(1:9 + lrid) = 'tri-diag.' // runid(1:lrid)
       inquire (file = filtmp(1:9 + lrid), opened = opend)
       if (opend) then
          inquire (file = filtmp(1:9 + lrid), number = luntmp)
          close (luntmp)
       endif
       !
       filtmp(1:8 + lrid) = 'td-diag.' // runid(1:lrid)
       inquire (file = filtmp(1:8 + lrid), exist = ex)
       if (ex) then
          inquire (file = filtmp(1:8 + lrid), opened = opend)
          if (.not.opend) then
             open (newunit=luntmp, file = filtmp(1:8 + lrid), form = 'formatted')
          else
             inquire (file = filtmp(1:8 + lrid), number = luntmp)
          endif
          close (luntmp, status = 'delete')
       endif
       !
       open (newunit=lundia, file = filtmp(1:8 + lrid), form = 'formatted',             &
            & status = 'new')
    !
    ! open LUNDIA (depends on value of prgnm = trisim)
    !
    else
       !
       ! for DELFT3DMOR test if lundia was already in use by td-diag
       ! if so close this file first
       !
       filtmp(1:8 + lrid) = 'td-diag.' // runid(1:lrid)
       inquire (file = filtmp(1:8 + lrid), opened = opend)
       if (opend) then
          inquire (file = filtmp(1:8 + lrid), number = luntmp)
          close (luntmp)
       endif
       !
       ! Initial open for FILMRS = ' ' (not DELFT3D-MOR)
       ! when file tri-prt exists delete it
       !
       if (filmrs==' ') then
          filtmp(1:8 + lrid) = 'tri-prt.' // runid(1:lrid)
          inquire (file = filtmp(1:8 + lrid), exist = ex)
          if (ex) then
             inquire (file = filtmp(1:8 + lrid), opened = opend)
             if (opend) then
                inquire (file = filtmp(1:8 + lrid), number = luntmp)
                close (luntmp, status = 'delete')
             endif
          endif
       endif
       !
       ! if the tri-diag file exists an append should be performed
       ! in case of FLOW in combination with other parts of DELFT3D
       !
       filtmp(1:9 + lrid) = 'tri-diag.' // runid(1:lrid)
       ! append node number to file name in case of parallel computing within single-domain case
       !
       linod = 0
       if ( parll ) then
          linod = 4
          write(filtmp(9+lrid+1:9+lrid+linod),'(a,i3.3)') '-', inode
       endif
       inquire (file = filtmp(1:9 + lrid+linod), exist = ex)
       if (ex .and. .not. parll) then
          !
          ! Not parallel: append to existing tri-diag file
          ! if LUNDIA is closed, then re-open file and read to end of
          ! file before appending (presumed is that the LUNDIA unit
          ! number is coupled to the diagnostic file !!)
          !
          inquire (file = filtmp(1:9 + lrid+linod), opened = opend)
          if (.not.opend) then
             open (newunit=luntmp, file = filtmp(1:9 + lrid+linod), form = 'formatted')
             lundia = luntmp
             nrec = 0
             ! -->
  110        continue
             nrec = nrec + 1
             read (luntmp, '(a)', iostat = iocond)
             if (iocond==0) goto 110
             ! <--
             !
             ! End-of-file encountered, read till end
             ! and write 1 blank line
             !
             if (iocond<0) then
                nrec = nrec - 1
                rewind (luntmp)
                do n = 1, nrec
                   read (luntmp, '(a)') outlin
                enddo
                write (lundia, '(a)')
             else
                !
                ! error occurred while reading, delete file and re-open
                !
                close (lundia, status = 'delete')
                open (newunit=lundia, file = filtmp(1:9 + lrid + linod), form = 'formatted',  &
                     & status = 'new')
             endif
          else
             !
             ! Define unit number
             !
             inquire (file = filtmp(1:9 + lrid+linod), number = lundia)
          endif
       else
          !
          ! Not parallel:
          !    tri-diag file did not exist. Create a new one
          ! Parallel:
          !    appending to existing tri-diag file is not supported
          !    always create a new one, replace one if it existed
          !
          open (newunit=lundia, file = filtmp(1:9 + lrid + linod), form = 'formatted',  &
               & status = 'replace')
       endif
    endif
    call SetMessageHandling(write2screen = .true., useLog = .false., lunMessages = lundia)
    !
 9999 continue
end subroutine iniid
