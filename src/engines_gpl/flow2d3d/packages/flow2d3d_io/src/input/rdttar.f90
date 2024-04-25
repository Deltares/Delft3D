subroutine rdttar(filnam    ,lundia    ,error     ,nttaru    ,ittaru    , &
                & rttaru    ,gdp       )
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
! Reads trachytope data from the
! trachytopes area file in U-direction.
! Routine is called for U/V-direction
! respectively.
!
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use string_module
    use system_utils, only: exifil
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
!
! Local parameters
!
    integer, parameter :: maxfld = 10
!
! Global variables
!
    integer                              :: lundia
    integer                              :: nttaru
    integer       , dimension(nttaru, 3) :: ittaru
    logical                              :: error
    real(fp)      , dimension(nttaru)    :: rttaru
    character(256)                       :: filnam
!
! Local variables
!
    integer                                     :: i
    integer                                     :: ibeg
    integer                                     :: iblbeg
    integer                                     :: iend
    integer                                     :: iocond
    integer                                     :: lcurec
    integer                                     :: lfile
    integer                                     :: luntmp
    integer                                     :: mcurec
    integer                                     :: mttaru
    integer                                     :: nblcks
    integer                                     :: nrflds
    integer       , dimension(4)                :: nmpblk
    integer       , dimension(maxfld)           :: ifield
    integer       , dimension(maxfld)           :: itype
    integer       , dimension(maxfld)           :: lenchr
    integer                          , external :: newlun
    logical                                     :: leql
    logical                                     :: lfirst
    logical                                     :: lokay
    logical                                     :: lprblk
    real(fp)      , dimension(maxfld)           :: rfield
    character(10) , dimension(maxfld)           :: cfield
    character(132)                              :: rec132
    character(300)                              :: errmsg
!
!! executable statements -------------------------------------------------------
!
    ! initialize
    !
    lfirst = .true.
    do i = 1, 4
       nmpblk(i) = 0
    enddo
    !
    ! test file existence
    !
    call remove_leading_spaces(filnam    ,lfile     )
    if (.not.exifil(filnam, lundia)) then
       !
       ! file does not exist !!
       !
       call prterr(lundia    ,'P101'    ,filnam(1:lfile)      )
       error = .true.
       goto 9999
    endif
    !
    ! open file
    !
    open (newunit=luntmp, file = filnam(1:lfile), form = 'formatted', iostat = iocond,  &
        & status = 'old')
    if (iocond/=0) then
       call prterr(lundia    ,'U015'    ,filnam(1:lfile)      )
       error = .true.
       goto 9999
    endif
    !
    ! freeformatted file
    !       read record and count number of useful areas
    !       till end of file
    !
    mttaru = 0
    lprblk = .false.
    mcurec = 0
    iblbeg = -1
    nblcks = 0
    ! -->
    !
    ! read line
    !
  210 continue
    read (luntmp, '(a)', iostat = iocond) rec132
    if (iocond==0) then
       mcurec = mcurec + 1
    else
       !
       ! End-of-file ?
       !
       if (iocond<0) then
          !
          ! Check on pending blocks
          !
          if (iblbeg>=0) then
             call expblk(lundia    ,ittaru    ,rttaru    ,nttaru    ,iblbeg    , &
                       & nblcks    ,nmpblk    ,error     ,gdp       )
          endif
          ! <--
          !
          ! Exit
          !
          !
          ! close file
          !
          close (luntmp)
          goto 9999
       endif
       !
       ! Reading error
       !
       error = .true.
       rec132 = ' '
       write (rec132, '(i12)') mcurec + 1
       call remove_leading_spaces(rec132    ,lcurec    )
       errmsg = filnam(1:lfile) // ', Record: ' // rec132(1:lcurec)
       call prterr(lundia    ,'G007'    ,errmsg    )
       close (luntmp)
       goto 9999
    endif
    !
    ! Interpret line ...
    !
    !
    ! Comment line
    !
    if ((rec132(1:1)=='*') .or. (rec132(1:1)=='#')) goto 210
    !
    ! Check for five integers: a block
    !
    ibeg = 1
    iend = 132
    call scannr(rec132    ,ibeg      ,iend      ,nrflds    ,itype     , &
              & ifield    ,rfield    ,cfield    ,lenchr    ,maxfld    , &
              & .true.    ,.true.    ,.true.    )
    !
    ! When no sub-fields are found, record appears to be empty
    !
    if (nrflds==0) goto 210
    !
    ! Check the contents
    !
    lokay = .false.
    !
    ! Check if it is a valid single point record
    !
    if (nrflds==4 .and. itype(1)==1 .and. itype(2)==1 .and. itype(3)==1 .and.   &
      & (itype(4)==2 .or. itype(4)==1)) then
       lokay = .true.
    endif
    !
    ! Check if it is a valid block record
    !
    if (nrflds==6 .and. itype(1)==1 .and. itype(2)==1 .and. itype(3)==1 .and.   &
      & itype(4)==1 .and. itype(5)==1 .and. (itype(6)==2 .or. itype(6)==1)) then
       lokay = .true.
    endif
    !
    if (.not.lokay) then
       !
       ! Cannot interpret line
       !
       error = .true.
       rec132 = ' '
       write (rec132, '(i12)') mcurec
       call remove_leading_spaces(rec132    ,lcurec    )
       errmsg = filnam(1:lfile) // ', Record: ' // rec132(1:lcurec)
       call prterr(lundia    ,'G007'    ,errmsg    )
       close (luntmp)
       goto 9999
    endif
    !
    if (nrflds==4) then
       !
       ! Handle pending block if applicable
       !
       if (lprblk) then
          !
          ! Expand block data
          !
          call expblk(lundia    ,ittaru    ,rttaru    ,nttaru    ,iblbeg    , &
                    & nblcks    ,nmpblk    ,error     ,gdp       )
          if (error) then
             close (luntmp)
             goto 9999
          endif
          !
          ! Reset block administration
          !
          iblbeg = -1
          nblcks = 0
          !
          ! Block separator
          !
          mttaru = mttaru + 1
          if (mttaru>nttaru) then
             call prterr(lundia    ,'J006'    ,filnam(1:lfile)      )
             error = .true.
             close (luntmp)
             goto 9999
          endif
          !
          ! Store block separator values
          !
          ittaru(mttaru, 1) = -1
          ittaru(mttaru, 2) = -1
          ittaru(mttaru, 3) = -1
          rttaru(mttaru) = -1.0
          !
          lprblk = .false.
       endif
       !
       ! Increment MTTARU for one point
       !
       mttaru = mttaru + 1
       if (mttaru>nttaru) then
          call prterr(lundia    ,'J006'    ,filnam(1:lfile)      )
          error = .true.
          close (luntmp)
          goto 9999
       endif
       !
       ! Store data read into arrays
       !
       ittaru(mttaru, 1) = ifield(1)
       ittaru(mttaru, 2) = ifield(2)
       ittaru(mttaru, 3) = ifield(3)
       if (itype(4)==2) then
          rttaru(mttaru) = rfield(4)
       else
          rttaru(mttaru) = real(ifield(4),fp)
       endif
    else
       !
       ! Reserve space for block separators
       ! Not for first record!!
       !
       if (.not.lfirst) then
          if (lprblk) then
             leql = .true.
             do i = 1, 4
                leql = leql .and. (ifield(i)==nmpblk(i))
             enddo
             if (.not.leql) then
                !
                ! New block, expand pending one first
                !
                call expblk(lundia    ,ittaru    ,rttaru    ,nttaru    ,iblbeg    , &
                          & nblcks    ,nmpblk    ,error     ,gdp       )
                if (error) then
                   close (luntmp)
                   goto 9999
                endif
                !
                ! Reset block administration
                !
                iblbeg = -1
                nblcks = 0
                !
                ! Block separator
                !
                mttaru = mttaru + 1
                if (mttaru>nttaru) then
                   call prterr(lundia    ,'J006'    ,filnam(1:lfile)      )
                   error = .true.
                   close (luntmp)
                   goto 9999
                endif
                !
                ! Store block separator values
                !
                ittaru(mttaru, 1) = -1
                ittaru(mttaru, 2) = -1
                ittaru(mttaru, 3) = -1
                rttaru(mttaru) = -1.0
             endif
          else
             !
             ! Block separator
             !
             mttaru = mttaru + 1
             if (mttaru>nttaru) then
                call prterr(lundia    ,'J006'    ,filnam(1:lfile)      )
                error = .true.
                close (luntmp)
                goto 9999
             endif
             !
             ! Store block separator values
             !
             ittaru(mttaru, 1) = -1
             ittaru(mttaru, 2) = -1
             ittaru(mttaru, 3) = -1
             rttaru(mttaru) = -1.0
          endif
       endif
       !
       ! Block administration
       !
       if (iblbeg<0) then
          iblbeg = mttaru
       endif
       nblcks = nblcks + 1
       !
       ! Increment MTTARU for block
       !
       mttaru = mttaru + (abs(ifield(1) - ifield(3)) + 1)                       &
              & *(abs(ifield(2) - ifield(4)) + 1)
       if (mttaru>nttaru) then
          call prterr(lundia    ,'J006'    ,filnam(1:lfile)      )
          error = .true.
          close (luntmp)
          goto 9999
       endif
       !
       ! Store N1, M1 block data in arrays
       !
       ittaru(iblbeg + nblcks, 1) = min(ifield(1), ifield(3))
       ittaru(iblbeg + nblcks, 2) = min(ifield(2), ifield(4))
       ittaru(iblbeg + nblcks, 3) = ifield(5)
       if (itype(6)==2) then
          rttaru(iblbeg + nblcks) = rfield(6)
       else
          rttaru(iblbeg + nblcks) = real(ifield(6),fp)
       endif
       !
       ! Set previous block on
       !
       lprblk = .true.
       !
       ! Save current values
       !
       do i = 1, 4
          nmpblk(i) = ifield(i)
       enddo
    endif
    !
    ! One record read
    !
    lfirst = .false.
    goto 210
 9999 continue
end subroutine rdttar
