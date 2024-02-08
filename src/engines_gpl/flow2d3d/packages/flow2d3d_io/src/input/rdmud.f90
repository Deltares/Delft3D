subroutine rdmud(lunmd     ,lundia    ,error     ,nrrec     ,gdp       )
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
!    Function: - Reads parameters Fluid Mud model.
! Method used: Reference: Transport of Fluid Mud, numerical
!              modelling with a two-layer system. Research
!              documentation. November 1995. Deltares.
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
    real(fp)               , pointer :: cbed
    real(fp)               , pointer :: cmud
    real(fp)               , pointer :: fmud
    real(fp)               , pointer :: fwat
    real(fp)               , pointer :: mers
    real(fp)               , pointer :: rhosus
    real(fp)               , pointer :: rhomud
    real(fp)               , pointer :: taubng
    real(fp)               , pointer :: tauers
    real(fp)               , pointer :: tauset
    real(fp)               , pointer :: vdew
    real(fp)               , pointer :: rhow
    real(fp)               , pointer :: rhoa
!
! Global variables
!
    integer               :: lundia !  Description and declaration in inout.igs
    integer               :: lunmd  !  Description and declaration in inout.igs
    integer               :: nrrec  !!  Pointer to the record number in the MD-file
    logical, intent(out)  :: error  !!  Flag=TRUE if an error is encountered
!
!
! Local variables
!
    integer                        :: iocond
    integer                        :: iost
    integer                        :: lenc   ! Help var. (length of character var.) 
    integer                        :: luninp
    integer                        :: nlook  ! Help var.: nr. of data to look for in the MD-file 
    integer                        :: ntrec  ! Help. var to keep track of NRREC 
    integer        , external      :: newlun
    logical                        :: lerror ! Flag=TRUE if a local error is encountered 
    logical                        :: newkw  ! Logical var. specifying whether a new recnam should be read from the MD-file or just new data in the continuation line 
    real(fp)                       :: ws
    character(12)                  :: fildef ! Default file name (usually = blank) 
    character(256)                 :: filnam
    character(300)                 :: mdfrec ! Record read from the MD-file 300 = 256 + a bit (field, =, ##, etc.) 
    character(6)                   :: keyw   ! Name of record to look for in the MD-file (usually KEYWRD or RECNAM) 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    rhow     => gdp%gdphysco%rhow
    rhoa     => gdp%gdphysco%rhoa
    cbed     => gdp%gdmudcoe%cbed
    cmud     => gdp%gdmudcoe%cmud
    fmud     => gdp%gdmudcoe%fmud
    fwat     => gdp%gdmudcoe%fwat
    mers     => gdp%gdmudcoe%mers
    rhosus   => gdp%gdmudcoe%rhosus
    rhomud   => gdp%gdmudcoe%rhomud
    taubng   => gdp%gdmudcoe%taubng
    tauers   => gdp%gdmudcoe%tauers
    tauset   => gdp%gdmudcoe%tauset
    vdew     => gdp%gdmudcoe%vdew
    !
    cbed = 0.0
    cmud = 0.0
    fmud = 0.0
    fwat = 0.0
    mers = 0.0
    rhosus = 0.0
    taubng = 0.0
    tauers = 0.0
    tauset = 0.0
    vdew = 0.0
    ws = 0.0
    !
    !
    !-----initialize local parameters
    !
    newkw = .true.
    nlook = 1
    fildef = ' '
    !
    !-----locate 'FilMud' record; file containing mud parameters
    !
    keyw = 'Filmud'
    ntrec = nrrec
    lenc = 12
    call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
              & mdfrec    ,filnam    ,fildef    ,lenc      ,nrrec     , &
              & ntrec     ,lundia    ,gdp       )
    !
    !
    !-----sediment values in file
    !-----error in READ2C can not occur because it has been tested before in
    !     routine DIMSED
    !     Open file and read data
    !
    open (newunit=luninp, file = filnam, form = 'formatted', status = 'old',            &
        & iostat = iost)
    if (iost==0) then
       !
       !
       !-------Free formatted file, skip lines starting with a '*'
       !
       call skipstarlines(luninp    )
       !
       write (lundia, *) '*** Special feature MUD model ***'
       read (luninp, *, iostat = iocond) cbed
       if (iocond/=0) then
          if (iocond<0) then
             call prterr(lundia    ,'G006'    ,filnam    )
          !
          else
             call prterr(lundia    ,'G007'    ,filnam    )
          !
          endif
          error = .true.
          close (luninp)
          goto 9999
       endif
       write (lundia, '(a,e12.5)') 'cbed   = ', cbed
       !
       read (luninp, *, iostat = iocond) cmud
       if (iocond/=0) then
          if (iocond<0) then
             call prterr(lundia    ,'G006'    ,filnam    )
          !
          else
             call prterr(lundia    ,'G007'    ,filnam    )
          !
          endif
          error = .true.
          close (luninp)
          goto 9999
       endif
       write (lundia, '(a,e12.5)') 'cmud   = ', cmud
       !
       read (luninp, *, iostat = iocond) fmud
       if (iocond/=0) then
          if (iocond<0) then
             call prterr(lundia    ,'G006'    ,filnam    )
          !
          else
             call prterr(lundia    ,'G007'    ,filnam    )
          !
          endif
          error = .true.
          close (luninp)
          goto 9999
       endif
       write (lundia, '(a,e12.5)') 'fmud   = ', fmud
       !
       read (luninp, *, iostat = iocond) fwat
       if (iocond/=0) then
          if (iocond<0) then
             call prterr(lundia    ,'G006'    ,filnam    )
          !
          else
             call prterr(lundia    ,'G007'    ,filnam    )
          !
          endif
          error = .true.
          close (luninp)
          goto 9999
       endif
       write (lundia, '(a,e12.5)') 'fwat   = ', fwat
       !
       read (luninp, *, iostat = iocond) mers
       if (iocond/=0) then
          if (iocond<0) then
             call prterr(lundia    ,'G006'    ,filnam    )
          !
          else
             call prterr(lundia    ,'G007'    ,filnam    )
          !
          endif
          error = .true.
          close (luninp)
          goto 9999
       endif
       write (lundia, '(a,e12.5)') 'mers   = ', mers
       !
       read (luninp, *, iostat = iocond) rhosus
       if (iocond/=0) then
          if (iocond<0) then
             call prterr(lundia    ,'G006'    ,filnam    )
          !
          else
             call prterr(lundia    ,'G007'    ,filnam    )
          !
          endif
          error = .true.
          close (luninp)
          goto 9999
       endif
       write (lundia, '(a,e12.5)') 'rhosus = ', rhosus
       rhoa = rhosus
       !
       read (luninp, *, iostat = iocond) rhomud
       if (iocond/=0) then
          if (iocond<0) then
             call prterr(lundia    ,'G006'    ,filnam    )
          !
          else
             call prterr(lundia    ,'G007'    ,filnam    )
          !
          endif
          error = .true.
          close (luninp)
          goto 9999
       endif
       write (lundia, '(a,e12.5)') 'rhomud = ', rhomud
       rhow = rhomud
       !
       read (luninp, *, iostat = iocond) taubng
       if (iocond/=0) then
          if (iocond<0) then
             call prterr(lundia    ,'G006'    ,filnam    )
          !
          else
             call prterr(lundia    ,'G007'    ,filnam    )
          !
          endif
          error = .true.
          close (luninp)
          goto 9999
       endif
       write (lundia, '(a,e12.5)') 'taubng = ', taubng
       !
       read (luninp, *, iostat = iocond) tauers
       if (iocond/=0) then
          if (iocond<0) then
             call prterr(lundia    ,'G006'    ,filnam    )
          !
          else
             call prterr(lundia    ,'G007'    ,filnam    )
          !
          endif
          error = .true.
          close (luninp)
          goto 9999
       endif
       write (lundia, '(a,e12.5)') 'tauers = ', tauers
       !
       read (luninp, *, iostat = iocond) tauset
       if (iocond/=0) then
          if (iocond<0) then
             call prterr(lundia    ,'G006'    ,filnam    )
          !
          else
             call prterr(lundia    ,'G007'    ,filnam    )
          !
          endif
          error = .true.
          close (luninp)
          goto 9999
       endif
       write (lundia, '(a,e12.5)') 'tauset = ', tauset
       !
       read (luninp, *, iostat = iocond) vdew
       if (iocond/=0) then
          if (iocond<0) then
             call prterr(lundia    ,'G006'    ,filnam    )
          !
          else
             call prterr(lundia    ,'G007'    ,filnam    )
          !
          endif
          error = .true.
          close (luninp)
          goto 9999
       endif
       write (lundia, '(a,e12.5)') 'vdew   = ', vdew
       write (lundia, '(a,e12.5)') 'ws     = ', ws
       close (luninp)
    else
       write (lundia, *) '*** No MUD model input specified ***'
    endif
 9999 continue
end subroutine rdmud
