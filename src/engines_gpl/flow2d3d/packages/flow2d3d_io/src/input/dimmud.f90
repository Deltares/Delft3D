subroutine dimmud(lunmd     ,lundia    ,error     ,nrrec     ,gdp       )
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
!    Function: - Reads mud keywords from mdf-file
!                Filmud= #mudfilnam# : mud layer
!                                      mudlay = .true.
!                                      flmd2l = .true.
!                Flumud= #YES#       : water layer
!                                      mudlay = .false.
!                                      flmd2l = .true.
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    !
    use globaldata
    use string_module
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    logical , pointer :: wave
    logical , pointer :: sedim
    logical , pointer :: flmd2l
    logical , pointer :: mudlay
    logical , pointer :: mudwave
    integer , pointer :: itis
!
! Global variables
!
    integer               :: lundia !  Description and declaration in inout.igs
    integer               :: lunmd  !  Description and declaration in inout.igs
    integer               :: nrrec  !!  Record counter keeping the track of the last record read
    logical , intent(out) :: error  !!  Flag=TRUE if an error is encountered
!
! Local variables
!
    integer        :: lenc   ! Help var. (length of var. cvar to be looked for in the MD-file) 
    integer        :: lfile  ! Number of non blank characters of file name 
    integer        :: lkw
    integer        :: nlook  ! Help var.: nr. of data to look for in the MD-file 
    integer        :: ntrec  ! Help. var to keep track of NRREC 
    logical        :: ex     ! Flag if file exists 
    logical        :: found  ! Flag is true if KEYWRD is found 
    logical        :: lerror ! Flag=TRUE if an error is encountered 
    logical        :: newkw  ! Logical var. specifying whether a new recnam should be read from the MD-file or just new data in the continuation line 
    character(1)   :: chulp
    character(12)  :: cdef   ! Default value when CHULP not found 
    character(256) :: filmud ! File name for sediment parameters 
    character(300) :: mdfrec ! Record read from the MD-file 300 = 256 + a bit (field, =, ##, etc.) 
    character(6)   :: keyw   ! Name of record to look for in the MD-file (usually KEYWRD or RECNAM) 
!
!! executable statements -------------------------------------------------------
!
    wave       => gdp%gdprocs%wave
    sedim      => gdp%gdprocs%sedim
    flmd2l     => gdp%gdprocs%flmd2l
    mudlay     => gdp%gdprocs%mudlay
    mudwave    => gdp%gdprocs%mudwave
    itis       => gdp%gdrdpara%itis
    !
    lerror = .false.
    newkw  = .true.
    found  = .false.
    nlook  = 1
    mdfrec = ' '
    !
    ! locate 'Filmud' containing parameter values for calculations of sediment
    !
    keyw  = 'Filmud'
    ntrec = nrrec
    cdef  = ' '
    lkw   = 6
    call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
              & 'NO'      )
    lerror = .false.
    !
    ! found ?
    !
    if (found) then
       lenc = 256
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,filmud    ,cdef      ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
          filmud = cdef
       endif
       !
       ! File name defined (Y)
       !
       if (filmud /= cdef) then
          !
          ! Define flag mudlay
          !
          call prterr(lundia, 'G051', 'Mud layer calculation')
          mudlay = .true.
          flmd2l = .true.
          call prop_get_logical(gdp%mdfile_ptr, '*', 'MudWave', mudwave)
          if (mudwave) then
             call prterr(lundia, 'G051', 'Mud - Wave interaction activated')
          endif
          !
          ! Test file's existence
          !
          call remove_leading_spaces(filmud    ,lfile     )
          inquire (file = filmud(1:lfile), exist = ex)
          if (.not.ex) then
             call prterr(lundia    ,'G004'    ,filmud(1:lfile)      )
             error = .true.
          endif
       endif
    endif
    !
    ! locate and read 'Flumud' as indication of a process flag
    ! default = no ('N') which means no fluidmud simulation
    !
    keyw  = 'Flumud'
    ntrec = nrrec
    lkw   = 6
    call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
              & 'NO'      )
    lerror = .false.
    !
    ! not found ?
    !
    if (found) then
       lenc  = 1
       nlook = 1
       cdef  = 'N'
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,chulp     ,cdef      ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       if (lerror) then
          lerror = .false.
       elseif ((chulp=='Y' .or. chulp=='y') .and. (.not.sedim)) then
          call prterr(lundia, 'P004', 'The fluid mud flag requires that the simulation includes sediment')
          error = .true.
       else
          !
          ! define flmd2l true if CHULP = Y/y and sedim==true
          !
          flmd2l = (chulp=='Y' .or. chulp=='y')
       endif
    endif
end subroutine dimmud
