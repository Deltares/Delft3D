subroutine dimiwe(lunmd     ,lundia    ,error     ,nrrec     ,iweflg    , &
                & kmxdt     ,kmax      ,npiwe     ,gdp       )
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
!    Function: - Reads the dimension for Internal Wave Energy
!                dim. from an attribute file
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
    integer , pointer :: itis
!
! Global variables
!
    integer               :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer               :: kmxdt  !  Description and declaration in dimens.igs
    integer               :: lundia !  Description and declaration in inout.igs
    integer               :: lunmd  !  Description and declaration in inout.igs
    integer               :: npiwe  !  Description and declaration in dimens.igs
    integer               :: nrrec  !!  Record counter keeping the track of the last record read
    logical , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    logical , intent(out) :: iweflg !  Description and declaration in procs.igs
!
! Local variables
!
    integer              :: ibeg
    integer              :: idef   ! Default value for NPIWE 
    integer              :: iend
    integer              :: ier
    integer              :: iocond
    integer              :: lenc   ! Help var. (length of var. cvar to be looked for in the MD-file) 
    integer              :: lfile  ! Number of non blank characters of file name 
    integer              :: lkw
    integer              :: luntmp ! Unit number of (input par. for) FILIWE 
    integer              :: newlun
    integer              :: nlook  ! Help var.: nr. of data to look for in the MD-file 
    integer              :: ntrec  ! Help. var to keep track of NRREC 
    logical              :: ex     ! Flag if file exists 
    logical              :: found  ! Flag is true if KEYWRD is found 
    logical              :: lerror ! Flag=TRUE if an error is encountered 
    logical              :: newkw  ! Logical var. specifying whether a new recnam should be read from the MD-file or just new data in the continuation line 
    character(12)        :: cdef   ! Default value when CHULP not found 
    character(256)       :: filiwe ! File name for the IWE parameters 
    character(300)       :: mdfrec ! Record read from the MD-file 300 = 256 + a bit (field, =, ##, etc.) 
    character(6)         :: keyw   ! Name of record to look for in the MD-file (usually KEYWRD or RECNAM) 
!
!! executable statements -------------------------------------------------------
!
    !
    itis  => gdp%gdrdpara%itis
    !
    lerror = .false.
    newkw  = .true.
    found  = .false.
    nlook  = 1
    mdfrec = ' '
    !
    ! locate 'Filiwe' containing parameter values for calculations of Internal Wave Energy;
    !
    keyw = 'Filiwe'
    ntrec = nrrec
    cdef = ' '
    lkw = 6
    call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
              & 'NO'      )
    lerror = .false.
    !
    ! found ?
    !
    if (found) then
       lenc = 12
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,filiwe    ,cdef      ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
          filiwe = cdef
       endif
       !
       ! File name defined (Y)
       !
       if (filiwe/=cdef) then
          !
          ! Define flag IWEFLG and recommended value for NPIWE
          !
          iweflg = .true.
          npiwe = 100
          !
          ! Test file existence and if open if found
          !
          lfile = len(filiwe)
          !
          inquire (file = filiwe(1:lfile), exist = ex)
          if (ex) then
             open (newunit=luntmp, file = filiwe(:lfile), form = 'formatted',           &
                  & status = 'old')
             !
             ! Free formatted file
             ! Skip first record and then read dimensions from mdfrec
             ! for KMXDT default value not allowed incase IER < 0
             ! if NFREQS and/or NPIWE not found then defined for recommended
             ! value (100 according to Rob Uittenbogaard)
             !
             lenc = 300
             !
             read (luntmp, '(a)', iostat = iocond) mdfrec
             if (iocond/=0) then
                if (iocond<0) then
                   call prterr(lundia    ,'G006'    ,filiwe(1:lfile)      )
                else
                   call prterr(lundia    ,'G007'    ,filiwe(1:lfile)      )
                endif
                close (luntmp)
                error = .true.
                goto 9999
             endif
             !
             read (luntmp, '(a)', iostat = iocond) mdfrec
             if (iocond/=0) then
                if (iocond<0) then
                   call prterr(lundia    ,'G006'    ,filiwe(1:lfile)      )
                else
                   call prterr(lundia    ,'G007'    ,filiwe(1:lfile)      )
                endif
                close (luntmp)
                error = .true.
                goto 9999
             endif
             !
             ! Read KMAXTD from record
             !
             iend = 0
             ibeg = iend + 1
             call read1i(mdfrec    ,lenc      ,ibeg      ,iend      ,kmxdt     , &
                       & kmax      ,ier       )
             !
             ! End of record or no value defined (IER <= 0) not allowed
             !
             if (ier<=0) then
                error = .true.
                call prterr(lundia    ,'G006'    ,filiwe(1:lfile)      )
                !
                goto 9999
             endif
             !
             ! Read NPIWE from record
             !
             idef = npiwe
             ibeg = iend + 1
             call read1i(mdfrec    ,lenc      ,ibeg      ,iend      ,npiwe     , &
                       & idef      ,ier       )
             !
             ! End of record or no value defined (IER <= 0) allowed
             !
             if (ier<0) npiwe = idef
             !
             ! Close input file
             !
             close (luntmp)
          !
          ! File does not exist
          !
          else
             call prterr(lundia    ,'G004'    ,filiwe(1:lfile)      )
             !
             error = .true.
          endif
       endif
    endif
    !
 9999 continue
end subroutine dimiwe
