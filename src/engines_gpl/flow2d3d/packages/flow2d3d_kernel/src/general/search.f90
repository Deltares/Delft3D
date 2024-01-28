subroutine search(lunmd     ,error     ,newkw     ,nrrec     ,found     , &
                & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
                & prt       )
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
!    Function: Searches a record containing KEYW from the MD-file
!              If not found the file is rewound once and the file
!              is read again until the record counter reaches
!              NTREC (usually it also starts from the same record
!              cound NTREC, but it may be specified otherwise
!              from outside). If a the record is found the posi-
!              tion of this record in the file (NRREAD) is re-
!              turned.
!              To search case insensitive both KEYWord as read
!              MDFRECord are copied to small character help
!              strings.
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Global variables
!
    integer                        :: itis   !  Description and declaration in rdpara.igs
    integer                        :: lkw    !!  Length of char. str (usually the KEYWRD or RECNAM)
    integer     , intent(in)       :: lunmd  !  Description and declaration in inout.igs
    integer                        :: nrrec  !!  Pointer to the record number in the MD-file
    integer     , intent(in)       :: ntrec  !!  Help. var to keep track of NRREC
    logical     , intent(out)      :: found  !!  If FOUND = TRUE then recnam in the MD-file was found
    logical     , intent(out)      :: error  !!  Flag=TRUE if an error is encountered
    logical     , intent(in)       :: newkw  !!  Logical var. specifying whether a new recnam should be read from the
                                             !!  MD-file or just new data in the continuation line
    character(*), intent(in)       :: keyw   !!  Name of record to look for in the MD-file (usually KEYWRD or RECNAM)
    character(*)                   :: mdfrec !!  Standard rec. length in MD-file (300)
    character(*), intent(in)       :: prt    !!  Flag, which if equal 'Y'/'y' and if error occurred (KEYW not found) then
                                             !!  ERRMSG will be printed
!
!
! Local variables
!
    integer         :: iocond ! General FORTRAN IO-errorcond. 
    integer         :: itel   ! Record count of md-file 
    integer         :: itw    ! Help var. to determine the length of var. KEYW 
    character(80)   :: errmsg ! Character var. containing the errormessage to be written to file. The message depend on the error. 
    character(80)   :: hlpkey ! Help text for case insensitive key 
    character(80)   :: hlprec ! Help text for case insensitive check 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !-----initialize
    !
    itel = nrrec
    iocond = 0
    found = .true.
    errmsg = ' *** error Record ' // keyw(:lkw) // ' not found'
    !
    !-----initial checks, which should only be hit by programmers
    !
    if (itis>80) then
       write (*, *) ' *** error Search string (ITIS) definition to small in SEARCH'
       error = .true.
       goto 999
    endif
    if (lkw>80) then
       write (*, *) ' *** error Search string (LKW) definition to small in SEARCH'
       error = .true.
       goto 999
    endif
    !
    !-----copy KEYWord to HLPKEY and make case insensitive
    !
    hlpkey = keyw
    call small(hlpkey    ,lkw       )
    !
    !-----old keyword then read new record
    !
    if (.not.newkw) then
       mdfrec = ' '
       read (lunmd, '(a300)', iostat = iocond) mdfrec
       if (iocond>0) goto 999
       !
       !--------end of file?
       !
       if (iocond<0) then
          itel = 0
          rewind (lunmd)
          read (lunmd, '(a300)', iostat = iocond) mdfrec
          if (iocond<0) found = .false.
          if (iocond/=0) goto 999
       endif
       itel = itel + 1
       !
       !--------read every record of md-file?
       !
       if (itel==ntrec) then
          if (prt(:1)=='Y' .or. prt(:1)=='y') write (*, *) errmsg
          found = .false.
          goto 999
       endif
    endif
    !
    !-----copy MDF RECord to HLPREC and make case insensitive
    !
    hlprec = mdfrec(1:itis)
    call small(hlprec    ,itis      )
    !
    !-----keyword in record?
    !
    itw = index(hlprec(1:itis), hlpkey(1:lkw))
    if (itw/=0) goto 999
    !
    !-----read md-file until keyw  is found
    !
   10 continue
    mdfrec = ' '
    read (lunmd, '(a300)', iostat = iocond) mdfrec
    if (iocond>0) goto 999
    !
    !--------end of file?
    !
    if (iocond<0) then
       itel = 0
       rewind (lunmd)
       read (lunmd, '(a300)', iostat = iocond) mdfrec
       if (iocond<0) found = .false.
       if (iocond/=0) goto 999
    endif
    itel = itel + 1
    !
    !--------copy MDF RECord to HLPREC and make case insensitive
    !
    hlprec = mdfrec(1:itis)
    call small(hlprec    ,itis      )
    !
    itw = index(hlprec(1:itis), hlpkey(1:lkw))
    if (itw==0) then
       !
       !-----------keyword not in record
       !
       if (itel==ntrec) then
          !
          !--------------all records of md-file are read
          !
          if (prt(:1)=='Y' .or. prt(:1)=='y') write (*, *) errmsg
          found = .false.
       else
          goto 10
       endif
    endif
    !
    !-----error in md-file?
    !
  999 continue
    if (iocond>0) then
       errmsg = ' *** errorI/O error while reading input file'
       if (prt(:1)=='Y' .or. prt(:1)=='y') write (*, *) errmsg
       error = .true.
    endif
    nrrec = itel
end subroutine search
