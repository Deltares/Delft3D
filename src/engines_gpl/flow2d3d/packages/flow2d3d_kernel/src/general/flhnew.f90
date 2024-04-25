subroutine flhnew(lunrd     ,lundia    ,error     ,record    ,access    , &
                & irecrd    ,namloc    ,cntent    ,interp    ,itdate    , &
                & timscl    ,ntimrd    ,parrd     ,npara     ,nparrd    , &
                & bubble    ,gdp       )
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
!    Function: Reads keywords in the Direct access file (new)
!              and reads the relevant parameters
!              Keywords: table name, contents, location and
!                        parameter (incl. constituent name) are
!                        compulsory. If they are not found then
!                        the errorflag (error) is set to TRUE.
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
    character*20, dimension(:) , pointer :: keywrd
!
! Global variables
!
    integer                                      :: irecrd  !!  Counter of records if input file is
                                                            !!  a direct access file
    integer                        , intent(in)  :: itdate  !  Description and declaration in exttim.igs
    integer                                      :: lundia  !  Description and declaration in inout.igs
    integer                        , intent(in)  :: lunrd   !!  Unit number with input file
    integer                                      :: npara   !!  NR. of parameter records equal to
                                                            !!  number of data values in a record to
                                                            !!  be read
    integer                                      :: nparrd  !!  NR. of parameter records actual read
    integer                                      :: ntimrd
    logical                        , intent(in)  :: access
    logical                        , intent(in)  :: bubble  !  Description and declaration in procs.igs
                                                            !!  Flag to read file as direct access
                                                            !!  or sequential
    logical                        , intent(out) :: error   !!  Flag=TRUE if an error is encountered
    real(fp)                       , intent(out) :: timscl
    character(*)                                 :: cntent
    character(*)                                 :: record  !!  Standard rec. length in an attribute
                                                            !!  file (maximum kmax*24*2 + 48)
    character(1)                                 :: interp
    character(20)                  , intent(in)  :: namloc
    character(36), dimension(npara), intent(out) :: parrd
!
! Local variables
!
    integer       :: idef
    integer       :: iend
    integer       :: ier
    integer       :: ifound
    integer       :: istart
    integer       :: lrec
    integer       :: nkeyfd
    integer       :: timref
    logical       :: ready
    character(1)  :: chlp1
    character(10) :: timuni
    character(15) :: timser
    character(20) :: chlp20
    character(20) :: namhlp
    character(36) :: chlp36
!
!! executable statements -------------------------------------------------------
!
    keywrd  => gdp%gdkeywtd%keywrd
    !
    cntent = ' '
    ntimrd = 0
    timser = ' '
    timscl = 1.0_fp
    timuni = 'minutes'
    !
    ! Initialize local parameters
    !
    nparrd = 0
    chlp1  = ' '
    chlp20 = ' '
    ready  = .false.
    !
    ! Test for 1-st Keyword
    !
    ! -->
    5 continue
    istart = 1
    iend   = len(record)
    call srckey(record    ,istart    ,iend      ,ifound    ,gdp       )
    !
    if (ifound== - 9999 .or. ifound==0) then
       error = .true.
       ready = .true.
       call prterr(lundia    ,'V091'    ,keywrd(1) )
    elseif (ifound == 9999) then
       if (access) then
          irecrd = irecrd + 1
          read (lunrd, '(a)', rec = irecrd) record
       else
          read (lunrd, '(a)') record
       endif
       goto 5
    ! <--
    !
    ! IFOUND should be 1
    !
    elseif (ifound /= 1) then
       call prterr(lundia    ,'V091'    ,keywrd(1) )
       error = .true.
       ready = .true.
    else
    endif
    !
    ! Read the strings in the file and determine the keywords read
    ! For direct access files record number IRECRD should be opgehoogd
    !
    ! ==>>
   10 continue
    if (.not.ready) then
       if (access) then
          irecrd = irecrd + 1
          read (lunrd, '(a)', rec = irecrd) record
       else
          read (lunrd, '(a)') record
       endif
       iend = len(record)
       nkeyfd = 0
       ! ==>>
   20  continue
       istart = 1
       call srckey(record    ,istart    ,iend      ,ifound    ,gdp       )
       !
       !
       ! IFOUND =      -9999 error occurred
       !        =       9999 comment line
       !        =          0 not found
       !        = [1,MXKWTD] found
       !
       if (ifound == -9999) then
          error = .true.
       elseif (ifound==0 .and. nkeyfd==0 .and. .not. error) then
          ready = .true.
       elseif (ifound==0 .and. nkeyfd==0 .and. error) then
          !
          ! When running parallel, boundaries outside this partition are removed
          ! The removed boundaries must be skipped in this routine too
          ! When the boundary name is not correct, error is set to .true. (and ready remains .false.)
          ! It is assumed that the boundary read should be skipped for this partition
          ! Some read parameters (related to the boundary to be skipped) are reset here
          ! and reading continues
          !
          ntimrd = 0
          timser = ' '
          nparrd = 0
          parrd  = ' '
          goto 10
       elseif (ifound==0 .and. nkeyfd>0) then
          !
          ! No second keyword in RECORD, read new RECORD
          !
          goto 10
       ! <<==
       elseif (ifound == 9999) then
          !
          ! Comment string, read new RECORD
          !
          goto 10
       ! <<==
       else
          !
          ! One of the MXKWTD keywords is found
          !
          nkeyfd = nkeyfd + 1
          if (ifound == 2) then
             !
             ! Contents keyword found String should not be empty.
             !
             ! For discharge file:
             !    Max number of parameters to be read depends on CNTENT
             !    Assumption (1): the default sequence of
             !       keyword records is used:
             !          KEYWRD(2) before KEYWRD(14)
             !    Assumption (2): regular, walking, inoutlet,power and
             !       culvert are used in the Contents string FOR
             !       DISCHARGES ONLY.
             !
             call keyinp(record(istart:iend)  ,cntent    )
             if (cntent == ' ') then
                call prterr(lundia    ,'V091'    ,keywrd(ifound)       )
                error = .true.
                ready = .true.
             else
                call small(cntent    ,len(cntent)          )
                if (cntent(:10)=='regular   ' .or. cntent(:10)                  &
                  & =='walking   ' .or. cntent(:10)=='inoutlet  ' .or.          &
                  & cntent(:10)=='power     ' .or. cntent(:10)=='culvert   ')   &
                  & then
                   npara = npara - 2
                endif
             endif
          elseif (ifound == 3) then
             !
             ! Location keyword found
             !
             call keyinp(record(istart:iend)  ,chlp20    )
             call small(chlp20    ,len(chlp20)          )
             !
             ! One can program in such a manner that data in the file
             ! for each location does not have to be specified in
             ! sequential manner. But for now we will assume this to be
             ! the case.
             !
             namhlp = namloc
             call small(namhlp    ,len(namhlp)          )
             if (chlp20 /= namhlp) then
                if (bubble) then
                   !
                   ! Do not check on names because of bubble screens
                   !
                   error = .false.
                   ready = .false.
                else
                   !
                   ! Name does not match
                   ! Assumed: this is a parallel run and the boundary read is completely outside this partition
                   ! Set error to .true. and continue reading (ready = .false.)
                   ! The related error message is removed
                   !
                   error = .true.
                   ready = .false.
                endif
             else
                !
                ! Name matches
                ! error might be set to .true. before  (see 6 lines above)
                ! and must be set to .false. here
                !
                error = .false.
             endif
          elseif (ifound == 17) then
             !
             ! xy-function keyword found
             !
             ! The information in this record is not used
             !
          elseif (ifound == 8) then
             !
             ! Time function keyword found
             !
             call keyinp(record(istart:iend)  ,timser    )
          elseif (ifound == 9) then
             !
             ! Reference time keyword found
             !
             lrec = iend - istart + 1
             idef = itdate
             call read1i(record    ,lrec      ,istart    ,iend      ,timref    , &
                       & idef      ,ier       )
             if (ier <= 0) timref = itdate
             !
             if (timref /= itdate) then
                call prterr(lundia    ,'V093'    ,' '       )
                error = .true.
                ready = .true.
             endif
          elseif (ifound == 10) then
             !
             !---------------Time unit keyword found
             !
             call keyinp(record(istart:iend)  ,timuni    )
             call small(timuni    ,len(timuni)          )
             if (timuni(:1) == 's') then
                timscl = 1.0_fp/60.0_fp
             elseif (timuni(:1) == 'm') then
                timscl = 1.0_fp
             elseif (timuni(:1) == 'h') then
                timscl = 60.0_fp
             elseif (timuni(:1) == 'd') then
                timscl = 1440.0_fp
             elseif (timuni(:1) == 'w') then
                timscl = 1440.0_fp * 7.0_fp   
             else
                call prterr(lundia    ,'V006'    ,' '       )
                error = .true.
             endif
          elseif (ifound == 12) then
             !
             ! Interpolation keyword found
             ! value is <linear> or <block> which correspond with
             ! INTERP = 'Y'      or 'N'
             !
             call keyinp(record(istart:iend)  ,chlp20    )
             call small(chlp20    ,len(chlp20)          )
             chlp1 = 'N'
             if (chlp20(:6) == 'linear') then
                chlp1 = 'Y'
             endif
             !
             ! The interpolation option on this data file will be
             ! overrulled by the option previous defined when defining
             ! the location definitions
             !
             if (chlp1 /= interp) then
                call prterr(lundia    ,'V094'    ,interp    )
             endif
          elseif (ifound == 14) then
             !
             ! Parameter keyword found; contains constituent name
             ! Only first parameter description record will be used
             !
             call keyinp(record(istart:iend)  ,chlp36    )
             call small(chlp36    ,len(chlp36)          )
             nparrd = nparrd + 1
             if (nparrd > npara) then
                call prterr(lundia    ,'V096'    ,keywrd(ifound)       )
                error = .true.
                ready = .true.
             else
                parrd(nparrd) = chlp36
             endif
          elseif (ifound == 16) then
             !
             ! Nr. of record keyword found
             ! Number of time record should be > 0
             !
             lrec = iend - istart + 1
             idef = 0
             call read1i(record    ,lrec      ,istart    ,iend      ,ntimrd    , &
                       & idef      ,ier       )
             if (ier <= 0) then
                ntimrd = idef
             endif
             if (ntimrd == idef) then
                call prterr(lundia    ,'V091'    ,keywrd(ifound)       )
                error = .true.
                ready = .true.
             endif
          else
          endif
          !
          ! Read rest of RECORD to test for more keywords in one
          ! record
          !
          ! <--
          if (record /= ' ') then
             goto 20
          endif
          !
          ! Read new RECORD
          !
          goto 10
       ! <<==
       endif
    endif
end subroutine flhnew
