subroutine rdencl( lunmd     ,lundia    ,error     , runid    , &
                &  mmax      ,nmaxus    ,gdp       )
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
!    Function: - Reads from either the MD-file or the attribute
!                file the grid enclosure
!              - Sets the default computational grid enclosure if
!                none is specified
!              - Writes the grid enclosure to unformatted semi-scratch file
! Method used:
!
!!--pseudo code and references--------------------------------------------------
!
!   COPIED FROM A PART OF FILE RDGRID.F90!!!
!
!!--declarations----------------------------------------------------------------
    use globaldata
    use string_module
    use properties
    !
    implicit none
    !
    type(globdat), target    :: gdp
!
! Global variables
!
    integer, intent(in)       :: lundia ! Description and declaration in inout.igs
    integer, intent(in)       :: lunmd  ! Description and declaration in inout.igs
    integer, intent(in)       :: mmax   ! Description and declaration in esm_alloc_int.f90
    integer, intent(in)       :: nmaxus ! Description and declaration in esm_alloc_int.f90
    logical, intent(out)      :: error  ! Flag=TRUE if an error is encountered
    character(*), intent(in)  :: runid  ! Run identification code for the current
                                        ! simulation (used to determine
                                        ! the names of the in- /output files
                                        ! used by the system)
!
! Local variables
!
    integer, pointer      :: itis
    integer               :: idef   ! Help var. containing default value(s) for integer variable
    integer               :: imng   ! Help var. for the grid points
    integer               :: j      ! Help var.
    integer               :: lenc   ! Help var. (length of var. cvar to be looked for in the MD-file)
    integer               :: lkw    ! Actual length of KEYW
    integer               :: lrid   ! Length of character string runid
    integer               :: lungrd ! Unit number of local scratch file for grid enclosure points
    integer               :: newlun
    integer               :: nlook  ! Help var.: nr. of data to look for in the MD-file
    integer               :: nrrec  !  Pointer to the record number in the MD-file
    integer               :: ntrec  ! Help. var to keep track of NRREC
    integer, dimension(2) :: ival   ! Help array
    logical               :: defaul ! Flag set to YES if default value may be applied in case var. read is empty (ier <= 0, or nrread < nlook)
    logical               :: flgrd
    logical               :: found  ! FOUND=TRUE if KEYW in the MD-file was found
    logical               :: lerror ! Flag=TRUE if a local error is encountered
    logical               :: newkw  ! Logical var. specifying whether a new recnam should be read from the MD-file or just new data in the continuation line
    character(11)         :: fmtdef ! Default file format (usually=blank)
    character(11)         :: fmttmp ! Help variable for file format
    character(12)         :: fildef ! Default file name (usually = blank)
    character(256)        :: filgrd ! File name for the grid enclosure file
    character(256)        :: fixid  ! fixed size version of runid, needed for character concatenation
    character(6)          :: keyw   ! Name of record to look for in the MD-file (usually KEYWRD or RECNAM)
    character(300)        :: mdfrec ! Standard rec. length in MD-file (300) 300 = 256 + a bit (field, =, ##, etc.)
!
!! executable statements -------------------------------------------------------
!
    itis => gdp%gdrdpara%itis
    !
    rewind (lunmd)
    read (lunmd, '(a300)') mdfrec
    nrrec = 1
    !
    filgrd = ' '
    !
    ! initialize local paramters
    !
    fildef = ' '
    fmtdef = 'FRformatted'
    idef   = 0
    lerror = .false.
    newkw  = .true.
    defaul = .true.
    !
    ival(1) = 0
    ival(2) = 0
    !
    ! define length of runid and put in fixed size array
    ! size is tested in iniid
    !
    call remove_leading_spaces(runid     ,lrid      )
    fixid(1:lrid) = runid(1:lrid)
    !=======================================================================
    ! open semi-scratch file
    !
    open (newunit=lungrd, file = 'TMP_' // fixid(1:lrid) // '.grd',                  &
         & form = 'unformatted', status = 'unknown')
    !
    ! 'Filgrd': grid enclosure file
    !
    filgrd = fildef
    call prop_get_string(gdp%mdfile_ptr,'*','Filgrd',filgrd)
    if (filgrd /= fildef) then
       !
       ! Grid enclosure in file
       !
       ! locate 'Fmtgrd' record for format definition of input file
       !
       keyw   = 'Fmtgrd'
       ntrec  = nrrec
       nlook  = 1
       lenc   = 2
       fmttmp = ' '
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,fmttmp    ,fmtdef    ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
          fmttmp = fmtdef(3:)
       else
          !
          ! determine file format (unformatted/freeformatted)
          !
          call filfmt(lundia    ,keyw      ,fmttmp    ,lerror    ,gdp       )
          !
          if (lerror) then
             lerror = .false.
             fmttmp = fmtdef(3:)
          endif
       endif
       !
       ! read data from external file
       !
       call grdfil(lundia    ,lungrd    ,error     ,filgrd    ,fmttmp    , &
                 & flgrd     ,gdp       )
    else
       !
       ! No grid enclosure file
       !
       ! locate and read 'MNgrd' record for ival
       ! first time newkw = true, if keyw not found -> error
       ! first default value allowed => defaul
       !
       imng = 1
       !
       keyw  = 'MNgrd '
       ntrec = nrrec
       newkw = .true.
       lkw   = 5
       call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
                 & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
                 & 'NO'      )
       lerror = .false.
       !
       ! not found ?
       !
       if (.not.found) then
          if (filgrd == ' ') then
             write (lungrd) 1, 1
             write (lungrd) 1, nmaxus
             write (lungrd) mmax, nmaxus
             write (lungrd) mmax, 1
             write (lungrd) 1, 1
             call prterr(lundia    ,'V028'    ,' '       )
          endif
          goto 200
       endif
       ! -->
  110  continue
       newkw = .true.
       nlook = 2
       call read2i(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,ival      ,idef      ,defaul    ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) goto 200
       !
       if (ival(1)==0 .or. ival(2)==0) then
          if (imng == 1) then
             !
             ! default grid enclosure, produce warning message
             !
             write (lungrd) 1, 1
             write (lungrd) 1, nmaxus
             write (lungrd) mmax, nmaxus
             write (lungrd) mmax, 1
             write (lungrd) 1, 1
             call prterr(lundia    ,'V028'    ,' '       )
          else
             !
             ! no value or error found => error
             !
             lerror = .true.
             call prterr(lundia    ,'V003'    ,'Comp. grid enclosure'          )
          endif
       else
          !
          ! write ival to semi-scratch file
          !
          write (lungrd) (ival(j), j = 1, 2)
          !
          imng = imng + 1
          !
          ! next records newkw = false
          !
          newkw = .false.
          call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
                    & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
                    & 'NO'      )
          lerror = .false.
          if (found) goto 110
       ! <--
       endif
       !
       ! stop reading
       !
  200  continue
       if (lerror) lerror = .false.
    endif
    !
    ! close files
    !
    if (error) then
       close (lungrd, status = 'delete')
    else
       close (lungrd)
    endif
    !
end subroutine rdencl
