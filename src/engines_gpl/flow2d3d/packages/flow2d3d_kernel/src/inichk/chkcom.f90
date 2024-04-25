subroutine chkcom(lundia    ,error     ,neffil    ,ftype     ,gdp       )
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
!    Function: Check the comm-file version by reading the element
!              FILE-VERSION or
!              check the existence of the group DWQTIM
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use string_module
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
    integer, parameter :: start = 1, stopp = 2, incr = 3
!
! Global variables
!
    integer     , intent(in)  :: lundia !  Description and declaration in inout.igs
    logical     , intent(out) :: error  !  Flag=TRUE if an error is encountered
    character(*), intent(in)  :: neffil !  File name for FLOW NEFIS output files: tri"h/m/d"-"casl""labl" or for Comm. file com-"casl""labl"
    character(3), intent(in)  :: ftype  !  String containing to which output file version group or to diagnostic file should be written
!
! Local variables
!
    integer                        :: celidt                ! Array with inidices identifying the cell from where the reading and the writing starts 
    integer                        :: datlen                ! Length of 'data file name' 
    integer                        :: deflen                ! Length of 'definition file name' 
    integer                        :: fds                   ! File description of NEFIS file set 
    integer                        :: ierror               ! Local errorflag for NEFIS files 
    integer                        :: ind
    integer, dimension(3)          :: uindex                !  Lowest allowed reference number
    integer, external              :: clsnef
    integer, external              :: crenef
    integer, external              :: getels
    integer, external              :: inqdat
    integer, external              :: neferr
    character(1)                   :: access                ! NEFIS file access type, here read only 
    character(1)                   :: coding                ! NEFIS file representation, here NEUTRAL 
    character(11)                  :: refnr                 ! Lowest allowed reference number 
    character(16)                  :: elnms4                ! Element name defined for the NEFIS-files 
    character(16)                  :: grdef5                ! Data-group name defined for the NEFIS-files as defined in definition part 
    character(16)                  :: grnam4                ! Data-group name defined for the NEFIS-files as defined in data part 
    character(16)                  :: grnam5                ! Data-group name defined for the NEFIS-files as defined in data part 
    character(16), dimension(1)    :: hlptxt                ! Help character string to check comm file version number 
    character(256)                 :: datnam                ! Data file name 
    character(256)                 :: defnam                ! Definition file name 
    character(256)                 :: errmsg                ! Character var. containing the errormessage to be written to file. The message depend on the error. 
    character(256)                 :: filnam                ! Help var. for FLOW file name 
    character(4)                   :: errnr                 ! Character var. containing the errormessage number corresponding to errormessage in ERRFIL 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !
    !-----Initialize local variables
    !
    coding = 'N'
    error = .false.
    errnr = ' '
    fds   = -1
    refnr = '03.20.00.00'
    celidt = 1
    hlptxt(1) = refnr
    filnam = neffil
    !
    grnam4 = ftype // '-version'
    elnms4 = 'FILE-VERSION'
    !
    grnam5 = 'DWQTIM'
    !
    uindex(start) = celidt
    uindex(stopp) = celidt
    uindex(incr) = 1
    !
    !
    !-----aggregate file names
    !
    ind = index(filnam, ' ')
    !
    datnam = filnam
    datnam(ind:ind + 3) = '.dat'
    call remove_leading_spaces(datnam    ,datlen    )
    !
    defnam = filnam
    defnam(ind:ind + 3) = '.def'
    call remove_leading_spaces(defnam    ,deflen    )
    !
    !-----Open NEFIS files with read access
    !
    access = 'r'
    !
    ierror = crenef(fds, datnam(1:datlen), defnam(1:deflen), coding, access)
    if (ierror/=0) goto 9999
    !
    !     If NEFIS exist then it should be the right version
    !     group 'com-version', element 'FILE-VERSION'
    !
    ierror = getels(fds, grnam4, elnms4, uindex, 1, 16, hlptxt)
    if (ierror ==0) then
       if (hlptxt(1)(1:7)>=refnr(2:8)) then
       !----------file-version read is higher than lowest allowed number
       else
          !----------file-version read is lower than lowest allowed number
          error = .true.
          errnr = 'S027'
          errmsg = hlptxt(1)(1:11)
       endif
    else
       !
       !        group com-version does not exist
       !        check existence of flow-group DWQTIM
       !
       ierror = inqdat(fds, grnam5, grdef5)
       if (ierror ==0) then
          !           group DWQTIM exists
          error = .true.
          errnr = 'S027'
          errmsg = 'group not defined, but DWQTIM is defined'
       !           group DWQTIM does not exist
       else
            error = .true.
            errnr = 'U021'
            ierror = neferr(0, errmsg)
            call prterr(lundia    ,errnr     ,errmsg    )
            write (errmsg, '(5a)') 'Incomplete COM-files #',trim(datnam),'/',trim(defnam),'# exist'
            call prterr(lundia    ,errnr     ,errmsg    )
            errmsg = 'Try removing the COM-files first'
       endif
    endif
    !
    !
    !-----Close data and definition file
    !
    ierror = clsnef(fds)
    if (ierror/=0 .and. .not.error) then
       error = .true.
       errnr = 'U021'
       ierror = neferr(0, errmsg)
    endif
    !
    !-----Report error
    !
 9999 continue
    if (error) then
       call prterr(lundia    ,errnr     ,errmsg    )
    endif
end subroutine chkcom
