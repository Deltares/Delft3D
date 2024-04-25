subroutine wridoc(error, neffil, ftype, simdat, runtxt, commrd, part_nr, gdp)
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
!    Function: Writes the initial group 4 ('"ftype"-version') to
!              the "ftype"-DAT/DEF files
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use datagroups
    use globaldata
    use string_module
    use flow2d3d_version_module
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    logical                    , pointer :: first
    integer                    , pointer :: lundia    !  Description and declaration in inout.igs
    integer                    , pointer :: lunprt    !  Description and declaration in inout.igs
    character*131, dimension(:), pointer :: header    !  Description and declaration in postpr.igs
    logical                              :: commrd
    type (datagroup)           , pointer :: group
!
! Global variables
!
    logical          , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    character(*)     , intent(in)  :: neffil !!  File name for FLOW NEFIS output
                                             !!  files: tri"h/m/d"-"casl""labl" or
                                             !!  for Comm. file com-"casl""labl"
    character(*)     , intent(in)  :: part_nr !! Partition number string
    character(16)    , intent(in)  :: simdat !!  Simulation date representing the flow condition at this date
    character(6)     , intent(in)  :: ftype  !!  String containing to which output file version group or to diagnostic file should be written
    character(30), dimension(10)   :: runtxt !!  Textual description of model input

!
! Local variables
!
    integer                                       :: fds
    integer                                       :: IO_FIL
    integer                                       :: i            ! Help var. 
    integer                                       :: ierror       ! Local errorflag for NEFIS files 
    integer                                       :: iheader      ! Loop counter for writing header
    integer                                       :: lrid         ! Help var. to determine the actual length of RUNID 
    integer                                       :: lridmx       ! Help var. for lunprt: LRID < 47 
    integer                                       :: na
    integer      , dimension(3,5)                 :: uindex
    integer                        , external     :: putels
    integer                        , external     :: clsnef
    integer                        , external     :: open_datdef
    integer                        , external     :: neferr
    character(10)                                 :: date         ! Date to be filled in the header 
    character(256)                                :: datnam
    character(256)                                :: defnam
    character(16), dimension(1)                   :: cdum16       ! Help array to read/write Nefis files 
    character(16)                                 :: grnam4       ! Data-group name defined for the NEFIS-files 
    character(256)                                :: filnam       ! Help var. for FLOW file name 
    character(4)                                  :: errnr        ! Character var. containing the errormessage number corresponding to errormessage in ERRFIL 
    character(256)                                :: errmsg       ! Character var. containing the errormessage to be written to file. The message depends on the error. 
    character(20)                                 :: rundat       ! Current date and time containing a combination of DATE and TIME 
    character(256)                                :: full_version ! Version nr. of the module of the current package
    character(256), dimension(1)                  :: cdumcident   ! Help array to read/write Nefis files 
!
!! executable statements -------------------------------------------------------
!
    lundia  => gdp%gdinout%lundia
    lunprt  => gdp%gdinout%lunprt
    header  => gdp%gdpostpr%header
    !
    !
    ! Initialize local variables
    !
    ierror = 0
    !
    filnam = neffil
    if (ftype(1:3)/='com') filnam = neffil(1:3) // ftype(1:1) // trim(neffil(5:)) // trim(part_nr) 
    grnam4 = ftype(1:3) // '-version'
    errmsg = ' '
    !
    ! Write system definition to diagnostic file for ftype = 'dia' and skip rest of routine
    !
    full_version  = ' '
    !version_short = ' '
    call getfullversionstring_flow2d3d(full_version)
    !
    if (ftype(1:3) == 'dia') then
       ! nothing
    elseif (ftype(1:5) == 'ascii') then
        !
        ! write start date and time to LUNPRT
        !
       call remove_leading_spaces(gdp%runid, lrid)
       lridmx = min(lrid, 47)
       !
       ! Date and time
       !
       call dattim(rundat)
       date(1:4)  = rundat(1:4)
       date(5:5)  = '-'
       date(6:7)  = rundat(6:7)
       date(8:8)  = '-'
       date(9:10) = rundat(9:10)
       !
       ! Version info
       !
       write (header(1 ), '(131a1)'    ) ('*', na = 1, 131)
       write (header(2 ), '(a,a,a,a,a,a,a,a,a,t129,a)') &
           & '*** Print of ', 'Delft3D-FLOW', ' for Run ', gdp%runid(:lridmx) , ' - Simulation date: ',  &
           & date, ' ', rundat(11:19), '  page     1', '***'
       write (header(3 ), '(2a,t129,a)') '*** ', trim(full_version), '***'
       write (header(4 ), '(a,t129,a)' ) '*** User: Unknown ', '***'
       write (header(5 ), '(131a1)'    ) ('*', na = 1, 131)
       write (header(6 ), '(a)'        )
       write (header(10), '(a)'        )
       !
       do iheader = 1,5
          write (lunprt, '(a)') header(iheader)
       enddo
    else
       select case (ftype(1:3))
       case ('his')
           IO_FIL = FILOUT_HIS
       case ('map')
           IO_FIL = FILOUT_MAP
       case ('dro')
           IO_FIL = FILOUT_DRO
       case ('com')
           IO_FIL = FILOUT_COM
       end select
       !
       call getdatagroup(gdp, IO_FIL, grnam4, group)
       first   => group%first
       !
       if (first) then
          !
          ! Set up the element chracteristics
          !
          call addelm(gdp, lundia, IO_FIL, grnam4, 'FLOW-SIMDAT', ' ', 16, 1, (/1/), ' ', 'FLOW Simulation date and time [YYYYMMDD  HHMMSS]', '[   -   ]') !CHARACTER
          call addelm(gdp, lundia, IO_FIL, grnam4, 'FLOW-SYSTXT', ' ', 256, 1, (/1/), ' ', 'FLOW System description', '[   -   ]') !CHARACTER
          call addelm(gdp, lundia, IO_FIL, grnam4, 'FLOW-RUNTXT', ' ', 30, 1, (/10/), ' ', 'FLOW User defined Model description', '[   -   ]') !CHARACTER
          call addelm(gdp, lundia, IO_FIL, grnam4, 'FILE-VERSION', ' ', 16, 1, (/1/), ' ', 'Version number of file', '[   -   ]') !CHARACTER
       endif
       !
       ierror = open_datdef(filnam, fds, .false.)
       if (ierror /= 0) goto 9999
       !
       if (first) then
          call defnewgrp(fds, IO_FIL, grnam4, gdp, filnam, errlog=ERRLOG_NONE)
          first = .false.
       endif
       !
       ! initialize group index
       !
       uindex (1,1) = 1 ! start index
       uindex (2,1) = 1 ! end index
       uindex (3,1) = 1 ! increment in time
       !
       ! element 'FLOW-SIMDAT'
       !
       cdum16(1) = simdat
       ierror = putels(fds, grnam4, 'FLOW-SIMDAT', uindex, 1, cdum16)
       if (ierror/= 0) goto 9999
       !
       ! element 'FLOW-SYSTXT'
       !
       cdumcident(1) = trim(full_version)
       ierror = putels(fds, grnam4, 'FLOW-SYSTXT', uindex, 1, cdumcident)
       if (ierror/= 0) goto 9999
       !
       ! element 'FLOW-RUNTXT'
       !
       ierror = putels(fds, grnam4, 'FLOW-RUNTXT', uindex, 1, runtxt)
       if (ierror/= 0) goto 9999
       !
       ! element 'FILE-VERSION'
       ! drogues file  'd'
       ! history file  'h'
       ! map     file  'f'
       ! comm    file  'c'
       !
       cdum16(1) = '00.00.00.00'
       if (ftype(1:1)=='d') then
          call getdrofileversionstring_flow2d3d(cdum16(1))
       elseif (ftype(1:1)=='h') then
          call gethisfileversionstring_flow2d3d(cdum16(1))
       elseif (ftype(1:1)=='m') then
          call getmapfileversionstring_flow2d3d(cdum16(1))
       elseif (ftype(1:1)=='c') then
          call getcomfileversionstring_flow2d3d(cdum16(1))
       else
       endif
       if (ftype(1:1)=='c') then
          !
          ! Check if COM-file is a new one or an existing one
          !
          if (.not. commrd) then
             !
             ! COM-file has been newly generated
             !
             ierror = putels(fds, grnam4, 'FILE-VERSION', uindex, 1, cdum16)
             if (ierror/= 0) goto 9999
          endif
       else
          ierror = putels(fds, grnam4, 'FILE-VERSION', uindex, 1, cdum16)
          if (ierror/= 0) goto 9999
       endif
       !
       ierror = clsnef(fds)
    endif
    !
    ! write error message if error occured and set error= .true.
    !
9999   continue
    if (ierror /= 0) then
       ierror = neferr(0, errmsg)
       call prterr(lundia, 'P004', errmsg)
       error= .true.
    endif
end subroutine wridoc
