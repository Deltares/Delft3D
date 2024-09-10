subroutine sysini(error     ,runid     ,filmrs    ,prgnm     , &
                & version_short ,filmd     ,gdp       )
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
!    Function: - Initialises the FLOW system (EXCEPT for the
!                User Interface prog.)
!              - Determines the program type, hardware type,
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    use globaldata
    use string_module
    use deltares_common_version_module
    use dfparall
    use flow2d3d_version_module
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer       , pointer :: lunmd
    integer       , pointer :: lundia
    integer       , pointer :: lunprt
    integer       , pointer :: lunscr
    real(fp)      , pointer :: earthrad
    real(fp)      , pointer :: eps
    real(fp)      , pointer :: amiss
    real(sp)      , pointer :: smiss
    integer       , pointer :: imiss
    real(hp)      , pointer :: dearthrad
    real(hp)      , pointer :: deps
!
! Global variables
!
    logical                     :: error         !!  Flag=TRUE if an error is encountered
    character(*)                :: filmd         !!  File name for MD FLOW file
    character(*)                :: runid         !!  Run identification code for the current simulation
    character(*)                :: version_short !!  Version nr. of the current package
    character(12)               :: filmrs        !!  File name for DELFT3D_MOR FLOW input file (MD-flow.xxx)
    character(6)                :: prgnm         !!  Help var. determining the prog. name currently active
!
! Local variables
!
    integer                    :: ierr
    integer                    :: lfil
    integer                    :: lrid         ! Help var. to determine the actual length of RUNID
    integer                    :: lridmx       ! Help var. for lunprt: LRID < 47
    integer                    :: lunhlp       ! Help var.
    integer                    :: n
    integer         , external :: newlun
    logical                    :: ex           ! Help flag = TRUE when file is found
    character(10)              :: date        ! Date to be filled in the header
    character(message_len)     :: txthlp       ! Help var.
    character(20)              :: rundat       ! Current date and time containing a combination of DATE and TIME
    character(256)             :: full_version
    character(256)             :: filtmp       ! Help var. to specify file name
    character(55)              :: txtput       ! Texts to be filled in the header
    type(message_stack)        :: stack
!
!! executable statements -------------------------------------------------------
!
    dearthrad  => gdp%gdconstd%dearthrad
    deps       => gdp%gdconstd%deps
    earthrad   => gdp%gdconst%earthrad
    eps        => gdp%gdconst%eps
    amiss      => gdp%gdconst%amiss
    smiss      => gdp%gdconst%smiss
    imiss      => gdp%gdconst%imiss
    lunmd      => gdp%gdinout%lunmd
    lundia     => gdp%gdinout%lundia
    lunprt     => gdp%gdinout%lunprt
    lunscr     => gdp%gdinout%lunscr
    !
    ! initialisation of constants in gdconst and gdconstd
    !
    earthrad = 6378137.0   ! Mathworld, IUGG
    eps      = 1.0e-6
    amiss    = -999.0_fp
    smiss    = -999.0_sp
    imiss    = -999
    !
    dearthrad = 6378137.0d0
    deps      = 1.0d-12
    !
    ! initialisation
    ! NOTE: lundia gets fake unit number (< 10), for inquire statement
    !           in triend when lundia is not yet opened
    !
    txtput = ' '
    if (prgnm=='tdatom') then
       txtput = 'Part I    - Initialisation Time Dep. Data module...'
    else
       txtput = 'Part III  - Initialisation of the Execution module...'
    endif
    !
    ! initialisation unit numbers
    ! a lun gets fake unit number (< 10), for the inquire statement
    ! in triend in case the lun is not yet opened
    !
    lunscr = 6
    lundia = 0
    lunmd  = 8
    !
    ! platform definition
    !
    call util_getenv('ARCH',txthlp)
    call small(txthlp,message_len)
    if (txthlp == 'win32' .or. txthlp == 'w32' .or. txthlp == 'x86') then
       gdp%arch = 'win32'
    elseif (txthlp == 'win64' .or. txthlp == 'x64') then
       gdp%arch = 'win64'
    else
       gdp%arch = 'linux'
    endif
    gdp%errorcode = 0
    !
    ! check userfile consistency
    !
    full_version  = ' '
    version_short = ' '
    !
    ! Force the version information of the module precision to be displayed within the what tool
    !
    call getfullversionstring_deltares_common(full_version)
    call getfullversionstring_flow2d3d(full_version)
    call getshortversionstring_flow2d3d(version_short)
    call flwlic(lunscr    ,full_version ,prgnm     ,gdp       )
    !
    ! put header on screen
    !
    if (.not.parll .or. (parll .and. inode==master)) then
    write (lunscr, '(a)') txtput
    endif
    !
    ! initialisation id's (computer, runid ) and open lundia
    !
    call iniid(error     ,prgnm     ,runid     ,filmd     ,filmrs    , &
             & gdp       )
    if (error) goto 9999
    call remove_leading_spaces(runid     ,lrid      )
    if (.not.parll .or. (parll .and. inode==master)) then
       if (prgnm == 'tdatom') then
          write (lunscr, '(a,a)') '            runid : ', runid(:lrid)
       endif
    endif
    gdp%runid = runid
    !
    ! Date and time
    !
    call dattim(rundat    )
    date(1:4)  = rundat(1:4)
    date(5:5)  = '-'
    date(6:7)  = rundat(6:7)
    date(8:8)  = '-'
    date(9:10) = rundat(9:10)
    !
    call remove_leading_spaces(filmd     ,lfil      )
    !
    ! get source code location
    !
    txthlp = deltares_common_source_code
    n = index(txthlp,'/src/utils_lgpl') ! regular checkout with src and examples level
    if (n==0) then
        n = index(txthlp,'/utils_lgpl') ! reduced checkout with src and examples level
    endif
    if (n==0) then
        txthlp = 'unknown source code location'
    else
        txthlp = txthlp(16:n-1)
    endif
    !
    ! write start date and time in LUNDIA
    !
    write (lundia, '(a)')
    write (lundia, '(80a1)') ('*', n = 1, 80)
    write (lundia, '(a)')   '***'
    write (lundia, '(2a)')  '*** ', trim(full_version)
    write (lundia, '(2a)')  '***           built from : ', trim(txthlp)
    write (lundia, '(a)')   '***'
    write (lundia, '(2a)')  '***           runid      : ', trim(runid)
    if (gdp%gdnfl%add_uniqueid) then
        write (lundia, '(2a)')  '***           uniqueid   : ', trim(gdp%uniqueid)
    endif
    write (lundia, '(4a)')  '***           date,time  : ', date, ',', rundat(11:19)
    write (lundia, '(a)')   '***'
    write (lundia, '(80a1)') ('*', n = 1, 80)
    write (lundia, '(a)')
    !
    if (prgnm=='tdatom') goto 9999
    !
    ! copy contents of old td-diag file created by tdatom
    ! test for ERRORS and if found, stop
    !
    if (inode==master) then
       filtmp(1:8 + lrid) = 'td-diag.' // runid
       inquire (file = filtmp(1:8 + lrid), exist = ex)
       if (ex) then
          open (newunit=lunhlp, file = filtmp(1:8 + lrid), form = 'formatted')
   50     continue
          read (lunhlp, '(a)', end = 100, err = 100) txthlp
          write (lundia, '(a,a)') '      ',trim(txthlp)
          ierr = index(txthlp, 'ERROR')
          if (ierr/=0) error = .true.
          goto 50
  100     continue
          close (lunhlp, status = 'delete')
       endif
    endif
    call dfbroadc_gdp(ierr, 1, dfint, gdp)
 9999 continue
end subroutine sysini
