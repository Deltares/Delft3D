subroutine rdhyb(lunmd     ,lundia    ,error     ,nrrec     ,mdfrec    , &
               & roumet    ,threed    ,filrgh    ,fmtrgh    ,ccofu     , &
               & ccofv     ,wave      ,rouwav    ,mmax      ,nmax      , &
               & nmaxus    ,cfurou    ,cfvrou    ,gdp       )
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
!    Function: - Reads the following records from the MD-file:
!                ROUMET, FILRGH, FMTRGH, CCOFU, CCOFV and
!                TKEMOD and ROUWAV
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
!
! Global variables
!
    integer                                                          :: lundia !  Description and declaration in inout.igs
    integer                                                          :: lunmd  !  Description and declaration in inout.igs
    integer                                                          :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                          :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                          :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer                                                          :: nrrec  !!  Pointer to the record number in the
                                                                               !!  MD-file
    logical                                                          :: error  !!  Flag=TRUE if an error is encountered
    logical                                            , intent(in)  :: threed !  Description and declaration in procs.igs
    logical                                            , intent(in)  :: wave   !  Description and declaration in procs.igs
    real(fp)                                                         :: ccofu  !!  Array containing the uniform bottom
                                                                               !!  roughness coefficient in the x- dir.
                                                                               !!  (value depends on ROUMET)
    real(fp)                                                         :: ccofv  !!  Array containing the uniform bottom
                                                                               !!  roughness coefficient in the y- dir.
                                                                               !!  (value depends on ROUMET)
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 3) :: cfurou !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 3) :: cfvrou !  Description and declaration in esm_alloc_real.f90
    character(*)                                                     :: filrgh !!  File name for bedstresses
                                                                               !! file will be read formatted !!
    character(*)                                                     :: mdfrec !!  Standard rec. length in MD-file (300)
    character(1)                                                     :: roumet !!  Bed stress formulation specified:
                                                                               !!   C : Chezy    W : White Colebrook
                                                                               !!   M : Manning  Z : roughness par.
    character(2)                                       , intent(out) :: fmtrgh !!  File format for bedstresses
    character(4)                                                     :: rouwav !  Description and declaration in tricom.igs
!
! Local variables
!
    integer                :: ddb
    integer                :: irou   ! Index number of ROUWAV in ROUTOT 
    integer                :: lenc   ! Help var. (length of var. cvar to be looked for in the MD-file) 
    integer                :: m      ! Help var. 
    integer                :: n      ! Help var. 
    integer                :: nlook  ! Help var.: nr. of data to look for in the MD-file 
    integer                :: ntrec  ! Help. var to keep track of NRREC 
    logical                :: defaul ! Flag set to YES if default value may be applied in case var. read is empty (ier <= 0, or nrread < nlook) 
    logical                :: lerror ! Flag=TRUE if an error is encountered 
    logical                :: newkw  ! Logical var. specifying whether a new recnam should be read from the MD-file or just new data in the continuation line 
    real(fp)               :: rdef   ! Help var. containing default va- lue(s) for real variable 
    character(4)           :: ccdef  ! Default value for string ROUWAV:= 'FR84' 
    character(4)           :: chulp  ! Help var. for reading strings 
    character(6)           :: keyw   ! Name of record to look for in the MD-file (usually KEYWRD or RECNAM) 
    character(11)          :: fmttmp ! Help variable for file format 
    character(45)          :: routot ! Character string containing all posible bottom stress terms for waves 
    !
    data routot/'FR84 MS90 HT91 GM79 DS88 BK67 CJ85 OY88 VR04 '/
!
!! executable statements -------------------------------------------------------
!
    !
    ! initialize local parameters
    !
    ddb    = gdp%d%ddbound
    !
    lerror = .false.
    newkw  = .true.
    defaul = .true.
    nlook  = 1
    !
    ! locate and read 'Roumet' record for bottom roughness formulation
    ! string with blanks, then roumet = 'C'
    !
    roumet = ' '
    call prop_get(gdp%mdfile_ptr,'*','Roumet',roumet)
    if (roumet == ' ') then
       roumet = 'C'
       call prterr(lundia, 'U190', 'No bottom roughness formulation specification')
       write(lundia,'(10x,2a)') 'Using Roumet = ', roumet
    endif
    !
    ! test combination of roumet = Z and threed = .false.
    !
    if (.not.threed .and. roumet=='Z') then
       call prterr(lundia    ,'V046'    ,' '       )
       error = .true.
    endif
    !
    ! locate 'Filrgh' record for non-uniform bottom roughness coeffi-
    ! cients in extra input file
    !
    filrgh = ' '
    call prop_get(gdp%mdfile_ptr, '*', 'Filrgh', filrgh)
    if (filrgh /= ' ') then
       !
       ! spatially varying roughness file specified
       ! locate 'Fmtrgh' record for format definition of input file
       !
       fmtrgh = 'FR'
       call prop_get(gdp%mdfile_ptr, '*', 'Fmtrgh', fmtrgh)
       fmttmp = fmtrgh
       call filfmt(lundia    ,'Fmtrgh'  ,fmttmp    ,lerror    ,gdp       )
       call hybfil(lundia    ,error     ,filrgh    ,fmttmp    ,nmax      , &
                 & mmax      ,nmaxus    ,cfurou    ,cfvrou    ,gdp       )
    else
       !
       ! no bottom roughness file
       !
       ! default value depends on roumet
       ! rdef   = 65.  if roumet = 'C', rdef   .026 if roumet = 'M',
       ! rdef   = 0.13 if roumet = 'W' or rdef .003 if roumet = 'Z'
       !
       if (roumet=='C') then
          rdef = 65.0
       elseif (roumet=='M') then
          rdef = 0.026
       elseif (roumet=='W') then
          rdef = 0.13
       elseif (roumet=='Z') then
          rdef = 0.003
       else
          rdef = 0.0
       endif
       !
       ! 'Ccofu': uniform bottom roughness coefficient in x- direction
       !
       chulp = ' '
       call prop_get(gdp%mdfile_ptr,'*','Ccofu',chulp)
       if (chulp == ' ') then
          ccofu = rdef
          call prterr(lundia, 'U190', 'No bottom roughness specification')
          write(lundia,'(10x,a,f7.3)') 'Using Ccofu = ', ccofu
       else
          ccofu = rdef
          call prop_get(gdp%mdfile_ptr,'*','Ccofu',ccofu)
       endif
       !
       ! write per nmaxus mmax ccofu  in cfurou array
       !
       do n = 1 - ddb, nmaxus
          do m = 1 - ddb, mmax
             cfurou(n, m, 1) = ccofu
          enddo
       enddo
       !
       ! 'Ccofv': uniform bottom roughness coefficient in y- direction
       !
       chulp = ' '
       call prop_get(gdp%mdfile_ptr,'*','Ccofv',chulp)
       if (chulp == ' ') then
          ccofv = rdef
          call prterr(lundia, 'U190', 'No bottom roughness specification')
          write(lundia,'(10x,a,f7.3)') 'Using Ccofv = ', ccofv
       else
          ccofv = rdef
          call prop_get(gdp%mdfile_ptr,'*','Ccofv',ccofv)
       endif
       !
       ! write per nmaxus mmax ccofv  in cfvrou array
       !
       do n = 1 - ddb, nmaxus
          do m = 1 - ddb, mmax
             cfvrou(n, m, 1) = ccofv
          enddo
       enddo
    endif
    !
    ! copy the first contents of the array CF to the second
    !
    do n = 1 - ddb, nmaxus
       do m = 1 - ddb, mmax
          cfurou(n, m, 2) = cfurou(n, m, 1)
          cfvrou(n, m, 2) = cfvrou(n, m, 1)
       enddo
    enddo
    !
    ! locate and read 'Rouwav' record for bottom roughness formulation
    ! for waves if WAVE = .true.
    ! string with blanks, then ROUWAV = 'FR84'
    !
    if (wave) then
       rouwav = 'FR84'
       !
       keyw  = 'Rouwav'
       ntrec = nrrec
       lenc  = 1
       ccdef = rouwav
       chulp = ' '
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,chulp     ,ccdef     ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
          rouwav = ccdef
          call prterr(lundia, 'U190', 'No bottom roughness for waves specification')
          write(lundia,'(10x,2a)') 'Using Rouwav = ', rouwav
       else
          rouwav = chulp
          irou = index(routot, rouwav)
          if (irou==0) then
             call prterr(lundia    ,'V048'    ,' '       )
             !
             ! It should be possible to select no wave effect on shear stress
             ! so instead of: rouwav = ccdef
             !
             rouwav = '    '
          endif
       endif
    else
       rouwav = '    '
    endif
end subroutine rdhyb
