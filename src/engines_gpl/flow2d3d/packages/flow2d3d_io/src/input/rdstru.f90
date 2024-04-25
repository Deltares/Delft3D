subroutine rdstru(lunmd     ,lundia    ,error     ,mdfrec    ,nrrec     , &
                & riglid    ,struct    ,nsluv     ,mmax      ,nmax      , &
                & nmaxus    ,kmax      ,nambar    ,kspu      ,kspv      , &
                & mnbar     ,cbuv      ,pship     ,ubrlsu    ,ubrlsv    , &
                & nxbub     ,nbub,nsrc ,mnksrc    ,namsrc    , &
                & uwtypu    ,uwtypv    ,dpu       ,dpv       ,hkru      , &
                & hkrv      ,cdwztu    ,cdwzbu    ,cdwztv    ,cdwzbv    , &
                & cdwlsu    ,cdwlsv    , gdp)
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
!    Function: - Read rigid lid factor from MDF file
!              - Reads the attribute file names from MDF file for
!                Fillwl, Fil2dw, Filgat, Filrgs,
!                Filppl, Filbrg, Filfls, Filbub, and/or Filbar
!              - UBRLSU,UBRLSV,UWTYPU,UWTYPV are filled
!                by reading contents from FILRGS or FILLWL file
!              - HKRU/HKRV are filled by reading contents from
!                FIL2DW
!              - SHIP-array  is filled by reading the array
!                contents from the Filfls
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
    integer , pointer :: itis
    real(fp), pointer :: thetaw
!
! Global variables
!
    integer                                                                                 :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                                 :: lundia !  Description and declaration in inout.igs
    integer                                                                                 :: lunmd  !  Description and declaration in inout.igs
    integer                                                                                 :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                                 :: nbub   !  Description and declaration in esm_alloc_int.f90
    integer                                                                                 :: nsrc   !  Description and declaration in esm_alloc_int.f90
    integer                                                                                 :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                                 :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer                                                                                 :: nrrec  !!  Pointer to the record number in the MD-file
    integer                                                                                 :: nsluv  !  Description and declaration in dimens.igs
    integer                                                                                 :: nxbub  !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(5, nsluv)                                                      :: mnbar  !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(3, nsrc)                                                       :: mnksrc  !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax)              :: kspu   !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax)              :: kspv   !  Description and declaration in esm_alloc_int.f90
    logical                                                                                 :: error  !!  Flag=TRUE if an error is encountered
    logical                                                                   , intent(out) :: struct !  Description and declaration in procs.igs
    real(fp)                                                                                :: riglid !!  Rigid lid factor to reduce horizontal
                                                                                                      !!  wet area (incompressible)
    real(fp)     , dimension(4, nsluv)                                                      :: cbuv   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                      :: cdwlsu
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                      :: cdwlsv
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                      :: cdwzbu
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                      :: cdwztu
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                      :: cdwzbv
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                      :: cdwztv
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                      :: dpu    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                      :: dpv    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                      :: hkru   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                      :: hkrv   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                      :: pship  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                      :: uwtypu !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                      :: uwtypv !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)                :: ubrlsu !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)                :: ubrlsv !  Description and declaration in esm_alloc_real.f90
    character(*)                                                                            :: mdfrec !!  Standard rec. length in MD-file (300)
    character(20), dimension(nsluv)                                                         :: nambar !  Description and declaration in esm_alloc_char.f90
    character(20), dimension(nsrc)                                            , intent(out) :: namsrc !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer                :: istruc ! =5: rigid sheet =3: local weir 
    integer                :: ja_upw ! Default around discharge locations upwind is expected = 1 For no upwind JA_UPW = -1 
    integer                :: lenc   ! Help var. (length of var. chulp to be looked for in the MD-file) 
    integer                :: lkw    ! Length (in characters) of keyword 
    integer                :: nlook  ! Help var.: nr. of data to look for in the MD-file 
    integer                :: ntrec  ! Help. var to keep track of NRREC 
    logical                :: defaul ! Flag set to YES if default value may be applied in case var. read is empty (ier <= 0, or nrread < nlook) 
    logical                :: found  ! FOUND=TRUE if KEYW in the MD-file was found 
    logical                :: lerror ! Flag=TRUE if a local error is encountered 
    logical                :: newkw  ! Logical var. specifying whether a new recnam should be read from the MD-file or just new data in the continuation line 
    logical                :: upwind ! Flag indicates whether upwind numerical scheme should be used near structure
    real(fp)               :: rdef   ! Dummy real variable (help var.) 
    real(fp), dimension(1) :: rval   ! Help array (real) where the data, recently read from the MD-file, are stored temporarily 
    character(1)           :: cdef   ! Help var. 
    character(1)           :: chulp  ! Help var. 
    character(12)          :: fildef ! Default file name (usually = blank) 
    character(6)           :: keyw   ! Name of record to look for in the MD-file (usually KEYWRD or RECNAM) 
    character(256)         :: fil2dw !!  File name for 2D Weirs
    character(256)         :: filbar !!  File name for Barriers/contr.gates
    character(256)         :: filbrg !!  File name for Bridges
    character(256)         :: filbub !!  File name for Bubble screen
    character(256)         :: filfls !!  File name for Floating Structures
    character(256)         :: filgat !!  File name for Gates
    character(256)         :: fillwl !!  File name for Local Weirs
    character(256)         :: filppl !!  File name for Porous Plates
    character(256)         :: filrgs !!  File name for Rigid Sheets
    character(256)         :: filcdw 
!
!! executable statements -------------------------------------------------------
!
    itis  => gdp%gdrdpara%itis
    thetaw=> gdp%gdrivpro%thetaw
    !
    newkw  = .true.
    fildef = ' '
    nlook  = 1
    defaul = .true.
    !
    ! Initialize global parameters
    !
    struct = .false.
    riglid = 1.0
    !
    ! Look for Rigid lid factor
    !
    rdef  = riglid
    keyw  = 'Riglid'
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
       call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,rval      ,rdef      ,defaul    ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
          riglid = rdef
       else
          riglid = rval(1)
       endif
    endif
    !
    ! locate 'Filfls' record for depth values in extra input file
    ! Initialize global parameter
    !
    filfls = ' '
    !
    keyw   = 'Filfls'
    ntrec  = nrrec
    lkw    = 6
    call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
              & 'NO'      )
    lerror = .false.
    !
    ! not found ?
    !
    if (found) then
       lenc = 12
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,filfls    ,fildef    ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
          filfls = fildef
       endif
       !
       ! Floating structure depth values in file? <YES>
       !
       if (filfls /= fildef) then
          !
          ! read flag for Upwind
          !
          keyw  = 'Upwfls'
          ntrec = nrrec
          nlook = 1
          lkw   = 6
          lenc  = 1
          call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
                    & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
                    & 'NO'      )
          !
          ! keyword in md-file (FOUND) then read (default = Y/y)
          !
          ja_upw = 1
          if (found) then
             cdef  = ' '
             chulp = cdef
             call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                       & mdfrec    ,chulp     ,cdef      ,lenc      ,nrrec     , &
                       & ntrec     ,lundia    ,gdp       )
             if (lerror) then
                error = .true.
                goto 9999
             else
                call small(chulp, 1)
                if (chulp == 'n') then
                   ja_upw = -1
                endif
             endif
          endif
          !
          ! Read attribute file; Flag for Structures STRUCT set to .true.
          !
          istruc = 2*ja_upw
          struct = .true.
          call flsfil(lundia    ,error     ,filfls    ,mmax      ,nmax      , &
                    & nmaxus    ,kmax      ,istruc    ,kspu      ,kspv      , &
                    & pship     ,gdp       )
          if (error) goto 9999
       endif
    endif
    !
    ! Look for filename for Weir data first
    ! locate 'Fillwl' record for weir info in extra input file
    ! Initialize global parameter
    !
    fillwl = ' '
    keyw   = 'Fillwl'
    ntrec  = nrrec
    lkw    = 6
    call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
              & 'NO'      )
    lerror = .false.
    !
    ! not found ?
    !
    if (found) then
       lenc = 12
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,fillwl    ,fildef    ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
          fillwl = fildef
       endif
       !
       ! weir values in file? <YES>
       !
       if (fillwl /= fildef) then
          !
          ! read flag for Upwind
          !
          keyw  = 'Upwlwl'
          ntrec = nrrec
          nlook = 1
          lkw   = 6
          lenc  = 1
          call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
                    & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
                    & 'NO'      )
          !
          ! keyword in md-file (FOUND) then read (default = Y/y)
          !
          ja_upw = 1
          if (found) then
             cdef  = ' '
             chulp = cdef
             call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                       & mdfrec    ,chulp     ,cdef      ,lenc      ,nrrec     , &
                       & ntrec     ,lundia    ,gdp       )
             if (lerror) then
                error = .true.
                goto 9999
             else
                call small(chulp, 1)
                if (chulp=='n') then
                   ja_upw = -1
                endif
             endif
          endif
          !
          ! Test file existence; Flag for Structures STRUCT set to .true.
          !
          struct = .true.
          istruc = 3*ja_upw
          call strfil(lundia    ,error     ,fillwl    ,mmax      , &
                    & nmaxus    ,kmax      ,istruc    ,kspu      ,kspv      , &
                    & ubrlsu    ,ubrlsv    ,uwtypu    ,uwtypv    ,dpu       , &
                    & dpv       ,gdp       )
          if (error) goto 9999
       endif
    endif
    !
    ! Look for filename for Gate data next
    ! locate 'Filgat' record for gate info in extra input file
    ! Initialize global parameter
    !
    filgat = ' '
    keyw   = 'Filgat'
    ntrec  = nrrec
    lkw    = 6
    call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
              & 'NO'      )
    lerror = .false.
    !
    ! not found ?
    !
    if (found) then
       lenc = 12
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,filgat    ,fildef    ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
          filgat = fildef
       endif
       !
       ! gate values in file? <YES>
       !
       if (filgat /= fildef) then
          !
          ! Test file existence; Flag for Structures STRUCT set to .true.
          !
          struct = .true.
          istruc = 4
          call strfil(lundia    ,error     ,filgat    ,mmax      , &
                    & nmaxus    ,kmax      ,istruc    ,kspu      ,kspv      , &
                    & ubrlsu    ,ubrlsv    ,uwtypu    ,uwtypv    ,dpu       , &
                    & dpv       ,gdp       )
          if (error) goto 9999
       endif
    endif
    !
    ! Look for filename for Rigid sheet data next
    ! Locate 'Filrgs' record for rigid sheet info in extra input file
    ! Initialize global parameter
    !
    filrgs = ' '
    keyw   = 'Filrgs'
    ntrec  = nrrec
    lkw    = 6
    call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
              & 'NO'      )
    lerror = .false.
    !
    ! not found ?
    !
    if (found) then
       lenc = 12
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,filrgs    ,fildef    ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! Reading error?
       !
       if (lerror) then
          lerror = .false.
          filrgs = fildef
       endif
       !
       ! Rigid sheet
       !
       if (filrgs /= fildef) then
          !
          ! read flag for Upwind
          !
          keyw  = 'Upwrgs'
          ntrec = nrrec
          nlook = 1
          lkw   = 6
          lenc  = 1
          call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
                    & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
                    & 'NO'      )
          !
          ! keyword in md-file (FOUND) then read (default = Y/y)
          !
          ja_upw = 1
          if (found) then
             cdef = ' '
             chulp = cdef
             call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                       & mdfrec    ,chulp     ,cdef      ,lenc      ,nrrec     , &
                       & ntrec     ,lundia    ,gdp       )
             if (lerror) then
                error = .true.
                goto 9999
             else
                call small(chulp, 1)
                if (chulp=='n') then
                   ja_upw = -1
                endif
             endif
          endif
          !
          ! Test file existence; Flag for Structures STRUCT set to .true.
          !
          struct = .true.
          istruc = 5*ja_upw
          call strfil(lundia    ,error     ,filrgs    ,mmax      , &
                    & nmaxus    ,kmax      ,istruc    ,kspu      ,kspv      , &
                    & ubrlsu    ,ubrlsv    ,uwtypu    ,uwtypv    ,dpu       , &
                    & dpv       ,gdp       )
          if (error) goto 9999
       endif
    endif
    !
    ! Look for filename for Porous Plate data
    ! Locate 'Filppl' record for porous plate info in extra input file
    ! Initialize global parameter
    !
    filppl = ' '
    keyw   = 'Filppl'
    ntrec  = nrrec
    lkw    = 6
    call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
              & 'NO'      )
    lerror = .false.
    !
    ! not found ?
    !
    if (found) then
       lenc = 12
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,filppl    ,fildef    ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! Reading error?
       !
       if (lerror) then
          lerror = .false.
          filppl = fildef
       endif
       !
       ! Porous Plate
       !
       if (filppl /= fildef) then
          !
          ! read flag for Upwind
          !
          keyw  = 'Upwppl'
          ntrec = nrrec
          nlook = 1
          lkw   = 6
          lenc  = 1
          call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
                    & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
                    & 'NO'      )
          !
          ! keyword in md-file (FOUND) then read (default = Y/y)
          !
          ja_upw = 1
          if (found) then
             cdef  = ' '
             chulp = cdef
             call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                       & mdfrec    ,chulp     ,cdef      ,lenc      ,nrrec     , &
                       & ntrec     ,lundia    ,gdp       )
             if (lerror) then
                error = .true.
                goto 9999
             else
                call small(chulp, 1)
                if (chulp=='n') then
                   ja_upw = -1
                endif
             endif
          endif
          !
          ! Test file existence; Flag for Structures STRUCT set to .true.
          !
          struct = .true.
          istruc = 6*ja_upw
          call strfil(lundia    ,error     ,filppl    ,mmax      , &
                    & nmaxus    ,kmax      ,istruc    ,kspu      ,kspv      , &
                    & ubrlsu    ,ubrlsv    ,uwtypu    ,uwtypv    ,dpu       , &
                    & dpv       ,gdp       )
          if (error) goto 9999
       endif
    endif
    !
    ! Look for filename for Bridge data next
    ! Locate 'Filbrg' record for bridge info in extra input file
    ! Initialize global parameter
    !
    filbrg = ' '
    keyw   = 'Filbrg'
    ntrec  = nrrec
    lkw    = 6
    call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
              & 'NO'      )
    lerror = .false.
    !
    ! not found ?
    !
    if (found) then
       lenc = 12
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,filbrg    ,fildef    ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! Reading error?
       !
       if (lerror) then
          lerror = .false.
          filbrg = fildef
       endif
       !
       ! Bridge
       !
       if (filbrg /= fildef) then
          !
          ! read flag for Upwind
          !
          keyw  = 'Upwbrg'
          ntrec = nrrec
          nlook = 1
          lkw   = 6
          lenc  = 1
          call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
                    & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
                    & 'NO'      )
          !
          ! keyword in md-file (FOUND) then read (default = Y/y)
          !
          ja_upw = 1
          if (found) then
             cdef  = ' '
             chulp = cdef
             call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                       & mdfrec    ,chulp     ,cdef      ,lenc      ,nrrec     , &
                       & ntrec     ,lundia    ,gdp       )
             if (lerror) then
                error = .true.
                goto 9999
             else
                call small(chulp, 1)
                if (chulp=='n') then
                   ja_upw = -1
                endif
             endif
          endif
          !
          ! Test file existence; Flag for Structures STRUCT set to .true.
          !
          struct = .true.
          istruc = 7*ja_upw
          call strfil(lundia    ,error     ,filbrg    ,mmax      , &
                    & nmaxus    ,kmax      ,istruc    ,kspu      ,kspv      , &
                    & ubrlsu    ,ubrlsv    ,uwtypu    ,uwtypv    ,dpu       , &
                    & dpv       ,gdp       )
          if (error) goto 9999
       endif
    endif
    !
    ! Special HONGKONG Filbri instead of Filbrg and No Upwind
    ! Look for filename for Bridge data next,
    ! Locate 'Filbri' record for bridge info in extra input file
    ! Initialize global parameter
    !
    filbrg = ' '
    keyw   = 'Filbri'
    ntrec  = nrrec
    lkw    = 6
    call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
              & 'NO'      )
    lerror = .false.
    !
    ! not found ?
    !
    if (found) then
       lenc = 12
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,filbrg    ,fildef    ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! Reading error?
       !
       if (lerror) then
          lerror = .false.
          filbrg = fildef
       endif
       !
       ! Bridge
       !
       if (filbrg /= fildef) then
          !
          ! read flag for Upwind
          !
          ja_upw = -1
          !
          ! Test file existence
          !
          istruc = 7*ja_upw
          call strfil(lundia    ,error     ,filbrg    ,mmax      , &
                    & nmaxus    ,kmax      ,istruc    ,kspu      ,kspv      , &
                    & ubrlsu    ,ubrlsv    ,uwtypu    ,uwtypv    ,dpu       , &
                    & dpv       ,gdp       )
          if (error) goto 9999
       endif
    endif
    !
    ! Look for filename for Barrier data
    ! Locate 'Filbar' record for barrier info in extra input file
    ! Initialize global parameter
    !
    filbar = ' '
    keyw   = 'Filbar'
    ntrec  = nrrec
    lkw    = 6
    call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
              & 'NO'      )
    lerror = .false.
    !
    ! not found ?
    !
    if (found) then
       lenc = 12
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,filbar    ,fildef    ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! Reading error?
       !
       if (lerror) then
          lerror = .false.
          filbar = fildef
       endif
       !
       ! Barrier info in file? <YES>
       !
       if (filbar /= fildef) then
          ja_upw = 1
          !
          ! Test file existence; Flag for Structures STRUCT set to .true.
          !
          istruc = 8*ja_upw
          struct = .true.
          call barfil(lundia    ,filbar    ,error     ,mmax      ,nmax      , &
                    & nmaxus    ,kmax      ,kspu      ,kspv      ,nsluv     , &
                    & mnbar     ,nambar    ,cbuv      ,gdp       )
          if (error) goto 9999
       endif
    endif
    !
    ! Look for filename for Bubblescreen data
    ! Locate 'Filbub' record for bubblescreen info in extra input file
    ! Initialize global parameter
    !
    filbub = fildef
    call prop_get_string(gdp%mdfile_ptr, '*', 'Filbub', filbub)
    !
    ! Bubble screen info in file? <YES>
    !
    if (filbub /= fildef) then
       ja_upw = 1
       !
       ! Test file existence; Flag for Structures STRUCT set to .true.
       !
       call bubfil(lundia    ,filbub    ,error     ,mmax      ,nmax      , &
                 & nmaxus    ,kmax      ,nsrc      ,nxbub     ,nbub      , &
                 & mnksrc    ,namsrc    ,gdp       )
       if (error) goto 9999
    endif
    !
    ! Read Fil2dw
    !
    fil2dw = ' '
    call prop_get_string(gdp%mdfile_ptr,'*','Fil2dw',fil2dw)
    if (fil2dw /= ' ') then
       !
       ! read flag for Upwind
       !
       upwind = .true.
       call prop_get_logical(gdp%mdfile_ptr,'*','Upw2dw',upwind)
       !
       istruc = 9
       if (.not.upwind) istruc = -istruc
       !
       ! Test file existence; Flag for Structures STRUCT set to .true.
       !
       struct = .true.
       call strfil(lundia    ,error     ,fil2dw    ,mmax      , &
                 & nmaxus    ,kmax      ,istruc    ,kspu      ,kspv      , &
                 & ubrlsu    ,ubrlsv    ,uwtypu    ,uwtypv    ,hkru      , &
                 & hkrv      ,gdp       )
       if (error) goto 9999
       !
       ! If weir file specified then read 'Thetaw' relaxation factor for
       ! sub-grid weir energy loss
       !
       thetaw = 0.0_fp
       call prop_get(gdp%mdfile_ptr,'*','Thetaw',thetaw)
    endif
    !
    ! Look for filename for Fixed gate; 'Filcdw' record
    ! Initialize global parameter
    !
    filcdw = ' '
    keyw   = 'Filcdw'
    ntrec  = nrrec
    lkw    = 6
    call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
              & 'NO'      )
    lerror = .false.
    !
    ! not found ?
    !
    if (found) then
       lenc = 12
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,filcdw    ,fildef    ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
          filcdw = fildef
       endif
       !
       ! weir values in file? <YES>
       !
       if (filcdw /= fildef) then
          !
          ! read flag for Upwind
          !
          keyw  = 'Upwcdw'
          ntrec = nrrec
          nlook = 1
          lkw   = 6
          lenc  = 1
          call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
                    & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
                    & 'NO'      )
          !
          ! keyword in md-file (FOUND) then read (default = Y/y)
          !
          ja_upw = 1
          if (found) then
             cdef  = ' '
             chulp = cdef
             call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                       & mdfrec    ,chulp     ,cdef      ,lenc      ,nrrec     , &
                       & ntrec     ,lundia    ,gdp       )
             if (lerror) then
                error = .true.
                goto 9999
             else
                call small(chulp, 1)
                if (chulp=='n') then
                   ja_upw = -1
                endif
             endif
          endif
          !
          ! Test file existence; Flag for Structures STRUCT set to .true.
          !
          struct = .true.
          istruc = 10*ja_upw
          call strfil(lundia    ,error     ,filcdw    ,mmax      , &
                    & nmaxus    ,kmax      ,istruc    ,kspu      ,kspv      , &
                    & cdwlsu    ,cdwlsv    ,cdwztu    ,cdwztv    ,cdwzbu    , &
                    & cdwzbv    ,gdp       )
          if (error) goto 9999
       endif
    endif
 9999 continue
end subroutine rdstru
