subroutine rdbch(lunmd     ,lundia    ,error     ,nrrec     ,mdfrec    , &
               & filbch    ,fmtbch    ,ntof      ,mxnto     ,kc        , &
               & mxkc      ,omega     ,hydrbc    ,ascon     ,gdp       )
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
!    Function: - Reads the boundary condition records from the
!                MD-file: FILBCH,FMTBCH,AMPAB and PHSAB)
!              - The order of reading is sequential for each
!                opening.
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
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
!
! Global variables
!
    integer                                       :: kc     !  Description and declaration in dimens.igs
    integer                                       :: lundia !  Description and declaration in inout.igs
    integer                                       :: lunmd  !  Description and declaration in inout.igs
    integer                                       :: mxkc   !!  Maximum number of frequencies
    integer                                       :: mxnto  !!  Maximum number of open boundaries
    integer                                       :: nrrec  !!  Pointer to the record number in the MD-file
    integer                                       :: ntof   !  Description and declaration in dimens.igs
    logical                                       :: error  !!  Flag=TRUE if an error is encountered
    real(fp), dimension(4, mxnto, mxkc)           :: hydrbc !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(mxkc)                     :: omega  !  Description and declaration in esm_alloc_real.f90
    character(*)                                  :: filbch !!  File name for the harmonic boundary conditions file
    character(*)                                  :: mdfrec !!  Standard rec. length in MD-file (300)
    character(1)                    , intent(in)  :: ascon  !!  'Y' if open bnd. contains type 'A'
    character(2)                    , intent(out) :: fmtbch !!  Format of filbch
!
! Local variables
!
    integer                :: j
    integer                :: k
    integer                :: lenc   ! Help var. (length of var. cvar to be looked for in the MD-file) 
    integer                :: lkw    ! Length of keyword 
    integer                :: n
    integer                :: nlook  ! Help var.: nr. of data to look for in the MD-file 
    integer                :: ntrec  ! Help. var to keep track of NRREC 
    logical                :: found  ! Flag=TRUE if keyword found 
    logical                :: lerror ! Flag=TRUE if an error is encountered 
    logical                :: newkw  ! Logical var. specifying whether a new recnam should be read from the MD-file or just new data in the continuation line 
    logical                :: nodef  ! Flag set to YES if default value may NOT be applied in case var. read is empty (ier <= 0, or nrread < nlook) 
    real(fp)               :: rdef   ! Help var. containing default va- lue(s) for real variable 
    real(fp), dimension(2) :: rval   ! Help array (real) where the data, recently read from the MD-file, are stored temporarily 
    character(11)          :: fmtdef ! Default file format (usually=blank) 
    character(11)          :: fmttmp ! Help variable for file format 
    character(12)          :: fildef ! Default file name (usually = blank) 
    character(6)           :: keyw   ! Name of record to look for in the MD-file (usually KEYWRD or RECNAM) 
!
!! executable statements -------------------------------------------------------
!
    itis  => gdp%gdrdpara%itis
    !
    lerror = .false.
    newkw  = .true.
    nodef  = .false.
    rdef   = 0.0
    fildef = ' '
    fmtdef = 'FRformatted'
    fmttmp = ' '
    !
    ! if ascon=Y then locate 'Filana' record for astronomical boundary
    ! conditions in extra input file. Empty name not allowed
    !
    if (ascon /= 'Y') then
       !
       ! locate 'FilbcH' record for harmonic boundary conditions
       ! in extra input file
       !
       keyw  = 'FilbcH'
       ntrec = nrrec
       lenc  = len(filbch)
       nlook = 1
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,filbch    ,fildef    ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
          filbch = fildef
       endif
       !
       ! harmonic boundary conditions in file? <YES>
       ! locate 'Fmtbch' record for format definition of input file
       !
       if (filbch /= fildef) then
          keyw  = 'FmtbcH'
          ntrec = nrrec
          lenc  = 2
          nlook = 1
          call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                    & mdfrec    ,fmttmp    ,fmtdef    ,lenc      ,nrrec     , &
                    & ntrec     ,lundia    ,gdp       )
          !
          !
          ! reading error?
          !
          if (lerror) then
             lerror = .false.
             fmttmp = fmtdef(3:)
          else
             !
             ! determine file format (unformatted / freeformatted)
             !
             call filfmt(lundia    ,keyw      ,fmttmp    ,lerror    ,gdp       )
             !
             if (lerror) then
                lerror = .false.
                fmttmp = fmtdef(3:)
             endif
          endif
          fmtbch = 'FR'
          if (fmttmp(:2) == 'un') then
             fmtbch = 'UN'
          endif
          !
          ! read data from external file
          !
          call bchfil(lundia    ,error     ,filbch    ,fmttmp    ,ntof      , &
                    & mxnto     ,kc        ,mxkc      ,omega     ,hydrbc    , &
                    & gdp       )
       !
       ! harmonic boundary conditions in file? <NO>
       ! first locate and read 'Omega' record for kc frequencies
       ! default value not allowed incase ier < 0
       !
       elseif (ntof > 0) then
          keyw  = 'Omega '
          ntrec = nrrec
          call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,kc        , &
                    & mdfrec    ,omega     ,rdef      ,nodef     ,nrrec     , &
                    & ntrec     ,lundia    ,gdp       )
          !
          ! reading error?
          !
          if (lerror) then
             error = .true.
             lerror = .false.
          endif
          !
          ! harmonic boundary conditions contains a group of records
          ! all records part of the group are supposed to lie between two
          ! records with keyword 'Ampab '
          ! first set records = first opening => start on top of file
          !
          rewind (lunmd)
          read (lunmd, '(a300)') mdfrec
          !
          nlook = 2
          nrrec = 1
          ntrec = nrrec
          !
          do n = 1, ntof
             !
             ! locate and read 'Ampab ' record for each k := <1,kc>
             ! default value not allowed incase ier < or = 0
             ! NOTE: if kc = 1, no phases are read which means we need to
             ! read a new record to get ampab of next ntof
             !
             newkw = .true.
             if (kc==1) then
                newkw = .false.
             endif
             do k = 1, kc
                keyw = 'Ampab '
                call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                          & mdfrec    ,rval      ,rdef      ,nodef     ,nrrec     , &
                          & ntrec     ,lundia    ,gdp       )
                !
                !
                ! reading error?
                !
                if (lerror) then
                   error = .true.
                   lerror = .false.
                   goto 9999
                else
                   !
                   ! no new keyword and copy rval to appropriate array
                   !
                   newkw = .false.
                   do j = 1, 2
                      hydrbc(j, n, k) = rval(j)
                   enddo
                endif
             enddo
             !
             ! locate and read 'Phsab' record for each k := <2,kc>
             ! default value not allowed incase ier < or = 0
             !
             newkw = .true.
             !
             do k = 2, kc
                keyw = 'Phsab '
                call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                          & mdfrec    ,rval      ,rdef      ,nodef     ,nrrec     , &
                          & ntrec     ,lundia    ,gdp       )
                !
                !
                ! reading error?
                !
                if (lerror) then
                   error = .true.
                   lerror = .false.
                   goto 9999
                else
                   !
                   ! no new keyword and copy rval to appropriate array
                   !
                   newkw = .false.
                   do j = 3, 4
                      hydrbc(j, n, k) = rval(j - 2)
                   enddo
                endif
             enddo
          enddo
       else
       endif
    endif
 9999 continue
end subroutine rdbch
