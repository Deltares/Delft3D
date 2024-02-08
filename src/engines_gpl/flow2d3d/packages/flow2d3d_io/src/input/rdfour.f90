subroutine rdfour(lunmd     ,lundia    ,error     ,nrrec     ,mdfrec    , &
                & nofou     ,kmax      ,lstsc     ,lsal      ,ltem      , &
                & gdp       )
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
!    Function: - Read fourier input file, if available
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use string_module
    use system_utils, only: exifil
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
    integer                   :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                   :: lsal   !  Description and declaration in dimens.igs
    integer                   :: lstsc  !  Description and declaration in dimens.igs
    integer                   :: ltem   !  Description and declaration in dimens.igs
    integer                   :: lundia !  Description and declaration in inout.igs
    integer                   :: lunmd  !  Description and declaration in inout.igs
    integer                   :: nofou  !  Description and declaration in dimens.igs
    integer                   :: nrrec  !!  Pointer to the record number in the MD-file
    logical                   :: error  !!  Flag=TRUE if an error is encountered
    character(*)              :: mdfrec !!  Standard rec. length in MD-file (300)
!
! Local variables
!
    integer                        :: lenc      ! Help var. (length of var. cvar to be looked for in the MD-file) 
    integer                        :: lfile     ! Length of file name 
    integer                        :: lkw       ! Length of keyword 
    integer                        :: lunfou    ! Unit number fourier input file 
    integer         , external     :: newlun
    integer                        :: nlook     ! Help var.: nr. of data to look for in the MD-file 
    integer                        :: ntrec     ! Help. var to keep track of NRREC 
    logical                        :: found     ! Flag=TRUE if keyword found 
    logical                        :: lerror    ! Flag=TRUE if an error is encountered 
    logical                        :: newkw     ! Logical var. specifying whether a new recnam should be read from the MD-file or just new data in the continuation line 
    character(12)                  :: fildef    ! Default file name (usually = blank) 
    character(256)                 :: filfou    ! File name for fourier analysis input 
    character(6)                   :: keyw      ! Name of record to look for in the MD-file (usually KEYWRD or RECNAM) 
    character(9)                   :: fmtfou    ! Format of fourier analysis input file 
!
!
!! executable statements -------------------------------------------------------
!
!
    itis  => gdp%gdrdpara%itis
    !
    lerror = .false.
    nlook = 1
    newkw = .true.
    fildef = ' '
    fmtfou = 'formatted'
    !
    !-----locate 'Filfou' record for Fourier analysis
    !
    keyw = 'Filfou'
    newkw = .true.
    ntrec = nrrec
    lkw = 6
    call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
              & 'NO'      )
    lerror = .false.
    !
    !-----not found ?
    !
    filfou = fildef
    if (found) then
       lenc = 12
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,filfou    ,fildef    ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       !
       !-------reading error?
       !
       if (lerror) then
          lerror = .false.
          filfou = fildef
       endif
    endif
    !
    !-----Quantities for Fourier analysis in file
    !
    if (filfou/=fildef) then
       !
       !-------define length of file name
       !
       call remove_leading_spaces(filfou    ,lfile     )
       !
       !-------test file existence <YES>
       !
       if (exifil(filfou, lundia)) then
          !
          !---------read data from external file
          !
          open (newunit=lunfou, file = filfou(1:lfile), form = fmtfou,               &
              & status = 'old')
          call reafou(error     ,lundia    ,lunfou    ,filfou    ,kmax      , &
                    & lstsc     ,lsal      ,ltem      ,nofou     ,gdp       )
          !
          close (lunfou)
       !
       !-------test file existence <NO>
       !
       else
          error = .true.
       endif
    endif
end subroutine rdfour
