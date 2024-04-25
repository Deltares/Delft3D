subroutine rdfcio(lunmd     ,lundia    ,error     ,mdfrec    ,nrrec     , &
                & sferic    ,anglat    ,dy        ,filcio    ,fmtcio    , &
                & mmax      ,nmax      ,nmaxus    ,fcorio    ,gdp       )
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
!    Function: - Reads the space varying coriolis field records
!                from attribute file FILCIO
!              - or calcutale coriolis values depending on
!                ANGLAT and DY for SFERIC = .true. or on ANGLAT
!                for all other situations
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
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
    integer                                                                          :: lundia !  Description and declaration in inout.igs
    integer                                                                          :: lunmd  !  Description and declaration in inout.igs
    integer                                                                          :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                          :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                          :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer                                                                          :: nrrec  !!  Pointer to the record number in the
                                                                                               !!  MD-file
    logical                                                                          :: error  !!  Flag=TRUE if an error is encountered
    logical                                                            , intent(in)  :: sferic !  Description and declaration in tricom.igs
    real(fp)                                                           , intent(in)  :: anglat !!  - Angle of latitude of the model
                                                                                               !!    centre (used to determine the coef.
                                                                                               !!    for the coriolis force)
                                                                                               !!  - In spherical coordinates this para-
                                                                                               !!    meter equals the angle of latitude
                                                                                               !!    for the origin (water level point)
                                                                                               !!    after INIPHY anglat = 0.
    real(fp)                                                           , intent(in)  :: dy     !!  Uniform grid-distance in the y-dir. in meters
                                                                                               !!  if sferic = T dy := dtheta in (decimals) degrees
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: fcorio !  Description and declaration in esm_alloc_real.f90
    character(*)                                                                     :: filcio !!  File name for variable coriolis values
    character(2)                                                       , intent(out) :: fmtcio
    character(300)                                                                   :: mdfrec !!  Standard rec. length in MD-file (300)
!
! Local variables
!
    integer       :: lenc   ! Help var. (length of var. chulp to be looked for in the MD-file) 
    integer       :: lkw    ! Length (in characters) of keyword 
    integer       :: m      ! Help loop var. 
    integer       :: n      ! Help loop var. 
    integer       :: nlook  ! Help var.: nr. of data to look for in the MD-file 
    integer       :: ntrec  ! Help. var to keep track of NRREC 
    logical       :: found  ! FOUND=TRUE if KEYW in the MD-file was found 
    logical       :: lerror ! Flag=TRUE if a local error is encountered 
    logical       :: newkw  ! Logical var. specifying whether a new recnam should be read from the MD-file or just new data in the continuation line 
    real(fp)      :: sidday ! Help var. 
    real(fp)      :: ff     ! Help var. (for Coriolis parameter) 
    real(fp)      :: posit
    character(12) :: fildef ! Default file name (usually = blank) 
    character(6)  :: keyw   ! Name of record to look for in the MD-file (usually KEYWRD or RECNAM) 
!
!! executable statements -------------------------------------------------------
!
    itis  => gdp%gdrdpara%itis
    !
    filcio = ' '
    fmtcio = 'FR'
    !
    ! initialize local parameters
    !
    newkw = .true.
    fildef = ' '
    nlook = 1
    !
    sidday = (365.25/366.25)*86400.0
    !
    ! locate 'Filcio' record for coriolis values in attribute file
    !
    filcio = fildef
    fmtcio = 'FR'
    !
    keyw = 'Filcio'
    ntrec = nrrec
    lkw = 6
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
                 & mdfrec    ,filcio    ,fildef    ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
          filcio = fildef
       endif
    endif
    !
    ! Coriolis values in file? <YES>
    !
    if (filcio/=fildef) then
       !
       ! Test attribute file in combination with SFERIC not allowed
       !
       if (sferic) then
          error = .true.
          call prterr(lundia    ,'U135'    ,' '       )
       !
       ! Test file existence
       !
       else
          call ciofil(lundia    ,error     ,filcio    ,mmax      ,nmax      , &
                    & nmaxus    ,fcorio    ,gdp       )
       endif
    !
    ! Coriolis values in file? <NO>
    !
    else
       !
       ! Set coriolis parameter based upon anglat
       !
       if (.not. sferic) then
          ff = sin(anglat*degrad)*4.0*pi/sidday
          do m = 1, mmax
             do n = 1, nmaxus
                fcorio(n, m) = ff
             enddo
          enddo
       endif
    endif
end subroutine rdfcio
