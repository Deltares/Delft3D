subroutine rdnamc(lunmd     ,lundia    ,error     ,nrrec     ,mdfrec    , &
                & salin     ,temp      ,lconc     ,lstsc     ,namcon    , &
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
!    Function: Define names of constituents
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
!
! Global variables
!
    integer           , intent(in)  :: lconc  !!  Number of constituents defined by
                                              !!  user (excl. Salinity, Temperature,
                                              !!  sediment, Secondary flow and
                                              !!  quantities for the Turb. models)
    integer           , intent(in)  :: lstsc  !  Description and declaration in dimens.igs
    integer                         :: lundia !  Description and declaration in inout.igs
    integer                         :: lunmd  !  Description and declaration in inout.igs
    integer                         :: nrrec  !!  Pointer to the record number in the
                                              !!  MD-file
    logical           , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    logical           , intent(in)  :: salin  !  Description and declaration in procs.igs
    logical           , intent(in)  :: temp   !  Description and declaration in procs.igs
    character(*)                    :: mdfrec !!  Standard rec. length in MD-file (300)
    character(20), dimension(lstsc) :: namcon !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer       :: l      ! Help var. 
    integer       :: lenc   ! Help var. (length of var. cvar to be looked for in the MD-file) 
    integer       :: lnconc ! Help var. for constituent 
    integer       :: nlook  ! Help var.: nr. of data to look for in the MD-file 
    integer       :: ntrec  ! Help. var to keep track of NRREC 
    logical       :: lerror ! Flag=TRUE if a local error is encountered 
    logical       :: newkw  ! Logical var. specifying whether a new recnam should be read from the MD-file or just new data in the continuation line 
    character(20) :: cdef   ! Default value when CHULP not found 
    character(20) :: chulp  ! Help var. 
    character(6)  :: keyw   ! Name of record to look for in the MD-file (usually KEYWRD or RECNAM) 
!
!! executable statements -------------------------------------------------------
!
    ! initialize local parameters
    !
    nlook  = 1
    lerror = .false.
    newkw  = .true.
    cdef   = ' '
    !
    ! define or read names of constituents
    !
    lnconc = 0
    !
    ! define name of salinity  (Sub1(1:1) = 'S')
    !
    if (salin) then
       lnconc = lnconc + 1
       namcon(lnconc) = 'salinity'
    endif
    !
    ! define name of temperature  (Sub1(2:2) = 'T')
    !
    if (temp) then
       lnconc = lnconc + 1
       namcon(lnconc) = 'temperature'
    endif
    !
    ! read names of constituents, if LCONC > 0
    !
    keyw  = 'Namc  '
    ntrec = nrrec
    lenc  = 20
    do l = 1, lconc
       lnconc = lnconc + 1
       if (l<10) then
       write (keyw(5:5), '(i1)') l
       else
          write (keyw(5:6), '(i2)') l
       endif
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,chulp     ,cdef      ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
          namcon(lnconc) = cdef
       else
          namcon(lnconc) = chulp
       endif
       !
       ! test for namcon = ' ', which is per definition not possible
       !        because LCONC is defined by namcon values <> ' '
       !
       if (namcon(lnconc)==cdef) then
          error = .true.
          call prterr(lundia    ,'V015'    ,' '       )
       endif
       !
       ! Define constituent name in small characters
       !
       call small(namcon(lnconc)       ,len(namcon(lnconc))  )
    enddo
end subroutine rdnamc
