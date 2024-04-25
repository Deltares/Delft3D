subroutine usrptr(lundia    ,error      ,pntnam    ,partyp    ,length    , &
                & ipoint    ,gdp       )
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
!    Function: Get pointer of user defined array defined by
!              PNTNAM and LENGTH for type TYPE
!              write start address in IPOINT
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
    integer(pntrsize), intent(out) :: ipoint
                                   !!  Pointer of requested array PNTNAM
    integer              :: length
                                   !!  Length of the requested array
    integer              :: lundia !  Description and declaration in inout.igs
    logical, intent(out) :: error
                                   !!  Flag=TRUE if an error is encountered
    character(*)         :: pntnam
                                   !!  Pointername
    character(9)         :: partyp
                                   !!  Type of pointer (real, integer etc.)
!
! Local variables
!
    integer           :: ierr   ! Error return value from dynamic array declaration routines 
    integer(pntrsize), external :: gtcpnt
    integer(pntrsize), external :: gtipnt
    integer(pntrsize), external :: gtrpnt
    integer, external :: mkcpnt
    integer, external :: mkipnt
    integer, external :: mkfpnt
!
!! executable statements -------------------------------------------------------
!
    ! Make pointer depending on TYPE
    !
    if (partyp(:4)=='real') ierr = mkfpnt(pntnam, length, gdp)
    if (partyp(:6)=='integer') ierr = mkipnt(pntnam, length, gdp)
    if (partyp(:9)=='character') ierr = mkcpnt(pntnam, length, gdp)
    !
    ! test if array outside declaration of ipsize (old situation)
    !
    if (ierr== - 2) then
       error = .true.
       call prterr(lundia    ,'G005'    ,' '       )
       !
       goto 9999
    endif
    !
    ! test if pointer declaration outside declaration in pointrs.inc
    !
    if (ierr== - 3) then
       error = .true.
       call prterr(lundia    ,'G005'    ,' '       )
       !
       goto 9999
    endif
    !
    ! Test exit code which are not allowed
    !
    if (ierr<= - 9) then
       error = .true.
       if (ierr== - 10000000) then
          call prterr(lundia    ,'G020'    ,partyp    )
       !
       elseif (ierr== - 20000000) then
          call prterr(lundia    ,'G021'    ,partyp    )
       !
       elseif (ierr== - 30000000) then
          call prterr(lundia    ,'G022'    ,partyp    )
       !
       elseif (ierr== - 41000000) then
          call prterr(lundia    ,'G021'    ,pntnam    )
       !
       elseif (ierr== - 42000000) then
          call prterr(lundia    ,'G023'    ,pntnam    )
       !
       elseif (ierr== - 43000000) then
          call prterr(lundia    ,'G023'    ,pntnam    )
       !
       else
          call prterr(lundia    ,'G005'    ,' '       )
          !
          write (lundia, *) '         Array name already declared !!'
       endif
    !
    ! test exit code ok => get pointer
    !
    else
       if (partyp(:4)=='real') ipoint = gtrpnt(pntnam, gdp)
       if (partyp(:6)=='integer') ipoint = gtipnt(pntnam, gdp)
       if (partyp(:9)=='character') ipoint = gtcpnt(pntnam, gdp)
    endif
    !
 9999 continue
end subroutine usrptr
