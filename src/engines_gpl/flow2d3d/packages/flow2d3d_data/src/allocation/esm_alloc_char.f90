subroutine esm_alloc_char(lundia, error, gdp)
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
!    Function: Determines memory requirements for the
!              CHARACTER ARRAY. In this subroutine the start
!              indices of all character arrays are calculated by
!              using the memory management function MKCPNT.
!              The start adress of an array can be found by using
!              the function GTCPNT.
!              Function MKCPNT will when errors occure call an
!              errorroutine (ERRPNT). The function MKCPNT will
!              return with value 1 or for memory already
!              declared with correct length with value -1
!              Because the Delft3D-FLOW module can use static
!              array declaration the error messages will stay
!              at the end of the routine
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
    integer , pointer :: lmax
    integer , pointer :: lsts
    integer , pointer :: lstsc
    integer , pointer :: nto
    integer , pointer :: kc
    integer , pointer :: nsrc
    integer , pointer :: nofou
    integer , pointer :: ndro
    integer , pointer :: nsluv
    integer , pointer :: ncpntr
!
! Global variables
!
    integer              :: lundia ! Description and declaration in inout.igs
    logical, intent(out) :: error  ! TRUE if an error is encountered
!
! Local variables
!
    integer           :: ierr   ! errorflag 
    integer, external :: mkcpnt
    character(6)      :: pntnam ! Pointername 
!
!! executable statements -------------------------------------------------------
!
    ncpntr    => gdp%gdpointrs%ncpntr
    lmax      => gdp%d%lmax
    lsts      => gdp%d%lsts
    lstsc     => gdp%d%lstsc
    nto       => gdp%d%nto
    kc        => gdp%d%kc
    nsrc      => gdp%d%nsrc
    nofou     => gdp%d%nofou
    ndro      => gdp%d%ndro
    nsluv     => gdp%d%nsluv
    !
    ! Initialize local variable
    !
    ! arrays for: discharge sources
    !
    pntnam = 'disint'        !  Global data
    ierr = mkcpnt(pntnam, nsrc, gdp)
                             !  Interpolation flag for discharges
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'dismmt'        !  Global data
    ierr = mkcpnt(pntnam, nsrc, gdp)
                             !  Option to take momentum into account
                             !  for the disch. Time dependent values
                             !  for velocity magnitude and direction
                             !  should be added in case DISMMT=Y
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'NAMSRC'        !  Global data
    ierr = mkcpnt(pntnam, 20*nsrc, gdp)
                             !  Names of discharge points
    if (ierr<= - 9) goto 9999
    !
    ! arrays for: time varying and fourier openings
    !               constituents Sal., Temp. & Cons.
    !
    pntnam = 'nambnd'        !  Global data
    ierr = mkcpnt(pntnam, nto*20, gdp)
                             !  Array containing the coordinates of the open boundary section
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'typbnd'        !  Global data
    ierr = mkcpnt(pntnam, nto, gdp)
                             !  Type of open boundary prescribed:
                             !     - Z : water level
                             !     - C : Current
                             !     - Q : Discharge
                             !     - R : Riemann
                             !     - T : Total discharge
                             !     - N : Neumann
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'tprofu'        !  Global data
    ierr = mkcpnt(pntnam, nto*20, gdp)
                             !  Type of vertical velocity profile
                             !     - uniform
                             !     - logarithmic
                             !     - 3d-profile
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'tprofc'        !  Global data
    ierr = mkcpnt(pntnam, nto*lstsc*10, gdp)
                             !  Shape function for constituent
                             !     - uniform
                             !     - linear
                             !     - step
                             !     - 3d-profile
                             !  old and new time in index 1 and 2
    if (ierr<= - 9) goto 9999
    !
    ! arrays for: constituent names (all)
    !
    pntnam = 'namcon'        !  Global data
    ierr = mkcpnt(pntnam, lmax*20, gdp)
                             !  Names of the constituents
                             !  (excl. salinity and temperature)
    if (ierr<= - 9) goto 9999
    !
    ! arrays for: bottom stress term
    !
    pntnam = 'ROUFLO'        !  Global data
    ierr = mkcpnt(pntnam, 4, gdp)
                             !  Selection flag for the computation of
                             !  bottom stress term due to flow:
                             !  [MANN, CHEZ, WHIT, 3D]
    if (ierr<= - 9) goto 9999
    !
    ! arrays for: drogue track definition
    !
    pntnam = 'namdro'        !  Global data
    ierr = mkcpnt(pntnam, 20*ndro, gdp)
                             !  Names of the drogues
    if (ierr<= - 9) goto 9999
    !
    ! arrays for: barriers
    !
    pntnam = 'nambar'        !  Global data
    ierr = mkcpnt(pntnam, 20*nsluv, gdp)
                             !  Barrier names
    if (ierr<= - 9) goto 9999
    !
    ! Test if pointer declaration outside declaration in POINTRS.INC
    !
    if (ierr== - 3) then
       error = .true.
       call prterr(lundia    ,'G005'    ,' '       )
       !
       write (lundia, *) '         Parameter MXCPNT to small, add ',            &
                       & ncpntr - mxcpnt
    endif
    !
    ! Test exit code which are not allowed (in theory not possible)
    !
 9999 continue
    if (ierr<= - 9) then
       error = .true.
       call prterr(lundia    ,'G920'    ,'esm_alloc_char'   )
    !
    endif
end subroutine esm_alloc_char
