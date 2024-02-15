subroutine tripoi(runid, filmrs, versio, filmd,gdp       )
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
!    Function: - initialize system paramaters for a FLOW
!                simulation run
!              - read or compute dimension of the FLOW arrays
!                from md-file or extra input files
!              - compute en set pointers of integer, real and
!                character arrays
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
    include 'fsm.i'
    include 'tri-dyn.igd'
    integer , pointer :: nmax
    integer , pointer :: mmax
    integer , pointer :: nmaxus
    integer , pointer :: kmax
    integer , pointer :: nlcest
    integer , pointer :: nto
    integer , pointer :: nopest
    integer , pointer :: nsrc
    integer , pointer :: lunmd
    integer , pointer :: lundia
!
! Global variables
!
    character(*)               :: filmd  !!  File name for MD FLOW file
    character(*)               :: runid  !!  Run identification code for the cur-
                                         !!  rent simulation (used to determine
                                         !!  the names of the in- /output files
                                         !!  used by the system)
    character(12)              :: filmrs !!  File name for DELFT3D_MOR FLOW
                                         !!  input file (MD-flow.xxx)
    character(5)               :: versio !!  Version nr. of the current package
!
! Local variables
!
    integer(pntrsize), external :: gtipnt
    logical           :: error  ! Flag=TRUE if an error is encountered 
    character(6)      :: prgnm  ! Help var. determining the prog. name currently active 
!
!! executable statements -------------------------------------------------------
!
    nmax      => gdp%d%nmax
    mmax      => gdp%d%mmax
    nmaxus    => gdp%d%nmaxus
    kmax      => gdp%d%kmax
    nlcest    => gdp%d%nlcest
    nto       => gdp%d%nto
    nopest    => gdp%d%nopest
    nsrc      => gdp%d%nsrc
    lunmd     => gdp%gdinout%lunmd
    lundia    => gdp%gdinout%lundia
    !
    error  = .false.
    prgnm  = 'trisim'
    !
    ! start FLOW simulation program
    !
    call sysini(error     ,runid     ,filmrs    ,prgnm     , &
              & versio    ,filmd     ,gdp       )
    if (error) goto 9999
    !
    ! read  dimensions of arrays and declare array pointers
    !
    call decarr(lunmd     ,lundia    ,error     ,runid     , &
              & prgnm     ,gdp       )
    if (error) goto 9999
    !
    ! test local dimensions in various subroutines
    !
    call chklod(lundia    ,error     ,nto       ,nsrc      , &
              & gdp       )
    if (error) goto 9999
    !
    ! set some single variables in the memory (for morfologic comp.)
    !
    i(gtipnt('NMAX', gdp))   = nmax
    i(gtipnt('NMAXUS', gdp)) = nmaxus
    i(gtipnt('MMAX', gdp))   = mmax
    i(gtipnt('NOROCO', gdp)) = nlcest
    i(gtipnt('NTO', gdp))    = nto
    i(gtipnt('NROB', gdp))   = nopest
    i(gtipnt('KMAX', gdp))   = kmax
    i(gtipnt('NSRC', gdp))   = nsrc
 9999 continue
    if (error) gdp%errorcode = 1
end subroutine tripoi
