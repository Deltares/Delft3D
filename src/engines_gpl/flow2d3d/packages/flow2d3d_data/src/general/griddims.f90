subroutine griddims( gdp )
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
!    Function: Initialises grid related dimensions
!              This routine is introduced with the implementation
!              of DD boundaries
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use dfparall
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer          , pointer :: nmax
    integer          , pointer :: mmax
    integer          , pointer :: nlb
    integer          , pointer :: nub
    integer          , pointer :: mlb
    integer          , pointer :: mub
    integer          , pointer :: nmlb
    integer          , pointer :: nmub
    integer          , pointer :: ddb
    integer          , pointer :: jstart
    integer          , pointer :: nmmaxj
    integer          , pointer :: nmmax
    integer          , pointer :: numdomains
    integer          , pointer :: nummappers
    type(griddimtype), pointer :: griddim
!
! Global variables
!
!   NONE
!
! Local variables
!
    integer                :: istat   !< error flag during memory (re)allocation
    integer                :: m       !< local m index
    integer                :: mglob   !< global m index
    integer                :: n       !< local n index
    integer                :: nglob   !< global n index
    integer                :: nmaxddb !< matrix size in N direction including any domain/partition boundaries
    integer                :: nm      !< local nm index
    integer                :: nnodes  !< number of nodes in cell polygon
    integer                :: node1   !< first node of cell polygon
!
!! executable statements -------------------------------------------------------
!
    nmax         => gdp%d%nmax
    mmax         => gdp%d%mmax
    nlb          => gdp%d%nlb
    nub          => gdp%d%nub
    mlb          => gdp%d%mlb
    mub          => gdp%d%mub
    nmlb         => gdp%d%nmlb
    nmub         => gdp%d%nmub
    ddb          => gdp%d%ddbound
    jstart       => gdp%d%jstart
    nmmaxj       => gdp%d%nmmaxj
    nmmax        => gdp%d%nmmax
    numdomains   => gdp%gdprognm%numdomains
    nummappers   => gdp%gdprognm%nummappers
    griddim      => gdp%griddim
    !
    ! basic dimensions:
    ! nmmax  = mmax   *   nmax
    ! nmmaxj = nmmax  + 2*nmax
    ! jstart = 1      - 2*nmax
    !
    !
    ! Probably, ddb can be 0 when numdomains=1, nummappers>=1
    ! For safety, ddb is always 1 when nummappers>=1
    !
    ! ddb is expected to be able to be 0 when parll
    ! Unfortunately, it must be 1
    !
    if (nummappers>=1 .or. parll) then
       ddb = 1
    else
       ddb = 0
    endif
    nlb  = 1 - ddb
    nub  = nmax + ddb
    mlb  = -1 - ddb
    mub  = mmax + 2 + ddb
    nmaxddb = nmax + 2*ddb
    nmmax      = (mmax + 2*ddb)*nmaxddb
    jstart     = 1 - 2*nmaxddb
    nmmaxj     = nmmax + 2*nmaxddb
    nmlb = jstart
    nmub = nmmaxj
    !
    griddim%mlb    = mlb
    griddim%mub    = mub
    griddim%mmax   = mmax
    !
    griddim%nlb    = nlb
    griddim%nub    = nub
    griddim%nmax   = nmax
    !
    griddim%nmlb   = nmlb
    griddim%nmub   = nmub
    griddim%nmmax  = nmmax
    !
    griddim%mfg    = gdp%gdparall%mfg
    griddim%mlg    = gdp%gdparall%mlg
    griddim%mmaxgl = gdp%gdparall%mmaxgl
    !
    griddim%nfg    = gdp%gdparall%nfg
    griddim%nlg    = gdp%gdparall%nlg
    griddim%nmaxgl = gdp%gdparall%nmaxgl
    !
    griddim%aggrtable => null()
    !
    allocate(griddim%nmglobal(nmlb:nmub), stat=istat)
    nm = nmlb - 1
    do m = mlb, mub
       do n = nlb, nub
          nm = nm + 1
          nglob   = griddim%nfg + n - 1
          mglob   = griddim%mfg + m - 1
          griddim%nmglobal(nm) = nglob + (griddim%nmaxgl + 2) * mglob + 1
       enddo
    enddo
    !
    allocate(griddim%celltype(nmlb:nmub), stat=istat)
    griddim%celltype(:) = 1
    !
    allocate(griddim%cell2node((mub-mlb+1)*(nub-nlb+1)*4), stat=istat)
    allocate(griddim%ncellnodes(nmlb:nmub), stat=istat)
    allocate(griddim%indexnode1(nmlb:nmub), stat=istat)
    nm = nmlb - 1
    node1 = 1
    do m = mlb, mub
       do n = nlb, nub
          nm = nm + 1
          if (n > 1 .and. m > 1) then
             griddim%cell2node(node1  ) = nm
             griddim%cell2node(node1+1) = nm - 1
             griddim%cell2node(node1+2) = nm - 1 - nmaxddb
             griddim%cell2node(node1+3) = nm - nmaxddb
             nnodes = 4
          else
             nnodes = 0
          endif
          griddim%ncellnodes(nm) = nnodes
          griddim%indexnode1(nm) = node1
          node1 = node1 + nnodes
       enddo
    enddo
    !
    allocate(griddim%nmbnd(0,2), stat=istat)
end subroutine griddims
