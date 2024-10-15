subroutine dfdecomp ( lunmd, lundia, error, runid, gdp )
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
!    Function: Carries out domain decomposition meant for parallel Delft3D-FLOW
!
! Method used: For parallel runs based on distributed-memory approach,
!              the single domain needs to be partitioned such that no
!              load imbalances occur. For this, we need the grid enclosure
!              so that only active points will be equally distributed over
!              the processors. Next, block administration is carried out
!              to be used for communication
!
!!--pseudo code and references--------------------------------------------------
!
!   store the original values of MMAX and NMAXUS of global computational grid
!   store first and last indices of global computational grid
!   if not parallel, return
!   allocate and initialize some work arrays (e.g. ICOM)
!   reads grid enclosure and writes to unformatted semi-scratch file
!   fill array ICOM based on grid enclosure
!   carry out the partitioning of computational grid using array ICOM
!   carry out the block administration
!   recompute grid related dimensions for each subdomain
!   store array ICOM for each subdomain
!
!
!!--declarations----------------------------------------------------------------
    use dfparall
    use m_dfparall, only: dfpartit
    use globaldata
    !
    implicit none
    !
    type(globdat), target    :: gdp
!
! Global variables
!
    integer, intent(in)       :: lundia ! Description and declaration in inout.igs
    integer, intent(in)       :: lunmd  ! Description and declaration in inout.igs
    logical, intent(out)      :: error  ! Flag=TRUE if an error is encountered
    character(*), intent(in)  :: runid  ! Run identification code for the current simulation
!
! Local variables
!
    type(gd_dimens), pointer             :: gddimens
    integer, pointer                     :: mmax
    integer, pointer                     :: nmax
    integer, pointer                     :: nmaxus
    integer, pointer                     :: nlb
    integer, pointer                     :: nub
    integer, pointer                     :: mlb
    integer, pointer                     :: mub
    type(dfparalltype), pointer          :: gdparall
    integer, pointer                     :: mmaxgl
    integer, pointer                     :: nmaxgl
    integer, pointer                     :: nfg
    integer, pointer                     :: nlg
    integer, pointer                     :: mfg
    integer, pointer                     :: mlg
    integer                              :: nmtot ! total number of array elements NMAX * (MMAX + 4)
    integer                              :: istat 
    integer, dimension(:,:), allocatable :: icom  ! mask array for the water level points in global domain
                                                  !  = 0 : point is not active
                                                  ! <> 0 : point is active
    integer, dimension(:,:), allocatable :: ipown ! array giving the subdomain number of each gridpoint
    integer, dimension(:), allocatable   :: ipx   ! X-coordinate of the comp. grid enclosure
    integer, dimension(:), allocatable   :: ipy   ! Y-coordinate of the comp. grid enclosure
    integer                              :: i
    integer                              :: j
    integer                              :: ap
    integer                              :: np
    integer                              :: cp
!
!! executable statements -------------------------------------------------------
!
    gddimens => gdp%d
    mmax     => gddimens%mmax
    nmax     => gddimens%nmax
    nmaxus   => gddimens%nmaxus
    nlb      => gddimens%nlb
    nub      => gddimens%nub
    mlb      => gddimens%mlb
    mub      => gddimens%mub
    gdparall => gdp%gdparall
    mmaxgl   => gdparall%mmaxgl
    nmaxgl   => gdparall%nmaxgl
    nfg      => gdparall%nfg
    nlg      => gdparall%nlg
    mfg      => gdparall%mfg
    mlg      => gdparall%mlg
    !
    ! store the original values of MMAX and NMAXUS of global computational grid
    !
    mmaxgl = mmax
    nmaxgl = nmaxus
    !
    ! store first and last indices of global computational grid
    !
    nfg = 1
    nlg = nmaxus
    mfg = 1
    mlg = mmax
    !
    ! if not parallel, return
    !
    if (.not.parll) then
       !
       ! mf, ml, nf, nl and iarrc are passed through as parameters in some output files,
       ! also without parall.
       ! To avoid crashes (only release version, only DD simulations, only sometimes....):
       allocate(gdparall%nf(0:0), &
           & gdparall%nl(0:0), &
           & gdparall%mf(0:0), &
           & gdparall%ml(0:0), &
           & gdparall%iarrc(4,0:0), stat=istat)
       return
    endif
    !
    ! allocate and initialize some work arrays (e.g. ICOM)
    !
    nmtot = nmax*(mmax + 4)
    allocate (icom(-1:mmax+2, nmax))
    allocate (ipx(nmtot))
    allocate (ipy(nmtot))
    allocate(ipown(mmax,nmaxus))
    !
    icom  = 0
    ipx   = 0
    ipy   = 0
    ipown = 0
    !
    if ( inode == master ) then
       !
       ! reads grid enclosure from either the MD-file or the attribute file
       ! and writes to unformatted semi-scratch file
       !
       call rdencl( lunmd     ,lundia    ,error     , runid     , &
                  &  mmax      ,nmaxus    ,gdp       )
       if (error) goto 9999
       !
       ! fill array ICOM based on grid enclosure
       !
       nlb =  1
       nub =  nmax
       mlb = -1
       mub =  mmax+2
       call inigrd( lundia    ,error     ,runid     ,nmax      ,mmax      , &
                 &  nmaxus    ,icom      ,ipx       ,ipy       ,gdp       )
       if (error) goto 9999

    endif
    !
    ! scatter array ICOM to all nodes
    !
    call dfbroadc_gdp ( icom, nmtot, dfint, gdp )
    !
    ! carry out the partitioning of the computational grid using array ICOM
    !
    call dfpartit( ipown, icom, mmax, nmaxus, gdp )
    !
    ! commented dfbalance for the time being    
    !    !
    !    ! divide domains based on weighting compute, communicate and dry points.
    !    !
    !    call dfbalance ( ipown, icom, mmax, nmaxus, mfg, mlg, nfg, nlg )
    !
    ! carry out the block administration
    !
    call dfbladm( ipown, icom, mmax, nmaxus, runid, gdp )
    !
    ! recompute grid related dimensions for each subdomain
    !
    mmax   = mlg - mfg + 1
    nmaxus = nlg - nfg + 1
    if (mod(nmaxus,2)==0) then
       nmax = nmaxus + 1
    else
       nmax = nmaxus
    endif
    !
    ! count active, non active and commnunication points
    !
    ap=0
    np=0
    cp=0
    do j=nfg,nlg
       do i=mfg,mlg
          if (ipown(i,j) == inode) then
            ap = ap+1
          else if (ipown(i,j) .ne. 0) then
            cp = cp+1 
          else
            np = np+1
          endif
       enddo
    enddo
    write(lundia,'(12x,a,i0)')'mmax                 : ', mmax   
    write(lundia,'(12x,a,i0)')'mfg                  : ', mfg   
    write(lundia,'(12x,a,i0)')'mlg                  : ', mlg   
    write(lundia,'(12x,a,i0)')'nmax                 : ', nmax   
    write(lundia,'(12x,a,i0)')'nfg                  : ', nfg   
    write(lundia,'(12x,a,i0)')'nlg                  : ', nlg   
    write(lundia,'(12x,a,i0)')'active        points : ', ap
    write(lundia,'(12x,a,i0)')'non-active    points : ', np
    write(lundia,'(12x,a,i0)')'communication points : ', cp
    write(lundia,'(12x,a,i0)')'total         points : ', mmax*nmaxus
    !
    ! store array ICOM for each subdomain (to be later used in routine inigrd)
    !
    call strgrd ( icom, runid, mmax, nmax, mmaxgl, nmaxgl, &
                & nfg , nlg  , mfg , mlg , gdp   )
 9999 continue
    deallocate(icom,ipx,ipy,ipown)
end subroutine dfdecomp
