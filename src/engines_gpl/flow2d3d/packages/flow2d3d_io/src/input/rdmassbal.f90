subroutine rdmassbal(xz        ,yz        ,kcs       ,gsqs      , &
                   & mmax      ,nmax      ,nmaxus    ,nmmax     , &
                   & lsedtot   ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2016.                                
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
!  $Id: rdmassbal.f90 5717 2016-01-12 11:35:24Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/io/src/input/rdmassbal.f90 $
!!--description-----------------------------------------------------------------
!
! Reads mass balance input data.
! Determine whether mass balance output is requested.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    use polygon_module
    use m_alloc
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    character(80), dimension(:)    , pointer :: volnames
    integer      , dimension(:)    , pointer :: volnr
    integer      , dimension(:,:)  , pointer :: exchnr
    logical                        , pointer :: massbal
    real(fp)     , dimension(:)    , pointer :: accdps
    real(fp)     , dimension(:)    , pointer :: horareas
    real(fp)     , dimension(:)    , pointer :: volumes
    real(fp)     , dimension(:,:)  , pointer :: mass_r1
    real(fp)     , dimension(:,:)  , pointer :: fluxes
    real(fp)     , dimension(:,:,:), pointer :: fluxes_r1
    real(fp)     , dimension(:,:,:), pointer :: fluxes_sd
    integer                        , pointer :: lundia
    integer                        , pointer :: lstsci
    integer                        , pointer :: nbalpol
    integer                        , pointer :: nneighb
!
! Global variables
!
    integer                                     , intent(in)  :: nmax    !  Description and declaration in esm_alloc_int.f90
    integer                                     , intent(in)  :: nmaxus  !  Description and declaration in esm_alloc_int.f90
    integer                                     , intent(in)  :: nmmax   !  Description and declaration in esm_alloc_int.f90
    integer                                     , intent(in)  :: mmax    !  Description and declaration in esm_alloc_int.f90
    integer                                     , intent(in)  :: lsedtot !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)   , intent(in)  :: kcs     !  Description and declaration in ijdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)  , intent(in)  :: xz      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)  , intent(in)  :: yz      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)  , intent(in)  :: gsqs    !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    type(tree_data), pointer                :: pol_ptr
    type(tree_data), pointer                :: link_ptr
    integer                                 :: i
    integer                                 :: idx
    integer                                 :: istat
    integer                                 :: ivol
    integer                                 :: j
    integer                                 :: jvol
    integer                                 :: maxnpnt
    integer                                 :: n
    integer                                 :: nm
    integer                                 :: nm2
    integer                                 :: nmaxddb
    integer                                 :: ni
    integer                                 :: firstpnt
    integer                                 :: nbalpnt
    integer                                 :: vol1
    integer                                 :: vol2
    character(256)                          :: filbal
    character(80)                           :: name
    character(20)                           :: keyword
    logical                                 :: found
    real(fp), dimension(:)  , allocatable   :: xdr
    real(fp), dimension(:)  , allocatable   :: ydr
    integer, dimension(:,:) , pointer       :: neighb
!
!! executable statements -------------------------------------------------------
!
    massbal           => gdp%gdmassbal%massbal
    nbalpol           => gdp%gdmassbal%nbalpol
    nneighb           => gdp%gdmassbal%nneighb
    lundia            => gdp%gdinout%lundia
    lstsci            => gdp%d%lstsci
    nmaxddb = gdp%d%nmax + 2*gdp%d%ddbound
    !
    ! Get value of Filbal. If no file name specified, then no balance output requested.
    !
    filbal = ' '
    call prop_get_string(gdp%mdfile_ptr, '*', 'Filbal', filbal)
    if (filbal == ' ') return
    massbal = .true.
    !
    ! Balance data requested: open and read balance polygon file.
    !
    call tree_create('Mass balance polygons',pol_ptr)
    call prop_file('tekal', filbal, pol_ptr, istat)
    if (istat /= 0) then
       select case (istat)
       case(1)
          call prterr(lundia, 'G004', filbal)
       case(3)
          call prterr(lundia, 'G006', filbal)
       case default
          call prterr(lundia, 'G007', filbal)
       endselect
       call d3stop(1, gdp)
    endif
    !
    ! Process all polygons.
    !
    nbalpol = 0
    maxnpnt = 0
    do i = 1, size(pol_ptr%child_nodes)
       link_ptr => pol_ptr%child_nodes(i)%node_ptr
       !
       ! Count number of balance areas and maximum number of points.
       ! Total nbalpol = size(pol_ptr%child_nodes)
       !
       nbalpnt = 0
       name = tree_get_name(link_ptr)
       call register_polygon(name  , pol_ptr, nbalpol, nbalpnt, &
                           & 'balance', .true., gdp)
       maxnpnt = max(maxnpnt,nbalpnt)
    enddo
    nbalpol = nbalpol+1 ! add one for default volume
    !
                  allocate(xdr(maxnpnt),ydr(maxnpnt), stat=istat)
    if (istat==0) allocate(gdp%gdmassbal%horareas(nbalpol), stat=istat)
    if (istat==0) allocate(gdp%gdmassbal%volnames(nbalpol+1), stat=istat)
    if (istat==0) allocate(gdp%gdmassbal%volnr(gdp%d%nmlb:gdp%d%nmub), stat=istat)
    if (istat==0) allocate(gdp%gdmassbal%exchnr(2,gdp%d%nmlb:gdp%d%nmub), stat=istat)
    !
    if (istat /= 0) then
       call prterr(lundia, 'U021', 'RdMassBal: memory alloc error')
       call d3stop(1, gdp)
    endif
    horareas  => gdp%gdmassbal%horareas
    volnames  => gdp%gdmassbal%volnames
    volnr     => gdp%gdmassbal%volnr
    exchnr    => gdp%gdmassbal%exchnr
    horareas  = 0.0_fp
    volnames  = ' '
    volnr     = 0
    exchnr    = 0
    !
    do i = 1, size(pol_ptr%child_nodes)
       link_ptr => pol_ptr%child_nodes(i)%node_ptr
       volnames(i) = tree_get_name(link_ptr)
       !
       ! Read polygon
       !
       idx = 1
       call read_polygon_data(link_ptr, idx, firstpnt, nbalpnt, &
                            & xdr, ydr, 'balance', i, gdp)
       !
       ! Determine internal points of polygons and assign them to this volume
       ! Note that if a cell is inside several polygons, it will be assigned to
       ! the volume corresponding to the first polygon.
       !
       do nm = 1, nmmax
          if (volnr(nm)==0 .and. kcs(nm) == 1) then
             call ipon(xdr, ydr, nbalpnt, xz(nm), yz(nm), istat, gdp)
             if (istat >= 0) then
                volnr(nm) = i
             endif
          endif
       enddo
    enddo
    !
    ! Assign all remaining internal grid cells to default volume
    ! Compute horizontal surface areas of the volumes
    !
    volnames(nbalpol) = 'Other Grid Cells'
    volnames(nbalpol+1) = 'Open Boundaries'
    do nm = 1, nmmax
       if (kcs(nm) == 1) then
          if (volnr(nm) == 0) then
             volnr(nm) = nbalpol
          endif
          horareas(volnr(nm)) = horareas(volnr(nm)) + gsqs(nm)
       elseif (kcs(nm) == 2) then
          volnr(nm) = nbalpol+1
       endif
    enddo
    !
    ! TODO: determine volume number of halo cells from other partitions and domains
    !       - domains/partitions should only accumulate their own volumes
    !       - avoid double counting exchanges:
    !            in case of DD refinement: best information on high resolution domain
    !            in case of equal resolution: let domain with lowest rank count
    !
    ! Determine connections between polygons (for this partition/domain)
    !
    nneighb = 0
    allocate(neighb(2,128), stat=istat)
    if (istat /= 0) then
       call prterr(lundia, 'U021', 'RdMassBal: memory alloc error')
       call d3stop(1, gdp)
    endif
    do nm = 1,nmmax
       ivol = volnr(nm)
       if (ivol==0) cycle
       !
       do j = 1,2
          if (j==1) then
             nm2 = nm+nmaxddb
          else
             nm2 = nm+1
          endif
          !
          jvol = volnr(nm2)
          if (jvol==0) cycle
          if (ivol==jvol) cycle
          !
          vol1 = min(ivol,jvol)
          vol2 = max(ivol,jvol)
          !
          n = 1
          found = .false.
          do while (n<=nneighb)
             if (vol1==neighb(1,n)) then
                if (vol2==neighb(2,n)) then
                   found = .true.
                   exit
                elseif (vol2<neighb(2,n)) then
                   exit
                endif
             elseif (vol1<neighb(1,n)) then
                exit
             endif
             n = n+1
          enddo
          if (.not.found) then
             ! insert at n
             if (nneighb==size(neighb,2)) then
                call reallocP(neighb,(/2,2*nneighb/),stat = istat)
                if (istat /= 0) then
                   call prterr(lundia, 'U021', 'RdMassBal: memory alloc error')
                   call d3stop(1, gdp)
                endif
             endif
             do ni = nneighb,n,-1
                neighb(2,ni+1) = neighb(2,ni)
                neighb(1,ni+1) = neighb(1,ni)                      
             enddo
             neighb(1,n) = vol1
             neighb(2,n) = vol2
             nneighb = nneighb+1
          endif
       enddo
    enddo
    !
    ! TODO: synchronize neighbour information across partitions and domains
    !
    ! Store NEIGHB array in GDP for output purposes.
    !
    call reallocP(neighb, (/2,nneighb/) )
    gdp%gdmassbal%neighb => neighb
    !
    ! Now convert NEIGHB array to EXCHNR array
    !
    do nm = 1,nmmax
       ivol = volnr(nm)
       if (ivol==0) cycle
       !
       do j = 1,2
          if (j==1) then
             nm2 = nm+nmaxddb
          else
             nm2 = nm+1
          endif
          !
          jvol = volnr(nm2)
          if (jvol==0) cycle
          if (ivol==jvol) cycle
          !
          vol1 = min(ivol,jvol)
          vol2 = max(ivol,jvol)
          !
          n = 1
          found = .false.
          do while (n<=nneighb)
             if (vol1==neighb(1,n)) then
                if (vol2==neighb(2,n)) then
                   found = .true.
                   exchnr(j,nm) = n
                   exit
                elseif (vol2<neighb(2,n)) then
                   exit
                endif
             elseif (vol1<neighb(1,n)) then
                exit
             endif
             n = n+1
          enddo
          if (.not.found) then
             call prterr(lundia, 'U021', 'RdMassBal: unexpected connection encountered.')
             call d3stop(1, gdp)
          endif
       enddo
    enddo
    !
    ! Allocate memory for volumes and fluxes, and initialize
    !
                  allocate(gdp%gdmassbal%volumes(nbalpol), stat=istat)
    if (istat==0) allocate(gdp%gdmassbal%accdps(nbalpol), stat=istat)
    if (istat==0) allocate(gdp%gdmassbal%mass_r1(nbalpol,lstsci), stat=istat)
    if (istat==0) allocate(gdp%gdmassbal%fluxes(2,nneighb), stat=istat)
    if (istat==0) allocate(gdp%gdmassbal%fluxes_r1(2,nneighb,lstsci), stat=istat)
    if (istat==0) allocate(gdp%gdmassbal%fluxes_sd(2,nneighb,lsedtot), stat=istat)
    !
    if (istat /= 0) then
       call prterr(lundia, 'U021', 'RdMassBal: memory alloc error')
       call d3stop(1, gdp)
    endif
    !
    volumes   => gdp%gdmassbal%volumes
    accdps    => gdp%gdmassbal%accdps
    mass_r1   => gdp%gdmassbal%mass_r1
    fluxes    => gdp%gdmassbal%fluxes
    fluxes_r1 => gdp%gdmassbal%fluxes_r1
    fluxes_sd => gdp%gdmassbal%fluxes_sd
    volumes   = 0.0_fp
    accdps    = 0.0_fp
    mass_r1   = 0.0_fp
    fluxes    = 0.0_fp
    fluxes_r1 = 0.0_fp
    fluxes_sd = 0.0_fp
    call tree_destroy(pol_ptr)
end subroutine rdmassbal
