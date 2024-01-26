!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! 
! 

!> Cross sections (crs) are used to monitor summed flow data across a line
!! over time. The definition of crs is by a crspath, which is a polyline
!! with all flow links (1D and 2D, including orientation) that cross it.
!! Given a norhtward crs, the positive  transport direction is eastward.
module m_monitoring_crosssections
use m_crspath
use m_missing
use MessageHandling, only: IdLen
implicit none

type tcrs
    character(len=IdLen)          :: name          !< Name
    integer                       :: nval          !< Amount of different quantities monitored
    type(tcrspath)                :: path          !< Polyline+crossed flow links that defines this cross section.
    integer                       :: loc2OC = 0    !< mapping from global obs index to obs that are defined by branchID and chainage
    double precision, allocatable :: sumvalcur(:)  !< Values integrated over the crs
    double precision, allocatable :: sumvalcum(:)  !< Values integrated over crs *and* time
    double precision, allocatable :: sumvalavg(:)  !< Values integrated over crs and averaged in time.
                                                   !! Size is nval: nr of monitored quantities.
end type tcrs

! Indices in sumvalcur and other arrays: postfix 'C' means cumulative/sum, 'A' means averaged.
integer                              :: IPNT_Q1C = 1           ! pointers in sumval* arrays
integer                              :: IPNT_AUC = 2           ! pointers in sumval* arrays
integer                              :: IPNT_U1A = 3           ! pointers in sumval* arrays
integer                              :: IPNT_S1A = 4           ! pointers in sumval* arrays
integer                              :: IPNT_HUA = 5           ! pointers in sumval* arrays

type (tcrs), allocatable, target     :: crs(:)
integer                              :: ncrs = 0               !< Current number of monitoring cross sections in crs(:)
integer                              :: maxcrs = 2             !< Current array size/max number of monitoring cross sections in crs(:)
integer                              :: maxnval = 5            !< Current max number of quantities monitored on cross sections in the sumval* arrays.

integer, private                     :: iUniq_ = 1
character(len=*), parameter, private :: defaultName_ = 'Crs'
double precision                     :: tlastupd_sumval        !< Time at which the sumval* arrays were last updated.
double precision, allocatable        :: sumvalcum_global(:,:)  !< Global cumulative values of monitored crs quantities, only needed by MPI_RANK_0 process 
double precision, allocatable        :: sumvalcum_local(:,:)   !< Local cumulative values of monitored crs quantities 
double precision, allocatable        :: sumvalcur_global(:,:)  !< Global current values of monitored crs quantities, only needed by MPI_RANK_0 process 
double precision, allocatable        :: sumvalcur_local(:,:)   !< Local  current values of monitored crs quantities 
double precision, allocatable        :: sumvalcum_timescale(:) !< Store the time-scale multiplication (e.g. morfac in the case of sediment).
integer                              :: nval = 0               !< number of quantities moonitored including sediment
integer                              :: nNodesCrs              !< [-] Total number of nodes for all cross section geometries
integer,          allocatable, target:: nodeCountCrs(:)        !< [-] Count of nodes per cross section geometry.
double precision, allocatable, target:: geomXCrs(:)            !< [m] x coordinates of cross section geometries.
double precision, allocatable, target:: geomYCrs(:)            !< [m] y coordinates of cross section geometries.
contains

!> Returns the index/position of a named crosssection in the global set arrays of this module.
subroutine getCrosssectionIndex(crsname, index)
   character(len=*), intent(in)  :: crsname
   integer,          intent(out) :: index !< The position of the (possibly moving) observation station in all set arrays. 0 if not present.

   integer :: i

   index = 0
   do i=1,ncrs
      if (trim(crs(i)%name) == trim(crsname)) then
         index = i
         exit
      end if
   end do
end subroutine getCrosssectionIndex

!> Allocates an array of cross sections, deallocating any existing memory.
subroutine allocCrossSections(cs, n)

use m_transport , only: NUMCONST
implicit none

    type(tcrs), allocatable, intent(inout) :: cs(:)   !< Array of cross sections
    integer,                 intent(in)    :: n       !< Desired nr of cross sections

    call deallocCrossSections(cs)
    allocate(cs(n))
    call ReallocCrossSectionSums(cs)                  !< needed only for old interactor
end subroutine allocCrossSections

subroutine ReallocCrosssectionSums(cs)
use m_transport , only: NUMCONST
use m_alloc
use m_sediment, only: jased, stmpar
implicit none
    type(tcrs), allocatable, intent(inout) :: cs(:)   !< Array of cross sections
    integer :: i
    integer                                :: maxnval

    maxnval = 5 + NUMCONST

    if( jased == 4 .and. stmpar%lsedtot > 0 ) then
       maxnval = maxnval + 1
       if( stmpar%lsedsus > 0 ) then
          maxnval = maxnval + 1
       endif
       maxnval = maxnval + stmpar%lsedtot
       maxnval = maxnval + stmpar%lsedsus
    endif


    do i=1,size(cs)
        call realloc(cs(i)%sumvalcur, maxnval, fill=0.0d0, keepExisting=.True.)
        call realloc(cs(i)%sumvalcum, maxnval, fill=0.0d0, keepExisting=.True.)
        call realloc(cs(i)%sumvalavg, maxnval, fill=0.0d0, keepExisting=.True.)
    end do
end subroutine ReallocCrossSectionSums

!> Deallocates an array of crs
subroutine deallocCrossSections(cs)
    type(tcrs), allocatable, intent(inout) :: cs(:)

    integer :: i, n

    if (.not. allocated(cs)) return

    n = size(cs)
    do i=1,n
        call deallocCrossSectionPath(cs(i)%path)
        if (allocated(cs(i)%sumvalcur)) then
            deallocate(cs(i)%sumvalcur)
            deallocate(cs(i)%sumvalcum)
            deallocate(cs(i)%sumvalavg)
        end if
    end do
    deallocate(cs)
end subroutine deallocCrossSections


!> Copies array of crs into another array of crs.
subroutine copyCrossSections(rfrom, rto)
use m_alloc
    type(tcrs), intent(inout) :: rfrom(:)
    type(tcrs), intent(inout) :: rto(:)

    integer :: i, n

    n = size(rfrom)
    if (n > size(rto) .or. n == 0) return

    do i=1,n
        !maxnp  = size(rfrom(i)%path%xp)
        !maxlnx = size(rfrom(i)%path%ln)
        !call increaseCrossSectionPath(rto(i)%path, maxnp, maxlnx)
        rto(i) = rfrom(i)
    end do
end subroutine copyCrossSections


!> Increases memory for crs
subroutine increaseCrossSections(n)
    integer, intent(in) :: n !< Desired number of cross sections.

    type(tcrs), allocatable :: crst(:) ! Temp storage
    integer                 :: jacopy

    jacopy = 0

    if (n < maxcrs .and. allocated(crs)) then
        return
    end if

    call allocCrossSections(crst, maxcrs)

    if (n > maxcrs) then
        maxcrs    = max(maxcrs, int(1.2*n))
    end if

    if (allocated(crs)) then
       call copyCrossSections(crs, crst)
    end if
    call allocCrossSections(crs, maxcrs)
    call copyCrossSections(crst, crs)

    call deallocCrossSections(crst)

end subroutine increaseCrossSections


!> Starts a new cross section in the active array of crs, increasing memory when necessary.
subroutine addCrossSections(name, xp, yp, iOC)
    character(len=*), intent(in) :: name
    double precision, intent(in) :: xp(:), yp(:)
    integer, optional, intent(in):: iOC          !< local index of cross sections that are defined via *.ini, in the m_network%network%observcrs set.

    integer :: m
    character(len=1) :: cdigits

    call increaseCrossSections(ncrs+1)

    ncrs           = ncrs + 1
    call setCrossSectionPathPolyline(crs(ncrs)%path, xp, yp)
    crs(ncrs)%path%lnx  = 0

    ! Set name (or generate one)
    m = len_trim(name)
    if (m > 0) then
        m = min(len(crs(ncrs)%name), len(name))
        crs(ncrs)%name = ' '
        crs(ncrs)%name(1:m) = name(1:m)
    else ! No name given, generate one.
        write(cdigits, '(i1)') max(2, int(floor(log10(dble(iUniq_))+1)))
        write(crs(ncrs)%name, '(a,i'//cdigits//'.'//cdigits//')'), trim(defaultName_), iUniq_
        iUniq_ = iUniq_ + 1
    end if

    ! Set mapping from global index to local crs that are defined by branchId and chainage
    if (present(iOC)) then
       crs(ncrs)%loc2OC = iOC
    else
       crs(ncrs)%loc2OC = 0
    end if

end subroutine addCrossSections


!> Deletes all cross sections from crs.
!! Does not free up memory, use deallocCrossSections for that.
subroutine delCrossSections()
    ncrs = 0
    iUniq_ = 1

    if (allocated(sumvalcur_local)) then
       deallocate(sumvalcur_local)
    end if
    if (allocated(sumvalcum_timescale)) then
       deallocate(sumvalcum_timescale)
    end if
    tlastupd_sumval = dmiss

    ! Do not reset crs data, just let it be overwritten later.
end subroutine delCrossSections

!> Reads observation cross sections and adds them to the normal crs adm
!! Two file types are supported: *_crs.pli and *_crs.ini.
subroutine loadObservCrossSections(filename, jadoorladen)
   use unstruc_messages
   use m_readObservCrossSections, only: readObservCrossSections
   use unstruc_channel_flow, only: network

   implicit none
   character(len=*), intent(in   ) :: filename    !< File containing the observation cross sections. Either a *_crs.pli or a *_crs.ini.
   integer,          intent(in   ) :: jadoorladen !< Append to existing observation cross sections or not

   logical :: jawel
   integer :: tok_pli, tok_ini

   !!!!!
   inquire(file = filename, exist = jawel)
   if (jawel) then
      if (jadoorladen == 0) then
         call delCrossSections()
      end if
      tok_pli = index(filename, '.pli')
      tok_ini = index(filename, '.ini')
      if (tok_pli > 0) then
         call loadObservCrossSections_from_pli(filename)
      else if (tok_ini > 0) then
         call readObservCrossSections(network, filename)
         call addObservCrsFromIni(network, filename)
      else
         call mess(LEVEL_WARN, "Observation cross section file ('"//trim(filename)//"') does not end with .pli or .ini.")
      end if
   else
       call mess(LEVEL_ERROR, "Observation cross section file '"//trim(filename)//"' not found!")
   endif
end subroutine loadObservCrossSections


!> Reads observation points from an *.pli file.
! Typically called via loadObservCrossSections().
subroutine loadObservCrossSections_from_pli(filename)
   use messageHandling
   use dfm_error
   use m_polygon
   implicit none
   character(len=*), intent(in) :: filename

   integer :: minp, ipli

   call oldfil(minp, filename)
   ipli = 0
   call reapol_nampli(minp, 0, 1, ipli)
   call pol_to_crosssections(xpl, ypl, npl, names=nampli)
   call doclose(minp)

end subroutine loadObservCrossSections_from_pli


!> Adds observation cross sections, that are read from *.ini file, to the normal cross section adm
subroutine addObservCrsFromIni(network, filename)
   use m_network
   use m_sferic, only:jsferic
   use odugrid
   use m_save_ugrid_state
   use dfm_error
   use m_missing
   use m_ObservCrossSections
   implicit none
   type(t_network),  intent(inout)       :: network            !< network
   character(len=*), intent(in   )       :: filename           !< filename of the cross section file

   integer                               :: nByBrch            ! number of cross sections that are defined by branchID and chainage
   integer                               :: ierr, ncrsini, i, numv
   type(t_observCrossSection), pointer   :: pCrs
   integer,              allocatable     :: branchIdx_tmp(:), ibrch2crs(:)
   double precision    , allocatable     :: Chainage_tmp(:), xx_tmp(:), yy_tmp(:)


   ierr    = DFM_NOERR
   nByBrch   = 0
   ncrsini = network%observcrs%count

   !! Step 1. get x- and y-coordinates of crs that are defined by branchID and chainage
   ! 1a. save their branchIdx and chainage to temporary arrays
   allocate(branchIdx_tmp(ncrsini))
   allocate(Chainage_tmp(ncrsini))
   allocate(ibrch2crs(ncrsini))

   do i=1, ncrsini
      pCrs => network%observcrs%observcross(i)
      if (pCrs%branchIdx > 0) then
         nByBrch = nByBrch + 1
         branchIdx_tmp(nByBrch) = pCrs%branchIdx
         Chainage_tmp(nByBrch)  = pCrs%chainage
         ibrch2crs(nByBrch)     = i
      end if
   end do

   ! 1b. get the corresponding x- and y-coordinates
      allocate(xx_tmp(nByBrch))
      allocate(yy_tmp(nByBrch))
   do i = 1, nByBrch
      ierr = odu_get_xy_coordinates(branchIdx_tmp(i:i), Chainage_tmp(i:i), meshgeom1d%ngeopointx, meshgeom1d%ngeopointy, &
                                       meshgeom1d%nbranchgeometrynodes, meshgeom1d%nbranchlengths, jsferic, xx_tmp(i:i), yy_tmp(i:i))
   enddo

      if (ierr /= DFM_NOERR) then
         call mess(LEVEL_ERROR, "Error occurs when getting xy coordinates for observation cross sections from file '"//trim(filename)//".")
      end if

      do i=1, nByBrch
         pCrs => network%observcrs%observcross(ibrch2crs(i))
         pCrs%x(1) = xx_tmp(i)
         pCrs%y(1) = yy_tmp(i)
      end do

   ! Step 2. add all observation crs from *.ini file
   do i =1, ncrsini
      pCrs => network%observcrs%observcross(i)
      numv = pCrs%numValues
      if (pCrs%branchIdx > 0) then ! crs which is defined by branchID and chainage
         call addCrossSections(pCrs%name, pCrs%x(1:numv), pCrs%y(1:numv), iOC = i)
      else
         call addCrossSections(pCrs%name, pCrs%x(1:numv), pCrs%y(1:numv))
      end if
   end do

   if(allocated(branchIdx_tmp))deallocate(branchIdx_tmp)
   if(allocated(Chainage_tmp)) deallocate(Chainage_tmp)
   if(allocated(ibrch2crs))    deallocate(ibrch2crs)
   if(allocated(xx_tmp))       deallocate(xx_tmp)
   if(allocated(yy_tmp))       deallocate(yy_tmp)

end subroutine addObservCrsFromIni


!> Converts a set of polylines into cross sections
!! The input arrays have the structure of the global polygon:
!! one or more polylines separated by dmiss values.
subroutine pol_to_crosssections(xpl, ypl, npl, names)
    use m_missing

    double precision, intent(in) :: xpl(:), ypl(:)     !< Long array with one or more polylines, separated by dmiss
    integer,          intent(in) :: npl                !< Total number of polyline points
    character(len=*), optional, intent(in) :: names(:) !< Optional names for cross sections

    integer :: i, i1, i2, ic, numnam
    character(len=IdLen) :: name

    if (present(names)) then
        numnam = size(names)
    else
        numnam = 0
    end if

    i1 = 1 ! First possible start index
    i2 = 0 ! No end index found yet.
    ic = 0 ! Nr of polylines found so far
    do i = 1,npl
        if (xpl(i) == dmiss .or. i == npl) then
            if (i == npl .and. xpl(i) /= dmiss) then
                i2 = i ! Last polyline, no dmiss separator, so also include last point #npl.
            end if
            if (i1 <= i2) then
                ! 1: Special name for this CRS or not?
                ic = ic + 1
                if (ic <= numnam) then
                    name = names(ic)
                else
                    name = ' '
                end if

                ! 2: add the current polyline as a new crs.
                call addCrossSections(name, xpl(i1:i2), ypl(i1:i2))
            end if
            i1 = i+1
            cycle
        else
            i2 = i ! Advance end point by one.
        end if
    end do
end subroutine pol_to_crosssections

!> Fills in the geometry arrays of cross sections for history output
!! Two special situations are also treated:
!! 1. The flowlinks of one cross section are not successive on one subdomain, which can happen in both sequetial and parallel simulations.
!! 2. In parallel simulations, a cross section lies on multiple subdomains.
subroutine fill_geometry_arrays_crs()
   use m_alloc
   use m_partitioninfo
   use m_GlobalParameters
   use m_flowparameters, only: eps6
   use precision_basics
   implicit none

   double precision, allocatable :: xGat(:), yGat(:)    ! Coordinates that are gathered data from all subdomains
   integer,          allocatable :: nodeCountCrsMPI(:)  ! Count of nodes per cross section after mpi communication.
   double precision, allocatable :: geomXCrsMPI(:)      ! [m] x coordinates of cross sections after mpi communication.
   double precision, allocatable :: geomYCrsMPI(:)      ! [m] y coordinates of cross sections after mpi communication.
   integer,          allocatable :: nodeCountCrsGat(:), nNodesCrsGat(:), displs(:)
   double precision, allocatable :: geomX(:), geomY(:)
   integer                       :: nlinks, i, j, j1, k, k1, ierror, is, ie, n, ii, nNodes, nNodesCrsMPI, L, L0, ks, ke, nPar, nNodesAdd, nn, jaexist, nb, nbLast, kk
   double precision              :: xNew, yNew, xOld, yOld
   integer,          allocatable :: maskBnd(:), maskBndAll(:), maskBndGat(:), indBndMPI(:), jaCoincide(:)  ! Arrays for boundary nodes, only used in parallel run

   ! Allocate and construct geometry variable arrays (on one subdomain)
   call realloc(nodeCountCrs,   ncrs, keepExisting = .false., fill = 0  )

   do i = 1, ncrs
      nlinks = crs(i)%path%lnx
      if (nlinks > 0 ) then
         nodeCountCrs(i) = nlinks + 1 ! Here assumes that the flowlinks of the cross section are successive.
                                      ! The situation when they are not successive will be handled later in this subroutine.
      end if
   end do
   nNodesCrs = sum(nodeCountCrs)
   call realloc(geomXCrs,       nNodesCrs,   keepExisting = .false., fill = 0d0)
   call realloc(geomYCrs,       nNodesCrs,   keepExisting = .false., fill = 0d0)
   if (jampi > 0) then
      ! In parallel runs, one cross section might lie on multiple subdomains. To handle this situation,
      ! we will need to know which nodes are on boundaries of a cross section on each subdomain, and the boundary nodes will be handled separately.
      ! This will aviod having duplicated (boundary) nodes in the arrays of coordinates of a cross section among all subdomains.
       call realloc(maskBndAll, nNodesCrs, keepExisting = .false., fill = 0) ! If the node is a boundary node then the value will be 1
   end if
   is = 0
   ie = 0
   do i = 1, ncrs
      nNodes = nodeCountCrs(i)
      nlinks = crs(i)%path%lnx
      if (nNodes > 0) then
         call realloc(geomX, max(allocSize(geomX), nNodes), keepExisting=.false.)
         call realloc(geomY, max(allocSize(geomY), nNodes), keepExisting=.false.)
         L = crs(i)%path%iperm(1)
         geomX(1) = crs(i)%path%xk(1,L)
         geomX(2) = crs(i)%path%xk(2,L)
         geomY(1) = crs(i)%path%yk(1,L)
         geomY(2) = crs(i)%path%yk(2,L)

         if (jampi > 0) then
            ! Determine the 1st boundary node (Boundary nodes are only useful for parallel simulations).
            call realloc(maskBnd, nNodes, keepExisting = .false., fill = 0)
            if (nlinks == 1) then
               ! If there is only one link, then the two nodes are the boundary nodes.
               maskBnd(1) = 1
               maskBnd(2) = 1
            else
               ! If there are more than one link, then (geomX(1),geomY(1)) is a boundary node.
               maskBnd(1) = 1
            end if
         end if

         if (nlinks > 1) then
            ! If there is more than one link, adding more nodes to coordinates arrays.
            k = 3
            nNodesAdd = 0
            do L0 = 2, nlinks
               L = crs(i)%path%iperm(L0)

               if (comparereal(crs(i)%path%xk(1,L), geomX(k-1), eps6)/=0 .or. comparereal(crs(i)%path%yk(1,L), geomY(k-1), eps6)/=0) then
                  ! If the 1st node of link L is not the ending node of the prvious link,
                  ! then this means that the flowlinks for this cross section are not succesive, and
                  ! they have a break between Link L and the previous link.
                  ! In this siutation, one more node, i.e. the 1st node of link L, should be included.
                  nNodes = nNodes + 1
                  nodeCountCrs(i) = nNodes
                  call realloc(geomX, max(allocSize(geomX), nNodes), keepExisting=.true.)
                  call realloc(geomY, max(allocSize(geomY), nNodes), keepExisting=.true.)
                  geomX(k) = crs(i)%path%xk(1,L)
                  geomY(k) = crs(i)%path%yk(1,L)

                  if (jampi > 0) then ! Mark this node and the previous node as boundary nodes.
                     call realloc(maskBnd, nNodes, keepExisting=.true.)
                     maskBnd(k-1) = 1
                     maskBnd(k) = 1
                  end if

               k = k+1
                  nNodesAdd = nNodesAdd + 1
               end if

               ! We take the 2nd node of link L, because the orientation of all links hxBndas been
               ! guaranteed in subroutien crspath_on_singlelink.
               geomX(k) = crs(i)%path%xk(2,L)
               geomY(k) = crs(i)%path%yk(2,L)

               if (jampi > 0 .and. L0 == nlinks) then ! The 2nd node of the last link is a boundary node.
                  maskBnd(k) = 1
               end if

               k = k+1
            end do

            if (nNodesAdd > 0) then
               nNodesCrs = nNodesCrs + nNodesAdd
               call realloc(geomXCrs, nNodesCrs, keepExisting = .true.)
               call realloc(geomYCrs, nNodesCrs, keepExisting = .true.)
               call realloc(maskBndAll, nNodesCrs, keepExisting = .true.)
            end if

            is = ie + 1
            ie = is + nNodes - 1
            geomXCrs(is:ie) = geomX(1:nNodes)
            geomYCrs(is:ie) = geomY(1:nNodes)
            if (jampi > 0) then
               maskBndAll(is:ie) = maskBnd(1:nNodes)
         end if
      end if
      end if
   end do

   !! The codes below are similar to subroutine "fill_geometry_arrays_lateral".
   !! They work for cross sections, including the situataion that a cross section lies on multiple subdomains.
   ! For parallel simulation: since only process 0000 writes the history output, the related arrays
   ! are only made on 0000.
   if (jampi > 0) then
      call reduce_int_sum(nNodesCrs, nNodesCrsMPI) ! Get total number of nodes among all subdomains

      if (my_rank == 0) then
         ! Allocate arrays
         call realloc(nodeCountCrsMPI, ncrs,  keepExisting = .false., fill = 0  )
         call realloc(geomXCrsMPI,     nNodesCrsMPI, keepExisting = .false., fill = 0d0)
         call realloc(geomYCrsMPI,     nNodesCrsMPI, keepExisting = .false., fill = 0d0)

         ! Allocate arrays that gather information from all subdomains
         ! Data on all subdomains will be gathered in a contiguous way
         call realloc(nodeCountCrsGat, ncrs*ndomains, keepExisting = .false., fill = 0  )
         call realloc(xGat,            nNodesCrsMPI,  keepExisting = .false., fill = 0d0)
         call realloc(yGat,            nNodesCrsMPI,  keepExisting = .false., fill = 0d0)
         call realloc(displs,          ndomains,      keepExisting = .false., fill = 0  )
         call realloc(nNodesCrsGat,    ndomains,      keepExisting = .false., fill = 0  )
         call realloc(maskBndGat,      nNodesCrsMPI,  keepExisting = .false., fill = 0  )
      else
         ! NOTE: dummy allocate to prevent crash in Debug-model on Intel MPI, even though receive buffers are officially not needed on non-root.
         allocate(nodeCountCrsGat(0), xGat(0), yGat(0), displs(0), nNodesCrsGat(0), maskBndGat(0))
      end if

      ! Gather integer data, where the same number of data, i.e. ncrs, are gathered from each subdomain to process 0000
      call gather_int_data_mpi_same(ncrs, nodeCountCrs, ncrs*ndomains, nodeCountCrsGat, ncrs, 0, ierror)

      if (my_rank == 0) then
         ! To use mpi gather call, construct displs, and nNodesCrsGat (used as receive count for mpi gather call)
         displs(1) = 0
         do i = 1, ndomains
            is = (i-1)*ncrs+1 ! Starting index in nodeCountCrsGat
            ie = is+ncrs-1    ! Endding index in nodeCountCrsGat
            nNodesCrsGat(i) = sum(nodeCountCrsGat(is:ie)) ! Total number of nodes on subdomain i
            if (i > 1) then
               displs(i) = displs(i-1) + nNodesCrsGat(i-1)
            end if
         end do
      end if

      ! Gather double precision data, here, different number of data can be gatherd from different subdomains to process 0000
      call gatherv_double_data_mpi_dif(nNodesCrs, geomXCrs, nNodesCrsMPI, xGat, ndomains, nNodesCrsGat, displs, 0, ierror)
      call gatherv_double_data_mpi_dif(nNodesCrs, geomYCrs, nNodesCrsMPI, yGat, ndomains, nNodesCrsGat, displs, 0, ierror)
      call gatherv_int_data_mpi_dif(nNodesCrs,maskBndAll, nNodesCrsMPI, maskBndGat, ndomains, nNodesCrsGat, displs, 0, ierror)
      if (my_rank == 0) then
         ! Construct nodeCountCrsMPI for history output
         do i = 1, ncrs
            do n = 1, ndomains
               k = (n-1)*ncrs+i
               nodeCountCrsMPI(i) = nodeCountCrsMPI(i) + nodeCountCrsGat(k) ! Total number of nodes for cross section i among all subdomains
            end do
         end do

         ! Construct geomXCrsMPI and geomYCrsMPI for history output
         j = 0
         do i = 1, ncrs                     ! for each cross section
            nPar = 0                        ! Number of subdomains that contain this cross section
            nb = 0                          ! Number of boundary nodes for this cross section
            nbLast = 0                      ! Number of boundary nodes for this cross section in the previous subdomains
            call realloc(indBndMPI, nodeCountCrsMPI(i), keepExisting = .false., fill = 0)
            call Realloc(jaCoincide,nodeCountCrsMPI(i), keepExisting = .false., fill = 0)
            do n = 1, ndomains              ! on each sudomain
               k = (n-1)*ncrs+i             ! index in nodeCountCrsGat
               nNodes = nodeCountCrsGat(k)  ! cross section i on sumdomain n has nNodes nodes
               if (nNodes > 0) then
                  nPar = nPar + 1
                  ii = (n-1)*ncrs
                  is = sum(nNodesCrsGat(1:n-1)) + sum(nodeCountCrsGat(ii+1:ii+i-1))! starting index in xGat
                  ks = 1
                  ke = nNodes
                  if (nPar > 1) then ! This cross section lies on multiple subdomains.
                     ! Select and add the nodes of this cross section on the current subdomain
                     do k1 = ks, ke
                        kk = is+k1
                        if (maskBndGat(kk) == 1) then ! If it is a boundary node, need to check if it already exists in the coordinate arrays, i.e. GeomXCrsMPI and GeomYCrsMPI
                           xNew = xGat(kk)
                           yNew = yGat(kk)
                           jaexist = 0
                           do j1 = 1, nbLast ! Loop over all the boundary nodes of the previous subdomains that have been added in the coordinate arrays
                              if (jaCoincide(j1) == 0) then
                                 ! If the j1 boundary node is not coincide with any boundary node, then check if node (xNew,yNew) is coincide with it or not.
                                 ! If jaCoincide(j1) == 1, then no need to check this node because one boundary node can be
                                 ! coincide with another boundary node maximal ONCE.
                                 xOld = geomXCrsMPI(indBndMPI(j1))
                                 yOld = geomYCrsMPI(indBndMPI(j1))
                                 if (comparereal(xNew, xOld, eps6)==0 .and. comparereal(xNew, xOld, eps6)==0) then
                                    jaexist = 1
                                    jaCoincide(j1) = 1
                                    exit
                                 end if
                              end if
                           end do
                           if (jaexist == 0) then ! If the new candidate node does not exist in the coordinate arrays, then add it
                              j = j + 1
                              geomXCrsMPI(j) = xNew
                              geomYCrsMPI(j) = yNew
                              nb = nb + 1         ! add one boundary node
                              indBndMPI(nb) = j   ! store its index in geomXCrsMPI (and geomYCrsMPI)
                           else
                        nodeCountCrsMPI(i) = nodeCountCrsMPI(i) - 1 ! adjust the node counter
                        nNodesCrsMPI = nNodesCrsMPI - 1
                     end if
                        else ! If it is not a boundary node, then add it directly
                           j = j + 1
                           geomXCrsMPI(j) = xGat(kk)
                           geomYCrsMPI(j) = yGat(kk)
                  end if
                     end do
                  else
                     do k1 = ks, ke
                     j = j + 1
                        kk = is + k1
                        geomXCrsMPI(j) = xGat(kk)
                        geomYCrsMPI(j) = yGat(kk)
                        if (maskBndGat(kk) == 1) then
                           nb = nb + 1
                           indBndMPI(nb) = j ! store the index in geomXCrsMPI for the boundary nodes
                        end if
                  end do
               end if
                  nbLast = nb ! update nbLast when the current subdomain is finished.
               end if
            end do
         end do
   
         ! Copy the MPI-arrays to nodeCountCrs, geomXCrs and geomYCrs for the his-output
         nNodesCrs = nNodesCrsMPI
         nodeCountCrs(1:ncrs) = nodeCountCrsMPI(1:ncrs)
         call realloc(geomXCrs, nNodesCrs, keepExisting = .false., fill = 0d0)
         call realloc(geomYCrs, nNodesCrs, keepExisting = .false., fill = 0d0)
         geomXCrs(1:nNodesCrs) = geomXCrsMPI(1:nNodesCrs)
         geomYCrs(1:nNodesCrs) = geomYCrsMPI(1:nNodesCrs)
      end if
   end if
end subroutine fill_geometry_arrays_crs

end module m_monitoring_crosssections
