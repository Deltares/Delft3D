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
   use precision, only: dp
   implicit none

   type tcrs
      character(len=IdLen) :: name !< Name
      integer :: nval !< Amount of different quantities monitored
      type(tcrspath) :: path !< Polyline+crossed flow links that defines this cross section.
      integer :: loc2OC = 0 !< mapping from global obs index to obs that are defined by branchID and chainage
      real(kind=dp), allocatable :: sumvalcur(:) !< Values integrated over the crs
      real(kind=dp), allocatable :: sumvalcum(:) !< Values integrated over crs *and* time
      real(kind=dp), allocatable :: sumvalavg(:) !< Values integrated over crs and averaged in time.
                                                   !! Size is nval: nr of monitored quantities.
   end type tcrs

! Indices in sumvalcur and other arrays: postfix 'C' means cumulative/sum, 'A' means averaged.
   integer :: IPNT_Q1C = 1 ! pointers in sumval* arrays
   integer :: IPNT_AUC = 2 ! pointers in sumval* arrays
   integer :: IPNT_U1A = 3 ! pointers in sumval* arrays
   integer :: IPNT_S1A = 4 ! pointers in sumval* arrays
   integer :: IPNT_HUA = 5 ! pointers in sumval* arrays

   type(tcrs), allocatable, target :: crs(:)
   integer :: ncrs = 0 !< Current number of monitoring cross sections in crs(:)
   integer :: maxcrs = 2 !< Current array size/max number of monitoring cross sections in crs(:)
   integer :: maxnval = 5 !< Current max number of quantities monitored on cross sections in the sumval* arrays.

   integer, private :: iUniq_ = 1
   character(len=*), parameter, private :: defaultName_ = 'Crs'
   real(dp), allocatable :: crs_values(:, :) !< Current values of monitored crs quantities; size = [ number of monitored values (nval) -by- number of cross-sections (ncrs)]
   real(dp), allocatable :: crs_timescales(:) !< Time-scale multiplication factors (e.g. morfac in the case of sediment).
   integer :: nval = 0 !< number of quantities moonitored including sediment
   integer :: nNodesCrs !< [-] Total number of nodes for all cross section geometries
   integer, allocatable, target :: nodeCountCrs(:) !< [-] Count of nodes per cross section geometry.
   real(kind=dp), allocatable, target :: geomXCrs(:) !< [m] x coordinates of cross section geometries.
   real(kind=dp), allocatable, target :: geomYCrs(:) !< [m] y coordinates of cross section geometries.
contains

!> Returns the index/position of a named crosssection in the global set arrays of this module.
   subroutine getCrosssectionIndex(crsname, index)
      character(len=*), intent(in) :: crsname
      integer, intent(out) :: index !< The position of the (possibly moving) observation station in all set arrays. 0 if not present.

      integer :: i

      index = 0
      do i = 1, ncrs
         if (trim(crs(i)%name) == trim(crsname)) then
            index = i
            exit
         end if
      end do
   end subroutine getCrosssectionIndex

!> Allocates an array of cross sections, deallocating any existing memory.
   subroutine allocCrossSections(cs, n)

      implicit none

      type(tcrs), allocatable, intent(inout) :: cs(:) !< Array of cross sections
      integer, intent(in) :: n !< Desired nr of cross sections

      call deallocCrossSections(cs)
      allocate (cs(n))
      call ReallocCrossSectionSums(cs) !< needed only for old interactor
   end subroutine allocCrossSections

   subroutine ReallocCrosssectionSums(cs)
      use m_transport, only: NUMCONST
      use m_alloc
      use m_sediment, only: jased, stmpar
      implicit none
      type(tcrs), allocatable, intent(inout) :: cs(:) !< Array of cross sections
      integer :: i
      integer :: maxnval

      maxnval = 5 + NUMCONST

      if (jased == 4 .and. stmpar%lsedtot > 0) then
         maxnval = maxnval + 1
         if (stmpar%lsedsus > 0) then
            maxnval = maxnval + 1
         end if
         maxnval = maxnval + stmpar%lsedtot
         maxnval = maxnval + stmpar%lsedsus
      end if

      do i = 1, size(cs)
         call realloc(cs(i)%sumvalcur, maxnval, fill=0.0d0, keepExisting=.true.)
         call realloc(cs(i)%sumvalcum, maxnval, fill=0.0d0, keepExisting=.true.)
         call realloc(cs(i)%sumvalavg, maxnval, fill=0.0d0, keepExisting=.true.)
      end do
   end subroutine ReallocCrossSectionSums

!> Deallocates an array of crs
   subroutine deallocCrossSections(cs)
      type(tcrs), allocatable, intent(inout) :: cs(:)

      integer :: i, n

      if (.not. allocated(cs)) return

      n = size(cs)
      do i = 1, n
         call deallocCrossSectionPath(cs(i)%path)
         if (allocated(cs(i)%sumvalcur)) then
            deallocate (cs(i)%sumvalcur)
            deallocate (cs(i)%sumvalcum)
            deallocate (cs(i)%sumvalavg)
         end if
      end do
      deallocate (cs)
   end subroutine deallocCrossSections

!> Copies array of crs into another array of crs.
   subroutine copyCrossSections(rfrom, rto)
      use m_alloc
      type(tcrs), intent(inout) :: rfrom(:)
      type(tcrs), intent(inout) :: rto(:)

      integer :: i, n

      n = size(rfrom)
      if (n > size(rto) .or. n == 0) return

      do i = 1, n
         !maxnp  = size(rfrom(i)%path%xp)
         !maxlnx = size(rfrom(i)%path%ln)
         !call increaseCrossSectionPath(rto(i)%path, maxnp, maxlnx)
         rto(i) = rfrom(i)
      end do
   end subroutine copyCrossSections

!> Increases memory for crs
   subroutine increaseCrossSections(n)
      integer, intent(in) :: n !< Desired number of cross sections.

      type(tcrs), allocatable :: temp_crs_array(:) ! Temp storage
      integer :: jacopy

      jacopy = 0

      if (n < maxcrs .and. allocated(crs)) then
         return
      end if

      call allocCrossSections(temp_crs_array, maxcrs)

      if (n > maxcrs) then
         maxcrs = max(maxcrs, int(1.2 * n))
      end if

      if (allocated(crs)) then
         call copyCrossSections(crs, temp_crs_array)
      end if
      call allocCrossSections(crs, maxcrs)
      call copyCrossSections(temp_crs_array, crs)

      call deallocCrossSections(temp_crs_array)

   end subroutine increaseCrossSections

!> Starts a new cross section in the active array of crs, increasing memory when necessary.
   subroutine addCrossSections(name, xp, yp, iOC)
      use precision, only: dp
      character(len=*), intent(in) :: name
      real(kind=dp), intent(in) :: xp(:), yp(:)
      integer, optional, intent(in) :: iOC !< local index of cross sections that are defined via *.ini, in the m_network%network%observcrs set.

      integer :: m
      character(len=1) :: cdigits

      call increaseCrossSections(ncrs + 1)

      ncrs = ncrs + 1
      call setCrossSectionPathPolyline(crs(ncrs)%path, xp, yp)
      crs(ncrs)%path%lnx = 0

      ! Set name (or generate one)
      m = len_trim(name)
      if (m > 0) then
         m = min(len(crs(ncrs)%name), len(name))
         crs(ncrs)%name = ' '
         crs(ncrs)%name(1:m) = name(1:m)
      else ! No name given, generate one.
         write (cdigits, '(i1)') max(2, int(floor(log10(dble(iUniq_)) + 1)))
         write (crs(ncrs)%name, '(a,i'//cdigits//'.'//cdigits//')') trim(defaultName_), iUniq_
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
      if (allocated(crs_values)) then
         deallocate (crs_values)
      end if
      if (allocated(crs_timescales)) then
         deallocate (crs_timescales)
      end if
      ! Do not reset crs data, just let it be overwritten later.
   end subroutine delCrossSections

!> Reads observation cross sections and adds them to the normal crs adm
!! Two file types are supported: *_crs.pli and *_crs.ini.
   subroutine loadObservCrossSections(filename, jadoorladen)
      use messagehandling, only: LEVEL_WARN, LEVEL_ERROR, mess
      use m_readObservCrossSections, only: readObservCrossSections
      use unstruc_channel_flow, only: network

      implicit none
      character(len=*), intent(in) :: filename !< File containing the observation cross sections. Either a *_crs.pli or a *_crs.ini.
      integer, intent(in) :: jadoorladen !< Append to existing observation cross sections or not

      logical :: jawel
      integer :: tok_pli, tok_ini

   !!!!!
      inquire (file=filename, exist=jawel)
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
      end if
   end subroutine loadObservCrossSections

!> Reads observation points from an *.pli file.
! Typically called via loadObservCrossSections().
   subroutine loadObservCrossSections_from_pli(filename)
      use messageHandling
      use dfm_error
      use m_polygon
      use m_reapol_nampli, only: reapol_nampli
      use m_filez, only: oldfil, doclose

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
      use precision, only: dp
      use m_network
      use m_sferic, only: jsferic
      use odugrid
      use m_save_ugrid_state
      use dfm_error
      use m_missing
      use m_ObservCrossSections
      implicit none
      type(t_network), intent(inout) :: network !< network
      character(len=*), intent(in) :: filename !< filename of the cross section file

      integer :: nByBrch ! number of cross sections that are defined by branchID and chainage
      integer :: ierr, ncrsini, i, numv
      type(t_observCrossSection), pointer :: pCrs
      integer, allocatable :: branchIdx_tmp(:), ibrch2crs(:)
      real(kind=dp), allocatable :: Chainage_tmp(:), xx_tmp(:), yy_tmp(:)

      ierr = DFM_NOERR
      nByBrch = 0
      ncrsini = network%observcrs%count

   !! Step 1. get x- and y-coordinates of crs that are defined by branchID and chainage
      ! 1a. save their branchIdx and chainage to temporary arrays
      allocate (branchIdx_tmp(ncrsini))
      allocate (Chainage_tmp(ncrsini))
      allocate (ibrch2crs(ncrsini))

      do i = 1, ncrsini
         pCrs => network%observcrs%observcross(i)
         if (pCrs%branchIdx > 0) then
            nByBrch = nByBrch + 1
            branchIdx_tmp(nByBrch) = pCrs%branchIdx
            Chainage_tmp(nByBrch) = pCrs%chainage
            ibrch2crs(nByBrch) = i
         end if
      end do

      ! 1b. get the corresponding x- and y-coordinates
      allocate (xx_tmp(nByBrch))
      allocate (yy_tmp(nByBrch))
      do i = 1, nByBrch
         ierr = odu_get_xy_coordinates(branchIdx_tmp(i:i), Chainage_tmp(i:i), meshgeom1d%ngeopointx, meshgeom1d%ngeopointy, &
                                       meshgeom1d%nbranchgeometrynodes, meshgeom1d%nbranchlengths, jsferic, xx_tmp(i:i), yy_tmp(i:i))
      end do

      if (ierr /= DFM_NOERR) then
         call mess(LEVEL_ERROR, "Error occurs when getting xy coordinates for observation cross sections from file '"//trim(filename)//".")
      end if

      do i = 1, nByBrch
         pCrs => network%observcrs%observcross(ibrch2crs(i))
         pCrs%x(1) = xx_tmp(i)
         pCrs%y(1) = yy_tmp(i)
      end do

      ! Step 2. add all observation crs from *.ini file
      do i = 1, ncrsini
         pCrs => network%observcrs%observcross(i)
         numv = pCrs%numValues
         if (pCrs%branchIdx > 0) then ! crs which is defined by branchID and chainage
            call addCrossSections(pCrs%name, pCrs%x(1:numv), pCrs%y(1:numv), iOC=i)
         else
            call addCrossSections(pCrs%name, pCrs%x(1:numv), pCrs%y(1:numv))
         end if
      end do

      if (allocated(branchIdx_tmp)) deallocate (branchIdx_tmp)
      if (allocated(Chainage_tmp)) deallocate (Chainage_tmp)
      if (allocated(ibrch2crs)) deallocate (ibrch2crs)
      if (allocated(xx_tmp)) deallocate (xx_tmp)
      if (allocated(yy_tmp)) deallocate (yy_tmp)

   end subroutine addObservCrsFromIni

!> Converts a set of polylines into cross sections
!! The input arrays have the structure of the global polygon:
!! one or more polylines separated by dmiss values.
   subroutine pol_to_crosssections(xpl, ypl, npl, names)
      use precision, only: dp
      use m_missing

      real(kind=dp), intent(in) :: xpl(:), ypl(:) !< Long array with one or more polylines, separated by dmiss
      integer, intent(in) :: npl !< Total number of polyline points
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
      do i = 1, npl
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
            i1 = i + 1
            cycle
         else
            i2 = i ! Advance end point by one.
         end if
      end do
   end subroutine pol_to_crosssections

end module m_monitoring_crosssections
