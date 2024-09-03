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

! $Id: volume_table.f90 142608 2023-03-01 15:48:13Z buwalda $
! : https://svn.oss.deltares.nl/repos/delft3d/trunk/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute/volume_table.f90 $

!> Module for using volume tables at 1d nodes for the computation of the total volume of water in a node.
module m_VolumeTables

   use messageHandling
   use m_GlobalParameters

   implicit none

   private

   character(len=11), parameter :: volumeTableFileType = 'volumeTable'
   integer, parameter :: VolumeTableFileMajorVersion = 2
   integer, parameter :: VolumeTableFileMinorVersion = 0

   public makeVolumeTables

   interface dealloc
      module procedure deallocVolTables
   end interface

   !> Derived type for storing multiple tables relevant for volume calculations.
   !! Includes type-bound procedures for accessing the tables.
   type, public :: t_voltable
      integer :: count !< Number of levels in the volume table.
      logical :: hasSummerdike !< Indicates whether on 1 or more links a summerdike is attached.
      !< A summerdike has a hysteresis. As a result the array vol contains
      !< the volumes corresponding to the rising part of the hysteresis
      logical :: hasDecreasingWidths !< In case of a simulation with the Nested Newton solver, a distinction
      !< between a non-decreasing part of the cross section is made and a
      !< non-increasing part. In case of Nested Newton and 1 or more
      !< surrounding closed cross sections, volDecreasing is allocated and
      !< filled.
      double precision :: bedLevel !< The bed level at the location of the volume table.
      double precision :: topHeight !< Highest level (w.r.t. the bed level) of the surrounding cross sections
      double precision, allocatable, dimension(:) :: vol !< Volume at each level of the table.
      double precision, allocatable, dimension(:) :: sur !< Surface area at each level of the table.
      double precision, allocatable, dimension(:) :: volDecreasing !< Volume table for decreasing widths (Nested Newton)
      double precision, allocatable, dimension(:) :: surDecreasing !< Surface area table for decreasing widths (Nested Newton)

      integer :: numberOfSummerDikes !< number of summerdikes connected to the volume table.
      logical, allocatable, dimension(:) :: inundationPhase !< During the inundation phase the storage area is different from the
      !< period, when the water levels are declining.
      integer, allocatable, dimension(:) :: linkNumber !< Flow link number of the location of the summer dike.
      double precision, allocatable, dimension(:) :: summerDikeCrestLevel !< crest level of the summerdike.
      double precision, allocatable, dimension(:) :: summerDikeBaseLevel !< base level (bottom level) of the summerdike.
      double precision, allocatable, dimension(:, :) :: sdinVolume !< correction for the volume due to the hysteresis during the inundation phase of the summerdike.
      double precision, allocatable, dimension(:, :) :: sdinArea !< correction for the area due to the hysteresis during the inundation phase of the summerdike.
   contains
      procedure, pass :: alloc => allocVoltable !< Allocates the allocatable arrays in this structure
      procedure, pass :: dealloc => deallocVoltable !< Deallocates the allocatable arrays in this structure
      procedure, pass :: getVolume => getVolumeVoltable !< Returns the volume for a given water level
      procedure, pass :: getSurface => getSurfaceVoltable !< Returns the surface area for a given water level
      procedure, pass :: getVolumeDecreasing => getVolumeDecreasingVoltable !< Returns the volume which is the result of a decreasing width for a given water level
      procedure, pass :: getSurfaceDecreasing => getSurfaceDecreasingVoltable !< Returns the decreasing surface area for a given water level
      procedure, pass :: computeSurface => computeSurfaceVoltable !< Computes the surface areas for the different levels
   end type

   type(t_voltable), target, public, allocatable, dimension(:) :: vltb !< [-] 1D Volume tables {"shape": ["ndx1d"]}
   type(t_voltable), target, public, allocatable, dimension(:, :) :: vltbOnLinks !< [-] 1D Volume tables, used for storage table output on branches. {"shape": [2 ,"ndx1d"]}
   integer, target, public :: ndx1d !< [-] volume table size {"rank": 0}

contains

   !> Allocate the volume table arrays and initialize to 0
   subroutine allocVoltable(this)
      use m_flowparameters

      class(t_voltable) :: this

      if (this%count < 0) then
         return
      end if

      allocate (this%vol(this%count))
      allocate (this%sur(this%count))
      this%vol = 0.0d0
      this%sur = 0.0d0

      if (this%numberOfSummerDikes > 0) then
         allocate (this%inundationPhase(this%numberOfSummerDikes))
         allocate (this%linkNumber(this%numberOfSummerDikes))
         allocate (this%summerDikeCrestLevel(this%numberOfSummerDikes))
         allocate (this%summerDikeBaseLevel(this%numberOfSummerDikes))
         allocate (this%sdinVolume(this%numberOfSummerDikes, this%count))
         allocate (this%sdinArea(this%numberOfSummerDikes, this%count))
         this%inundationPhase = .false.
         this%sdinVolume = 0d0
         this%sdinArea = 0d0
      end if

      if (this%hasDecreasingWidths) then
         allocate (this%volDecreasing(this%count))
         allocate (this%surDecreasing(this%count))
         this%volDecreasing = 0.0d0
         this%surDecreasing = 0.0d0
      end if
   end subroutine allocVoltable

   !> Deallocate the volume table arrays
   subroutine deallocVoltable(this)
      class(t_voltable) :: this

      if (allocated(this%vol)) deallocate (this%vol)
      if (allocated(this%sur)) deallocate (this%sur)

      if (this%numberOfSummerDikes /= 0) then
         if (allocated(this%inundationPhase)) deallocate (this%inundationPhase)
         if (allocated(this%linkNumber)) deallocate (this%linkNumber)
         if (allocated(this%summerDikeCrestLevel)) deallocate (this%summerDikeCrestLevel)
         if (allocated(this%summerDikeBaseLevel)) deallocate (this%summerDikeBaseLevel)
         if (allocated(this%sdinVolume)) deallocate (this%sdinVolume)
         if (allocated(this%sdinArea)) deallocate (this%sdinArea)
      end if

      if (this%hasDecreasingWidths) then
         if (allocated(this%volDecreasing)) deallocate (this%volDecreasing)
         if (allocated(this%surDecreasing)) deallocate (this%surDecreasing)
      end if
   end subroutine deallocVoltable

   !> Retrieve the volume for given volume table and water level
   double precision function getVolumeVoltable(this, level)
      use unstruc_channel_flow
      class(t_voltable) :: this
      double precision, intent(in) :: level !< water level

      integer :: index
      integer :: i
      double precision :: heightIncrement
      index = min(int(max(0d0, level - this%bedLevel) / tableIncrement) + 1, this%count)

      heightIncrement = max(0d0, ((level - this%bedLevel) - dble(index - 1) * tableIncrement))

      getVolumeVoltable = this%vol(index) + this%sur(index) * heightIncrement

      ! Take the effect of summerdikes into account, only a correction is necessary during the indundation phase. The other phase
      ! is already taken into account in the volume table.
      do i = 1, this%numberOfSummerDikes
         if (level >= (this%summerDikeCrestLevel(i) + summerDikeTransitionHeight)) then
            this%inundationPhase(i) = .false.
         elseif (level < this%summerDikeBaseLevel(i)) then
            this%inundationPhase(i) = .true.
         end if

         if (this%inundationPhase(i)) then
            ! correct volume for the inundation hysteresis of the summerdike.
            getVolumeVoltable = getVolumeVoltable + this%sdinVolume(i, index) + this%sdinArea(i, index) * heightIncrement
         end if
      end do

   end function getVolumeVoltable

   !> Retrieve the surface area for given volume table and water level
   double precision function getSurfaceVoltable(this, level)
      use unstruc_channel_flow

      class(t_voltable) :: this
      double precision, intent(in) :: level !< water level

      integer :: index
      integer :: i
      if (level < this%bedlevel) then
         getSurfaceVoltable = 0d0
         return
      end if

      index = min(int(max(0d0, level - this%bedLevel) / tableIncrement) + 1, this%count)

      getSurfaceVoltable = this%sur(index)

      ! During the inundation phase the water surface area must be corrected
      do i = 1, this%numberOfSummerDikes
         if (level >= (this%summerDikeCrestLevel(i) + summerDikeTransitionHeight)) then
            this%inundationPhase(i) = .false.
         else if (level < this%summerDikeBaseLevel(i)) then
            this%inundationPhase(i) = .true.
         end if
         if (this%inundationPhase(i)) then
            ! correct surface area for the inundation hysteresis of the summerdike.
            getSurfaceVoltable = getSurfaceVoltable + this%sdinArea(i, index)
         end if
      end do
   end function getSurfaceVoltable

   !> Returns the volume which is the result of a decreasing width for a given water level
   double precision function getVolumeDecreasingVoltable(this, level)
      use unstruc_channel_flow

      class(t_voltable) :: this
      double precision, intent(in) :: level !< water level

      integer :: index
      double precision :: heightIncrement
      index = min(int(max(0d0, level - this%bedLevel) / tableIncrement) + 1, this%count)

      heightIncrement = ((level - this%bedLevel) - dble(index - 1) * tableIncrement)

      getVolumeDecreasingVoltable = this%volDecreasing(index) + this%surDecreasing(index) * heightIncrement

   end function getVolumeDecreasingVoltable

   !> Retrieve the surface area for given volume table and water level
   double precision function getSurfaceDecreasingVoltable(this, level)
      use unstruc_channel_flow

      class(t_voltable) :: this
      double precision, intent(in) :: level !< water level

      integer :: index
      index = min(int(max(0d0, level - this%bedLevel) / tableIncrement) + 1, this%count)

      getSurfaceDecreasingVoltable = this%surDecreasing(index)

   end function getSurfaceDecreasingVoltable

   !> Compute the surfaces in the volume table out of the volumes.
   subroutine computeSurfaceVoltable(this)
      use unstruc_channel_flow

      class(t_voltable) :: this

      integer i, j

      do j = 2, this%count
         this%sur(j - 1) = (this%vol(j) - this%vol(j - 1)) / tableIncrement
         if (this%hasDecreasingWidths) then
            this%surDecreasing(j - 1) = (this%volDecreasing(j) - this%volDecreasing(j - 1)) / tableIncrement
         end if
         do i = 1, this%numberOfSummerDikes
            this%sdinArea(i, j - 1) = (this%sdinVolume(i, j) - this%sdinVolume(i, j - 1)) / tableIncrement
         end do
      end do
   end subroutine computeSurfaceVoltable

   !> Generate the volume tables, by using GetCSParsTotal.
   subroutine makeVolumeTables(filename, branchOutput)

      use unstruc_channel_flow
      use m_flowparameters
      use m_flowgeom
      use m_GlobalParameters
      use m_Storage
      use m_flow
      use m_missing

      character(len=*), intent(in) :: filename !< Name of the volumetablefile
      logical, optional, intent(in) :: branchOutput !< Flag indicates whether the volumes on flow links are required.
      !< This option is used by the volume tool to aggregate volumes to branches.

      integer :: nod
      integer :: n
      integer :: LL, L
      integer :: i
      integer :: summerDikeIndex

      logical :: generateVLTBOnLinks

      type(t_chainage2cross), dimension(:, :), pointer :: line2cross
      type(t_CrossSection), pointer, dimension(:) :: cross
      type(t_storage), dimension(:), pointer :: stors

      if (present(branchOutput)) then
         generateVLTBOnLinks = branchOutput
      else
         generateVLTBOnLinks = .false.
      end if

      ndx1d = ndx - ndx2d ! include also the 2d boundary nodes.

      if (useVolumeTableFile .and. .not. generateVLTBOnLinks) then
         ! Do not use the volumetable file for the storage tables.
         volumeTableFile = fileName
         if (readVolumeTables()) then
            return
         end if
      end if

      line2cross => network%adm%line2cross
      cross => network%crs%cross
      stors => network%storS%stor

      if (allocated(vltb)) then
         call dealloc(vltb)
      end if
      allocate (vltb(ndx1d))
      do n = 1, ndx1d
         vltb(n)%count = 0
         vltb(n)%topHeight = 0d0
      end do

      if (generateVLTBOnLinks) then
         allocate (vltbOnLinks(2, lnx1d))
         do n = 1, lnx1d
            vltbOnLinks(2, n)%count = 0
            vltbOnLinks(2, n)%topHeight = 0d0
         end do
      end if

      ! determine the highest level for the storage nodes
      call calculateHighestLevelOnStorageNodes(vltb)

      call initializeVltb(vltb)

      call addStorageToVltb(vltb)

      if (generateVLTBOnLinks) then
         call initializeVltbOnlinks(vltb, vltbOnLinks)
      end if

      ! Compute the contributions of all incoming and outgoing links to the volume table of the corresponding node
      do n = 1, ndx1d

         nod = n + ndx2d
         summerDikeIndex = 0

         ! Only generate volume tables for 1d nodes (so skip the 2d boundary points)
         if (abs(kcs(nod)) == 1) then

            ! compute volumes, NOTE the volume at the first level is 0 by design
            do LL = 1, nd(nod)%lnx
               L = abs(nd(nod)%ln(LL))
               if (L > lnxi) then
                  L = lbnd1d(L)
               end if
               if (kcu(L) /= 1) then
                  ! This is a 1d2d link and is not added to the volume tables
                  cycle
               end if

               if (line2cross(L, 2)%c1 > 0) then
                  if (cross(line2cross(L, 2)%c1)%hasSummerDike() .or. cross(line2cross(L, 2)%c2)%hasSummerDike()) then
                     summerDikeIndex = summerDikeIndex + 1
                  end if
               end if
               ! Reset L to original value
               L = abs(nd(nod)%ln(LL))
               if (generateVLTBOnLinks) then
                  call addVolumeAtLinkToVltb(L, n, summerDikeIndex, nd(nod)%ln(LL), vltb, vltbOnLinks)
               else
                  call addVolumeAtLinkToVltb(L, n, summerDikeIndex, nd(nod)%ln(LL), vltb)
               end if
            end do

            if (vltb(n)%numberOfSummerDikes > 0) then
               vltb(n)%sdinArea(i, vltb(n)%count) = 0d0
               vltb(n)%inundationPhase = .true.
            end if
         end if
      end do

      do n = 1, ndx1d
         call vltb(n)%computeSurface()
      end do

      if (generateVLTBOnLinks) then
         do L = 1, lnx1d
            call vltbOnLinks(1, L)%computeSurface()
            call vltbOnLinks(2, L)%computeSurface()
         end do
      end if

      if (useVolumeTableFile) then
         call writeVolumeTables()
      end if

   end subroutine makeVolumeTables

   !> Initialize the volume table:
   !> * compute the dimensions of the tables:
   !> ** number of levels
   !> ** number of summerdikes
   !> * allocate the arrays in the volume tables.
   subroutine initializeVltb(vltb)
      use m_flowparameters
      use m_flowgeom
      use unstruc_channel_flow

      type(t_voltable), dimension(:), intent(inout) :: vltb !< Volume tables.

      integer :: n, nod
      integer :: LL, L
      integer :: index
      integer :: numberOfSummerDikes
      double precision :: bobAboveBedLevel
      double precision :: topheight
      type(t_chainage2cross), dimension(:, :), pointer :: line2cross
      type(t_CrossSection), pointer :: cross1, cross2
      type(t_storage), dimension(:), pointer :: stors
      type(t_CrossSection), pointer, dimension(:) :: cross

      line2cross => network%adm%line2cross
      cross => network%crs%cross
      stors => network%storS%stor

      do n = 1, ndx1d
         vltb(n)%hasDecreasingWidths = nonlin1D >= 2
         nod = n + ndx2d
         vltb(n)%bedLevel = bl(nod)
         numberOfSummerDikes = 0
         vltb(n)%hasSummerdike = .false.

         ! Determine highest level (characteristic height) of all incoming and outgoing links to node nod,
         ! also determine the number of links that have a summerdike in it's cross section.

         do LL = 1, nd(nod)%lnx
            L = nd(nod)%ln(LL)
            index = 2
            if (L < 0) then
               ! link from this flow node
               bobAboveBedLevel = bob0(1, -L) - bl(nod)
            else
               ! link to this node
               bobAboveBedLevel = bob0(2, L) - bl(nod)
            end if
            L = abs(L)
            if (abs(kcu(L)) == 1) then
               if (L > lnxi) then ! for 1D boundary links, refer to attached link
                  L = LBND1D(L)
               end if
               if (line2cross(L, index)%c1 > 0) then
                  ! The topheight is the highest point w.r.t. the bedlevel in that point.
                  ! Since the bed level and the bob of the link can be different, the actual highest point of the
                  ! cross section is the characteristic height + bob - bl
                  cross1 => cross(line2cross(L, index)%c1)
                  cross2 => cross(line2cross(L, index)%c2)
                  topHeight = getHighest1dLevel(cross1) + bobAboveBedLevel - bl(nod)
                  if (topHeight > vltb(n)%topHeight) then
                     vltb(n)%topHeight = topHeight
                  end if
                  topHeight = getHighest1dLevel(cross2) + bobAboveBedLevel - bl(nod)
                  if (topHeight > vltb(n)%topHeight) then
                     vltb(n)%topHeight = topHeight
                  end if

                  ! Count the number of summerdikes and subsequently set the crestlevel and the base level of the summerdikes
                  if (cross1%hasSummerDike() .or. cross2%hasSummerDike()) then
                     vltb(n)%hasSummerDike = .true.
                     numberOfSummerDikes = numberOfSummerDikes + 1
                     if (cross1%hasSummerDike()) then
                        topHeight = cross1%tabDef%summerdike%crestLevel + summerDikeTransitionHeight
                        if (topHeight > vltb(n)%topHeight) then
                           vltb(n)%topHeight = topHeight
                        end if
                     end if
                     if (cross2%hasSummerDike()) then
                        topHeight = cross2%tabDef%summerdike%crestLevel + summerDikeTransitionHeight
                        if (topHeight > vltb(n)%topHeight) then
                           vltb(n)%topHeight = topHeight
                        end if
                     end if
                  end if
               end if
            end if
         end do
         vltb(n)%numberOfSummerDikes = numberOfSummerDikes

         ! Make sure the volume table consists of at least two levels
         vltb(n)%count = max(2, int(vltb(n)%topHeight / tableIncrement) + 2)
         call vltb(n)%alloc()
      end do
   end subroutine initializeVltb

   !> Calculate the highest level for all storage nodes.
   subroutine calculateHighestLevelOnStorageNodes(vltb)
      use unstruc_channel_flow
      use m_flowgeom

      type(t_voltable), dimension(:), intent(inout) :: vltb !< Volume table

      type(t_storage), dimension(:), pointer :: stors
      integer :: nstor, i, nod, n

      nstor = network%stors%count
      if (nstor > 0) then
         stors => network%stors%stor
         do i = 1, nstor
            nod = stors(i)%grid_Point
            n = nod - ndx2d
            if (n > 0) then
               vltb(n)%topheight = max(vltb(n)%topheight, get_top_level(stors(i))) - bl(nod)
            end if
         end do
      end if
   end subroutine calculateHighestLevelOnStorageNodes

   !> Add the storage of storage nodes to the volume tables.
   subroutine addStorageToVltb(vltb)
      use m_Storage
      use m_flowgeom
      use m_flowparameters
      use unstruc_channel_flow

      type(t_voltable), dimension(:), intent(inout) :: vltb !< Volume table

      type(t_storage), dimension(:), pointer :: stors

      integer :: i, n, nod, nstor, j
      double precision :: level

      stors => network%stors%stor
      nstor = network%stors%count
      ! Compute the contribution of all the storage nodes to the volume table of the corresponding node
      do i = 1, nstor
         nod = stors(i)%grid_point
         n = nod - ndx2d
         if (n > 0) then

            do j = 1, vltb(n)%count
               level = bl(nod) + (j - 1) * tableIncrement
               vltb(n)%vol(j) = vltb(n)%vol(j) + get_volume(stors(i), level)
               ! NOTE: %sur follows at the end as an average for each level
            end do
            vltb(n)%sur(vltb(n)%count) = vltb(n)%sur(vltb(n)%count) + get_surface(stors(i), level)
         end if
      end do
   end subroutine addStorageToVltb

   !> Initialize vltbOnLinks, using the volume table on nodes.
   subroutine initializeVltbOnlinks(vltb, vltbOnLinks)
      use m_flowgeom

      type(t_voltable), dimension(:), intent(in) :: vltb !< Volume tables on nodes.
      type(t_voltable), dimension(:, :), intent(inout) :: vltbOnLinks !< Volume tables on links.

      integer :: n, nod, L, LL, numlinks
      integer :: Lindex

      do n = 1, ndx1d
         nod = n + ndx2d
         ! there is additional storage on this node, split this over the flow links
         numlinks = nd(nod)%lnx
         do LL = 1, numlinks
            L = abs(nd(nod)%ln(LL))

            if (L > lnx1d) then
               !Skip boundary links

               cycle
            end if

            if (nd(nod)%ln(LL) < 0) then
               ! link from this flow node
               Lindex = 1
            else
               ! link to this node
               Lindex = 2
            end if
            vltbOnLinks(Lindex, L)%count = vltb(n)%count
            vltbOnLinks(Lindex, L)%hasSummerdike = .false.
            vltbOnLinks(Lindex, L)%hasDecreasingWidths = .false.
            vltbOnLinks(Lindex, L)%bedLevel = vltb(n)%bedLevel
            vltbOnLinks(Lindex, L)%topHeight = vltb(n)%topHeight
            vltbOnLinks(Lindex, L)%numberOfSummerDikes = 0
            call vltbOnLinks(Lindex, L)%alloc()

            ! Distribute storage node contribution over flow links
            if (vltb(n)%vol(vltb(n)%count) > 0d0) then
               vltbOnLinks(Lindex, L)%vol = vltb(n)%vol / numlinks
               vltbOnLinks(Lindex, L)%sur(vltb(n)%count) = vltb(n)%sur(vltb(n)%count) / numlinks
            end if
         end do
      end do
   end subroutine initializeVltbOnlinks

   !> Add the volumes for flow link Lorg to the volume tables.
   subroutine addVolumeAtLinkToVltb(Lorg, n, summerDikeIndex, dir, vltb, vltbOnLinks)
      use m_flowgeom
      use unstruc_channel_flow
      use m_flowparameters
      use fm_external_forcings_data
      use m_Crosssections

      integer, intent(in) :: Lorg !< FLow link.
      integer, intent(in) :: n !< 1d node number.
      integer, intent(in) :: summerDikeIndex !< Index of the summer dike for this flow link.
      integer, intent(in) :: dir !< Direction of the flow link.
      type(t_voltable), dimension(:), intent(inout) :: vltb !< Volume tables on nodes.
      type(t_voltable), dimension(:, :), intent(inout), optional :: vltbOnLinks !< Volume tables on flow links.

      integer :: nod, nodintern
      integer :: Lindex, L
      integer :: j
      integer :: jacustombnd1d, ibndsect

      logical :: inundationPhase(2)

      double precision :: height
      double precision :: area, areadecr, sdarea
      double precision :: width, widthdecr, sdwidth
      double precision :: bobAboveBedLevel
      double precision :: dxL

      type(t_chainage2cross), dimension(:, :), pointer :: line2cross
      type(t_CrossSection), pointer, dimension(:) :: cross

      line2cross => network%adm%line2cross
      cross => network%crs%cross

      L = Lorg
      nod = n + ndx2d
      if (dir < 0) then
         ! link from this flow node
         bobAboveBedLevel = bob0(1, L) - bl(nod)
         Lindex = 1
      else
         ! link to this node
         bobAboveBedLevel = bob0(2, L) - bl(nod)
         Lindex = 2
      end if

      if (abs(kcu(L)) == 1) then ! for 1D boundary links, refer to attached link

         ! Handle boundary links
         if (dxDoubleAt1DEndNodes .and. nd(nod)%lnx == 1) then
            dxL = dx(L)
         else
            dxL = 0.5d0 * dx(L)
         end if

         jacustombnd1d = 0
         if (kcu(L) == -1 .and. allocated(bndWidth1D)) then
            ibndsect = lnxbnd(L - lnxi)
            if (ibndsect > 0) then
               if (bndWidth1D(ibndsect) /= dmiss) then
                  jacustombnd1d = 1
               end if
            end if
         end if

         ! Loop over all levels
         do j = 2, vltb(n)%count
            height = (j - 1) * tableIncrement
            if (jacustombnd1d == 1) then ! This link is a 1D bnd *and* has a custom width.
               width = bndwidth1D(ibndsect)
               area = (height - bobAboveBedLevel) * width
               ! Use the water level at the inner point of the boundary link

               if (vltb(n)%hasDecreasingWidths) then
                  widthdecr = 0d0
                  areadecr = 0d0
               end if
            else
               if (L > lnxi) then ! for 1D boundary links, refer to attached link
                  L = LBND1D(L)
               end if

               ! The bed level is the lowest point of all flow links and possibly storage nodes.
               ! In order to take this difference into account the variable bobAboveBedLevel is used
               inundationPhase = .false.
               call GetCSParsTotal(line2cross(L, 2), cross, height - bobAboveBedLevel, area, width, CSCalculationOption, hysteresis=inundationPhase, &
                                   doSummerDike=.true.)

               vltb(n)%vol(j) = vltb(n)%vol(j) + dxL * area
               if (present(vltbOnLinks)) then
                  if (lorg > lnxi) then
                     nodintern = ln(2, Lorg)
                     if (ln(1, L) == nodintern) then
                        lindex = 1
                     else
                        lindex = 2
                     end if
                  end if

                  ! Note: vltbOnlinks is only used for aggregated per branch, and the hysteresis for summerdikes
                  !       is turned of, as well as the Nested Newton solver
                  vltbOnLinks(Lindex, L)%vol(j) = vltbOnLinks(Lindex, L)%vol(j) + dxL * area
               end if

               ! A summerdike has a hysteresis during inundation the characteristic for the total volume is different from the
               ! drainage phase. In the overall volume table the summerdike during drainage is taken into account.
               ! During inundation SDINVOLUME contains the correction for the proper storage during inundation.
               if (line2cross(L, 2)%c1 > 0) then
                  if (cross(line2cross(L, 2)%c1)%hasSummerDike() .or. cross(line2cross(L, 2)%c2)%hasSummerDike()) then
                     if (j == 2) then
                        vltb(n)%linkNumber(summerDikeIndex) = L
                        call getSummerDikeData(line2cross(L, 2), cross, vltb(n)%summerDikeCrestLevel(summerDikeIndex), &
                                               vltb(n)%summerDikeBaseLevel(summerDikeIndex))

                     end if
                     inundationPhase = .true.
                     call GetCSParsTotal(line2cross(L, 2), cross, height - bobAboveBedLevel, sdarea, sdwidth, CSCalculationOption, inundationPhase, &
                                         doSummerDike=.true.)
                     sdarea = sdarea - area
                     vltb(nod)%sdinVolume(summerDikeIndex, j) = vltb(nod)%sdinVolume(summerDikeIndex, j) + &
                                                                sdarea * dxL
                  end if
               end if

               ! For Nested Newton, the
               if (vltb(n)%hasDecreasingWidths) then
                  call GetCSParsTotal(network%adm%line2cross(L, 2), cross, height - bobAboveBedLevel, areadecr, widthdecr, CS_TYPE_MIN)
               end if
            end if

            ! compute the decreasing volumes and areas
            if (vltb(n)%hasDecreasingWidths) then
               vltb(n)%volDecreasing(j) = vltb(n)%volDecreasing(j) + dxL * areadecr
               if (j == vltb(n)%count) then
                  ! water surface at the highest level is equal to the width*dx of the cross section at the highest level.
                  vltb(n)%surDecreasing(vltb(n)%count) = vltb(n)%surDecreasing(vltb(n)%count) + dxL * widthdecr
               end if
            end if
         end do

         ! water surface at the highest level is equal to the width*dx of the cross section at the highest level.
         vltb(n)%sur(vltb(n)%count) = vltb(n)%sur(vltb(n)%count) + dxL * width
         if (present(vltbOnLinks)) then
            if (L > lnxi) then ! for 1D boundary links, refer to attached link
               L = LBND1D(L)
            end if
            if (lorg > lnxi) then
               nodintern = ln(2, Lorg)
               if (ln(1, L) == nodintern) then
                  lindex = 1
               else
                  lindex = 2
               end if
            end if
            vltbOnLinks(Lindex, L)%sur(vltb(n)%count) = vltbOnLinks(Lindex, L)%sur(vltb(n)%count) + dxL * width
         end if

      end if

   end subroutine addVolumeAtLinkToVltb

   !> Deallocate all volume tables.
   subroutine deallocVolTables(vltb)

      type(t_voltable), allocatable, dimension(:) :: vltb

      integer i

      if (allocated(vltb)) then
         do i = 1, size(vltb)
            call vltb(i)%dealloc()
         end do
         deallocate (vltb)
      end if

   end subroutine deallocVolTables

   !> write the volume table to a binary file.
   subroutine writeVolumeTables()

      use m_flowgeom
      use m_flowparameters
      use m_GlobalParameters
      use unstruc_channel_flow

      integer :: ibin
      integer :: i, n, istat, j
      integer :: count, numberOfSummerdikes

      open (newunit=ibin, file=volumeTableFile, form='unformatted', access='stream', iostat=istat)
      if (istat /= 0) then
         call SetMessage(LEVEL_WARN, 'Something went wrong during the opening of binary volume table file: '//trim(volumeTableFile))
         return
      end if

      write (ibin) volumeTableFileType
      write (ibin) VolumeTableFileMajorVersion, VolumeTableFileMinorVersion

      write (ibin) ndx1d, nonlin1D

      do n = 1, ndx1d
         count = vltb(n)%count
         write (ibin) count
         write (ibin) vltb(n)%hasSummerdike
         numberOfSummerdikes = vltb(n)%numberOfSummerDikes
         write (ibin) numberOfSummerDikes
         write (ibin) vltb(n)%hasDecreasingWidths
         write (ibin) (vltb(n)%inundationPhase(i), i=1, vltb(n)%numberOfSummerDikes)
         write (ibin) vltb(n)%bedLevel
         write (ibin) vltb(n)%topHeight
         write (ibin) (vltb(n)%vol(i), i=1, count)
         write (ibin) (vltb(n)%sur(i), i=1, count)
         if (vltb(n)%hasDecreasingWidths) then
            write (ibin) (vltb(n)%volDecreasing(i), i=1, count)
            write (ibin) (vltb(n)%surDecreasing(i), i=1, count)
         end if
         if (vltb(n)%hasSummerdike) then
            write (ibin) (vltb(n)%summerDikeCrestLevel(i), i=1, numberOfSummerDikes)
            write (ibin) (vltb(n)%summerDikeBaseLevel(i), i=1, numberOfSummerDikes)
            write (ibin) ((vltb(n)%sdinVolume(i, j), i=1, numberOfSummerdikes), j=1, count)
            write (ibin) ((vltb(n)%sdinArea(i, j), i=1, numberOfSummerdikes), j=1, count)
         end if
      end do
      close (ibin)

   end subroutine writeVolumeTables

   !> Read the volume tables from a previously saved binary file.
   !> The function returns .true. when succesfull, otherwise .false. is returned
   logical function readVolumeTables()

      use m_flowgeom
      use m_GlobalParameters
      use unstruc_channel_flow
      use m_flowparameters
      use messageHandling

      integer :: ibin
      integer :: i, j, n, istat
      integer :: ndx1d_read
      integer :: count
      integer :: nonlin1D_file
      integer :: numberOfSummerdikes
      integer :: majorVersion, minorVersion
      logical :: fileExist
      character(len=11) :: fileType_

      readVolumeTables = .false.
      inquire (file=volumeTableFile, exist=fileExist)

      if (.not. fileExist) then
         call SetMessage(LEVEL_INFO, 'Volume table file: '//trim(volumeTableFile)//' does not exist, generating volume tables from scratch.')
         return
      end if

      open (newunit=ibin, file=volumeTableFile, status='old', form='unformatted', access='stream', action='read', iostat=istat)

      if (istat /= 0) then
         call SetMessage(LEVEL_WARN, 'Something went wrong during the opening of binary volume table file: '//trim(volumeTableFile))
         return
      end if

      read (ibin, iostat=istat) fileType_
      if (trim(fileType_) /= volumeTableFileType .or. istat /= 0) then
         call SetMessage(LEVEL_WARN, trim(volumeTableFile)//' is not a volume table file, the file type on the file is '''//trim(fileType_)//'''.')
         close (ibin)
         return
      end if

      read (ibin) majorVersion, minorVersion
      if (majorVersion /= VolumeTableFileMajorVersion) then
         write (msgbuf, '(''The major version of '' , a,  '' = '', i0, ''. This is not compatible with the current version'', i0)') &
            trim(volumeTableFile), majorVersion, VolumeTableFileMajorVersion
         call warn_flush()
         close (ibin)
         return
      end if
      read (ibin) ndx1d_read, nonlin1D_file

      if (ndx1d_read /= ndx1d) then
         call SetMessage(LEVEL_WARN, trim(volumeTableFile)//' is not compatible with the current model, the number of 1d cells are different.')
         return
      end if

      if (nonlin1D_file /= nonlin1d) then
         msgbuf = 'The selected method for solving the nonlinear iteration (='''//trim(getNonlin(nonlin1d))//'''), and the nonlinear iteration method '
         call warn_flush()
         msgbuf = 'on '''//trim(volumeTableFile)//''' are different (='''//trim(getNonlin(nonlin1D_file))//''').'
         call warn_flush()
         close (ibin)
         return
      end if

      allocate (vltb(ndx1d))

      do n = 1, ndx1d
         read (ibin) count
         vltb(n)%count = count
         read (ibin) vltb(n)%hasSummerdike
         read (ibin) numberOfSummerDikes
         read (ibin) vltb(n)%bedLevel
         read (ibin) vltb(n)%topHeight
         call vltb(n)%alloc()
         read (ibin) (vltb(n)%vol(i), i=1, count)
         read (ibin) (vltb(n)%sur(i), i=1, count)
         if (vltb(n)%hasDecreasingWidths) then
            read (ibin) (vltb(n)%volDecreasing(i), i=1, count)
            read (ibin) (vltb(n)%surDecreasing(i), i=1, count)
         end if
         if (vltb(n)%hasSummerdike) then
            read (ibin) (vltb(n)%inundationPhase(i), i=1, numberOfSummerDikes)
            read (ibin) (vltb(n)%summerDikeCrestLevel(i), i=1, numberOfSummerDikes)
            read (ibin) (vltb(n)%summerDikeBaseLevel(i), i=1, numberOfSummerDikes)
            read (ibin) ((vltb(n)%sdinVolume(i, j), i=1, numberOfSummerdikes), j=1, count)
            read (ibin) ((vltb(n)%sdinArea(i, j), i=1, numberOfSummerdikes), j=1, count)
         end if
      end do
      do n = 1, ndx1d
         if (vltb(n)%hasSummerdike) then
         end if
      end do

      close (ibin)
      readVolumeTables = .true.

   end function readVolumeTables

   !> Returns the nonlinear option in a text string.
   function getNonlin(nlin1D) result(string)

      character(len=30) :: string
      integer, intent(in) :: nlin1d

      select case (nlin1d)
      case (1)
         string = 'Preismann'
      case (2)
         string = 'Nested Newton'
      case (3)
         string = 'Improved Nested Newton'
      case default
         string = 'Unknown'
      end select
   end function

end module m_volumeTables
