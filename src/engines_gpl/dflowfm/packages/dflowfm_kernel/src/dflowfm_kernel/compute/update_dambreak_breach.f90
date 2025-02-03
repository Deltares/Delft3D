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

module m_update_dambreak_breach
   use precision, only: dp

   implicit none

   private

   public :: update_dambreak_breach
   public :: allocate_and_initialize_dambreak_data

   ! time varying, can be get/set via BMI interface
   real(kind=dp), dimension(:), allocatable, target, public :: breachDepthDambreak !< the dambreak breach width (as a level)
   real(kind=dp), dimension(:), allocatable, target, public :: breachWidthDambreak !< the dambreak breach width (as a level)
   real(kind=dp), dimension(:), allocatable, target, public :: waterLevelsDambreakUpStream !< the water levels computed each time step upstream
   real(kind=dp), dimension(:), allocatable, target, public :: waterLevelsDambreakDownStream !< the water levels computed each time step downstream

   real(kind=dp), dimension(:), allocatable, public :: normalVelocityDambreak !< dambreak normal velocity
   real(kind=dp), dimension(:), allocatable, public :: breachWidthDerivativeDambreak !< breach width derivatives
   real(kind=dp), dimension(:), allocatable, public :: waterLevelJumpDambreak !< water level jumps
   
   real(kind=dp), dimension(:,:), allocatable :: dambreakAveraging   !< (1,:) weight averaged values of waterlevel per dambreaklink
                                                                     !! (2,:) weight per dambreaklink
   ! Upstream water level
   integer, public :: nDambreakLocationsUpstream !< nr of dambreak signals with locations upstream
   integer, dimension(:), allocatable, public :: dambreakLocationsUpstreamMapping !< mapping of dambreak locations upstream
   integer, dimension(:), allocatable, public :: dambreakLocationsUpstream !< store cell ids for water level locations upstream
   integer, public :: nDambreakAveragingUpstream !< nr of dambreak signals upstream with averaging
   integer, dimension(:), allocatable, public :: dambreakAveragingUpstreamMapping !< mapping of dambreak averaging upstream
   ! Downstream water level
   integer, public :: nDambreakLocationsDownstream !< nr of dambreak signals with locations downstream
   integer, dimension(:), allocatable, public :: dambreakLocationsDownstreamMapping !< mapping of dambreak locations downstream
   integer, dimension(:), allocatable, public :: dambreakLocationsDownstream !< store cell ids for water level locations downstream
   integer, public :: nDambreakAveragingDownstream !< nr of dambreak signals downstream with averaging
   integer, dimension(:), allocatable, public :: dambreakAveragingDownstreamMapping !< mapping of dambreak averaging in the dambreak arrays

   contains

   subroutine allocate_and_initialize_dambreak_data(ndambreaksignals)
      use m_alloc, only: realloc

      integer, intent(in) :: ndambreaksignals
     
      call realloc(waterLevelsDambreakUpstream, ndambreaksignals)
      call realloc(waterLevelsDambreakDownstream, ndambreaksignals)
      call realloc(normalVelocityDambreak, ndambreaksignals)
      call realloc(breachWidthDerivativeDambreak, ndambreaksignals)
      call realloc(waterLevelJumpDambreak, ndambreaksignals)
      call realloc(dambreakAveraging, [2,ndambreaksignals])
      call realloc(dambreakLocationsUpstreamMapping, ndambreaksignals, fill=0)
      call realloc(dambreakLocationsUpstream, ndambreaksignals, fill=0)
      call realloc(dambreakAveragingUpstreamMapping, ndambreaksignals, fill=0)
      call realloc(dambreakLocationsDownstreamMapping, ndambreaksignals, fill=0)
      call realloc(dambreakLocationsDownstream, ndambreaksignals, fill=0)
      call realloc(dambreakAveragingDownstreamMapping, ndambreaksignals, fill=0)
      nDambreakLocationsUpstream = 0
      nDambreakAveragingUpstream = 0
      nDambreakLocationsDownstream = 0
      nDambreakAveragingDownstream = 0

   end subroutine allocate_and_initialize_dambreak_data

   subroutine update_dambreak_breach(startTime, deltaTime)
      use precision, only: dp
      use m_flowgeom, only: wu
      use m_flow, only: s1, hu, au, u1
      use m_missing, only: dmiss
      use unstruc_channel_flow, only: network
      use m_Dambreak, only: prepareComputeDambreak
      use m_partitioninfo, only: getAverageQuantityFromLinks
      use m_meteo, only: ec_gettimespacevalue_by_itemID, ecInstancePtr, item_dambreakLevelsAndWidthsFromTable
      use fm_external_forcings_data, only: success, ndambreaklinks, ndambreaksignals, &
         dambreaks, dambreakLevelsAndWidthsFromTable, &
         LStartBreach, L1dambreaksg, L2dambreaksg, kdambreak, activeDambreakLinks
      use m_dambreak, only: BREACH_GROWTH_VDKNAAP, BREACH_GROWTH_VERHEIJVDKNAAP, BREACH_GROWTH_TIMESERIES
      use m_flowtimes, only: irefdate, tunit, tzone

      implicit none

      !in-out
      real(kind=dp), intent(in) :: startTime
      real(kind=dp), intent(in) :: deltaTime

      !locals
      real(kind=dp) :: tempValue, smax, smin, hmx, hmn
      integer :: n, ierr, istru, indexLevelsAndWidths

      if (ndambreaksignals > 0) then ! Variable ndambreaksignals is >0 for all partitions if there is a dambreak, even if it is outside
         ! of a partition. In a parallel simulation, we need to run this subroutine even in a special situation that there is
         ! no dambreak on the current subdomain (i.e. ndambreaklinks == 0), because the following function getAverageQuantityFromLinks
         ! involves mpi communication among all subdomains. However, in this special situation,
         ! all the necessary variables are set to 0 and do not participate the dambreak related computation in this subroutine.

         !
         ! Initialize
         !
         dambreakAveraging(:,:) = 0.0d0
         waterLevelsDambreakUpStream(:) = 0.0d0
         waterLevelsDambreakDownStream(:) = 0.0d0
         normalVelocityDambreak(:) = 0.0d0
         breachWidthDerivativeDambreak(:) = 0.0d0
         waterLevelJumpDambreak(:) = 0.0d0
         !
         ! Upstream water level
         !
         if (nDambreakLocationsUpstream > 0) then
            waterLevelsDambreakUpStream(dambreakLocationsUpstreamMapping(1:nDambreakLocationsUpstream)) = s1(dambreakLocationsUpstream(1:nDambreakLocationsUpstream))
         end if

         !call this code only if something has to be averaged
         if (nDambreakAveragingUpstream > 0) then

            ! Compute sumQuantitiesByWeight upstream
            ierr = getAverageQuantityFromLinks(L1dambreaksg(dambreakAveragingUpstreamMapping(1:nDambreakAveragingUpstream)), L2dambreaksg(dambreakAveragingUpstreamMapping(1:nDambreakAveragingUpstream)), wu, kdambreak(3, :), s1, kdambreak(1, :), dambreakAveraging, 0, &
                                               hu, dmiss, activeDambreakLinks, 0)

            if (ierr /= 0) then
               success = .false.
               return
            end if

            if (ndambreaklinks > 0) then
               do n = 1, nDambreakAveragingUpstream
                  if (dambreakAveraging(2, n) > 0.0d0) then
                     waterLevelsDambreakUpStream(dambreakAveragingUpstreamMapping(n)) = dambreakAveraging(1, n) / dambreakAveraging(2, n)
                  else if (abs(startTime - network%sts%struct(dambreaks(dambreakAveragingUpstreamMapping(n)))%dambreak%T0) < 1d-10) then
                     waterLevelsDambreakUpStream(dambreakAveragingUpstreamMapping(n)) = s1(kdambreak(1, LStartBreach(dambreakAveragingUpstreamMapping(n))))
                  else
                     continue
                  end if
               end do
            end if
         end if

         !
         ! Downstream water level
         !
         if (nDambreakLocationsDownstream > 0) then
            waterLevelsDambreakDownStream(dambreakLocationsDownstreamMapping(1:nDambreakLocationsDownstream)) = s1(dambreakLocationsDownstream(1:nDambreakLocationsDownstream))
         end if

         !call this code only if something has to be averaged downstream
         if (nDambreakAveragingDownstream > 0) then

            ! Compute sumQuantitiesByWeight downstream
            ierr = getAverageQuantityFromLinks(L1dambreaksg(dambreakAveragingDownstreamMapping(1:nDambreakAveragingDownstream)), L2dambreaksg(dambreakAveragingDownstreamMapping(1:nDambreakAveragingDownstream)), wu, kdambreak(3, :), s1, kdambreak(2, :), dambreakAveraging, 0, &
                                               hu, dmiss, activeDambreakLinks, 0)

            if (ierr /= 0) then
               success = .false.
               return
            end if

            if (ndambreaklinks > 0) then
               do n = 1, nDambreakAveragingDownstream
                  if (dambreakAveraging(2, n) > 0.0d0) then
                     waterLevelsDambreakDownStream(dambreakAveragingDownstreamMapping(n)) = dambreakAveraging(1, n) / dambreakAveraging(2, n)
                  else if (abs(startTime - network%sts%struct(dambreaks(dambreakAveragingDownstreamMapping(n)))%dambreak%T0) < 1d-10) then
                     waterLevelsDambreakDownStream(dambreakAveragingDownstreamMapping(n)) = s1(kdambreak(2, LStartBreach(dambreakAveragingDownstreamMapping(n))))
                  else
                     continue
                  end if
               end do
            end if
         end if

         !
         ! u0 velocity on the flowlinks (averaged by the wetted area). The mask is the water level itself
         !
         ierr = getAverageQuantityFromLinks(L1dambreaksg, L2dambreaksg, au, kdambreak(3, :), u1, kdambreak(3, :), dambreakAveraging, 1, &
                                            hu, dmiss, activeDambreakLinks, 0)
         if (ierr /= 0) success = .false.

         if (ndambreaklinks > 0) then
            do n = 1, ndambreaksignals
               if (dambreakAveraging(2, n) > 0.0d0) then
                  normalVelocityDambreak(n) = dambreakAveraging(1, n) / dambreakAveraging(2, n)
               end if
            end do

            !Compute dambreak widths
            do n = 1, ndambreaksignals
               istru = dambreaks(n)
               if (istru /= 0) then
                  if (network%sts%struct(istru)%dambreak%algorithm == BREACH_GROWTH_VDKNAAP .or. network%sts%struct(istru)%dambreak%algorithm == BREACH_GROWTH_VERHEIJVDKNAAP) then
                     ! Compute the breach width
                     call prepareComputeDambreak(network%sts%struct(istru)%dambreak, waterLevelsDambreakUpStream(n), waterLevelsDambreakDownStream(n), normalVelocityDambreak(n), startTime, deltaTime)
                  end if
                  if (network%sts%struct(istru)%dambreak%algorithm == BREACH_GROWTH_TIMESERIES .and. startTime > network%sts%struct(istru)%dambreak%t0) then
                     !Time in the tim file is relative to the start time
                     success = ec_gettimespacevalue_by_itemID(ecInstancePtr, item_dambreakLevelsAndWidthsFromTable, irefdate, tzone, tunit, startTime - network%sts%struct(istru)%dambreak%t0)
                     ! NOTE: AvD: the code above works correctly, but is dangerous:
                     ! the addtimespace for dambreak has added each dambreak separately with a targetoffset.
                     ! The gettimespace above, however, gets the values for *all* dambreaks, but with the relative time
                     ! of the *current* dambreak #n.
                     ! This means that if t0 values for all dambreaks are different, then the dambreakLevelsAndWidthsFromTable(1:n-1) have become obsolete now.
                     ! It works, because in the previous loop iterations the values that were then still correct
                     ! have already been set into the %crl and %width values.
                     if (success) then
                        indexLevelsAndWidths = (n - 1) * 2 + 1
                        network%sts%struct(istru)%dambreak%crl = dambreakLevelsAndWidthsFromTable(indexLevelsAndWidths)
                        network%sts%struct(istru)%dambreak%width = dambreakLevelsAndWidthsFromTable(indexLevelsAndWidths + 1)
                     else
                        return
                     end if
                  end if
                  ! Store breach width derivatives
                  tempValue = network%sts%struct(istru)%dambreak%breachWidthDerivative
                  if (tempValue > 0) then
                     breachWidthDerivativeDambreak(n) = tempValue
                  else
                     breachWidthDerivativeDambreak(n) = &
                        (network%sts%struct(istru)%dambreak%width - breachWidthDambreak(n)) / deltaTime
                  end if

                  ! Store the current dambreak width
                  breachWidthDambreak(n) = network%sts%struct(istru)%dambreak%width
                  ! Store the current dambreak crest level
                  breachDepthDambreak(n) = network%sts%struct(istru)%dambreak%crl

                  ! Store water level jump
                  tempValue = network%sts%struct(istru)%dambreak%waterLevelJumpDambreak
                  if (tempValue > 0) then
                     ! Algo 1 or 2: from prepareComputeDambreak
                     waterLevelJumpDambreak(n) = tempValue
                  else
                     ! Algo 3 (timeseries), compute here:
                     smax = max(waterLevelsDambreakUpStream(n), waterLevelsDambreakDownStream(n))
                     smin = min(waterLevelsDambreakUpStream(n), waterLevelsDambreakDownStream(n))
                     hmx = max(0d0, smax - network%sts%struct(istru)%dambreak%crl)
                     hmn = max(0d0, smin - network%sts%struct(istru)%dambreak%crl)
                     waterLevelJumpDambreak(n) = hmx - hmn
                  end if
               end if
            end do
         end if
      end if
   end subroutine update_dambreak_breach

end module m_update_dambreak_breach
