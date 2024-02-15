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

subroutine update_dambreak_breach(startTime, deltaTime)

   use m_flowgeom
   use m_flow
   use m_missing
   use m_structures
   use unstruc_channel_flow
   use m_Dambreak
   use m_partitioninfo
   use m_meteo
   use m_flowexternalforcings
   use m_flowtimes

   implicit none

   !in-out
   double precision, intent(in)          :: startTime
   double precision, intent(in)          :: deltaTime

   !locals
   double precision                      :: tempValue, smax, smin, hmx, hmn
   integer                               :: indAverageUpStream(ndambreak)
   integer                               :: indAverageDownStream(ndambreak)
   integer                               :: nAverageUpStream, nAverageDownStream
   integer                               :: n, ierr, istru, indexLevelsAndWidths

   if (ndambreaksg > 0) then ! Variable ndambreaksg is >0 for all partitions if there is a dambreak, even if it is outside
      ! of a partition. In a parallel simulation, we need to run this subroutine even in a special situation that there is
      ! no dambreak on the current subdomain (i.e. ndambreak == 0), because the following function getAverageQuantityFromLinks
      ! involves mpi communication among all subdomains. However, in this special situation,
      ! all the necessary variables are set to 0 and do not participate the dambreak related computation in this subroutine.

      !
      ! Initialize
      !
      dambreakAveraging              = 0.0d0
      waterLevelsDambreakUpStream    = 0.0d0
      waterLevelsDambreakDownStream  = 0.0d0
      normalVelocityDambreak         = 0.0d0
      breachWidthDerivativeDambreak  = 0.0d0
      waterLevelJumpDambreak         = 0.0d0

      !
      ! Upstream water level
      !
      if (nDambreakLocationsUpstream > 0) then
         waterLevelsDambreakUpStream(dambreakLocationsUpstreamMapping(1:nDambreakLocationsUpstream)) = s1(dambreakLocationsUpstream(1:nDambreakLocationsUpstream))
      endif

      !call this code only if something has to be averaged
      if (nDambreakAveragingUpstream > 0) then

         ! Compute sumQuantitiesByWeight upstream
         ierr = getAverageQuantityFromLinks(L1dambreaksg(dambreakAverigingUpstreamMapping(1:nDambreakAveragingUpstream)), L2dambreaksg(dambreakAverigingUpstreamMapping(1:nDambreakAveragingUpstream)), wu, kdambreak(3,:), s1, kdambreak(1,:), dambreakAveraging, 0, &
                                            hu, dmiss, activeDambreakLinks, 0)

         if (ierr.ne.0) then
            success=.false.
            return
         endif

         if (ndambreak > 0) then
            do n = 1, nDambreakAveragingUpstream
               if (dambreakAveraging(2,n)>0.0d0) then
                  waterLevelsDambreakUpStream(dambreakAverigingUpstreamMapping(n))  = dambreakAveraging(1,n)/dambreakAveraging(2,n)
               else if (abs(startTime-network%sts%struct(dambreaks(dambreakAverigingUpstreamMapping(n)))%dambreak%T0)<1d-10) then
                  waterLevelsDambreakUpStream(dambreakAverigingUpstreamMapping(n)) = s1(kdambreak(1,LStartBreach(dambreakAverigingUpstreamMapping(n))))
               else
                  continue
               endif
            enddo
         end if
      endif

      !
      ! Downstream water level
      !
      if (nDambreakLocationsDownstream > 0) then
         waterLevelsDambreakDownStream(dambreakLocationsDownstreamMapping(1:nDambreakLocationsDownstream)) = s1(dambreakLocationsDownstream(1:nDambreakLocationsDownstream))
      endif


      !call this code only if something has to be averaged downstream
      if (nDambreakAveragingDownstream > 0) then

         ! Compute sumQuantitiesByWeight downstream
         ierr = getAverageQuantityFromLinks(L1dambreaksg(dambreakAverigingDownstreamMapping(1:nDambreakAveragingDownstream)), L2dambreaksg(dambreakAverigingDownstreamMapping(1:nDambreakAveragingDownstream)), wu, kdambreak(3,:), s1, kdambreak(2,:), dambreakAveraging, 0, &
                                            hu, dmiss, activeDambreakLinks, 0)

         if (ierr.ne.0) then
            success=.false.
            return
         endif

         if (ndambreak > 0) then
            do n = 1, nDambreakAveragingDownstream
               if (dambreakAveraging(2,n)>0.0d0) then
                  waterLevelsDambreakDownStream(dambreakAverigingDownstreamMapping(n))  = dambreakAveraging(1,n)/dambreakAveraging(2,n)
               else if (abs(startTime-network%sts%struct(dambreaks(dambreakAverigingDownstreamMapping(n)))%dambreak%T0)<1d-10) then
                  waterLevelsDambreakDownStream(dambreakAverigingDownstreamMapping(n)) = s1(kdambreak(2,LStartBreach(dambreakAverigingDownstreamMapping(n))))
               else
                  continue
               endif
            enddo
         end if
      endif

      !
      ! u0 velocity on the flowlinks (averaged by the wetted area). The mask is the water level itself
      !
      ierr = getAverageQuantityFromLinks(L1dambreaksg, L2dambreaksg, au, kdambreak(3,:), u1, kdambreak(3,:), dambreakAveraging, 1, &
                                        hu, dmiss, activeDambreakLinks, 0)
      if (ierr.ne.0) success=.false.

      if (ndambreak > 0) then
         do n = 1, ndambreaksg
            if (dambreakAveraging(2,n)>0.0d0) then
               normalVelocityDambreak(n)  = dambreakAveraging(1,n)/dambreakAveraging(2,n)
            endif
         enddo

         !Compute dambreak widths
         do n = 1, ndambreaksg
            istru = dambreaks(n)
            if (istru.ne.0) then
               if(network%sts%struct(istru)%dambreak%algorithm == 1 .or. network%sts%struct(istru)%dambreak%algorithm == 2) then
                  ! Compute the breach width
                  call prepareComputeDambreak(network%sts%struct(istru)%dambreak, waterLevelsDambreakUpStream(n), waterLevelsDambreakDownStream(n), normalVelocityDambreak(n), startTime, deltaTime, maximumDambreakWidths(n))
               endif
               if(network%sts%struct(istru)%dambreak%algorithm == 3 .and. startTime > network%sts%struct(istru)%dambreak%t0) then
                  !Time in the tim file is relative to the start time
                  success = ec_gettimespacevalue_by_itemID(ecInstancePtr, item_dambreakLevelsAndWidthsFromTable, irefdate, tzone, tunit, startTime-network%sts%struct(istru)%dambreak%t0)
                  ! NOTE: AvD: the code above works correctly, but is dangerous:
                  ! the addtimespace for dambreak has added each dambreak separately with a targetoffset.
                  ! The gettimespace above, however, gets the values for *all* dambreaks, but with the relative time
                  ! of the *current* dambreak #n.
                  ! This means that if t0 values for all dambreaks are different, then the dambreakLevelsAndWidthsFromTable(1:n-1) have become obsolete now.
                  ! It works, because in the previous loop iterations the values that were then still correct
                  ! have already been set into the %crl and %width values.
                  if (success)  then
                     indexLevelsAndWidths = (n - 1) * 2 + 1
                     network%sts%struct(istru)%dambreak%crl   = dambreakLevelsAndWidthsFromTable(indexLevelsAndWidths)
                     network%sts%struct(istru)%dambreak%width = dambreakLevelsAndWidthsFromTable(indexLevelsAndWidths + 1 )
                  else
                      return
                  endif
               endif
               ! Store breach width derivatives
               tempValue = network%sts%struct(istru)%dambreak%breachWidthDerivative
               if (tempValue>0) then
                  breachWidthDerivativeDambreak(n) = tempValue
               else
                  breachWidthDerivativeDambreak(n) = &
                     (network%sts%struct(istru)%dambreak%width - breachWidthDambreak(n)) / deltaTime
               endif

               ! Store the current dambreak width
               breachWidthDambreak(n) = network%sts%struct(istru)%dambreak%width
               ! Store the current dambreak crest level
               breachDepthDambreak(n) = network%sts%struct(istru)%dambreak%crl

               ! Store water level jump
               tempValue = network%sts%struct(istru)%dambreak%waterLevelJumpDambreak
               if (tempValue>0) then
                  ! Algo 1 or 2: from prepareComputeDambreak
                  waterLevelJumpDambreak(n) = tempValue
               else
                  ! Algo 3 (timeseries), compute here:
                  smax = max(waterLevelsDambreakUpStream(n), waterLevelsDambreakDownStream(n))
                  smin = min(waterLevelsDambreakUpStream(n), waterLevelsDambreakDownStream(n))
                  hmx  = max(0d0,smax - network%sts%struct(istru)%dambreak%crl)
                  hmn  = max(0d0,smin - network%sts%struct(istru)%dambreak%crl)
                  waterLevelJumpDambreak(n) = hmx - hmn
               endif
            endif
         enddo
      end if
   endif
end subroutine update_dambreak_breach
