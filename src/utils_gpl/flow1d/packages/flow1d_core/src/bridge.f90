module m_Bridge
!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
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
!-------------------------------------------------------------------------------
   
   ! Modules
   use precision_basics, only: dp
   use m_GlobalParameters
   use m_CrossSections
   use m_Roughness

   implicit none

   public ComputeBridge

   type, public :: t_bridge
      real(kind=dp)              :: bedLevel             !< bedlevel of the standard bridge
      real(kind=dp)              :: bedLevel_actual      !< used bedlevel of the bridge
      real(kind=dp)              :: flowArea             !< flow area as defined in the cross section of the standard bridge
      real(kind=dp)              :: flowArea_actual      !< used flow area of the bridge
      real(kind=dp)              :: pillarwidth          !< pillar width
      real(kind=dp)              :: formfactor          
      integer                       :: allowedflowdir       !< 0 all directions
                                                            !< 1 only positive flow
                                                            !< 2 only negative flow
                                                            !< 3 no flow allowed
      logical                       :: useOwnCrossSection 
      type(t_crosssection), pointer :: pcross => null()     
      integer                       :: crosssectionnr     
      integer                       :: bedFrictionType    
      real(kind=dp)              :: bedFriction        
      real(kind=dp)              :: length             
      real(kind=dp)              :: inletlosscoeff     
      real(kind=dp)              :: outletlosscoeff    
   end type

   private

contains

   subroutine ComputeBridge(bridge, fum, rum, aum, dadsm, s1m1, s1m2, u1m,              &
                            dxm, dt, as1, as2, bob, changeStructureDimensions)
      implicit none
      !
      ! Global variables
      !
      type(t_bridge), pointer, intent(in    )   :: bridge    !< Object, containing bridge specific data
      real(kind=dp),        intent(  out)    :: fum       !< FU
      real(kind=dp),        intent(  out)    :: rum       !< RU
      real(kind=dp),        intent(  out)    :: aum       !< Flow area
      real(kind=dp),        intent(inout)    :: dadsm     !< Flow width
      real(kind=dp),        intent(in   )    :: s1m1      !< Waterlevel at left side of culvert
      real(kind=dp),        intent(in   )    :: s1m2      !< Waterlevel at right side of culvert
      real(kind=dp),        intent(in   )    :: u1m       !< Flow velocity
      real(kind=dp),        intent(in   )    :: dxm       !< Delta x
      real(kind=dp),        intent(in   )    :: dt        !< Time step
      real(kind=dp),        intent(in   )    :: as1       !< Left flow area 
      real(kind=dp),        intent(in   )    :: as2       !< Right flow area 
      real(kind=dp),        intent(in   )    :: bob(2)    !< BOB's at left and right of the bridge
      logical,                 intent(in   )    :: changeStructureDimensions !< Indicates whether the crest level and the flow area of the bridge
                                                                             !< can be changed.
      !
      !
      ! Local variables
      !
      integer                                   :: dir
      integer                                   :: allowedFlowDir
      
      real(kind=dp)                          :: cmus      
      real(kind=dp)                          :: bobup      
      real(kind=dp)                          :: wetup      
      real(kind=dp)                          :: wetdown      
      real(kind=dp)                          :: smax
      real(kind=dp)                          :: smin
      real(kind=dp)                          :: gl_thickness
      real(kind=dp)                          :: crestLevel
      real(kind=dp)                          :: depth
      real(kind=dp)                          :: chezyBridge
      real(kind=dp)                          :: wPerimeter
      real(kind=dp)                          :: hydrRadius
      real(kind=dp)                          :: frictloss
      real(kind=dp)                          :: exitLoss
      real(kind=dp)                          :: totalLoss
      real(kind=dp)                          :: pillarLoss
      real(kind=dp)                          :: cu
      real(kind=dp)                          :: fr
      real(kind=dp)                          :: bu
      real(kind=dp)                          :: du

      ! Initializing at declaration is not enough....
      cmus         = 1.0_dp
      gl_thickness = 0.0_dp
      chezyBridge  = 0.0_dp
      wPerimeter   = 0.0_dp
      hydrRadius   = 0.0_dp
      frictloss    = 0.0_dp
      exitLoss     = 0.0_dp
      pillarLoss   = 0.0_dp
      totalLoss    = 0.0_dp
      cu           = 0.0_dp
      fr           = 0.0_dp
      bu           = 0.0_dp
      du           = 0.0_dp
      bridge%bedLevel_actual = bridge%bedLevel

      ! Initialize with flow
      
      ! Find the flow direction
      if (s1m1 > s1m2) then
         smax    = s1m1
         smin    = s1m2
         wetup   = as1
         wetdown = as2
         bobup   = bob(1)
         dir  = 1
      else
         smax    = s1m2
         smin    = s1m1
         wetup   = as2
         wetdown = as1
         bobup   = bob(2)
         dir  = -1
      endif

      allowedFlowDir = bridge%allowedflowdir
      if ((smax <=bobup) .or. (allowedFlowDir == 3) .or. &
          (dir == 1  .and. allowedFlowDir == 2) .or. &
          (dir == -1 .and. allowedFlowDir == 1)) then
         fum = 0.0_dp
         rum = 0.0_dp
         return
      endif
      
      aum   = wetup
      depth = smax - bobup 
      
      if (bridge%useOwnCrossSection) then
      
         ! abutment bridge definition
         
         gl_thickness = getGroundLayer(bridge%pcross)
      
         if (changeStructureDimensions) then
            crestLevel = max(bob(1), bob(2), bridge%bedlevel)
         else
            crestLevel = bridge%bedlevel
         endif
         bridge%bedLevel_actual = crestLevel

         depth = smax - crestLevel
         if (depth <= 0.0_dp) then
            fum = 0.0_dp
            rum = 0.0_dp
            return
         end if
          
         call GetCSParsFlow(bridge%pcross, depth, aum, wPerimeter, dadsm)   
         if (bridge%pcross%closed .and. smax > getHighest1dLevel(bridge%pcross)) then
            depth = getHighest1dLevel(bridge%pcross) - crestLevel
         endif

         bridge%flowArea = aum
         
         ! in case the flow area is limited by the upstream flow area, the hydraulic radius
         ! is still based on the cross section of the bridge
         hydrRadius = aum / wPerimeter
         
         ! Limit the flow area to the upstream flow area
         if (changeStructureDimensions) then
            aum = min(aum, wetup)
         endif

         bridge%flowArea_actual = aum
         

         ! Friction Loss
         chezyBridge = getchezy(bridge%pcross%frictionTypePos(1), bridge%pcross%frictionValuePos(1), aum/wPerimeter, depth, 1.0_dp)
         frictLoss = 2.0_dp * gravity * bridge%length / (chezyBridge * chezyBridge * hydrRadius)

         ! Exit Loss
         exitLoss = bridge%outletlosscoeff * ((max((1.0_dp - aum / wetdown), 0.0_dp))**2)
         exitLoss = max(exitLoss, 0.0_dp)
      endif

      if (bridge%pillarwidth > 1.0e-5_dp) then
      
         ! pilllar bridge definition

         dadsm = dadsm - bridge%pillarwidth   !hk: Only true if pillar length equals link length
         if (dadsm <= 0.0_dp) then
            fum = 0.0
            rum = 0.0
            return
         endif
         
         pillarLoss = bridge%formfactor * (bridge%pillarwidth * depth) / aum
         aum = aum - bridge%pillarwidth * depth
         if (aum <= 0.0_dp) then
            fum = 0.0
            rum = 0.0
            return
         endif
         
      endif

      totalLoss = bridge%inletlosscoeff + frictLoss + exitLoss + pillarloss
      totalLoss = max(totalLoss, 0.01_dp)
      
      cmus = 1.0_dp / sqrt(totalLoss)
      cmus = min(cmus, 1.0_dp)    ! Limit to maximum of 1.0

      cu = cmus * cmus * 2  *gravity / dxm
      fr = abs(u1m) / dxm
      bu = 1.0_dp / dt + fr
      du = u1m / dt
      fum = cu / bu
      rum = du / bu
      
   end subroutine ComputeBridge
      
end module m_Bridge
