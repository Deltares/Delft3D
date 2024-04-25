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
   use m_GlobalParameters
   use m_CrossSections
   use m_Roughness

   implicit none

   public ComputeBridge

   type, public :: t_bridge
      double precision              :: bedLevel             !< bedlevel of the standard bridge
      double precision              :: bedLevel_actual      !< used bedlevel of the bridge
      double precision              :: flowArea             !< flow area as defined in the cross section of the standard bridge
      double precision              :: flowArea_actual      !< used flow area of the bridge
      double precision              :: pillarwidth          !< pillar width
      double precision              :: formfactor          
      integer                       :: allowedflowdir       !< 0 all directions
                                                            !< 1 only positive flow
                                                            !< 2 only negative flow
                                                            !< 3 no flow allowed
      logical                       :: useOwnCrossSection 
      type(t_crosssection), pointer :: pcross => null()     
      integer                       :: crosssectionnr     
      integer                       :: bedFrictionType    
      double precision              :: bedFriction        
      double precision              :: length             
      double precision              :: inletlosscoeff     
      double precision              :: outletlosscoeff    
   end type

   private

contains

   subroutine ComputeBridge(bridge, fum, rum, aum, dadsm, kfum, s1m1, s1m2, u1m,              &
                            dxm, dt, as1, as2, bob, changeStructureDimensions)
      implicit none
      !
      ! Global variables
      !
      type(t_bridge), pointer, intent(in    )   :: bridge    !< Object, containing bridge specific data
      double precision,        intent(  out)    :: fum       !< FU
      double precision,        intent(  out)    :: rum       !< RU
      double precision,        intent(  out)    :: aum       !< Flow area
      double precision,        intent(inout)    :: dadsm     !< Flow width
      integer         ,        intent(inout)    :: kfum      !< Drying flooding flag
      double precision,        intent(in   )    :: s1m1      !< Waterlevel at left side of culvert
      double precision,        intent(in   )    :: s1m2      !< Waterlevel at right side of culvert
      double precision,        intent(in   )    :: u1m       !< Flow velocity
      double precision,        intent(in   )    :: dxm       !< Delta x
      double precision,        intent(in   )    :: dt        !< Time step
      double precision,        intent(in   )    :: as1       !< Left flow area 
      double precision,        intent(in   )    :: as2       !< Right flow area 
      double precision,        intent(in   )    :: bob(2)    !< BOB's at left and right of the bridge
      logical,                 intent(in   )    :: changeStructureDimensions !< Indicates whether the crest level and the flow area of the bridge
                                                                             !< can be changed.
      !
      !
      ! Local variables
      !
      integer                                   :: dir
      integer                                   :: allowedFlowDir
      
      double precision                          :: cmus      
      double precision                          :: bobup      
      double precision                          :: wetup      
      double precision                          :: wetdown      
      double precision                          :: smax
      double precision                          :: smin
      double precision                          :: gl_thickness
      double precision                          :: crestLevel
      double precision                          :: depth
      double precision                          :: chezyBridge
      double precision                          :: wPerimeter
      double precision                          :: hydrRadius
      double precision                          :: frictloss
      double precision                          :: exitLoss
      double precision                          :: totalLoss
      double precision                          :: pillarLoss
      double precision                          :: cu
      double precision                          :: fr
      double precision                          :: bu
      double precision                          :: du

      ! Initializing at declaration is not enough....
      cmus         = 1.0d0
      gl_thickness = 0.0d0
      chezyBridge  = 0.0d0
      wPerimeter   = 0.0d0
      hydrRadius   = 0.0d0
      frictloss    = 0.0d0
      exitLoss     = 0.0d0
      pillarLoss   = 0.0d0
      totalLoss    = 0.0d0
      cu           = 0.0d0
      fr           = 0.0d0
      bu           = 0.0d0
      du           = 0.0d0
      bridge%bedLevel_actual = bridge%bedLevel

      ! Initialize with flow
      kfum = 1
      
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
      if ((allowedFlowDir == 3) .or. &
          (dir == 1  .and. allowedFlowDir == 2) .or. &
          (dir == -1 .and. allowedFlowDir == 1)) then
         kfum = 0
         fum = 0.0d0
         rum = 0.0d0
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
         chezyBridge = getchezy(bridge%pcross%frictionTypePos(1), bridge%pcross%frictionValuePos(1), aum/wPerimeter, depth, 1d0)
         frictLoss = 2.0d0 * gravity * bridge%length / (chezyBridge * chezyBridge * hydrRadius)

         ! Exit Loss
         exitLoss = bridge%outletlosscoeff * ((max((1.0d0 - aum / wetdown), 0.0d0))**2)
         exitLoss = max(exitLoss, 0.0d0)
      endif

      if (bridge%pillarwidth > 1.0d-5) then
      
         ! pilllar bridge definition

         dadsm = dadsm - bridge%pillarwidth   !hk: Only true if pillar length equals link length
         if (dadsm <= 0.0d0) then
            kfum = 0
         endif
         
         pillarLoss = bridge%formfactor * (bridge%pillarwidth * depth) / aum
         aum = aum - bridge%pillarwidth * depth
         if (aum <= 0.0d0) kfum = 0
         
         if (kfum == 0) then
            fum = 0.0
            rum = 0.0
            return
         endif
         
      endif

      totalLoss = bridge%inletlosscoeff + frictLoss + exitLoss + pillarloss
      totalLoss = max(totalLoss, 0.01d0)
      
      cmus = 1.0d0 / sqrt(totalLoss)
      cmus = min(cmus, 1.0d0)    ! Limit to maximum of 1.0

      cu = cmus * cmus * 2  *gravity / dxm
      fr = abs(u1m) / dxm
      bu = 1.0d0 / dt + fr
      du = u1m / dt
      fum = cu / bu
      rum = du / bu
      
   end subroutine ComputeBridge
      
end module m_Bridge
