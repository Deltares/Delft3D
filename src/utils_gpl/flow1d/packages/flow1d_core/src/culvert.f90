module m_Culvert
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

   use m_CrossSections
   use m_tables
   use MessageHandling
   use m_GlobalParameters

   implicit none

   private

   public ComputeCulvert
   public dealloc
   
   interface dealloc
      module procedure deallocCulvert
   end interface dealloc
   
   type, public :: t_culvert
      double precision                :: leftlevel             !< Left invert level of culvert
      double precision                :: rightlevel            !< Right invert level of culvert
      type(t_crosssection), pointer   :: pcross => null()      !< Pointer to cross section of culvert
      integer                         :: crosssectionnr        !< Cross section index in cross section array
      integer                         :: allowedflowdir        !< Allowed flow direction
                                                               !< 0 all directions
                                                               !< 1 only positive flow
                                                               !< 2 only negative flow
                                                               !< 3 no flow allowed
      double precision                :: length                !< Length of the culvert
      double precision                :: inletlosscoeff        !< Loss coefficient at inflow point
      double precision                :: outletlosscoeff       !< Loss coefficient at outflow point
      logical                         :: has_valve             !< Indicates whether a valve has been added
      double precision                :: valveOpening          !< Current valve opening
      type(t_table), pointer          :: losscoeff => null()   !< Table containing loss coefficients as a function of the relative opening
      integer                         :: state                 !< State of Culvert/Siphon
                                                               !< 0 = No Flow
                                                               !< 1 = Free Culvert Flow 
                                                               !< 2 = Submerged Culvert Flow 
      logical                        :: isInvertedSiphon       !< Indicates wether the culvert is of subtype inverted siphon.
      double precision               :: bendLossCoeff          !< Bend loss coefficient of siphon
   end type

contains

   !> deallocate culvert 
   subroutine deallocCulvert(culvert)
      ! Modules

      implicit none
      ! Input/output parameters
      type(t_culvert), pointer, intent(inout)   :: culvert     !< culvert object

      ! Local variables

      ! Program code
      if (associated(culvert) ) then
         if (associated(culvert%lossCoeff))           deallocate(culvert%lossCoeff)
         culvert%lossCoeff => null()
         deallocate(culvert)
      endif
      
      culvert => null()
      
   end subroutine deallocCulvert
                              
   !> 
   subroutine ComputeCulvert(culvert, fum, rum, aum, dadsm, kfum, cmustr, s1m1, s1m2, qm,  &
                             q0m, u1m, u0m, dxm, dt, wetdown)
      use m_Roughness

      implicit none
      !
      ! Global variables
      !
      type(t_culvert), pointer                     :: culvert
      integer,          intent(  out)              :: kfum
      double precision, intent(  out)              :: aum
      double precision, intent(  out)              :: dadsm
      double precision, intent(  out)              :: fum
      double precision, intent(inout)              :: q0m
      double precision, intent(inout)              :: qm
      double precision, intent(  out)              :: rum
      double precision, intent(  out)              :: cmustr
      double precision, intent(inout)              :: u0m
      double precision, intent(inout)              :: u1m
      double precision, intent(in   )              :: s1m1         !< left waterlevel s(m)          sleft
      double precision, intent(in   )              :: s1m2         !< right waterlevel s(m+1)       sright
      double precision, intent(in   )              :: dxm
      double precision, intent(in   )              :: dt
      double precision, intent(in   )              :: wetdown
         
      ! Local variables
      type(t_CrossSection)           :: CrossSection
      integer                        :: allowedflowdir
      integer                        :: dir

      double precision               :: smax             !< zeta_1 (upstream water level)
      double precision               :: smin             !< zeta_2 (downstream water level)
      logical                        :: isfreeflow
      double precision               :: bu
      double precision               :: cmus
      double precision               :: cu
      double precision               :: dc                  !< hc_2 critical depth
      double precision               :: culvertCrest
      double precision               :: inflowCrest         !< zc_1 (at upstream water level)
      double precision               :: outflowCrest        !< zc_2 (at downstream water level)
      double precision               :: du
      double precision               :: gl_thickness
      double precision               :: dpt                 !< upstream water depth
      double precision               :: openingfac
      double precision               :: valveOpening
      double precision               :: chezyCulvert
      double precision               :: chezyValve
      double precision               :: wArea               !< upstream wet area (no valve)
      double precision               :: wPerimiter          !< upstream wet perimeter  (no valve)
      double precision               :: wWidth              !< upstream wet surface width (no valve)
      double precision               :: valveArea           !< upstream wet area
      double precision               :: valvePerimiter      !< upstream wet perimeter
      double precision               :: valveWidth          !< upstream wet surface width
      double precision               :: hydrRadius
      double precision               :: culvertArea
      double precision               :: valveloss
      double precision               :: exitloss
      double precision               :: frictloss
      double precision               :: totalLoss
      double precision               :: dlim
      double precision               :: dxlocal
      double precision               :: eps10 = 1d-10

      ! Culvert Type
      
      ! Find the flow direction
      if (s1m1 > s1m2) then
         smax = s1m1
         smin = s1m2
         dir  = 1
         outflowCrest = culvert%rightlevel
         inflowCrest  = culvert%leftlevel
      else
         smax = s1m2
         smin = s1m1
         dir  = -1
         outflowCrest = culvert%leftlevel
         inflowCrest  = culvert%rightlevel
      endif
      culvertCrest = max(outflowCrest, inflowCrest) 

      ! Check on Flow Direction
      allowedFlowDir = culvert%allowedflowdir
      if ((allowedFlowDir == 3) .or. &
          (dir == 1  .and. allowedFlowDir == 2) .or. &
          (dir == -1 .and. allowedFlowDir == 1)) then
         kfum  = 0
         fum   = 0.0d0
         rum   = 0.0d0
         u1m   = 0.0d0
         u0m   = 0.0d0
         qm    = 0.0d0
         q0m   = 0.0d0
         culvert%state = 0
         return
      endif

      CrossSection = culvert%pcross
      gl_thickness = getGroundLayer(CrossSection)

      ! Check on Valve
      !     First find out the critical depth that can be used in free flow equations
      !     pjo, 13-04-2000, ars 4952, when flow direction changes, critical
      !     depth is taken as zero.
      if ( ( (dir == -1) .and. (qm > 0.0d0) ) .or.    &
           ( (dir == 1)  .and. (qm < 0.0d0) ) ) then
         dc = 0.0d0 
      else
         dc = GetCriticalDepth(qm, CrossSection)
      endif


      ! Calculate cross-section values in culvert
      dpt = smax - inflowCrest
      if (culvert%isInvertedSiphon) then
         ! When flowing, always assume that the (lower lying) inverted siphon is fully filled.
         dpt = max(CrossSection%charHeight, dpt)
      endif
      call GetCSParsFlow(CrossSection, dpt, wArea, wPerimiter, wWidth)

      if (abs(wArea) < eps10 .or. (culvert%has_valve .and. abs(culvert%valveOpening) < eps10)) then
         kfum  = 0
         fum   = 0.0d0
         rum   = 0.0d0
         u1m   = 0.0d0
         u0m   = 0.0d0
         qm    = 0.0d0
         q0m   = 0.0d0
         culvert%state = 0
         return
      endif
      chezyCulvert = getchezy(CrossSection%frictionTypePos(1), CrossSection%frictionValuePos(1), warea/wPerimiter, dpt, 1d0)
                  
      ! Valve Loss
      if (culvert%has_valve .and. (culvert%valveOpening < dpt)) then
         valveOpening = culvert%valveOpening
         chezyValve = 0.0d0
         call GetCSParsFlow(CrossSection, valveOpening, valveArea, valvePerimiter, valveWidth)     
         chezyCulvert = getchezy(CrossSection%frictionTypePos(1), CrossSection%frictionValuePos(1), valveArea/valvePerimiter, valveOpening, 1d0)
         openingfac = (valveOpening - gl_thickness) / (CrossSection%charHeight - gl_thickness)
      else
         openingfac = 2.0d0     ! >> 1, so not influenced by valve
      endif
      
      if (openingfac >= 1.0d0) then
         hydrRadius  = wArea / wPerimiter
         culvertArea  = wArea
         valveloss = 0.0d0
      else
         valveloss = interpolate(culvert%lossCoeff, openingfac)
         hydrRadius  = valveArea / (valvePerimiter + valveWidth)
         culvertArea  = valveArea
      endif
      
      ! Friction Loss
      frictloss = 2.0d0 * gravity * culvert%length / (chezyCulvert * chezyCulvert * hydrRadius)            ! culvert friction established
      
      ! Check if flow is free flow or submerged
      if (smin >= (outflowCrest + gl_thickness + dc)) then  

         ! Submerged Flow
         isfreeflow = .false.

         exitloss = culvert%outletlosscoeff * (max((1.0d0 - culvertArea / wetdown), 0.0d0))**2
         exitloss = max(exitloss, 0.0d0)

      else
      
         ! Free Flow
         isfreeflow = .true.
         exitloss = 0.0d0
         
      endif
      
      totalLoss = exitloss + frictloss + culvert%inletlosscoeff + valveloss + culvert%bendLossCoeff
            
      totalLoss = max(totalLoss, 0.01d0)
      
      cmus = 1.0d0 / sqrt(totalLoss)
      cmus = min(cmus, 1.0d0)    ! Limit to maximum of 1.0
 
      cmustr = cmus
      aum    = culvertArea
      dadsm  = wWidth

      if (isfreeflow) then
         
         dlim = dir * ( smin -dc-outflowcrest)
            
      else
         
         dlim = 0d0
            
      endif
         
      if (isfreeflow) then
         culvert%state = 1
      else
         culvert%state = 2
      endif

      dxlocal = max(culvert%length, dxm)
      bu = dxlocal/dt + abs(u1m)/(2d0*(cmus**2))
      cu = gravity
      du = dxlocal*u1m/dt 
      fum = cu / bu
      rum = (du + cu*dlim)/ bu 
    
   end subroutine ComputeCulvert

end module m_Culvert


