module m_Pump
!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2025.                                
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

   use m_tables
   use MessageHandling

   implicit none

   public PrepareComputePump
   public dealloc

   interface dealloc
      module procedure deallocPump
   end interface dealloc

   type, public :: t_pump
      !> direction > 0: positive flow \n
      !! direction < 0: negative flow \n
      !! abs(direction): \n
      !! - 1: control only on suction side \n
      !! - 2: control only on delivery side \n
      !! - 3: control on suction side and on delivery side \n
      integer                                 :: direction
      integer                                 :: nrstages
      double precision, dimension(:), pointer :: capacity    => null()
      double precision, dimension(:), pointer :: ss_onlevel  => null()
      double precision, dimension(:), pointer :: ss_offlevel => null()
      double precision, dimension(:), pointer :: ds_onlevel  => null()
      double precision, dimension(:), pointer :: ds_offlevel => null()
      logical         , dimension(:), pointer :: ss_trigger  => null()
      logical         , dimension(:), pointer :: ds_trigger  => null()
      type(t_table), pointer                  :: reducfact   => null()

      ! Output Parameters for Pump History File
      ! Terminology:
      ! * Geometric orientation: handled by calling kernel, orientation of pump w.r.t. branch direction/polyline.
      ! * Pump orientation: orientation of capacity w.r.t. structure's geometric orientation.
      ! * Pumping direction: combined direction of pump orientation and sign of the capacity.
      double precision                        :: ss_level         !< Suction side water level w.r.t. pumping direction.
      double precision                        :: ds_level         !< Delivery side water level w.r.t. pumping direction.
      double precision                        :: pump_head        !< Head difference in pumping direction.
      integer                                 :: actual_stage
      logical                                 :: is_active
      double precision                        :: current_capacity !< Current capacity, w.r.t. pump orientation.
      double precision                        :: reduction_factor
      double precision                        :: discharge        !< Current discharge, w.r.t. structure's geometric orientation.

   end type

   private

contains

   subroutine deallocPump(pump)
      ! Modules

      implicit none
      ! Input/output parameters
      type(t_pump), pointer   :: pump

      ! Local variables

      ! Program code
      if (associated(pump)) then
         
         if (associated(pump%capacity))    deallocate(pump%capacity)
         if (associated(pump%ss_onlevel))  deallocate(pump%ss_onlevel)
         if (associated(pump%ss_offlevel)) deallocate(pump%ss_offlevel)
         if (associated(pump%ds_onlevel))  deallocate(pump%ds_onlevel)
         if (associated(pump%ds_offlevel)) deallocate(pump%ds_offlevel)
         if (associated(pump%ss_trigger))  deallocate(pump%ss_trigger)
         if (associated(pump%ds_trigger))  deallocate(pump%ds_trigger)
         call dealloc(pump%reducfact)
         
         pump%capacity    => null()
         pump%ss_onlevel  => null()
         pump%ss_offlevel => null()
         pump%ds_onlevel  => null()
         pump%ds_offlevel => null()
         pump%ss_trigger  => null()
         pump%ds_trigger  => null()
         pump%reducfact   => null()
         
         deallocate(pump)
         
         pump => null()
         
      endif
      
   end subroutine deallocPump

   !> The discharge through a pump with different capacities
   !! and possible control on suction and pressure side is calculated.
   !! Result is stored in pump%discharge, for later use in some computePump.
   !! Input and result are w.r.t. the *spatial* orientation of the pump,
   !! i.e., the caller does not have to account for the *pumping* direction,
   !! that is done by this subroutine.
   subroutine PrepareComputePump(pump, s1m1, s1m2)
      implicit none

      type(t_pump), pointer          :: pump     !< Object containing pump specific data.
      double precision, intent(in)   :: s1m1     !< 'Left side' water level, w.r.t. pump spatial orientation.
      double precision, intent(in)   :: s1m2     !< 'Right side' water level, w.r.t. pump spatial orientation.
      !
      !
      ! Local variables
      !
      integer                        :: istage
      integer                        :: nstages
      double precision               :: qp
      logical                        :: ss_switch_on
      logical                        :: ss_switch_off
      logical                        :: ds_switch_on
      logical                        :: ds_switch_off

      double precision               :: ss_level !< Suction Side level, w.r.t. pumping direction.
      double precision               :: ds_level !< Delivery Side Level, w.r.t. pump spatial orientation.

      ! Direction (==orientation) may be positive or negative,
      ! and additionally, non-staged pumps may have negative capacity.
      if (pump%direction * pump%capacity(1) > 0) then
         ss_level = s1m1
         ds_level = s1m2
      else
         ss_level = s1m2
         ds_level = s1m1
      end if

      nstages = pump%nrstages

      ! Get Reduction Factor from Table
      pump%pump_head = ds_level - ss_level
      pump%reduction_factor = interpolate(pump%reducfact, pump%pump_head)

      if (nstages == 0) then
         ! No stages for this pump. pump%capacity(1) is the pump capacity to be used
         pump%is_active = .true.
         qp = pump%reduction_factor * pump%capacity(1)
         pump%current_capacity = pump%capacity(1)
      else
         
         ! Check Suction Side Conditions
         ! * IF water level is ABOVE the on level of a stage, according to
         !   the suction side, the stage is turned on.
         ! * ELSEIF the water level is BELOW the off level of a stage, according to 
         !   the suction side, the stage is turned off
         ! * ELSE no action is taken. (on stays on, off stays off)

         if (abs(pump%direction) == 1 .or. abs(pump%direction) == 3) then
           do istage = 1, nstages
              ss_switch_on = (ss_level > pump%ss_onlevel(istage))
              ss_switch_off = (ss_level < pump%ss_offlevel(istage))

              if (ss_switch_on .and. .not. ss_switch_off) then
                 pump%ss_trigger(istage) = .true.
              elseif (.not. ss_switch_on .and. ss_switch_off) then
                 pump%ss_trigger(istage) = .false.
              else
                 ! Keep Old Value, So Do Nothing
              endif
           enddo
         endif

         ! Check Delivery Side Conditions
         ! * IF water level is BELOW the on level of a stage, according to
         !   the suction side, the stage is turned on.
         ! * ELSEIF the water level is ABOVE the off level of a stage, according to 
         !   the suction side, the stage is turned off
         ! * ELSE no action is taken. (on stays on, off stays off)

         if (abs(pump%direction) == 2 .or. abs(pump%direction) == 3) then
           do istage = 1, nstages
              ds_switch_on = (ds_level < pump%ds_onlevel(istage))
              ds_switch_off = (ds_level > pump%ds_offlevel(istage))

              if (ds_switch_on .and. .not. ds_switch_off) then
                 pump%ds_trigger(istage) = .true.
              elseif (.not. ds_switch_on .and. ds_switch_off) then
                 pump%ds_trigger(istage) = .false.
              else
                 ! Keep Old Value, So Do Nothing
              endif
           enddo
         endif

         ! Give non-controlled side all freedom
         if (abs(pump%direction) == 1) then
           pump%ds_trigger = .true.
         elseif (abs(pump%direction) == 2) then
           pump%ss_trigger = .true.
         endif

         ! Find the active stage 
         ! The highest stage, with both suction side and delivery side turned on results in a 
         ! actual stage.
         pump%actual_stage = 0
         do istage = nstages, 1, -1
           if (pump%ss_trigger(istage) .and. pump%ds_trigger(istage)) then
             pump%actual_stage = istage
             exit
           endif
         enddo

         ! Calculate Capacity
         if (pump%actual_stage == 0) then
           pump%is_active = .false.
           qp = 0d0
           pump%current_capacity = 0d0
         else
           pump%is_active = .true.
           qp = pump%reduction_factor * pump%capacity(pump%actual_stage)
           pump%current_capacity = pump%capacity(pump%actual_stage)
         endif

      endif
         
      pump%ss_level = ss_level
      pump%ds_level = ds_level

      ! Translate computed discharge (not the current_capacity) back to
      ! the caller's orientation, that is: *spatial* orientation of
      ! the pump
      if (pump%direction > 0) then
         pump%discharge = qp
      else
         pump%discharge = -qp
         !pump%current_capacity = -pump%current_capacity
      endif

   end subroutine PrepareComputePump

    
   !subroutine computePump(pump, fum, rum, um, qm, aum)
   !   ! modules
   !
   !   ! Global variables
   !   type(t_pump), pointer, intent(in)            :: pump
   !   double precision, intent(out)                :: fum
   !   double precision, intent(out)                :: rum
   !   double precision, intent(out)                :: um
   !   double precision, intent(out)                :: qm
   !   double precision, intent(in)                 :: aum
   !
   !   fum = 0.0
   !   rum = pump%discharge/max(aum, 1.0D-2)
   !   um = rum
   !   qm  = pump%discharge
   !end subroutine computePump

end module m_Pump
