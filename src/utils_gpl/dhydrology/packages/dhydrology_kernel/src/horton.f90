!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2026.                                
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

module m_horton

   use dhydrology_error
   use precision_basics

   implicit none	

   private
   
   public :: HORTON_CAPSTAT_NOCHANGE
   public :: HORTON_CAPSTAT_DECREASE
   public :: HORTON_CAPSTAT_RECOVERY
   public :: t_HortonInfiltrationConfig
   public :: compute_horton_infiltration

   ! Horton infiltration capacity states
   integer, parameter :: HORTON_CAPSTAT_NOCHANGE = 0 !< No change in infiltration state
   integer, parameter :: HORTON_CAPSTAT_DECREASE = 1 !< Infiltration in decreasing mode
   integer, parameter :: HORTON_CAPSTAT_RECOVERY = 2 !< Infiltration in recovery/increasing mode
   
   type :: t_HortonInfiltrationConfig
      real(kind=dp), allocatable :: minInfCap(:) !< [mm/hr] Minimum infiltration capacity in Horton's equation {"location": "face", "shape": ["ndx"]}
      real(kind=dp), allocatable :: maxInfCap(:) !< [mm/hr] Maximum infiltration capacity in Horton's equation {"location": "face", "shape": ["ndx"]}
      real(kind=dp), allocatable :: decreaseRate(:) !< [1/hr]  Decrease rate in Horton's equation {"location": "face", "shape": ["ndx"]}
      real(kind=dp), allocatable :: recoveryRate(:) !< [1/hr]  Recovery rate in Horton's equation {"location": "face", "shape": ["ndx"]}
   end type t_HortonInfiltrationConfig

   contains 
   
      !> Computes infiltration capacity as defined by Horton equations.
      !!
      !! Infiltration capacity defined in m/s, decrease and recovery rate in 1/hr.
      !! Typical timestep used in application is 1 minute (i.e. much smaller than 1 hour),
      !! otherwise computation of infiltration volume (in mm) should be more refined
      !! (using integral of capacity function, depending on state recovery or decrease).
      function compute_horton_infiltration(n, config, infiltration_capacity, timestep_size, initial_storage, rainfall, include_rain, &
                                        infiltration_capacity_state, infiltration_mm) result(ierr)
         
         integer, intent(in) :: n !< Array length (grid cell count)
         type(t_HortonInfiltrationConfig), intent(in) :: config !< Horton infiltration configuration containing min/max infiltration capacity and decrease/recovery rates
         real(kind=dp), intent(out) :: infiltration_capacity(n) !< Infiltration capacity (m/s)
         real(kind=dp), intent(in) :: timestep_size !< Timestep size (s)
         real(kind=dp), intent(in) :: initial_storage(n) !< Initial storage (=storage at start of timestep) (m)
         real(kind=dp), intent(in) :: rainfall(:) !< Rainfall in current timestep (or more precise: additional ground rainfall, so minus interception) (mm/day)
         integer, intent(in) :: include_rain !< Indicates whether or not (1/0) array Rainfall is available, otherwise no rainfall is assumed
         integer, intent(out) :: infiltration_capacity_state(n) !< Infiltration capacity state; (one of HORTON_CAPSTAT_(NOCHANGE|RECOVERY|INCREASE))
         real(kind=dp), optional, intent(out) :: infiltration_mm(n) !< Infiltration amount (mm)
         integer :: ierr !< Result status, DHYD_NOERR if successful.
         
         ! local
         integer, parameter :: SECONDS_PER_HOUR = 3600 !< Number of seconds per hour
         integer, parameter :: HOURS_PER_DAY = 24 !< Number of hours per day
         integer, parameter :: MPS_TO_MMPHR = SECONDS_PER_HOUR * 1000 !< Conversion factor from m/s to mm/hr
         real(kind=dp)      :: timestep_size_hr
         integer            :: i
         
         ! Set error status to no error and do unit conversions
         ierr = DHYD_NOERR
         timestep_size_hr = timestep_size / SECONDS_PER_HOUR ! Fraction of hour represented by timestep
         infiltration_capacity = infiltration_capacity * MPS_TO_MMPHR ! Convert to mm/hr for computation

         do i = 1, n

            if (config%maxInfCap(i) <= config%minInfCap(i)) then
               
               ! No valid band width between min and max infiltration capacity
               infiltration_capacity_state(i) = HORTON_CAPSTAT_NOCHANGE

            else if ((include_rain == 1 .and. (rainfall(i) / HOURS_PER_DAY > config%minInfCap(i))) .or. comparereal(initial_storage(i), 0.0_dp) == 1) then
               
               ! Wet situation, infiltration capacity is decreasing
               infiltration_capacity_state(i) = HORTON_CAPSTAT_DECREASE
               infiltration_capacity(i) = config%minInfCap(i) + (infiltration_capacity(i) - config%minInfCap(i)) * exp(-1d0 * config%decreaseRate(i) * timestep_size_hr)

            else

               ! Dry situation, infiltration capacity is recovering
               infiltration_capacity_state(i) = HORTON_CAPSTAT_RECOVERY
               infiltration_capacity(i) = config%maxInfCap(i) - (config%maxInfCap(i) - infiltration_capacity(i)) * exp(-1d0 * config%recoveryRate(i) * timestep_size_hr)

            end if
         end do

         infiltration_capacity = infiltration_capacity / MPS_TO_MMPHR ! Convert back to m/s

         if (present(infiltration_mm)) then
            infiltration_mm = infiltration_capacity * timestep_size * 1e-3_dp ! m/s * s -> m -> mm
         end if

      end function compute_horton_infiltration

end module m_horton
