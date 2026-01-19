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
      real(kind=dp), dimension (:), allocatable :: min_inf_cap !< [mm/hr] Minimum infiltration capacity in Horton's equation {"location": "face", "shape": ["ndx"]}
      real(kind=dp), dimension (:), allocatable :: max_inf_cap !< [mm/hr] Maximum infiltration capacity in Horton's equation {"location": "face", "shape": ["ndx"]}
      real(kind=dp), dimension (:), allocatable :: decrease_rate !< [1/hr]  Decrease rate in Horton's equation {"location": "face", "shape": ["ndx"]}
      real(kind=dp), dimension (:), allocatable :: recovery_rate !< [1/hr]  Recovery rate in Horton's equation {"location": "face", "shape": ["ndx"]}
   end type t_HortonInfiltrationConfig

   contains
   
      !> Computes infiltration capacity as defined by Horton equations.
      !!
      !! Infiltration capacity defined in m/s, decrease and recovery rate in 1/hr.
      !! Typical timestep used in application is 1 minute (i.e. much smaller than 1 hour),
      !! otherwise computation of infiltration volume (in mm) should be more refined
      !! (using integral of capacity function, depending on state recovery or decrease).
      function compute_horton_infiltration(config, n, include_rain, timestep, inf_cap, waterlevel, rainfall, inf_cap_state, infiltration_mm) result(ierr)

         type(t_HortonInfiltrationConfig), intent(in) :: config !< Horton infiltration configuration containing min/max infiltration capacity and decrease/recovery rates
         integer, intent(in) :: n !< Number of grid cells
         integer, intent(in) :: include_rain !< Indicates whether or not (1/0) rainfall array is available
         real(kind=dp), intent(in) :: timestep !< [s] Timestep size
         real(kind=dp), dimension(:), intent(inout) :: inf_cap !< [m/s] Infiltration capacity
         real(kind=dp), dimension(:), intent(in) :: waterlevel !< [m] Waterlevel in current timestep
         real(kind=dp), dimension(:), intent(in) :: rainfall !< [mm/day] Rainfall in current timestep
         integer, dimension(:), intent(inout) :: inf_cap_state !< Infiltration capacity state; (one of HORTON_CAPSTAT_(NOCHANGE|RECOVERY|INCREASE))
         real(kind=dp), optional, intent(out) :: infiltration_mm(:) !< [mm] Infiltration amount
         integer :: ierr !< Result status, DHYD_NOERR if successful.
         
         ! local
         real(kind=dp), parameter :: SECOND_TO_HOUR = 1.0_dp / 3600.0_dp !< Number of seconds per hour
         real(kind=dp), parameter :: METER_TO_MILLIMETER = 1000.0_dp !< Conversion factor from meter to millimeter
         integer, parameter :: DAY_TO_HOUR = 24 !< Number of hours per day
         real(kind=dp), parameter :: MPS_TO_MMPHR = METER_TO_MILLIMETER / SECOND_TO_HOUR !< Conversion factor from m/s to mm/hr
         integer :: i
         real(kind=dp) :: timestep_hr
         real(kind=dp), dimension(:), allocatable :: rainfall_local !< Local rainfall array in mm/hr
         
         ! Set error status to no error and do unit conversions
         ierr = DHYD_NOERR
         timestep_hr = timestep * SECOND_TO_HOUR ! Convert timestep to hours
         inf_cap = inf_cap * MPS_TO_MMPHR ! Convert infiltration capacity to mm/hr
         rainfall_local = rainfall / DAY_TO_HOUR ! Convert rainfall to mm/hr

         do i = 1, n

            if (config%max_inf_cap(i) <= config%min_inf_cap(i)) then
               
               ! No valid band width between min and max infiltration capacity
               inf_cap_state(i) = HORTON_CAPSTAT_NOCHANGE

            else if ((include_rain == 1 .and. (rainfall_local(i) > config%min_inf_cap(i))) .or. comparereal(waterlevel(i), 0.0_dp) == 1) then
               
               ! Wet situation, infiltration capacity is decreasing
               inf_cap_state(i) = HORTON_CAPSTAT_DECREASE
               inf_cap(i) = config%min_inf_cap(i) + (inf_cap(i) - config%min_inf_cap(i)) * exp(-1d0 * config%decrease_rate(i) * timestep_hr)

            else

               ! Dry situation, infiltration capacity is recovering
               inf_cap_state(i) = HORTON_CAPSTAT_RECOVERY
               inf_cap(i) = config%max_inf_cap(i) - (config%max_inf_cap(i) - inf_cap(i)) * exp(-1d0 * config%recovery_rate(i) * timestep_hr)

            end if
         end do

         inf_cap = inf_cap / MPS_TO_MMPHR ! Convert back to m/s

         if (present(infiltration_mm)) then
            infiltration_mm = inf_cap * timestep * METER_TO_MILLIMETER ! m/s * s -> m -> mm
         end if

      end function compute_horton_infiltration

end module m_horton
