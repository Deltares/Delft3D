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

module m_horton

   use m_hydrology_data, only: t_HortonInfiltrationConfig
   use dhydrology_error
   use precision_basics

   implicit none	

   private
   
   public :: HORTON_CAPSTAT_NOCHANGE
   public :: HORTON_CAPSTAT_DECREASE
   public :: HORTON_CAPSTAT_RECOVERY
   public :: compute_horton_infiltration


   ! Horton infiltration capacity states
   integer, parameter :: HORTON_CAPSTAT_NOCHANGE = 0 !< No change in infiltration state
   integer, parameter :: HORTON_CAPSTAT_DECREASE = 1 !< Infiltration in decreasing mode
   integer, parameter :: HORTON_CAPSTAT_RECOVERY = 2 !< Infiltration in recovery/increasing mode
   
   contains 
   
      !> Computes infiltration capacity as defined by Horton equations.
      !!
      !! Infiltration capacity defined in m/s, decrease and recovery rate in 1/hr.
      !! Typical timestep used in application is 1 minute (i.e. much smaller than 1 hour),
      !! otherwise computation of infiltration volume (in mm) should be more refined
      !! (using integral of capacity function, depending on state recovery or decrease).
      function compute_horton_infiltration(n, config, infCap, &
                                           timestepSize, initialStorage, rainfall, includeRain, infCapState, infiltrationMM) result(ierr)
         
         
         integer,                 intent(in)  :: n                  !< Array length (grid cell count)
         type(t_HortonInfiltrationConfig), intent(in) :: config     !< Horton infiltration configuration containing min/max infiltration capacity and decrease/recovery rates
         real(kind=dp),           intent(out) :: infCap(n)          !< Infiltration capacity (m/s)
         real(kind=dp),           intent(in)  :: timestepSize       !< Timestep size (s)
         real(kind=dp),           intent(in)  :: initialStorage(n)  !< Initial storage (=storage at start of timestep) (m)
         real(kind=dp),           intent(in)  :: rainfall(:)        !< Rainfall in current timestep (or more precise: additional ground rainfall, so minus interception) (mm/day)
         integer,                 intent(in)  :: includeRain        !< Indicates whether or not (1/0) array Rainfall is available, otherwise no rainfall is assumed
         integer,                 intent(out) :: infCapState(n)     !< Infiltration capacity state; (one of HORTON_CAPSTAT_(NOCHANGE|RECOVERY|INCREASE))
         real(kind=dp), optional, intent(out) :: infiltrationMM(n)  !< Infiltration amount (mm)
         integer                              :: ierr               !< Result status, DHYD_NOERR if successful.
         
         ! local
         integer, parameter :: SECONDS_PER_HOUR = 3600 !< Number of seconds per hour
         integer, parameter :: HOURS_PER_DAY = 24 !< Number of hours per day
         integer, parameter :: MPS_TO_MMPHR = SECONDS_PER_HOUR * 1000 !< Conversion factor from m/s to mm/hr
         real(kind=dp)      :: timestepSizeHr
         integer            :: i
         
         ! Set error status to no error and do unit conversions
         ierr = DHYD_NOERR
         timestepSizeHr = timestepSize / SECONDS_PER_HOUR ! Fraction of hour represented by timestep
         infCap = infCap * MPS_TO_MMPHR ! Convert to mm/hr for computation

         do i = 1, n

            if (config%maxInfCap(i) <= config%minInfCap(i)) then
               
               ! No valid band width between min and max infiltration capacity
               infCapState(i) = HORTON_CAPSTAT_NOCHANGE

            else if ((includeRain == 1 .and. (rainfall(i) / HOURS_PER_DAY > config%minInfCap(i))) .or. comparereal(initialStorage(i), 0.0_dp) == 1) then
               
               ! Wet situation, infiltration capacity is decreasing
               infCapState(i) = HORTON_CAPSTAT_DECREASE
               infCap(i) = config%minInfCap(i) + (infCap(i) - config%minInfCap(i)) * exp(-1d0 * config%decreaseRate(i) * timestepSizeHr)

            else

               ! Dry situation, infiltration capacity is recovering
               infCapState(i) = HORTON_CAPSTAT_RECOVERY
               infCap(i) = config%maxInfCap(i) - (config%maxInfCap(i) - infCap(i)) * exp(-1d0 * config%recoveryRate(i) * timestepSizeHr)

            end if
         end do

         infCap = infCap / MPS_TO_MMPHR ! Convert back to m/s

         if (present(infiltrationMM)) then
            infiltrationMM = infCap * timestepSize * 1e-3_dp ! m/s * s -> m -> mm
         end if

      end function compute_horton_infiltration

end module m_horton
