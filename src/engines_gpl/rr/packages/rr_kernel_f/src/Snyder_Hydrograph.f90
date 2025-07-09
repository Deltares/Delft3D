!----- AGPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
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

    module Snyder_hydrograph
   
   ! variables
   implicit none
   
   private
   
   type, public :: t_SHG
      real, dimension(7)                                    :: time_array
      real, dimension(7)                                    :: Q_array
   end type t_SHG
   
   type, public :: t_snyder_hydrograph_set
      integer                                               :: size= 0
      type(t_SHG), pointer, dimension(:)                    :: SHG
   end type t_snyder_hydrograph_set
   
   type(t_snyder_hydrograph_set) :: SHG_set
   
   public SHG_set, compute_snyder_hydrograph, interpolate_snyder_hydrograph, calculate_base_flow
      
   contains
   
   Subroutine compute_snyder_hydrograph(SHG, A_input, Cp, tp, tR_input)

   type(t_SHG),    intent(out) :: SHG        !< Snyder Hydrograph object
   real,           intent(in ) :: A_input    !< drainage area (m^2)
   real,           intent(in ) :: Cp         !< Basin peaking factor
   real,           intent(in ) :: tp         !< Basin lag time.
   real, optional, intent(in ) :: tR_input   !< Duration of the excess rainfall.
   
   real :: A, PT, tr, tPR, QPR,  W50, W75, Tb
      
   A = A_input / 1000000.D0 ! m2 -> km2
   tr = tp / 5.5                      ! standard effective rainfall duration
   
   if (.not. present(tR_input)) then  ! Standard Case
      tPR = tp
   else                               ! Non-Standard Case 
      tPR = tp - (tr - tR_input) / 4  ! correct lag time for non-default rainfall duration
      tr = tR_input                   ! adjust tr to input rainfall duration for graph calculation
   endif
   QPR = 2.75 * Cp * (A) / tPR ! Peak runoff
   
   W50 = 2.14 / (QPR / A) ** 1.08 ! width at 50% of peak
   W75 = 1.22 / (QPR / A) ** 1.08 ! width at 75% of peak
   Tb = 11.11 * (A / QPR) - 1.5 * W50 - W75 ! total runoff time
   PT = tr / 2. + tPR ! peak time = basin lag time + rainfall duration

   SHG%time_array = [ 0. , PT - W50/3. , PT - W75/3. , PT  , PT + 2*W75/3. , PT + 2*W50/3. , Tb ]
   SHG%Q_array    = [ 0. , QPR * 0.5   , QPR * 0.75  , QPR , QPR * 0.75    , QPR * 0.5     , 0. ]
   
   end Subroutine compute_snyder_hydrograph
   
   !> This routine takes the 7 support points of the Snyder Unit Hydrograph and interpolates them according to the timestep.
   subroutine interpolate_snyder_hydrograph(SHG,SCS_UnitHydComp,NrUHcomponents,UH_decayfrac,UH_decayrate)
   type(t_SHG),            intent(in   ) :: SHG             !> Snyder Unit Hydrograph containing 7 support points that need to be interpolated.
   real,     dimension(:), intent(  out) :: SCS_UnitHydComp !> array containing Unit Hydrograph increments (summing to 1).
   real,                   intent(in   ) :: UH_decayfrac    !> fraction of peak flow at which UH starts to decay exponentially.
   real,                   intent(in   ) :: UH_decayrate    !> Rate (1/hour) at which UH will decay exponentially.
   integer,                intent(in   ) :: NrUHcomponents  !> Timesteps in the unit hydrograph (ceil(TC))
   
   integer :: idum, j, j_exp
   real    :: InputTPValue, OutputQpvalue, t1, t0
   
   Do j=1,NrUHComponents
      InputTpValue = float (j )*SHG%time_array(7) / NrUHComponents
      CALL RR_INTERP (7, SHG%time_array, SHG%Q_array, InputTpValue, OutputQpValue, idum)
      SCS_UnitHydComp(j) = max(0.,OutputQpValue)
      if(InputTPValue > SHG%time_array(6) .and. OutputQpvalue < UH_decayfrac*SHG%Q_array(4)) then
         j_exp = j ! exponential decay starts here
         exit
      endif
   Enddo
   Do j=1,j_exp-1 ! integrate hydrograph curve
      SCS_UnitHydComp(j) = (SCS_UnitHydComp(j+1)-0.5*(SCS_UnitHydComp(j+1)-SCS_UnitHydComp(j)))*(SHG%time_array(7) / NrUHComponents)
   Enddo

   InputTpValue = float (j_exp )*SHG%time_array(7) / NrUHComponents
   CALL RR_INTERP (7, SHG%time_array, SHG%Q_array, InputTpValue, OutputQpValue, idum) !interpolate discharge value at which exponential decay starts
   if (j_exp < NrUHComponents) then
      do j = j_exp,NrUHComponents ! integrate exponential decay curve
         t1 = (float (j+1-j_exp )*SHG%time_array(7) / NrUHComponents)
         t0 = (float (j  -j_exp )*SHG%time_array(7) / NrUHComponents)
         SCS_UnitHydComp(j) = -OutputQpValue/UH_decayrate*(exp(-UH_decayrate*t1)-exp(-UH_decayrate*t0))
      enddo
   endif
   SCS_UnitHydComp = SCS_UnitHydComp/sum(SCS_UnitHydComp) ! normalize hydrograph
   end subroutine
   
   !> Compute the base flow either constant or with exponential or linear decay from the start of the simulation.
   real function calculate_base_flow(STRTQ, current_time, decay_rate, interpolation_method)
   use Network, only: TimeSettings
   ! Parameters:
   real,              intent(in) :: STRTQ                 !> Initial flow in the river [mm/hour]
   real,    optional, intent(in) :: current_time          !> current simulation time [hours]
   real,    optional, intent(in) :: decay_rate            !> decay rate [1/hour] or [mm/hour^2]
   integer, optional, intent(in) :: interpolation_method  !> Interpolation method ('exponential' or 'linear').

    if      (interpolation_method == 0) then
       calculate_base_flow = 0.
    else if (interpolation_method == 1) then
            calculate_base_flow = STRTQ 
    else if (interpolation_method == 2) then
            calculate_base_flow = STRTQ * exp(decay_rate * -current_time)
    else if (interpolation_method == 3) then
            calculate_base_flow = max(0.,STRTQ -decay_rate * current_time)
    endif
    calculate_base_flow = calculate_base_flow*TimeSettings%TimestepSize/3600
   end function
   
   end module Snyder_hydrograph
