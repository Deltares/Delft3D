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
module horton
   implicit none	

   !! Horton is based on in/decrease of infiltration between min and max value.
   integer, parameter :: HORTON_CAPSTAT_NOCHANGE = 0 !< No change in infiltration state
   integer, parameter :: HORTON_CAPSTAT_DECREASE = 1 !< Infiltration in decreasing mode
   integer, parameter :: HORTON_CAPSTAT_RECOVERY = 2 !< Infiltration in recovery/increasing mode
   contains 
   
   !> Computes infiltration capacity as defined by Horton equations.
   !!
   !! Infiltration capacity defined in mm/hr, decrease and recovery rate in 1/hr.
   !! Typical timestep used in application is 1 minute (i.e. much smaller than 1 hour),
   !! otherwise computation of infiltration volume (in mm) should be more refined
   !! (using integral of capacity function, depending on state recovery or decrease).
   
   function infiltration_horton_formula(n, MinInfCap, MaxInfCap, DecreaseRate, RecoveryRate, PreviousInfCap, NewInfCap, &
                                       TimestepSize, InitialStorage, Rainfall, includerain, InfCapState, InfiltrationMM) result(ierr)
      use dhydrology_error
      use precision_basics
      
      integer,                    intent(in   ) :: n                  !< Array length (grid cell count)
      double precision,           intent(in   ) :: MinInfCap(n)       !< Minimum infiltration capacity (mm/hr)
      double precision,           intent(in   ) :: MaxInfCap(n)       !< Maximum infiltration capacity (mm/hr)
      double precision,           intent(in   ) :: DecreaseRate(n)    !< Decrease rate (1/hr)
      double precision,           intent(in   ) :: RecoveryRate(n)    !< Recovery rate (1/hr)
      double precision,           intent(in   ) :: PreviousInfCap(n)  !< Last infiltration capacity (mm/hr)
      double precision,           intent(  out) :: NewInfCap(n)       !< New infiltration capacity (mm/hr)
      double precision,           intent(in   ) :: TimestepSize       !< Timestep size (s)
      double precision,           intent(in   ) :: InitialStorage(n)  !< Initial storage (=storage at start of timestep) (m)
      double precision,           intent(in   ) :: Rainfall(:)        !< Rainfall in current timestep (or more precise: additional ground rainfall, so minus interception)
      integer,                    intent(in   ) :: includerain        !< indicates whether or not (1/0) array Rainfall is available, otherwise no rainfall is assumed
      integer,                    intent(  out) :: InfCapState(n)     !< Infiltration capacity state; (one of HORTON_CAPSTAT_(NOCHANGE|RECOVERY|INCREASE))
      double precision, optional, intent(  out) :: InfiltrationMM(n)  !< Infiltration amount (mm)
      integer                                   :: ierr               !< Result status, DHYD_NOERR if successful.
      
      ! local
      integer, parameter              :: NrSecondsPerHour = 3600
      double precision                :: RFrac
      double precision                :: ratio
      integer                         :: i
      logical                         :: rainIsFalling 
      
      ierr = DHYD_NOERR


      RFRAC = TimestepSize / NrSecondsPerHour

      do i = 1, n
         if (includerain == 1) then
            rainIsFalling = comparereal(Rainfall(i), 0d0) ==1
         else 
            rainIsFalling = .false.
         endif

         ! Compute the actual infiltration rate.
         if (comparereal(MaxInfCap(i), MinInfCap(i)) <= 0) then
            
            ! Do nothing, because of lack of band width between minimum and maximum capacity.
            InfCapState(i) = HORTON_CAPSTAT_NOCHANGE
            NewInfCap(i) = PreviousInfCap(i)  
            
         else if(comparereal(InitialStorage(i), 0d0) == 1 .or. rainIsFalling) then
            
            !  Wet situation, infiltration capacity is decreasing
            InfCapState(i) = HORTON_CAPSTAT_DECREASE
            ratio = (PreviousInfCap(i)-MinInfCap(i)) / (MaxInfCap(i) - MinInfCap(i))
            NewInfCap(i) = MinInfCap(i) + (MaxInfCap(i) - MinInfCap(i)) * ratio * exp(-1d0*DecreaseRate(i) * rfrac)
            
         else 
            
            !  Dry situation, infiltration capacity is recovering
            InfCapState(i) = HORTON_CAPSTAT_RECOVERY
            ratio = (PreviousInfCap(i)-MaxInfcap(i)) / (MinInfCap(i)- MaxInfCap(i))
            NewInfCap(i) = MaxInfCap(i) - (MaxInfCap(i) - MinInfCap(i)) * ratio * exp(-1d0*RecoveryRate(i) * rfrac)
            
         end if
      enddo

      if (present(InfiltrationMM)) then
         InfiltrationMM = NewInfCap * TimeStepSize / NrSecondsPerHour
      end if

   end function infiltration_horton_formula
end module horton