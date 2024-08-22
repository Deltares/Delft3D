!----- AGPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
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

 module Infiltration

 implicit none

 contains


    Subroutine Infiltration_HortonFormula (n, MinInfCap, MaxInfCap, DecreaseRate, RecoveryRate, PreviousInfCap, NewInfCap, &
                                           TimestepSize, Dt, InitialStorage, Rainfall, InfCapStatus, InfiltrationMM)

    integer, intent(in)           :: n
    Double precision, intent(in)  :: MinInfCap(n), MaxInfCap(n), DecreaseRate(n), RecoveryRate(n), PreviousInfCap(n)
    Double precision, intent(in)  :: InitialStorage(n), Rainfall(n)
    Double precision, intent(out) :: NewInfCap(n)
    Double precision, intent(out) :: InfiltrationMM(n)
    integer, intent(out)          :: InfCapStatus(n)
    Double precision              :: DT(n), Dt1(n)

    Integer                          TimestepSize, NrSecondsPerHour
    Double precision                 RFrac, Ratio(n)

      ! Input data:
      !    n  = array dimension
      !    MinInfCap = minimum infiltration capacity (mm/hr)
      !    MaxInfCap = maximum infiltration capacity (mm/hr)
      !    DecreaseRate = decrease rate (1/hr)
      !    RecoveryRate = recovery rate (1/hr)
      !    PreviousInfCap= last infiltration capacity (mm/hr)
      !    TimestepSize  = timestep size in seconds
      !    Dt  (I/O)     = time in hours since start of decreasing/recovery mode
      !    InitialStorage= initial storage (=storage at start of timestep)
      !    Rainfall      = rainfall in current timestep (or more precise: additional ground rainfall, so minus interception)
      ! Output:
      !    NewInfCap    = new infiltration capacity (mm/hr)
      !    InfiltrationMM = infiltration in mm
      !    InfCapStatus = infiltration capacity status; 0=no change, 1=decrease, 2=recovery
      !    Dt  (I/O)    = time in hours since start of decreasing/recovery mode


      ! Compute infiltration capacity as defined by Horton equations

      ! Note: infiltration capacity defined in mm/hr, decrease and recovery rate in 1/hr
      ! Note: typical timestep used in application is 1 minute (i.e. much smaller than 1 hour)
      ! Note: otherwise computation of infiltration volume (in mm) should be more refined (using integral of capacity function, depending on status recovery or decrease)

       NrSecondsPerHour = 3600
       RFRAC = Dble(TimestepSize) / Dble(NrSecondsPerHour)
       DT1   = DT  + RFRAC

       Where (MaxInfCap .le. MinInfCap)
    !      constant infiltratiecapacity; infiltration status not changed
           InfCapStatus = 0
       Else Where (PreviousInfCap .GE. MaxInfCap)
    !      Previous infiltration capacity is at maximum value; now decrease
           InfCapStatus = 1
           Where (DT .GT. RFRAC .and. PreviousInfCap .GE. MaxInfCap)
              DT1 = RFRAC
              DT  = 0
           End Where
       Else Where (PreviousInfCap .LE. MinInfCap)
    !      Previous infiltration capacity is at minimum value, now increase
           InfCapStatus = 2
           Where (DT .GT. RFRAC .and. PreviousInfCap .LE. MinInfCap)
             DT1 = RFRAC
             DT  = 0
           End Where
       End Where

  ! additional checks: decrease of infiltration capacity as long as there is storage and/or rain
  !                    recovery of infiltration capacity as soon as storage is empty and no rain
  ! note: not just simple  ft = fc + (f0-fc)*exp (-kt)
  !       trick required for computation of t for intermediate switch from decrease to recovery or vice versa, before reaching max. or min.

       Ratio = -1.
       Where (InfCapStatus .EQ. 1 .AND. InitialStorage .LE. 0 .AND. Rainfall .LE. 0)
          ! status is decrease, but no storage and no rain anymore: switch to recovery
          InfCapStatus = 2
          Ratio = (PreviousInfCap-MaxInfcap) / (MinInfCap - MaxInfCap)
          Where (RATIO .GT. 0)
              DT1 = -1/RecoveryRate * LOG (RATIO)  + RFRAC
          Else where (RATIO .LE. 0)
              DT1 = 9999.
          End Where
          DT  = MAX (0.0d0, DT1 - RFRAC)
       Else Where (InfCapStatus .EQ. 2 .AND. (InitialStorage .GT. 0 .OR. Rainfall .GT. 0) )
          ! status is recovery, but storage or rain: switch to decrease
          InfCapStatus = 1
          RATIO = (PreviousInfCap-MinInfCap) / (MaxInfCap - MinInfCap)
          Where (RATIO .GT. 0)
              DT1 = -1/DecreaseRate * LOG (RATIO)  + RFRAC
          Else where (Ratio .le. 0)
              DT1 = 9999.
          End Where
          DT = MAX (0.0d0, DT1 - RFRAC)
       End Where

       Where (InfCapStatus .EQ. 0)
    !     do nothing, unchanged
       Else Where (InfCapStatus .EQ. 1)
    !     infiltration capacity is decreasing
          NewInfCap = MinInfCap + (MaxInfCap - MinInfCap) * EXP (-1*DecreaseRate * DT1)
       Else Where (InfCapStatus .EQ. 2)
    !     infiltration capacity is recovering
          NewInfCap = MaxInfCap - (MaxInfCap - MinInfCap) * EXP (-1*RecoveryRate * DT1)
       End Where

       NewInfCap = MIN (NewInfCap, MaxInfCap)
       NewInfCap = Max (NewInfCap, MinInfCap)

       InfiltrationMM = NewInfCap * TimeStepSize / NrSecondsPerHour

    End subroutine Infiltration_HortonFormula



 End module Infiltration
