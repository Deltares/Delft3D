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

 module InfiltrationFormulations

 use Messages

 implicit none

 contains


    Subroutine HortonFormula (MinInfCap, MaxInfCap, DecreaseRate, RecoveryRate, PreviousInfCap, NewInfCap, &
                              Idebug, TimestepSize, Dt, InitialStorage, Rainfall, InfCapStatus)

    Real    MinInfCap, MaxInfCap, DecreaseRate, RecoveryRate, PreviousInfCap, NewInfCap, &
            Dt, InitialStorage, Rainfall
    Real    RFrac, Dt1, Ratio
    Integer TimestepSize, Idebug, InfCapStatus, NrSecondsPerHour

!   write(*,*) ' Start HortonFormula'

      ! Compute infiltration capacity as defined by Horton equations
      !
      ! Input data:
      !    MinInfCap = minimum infiltration capacity (mm/hr)
      !    MaxInfCap = maximum infiltration capacity (mm/hr)
      !    DecreaseRate = decrease rate (1/hr)
      !    RecoveryRate = recovery rate (1/hr)
      !    PreviousInfCap= Runoff factor c
      !    TimestepSize  = timestep in seconds
      !    Idebug        = file unit nr of debug file
      !    Dt  (I/O) = time in hours since start of decreasing/recovery mode
      !    InitialStorage   = initial storage (=storage at start of timestep)
      !    Rainfall  = rainfall in current timestep
      ! Output:
      !    NewInfCap = new infiltration capacity (mm/hr)
      !    InfCapStatus = infiltration capacity status; 0=no change, 1=decrease, 2=recovery
      !    Dt  (I/O) = time in hours since start of decreasing/recovery mode

       NrSecondsPerHour = 3600
       RFRAC = FLOAT(TimestepSize) / FLOAT(NrSecondsPerHour)

       IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' RFRAC TMSIZE',RFRAC,TimestepSize
       DT1 = DT   + RFRAC
       IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' DT1', DT1
       IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' MinInfCap', MinInfCap
       IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' MaxInfCap', MaxInfCap
       IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' PreviousInfCap', PreviousInfCap

       IF (MaxInfCap .le. MinInfCap) THEN
    !           constante infiltratiecapaciteit; infiltration status not changed
           InfCapStatus = 0
       ELSEIF (PreviousInfCap .GE. MaxInfCap) THEN
    !          Previous infcap is maximaal; nu afname,
           IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Switch Afname'
           InfCapStatus = 1
           IF (DT .GT. RFRAC) THEN
              DT1 = RFRAC
              DT  = 0
           ENDIF
       ELSEIF (PreviousInfCap .LE. MinInfCap) THEN
    !          Previous infcap is minimaal; nu toename
           InfCapStatus = 2
           IF (DT .GT. RFRAC) THEN
             DT1 = RFRAC
             DT  = 0
           ENDIF
       ENDIF

  ! uitbreiden checks: afname infiltratiecapaciteit zolang er berging en/of neerslag is;
  !                    toename infiltratiecapaciteit zodra berging leeg is en geen regen meer
        !                               tenzij er geen berging op oppervlak is en het droog blijft
       IF (InfCapStatus .EQ. 1 .AND. InitialStorage .LE. 0 .AND. Rainfall .LE. 0) THEN
! toestand is afname, maar geen berging, geen neerslag meer; switch naar herstel
          InfCapStatus = 2
          Ratio = (PreviousInfCap-MaxInfcap) / (MinInfCap - MaxInfCap)
          IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Switch herstel RATIO', RATIO
!         WRITE(*,*) ' Switch herstel RATIO', RATIO
          IF (RATIO .GT. 0) THEN
              DT1 = -1/RecoveryRate * LOG (RATIO)  + RFRAC
          ELSE
              DT1 = 9999.
          ENDIF
          DT  = MAX (0.0, DT1 - RFRAC)
       ELSEIF (InfCapStatus .EQ. 2 .AND. (InitialStorage .GT. 0 .OR. Rainfall .GT. 0) ) THEN
! toestand is herstel, maar er is weer berging of neerslag: switch naar afname
          InfCapStatus = 1
          RATIO = (PreviousInfCap-MinInfCap) / (MaxInfCap - MinInfCap)
          IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Switch afname RATIO', RATIO
!         WRITE(*,*) ' Switch afname RATIO', RATIO, ' type&opp',iptyp, ipopp
          IF (RATIO .GT. 0) THEN
              DT1 = -1/DecreaseRate * LOG (RATIO)  + RFRAC
          ELSE
              DT1 = 9999.
          ENDIF
          DT = MAX (0.0, DT1 - RFRAC)
       ENDIF
       IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' INFCapStatus DT1 INFCP', InfCapStatus, DT1, PreviousInfCap

       IF (InfCapStatus .EQ. 0) THEN
    !           doe niets; INFCP ongewijzigd
       ELSEIF (InfCapStatus .EQ. 1) THEN
          NewInfCap = MinInfCap + (MaxInfCap - MinInfCap) * EXP (-1*DecreaseRate * DT1)
       ELSEIF (InfCapStatus .EQ. 2) THEN
          NewInfCap = MaxInfCap - (MaxInfCap - MinInfCap) * EXP (-1*RecoveryRate * DT1)
       ENDIF
       NewInfCap = MIN (NewInfCap, MaxInfCap)
       NewInfCap = Max (NewInfCap, MinInfCap)


!   write(*,*) ' Exit HortonFormula'


    End subroutine HortonFormula





 End module InfiltrationFormulations
