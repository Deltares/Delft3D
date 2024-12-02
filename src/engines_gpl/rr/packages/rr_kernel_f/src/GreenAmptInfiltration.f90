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

module GreenAmptInfiltration


 implicit none

 contains


    Subroutine SetGreenAmptConstants (KSat, Psi, Theta_DMax, Lu, Kr, Tr, idebug)

    double precision      KSat, Psi, Theta_DMax, Lu, Kr, Tr
    integer               Idebug

    double precision      dt, i, dtheta, Ttemp, FTemp, dF, F
    double precision      Inches2mm


       Inches2mm = 25.4D0

      ! set constants
      ! Note we use SI units mm and hour
      ! Note SWMM description is in inches and hours, take care of conversion of proper conversion of units (and some constants as well)
      !
       Lu = 4 * Sqrt (KSat)        ! Lu = 4 * sqrt(Ksat) with Lu in inches, Ksat in inches per hour
       Lu = Lu * Sqrt (Inches2mm)   ! with Lu = mm, Ksat in mm/hour, add sqrt(25.4) to get correct dimensions (mm)

       Kr = Sqrt (Ksat) / 75.D0    ! Kr in  1/hour, Ksat in inches/hour, the dimension of 75 is (in/hr)**0.5
       Kr = Kr / Sqrt (Inches2mm)  ! with Ksat in mm/hour we need conversion factor to get to 1/hour

       Tr = 0.06D0 / Kr            ! T2 in hour, Kr in 1/hour, the 0.06 is dimensionless

       if (idebug .ne. 0) then
           write(idebug,*) ' Ksat [mm/hour] ', KSat
           write(idebug,*) ' psi  [mm] ', psi
           write(idebug,*) ' Lu   [mm] ', Lu
           write(idebug,*) ' Kr   [1/hour] ', Kr
           write(idebug,*) ' Tr   [hour] ', Tr
       endif

    End subroutine SetGreenAmptConstants



    Subroutine GreenAmpt (RainfallRate, DepthPonding, Theta_D, Theta_DU, F, T, &
                          KSat, Psi, Theta_DMax, Lu, Kr, Tr, smallf, &
                          Idebug, TimestepSize)

    double precision      RainfallRate, DepthPonding, Theta_D, Theta_DU, F, T
    double precision      KSat, Psi, Theta_DMax, Lu, Kr, Tr, smallf
    integer               TimestepSize, Idebug

    double precision      dt, dtsat, i, dtheta, Ttemp, FTemp, dF, F1, F2, Fs
    double precision      Inches2mm
    logical               unsaturated

        Inches2mm = 25.4D0

      ! Compute GreenAmpt infiltration according to SWMM manual
      ! Note we use SI units mm and hour
      ! Note SWMM description is in inches and hours, take care of conversion of proper conversion of units (and some constants as well)
      !
      ! Input data:
      !    RainfallRate   = in mm per hour
      !    DepthPonding   = surface ponding depth in mm available for infiltration (to be added to rainfallrate)
      !    Theta_D        = soil moisture deficit at start of current rainfall event
      !    Theta_DU       = soil moisture deficit of upper soil recovery zone
      !    F              = cumulative infiltrated volume (mm) at beginning of timestep
      !    T              = recovery time remaining before next event can begin (hour)
      ! Constants
      !    KSat           = Saturated hydraulic conductivity (mm/hour)
      !    Psi            = suction head at wetting front (mm)
      !    Theta_DMax     = max. soil moisture deficit
      !    Lu             = depth of upper soil recovery zone (mm)
      !    Kr             = moisture deficit recovery constant (1/hour)
      !    Tr             = minimum recovery time before new rainfall event (hours)
      !
      ! At time t=0: theta_D = Theta_DU = Theta_DMax,  CumInfVol_F=0, T=0
      !
      !
      ! Output:
      !    F      =  Cum. infiltration end of timestep (mm)
      !   smallf  = dF/dt

      ! Intermediate
      !    i  = rainfall rate + depthPonding / dt
      !    dt = timestep in hours (TimestepSize / 3600)
      !    F1 = CumInfVol_F at beginning of timestep
      !    F2 = CumInfVol_F at end of timestep
      !    dt = timestep in hours
      !    Ttemp  = adjusted T
      !    dtheta = delta theta

      !  depth ponding >= 0
       DepthPonding = max (0.0D0, DepthPonding)

       IF (iDebug .ne. 0) then
          WRITE(IDEBUG,*) ' Call GreenAmpt with'
          WRITE(IDEBUG,*) ' RainfallRate = ', RainfallRate, '[mm/hour]'
          WRITE(IDEBUG,*) ' DepthPonding = ', DepthPonding, '[mm], corrected to be >= 0'
          WRITE(IDEBUG,*) ' Theta_D      = ', Theta_D     , '[]'
          WRITE(IDEBUG,*) ' Theta_DU     = ', Theta_DU    , '[]'
          WRITE(IDEBUG,*) ' F            = ', F           , '[mm]'
          WRITE(IDEBUG,*) ' T            = ', T           , '[hr]'
          WRITE(IDEBUG,*) ' Ksat         = ', Ksat        , '[mm/hr]'
          WRITE(IDEBUG,*) ' Psi          = ', Psi         , '[mm]'
          WRITE(IDEBUG,*) ' Theta_Dmax   = ', Theta_DMax  , '[]'
          WRITE(IDEBUG,*) ' Lu           = ', Lu          , '[mm]'
          WRITE(IDEBUG,*) ' Kr           = ', Kr          , '[1/hour]'
          WRITE(IDEBUG,*) ' Tf           = ', Tr          , '[hour]'
          WRITE(IDEBUG,*) ' TimestepSize = ', TimeStepSize, '[s]'
       endif

! step 0
       F1 = F
       Ftemp = F
       dt = TimestepSize * 1.D0 / 3600.D0      ! in hours

       unsaturated = (theta_DU .gt. 0.1D-9 .or. T .lt. 0)
!  unsaturated
       if (unsaturated) then
!     step 1
           i  = RainfallRate + DepthPonding / dt
!     step 2
           Ttemp = T - dt
!     step 3
           if (i .le. 0) then
               smallf = 0
               dtheta   = kr * Theta_DMax * dt
               theta_du = theta_du + dtheta
               theta_du = max (0.0D0, theta_du)
               theta_du = min (theta_DMax, theta_du)
               Ftemp    = Ftemp - dtheta * Lu
               if (Ttemp .le. 0) then
                   theta_D = theta_DU
                   FTemp   = 0.0001
                   F       = Ftemp
               endif
           elseif (i .le. Ksat) then
!     step 4
               smallf = i
               dF = i * dt
               Ftemp = Ftemp + dF
               theta_du = theta_du - dF / Lu
           elseif (i .gt. Ksat) then
!     step 5
               Ttemp = Tr
               Fs = Ksat * psi * theta_d / ( i - Ksat)
               IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' i > Ksat; Ftemp Fs', Ftemp, Fs
               if (Ftemp .ge. Fs) then
                   unsaturated = .false.  ! switch to saturated
                   if (idebug .gt. 0) write(Idebug,*) 'switch to saturated'
                   goto 99
               elseif (Ftemp + i * dt .le. Fs) then
                   smallf = i
                   dF = i * dt
                   Ftemp = Ftemp + i * dt
                   theta_du = theta_du - dF / Lu
               else
!     step 5e
                   dtsat = dt - (Fs - Ftemp)/i
                   call Solve432 (Fs, dtsat, KSat, Psi,theta_d, i, F2, idebug)
                   dF = F2 - Ftemp
                   Ftemp = F2
                   theta_DU = Theta_Du - dF / Lu
                   smallf = dF / dt
               endif
            endif
            theta_du = max (0.0D0, theta_du)
            theta_du = min (theta_DMax, theta_du)
       else
!         saturated
  99      continue
!         step 1
          i  = RainfallRate + DepthPonding / dt
!         step 2
          Ttemp = Tr
!         step 3
          call Solve432 (F1, dt, KSat, Psi,theta_d, i, F2, idebug)
!         step 4
          dF = F2 - F1
          if (dF .gt. i * dt) then
!            step 5
             dF = i * dt
             unsaturated = .true.
             if (idebug .ne. 0) write(Idebug,*) 'switch to unsaturated'
          endif
          ! added step to allow switch to unsaturated and reduction of T
          if (dF .le. 0) then
             unsaturated = .true.
             if (idebug .ne. 0) write(Idebug,*) 'switch to unsaturated'
             Ttemp = T - dt
             if (Ttemp < 0) then
                theta_du = theta_dMax
                theta_d  = theta_dMax
             endif
          endif
!         step 6
          Ftemp = Ftemp + dF
          Theta_du = theta_du - dF / Lu
          smallf = dF / dt
          theta_du = max (0.0D0, theta_du)
          theta_du = min (theta_DMax, theta_du)
       endif

       F2 = Ftemp
       T  = Ttemp
       F  = F2

       IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' dF ',dF
       IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' F2 ',F2
       IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Ftemp ',Ftemp
       IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Ttemp ',Ttemp
       IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' smallf ', smallf
       IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' theta_du ',theta_du
       IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Unsaturated = ', Unsaturated

    End subroutine GreenAmpt


  Subroutine Solve432(F1, dt, KSat, Psi,theta_d, i, F2, idebug)


     double precision F1, dt, KSat, Psi, Theta_d, i, F2
     double precision C, F2min, F2max, F2minerror, F2maxError, F2error, averageInfiltration
     integer        idebug

     C = KSat * dt + F1 - psi * Theta_d * log (F1 + psi * theta_D)

     F2min = F1
     F2max = F1 + max (i, Ksat)

     ! bisection
     F2minError = C + psi * theta_d * log (F2min + psi * theta_d) - F2min       ! > 0
     F2maxError = C + psi * theta_d * log (F2max + psi * theta_d) - F2max       ! <= 0
     if (idebug .ne. 0) write(idebug,*) ' F2min F2 max           ', F2min, F2max
     if (idebug .ne. 0) write(idebug,*) ' F2minerror F2 maxerror ', F2minerror, F2maxerror
     F2 = F2max
     F2error = F2maxError

     do while (F2MaxError .lt. -0.001)
        F2 = F2min + 0.5D0 * (F2max-F2min)
        F2error = C + psi * theta_d * log (F2 + psi * theta_d) - F2
        if (F2error .ge. 0) then
             F2min = F2
             F2minError = F2error
        else
             F2max = F2
             F2maxError = F2error
        endif
        IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' F2min ',F2min, F2MinError
        IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' F2max ',F2max, F2MaxError

     enddo

     if (idebug .ne. 0) write(idebug,*) ' F2 found', F2, F2error
!    AverageInfiltrationCap = (F2-F1)/ dt
     if (idebug .ne. 0) write(idebug,*) ' F2-F1', (F2-F1)

   Return
  END subroutine Solve432




 End module GreenAmptInfiltration
