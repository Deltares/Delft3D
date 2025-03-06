module GreenAmptInfiltration

 use Messages

 implicit none

 contains


    Subroutine SetGreenAmptConstants (KSat, Psi, Theta_DMax, Lu, Kr, Tr, idebug)

    double precision      KSat, Psi, Theta_DMax, Lu, Kr, Tr
    integer               TimestepSize, Idebug

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

       T2 = 0.06D0 / Kr            ! T2 in hour, Kr in 1/hour, the 0.06 is dimensionless

       if (idebug .gt. 0) then
           write(idebug,*) ' Ksat [mm/hour] ', KSat
           write(idebug,*) ' psi  [mm] ', psi
           write(idebug,*) ' Lu   [mm] ', Lu
           write(idebug,*) ' Kr   [1/hour] ', Kr
       endif

    End subroutine SetGreenAmptConstants



    Subroutine GreenAmpt (RainfallRate, DepthPonding, Theta_D, Theta_DU, F, T, &
                          KSat, Psi, Theta_DMax, Lu, Kr, Tr, &
                          Idebug, TimestepSize)

    double precision      RainfallRate, DepthPonding, Theta_D, Theta_DU, F, T
    double precision      KSat, Psi, Theta_DMax, Lu, Kr, TRecoveryMin
    integer               TimestepSize, Idebug

    double precision      dt, i, dtheta, Ttemp, FTemp, dF, F
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
      !    F              = cumulative infiltrated volume (in mm) at beginning of timestep
      !    T              = recovery time remaining before next event can begin (hour)
      ! Constants
      !    KSat           = Saturated hydraulic conductivity (mm/hour)
      !    Psi            = suction head at wetting frotn (mm)
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

      ! Intermediate
      !    i  = rainfall rate + depthPonding / dt
      !    dt = timestep in hours (TimestepSize / 3600)
      !    F1 = CumInfVol_F at beginning of timestep
      !    F2 = CumInfVol_F at end of timestep
      !    dt = timestep in hours
      !    Ttemp  = adjusted T
      !    dtheta = delta theta
      !
! step 0
       F1 = F
       Ftemp = F1
       dt = TimestepSize * 1.D0 / 3600.D0

       unsaturated = (theta_D .gt. 0)
!  unsatureted
       if (unsaturated) then
!     step 1
           i  = RainfallRate + DepthPonding / dt
!     step 2
           Ttemp = T - dt
!     step 3
           if (i .le. 0) then
               f = 0
               dtheta   = kr * Theta_DMax * dt
               theta_du = theta_du + dtheta
               Ftemp    = Ftemp - dtheta * Lu
               if (Ttemp .le. 0) then
                   theta_D = theta_DU
                   FTemp   = 0
                   F       = 0
               endif
           elseif (i .le. Ksat) then
!     step 4
               f = i
               dF = i * dt
               Ftemp = Ftemp + dF
               theta_du = theta_du - dF / Lu
           elseif (i .gt. Ksat) then
!     step 5
               Ttemp = Tr
               Fs = Ks * psi * theta_d / ( i - Ksat)
               if (Ftemp .ge. Fs) then
                   unsaturated = .false.  ! switch to saturated
                   goto 99
               elseif (Ftemp + i * dt .le. Fs) then
                   f = i
                   Ftemp = Ftemp + dF
                   theta_du = theta_du - dF / Lu
               else
!     step 5e
                   dtsat = dt - (Fs - Ftemp)/i
                   call Solve432 (Fs, dtsat, Psi,theta_d, F2)
                   dF = F2 - Ftemp
                   Ftemp = F2
                   theta_DU = Theta_Du - dF / Lu
                   f = dF / dt
               endif
            endif
            theta_du = max (0, theta_du)
            theta_du = min (theta_DMax, theta_du)
       else
!         saturated
  99      continue
!         step 1
          i  = RainfallRate + DepthPonding / dt
!         step 2
          Ttemp = Tr
!         step 3
          call Solve432 (F1, dt, Psi,theta_d, F2)
!         step 4
          dF = F2 - F1
          if (dF .gt. i * dt) then
!            step 5
             dF = i * dt
             unsatyrated = .true.
          endif
!         step 6
          Ftemp = Ftemp + dF
          Theta_du = theta_du - dF / Lu
          f = dF / dt
          theta_du = max (0, theta_du)
          theta_du = min (theta_DMax, theta_du)
       endif

       F2 = Ftemp

       IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' dF ',dF
       IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' F2 ',F2
       IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' theta_du ',theta_du

    End subroutine GreenAmpt


  Subroutine Solve432(F1, dt, Psi,theta_d, i, F2)


     double precision F1, dt, Psi, Theta_d, i, F2
     double precision C, F2min, F2max, temp

     C = KSat * dt + F1 - psi * Theta_d * log (F1 + psi * theta_D)

     F2min = F1
     F2max = F1 + i

     ! bisection
     F2minError = C + psi * theta_d * log (F2min + psi * theta_d) - F2min       ! > 0
     F2maxError = C + psi * theta_d * log (F2max + psi * theta_d) - F2max       ! <= 0
     write(*,*) ' F2min F2 max           ', F2min, F2max
     write(*,*) ' F2minerror F2 maxerror ', F2minerror, F2maxerror

     do while (F2MaxError .lt. -0.001) then
        F2 = F1max + 0.5D0 * (F2max-F2min)
        F2error = C + psi * theta_d * log (F2 + psi * theta_d) - F2
        if (F2error .ge. 0) then
             F2min = F2
             F2minError = F2error
        else
             F2max = F2
             F2maxError = F2error
        endif
     enddo

     write(*,*) ' F2 found', F2, F2error
     AverageInfiltrationCap = (F2-F1)/ dt

   Return
  END subroutine Solve432




 End module GreenAmptInfiltration
