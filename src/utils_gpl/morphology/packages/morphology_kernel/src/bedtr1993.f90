subroutine bedtr1993(uuu       ,vvv       ,u2dh      ,d50       ,d90       , &
                   & h1        ,taurat    ,ustarc    ,muc       ,rhosol    , &
                   & dstar     ,ws        ,hrms      ,tp        ,teta      , &
                   & rlabda    ,umod      ,qbcu      ,qbcv      ,qbwu      , &
                   & qbwv      ,qswu      ,qswv      ,rhowat    ,ag        , &
                   & wave      ,eps       ,uon       ,uoff      ,vcr       , &
                   & error     ,message   )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
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
!!--description-----------------------------------------------------------------
!
! Compute bed load transport according to Van Rijn
! Note: Formulation used depends on presence of waves in the
! simulation.
! If no waves then use traditional Van Rijn formulation
! If waves then use new parameterization which
! includes wave asymmetry
! Note: The two methods are known to give different results
! (order factor 2) for situations without waves
! Van Rijn (1993,2000)
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    !
    implicit none
!
! Arguments
!
    real(fp)    , intent(in)  :: ag
    real(fp)    , intent(in)  :: d50
    real(fp)    , intent(in)  :: d90
    real(fp)    , intent(in)  :: dstar
    real(fp)    , intent(in)  :: eps
    real(fp)    , intent(in)  :: h1
    real(fp)    , intent(in)  :: hrms    !  Description and declaration in esm_alloc_real.f90
    real(fp)    , intent(in)  :: muc
    real(fp)    , intent(out) :: qbcu
    real(fp)    , intent(out) :: qbcv
    real(fp)    , intent(out) :: qbwu
    real(fp)    , intent(out) :: qbwv
    real(fp)    , intent(out) :: qswu
    real(fp)    , intent(out) :: qswv
    real(fp)    , intent(in)  :: rhosol  !  Description and declaration in esm_alloc_real.f90
    real(fp)    , intent(in)  :: rhowat
    real(fp)    , intent(in)  :: rlabda  !  Description and declaration in esm_alloc_real.f90
    real(fp)    , intent(in)  :: taurat
    real(fp)    , intent(in)  :: teta    !  Description and declaration in esm_alloc_real.f90
    real(fp)    , intent(in)  :: tp      !  Description and declaration in esm_alloc_real.f90
    real(fp)    , intent(in)  :: u2dh
    real(fp)    , intent(in)  :: umod
    real(fp)    , intent(in)  :: ustarc
    real(fp)    , intent(in)  :: uuu
    real(fp)    , intent(in)  :: vvv
    real(fp)    , intent(in)  :: ws      !  Description and declaration in esm_alloc_real.f90
    real(fp)    , intent(out) :: uoff
    real(fp)    , intent(out) :: uon
    real(fp)    , intent(out) :: vcr
    logical     , intent(out) :: error
    logical     , intent(in)  :: wave
    character(*), intent(out) :: message !  Contains error message
!
! Local variables
!
    real(fp) :: a11
    real(fp) :: a22
    real(fp) :: a33
    real(fp) :: a44
    real(fp) :: a55
    real(fp) :: arg
    real(fp) :: em
    real(fp) :: eme
    real(fp) :: epst1
    real(fp) :: gamma
    real(fp) :: hs
    real(fp) :: lt
    real(fp) :: phicur
    real(fp) :: phiwav
    real(fp) :: phiwr
    real(fp) :: qbc
    real(fp) :: qbt
    real(fp) :: qbw
    real(fp) :: qsw
    real(fp) :: r
    real(fp) :: ra1
    real(fp) :: rhs13
    real(fp) :: rls
    real(fp) :: rmax
    real(fp) :: rr
    real(fp) :: s
    real(fp) :: s11b
    real(fp) :: t
    real(fp) :: t1
    real(fp) :: tp1
    real(fp) :: u1
    real(fp) :: u1strc
    real(fp) :: ua
    real(fp) :: ubw
    real(fp) :: umax
    real(fp) :: veff
    real(fp) :: vr
!
!! executable statements -------------------------------------------------------
!
    error = .false.
    message = ''
    !
    ! CALCULATE BED LOAD TRANSPORT
    !
    ! Note: formulation used depends on presence of waves in the
    ! simulation.
    ! If no waves then use traditional Van Rijn formulation
    ! If waves then use new parameterization which includes wave asymmetry
    ! Note: The two methods are known to give different results
    ! (order factor 2) for situations without waves
    !
    if (wave) then
       !
       ! Use Van Rijn's parameterisation including wave asymmetry
       ! effects
       ! Velocity components w.r.t. U-V grid, velocity direction in
       ! degrees (0.,360.)
       !
       s = rhosol/rhowat
       !
       ! Calculate imaginary "depth-averaged current" which has a
       ! logarithmic velocity profile, and a velocity at the bottom
       ! zeta point equivalent to that calculated by the model for
       ! 3D current and waves.
       !
       vr = u2dh
       !
       ! Calculate critical (depth averaged) velocity
       !
       if (d50 >= 0.0005_fp) then
          vcr = 8.5_fp *(d50)**0.6_fp*log10(4.0_fp*h1/d90)
       elseif (d50 > 0.0001_fp) then
          vcr = 0.19_fp*(d50)**0.1_fp*log10(4.0_fp*h1/d90)
       else
          message = 'd50 < 0.0001 not allowed'
          error = .true.
          return
       endif
       phicur = atan2(vvv, uuu)/degrad
       if (phicur < 0.0_fp) phicur = phicur + 360.0_fp
       if (tp > 1.0_fp) then
          phiwav = teta
          phiwr  = (phiwav - phicur)*degrad
          phiwav = phiwav*degrad
          !
          ! Calculate Uon and Uoff, asymmetry
          !
          hs    = hrms*sqrt(2.0_fp)
          tp1   = tp
          rls   = max(rlabda,1.0e-12_fp)
          arg = 2.0_fp * pi * h1 / rls
          !
          rhs13 = hs/rls
          if (arg > 20.0_fp) then
             ubw = 0.0_fp
          else
             ubw = pi*hs/tp1/sinh(arg)
          endif
          rr   = 0.75_fp - 0.1_fp*tanh(2.5_fp*rhs13 - 1.4_fp)
          umax = rr*2.0_fp*ubw
          t1   = tp1*sqrt(ag/h1)
          u1   = umax/sqrt(ag*h1)
          a55  = 0.0032_fp*t1**2 + 0.00008_fp*t1**3
          if (t1 > 20.0_fp) a55 = 0.0056_fp*t1**2 - 0.00004_fp*t1**3
          !
          ! Only continue at 20; not at 30 as used in the formulations
          !
          a44 = -15.0_fp + 1.35_fp*t1
          if (t1 > 15.0_fp) a44 = -2.7_fp + 0.53_fp*t1
          epst1 = t1 - 100.0_fp/9.0_fp
          if (abs(epst1) < 0.001_fp) then
             !
             ! Approximation of ra1 for t1--->100/9 (Henri Petit).
             !
             ra1 = 0.5_fp + 368.0_fp/729.0_fp*u1 - 7.0_fp/1458.0_fp*u1**2                &
                 & + (-1667.0_fp/16200.0_fp*u1**2 + 7.0_fp/3240.0_fp*u1**3            &
                 &    + 68.0_fp/675.0_fp*u1                         )*epst1     &
                 & + (11.0_fp/1875.0_fp*u1 - 37039.0_fp/720000.0_fp*u1**2             &
                 &    + 1667.0_fp/36000.0_fp*u1**3 - 7.0_fp/9600.0_fp*u1**4)*epst1**2
          else
             a33 = (0.5_fp - a55)/(a44 - 1.0_fp + exp( - a44))
             a22 = a33*a44 + a55
             a11 = 0.5_fp - a33
             ra1 = a11 + a22*u1 + a33*exp( - a44*u1)
          endif
          rmax = -2.50_fp*h1/rls + 0.85_fp
          ra1  = min(ra1, rmax)
          if (ra1 >= 0.75_fp) then
             rmax = 0.75_fp
          elseif (ra1 <= 0.62_fp) then
             rmax = 0.62_fp
          else
          endif
          uon  = umax*(0.5_fp + (rmax - 0.5_fp)*tanh((ra1 - 0.5_fp)/(rmax - 0.5_fp)))
          uon  = max(0.5_fp*umax, uon)
          uoff = umax - uon
       else
          uon  = 0.0_fp
          uoff = 0.0_fp
       endif
       !
       ! compute transport rates
       !
       veff = sqrt(vr*vr + uon*uon)
       if (veff - vcr > eps) then
          eme = (veff - vcr)**2/((s - 1.0_fp)*ag*d50)
          em  = (veff*veff)/((s - 1.0_fp)*ag*d50)
          !
          ! Total bed-load transport qbt, due to currents qbc, and waves qbw
          !
          qbt = 0.006_fp * rhosol * ws * d50 * sqrt(em) * (eme)**0.7_fp
          !
          if (vr - vcr <= 0.0_fp) then
             r = ((abs(uon) - vcr)/0.001_fp)**3
          else
             r = ((abs(uon) - vcr)/(vr - vcr))**3
          endif
          if (r >= 100.0_fp) then
             qbc = 0.0_fp
             qbw = qbt
          elseif (r <= 0.01_fp) then
             qbc = qbt
             qbw = 0.0_fp
          else
             qbc = qbt/sqrt(1.0_fp + r*r + 2.0_fp*r*cos(phiwr))
             qbw = r*qbc
          endif
       else
          r   = 0.0_fp
          qbt = 0.0_fp
          qbc = 0.0_fp
          qbw = 0.0_fp
       endif
       !
       ! Bed load due to currents
       !
       !
       ! Calculate vector components, assuming bed load is in same
       ! direction as the current direction
       !
       if (umod > eps) then
          qbcu = qbc*uuu/umod
          qbcv = qbc*vvv/umod
	   !
       !Following `elseif` should be studied in detail (UNST-7050)
       !	   
       !elseif (qbc > 1.0e-4_fp) then
       !   message = 'umod<eps and qbc>1e-4'
       !   error = .true.
       else
          qbcu = 0.0_fp
          qbcv = 0.0_fp
       endif
       !
       ! Bed load due to waves
       !
       ! Calculate vector components, assuming bed load is in same
       ! direction as the wave direction. Adjust for user-specified
       ! tuning parameter BEDW (default value 1.0) read from
       ! morph.inp file
       !
       if (tp > 1.0_fp) then
          qbwu = qbw*cos(phiwav)
          qbwv = qbw*sin(phiwav)
       else
          qbwu = 0.0_fp
          qbwv = 0.0_fp
       endif
       !
       ! Suspended transport qsw due to waves, oriented in wave
       ! direction
       !
       if (tp>1.0_fp .and. r>0.01_fp) then
          gamma = 0.2_fp
          ua = (uon**4 - uoff**4)/(uon**3 + uoff**3)
          lt = 0.007_fp*rhosol*d50*em
          !
          ! Tuning parameter SUSW read from morph.inp file
          ! Van Rijn suggests default of 0.5 for field cases, 1.0 for flumes
          !
          qsw  = gamma*ua*lt
          qswu = qsw*cos(phiwav)
          qswv = qsw*sin(phiwav)
       else
          qsw  = 0.0_fp
          qswu = 0.0_fp
          qswv = 0.0_fp
       endif
    else
       ! no waves
       !
       ! Calculate bed load transport by original formulation
       ! Note: bed slope effect on tau critical ignored,
       ! taucrb set = taucr above
       !
       ! Recalculate Van Rijn's T parameter for bed load transport.
       !
       t = taurat - 1.0_fp
       t = max(1.0e-10_fp, t)
       !
       ! Calculate magnitude of bed load, assuming a horizontal bed
       ! following Van Rijn (1993) equation 7.2.45
       !
       u1strc = ustarc * muc**0.5_fp
       !
       ! Note bed-load transport formula changed on advice of Van Rijn
       !
       s11b = 0.5_fp * rhosol * d50 * u1strc * dstar**( - 0.3_fp) * t
       !
       ! Calculate vector components, assuming bed load is in same direction
       ! as the near-bed velocity
       !
       if (umod > eps) then
          qbcu = s11b*uuu/umod
          qbcv = s11b*vvv/umod
       else
          qbcu = 0.0_fp
          qbcv = 0.0_fp
       endif
       qbwu = 0.0_fp
       qbwv = 0.0_fp
       qswu = 0.0_fp
       qswv = 0.0_fp
       !
       uon  = 0.0_fp
       uoff = 0.0_fp
       vcr  = 0.0_fp
    endif
    !
    ! end of computing bed-load transport on flat plane
    !
end subroutine bedtr1993
