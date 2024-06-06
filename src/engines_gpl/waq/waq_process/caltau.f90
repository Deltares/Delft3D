!!  Copyright (C)  Stichting Deltares, 2012-2024.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

module m_caltau
    implicit none
    private

    public :: caltau

    contains

    subroutine caltau(pmsa, fl, ipoint, increm, segment_count, &
                      noflux, iexpnt, iknmrk, noq1, noq2, &
                      noq3, noq4)
        !< Calculation of bottom friction
        !<
        !< Remarks:
        !<
        !< Calculate shear stress, caused by flow, wind and human activities
        !< (ships, dredging). Model is vertically averaged.
        !<
        !< The subroutine also produces a calculated stream velocity
        !< based on tau-total for comparison with critical stream velocities
        !< instead of critical values for tau (delta study, t692).

        implicit none

        ! arguments of the subroutine
        real     :: pmsa(*), fl(*)
        integer  :: ipoint(*), increm(*), segment_count, noflux, &
                  iexpnt(4, *), iknmrk(*), noq1, noq2, noq3, noq4

        ! process-specific variable
        real     :: h           !< Significant wave height                              [m]
        real     :: rl          !< Significant wave length                              [m]
        real     :: t           !< Significant wave period                              [s]
        real     :: chz         !< Chezy coefficient                            [sqrt(m)/s]
        real     :: depth       !< Water depth                                          [m]
        real     :: totdep      !< Total water depth                                    [m]
        real     :: tauwin      !< Shearstress by wind                     [kg/m/s2 = N/m2]
        real     :: tauflo      !< Shearstress by flow                               [N/m2]
        real     :: tausch      !< Shearstress by ships and human activity           [N/m2]
        real     :: tau         !< Total shearstress                                 [N/m2]
        real     :: tauvel      !< Calculated velocity based on TAU                   [m/s]
        real     :: veloc       !< Velocity                                           [m/s]
        real     :: max_nelson  !< Quotient between maximum wave height and total depth [-]
        integer  :: iswtau      !< Switch to indicate wave shear sterss formulation     [-]
                                !< 1 => Tamminga, 2 => Swart, 3 => Soulsby
        integer  :: iswtauveloc !< Switch shear stress by flow                          [-]
                                !< 1 => calculate, 2 => use value from input
        integer  :: iswtaumax   !< Switch for factor in shear stress formula            [-]
                                !< 1 => 0.5, any other value => 0.25
        integer  :: iswhrms     !< Switch for wave height: 1 => Hs, 2 => HRMS           [-]

        ! Other local variables
        integer  :: params_count                       !< Number of parameters (input+output variables) in this process
        integer  :: iflux                              !< Index fluxes
        integer  :: isegment                           !< index of current segment
        integer, dimension(:), allocatable  :: iparray !< Array with integer pointers for parameters


        call initialize_variables(params_count, iparray, ipoint, iflux)
        do isegment = 1, segment_count
            if(must_calculate_segment(iknmrk(isegment))) then
                call assign_input_params(iparray, pmsa, h, rl, t, tausch, iswtauveloc, tauflo, &
                                         veloc, chz, totdep, iswtau, depth, iswtaumax, max_nelson, &
                                         iswhrms)
                call validate_switches(iswtau, iswtauveloc, iswhrms)
                call calculate_process_in_segment(h, max_nelson, totdep, chz, depth, iknmrk(isegment), &
                                                  tauflo, veloc, tauwin, rl, t, tauvel, iswtau, &
                                                  iswtaumax, iswtauveloc, iswhrms, tau, tausch)
                call assign_output_params(iparray, pmsa, tau, tauflo, tauwin, tauvel)
            end if
            call update_loop_vars(iflux, noflux, params_count, iparray, increm)
        end do
    end subroutine caltau

    subroutine initialize_variables(count_params, iparray, ipoint, iflux)
        !< Initializes arrays and other variables.
        integer, intent(inout) :: count_params, iflux
        integer, allocatable, intent(out) :: iparray(:)
        integer, intent(in) :: ipoint(*)

        count_params = 18
        allocate(iparray(1:count_params))
        iparray(:) = ipoint(1:count_params)
        iflux = 0
    end subroutine initialize_variables

    subroutine assign_input_params(iparray, pmsa, h, rl, t, tausch, iswtauveloc, tauflo, &
                                  veloc, chz, totdep, iswtau, depth, iswtaumax, max_nelson, &
                                  iswhrms)
        !< Transfer values from generic array to process-specific input parameters.
        real, intent(out)    :: h, rl, t, tausch,tauflo, veloc, chz, totdep, depth, max_nelson
        integer, intent(out) :: iswtauveloc, iswtau, iswtaumax, iswhrms
        real, intent(in)     :: pmsa(*)
        integer, intent(in)  :: iparray(*)

        h           = pmsa(iparray(1))
        rl          = pmsa(iparray(2))
        t           = pmsa(iparray(3))
        tausch      = pmsa(iparray(4))
        iswtauveloc = nint(pmsa(iparray(5)))
        tauflo      = pmsa(iparray(6))
        veloc       = pmsa(iparray(7))
        chz         = pmsa(iparray(8))
        totdep      = pmsa(iparray(9))
        iswtau      = nint(pmsa(iparray(10)))
        depth       = pmsa(iparray(11))
        iswtaumax   = nint(pmsa(iparray(12)))
        max_nelson  = pmsa(iparray(13))
        iswhrms     = pmsa(iparray(14))
    end subroutine assign_input_params

    subroutine validate_switches(switch_tau, switch_tau_velocity, switch_hrms)
        !< Evaluates, based on switches, whether the proces calculation must be carried out or not. If not, it immediately stops entire calculation.
        use m_logger_helper, only : write_error_message

        integer, intent(in) :: switch_tau, switch_tau_velocity, switch_hrms

        if ( ALL( [ 1, 2, 3 ] /= switch_tau ) ) then
            call write_error_message('invalid switch for tau (SwTau) in caltau')
        end if
        if ( ALL( [ 1, 2 ] /= switch_tau_velocity ) ) then
            call write_error_message('invalid switch for tau (SwTauVeloc) in caltau')
        end if
        if ( ALL( [ 1, 2 ] /= switch_hrms ) ) then
            call write_error_message('invalid switch for tau (SwHrms) in caltau')
        end if
    end subroutine validate_switches

    subroutine calculate_process_in_segment(h, max_nelson, totdep, chz, depth, segment_attribute, tauflo, &
                                            veloc, tauwin, rl, t, tauvel, iswtau, iswtaumax, iswtauveloc, &
                                            iswhrms, tau, tausch)
        !< Carry out all process-specific calculations.
        use m_evaluate_waq_attribute, only : extract_waq_attribute

        real, intent(in)    :: rl, depth, max_nelson, t, tausch, totdep, veloc
        integer, intent(in) :: segment_attribute, iswtau, iswtaumax, iswtauveloc, iswhrms
        real, intent(inout) :: h, chz, tauflo
        real, intent(out)   :: tau, tauvel, tauwin

        integer :: ikmrk2
        real :: rough, chz3d, karmc1, karmc2
        real, parameter :: karman = 0.41,   &
                           gravity = 9.811, & !< Acceleration of gravity  [m/s2]
                           rhow = 1000.0      !< Density of water        [kg/m3]

        call extract_waq_attribute(2, segment_attribute, ikmrk2)
        karmc1 = sqrt(gravity)/karman
        karmc2 = karman/sqrt(gravity)

        ! Interpretation of wave height: as H-significant or H-RMS
        if ( iswhrms == 2 ) then
            h = h / sqrt(2.0)
        endif

        ! Nelson criteria
        h = min(h, max_nelson*totdep)
        chz = max(1.0, chz)

        ! Calculate roughness from Chezy
        rough = 12.*totdep/(10**(chz/18.))

        ! Calculate Chezy 3D
        chz3d = chezy_3d(ikmrk2, totdep, karmc1, depth, karmc2, chz)

        ! Shear stress by flow, calculate if wanted otherwise from input
        if (iswtauveloc == 1) then
            tauflo = rhow*gravity*veloc**2/chz3d**2
        end if

        ! Shear stress by waves
        tauwin = shear_stress_wind(h, rl, t, totdep, rough, rhow, iswtau, iswtaumax)

        ! Total shear stress must actually be calculated using wind direction, which is not the case here.
        ! Re-calculate total shear stress (TAU) to a total stream velocity
        if (tausch == -1.0) then
            tau = -1.0
            tauvel = -1.0
        else
            tau = tauflo + tauwin + tausch
            tauvel = sqrt(tau*chz3d**2/(rhow*gravity))
        end if

   end subroutine calculate_process_in_segment

    subroutine assign_output_params(iparray, pmsa, tau, tauflo, tauwin, tauvel)
        !< Transfer values from the process-specific output parameters to a generic array.
        integer, intent(in) :: iparray(*)
        real, intent(in) :: tau, tauflo, tauwin, tauvel
        real, intent(out) :: pmsa(*)

        pmsa(iparray(15)) = tau
        pmsa(iparray(16)) = tauflo
        pmsa(iparray(17)) = tauwin
        pmsa(iparray(18)) = tauvel
    end subroutine assign_output_params

    subroutine update_loop_vars(iflux, noflux, count_params, iparray, increm)
        !< Update all variables for the next cell (segment) iteration.

        integer, intent(in) :: noflux, count_params
        integer, intent(inout) :: iflux, iparray(*)
        integer, intent(in) :: increm(*)

        integer :: idx

        iflux = iflux + noflux
             do idx = 1, count_params
                iparray(idx) = iparray(idx) + increm(idx)
            end do
    end subroutine update_loop_vars

    logical function must_calculate_segment(segment_attribute)
        !< Boolean indicating whether the calculation for current cell (segment) should be carried out or not. If false, then the cell is skipped.
        use m_evaluate_waq_attribute


        integer, intent(in) :: segment_attribute
        integer :: ikmrk2

        call extract_waq_attribute(2, segment_attribute, ikmrk2)

        must_calculate_segment = ((btest(segment_attribute, 0)) .and. (ikmrk2 == 0 .or. ikmrk2 == 3))
    end function must_calculate_segment

    real function wave_friction_factor(iswtau, rlf)
        !< Calculates wave friction factor using Tamminga's, Swart's or Soulsby's formulation based on value of switch iswtau.
        real, intent(in) :: rlf
        integer, intent(in) :: iswtau
        real, parameter :: pi = 3.14159265
        if (iswtau == 1) then
            ! Formulation Tamminga
            wave_friction_factor = 0.16/sqrt(rlf)
        else if (iswtau == 2) then
            ! Formulation Swart
            if (rlf > (pi/2)) then
                wave_friction_factor = 0.00251*exp(5.213*rlf**(-0.19))
            else
                wave_friction_factor = 0.32
            end if
        else if (iswtau == 3) then
            ! Formulation Soulsby
            wave_friction_factor = 0.237*rlf**(-0.52)
        end if
    end function wave_friction_factor

    real function chezy_3d(ikmrk2, totdep, karmc1, depth, karmc2, chz)
        !< Returns the 3D (equivalent) of the Chezy coefficient.
        integer, intent(in) :: ikmrk2
        real, intent(in)    :: totdep, karmc1, depth, karmc2, chz
        if (ikmrk2 == 3) then
            ! bottom cell => it must be 3d
            if (totdep > 0.0) then
                chezy_3d = karmc1*log(1 + ((0.5*depth) &
                           /(totdep*exp(-1.*(1 + (karmc2*chz)))) &
                           ))
                chezy_3d = max(1.0, chezy_3d)
            else
                chezy_3d = 1.0
            end if
        else
            chezy_3d = chz
        end if
    end function chezy_3d

    real function shear_stress_wind(h, rl, t, totdep, rough, rhow, iswtau, iswtaumax)
        !< Calculates the shear stress due to wind based on the wave characteristics.
        real, intent(in)   :: h, rl, t, totdep, rough, rhow
        integer, intent(in) :: iswtau, iswtaumax

        real  :: a6, ubg, alm, rlf, factor
        real, parameter :: pi = 3.14159265

        if (min(abs(h), abs(rl), abs(t)) < 1.e-20) then
            shear_stress_wind = 0
            return
        endif
        a6 = 2.0*pi*totdep/rl
        if (a6 <= 10.0) then
            ubg = pi*h/(t*sinh(a6))
            alm = ubg*t/(2*pi)
            rlf = alm/rough
            if (iswtaumax == 1) then
                factor = 0.5
            else
                factor = 0.25
            end if
            shear_stress_wind = wave_friction_factor(iswtau, rlf)*factor*rhow*ubg**2
        else
            shear_stress_wind = 0.0
        end if
    end function shear_stress_wind

end module m_caltau
