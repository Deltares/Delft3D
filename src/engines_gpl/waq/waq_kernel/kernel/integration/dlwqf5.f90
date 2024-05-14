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
module m_dlwqf5
    use m_waq_precision
    use m_string_utils

    implicit none

    contains


    !> Initialise numerical parameters for Generalized Minimal Residual (GMRES) method
    !! from user input-parameters (if found) or from the default values (if not user-defined).
    !! The numerical parameters assigned are:
    !!    * Preconditioner: 0 = none, 1 = GS (Lower), 2 = GS (Upper), 3 = (default) Symmetric-> SSOR (Lower and Upper)
    !!    * Maximum number of iterations: default = 100
    !!    * Relative tolerance: default = 1.D-7
    !!    * Row scaling: 0 = no, 1 = (default) yes
    !!    * Generate iteration report: true or false
    subroutine initialize_gmres(lunrep, nocons, coname, cons, preconditioner, &
            &                   max_iterations, rel_tolerance, row_scaling, report_iterations, noseg, &
            &                   noq3, noq, novec, matrix_size, &
            &                   nolay, integration_type, integration_option)

        use timers                         ! WAQ performance timers

        implicit none

        integer(kind = int_wp), intent(in)  :: lunrep             ! Unit number report file
        integer(kind = int_wp), intent(in)  :: nocons             ! Number of constants used
        character(20),          intent(in)  :: coname(nocons)     ! Constant names
        real(kind = real_wp),   intent(in)  :: cons  (nocons)     ! Model constants
        integer(kind = int_wp), intent(out) :: preconditioner     ! Preconditioner switch:
                                                                  ! 0 = none
                                                                  ! 1 = GS (L)
                                                                  ! 2 = GS (U)
                                                                  ! 3 = SSOR
        integer(kind = int_wp), intent(out) :: max_iterations     ! Maximum number of iterations
        real(kind = dp),        intent(out) :: rel_tolerance      ! Relative tolerance
        integer(kind = int_wp), intent(out) :: row_scaling        ! Row scaling switch [0 = no, 1 =yes]
        logical,                intent(out) :: report_iterations  ! Switch on reporting iterarions
        integer(kind = int_wp), intent(in)  :: noseg              ! Number of cells or computational volumes
        integer(kind = int_wp), intent(in)  :: noq3               ! Number of exchange surfaces in 3rd direction
        integer(kind = int_wp), intent(in)  :: noq                ! total number of exchange surfaces
        integer(kind = int_wp), intent(in)  :: novec              ! vector_count
        integer(kind = int_wp), intent(in)  :: matrix_size        ! size of matrix with off-diagonals
        integer(kind = int_wp), intent(out) :: nolay              ! number of layers
        integer(kind = int_wp), intent(in)  :: integration_type   ! integration type
        integer(kind = int_wp), intent(in)  :: integration_option ! integration option

        !     Local declarations
        integer(kind = int_wp) :: ierr                          ! Error count
        integer(kind = int_wp) :: default_preconditioner = 3    ! Default preconditioner switch
        integer(kind = int_wp) :: default_max_iterations = 100  ! Default maximum number of iterations
        integer(kind = int_wp) :: default_row_scaling = 1       ! Default value for row scaling
        real(kind = dp)        :: default_rel_tolerance = 1.D-7 ! Default value for relative tolerance
        integer(kind = int_wp) :: default_report_iterations = 0 ! Default value for iteration report
        integer(kind = int_wp) :: idef, itrep                   ! Auxiliary variables
        character(20) defnam                                    ! Auxiliary string
        integer(kind = int_wp) :: threads_count                 ! Number of available threads

        !     The WAQ-timer
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwqf5", ithandl)

        ! look for unstructured setting, this is misuse of nolay, fractim depends also on this
        if (btest(integration_option, 15)) then
            nolay = 1
        end if

        ! Some initialisations
        ierr = 0
        write (lunrep, *)
        write (lunrep, 2000)

        ! Preconditioner switch
        write (lunrep, *)
        defnam = 'swprecond'
        idef = index_in_array(defnam, coname)
        if (idef > 0) then
            preconditioner = nint(cons(idef))
            write (lunrep, 2010)
        else
            preconditioner = default_preconditioner
            write (lunrep, 2020)
        end if
        ! Check value
        select case (preconditioner)
        case (0)   ; write (lunrep, 2030) preconditioner
        case (1)   ; write (lunrep, 2040) preconditioner
        case (2)   ; write (lunrep, 2050) preconditioner
        case (3)   ; write (lunrep, 2060) preconditioner
        case default ; ierr = ierr + 1
        write (lunrep, 2070) preconditioner
        end select

        ! Maximum number of iterations
        write (lunrep, *)
        defnam = 'maxiter'
        idef = index_in_array(defnam, coname)
        if (idef > 0) then
            max_iterations = nint(cons(idef))
            write (lunrep, 2080)
        else
            max_iterations = default_max_iterations
            write (lunrep, 2090)
        end if
        ! Check value
        if (max_iterations > 0) then
            write (lunrep, 2100) max_iterations
        else
            ierr = ierr + 1
            write (lunrep, 2110) max_iterations
        end if

        ! Number of vectors
        write (lunrep, 2260) novec

        ! Relative tolerance
        write (lunrep, *)
        defnam = 'tolerance'
        idef = index_in_array(defnam, coname)
        if (idef > 0) then
            rel_tolerance = cons(idef)
            write (lunrep, 2120)
        else
            rel_tolerance = default_rel_tolerance
            write (lunrep, 2130)
        end if
        ! Check value
        if (rel_tolerance  > 0.0) then
            write (lunrep, 2140) rel_tolerance
        else
            ierr = ierr + 1
            write (lunrep, 2150) rel_tolerance
        end if

        ! Look for the row scaling switch
        write (lunrep, *)
        defnam = 'swscale'
        idef = index_in_array(defnam, coname)
        if (idef > 0) then
            row_scaling = nint(cons(idef))
            write (lunrep, 2160)
        else
            row_scaling = default_row_scaling
            write (lunrep, 2170)
        end if
        ! Check value
        select case (row_scaling)
        case (0)   ; write (lunrep, 2180) row_scaling
        case (1)   ; write (lunrep, 2190) row_scaling
        case default ; ierr = ierr + 1
        write (lunrep, 2200) row_scaling
        end select

        ! Iteration report flag
        write (lunrep, *)
        defnam = 'iteration report'
        idef = index_in_array(defnam, coname)
        if (idef > 0) then
            itrep = nint(cons(idef))
            write (lunrep, 2210)
        else
            itrep = default_report_iterations
            write (lunrep, 2220)
        end if
        !     Check value
        select case (itrep)
        case (0)   ; write (lunrep, 2230) itrep
        report_iterations = .false.
        case (1)   ; write (lunrep, 2240) itrep
        report_iterations = .true.
        case default ; ierr = ierr + 1
        write (lunrep, 2250) itrep
        end select

        !     Close timer and return
        if (timon) call timstop (ithandl)
        return

        !     Formats
        2000 format(' Initialising numerical options for method 15...18')
        2010 format(' Preconditioner switch found in input')
        2020 format(' Preconditioner switch not found, using default')
        2030 format(' switch = ', I1, ', corrections based on previous operand')
        2040 format(' switch = ', I1, ', corrections based on lower triangular')
        2050 format(' switch = ', I1, ', corrections based on upper triangular')
        2060 format(' switch = ', I1, ', corrections based on lower and upper ')
        2070 format(' ERROR switch =', I6, ' out of range [0-3]')
        2080 format(' Maximum number of iterations found in input')
        2090 format(' Maximum number of iterations not found, using default')
        2100 format(' Maximum number of iterations set to :', I6)
        2110 format(' ERROR maximum number out of range :', I10)
        2120 format(' Relative tolerance found in input')
        2130 format(' Relative tolerance not found, using default')
        2140 format(' Relative tolerance set to :', E10.3)
        2150 format(' ERROR Relative tolerance out of range :', E10.3)
        2160 format(' Scaling switch found in input')
        2170 format(' Scaling switch not found, using default')
        2180 format(' switch = ', I1, ', scaling is switched off')
        2190 format(' switch = ', I1, ', scaling is switched on')
        2200 format(' ERROR switch =', I6, ' out of range [0-1]')
        2210 format(' Iteration report switch found in input')
        2220 format(' Iteration report switch not found, using default')
        2230 format(' switch = ', I1, ', iteration report is switched off')
        2240 format(' switch = ', I1, ', iteration report is switched on')
        2250 format(' ERROR switch =', I6, ' out of range [0-1]')
        2260 format(' Maximum number of vectors is:', I6)

    end

end module m_dlwqf5
