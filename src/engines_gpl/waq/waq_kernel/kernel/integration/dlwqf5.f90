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


    subroutine dlwqf5 (lunrep, nocons, coname, cons, ioptpc, &
            &                    iter, tol, iscale, litrep, noseg, &
            &                    noq3, noq, nobnd, novec, nomat, &
            &                    nolay, intsrt, intopt)

        !     Deltares - Delft Software Department

        !     Created   : December 96 by Jan van Beek

        !     Function  : Initialise numerical parameters for GMRES Fast Solver
        !                 from user input-parameters or from the default values.

        !     Modified  : feb. 1997, Jan van Beek: set prec. switch equal 3 default
        !                                          added iteration report
        !                 July 2009, Leo Postma  : allocation double precission arrays

        use timers                         ! WAQ performance timers

        implicit none

        !     Arguments           :

        !     Kind           Function         Name             Description

        integer(kind = int_wp), intent(in) :: lunrep         ! Unit number report file
        integer(kind = int_wp), intent(in) :: nocons         ! Number of constants used
        character(20), intent(in) :: coname(nocons) ! Constant names
        real(kind = real_wp), intent(in) :: cons  (nocons) ! Model constants
        integer(kind = int_wp), intent(out) :: ioptpc         ! Preconditioner switch, 0 = none,
        ! 1 = GS (L), 2 = GS (U),3 = SSOR
        integer(kind = int_wp), intent(out) :: iter           ! Maximum number of iterations
        real(kind = dp), intent(out) :: tol            ! Relative tolerance
        integer(kind = int_wp), intent(out) :: iscale         ! Row scaling switch [0 = no, 1 =yes]
        logical, intent(out) :: litrep         ! Switch on reporting iterarions
        integer(kind = int_wp), intent(in) :: noseg          ! Number of computational volumes
        integer(kind = int_wp), intent(in) :: noq3           ! Number of exchange surfaces in 3rd direction
        integer(kind = int_wp), intent(in) :: noq            ! total number of exchange surfaces
        integer(kind = int_wp), intent(in) :: nobnd          ! Number of open boundaries
        integer(kind = int_wp), intent(in) :: novec          !
        integer(kind = int_wp), intent(in) :: nomat          ! size of matrix with off-diagonals
        integer(kind = int_wp), intent(out) :: nolay          ! number of layers
        integer(kind = int_wp), intent(in) :: intsrt         ! integration type
        integer(kind = int_wp), intent(in) :: intopt         ! integration option

        !     Local declarations

        integer(kind = int_wp) :: ierr               ! Error count
        integer(kind = int_wp) :: defopt = 3    ! Default preconditioner switch
        integer(kind = int_wp) :: defite = 100    ! Default maximum number of iterations
        integer(kind = int_wp) :: defsca = 1    ! Default value for row scaling
        real(kind = dp) :: deftol = 1.D-7    ! Default tolerance value
        integer(kind = int_wp) :: defrep = 0    ! Default value for iteration report
        integer(kind = int_wp) :: idef, itrep        ! Help variables
        character(20) defnam             ! Help string
        integer(kind = int_wp) :: noth               ! Number of available threads

        !     The WAQ-timer

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwqf5", ithandl)

        !     look for unstructured setting, this is misuse of nolay, fractim depends also on this

        if (btest(intopt, 15)) then
            nolay = 1
        endif

        !     Some initialisations

        ierr = 0
        write (lunrep, *)
        write (lunrep, 2000)

        !     Look for preconditioner switch

        write (lunrep, *)
        defnam = 'swprecond'
        idef = index_in_array(defnam, coname)
        if (idef > 0) then
            ioptpc = nint(cons(idef))
            write (lunrep, 2010)
        else
            ioptpc = defopt
            write (lunrep, 2020)
        endif

        !     Check value

        select case (ioptpc)
        case (0)   ; write (lunrep, 2030) ioptpc
        case (1)   ; write (lunrep, 2040) ioptpc
        case (2)   ; write (lunrep, 2050) ioptpc
        case (3)   ; write (lunrep, 2060) ioptpc
        case default ; ierr = ierr + 1
        write (lunrep, 2070) ioptpc
        end select

        !     Look for maximum number of iterations

        write (lunrep, *)
        defnam = 'maxiter'
        idef = index_in_array(defnam, coname)
        if (idef > 0) then
            iter = nint(cons(idef))
            write (lunrep, 2080)
        else
            iter = defite
            write (lunrep, 2090)
        endif

        !     Check value

        if (iter > 0) then ; write (lunrep, 2100) iter
        else                    ; ierr = ierr + 1
        write (lunrep, 2110) iter
        endif

        !     Nr. of vectors

        write (lunrep, 2260) novec

        !     Look for the relative tolerance

        write (lunrep, *)
        defnam = 'tolerance'
        idef = index_in_array(defnam, coname)
        if (idef > 0) then
            tol = cons(idef)
            write (lunrep, 2120)
        else
            tol = deftol
            write (lunrep, 2130)
        endif

        !     Check value

        if (tol  > 0.0) then ; write (lunrep, 2140) tol
        else                      ; ierr = ierr + 1
        write (lunrep, 2150) tol
        endif

        !     Look for the row scaling switch

        write (lunrep, *)
        defnam = 'swscale'
        idef = index_in_array(defnam, coname)
        if (idef > 0) then
            iscale = nint(cons(idef))
            write (lunrep, 2160)
        else
            iscale = defsca
            write (lunrep, 2170)
        endif

        !     Check value

        select case (iscale)
        case (0)   ; write (lunrep, 2180) iscale
        case (1)   ; write (lunrep, 2190) iscale
        case default ; ierr = ierr + 1
        write (lunrep, 2200) iscale
        end select

        !     Look for the iteration report flag

        write (lunrep, *)
        defnam = 'iteration report'
        idef = index_in_array(defnam, coname)
        if (idef > 0) then
            itrep = nint(cons(idef))
            write (lunrep, 2210)
        else
            itrep = defrep
            write (lunrep, 2220)
        endif

        !     Check value

        select case (itrep)
        case (0)   ; write (lunrep, 2230) itrep
        litrep = .false.
        case (1)   ; write (lunrep, 2240) itrep
        litrep = .true.
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
