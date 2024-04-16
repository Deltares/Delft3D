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

module m_sysn
    !   System characteristics

    integer :: noseg    !  number of elements
    integer :: nolay    !  number of layers in the water
    integer :: nseg2    !  number of bottom elements
    integer :: nosys    !  number of active systems
    integer :: notot    !  total number of systems
    integer :: nodisp   !  number of dispersion arrays
    integer :: novelo   !  number of velocity arrays
    integer :: noq1     !  number of exch. 1st direction
    integer :: noq2     !  number of exch. 2nd direction
    integer :: noq3     !  number of exch. 3rd direction
    integer :: noq4     !  number of exch. bottom direction
    integer :: noq      !  number of exchanges
    integer :: nodump   !  number of points monit. outp.
    integer :: nobnd    !  number of boundaries
    integer :: nowst    !  number of waste loads
    integer :: nocons   !  number of constants
    integer :: nopa     !  number of parameters
    integer :: nofun    !  number of functions
    integer :: nosfun   !  number of segment functions
    integer :: nharms   !  total space  of harmonics
    integer :: niharm   !  total number of harmonics
    integer :: nlines   !  cumulative record space
    integer :: npoins   !  cumulative pointer space
    integer :: nolun    !  number of unit numbers
    integer :: nx       !  width of grid
    integer :: ny       !  depth of grid
    integer :: noitem   !  number of items with time-functions
    integer :: jtrack   !  number of codiagonals
    integer :: nipmsa   !  Length IPMSA
    integer :: nproc    !  Number of called processes
    integer :: noloc    !  Number of local vars in the proces subsystem
    integer :: nflux    !  total number of fluxes
    integer :: nodef    !  Number of defaults in proces subsystem
    integer :: noutp    !  Number of files in OUTPUT system
    integer :: nrvart   !  Number of extra output variables
    integer :: nbufmx   !  Length output buffer
    integer :: ifiopk   !  File option kenmerk array
    integer :: ipbloo   !  Number of Bloom module (if >0)
    integer :: ioffbl   !  Offset in IPMSA for Bloom
    integer :: ndspx    !  Number of extra dispersion array's
    integer :: nvelx    !  Number of extra velocity array's
    integer :: nlocx    !  Number of local variables on exch. level
    integer :: ndspn    !  Number of new dispersion array's
    integer :: nveln    !  Number of new velocity array's
    integer :: ndmpar   !  Number of dump area's for balance output
    integer :: ndmpq    !  Number of exchanges of interest for balance
    integer :: ndmps    !  Number of segments of interest for balance
    integer :: ntdmpq   !  Number of times exchanges contribute to balance
    integer :: ntdmps   !  Number of times segments contribute to balance
    integer :: ncbufm   !  Length character array buffer.
    integer :: noraai   !  Number of raaien
    integer :: ntraaq   !  Total number of times exchanges contribute to raai
    integer :: newrsp   !  Integer array size for new time functions
    integer :: newisp   !  Real    array size for new time functions
    integer :: nobtyp   !  Nr of boundary  types
    integer :: nowtyp   !  Nr of wasteload types
    integer :: nomat    !  Size of the fast solvers matrix
    integer :: novec    !  Number of fast solve vectors
    integer :: mmax     !  Number of columns in regular grid
    integer :: nmax     !  Number of rows in regular grid
    integer :: kmax     !  Number of layers in regular grid
    integer :: novar    !  Number of variables
    integer :: noarr    !  Number of array's in workspace
    integer :: nogrid   !  Number of defined grids
    integer :: nufil    !  Number of extra unformatted files
    integer :: nrref    !  Maximum nr of input references for processes
    integer :: nothrd   !  Nr of threads to be used for parallel processing
    integer :: nototp   !  Total number of substances inclusive of particle tracking

    integer, parameter :: insize = 72

    common  /  sysn   / noseg, nolay, nseg2, nosys, notot, &
            nodisp, novelo, noq1, noq2, noq3, &
            noq4, noq, nodump, nobnd, nowst, &
            nocons, nopa, nofun, nosfun, nharms, &
            niharm, nlines, npoins, nolun, nx, &
            ny, noitem, jtrack, nipmsa, nproc, &
            noloc, nflux, nodef, noutp, nrvart, &
            nbufmx, ifiopk, ipbloo, ioffbl, &
            ndspx, nvelx, nlocx, ndspn, &
            nveln, ndmpar, ndmpq, ndmps, ntdmpq, &
            ntdmps, ncbufm, noraai, ntraaq, newrsp, &
            newisp, nobtyp, nowtyp, nomat, novec, &
            mmax, nmax, kmax, novar, noarr, &
            nogrid, nufil, nrref, &
            nothrd, nototp

    integer :: in(insize)
    equivalence       (in(1), noseg)
end module m_sysn
