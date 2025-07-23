!!  Copyright (C)  Stichting Deltares, 2012-2025.
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
module m_setabc
    use m_waq_precision

    implicit none

contains


    !  *********************************************************************
    !  *          SUBROUTINE TO SET MATRIX A AND B                         *
    !  *********************************************************************

    subroutine setabc(xinit, extb, exttot, csol, dsol, t, dep, nset)

        use m_maxprd
        use bloom_data_dim
        use bloom_data_size
        use bloom_data_arran
        use bloom_data_matrix
        use bloom_data_io
        use bloom_data_phyt

        implicit none

        save

        real(kind = dp) :: xinit(*), pmax20(mt), tcorr(mt), sdmixn(mt)
        real(kind = dp) :: csol, dsol, dep, extb, exttot, t
        integer(kind = int_wp) :: k, j, nset, idrem, idprev, imprev

        !
        !  If this is the first time through the subroutine,
        !  then initiate A, B and C
        !

        !jvb  , perform this every time for Ulva
        !
        idrem = idump
        idump = 0
        call maxprd(tefcur)
        do k = 1, nuspec
            pmax20(k) = pmax(k)
        end do
        call maxprd(t)
        idump = idrem
        !
        idrem = 0
        do k = 1, nuspec
            if (sdmix(k) < 0.0) then
                sdmixn(k) = 1.0d0 + sdmix(k)
                dmix(k) = abs(sdmix(k)) * dep
                idrem = idump
            else
                sdmixn(k) = 0.0d0
            end if
        end do
        !jvb
        nset = nset + 1
        if (nset <= 1) then
            !  Initialize "C" values for all species to 1.0: maximize.
            !  (See also subroutines SOLVLP and MAXGROGR)
            do j = 1, nuspec
                c(j) = 1.0
            end do

            !  Initiate multiplier for exponential term at zero:
            !  start with steady state solution for the dead algal pool
            if (idrem /= 0) then
                write (outdbg, 99996) (abs(sdmix(k)), k = 1, nuspec)
                write (outdbg, 99995) (sdmixn(k), k = 1, nuspec)
            end if
        end if

        !  Convert CSOL from:
        !  Joules per cm2 per week to Joules per m2 per day.
        !  Determine temperature correction, assuming that the nominal
        !  efficiency curves are all for temperatures of TEFCUR deg. centigrade.
        dsol = 1428.57 * csol

        !  Compute equivalent radiation level.
        !  Multiply by the light reduction of overlying water columns. Usually
        !  this factor is 1.0 as SDMIXN = 0.0; for types attached to the bottom
        !  (Ulva) this factor is not 1.0, however.
        do k = 1, nuspec
            tcorr(k) = pmax20(k) / pmax(k)
        end do
        do k = 1, nuspec
            surf(k) = tcorr(k) * dsol * dexp (- exttot * sdmixn(k) * dep)
        end do
        if (idump == 1) write (outdbg, 99997) (tcorr(k), k = 1, nuspec)

        !  Set "B" values for nutrients by substracting the amount in
        !  zooplankton from the input values and correcting for deviations
        !  from steady state if option DYNADEAD was selected
        170 do k = 1, nunuco
            b(k) = concen(k)
        end do

        !  Formats for this subroutine
        99997 format ('  Tcorr(j):  ', 30(F5.2, 1X))
        99996 format (//, 1X, 'Computation with inhomogeneous mixing.', /, '  SDMIX(J):   ', 30(F5.2, 1X))
        99995 format ('  SDMIXN(J):  ', 30(F5.2, 1X))
        return
    end

end module m_setabc
