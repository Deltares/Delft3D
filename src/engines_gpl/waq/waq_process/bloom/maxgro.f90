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
module m_maxgro
    use m_waq_precision
    use m_bloom_3dl

    implicit none

contains


    !  *********************************************************************
    !  *      SUBROUTINE TO CALCULATE MAXIMUM ATTAINABLE EQUILIBRIUM       *
    !  *            VALUES BASED UPON THE INITIAL GROWTH RATE              *
    !  *********************************************************************

    subroutine maxgro(xinit, root, exttot, eadj, j, iskmax, dep)

        use bloom_data_3dl
        use bloom_data_size
        use bloom_data_caldynam
        use bloom_data_io
        use bloom_data_matrix
        use bloom_data_phyt

        implicit none

        real(kind = dp) :: root(*), xinit(*)
        real(kind = dp) :: effi, exttot, grlim, bt, eadj, dep
        integer(kind = int_wp) :: iskmax, j, k

        !----------------------------------------------------------------------
        ! Purpose of this subroutine: find the growth efficiency EFFI for each
        ! phytoplankton species at the current total extinction level EXTTOT.
        !
        ! get average efficiency over the layers
        if (active_efft) then
            effi = aveffi(iskmax)
        else
            if (ifix_3dl(it2(j, 1)) < 0) then
                effi = effic_3dl(iskmax, iseg_3dl)
            else
                call effi_3dl(effi, iskmax)
            endif
        endif

        ! Check whether ROOT(2) (UKmax) is larger than EXTTOT.
        if (root(2) <= exttot) then

            ! num_layers_grid {=ROOT(2)} is smaller than the total extinction, indicating that
            ! the species cannot maintain its current biomass.
            ! We do need the average efficiency
            ! EFFI, however, to compute the coefficients for the objective
            ! function.
            ! When num_layers_grid is negative for a species, its biomass should approach
            ! zero as quickly as possible. Therefore put 0.01 in the objective
            ! function.
            ! Note: do not use a negative number because the mortality constraint
            ! of the species could be positive, indicating that it should be
            ! included in the final solution.
            if (root(2) <= 0.0) then
                effi = 0.01
            end if
            if (lgroch == 1) then
                ! Compute the right hand side of the growth constraint for a species,
                ! whose num_layers_grid < EXTTOT of previous time-step. Normally we might put ANY
                ! value into the growth constraint as long as it exceeds the energy
                ! limitation level: the biomass will most likely get energy limited.
                ! However, as an extra precaution for dealing with infeasible
                ! solutions put XINIT into the growth constraint: whatever
                ! is done with the other constraints, this particular type will not
                ! increase in biomass.
                ! Check and correct for growth constraints which are lower than the
                ! mortality constraints.
                grlim = xinit(j)
                if (lmorch == 0) then
                    b(j + nuexro) = grlim
                    return
                end if
                if (grlim > b(j + nuexro + nuecog)) then
                    b(j + nuexro) = grlim
                else
                    b(j + nuexro) = b(j + nuexro + nuecog)
                end if
            end if
        else
            ! Compute value BT for the growth constraint.
            bt = dexp(((pmax(iskmax) - lpmort * rmort(iskmax)) * effi - resp(iskmax)) * tstep)
            bt = bt * xinit(j)
            ! Set the growth constraint to 0.0 when BT is negative.
            if (bt < 0.0) then
                write(outdbg, 99995) bt, grname(j)
                bt = 0.0d0
            end if
            !  Store growth constraint value in B-vector. Optionally print results to unit outdbg.
            b(j + nuexro) = bt
            if (idump /= 0) write (outdbg, 99990) grname(j), effi, effi * pmax(iskmax), b(j + nuexro)
        end if

        ! Store the nett growth rate of each phytoplankton type in the row
        ! for the objective function if growth is maximized.
        if (lobfun == 1) then
            do k = it2(j, 1), it2(j, 2)
                c(k) = dmax1 ((effi * pmax(k) - resp(k)), 1.0d-6)
            end do
        end if

        99995 format (' Warning from MAXGRO: negative growth constraint of species ', A8, ' = ', F10.5, /, ' replaced by 0.0')
        99990 format ('  Species ', A8, ' EFFI = ', F5.2, ' Growth rate = ', F5.2, ' B-value = ', F7.3)
        return
    end

end module m_maxgro
