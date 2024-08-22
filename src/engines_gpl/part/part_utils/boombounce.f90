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
module m_boombounce

    implicit none

contains


    subroutine boombounce(xold, yold, xnew, ynew, nboom, xboom, &
            yboom, xcatch, ycatch, catch, xbounce, ybounce, bounce, leftside)
        !
        !     Does a boom catch a particle in it's path from xold, yold to xnew, ynew?
        !     And if it does, does it bounce without crossing another part of the boom?

        use m_waq_precision      ! single/double precision
        use timers

        implicit none

        real    (real_wp) :: xold, yold, xnew, ynew
        integer (int_wp) :: nboom
        integer (int_wp) :: npolbounce
        real    (sp) :: xboom(nboom), yboom(nboom)
        real    (sp) :: xcatch, ycatch
        logical :: catch
        logical :: leftside
        real    (sp) :: xbounce, ybounce
        logical :: bounce

        !     local variables
        real    (real_wp) :: xmin, ymin, xmax, ymax
        real    (dp) :: xoldd, yoldd, xnewd, ynewd
        real    (dp) :: xboomd(nboom), yboomd(nboom)
        real    (dp) :: a1, b1, c1, rl1, rlnear
        real    (dp) :: a2(nboom), b2(nboom), c2(nboom), rl2(nboom)
        real    (dp) :: xcross(nboom), ycross(nboom)
        real    (sp) :: xcrossboom(2), ycrossboom(2)
        real    (dp) :: rl, xn, yn, xb, yb
        integer (4) :: iboom
        integer (4) :: inear
        real    (dp) :: det
        logical :: boomgap(nboom), notnear

        integer(4) ithndl              ! handle to time this subroutine
        data       ithndl / 0 /
        if (timon) call timstrt("lines_cross", ithndl)

        catch = .false.
        xcatch = 999.999e0
        ycatch = 999.999e0
        bounce = .false.
        xbounce = 999.999e0
        ybounce = 999.999e0
        inear = 0
        rlnear = 1.1D0
        boomgap = .false.
        xcrossboom = 999.999e0
        ycrossboom = 999.999e0
        leftside = .true.

        xoldd = dble(xold)
        yoldd = dble(yold)
        xnewd = dble(xnew)
        ynewd = dble(ynew)
        xboomd = dble(xboom)
        yboomd = dble(yboom)

        !     at least one coordinate must have changed, otherwise there is no line between old and new location
        if (xold /= xnew .or. yold /= ynew) then
            !        fill matrix for particle track
            a1 = ynewd - yoldd
            b1 = xoldd - xnewd
            c1 = xnewd * yoldd - xoldd * ynewd
            xmin = min(xold, xnew)
            xmax = max(xold, xnew)
            ymin = min(yold, ynew)
            ymax = max(yold, ynew)

            !        loop over booms
            do iboom = 1, (nboom - 1)
                !           skip 'gaps' when both coordinates of either start or end are 999.999e0 (missing value)
                boomgap(iboom) = (xboom(iboom) == 999.999e0 .and. xboom(iboom) == 999.999e0) .or. &
                        (xboom(iboom + 1) == 999.999e0 .and. xboom(iboom + 1) == 999.999e0)
                !           when both old and new x- or y-coordinate are less or higher than both the boom x- or y-coordinate
                notnear = (xmax < xboom(iboom) .and. xmax < xboom(iboom + 1)) .or. &
                        (xmin > xboom(iboom) .and. xmin > xboom(iboom + 1)) .or. &
                        (ymax < yboom(iboom) .and. ymax < yboom(iboom + 1)) .or. &
                        (ymin > yboom(iboom) .and. ymin > yboom(iboom + 1))
                !           if both are false we might have a colission (sorry for the double denial)
                if (.not. boomgap(iboom) .and. .not. notnear) then
                    !              only when at least one coordinate is different we have a boom line
                    if (xboomd(iboom) /= xboomd(iboom + 1) .or. yboomd(iboom) /= yboomd(iboom + 1)) then
                        !                 fill matrix for boom track
                        a2(iboom) = yboomd(iboom + 1) - yboomd(iboom)
                        b2(iboom) = xboomd(iboom) - xboomd(iboom + 1)
                        c2(iboom) = xboomd(iboom + 1) * yboomd(iboom) - xboomd(iboom) * yboomd(iboom + 1)

                        !                 matrix determinant
                        det = a1 * b2(iboom) - b1 * a2(iboom)

                        !                 when the determinant is zero, the lines are exactly parallel, and never cross
                        if (det /= 0.0D+00) then
                            !                    calculate where the lines through the points would cross
                            xcross(iboom) = (b1 / det) * c2(iboom) - (b2(iboom) / det) * c1
                            ycross(iboom) = (a2(iboom) / det) * c1 - (a1 / det) * c2(iboom)

                            !                    relative distance of crosspoint between old and new location of particle
                            if (xnew /= xold) then
                                rl1 = (xcross(iboom) - xoldd) / (xnewd - xoldd)
                            else
                                rl1 = (ycross(iboom) - yoldd) / (ynewd - yoldd)
                            end if
                            !                    relative location of crosspoint on the boom
                            if (xboom(iboom + 1) /= xboom(iboom)) then
                                rl2(iboom) = (xcross(iboom) - xboom(iboom)) / (xboom(iboom + 1) - xboom(iboom))
                            else
                                rl2(iboom) = (ycross(iboom) - yboom(iboom)) / (yboom(iboom + 1) - yboom(iboom))
                            end if
                            !                    both relative locations must be between 0.0 and 1.0 to cross
                            if ((rl1 >= 0.0D0 .and. rl1 <= 1.0D0) .and. (rl2(iboom) >= 0.0D0 .and. rl2(iboom) <= 1.0D0)) then
                                !                       determine if this is the lowest relative location of the particle path
                                if (rl1 < rlnear) then
                                    catch = .true.
                                    rlnear = rl1
                                    inear = iboom
                                    leftside = det > 0.0D+0
                                end if
                            end if
                        end if
                    end if
                end if
            end do
            if (catch) then
                !           return information about collision location
                xcrossboom(1) = xboom(inear)
                xcrossboom(2) = xboom(inear + 1)
                ycrossboom(1) = yboom(inear)
                ycrossboom(2) = yboom(inear + 1)
                xcatch = sngl(xcross(inear))
                ycatch = sngl(ycross(inear))
                npolbounce = inear

                !           bounce without crossing boom?
                rl = ((xboomd(inear) - xnewd) * b2(inear) + (ynewd - yboomd(inear)) * a2(inear)) / (b2(inear)**2 + a2(inear)**2)
                xn = xboomd(inear) - rl * b2(inear)
                yn = yboomd(inear) + rl * a2(inear)
                !           potential location after bounce at boom
                xb = xn - (xnewd - xn)
                yb = yn - (ynewd - yn)
                bounce = .true.

                !           fill matrix for particle track
                a1 = yb - yoldd
                b1 = xoldd - xb
                c1 = xb * yoldd - xoldd * yb
                xmin = min(xold, real(xb,kind(xold)))
                xmax = max(xold, real(xb,kind(xold)))
                ymin = min(yold, real(yb,kind(yold)))
                ymax = max(yold, real(yb,kind(yold)))

                !           loop over booms, we saved some data from the previous loop (boomgap, a2, b2, c2)
                do iboom = 1, (nboom - 1)
                    !              when both old and new x- or y-coordinate are less or higher than both the boom x- or y-coordinate
                    notnear = (xmax < xboom(iboom) .and. xmax < xboom(iboom + 1)) .or. &
                            (xmin > xboom(iboom) .and. xmin > xboom(iboom + 1)) .or. &
                            (ymax < yboom(iboom) .and. ymax < yboom(iboom + 1)) .or. &
                            (ymin > yboom(iboom) .and. ymin > yboom(iboom + 1))
                    !              if both are false we might have a colission (sorry for the double denial)
                    if (.not.(iboom==npolbounce) .and. bounce .and. (.not.boomgap(iboom) .and. .not. notnear)) then
                        !                 matrix determinant
                        det = a1 * b2(iboom) - b1 * a2(iboom)
                        !                 when the determinant is zero, the lines are exactly parallel, and never cross
                        if (det /= 0.0D+00) then
                            !                    calculate where the lines through the points would cross
                            xcross(iboom) = (b1 / det) * c2(iboom) - (b2(iboom) / det) * c1
                            ycross(iboom) = (a2(iboom) / det) * c1 - (a1 / det) * c2(iboom)

                            !                    relative distance of crosspoint between old and new location of particle
                            if (xb /= xold) then
                                rl1 = (xcross(iboom) - xoldd) / (xb - xoldd)
                            else
                                rl1 = (ycross(iboom) - yoldd) / (yb - yoldd)
                            end if
                            !                    relative location of crosspoint on the boom
                            if (xboom(iboom + 1) /= xboom(iboom)) then
                                rl2(iboom) = (xcross(iboom) - xboom(iboom)) / (xboom(iboom + 1) - xboom(iboom))
                            else
                                rl2(iboom) = (ycross(iboom) - yboom(iboom)) / (yboom(iboom + 1) - yboom(iboom))
                            end if
                            !                    if both relative locations are between 0.0 and 1.0 we have crossed another part of the boom when bouncing,
                            !                    so boncing is not possible unless we would futher iterate
                            if ((rl1 >= 0.0D0 .and. rl1 <= 1.0D0) .and. (rl2(iboom) >= 0.0D0 .and. rl2(iboom) <= 1.0D0)) then
                                bounce = .false.
                            end if
                        end if
                    end if
                end do

                !           return bouncing location
                xbounce = sngl(xb)
                ybounce = sngl(yb)
            end if
        end if

        if (timon) call timstop (ithndl)
    end subroutine boombounce

end module m_boombounce
