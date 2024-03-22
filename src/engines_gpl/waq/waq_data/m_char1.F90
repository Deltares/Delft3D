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

module m_char1

    implicit real(kind = 8) (a-h, o-z)
    ! note:::: change dimensions in matinv to value of maxmd if changed
    integer pf
    character*6 ka, kb, nr, kn, nam, end, blank, h2o, hplus
    parameter (maxm = 40, &
            maxmd = 60, &
            maxp = 25, &
            maxn = 200, &
            maxaij = 600, maxn2 = maxn + 1, maxp2 = maxp + 1)
    common /real8/ aij(maxaij), bbb(maxm, 5), phk, t(20), &
            bmult(5), b(maxm), pie(maxmd), v1(maxmd), v2(maxmd), &
            v3(maxmd), v4(maxmd), x(maxn), xmf(maxn), c(maxn), &
            x1(maxn), x2(maxn), x3(maxn), xbar(maxp), &
            r(maxmd, maxmd), fe, fe2, ermb, xemb, erma, xema, &
            tol1, tol2, xmin, xstart, barmin, slacks, aliter, rt
    common /hol6/ ka(12), kb(12), nr(maxm, 2), kn(maxn), &
            nam(maxp, 2), end, blank, h2o, hplus
    common /intgr/ irow(maxaij), jcol(maxaij), jcomp(maxn), &
            ijfind(maxn2), kl(maxp2), m, mend, ncomp, n, nit, not, pf, &
            iter, itmax, ierror, lastcp, ke, ncycle, nbstar, kpf, &
            naij, iopt, mtog, marith, nemb, nema, iarith, ifind, &
            intitl, insolv, ijohn, ilp, imatrx, ipush, irows, iscale
    common /water/ c1(maxn), c2(maxn), diam(maxn), gamma(maxn), &
            delth(maxn), ch(maxn), gfw(maxn), ehk, ceck, &
            apt(maxn), bpt(maxn), compr(maxn), delv(maxn), delv2(maxn), &
            adt, bdt, bdot, temp, tt, press, str, str1, &
            bready, bactiv, bactwa, bconc, btemp, bpress, bppm, brt, bnew
    logical  bready, bactiv, bactwa, bconc, btemp, bpress, bppm, brt, bnew

end module m_char1
