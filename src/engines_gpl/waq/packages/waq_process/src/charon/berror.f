!!  Copyright (C)  Stichting Deltares, 2012-2023.
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

      subroutine berror(bmax)
!           4-69
!           compute g(i) for each equation as the error in the current
!        solution.  bmax is the absolute maximum error.
!           berror uses double precision version of
!              abs
      use m_char1
      use m_char5
      dimension g(1)
      do 10 i=1,m
         g(i) = 0.0
   10 continue
      do 20 ij=1,naij
         i = irow(ij)
         j = jcol(ij)
         g(i) = g(i) - x(j)*aij(ij)
   20 continue
      do 30 i=1,m
         g(i) = g(i) + b(i)
   30 continue
      do 50 k=1,ncomp
         zt = slacks
         if(bslack) zt = slack(k)
         mta = kl(k)
         mtb = kl(k+1) - 1
         do 40 j=mta,mtb
            zt = zt + x(j)
   40    continue
         mk = m + k
         g(mk) = xbar(k) - zt
   50 continue
      bmax = 0.
      do 60 i=1,mend
         if (dabs(g(i)).gt.dabs(bmax)) bmax = g(i)
   60 continue
      return
      end
