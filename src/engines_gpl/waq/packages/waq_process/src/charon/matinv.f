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

!    Date:       22 Oct 1992
!    Time:       21:28
!    Program:    MATINV.FOR
!    Version:    6.00.00
!    Programmer: Nicolaas M de Rooij
!
      subroutine matinv(a, n, b, m, d, w, ising)
!           4-69
!           cholesky's diagonal method -- for symmetric matrices only
!           matrix inversion and / or simultaneous equation solution
!             m =  0    invert only and store inverse in a
!             m = -1    solve simultaneous equations.  solution in b
!             m =  1    combination of m = 0 and m = -1.
!             m = -2    solve simultaneous equations with already
!                       calculated upper triangular and diagonal
!                       inverse stored in a, and solution in b
!           dimension of the arrays should be equal to the value of
!        maxmd in common labeled /intgr/ of other routines in deck.
      implicit real*8 (a-h,o-z)
      dimension a(60,60), b(60), d(60), w(60)
      ising = 0
      if(m.eq.-2)go to 35
!        find upper triangular matrix and diagonal matrix
      do 30 i=1,n
         if (a(i,i).eq.0.) go to 170
         d(i) = a(i,i)
         a(i,i) = 1.
         if (i.eq.n) go to 30
         k = i + 1
         do 20 j=k,n
            do 10 l=j,n
               a(j,l) = a(j,l) - a(i,j)*a(i,l)/d(i)
   10       continue
            a(i,j) = a(i,j)/d(i)
   20    continue
   30 continue
      if (m.eq.0) go to 80
!        solve simultaneous equations
   35 w(1) = b(1)
      do 50 i=2,n
         s = 0.
         l2 = i - 1
         do 40 l=1,l2
            s = s + a(l,i)*w(l)
   40    continue
         w(i) = b(i) - s
   50 continue
      b(n) = w(n)/d(n)
      do 70 ii=2,n
         i = n + 1 - ii
         s = 0.
         l1 = i + 1
         do 60 l=l1,n
            s = s + a(i,l)*b(l)
   60    continue
         b(i) = w(i)/d(i) - s
   70 continue
   80 if (m.lt.0) return
!        invert upper triangular matrix  to get  inverse
      do 110 ii=2,n
         i = n + 1 - ii
         jj = ii - 1
         j3 = i + 1
         do 100 j2=1,jj
            j = n + 1 - j2
            s = 0.
            do 90 k=j3,j
               s = s + a(i,k)*a(k,j)
   90       continue
            a(i,j) = -s
  100    continue
  110 continue
!        multiply inverse * reciprocal diagonal * inverse transposed
      do 140 i=1,n
         do 130 j=i,n
            s = 0.
            do 120 k=j,n
               s = s + a(i,k)*a(j,k)/d(k)
  120       continue
            a(i,j) = s
  130    continue
  140 continue
!        re-symmetrize
      do 160 j=2,n
         j2 = j - 1
         do 150 i=1,j2
            a(j,i) = a(i,j)
  150    continue
  160 continue
      return
  170 ising = i
      return
      end
