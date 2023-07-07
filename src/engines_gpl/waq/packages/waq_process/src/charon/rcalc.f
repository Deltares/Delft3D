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

      subroutine rcalc
!           4-69
!           compute the r matrix which equals aij * aij transposed * x
!        r is symmetric and has a dimension of (mend,mend)
!           rcalc uses subprogram
!              find
!           from 7/11/79 rcalc doesn't use find anymore, takes too
!           much time. it find the entries in the ijfind array
!           which is filled each time that solve is called.
!           (n.m. de rooij)
      use m_char1
      use m_char5
      if (ijfind(1) .ge. 0) go to 9
      jj = 2
      ijfind(1) = 1
      do 8 ii=1,naij
      if(jcol(ii) .ne. jj) go to 8
      ijfind(jj) = ii
      jj = jj + 1
8     continue
      ijfind(jj) = naij +1
!        compute r
9     do 20 i=1,mend
         do 10 j=1,i
            r(i,j) = 0.0
   10    continue
   20 continue
      do 50 k=1,ntot
         mta=ijfind(k)
         mtb=ijfind(k+1)-1
         if (mta.eq.0) go to 50
         do 40 ii=mta,mtb
            aikx = aij(ii)*x(k)
            i = irow(ii)
!        do computation for j ranging from 1 to i
            do 30 jk=mta,mtb
               j = irow(jk)
               if (j.le.i) r(i,j) = r(i,j) + aij(jk)*aikx
   30       continue
   40    continue
   50 continue
      do 100 k=1,ncomp
         ih = k + m
         r(ih,ih) = -slacks
         if(bslack) r(ih,ih) = -slack(k)
         j1 = kl(k)
         j2 = kl(k+1) - 1
   60    mta=ijfind(j1)
         ii= ijfind(j1+1)-1
         if (mta.ne.0) go to 70
         j1 = j1 + 1
         if (j1.gt.j2) go to 100
         go to 60
   70    ii= ijfind(j2)
         mtb=ijfind(j2+1)-1
         if (ii.ne.0) go to 80
         j2 = j2 - 1
         if (j2.lt.kl(k)) go to 100
         go to 70
   80    continue
         do 90 ii=mta,mtb
            l = jcol(ii)
            j = irow(ii)
            r(ih,j) = r(ih,j) + aij(ii)*x(l)
   90    continue
  100 continue
      do 120 j=2,mend
         jl = j - 1
         do 110 i=1,jl
            r(i,j) = r(j,i)
  110    continue
  120 continue
      return
      end
