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

      subroutine matrix(iii,ntot1,ntot2,ncomp1,ncomp2,nsl,
     2   mslr,nslxv,nslrow,nslero,slaij)
!           matrix stores condensed, non-zero elements in aij, component
!           number in irow and species number in jcol
!           jcomp contains phase number for each species
!           if dimensions for the matrix are extended, change the
!        maximum values for the appropriate variables in subroutine
!        start.
!                                   iii         control
!                                    -1         altera
!                                     0         matrix
!                                     1         alterc
!                                     2         addc
!           matrix uses subprograms
!              find, leave, pop, push
      use m_char1
      dimension nslxv(mslr),nslrow(mslr),nslero(mslr,12),slaij(mslr,12)
      dimension z(5)
      character*6 kapr
!           for this subroutine temporarily set toggle so that
!        subroutine push will return control to matrix
      ipusht = ipush
      ipush = 1
      kerr = 0
      if (iii.le.0) ijfind(1) = -1
      if (iii.ne.0) go to 130
!        process matrix cards
      ncomp = 0
      ntot = 0
      naij = 0
      do 10 j=1,maxaij
         aij(j) = 0.0
         irow(j) = 0
         jcol(j) = 0
   10 continue
!        read phases  and species
   20 read (nit,99999) ka(1), ka(2), z(1), (z(i+1),ka(i+2),i=1,4)
      if (ka(1).eq.end) go to 80
      if (ka(1).eq.blank) go to 30
      ncomp = ncomp + 1
      kapr = end
      write (not,99998) ka(1), ka(2)
      if (ncomp.gt.maxp) go to 20
      kl(ncomp) = ntot + 1
      nam(ncomp,1) = ka(1)
      nam(ncomp,2) = ka(2)
      go to 20
!        read species
   30 if (ka(2).eq.kapr) go to 40
      ntot = ntot + 1
      kapr = ka(2)
      if (ntot.gt.maxn) go to 40
      c(ntot) = z(1)
      c2(ntot) = z(1)
      c1(ntot) = z(1)
      kn(ntot) = kapr
      jcomp(ntot) = ncomp
   40 jmax = 1
      do 70 j=1,4
         if (ka(j+2).eq.blank) go to 70
         jmax = j
         do 50 i=1,m
            if (nr(i,1).eq.ka(j+2)) go to 60
   50    continue
         write (not,99997) ka(j+2)
         go to 70
!        if z(j+1) is not equal to zero
!        increase matrix count and store entries
   60    if (z(j+1).eq.0.0) go to 70
         naij = naij + 1
         if (naij.gt.maxaij) go to 70
         aij(naij) = z(j+1)
         irow(naij) = i
         jcol(naij) = ntot
   70 continue
!        print input card
      write (not,99996) ntot, ka(2), z(1), (z(i+1),ka(i+2),i=1,jmax)
      go to 20
!        end card has been read
   80 kl(ncomp+1) = ntot + 1
      ntot1 = ntot
      ncomp1 = ncomp
      ntot2 = ntot
      ncomp2 = ncomp
      mend = m + ncomp
   90 write (not,99995) m, ntot, ncomp, naij
      if (naij+m.le.maxaij) go to 100
      write (not,99994) m
  100 if (ncomp.gt.maxp) go to 110
      if (ntot.gt.maxn) go to 110
      if (naij.gt.maxaij) go to 110
      if (kerr.ne.0) go to 120
      go to 330
!        print message for problem exceeding dimension
  110 kerr = 1
      write (not,99993) maxm, maxn, maxp, maxaij
      if (kerr.eq.0) go to 330
      write (not,99992)
  120 call leave(imatrx, not)
      go to 330
!        process alter cards
  130 k = 0
      kerr = 0
  140 read (nit,99999) ka(1), ka(2), z(1), (z(i+1),ka(i+2),i=1,4)
      write (not,99991) ka(1), ka(2), z(1), (z(i+1),ka(i+2),i=1,4)
      if (ka(1).eq.blank) go to 170
      if (ka(1).eq.end) go to 90
      do 150 k=1,ncomp1
         if (ka(1).eq.nam(k,1)) go to 160
  150 continue
      k = 0
  160 go to 140
  170 if (k) 190, 180, 190
  180 write (not,99990)
      kerr = 1
      go to 140
  190 mta = kl(k)
      mtb = kl(k+1) - 1
      do 200 jj=mta,mtb
         if (ka(2).eq.kn(jj)) go to 210
  200 continue
      write (not,99989)
      kerr = 1
      go to 140
  210 if (iii) 250, 250, 220
  220 if (iii-1) 240, 240, 230
  230 c(jj) =c2(jj) + z(1)
      c1(jj)=c(jj)
      c2(jj)=c(jj)
      go to 250
  240 c(jj) = z(1)
      c2(jj)=z(1)
      c1(jj)=z(1)
  250 do 320 j=1,4
         if (ka(j+2).eq.blank) go to 320
         do 260 i=1,m
            if (nr(i,1).eq.ka(j+2)) go to 270
  260    continue
         write (not,99997) ka(j+2)
         kerr = 1
         go to 320
  270    call find(i, jj, ii, iflag)
         if (iflag.eq.0) go to 280
!        push down matrix entries in order to insert new one
         call push(ii)
         if (ii.gt.maxaij) go to 320
         irow(ii) = i
         jcol(ii) = jj
         aij(ii) = 0.0
!        test for alter or add entry
  280    if (iii-1) 300, 300, 290
  290    aij(ii) = aij(ii) + z(j+1)
         go to 310
  300    aij(ii) = z(j+1)
         if(jj .gt. ntot2) go to 311
  310    if (aij(ii).ne.0.0) go to 320
         call pop(ii)
         go to 320
!********
!        change slow stochiometry
!********
311      do 312 kk = 1 ,nsl
         if(kn(nslxv(kk)) .eq. ka(2) ) go to 313
312      continue
313      kr = nslrow(kk)
         do 314 i =1,kr
         if(nr(nslero(kk,i),1) .eq. ka(j+2) ) go to 315
314      continue
315      slaij(kk,i) = z(j+1)
         naij = naij - 1
  320 continue
      go to 140
  330 ipush = ipusht
      return
99999 format (2a6, f12.0, 4(f6.1, a6))
99998 format (' ', 3x, 2a6)
99997 format (' component ', a6, ' is undefined')
99996 format (1x, i3, 2x, a6, f12.4, 4(f7.2, 1x, a6))
99995 format (' problem has',i6,' components',i6,' species,',/,12x,i6,
     1 ' phases  ',i8, ' non zero matrix entries.')
99994 format (' *** warning', 6x, 'this problem will exceed the limit',
     1 ',on matrix entries if subroutine lp is entered to do simplex.',
     2  /15x, 'the code has to generate ', i4, ' new entries.',/'0')
99993 format (' ** problem has exceeded the limit of', /12x, i6,
     1 ' components,',i6,' species,',i6,' phases,', i8, ' non ',
     2 'zero matrix entries.', /' compare the limits with the values ',
     3 'printed above for the problem.', /1h )
99992 format (' ',/' matrix error', /1h )
99991 format (1x, 2a6, f12.4, 4(f7.2,1x, a6))
99990 format (' above species has no phase')
99989 format (' above species not in phase')
      end
