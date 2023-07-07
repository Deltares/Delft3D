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
!    Time:       21:06
!    Program:    ROWS.FOR
!    Version:    6.00.00
!    Programmer: Nicolaas M de Rooij
!
      subroutine rows(kalter)
!        the following options are available
!                                kalter      control
!                                   -3       punchrows
!                                   -2       printrows
!                                   -1   (recomputes b vector)
!                                    0       rows
!                                   +1       alterb
!                                   +2       addb
!                                   +3       b
!                                   +4       relaxb
!        rows uses subprogram
!           leave
      use m_char1
      logical nerr
      character*6 nr1, nr2
      nmult = 5
      if(bmult(5) .eq. 0.0d+00)nmult = 4
      if(bmult(4) .eq. 0.0d+00 .and. nmult .eq. 4)nmult = 3
      if (kalter.ne.(-1)) go to 50
!        compute b
   10 do 30 i=1,m
         b(i) = 0.0
         do 20 k=1,5
            b(i) = b(i) + bmult(k)*bbb(i,k)
   20    continue
   30 continue
      return
!        printrows
   50 if (kalter.ge.0) go to 120
      if (kalter.ne.(-2)) go to 60
      write (not,99999) (i,i=1,nmult)
      write (not,99981) (bmult(i),i=1,nmult)
      go to 70
   60 write(7,99979) (bmult(i),i=1,nmult)
      write(7,99980)
   70 do 100 i=1,m
         if (kalter.ne.(-2)) go to 80
      write (not,99997) i, nr(i,1), nr(i,2), b(i), (bbb(i,k),k=1,nmult)
         go to 100
   80    if (bbb(i,1).ge.9999.0 .or. bbb(i,1).lt.0.1) go to 90
         write(7,99996) nr(i,1), nr(i,2), (bbb(i,k),k=1,nmult)
         go to 100
   90    write(7,99995) nr(i,1), nr(i,2), (bbb(i,k),k=1,nmult)
  100 continue
      if (kalter.ne.(-2)) go to 110
      write (not,99994)
      return
  110 write(7,99993)
      return
  120 if (kalter.gt.0) go to 210
!        rows
      write (not,99999) (i,i=1,nmult)
      write (not,99981) (bmult(i),i=1,nmult)
      m = 0
      do 140 i=1,maxm
         b(i) = 0.0
         do 130 j=1,5
            bbb(i,j) = 0.0
  130    continue
  140 continue
!        read first card
      read (nit,99992) nr(1,1), nr(1,2), (bbb(1,k),k=1,5)
      if (nr(1,1).eq.end) return
  150 m = m + 1
      ma = m + 1
      do 160 k=1,5
         b(m) = b(m) + bmult(k)*bbb(m,k)
  160 continue
      if (pf.ge.0) write (not,99997) m, nr(m,1), nr(m,2), b(m),
     1 (bbb(m,k),k=1,nmult)
!        1298 format (1x, i3, 2x, 2a6, 1pe17.7, 4x, 5e16.7)
      if (ma.gt.maxm) go to 200
  170 read (nit,99992) nr(ma,1), nr(ma,2), (bbb(ma,k),k=1,5)
!        1099 format (2a6,5f12.3)
      if (nr(ma,1).eq.end) return
      do 180 i=1,m
         if (nr(ma,1).eq.nr(i,1)) go to 190
  180 continue
      go to 150
  190 write (not,99991) nr(ma,1), nr(ma,2)
      go to 170
!        m is maximum, next card should be an end card
  200 read (nit,99992) nr1, nr2
!        1099 format (2a6,5f12.3)
      if (nr1.eq.end) return
      write (not,99990) nr1, nr2, ma, maxm
      go to 320
!        kalter greater than zero (alterb, addb, b, relaxb)
  210 lka = nbstar
      if (lka.le.5) go to 230
  220 write (not,99989) ka(2), ka(1)
      go to 320
  230 if (kalter.ne.3) go to 250
      do 240 i=1,maxm
         bbb(i,lka) = 0.0
  240 continue
  250 if (kalter.eq.4) go to 330
      nerr = .false.
  260 read (nit,99988) ka(3), ka(4), t(1)
      if (ka(3).eq.end) go to 310
      do 270 i=1,m
         if (ka(3).eq.nr(i,1)) go to 290
  270 continue
      if (pf.ge.0) write (not,99987) ka(3), ka(4), t(1)
      if (kalter.ne.2) go to 280
      write (not,99986) ka(3), ka(4)
      nerr = .true.
      go to 260
  280 m = m + 1
      i = m
      nr(i,1) = ka(3)
      nr(i,2) = ka(4)
      go to 300
  290 if (pf.ge.0) write (not,99985) ka(3), ka(4), t(1)
  300 if (kalter.ne.2) bbb(i,lka) = 0.0
      bbb(i,lka) = bbb(i,lka) + t(1)
      go to 260
  310 if (pf.ge.0) write (not,99984)
      if (nerr) go to 320
      if (m.le.maxm) go to 10
      write (not,99983) m, maxm
  320 call leave(irows, not)
      return
!        relaxb
  330 if (nbstar.eq.1) go to 220
      if (bmult(1).eq.0.0) go to 350
      zzz = bmult(nbstar)/bmult(1)
      do 340 i=1,m
         bbb(i,1) = bbb(i,1) + zzz*bbb(i,nbstar)
  340 continue
      bmult(nbstar) = 0.0
      go to 10
  350 write (not,99982)
      go to 320
99999 format (' component ',11x, ' b     ', 5(3x, 'bbb(i,', i1, ')',1x))
99981 format (23x, ' mult.=', 1pe12.4, 4(1pe12.4) /1h )
99979 format ('MULTIPLIERS',/,5(1pe12.5))
99980 format ('ROWS ')
99997 format (1x, i3, 1x, 2a6, 1pe13.6, 1x, 5(1pe12.5) )
99996 format (2a6, f12.7, 4(1pe12.5))
99995 format (2a6, 5(1pe12.5))
99994 format (' end of components in storage')
99993 format ('END')
99992 format (2a6, 5f12.3)
99991 format (' component ',2a6,' was read twice.',/'the 2nd reading is'
     1 ,' ignored.  **error**',/,1h )
99990 format ('  ',2a6, ' is component# ',i4,', maximum allowed is ',i4,
     1 /1h )
99989 format (' the 2nd field ', a6,' in above ', a6,' was inc',
     1 'orrectly typed '/1h )
99988 format (2a6, f12.3)
99987 format (' * ', 2a6, 1pe17.7)
99986 format (' component ', 2a6,' is not in b-vector.  **error**'/1h )
99985 format (3x, 2a6, 1pe17.7)
99984 format (3x, 'end   (new components are *-ed)')
99983 format (' no. of components=',i4,' maximum allowed=',i4,'*error*')
99982 format (' ** bmult(1) may not equal zero for relaxb. **'/1h )
      end
