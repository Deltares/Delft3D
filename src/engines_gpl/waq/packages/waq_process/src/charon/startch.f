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
!    Time:       21:17
!    Program:    START.FOR
!    Version:    6.00.00
!    Programmer: Nicolaas M de Rooij
!
      subroutine start_ch
!        4-69
!        clears data arrays and sets standard values for parameters.
      use m_char1
      dimension xx1(1),xx2(1),nx1(1)
      common /ndelu/ ndelz, nundel, ktally
      common /ovflo/ anumbr, anumlg, aquot, iquot
      character*6 nhol(4),ix2(1),messag
      DATA NHOL /'END   ','      ','H2O   ','H+    '/
      DATA MESSAG /'MESSAG'/
!        set all data to zero
!        real8
      ipf = 0
      intit2 = intitl
      if(ka(2) .eq. messag)ipf = 1
      num = 6*maxn + (5 + maxmd) * maxmd + 2*maxp + 6*maxm + maxaij + 39
      do 60 i=1,num
      xx1(i) = 0.0d+00
   60 continue
      num = num * 8
      num1 = num
      if(ipf .gt. 0)write(not,1234)num
1234  format(' size common real8 (bytes): ',i5)
!        hol8
      num =   maxn +  2*maxp + 2*maxm + 28
      do 61 i=1,num
      ix2(i) = nhol(2)
   61 continue
      num = num * 6
      num1 = num1 + num
      if(ipf .gt. 0)write(not,1235)num
1235  format(' size common hol8         : ',i5)
!        intgr
      num = 2*maxn  + maxp + 2*maxaij  + 33
      nitt = nit
      nott = not
      do 62 i=1,num
      nx1(i) = 0
   62 continue
      nit  = nitt
      not  = nott
      num = num * 4
      num1 = num1 + num
      if(ipf .gt. 0)write(not,1236)num
1236  format(' size common intgr        : ',i5)
!        water (exept the logicals and undimensioned )
      num = 6.5*maxn  + 2*maxp
      do 63 i=1,num
      xx2(i) = 0.0d+00
   63 continue
      num = num  + 5  + 3
      num = num * 8
      num1 = num1 + num
      if(ipf .gt. 0)write(not,1237)num
1237  format(' size common water        : ',i5)
      if(ipf .gt. 0)write(not,1238)num1
1238  format(' total adjustable size    : ',i5)
      bactiv = .false.
      bconc = .false.
      btemp = .false.
      bactwa = .false.
      bppm   = .false.
      bnew   = .false.
      brt    = .false.
      bpress = .false.
      adt   =1.17647
      bdt   =0.32867
      bdot  =0.0944
      tt = 298.18
      temp=25.0
      press = 1.0
!        clear constants for delete subroutine
      ndelz = 0
      nundel = 0
      ktally = 0
!     nit = 8
!     not = 9
      bmult(1) = 1.0
      aliter = 55.551
      itmax = 250
      rt = 592.418
      end = nhol(1)
      blank = nhol(2)
      h2o = nhol(3)
      hplus = nhol(4)
      if(ipf .gt. 0)write (not,99999) maxm, maxmd, maxp, maxn, maxaij
99999 format(' maximum dimensions are: ',/,
     2 i5, ' maximum number of components ',/,
     3 i5, ' maximum number of components plus phases ',/,
     4 i5, ' maximum number of phases ',/,
     5 i5, ' maximum number of species ',/,
     6 i5, ' maximum number of stochiometric coefficients ')
!        set values for slack tolerances
      barmin = 1.0d-14
      slacks = 2.0*barmin
      xmin = dmin1(5.d-5*slacks,1.d-12)
      xstart = 1.0d-06
      tol1   = 1.0d-02
      tol2   = 1.0d-05
      if(ipf .gt. 0)write (not,99998)
     2           barmin, slacks, xmin, xstart, tol1, tol2
99998 format(' values of toggles are: ',/,
     1 e15.5, ' for barmin',/,
     2 e15.5, ' for slacks',/,
     3 e15.5, ' for xmin  ',/,
     4 e15.5, ' for xstart',/,
     5 e15.5, ' for tol1  ',/,
     6 e15.5, ' for tol2  ')
!        set values for overflow test in function subprogram divide
!           anumbr is largest number
!           aquot represents infinity.  it is the value printed if
!        a divide overflow will occur.
      if(ipf .gt. 0)write (not,99997)
     2           adt, bdt, bdot
99997 format(' values of constants debye huckel equation are : ',/,
     1 e15.5, ' for a',/,
     2 e15.5, ' for b',/,
     3 e15.5, ' for c ')
      iquot = 0
      anumbr = 1.0d+72
      anumlg = dlog(anumbr)
      aquot = 9.9999999d+60
      pf = ipf
      intitl = intit2
      return
      end subroutine start_ch
