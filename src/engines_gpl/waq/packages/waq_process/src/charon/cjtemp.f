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

!    Date:       7 Nov 1992
!    Time:       16:17
!    Program:    CJTEMP.FOR
!    Version:    1.1
!    Programmer: Nicolaas M de Rooij
!    Previous version(s):
!    1.0 -- 7 Nov 1992 -- 16:12
!    0.0 -- 23 Oct 1992 --  1:09
      subroutine cjtemp
!        cjtemp computes a and b for debye huckel equation, for a given
!        temperature.
!        it recomputes c-value with delta(h),and also gives a new rt
!        value
      use m_char1
      data rv / .0239/
! convert r in 1.98719 to 83.147 cm3 bar /K. Mol
      tt = temp+273.16
      cc = (298.16-tt)/298.16
! divide by RT removed 5-11-92 NMdR
      do 10 i = 1, ntot
         c1(i) = c2(i) + delth(i)*cc
         c(i) = c1(i)
!         c2 contains original value
!         c1 contains c-value corrected for temperature and pressure
!        c contains c-value used in computations, it will be recom-
!         puted in cgamma if activity corrections are carried out.
   10 continue
      if(.not.bactiv) go to 19
      s1 = 374.11 - temp
      s2 = s1**0.3333333
      s3 = dsqrt((1.0+0.134249*s2-3.946263e-3*s1)/(3.1975-0.3151548*
     1 s2-1.203374e-3*s1+7.48908e-13*s1**4))
      tt = temp + 273.16
      if (temp.lt.100.0) go to 20
      cc = 5321.0/tt + 233.76 - tt*(tt*(8.292e-7*tt-1.417e-3)+0.9297)
      go to 30
   20 cc = 87.74 - temp*(temp*(1.41e-6*temp-9.398e-4)+0.4008)
   30 cc = dsqrt(cc*tt)
      adt = 1824600.0*s3/cc**3
      adt = adt*dlog(1.0d01)
!        multipliing adt with dlog(1.0d01) gives possibility to
!        compute the natural log of gamma in subroutine cgamma
      bdt = 50.29*s3/cc
      if (pf.gt.0) write (not,99998) adt, bdt, temp, tt
19    do 50 i=1,ntot
      delv2(i) = delv(i)+apt(i)*temp+bpt(i)*temp*temp
      co    =  delv2(i)*(press-1)-compr(i)*(press-1)*(press-1)
      c1(i) = c1(i) + rv * co /rt
      c(i) = c1(i)
50    continue
      return
99998 format (' cjtemp, adt, bdt, temp, tt:',4e12.5)
      end
