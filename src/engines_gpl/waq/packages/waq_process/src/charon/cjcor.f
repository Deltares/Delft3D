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
!    Time:       16:13
!    Program:    CJCOR.FOR
!    Version:    1.0
!    Programmer: Nicolaas M de Rooij
!    Previous version(s):
!    0.0 -- 23 Oct 1992 --  1:09
      subroutine cjcor
!        cjcor reads charge and ionic diameter for charged species
!        and reads delta(h) values for species.
!        it is not necessary to give this information for all species
!        although charges and ionic diameters for water species are
!        required for activity calculation.
!        read in procedure is equivalent to that of matrix.
      use m_char1
      dimension z(4)
      bnew = .false.
      IF(KA(2) .EQ. 'NEW   ')BNEW = .TRUE.
      kk = 1
10    read (nit,99999) ka(1),ka(2),(z(i),i=1,4)
      if (ka(1).eq.end) return
      if (ka(1).eq.blank) go to 20
      if (pf.ge.0) write (not,99997) ka(1)
!        find compartment number k
      call lookup(ka(1), k, 1, ncomp, nam)
      if (k.gt.ncomp) go to 40
      go to 10
   20 mta = kl(k)
      mtb = kl(k+1) - 1
      kk = kk + 1
      if(ka(2) .eq. blank) go to 25
      if (pf.ge.0 .and. ka(1).eq.blank) write (not,99998) kk,
     1                    ka(2), (z(i),i=1,4)
      call lookup(ka(2), j, mta, mtb, kn)
!        find species number j
      if (j.gt.mtb) go to 31
      ch(j) = z(1)
      if(bnew) ch(j) = ch(j) * ch(j)
      diam(j) = z(2)
      delth(j) = z(3)
      gfw(j) = z(4)
      go to 30
25    if (pf.ge.0 .and. ka(1).eq.blank) write (not,99994)(z(i),i=1,4)
      delv(j) = z(1)
      apt(j) = z(2)
      bpt(j) = z(3)
      compr(j) = z(4)
   30 continue
      go to 10
   31 write (not,99996) ka(1), ka(2)
      go to 10
!        if a compartment name is invalid, information will be skipped
!         till next compartment name or end.
   40 write (not,99995) ka(1)
   50 read (nit,99999) ka(1)
      if (ka(1).eq.blank) go to 50
      if (ka(1).eq.end) return
      go to 10
99999 format (2a6, 4f12.6)
99998 format (i4, 6x,a6, 4f12.6)
99994 format (22x, 4f12.6)
99997 format (1x, a6)
99996 format (9h  species, 2a6, 13h is undefined)
99995 format (1x, a6, ' is not a phase name')
      end
