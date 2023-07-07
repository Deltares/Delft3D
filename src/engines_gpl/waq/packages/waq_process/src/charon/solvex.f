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
!    Time:       21:16
!    Program:    SOLVEX.FOR
!    Version:    6.00.00
!    Programmer: Nicolaas M de Rooij
!
      subroutine solvex(k1,str6)
      use m_char1
      logical done,done2
      character*6 str6
      if(intitl .ne. 846342) then
        write(*,1233)
1233    format(' corrupt module ')
        call chexit
      endif
      done  = .false.
      done2 = .false.
      kk = k1
1     if(kk.eq.0) then
! second order wanted
        call solve2(kk)
        if(ierror .eq. 3) call solve2(kk)
        if(ierror .eq. 1) go to 999
        if(ierror .eq. 8) then
! change to projection
          kk  = 1
        endif
      endif
      if(kk .eq. 1)then
! projection wanted, or second order failed
        call solve2(kk)
        if(ierror .eq. 3) call solve2(kk)
        if(ierror .eq. 1) go to 999
        if(ierror .eq. 8) then
          if(done) go to 90
! if simplex already done go to sec order
          if(kk .eq. 2) then
            done = .true.
            kk = 0
            go to 1
          endif
! change to simplex
          kk  = 2
        endif
      endif
! simplex wanted or projection failed
      if(kk .eq. 2)then
        call solve2(kk)
        if(ierror .eq. 3) call solve2(kk)
        if(ierror .eq. 1) go to 999
        write(not,1236)kk
1236    format(' simplex failed ',i4)
        if(.not.done) then
! simplex failed, try once projection
          done = .true.
          kk = 1
          go to 1
        endif
      endif
! all failed
90    if(done2) go to 99
      if(ierror .ne. 8) go to 99
      do 92 i=1,m
      b(i) = b(i) * 1.00000001
92    continue
      done2 = .true.
      kk = 0
      go to 1
99    write(not,1201) ierror,str6,(nr(i,1),b(i),i=1,m),
     2   (kn(j),c(j),j=1,ntot),(kn(j),x(j),j=1,ntot)
1201  format(' error in solve, ierror is: ',i4, /,'called by: ',a6,/,
     2  '  b,c,x is: ',/,999(a6,6x,e18.10,/))
999   continue
      call phcalc
      call ehcalc
      call ceccal
      return
      end
