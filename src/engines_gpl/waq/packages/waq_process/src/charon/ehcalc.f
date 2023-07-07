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

!    Date:       5 Oct 1992
!    Time:       22:20
!    Program:    EHCALC.FOR
!    Version:    6.00.00
!    Programmer: Nicolaas M de Rooij
!
      subroutine ehcalc
!        ehcalc computes eh from pie of the electron
!        procedure equivalent to phcalc
      use m_char1
      character*6 el,oh,o2
      DATA EL/'EL-   '/
      DATA oh/'OH-   '/
      DATA o2/'O2    '/
      iel = 0
      ihp = 0
      ioh = 0
      ih2 = 0
      ehk = -0.0d+00
      do 10 i=1,m
      if(nr(i,1).eq.el)iel = i
      if(nr(i,1).eq.hplus) ihp = i
      if(nr(i,1).eq.oh)ioh = i
      if(nr(i,1).eq.h2o)ih2 = i
   10 continue
      if(iel .ne. 0) then
        pda = pie(iel)
        goto 100
      endif
      if(ihp .ne. 0 .and. ioh .ne. 0) then
        pda = 2.0d+00*pie(ioh) - 2.0d+00*pie(ihp)
        goto 90
      endif
      if(ihp .ne. 0 .and. ih2 .ne. 0) then
        pda = 2.0d+00*pie(ih2) - 4.0d+00*pie(ihp)
        goto 90
      endif
      if(ioh .ne. 0 .and. ih2 .ne. 0) then
        pda = -2.0d+00*pie(ih2) + 4.0d+00*pie(ioh)
        goto 90
      endif
85    continue
      return
90    call lookup(o2,lka,1,m,nr)
      if(lka .gt. m) go to 85
      pda = (pda - pie(lka))/ 4.0d+00
100   ehk =-pda/dlog(10.0d+00)
      return
      end
