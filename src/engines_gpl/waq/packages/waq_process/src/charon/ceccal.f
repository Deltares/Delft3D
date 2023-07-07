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
!    Program:    CECCAL.FOR
!    Version:    6.00.00
!    Programmer: Nicolaas M de Rooij
!
      subroutine ceccal
!        ceccalc computes pie cec for compartment k
!        procedure equivalent to phcalc
      use m_char1
      character*6 cacec,cecc,capl
      DATA CACEC/'CACEC '/
      DATA CECC/'CEC   '/
      DATA CAPL/'CA++  '/
      do 10 i=1,m
      if(nr(i,1).eq.cacec) go to 20
   10 continue
      go to 60
   20 do 30 j=1,ntot
      if(kn(j).eq.cacec) go to 40
   30 continue
      go to 60
   40 do 50 i=1,m
      if(nr(i,1).eq.capl ) go to 80
   50 continue
   60 do 70 i=1,m
      if(nr(i,1).eq.cecc) go to 90
   70 continue
      go to 99
   80 if( x(j) .lt. 1.0d-60 .or. xbar(jcomp(j)) .lt. 1.0d-60 )go to 99
      ceck= divide( x(j),xbar(jcomp(j)) )
      ceck= dlog( ceck ) -0.5d+00 * pie(i)
      return
   90 ceck=  pie(i)
      return
   99 ceck  = -0.0d+00
      return
      end
