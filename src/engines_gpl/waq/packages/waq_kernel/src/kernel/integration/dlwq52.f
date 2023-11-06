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
      module m_dlwq52

      implicit none

      contains


      subroutine dlwq52 ( nosys  , notot  , noseg  , volume , amass  ,
     &                    conc2  , conc    )

!     Deltares Software Centre

!>\File
!>           Makes masses and concentrations after the flux correction step

!     Created:    april 1988 by L.Postma

!     Logical unitnumbers : none

!     Subroutines called  : none

      use timers

      implicit none

!     Parameters          :

!     type     kind  function         name                      description

      integer   (4), intent(in   ) :: nosys                   !< number of transported substances
      integer   (4), intent(in   ) :: notot                   !< total number of substances
      integer   (4), intent(in   ) :: noseg                   !< number of computational volumes
      real      (4), intent(inout) :: volume(noseg )          !< volumes of the segments
      real      (4), intent(inout) :: amass (notot ,noseg)    !< masses per substance per volume
      real      (4), intent(in   ) :: conc2 (notot ,noseg)    !< concentrations per substance per volume
      real      (4), intent(  out) :: conc  (notot ,noseg)    !< concentrations per substance per volume

!     local variables

      integer(4)          isys            ! loopcounter substances
      integer(4)          iseg            ! loopcounter computational volumes
      real   (4)          vol             ! helpvariable for this volume
      integer(4), save :: ithandl         ! timer handle
      data       ithandl /0/
      if ( timon ) call timstrt ( "dlwq52", ithandl )

!         loop accross the number of computational elements

      do iseg = 1, noseg
            vol = volume(iseg)
            do isys = 1, nosys
               conc (isys,iseg) = conc2(isys,iseg)
               amass(isys,iseg) = conc2(isys,iseg)*vol
            enddo
            do isys = nosys+1, notot
               conc (isys,iseg) = conc2(isys,iseg)
            enddo
      enddo

      if ( timon ) call timstop ( ithandl )

      return
      end

      end module m_dlwq52
