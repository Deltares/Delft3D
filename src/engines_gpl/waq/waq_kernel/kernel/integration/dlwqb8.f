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
      module m_dlwqb8

      implicit none

      contains


      subroutine dlwqb8 ( nosys  , notot  , nototp , noseg  , volume ,
     &                    surface, amass  , conc   )

!     Deltares Software Centre

!>\File
!>           Restores conc array after mass has changed by proces routines

!     Created             :    May     1993 by Jos van Gils

!     Modified            : 13 Januari 2011 by Leo Postma
!                                           2D arrays, fortran 90 look and feel
!                                           conc of passive substances in mass/m2
!                            4 April   2013 by Leo Postma
!                                           take presence of particle-substances into account

!     Logical unitnumbers : LUN     = number of monitoring file

!     Subroutines called  : none

      use timers

      implicit none

!     Parameters          :
!     type     kind  function         name                      description

      integer   (4), intent(in   ) :: nosys                   !< number of transported substances
      integer   (4), intent(in   ) :: notot                   !< total number of substances
      integer  ( 4), intent(in   ) :: nototp                  !< number of particle substances
      integer   (4), intent(in   ) :: noseg                   !< number of computational volumes
      real      (4), intent(inout) :: volume(noseg )          !< volumes of the segments
      real     ( 4), intent(in   ) :: surface(noseg )         !< horizontal surface area
      real      (4), intent(inout) :: amass (notot ,noseg)    !< masses per substance per volume
      real      (4), intent(inout) :: conc  (notot ,noseg)    !< concentrations per substance per volume

!     local variables

      integer(4)          isys            ! loopcounter substances
      integer(4)          iseg            ! loopcounter computational volumes
      real   (4)          surf            ! the horizontal surface area of the cell
      real   (4)          vol             ! helpvariable for this volume
      integer(4), save :: ithandl         ! timer handle
      data       ithandl /0/
      if ( timon ) call timstrt ( "dlwq18", ithandl )

!         loop accross the number of computational volumes for the concentrations

      do iseg = 1, noseg

!        check for positivity

         vol  = volume(iseg)
         surf = surface(iseg)
         if ( abs(vol) .lt. 1.0e-25 ) vol = 1.0

!         transported substances first

         do isys = 1, nosys
            conc (isys,iseg) = amass(isys,iseg) / vol
         enddo

!         then the passive substances

         do isys = nosys+1, notot - nototp
            conc(isys,iseg) = amass(isys,iseg) / surf
         enddo

      enddo

      if ( timon ) call timstop ( ithandl )
      return
      end

      end module m_dlwqb8
