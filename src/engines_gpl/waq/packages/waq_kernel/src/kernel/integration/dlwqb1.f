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
      module m_dlwqb1

      implicit none

      contains


      subroutine dlwqb1 ( notot  , noseg  , volold , volnew , conc   ,
     *                    deriv  , isys0  , nsys   , noco   , amat   ,
     *                    rhs    , idt    )

!     Deltares Software Centre

!>\File
!>           zero the matrix, define right hand side, set diagonal

!     Created             :    April   1992 by Jos van Gils

!     Modified            : 28 Januari 2011 by Leo Postma
!                                           2D arrays, fortran 90 look and feel

!     Logical unitnumbers : none

!     Subroutines called  : none

      use timers
      implicit none

!     Parameters          :
!     type     kind  function         name                      description

      integer   (4), intent(in   ) :: notot                   !< total number of substances
      integer   (4), intent(in   ) :: noseg                   !< number of computational volumes
      real      (4), intent(in   ) :: volold(noseg )          !< volumes at the start of the time step
      real      (4), intent(in   ) :: volnew(noseg )          !< volumes at the end of the time step
      real      (4), intent(inout) :: conc  (notot ,noseg )   !< concentrations per substance per volume
      real      (4), intent(inout) :: deriv (notot ,noseg )   !< derivatives per substance per volume
      integer   (4), intent(in   ) :: isys0                   !< first substance number
      integer   (4), intent(in   ) :: nsys                    !< number of substances to deal with
      integer   (4), intent(in   ) :: noco                    !< number of codiagonals
      real      (4), intent(inout) :: amat  (noco*2+1,noseg)  !< band matrix to invert
      real      (4), intent(inout) :: rhs   (nsys  ,noseg )   !< right hand side vector
      integer   (4), intent(in   ) :: idt                     !< integration time step size

!     local variables

      integer(4)          isys            ! loopcounter substances
      integer(4)          iseg            ! loopcounter computational volumes
      real   (4)          invdt           ! 1/idt
      real   (4)          ioff            ! isys0 first substance number to deal with
      integer(4), save :: ithandl         ! timer handle
      data       ithandl /0/

      if ( timon ) call timstrt ( "dlwqb1", ithandl )

!         some initialisation

      invdt = 1.0 / idt
      ioff  = isys0 - 1

!         zero the matrix

      amat = 0.0

!         set the right hand side

      do iseg = 1 , noseg
         do isys = 1 , nsys
            rhs(isys,iseg) = deriv(isys+ioff,iseg) + volold(iseg)*conc(isys+ioff,iseg)*invdt
         enddo
      enddo

!         set the diagonal

      do iseg = 1 , noseg
         amat(noco+1,iseg) = volnew(iseg)*invdt
      enddo

      if ( timon ) call timstop ( ithandl )
      return
      end

      end module m_dlwqb1
