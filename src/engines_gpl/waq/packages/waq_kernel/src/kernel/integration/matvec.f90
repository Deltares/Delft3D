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
module m_matvec

implicit none

contains


      subroutine matvec ( ntrace , nomat  , alpha  , amat   , imat   ,                          &
     &                    diag   , idiag  , xvec   , beta   , yvec   )

!     Deltares - Delft Software Department

!     Created   : November 96 by Kian Tan

!     Function  : Matrix-vector multiply: y = beta y + alpha A x
!                 A is square matrix

!     Modified  : July 2009, Leo Postma  : allocation double precission arrays

      use timers                         ! WAQ performance timers

      implicit none

!     Arguments           :

!     Kind        Function         Name             Description

      integer(4), intent(in   ) :: ntrace           ! Dimension of the matrix
      integer(4), intent(in   ) :: nomat            ! Dimension of the off-diagonal entries
      real   (8), intent(in   ) :: alpha            ! Coefficient to multiply Ax with
      real   (8), intent(in   ) :: amat  (  nomat)  ! Off diagonal entries of A in LP format
      integer(4), intent(in   ) :: imat  (  nomat)  ! Pointer table off-diagonal entries
      real   (8), intent(in   ) :: diag  (  ntrace) ! diagonal of the matrix
      integer(4), intent(in   ) :: idiag (0:ntrace) ! position of the diagonals in amat
      real   (8), intent(in   ) :: xvec  (  ntrace) ! vector to multiply amat with
      real   (8), intent(in   ) :: beta             ! Coefficient to multiply yvec with
      real   (8), intent(inout) :: yvec  (  ntrace) ! yvec = beta*yvec + alpha*A*xvec

!        local variables

      integer(4)                :: i, j             ! Help variables for loop processing

!        WAQ timer

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "matvec", ithandl )

      yvec = yvec*beta/alpha

!        loop over rows of A

      do i = 1, ntrace

!           loop over non-zero entries of row(i) and multiply

         do j = idiag(i-1)+1 , idiag(i)
            if ( imat(j) > 0 ) yvec(i) = yvec(i) + amat(j) * xvec( imat(j) )
         enddo

!           add diagonal element

         yvec(i) = yvec(i) + diag(i) * xvec(i)

      enddo

      yvec = yvec*alpha

      if ( timon ) call timstop ( ithandl )

      return
      end

end module m_matvec
