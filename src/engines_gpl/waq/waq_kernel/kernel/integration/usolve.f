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
      module m_usolve

      implicit none

      contains


      subroutine usolve (  ntrace , nolay  , nsegl  , nomat  , amat   ,
     &                     imat   , diag   , idiag  , x      , rhs    ,
     &                     triwrk , iadd   , iexseg )

!     Deltares - Delft Software Department

!     Created   : November 1996 by Kian Tan

!     Function  : solve with lower triangular matrix:
!                 Let A = (D - L - U) , solve (D-L) x = y
!                 [ How about (I - L/D) x = y ?? ]

!     Modified  : July     2008, Leo Postma  : WAQ perfomance timers
!                 July     2009, Leo Postma  : double precission version
!                 July     2009, Leo Postma  : vertical expanded in this code
!                 November 2009, Leo Postma  : streamlined for parallel computing

      use timers
      implicit none

!     Arguments           :

!     Kind        Function         Name                  Description

      integer(4), intent(in   ) :: ntrace               ! dimension of matrix (length of diagonal)
      integer(4), intent(in   ) :: nolay                ! number of layers in the vertical
      integer(4), intent(in   ) :: nsegl                ! number of volumes per layer
      integer(4), intent(in   ) :: nomat                ! number of off-diagonal entries matrix
      real   (8), intent(in   ) :: amat  (  nomat )     ! off-diagonal entries matrix
      integer(4), intent(in   ) :: imat  (  nomat )     ! collumn nrs of off-diagonal entries matrix
      real   (8), intent(in   ) :: diag  (  ntrace)     ! diagonal entries of matrix
      integer(4), intent(in   ) :: idiag (0:ntrace)     ! start of rows in amat
      real   (8), intent(  out) :: x     (  ntrace)     ! x = M^{-1} y
      real   (8), intent(in   ) :: rhs   (  ntrace)     ! right hand side of this iteration only
      real   (8), intent(inout) :: triwrk(  nolay )     ! work array for vertical double sweep
      integer(4), intent(in   ) :: iadd                 ! offset for vertical off-diagonals
      integer(4), intent(in   ) :: iexseg(  ntrace)     ! = 0 if volume is fully explicit

!        local variables

      real   (8)                :: pivot                ! multiplier in double sweep vertical
      integer(4)                :: isegl                ! this volume of one layer
      integer(4)                :: iseg                 ! this volume
      integer(4)                :: ilay                 ! this layer
      integer(4)                :: jcol                 ! collumn counter for off-diagonals
      integer(4)                :: ilow, ihigh          ! loop boundaries

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "usolve", ithandl )

!        First copy rhs into x

      x = rhs

!        loop over all interior grid points in top layer.

      do isegl = nsegl, 1, -1

         do ilay = 1 , nolay

            iseg = isegl + (ilay-1)*nsegl
            if ( iexseg(iseg) .eq. 0 ) cycle
            ilow  = idiag(iseg-1) + 1
            ihigh = idiag(iseg)
            do jcol = ilow+iadd, ihigh
               if ( imat(jcol) .gt. iseg ) x(iseg) = x(iseg) - amat(jcol) * x( imat(jcol) )
            enddo
         enddo

         if ( nolay .eq. 1 ) then

            x(isegl) = x(isegl) / diag(isegl)

         else

!           direct tridiagonal solver expanded in this code

            pivot     = diag(isegl)
            x(isegl)  = x(isegl)/pivot
            ilow      = idiag(isegl-1) + 2
            iseg      = isegl
            triwrk(1) = amat(ilow)/pivot
            do ilay = 2, nolay
               iseg         = iseg + nsegl
               pivot        =   diag( iseg ) - amat(idiag(iseg-1)+1)*triwrk(ilay-1)
               x(iseg)      = ( x   ( iseg ) - amat(idiag(iseg-1)+1)*x(iseg-nsegl) ) / pivot
               triwrk(ilay) =                  amat(idiag(iseg-1)+2)                 / pivot
            enddo
            do ilay = nolay-1 , 1 , -1
               x(isegl+(ilay-1)*nsegl) = x(isegl+(ilay-1)*nsegl) - triwrk(ilay) * x(isegl+ilay*nsegl)
            enddo

         endif
      enddo

      if ( timon ) call timstop ( ithandl )
      return
      end

      end module m_usolve
