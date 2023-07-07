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
      module m_makpnt

      implicit none

      contains


      subroutine makpnt( nmax   , mmax   , kmax   , noseg  , nobnd  ,
     &                   noq    , noq1   , noq2   , lgrida , ipoint ,
     &                   cellpnt, flowpnt )

!       Deltares Software Centre

!>\file
!>            Makes from-to pointer table from regular matrix grid table
!>
!>            The routine scans the grid table to identify from-to entries./n
!>            If the active grid is 'active-only' contracted, then the pointer is.
!>            This is checked by comparing the obtained noq1, noq2 and noq3 with
!>            those in the lga file./n
!>            A backpointer from noseg to mnmaxk is constructed, to fill in the
!>            volumes in the complete matrix for solvers 18, 19 and Delpar./n
!>            A backpointer from noq to 3*mnmaxk - mnmax is constructed, to fill in
!>            areas and flows into the complete matrix for solvers 18, 19 and Delpar.

!     Created             : June   1996 by Jan van Beek
!     Modified            : May    2011    Leo Postma    : Fortran90 look and feel
!                           August 2011    Leo Postma    : adapted for 'active only' grids

!     Subroutines called  : -

!     Files               : -

      implicit none

!     Parameters

!     kind        function         name                 Descriptipon

      integer(4), intent(in   ) :: nmax               !< first grid dimension
      integer(4), intent(in   ) :: mmax               !< second grid dimension
      integer(4), intent(in   ) :: kmax               !< number of layers
      integer(4), intent(in   ) :: noseg              !< total number of volumes
      integer(4), intent(in   ) :: nobnd              !< total number of boundaries
      integer(4), intent(in   ) :: noq                !< total number of exchanges
      integer(4), intent(in   ) :: noq1               !< total number of exchanges in first direction
      integer(4), intent(in   ) :: noq2               !< total number of exchanges in second direction
      integer(4), intent(in   ) :: lgrida(nmax,mmax)  !< active grid table
      integer(4), intent(  out) :: ipoint(4,noq)      !< from to pointer
      integer(4), intent(  out) :: cellpnt(noseg)     !< from to pointer
      integer(4), intent(  out) :: flowpnt( noq )     !< from to pointer

!     Local declarations

      integer   n, m, k         !  loop variables in the matrix
      integer   iq, nq          !  help variable exchange nr.
      integer   ifrom , ito     !  from and to exchange pointers
      integer   ifrom1, itopl1  !  from-1 and to+1 exchange pointers
      integer   kmax1           !  number of layers
      integer   nosegl          !  number of volumes per layer
      integer   nobndl          !  number of boundaries per layer
      integer   noq1l           !  number of exchanges first direction per layer
      integer   noq2l           !  number of exchanges second direction per layer
      logical   contracted      !  if true, it is an 'active only' grid
      integer   mnmax           !  help variable nmax*mmax (horizontal size of the matrix)
      integer   mnmaxk          !  help variable mnmax*kmax (total size of the cube)
      integer   nqn, nqc        !  non contracted and contracted counters

!     Some init

      kmax1  = max(kmax,1)
      nosegl = noseg / kmax1
      nobndl = nobnd / kmax1
      noq1l  = noq1  / kmax1
      noq2l  = noq2  / kmax1
      mnmax  = nmax*mmax
      mnmaxk = mnmax*kmax1
      contracted = .false.
      if ( nosegl .lt. mnmax ) contracted = .true.

!     construct backpointers from active-only grid to total grid, both segments and exchanges

      cellpnt = 0
      flowpnt = 0
      nqn = 0
      do m = 1, mmax
         do n = 1, nmax
            nqn = nqn + 1
            if ( lgrida(n,m) .gt. 0 ) then
               do k = 1, kmax1
                  cellpnt( lgrida(n,m) + (k-1)*nosegl ) = nqn + (k-1)*mnmax
               enddo
            endif
         enddo
      enddo
      do n = 1, noseg
         if ( cellpnt(n) .le. 0 ) cellpnt(n) = n       ! happens if 1-1 coupling
      enddo

!     initialise all pointers as inactive

      ipoint = 0

!     Horizontal 1st direction

      nqn = 0
      nqc = 0
      do m = 1 , mmax
         do n = 1 , nmax - 1
            nqn = nqn + 1
            ifrom = lgrida( n  , m )
            if ( ifrom .eq. 0 ) cycle
            ito   = lgrida( n+1, m )
            if ( ito   .eq. 0 ) cycle
            if ( ifrom .lt. 0 .and. ito   .lt. 0 ) cycle
            nqc = nqc + 1
            ifrom1 = 0
            itopl1 = 0
            if ( n .gt.      1 ) ifrom1= lgrida( n-1, m )
            if ( n .lt. nmax-1 ) itopl1= lgrida( n+2, m )
            nq = nqn
            if ( contracted ) nq = nqc
            do k = 1 , kmax1
                       iq  = (k-1)*noq1l + nq
               flowpnt(iq) = (k-1)*mnmax + nqn
               if ( ifrom1 .gt. 0 ) then
                  ipoint(3,iq) = (k-1)*nosegl + ifrom1
               else
                  ipoint(3,iq) = (1-k)*nobndl + ifrom1
               endif
               if ( ifrom  .gt. 0 ) then
                  ipoint(1,iq) = (k-1)*nosegl + ifrom
               else
                  ipoint(1,iq) = (1-k)*nobndl + ifrom
               endif
               if ( ito    .gt. 0 ) then
                  ipoint(2,iq) = (k-1)*nosegl + ito
               else
                  ipoint(2,iq) = (1-k)*nobndl + ito
               endif
               if ( itopl1 .gt. 0 ) then
                  ipoint(4,iq) = (k-1)*nosegl + itopl1
               else
                  ipoint(4,iq) = (1-k)*nobndl + itopl1
               endif
            enddo
         enddo
         if ( .not. contracted ) nqn = nqn + 1
      enddo
      nq = nqn
      if ( contracted ) nq = nqc
      if ( nq .ne. noq1l ) write ( 338, * ) ' ERROR1 in makpnt: ',nq,noq1l

!     Horizontal 2nd direction

      nqn = mnmaxk
      nqc = noq1
      do m = 1 , mmax - 1
         do n = 1 , nmax
            nqn = nqn + 1
            ifrom = lgrida( n, m   )
            if ( ifrom .eq. 0 ) cycle
            ito   = lgrida( n, m+1 )
            if ( ito   .eq. 0 ) cycle
            if ( ifrom .lt. 0 .and. ito   .lt. 0 ) cycle
            ifrom1 = 0
            itopl1 = 0
            if ( m .gt.      1 ) ifrom1= lgrida( n, m-1 )
            if ( m .lt. mmax-1 ) itopl1= lgrida( n, m+2 )
            nqc = nqc + 1
            nq = nqn
            if ( contracted ) nq = nqc
            do k = 1 , kmax1
                       iq  = (k-1)*noq2l + nq
               flowpnt(iq) = (k-1)*mnmax + nqn - mnmaxk
               if ( ifrom1 .gt. 0 ) then
                  ipoint(3,iq) = (k-1)*nosegl + ifrom1
               else
                  ipoint(3,iq) = (1-k)*nobndl + ifrom1
               endif
               if ( ifrom  .gt. 0 ) then
                  ipoint(1,iq) = (k-1)*nosegl + ifrom
               else
                  ipoint(1,iq) = (1-k)*nobndl + ifrom
               endif
               if ( ito    .gt. 0 ) then
                  ipoint(2,iq) = (k-1)*nosegl + ito
               else
                  ipoint(2,iq) = (1-k)*nobndl + ito
               endif
               if ( itopl1 .gt. 0 ) then
                  ipoint(4,iq) = (k-1)*nosegl + itopl1
               else
                  ipoint(4,iq) = (1-k)*nobndl + itopl1
               endif
            enddo
         enddo
      enddo
      nqn = nqn + nmax
      nq = nqn
      if ( contracted ) nq = nqc
      if ( nq .ne. noq1+noq2l ) write ( 338, * ) ' ERROR2 in makpnt: ',nq,noq1+noq2l

!     Vertical 3d direction

      nqn = mnmaxk*2
      nqc = noq1 + noq2
      do k = 1 , kmax1-1
         do m = 1 , mmax
            do n = 1 , nmax
               nqn = nqn + 1
               ifrom = lgrida( n, m )
               if ( ifrom .gt. 0 ) then
                  nqc = nqc + 1
                  nq = nqn
                  if ( contracted ) nq = nqc
                          iq  = nq
                  flowpnt(iq) = nqn - 2*mnmaxk
                  ifrom1 = 0
                  itopl1 = 0
                  if ( k .gt.       1 ) ifrom1 = (k-2)*nosegl + ifrom
                  if ( k .lt. kmax1-1 ) itopl1 = (k+1)*nosegl + ifrom
                  ipoint(1,iq) = (k-1)*nosegl + ifrom
                  ipoint(2,iq) =  k   *nosegl + ifrom
                  ipoint(3,iq) = ifrom1
                  ipoint(4,iq) = itopl1
               endif
            enddo
         enddo
      enddo
      nq = nqn
      if ( contracted ) nq = nqc
      if ( nq .ne. noq ) write ( 338, * ) ' ERROR3 in makpnt: ', nq, noq
      do n = 1, noq1
         if ( flowpnt(n) .le. 0 ) flowpnt(n) = n       ! happens if 1-1 coupling
      enddo
      do n = noq1+1, noq1+noq2
         if ( flowpnt(n) .le. 0 ) flowpnt(n) = n - noq1
      enddo
      do n = noq1+noq2+1, noq
         if ( flowpnt(n) .le. 0 ) flowpnt(n) = n - noq1 - noq2
      enddo

      return
      end subroutine
      end module m_makpnt
