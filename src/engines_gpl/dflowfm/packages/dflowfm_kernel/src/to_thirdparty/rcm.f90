!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

!
!
!> @file rcm.f90
!! Reverse-Cuthill-Mckee algorithm + helpers.
!! Used for network-node renumbering to improve cache efficiency
!! of the unstructured flow solver.
!<
module m_rcm
   use precision, only: dp
   implicit none
contains
   function adj_bandwidth(node_num, adj_num, adj_row, adj)

!*****************************************************************************80
!
!! ADJ_BANDWIDTH computes the bandwidth of an adjacency matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 March 2005
!
!  Author:
!
!    John Burkardt
!
!  Author:
!
!    Original FORTRAN77 version by Alan George, Joseph Liu.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Alan George, Joseph Liu,
!    Computer Solution of Large Sparse Positive Definite Systems,
!    Prentice Hall, 1981.
!
!  Parameters:
!
!    Input, integer   NODE_NUM, the number of nodes.
!
!    Input, integer   ADJ_NUM, the number of adjacency entries.
!
!    Input, integer   ADJ_ROW(NODE_NUM+1).  Information about
!    row I is stored in entries ADJ_ROW(I) through ADJ_ROW(I+1)-1 of ADJ.
!
!    Input, integer   ADJ(ADJ_NUM), the adjacency structure.
!    For each row, it contains the column indices of the nonzero entries.
!
!    Output, integer   ADJ_BANDWIDTH, the bandwidth of the adjacency
!    matrix.
!
      integer adj_num
      integer node_num

      integer adj(adj_num)
      integer adj_bandwidth
      integer adj_row(node_num + 1)
      integer band_hi
      integer band_lo
      integer col
      integer i
      integer j

      band_lo = 0
      band_hi = 0

      do i = 1, node_num

         do j = adj_row(i), adj_row(i + 1) - 1
            col = adj(j)
            band_lo = max(band_lo, i - col)
            band_hi = max(band_hi, col - i)
         end do

      end do

      adj_bandwidth = band_lo + 1 + band_hi

      return
   end
   function adj_contains_ij(node_num, adj_num, adj_row, adj, i, j)

!*****************************************************************************80
!
!! ADJ_CONTAINS_IJ determines if (I,J) is in an adjacency structure.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 October 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer   NODE_NUM, the number of nodes.
!
!    Input, integer   ADJ_NUM, the number of adjacency entries.
!
!    Input, integer   ADJ_ROW(NODE_NUM+1).  Information about
!    row I is stored in entries ADJ_ROW(I) through ADJ_ROW(I+1)-1 of ADJ.
!
!    Input, integer   ADJ(ADJ_NUM), the adjacency structure.
!
!    Input, integer   I, J, the two nodes, for which we want to know
!    whether I is adjacent to J.
!
!    Output, logical ADJ_CONTAINS_IJ, is TRUE if I = J, or the adjacency
!    structure contains the information that I is adjacent to J.
!
      implicit none

      integer adj_num
      integer node_num

      integer adj(adj_num)
      logical adj_contains_ij
      integer adj_row(node_num + 1)
      integer i
      integer j
      integer k
      integer khi
      integer klo
!
!  Symmetric entries are not stored.
!
      if (i == j) then
         adj_contains_ij = .true.
         return
      end if
!
!  Illegal I, J entries.
!
      if (node_num < i) then
         adj_contains_ij = .false.
         return
      else if (i < 1) then
         adj_contains_ij = .false.
         return
      else if (node_num < j) then
         adj_contains_ij = .false.
         return
      else if (j < 1) then
         adj_contains_ij = .false.
         return
      end if
!
!  Search the adjacency entries already stored for row I,
!  to see if J has already been stored.
!
      klo = adj_row(i)
      khi = adj_row(i + 1) - 1

      do k = klo, khi

         if (adj(k) == j) then
            adj_contains_ij = .true.
            return
         end if

      end do

      adj_contains_ij = .false.

      return
   end
   subroutine adj_insert_ij(node_num, adj_max, adj_num, adj_row, adj, i, j)

!*****************************************************************************80
!
!! ADJ_INSERT_IJ inserts (I,J) into an adjacency structure.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer   NODE_NUM, the number of nodes.
!
!    Input, integer   ADJ_MAX, the maximum number of adjacency
!    entries.
!
!    Input/output, integer   ADJ_NUM, the number of adjacency
!    entries.
!
!    Input/output, integer   ADJ_ROW(NODE_NUM+1).  Information about
!    row I is stored in entries ADJ_ROW(I) through ADJ_ROW(I+1)-1 of ADJ.
!
!    Input/output, integer   ADJ(ADJ_NUM), the adjacency structure.
!
!    Input, integer   I, J, the two nodes which are adjacent.
!
      implicit none

      integer adj_max
      integer node_num

      integer adj(adj_max)
      integer adj_num
      integer adj_row(node_num + 1)
      integer i
      integer j
      integer j_spot
      integer k
      integer :: ii, jj
!
!  A new adjacency entry must be made.
!  Check that we're not exceeding the storage allocation for ADJ.
!
      if (adj_max < adj_num + 1) then
         write (*, '(a)') ' '
         write (*, '(a)') 'ADJ_INSERT_IJ - Fatal error!'
         write (*, '(a)') '  All available storage has been used.'
         write (*, '(a)') '  No more information can be stored!'
         write (*, '(a)') '  This error occurred for '
         write (*, '(a,i8)') '  Row I =    ', i
         write (*, '(a,i8)') '  Column J = ', j
         stop
      end if
!
!  The action is going to occur between ADJ_ROW(I) and ADJ_ROW(I+1)-1:
!
      j_spot = adj_row(i)

      do k = adj_row(i), adj_row(i + 1) - 1

         if (adj(k) == j) then
            return
         else if (adj(k) < j) then
            j_spot = k + 1
         else
            exit
         end if

      end do

      do jj = adj_num, j_spot, -1
         adj(jj + 1) = adj(jj)
      end do
      adj(j_spot) = j

      do ii = i + 1, node_num + 1
         adj_row(ii) = adj_row(ii) + 1
      end do
      adj_num = adj_num + 1

      return
   end

   function adj_perm_bandwidth(node_num, adj_num, adj_row, adj, perm, perm_inv)

!*****************************************************************************80
!
!! ADJ_PERM_BANDWIDTH computes the bandwidth of a permuted adjacency matrix.
!
!  Discussion:
!
!    The matrix is defined by the adjacency information and a permutation.
!
!    The routine also computes the bandwidth and the size of the envelope.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 March 2005
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Alan George, Joseph Liu,
!    Computer Solution of Large Sparse Positive Definite Systems,
!    Prentice Hall, 1981.
!
!  Parameters:
!
!    Input, integer   NODE_NUM, the number of nodes.
!
!    Input, integer   ADJ_NUM, the number of adjacency entries.
!
!    Input, integer   ADJ_ROW(NODE_NUM+1).  Information about
!    row I is stored in entries ADJ_ROW(I) through ADJ_ROW(I+1)-1 of ADJ.
!
!    Input, integer   ADJ(ADJ_NUM), the adjacency structure.
!    For each row, it contains the column indices of the nonzero entries.
!
!    Input, integer   PERM(NODE_NUM), PERM_INV(NODE_NUM), the
!    permutation and inverse permutation.
!
!    Output, integer   ADJ_PERM_BANDWIDTH, the bandwidth of the
!    permuted adjacency matrix.
!
      implicit none

      integer adj_num
      integer node_num

      integer adj(adj_num)
      integer adj_perm_bandwidth
      integer adj_row(node_num + 1)
      integer band_hi
      integer band_lo
      integer col
      integer i
      integer j
      integer perm(node_num)
      integer perm_inv(node_num)

      band_lo = 0
      band_hi = 0

      do i = 1, node_num

         do j = adj_row(perm(i)), adj_row(perm(i) + 1) - 1
            col = perm_inv(adj(j))
            band_lo = max(band_lo, i - col)
            band_hi = max(band_hi, col - i)
         end do

      end do

      adj_perm_bandwidth = band_lo + 1 + band_hi

      return
   end
   subroutine adj_perm_show(node_num, adj_num, adj_row, adj, perm, perm_inv)

!*****************************************************************************80
!
!! ADJ_PERM_SHOW displays a symbolic picture of a permuted adjacency matrix.
!
!  Discussion:
!
!    The matrix is defined by the adjacency information and a permutation.
!
!    The routine also computes the bandwidth and the size of the envelope.
!
!    If no permutation has been done, you must set PERM(I) = PERM_INV(I) = I
!    before calling this routine.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 October 2003
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Alan George, Joseph Liu,
!    Computer Solution of Large Sparse Positive Definite Systems,
!    Prentice Hall, 1981.
!
!  Parameters:
!
!    Input, integer   NODE_NUM, the number of nodes.
!
!    Input, integer   ADJ_NUM, the number of adjacency entries.
!
!    Input, integer   ADJ_ROW(NODE_NUM+1).  Information about
!    row I is stored in entries ADJ_ROW(I) through ADJ_ROW(I+1)-1 of ADJ.
!
!    Input, integer   ADJ(ADJ_NUM), the adjacency structure.
!    For each row, it contains the column indices of the nonzero entries.
!
!    Input, integer   PERM(NODE_NUM), PERM_INV(NODE_NUM), the
!    permutation and inverse permutation.
!
      implicit none

      integer, parameter :: n_max = 100

      integer adj_num
      integer node_num

      integer adj(adj_num)
      integer adj_row(node_num + 1)
      character band(n_max)
      integer band_lo
      integer col
      integer i
      integer j
      integer k
      integer nonzero_num
      integer perm(node_num)
      integer perm_inv(node_num)

      band_lo = 0
      nonzero_num = 0

      if (n_max < node_num) then
         write (*, '(a)') ' '
         write (*, '(a)') 'ADJ_PERM_SHOW - Fatal error!'
         write (*, '(a)') '  NODE_NUM is too large!'
         write (*, '(a,i8)') '  Maximum legal value is ', n_max
         write (*, '(a,i8)') '  Your input value was   ', node_num
         stop
      end if

      write (*, '(a)') ' '
      write (*, '(a)') '  Nonzero structure of matrix:'
      write (*, '(a)') ' '

      do i = 1, node_num

         do k = 1, node_num
            band(k) = '.'
         end do

         band(i) = 'D'

         do j = adj_row(perm(i)), adj_row(perm(i) + 1) - 1

            col = perm_inv(adj(j))

            if (col < i) then
               nonzero_num = nonzero_num + 1
            end if

            band_lo = max(band_lo, i - col)

            if (col /= i) then
               band(col) = 'X'
            end if

         end do

         write (*, '(2x,i8,1x,100a1)') i, band(1:node_num)

      end do

      write (*, '(a)') ' '
      write (*, '(a,i8)') '  Lower bandwidth = ', band_lo
      write (*, '(a,i8,a)') '  Lower envelope contains ', &
         nonzero_num, ' nonzeros.'

      return
   end
   subroutine adj_print(node_num, adj_num, adj_row, adj, title)

!*****************************************************************************80
!
!! ADJ_PRINT prints adjacency information.
!
!  Discussion:
!
!    The list has the form:
!
!    Row   Nonzeros
!
!    1       2   5   9
!    2       7   8   9   15   78   79   81  86  91  99
!          100 103
!    3      48  49  53
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 December 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer   NODE_NUM, the number of nodes.
!
!    Input, integer   ADJ_NUM, the number of adjacency entries.
!
!    Input, integer   ADJ_ROW(NODE_NUM+1), organizes the adjacency
!    entries into rows.  The entries for row I are in entries ADJ_ROW(I)
!    through ADJ_ROW(I+1)-1.
!
!    Input, integer   ADJ(ADJ_NUM), the adjacency structure, which
!    contains, for each row, the column indices of the nonzero entries.
!
!    Input, character ( len = * ) TITLE, a title to be printed.
!
      implicit none

      integer adj_num
      integer node_num

      integer adj(adj_num)
      integer adj_row(node_num + 1)
      character(len=*) title

      call adj_print_some(node_num, 1, node_num, adj_num, adj_row, adj, title)

      return
   end
   subroutine adj_print_some(node_num, node_lo, node_hi, adj_num, adj_row, &
                             adj, title)

!*****************************************************************************80
!
!! ADJ_PRINT_SOME prints some adjacency information.
!
!  Discussion:
!
!    The list has the form:
!
!    Row   Nonzeros
!
!    1       2   5   9
!    2       7   8   9   15   78   79   81  86  91  99
!          100 103
!    3      48  49  53
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 December 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer   NODE_NUM, the number of nodes.
!
!    Input, integer   NODE_LO, NODE_HI, the first and last nodes for
!    which the adjacency information is to be printed.
!
!    Input, integer   ADJ_NUM, the number of adjacency entries.
!
!    Input, integer   ADJ_ROW(NODE_NUM+1), organizes the adjacency
!    entries into rows.  The entries for row I are in entries ADJ_ROW(I)
!    through ADJ_ROW(I+1)-1.
!
!    Input, integer   ADJ(ADJ_NUM), the adjacency structure, which
!    contains, for each row, the column indices of the nonzero entries.
!
!    Input, character ( len = * ) TITLE, a title to be printed.
!
      implicit none

      integer adj_num
      integer node_num

      integer adj(adj_num)
      integer adj_row(node_num + 1)
      integer i
      integer jhi
      integer jlo
      integer jmax
      integer jmin
      integer node_hi
      integer node_lo
      character(len=*) title

      if (0 < len_trim(title)) then
         write (*, '(a)') ' '
         write (*, '(a)') trim(title)
      end if

      write (*, '(a)') ' '
      write (*, '(a)') '  Sparse adjacency structure:'
      write (*, '(a)') ' '
      write (*, '(a,i8)') '  Number of nodes       = ', node_num
      write (*, '(a,i8)') '  Number of adjacencies = ', adj_num
      write (*, '(a)') ' '
      write (*, '(a)') '  Node Min Max      Nonzeros '
      write (*, '(a)') ' '

      do i = node_lo, node_hi

         jmin = adj_row(i)
         jmax = adj_row(i + 1) - 1

         if (jmax < jmin) then

            write (*, '(2x,3i4)') i, jmin, jmax

         else

            do jlo = jmin, jmax, 5

               jhi = min(jlo + 4, jmax)

               if (jlo == jmin) then
                  write (*, '(2x,3i4,3x,5i8)') i, jmin, jmax, adj(jlo:jhi)
               else
                  write (*, '(2x,12x,3x,5i8)') adj(jlo:jhi)
               end if

            end do

         end if

      end do

      return
   end
   subroutine adj_set(node_num, adj_max, adj_num, adj_row, adj, irow, jcol)

!*****************************************************************************80
!
!! ADJ_SET sets up the adjacency information.
!
!  Discussion:
!
!    The routine records the locations of each nonzero element,
!    one at a time.
!
!    The first call for a given problem should be with IROW or ICOL
!    negative.  This is a signal indicating the data structure should
!    be initialized.
!
!    Then, for each case in which A(IROW,JCOL) is nonzero, or
!    in which IROW is adjacent to JCOL, call this routine once
!    to record that fact.
!
!    Diagonal entries are not to be stored.
!
!    The matrix is assumed to be symmetric, so setting I adjacent to J
!    will also set J adjacent to I.
!
!    Repeated calls with the same values of IROW and JCOL do not
!    actually hurt.  No extra storage will be allocated.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 October 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer   NODE_NUM, the number of nodes.
!
!    Input, integer   ADJ_MAX, the maximum dimension of the
!    adjacency array.
!
!    Input/output, integer   ADJ_NUM, the number of adjaceny entries.
!
!    Input/output, integer   ADJ_ROW(NODE_NUM+1).  Information about
!    row I is stored in entries ADJ_ROW(I) through ADJ_ROW(I+1)-1 of ADJ.
!
!    Input/output, integer   ADJ(ADJ_NUM), the adjacency structure.
!
!    Input, integer   IROW, JCOL, the row and column indices of a
!    nonzero entry of the matrix.
!
      implicit none

      integer adj_max
      integer node_num

      integer adj(adj_max)
      integer adj_num
      integer adj_row(node_num + 1)
      integer irow
      integer jcol
!
!  Negative IROW or JCOL indicates the data structure should be initialized.
!
      if (irow < 0 .or. jcol < 0) then

         write (*, '(a)') ' '
         write (*, '(a)') 'ADJ_SET - Note:'
         write (*, '(a)') '  Initializing adjacency information.'
         write (*, '(a,i8)') '  Number of nodes NODE_NUM =  ', node_num
         write (*, '(a,i8)') '  Maximum adjacency ADJ_MAX = ', adj_max

         adj_num = 0
         adj_row(1:node_num + 1) = 1
         adj(1:adj_max) = 0

         return

      end if
!
!  Diagonal entries are not stored.
!
      if (irow == jcol) then
         return
      end if

      if (node_num < irow) then
         write (*, '(a)') ' '
         write (*, '(a)') 'ADJ_SET - Fatal error!'
         write (*, '(a)') '  NODE_NUM < IROW.'
         write (*, '(a,i8)') '  IROW =     ', irow
         write (*, '(a,i8)') '  NODE_NUM = ', node_num
         stop
      else if (irow < 1) then
         write (*, '(a)') ' '
         write (*, '(a)') 'ADJ_SET - Fatal error!'
         write (*, '(a)') '  IROW < 1.'
         write (*, '(a,i8)') '  IROW = ', irow
         stop
      else if (node_num < jcol) then
         write (*, '(a)') ' '
         write (*, '(a)') 'ADJ_SET - Fatal error!'
         write (*, '(a)') '  NODE_NUM < JCOL.'
         write (*, '(a,i8)') '  JCOL =     ', jcol
         write (*, '(a,i8)') '  NODE_NUM = ', node_num
         stop
      else if (jcol < 1) then
         write (*, '(a)') ' '
         write (*, '(a)') 'ADJ_SET - Fatal error!'
         write (*, '(a)') '  JCOL < 1.'
         write (*, '(a,i8)') '  JCOL = ', jcol
         stop
      end if

      if (.not. &
          adj_contains_ij(node_num, adj_num, adj_row, adj, irow, jcol)) then
         call adj_insert_ij(node_num, adj_max, adj_num, adj_row, adj, irow, jcol)
      end if

      if (.not. &
          adj_contains_ij(node_num, adj_num, adj_row, adj, jcol, irow)) then
         call adj_insert_ij(node_num, adj_max, adj_num, adj_row, adj, jcol, irow)
      end if

      return
   end
   subroutine adj_show(node_num, adj_num, adj_row, adj)

!*****************************************************************************80
!
!! ADJ_SHOW displays a symbolic picture of an adjacency matrix.
!
!  Discussion:
!
!    The matrix is defined by the adjacency information and a permutation.
!
!    The routine also computes the bandwidth and the size of the envelope.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 March 2005
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Alan George, Joseph Liu,
!    Computer Solution of Large Sparse Positive Definite Systems,
!    Prentice Hall, 1981.
!
!  Parameters:
!
!    Input, integer   NODE_NUM, the number of nodes.
!
!    Input, integer   ADJ_NUM, the number of adjacency entries.
!
!    Input, integer   ADJ_ROW(NODE_NUM+1).  Information about
!    row I is stored in entries ADJ_ROW(I) through ADJ_ROW(I+1)-1 of ADJ.
!
!    Input, integer   ADJ(ADJ_NUM), the adjacency structure.
!    For each row, it contains the column indices of the nonzero entries.
!
      implicit none

      integer, parameter :: n_max = 100

      integer adj_num
      integer node_num

      integer adj(adj_num)
      integer adj_row(node_num + 1)
      character band(n_max)
      integer band_lo
      integer col
      integer i
      integer j
      integer k
      integer nonzero_num

      band_lo = 0
      nonzero_num = 0

      if (n_max < node_num) then
         write (*, '(a)') ' '
         write (*, '(a)') 'ADJ_SHOW - Fatal error!'
         write (*, '(a)') '  NODE_NUM is too large!'
         write (*, '(a,i8)') '  Maximum legal value is ', n_max
         write (*, '(a,i8)') '  Your input value was   ', node_num
         stop
      end if

      write (*, '(a)') ' '
      write (*, '(a)') '  Nonzero structure of matrix:'
      write (*, '(a)') ' '

      do i = 1, node_num

         do k = 1, node_num
            band(k) = '.'
         end do

         band(i) = 'D'

         do j = adj_row(i), adj_row(i + 1) - 1

            col = adj(j)

            if (col < i) then
               nonzero_num = nonzero_num + 1
            end if

            band_lo = max(band_lo, i - col)
            band(col) = 'X'

         end do

         write (*, '(2x,i8,1x,100a1)') i, band(1:node_num)

      end do

      write (*, '(a)') ' '
      write (*, '(a,i8)') '  Lower bandwidth = ', band_lo
      write (*, '(a,i8,a)') '  Lower envelope contains ', &
         nonzero_num, ' nonzeros.'

      return
   end
   subroutine degree(root, adj_num, adj_row, adj, mask, deg, iccsze, ls, &
                     node_num)

!*****************************************************************************80
!
!! DEGREE computes the degrees of the nodes in the connected component.
!
!  Discussion:
!
!    The connected component is specified by MASK and ROOT.
!    Nodes for which MASK is zero are ignored.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 January 2003
!
!  Author:
!
!    Original FORTRAN77 version by Alan George, Joseph Liu.
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Alan George, Joseph Liu,
!    Computer Solution of Large Sparse Positive Definite Systems,
!    Prentice Hall, 1981.
!
!  Parameters:
!
!    Input, integer   ROOT, the node that defines the connected
!    component.
!
!    Input, integer   ADJ_NUM, the number of adjacency entries.
!
!    Input, integer   ADJ_ROW(NODE_NUM+1).  Information about
!    row I is stored in entries ADJ_ROW(I) through ADJ_ROW(I+1)-1 of ADJ.
!
!    Input, integer   ADJ(ADJ_NUM), the adjacency structure.
!    For each row, it contains the column indices of the nonzero entries.
!
!    Input, integer   MASK(NODE_NUM), is nonzero for those nodes
!    which are to be considered.
!
!    Output, integer   DEG(NODE_NUM), contains, for each  node in
!    the connected component, its degree.
!
!    Output, integer   ICCSIZE, the number of nodes in the
!    connected component.
!
!    Output, integer   LS(NODE_NUM), stores in entries 1 through
!    ICCSIZE the nodes in the connected component, starting with ROOT, and
!    proceeding by levels.
!
!    Input, integer   NODE_NUM, the number of nodes.
!
      implicit none

      integer adj_num
      integer node_num

      integer adj(adj_num)
      integer adj_row(node_num + 1)
      integer deg(node_num)
      integer i
      integer iccsze
      integer ideg
      integer j
      integer jstop
      integer jstrt
      integer lbegin
      integer ls(node_num)
      integer lvlend
      integer lvsize
      integer mask(node_num)
      integer nbr
      integer node
      integer root
!
!  The sign of ADJ_ROW(I) is used to indicate if node I has been considered.
!
      ls(1) = root
      adj_row(root) = -adj_row(root)
      lvlend = 0
      iccsze = 1
!
!  LBEGIN is the pointer to the beginning of the current level, and
!  LVLEND points to the end of this level.
!
      do

         lbegin = lvlend + 1
         lvlend = iccsze
!
!  Find the degrees of nodes in the current level,
!  and at the same time, generate the next level.
!
         do i = lbegin, lvlend

            node = ls(i)
            jstrt = -adj_row(node)
            jstop = abs(adj_row(node + 1)) - 1
            ideg = 0

            do j = jstrt, jstop

               nbr = adj(j)

               if (mask(nbr) /= 0) then

                  ideg = ideg + 1

                  if (0 <= adj_row(nbr)) then
                     adj_row(nbr) = -adj_row(nbr)
                     iccsze = iccsze + 1
                     ls(iccsze) = nbr
                  end if

               end if

            end do

            deg(node) = ideg

         end do
!
!  Compute the current level width.
!
         lvsize = iccsze - lvlend
!
!  If the current level width is nonzero, generate another level.
!
         if (lvsize == 0) then
            exit
         end if

      end do
!
!  Reset ADJ_ROW to its correct sign and return.
!
      do i = 1, iccsze
         node = ls(i)
         adj_row(node) = -adj_row(node)
      end do

      return
   end
   subroutine genrcm(node_num, adj_num, adj_row, adj, perm)

!*****************************************************************************80
!
!! GENRCM finds the reverse Cuthill-Mckee ordering for a general graph.
!
!  Discussion:
!
!    For each connected component in the graph, the routine obtains
!    an ordering by calling RCM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 January 2003
!
!  Author:
!
!    Original FORTRAN77 version by Alan George, Joseph Liu.
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Alan George, Joseph Liu,
!    Computer Solution of Large Sparse Positive Definite Systems,
!    Prentice Hall, 1981.
!
!  Parameters:
!
!    Input, integer   NODE_NUM, the number of nodes.
!
!    Input, integer   ADJ_NUM, the number of adjacency entries.
!
!    Input, integer   ADJ_ROW(NODE_NUM+1).  Information about
!    row I is stored in entries ADJ_ROW(I) through ADJ_ROW(I+1)-1 of ADJ.
!
!    Input, integer   ADJ(ADJ_NUM), the adjacency structure.
!    For each row, it contains the column indices of the nonzero entries.
!
!    Output, integer   PERM(NODE_NUM), the RCM ordering.
!
!  Local Parameters:
!
!    Local, integer LEVEL_ROW(NODE_NUM+1), the index vector for a level
!    structure.  The level structure is stored in the currently unused
!    spaces in the permutation vector PERM.
!
!    Local, integer MASK(NODE_NUM), marks variables that have been numbered.
!
      implicit none

      integer adj_num
      integer node_num

      integer adj(adj_num)
      integer adj_row(node_num + 1)
      integer i
      integer iccsze
      integer, allocatable :: mask(:)
      integer level_num
      integer, allocatable :: level_row(:)
      integer num
      integer perm(node_num)
      integer root

      allocate (mask(node_num))
      allocate (level_row(node_num + 1))
      mask(1:node_num) = 1

      num = 1

      do i = 1, node_num
!
!  For each masked connected component...
!
         if (mask(i) /= 0) then

            root = i
!
!  Find a pseudo-peripheral node ROOT.  The level structure found by
!  ROOT_FIND is stored starting at PERM(NUM).
!
            call root_find(root, adj_num, adj_row, adj, mask, level_num, &
                           level_row, perm(num), node_num)
!
!  RCM orders the component using ROOT as the starting node.
!
            call rcm(root, adj_num, adj_row, adj, mask, perm(num), iccsze, &
                     node_num)

            num = num + iccsze
!
!  We can stop once every node is in one of the connected components.
!
            if (node_num < num) then
               return
            end if

         end if

      end do

      deallocate (mask)
      deallocate (level_row)
      return
   end
   subroutine graph_01_adj(node_num, adj_num, adj_row, adj)

!*****************************************************************************80
!
!! GRAPH_01_ADJ returns the adjacency vector for graph 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 October 2003
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Alan George, Joseph Liu,
!    Computer Solution of Large Sparse Positive Definite Systems,
!    Prentice Hall, 1981.
!
!  Parameters:
!
!    Input, integer   NODE_NUM, the number of nodes.
!
!    Input, integer   ADJ_NUM, the number of adjacencies.
!
!    Output, integer   ADJ_ROW(NODE_NUM+1), node pointers into ADJ.
!
!    Output, integer   ADJ(ADJ_NUM), the adjacency information.
!
      implicit none

      integer adj_num
      integer node_num

      integer adj(adj_num)
      integer adj_row(node_num + 1)

      adj(1:adj_num) = (/ &
                       4, 6, &
                       3, 5, 7, 10, &
                       2, 4, 5, &
                       1, 3, 6, 9, &
                       2, 3, 7, &
                       1, 4, 7, 8, &
                       2, 5, 6, 8, &
                       6, 7, &
                       4, &
                       2/)

      adj_row(1:node_num + 1) = (/1, 3, 7, 10, 14, 17, 21, 25, 27, 28, 29/)

      return
   end
   subroutine graph_01_size(node_num, adj_num)

!*****************************************************************************80
!
!! GRAPH_01_ADJ_NUM returns the number of adjacencies for graph 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 October 2003
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Alan George, Joseph Liu,
!    Computer Solution of Large Sparse Positive Definite Systems,
!    Prentice Hall, 1981.
!
!  Parameters:
!
!    Output, integer   NODE_NUM, the number of items that can
!    be adjacent.
!
!    Output, integer   ADJ_NUM, the number of adjacencies.
!
      implicit none

      integer adj_num
      integer node_num

      node_num = 10
      adj_num = 28

      return
   end
   subroutine i4_swap(i, j)

!*****************************************************************************80
!
!! I4_SWAP swaps two I4's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 November 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, integer   I, J.  On output, the values of I and
!    J have been interchanged.
!
      implicit none

      integer i
      integer j
      integer k

      k = i
      i = j
      j = k

      return
   end
   function i4_uniform(a, b, seed)

!*****************************************************************************80
!
!! I4_UNIFORM returns a scaled pseudorandom I4.
!
!  Discussion:
!
!    An I4 is an integer   value.
!
!    The pseudorandom number will be scaled to be uniformly distributed
!    between A and B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 November 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Springer Verlag, pages 201-202, 1983.
!
!    Pierre L'Ecuyer,
!    Random Number Generation,
!    in Handbook of Simulation,
!    edited by Jerry Banks,
!    Wiley Interscience, page 95, 1998.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, pages 362-376, 1986.
!
!    Peter Lewis, Allen Goodman, James Miller
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, pages 136-143, 1969.
!
!  Parameters:
!
!    Input, integer   A, B, the limits of the interval.
!
!    Input/output, integer   SEED, the "seed" value,
!    which should NOT be 0.  On output, SEED has been updated.
!
!    Output, integer   I4_UNIFORM, a number between
!    A and B.
!
      implicit none

      integer a
      integer b
      integer i4_uniform
      integer k
      real r
      integer seed
      integer value

      if (seed == 0) then
         write (*, '(a)') ' '
         write (*, '(a)') 'I4_UNIFORM - Fatal error!'
         write (*, '(a)') '  Input value of SEED = 0.'
         stop
      end if

      k = seed / 127773

      seed = 16807 * (seed - k * 127773) - k * 2836

      if (seed < 0) then
         seed = seed + 2147483647
      end if

      r = real(seed) * 4.656612875e-10
!
!  Scale R to lie between A-0.5 and B+0.5.
!
      r = (1.0e+00 - r) * (real(min(a, b)) - 0.5e+00) &
          + r * (real(max(a, b)) + 0.5e+00)
!
!  Use rounding to convert R to an integer between A and B.
!
      value = nint(r)

      value = max(value, min(a, b))
      value = min(value, max(a, b))

      i4_uniform = value

      return
   end
   subroutine i4col_compare(m, n, a, i, j, isgn)

!*****************************************************************************80
!
!! I4COL_COMPARE compares columns I and J of an I4COL.
!
!  Example:
!
!    Input:
!
!      M = 3, N = 4, I = 2, J = 4
!
!      A = (
!        1  2  3  4
!        5  6  7  8
!        9 10 11 12 )
!
!    Output:
!
!      ISGN = -1
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 June 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer   M, N, the number of rows and columns.
!
!    Input, integer   A(M,N), an array of N columns of vectors
!    of length M.
!
!    Input, integer   I, J, the columns to be compared.
!    I and J must be between 1 and N.
!
!    Output, integer   ISGN, the results of the comparison:
!    -1, column I < column J,
!     0, column I = column J,
!    +1, column J < column I.
!
      implicit none

      integer m
      integer n

      integer a(m, n)
      integer i
      integer isgn
      integer j
      integer k
!
!  Check.
!
      if (i < 1 .or. n < i) then
         write (*, '(a)') ' '
         write (*, '(a)') 'I4COL_COMPARE - Fatal error!'
         write (*, '(a)') '  Column index I is out of bounds.'
         stop
      end if

      if (j < 1 .or. n < j) then
         write (*, '(a)') ' '
         write (*, '(a)') 'I4COL_COMPARE - Fatal error!'
         write (*, '(a)') '  Column index J is out of bounds.'
         stop
      end if

      isgn = 0

      if (i == j) then
         return
      end if

      k = 1

      do while (k <= m)

         if (a(k, i) < a(k, j)) then
            isgn = -1
            return
         else if (a(k, j) < a(k, i)) then
            isgn = +1
            return
         end if

         k = k + 1

      end do

      return
   end
   subroutine i4col_sort_a(m, n, a)

!*****************************************************************************80
!
!! I4COL_SORT_A ascending sorts an I4COL.
!
!  Discussion:
!
!    In lexicographic order, the statement "X < Y", applied to two real
!    vectors X and Y of length M, means that there is some index I, with
!    1 <= I <= M, with the property that
!
!      X(J) = Y(J) for J < I,
!    and
!      X(I) < Y(I).
!
!    In other words, the first time they differ, X is smaller.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 September 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer   M, the number of rows of A, and the length of
!    a vector of data.
!
!    Input, integer   N, the number of columns of A.
!
!    Input/output, integer   A(M,N).
!    On input, the array of N columns of M-vectors.
!    On output, the columns of A have been sorted in ascending
!    lexicographic order.
!
      implicit none

      integer m
      integer n

      integer a(m, n)
      integer i
      integer indx
      integer isgn
      integer j

      if (m <= 0) then
         return
      end if

      if (n <= 1) then
         return
      end if
!
!  Initialize.
!
      i = 0
      indx = 0
      isgn = 0
      j = 0
!
!  Call the external heap sorter.
!
      do

         call sort_heap_external(n, indx, i, j, isgn)
!
!  Interchange the I and J objects.
!
         if (0 < indx) then

            call i4col_swap(m, n, a, i, j)
!
!  Compare the I and J objects.
!
         else if (indx < 0) then

            call i4col_compare(m, n, a, i, j, isgn)

         else if (indx == 0) then

            exit

         end if

      end do

      return
   end
   subroutine i4col_swap(m, n, a, i, j)

!*****************************************************************************80
!
!! I4COL_SWAP swaps columns I and J of an I4COL.
!
!  Example:
!
!    Input:
!
!      M = 3, N = 4, I = 2, J = 4
!
!      A = (
!        1  2  3  4
!        5  6  7  8
!        9 10 11 12 )
!
!    Output:
!
!      A = (
!        1  4  3  2
!        5  8  7  6
!        9 12 11 10 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 April 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer   M, N, the number of rows and columns
!    in the array.
!
!    Input/output, integer   A(M,N), an array of N columns
!    of length M.
!
!    Input, integer   I, J, the columns to be swapped.
!
      implicit none

      integer m
      integer n

      integer a(m, n)
      integer col(m)
      integer i
      integer j

      if (i < 1 .or. n < i .or. j < 1 .or. n < j) then

         write (*, '(a)') ' '
         write (*, '(a)') 'I4COL_SWAP - Fatal error!'
         write (*, '(a)') '  I or J is out of bounds.'
         write (*, '(a,i8)') '  I =    ', i
         write (*, '(a,i8)') '  J =    ', j
         write (*, '(a,i8)') '  N =    ', n
         stop

      end if

      if (i == j) then
         return
      end if

      col(1:m) = a(1:m, i)
      a(1:m, i) = a(1:m, j)
      a(1:m, j) = col(1:m)

      return
   end
   subroutine i4mat_print_some(m, n, a, ilo, jlo, ihi, jhi, title)

!*****************************************************************************80
!
!! I4MAT_PRINT_SOME prints some of an I4MAT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 November 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer   M, N, the number of rows and columns.
!
!    Input, integer   A(M,N), an M by N matrix to be printed.
!
!    Input, integer   ILO, JLO, the first row and column to print.
!
!    Input, integer   IHI, JHI, the last row and column to print.
!
!    Input, character ( len = * ) TITLE, an optional title.
!
      implicit none

      integer, parameter :: incx = 10
      integer m
      integer n

      integer a(m, n)
      character(len=7) ctemp(incx)
      integer i
      integer i2hi
      integer i2lo
      integer ihi
      integer ilo
      integer inc
      integer j
      integer j2
      integer j2hi
      integer j2lo
      integer jhi
      integer jlo
      character(len=*) title

      if (0 < len_trim(title)) then
         write (*, '(a)') ' '
         write (*, '(a)') trim(title)
      end if

      do j2lo = max(jlo, 1), min(jhi, n), incx

         j2hi = j2lo + incx - 1
         j2hi = min(j2hi, n)
         j2hi = min(j2hi, jhi)

         inc = j2hi + 1 - j2lo

         write (*, '(a)') ' '

         do j = j2lo, j2hi
            j2 = j + 1 - j2lo
            write (ctemp(j2), '(i7)') j
         end do

         write (*, '(''  Col '',10a7)') ctemp(1:inc)
         write (*, '(a)') '  Row'
         write (*, '(a)') ' '

         i2lo = max(ilo, 1)
         i2hi = min(ihi, m)

         do i = i2lo, i2hi

            do j2 = 1, inc

               j = j2lo - 1 + j2

               write (ctemp(j2), '(i7)') a(i, j)

            end do

            write (*, '(i5,1x,10a7)') i, (ctemp(j), j=1, inc)

         end do

      end do

      write (*, '(a)') ' '

      return
   end
   subroutine i4mat_transpose_print(m, n, a, title)

!*****************************************************************************80
!
!! I4MAT_TRANSPOSE_PRINT prints an I4MAT, transposed.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer   M, N, the number of rows and columns.
!
!    Input, integer   A(M,N), an M by N matrix to be printed.
!
!    Input, character ( len = * ) TITLE, an optional title.
!
      implicit none

      integer m
      integer n

      integer a(m, n)
      character(len=*) title

      call i4mat_transpose_print_some(m, n, a, 1, 1, m, n, title)

      return
   end
   subroutine i4mat_transpose_print_some(m, n, a, ilo, jlo, ihi, jhi, title)

!*****************************************************************************80
!
!! I4MAT_TRANSPOSE_PRINT_SOME prints some of the transpose of an I4MAT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 February 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer   M, N, the number of rows and columns.
!
!    Input, integer   A(M,N), an M by N matrix to be printed.
!
!    Input, integer   ILO, JLO, the first row and column to print.
!
!    Input, integer   IHI, JHI, the last row and column to print.
!
!    Input, character ( len = * ) TITLE, an optional title.
!
      implicit none

      integer, parameter :: incx = 10
      integer m
      integer n

      integer a(m, n)
      character(len=7) ctemp(incx)
      integer i
      integer i2
      integer i2hi
      integer i2lo
      integer ihi
      integer ilo
      integer inc
      integer j
      integer j2hi
      integer j2lo
      integer jhi
      integer jlo
      character(len=*) title

      if (0 < len_trim(title)) then
         write (*, '(a)') ' '
         write (*, '(a)') trim(title)
      end if

      do i2lo = max(ilo, 1), min(ihi, m), incx

         i2hi = i2lo + incx - 1
         i2hi = min(i2hi, m)
         i2hi = min(i2hi, ihi)

         inc = i2hi + 1 - i2lo

         write (*, '(a)') ' '

         do i = i2lo, i2hi
            i2 = i + 1 - i2lo
            write (ctemp(i2), '(i7)') i
         end do

         write (*, '(''  Row '',10a7)') ctemp(1:inc)
         write (*, '(a)') '  Col'
         write (*, '(a)') ' '

         j2lo = max(jlo, 1)
         j2hi = min(jhi, n)

         do j = j2lo, j2hi

            do i2 = 1, inc

               i = i2lo - 1 + i2

               write (ctemp(i2), '(i7)') a(i, j)

            end do

            write (*, '(i5,1x,10a7)') j, (ctemp(i), i=1, inc)

         end do

      end do

      write (*, '(a)') ' '

      return
   end
   subroutine i4vec_heap_d(n, a)

!*****************************************************************************80
!
!! I4VEC_HEAP_D reorders an I4VEC into an descending heap.
!
!  Discussion:
!
!    An I4VEC is a vector of integer values.
!
!    A descending heap is an array A with the property that, for every index J,
!    A(J) >= A(2*J) and A(J) >= A(2*J+1), (as long as the indices
!    2*J and 2*J+1 are legal).
!
!                  A(1)
!                /      \
!            A(2)         A(3)
!          /     \        /  \
!      A(4)       A(5)  A(6) A(7)
!      /  \       /   \
!    A(8) A(9) A(10) A(11)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Albert Nijenhuis, Herbert Wilf,
!    Combinatorial Algorithms,
!    Academic Press, 1978, second edition,
!    ISBN 0-12-519260-6.
!
!  Parameters:
!
!    Input, integer   N, the size of the input array.
!
!    Input/output, integer   A(N).
!    On input, an unsorted array.
!    On output, the array has been reordered into a heap.
!
      implicit none

      integer n

      integer a(n)
      integer i
      integer ifree
      integer key
      integer m
!
!  Only nodes N/2 down to 1 can be "parent" nodes.
!
      do i = n / 2, 1, -1
!
!  Copy the value out of the parent node.
!  Position IFREE is now "open".
!
         key = a(i)
         ifree = i

         do
!
!  Positions 2*IFREE and 2*IFREE + 1 are the descendants of position
!  IFREE.  (One or both may not exist because they exceed N.)
!
            m = 2 * ifree
!
!  Does the first position exist?
!
            if (n < m) then
               exit
            end if
!
!  Does the second position exist?
!
            if (m + 1 <= n) then
!
!  If both positions exist, take the larger of the two values,
!  and update M if necessary.
!
               if (a(m) < a(m + 1)) then
                  m = m + 1
               end if

            end if
!
!  If the large descendant is larger than KEY, move it up,
!  and update IFREE, the location of the free position, and
!  consider the descendants of THIS position.
!
            if (a(m) <= key) then
               exit
            end if

            a(ifree) = a(m)
            ifree = m

         end do
!
!  Once there is no more shifting to do, KEY moves into the free spot IFREE.
!
         a(ifree) = key

      end do

      return
   end
   subroutine i4vec_indicator(n, a)

!*****************************************************************************80
!
!! I4VEC_INDICATOR sets an I4VEC to the vector A(I)=I.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer   N, the number of elements of A.
!
!    Output, integer   A(N), the array to be initialized.
!
      implicit none

      integer n

      integer a(n)
      integer i

      do i = 1, n
         a(i) = i
      end do

      return
   end
   subroutine i4vec_print(n, a, title)

!*****************************************************************************80
!
!! I4VEC_PRINT prints an I4VEC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer   N, the number of components of the vector.
!
!    Input, integer   A(N), the vector to be printed.
!
!    Input, character ( len = * ) TITLE, a title to be printed first.
!    TITLE may be blank.
!
      implicit none

      integer n

      integer a(n)
      integer big
      integer i
      character(len=*) title

      if (0 < len_trim(title)) then
         write (*, '(a)') ' '
         write (*, '(a)') trim(title)
      end if

      big = maxval(abs(a(1:n)))

      write (*, '(a)') ' '
      if (big < 1000) then
         do i = 1, n
            write (*, '(2x,i8,2x,i4)') i, a(i)
         end do
      else if (big < 1000000) then
         do i = 1, n
            write (*, '(2x,i8,2x,i7)') i, a(i)
         end do
      else
         do i = 1, n
            write (*, '(2x,i8,2x,i12)') i, a(i)
         end do
      end if

      return
   end
   subroutine i4vec_reverse(n, a)

!*****************************************************************************80
!
!! I4VEC_REVERSE reverses the elements of an I4VEC.
!
!  Example:
!
!    Input:
!
!      N = 5,
!      A = ( 11, 12, 13, 14, 15 ).
!
!    Output:
!
!      A = ( 15, 14, 13, 12, 11 ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 July 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer   N, the number of entries in the array.
!
!    Input/output, integer   A(N), the array to be reversed.
!
      implicit none

      integer n

      integer a(n)
      integer i

      do i = 1, n / 2
         call i4_swap(a(i), a(n + 1 - i))
      end do

      return
   end
   subroutine i4vec_sort_heap_a(n, a)

!*****************************************************************************80
!
!! I4VEC_SORT_HEAP_A ascending sorts an I4VEC using heap sort.
!
!  Discussion:
!
!    An I4VEC is a vector of integer values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Albert Nijenhuis, Herbert Wilf,
!    Combinatorial Algorithms,
!    Academic Press, 1978, second edition,
!    ISBN 0-12-519260-6.
!
!  Parameters:
!
!    Input, integer   N, the number of entries in the array.
!
!    Input/output, integer   A(N).
!    On input, the array to be sorted;
!    On output, the array has been sorted.
!
      implicit none

      integer n

      integer a(n)
      integer n1

      if (n <= 1) then
         return
      end if
!
!  1: Put A into descending heap form.
!
      call i4vec_heap_d(n, a)
!
!  2: Sort A.
!
!  The largest object in the heap is in A(1).
!  Move it to position A(N).
!
      call i4_swap(a(1), a(n))
!
!  Consider the diminished heap of size N1.
!
      do n1 = n - 1, 2, -1
!
!  Restore the heap structure of A(1) through A(N1).
!
         call i4vec_heap_d(n1, a)
!
!  Take the largest object from A(1) and move it to A(N1).
!
         call i4_swap(a(1), a(n1))

      end do

      return
   end
   subroutine level_set(root, adj_num, adj_row, adj, mask, level_num, &
                        level_row, level, node_num)

!*****************************************************************************80
!
!! LEVEL_SET generates the connected level structure rooted at a given node.
!
!  Discussion:
!
!    Only nodes for which MASK is nonzero will be considered.
!
!    The root node chosen by the user is assigned level 1, and masked.
!    All (unmasked) nodes reachable from a node in level 1 are
!    assigned level 2 and masked.  The process continues until there
!    are no unmasked nodes adjacent to any node in the current level.
!    The number of levels may vary between 2 and NODE_NUM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 October 2003
!
!  Author:
!
!    Original FORTRAN77 version by Alan George, Joseph Liu.
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Alan George, Joseph Liu,
!    Computer Solution of Large Sparse Positive Definite Systems,
!    Prentice Hall, 1981.
!
!  Parameters:
!
!    Input, integer   ROOT, the node at which the level structure
!    is to be rooted.
!
!    Input, integer   ADJ_NUM, the number of adjacency entries.
!
!    Input, integer   ADJ_ROW(NODE_NUM+1).  Information about
!    row I is stored in entries ADJ_ROW(I) through ADJ_ROW(I+1)-1 of ADJ.
!
!    Input, integer   ADJ(ADJ_NUM), the adjacency structure.
!    For each row, it contains the column indices of the nonzero entries.
!
!    Input/output, integer   MASK(NODE_NUM).  On input, only nodes
!    with nonzero MASK are to be processed.  On output, those nodes which were
!    included in the level set have MASK set to 1.
!
!    Output, integer   LEVEL_NUM, the number of levels in the level
!    structure.  ROOT is in level 1.  The neighbors of ROOT
!    are in level 2, and so on.
!
!    Output, integer   LEVEL_ROW(NODE_NUM+1), LEVEL(NODE_NUM),
!    the rooted level structure.
!
!    Input, integer   NODE_NUM, the number of nodes.
!
      implicit none

      integer adj_num
      integer node_num

      integer adj(adj_num)
      integer adj_row(node_num + 1)
      integer i
      integer iccsze
      integer j
      integer jstop
      integer jstrt
      integer lbegin
      integer level_num
      integer level_row(node_num + 1)
      integer level(node_num)
      integer lvlend
      integer lvsize
      integer mask(node_num)
      integer nbr
      integer node
      integer root

      mask(root) = 0
      level(1) = root
      level_num = 0
      lvlend = 0
      iccsze = 1
!
!  LBEGIN is the pointer to the beginning of the current level, and
!  LVLEND points to the end of this level.
!
      do

         lbegin = lvlend + 1
         lvlend = iccsze
         level_num = level_num + 1
         level_row(level_num) = lbegin
!
!  Generate the next level by finding all the masked neighbors of nodes
!  in the current level.
!
         do i = lbegin, lvlend

            node = level(i)
            jstrt = adj_row(node)
            jstop = adj_row(node + 1) - 1

            do j = jstrt, jstop

               nbr = adj(j)

               if (mask(nbr) /= 0) then
                  iccsze = iccsze + 1
                  level(iccsze) = nbr
                  mask(nbr) = 0
               end if

            end do

         end do
!
!  Compute the current level width (the number of nodes encountered.)
!  If it is positive, generate the next level.
!
         lvsize = iccsze - lvlend

         if (lvsize <= 0) then
            exit
         end if

      end do

      level_row(level_num + 1) = lvlend + 1
!
!  Reset MASK to 1 for the nodes in the level structure.
!
      do i = 1, iccsze
         mask(level(i)) = 1
      end do
      return
   end
   subroutine level_set_print(node_num, level_num, level_row, level)

!*****************************************************************************80
!
!! LEVEL_SET_PRINT prints level set information.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 October 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer   NODE_NUM, the number of nodes.
!
!    Input, integer   LEVEL_NUM, the number of levels.
!
!    Input, integer   LEVEL_ROW(LEVEL_NUM+1), organizes the entries
!    of LEVEL.  The entries for level I are in entries LEVEL_ROW(I)
!    through LEVEL_ROW(I+1)-1.
!
!    Input, integer   LEVEL(NODE_NUM), is simply a list of the
!    nodes in an order induced by the levels.
!
      implicit none

      integer level_num
      integer node_num

      integer level(node_num)
      integer level_row(level_num + 1)
      integer i
      integer jhi
      integer jlo
      integer jmax
      integer jmin

      write (*, '(a)') ' '
      write (*, '(a)') 'LEVEL_SET_PRINT'
      write (*, '(a)') '  Show the level set structure of a rooted graph.'
      write (*, '(a,i8)') '  The number of nodes is  ', node_num
      write (*, '(a,i8)') '  The number of levels is ', level_num
      write (*, '(a)') ' '
      write (*, '(a)') '  Level Min Max      Nonzeros '
      write (*, '(a)') ' '

      do i = 1, level_num

         jmin = level_row(i)
         jmax = level_row(i + 1) - 1

         if (jmax < jmin) then

            write (*, '(2x,3i4,6x,10i8)') i, jmin, jmax

         else

            do jlo = jmin, jmax, 5

               jhi = min(jlo + 4, jmax)

               if (jlo == jmin) then
                  write (*, '(2x,3i4,3x,5i8)') i, jmin, jmax, level(jlo:jhi)
               else
                  write (*, '(2x,12x,3x,5i8)') level(jlo:jhi)
               end if

            end do

         end if

      end do

      return
   end
   subroutine perm_check(n, p, ierror)

!*****************************************************************************80
!
!! PERM_CHECK checks that a vector represents a permutation.
!
!  Discussion:
!
!    The routine verifies that each of the integers from 1
!    to N occurs among the N entries of the permutation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer   N, the number of entries.
!
!    Input, integer   P(N), the array to check.
!
!    Output, integer   IERROR, error flag.
!    0, the array represents a permutation.
!    nonzero, the array does not represent a permutation.  The smallest
!    missing value is equal to IERROR.
!
      implicit none

      integer n

      integer ierror
      integer ifind
      integer iseek
      integer p(n)

      ierror = 0

      do iseek = 1, n

         ierror = iseek

         do ifind = 1, n
            if (p(ifind) == iseek) then
               ierror = 0
               exit
            end if
         end do

         if (ierror /= 0) then
            return
         end if

      end do

      return
   end
   subroutine perm_inverse3(n, perm, perm_inv)

!*****************************************************************************80
!
!! PERM_INVERSE3 produces the inverse of a given permutation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 October 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer   N, the number of items permuted.
!
!    Input, integer   PERM(N), a permutation.
!
!    Output, integer   PERM_INV(N), the inverse permutation.
!
      implicit none

      integer n

      integer i
      integer perm(n)
      integer perm_inv(n)

      do i = 1, n
         perm_inv(perm(i)) = i
      end do

      return
   end
   subroutine perm_uniform(n, seed, p)

!*****************************************************************************80
!
!! PERM_UNIFORM selects a random permutation of N objects.
!
!  Discussion:
!
!    The routine assumes the objects are labeled 1, 2, ... N.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 May 2002
!
!  Author:
!
!    Original FORTRAN77 version by Albert Nijenhuis, Herbert Wilf.
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Albert Nijenhuis, Herbert Wilf,
!    Combinatorial Algorithms,
!    Academic Press, 1978, second edition,
!    ISBN 0-12-519260-6.
!
!  Parameters:
!
!    Input, integer   N, the number of objects to be permuted.
!
!    Input/output, integer   SEED, a seed for the random
!    number generator.
!
!    Output, integer   P(N), a permutation of ( 1, 2, ..., N ),
!    in standard index form.
!
      implicit none

      integer n

      integer i
      integer j
      integer p(n)
      integer seed

      call i4vec_indicator(n, p)

      do i = 1, n
         j = i4_uniform(i, n, seed)
         call i4_swap(p(i), p(j))
      end do

      return
   end
   subroutine r82vec_permute(n, a, p)

!*****************************************************************************80
!
!! R82VEC_PERMUTE permutes an R82VEC in place.
!
!  Discussion:
!
!    This routine permutes an array of real "objects", but the same
!    logic can be used to permute an array of objects of any arithmetic
!    type, or an array of objects of any complexity.  The only temporary
!    storage required is enough to store a single object.  The number
!    of data movements made is N + the number of cycles of order 2 or more,
!    which is never more than N + N/2.
!
!  Example:
!
!    Input:
!
!      N = 5
!      P = (   2,    4,    5,    1,    3 )
!      A = ( 1.0,  2.0,  3.0,  4.0,  5.0 )
!          (11.0, 22.0, 33.0, 44.0, 55.0 )
!
!    Output:
!
!      A    = (  2.0,  4.0,  5.0,  1.0,  3.0 )
!             ( 22.0, 44.0, 55.0, 11.0, 33.0 ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 March 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer   N, the number of objects.
!
!    Input/output, real(kind=dp) ::   A(2,N), the array to be permuted.
!
!    Input, integer   P(N), the permutation.  P(I) = J means
!    that the I-th element of the output array should be the J-th
!    element of the input array.  P must be a legal permutation
!    of the integers from 1 to N, otherwise the algorithm will
!    fail catastrophically.
!
      implicit none

      integer n
      integer, parameter :: ndim = 2

      real(kind=dp) :: a(ndim, n)
      real(kind=dp) :: a_temp(ndim)
      integer ierror
      integer iget
      integer iput
      integer istart
      integer p(n)

      call perm_check(n, p, ierror)

      if (ierror /= 0) then
         write (*, '(a)') ' '
         write (*, '(a)') 'R82VEC_PERMUTE - Fatal error!'
         write (*, '(a)') '  The input array does not represent'
         write (*, '(a)') '  a proper permutation.  In particular, the'
         write (*, '(a,i8)') '  array is missing the value ', ierror
         stop
      end if
!
!  Search for the next element of the permutation that has not been used.
!
      do istart = 1, n

         if (p(istart) < 0) then

            cycle

         else if (p(istart) == istart) then

            p(istart) = -p(istart)
            cycle

         else

            a_temp(1:ndim) = a(1:ndim, istart)
            iget = istart
!
!  Copy the new value into the vacated entry.
!
            do

               iput = iget
               iget = p(iget)

               p(iput) = -p(iput)

               if (iget < 1 .or. n < iget) then
                  write (*, '(a)') ' '
                  write (*, '(a)') 'R82VEC_PERMUTE - Fatal error!'
                  write (*, '(a)') '  A permutation index is out of range.'
                  write (*, '(a,i8,a,i8)') '  P(', iput, ') = ', iget
                  stop
               end if

               if (iget == istart) then
                  a(1:ndim, iput) = a_temp(1:ndim)
                  exit
               end if

               a(1:ndim, iput) = a(1:ndim, iget)

            end do

         end if

      end do
!
!  Restore the signs of the entries.
!
      p(1:n) = -p(1:n)

      return
   end
   subroutine r8mat_print_some(m, n, a, ilo, jlo, ihi, jhi, title)

!*****************************************************************************80
!
!! R8MAT_PRINT_SOME prints some of an R8MAT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer   M, N, the number of rows and columns.
!
!    Input, real(kind=dp) ::   A(M,N), an M by N matrix to be printed.
!
!    Input, integer   ILO, JLO, the first row and column to print.
!
!    Input, integer   IHI, JHI, the last row and column to print.
!
!    Input, character ( len = * ) TITLE, an optional title.
!
      implicit none

      integer, parameter :: incx = 5
      integer m
      integer n

      real(kind=dp) :: a(m, n)
      character(len=14) ctemp(incx)
      integer i
      integer i2hi
      integer i2lo
      integer ihi
      integer ilo
      integer inc
      integer j
      integer j2
      integer j2hi
      integer j2lo
      integer jhi
      integer jlo
      character(len=*) title

      if (0 < len_trim(title)) then
         write (*, '(a)') ' '
         write (*, '(a)') trim(title)
      end if

      do j2lo = max(jlo, 1), min(jhi, n), incx

         j2hi = j2lo + incx - 1
         j2hi = min(j2hi, n)
         j2hi = min(j2hi, jhi)

         inc = j2hi + 1 - j2lo

         write (*, '(a)') ' '

         do j = j2lo, j2hi
            j2 = j + 1 - j2lo
            write (ctemp(j2), '(i7,7x)') j
         end do

         write (*, '(''  Col   '',5a14)') ctemp(1:inc)
         write (*, '(a)') '  Row'
         write (*, '(a)') ' '

         i2lo = max(ilo, 1)
         i2hi = min(ihi, m)

         do i = i2lo, i2hi

            do j2 = 1, inc

               j = j2lo - 1 + j2

               write (ctemp(j2), '(g14.6)') a(i, j)

            end do

            write (*, '(i5,1x,5a14)') i, (ctemp(j), j=1, inc)

         end do

      end do

      write (*, '(a)') ' '

      return
   end
   subroutine r8mat_transpose_print_some(m, n, a, ilo, jlo, ihi, jhi, title)

!*****************************************************************************80
!
!! R8MAT_TRANSPOSE_PRINT_SOME prints some of an R8MAT, transposed.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 June 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer   M, N, the number of rows and columns.
!
!    Input, real(kind=dp) ::   A(M,N), an M by N matrix to be printed.
!
!    Input, integer   ILO, JLO, the first row and column to print.
!
!    Input, integer   IHI, JHI, the last row and column to print.
!
!    Input, character ( len = * ) TITLE, an optional title.
!
      implicit none

      integer, parameter :: incx = 5
      integer m
      integer n

      real(kind=dp) :: a(m, n)
      character(len=14) ctemp(incx)
      integer i
      integer i2
      integer i2hi
      integer i2lo
      integer ihi
      integer ilo
      integer inc
      integer j
      integer j2hi
      integer j2lo
      integer jhi
      integer jlo
      character(len=*) title

      if (0 < len_trim(title)) then
         write (*, '(a)') ' '
         write (*, '(a)') trim(title)
      end if

      do i2lo = max(ilo, 1), min(ihi, m), incx

         i2hi = i2lo + incx - 1
         i2hi = min(i2hi, m)
         i2hi = min(i2hi, ihi)

         inc = i2hi + 1 - i2lo

         write (*, '(a)') ' '

         do i = i2lo, i2hi
            i2 = i + 1 - i2lo
            write (ctemp(i2), '(i7,7x)') i
         end do

         write (*, '(''  Row   '',5a14)') ctemp(1:inc)
         write (*, '(a)') '  Col'
         write (*, '(a)') ' '

         j2lo = max(jlo, 1)
         j2hi = min(jhi, n)

         do j = j2lo, j2hi

            do i2 = 1, inc
               i = i2lo - 1 + i2
               write (ctemp(i2), '(g14.6)') a(i, j)
            end do

            write (*, '(i5,1x,5a14)') j, (ctemp(i), i=1, inc)

         end do

      end do

      write (*, '(a)') ' '

      return
   end
   subroutine rcm(root, adj_num, adj_row, adj, mask, perm, iccsze, node_num)

!*****************************************************************************80
!
!! RCM renumbers a connected component by the reverse Cuthill McKee algorithm.
!
!  Discussion:
!
!    The connected component is specified by a node ROOT and a mask.
!    The numbering starts at the root node.
!
!    An outline of the algorithm is as follows:
!
!    X(1) = ROOT.
!
!    for ( I = 1 to N-1)
!      Find all unlabeled neighbors of X(I),
!      assign them the next available labels, in order of increasing degree.
!
!    When done, reverse the ordering.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2007
!
!  Author:
!
!    Original FORTRAN77 version by Alan George, Joseph Liu.
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Alan George, Joseph Liu,
!    Computer Solution of Large Sparse Positive Definite Systems,
!    Prentice Hall, 1981.
!
!  Parameters:
!
!    Input, integer   ROOT, the node that defines the connected
!    component.  It is used as the starting point for the RCM ordering.
!
!    Input, integer   ADJ_NUM, the number of adjacency entries.
!
!    Input, integer   ADJ_ROW(NODE_NUM+1).  Information about
!    row I is stored in entries ADJ_ROW(I) through ADJ_ROW(I+1)-1 of ADJ.
!
!    Input, integer   ADJ(ADJ_NUM), the adjacency structure.
!    For each row, it contains the column indices of the nonzero entries.
!
!    Input/output, integer   MASK(NODE_NUM), a mask for the nodes.
!    Only those nodes with nonzero input mask values are considered by the
!    routine.  The nodes numbered by RCM will have their mask values
!    set to zero.
!
!    Output, integer   PERM(NODE_NUM), the RCM ordering.
!
!    Output, integer   ICCSZE, the size of the connected component
!    that has been numbered.
!
!    Input, integer   NODE_NUM, the number of nodes.
!
!  Local Parameters:
!
!    Workspace, integer DEG(NODE_NUM), a temporary vector used to hold
!    the degree of the nodes in the section graph specified by mask and root.
!
      implicit none

      integer adj_num
      integer node_num

      integer adj(adj_num)
      integer adj_row(node_num + 1)
      integer, allocatable :: deg(:)
      integer fnbr
      integer i
      integer iccsze
      integer j
      integer jstop
      integer jstrt
      integer k
      integer l
      integer lbegin
      integer lnbr
      integer lperm
      integer lvlend
      integer mask(node_num)
      integer nbr
      integer node
      integer perm(node_num)
      integer root

      allocate (deg(node_num))
!
!  Find the degrees of the nodes in the component specified by MASK and ROOT.
!
      call degree(root, adj_num, adj_row, adj, mask, deg, iccsze, perm, node_num)

      mask(root) = 0

      if (iccsze <= 1) then
         return
      end if

      lvlend = 0
      lnbr = 1
!
!  LBEGIN and LVLEND point to the beginning and
!  the end of the current level respectively.
!
      do while (lvlend < lnbr)

         lbegin = lvlend + 1
         lvlend = lnbr

         do i = lbegin, lvlend
!
!  For each node in the current level...
!
            node = perm(i)
            jstrt = adj_row(node)
            jstop = adj_row(node + 1) - 1
!
!  Find the unnumbered neighbors of NODE.
!
!  FNBR and LNBR point to the first and last neighbors
!  of the current node in PERM.
!
            fnbr = lnbr + 1

            do j = jstrt, jstop

               nbr = adj(j)

               if (mask(nbr) /= 0) then
                  lnbr = lnbr + 1
                  mask(nbr) = 0
                  perm(lnbr) = nbr
               end if

            end do
!
!  If no neighbors, skip to next node in this level.
!
            if (lnbr <= fnbr) then
               cycle
            end if
!
!  Sort the neighbors of NODE in increasing order by degree.
!  Linear insertion is used.
!
            k = fnbr

            do while (k < lnbr)

               l = k
               k = k + 1
               nbr = perm(k)

               do while (fnbr < l)

                  lperm = perm(l)

                  if (deg(lperm) <= deg(nbr)) then
                     exit
                  end if

                  perm(l + 1) = lperm
                  l = l - 1

               end do

               perm(l + 1) = nbr

            end do

         end do

      end do
!
!  We now have the Cuthill-McKee ordering.  Reverse it.
!
      call i4vec_reverse(iccsze, perm)

      deallocate (deg)
      return
   end
   subroutine root_find(root, adj_num, adj_row, adj, mask, level_num, &
                        level_row, level, node_num)

!*****************************************************************************80
!
!! ROOT_FIND finds a pseudo-peripheral node.
!
!  Discussion:
!
!    The diameter of a graph is the maximum distance (number of edges)
!    between any two nodes of the graph.
!
!    The eccentricity of a node is the maximum distance between that
!    node and any other node of the graph.
!
!    A peripheral node is a node whose eccentricity equals the
!    diameter of the graph.
!
!    A pseudo-peripheral node is an approximation to a peripheral node;
!    it may be a peripheral node, but all we know is that we tried our
!    best.
!
!    The routine is given a graph, and seeks pseudo-peripheral nodes,
!    using a modified version of the scheme of Gibbs, Poole and
!    Stockmeyer.  It determines such a node for the section subgraph
!    specified by MASK and ROOT.
!
!    The routine also determines the level structure associated with
!    the given pseudo-peripheral node; that is, how far each node
!    is from the pseudo-peripheral node.  The level structure is
!    returned as a list of nodes LS, and pointers to the beginning
!    of the list of nodes that are at a distance of 0, 1, 2, ...,
!    NODE_NUM-1 from the pseudo-peripheral node.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 October 2003
!
!  Author:
!
!    Original FORTRAN77 version by Alan George, Joseph Liu.
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Alan George, Joseph Liu,
!    Computer Solution of Large Sparse Positive Definite Systems,
!    Prentice Hall, 1981.
!
!    Norman Gibbs, William Poole, Paul Stockmeyer,
!    An Algorithm for Reducing the Bandwidth and Profile of a Sparse Matrix,
!    SIAM Journal on Numerical Analysis,
!    Volume 13, pages 236-250, 1976.
!
!    Norman Gibbs,
!    Algorithm 509: A Hybrid Profile Reduction Algorithm,
!    ACM Transactions on Mathematical Software,
!    Volume 2, pages 378-387, 1976.
!
!  Parameters:
!
!    Input/output, integer   ROOT.  On input, ROOT is a node in the
!    the component of the graph for which a pseudo-peripheral node is
!    sought.  On output, ROOT is the pseudo-peripheral node obtained.
!
!    Input, integer   ADJ_NUM, the number of adjacency entries.
!
!    Input, integer   ADJ_ROW(NODE_NUM+1).  Information about
!    row I is stored in entries ADJ_ROW(I) through ADJ_ROW(I+1)-1 of ADJ.
!
!    Input, integer   ADJ(ADJ_NUM), the adjacency structure.
!    For each row, it contains the column indices of the nonzero entries.
!
!    Input, integer   MASK(NODE_NUM), specifies a section subgraph.
!    Nodes for which MASK is zero are ignored by FNROOT.
!
!    Output, integer   LEVEL_NUM, is the number of levels in the
!    level structure rooted at the node ROOT.
!
!    Output, integer   LEVEL_ROW(NODE_NUM+1), LEVEL(NODE_NUM), the
!    level structure array pair containing the level structure found.
!
!    Input, integer   NODE_NUM, the number of nodes.
!
      implicit none

      integer adj_num
      integer node_num

      integer adj(adj_num)
      integer adj_row(node_num + 1)
      integer iccsze
      integer j
      integer jstrt
      integer k
      integer kstop
      integer kstrt
      integer level(node_num)
      integer level_num
      integer level_num2
      integer level_row(node_num + 1)
      integer mask(node_num)
      integer mindeg
      integer nabor
      integer ndeg
      integer node
      integer root
!
!  Determine the level structure rooted at ROOT.
!
      call level_set(root, adj_num, adj_row, adj, mask, level_num, &
                     level_row, level, node_num)
!
!  Count the number of nodes in this level structure.
!
      iccsze = level_row(level_num + 1) - 1
!
!  Extreme case:
!    A complete graph has a level set of only a single level.
!    Every node is equally good (or bad).
!
      if (level_num == 1) then
         return
      end if
!
!  Extreme case:
!    A "line graph" 0--0--0--0--0 has every node in its only level.
!    By chance, we've stumbled on the ideal root.
!
      if (level_num == iccsze) then
         return
      end if
!
!  Pick any node from the last level that has minimum degree
!  as the starting point to generate a new level set.
!
      do

         mindeg = iccsze

         jstrt = level_row(level_num)
         root = level(jstrt)

         if (jstrt < iccsze) then

            do j = jstrt, iccsze

               node = level(j)
               ndeg = 0
               kstrt = adj_row(node)
               kstop = adj_row(node + 1) - 1

               do k = kstrt, kstop
                  nabor = adj(k)
                  if (0 < mask(nabor)) then
                     ndeg = ndeg + 1
                  end if
               end do

               if (ndeg < mindeg) then
                  root = node
                  mindeg = ndeg
               end if

            end do

         end if
!
!  Generate the rooted level structure associated with this node.
!
         call level_set(root, adj_num, adj_row, adj, mask, level_num2, &
                        level_row, level, node_num)
!
!  If the number of levels did not increase, accept the new ROOT.
!
         if (level_num2 <= level_num) then
            exit
         end if

         level_num = level_num2
!
!  In the unlikely case that ROOT is one endpoint of a line graph,
!  we can exit now.
!
         if (iccsze <= level_num) then
            exit
         end if

      end do

      return
   end
   subroutine sort_heap_external(n, indx, i, j, isgn)

!*****************************************************************************80
!
!! SORT_HEAP_EXTERNAL externally sorts a list of items into ascending order.
!
!  Discussion:
!
!    The actual list of data is not passed to the routine.  Hence this
!    routine may be used to sort integers, reals, numbers, names,
!    dates, shoe sizes, and so on.  After each call, the routine asks
!    the user to compare or interchange two items, until a special
!    return value signals that the sorting is completed.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 February 2004
!
!  Author:
!
!    Original FORTRAN77 version by Albert Nijenhuis, Herbert Wilf.
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Albert Nijenhuis, Herbert Wilf,
!    Combinatorial Algorithms,
!    Academic Press, 1978, second edition,
!    ISBN 0-12-519260-6.
!
!  Parameters:
!
!    Input, integer   N, the number of items to be sorted.
!
!    Input/output, integer   INDX, the main communication signal.
!
!    The user must set INDX to 0 before the first call.
!    Thereafter, the user should not change the value of INDX until
!    the sorting is done.
!
!    On return, if INDX is
!
!      greater than 0,
!      * interchange items I and J;
!      * call again.
!
!      less than 0,
!      * compare items I and J;
!      * set ISGN = -1 if I < J, ISGN = +1 if J < I;
!      * call again.
!
!      equal to 0, the sorting is done.
!
!    Output, integer   I, J, the indices of two items.
!    On return with INDX positive, elements I and J should be interchanged.
!    On return with INDX negative, elements I and J should be compared, and
!    the result reported in ISGN on the next call.
!
!    Input, integer   ISGN, results of comparison of elements
!    I and J.  (Used only when the previous call returned INDX less than 0).
!    ISGN <= 0 means I is less than or equal to J;
!    0 <= ISGN means I is greater than or equal to J.
!
      implicit none

      integer i
      integer, save :: i_save = 0
      integer indx
      integer isgn
      integer j
      integer, save :: j_save = 0
      integer, save :: k = 0
      integer, save :: k1 = 0
      integer n
      integer, save :: n1 = 0
!
!  INDX = 0: This is the first call.
!
      if (indx == 0) then

         i_save = 0
         j_save = 0
         k = n / 2
         k1 = k
         n1 = n
!
!  INDX < 0: The user is returning the results of a comparison.
!
      else if (indx < 0) then

         if (indx == -2) then

            if (isgn < 0) then
               i_save = i_save + 1
            end if

            j_save = k1
            k1 = i_save
            indx = -1
            i = i_save
            j = j_save
            return

         end if

         if (0 < isgn) then
            indx = 2
            i = i_save
            j = j_save
            return
         end if

         if (k <= 1) then

            if (n1 == 1) then
               i_save = 0
               j_save = 0
               indx = 0
            else
               i_save = n1
               n1 = n1 - 1
               j_save = 1
               indx = 1
            end if

            i = i_save
            j = j_save
            return

         end if

         k = k - 1
         k1 = k
!
!  0 < INDX, the user was asked to make an interchange.
!
      else if (indx == 1) then

         k1 = k

      end if

      do

         i_save = 2 * k1

         if (i_save == n1) then
            j_save = k1
            k1 = i_save
            indx = -1
            i = i_save
            j = j_save
            return
         else if (i_save <= n1) then
            j_save = i_save + 1
            indx = -2
            i = i_save
            j = j_save
            return
         end if

         if (k <= 1) then
            exit
         end if

         k = k - 1
         k1 = k

      end do

      if (n1 == 1) then
         i_save = 0
         j_save = 0
         indx = 0
         i = i_save
         j = j_save
      else
         i_save = n1
         n1 = n1 - 1
         j_save = 1
         indx = 1
         i = i_save
         j = j_save
      end if

      return
   end
   subroutine timestamp()

!*****************************************************************************80
!
!! TIMESTAMP prints the current YMDHMS date as a time stamp.
!
!  Example:
!
!    31 May 2001   9:45:54.872 AM
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 August 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    None
!
      implicit none

      character(len=8) ampm
      integer d
      integer h
      integer m
      integer mm
      character(len=9), parameter, dimension(12) :: month = (/ &
                                                    'January  ', 'February ', 'March    ', 'April    ', &
                                                    'May      ', 'June     ', 'July     ', 'August   ', &
                                                    'September', 'October  ', 'November ', 'December '/)
      integer n
      integer s
      integer values(8)
      integer y

      call date_and_time(values=values)

      y = values(1)
      m = values(2)
      d = values(3)
      h = values(5)
      n = values(6)
      s = values(7)
      mm = values(8)

      if (h < 12) then
         ampm = 'AM'
      else if (h == 12) then
         if (n == 0 .and. s == 0) then
            ampm = 'Noon'
         else
            ampm = 'PM'
         end if
      else
         h = h - 12
         if (h < 12) then
            ampm = 'PM'
         else if (h == 12) then
            if (n == 0 .and. s == 0) then
               ampm = 'Midnight'
            else
               ampm = 'AM'
            end if
         end if
      end if

      write (*, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)') &
         d, trim(month(m)), y, h, ':', n, ':', s, '.', mm, trim(ampm)

      return
   end
   subroutine triangulation_neighbor_triangles(triangle_order, triangle_num, &
                                               triangle_node, triangle_neighbor)

!*****************************************************************************80
!
!! TRIANGULATION_NEIGHBOR_TRIANGLES determines triangle neighbors.
!
!  Discussion:
!
!    A triangulation of a set of nodes can be completely described by
!    the coordinates of the nodes, and the list of nodes that make up
!    each triangle.  However, in some cases, it is necessary to know
!    triangle adjacency information, that is, which triangle, if any,
!    is adjacent to a given triangle on a particular side.
!
!    This routine creates a data structure recording this information.
!
!    The primary amount of work occurs in sorting a list of 3 * TRIANGLE_NUM
!    data items.
!
!    Note that ROW is a work array allocated dynamically inside this
!    routine.  It is possible, for very large values of TRIANGLE_NUM,
!    that the necessary amount of memory will not be accessible, and the
!    routine will fail.  This is a limitation of the implementation of
!    dynamic arrays in FORTRAN90.  One way to get around this would be
!    to require the user to declare ROW in the calling routine
!    as an allocatable array, get the necessary memory explicitly with
!    an ALLOCATE statement, and then pass ROW into this routine.
!
!    Of course, the point of dynamic arrays was to make it easy to
!    hide these sorts of temporary work arrays from the poor user!
!
!    This routine was revised to store the edge data in a column
!    array rather than a row array.
!
!  Example:
!
!    The input information from TRIANGLE_NODE:
!
!    Triangle   Nodes
!    --------   ---------------
!     1         3      4      1
!     2         3      1      2
!     3         3      2      8
!     4         2      1      5
!     5         8      2     13
!     6         8     13      9
!     7         3      8      9
!     8        13      2      5
!     9         9     13      7
!    10         7     13      5
!    11         6      7      5
!    12         9      7      6
!    13        10      9      6
!    14         6      5     12
!    15        11      6     12
!    16        10      6     11
!
!    The output information in TRIANGLE_NEIGHBOR:
!
!    Triangle  Neighboring Triangles
!    --------  ---------------------
!
!     1        -1     -1      2
!     2         1      4      3
!     3         2      5      7
!     4         2     -1      8
!     5         3      8      6
!     6         5      9      7
!     7         3      6     -1
!     8         5      4     10
!     9         6     10     12
!    10         9      8     11
!    11        12     10     14
!    12         9     11     13
!    13        -1     12     16
!    14        11     -1     15
!    15        16     14     -1
!    16        13     15     -1
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 February 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer   TRIANGLE_ORDER, the order of the triangles.
!
!    Input, integer   TRIANGLE_NUM, the number of triangles.
!
!    Input, integer   TRIANGLE_NODE(TRIANGLE_ORDER,TRIANGLE_NUM),
!    the nodes that make up each triangle.
!
!    Output, integer   TRIANGLE_NEIGHBOR(3,TRIANGLE_NUM), the three
!    triangles that are direct neighbors of a given triangle.
!    TRIANGLE_NEIGHBOR(1,I) is the index of the triangle which touches side 1,
!    defined by nodes 2 and 3, and so on.  TRIANGLE_NEIGHBOR(1,I) is negative
!    if there is no neighbor on that side.  In this case, that side of the
!    triangle lies on the boundary of the triangulation.
!
      implicit none

      integer triangle_num
      integer triangle_order

      integer col(4, 3 * triangle_num)
      integer i
      integer icol
      integer j
      integer k
      integer side1
      integer side2
      integer triangle_neighbor(3, triangle_num)
      integer tri
      integer triangle_node(triangle_order, triangle_num)
      integer tri1
      integer tri2
!
!  Step 1.
!  From the list of nodes for triangle T, of the form: (I,J,K)
!  construct the three neighbor relations:
!
!    (I,J,3,T) or (J,I,3,T),
!    (J,K,1,T) or (K,J,1,T),
!    (K,I,2,T) or (I,K,2,T)
!
!  where we choose (I,J,1,T) if I < J, or else (J,I,1,T)
!
      do tri = 1, triangle_num

         i = triangle_node(1, tri)
         j = triangle_node(2, tri)
         k = triangle_node(3, tri)

         if (i < j) then
            col(1:4, 3 * (tri - 1) + 1) = (/i, j, 3, tri/)
         else
            col(1:4, 3 * (tri - 1) + 1) = (/j, i, 3, tri/)
         end if

         if (j < k) then
            col(1:4, 3 * (tri - 1) + 2) = (/j, k, 1, tri/)
         else
            col(1:4, 3 * (tri - 1) + 2) = (/k, j, 1, tri/)
         end if

         if (k < i) then
            col(1:4, 3 * (tri - 1) + 3) = (/k, i, 2, tri/)
         else
            col(1:4, 3 * (tri - 1) + 3) = (/i, k, 2, tri/)
         end if

      end do
!
!  Step 2. Perform an ascending dictionary sort on the neighbor relations.
!  We only intend to sort on rows 1 and 2; the routine we call here
!  sorts on rows 1 through 4 but that won't hurt us.
!
!  What we need is to find cases where two triangles share an edge.
!  Say they share an edge defined by the nodes I and J.  Then there are
!  two columns of COL that start out ( I, J, ?, ? ).  By sorting COL,
!  we make sure that these two columns occur consecutively.  That will
!  make it easy to notice that the triangles are neighbors.
!
      call i4col_sort_a(4, 3 * triangle_num, col)
!
!  Step 3. Neighboring triangles show up as consecutive columns with
!  identical first two entries.  Whenever you spot this happening,
!  make the appropriate entries in TRIANGLE_NEIGHBOR.
!
      triangle_neighbor(1:3, 1:triangle_num) = -1

      icol = 1

      do

         if (3 * triangle_num <= icol) then
            exit
         end if

         if (col(1, icol) /= col(1, icol + 1) .or. col(2, icol) /= col(2, icol + 1)) then
            icol = icol + 1
            cycle
         end if

         side1 = col(3, icol)
         tri1 = col(4, icol)
         side2 = col(3, icol + 1)
         tri2 = col(4, icol + 1)

         triangle_neighbor(side1, tri1) = tri2
         triangle_neighbor(side2, tri2) = tri1

         icol = icol + 2

      end do

      return
   end
   subroutine triangulation_order3_adj_count(node_num, triangle_num, &
                                             triangle_node, triangle_neighbor, adj_num, adj_col)

!*****************************************************************************80
!
!! TRIANGULATION_ORDER3_ADJ_COUNT counts adjacencies in a triangulation.
!
!  Discussion:
!
!    This routine is called to count the adjacencies, so that the
!    appropriate amount of memory can be set aside for storage when
!    the adjacency structure is created.
!
!    The triangulation is assumed to involve 3-node triangles.
!
!    Two nodes are "adjacent" if they are both nodes in some triangle.
!    Also, a node is considered to be adjacent to itself.
!
!  Diagram:
!
!       3
!    s  |\
!    i  | \
!    d  |  \
!    e  |   \  side 2
!       |    \
!    3  |     \
!       |      \
!       1-------2
!
!         side 1
!
!    The local node numbering
!
!
!   21-22-23-24-25
!    |\ |\ |\ |\ |
!    | \| \| \| \|
!   16-17-18-19-20
!    |\ |\ |\ |\ |
!    | \| \| \| \|
!   11-12-13-14-15
!    |\ |\ |\ |\ |
!    | \| \| \| \|
!    6--7--8--9-10
!    |\ |\ |\ |\ |
!    | \| \| \| \|
!    1--2--3--4--5
!
!    A sample grid.
!
!
!    Below, we have a chart that summarizes the adjacency relationships
!    in the sample grid.  On the left, we list the node, and its neighbors,
!    with an asterisk to indicate the adjacency of the node to itself
!    (in some cases, you want to count this self adjacency and in some
!    you don't).  On the right, we list the number of adjancencies to
!    lower-indexed nodes, to the node itself, to higher-indexed nodes,
!    the total number of adjacencies for this node, and the location
!    of the first and last entries required to list this set of adjacencies
!    in a single list of all the adjacencies.
!
!    N   Adjacencies                Below  Self   Above   Total First  Last
!
!   --  -- -- -- -- -- -- --           --    --      --      --   ---     0
!    1:  *  2  6                        0     1       2       3     1     3
!    2:  1  *  3  6  7                  1     1       3       5     4     8
!    3:  2  *  4  7  8                  1     1       3       5     9    13
!    4:  3  *  5  8  9                  1     1       3       5    14    18
!    5:  4  *  9 10                     1     1       2       4    19    22
!    6:  1  2  *  7 11                  2     1       2       5    23    27
!    7:  2  3  6  *  8 11 12            3     1       3       7    28    34
!    8:  3  4  7  *  9 12 13            3     1       3       7    35    41
!    9:  4  5  8  * 10 13 14            3     1       3       7    42    48
!   10:  5  9  * 14 15                  2     1       2       5    49    53
!   11:  6  7  * 12 16                  2     1       2       5    54    58
!   12:  7  8 11  * 13 16 17            3     1       3       7    59    65
!   13:  8  9 12  * 14 17 18            3     1       3       7    66    72
!   14:  9 10 13  * 15 18 19            3     1       3       7    73    79
!   15: 10 14  * 19 20                  2     1       2       5    80    84
!   16: 11 12  * 17 21                  2     1       2       5    85    89
!   17: 12 13 16  * 18 21 22            3     1       3       7    90    96
!   18: 13 14 17  * 19 22 23            3     1       3       7    97   103
!   19: 14 15 18  * 20 23 24            3     1       3       7   104   110
!   20: 15 19  * 24 25                  2     1       2       5   111   115
!   21: 16 17  * 22                     2     1       1       4   116   119
!   22: 17 18 21  * 23                  3     1       1       5   120   124
!   23: 18 19 22  * 24                  3     1       1       5   125   129
!   24: 19 20 23  * 25                  3     1       1       5   130   134
!   25: 20 24  *                        2     1       0       3   135   137
!   --  -- -- -- -- -- -- --           --    --      --      --   138   ---
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 August 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters
!
!    Input, integer   NODE_NUM, the number of nodes.
!
!    Input, integer   TRIANGLE_NUM, the number of triangles.
!
!    Input, integer   TRIANGLE_NODE(3,TRIANGLE_NUM), lists the nodes
!    that make up each triangle, in counterclockwise order.
!
!    Input, integer   TRIANGLE_NEIGHBOR(3,TRIANGLE_NUM), for each
!    side of a triangle, lists the neighboring triangle, or -1 if there is
!    no neighbor.
!
!    Output, integer   ADJ_NUM, the number of adjacencies.
!
!    Output, integer   ADJ_COL(NODE_NUM+1).  Information about
!    column J is stored in entries ADJ_COL(J) through ADJ_COL(J+1)-1 of ADJ.
!
      implicit none

      integer node_num
      integer triangle_num
      integer, parameter :: triangle_order = 3

      integer adj_num
      integer adj_col(node_num + 1)
      integer i
      integer n1
      integer n2
      integer n3
      integer triangle
      integer triangle2
      integer triangle_neighbor(3, triangle_num)
      integer triangle_node(triangle_order, triangle_num)

      adj_num = 0
!
!  Set every node to be adjacent to itself.
!
      adj_col(1:node_num) = 1
!
!  Examine each triangle.
!
      do triangle = 1, triangle_num

         n1 = triangle_node(1, triangle)
         n2 = triangle_node(2, triangle)
         n3 = triangle_node(3, triangle)
!
!  Add edge (1,2) if this is the first occurrence,
!  that is, if the edge (1,2) is on a boundary (TRIANGLE2 <= 0)
!  or if this triangle is the first of the pair in which the edge
!  occurs (TRIANGLE < TRIANGLE2).
!
         triangle2 = triangle_neighbor(1, triangle)

         if (triangle2 < 0 .or. triangle < triangle2) then
            adj_col(n1) = adj_col(n1) + 1
            adj_col(n2) = adj_col(n2) + 1
         end if
!
!  Add edge (2,3).
!
         triangle2 = triangle_neighbor(2, triangle)

         if (triangle2 < 0 .or. triangle < triangle2) then
            adj_col(n2) = adj_col(n2) + 1
            adj_col(n3) = adj_col(n3) + 1
         end if
!
!  Add edge (3,1).
!
         triangle2 = triangle_neighbor(3, triangle)

         if (triangle2 < 0 .or. triangle < triangle2) then
            adj_col(n1) = adj_col(n1) + 1
            adj_col(n3) = adj_col(n3) + 1
         end if

      end do
!
!  We used ADJ_COL to count the number of entries in each column.
!  Convert it to pointers into the ADJ array.
!
      adj_col(2:node_num + 1) = adj_col(1:node_num)

      adj_col(1) = 1
      do i = 2, node_num + 1
         adj_col(i) = adj_col(i - 1) + adj_col(i)
      end do

      adj_num = adj_col(node_num + 1) - 1

      return
   end
   subroutine triangulation_order3_adj_set(node_num, triangle_num, &
                                           triangle_node, triangle_neighbor, adj_num, adj_col, adj)

!*****************************************************************************80
!
!! TRIANGULATION_ORDER3_ADJ_SET sets adjacencies in a triangulation.
!
!  Discussion:
!
!    This routine is called to count the adjacencies, so that the
!    appropriate amount of memory can be set aside for storage when
!    the adjacency structure is created.
!
!    The triangulation is assumed to involve 3-node triangles.
!
!    Two nodes are "adjacent" if they are both nodes in some triangle.
!    Also, a node is considered to be adjacent to itself.
!
!    This routine can be used to create the compressed column storage
!    for a linear triangle finite element discretization of
!    Poisson's equation in two dimensions.
!
!  Diagram:
!
!       3
!    s  |\
!    i  | \
!    d  |  \
!    e  |   \  side 2
!       |    \
!    3  |     \
!       |      \
!       1-------2
!
!         side 1
!
!    The local node numbering
!
!
!   21-22-23-24-25
!    |\ |\ |\ |\ |
!    | \| \| \| \|
!   16-17-18-19-20
!    |\ |\ |\ |\ |
!    | \| \| \| \|
!   11-12-13-14-15
!    |\ |\ |\ |\ |
!    | \| \| \| \|
!    6--7--8--9-10
!    |\ |\ |\ |\ |
!    | \| \| \| \|
!    1--2--3--4--5
!
!    A sample grid
!
!
!    Below, we have a chart that summarizes the adjacency relationships
!    in the sample grid.  On the left, we list the node, and its neighbors,
!    with an asterisk to indicate the adjacency of the node to itself
!    (in some cases, you want to count this self adjacency and in some
!    you don't).  On the right, we list the number of adjancencies to
!    lower-indexed nodes, to the node itself, to higher-indexed nodes,
!    the total number of adjacencies for this node, and the location
!    of the first and last entries required to list this set of adjacencies
!    in a single list of all the adjacencies.
!
!    N   Adjacencies                Below  Self    Above  Total First  Last
!
!   --  -- -- -- -- -- -- --           --    --      --      --   ---     0
!    1:  *  2  6                        0     1       2       3     1     3
!    2:  1  *  3  6  7                  1     1       3       5     4     8
!    3:  2  *  4  7  8                  1     1       3       5     9    13
!    4:  3  *  5  8  9                  1     1       3       5    14    18
!    5:  4  *  9 10                     1     1       2       4    19    22
!    6:  1  2  *  7 11                  2     1       2       5    23    27
!    7:  2  3  6  *  8 11 12            3     1       3       7    28    34
!    8:  3  4  7  *  9 12 13            3     1       3       7    35    41
!    9:  4  5  8  * 10 13 14            3     1       3       7    42    48
!   10:  5  9  * 14 15                  2     1       2       5    49    53
!   11:  6  7  * 12 16                  2     1       2       5    54    58
!   12:  7  8 11  * 13 16 17            3     1       3       7    59    65
!   13:  8  9 12  * 14 17 18            3     1       3       7    66    72
!   14:  9 10 13  * 15 18 19            3     1       3       7    73    79
!   15: 10 14  * 19 20                  2     1       2       5    80    84
!   16: 11 12  * 17 21                  2     1       2       5    85    89
!   17: 12 13 16  * 18 21 22            3     1       3       7    90    96
!   18: 13 14 17  * 19 22 23            3     1       3       7    97   103
!   19: 14 15 18  * 20 23 24            3     1       3       7   104   110
!   20: 15 19  * 24 25                  2     1       2       5   111   115
!   21: 16 17  * 22                     2     1       1       4   116   119
!   22: 17 18 21  * 23                  3     1       1       5   120   124
!   23: 18 19 22  * 24                  3     1       1       5   125   129
!   24: 19 20 23  * 25                  3     1       1       5   130   134
!   25: 20 24  *                        2     1       0       3   135   137
!   --  -- -- -- -- -- -- --           --    --      --      --   138   ---
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 August 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters
!
!    Input, integer   NODE_NUM, the number of nodes.
!
!    Input, integer   TRIANGLE_NUM, the number of triangles.
!
!    Input, integer   TRIANGLE_NODE(3,TRIANGLE_NUM), lists the nodes
!    that make up each triangle in counterclockwise order.
!
!    Input, integer   TRIANGLE_NEIGHBOR(3,TRIANGLE_NUM), for each
!    side of a triangle, lists the neighboring triangle, or -1 if there is
!    no neighbor.
!
!    Input, integer   ADJ_NUM, the number of adjacencies.
!
!    Input, integer   ADJ_COL(NODE_NUM+1).  Information about
!    column J is stored in entries ADJ_COL(J) through ADJ_COL(J+1)-1 of ADJ.
!
!    Output, integer   ADJ(ADJ_NUM), the adjacency information.
!
      implicit none

      integer adj_num
      integer node_num
      integer triangle_num
      integer, parameter :: triangle_order = 3

      integer adj(adj_num)
      integer adj_copy(node_num)
      integer adj_col(node_num + 1)
      integer k1
      integer k2
      integer n1
      integer n2
      integer n3
      integer node
      integer triangle
      integer triangle2
      integer triangle_neighbor(3, triangle_num)
      integer triangle_node(triangle_order, triangle_num)

      adj(1:adj_num) = -1
      adj_copy(1:node_num) = adj_col(1:node_num)
!
!  Set every node to be adjacent to itself.
!
      do node = 1, node_num
         adj(adj_copy(node)) = node
         adj_copy(node) = adj_copy(node) + 1
      end do
!
!  Examine each triangle.
!
      do triangle = 1, triangle_num

         n1 = triangle_node(1, triangle)
         n2 = triangle_node(2, triangle)
         n3 = triangle_node(3, triangle)
!
!  Add edge (1,2) if this is the first occurrence,
!  that is, if the edge (1,2) is on a boundary (TRIANGLE2 <= 0)
!  or if this triangle is the first of the pair in which the edge
!  occurs (TRIANGLE < TRIANGLE2).
!
         triangle2 = triangle_neighbor(1, triangle)

         if (triangle2 < 0 .or. triangle < triangle2) then
            adj(adj_copy(n1)) = n2
            adj_copy(n1) = adj_copy(n1) + 1
            adj(adj_copy(n2)) = n1
            adj_copy(n2) = adj_copy(n2) + 1
         end if
!
!  Add edge (2,3).
!
         triangle2 = triangle_neighbor(2, triangle)

         if (triangle2 < 0 .or. triangle < triangle2) then
            adj(adj_copy(n2)) = n3
            adj_copy(n2) = adj_copy(n2) + 1
            adj(adj_copy(n3)) = n2
            adj_copy(n3) = adj_copy(n3) + 1
         end if
!
!  Add edge (3,1).
!
         triangle2 = triangle_neighbor(3, triangle)

         if (triangle2 < 0 .or. triangle < triangle2) then
            adj(adj_copy(n1)) = n3
            adj_copy(n1) = adj_copy(n1) + 1
            adj(adj_copy(n3)) = n1
            adj_copy(n3) = adj_copy(n3) + 1
         end if

      end do
!
!  Ascending sort the entries for each node.
!
      do node = 1, node_num
         k1 = adj_col(node)
         k2 = adj_col(node + 1) - 1
         call i4vec_sort_heap_a(k2 + 1 - k1, adj(k1:k2))
      end do

      return
   end
   subroutine triangulation_order3_example2(node_num, triangle_num, node_xy, &
                                            triangle_node, triangle_neighbor)

!*****************************************************************************80
!
!! TRIANGULATION_ORDER3_EXAMPLE2 returns an example triangulation.
!
!  Discussion:
!
!    This triangulation is actually a Delaunay triangulation.
!
!    The appropriate input values of NODE_NUM and TRIANGLE_NUM can be
!    determined by calling TRIANGULATION_ORDER3_EXAMPLE2_SIZE first.
!
!  Diagram:
!
!   21-22-23-24-25
!    |\ |\ |\ |\ |
!    | \| \| \| \|
!   16-17-18-19-20
!    |\ |\ |\ |\ |
!    | \| \| \| \|
!   11-12-13-14-15
!    |\ |\ |\ |\ |
!    | \| \| \| \|
!    6--7--8--9-10
!    |\ |\ |\ |\ |
!    | \| \| \| \|
!    1--2--3--4--5
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 January 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters
!
!    Input, integer   NODE_NUM, the number of nodes.
!
!    Input, integer   TRIANGLE_NUM, the number of triangles.
!
!    Output, real(kind=dp) ::   NODE_XY(2,NODE_NUM), the coordinates of the
!    nodes.
!
!    Output, integer   TRIANGLE_NODE(3,TRIANGLE_NUM), lists the
!    nodes that make up each triangle, in counterclockwise order.
!
!    Output, integer   TRIANGLE_NEIGHBOR(3,TRIANGLE_NUM), for each
!    side of a triangle, lists the neighboring triangle, or -1 if there is
!    no neighbor.
!
      implicit none

      integer, parameter :: dim_num = 2
      integer node_num
      integer triangle_num
      integer, parameter :: triangle_order = 3

      real(kind=dp) :: node_xy(dim_num, node_num)
      integer triangle_neighbor(3, triangle_num)
      integer triangle_node(triangle_order, triangle_num)

      node_xy = reshape((/ &
                        0.0d+00, 0.0d+00, &
                        1.0d+00, 0.0d+00, &
                        2.0d+00, 0.0d+00, &
                        3.0d+00, 0.0d+00, &
                        4.0d+00, 0.0d+00, &
                        0.0d+00, 1.0d+00, &
                        1.0d+00, 1.0d+00, &
                        2.0d+00, 1.0d+00, &
                        3.0d+00, 1.0d+00, &
                        4.0d+00, 1.0d+00, &
                        0.0d+00, 2.0d+00, &
                        1.0d+00, 2.0d+00, &
                        2.0d+00, 2.0d+00, &
                        3.0d+00, 2.0d+00, &
                        4.0d+00, 2.0d+00, &
                        0.0d+00, 3.0d+00, &
                        1.0d+00, 3.0d+00, &
                        2.0d+00, 3.0d+00, &
                        3.0d+00, 3.0d+00, &
                        4.0d+00, 3.0d+00, &
                        0.0d+00, 4.0d+00, &
                        1.0d+00, 4.0d+00, &
                        2.0d+00, 4.0d+00, &
                        3.0d+00, 4.0d+00, &
                        4.0d+00, 4.0d+00 &
                        /), (/dim_num, node_num/))

      triangle_node(1:triangle_order, 1:triangle_num) = reshape((/ &
                                                                1, 2, 6, &
                                                                7, 6, 2, &
                                                                2, 3, 7, &
                                                                8, 7, 3, &
                                                                3, 4, 8, &
                                                                9, 8, 4, &
                                                                4, 5, 9, &
                                                                10, 9, 5, &
                                                                6, 7, 11, &
                                                                12, 11, 7, &
                                                                7, 8, 12, &
                                                                13, 12, 8, &
                                                                8, 9, 13, &
                                                                14, 13, 9, &
                                                                9, 10, 14, &
                                                                15, 14, 10, &
                                                                11, 12, 16, &
                                                                17, 16, 12, &
                                                                12, 13, 17, &
                                                                18, 17, 13, &
                                                                13, 14, 18, &
                                                                19, 18, 14, &
                                                                14, 15, 19, &
                                                                20, 19, 15, &
                                                                16, 17, 21, &
                                                                22, 21, 17, &
                                                                17, 18, 22, &
                                                                23, 22, 18, &
                                                                18, 19, 23, &
                                                                24, 23, 19, &
                                                                19, 20, 24, &
                                                                25, 24, 20/), (/triangle_order, triangle_num/))

      triangle_neighbor(1:3, 1:triangle_num) = reshape((/ &
                                                       -1, 2, -1, &
                                                       9, 1, 3, &
                                                       -1, 4, 2, &
                                                       11, 3, 5, &
                                                       -1, 6, 4, &
                                                       13, 5, 7, &
                                                       -1, 8, 6, &
                                                       15, 7, -1, &
                                                       2, 10, -1, &
                                                       17, 9, 11, &
                                                       4, 12, 10, &
                                                       19, 11, 13, &
                                                       6, 14, 12, &
                                                       21, 13, 15, &
                                                       8, 16, 14, &
                                                       23, 15, -1, &
                                                       10, 18, -1, &
                                                       25, 17, 19, &
                                                       12, 20, 18, &
                                                       27, 19, 21, &
                                                       14, 22, 20, &
                                                       29, 21, 23, &
                                                       16, 24, 22, &
                                                       31, 23, -1, &
                                                       18, 26, -1, &
                                                       -1, 25, 27, &
                                                       20, 28, 26, &
                                                       -1, 27, 29, &
                                                       22, 30, 28, &
                                                       -1, 29, 31, &
                                                       24, 32, 30, &
                                                       -1, 31, -1/), (/3, triangle_num/))

      return
   end
   subroutine triangulation_order3_example2_size(node_num, triangle_num, &
                                                 hole_num)

!*****************************************************************************80
!
!! TRIANGULATION_ORDER3_EXAMPLE2_SIZE returns the size of an example.
!
!  Diagram:
!
!   21-22-23-24-25
!    |\ |\ |\ |\ |
!    | \| \| \| \|
!   16-17-18-19-20
!    |\ |\ |\ |\ |
!    | \| \| \| \|
!   11-12-13-14-15
!    |\ |\ |\ |\ |
!    | \| \| \| \|
!    6--7--8--9-10
!    |\ |\ |\ |\ |
!    | \| \| \| \|
!    1--2--3--4--5
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 January 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters
!
!    Output, integer   NODE_NUM, the number of nodes.
!
!    Output, integer   TRIANGLE_NUM, the number of triangles.
!
!    Output, integer   HOLE_NUM, the number of holes.
!
      implicit none

      integer hole_num
      integer node_num
      integer triangle_num

      node_num = 25
      triangle_num = 32
      hole_num = 0

      return
   end
   subroutine triangulation_order6_adj_count(node_num, triangle_num, &
                                             triangle_node, triangle_neighbor, adj_num, adj_col)

!*****************************************************************************80
!
!! TRIANGULATION_ORDER6_ADJ_COUNT counts adjacencies in a triangulation.
!
!  Discussion:
!
!    This routine is called to count the adjacencies, so that the
!    appropriate amount of memory can be set aside for storage when
!    the adjacency structure is created.
!
!    The triangulation is assumed to involve 6-node triangles.
!
!    Two nodes are "adjacent" if they are both nodes in some triangle.
!    Also, a node is considered to be adjacent to itself.
!
!  Diagram:
!
!       3
!    s  |\
!    i  | \
!    d  |  \
!    e  6   5  side 2
!       |    \
!    3  |     \
!       |      \
!       1---4---2
!
!         side 1
!
!    The local node numbering
!
!
!   21-22-23-24-25
!    |\    |\    |
!    | \   | \   |
!   16 17 18 19 20
!    |   \ |   \ |
!    |    \|    \|
!   11-12-13-14-15
!    |\    |\    |
!    | \   | \   |
!    6  7  8  9 10
!    |   \ |   \ |
!    |    \|    \|
!    1--2--3--4--5
!
!    A sample grid.
!
!
!    Below, we have a chart that lists the nodes adjacent to each node, with
!    an asterisk to indicate the adjacency of the node to itself
!    (in some cases, you want to count this self adjacency and in some
!    you don't).
!
!    N   Adjacencies
!
!    1:  *  2  3  6  7 11
!    2:  1  *  3  6  7 11
!    3:  1  2  *  4  5  6  7  8  9 11 12 13
!    4:  3  *  5  8  9 13
!    5:  3  4  *  8  9 10 13 14 15
!    6:  1  2  3  *  7 11
!    7:  1  2  3  6  *  8 11 12 13
!    8:  3  4  5  7  *  9 11 12 13
!    9:  3  4  5  8  * 10 13 14 15
!   10:  5  9  * 13 14 15
!   11:  1  2  3  6  7  8  * 12 13 16 17 21
!   12:  3  7  8 11  * 13 16 17 21
!   13:  3  4  5  7  8  9 10 11 12  * 14 15 16 17 18 19 21 22 23
!   14:  5  9 10 13  * 15 18 19 23
!   15:  5  9 10 13 14  * 18 19 20 23 24 25
!   16: 11 12 13  * 17 21
!   17: 11 12 13 16  * 18 21 22 23
!   18: 13 14 15 17  * 19 21 22 23
!   19: 13 14 15 18  * 20 23 24 25
!   20: 15 19  * 23 24 25
!   21: 11 12 13 16 17 18  * 22 23
!   22: 13 17 18 21  * 23
!   23: 13 14 15 17 18 19 20 21 22  * 24 25
!   24: 15 19 20 23  * 25
!   25: 15 19 20 23 24  *
!
!    Below, we list the number of adjancencies to lower-indexed nodes, to
!    the node itself, to higher-indexed nodes, the total number of
!    adjacencies for this node, and the location of the first and last
!    entries required to list this set of adjacencies in a single list
!    of all the adjacencies.
!
!    N   Below  Self   Above   Total First  Last
!
!   --      --    --      --      --   ---     0
!    1:      0     1       5       6     1     6
!    2:      1     1       4       6     7    12
!    3:      2     1       9      12    13    24
!    4:      1     1       4       6    25    30
!    5:      2     1       6       9    31    39
!    6:      3     1       2       6    40    45
!    7:      4     1       4       9    46    54
!    8:      4     1       4       9    55    63
!    9:      4     1       4       9    62    72
!   10:      2     1       3       6    73    78
!   11:      6     1       5      12    79    90
!   12:      4     1       4       9    91    99
!   13:      9     1       9      19   100   118
!   14:      4     1       4       9   119   127
!   15:      5     1       6      12   128   139
!   16:      3     1       2       6   140   145
!   17:      4     1       4       9   146   154
!   18:      4     1       4       9   155   163
!   19:      4     1       4       9   164   172
!   20:      2     1       3       6   173   178
!   21:      6     1       2       9   179   187
!   22:      4     1       1       6   188   193
!   23:      9     1       2      12   194   205
!   24:      4     1       1       6   206   211
!   25:      5     1       0       6   212   217
!   --      --    --      --      --   218   ---
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 August 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters
!
!    Input, integer   NODE_NUM, the number of nodes.
!
!    Input, integer   TRIANGLE_NUM, the number of triangles.
!
!    Input, integer   TRIANGLE_NODE(6,TRIANGLE_NUM), lists the nodes
!    that make up each triangle.  The first three nodes are the vertices,
!    in counterclockwise order.  The fourth value is the midside
!    node between nodes 1 and 2; the fifth and sixth values are
!    the other midside nodes in the logical order.
!
!    Input, integer   TRIANGLE_NEIGHBOR(3,TRIANGLE_NUM), for each
!    side of a triangle, lists the neighboring triangle, or -1 if there is
!    no neighbor.
!
!    Output, integer   ADJ_NUM, the number of adjacencies.
!
!    Output, integer   ADJ_COL(NODE_NUM+1).  Information about
!    column J is stored in entries ADJ_COL(J) through ADJ_COL(J+1)-1 of ADJ.
!
      implicit none

      integer node_num
      integer triangle_num
      integer, parameter :: triangle_order = 6

      integer adj_num
      integer adj_col(node_num + 1)
      integer i
      integer n1
      integer n2
      integer n3
      integer n4
      integer n5
      integer n6
      integer triangle
      integer triangle2
      integer triangle_neighbor(3, triangle_num)
      integer triangle_node(triangle_order, triangle_num)

      adj_num = 0
!
!  Set every node to be adjacent to itself.
!
      adj_col(1:node_num) = 1
!
!  Examine each triangle.
!
      do triangle = 1, triangle_num

         n1 = triangle_node(1, triangle)
         n2 = triangle_node(2, triangle)
         n3 = triangle_node(3, triangle)
         n4 = triangle_node(4, triangle)
         n5 = triangle_node(5, triangle)
         n6 = triangle_node(6, triangle)
!
!  For sure, we add the adjacencies:
!    43 / (34)
!    51 / (15)
!    54 / (45)
!    62 / (26)
!    64 / (46)
!    65 / (56)
!
         adj_col(n3) = adj_col(n3) + 1
         adj_col(n4) = adj_col(n4) + 1
         adj_col(n1) = adj_col(n1) + 1
         adj_col(n5) = adj_col(n5) + 1
         adj_col(n4) = adj_col(n4) + 1
         adj_col(n5) = adj_col(n5) + 1
         adj_col(n2) = adj_col(n2) + 1
         adj_col(n6) = adj_col(n6) + 1
         adj_col(n4) = adj_col(n4) + 1
         adj_col(n6) = adj_col(n6) + 1
         adj_col(n5) = adj_col(n5) + 1
         adj_col(n6) = adj_col(n6) + 1
!
!  Add edges (1,2), (1,4), (2,4) if this is the first occurrence,
!  that is, if the edge (1,4,2) is on a boundary (TRIANGLE2 <= 0)
!  or if this triangle is the first of the pair in which the edge
!  occurs (TRIANGLE < TRIANGLE2).
!
!  Maybe add
!    21 / 12
!    41 / 14
!    42 / 24
!
         triangle2 = triangle_neighbor(1, triangle)

         if (triangle2 < 0 .or. triangle < triangle2) then
            adj_col(n1) = adj_col(n1) + 1
            adj_col(n2) = adj_col(n2) + 1
            adj_col(n1) = adj_col(n1) + 1
            adj_col(n4) = adj_col(n4) + 1
            adj_col(n2) = adj_col(n2) + 1
            adj_col(n4) = adj_col(n4) + 1
         end if
!
!  Maybe add
!    32 / 23
!    52 / 25
!    53 / 35
!
         triangle2 = triangle_neighbor(2, triangle)

         if (triangle2 < 0 .or. triangle < triangle2) then
            adj_col(n2) = adj_col(n2) + 1
            adj_col(n3) = adj_col(n3) + 1
            adj_col(n2) = adj_col(n2) + 1
            adj_col(n5) = adj_col(n5) + 1
            adj_col(n3) = adj_col(n3) + 1
            adj_col(n5) = adj_col(n5) + 1
         end if
!
!  Maybe add
!    31 / 13
!    61 / 16
!    63 / 36
!
         triangle2 = triangle_neighbor(3, triangle)

         if (triangle2 < 0 .or. triangle < triangle2) then
            adj_col(n1) = adj_col(n1) + 1
            adj_col(n3) = adj_col(n3) + 1
            adj_col(n1) = adj_col(n1) + 1
            adj_col(n6) = adj_col(n6) + 1
            adj_col(n3) = adj_col(n3) + 1
            adj_col(n6) = adj_col(n6) + 1
         end if

      end do
!
!  We used ADJ_COL to count the number of entries in each column.
!  Convert it to pointers into the ADJ array.
!
      adj_col(2:node_num + 1) = adj_col(1:node_num)

      adj_col(1) = 1
      do i = 2, node_num + 1
         adj_col(i) = adj_col(i - 1) + adj_col(i)
      end do

      adj_num = adj_col(node_num + 1) - 1

      return
   end
   subroutine triangulation_order6_adj_set(node_num, triangle_num, &
                                           triangle_node, triangle_neighbor, adj_num, adj_col, adj)

!*****************************************************************************80
!
!! TRIANGULATION_ORDER6_ADJ_SET sets adjacencies in a triangulation.
!
!  Discussion:
!
!    This routine is called to count the adjacencies, so that the
!    appropriate amount of memory can be set aside for storage when
!    the adjacency structure is created.
!
!    The triangulation is assumed to involve 6-node triangles.
!
!    Two nodes are "adjacent" if they are both nodes in some triangle.
!    Also, a node is considered to be adjacent to itself.
!
!    This routine can be used to create the compressed column storage
!    for a quadratic triangle finite element discretization of
!    Poisson's equation in two dimensions.
!
!  Diagram:
!
!       3
!    s  |\
!    i  | \
!    d  |  \
!    e  6   5  side 2
!       |    \
!    3  |     \
!       |      \
!       1---4---2
!
!         side 1
!
!    The local node numbering
!
!
!   21-22-23-24-25
!    |\    |\    |
!    | \   | \   |
!   16 17 18 19 20
!    |   \ |   \ |
!    |    \|    \|
!   11-12-13-14-15
!    |\    |\    |
!    | \   | \   |
!    6  7  8  9 10
!    |   \ |   \ |
!    |    \|    \|
!    1--2--3--4--5
!
!    A sample grid.
!
!
!    Below, we have a chart that lists the nodes adjacent to each node, with
!    an asterisk to indicate the adjacency of the node to itself
!    (in some cases, you want to count this self adjacency and in some
!    you don't).
!
!    N   Adjacencies
!
!    1:  *  2  3  6  7 11
!    2:  1  *  3  6  7 11
!    3:  1  2  *  4  5  6  7  8  9 11 12 13
!    4:  3  *  5  8  9 13
!    5:  3  4  *  8  9 10 13 14 15
!    6:  1  2  3  *  7 11
!    7:  1  2  3  6  *  8 11 12 13
!    8:  3  4  5  7  *  9 11 12 13
!    9:  3  4  5  8  * 10 13 14 15
!   10:  5  9  * 13 14 15
!   11:  1  2  3  6  7  8  * 12 13 16 17 21
!   12:  3  7  8 11  * 13 16 17 21
!   13:  3  4  5  7  8  9 10 11 12  * 14 15 16 17 18 19 21 22 23
!   14:  5  9 10 13  * 15 18 19 23
!   15:  5  9 10 13 14  * 18 19 20 23 24 25
!   16: 11 12 13  * 17 21
!   17: 11 12 13 16  * 18 21 22 23
!   18: 13 14 15 17  * 19 21 22 23
!   19: 13 14 15 18  * 20 23 24 25
!   20: 15 19  * 23 24 25
!   21: 11 12 13 16 17 18  * 22 23
!   22: 13 17 18 21  * 23
!   23: 13 14 15 17 18 19 20 21 22  * 24 25
!   24: 15 19 20 23  * 25
!   25: 15 19 20 23 24  *
!
!    Below, we list the number of adjancencies to lower-indexed nodes, to
!    the node itself, to higher-indexed nodes, the total number of
!    adjacencies for this node, and the location of the first and last
!    entries required to list this set of adjacencies in a single list
!    of all the adjacencies.
!
!    N   Below  Self   Above   Total First  Last
!
!   --      --    --      --      --   ---     0
!    1:      0     1       5       6     1     6
!    2:      1     1       4       6     7    12
!    3:      2     1       9      12    13    24
!    4:      1     1       4       6    25    30
!    5:      2     1       6       9    31    39
!    6:      3     1       2       6    40    45
!    7:      4     1       4       9    46    54
!    8:      4     1       4       9    55    63
!    9:      4     1       4       9    62    72
!   10:      2     1       3       6    73    78
!   11:      6     1       5      12    79    90
!   12:      4     1       4       9    91    99
!   13:      9     1       9      19   100   118
!   14:      4     1       4       9   119   127
!   15:      5     1       6      12   128   139
!   16:      3     1       2       6   140   145
!   17:      4     1       4       9   146   154
!   18:      4     1       4       9   155   163
!   19:      4     1       4       9   164   172
!   20:      2     1       3       6   173   178
!   21:      6     1       2       9   179   187
!   22:      4     1       1       6   188   193
!   23:      9     1       2      12   194   205
!   24:      4     1       1       6   206   211
!   25:      5     1       0       6   212   217
!   --      --    --      --      --   218   ---
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 August 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters
!
!    Input, integer   NODE_NUM, the number of nodes.
!
!    Input, integer   TRIANGLE_NUM, the number of triangles.
!
!    Input, integer   TRIANGLE_NODE(6,TRIANGLE_NUM), lists the nodes
!    that make up each triangle.  The first three nodes are the vertices,
!    in counterclockwise order.  The fourth value is the midside
!    node between nodes 1 and 2; the fifth and sixth values are
!    the other midside nodes in the logical order.
!
!    Input, integer   TRIANGLE_NEIGHBOR(3,TRIANGLE_NUM), for each
!    side of a triangle, lists the neighboring triangle, or -1 if there is
!    no neighbor.
!
!    Input, integer   ADJ_NUM, the number of adjacencies.
!
!    Input, integer   ADJ_COL(NODE_NUM+1).  Information about
!    column J is stored in entries ADJ_COL(J) through ADJ_COL(J+1)-1 of ADJ.
!
!    Output, integer   ADJ(ADJ_NUM), the adjacency information.
!
      implicit none

      integer adj_num
      integer node_num
      integer triangle_num
      integer, parameter :: triangle_order = 6

      integer adj(adj_num)
      integer adj_copy(node_num)
      integer adj_col(node_num + 1)
      integer k1
      integer k2
      integer n1
      integer n2
      integer n3
      integer n4
      integer n5
      integer n6
      integer node
      integer triangle
      integer triangle2
      integer triangle_neighbor(3, triangle_num)
      integer triangle_node(triangle_order, triangle_num)

      adj(1:adj_num) = -1
      adj_copy(1:node_num) = adj_col(1:node_num)
!
!  Set every node to be adjacent to itself.
!
      do node = 1, node_num
         adj(adj_copy(node)) = node
         adj_copy(node) = adj_copy(node) + 1
      end do
!
!  Examine each triangle.
!
      do triangle = 1, triangle_num

         n1 = triangle_node(1, triangle)
         n2 = triangle_node(2, triangle)
         n3 = triangle_node(3, triangle)
         n4 = triangle_node(4, triangle)
         n5 = triangle_node(5, triangle)
         n6 = triangle_node(6, triangle)
!
!  For sure, we add the adjacencies:
!    43 / (34)
!    51 / (15)
!    54 / (45)
!    62 / (26)
!    64 / (46)
!    65 / (56)
!
         adj(adj_copy(n3)) = n4
         adj_copy(n3) = adj_copy(n3) + 1
         adj(adj_copy(n4)) = n3
         adj_copy(n4) = adj_copy(n4) + 1

         adj(adj_copy(n1)) = n5
         adj_copy(n1) = adj_copy(n1) + 1
         adj(adj_copy(n5)) = n1
         adj_copy(n5) = adj_copy(n5) + 1

         adj(adj_copy(n4)) = n5
         adj_copy(n4) = adj_copy(n4) + 1
         adj(adj_copy(n5)) = n4
         adj_copy(n5) = adj_copy(n5) + 1

         adj(adj_copy(n2)) = n6
         adj_copy(n2) = adj_copy(n2) + 1
         adj(adj_copy(n6)) = n2
         adj_copy(n6) = adj_copy(n6) + 1

         adj(adj_copy(n4)) = n6
         adj_copy(n4) = adj_copy(n4) + 1
         adj(adj_copy(n6)) = n4
         adj_copy(n6) = adj_copy(n6) + 1

         adj(adj_copy(n5)) = n6
         adj_copy(n5) = adj_copy(n5) + 1
         adj(adj_copy(n6)) = n5
         adj_copy(n6) = adj_copy(n6) + 1
!
!  Add edges (1,2), (1,4), (2,4) if this is the first occurrence,
!  that is, if the edge (1,4,2) is on a boundary (TRIANGLE2 <= 0)
!  or if this triangle is the first of the pair in which the edge
!  occurs (TRIANGLE < TRIANGLE2).
!
!  Maybe add
!    21 / 12
!    41 / 14
!    42 / 24
!
         triangle2 = triangle_neighbor(1, triangle)

         if (triangle2 < 0 .or. triangle < triangle2) then
            adj(adj_copy(n1)) = n2
            adj_copy(n1) = adj_copy(n1) + 1
            adj(adj_copy(n2)) = n1
            adj_copy(n2) = adj_copy(n2) + 1
            adj(adj_copy(n1)) = n4
            adj_copy(n1) = adj_copy(n1) + 1
            adj(adj_copy(n4)) = n1
            adj_copy(n4) = adj_copy(n4) + 1
            adj(adj_copy(n2)) = n4
            adj_copy(n2) = adj_copy(n2) + 1
            adj(adj_copy(n4)) = n2
            adj_copy(n4) = adj_copy(n4) + 1
         end if
!
!  Maybe add
!    32 / 23
!    52 / 25
!    53 / 35
!
         triangle2 = triangle_neighbor(2, triangle)

         if (triangle2 < 0 .or. triangle < triangle2) then
            adj(adj_copy(n2)) = n3
            adj_copy(n2) = adj_copy(n2) + 1
            adj(adj_copy(n3)) = n2
            adj_copy(n3) = adj_copy(n3) + 1
            adj(adj_copy(n2)) = n5
            adj_copy(n2) = adj_copy(n2) + 1
            adj(adj_copy(n5)) = n2
            adj_copy(n5) = adj_copy(n5) + 1
            adj(adj_copy(n3)) = n5
            adj_copy(n3) = adj_copy(n3) + 1
            adj(adj_copy(n5)) = n3
            adj_copy(n5) = adj_copy(n5) + 1
         end if
!
!  Maybe add
!    31 / 13
!    61 / 16
!    63 / 36
!
         triangle2 = triangle_neighbor(3, triangle)

         if (triangle2 < 0 .or. triangle < triangle2) then
            adj(adj_copy(n1)) = n3
            adj_copy(n1) = adj_copy(n1) + 1
            adj(adj_copy(n3)) = n1
            adj_copy(n3) = adj_copy(n3) + 1
            adj(adj_copy(n1)) = n6
            adj_copy(n1) = adj_copy(n1) + 1
            adj(adj_copy(n6)) = n1
            adj_copy(n6) = adj_copy(n6) + 1
            adj(adj_copy(n3)) = n6
            adj_copy(n3) = adj_copy(n3) + 1
            adj(adj_copy(n6)) = n3
            adj_copy(n6) = adj_copy(n6) + 1
         end if

      end do
!
!  Ascending sort the entries for each node.
!
      do node = 1, node_num
         k1 = adj_col(node)
         k2 = adj_col(node + 1) - 1
         call i4vec_sort_heap_a(k2 + 1 - k1, adj(k1:k2))
      end do

      return
   end
   subroutine triangulation_order6_example2(node_num, triangle_num, node_xy, &
                                            triangle_node, triangle_neighbor)

!*****************************************************************************80
!
!! TRIANGULATION_ORDER6_EXAMPLE2 returns an example triangulation.
!
!  Discussion:
!
!    This triangulation is actually a Delaunay triangulation.
!
!    The appropriate input values of NODE_NUM and TRIANGLE_NUM can be
!    determined by calling TRIANGULATION_ORDER6_EXAMPLE2_SIZE first.
!
!  Diagram:
!
!   21-22-23-24-25
!    |\  6 |\  8 |
!    | \   | \   |
!   16 17 18 19 20
!    |   \ |   \ |
!    | 5  \| 7  \|
!   11-12-13-14-15
!    |\  2 |\  4 |
!    | \   | \   |
!    6  7  8  9 10
!    | 1 \ | 3 \ |
!    |    \|    \|
!    1--2--3--4--5
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 January 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters
!
!    Input, integer   NODE_NUM, the number of nodes.
!
!    Input, integer   TRIANGLE_NUM, the number of triangles.
!
!    Output, real(kind=dp) ::   NODE_XY(2,NODE_NUM), the coordinates of
!    the nodes.
!
!    Output, integer   TRIANGLE_NODE(6,TRIANGLE_NUM), lists the
!    nodes that make up each triangle.  The first three nodes are the vertices,
!    in counterclockwise order.  The fourth value is the midside
!    node between nodes 1 and 2; the fifth and sixth values are
!    the other midside nodes in the logical order.
!
!    Output, integer   TRIANGLE_NEIGHBOR(3,TRIANGLE_NUM), for each
!    side of a triangle, lists the neighboring triangle, or -1 if there is
!    no neighbor.
!
      implicit none

      integer, parameter :: dim_num = 2
      integer node_num
      integer triangle_num
      integer, parameter :: triangle_order = 6

      real(kind=dp) :: node_xy(dim_num, node_num)
      integer triangle_neighbor(3, triangle_num)
      integer triangle_node(triangle_order, triangle_num)

      node_xy = reshape((/ &
                        0.0d+00, 0.0d+00, &
                        1.0d+00, 0.0d+00, &
                        2.0d+00, 0.0d+00, &
                        3.0d+00, 0.0d+00, &
                        4.0d+00, 0.0d+00, &
                        0.0d+00, 1.0d+00, &
                        1.0d+00, 1.0d+00, &
                        2.0d+00, 1.0d+00, &
                        3.0d+00, 1.0d+00, &
                        4.0d+00, 1.0d+00, &
                        0.0d+00, 2.0d+00, &
                        1.0d+00, 2.0d+00, &
                        2.0d+00, 2.0d+00, &
                        3.0d+00, 2.0d+00, &
                        4.0d+00, 2.0d+00, &
                        0.0d+00, 3.0d+00, &
                        1.0d+00, 3.0d+00, &
                        2.0d+00, 3.0d+00, &
                        3.0d+00, 3.0d+00, &
                        4.0d+00, 3.0d+00, &
                        0.0d+00, 4.0d+00, &
                        1.0d+00, 4.0d+00, &
                        2.0d+00, 4.0d+00, &
                        3.0d+00, 4.0d+00, &
                        4.0d+00, 4.0d+00 &
                        /), (/dim_num, node_num/))

      triangle_node(1:triangle_order, 1:triangle_num) = reshape((/ &
                                                                1, 3, 11, 2, 7, 6, &
                                                                13, 11, 3, 12, 7, 8, &
                                                                3, 5, 13, 4, 9, 8, &
                                                                15, 13, 5, 14, 9, 10, &
                                                                11, 13, 21, 12, 17, 16, &
                                                                23, 21, 13, 22, 17, 18, &
                                                                13, 15, 23, 14, 19, 18, &
                                                                25, 23, 15, 24, 19, 20/), (/triangle_order, triangle_num/))

      triangle_neighbor(1:3, 1:triangle_num) = reshape((/ &
                                                       -1, 2, -1, &
                                                       5, 1, 3, &
                                                       -1, 4, 2, &
                                                       7, 3, -1, &
                                                       2, 6, -1, &
                                                       -1, 5, 7, &
                                                       4, 8, 6, &
                                                       -1, 7, -1/), (/3, triangle_num/))

      return
   end
   subroutine triangulation_order6_example2_size(node_num, triangle_num, &
                                                 hole_num)

!*****************************************************************************80
!
!! TRIANGULATION_ORDER6_EXAMPLE2_SIZE returns the size of an example.
!
!  Diagram:
!
!   21-22-23-24-25
!    |\  6 |\  8 |
!    | \   | \   |
!   16 17 18 19 20
!    |   \ |   \ |
!    | 5  \| 7  \|
!   11-12-13-14-15
!    |\  2 |\  4 |
!    | \   | \   |
!    6  7  8  9 10
!    | 1 \ | 3 \ |
!    |    \|    \|
!    1--2--3--4--5
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 January 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters
!
!    Output, integer   NODE_NUM, the number of nodes.
!
!    Output, integer   TRIANGLE_NUM, the number of triangles.
!
!    Output, integer   HOLE_NUM, the number of holes.
!
      implicit none

      integer hole_num
      integer node_num
      integer triangle_num

      node_num = 25
      triangle_num = 8
      hole_num = 0

      return
   end
end module m_rcm

module Solve_Real_Poly
   use precision, only: dp
! CACM Algorithm 493 by Jenkins & Traub

! Compliments of netlib   Sat Jul 26 11:57:43 EDT 1986

! Code converted using TO_F90 by Alan Miller
! Date: 2003-06-02  Time: 10:42:22

   implicit none
   integer, parameter :: dp_14_60 = selected_real_kind(14, 60)

! COMMON /global/ p, qp, k, qk, svk, sr, si, u, v, a, b, c, d, a1,  &
!    a2, a3, a6, a7, e, f, g, h, szr, szi, lzr, lzi, eta, are, mre, n, nn

   real(kind=dp), allocatable, save :: p(:), qp(:), k(:), qk(:), svk(:)
   real(kind=dp), save :: sr, si, u, v, a, b, c, d, a1, a2, a3, a6, &
                          a7, e, f, g, h, szr, szi, lzr, lzi
   real, save :: eta, are, mre
   integer, save :: n, nn

   private
   public :: dp_14_60, rpoly

contains

   subroutine rpoly(op, degree, zeror, zeroi, fail)

! Finds the zeros of a real polynomial
! op  - real(kind=dp) vector of coefficients in order of
!       decreasing powers.
! degree   - integer degree of polynomial.
! zeror, zeroi - output real(kind=dp) vectors of real and imaginary parts
!                of the zeros.
! fail  - output logical parameter, true only if leading coefficient is zero
!         or if rpoly has found fewer than degree zeros.
!         In the latter case degree is reset to the number of zeros found.

! To change the size of polynomials which can be solved, reset the dimensions
! of the arrays in the common area and in the following declarations.
! The subroutine uses single precision calculations for scaling, bounds and
! error calculations.  All calculations for the iterations are done in
! real(kind=dp).

      real(kind=dp), intent(IN) :: op(:)
      integer, intent(IN OUT) :: degree
      real(kind=dp), intent(OUT) :: zeror(:), zeroi(:)
      logical, intent(OUT) :: fail

      real(kind=dp), allocatable :: temp(:)
      real, allocatable :: pt(:)

      real(kind=dp) :: t, aa, bb, cc, factor
      real :: lo, MAX, MIN, xx, yy, cosr, sinr, xxx, x, sc, bnd, &
              xm, ff, df, dx, infin, smalno, base
      integer :: cnt, nz, i, j, jj, l, nm1
      logical :: zerok

! The following statements set machine constants used in various parts of the
! program.  The meaning of the four constants are...
! eta     the maximum relative representation error which can be described
!         as the smallest positive floating point number such that
!         1.d0+eta is greater than 1.
! infiny  the largest floating-point number.
! smalno  the smallest positive floating-point number if the exponent range
!         differs in single and real(kind=dp) then smalno and infin should
!         indicate the smaller range.
! base    the base of the floating-point number system used.

      base = radix(0.0)
      eta = epsilon(1.0)
      infin = huge(0.0)
      smalno = tiny(0.0)

! are and mre refer to the unit error in + and * respectively.
! They are assumed to be the same as eta.
      are = eta
      mre = eta
      lo = smalno / eta

! Initialization of constants for shift rotation
      xx = sqrt(0.5)
      yy = -xx
      cosr = -.069756474
      sinr = .99756405
      fail = .false.
      n = degree
      nn = n + 1

! Algorithm fails if the leading coefficient is zero.
      if (op(1) == 0.d0) then
         fail = .true.
         degree = 0
         return
      end if

! Remove the zeros at the origin if any
10    if (op(nn) == 0.0d0) then
         j = degree - n + 1
         zeror(j) = 0.d0
         zeroi(j) = 0.d0
         nn = nn - 1
         n = n - 1
         GO TO 10
      end if

! Allocate various arrays

      if (allocated(p)) deallocate (p, qp, k, qk, svk)
      allocate (p(nn), qp(nn), k(nn), qk(nn), svk(nn), temp(nn), pt(nn))

! Make a copy of the coefficients
      p(1:nn) = op(1:nn)

! Start the algorithm for one zero
30    if (n <= 2) then
         if (n < 1) return

! calculate the final zero or pair of zeros
         if (n /= 2) then
            zeror(degree) = -p(2) / p(1)
            zeroi(degree) = 0.0d0
            return
         end if
         call quad(p(1), p(2), p(3), zeror(degree - 1), zeroi(degree - 1), &
                   zeror(degree), zeroi(degree))
         return
      end if

! Find largest and smallest moduli of coefficients.
      MAX = 0.
      MIN = infin
      do i = 1, nn
         x = abs(real(p(i)))
         if (x > MAX) MAX = x
         if (x /= 0. .and. x < MIN) MIN = x
      end do

! Scale if there are large or very small coefficients computes a scale
! factor to multiply the coefficients of the polynomial.
! The scaling is done to avoid overflow and to avoid undetected underflow
! interfering with the convergence criterion.
! The factor is a power of the base
      sc = lo / MIN
      if (sc <= 1.0) then
         if (MAX < 10.) GO TO 60
         if (sc == 0.) sc = smalno
      else
         if (infin / sc < MAX) GO TO 60
      end if
      l = log(sc) / log(base) + .5
      factor = (base * 1.0d0)**l
      if (factor /= 1.d0) then
         p(1:nn) = factor * p(1:nn)
      end if

! compute lower bound on moduli of zeros.
60    pt(1:nn) = abs(p(1:nn))
      pt(nn) = -pt(nn)

! compute upper estimate of bound
      x = exp((log(-pt(nn)) - log(pt(1))) / n)
      if (pt(n) /= 0.) then
! if newton step at the origin is better, use it.
         xm = -pt(nn) / pt(n)
         if (xm < x) x = xm
      end if

! chop the interval (0,x) until ff .le. 0
80    xm = x*.1
      ff = pt(1)
      do i = 2, nn
         ff = ff * xm + pt(i)
      end do
      if (ff > 0.) then
         x = xm
         GO TO 80
      end if
      dx = x

! do newton iteration until x converges to two decimal places
100   if (abs(dx / x) > .005) then
         ff = pt(1)
         df = ff
         do i = 2, n
            ff = ff * x + pt(i)
            df = df * x + ff
         end do
         ff = ff * x + pt(nn)
         dx = ff / df
         x = x - dx
         GO TO 100
      end if
      bnd = x

! compute the derivative as the intial k polynomial
! and do 5 steps with no shift
      nm1 = n - 1
      do i = 2, n
         k(i) = (nn - i) * p(i) / n
      end do
      k(1) = p(1)
      aa = p(nn)
      bb = p(n)
      zerok = k(n) == 0.d0
      do jj = 1, 5
         cc = k(n)
         if (.not. zerok) then
! use scaled form of recurrence if value of k at 0 is nonzero
            t = -aa / cc
            do i = 1, nm1
               j = nn - i
               k(j) = t * k(j - 1) + p(j)
            end do
            k(1) = p(1)
            zerok = abs(k(n)) <= abs(bb) * eta * 10.
         else
! use unscaled form of recurrence
            do i = 1, nm1
               j = nn - i
               k(j) = k(j - 1)
            end do
            k(1) = 0.d0
            zerok = k(n) == 0.d0
         end if
      end do

! save k for restarts with new shifts
      temp(1:n) = k(1:n)

! loop to select the quadratic  corresponding to each
! new shift
      do cnt = 1, 20
! Quadratic corresponds to a double shift to a non-real point and its complex
! conjugate.  The point has modulus bnd and amplitude rotated by 94 degrees
! from the previous shift
         xxx = cosr * xx - sinr * yy
         yy = sinr * xx + cosr * yy
         xx = xxx
         sr = bnd * xx
         si = bnd * yy
         u = -2.0d0 * sr
         v = bnd

! second stage calculation, fixed quadratic
         call fxshfr(20 * cnt, nz)
         if (nz /= 0) then

! The second stage jumps directly to one of the third stage iterations and
! returns here if successful.
! Deflate the polynomial, store the zero or zeros and return to the main
! algorithm.
            j = degree - n + 1
            zeror(j) = szr
            zeroi(j) = szi
            nn = nn - nz
            n = nn - 1
            p(1:nn) = qp(1:nn)
            if (nz == 1) GO TO 30
            zeror(j + 1) = lzr
            zeroi(j + 1) = lzi
            GO TO 30
         end if

! If the iteration is unsuccessful another quadratic
! is chosen after restoring k
         k(1:nn) = temp(1:nn)
      end do

! Return with failure if no convergence with 20 shifts
      fail = .true.
      degree = degree - n
      return
   end subroutine rpoly

   subroutine fxshfr(l2, nz)

! Computes up to  l2  fixed shift k-polynomials, testing for convergence in
! the linear or quadratic case.  Initiates one of the variable shift
! iterations and returns with the number of zeros found.
! l2 - limit of fixed shift steps
! nz - number of zeros found

      integer, intent(IN) :: l2
      integer, intent(OUT) :: nz

      real(kind=dp) :: svu, svv, ui, vi, s
      real :: betas, betav, oss, ovv, ss, vv, ts, tv, ots, otv, tvv, tss
      integer :: type, j, iflag
      logical :: vpass, spass, vtry, stry

      nz = 0
      betav = .25
      betas = .25
      oss = sr
      ovv = v

! Evaluate polynomial by synthetic division
      call quadsd(nn, u, v, p, qp, a, b)
      call calcsc(type)
      do j = 1, l2
! calculate next k polynomial and estimate v
         call nextk(type)
         call calcsc(type)
         call newest(type, ui, vi)
         vv = vi

! Estimate s
         ss = 0.
         if (k(n) /= 0.d0) ss = -p(nn) / k(n)
         tv = 1.
         ts = 1.
         if (j /= 1 .and. type /= 3) then
! Compute relative measures of convergence of s and v sequences
            if (vv /= 0.) tv = abs((vv - ovv) / vv)
            if (ss /= 0.) ts = abs((ss - oss) / ss)

! If decreasing, multiply two most recent convergence measures
            tvv = 1.
            if (tv < otv) tvv = tv * otv
            tss = 1.
            if (ts < ots) tss = ts * ots

! Compare with convergence criteria
            vpass = tvv < betav
            spass = tss < betas
            if (spass .or. vpass) then

! At least one sequence has passed the convergence test.
! Store variables before iterating
               svu = u
               svv = v
               svk(1:n) = k(1:n)
               s = ss

! Choose iteration according to the fastest converging sequence
               vtry = .false.
               stry = .false.
               if (spass .and. ((.not. vpass) .or. tss < tvv)) GO TO 40
20             call quadit(ui, vi, nz)
               if (nz > 0) return

! Quadratic iteration has failed. flag that it has
! been tried and decrease the convergence criterion.
               vtry = .true.
               betav = betav*.25

! Try linear iteration if it has not been tried and
! the s sequence is converging
               if (stry .or. (.not. spass)) GO TO 50
               k(1:n) = svk(1:n)
40             call realit(s, nz, iflag)
               if (nz > 0) return

! Linear iteration has failed.  Flag that it has been
! tried and decrease the convergence criterion
               stry = .true.
               betas = betas*.25
               if (iflag /= 0) then

! If linear iteration signals an almost double real
! zero attempt quadratic interation
                  ui = -(s + s)
                  vi = s * s
                  GO TO 20
               end if

! Restore variables
50             u = svu
               v = svv
               k(1:n) = svk(1:n)

! Try quadratic iteration if it has not been tried
! and the v sequence is converging
               if (vpass .and. (.not. vtry)) GO TO 20

! Recompute qp and scalar values to continue the second stage
               call quadsd(nn, u, v, p, qp, a, b)
               call calcsc(type)
            end if
         end if
         ovv = vv
         oss = ss
         otv = tv
         ots = ts
      end do
      return
   end subroutine fxshfr

   subroutine quadit(uu, vv, nz)

! Variable-shift k-polynomial iteration for a quadratic factor, converges
! only if the zeros are equimodular or nearly so.
! uu,vv - coefficients of starting quadratic
! nz - number of zero found

      real(kind=dp), intent(IN) :: uu
      real(kind=dp), intent(IN) :: vv
      integer, intent(OUT) :: nz

      real(kind=dp) :: ui, vi
      real :: mp, omp, ee, relstp, t, zm
      integer :: type, i, j
      logical :: tried

      nz = 0
      tried = .false.
      u = uu
      v = vv
      j = 0

! Main loop
10    call quad(1.d0, u, v, szr, szi, lzr, lzi)

! Return if roots of the quadratic are real and not
! close to multiple or nearly equal and  of opposite sign.
      if (abs(abs(szr) - abs(lzr)) > .01d0 * abs(lzr)) return

! Evaluate polynomial by quadratic synthetic division
      call quadsd(nn, u, v, p, qp, a, b)
      mp = abs(a - szr * b) + abs(szi * b)

! Compute a rigorous  bound on the rounding error in evaluting p
      zm = sqrt(abs(real(v)))
      ee = 2.*abs(real(qp(1)))
      t = -szr * b
      do i = 2, n
         ee = ee * zm + abs(real(qp(i)))
      end do
      ee = ee * zm + abs(real(a) + t)
      ee = (5.*mre + 4.*are) * ee - (5.*mre + 2.*are) * (abs(real(a) + t) + &
                                                         abs(real(b)) * zm) + 2.*are * abs(t)

! Iteration has converged sufficiently if the
! polynomial value is less than 20 times this bound
      if (mp <= 20.d0 * ee) then
         nz = 2
         return
      end if
      j = j + 1

! Stop iteration after 20 steps
      if (j > 20) return
      if (j >= 2) then
         if (.not. (relstp > .01 .or. mp < omp .or. tried)) then

! A cluster appears to be stalling the convergence.
! five fixed shift steps are taken with a u,v close to the cluster
            if (relstp < eta) relstp = eta
            relstp = sqrt(relstp)
            u = u - u * relstp
            v = v + v * relstp
            call quadsd(nn, u, v, p, qp, a, b)
            do i = 1, 5
               call calcsc(type)
               call nextk(type)
            end do
            tried = .true.
            j = 0
         end if
      end if
      omp = mp

! Calculate next k polynomial and new u and v
      call calcsc(type)
      call nextk(type)
      call calcsc(type)
      call newest(type, ui, vi)

! If vi is zero the iteration is not converging
      if (vi == 0.d0) return
      relstp = abs((vi - v) / vi)
      u = ui
      v = vi
      GO TO 10
   end subroutine quadit

   subroutine realit(sss, nz, iflag)

! Variable-shift h polynomial iteration for a real
! zero.
! sss   - starting iterate
! nz    - number of zero found
! iflag - flag to indicate a pair of zeros near real axis.

      real(kind=dp), intent(IN OUT) :: sss
      integer, intent(OUT) :: nz, iflag

      real(kind=dp) :: pv, kv, t, s
      real :: ms, mp, omp, ee
      integer :: i, j

      nz = 0
      s = sss
      iflag = 0
      j = 0

! Main loop
10    pv = p(1)

! Evaluate p at s
      qp(1) = pv
      do i = 2, nn
         pv = pv * s + p(i)
         qp(i) = pv
      end do
      mp = abs(pv)

! Compute a rigorous bound on the error in evaluating p
      ms = abs(s)
      ee = (mre / (are + mre)) * abs(real(qp(1)))
      do i = 2, nn
         ee = ee * ms + abs(real(qp(i)))
      end do

! Iteration has converged sufficiently if the
! polynomial value is less than 20 times this bound
      if (mp <= 20.d0 * ((are + mre) * ee - mre * mp)) then
         nz = 1
         szr = s
         szi = 0.d0
         return
      end if
      j = j + 1

! Stop iteration after 10 steps
      if (j > 10) return
      if (j >= 2) then
         if (abs(t) <= .001d0 * abs(s - t) .and. mp > omp) then
! A cluster of zeros near the real axis has been encountered,
! return with iflag set to initiate a quadratic iteration
            iflag = 1
            sss = s
            return
         end if
      end if

! Return if the polynomial value has increased significantly
      omp = mp

! Compute t, the next polynomial, and the new iterate
      kv = k(1)
      qk(1) = kv
      do i = 2, n
         kv = kv * s + k(i)
         qk(i) = kv
      end do
      if (abs(kv) > abs(k(n)) * 10.*eta) then
! Use the scaled form of the recurrence if the value of k at s is nonzero
         t = -pv / kv
         k(1) = qp(1)
         do i = 2, n
            k(i) = t * qk(i - 1) + qp(i)
         end do
      else
! Use unscaled form
         k(1) = 0.0d0
         do i = 2, n
            k(i) = qk(i - 1)
         end do
      end if
      kv = k(1)
      do i = 2, n
         kv = kv * s + k(i)
      end do
      t = 0.d0
      if (abs(kv) > abs(k(n)) * 10.*eta) t = -pv / kv
      s = s + t
      GO TO 10
   end subroutine realit

   subroutine calcsc(type)

! This routine calculates scalar quantities used to
! compute the next k polynomial and new estimates of
! the quadratic coefficients.
! type - integer variable set here indicating how the
! calculations are normalized to avoid overflow

      integer, intent(OUT) :: type

! Synthetic division of k by the quadratic 1,u,v
      call quadsd(n, u, v, k, qk, c, d)
      if (abs(c) <= abs(k(n)) * 100.*eta) then
         if (abs(d) <= abs(k(n - 1)) * 100.*eta) then
            type = 3
! type=3 indicates the quadratic is almost a factor of k
            return
         end if
      end if

      if (abs(d) >= abs(c)) then
         type = 2
! type=2 indicates that all formulas are divided by d
         e = a / d
         f = c / d
         g = u * b
         h = v * b
         a3 = (a + g) * e + h * (b / d)
         a1 = b * f - a
         a7 = (f + u) * a + h
         return
      end if
      type = 1
! type=1 indicates that all formulas are divided by c
      e = a / c
      f = d / c
      g = u * e
      h = v * b
      a3 = a * e + (h / c + g) * b
      a1 = b - a * (d / c)
      a7 = a + g * d + h * f
      return
   end subroutine calcsc

   subroutine nextk(type)

! Computes the next k polynomials using scalars computed in calcsc.

      integer, intent(IN) :: type

      real(kind=dp) :: temp
      integer :: i

      if (type /= 3) then
         temp = a
         if (type == 1) temp = b
         if (abs(a1) <= abs(temp) * eta * 10.) then
! If a1 is nearly zero then use a special form of the recurrence
            k(1) = 0.d0
            k(2) = -a7 * qp(1)
            do i = 3, n
               k(i) = a3 * qk(i - 2) - a7 * qp(i - 1)
            end do
            return
         end if

! Use scaled form of the recurrence
         a7 = a7 / a1
         a3 = a3 / a1
         k(1) = qp(1)
         k(2) = qp(2) - a7 * qp(1)
         do i = 3, n
            k(i) = a3 * qk(i - 2) - a7 * qp(i - 1) + qp(i)
         end do
         return
      end if

! Use unscaled form of the recurrence if type is 3
      k(1) = 0.d0
      k(2) = 0.d0
      do i = 3, n
         k(i) = qk(i - 2)
      end do
      return
   end subroutine nextk

   subroutine newest(type, uu, vv)

! Compute new estimates of the quadratic coefficients
! using the scalars computed in calcsc.

      integer, intent(IN) :: type
      real(kind=dp), intent(OUT) :: uu
      real(kind=dp), intent(OUT) :: vv

      real(kind=dp) :: a4, a5, b1, b2, c1, c2, c3, c4, temp

! Use formulas appropriate to setting of type.
      if (type /= 3) then
         if (type /= 2) then
            a4 = a + u * b + h * f
            a5 = c + (u + v * f) * d
         else
            a4 = (a + g) * f + h
            a5 = (f + u) * c + v * d
         end if

! Evaluate new quadratic coefficients.
         b1 = -k(n) / p(nn)
         b2 = -(k(n - 1) + b1 * p(n)) / p(nn)
         c1 = v * b2 * a1
         c2 = b1 * a7
         c3 = b1 * b1 * a3
         c4 = c1 - c2 - c3
         temp = a5 + b1 * a4 - c4
         if (temp /= 0.d0) then
            uu = u - (u * (c3 + c2) + v * (b1 * a1 + b2 * a7)) / temp
            vv = v * (1.+c4 / temp)
            return
         end if
      end if

! If type=3 the quadratic is zeroed
      uu = 0.d0
      vv = 0.d0
      return
   end subroutine newest

   subroutine quadsd(nn, u, v, p, q, a, b)

! Divides p by the quadratic  1,u,v  placing the
! quotient in q and the remainder in a,b.

      integer, intent(IN) :: nn
      real(kind=dp), intent(IN) :: u, v, p(nn)
      real(kind=dp), intent(OUT) :: q(nn), a, b

      real(kind=dp) :: c
      integer :: i

      b = p(1)
      q(1) = b
      a = p(2) - u * b
      q(2) = a
      do i = 3, nn
         c = p(i) - u * a - v * b
         q(i) = c
         b = a
         a = c
      end do
      return
   end subroutine quadsd

   subroutine quad(a, b1, c, sr, si, lr, li)

! Calculate the zeros of the quadratic a*z**2+b1*z+c.
! The quadratic formula, modified to avoid overflow, is used to find the
! larger zero if the zeros are real and both zeros are complex.
! The smaller real zero is found directly from the product of the zeros c/a.

      real(kind=dp), intent(IN) :: a, b1, c
      real(kind=dp), intent(OUT) :: sr, si, lr, li

      real(kind=dp) :: b, d, e

      if (a /= 0.d0) GO TO 20
      sr = 0.d0
      if (b1 /= 0.d0) sr = -c / b1
      lr = 0.d0
10    si = 0.d0
      li = 0.d0
      return

20    if (c == 0.d0) then
         sr = 0.d0
         lr = -b1 / a
         GO TO 10
      end if

! Compute discriminant avoiding overflow
      b = b1 / 2.d0
      if (abs(b) >= abs(c)) then
         e = 1.d0 - (a / b) * (c / b)
         d = sqrt(abs(e)) * abs(b)
      else
         e = a
         if (c < 0.d0) e = -a
         e = b * (b / abs(c)) - e
         d = sqrt(abs(e)) * sqrt(abs(c))
      end if
      if (e >= 0.d0) then

! Real zeros
         if (b >= 0.d0) d = -d
         lr = (-b + d) / a
         sr = 0.d0
         if (lr /= 0.d0) sr = (c / lr) / a
         GO TO 10
      end if
! complex conjugate zeros
      sr = -b / a
      lr = sr
      si = abs(d / a)
      li = -si
      return
   end subroutine quad

end module Solve_Real_Poly
