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
module m_wrildb
   implicit none
contains
   !> Writes a polygon/land boundary/cross section file.
      !! The polyline(s) are written as a sequence of Tekal blocks.
      !! The name for each Tekal block can be specified, or is auto-generated
      !! as 'L00x' otherwise.
   subroutine WRILDB(MPOL, XSH, YSH, NSH, NCLAN, nnclan, ZSH, nzsh, names, namlen, nnam)
      use precision, only: dp
      use M_MISSING
      use m_polygon ! , only : zpl, DZL, DZR, jakol45
      use gridoperations
      use m_readyy
      use m_inview

      integer, intent(inout) :: mpol !< Open file pointer where to write to.
      integer, intent(in) :: nsh !< Number of points in polyline.
      real(kind=dp), intent(in) :: XSH(NSH), YSH(NSH) !< Coordinates, polylines can be separated by dmiss value.
      integer, intent(in) :: namlen !< string length of names.
      integer, intent(in) :: nnam !< Number of polyline names.
      character(len=namlen), intent(in) :: names(nnam) !< Names of all polylines, header of each Tekal Block.
      integer, intent(in) :: NCLAN(*) !< Third integer value for each point in XSH, optional: use nnclan=0 to ignore
      integer, intent(in) :: nnclan !< Size of NCLAN, use 0 to ignore.
      real(kind=dp), intent(in) :: ZSH(*) !< Third double  value for each point in XSH, optional: use nzsh=0 to ignore
      integer, intent(in) :: nzsh !< Size of ZSH, use 0 to ignore.

      integer :: L

      integer :: i, ipli, npli, mbna, ncol, inscreen
      integer, allocatable :: istart(:), iend(:)
      character(len=max(namlen, 10)) :: name
      character(len=1) :: cdigits
      character(len=40) :: rec
      logical :: jaNCLAN, jaZSH

      ! Only include third column when size is equal to XSH array (or larger).
      jaNCLAN = nNCLAN >= NSH
      jaZSH = nZSH >= NSH

      call READYY('Writing Polygon / Land Boundary FILE', 0d0)

      MBNA = 0
      if (MBNA > 0) call newfil(mbna, 'bna.bna')

      if (NSH <= 0) goto 11
      allocate (istart(nsh), iend(nsh))

      ! First, find starts and ends of all polylines (separated by dmiss line(s))
      ! such that each can be written as a named Tekal block later.
      ipli = 0
      i = 0
      pli: do
         i = i + 1
         if (i > nsh) exit pli
         if (xsh(i) == dmiss) cycle pli

         ! Start of a new polyline found
         ipli = ipli + 1
         istart(ipli) = i
         pts: do
            i = i + 1
            if (i > nsh) exit pts
            if (xsh(i) == dmiss) exit pts
         end do pts
         iend(ipli) = i - 1
      end do pli

      npli = ipli

      ! Start writing the set of polyline(s).
      KMOD = max(1, NSH / 100)

      write (cdigits, '(i1)') int(floor(log10(dble(npli)) + 1)) ! nr of digits in npli

      if (jaNCLAN .or. jaZSH) then
         ncol = 3
         if (jakol45 == 1) then
            ncol = 5
         else if (jakol45 == 2) then
            ncol = 9
         end if
      else
         ncol = 2
      end if
      do ipli = 1, npli

         if (jakol45 == 2) then
            inscreen = 0
            do I = istart(ipli), iend(ipli)
               if (inview(xpl(i), ypl(i))) then
                  inscreen = 1
               end if
            end do
            if (inscreen == 0) then
               cycle
            end if
         end if

         if (ipli <= nnam) then
            name = names(ipli)
         else
            name = ' '
         end if
         ! Generate 'L00x' name if empty
         if (len_trim(name) <= 0) then
            write (name, '(A1,I'//cdigits//'.'//cdigits//')') 'L', ipli

            if (MBNA > 0) then
               rec = ' '
               write (rec, '(A1,I'//cdigits//'.'//cdigits//')') '"', ipli
               L = len_trim(rec)
               write (rec(L + 1:), '(A)') '","",'
               L = len_trim(rec)
               write (rec(L + 1:), '(i10)') - (iend(ipli) - istart(ipli) + 1)
               write (mbna, '(a)') rec
            end if

         end if

         write (MPOL, '(A)') trim(name)
         write (MPOL, '(I6,I6)') iend(ipli) - istart(ipli) + 1, ncol

!        rec = '"324","",-2

         do I = istart(ipli), iend(ipli)
            if (jaNCLAN) then
               write (MPOL, '(2F15.6,I5)') XSH(I), YSH(I), NCLAN(I)
            elseif (jaZSH) then
               if (jakol45 == 1) then
                  write (MPOL, '(5F15.6)') XSH(I), YSH(I), zpl(i), DZL(i), DZR(i)
               else if (jakol45 == 2) then
                  write (MPOL, '(2F15.6, 7F7.2)') XSH(I), YSH(I), zpl(i), DZL(i), DZR(i), dcrest(i), dtL(i), dtr(i), dveg(i)
               else
                  write (MPOL, '(3F15.6)') XSH(I), YSH(I), ZSH(I)
               end if
               if (MBNA > 0) write (Mbna, '(2F15.6)') XSH(I), YSH(I)
            else
               write (MPOL, '(2F15.6)') XSH(I), YSH(I)
            end if

            if (mod(I, KMOD) == 0) then
               call READYY(' ', min(1d0, dble(I) / max(1, NSH)))
            end if
         end do ! pts of one polyline
      end do ! all polylines

      deallocate (istart, iend)
11    call READYY(' ', -1d0)
      call doclose(MPOL)

      if (MBNA > 0) call doclose(MBNA)

      return
   end
end module m_wrildb
