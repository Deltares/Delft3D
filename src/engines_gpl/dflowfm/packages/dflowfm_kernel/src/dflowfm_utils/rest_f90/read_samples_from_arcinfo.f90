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

!> Read samples from an ASCII file.
!! Samples are being stored in a global dataset of m_samples.
subroutine read_samples_from_arcinfo(filnam, jadoorladen, japrompt) ! reaasc
   use m_missing
   use m_samples
   use m_samples_refine, only: iHesstat, iHesstat_DIRTY
   use m_arcinfo
   use m_drawthis
   use m_readyy
   use m_get_samples_boundingbox
   implicit none
   character(len=*), intent(in) :: filnam !< Name of *.asc file.
   integer, intent(in) :: jadoorladen !< Whether or not (1/0) to keep the existing samples in the global set.
   integer, intent(in) :: japrompt !< Whether or not (1/0) to prompt in the GUI for istep-jstep subsampled reading.

   integer :: i, j, istep, marc
   character(len=10) :: TEX

   call READYY('Reading arcinfo file', 0d0)
   call oldfil(marc, filnam)
   call reaarc(marc, japrompt)
   call DOCLOSE(marc)
   call READYY('Reading arcinfo file', 1d0)

   if (mca <= 0 .or. nca <= 0) then
      call message('No samples read from file ', filnam, ' ')
      return
   else if (mca * nca > maxsamarc) then
      return
   end if

   call savesam()
   if (jadoorladen == 0) then
      ns = 0
   end if
   call INCREASEsam(ns + mca * nca)

   write (TEX, '(I10)') mca * nca
   call READYY('Filtering '//trim(TEX)//' Samples Points', 0d0)
   istep = max(int(mca / 100d0 + .5d0), 1)
! SPvdP: j needs to be fastest running index
   do i = 1, mca
      if (mod(i, istep) == 0) then
         call READYY('Filtering '//trim(TEX)//' Samples Points', min(1d0, dble(i) / mca))
      end if

      do j = nca, 1, -1 ! SPvdP: first line needs to be nca'th row
!            if (d(I,J) .ne. dmiss) then  ! SPvdP: we need to maintain structured data
         ns = ns + 1
         xs(ns) = x0 + dxa * (i - 1)
         ys(ns) = y0 + dya * (j - 1)
         zs(ns) = d(i, j)
!            endif
      end do
   end do
   call READYY(' ', -1d0)

!   mark samples as structured, and in supply block sizes
   MXSAM = nca ! j is fastest running index
   MYSAM = mca
   IPSTAT = IPSTAT_NOTOK

!   new sample set: no Hessians computed yet
   iHesstat = iHesstat_DIRTY

   ! deallocate(d) ! Save memory, arcinfo block is no longer needed.

   if (NS > 100000) NDRAW(32) = 7 ! Squares (faster than circles)
   if (NS > 500000) NDRAW(32) = 3 ! Small dots (fastest)

   ! No TIDYSAMPLES required: arcinfo grid was already loaded in correctly sorted order.
   do i = 1, NS
      IPSAM(i) = i
   end do
   call get_samples_boundingbox()
   IPSTAT = IPSTAT_OK
end subroutine read_samples_from_arcinfo
