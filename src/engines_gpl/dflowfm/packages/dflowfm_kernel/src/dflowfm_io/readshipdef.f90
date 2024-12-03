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

module m_readshipdef

   implicit none

   private

   public :: readshipdef

contains

   subroutine readshipdef()
      use precision, only: dp
      use m_sferic
      use m_ship
      use unstruc_model
      use m_arcinfo
      use m_missing
      use m_flow
      use m_flowgeom
      implicit none
      integer :: minp, ja, n, nn, i, j, ierr, L1
      logical jawel
      real(kind=dp), allocatable :: e(:, :)

      inquire (file=md_shipdeffile, exist=jawel)

      if (.not. jawel) then
         return
      end if

      call oldfil(minp, md_shipdeffile)

      call zoekinteger(minp, 'JAPRESSUREHULL', japressurehull, ja)
      call zoekinteger(minp, 'JAFRIC', jafric, ja)
      call zoekinteger(minp, 'JAPROP', japrop, ja)
      call zoekinteger(minp, 'JASHFRICIMPL', jashfricimpl, ja)
      call zoekinteger(minp, 'JAPHIFROMTXY', japhifromtxy, ja)
      call zoekdouble(minp, 'TRELAX', Trelax, ja)
      call zoekdouble(minp, 'CFSKIN', Cfskin, ja)
      call zoekdouble(minp, 'ALFAHULL', alfahull, ja) ! not used
      call zoekinteger(minp, 'ITHULLMX', ithullmx, ja)
      call zoekdouble(minp, 'VICUSHIP', vicuship, ja)
      call zoekinteger(minp, 'NUMSMO', numsmo, ja)
      call zoekdouble(minp, 'WSMO', wsmo, ja)
      call zoekdouble(minp, 'RETURB', returb, ja)
      call zoekinteger(minp, 'IHULLMETHOD', ihullmethod, ja)

      rewind (minp)

      do N = 1, nshiptxy

         call zoekinteger(minp, 'NSHIPN', nn, ja)
         call zoekinteger(minp, 'ICONTROLTYP', icontroltyp(n), ja)
         call zoekdouble(minp, 'SHL', shL(n), ja); if (ja == 1) shL(n) = 0.5d0 * shL(n) ! shiplenght on input, then half length
         call zoekdouble(minp, 'SHB', shB(n), ja); if (ja == 1) shB(n) = 0.5d0 * shB(n) ! idem width
         call zoekdouble(minp, 'SHD', shd(n), ja); if (ja == 1) chkadvd = min(chkadvd, 1d-2 * shd(n))
         call zoekdouble(minp, 'DEADW', deadw(n), ja); if (ja == 1) deadw(n) = 1000d0 * deadw(n) !kg
         call zoekdouble(minp, 'POWERMX', powermx(n), ja); if (ja == 1) powermx(n) = 1000d0 * 0.75d0 * powermx(n) ! conversion hp to kw
         call zoekdouble(minp, 'SPEEDMX', speedmx(n), ja); if (ja == 1) speedmx(n) = 0.514444d0 * speedmx(n) ! conversion knots to m/s

         if (ja == 1 .and. speedmx(n) /= 0d0) then
            stuwmx(n) = 0.65d0 * powermx(n) / speedmx(n) ! propellor efficiency 0.65
         end if

         call zoekdouble(minp, 'FSTUW', fstuw(n), ja)
         call zoekdouble(minp, 'FROER', froer(n), ja)

         call zoekdouble(minp, 'SHX', shx(n), ja)
         call zoekdouble(minp, 'SHY', shy(n), ja)
         call zoekdouble(minp, 'SHI', shi(n), ja); if (ja == 1) shi(n) = shi(n) * dg2rd
         call zoekdouble(minp, 'SHU', shu(n), ja)
         call zoekdouble(minp, 'SHV', shv(n), ja)
         call zoekdouble(minp, 'SHO', sho(n), ja)

         if (ja == 1 .and. sho(n) /= 0d0) then
            sho(n) = twopi / sho(n)
         end if

      end do

      call doclose(minp)

      L1 = index(md_shipdeffile, '.') - 1

      inquire (file=trim(md_shipdeffile(1:L1))//'_hull.asc', exist=jawel)
      if (jawel) then
         call oldfil(minp, trim(md_shipdeffile(1:L1))//'_hull.asc')
         call REAARC(MINP, -1)
         dxa = 1d0 / (mca - 1)
         dya = 1d0 / (nca - 1)
         do i = 1, mca
            do j = 1, nca
               if (d(i, j) == dmiss) then
                  d(i, j) = 0d0
               else
                  d(i, j) = d(i, j) * shD(1)
               end if
            end do
         end do
         if (1 == 0) then
            mca = mca + 2
            nca = nca + 2
            allocate (e(mca, nca)); e = 0d0
            do i = 2, mca - 1
               do j = 2, nca - 1
                  e(i, j) = d(i - 1, j - 1)
               end do
            end do
            deallocate (d)
            allocate (d(mca, nca))
            d = e
            deallocate (e)
         end if
      end if

      if (vicuship /= 0d0) then
         if (allocated(vicushp)) deallocate (vicushp)
         allocate (vicushp(lnx), stat=ierr); vicushp = 0d0
         call aerr('vicushp(lnx)', ierr, lnx)
      end if

      if (japressurehull >= 2) then
         nonlin2D = 2
      end if

   end subroutine readshipdef

end module m_readshipdef
