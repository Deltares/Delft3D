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

module m_addexternalboundarypoints
   use m_a1x1a2x2, only: a1x1a2x2
   use m_mirrorcell, only: mirrorcell

   implicit none

   private

   public :: addexternalboundarypoints

contains

   subroutine addexternalboundarypoints
      use precision, only: dp
      use m_netw
      use m_flow
      use m_flowgeom
      use unstruc_messages
      use m_alloc
      use timespace, only: polyindexweight
      use m_missing
      use m_sobekdfm
      use m_sferic, only: jsferic, jasfer3D
      use geometry_module, only: half
      use m_reapol
      use m_is_1d_boundary_candidate, only: is_1d_boundary_candidate
      use m_filez, only: oldfil, doclose

      integer :: i, k, k1, k2, L, Lf, lb, nn, ierr, k3, k4, id, istart, num1d2d, kL, kR, mpliz

      real(kind=dp) :: wL, wR
      real(kind=dp) :: xci, yci, xce2, yce2
      integer, allocatable :: kdum(:)

      ! Update Mar'15: 1D/2D bnds points/links used to be mixed, now all 1D bnds nodes/links come first,
      ! followed by 2D bnd.
      ! So: links: 1..lnx1d..lnxi..lnx1db..lnx
      !     nodes: 1..ndx2d..ndxi..ndx1db..ndx  (notice how 1D bnds nodes go before 2D bnds, while 2D internal go before 1D internal).
      Lf = lnxi
      k = ndxi
      lnx1db = lnxi ! Counter for last 1D boundary link (==lnxi if no 1D bnds)
      ndx1db = ndxi ! Counter for last 1D boundary node (==ndxi if no 1D bnds)
      do id = 1, 2 ! 1D and 2D treated after one another
         do Lb = 1, nbndz ! add boundary link and update ln array waterlevel bnds
            L = kez(Lb)
            !if (    (id == 1 .and. (kn(3,L) .ne. 1 .and. kn(3,L) .ne. 4)) &     !    we're in 1D loop, so skip this 2D boundary cell for the moment.
            !   .or. (id == 2 .and. (kn(3,L) .eq. 1 .or. kn(3,L) .eq. 4))) then  ! or we're in 2D loop, so skip this 1D boundary cell for the moment.
            ! Wijzigingsvoorstel:
            if (id == 1 .and. kn(3, L) /= 1 .and. kn(3, L) /= 6 & !    we're in 1D loop, so skip this 2D boundary cell for the moment.
                .or. id == 2 .and. kn(3, L) /= 2) then ! or we're in 2D loop, so skip this 1D boundary cell for the moment.
               cycle
            end if

            k = k + 1
            Lf = Lf + 1
            if (id == 1) then ! Increment 1D boundary node & link counters
               lnx1db = Lf
               ndx1db = k
            end if

            k1 = k ! external point
            k2 = abs(lne(1, L)) ! internal point

            ln(1, Lf) = k1
            ln(2, Lf) = k2

            ln2lne(Lf) = L
            lne2ln(L) = Lf ! after this, only closed edges will still have a (negative)
            ! reference to an inside node

            nn = 4
            allocate (nd(k1)%x(nn), nd(k1)%y(nn), stat=ierr)
            call aerr('nd(k1)%x(nn), nd(k1)%y(nn)', ierr, nn * 2)

            k3 = kn(1, L); k4 = kn(2, L)
            if (kn(3, L) == 2) then ! in 2D mirror cell
               call mirrorcell(k2, xk(k3), yk(k3), xk(k4), yk(k4), xci, yci, xz(k1), yz(k1), xce2, yce2, nd(k1)%x, nd(k1)%y)
               xzw(k1) = xz(k1); yzw(k1) = yz(k1)

               if (izbndpos == 0) then ! as in D3DFLOW

               else if (izbndpos == 1) then ! on network boundary
!           xz(k1)  = 0.5d0*( xk(k3) + xk(k4 ) )
!           yz(k1)  = 0.5d0*( yk(k3) + yk(k4 ) )
                  call half(xk(k3), yk(k3), xk(k4), yk(k4), xz(k1), yz(k1), jsferic, jasfer3D)
               else if (izbndpos == 2) then ! on specified boundary polyline

               end if

               kcu(Lf) = -2
               kcs(k1) = -2
            else ! in 1D mirror point
               if (is_1d_boundary_candidate(L, 1)) then
                  if (izbndpos == 0) then ! as in D3DFLOW
!               xz(k1)  = 2d0*xk(k3) - xk(k4)
!               yz(k1)  = 2d0*yk(k3) - yk(k4)
                     call a1x1a2x2(xk(k3), yk(k3), xk(k4), yk(k4), 2d0, -1d0, xz(k1), yz(k1))
                  else if (izbndpos == 1) then ! on network boundary
!               xz(k1)  = 1.5d0*xk(k3) - 0.5d0*xk(k4)
!               yz(k1)  = 1.5d0*yk(k3) - 0.5d0*yk(k4)
                     call a1x1a2x2(xk(k3), yk(k3), xk(k4), yk(k4), 1.5d0, -0.5d0, xz(k1), yz(k1))
                  else if (izbndpos == 2) then ! on specified boundary polyline

                  end if
                  ln(2, Lf) = abs(lne(1, L)) ! this overrides previous k2
                  kcu(Lf) = -1
                  kcs(k1) = -1
                  nd(k1)%x = xz(k1); nd(k1)%y = yz(k1) ! todo, naar allocateandset1D nodestuff
                  xzw(k1) = xz(k1); yzw(k1) = yz(k1)
               else if (is_1d_boundary_candidate(L, 2)) then
                  if (izbndpos == 0) then ! as in D3DFLOW
!               xz(k1) = 2d0*xk(k4) - xk(k3)
!               yz(k1) = 2d0*yk(k4) - yk(k3)
                     call a1x1a2x2(xk(k3), yk(k3), xk(k4), yk(k4), -1d0, 2d0, xz(k1), yz(k1))
                  else if (izbndpos == 1) then ! on network boundary
!               xz(k1) = 1.5d0*xk(k4) - 0.5d0*xk(k3)
!               yz(k1) = 1.5d0*yk(k4) - 0.5d0*yk(k3)
                     call a1x1a2x2(xk(k3), yk(k3), xk(k4), yk(k4), -0.5d0, 1.5d0, xz(k1), yz(k1))
                  else if (izbndpos == 2) then ! on specified boundary polyline

                  end if
                  ln(2, Lf) = abs(lne(2, L))
                  kcu(Lf) = -1
                  kcs(k1) = -1
                  nd(k1)%x = xz(k1); nd(k1)%y = yz(k1) ! todo, naar allocateandset1D nodestuff
                  xzw(k1) = xz(k1); yzw(k1) = yz(k1)
               end if
            end if

         end do
! enddo ! id=1,2

! do id=1,2 ! 1D and 2D treated after one another
         do Lb = 1, nbndu ! idem u bnds, duplicatie niet top in elegantie

            L = keu(Lb)
            !if (    (id == 1 .and. (kn(3,L) .ne. 1 .and. kn(3,L) .ne. 4)) &      !    we're in 1D loop, so skip this 2D boundary cell for the moment.
            !   .or. (id == 2 .and. (kn(3,L) .eq. 1 .or. kn(3,L) .eq. 4))) then  ! or we're in 2D loop, so skip this 1D boundary cell for the moment.
            ! Wijzigingsvoorstel:
            if (id == 1 .and. kn(3, L) /= 1 .and. kn(3, L) /= 6 & !    we're in 1D loop, so skip this 2D boundary cell for the moment.
                .or. id == 2 .and. kn(3, L) /= 2) then ! or we're in 2D loop, so skip this 1D boundary cell for the moment.
               cycle
            end if

            k = k + 1
            Lf = Lf + 1
            if (id == 1) then ! Increment 1D boundary node & link counters
               lnx1db = Lf
               ndx1db = k
            end if
            k1 = k ! external point
            k2 = abs(lne(1, L)) ! internal point

            ln(1, Lf) = k1
            ln(2, Lf) = k2
            ln2lne(Lf) = L
            lne2ln(L) = Lf

            nn = 4
            allocate (nd(k1)%x(nn), nd(k1)%y(nn), stat=ierr)
            call aerr('nd(k1)%x(nn), nd(k1)%y(nn)', ierr, nn * 2)
            k3 = kn(1, L); k4 = kn(2, L)
            if (kn(3, L) == 2) then ! in 2D mirror cell
               call mirrorcell(k2, xk(k3), yk(k3), xk(k4), yk(k4), xci, yci, xz(k1), yz(k1), xce2, yce2, nd(k1)%x, nd(k1)%y)
               xzw(k1) = xz(k1); yzw(k1) = yz(k1)

               kcu(Lf) = -2
               kcs(k1) = -2
            else ! in 1D mirror point
               if (is_1d_boundary_candidate(L, 1)) then
!            xz(k1)  = 2d0*xk(k3) - xk(k4)
!            yz(k1)  = 2d0*yk(k3) - yk(k4)
                  call a1x1a2x2(xk(k3), yk(k3), xk(k4), yk(k4), 2d0, -1d0, xz(k1), yz(k1))
                  ln(2, Lf) = abs(lne(1, L)) ! this overrides previous k2
                  kcu(Lf) = -1
                  kcs(k1) = -1
                  nd(k1)%x = xz(k1); nd(k1)%y = yz(k1) ! todo: JN: naar allocateandset1D nodestuff
                  xzw(k1) = xz(k1); yzw(k1) = yz(k1)
               else if (is_1d_boundary_candidate(L, 2)) then
!            xz(k1)  = 2d0*xk(k4) - xk(k3)
!            yz(k1)  = 2d0*yk(k4) - yk(k3)
                  call a1x1a2x2(xk(k3), yk(k3), xk(k4), yk(k4), -1d0, 2.0d0, xz(k1), yz(k1))
                  ln(2, Lf) = abs(lne(2, L))
                  kcu(Lf) = -1
                  kcs(k1) = -1
                  nd(k1)%x = xz(k1); nd(k1)%y = yz(k1) ! todo: JN: naar allocateandset1D nodestuff
                  xzw(k1) = xz(k1); yzw(k1) = yz(k1)
               end if
            end if

         end do
      end do ! id=1,2

      do Lb = 1, nbnd1d2d ! add boundary link and update ln array 1d2dbnds
         L = ke1d2d(Lb)

         k = k + 1
         Lf = Lf + 1
         k1 = k ! external point
         k2 = abs(lne(1, L)) ! internal point

         ln(1, Lf) = k1
         ln(2, Lf) = k2

         ln2lne(Lf) = L
         lne2ln(L) = Lf ! after this, only closed edges will still have a (negative)
         ! reference to an inside node

         nn = 4
         allocate (nd(k1)%x(nn), nd(k1)%y(nn), stat=ierr)
         call aerr('nd(k1)%x(nn), nd(k1)%y(nn)', ierr, nn * 2)

         k3 = kn(1, L); k4 = kn(2, L)
         if (kn(3, L) /= 1) then ! in 2D mirror cell
            call mirrorcell(k2, xk(k3), yk(k3), xk(k4), yk(k4), xci, yci, xz(k1), yz(k1), xce2, yce2, nd(k1)%x, nd(k1)%y)
            xzw(k1) = xz(k1); yzw(k1) = yz(k1)

            !xz(k1)  = 0.5d0*( xk(k3) + xk(k4 ) )
            !yz(k1)  = 0.5d0*( yk(k3) + yk(k4 ) )

            kcu(Lf) = -2
            kcs(k1) = -2
         else ! in 1D mirror point
            ! non-sensible: 1D internal point that accidentally lies on 2DFM -- 1DSOBEK boundary (should not happen)
         end if
      end do

      write (msgbuf, '(a,i0,a)') 'addexternalboundarypoints: added ', nbnd1d2d, ' bnd points for SOBEK1D-FM2D connections.'
      call dbg_flush()

      ! Special for 1D2D: interpolate zpl crest levels for each open boundary link u-point
      if (nbnd1d2d > 0) then
         call realloc(zcrest1d2d, nbnd1d2d, keepExisting=.false.)
         call savepol()
         istart = 0
         num1d2d = 0 ! local counter for 1D2D open boundary points (should be identical to ordering inside zbnd1d2d array, etc. If not, programming error!)
         do i = 1, nopenbndsect
            if (openbndtype(i) == IBNDTP_1D2D) then
               call oldfil(mpliz, trim(openbndfile(i))) ! The original .pli(z) file as listed in the external forcings file.
               call reapol(mpliz, 0)
               call realloc(kdum, maxpol, keepExisting=.false.)
               kdum = 1 ! Mask all pli points as valid for interpolation: each pli point should have a zpl crest value.

               do Lb = istart + 1, nopenbndlin(i)
                  num1d2d = num1d2d + 1
                  L = openbndlin(Lb) ! Net link
                  Lf = lne2ln(L) ! Flow link

                  if (Lf > 0 .and. Lf <= lnx) then
                     k1 = ln(1, Lf)
                     k2 = ln(2, Lf)

                     k3 = kn(1, L); k4 = kn(2, L)
                     ! NOTE: UNST-1324: do this once more, so we have the "probe" point xce2, yce2 for the proper intersection.
                     ! TODO: AvD: remove dupliacy by merging this loop 1:nopenbndsect and the preceding loop 1:nbnd1d2d.
                     call mirrorcell(k2, xk(k3), yk(k3), xk(k4), yk(k4), xci, yci, xz(k1), yz(k1), xce2, yce2, nd(k1)%x, nd(k1)%y)

                     call polyindexweight(xce2, yce2, xz(k2), yz(k2), xpl, ypl, kdum, npl, kL, wL, kR, wR) ! xz(k1), yz(k1),
                     if (.not. (kL > 0 .and. kR > 0)) then
                        call polyindexweight(xz(k2), yz(k2), 2 * xz(k2) - xce2, 2 * yz(k2) - yce2, xpl, ypl, kdum, npl, kL, wL, kR, wR)
                     end if
                     if (kL > 0 .and. kR > 0) then
                        if (zpl(kL) == dmiss) then
                           write (msgbuf, '(a,a,a,i0)') 'Missing crest level for SOBEK1D-FM2D boundary ''', trim(openbndname(i)), ''': missing value found on point #', kL
                           call err_flush()
                        elseif (zpl(kR) == dmiss) then
                           write (msgbuf, '(a,a,a,i0)') 'Missing crest level for SOBEK1D-FM2D boundary ''', trim(openbndname(i)), ''': missing value found on point #', kR
                           call err_flush()
                        else
                           zcrest1d2d(num1d2d) = wL * zpl(kL) + wR * zpl(kR)
                        end if
                     else
                        ! Should not happen for any model, only for debugging
                        write (msgbuf, '(a,i0,a,f12.4,a,f12.4,a)') 'Could not find crest level for SOBEK1D-FM2D boundary '''//trim(openbndname(i))//''': no overlap found for net link ', &
                           L, ' [', (xce2 + xz(k2)) / 2., ',', (yce2 + yz(k2)) / 2., ']'
                        call err_flush()
                     end if
                  else
                     ! Should not happen for any model, only for debugging
                     write (msgbuf, '(a,a,a,i0)') 'Could not set crest level for SOBEK1D-FM2D boundary ''', trim(openbndname(i)), ''': no flow link found for net link ', L
                     call err_flush()
                  end if
               end do
               call doclose(mpliz)
            end if
            istart = nopenbndlin(i)
         end do
         call restorepol()
         if (allocated(kdum)) deallocate (kdum)
      end if ! nbnd1d2d > 0
   end subroutine addexternalboundarypoints

end module m_addexternalboundarypoints
