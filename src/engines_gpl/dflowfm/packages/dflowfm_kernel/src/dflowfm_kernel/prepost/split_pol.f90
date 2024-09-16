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

!> split polygon recursively into (Ni X Nj) smaller polygons, at most MAXsplit times, until polygons have at most NPLmax nodes
 subroutine split_pol(Ni, Nj, NPLmax, MAXsplit)
    use m_polygon
    use m_tpoly
    use m_alloc
    use m_missing
    use unstruc_messages
    use m_wrildb
    implicit none

    integer, intent(in) :: Ni
    integer, intent(in) :: Nj
    integer, intent(in) :: NPLmax !< maximum polygon dimension for splitting
    integer, intent(in) :: MAXsplit !< maximum number of split levels in recursion

    type(tpoly), dimension(:), allocatable :: pli

    character(len=128) :: FNAM

    double precision :: xmin, ymin, xmax, ymax
    double precision :: xa, ya, xb, yb

    integer :: i, j, ipol, N, numpols

    double precision, dimension(:), allocatable :: rwrk
    integer, dimension(:), allocatable :: iwrk
    integer :: lrwk

    integer :: isplit, numsplit, mpol
    integer :: NPL_prev

    integer :: ierr

    integer, parameter :: NCCP = 5
    double precision, dimension(NCCP) :: xccp, yccp

    integer, dimension(0) :: nclan

    integer, parameter :: MAXLRWK = 1e9

    external :: addtopol

    if (NPL < 3) return

    call savepol()

!   polypack: based on Vatti polygon clipping algorithm
    lrwk = NPL
    allocate (rwrk(lrwk))
    allocate (iwrk(lrwk))

    numsplit = 0

    ilp: do isplit = 1, MAXSPLIT
       call pol_to_tpoly(numpols, pli, keepExisting=.false.)
       NPL = 0
       numsplit = 0

       do ipol = 1, numpols
          if (pli(ipol)%len < NPLmax .and. NPLmax > 0) then
             ierr = 0 ! skip, just copy
             call tpoly_to_pol(pli, iselect=ipol)
          else
             numsplit = numsplit + 1

             N = pli(ipol)%len
             xmin = pli(ipol)%xmin
             xmax = pli(ipol)%xmax
             ymin = pli(ipol)%ymin
             ymax = pli(ipol)%ymax

             xa = xmin
             do i = 1, Ni
                xb = xa
                xa = xmin + dble(i) / dble(Ni) * (xmax - xmin)
                xccp = (/xa, xb, xb, xa, xa/)

                ya = ymin
                do j = 1, Nj
                   yb = ya
                   ya = ymin + dble(j) / dble(Nj) * (ymax - ymin)
                   yccp = (/ya, ya, yb, yb, ya/)

                   ierr = 1
                   do while (ierr /= 0 .and. lrwk <= MAXLRWK)
                      NPL_prev = NPL
                      call PPINPO(XCCP, YCCP, NCCP, pli(ipol)%x, pli(ipol)%y, pli(ipol)%len, rwrk, iwrk, lrwk, addtopol, ierr)
                      if (ierr /= 0) then
                         NPL = NPL_prev ! Restore the counter that was modified by callback addtopol().
                         lrwk = int(1.2d0 * dble(lrwk)) + 1
                         call realloc(rwrk, lrwk, keepExisting=.false.)
                         call realloc(iwrk, lrwk, keepExisting=.false.)
                      else
!                        set z-values of new polygons with first z-value of subject polygon
                         if (NPL > NPL_prev) then
                            ZPL(NPL_prev + 1:NPL) = pli(ipol)%z(1)
                         end if
                      end if
                   end do

                   if (ierr /= 0) then
                      write (FNAM, "('error_', I0, '_', I0, '.pol')") isplit, ipol
                      call mess(LEVEL_WARN, 'POLYPACK error, falling back to Sutherland-Hodgman, saving polygon '//trim(FNAM))
                      call newfil(mpol, trim(FNAM))
                      call wrildb(mpol, pli(ipol)%x, pli(ipol)%y, pli(ipol)%len, nclan, 0, zpl, 0, 'error', 5, 1)
                      call doclose(mpol)
                      call restorepol()
                      exit ilp
                   end if
                end do
             end do
          end if
       end do

       if (numsplit == 0) then
!         no polygons were split
          exit
       end if

    end do ilp

    deallocate (rwrk)
    deallocate (iwrk)
    call dealloc_tpoly(pli)

    if (ierr /= 0) then ! fallback

       xmin = huge(1d0)
       xmax = -xmin
       ymin = huge(1d0)
       ymax = -ymin
       do i = 1, NPL
          if (xpl(i) /= DMISS .and. ypl(i) /= DMISS) then
             xmin = min(xpl(i), xmin)
             xmax = max(xpl(i), xmax)
             ymin = min(ypl(i), ymin)
             ymax = max(ypl(i), ymax)
          end if
       end do

!      this turned out to be based on the Sutherland-Hodgman polygon clipping algorithm
       do i = 1, Ni - 1
          call savepol()
          xa = xmin + dble(i) / dble(Ni) * (xmax - xmin)
          call split_pol_with_line(xa, ymin, xa, ymax, 1)
          call pol_to_tpoly(numpols, pli, keepExisting=.false.)

          call restorepol()
          call split_pol_with_line(xa, ymin, xa, ymax, 2)
          call tpoly_to_pol(pli)
       end do

       do j = 1, Nj - 1
          call savepol()
          ya = ymin + dble(j) / dble(Nj) * (ymax - ymin)
          call split_pol_with_line(xmin, ya, xmax, ya, 1)
          call pol_to_tpoly(numpols, pli, keepExisting=.false.)

          call restorepol()
          call split_pol_with_line(xmin, ya, xmax, ya, 2)
          call tpoly_to_pol(pli)
       end do

       call dealloc_tpoly(pli)
    end if

    return
 end subroutine split_pol
