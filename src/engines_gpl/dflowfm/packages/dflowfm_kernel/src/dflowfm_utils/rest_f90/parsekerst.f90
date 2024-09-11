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

    !> Parses a manually preprocessed SVG file into 1D network 'drawing'.
    !!
    !! Format: each line should have one SVG command (m/M/c/l/z) with coordi nates.
    subroutine parsekerst(filename)
       use m_polygon
       use m_missing
       use unstruc_messages
       use network_data
       use gridoperations
       use unstruc_display, only: minmxns
       use m_wearelt, only: rcir
       use m_delpol

       implicit none

       character(len=*), intent(in) :: filename
       integer :: mfil, i, maxpts, it, maxt, KP, K1, LNU
       character(len=2000) :: line
       real, allocatable :: pts(:)
       real :: startx, starty, curx, cury, x, y, t
       double precision :: zp

       maxpts = 200
       allocate (pts(maxpts))

       curx = 0.0
       cury = 0.0
       open (newunit=mfil, file=trim(filename))

       do
          read (mfil, '(a)', end=999) line
          select case (line(1:1))
          case ('c')
             pts = dmiss
             read (line(3:), *, end=97) pts
97           continue

             do i = 1, maxpts - 5, 6
                if (pts(i) == dmiss .or. pts(i + 1) == dmiss) exit

                !curx = curx + pts(i+4) ! Just take endpoint of bezier curve
                !cury = cury + pts(i+5)
                !npl = npl+1
                !call increasepol(npl, 1)
                !xpl(npl) = curx
                !ypl(npl) = cury
                maxt = 5
                do it = 1, maxt
                   t = real(it) / real(maxt)
                   x = (1.0 - t)**3 * (curx) &
                       + 3.0 * (1.0 - t)**2 * t * (curx + pts(i)) &
                       + 3.0 * (1.0 - t) * t**2 * (curx + pts(i + 2)) &
                       + t**3 * (curx + pts(i + 4))
                   y = (1.0 - t)**3 * (cury) &
                       + 3.0 * (1.0 - t)**2 * t * (cury + pts(i + 1)) &
                       + 3.0 * (1.0 - t) * t**2 * (cury + pts(i + 3)) &
                       + t**3 * (cury + pts(i + 5))

                   npl = npl + 1
                   call increasepol(npl, 1)
                   xpl(npl) = x
                   ypl(npl) = y
                end do
                curx = x
                cury = y
             end do

          case ('m', 'M') ! Move to new location
             pts = dmiss
             read (line(3:), *, end=98) pts
98           continue
             if (line(1:1) == 'M') then
                curx = pts(1)
                cury = pts(2)
             else
                curx = curx + pts(1)
                cury = cury + pts(2)
             end if
             startx = curx
             starty = cury
             npl = npl + 1
             call increasepol(npl + 1, 1)
             xpl(npl) = dmiss
             ypl(npl) = dmiss
             npl = npl + 1
             xpl(npl) = startx
             ypl(npl) = starty
             ! If more than one point was given, treat as implicit subsequent lineto commands
             do i = 3, maxpts - 1, 2
                if (pts(i) == dmiss .or. pts(i + 1) == dmiss) exit

                npl = npl + 1
                call increasepol(npl, 1)
                if (line(1:1) == 'M') then
                   curx = pts(i)
                   cury = pts(i + 1)
                else
                   curx = curx + pts(i)
                   cury = cury + pts(i + 1)
                end if
                xpl(npl) = curx
                ypl(npl) = cury
             end do
          case ('z') ! Close current subpath, return to subpath start
             npl = npl + 1
             call increasepol(npl, 1)
             xpl(npl) = startx
             ypl(npl) = starty
             curx = startx
             cury = starty
          case ('l')
             pts = dmiss
             read (line(3:), *, end=99) pts
99           continue

             do i = 1, maxpts - 1, 2
                if (pts(i) == dmiss .or. pts(i + 1) == dmiss) exit

                npl = npl + 1
                call increasepol(npl, 1)
                curx = curx + pts(i)
                cury = cury + pts(i + 1)
                xpl(npl) = curx
                ypl(npl) = cury
             end do
          case default
!            call mess('Unrecognized SVG command: '//trim(line))
          end select
       end do
999    continue

       do i = 1, npl
          ypl(i) = -ypl(i)
       end do

       call zeronet()
       KN3TYP = 1
       K1 = 0
       rcir = 1d-10 ! isnode only for 'exact' hits
       do i = 1, npl
          if (XPL(i) == dmiss) then
             K1 = 0
             cycle
          end if
          zp = 0d0
          call ISNODE(KP, XPL(i), YPL(i), zp)
          if (KP == 0) then
             call dSETNEWPOINT(XPL(i), YPL(i), KP)
          end if
          if (K1 /= 0) then
             call CONNECTDBN(K1, KP, LNU)
          end if
          K1 = KP
       end do
       KN3TYP = 2
       call savepol() ! Store pol for optional restore
       call delpol() ! Delete/hide the pol read from SVG file
       call MINMXNS() ! New world scaling
       !call wearel()
       !CALL MERGENODESINPOLYGON() ! Merge 1D nodes that are too close.

       deallocate (pts)

    end subroutine parsekerst
