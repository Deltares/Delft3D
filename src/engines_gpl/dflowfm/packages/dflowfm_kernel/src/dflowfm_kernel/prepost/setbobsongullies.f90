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

 subroutine setbobsongullies() ! override bobs along pliz's, jadykes == 0: only heights, 1 = also dyke attributes
    use m_netw
    use m_flowgeom
    use m_flow
    use m_missing
    use unstruc_model
    use unstruc_messages
    use kdtree2Factory
    use m_sferic
    use m_polygon
    use geometry_module, only: crossinbox, dbdistance
    use m_readyy
    use m_wall_clock_time
    use m_reapol
    use m_find_crossed_links_kdtree2

    implicit none

    integer :: i, k, L, n1, n2, nt, minp, lastfoundk, kL, kint, kf, jacros
    integer :: iL, numLL, numcrossedLinks, ierror, jakdtree = 1, ja2pt
    double precision :: SL, SM, XCR, YCR, CRP, Xa, Ya, Xb, Yb, zc, af, width
    double precision, allocatable :: dSL(:)
    integer, allocatable :: iLink(:), iPol(:)
    double precision :: t0, t1
    character(len=128) :: mesg

    if (len_trim(md_gulliesfile) == 0) then
       return
    end if

    call readyy('Setbobsongullies', 0d0)

    call oldfil(minp, md_gulliesfile)
    call reapol(minp, 0)

    if (jakdtree == 1) then
       call wall_clock_time(t0)
       allocate (iLink(Lnx), ipol(Lnx), dSL(Lnx))
       call find_crossed_links_kdtree2(treeglob, NPL, XPL, YPL, 2, numL, 0, numcrossedLinks, iLink, iPol, dSL, ierror)
       numLL = numcrossedLinks
       if (ierror /= 0) then !   check if kdtree was succesfull, disable if not so
          deallocate (iLink, ipoL, dSL)
          jakdtree = 0
       end if
       call wall_clock_time(t1)
       write (mesg, "('set bobs (on gullies) with kdtree2, elapsed time: ', G15.5, 's.')") t1 - t0
       call mess(LEVEL_INFO, trim(mesg))
    else
       numLL = Lnxi
    end if

    kint = max(numLL / 100, 1); nt = 0
    do iL = 1, numLL

       jacros = 0
       if (jakdtree == 0) then
          L = iL
       else
          L = ilink(iL)
          ! L = lne2ln( iLink(iL) )
          if (L <= 0) cycle
          k = iPol(iL)
       end if

       if (mod(iL, kint) == 0) then
          AF = dble(iL) / dble(numLL)
          call readyy('Setbobsongullies', af)
       end if

       n1 = ln(1, L); n2 = ln(2, L)
       if (jakdtree == 0) then

          xa = xz(n1); ya = yz(n1)
          xb = xz(n2); yb = yz(n2)

          iloop: do i = 1, 2

             if (i == 1) then
                if (Lastfoundk == 0) cycle
                kf = max(1, Lastfoundk - 100)
                kL = min(npl - 1, Lastfoundk + 100)
             else
                kf = 1
                kL = npl - 1
             end if

             Lastfoundk = 0
             do k = kf, kL

                if (xpl(k) /= dmiss .and. xpl(k + 1) /= dmiss) then
                   call CROSSinbox(XPL(k), YPL(k), XPL(k + 1), YPL(k + 1), Xa, Ya, Xb, Yb, jacros, SL, SM, XCR, YCR, CRP, jsferic, dmiss)

                   if (jacros == 1) then
                      Lastfoundk = k
                      exit iloop
                   end if
                end if
             end do

          end do iloop
       else !       use kdtree to find nearest dike
          k = iPol(iL)
          jacros = 1
          sL = dSL(iL)
       end if

       if (jacros == 1) then !        dig the gullies
          zc = sl * zpL(k + 1) + (1d0 - sl) * zpL(k)
          bob(1, L) = min(zc, bob(1, L), bob(2, L)); bob(2, L) = bob(1, L)
          bob0(:, L) = bob(:, L)

          bl(n1) = min(bl(n1), zc)
          bl(n2) = min(bl(n2), zc)
          nt = nt + 1

          ja2pt = 0
          if (npl == 2) then
             ja2pt = 1
          else if (k == 1 .and. xpl(3) == dmiss) then
             ja2pt = 1
          else if (xpl(k - 1) == dmiss .and. xpl(k + 2) == dmiss) then
             ja2pt = 1
          end if

          if (ja2pt == 1) then
             width = dbdistance(xpl(k), ypl(k), xpl(k + 1), ypl(k + 1), jsferic, jasfer3D, dmiss)
             wu(L) = min(wu(L), width)
          end if

       end if

    end do

    if (nt > 0) then
       call mess(LEVEL_INFO, 'Number of flow Links with lowered gullies :: ', nt)
    end if

    call readyy(' ', -1d0)

1234 continue

! deallocate
    if (jakdtree == 1) then
       if (allocated(iLink)) deallocate (iLink)
       if (allocated(iPol)) deallocate (iPol)
       if (allocated(dSL)) deallocate (dSL)
    end if

 end subroutine setbobsongullies
