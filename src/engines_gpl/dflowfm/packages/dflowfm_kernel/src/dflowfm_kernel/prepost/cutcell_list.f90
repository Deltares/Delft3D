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

  subroutine cutcell_list(n12, FILNAM, lenf, jamasks) ! filnam = mask

     use M_NETW
     use M_FLOWGEOM
     use m_missing
     use unstruc_messages
     use kdtree2Factory
     use m_sferic
     use m_tpoly
     use m_cutcells
     use gridoperations
     use unstruc_model
     implicit none

     integer, intent(in) :: n12, lenf !< type of operation (1, 2, 3, 4, 5), see docs below.
     character(LEN=lenf), intent(in) :: FILNAM
     integer, intent(in) :: jamasks !< store masks and polygons (1), use stored masks and polygons (2), use stored masks masks and polygons and clear masks and polygons (3), do not use stored masks and polygons at all (0)
     logical JAWEL
     double precision :: t0, t1

     integer N, MPOL, MLIST, KEY, JADEL, NN, L, K, NUMFIL, ierror
     character(LEN=132), allocatable :: FILIST(:)

     character(len=128) :: mesg

     integer, dimension(:), allocatable :: kc_bak ! backup of kc

     integer :: Lf

     integer :: ipoly
     integer :: ipol_stored
     integer :: NMAX

     integer, parameter :: jaalltogether = 1 !< all polygons at once (1) or not (0)

     jastored = 0

     inquire (FILE=md_cutcelllist, EXIST=JAWEL)
     NUMFIL = 0
     if (JAWEL) then
        call OLDFIL(MLIST, md_cutcelllist)
777     read (MLIST, *, end=888)
        NUMFIL = NUMFIL + 1
        goto 777
888     allocate (FILIST(NUMFIL)); filist = ' '
        rewind (MLIST)
        do N = 1, NUMFIL
           read (MLIST, '(A)') FILIST(N)
        end do
        call DOCLOSE(MLIST)
     else
        return
     end if

     call mess(LEVEL_INFO, 'cutcell_list; nr of *.cut files found = ', numfil, n12)

     do n = 1, numfil
        call message('cutcell ', filist(n), ' ')
     end do

! store kc
     allocate (kc_bak(numk))
     do k = 1, numk
        kc_bak(k) = kc(k)
     end do

     !IF (N12 == 3) THEN
     call SAVEPOL()
     call DELPOL()
     !ENDIF

     KC = 0 ! VOOR NU EVEN, ALLE NET NODES DOEN MEE
     if (N12 >= 3) then !prepare for cutcellwu
        if (jamasks == 0 .or. jamasks == 1) then
           call build_kdtree(treeglob, numk, xk, yk, ierror, jsferic, dmiss)
        end if
     end if

     ipol_stored = 0

     if (jaalltogether == 1 .and. (jamasks == 0 .or. jamasks == 1)) then
        call dealloc_tpoly(pli) ! safety

        do N = 1, NUMFIL
           call OLDFIL(MPOL, trim(FILIST(N)))
           call REAPOL(MPOL, 0)

           if (jsferic == 1) then
              call fix_global_polygons(1, 0)
           end if

!       add polygon to all tpoly-type polygons
           call pol_to_tpoly(numpols, pli, keepExisting=.true.)
        end do
     end if

     if (jaalltogether == 1) then
        NMAX = 1 ! all polygons stored as tpoly-type polygons
!     call realloc(idxL, 1)
!     call realloc(jdxL, 1)
!     call realloc(pdxL, 1)
!     call find_intersecting_polysections(numpols, pli, idxL, jdxL, pdxL)
     else
        NMAX = NUMFIL
     end if

     do N = 1, NMAX
        if (jaalltogether /= 1) then
!       read polygons from file
           call OLDFIL(MPOL, trim(FILIST(N)))
           call REAPOL(MPOL, 0)

           if (jsferic == 1) then
              call fix_global_polygons(1, 0)
           end if

           call pol_to_tpoly(numpols, pli, keepExisting=.false.)
        end if

        do ipoly = 1, numpols
           call klok(t0)

           ipol_stored = ipol_stored + 1
           call delpol()
           call tpoly_to_pol(pli, iselect=ipoly)

           if (jaalltogether == 1) then
              write (mesg, "('cutcells: processing polygon ', I0, ' of ', I0, '...')") ipoly, numpols
              call mess(LEVEL_INFO, trim(mesg))
           end if

           if (n12 == 1) then
              call CUTCELLS(n12)
           else if (N12 == 2) then ! DELETE NEtNODES IF INSIDE POLYGON
              call delnet(key, 0, 0)
!        else if (N12 == 3) then                ! DELETE NETCELLS IF NETCELLS ENTIRELY INSIDE POLYGONS
!           IN = -1
!           DO K = 1,NUMK
!              CALL DBPINPOL( XK(K), YK(K), IN)
!              IF (IN == 1) THEN
!                 KC(K) = 0                     ! ZIT IE IN POL DAN DOET IE NIET MEE
!              ENDIF
!           ENDDO
           else if (N12 >= 3) then ! 3, 4 and 5
              call CUTCELWU(n12, jamasks, ipol_stored) ! calls SAVEPOL via split_pol
           end if

           call klok(t1)

           write (mesg, "('done in ', F12.5, ' sec.')") t1 - t0
           call mess(LEVEL_INFO, trim(mesg))

        end do
     end do
     if (N12 >= 3) then ! cleanup after cutcellwu
        if (jamasks == 0 .or. jamasks == 1) then
           call delete_kdtree2(treeglob)
        end if
     end if

     if (N12 == 3) then
        kc = 1 - kc ! 1: active, 0: inactive
        NPL = 0
        do N = 1, NUMP
           JADEL = 1
           do NN = 1, NETCELL(N)%N
              K = NETCELL(N)%NOD(NN)
              if (KC(K) == 1) then ! ER HOEFT ER MAAR 1 OP 1 TE STAAN OF WE DELETEN NIET
                 JADEL = 0
              end if
           end do
           if (JADEL == 1) then
              do NN = 1, NETCELL(N)%N
                 L = NETCELL(N)%LIN(NN)
                 if (LNE(1, L) == N) then
                    LNE(1, L) = 0
                 elseif (LNE(2, L) == N) then
                    LNE(2, L) = 0
                 end if
              end do
           end if
        end do

        do L = 1, NUML
           if (LNE(1, L) == 0 .and. LNE(2, L) == 0) then
              KN(1, L) = 0; KN(2, L) = 0; KN(3, L) = -1
           end if
        end do

!    mark original netboundary (setnodadm will make lnn invalid)
        do L = 1, numL
           if (lnn(L) == 1 .and. kn(3, L) > 0) then
              kc(kn(1, L)) = -abs(kc(kn(1, L))) ! 0 or -1
              kc(kn(2, L)) = -abs(kc(kn(2, L))) ! 0 or -1
           end if
        end do

        call SETNODADM(0)

!    output newly created cells that are no cells as polygons
        call write_illegal_cells_to_pol(1)

!    clean
        call dealloc_tpoly(pli)
     end if

     if (n12 == 5) then
!    SPvdP: disable flow-links that are associated to disabled net-links
        do Lf = 1, Lnx
           if (kcu(Lf) /= 2) cycle
           L = iabs(ln2lne(Lf))
           if (L > 0) then
              if (lnn(L) == 0) then
                 wu(Lf) = 0d0
              end if
           end if
        end do
     end if

     if (n12 == 6) then
        call realloc(cellmask, nump, fill=1, keepExisting=.false.)
!    disable cells with only "lnn<0" links
        do L = 1, numL
           if (kn(3, L) /= 2) cycle
           if (lnn(L) > 0) then
!          unmask neighboring cell(s)
              cellmask(lne(1, L)) = 0
              if (lnn(L) > 1) cellmask(lne(2, L)) = 0
           else if (lnn(L) < 0) then
!          reset lnn
              lnn(L) = -lnn(L)
           end if
        end do
        call remove_masked_netcells()
        if (allocated(cellmask)) deallocate (cellmask)
     end if

     ! call restorepol() ! initial SAVEPOL no longer valid due to CUTCELWU call
     call DELPOL() ! don't keep the cutcell polygons since they will clip the bed levels

     if (jaalltogether /= 1) then
        call dealloc_tpoly(pli)
     end if

! restore kc
     if (allocated(kc_bak)) then
        do k = 1, numk
           kc(k) = kc_bak(k)
        end do
        deallocate (kc_bak)
     end if

     if (jamasks == 3) then
        call dealloc_cutcellmasks()
        call dealloc_tpoly(pli)
     end if

     deallocate (filist)

     return

  contains

!>   determine for each netlink if it is intersected by the polygon
     subroutine find_intersecting_polysections(numpols, pli, idxL, jdxL, pdxL)
        use network_data
!        use m_polygon
        use kdtree2Factory
        use m_alloc
        use m_missing
        use m_tpoly
        implicit none

        integer, intent(in) :: numpols !< number of tpoly-type polygons
        type(tpoly), dimension(numpols), intent(in) :: pli !< tpoly-type polygons

        integer, dimension(:), allocatable, intent(out) :: idxL, jdxL ! intersecting polygon sections per netlink in CRS
        integer, dimension(:), allocatable, intent(out) :: pdxL ! intersecting polygon numbers  per netlink in CRS

        type(kdtree_instance) :: kdtree

        double precision, dimension(:), allocatable :: x, y

        double precision, dimension(:), allocatable :: dsL
        integer, dimension(:), allocatable :: iLink, iPol
        integer, dimension(:), allocatable :: numcrossed
        integer, dimension(:), allocatable :: polynum
        integer, dimension(:), allocatable :: polysec

        integer :: numcrossedlinks
        integer :: i, j, L, num
        integer :: ierror
        double precision :: t0, t1

!       count total number of polygon nodes, including missing and closures
        num = numpols - 1 ! missing values as seperators
        do i = 1, numpols
           num = num + pli(i)%len + 1 ! 1: closure
        end do

!       allocate
        allocate (iLink(numL))
        allocate (iPol(numL))
        allocate (dSL(numL))
        allocate (x(num))
        allocate (y(num))
        allocate (polynum(num))
        allocate (polysec(num))

        call klok(t0)
        num = 0
        do i = 1, numpols
!          copy i-the tpoly-type polygon
           do j = 1, pli(i)%len
              num = num + 1
              x(num) = pli(i)%x(j)
              y(num) = pli(i)%y(j)
!             add identifier
              polynum(num) = i
              polysec(num) = j
           end do
!          add closure
           num = num + 1
           x(num) = pli(i)%x(1)
           y(num) = pli(i)%y(1)
!          add identifier
           polynum(num) = i
           polysec(num) = pli(i)%len + 1

           if (i < numpols) then
!             add seperator
              x(num) = DMISS
              y(num) = DMISS
!             add identifier
              polynum(num) = 0
              polysec(num) = 0
           end if
        end do

!       find crossed links
        call find_crossed_links_kdtree2(kdtree, num, x, y, 3, numL, 1, numcrossedlinks, iLink, iPol, dsL, ierror)
        deallocate (x, y)
        if (ierror /= 0) goto 1234

!       (re)alloc
        call realloc(idxL, numL + 1, keepExisting=.false., fill=0)
        call realloc(jdxL, numcrossedlinks + 1, keepExisting=.false., fill=0)
        call realloc(pdxL, numcrossedlinks + 1, keepExisting=.false., fill=0)

!       count number of intersections per netlink
        allocate (numcrossed(numL))
        numcrossed = 0
        do i = 1, numcrossedlinks
           L = iLink(i)
           numcrossed(L) = numcrossed(L) + 1
        end do

!       construct CRS of polygon sections that cross the links
        idxL(1) = 1
        do L = 1, numL
           idxL(L + 1) = idxL(L) + numcrossed(L)
        end do

        numcrossed = 0
        do i = 1, numcrossedlinks
           L = iLink(i)
           num = idxL(L) + numcrossed(L)
           j = iPol(i)
           jdxL(num) = polysec(j)
           pdxL(num) = polynum(j)
           numcrossed(L) = numcrossed(L) + 1
        end do

1234    continue

        call klok(t1)
        write (mesg, "('cutcell with kdtree2, elapsed time: ', G15.5, 's.')") t1 - t0
        call mess(LEVEL_INFO, trim(mesg))
!       deallocate
        if (allocated(iLink)) deallocate (iLink)
        if (allocated(iPol)) deallocate (iPol)
        if (allocated(dsL)) deallocate (dsL)
        if (allocated(numcrossed)) deallocate (numcrossed)
        if (allocated(polynum)) deallocate (polynum)
        if (allocated(polysec)) deallocate (polysec)

        return
     end subroutine find_intersecting_polysections

  end subroutine cutcell_list
