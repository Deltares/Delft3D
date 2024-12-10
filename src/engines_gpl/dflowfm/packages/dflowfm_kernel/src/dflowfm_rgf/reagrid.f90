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

      !> Read a curvilinear grid to (ascii) grd-file.
      !! NOTE: reads 'old' (RGFGrid 3.x) and 'new' format (RGFGrid 4.x)
      !!
      !! Format:
      !! Start with at least one comment line, prefixed by '*', with optional keyword Spherical (old RGFGRID 3.x style)
      !! Next, zero or more key=value lines (all optional):
      !!   Coordinate System=Spherical
      !!   Missing Value=...
      !! First subsequent line without a '=' should be '0 0 0' (backwards compatibility)
      !! Next line should be mmax, nmax
      !! That ends the header, start reading coordinates in the usual fashion.
      subroutine REAgrid(Mrgf, FILNAM, ja)
         use precision, only: dp
         use m_reaweir, only: reaweir
         use m_reathd2pli, only: reathd2pli
         use m_reaobs2stat, only: reaobs2stat
         use m_reamdd, only: reamdd
         use m_reacrs
         use m_reabot
         use m_reabnd2pol
         use M_SFERIC
         use m_grid
         use M_MISSING, notinuse => xymis ! AvD: temp
         use m_netw ! vanwege zkuni
         use unstruc_model
         use M_ARCINFO
         use m_readyy
         use m_qnerror
         use m_delpol
         use m_increase_grid
         use m_read_dry2pli
         use m_read_bar2pli
         use m_isitu
         use m_ecrrea

         implicit none

         character(LEN=*) :: FILNAM
         integer :: MRGF, JA
         real(kind=dp) :: xymis

         integer :: Mbnd, mbca, mobs, mout

         character NAME2 * 76, REC * 132

         integer :: IPNT, mdep, merr, ja2, i, j, istat

         logical :: jawel, kw_found

         ja = 0
         JSFERIC = 0
         JSFERTEK = 0

         MERR = 0
         MC = 0

         xymis = 0d0 ! this is the default for this file type

         read (mrgf, '(a)', iostat=istat) rec
         if (istat > 0) goto 888
         if (istat < 0) goto 9999
         !
         ! Backwards compatible: first line could contain spherical keyword
         if (index(rec, 'Spherical') >= 1 .or. &
             index(rec, 'SPHERICAL') >= 1) then
            ! grid has spherical coordinates.
            jsferic = 1
         end if
         !
         ! looping keyword records, excluding comment lines
         !
         do
            kw_found = .false.
            read (mrgf, '(a)', iostat=istat) rec
            if (istat > 0) goto 888
            if (istat < 0) goto 9999
            if (rec(1:1) == '*') cycle
            !
            if (index(rec, 'Coordinate System') >= 1) then
               kw_found = .true.
               i = index(rec, '=') + 1
               if (index(rec(i:), 'Spherical') >= 1) then
                  ! grid has spherical coordinates, overruling previous choices.
                  jsferic = 1
               end if
            end if
            !
            if (index(rec, 'Missing Value') >= 1) then
               kw_found = .true.
               i = index(rec, '=') + 1
               read (rec(i:), *, iostat=istat) xymis
               if (istat > 0) goto 888
               if (istat < 0) goto 9999
            end if
            !
            if (.not. kw_found) then
               if (index(rec, '=') >= 1) kw_found = .true.
            end if
            !
            if (kw_found) then
               cycle ! read next record from file
            else
               exit ! record contains the dimensions
            end if
         end do
         !
         ! End loop, keywords
         !
         !
         ! First record behind the keywords contains dimension of the grid
         !
         read (rec, *, iostat=istat) mc, nc
         !
         read (mrgf, '(a)', iostat=istat) rec ! read three zero's
         if (istat > 0) goto 888
         if (istat < 0) goto 9999
         !
         !  end read header of rgf-file
         !

         call READYY('Reading Grid-File', 0d0)

         call INCREASEGRID(MC, NC)

         zc = zkuni

         call ECRREA(Xc, MMAX, NMAX, MC, NC, MRGF, 0d0)
         call ECRREA(Yc, MMAX, NMAX, MC, NC, MRGF, 0.5d0)

         ! Set to system-wide dxymiss where necessary.
         do i = 1, mc
            do j = 1, nc
               if (xc(i, j) == xymis .and. yc(i, j) == xymis) then
                  xc(i, j) = dxymis
                  yc(i, j) = dxymis
               end if
            end do
         end do

         call isitu()

         IPNT = index(FILNAM, '.') ! NOW READ *.DEP FILE, ONLY IF ORGANISED IN DEPTH POINTS
         NAME2 = FILNAM
         write (NAME2(IPNT + 1:), '(A)') 'DEP'
         inquire (FILE=NAME2, EXIST=JAWEL)
         if (JAWEL) then
            call OLDFIL(MDEP, NAME2)
            call REAMDD(MDEP, Zc, MC + 1, NC + 1, JA2)
            do i = 1, mc
               do j = 1, nc
                  if (zc(i, j) /= dmiss) then
                     zc(i, j) = -1d0 * zc(i, j)
                  end if
               end do
            end do
         end if

         write (NAME2(IPNT + 1:), '(A)') 'ASC'
         inquire (FILE=NAME2, EXIST=JAWEL)
         if (JAWEL) then
            call OLDFIL(MDEP, NAME2)
            call savepol() ! we do not want to use the selecting polygon
            call delpol()
            call REAARC(MDEP, 0)
            call restorepol()
            if (ubound(d, 1) >= MC .and. ubound(d, 2) >= NC) then
               do i = 1, mc
                  do j = 1, nc
                     zc(i, j) = d(i, j)
                  end do
               end do
            end if
         end if

         write (NAME2(IPNT + 1:), '(A)') 'bottom'
         inquire (FILE=NAME2, EXIST=JAWEL)
         if (JAWEL) then
            call OLDFIL(MDEP, NAME2)
            call REABOT(MDEP, JA2)
         end if

         write (NAME2(IPNT + 1:), '(A)') 'weirs'
         inquire (FILE=NAME2, EXIST=JAWEL)
         if (JAWEL) then
            call OLDFIL(MDEP, NAME2)
            call REAweir(MDEP, JA2)
         end if

         write (NAME2(IPNT + 1:), '(A)') 'crs'
         inquire (FILE=NAME2, EXIST=JAWEL)
         if (JAWEL) then
            call OLDFIL(MDEP, NAME2)
            call REAcrs(MDEP, JA2)
         end if

         write (NAME2(IPNT + 1:), '(A)') 'bnd' ! also read *.bnd file and create polygonfile
         inquire (FILE=NAME2, EXIST=JAWEL)
         if (JAWEL) then
            call OLDFIL(Mbnd, NAME2)

            write (NAME2(IPNT + 1:), '(A)') 'bca' ! also read *.bca file and make *.cmp files
            inquire (FILE=NAME2, EXIST=JAWEL)
            mbca = 0
            if (jawel) then
               call OLDFIL(Mbca, NAME2)
            end if

            call reabnd2pol(mbnd, mbca) ! Old, model-specific. TODO: remove or generalize

         end if

         write (NAME2(IPNT + 1:), '(A)') 'obs' ! also read *.obs file and create obsxyn
         inquire (FILE=NAME2, EXIST=JAWEL)
         if (JAWEL) then
            call OLDFIL(Mobs, NAME2)
            write (NAME2(IPNT + 1:), '(A)') '_obs.xyn'
            call newfil(mout, name2)
            call reaobs2stat(mobs, mout)
         end if

         write (NAME2(IPNT + 1:), '(A)') 'thd' ! also read *.thd file and create polygonfile
         inquire (FILE=NAME2, EXIST=JAWEL)
         if (JAWEL) then
            call OLDFIL(Mbnd, NAME2)

            write (NAME2(IPNT:), '(A)') '_thd.pli'
            call newfil(mout, name2)

            call reathd2pli(mbnd, mout)

         end if

         name2 = filnam
         write (NAME2(IPNT + 1:), '(A)') 'mnbar' ! also read *.thd file and create polygonfile
         inquire (FILE=NAME2, EXIST=JAWEL)
         if (JAWEL) then
            call OLDFIL(Mbnd, NAME2)

            write (NAME2(IPNT:), '(A)') '_bar.pli'

            call newfil(mout, name2)

            call reabar2pli(mbnd, mout)

         end if

         name2 = filnam
         write (NAME2(IPNT + 1:), '(A)') 'dry' ! also read *.thd file and create polygonfile
         inquire (FILE=NAME2, EXIST=JAWEL)
         if (JAWEL) then
            call OLDFIL(Mbnd, NAME2)

            write (NAME2(IPNT:), '(A)') '_dry.pli'

            call newfil(mout, name2)

            call readry2pli(mbnd, mout)

         end if

         call READYY(' ', -1d0)
         call DOCLOSE(MRGF)

         ! call gridtonet()

         ! call delgrd(key,1,0)

         ja = 1
         return

9999     continue
         call READYY(' ', -1d0)
         call DOCLOSE(MRGF)
         return

888      continue
         call QNERROR('Reading Error, Try UX2DOS or DOS2UX', ' ', ' ')
         call READYY(' ', -1d0)
         call DOCLOSE(MRGF)
         return

      end subroutine REAgrid
