!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2025.
!
!  This library is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation version 2.1.
!
!  This library is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
!  You should have received a copy of the GNU Lesser General Public
!  License along with this library; if not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------
!
!

module m_slow_nefis_tests
   use assertions_gtest
   use tests_nefis_helper
   implicit none

   character(len=30), dimension(3) :: skiplines = ["Version", "version", "-----"]

contains

   !$f90tw TESTCODE(TEST, slow_nefis_tests, test_06, test_06,
   subroutine test_06() bind(C)
      character(len=30) :: filename1
      character(len=30) :: filename2
      character(len=30) :: nefis_filename_def
      character(len=30) :: nefis_filename_dat
      integer file_unit
      integer START, stop, INCR
      parameter(START=1, stop=2, INCR=3)
      integer * 4 fds
      integer clsdat,&
      &clsdef,&
      &credat,&
      &defelm,&
      &defcel,&
      &defgrp,&
      &getelt,&
      &getnfv,&
      &neferr,&
      &reserr
      integer opndat,&
      &opndef,&
      &putelt
      integer error,&
      &i, j, k, l, m, n, im,&
      &grpdms(5),&
      &grpord(5),&
      &usrord(5),&
      &UINDEX(3, 5)
      real buffer(26880)
      character names(2) * 14, coding * 1
      character ERRSTR * 1024
      real cpu1, cpu2, elap_w, elap_r
      character * 255 version

      ! delete previous output files if they exist
      filename1 = 'test06.out'
      filename2 = 'test06.scr'
      nefis_filename_def = 'nefis_ex6.def'
      nefis_filename_dat = 'nefis_ex6.dat'

      call delete_file(filename1)
      call delete_file(nefis_filename_def)
      call delete_file(nefis_filename_dat)
      open (newunit=file_unit, file=filename1)

      error = reserr()
      error = getnfv(version)
      write (file_unit, *) '-----------------------------------------------'
      write (file_unit, '(a)') 'Version: '//trim(version(5:))
      write (file_unit, *) '-----------------------------------------------'

      call clock(cpu1)
      coding = ' '
      error = Opndef(fds, nefis_filename_def, coding)
      if (error /= 0) goto 9999

      error = Opndat(fds, nefis_filename_dat, coding)
      if (error /= 0) goto 9999

      error = Defelm(fds, 'ELEM_R_4', 'REAL', 4,&
      &'GROOTHEID 1', 'eenheid 1', 'Beschrijving 1',&
      &0, 0)
      if (error /= 0) goto 9999

      error = Defelm(fds, 'ELEM_R_4_DIM_1', 'REAL', 4,&
      &'GROOTHEID 2', 'eenheid 2', 'Beschrijving 2',&
      &1, 3)
      if (error /= 0) goto 9999

      names(1) = 'ELEM_R_4_DIM_1'
      names(2) = 'ELEM_R_4'
      error = Defcel(fds, 'CEL_TEST_3', 2, names)
      if (error /= 0) goto 9999

      grpdms(1) = 4
      grpdms(2) = 5
      grpdms(3) = 6
      grpdms(4) = 7
      grpdms(5) = 8

      grpord(1) = 1
      grpord(2) = 2
      grpord(3) = 3
      grpord(4) = 4
      grpord(5) = 5
      error = Defgrp(fds, 'GRP_TEST_3A', 'CEL_TEST_3', 5,&
      &grpdms, grpord)
      if (error /= 0) goto 9999

      grpord(1) = 5
      grpord(2) = 4
      grpord(3) = 3
      grpord(4) = 2
      grpord(5) = 1
      error = Defgrp(fds, 'GRP_TEST_3B', 'CEL_TEST_3', 5,&
      &grpdms, grpord)
      if (error /= 0) goto 9999

      grpord(1) = 5
      grpord(2) = 3
      grpord(3) = 1
      grpord(4) = 2
      grpord(5) = 4
      error = Defgrp(fds, 'GRP_TEST_3C', 'CEL_TEST_3', 5,&
      &grpdms, grpord)
      if (error /= 0) goto 9999

      grpdms(1) = 0

      grpord(1) = 5
      grpord(2) = 4
      grpord(3) = 3
      grpord(4) = 2
      grpord(5) = 1

      error = Defgrp(fds, 'GRP_TEST_3D', 'CEL_TEST_3', 5,&
      &grpdms, grpord)
      if (error /= 0) goto 9999
!---------------------------------------------------------------------
      error = Credat(fds, 'DATAGRP_TEST_3A', 'GRP_TEST_3A')
      if (error /= 0) goto 9999

      error = Credat(fds, 'DATAGRP_TEST_3B', 'GRP_TEST_3B')
      if (error /= 0) goto 9999

      error = Credat(fds, 'DATAGRP_TEST_3C', 'GRP_TEST_3C')
      if (error /= 0) goto 9999

      error = Credat(fds, 'DATAGRP_TEST_3D', 'GRP_TEST_3D')
      if (error /= 0) goto 9999
!---------------------------------------------------------------------
      call clock(cpu2)
      write (file_unit, '(''Initialisation NEFIS files [sec]'',1PE13.5)')&
      &cpu2 - cpu1

      UINDEX(incr, 1) = 1
      UINDEX(incr, 2) = 1
      UINDEX(incr, 3) = 1
      UINDEX(incr, 4) = 1
      UINDEX(incr, 5) = 1
      usrord(1) = 1
      usrord(2) = 2
      usrord(3) = 3
      usrord(4) = 4
      usrord(5) = 5

      call clock(cpu1)
      write (file_unit, *)
      write (file_unit, '(&
      &  ''6720 schrijfopdrachten (6720 cellen) van 16 bytes elk'')')
      do i = 1, 8
         UINDEX(start, 5) = I
         UINDEX(stop, 5) = I
         do j = 1, 7
            UINDEX(start, 4) = J
            UINDEX(stop, 4) = J
            do k = 1, 6
               UINDEX(start, 3) = K
               UINDEX(stop, 3) = K
               do l = 1, 5
                  UINDEX(start, 2) = L
                  UINDEX(stop, 2) = L
                  do m = 1, 4
                     UINDEX(start, 1) = M
                     UINDEX(stop, 1) = M
                     do n = 1, 4
                        buffer(n) = real(i) * real(j) * real(k) * real(l) *&
                        &real(m) * real(n)
                     end do
                     error = Putelt(fds, 'DATAGRP_TEST_3A',&
                     &'*', UINDEX, usrord, buffer)
                     if (error /= 0) goto 9999
                  end do
               end do
            end do
         end do
      end do
      call clock(cpu2)
      elap_w = cpu2 - cpu1
      write (file_unit, '(''DATAGRP_TEST_3A'',&
      &          '' written in [sec]'',1PE13.5)') cpu2 - cpu1

      write (file_unit, '(&
      &  ''6720 schrijfopdrachten (6720 cellen) van 16 bytes elk'')')
      call clock(cpu1)
      do i = 1, 8
         UINDEX(start, 5) = I
         UINDEX(stop, 5) = I
         do j = 1, 7
            UINDEX(start, 4) = J
            UINDEX(stop, 4) = J
            do k = 1, 6
               UINDEX(start, 3) = K
               UINDEX(stop, 3) = K
               do l = 1, 5
                  UINDEX(start, 2) = L
                  UINDEX(stop, 2) = L
                  do m = 1, 4
                     UINDEX(start, 1) = M
                     UINDEX(stop, 1) = M
                     do n = 1, 4
                        buffer(n) = real(i) * real(j) * real(k) * real(l) *&
                        &real(m) * real(n) * 2.
                     end do
                     error = Putelt(fds, 'DATAGRP_TEST_3B',&
                     &'*', UINDEX, usrord, buffer)
                     if (error /= 0) goto 9999
                  end do
               end do
            end do
         end do
      end do
      call clock(cpu2)
      elap_w = elap_w + cpu2 - cpu1
      write (file_unit, '(''DATAGRP_TEST_3B'',&
      &          '' written in [sec]'',1PE13.5)') cpu2 - cpu1

      write (file_unit, '(&
      &  ''6720 schrijfopdrachten (6720 cellen) van 16 bytes elk'')')
      call clock(cpu1)
      do i = 1, 8
         UINDEX(start, 5) = I
         UINDEX(stop, 5) = I
         do j = 1, 7
            UINDEX(start, 4) = J
            UINDEX(stop, 4) = J
            do k = 1, 6
               UINDEX(start, 3) = K
               UINDEX(stop, 3) = K
               do l = 1, 5
                  UINDEX(start, 2) = L
                  UINDEX(stop, 2) = L
                  do m = 1, 4
                     UINDEX(start, 1) = M
                     UINDEX(stop, 1) = M
                     do n = 1, 4
                        buffer(n) = real(i) * real(j) * real(k) * real(l) *&
                        &real(m) * real(n) * 3.
                     end do
                     error = Putelt(fds, 'DATAGRP_TEST_3C',&
                     &'*', UINDEX, usrord, buffer)
                     if (error /= 0) goto 9999
                  end do
               end do
            end do
         end do
      end do
      call clock(cpu2)
      elap_w = elap_w + cpu2 - cpu1
      write (file_unit, '(''DATAGRP_TEST_3C'',&
      &          '' written in [sec]'',1PE13.5)') cpu2 - cpu1

      write (file_unit, '(&
      &  ''6720 schrijfopdrachten (6720 cellen) van 16 bytes elk'')')
      call clock(cpu1)
      do i = 1, 8
         UINDEX(start, 5) = I
         UINDEX(stop, 5) = I
         do j = 1, 7
            UINDEX(start, 4) = J
            UINDEX(stop, 4) = J
            do k = 1, 6
               UINDEX(start, 3) = K
               UINDEX(stop, 3) = K
               do l = 1, 5
                  UINDEX(start, 2) = L
                  UINDEX(stop, 2) = L
                  do m = 1, 4
                     UINDEX(start, 1) = M
                     UINDEX(stop, 1) = M
                     do n = 1, 4
                        buffer(n) = real(i) * real(j) * real(k) * real(l) *&
                        &real(m) * real(n) * 2.
                     end do
                     error = Putelt(fds, 'DATAGRP_TEST_3D',&
                     &'*', UINDEX, usrord, buffer)
                     if (error /= 0) goto 9999
                  end do
               end do
            end do
         end do
      end do
      call clock(cpu2)
      elap_w = elap_w + cpu2 - cpu1
      write (file_unit, '(''DATAGRP_TEST_3D'',&
      &          '' written in [sec]'',1PE13.5)') cpu2 - cpu1

      do i = 1, 26880
         buffer(i) = 0.0
      end do

      write (file_unit, *)
      write (file_unit, '(&
      &  ''6720 leesopdrachten (6720 cellen) van 16 bytes elk'')')
      call clock(cpu1)
      do i = 1, 4
         UINDEX(start, 1) = I
         UINDEX(stop, 1) = I
         do j = 1, 5
            UINDEX(start, 2) = J
            UINDEX(stop, 2) = J
            do k = 1, 6
               UINDEX(start, 3) = K
               UINDEX(stop, 3) = K
               do l = 1, 7
                  UINDEX(start, 4) = L
                  UINDEX(stop, 4) = L
                  do m = 1, 8
                     UINDEX(start, 5) = M
                     UINDEX(stop, 5) = M
                     error = Getelt(fds, 'DATAGRP_TEST_3A',&
                     &'*', UINDEX, usrord, 4 * 4, buffer)
                     !call f90_assert_eq(to_c(error), to_c(0), 'error')
                     if (error /= 0) goto 9999
                     do n = 1, 4
                        if (int(buffer(n) / real(i) / real(j) / real(k) / real(l) /&
                        &real(m) - n) /= 0) print *, 'error, i= ', i
                     end do
                  end do
               end do
            end do
         end do
      end do
      call clock(cpu2)
      elap_r = cpu2 - cpu1
      write (file_unit, '(''DATAGRP_TEST_3A'',&
      &          '' read in [sec]'',1PE13.5)') cpu2 - cpu1

      write (file_unit, '(&
      &  ''6720 leesopdrachten (6720 cellen) van 16 bytes elk'')')
      call clock(cpu1)
      do i = 1, 4
         UINDEX(start, 1) = I
         UINDEX(stop, 1) = I
         do j = 1, 5
            UINDEX(start, 2) = J
            UINDEX(stop, 2) = J
            do k = 1, 6
               UINDEX(start, 3) = K
               UINDEX(stop, 3) = K
               do l = 1, 7
                  UINDEX(start, 4) = L
                  UINDEX(stop, 4) = L
                  do m = 1, 8
                     UINDEX(start, 5) = M
                     UINDEX(stop, 5) = M
                     error = Getelt(fds, 'DATAGRP_TEST_3B',&
                     &'*', UINDEX, usrord, 4 * 4, buffer)
                     if (error /= 0) goto 9999
                     do n = 1, 4
                        if (int(buffer(n) / real(i) / real(j) / real(k) / real(l) /&
                        &real(m) - 2 * n) /= 0) print *, 'error, i= ', i
                     end do
                  end do
               end do
            end do
         end do
      end do
      call clock(cpu2)
      elap_r = elap_r + cpu2 - cpu1
      write (file_unit, '(''DATAGRP_TEST_3B'',&
      &          '' read in [sec]'',1PE13.5)') cpu2 - cpu1

      write (file_unit, '(&
      &  ''6720 leesopdrachten (6720 cellen) van 16 bytes elk'')')
      call clock(cpu1)
      do i = 1, 4
         UINDEX(start, 1) = I
         UINDEX(stop, 1) = I
         do j = 1, 5
            UINDEX(start, 2) = J
            UINDEX(stop, 2) = J
            do k = 1, 6
               UINDEX(start, 3) = K
               UINDEX(stop, 3) = K
               do l = 1, 7
                  UINDEX(start, 4) = L
                  UINDEX(stop, 4) = L
                  do m = 1, 8
                     UINDEX(start, 5) = M
                     UINDEX(stop, 5) = M
                     error = Getelt(fds, 'DATAGRP_TEST_3C',&
                     &'*', UINDEX, usrord, 4 * 4, buffer)
                     if (error /= 0) goto 9999
                     do n = 1, 4
                        if (int(buffer(n) / real(i) / real(j) / real(k) / real(l) /&
                        &real(m) - 3 * n) /= 0) print *, 'error, i= ', i
                     end do
                  end do
               end do
            end do
         end do
      end do
      call clock(cpu2)
      elap_r = elap_r + cpu2 - cpu1
      write (file_unit, '(''DATAGRP_TEST_3C'',&
      &          '' read in [sec]'',1PE13.5)') cpu2 - cpu1

      write (file_unit, '(&
      &  ''6720 leesopdrachten (6720 cellen) van 16 bytes elk'')')
      call clock(cpu1)
      do i = 1, 4
         UINDEX(start, 1) = I
         UINDEX(stop, 1) = I
         do j = 1, 5
            UINDEX(start, 2) = J
            UINDEX(stop, 2) = J
            do k = 1, 6
               UINDEX(start, 3) = K
               UINDEX(stop, 3) = K
               do l = 1, 7
                  UINDEX(start, 4) = L
                  UINDEX(stop, 4) = L
                  do m = 1, 8
                     UINDEX(start, 5) = M
                     UINDEX(stop, 5) = M
                     error = Getelt(fds, 'DATAGRP_TEST_3D',&
                     &'*', UINDEX, usrord, 4 * 4, buffer)
                     if (error /= 0) goto 9999
                     do n = 1, 4
                        if (int(buffer(n) / real(i) / real(j) / real(k) / real(l) /&
                        &real(m) - 2 * n) /= 0) print *, 'error, i= ', i
                     end do
                  end do
               end do
            end do
         end do
      end do
      call clock(cpu2)
      elap_r = elap_r + cpu2 - cpu1
      write (file_unit, '(''DATAGRP_TEST_3D'',&
      &          '' read in [sec]'',1PE13.5)') cpu2 - cpu1

      usrord(1) = 5
      usrord(2) = 4
      usrord(3) = 3
      usrord(4) = 2
      usrord(5) = 1
      UINDEX(start, 1) = 1
      UINDEX(start, 2) = 1
      UINDEX(start, 3) = 1
      UINDEX(start, 4) = 1
      UINDEX(start, 5) = 1
      UINDEX(stop, 1) = 8
      UINDEX(stop, 2) = 7
      UINDEX(stop, 3) = 6
      UINDEX(stop, 4) = 5
      UINDEX(stop, 5) = 4

      write (file_unit, *)
      write (file_unit, '(&
      &  ''1 schrijfopdracht van 6720 elementen van 4 bytes'')')
      call clock(cpu1)
      n = 0
      do i = 1, 4
         do j = 1, 5
            do k = 1, 6
               do l = 1, 7
                  do m = 1, 8
                     n = n + 1
                     buffer(n) = real(i) * real(j) * real(k) * real(l) * real(m)
                  end do
               end do
            end do
         end do
      end do
      error = Putelt(fds, 'DATAGRP_TEST_3A',&
      &'ELEM_R_4', UINDEX, usrord, buffer)
      if (error /= 0) goto 9999
      call clock(cpu2)
      elap_w = elap_w + cpu2 - cpu1
      write (file_unit, '(''DATAGRP_TEST_3A'',&
      &          '' written in [sec]'',1PE13.5)') cpu2 - cpu1

      write (file_unit, '(&
      &  ''1 schrijfopdracht van 6720 elementen van 4 bytes'')')
      call clock(cpu1)
      n = 0
      do i = 1, 4
         do j = 1, 5
            do k = 1, 6
               do l = 1, 7
                  do m = 1, 8
                     n = n + 1
                     buffer(n) = real(i) * real(j) * real(k) * real(l) * real(m) * 2.
                  end do
               end do
            end do
         end do
      end do
      error = Putelt(fds, 'DATAGRP_TEST_3B',&
      &'ELEM_R_4', UINDEX, usrord, buffer)
      if (error /= 0) goto 9999
      call clock(cpu2)
      elap_w = elap_w + cpu2 - cpu1
      write (file_unit, '(''DATAGRP_TEST_3B'',&
      &          '' written in [sec]'',1PE13.5)') cpu2 - cpu1

      write (file_unit, '(&
      &  ''1 schrijfopdracht van 6720 elementen van 4 bytes'')')
      call clock(cpu1)
      n = 0
      do i = 1, 4
         do j = 1, 5
            do k = 1, 6
               do l = 1, 7
                  do m = 1, 8
                     n = n + 1
                     buffer(n) = real(i) * real(j) * real(k) * real(l) * real(m) * 3.
                  end do
               end do
            end do
         end do
      end do
      error = Putelt(fds, 'DATAGRP_TEST_3C',&
      &'ELEM_R_4', UINDEX, usrord, buffer)
      if (error /= 0) goto 9999
      call clock(cpu2)
      elap_w = elap_w + cpu2 - cpu1
      write (file_unit, '(''DATAGRP_TEST_3C'',&
      &          '' written in [sec]'',1PE13.5)') cpu2 - cpu1

      write (file_unit, '(&
      &  ''1 schrijfopdracht van 6720 elementen van 4 bytes'')')
      call clock(cpu1)
      n = 0
      do i = 1, 4
         do j = 1, 5
            do k = 1, 6
               do l = 1, 7
                  do m = 1, 8
                     n = n + 1
                     buffer(n) = real(i) * real(j) * real(k) * real(l) * real(m) * 4.
                  end do
               end do
            end do
         end do
      end do
      error = Putelt(fds, 'DATAGRP_TEST_3D',&
      &'ELEM_R_4', UINDEX, usrord, buffer)
      if (error /= 0) goto 9999
      call clock(cpu2)
      elap_w = elap_w + cpu2 - cpu1
      write (file_unit, '(''DATAGRP_TEST_3D'',&
      &          '' written in [sec]'',1PE13.5)') cpu2 - cpu1

      usrord(1) = 1
      usrord(2) = 2
      usrord(3) = 3
      usrord(4) = 4
      usrord(5) = 5
      UINDEX(stop, 1) = 4
      UINDEX(stop, 2) = 5
      UINDEX(stop, 3) = 6
      UINDEX(stop, 4) = 7
      UINDEX(stop, 5) = 8

      write (file_unit, *)
      write (file_unit, '(&
      &  ''1 schrijfopdracht van 6720 elementen van 12 bytes'')')
      call clock(cpu1)
      n = 0
      do i = 1, 8
         do j = 1, 7
            do k = 1, 6
               do l = 1, 5
                  do m = 1, 4
                     do im = 1, 3
                        n = n + 1
                        buffer(n) = real(i) * real(j) * real(k) * real(l) * real(m) *&
                        &real(im)
                     end do
                  end do
               end do
            end do
         end do
      end do
      error = Putelt(fds, 'DATAGRP_TEST_3A',&
      &'ELEM_R_4_DIM_1', UINDEX, usrord, buffer)
      if (error /= 0) goto 9999
      call clock(cpu2)
      elap_w = elap_w + cpu2 - cpu1
      write (file_unit, '(''DATAGRP_TEST_3A'',&
      &          '' written in [sec]'',1PE13.5)') cpu2 - cpu1

      write (file_unit, '(&
      &  ''1 schrijfopdracht van 6720 elementen van 12 bytes'')')
      call clock(cpu1)
      n = 0
      do i = 1, 8
         do j = 1, 7
            do k = 1, 6
               do l = 1, 5
                  do m = 1, 4
                     do im = 1, 3
                        n = n + 1
                        buffer(n) = real(i) * real(j) * real(k) * real(l) * real(m) *&
                        &real(im) * 2.
                     end do
                  end do
               end do
            end do
         end do
      end do
      error = Putelt(fds, 'DATAGRP_TEST_3B',&
      &'ELEM_R_4_DIM_1', UINDEX, usrord, buffer)
      if (error /= 0) goto 9999
      call clock(cpu2)
      elap_w = elap_w + cpu2 - cpu1
      write (file_unit, '(''DATAGRP_TEST_3B'',&
      &          '' written in [sec]'',1PE13.5)') cpu2 - cpu1

      write (file_unit, '(&
      &  ''1 schrijfopdracht van 6720 elementen van 12 bytes'')')
      call clock(cpu1)
      n = 0
      do i = 1, 8
         do j = 1, 7
            do k = 1, 6
               do l = 1, 5
                  do m = 1, 4
                     do im = 1, 3
                        n = n + 1
                        buffer(n) = real(i) * real(j) * real(k) * real(l) * real(m) *&
                        &real(im) * 3.
                     end do
                  end do
               end do
            end do
         end do
      end do
      error = Putelt(fds, 'DATAGRP_TEST_3C',&
      &'ELEM_R_4_DIM_1', UINDEX, usrord, buffer)
      if (error /= 0) goto 9999
      call clock(cpu2)
      elap_w = elap_w + cpu2 - cpu1
      write (file_unit, '(''DATAGRP_TEST_3C'',&
      &          '' written in [sec]'',1PE13.5)') cpu2 - cpu1

      write (file_unit, '(&
      &  ''1 schrijfopdracht van 6720 elementen van 12 bytes'')')
      call clock(cpu1)
      n = 0
      do i = 1, 8
         do j = 1, 7
            do k = 1, 6
               do l = 1, 5
                  do m = 1, 4
                     do im = 1, 3
                        n = n + 1
                        buffer(n) = real(i) * real(j) * real(k) * real(l) * real(m) *&
                        &real(im) * 4.
                     end do
                  end do
               end do
            end do
         end do
      end do
      error = Putelt(fds, 'DATAGRP_TEST_3D',&
      &'ELEM_R_4_DIM_1', UINDEX, usrord, buffer)
      if (error /= 0) goto 9999
      call clock(cpu2)
      elap_w = elap_w + cpu2 - cpu1
      write (file_unit, '(''DATAGRP_TEST_3D'',&
      &          '' written in [sec]'',1PE13.5)') cpu2 - cpu1

      write (file_unit, *)
      write (file_unit, '(''Lees een cel van 16 bytes'')')
      call clock(cpu1)
      UINDEX(start, 1) = 2
      UINDEX(start, 2) = 2
      UINDEX(start, 3) = 2
      UINDEX(start, 4) = 2
      UINDEX(start, 5) = 2
      UINDEX(stop, 1) = 2
      UINDEX(stop, 2) = 2
      UINDEX(stop, 3) = 2
      UINDEX(stop, 4) = 2
      UINDEX(stop, 5) = 2
      error = Getelt(fds, 'DATAGRP_TEST_3A',&
      &'*', UINDEX, usrord, 26880 * 4, buffer)
      if (error /= 0) goto 9999
      do i = 1, 3
         if (int(buffer(i) - (2 * 2 * 2 * 2 * 2 * i)) /= 0) print *, 'error, i= ', i
      end do
      if (int(buffer(4) - (2 * 2 * 2 * 2 * 2)) /= 0) print *, 'error, i= ', i
      call clock(cpu2)
      elap_r = elap_r + cpu2 - cpu1
      write (file_unit, '(''DATAGRP_TEST_3A'',&
      &          '' read in [sec]'',1PE13.5)') cpu2 - cpu1

      write (file_unit, '(''Lees een cel van 16 bytes'')')
      call clock(cpu1)
      UINDEX(start, 1) = 3
      UINDEX(start, 2) = 3
      UINDEX(start, 3) = 3
      UINDEX(start, 4) = 3
      UINDEX(start, 5) = 3
      UINDEX(stop, 1) = 3
      UINDEX(stop, 2) = 3
      UINDEX(stop, 3) = 3
      UINDEX(stop, 4) = 3
      UINDEX(stop, 5) = 3
      error = Getelt(fds, 'DATAGRP_TEST_3B',&
      &'*', UINDEX, usrord, 26880 * 4, buffer)
      if (error /= 0) goto 9999
      do i = 1, 3
         if (int(buffer(i) - (3 * 3 * 3 * 3 * 3 * 2 * i)) /= 0) print *, 'error, i= ', i
      end do
      if (int(buffer(4) - (3 * 3 * 3 * 3 * 3 * 2)) /= 0) print *, 'error, i= ', i
      call clock(cpu2)
      elap_r = elap_r + cpu2 - cpu1
      write (file_unit, '(''DATAGRP_TEST_3B'',&
      &          '' read in [sec]'',1PE13.5)') cpu2 - cpu1

      write (file_unit, '(''Lees een cel van 16 bytes'')')
      call clock(cpu1)
      UINDEX(start, 1) = 4
      UINDEX(start, 2) = 4
      UINDEX(start, 3) = 4
      UINDEX(start, 4) = 4
      UINDEX(start, 5) = 4
      UINDEX(stop, 1) = 4
      UINDEX(stop, 2) = 4
      UINDEX(stop, 3) = 4
      UINDEX(stop, 4) = 4
      UINDEX(stop, 5) = 4
      error = Getelt(fds, 'DATAGRP_TEST_3C',&
      &'*', UINDEX, usrord, 26880 * 4, buffer)
      if (error /= 0) goto 9999
      do i = 1, 3
         if (int(buffer(i) - (4 * 4 * 4 * 4 * 4 * 3 * i)) /= 0) print *, 'error, i= ', i
      end do
      if (int(buffer(4) - (4 * 4 * 4 * 4 * 4 * 3)) /= 0) print *, 'error, i= ', i
      call clock(cpu2)
      elap_r = elap_r + cpu2 - cpu1
      write (file_unit, '(''DATAGRP_TEST_3C'',&
      &          '' read in [sec]'',1PE13.5)') cpu2 - cpu1

      write (file_unit, '(''Lees een cel van 16 bytes'')')
      call clock(cpu1)
      UINDEX(start, 1) = 3
      UINDEX(start, 2) = 3
      UINDEX(start, 3) = 3
      UINDEX(start, 4) = 3
      UINDEX(start, 5) = 3
      UINDEX(stop, 1) = 3
      UINDEX(stop, 2) = 3
      UINDEX(stop, 3) = 3
      UINDEX(stop, 4) = 3
      UINDEX(stop, 5) = 3
      error = Getelt(fds, 'DATAGRP_TEST_3D',&
      &'*', UINDEX, usrord, 26880 * 4, buffer)
      if (error /= 0) goto 9999
      do i = 1, 3
         if (int(buffer(i) - (3 * 3 * 3 * 3 * 3 * 4 * i)) /= 0) then
            print *, 'error, i=', i, ' buffer=', nint(buffer(i))
         end if
      end do
      if (int(buffer(4) - (3 * 3 * 3 * 3 * 3 * 4)) /= 0)&
      &print *, 'error, i=', i, ' buffer=', nint(buffer(4))
      call clock(cpu2)
      elap_r = elap_r + cpu2 - cpu1
      write (file_unit, '(''DATAGRP_TEST_3D'',&
      &          '' read in [sec]'',1PE13.5)') cpu2 - cpu1

      usrord(1) = 5
      usrord(2) = 3
      usrord(3) = 1
      usrord(4) = 2
      usrord(5) = 4
      UINDEX(start, 1) = 1
      UINDEX(start, 2) = 1
      UINDEX(start, 3) = 1
      UINDEX(start, 4) = 1
      UINDEX(start, 5) = 1
      UINDEX(stop, 1) = 8
      UINDEX(stop, 2) = 6
      UINDEX(stop, 3) = 4
      UINDEX(stop, 4) = 5
      UINDEX(stop, 5) = 7

      write (file_unit, *)
      write (file_unit, '(&
      &  ''Lees 6720 elementen van 4 bytes in een (1) opdracht'')')
      call clock(cpu1)
      error = Getelt(fds, 'DATAGRP_TEST_3A',&
      &'ELEM_R_4', UINDEX, usrord, 26880 * 4, buffer)
      if (error /= 0) goto 9999
      n = 0
      do i = 1, 7
         do j = 1, 5
            do k = 1, 4
               do l = 1, 6
                  do m = 1, 8
                     n = n + 1
                     if (int(buffer(n) / (real(i) * real(j) * real(k) * real(l) *&
                     &real(m)) - 1) /= 0) print *, 'error, i= ', i
                  end do
               end do
            end do
         end do
      end do
      call clock(cpu2)
      elap_r = elap_r + cpu2 - cpu1
      write (file_unit, '(''DATAGRP_TEST_3A'',&
      &          '' read in [sec]'',1PE13.5)') cpu2 - cpu1

      write (file_unit, '(&
      &  ''Lees 6720 elementen van 4 bytes in een (1) opdracht'')')
      call clock(cpu1)
      error = Getelt(fds, 'DATAGRP_TEST_3B',&
      &'ELEM_R_4', UINDEX, usrord, 26880 * 4, buffer)
      if (error /= 0) goto 9999
      n = 0
      do i = 1, 7
         do j = 1, 5
            do k = 1, 4
               do l = 1, 6
                  do m = 1, 8
                     n = n + 1
                     if (int(buffer(n) / (real(i) * real(j) * real(k) * real(l) *&
                     &real(m)) - 2) /= 0) print *, 'error, i= ', i
                  end do
               end do
            end do
         end do
      end do
      call clock(cpu2)
      elap_r = elap_r + cpu2 - cpu1
      write (file_unit, '(''DATAGRP_TEST_3B'',&
      &          '' read in [sec]'',1PE13.5)') cpu2 - cpu1

      write (file_unit, '(&
      &  ''Lees 6720 elementen van 4 bytes in een (1) opdracht'')')
      call clock(cpu1)
      error = Getelt(fds, 'DATAGRP_TEST_3C',&
      &'ELEM_R_4', UINDEX, usrord, 26880 * 4, buffer)
      if (error /= 0) goto 9999
      n = 0
      do i = 1, 7
         do j = 1, 5
            do k = 1, 4
               do l = 1, 6
                  do m = 1, 8
                     n = n + 1
                     if (int(buffer(n) / (real(i) * real(j) * real(k) * real(l) *&
                     &real(m)) - 3) /= 0) print *, 'error, i= ', i
                  end do
               end do
            end do
         end do
      end do
      call clock(cpu2)
      elap_r = elap_r + cpu2 - cpu1
      write (file_unit, '(''DATAGRP_TEST_3C'',&
      &          '' read in [sec]'',1PE13.5)') cpu2 - cpu1

      write (file_unit, '(&
      &  ''Lees 6720 elementen van 4 bytes in een (1) opdracht'')')
      call clock(cpu1)
      error = Getelt(fds, 'DATAGRP_TEST_3D',&
      &'ELEM_R_4', UINDEX, usrord, 26880 * 4, buffer)
      if (error /= 0) goto 9999
      n = 0
      do i = 1, 7
         do j = 1, 5
            do k = 1, 4
               do l = 1, 6
                  do m = 1, 8
                     n = n + 1
                     if (int(buffer(n) / (real(i) * real(j) * real(k) * real(l) *&
                     &real(m)) - 4) /= 0) print *, 'error, i= ', i
                  end do
               end do
            end do
         end do
      end do
      call clock(cpu2)
      elap_r = elap_r + cpu2 - cpu1
      write (file_unit, '(''DATAGRP_TEST_3D'',&
      &          '' read in [sec]'',1PE13.5)') cpu2 - cpu1

      write (file_unit, *)
      write (file_unit, '(&
      &  ''Lees 6720 elementen van 12 bytes in een (1) opdracht'')')
!        call clock(cpu1)
      error = Getelt(fds, 'DATAGRP_TEST_3A',&
      &'ELEM_R_4_DIM_1', UINDEX, usrord, 26880 * 4, buffer)
      if (error /= 0) goto 9999
      n = 0
      do i = 1, 7
         do j = 1, 5
            do k = 1, 4
               do l = 1, 6
                  do m = 1, 8
                     do im = 1, 3
                        n = n + 1
                        if (int(buffer(n) / (real(i) * real(j) * real(k) * real(l) *&
                        &real(m) * real(im)) - 1) /= 0) print *, 'error, i= ', i
                     end do
                  end do
               end do
            end do
         end do
      end do
      call clock(cpu2)
      elap_r = elap_r + cpu2 - cpu1
      write (file_unit, '(''DATAGRP_TEST_3A'',&
      &          '' read in [sec]'',1PE13.5)') cpu2 - cpu1

      write (file_unit, '(&
      &  ''Lees 6720 elementen van 12 bytes in een (1) opdracht'')')
      call clock(cpu1)
      error = Getelt(fds, 'DATAGRP_TEST_3B',&
      &'ELEM_R_4_DIM_1', UINDEX, usrord, 26880 * 4, buffer)
      if (error /= 0) goto 9999
      n = 0
      do i = 1, 7
         do j = 1, 5
            do k = 1, 4
               do l = 1, 6
                  do m = 1, 8
                     do im = 1, 3
                        n = n + 1
                        if (int(buffer(n) / (real(i) * real(j) * real(k) * real(l) *&
                        &real(m) * real(im)) - 2) /= 0) print *, 'error, i= ', i
                     end do
                  end do
               end do
            end do
         end do
      end do
      call clock(cpu2)
      elap_r = elap_r + cpu2 - cpu1
      write (file_unit, '(''DATAGRP_TEST_3B'',&
      &          '' read in [sec]'',1PE13.5)') cpu2 - cpu1

      write (file_unit, '(&
      &  ''Lees 6720 elementen van 12 bytes in een (1) opdracht'')')
      call clock(cpu1)
      error = Getelt(fds, 'DATAGRP_TEST_3C',&
      &'ELEM_R_4_DIM_1', UINDEX, usrord, 26880 * 4, buffer)
      if (error /= 0) goto 9999
      n = 0
      do i = 1, 7
         do j = 1, 5
            do k = 1, 4
               do l = 1, 6
                  do m = 1, 8
                     do im = 1, 3
                        n = n + 1
                        if (int(buffer(n) / (real(i) * real(j) * real(k) * real(l) *&
                        &real(m) * real(im)) - 3) /= 0) print *, 'error, i= ', i
                     end do
                  end do
               end do
            end do
         end do
      end do
      call clock(cpu2)
      elap_r = elap_r + cpu2 - cpu1
      write (file_unit, '(''DATAGRP_TEST_3C'',&
      &          '' read in [sec]'',1PE13.5)') cpu2 - cpu1

      write (file_unit, '(&
      &  ''Lees 6720 elementen van 12 bytes in een (1) opdracht'')')
      call clock(cpu1)
      error = Getelt(fds, 'DATAGRP_TEST_3D',&
      &'ELEM_R_4_DIM_1', UINDEX, usrord, 26880 * 4, buffer)
      if (error /= 0) goto 9999
      n = 0
      do i = 1, 7
         do j = 1, 5
            do k = 1, 4
               do l = 1, 6
                  do m = 1, 8
                     do im = 1, 3
                        n = n + 1
                        if (int(buffer(n) / (real(i) * real(j) * real(k) * real(l) *&
                        &real(m) * real(im)) - 4) /= 0) print *, 'error, i= ', i
                     end do
                  end do
               end do
            end do
         end do
      end do
      call clock(cpu2)
      elap_r = elap_r + cpu2 - cpu1
      write (file_unit, '(''DATAGRP_TEST_3D'',&
      &          '' read in [sec]'',1PE13.5)') cpu2 - cpu1
9999  continue
      error = Clsdat(fds)
      if (error /= 0) goto 9999

      error = Clsdef(fds)
      if (error /= 0) goto 9999

      ERROR = NEFERR(0, ERRSTR)
      write (file_unit, *)
      write (file_unit, '(a)') trim(errstr)
      write (file_unit, '(''Total elapsed write time [sec]: '', 1PE13.5)') elap_w
      write (file_unit, '(''Total elapsed read time [sec] : '', 1PE13.5)') elap_r

      close (file_unit)
      !
      call compare_text_files(filename1, filename2, skiplines)

   end subroutine test_06
   !$f90tw)

   !$f90tw TESTCODE(TEST, slow_nefis_tests, test_14, test_14,
   subroutine test_14() bind(C)

      integer NTIMES, BUFSIZ
!
! size of nefis file: 4.800 Mbyte = 4xNTIMESxBUFSIZ: NTIMES=600, BUFSIZ=2000000
! size of nefis file: 3.200 Mbyte = 4xNTIMESxBUFSIZ: NTIMES=400, BUFSIZ=2000000
!
!      PARAMETER (NTIMES=600, BUFSIZ=2000000)  ! 4.8 Gbyte
      parameter(NTIMES=400, BUFSIZ=2000000) ! 3.2 Gbyte
!      PARAMETER (NTIMES=400000, BUFSIZ=2000) ! 3.2 Gbyte
!      PARAMETER (NTIMES=20, BUFSIZ=200)

      integer START, stp, INCR
      parameter(START=1, stp=2, INCR=3)
      integer fds
      integer clsdat,&
      &clsdef,&
      &credat,&
      &defelm,&
      &defcel,&
      &defgrp,&
      &getnfv,&
      &getelt
      integer crenef,&
      &putelt,&
      &clsnef,&
      &neferr,&
      &reserr
      integer error, ierror,&
      &i, j,&
      &grpdms(1),&
      &grpord(1),&
      &usrord(1),&
      &UINDEX(3, 5)
      integer buffer(BUFSIZ)
      character names * 14, coding * 1
      character ERRSTR * 1024
      character * 16 dat_name, def_name
      real cpu1, cpu2
      real elap_r, elap_w
      character * 255 version
      character(len=30) :: filename1
      character(len=30) :: filename2
      integer file_unit

      ! delete previous output files if they exist
      filename1 = 'test14.out'
      filename2 = 'test14.scr'
      call delete_file(filename1)

      open (newunit=file_unit, file=filename1)

      error = reserr()
      error = getnfv(version)
      write (file_unit, *) '-----------------------------------------------'
      write (file_unit, '(a)') 'Version: '//trim(version(5:))
      write (file_unit, *) '-----------------------------------------------'

      elap_w = 0
      elap_r = 0
      call clock(cpu1)
      if (0 == 0) then
         coding = 'B'
         dat_name = 'data_c14.dat'
         def_name = 'data_c14.def'
         error = crenef(fds, dat_name, def_name, coding, 'C')

         if (error /= 0) then
            ierror = neferr(0, errstr)
            write (file_unit, *)
            write (file_unit, '(a)') trim(errstr)
            goto 9999
         end if

         error = Defelm(fds, 'ELEM_R_4_DIM_1', 'INTEGER', 4,&
         &'GROOTHEID 2', 'eenheid 2', 'Beschrijving 2',&
         &1, BUFSIZ)
         if (error /= 0) then
            ierror = neferr(0, errstr)
            write (file_unit, *)
            write (file_unit, '(a)') trim(errstr)
            goto 9999
         end if

         names = 'ELEM_R_4_DIM_1'
         error = Defcel(fds, 'CEL_TEST_3', 1, names)
         if (error /= 0) then
            ierror = neferr(0, errstr)
            write (file_unit, *)
            write (file_unit, '(a)') trim(errstr)
            goto 9999
         end if

         grpdms(1) = 0
         grpord(1) = 1
         error = Defgrp(fds, 'GRP_TEST_3D', 'CEL_TEST_3', 1,&
         &grpdms, grpord)
         if (error /= 0) then
            ierror = neferr(0, errstr)
            write (file_unit, *)
            write (file_unit, '(a)') trim(errstr)
            goto 9999
         end if
!---------------------------------------------------------------------
         error = Credat(fds, 'DATAGRP_TEST_3D', 'GRP_TEST_3D')
         if (error /= 0) then
            ierror = neferr(0, errstr)
            write (file_unit, *)
            write (file_unit, '(a)') trim(errstr)
            goto 9999
         end if
!---------------------------------------------------------------------
         call clock(cpu2)
         write (file_unit, '(''Initialisation (real time) [sec]:'',1PE13.5)')&
         &cpu2 - cpu1
         write (file_unit, *)

         usrord(1) = 1
         UINDEX(incr, 1) = 1

         write (file_unit,&
         &'(I5,'' schrijfopdrachten van '',I9,'' bytes'')') NTIMES, BUFSIZ * 4
         do j = 1, NTIMES
            do i = 1, BUFSIZ
               buffer(i) = 1000 * i + j
            end do
            if (ntimes > 1000) then
               if (mod(j, 100) == 1)&
               &write (file_unit, '(''opdracht '', i3, '' van '', i3)') j, ntimes
            elseif (ntimes > 100) then
               if (mod(j, 10) == 1)&
               &write (file_unit, '(''opdracht '', i3, '' van '', i3)') j, ntimes
            elseif (ntimes > 10) then
               write (file_unit, '(''opdracht '', i3, '' van '', i3)') j, ntimes
            end if
            UINDEX(start, 1) = j
            UINDEX(stp, 1) = j
            call clock(cpu1)
            if (j == 265) then
               write (file_unit, *)
            end if

            error = Putelt(fds, 'DATAGRP_TEST_3D',&
            &'ELEM_R_4_DIM_1', UINDEX, usrord, buffer)
            call clock(cpu2)
            elap_w = elap_w + cpu2 - cpu1
            if (error /= 0) then
               ierror = neferr(0, errstr)
               write (file_unit, *)
               write (file_unit, '(a)') trim(errstr)
               goto 9999
            end if
         end do
         write (file_unit, '(''Writing (real time) [sec]:'',1PE13.5)') elap_w
         write (file_unit, *)
         error = Clsnef(fds)
      end if

      coding = ' '
      error = crenef(fds, dat_name, def_name, coding, 'R')
      if (error /= 0) then
         ierror = neferr(0, errstr)
         write (file_unit, *)
         write (file_unit, '(a)') trim(errstr)
         goto 9999
      end if
      write (file_unit,&
      &'(''Lees '', I5, '' keer '', I9, '' bytes'')') NTIMES, BUFSIZ * 4
      do j = NTIMES - 9, NTIMES + 1
!      DO 40 j=NTIMES, NTIMES+1
         write (file_unit, '(''opdracht '', I3)') j
         UINDEX(start, 1) = j
         UINDEX(stp, 1) = j
         UINDEX(incr, 1) = 1
         usrord(1) = 1
         call clock(cpu1)
         error = Getelt(fds, 'DATAGRP_TEST_3D',&
         &'ELEM_R_4_DIM_1', UINDEX, usrord, BUFSIZ * 4,&
         &buffer)
         call clock(cpu2)
         elap_r = elap_r + cpu2 - cpu1
         if (error /= 0) then
            ierror = neferr(0, errstr)
            write (file_unit, *)
            write (file_unit, '(a)') trim(errstr)
            goto 9999
         else
            do i = 1, BUFSIZ
               if ((buffer(i) - (1000 * i + j)) /= 0) then
                  print *, 'error, i= ', i, buffer(i), 1000 * i + j
               end if
            end do
         end if
      end do
      write (file_unit, '(''Writing (real time) [sec]:'',1PE13.5)') elap_w
      write (file_unit, '(''Reading (real time) [sec]:'',1PE13.5)') elap_r

9999  continue

      error = Clsdat(fds)
      if (error /= 0) then
         ierror = neferr(0, errstr)
         write (file_unit, *)
         write (file_unit, '(a)') trim(errstr)
      end if

      error = Clsdef(fds)
      if (error /= 0) then
         ierror = neferr(0, errstr)
         write (file_unit, *)
         write (file_unit, '(a)') trim(errstr)
      end if

      ierror = neferr(0, errstr)
      write (file_unit, *)
      write (file_unit, '(a)') trim(errstr)

      close (file_unit)
      !
      call compare_text_files(filename1, filename2, skiplines)

   end subroutine test_14
   !$f90tw)

   !$f90tw TESTCODE(TEST, slow_nefis_tests, test_16, test_16,
   subroutine test_16() bind(C)
      integer * 4 fds_a,&
      &fds_b,&
      &fds_c
      integer clsdat,&
      &clsdef,&
      &getnfv,&
      &NEFERR,&
      &reserr
      integer i, error
      character ERRSTR * 1024
      character * 255 version
      character(len=30) :: filename1
      character(len=30) :: filename2
      integer file_unit

      ! delete previous output files if they exist
      filename1 = 'test16.out'
      filename2 = 'test16.scr'
      call delete_file(filename1)
      call delete_file('data_c16a.def')
      call delete_file('data_c16a.dat')
      call delete_file('data_c16b.def')
      call delete_file('data_c16b.dat')
      call delete_file('data_c16c.def')
      call delete_file('data_c16c.dat')

      open (newunit=file_unit, file=filename1)

      error = reserr()
      error = getnfv(version)
      write (file_unit, *) '-----------------------------------------------'
      write (file_unit, '(a)') 'Version: '//trim(version(5:))
      write (file_unit, *) '-----------------------------------------------'

      write (file_unit, '('' Same test as test test_12'',&
      &          '' but open en close files 10 times '')')

      do i = 1, 10
         write (file_unit, '(i0)') i

         call WriteFile2('data_c16a', fds_a, 33, file_unit)
         call WriteFile2('data_c16b', fds_b, 39, file_unit)
         call WriteFile2('data_c16c', fds_c, 78, file_unit)

         call ReadFile2(fds_a, 33, file_unit)
         call ReadFile2(fds_b, 39, file_unit)
         call ReadFile2(fds_c, 78, file_unit)

         error = Clsdat(fds_a)
         if (error /= 0) then
            error = neferr(0, errstr)
            write (file_unit, '(a)') trim(errstr)
         end if

         error = Clsdat(fds_b)
         if (error /= 0) then
            error = neferr(0, errstr)
            write (file_unit, '(a)') trim(errstr)
         end if

         error = Clsdat(fds_c)
         if (error /= 0) then
            error = neferr(0, errstr)
            write (file_unit, '(a)') trim(errstr)
         end if

         error = Clsdef(fds_a)
         if (error /= 0) then
            error = neferr(0, errstr)
            write (file_unit, '(a)') trim(errstr)
         end if

         error = Clsdef(fds_b)
         if (error /= 0) then
            error = neferr(0, errstr)
            write (file_unit, '(a)') trim(errstr)
         end if

         error = Clsdef(fds_c)
         if (error /= 0) then
            error = neferr(0, errstr)
            write (file_unit, '(a)') trim(errstr)
         end if

      end do

      if (error == 0) then
         error = neferr(0, errstr)
         write (file_unit, '(a)')
         write (file_unit, '(a)') trim(errstr)
      end if

      close (file_unit)
      !
      call compare_text_files(filename1, filename2, skiplines)

   end subroutine test_16
   !$f90tw)

end module m_slow_nefis_tests
