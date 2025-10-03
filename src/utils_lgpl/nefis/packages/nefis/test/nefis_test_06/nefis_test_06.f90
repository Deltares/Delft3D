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

module m_nefis_test_06
   use assertions_gtest
   use tests_nefis_helper
   implicit none

   character(len=30), dimension(3) :: skiplines = ["Version", "version", "-----"]

contains

   !$f90tw TESTCODE(TEST, nefis_tests, test_06, test_06,
   subroutine test_06() bind(C)
      character(len=30) :: filename1
      character(len=30) :: filename2
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
      &neferr
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
      filename1 = 'test.out'
      filename2 = 'test.scr'
      call delete_file(filename1)
      call delete_file('nefis_ex.def')
      call delete_file('nefis_ex.dat')
      open (newunit=file_unit, file=filename1)

      error = getnfv(version)
      write (file_unit, *) '-----------------------------------------------'
      write (file_unit, '(a)') 'Version: '//trim(version(5:))
      write (file_unit, *) '-----------------------------------------------'

      call clock(cpu1)
      coding = ' '
      error = Opndef(fds, 'nefis_ex.def', coding)
      if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)

      error = Opndat(fds, 'nefis_ex.dat', coding)
      if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)

      error = Defelm(fds, 'ELEM_R_4', 'REAL', 4,&
      &'GROOTHEID 1', 'eenheid 1', 'Beschrijving 1',&
      &0, 0)
      if (error /= 0) ERROR = NEFERR(1, ERRSTR)

      error = Defelm(fds, 'ELEM_R_4_DIM_1', 'REAL', 4,&
      &'GROOTHEID 2', 'eenheid 2', 'Beschrijving 2',&
      &1, 3)
      if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)

      names(1) = 'ELEM_R_4_DIM_1'
      names(2) = 'ELEM_R_4'
      error = Defcel(fds, 'CEL_TEST_3', 2, names)
      if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)

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
      if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)

      grpord(1) = 5
      grpord(2) = 4
      grpord(3) = 3
      grpord(4) = 2
      grpord(5) = 1
      error = Defgrp(fds, 'GRP_TEST_3B', 'CEL_TEST_3', 5,&
      &grpdms, grpord)
      if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)

      grpord(1) = 5
      grpord(2) = 3
      grpord(3) = 1
      grpord(4) = 2
      grpord(5) = 4
      error = Defgrp(fds, 'GRP_TEST_3C', 'CEL_TEST_3', 5,&
      &grpdms, grpord)
      if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)

      grpdms(1) = 0

      grpord(1) = 5
      grpord(2) = 4
      grpord(3) = 3
      grpord(4) = 2
      grpord(5) = 1

      error = Defgrp(fds, 'GRP_TEST_3D', 'CEL_TEST_3', 5,&
      &grpdms, grpord)
      if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
!---------------------------------------------------------------------
      error = Credat(fds, 'DATAGRP_TEST_3A', 'GRP_TEST_3A')
      if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)

      error = Credat(fds, 'DATAGRP_TEST_3B', 'GRP_TEST_3B')
      if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)

      error = Credat(fds, 'DATAGRP_TEST_3C', 'GRP_TEST_3C')
      if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)

      error = Credat(fds, 'DATAGRP_TEST_3D', 'GRP_TEST_3D')
      if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
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
      do 60 i = 1, 8
         UINDEX(start, 5) = I
         UINDEX(stop, 5) = I
         do 50 j = 1, 7
            UINDEX(start, 4) = J
            UINDEX(stop, 4) = J
            do 40 k = 1, 6
               UINDEX(start, 3) = K
               UINDEX(stop, 3) = K
               do 30 l = 1, 5
                  UINDEX(start, 2) = L
                  UINDEX(stop, 2) = L
                  do 20 m = 1, 4
                     UINDEX(start, 1) = M
                     UINDEX(stop, 1) = M
                     do 10 n = 1, 4
                        buffer(n) = real(i) * real(j) * real(k) * real(l) *&
                        &real(m) * real(n)
10                      continue
                        error = Putelt(fds, 'DATAGRP_TEST_3A',&
                        &'*', UINDEX, usrord, buffer)
                        if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
20                      continue
30                      continue
40                      continue
50                      continue
60                      continue
                        call clock(cpu2)
                        elap_w = cpu2 - cpu1
                        write (file_unit, '(''DATAGRP_TEST_3A'',&
                        &          '' written in [sec]'',1PE13.5)') cpu2 - cpu1

                        write (file_unit, '(&
                        &  ''6720 schrijfopdrachten (6720 cellen) van 16 bytes elk'')')
                        call clock(cpu1)
                        do 120 i = 1, 8
                           UINDEX(start, 5) = I
                           UINDEX(stop, 5) = I
                           do 110 j = 1, 7
                              UINDEX(start, 4) = J
                              UINDEX(stop, 4) = J
                              do 100 k = 1, 6
                                 UINDEX(start, 3) = K
                                 UINDEX(stop, 3) = K
                                 do 90 l = 1, 5
                                    UINDEX(start, 2) = L
                                    UINDEX(stop, 2) = L
                                    do 80 m = 1, 4
                                       UINDEX(start, 1) = M
                                       UINDEX(stop, 1) = M
                                       do 70 n = 1, 4
                                          buffer(n) = real(i) * real(j) * real(k) * real(l) *&
                                          &real(m) * real(n) * 2.
70                                        continue
                                          error = Putelt(fds, 'DATAGRP_TEST_3B',&
                                          &'*', UINDEX, usrord, buffer)
                                          if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
80                                        continue
90                                        continue
100                                       continue
110                                       continue
120                                       continue
                                          call clock(cpu2)
                                          elap_w = elap_w + cpu2 - cpu1
                                          write (file_unit, '(''DATAGRP_TEST_3B'',&
                                          &          '' written in [sec]'',1PE13.5)') cpu2 - cpu1

                                          write (file_unit, '(&
                                          &  ''6720 schrijfopdrachten (6720 cellen) van 16 bytes elk'')')
                                          call clock(cpu1)
                                          do 180 i = 1, 8
                                             UINDEX(start, 5) = I
                                             UINDEX(stop, 5) = I
                                             do 170 j = 1, 7
                                                UINDEX(start, 4) = J
                                                UINDEX(stop, 4) = J
                                                do 160 k = 1, 6
                                                   UINDEX(start, 3) = K
                                                   UINDEX(stop, 3) = K
                                                   do 150 l = 1, 5
                                                      UINDEX(start, 2) = L
                                                      UINDEX(stop, 2) = L
                                                      do 140 m = 1, 4
                                                         UINDEX(start, 1) = M
                                                         UINDEX(stop, 1) = M
                                                         do 130 n = 1, 4
                                                            buffer(n) = real(i) * real(j) * real(k) * real(l) *&
                                                            &real(m) * real(n) * 3.
130                                                         continue
                                                            error = Putelt(fds, 'DATAGRP_TEST_3C',&
                                                            &'*', UINDEX, usrord, buffer)
                                                            if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
140                                                         continue
150                                                         continue
160                                                         continue
170                                                         continue
180                                                         continue
                                                            call clock(cpu2)
                                                            elap_w = elap_w + cpu2 - cpu1
                                                            write (file_unit, '(''DATAGRP_TEST_3C'',&
                                                            &          '' written in [sec]'',1PE13.5)') cpu2 - cpu1

                                                            write (file_unit, '(&
                                                            &  ''6720 schrijfopdrachten (6720 cellen) van 16 bytes elk'')')
                                                            call clock(cpu1)
                                                            do 4120 i = 1, 8
                                                               UINDEX(start, 5) = I
                                                               UINDEX(stop, 5) = I
                                                               do 4110 j = 1, 7
                                                                  UINDEX(start, 4) = J
                                                                  UINDEX(stop, 4) = J
                                                                  do 4100 k = 1, 6
                                                                     UINDEX(start, 3) = K
                                                                     UINDEX(stop, 3) = K
                                                                     do 4090 l = 1, 5
                                                                        UINDEX(start, 2) = L
                                                                        UINDEX(stop, 2) = L
                                                                        do 4080 m = 1, 4
                                                                           UINDEX(start, 1) = M
                                                                           UINDEX(stop, 1) = M
                                                                           do 4070 n = 1, 4
                                                                              buffer(n) = real(i) * real(j) * real(k) * real(l) *&
                                                                              &real(m) * real(n) * 2.
4070                                                                          continue
                                                                              error = Putelt(fds, 'DATAGRP_TEST_3D',&
                                                                              &'*', UINDEX, usrord, buffer)
                                                                              if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
4080                                                                          continue
4090                                                                          continue
4100                                                                          continue
4110                                                                          continue
4120                                                                          continue
                                                                              call clock(cpu2)
                                                                              elap_w = elap_w + cpu2 - cpu1
                                                                              write (file_unit, '(''DATAGRP_TEST_3D'',&
                                                                              &          '' written in [sec]'',1PE13.5)') cpu2 - cpu1

                                                                              do 185 i = 1, 26880
                                                                                 buffer(i) = 0.0
185                                                                              continue

                                                                                 write (file_unit, *)
                                                                                 write (file_unit, '(&
                                                                                 &  ''6720 leesopdrachten (6720 cellen) van 16 bytes elk'')')
                                                                                 call clock(cpu1)
                                                                                 do 240 i = 1, 4
                                                                                    UINDEX(start, 1) = I
                                                                                    UINDEX(stop, 1) = I
                                                                                    do 230 j = 1, 5
                                                                                       UINDEX(start, 2) = J
                                                                                       UINDEX(stop, 2) = J
                                                                                       do 220 k = 1, 6
                                                                                          UINDEX(start, 3) = K
                                                                                          UINDEX(stop, 3) = K
                                                                                          do 210 l = 1, 7
                                                                                             UINDEX(start, 4) = L
                                                                                             UINDEX(stop, 4) = L
                                                                                             do 200 m = 1, 8
                                                                                                UINDEX(start, 5) = M
                                                                                                UINDEX(stop, 5) = M
                                                                                                error = Getelt(fds, 'DATAGRP_TEST_3A',&
                                                                                                &'*', UINDEX, usrord, 4 * 4, buffer)
                                                                                                !call f90_assert_eq(to_c(error), to_c(0), 'error')
                                                                                                if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
                                                                                                do 190 n = 1, 4
                                                                                                   if (int(buffer(n) / real(i) / real(j) / real(k) / real(l) /&
                                                                                                   &real(m) - n) /= 0) print *, 'error, i= ', i
190                                                                                                continue
200                                                                                                continue
210                                                                                                continue
220                                                                                                continue
230                                                                                                continue
240                                                                                                continue
                                                                                                   call clock(cpu2)
                                                                                                   elap_r = cpu2 - cpu1
                                                                                                   write (file_unit, '(''DATAGRP_TEST_3A'',&
                                                                                                   &          '' read in [sec]'',1PE13.5)') cpu2 - cpu1

                                                                                                   write (file_unit, '(&
                                                                                                   &  ''6720 leesopdrachten (6720 cellen) van 16 bytes elk'')')
                                                                                                   call clock(cpu1)
                                                                                                   do 300 i = 1, 4
                                                                                                      UINDEX(start, 1) = I
                                                                                                      UINDEX(stop, 1) = I
                                                                                                      do 290 j = 1, 5
                                                                                                         UINDEX(start, 2) = J
                                                                                                         UINDEX(stop, 2) = J
                                                                                                         do 280 k = 1, 6
                                                                                                            UINDEX(start, 3) = K
                                                                                                            UINDEX(stop, 3) = K
                                                                                                            do 270 l = 1, 7
                                                                                                               UINDEX(start, 4) = L
                                                                                                               UINDEX(stop, 4) = L
                                                                                                               do 260 m = 1, 8
                                                                                                                  UINDEX(start, 5) = M
                                                                                                                  UINDEX(stop, 5) = M
                                                                                                                  error = Getelt(fds, 'DATAGRP_TEST_3B',&
                                                                                                                  &'*', UINDEX, usrord, 4 * 4, buffer)
                                                                                                                  if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
                                                                                                                  do 250 n = 1, 4
                                                                                                                     if (int(buffer(n) / real(i) / real(j) / real(k) / real(l) /&
                                                                                                                     &real(m) - 2 * n) /= 0) print *, 'error, i= ', i
250                                                                                                                  continue
260                                                                                                                  continue
270                                                                                                                  continue
280                                                                                                                  continue
290                                                                                                                  continue
300                                                                                                                  continue
                                                                                                                     call clock(cpu2)
                                                                                                                     elap_r = elap_r + cpu2 - cpu1
                                                                                                                     write (file_unit, '(''DATAGRP_TEST_3B'',&
                                                                                                                     &          '' read in [sec]'',1PE13.5)') cpu2 - cpu1

                                                                                                                     write (file_unit, '(&
                                                                                                                     &  ''6720 leesopdrachten (6720 cellen) van 16 bytes elk'')')
                                                                                                                     call clock(cpu1)
                                                                                                                     do 360 i = 1, 4
                                                                                                                        UINDEX(start, 1) = I
                                                                                                                        UINDEX(stop, 1) = I
                                                                                                                        do 350 j = 1, 5
                                                                                                                           UINDEX(start, 2) = J
                                                                                                                           UINDEX(stop, 2) = J
                                                                                                                           do 340 k = 1, 6
                                                                                                                              UINDEX(start, 3) = K
                                                                                                                              UINDEX(stop, 3) = K
                                                                                                                              do 330 l = 1, 7
                                                                                                                                 UINDEX(start, 4) = L
                                                                                                                                 UINDEX(stop, 4) = L
                                                                                                                                 do 320 m = 1, 8
                                                                                                                                    UINDEX(start, 5) = M
                                                                                                                                    UINDEX(stop, 5) = M
                                                                                                                                    error = Getelt(fds, 'DATAGRP_TEST_3C',&
                                                                                                                                    &'*', UINDEX, usrord, 4 * 4, buffer)
                                                                                                                                    if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
                                                                                                                                    do 310 n = 1, 4
                                                                                                                                       if (int(buffer(n) / real(i) / real(j) / real(k) / real(l) /&
                                                                                                                                       &real(m) - 3 * n) /= 0) print *, 'error, i= ', i
310                                                                                                                                    continue
320                                                                                                                                    continue
330                                                                                                                                    continue
340                                                                                                                                    continue
350                                                                                                                                    continue
360                                                                                                                                    continue
                                                                                                                                       call clock(cpu2)
                                                                                                                                       elap_r = elap_r + cpu2 - cpu1
                                                                                                                                       write (file_unit, '(''DATAGRP_TEST_3C'',&
                                                                                                                                       &          '' read in [sec]'',1PE13.5)') cpu2 - cpu1

                                                                                                                                       write (file_unit, '(&
                                                                                                                                       &  ''6720 leesopdrachten (6720 cellen) van 16 bytes elk'')')
                                                                                                                                       call clock(cpu1)
                                                                                                                                       do 4300 i = 1, 4
                                                                                                                                          UINDEX(start, 1) = I
                                                                                                                                          UINDEX(stop, 1) = I
                                                                                                                                          do 4290 j = 1, 5
                                                                                                                                             UINDEX(start, 2) = J
                                                                                                                                             UINDEX(stop, 2) = J
                                                                                                                                             do 4280 k = 1, 6
                                                                                                                                                UINDEX(start, 3) = K
                                                                                                                                                UINDEX(stop, 3) = K
                                                                                                                                                do 4270 l = 1, 7
                                                                                                                                                   UINDEX(start, 4) = L
                                                                                                                                                   UINDEX(stop, 4) = L
                                                                                                                                                   do 4260 m = 1, 8
                                                                                                                                                      UINDEX(start, 5) = M
                                                                                                                                                      UINDEX(stop, 5) = M
                                                                                                                                                      error = Getelt(fds, 'DATAGRP_TEST_3D',&
                                                                                                                                                      &'*', UINDEX, usrord, 4 * 4, buffer)
                                                                                                                                                      if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
                                                                                                                                                      do 4250 n = 1, 4
                                                                                                                                                         if (int(buffer(n) / real(i) / real(j) / real(k) / real(l) /&
                                                                                                                                                         &real(m) - 2 * n) /= 0) print *, 'error, i= ', i
4250                                                                                                                                                     continue
4260                                                                                                                                                     continue
4270                                                                                                                                                     continue
4280                                                                                                                                                     continue
4290                                                                                                                                                     continue
4300                                                                                                                                                     continue
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
                                                                                                                                                         do 375 i = 1, 4
                                                                                                                                                            do 374 j = 1, 5
                                                                                                                                                               do 373 k = 1, 6
                                                                                                                                                                  do 372 l = 1, 7
                                                                                                                                                                     do 371 m = 1, 8
                                                                                                                                                                        n = n + 1
                                                                                                                                                                        buffer(n) = real(i) * real(j) * real(k) * real(l) * real(m)
371                                                                                                                                                                     continue
372                                                                                                                                                                     continue
373                                                                                                                                                                     continue
374                                                                                                                                                                     continue
375                                                                                                                                                                     continue
                                                                                                                                                                        error = Putelt(fds, 'DATAGRP_TEST_3A',&
                                                                                                                                                                        &'ELEM_R_4', UINDEX, usrord, buffer)
                                                                                                                                                                        if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
                                                                                                                                                                        call clock(cpu2)
                                                                                                                                                                        elap_w = elap_w + cpu2 - cpu1
                                                                                                                                                                        write (file_unit, '(''DATAGRP_TEST_3A'',&
                                                                                                                                                                        &          '' written in [sec]'',1PE13.5)') cpu2 - cpu1

                                                                                                                                                                        write (file_unit, '(&
                                                                                                                                                                        &  ''1 schrijfopdracht van 6720 elementen van 4 bytes'')')
                                                                                                                                                                        call clock(cpu1)
                                                                                                                                                                        n = 0
                                                                                                                                                                        do 385 i = 1, 4
                                                                                                                                                                           do 384 j = 1, 5
                                                                                                                                                                              do 383 k = 1, 6
                                                                                                                                                                                 do 382 l = 1, 7
                                                                                                                                                                                    do 381 m = 1, 8
                                                                                                                                                                                       n = n + 1
                                                                                                                                                                                       buffer(n) = real(i) * real(j) * real(k) * real(l) * real(m) * 2.
381                                                                                                                                                                                    continue
382                                                                                                                                                                                    continue
383                                                                                                                                                                                    continue
384                                                                                                                                                                                    continue
385                                                                                                                                                                                    continue
                                                                                                                                                                                       error = Putelt(fds, 'DATAGRP_TEST_3B',&
                                                                                                                                                                                       &'ELEM_R_4', UINDEX, usrord, buffer)
                                                                                                                                                                                       if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
                                                                                                                                                                                       call clock(cpu2)
                                                                                                                                                                                       elap_w = elap_w + cpu2 - cpu1
                                                                                                                                                                                       write (file_unit, '(''DATAGRP_TEST_3B'',&
                                                                                                                                                                                       &          '' written in [sec]'',1PE13.5)') cpu2 - cpu1

                                                                                                                                                                                       write (file_unit, '(&
                                                                                                                                                                                       &  ''1 schrijfopdracht van 6720 elementen van 4 bytes'')')
                                                                                                                                                                                       call clock(cpu1)
                                                                                                                                                                                       n = 0
                                                                                                                                                                                       do 395 i = 1, 4
                                                                                                                                                                                          do 394 j = 1, 5
                                                                                                                                                                                             do 393 k = 1, 6
                                                                                                                                                                                                do 392 l = 1, 7
                                                                                                                                                                                                   do 391 m = 1, 8
                                                                                                                                                                                                      n = n + 1
                                                                                                                                                                                                      buffer(n) = real(i) * real(j) * real(k) * real(l) * real(m) * 3.
391                                                                                                                                                                                                   continue
392                                                                                                                                                                                                   continue
393                                                                                                                                                                                                   continue
394                                                                                                                                                                                                   continue
395                                                                                                                                                                                                   continue
                                                                                                                                                                                                      error = Putelt(fds, 'DATAGRP_TEST_3C',&
                                                                                                                                                                                                      &'ELEM_R_4', UINDEX, usrord, buffer)
                                                                                                                                                                                                      if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
                                                                                                                                                                                                      call clock(cpu2)
                                                                                                                                                                                                      elap_w = elap_w + cpu2 - cpu1
                                                                                                                                                                                                      write (file_unit, '(''DATAGRP_TEST_3C'',&
                                                                                                                                                                                                      &          '' written in [sec]'',1PE13.5)') cpu2 - cpu1

                                                                                                                                                                                                      write (file_unit, '(&
                                                                                                                                                                                                      &  ''1 schrijfopdracht van 6720 elementen van 4 bytes'')')
                                                                                                                                                                                                      call clock(cpu1)
                                                                                                                                                                                                      n = 0
                                                                                                                                                                                                      do 4385 i = 1, 4
                                                                                                                                                                                                         do 4384 j = 1, 5
                                                                                                                                                                                                            do 4383 k = 1, 6
                                                                                                                                                                                                               do 4382 l = 1, 7
                                                                                                                                                                                                                  do 4381 m = 1, 8
                                                                                                                                                                                                                     n = n + 1
                                                                                                                                                                                                                     buffer(n) = real(i) * real(j) * real(k) * real(l) * real(m) * 4.
4381                                                                                                                                                                                                                 continue
4382                                                                                                                                                                                                                 continue
4383                                                                                                                                                                                                                 continue
4384                                                                                                                                                                                                                 continue
4385                                                                                                                                                                                                                 continue
                                                                                                                                                                                                                     error = Putelt(fds, 'DATAGRP_TEST_3D',&
                                                                                                                                                                                                                     &'ELEM_R_4', UINDEX, usrord, buffer)
                                                                                                                                                                                                                     if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
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
                                                                                                                                                                                                                     do 406 i = 1, 8
                                                                                                                                                                                                                        do 405 j = 1, 7
                                                                                                                                                                                                                           do 404 k = 1, 6
                                                                                                                                                                                                                              do 403 l = 1, 5
                                                                                                                                                                                                                                 do 402 m = 1, 4
                                                                                                                                                                                                                                    do 401 im = 1, 3
                                                                                                                                                                                                                                       n = n + 1
                                                                                                                                                                                                                                       buffer(n) = real(i) * real(j) * real(k) * real(l) * real(m) *&
                                                                                                                                                                                                                                       &real(im)
401                                                                                                                                                                                                                                    continue
402                                                                                                                                                                                                                                    continue
403                                                                                                                                                                                                                                    continue
404                                                                                                                                                                                                                                    continue
405                                                                                                                                                                                                                                    continue
406                                                                                                                                                                                                                                    continue
                                                                                                                                                                                                                                       error = Putelt(fds, 'DATAGRP_TEST_3A',&
                                                                                                                                                                                                                                       &'ELEM_R_4_DIM_1', UINDEX, usrord, buffer)
                                                                                                                                                                                                                                       if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
                                                                                                                                                                                                                                       call clock(cpu2)
                                                                                                                                                                                                                                       elap_w = elap_w + cpu2 - cpu1
                                                                                                                                                                                                                                       write (file_unit, '(''DATAGRP_TEST_3A'',&
                                                                                                                                                                                                                                       &          '' written in [sec]'',1PE13.5)') cpu2 - cpu1

                                                                                                                                                                                                                                       write (file_unit, '(&
                                                                                                                                                                                                                                       &  ''1 schrijfopdracht van 6720 elementen van 12 bytes'')')
                                                                                                                                                                                                                                       call clock(cpu1)
                                                                                                                                                                                                                                       n = 0
                                                                                                                                                                                                                                       do 416 i = 1, 8
                                                                                                                                                                                                                                          do 415 j = 1, 7
                                                                                                                                                                                                                                             do 414 k = 1, 6
                                                                                                                                                                                                                                                do 413 l = 1, 5
                                                                                                                                                                                                                                                   do 412 m = 1, 4
                                                                                                                                                                                                                                                      do 411 im = 1, 3
                                                                                                                                                                                                                                                         n = n + 1
                                                                                                                                                                                                                                                         buffer(n) = real(i) * real(j) * real(k) * real(l) * real(m) *&
                                                                                                                                                                                                                                                         &real(im) * 2.
411                                                                                                                                                                                                                                                      continue
412                                                                                                                                                                                                                                                      continue
413                                                                                                                                                                                                                                                      continue
414                                                                                                                                                                                                                                                      continue
415                                                                                                                                                                                                                                                      continue
416                                                                                                                                                                                                                                                      continue
                                                                                                                                                                                                                                                         error = Putelt(fds, 'DATAGRP_TEST_3B',&
                                                                                                                                                                                                                                                         &'ELEM_R_4_DIM_1', UINDEX, usrord, buffer)
                                                                                                                                                                                                                                                         if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
                                                                                                                                                                                                                                                         call clock(cpu2)
                                                                                                                                                                                                                                                         elap_w = elap_w + cpu2 - cpu1
                                                                                                                                                                                                                                                         write (file_unit, '(''DATAGRP_TEST_3B'',&
                                                                                                                                                                                                                                                         &          '' written in [sec]'',1PE13.5)') cpu2 - cpu1

                                                                                                                                                                                                                                                         write (file_unit, '(&
                                                                                                                                                                                                                                                         &  ''1 schrijfopdracht van 6720 elementen van 12 bytes'')')
                                                                                                                                                                                                                                                         call clock(cpu1)
                                                                                                                                                                                                                                                         n = 0
                                                                                                                                                                                                                                                         do 426 i = 1, 8
                                                                                                                                                                                                                                                            do 425 j = 1, 7
                                                                                                                                                                                                                                                               do 424 k = 1, 6
                                                                                                                                                                                                                                                                  do 423 l = 1, 5
                                                                                                                                                                                                                                                                     do 422 m = 1, 4
                                                                                                                                                                                                                                                                        do 421 im = 1, 3
                                                                                                                                                                                                                                                                           n = n + 1
                                                                                                                                                                                                                                                                           buffer(n) = real(i) * real(j) * real(k) * real(l) * real(m) *&
                                                                                                                                                                                                                                                                           &real(im) * 3.
421                                                                                                                                                                                                                                                                        continue
422                                                                                                                                                                                                                                                                        continue
423                                                                                                                                                                                                                                                                        continue
424                                                                                                                                                                                                                                                                        continue
425                                                                                                                                                                                                                                                                        continue
426                                                                                                                                                                                                                                                                        continue
                                                                                                                                                                                                                                                                           error = Putelt(fds, 'DATAGRP_TEST_3C',&
                                                                                                                                                                                                                                                                           &'ELEM_R_4_DIM_1', UINDEX, usrord, buffer)
                                                                                                                                                                                                                                                                           if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
                                                                                                                                                                                                                                                                           call clock(cpu2)
                                                                                                                                                                                                                                                                           elap_w = elap_w + cpu2 - cpu1
                                                                                                                                                                                                                                                                           write (file_unit, '(''DATAGRP_TEST_3C'',&
                                                                                                                                                                                                                                                                           &          '' written in [sec]'',1PE13.5)') cpu2 - cpu1

                                                                                                                                                                                                                                                                           write (file_unit, '(&
                                                                                                                                                                                                                                                                           &  ''1 schrijfopdracht van 6720 elementen van 12 bytes'')')
                                                                                                                                                                                                                                                                           call clock(cpu1)
                                                                                                                                                                                                                                                                           n = 0
                                                                                                                                                                                                                                                                           do 4416 i = 1, 8
                                                                                                                                                                                                                                                                              do 4415 j = 1, 7
                                                                                                                                                                                                                                                                                 do 4414 k = 1, 6
                                                                                                                                                                                                                                                                                    do 4413 l = 1, 5
                                                                                                                                                                                                                                                                                       do 4412 m = 1, 4
                                                                                                                                                                                                                                                                                          do 4411 im = 1, 3
                                                                                                                                                                                                                                                                                             n = n + 1
                                                                                                                                                                                                                                                                                             buffer(n) = real(i) * real(j) * real(k) * real(l) * real(m) *&
                                                                                                                                                                                                                                                                                             &real(im) * 4.
4411                                                                                                                                                                                                                                                                                         continue
4412                                                                                                                                                                                                                                                                                         continue
4413                                                                                                                                                                                                                                                                                         continue
4414                                                                                                                                                                                                                                                                                         continue
4415                                                                                                                                                                                                                                                                                         continue
4416                                                                                                                                                                                                                                                                                         continue
                                                                                                                                                                                                                                                                                             error = Putelt(fds, 'DATAGRP_TEST_3D',&
                                                                                                                                                                                                                                                                                             &'ELEM_R_4_DIM_1', UINDEX, usrord, buffer)
                                                                                                                                                                                                                                                                                             if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
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
                                                                                                                                                                                                                                                                                             if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
                                                                                                                                                                                                                                                                                             do 427 i = 1, 3
                                                                                                                                                                                                                                                                                                if (int(buffer(i) - (2 * 2 * 2 * 2 * 2 * i)) /= 0) print *, 'error, i= ', i
427                                                                                                                                                                                                                                                                                             continue
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
                                                                                                                                                                                                                                                                                                if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
                                                                                                                                                                                                                                                                                                do 428 i = 1, 3
                                                                                                                                                                                                                                                                                                   if (int(buffer(i) - (3 * 3 * 3 * 3 * 3 * 2 * i)) /= 0) print *, 'error, i= ', i
428                                                                                                                                                                                                                                                                                                continue
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
                                                                                                                                                                                                                                                                                                   if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
                                                                                                                                                                                                                                                                                                   do 429 i = 1, 3
                                                                                                                                                                                                                                                                                                      if (int(buffer(i) - (4 * 4 * 4 * 4 * 4 * 3 * i)) /= 0) print *, 'error, i= ', i
429                                                                                                                                                                                                                                                                                                   continue
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
                                                                                                                                                                                                                                                                                                      if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
                                                                                                                                                                                                                                                                                                      do 4428 i = 1, 3
                                                                                                                                                                                                                                                                                                         if (int(buffer(i) - (3 * 3 * 3 * 3 * 3 * 4 * i)) /= 0) then
                                                                                                                                                                                                                                                                                                            print *, 'error, i=', i, ' buffer=', nint(buffer(i))
                                                                                                                                                                                                                                                                                                         end if
4428                                                                                                                                                                                                                                                                                                     continue
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
                                                                                                                                                                                                                                                                                                         if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
                                                                                                                                                                                                                                                                                                         n = 0
                                                                                                                                                                                                                                                                                                         do 435 i = 1, 7
                                                                                                                                                                                                                                                                                                            do 434 j = 1, 5
                                                                                                                                                                                                                                                                                                               do 433 k = 1, 4
                                                                                                                                                                                                                                                                                                                  do 432 l = 1, 6
                                                                                                                                                                                                                                                                                                                     do 431 m = 1, 8
                                                                                                                                                                                                                                                                                                                        n = n + 1
                                                                                                                                                                                                                                                                                                                        if (int(buffer(n) / (real(i) * real(j) * real(k) * real(l) *&
                                                                                                                                                                                                                                                                                                                        &real(m)) - 1) /= 0) print *, 'error, i= ', i
431                                                                                                                                                                                                                                                                                                                     continue
432                                                                                                                                                                                                                                                                                                                     continue
433                                                                                                                                                                                                                                                                                                                     continue
434                                                                                                                                                                                                                                                                                                                     continue
435                                                                                                                                                                                                                                                                                                                     continue
                                                                                                                                                                                                                                                                                                                        call clock(cpu2)
                                                                                                                                                                                                                                                                                                                        elap_r = elap_r + cpu2 - cpu1
                                                                                                                                                                                                                                                                                                                        write (file_unit, '(''DATAGRP_TEST_3A'',&
                                                                                                                                                                                                                                                                                                                        &          '' read in [sec]'',1PE13.5)') cpu2 - cpu1

                                                                                                                                                                                                                                                                                                                        write (file_unit, '(&
                                                                                                                                                                                                                                                                                                                        &  ''Lees 6720 elementen van 4 bytes in een (1) opdracht'')')
                                                                                                                                                                                                                                                                                                                        call clock(cpu1)
                                                                                                                                                                                                                                                                                                                        error = Getelt(fds, 'DATAGRP_TEST_3B',&
                                                                                                                                                                                                                                                                                                                        &'ELEM_R_4', UINDEX, usrord, 26880 * 4, buffer)
                                                                                                                                                                                                                                                                                                                        if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
                                                                                                                                                                                                                                                                                                                        n = 0
                                                                                                                                                                                                                                                                                                                        do 445 i = 1, 7
                                                                                                                                                                                                                                                                                                                           do 444 j = 1, 5
                                                                                                                                                                                                                                                                                                                              do 443 k = 1, 4
                                                                                                                                                                                                                                                                                                                                 do 442 l = 1, 6
                                                                                                                                                                                                                                                                                                                                    do 441 m = 1, 8
                                                                                                                                                                                                                                                                                                                                       n = n + 1
                                                                                                                                                                                                                                                                                                                                       if (int(buffer(n) / (real(i) * real(j) * real(k) * real(l) *&
                                                                                                                                                                                                                                                                                                                                       &real(m)) - 2) /= 0) print *, 'error, i= ', i
441                                                                                                                                                                                                                                                                                                                                    continue
442                                                                                                                                                                                                                                                                                                                                    continue
443                                                                                                                                                                                                                                                                                                                                    continue
444                                                                                                                                                                                                                                                                                                                                    continue
445                                                                                                                                                                                                                                                                                                                                    continue
                                                                                                                                                                                                                                                                                                                                       call clock(cpu2)
                                                                                                                                                                                                                                                                                                                                       elap_r = elap_r + cpu2 - cpu1
                                                                                                                                                                                                                                                                                                                                       write (file_unit, '(''DATAGRP_TEST_3B'',&
                                                                                                                                                                                                                                                                                                                                       &          '' read in [sec]'',1PE13.5)') cpu2 - cpu1

                                                                                                                                                                                                                                                                                                                                       write (file_unit, '(&
                                                                                                                                                                                                                                                                                                                                       &  ''Lees 6720 elementen van 4 bytes in een (1) opdracht'')')
                                                                                                                                                                                                                                                                                                                                       call clock(cpu1)
                                                                                                                                                                                                                                                                                                                                       error = Getelt(fds, 'DATAGRP_TEST_3C',&
                                                                                                                                                                                                                                                                                                                                       &'ELEM_R_4', UINDEX, usrord, 26880 * 4, buffer)
                                                                                                                                                                                                                                                                                                                                       if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
                                                                                                                                                                                                                                                                                                                                       n = 0
                                                                                                                                                                                                                                                                                                                                       do 455 i = 1, 7
                                                                                                                                                                                                                                                                                                                                          do 454 j = 1, 5
                                                                                                                                                                                                                                                                                                                                             do 453 k = 1, 4
                                                                                                                                                                                                                                                                                                                                                do 452 l = 1, 6
                                                                                                                                                                                                                                                                                                                                                   do 451 m = 1, 8
                                                                                                                                                                                                                                                                                                                                                      n = n + 1
                                                                                                                                                                                                                                                                                                                                                      if (int(buffer(n) / (real(i) * real(j) * real(k) * real(l) *&
                                                                                                                                                                                                                                                                                                                                                      &real(m)) - 3) /= 0) print *, 'error, i= ', i
451                                                                                                                                                                                                                                                                                                                                                   continue
452                                                                                                                                                                                                                                                                                                                                                   continue
453                                                                                                                                                                                                                                                                                                                                                   continue
454                                                                                                                                                                                                                                                                                                                                                   continue
455                                                                                                                                                                                                                                                                                                                                                   continue
                                                                                                                                                                                                                                                                                                                                                      call clock(cpu2)
                                                                                                                                                                                                                                                                                                                                                      elap_r = elap_r + cpu2 - cpu1
                                                                                                                                                                                                                                                                                                                                                      write (file_unit, '(''DATAGRP_TEST_3C'',&
                                                                                                                                                                                                                                                                                                                                                      &          '' read in [sec]'',1PE13.5)') cpu2 - cpu1

                                                                                                                                                                                                                                                                                                                                                      write (file_unit, '(&
                                                                                                                                                                                                                                                                                                                                                      &  ''Lees 6720 elementen van 4 bytes in een (1) opdracht'')')
                                                                                                                                                                                                                                                                                                                                                      call clock(cpu1)
                                                                                                                                                                                                                                                                                                                                                      error = Getelt(fds, 'DATAGRP_TEST_3D',&
                                                                                                                                                                                                                                                                                                                                                      &'ELEM_R_4', UINDEX, usrord, 26880 * 4, buffer)
                                                                                                                                                                                                                                                                                                                                                      if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
                                                                                                                                                                                                                                                                                                                                                      n = 0
                                                                                                                                                                                                                                                                                                                                                      do 4445 i = 1, 7
                                                                                                                                                                                                                                                                                                                                                         do 4444 j = 1, 5
                                                                                                                                                                                                                                                                                                                                                            do 4443 k = 1, 4
                                                                                                                                                                                                                                                                                                                                                               do 4442 l = 1, 6
                                                                                                                                                                                                                                                                                                                                                                  do 4441 m = 1, 8
                                                                                                                                                                                                                                                                                                                                                                     n = n + 1
                                                                                                                                                                                                                                                                                                                                                                     if (int(buffer(n) / (real(i) * real(j) * real(k) * real(l) *&
                                                                                                                                                                                                                                                                                                                                                                     &real(m)) - 4) /= 0) print *, 'error, i= ', i
4441                                                                                                                                                                                                                                                                                                                                                                 continue
4442                                                                                                                                                                                                                                                                                                                                                                 continue
4443                                                                                                                                                                                                                                                                                                                                                                 continue
4444                                                                                                                                                                                                                                                                                                                                                                 continue
4445                                                                                                                                                                                                                                                                                                                                                                 continue
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
                                                                                                                                                                                                                                                                                                                                                                     if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
                                                                                                                                                                                                                                                                                                                                                                     n = 0
                                                                                                                                                                                                                                                                                                                                                                     do 486 i = 1, 7
                                                                                                                                                                                                                                                                                                                                                                        do 485 j = 1, 5
                                                                                                                                                                                                                                                                                                                                                                           do 484 k = 1, 4
                                                                                                                                                                                                                                                                                                                                                                              do 483 l = 1, 6
                                                                                                                                                                                                                                                                                                                                                                                 do 482 m = 1, 8
                                                                                                                                                                                                                                                                                                                                                                                    do 481 im = 1, 3
                                                                                                                                                                                                                                                                                                                                                                                       n = n + 1
                                                                                                                                                                                                                                                                                                                                                                                       if (int(buffer(n) / (real(i) * real(j) * real(k) * real(l) *&
                                                                                                                                                                                                                                                                                                                                                                                       &real(m) * real(im)) - 1) /= 0) print *, 'error, i= ', i
481                                                                                                                                                                                                                                                                                                                                                                                    continue
482                                                                                                                                                                                                                                                                                                                                                                                    continue
483                                                                                                                                                                                                                                                                                                                                                                                    continue
484                                                                                                                                                                                                                                                                                                                                                                                    continue
485                                                                                                                                                                                                                                                                                                                                                                                    continue
486                                                                                                                                                                                                                                                                                                                                                                                    continue
                                                                                                                                                                                                                                                                                                                                                                                       call clock(cpu2)
                                                                                                                                                                                                                                                                                                                                                                                       elap_r = elap_r + cpu2 - cpu1
                                                                                                                                                                                                                                                                                                                                                                                       write (file_unit, '(''DATAGRP_TEST_3A'',&
                                                                                                                                                                                                                                                                                                                                                                                       &          '' read in [sec]'',1PE13.5)') cpu2 - cpu1

                                                                                                                                                                                                                                                                                                                                                                                       write (file_unit, '(&
                                                                                                                                                                                                                                                                                                                                                                                       &  ''Lees 6720 elementen van 12 bytes in een (1) opdracht'')')
                                                                                                                                                                                                                                                                                                                                                                                       call clock(cpu1)
                                                                                                                                                                                                                                                                                                                                                                                       error = Getelt(fds, 'DATAGRP_TEST_3B',&
                                                                                                                                                                                                                                                                                                                                                                                       &'ELEM_R_4_DIM_1', UINDEX, usrord, 26880 * 4, buffer)
                                                                                                                                                                                                                                                                                                                                                                                       if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
                                                                                                                                                                                                                                                                                                                                                                                       n = 0
                                                                                                                                                                                                                                                                                                                                                                                       do 466 i = 1, 7
                                                                                                                                                                                                                                                                                                                                                                                          do 465 j = 1, 5
                                                                                                                                                                                                                                                                                                                                                                                             do 464 k = 1, 4
                                                                                                                                                                                                                                                                                                                                                                                                do 463 l = 1, 6
                                                                                                                                                                                                                                                                                                                                                                                                   do 462 m = 1, 8
                                                                                                                                                                                                                                                                                                                                                                                                      do 461 im = 1, 3
                                                                                                                                                                                                                                                                                                                                                                                                         n = n + 1
                                                                                                                                                                                                                                                                                                                                                                                                         if (int(buffer(n) / (real(i) * real(j) * real(k) * real(l) *&
                                                                                                                                                                                                                                                                                                                                                                                                         &real(m) * real(im)) - 2) /= 0) print *, 'error, i= ', i
461                                                                                                                                                                                                                                                                                                                                                                                                      continue
462                                                                                                                                                                                                                                                                                                                                                                                                      continue
463                                                                                                                                                                                                                                                                                                                                                                                                      continue
464                                                                                                                                                                                                                                                                                                                                                                                                      continue
465                                                                                                                                                                                                                                                                                                                                                                                                      continue
466                                                                                                                                                                                                                                                                                                                                                                                                      continue
                                                                                                                                                                                                                                                                                                                                                                                                         call clock(cpu2)
                                                                                                                                                                                                                                                                                                                                                                                                         elap_r = elap_r + cpu2 - cpu1
                                                                                                                                                                                                                                                                                                                                                                                                         write (file_unit, '(''DATAGRP_TEST_3B'',&
                                                                                                                                                                                                                                                                                                                                                                                                         &          '' read in [sec]'',1PE13.5)') cpu2 - cpu1

                                                                                                                                                                                                                                                                                                                                                                                                         write (file_unit, '(&
                                                                                                                                                                                                                                                                                                                                                                                                         &  ''Lees 6720 elementen van 12 bytes in een (1) opdracht'')')
                                                                                                                                                                                                                                                                                                                                                                                                         call clock(cpu1)
                                                                                                                                                                                                                                                                                                                                                                                                         error = Getelt(fds, 'DATAGRP_TEST_3C',&
                                                                                                                                                                                                                                                                                                                                                                                                         &'ELEM_R_4_DIM_1', UINDEX, usrord, 26880 * 4, buffer)
                                                                                                                                                                                                                                                                                                                                                                                                         if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
                                                                                                                                                                                                                                                                                                                                                                                                         n = 0
                                                                                                                                                                                                                                                                                                                                                                                                         do 476 i = 1, 7
                                                                                                                                                                                                                                                                                                                                                                                                            do 475 j = 1, 5
                                                                                                                                                                                                                                                                                                                                                                                                               do 474 k = 1, 4
                                                                                                                                                                                                                                                                                                                                                                                                                  do 473 l = 1, 6
                                                                                                                                                                                                                                                                                                                                                                                                                     do 472 m = 1, 8
                                                                                                                                                                                                                                                                                                                                                                                                                        do 471 im = 1, 3
                                                                                                                                                                                                                                                                                                                                                                                                                           n = n + 1
                                                                                                                                                                                                                                                                                                                                                                                                                           if (int(buffer(n) / (real(i) * real(j) * real(k) * real(l) *&
                                                                                                                                                                                                                                                                                                                                                                                                                           &real(m) * real(im)) - 3) /= 0) print *, 'error, i= ', i
471                                                                                                                                                                                                                                                                                                                                                                                                                        continue
472                                                                                                                                                                                                                                                                                                                                                                                                                        continue
473                                                                                                                                                                                                                                                                                                                                                                                                                        continue
474                                                                                                                                                                                                                                                                                                                                                                                                                        continue
475                                                                                                                                                                                                                                                                                                                                                                                                                        continue
476                                                                                                                                                                                                                                                                                                                                                                                                                        continue
                                                                                                                                                                                                                                                                                                                                                                                                                           call clock(cpu2)
                                                                                                                                                                                                                                                                                                                                                                                                                           elap_r = elap_r + cpu2 - cpu1
                                                                                                                                                                                                                                                                                                                                                                                                                           write (file_unit, '(''DATAGRP_TEST_3C'',&
                                                                                                                                                                                                                                                                                                                                                                                                                           &          '' read in [sec]'',1PE13.5)') cpu2 - cpu1

                                                                                                                                                                                                                                                                                                                                                                                                                           write (file_unit, '(&
                                                                                                                                                                                                                                                                                                                                                                                                                           &  ''Lees 6720 elementen van 12 bytes in een (1) opdracht'')')
                                                                                                                                                                                                                                                                                                                                                                                                                           call clock(cpu1)
                                                                                                                                                                                                                                                                                                                                                                                                                           error = Getelt(fds, 'DATAGRP_TEST_3D',&
                                                                                                                                                                                                                                                                                                                                                                                                                           &'ELEM_R_4_DIM_1', UINDEX, usrord, 26880 * 4, buffer)
                                                                                                                                                                                                                                                                                                                                                                                                                           if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
                                                                                                                                                                                                                                                                                                                                                                                                                           n = 0
                                                                                                                                                                                                                                                                                                                                                                                                                           do 4466 i = 1, 7
                                                                                                                                                                                                                                                                                                                                                                                                                              do 4465 j = 1, 5
                                                                                                                                                                                                                                                                                                                                                                                                                                 do 4464 k = 1, 4
                                                                                                                                                                                                                                                                                                                                                                                                                                    do 4463 l = 1, 6
                                                                                                                                                                                                                                                                                                                                                                                                                                       do 4462 m = 1, 8
                                                                                                                                                                                                                                                                                                                                                                                                                                          do 4461 im = 1, 3
                                                                                                                                                                                                                                                                                                                                                                                                                                             n = n + 1
                                                                                                                                                                                                                                                                                                                                                                                                                                             if (int(buffer(n) / (real(i) * real(j) * real(k) * real(l) *&
                                                                                                                                                                                                                                                                                                                                                                                                                                             &real(m) * real(im)) - 4) /= 0) print *, 'error, i= ', i
4461                                                                                                                                                                                                                                                                                                                                                                                                                                         continue
4462                                                                                                                                                                                                                                                                                                                                                                                                                                         continue
4463                                                                                                                                                                                                                                                                                                                                                                                                                                         continue
4464                                                                                                                                                                                                                                                                                                                                                                                                                                         continue
4465                                                                                                                                                                                                                                                                                                                                                                                                                                         continue
4466                                                                                                                                                                                                                                                                                                                                                                                                                                         continue
                                                                                                                                                                                                                                                                                                                                                                                                                                             call clock(cpu2)
                                                                                                                                                                                                                                                                                                                                                                                                                                             elap_r = elap_r + cpu2 - cpu1
                                                                                                                                                                                                                                                                                                                                                                                                                                             write (file_unit, '(''DATAGRP_TEST_3D'',&
                                                                                                                                                                                                                                                                                                                                                                                                                                             &          '' read in [sec]'',1PE13.5)') cpu2 - cpu1

                                                                                                                                                                                                                                                                                                                                                                                                                                             error = Clsdat(fds)
                                                                                                                                                                                                                                                                                                                                                                                                                                             if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)

                                                                                                                                                                                                                                                                                                                                                                                                                                             error = Clsdef(fds)
                                                                                                                                                                                                                                                                                                                                                                                                                                             if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)

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

                                                                                                                                                                                                                                                                                                                                                                                                                                             end module m_nefis_test_06
