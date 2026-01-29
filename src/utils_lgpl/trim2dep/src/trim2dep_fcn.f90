!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2026.
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
    subroutine trim2dep_fcn(simfil, depfil, lundia, filetype, fout, &
        & grpnam, elmnam, simidx, nanval)
       !
       !=============================================================
       ! DEFINE VARIABLES
       !-------------------------------------------------------------
       !
       implicit none
       !
       ! External INTEGER functions
       !
       integer, external :: OPEN_DATDEF
       integer, external :: CLSNEF

       integer, external :: inqfel
       integer, external :: inqnel
       !
       integer, external :: inqelm
       integer, external :: defelm
       !
       integer, external :: inqcel
       integer, external :: defcel
       !
       integer, external :: inqgrp
       integer, external :: defgrp
       !
       integer, external :: inqdat
       integer, external :: credat
       integer, external :: inqmxi
       !
       integer, external :: getelt
       integer, external :: putelt
       !
       integer, external :: neferr

       !
       ! CHARACTER variables
       !
       character*(*) :: filetype
       character*(*) :: grpnam
       character*(*) :: elmnam
       character*(*) :: simfil
       character*(*) :: depfil

       character*16 :: elmnam2
       character*16 :: grpnam2
       character*16 :: grpdef
       character*16 :: elmqty
       character*16 :: elmunt
       character*64 :: elmdes
       character*8 :: elmtyp
       character*16 :: celnam
       character*16, pointer :: elmnms(:)
       character*1024 :: error_string
       character*64 :: errmsg
       !
       ! INTEGER variables
       !
       integer :: idx
       integer :: nidx
       integer :: idx1
       integer :: idx2
       integer :: midx1
       integer :: simidx
       integer :: lundia
       integer :: ierr
       integer :: fds1
       integer :: fdsdep
       integer :: elmndm
       integer :: elmdms(5)
       integer :: bytes
       integer :: grpndm
       integer :: grpdms(5)
       integer :: grpord(5)
       integer :: fdim
       integer :: i
       integer :: j
       integer :: d
       integer :: grpsz
       integer :: buflen
       integer :: uidx1(3, 5)
       integer :: usrord(5)
       integer :: nelems
       integer :: numbyt

       !
       ! LOGICAL variables
       !
       logical :: fout
       !

       !
       ! POINTER variables
       !
       byte, pointer :: barray(:)
       integer, pointer :: iarray(:, :, :, :, :)
       integer, pointer :: kcs(:, :, :, :, :)
       real, pointer :: rarray(:, :, :, :, :)
       real, pointer :: rarray2(:, :, :, :, :)
       real :: nanval

       !-------------------------------------------------------------
       ! end of DEFINE VARIABLES
       !=============================================================
       !
       ! beginning of actual commands
       !
       idx1 = simidx
       !
       ! Get definition name of group.
       !
       !

       !===========================================================
       ! OPEN TARGET DEPTH FILE
       !-----------------------------------------------------------
       !
       errmsg = 'Error opening depth file'
       open (newunit=fdsdep, file=depfil, form='formatted', err=9999)
       !write(*,*) 'Open unit', fdsdep
       !===========================================================
       ! READ DATA FROM SIM FILE
       !-----------------------------------------------------------
       !
       ierr = open_datdef(simfil, fds1)
       if (ierr /= 0) goto 9997

       grpnam2 = grpnam
       grpnam = 'map-const'
       idx2 = idx1
       idx1 = -1
       !
       ! Get definition name of group.
       !
       ierr = inqdat(fds1, grpnam, grpdef)
       if (ierr /= 0) goto 9997
       !
       ! Get dimensions of the group definition and cell name.
       !
       grpndm = 5
       ierr = inqgrp(fds1, grpdef, celnam, grpndm, grpdms, grpord)
       if (ierr /= 0) goto 9997
       !
       ! Find index of free group dimension
       !
       fdim = 0
       do i = 1, grpndm
          if (grpdms(i) == 0) then
             fdim = i
             exit
          end if
       end do
       !
       if (fdim /= 0) then
          if (idx1 == 0) then
             write (*, *) 'Group index 0 not allowed as reading index'
             goto 9999
          else
             !
             ! Obtain maximum used index of free group dimension
             !
             ierr = inqmxi(fds1, grpnam, midx1)
             !
             if (idx1 < 0) then
                !
                ! count back index
                !
                if (-idx1 > midx1) then
                   write (*, '(A,I3,A,I3,A)') 'Negative group index ', idx1, &
                       & ' bigger than number of indices in use (', midx1, ')'
                   goto 9999
                end if
                idx1 = midx1 + 1 + idx1
             else
                !
                ! direct index
                !
                if (idx1 > midx1) then
                   write (*, '(A,I3,A,I3,A)') 'Group index ', idx1, &
                       & ' bigger than number of indices in use (', midx1, ')'
                   goto 9999
                end if
             end if
             !
          end if
       end if
       !
       ! Count number of elements to determine the maximum number of
       ! elements in the group.
       !
       nelems = 0
       elmndm = 5
       ierr = inqfel(fds1, elmnam2, elmtyp, elmqty, elmunt, elmdes, bytes, numbyt, elmndm, elmdms)
       !
       ! write(*,*) elmnam
       !
       do while (ierr == 0)
          nelems = nelems + 1
          elmndm = 5
          ierr = inqnel(fds1, elmnam2, elmtyp, elmqty, elmunt, elmdes, bytes, numbyt, elmndm, elmdms)
          if (ierr /= 0) exit
          !
          !write(*,*) nelems
          !
       end do
       !
       ! write(*,*) 'Total number of elements = ',nelems
       !
       ! Inquire which elements form the cell/group.
       !
       allocate (elmnms(nelems))
       ierr = inqcel(fds1, celnam, nelems, elmnms)
       if (ierr /= 0) goto 9997
       !
       !write(*,'(A)') elmnms(1:nelems)
       !
       ! Initialize the group indices.
       !
       do i = 1, grpndm
          uidx1(1, i) = 1
          uidx1(2, i) = grpdms(i)
          uidx1(3, i) = 1
          usrord(i) = i
       end do
       !
       if (fdim /= 0) then
          uidx1(1, fdim) = idx1
          uidx1(2, fdim) = idx1
       end if
       !
       grpsz = 1
       do i = 1, grpndm
          grpsz = grpsz * (uidx1(2, i) - uidx1(1, i) + 1)
       end do
       !
       ! Get KCS - active cells
       !
       do i = 1, nelems
          !
          !
          !write(*,*) elmnms(i)
          if (trim(elmnms(i)) == trim('KCS')) then
             elmndm = 5
             ierr = inqelm(fds1, elmnms(i), elmtyp, bytes, elmqty, elmunt, elmdes, elmndm, elmdms)
             if (ierr /= 0) goto 9997
             !
             if (elmtyp(1:7) == 'INTEGER') then
                buflen = bytes * grpsz
                do d = 1, elmndm
                   buflen = buflen * elmdms(d)
                end do
                allocate (kcs(elmdms(1), elmdms(2), elmdms(3), elmdms(4), elmdms(5)))
                !
                ierr = getelt(fds1, grpnam, elmnms(i), uidx1, usrord, buflen, kcs)
                if (ierr /= 0) goto 9997
             else
                buflen = bytes * grpsz
                do d = 1, elmndm
                   buflen = buflen * elmdms(d)
                end do
                allocate (barray(buflen))
                !
                ierr = getelt(fds1, grpnam, elmnms(i), uidx1, usrord, buflen, barray)
                if (ierr /= 0) goto 9997
                !
                ! write(*,*) elmnms(i)
                deallocate (barray)
             end if
          end if
          !
       end do

       !================================================
       ! GET OTHER AS DEFINED IN FILE.
       !------------------------------------------------
       !

       grpnam = grpnam2
       idx1 = idx2

       !
       ! Get definition name of group.
       !
       ierr = inqdat(fds1, grpnam, grpdef)
       if (ierr /= 0) goto 9997
       !
       ! Get dimensions of the group definition and cell name.
       !
       grpndm = 5
       ierr = inqgrp(fds1, grpdef, celnam, grpndm, grpdms, grpord)
       if (ierr /= 0) goto 9997
       !
       ! Find index of free group dimension
       !
       fdim = 0
       do i = 1, grpndm
          if (grpdms(i) == 0) then
             fdim = i
             exit
          end if
       end do
       !
       if (fdim /= 0) then
          if (idx1 == 0) then
             write (*, *) 'Group index 0 not allowed as reading index'
             goto 9999
          else
             !
             ! Obtain maximum used index of free group dimension
             !
             ierr = inqmxi(fds1, grpnam, midx1)
             !
             if (idx1 < 0) then
                !
                ! count back index
                !
                if (-idx1 > midx1) then
                   write (*, '(A,I3,A,I3,A)') 'Negative group index ', idx1, &
                       & ' bigger than number of indices in use (', midx1, ')'
                   goto 9999
                end if
                idx1 = midx1 + 1 + idx1
             else
                !
                ! direct index
                !
                if (idx1 > midx1) then
                   write (*, '(A,I3,A,I3,A)') 'Group index ', idx1, &
                       & ' bigger than number of indices in use (', midx1, ')'
                   goto 9999
                end if
             end if
             !
          end if
       end if
       !
       ! Count number of elements to determine the maximum number of
       ! elements in the group.
       !
       nelems = 0
       elmndm = 5
       ierr = inqfel(fds1, elmnam2, elmtyp, elmqty, elmunt, elmdes, bytes, numbyt, elmndm, elmdms)
       !
       ! write(*,*) elmnam
       !
       do while (ierr == 0)
          nelems = nelems + 1
          elmndm = 5
          ierr = inqnel(fds1, elmnam2, elmtyp, elmqty, elmunt, elmdes, bytes, numbyt, elmndm, elmdms)
          if (ierr /= 0) exit
          !
          !write(*,*) nelems
          !
       end do
       !
       ! write(*,*) 'Total number of elements = ',nelems
       !
       ! Inquire which elements form the cell/group.
       !
       if (associated(elmnms)) deallocate (elmnms)
       allocate (elmnms(nelems))
       ierr = inqcel(fds1, celnam, nelems, elmnms)
       if (ierr /= 0) goto 9997
       !
       !write(*,'(A)') elmnms(1:nelems)
       !
       ! Initialize the group indices.
       !
       do i = 1, grpndm
          uidx1(1, i) = 1
          uidx1(2, i) = grpdms(i)
          uidx1(3, i) = 1
          usrord(i) = i
       end do
       !
       if (fdim /= 0) then
          uidx1(1, fdim) = idx1
          uidx1(2, fdim) = idx1
       end if
       !
       grpsz = 1
       do i = 1, grpndm
          grpsz = grpsz * (uidx1(2, i) - uidx1(1, i) + 1)
       end do

       do i = 1, nelems
          if (trim(elmnms(i)) == trim(elmnam)) then
             elmndm = 5
             ierr = inqelm(fds1, elmnms(i), elmtyp, bytes, elmqty, elmunt, elmdes, elmndm, elmdms)
             if (ierr /= 0) goto 9997
             !
             if (elmtyp(1:4) == 'REAL') then
                buflen = bytes * grpsz
                do d = 1, elmndm
                   buflen = buflen * elmdms(d)
                end do
                allocate (rarray(elmdms(1), elmdms(2), elmdms(3), elmdms(4), elmdms(5)))
                !
                ierr = getelt(fds1, grpnam, elmnms(i), uidx1, usrord, buflen, rarray)
                if (ierr /= 0) goto 9997

                allocate (rarray2(elmdms(1), elmdms(2), elmdms(3), elmdms(4), elmdms(5)))
                rarray2 = merge(rarray2(:, :, :, :, :), rarray, kcs > 0)
                rarray2 = merge(nanval, rarray, kcs < 1)
                do j = 1, elmdms(1)
                   write (fdsdep, "(12ES17.7)") rarray2(j, :, :, :, :)
                end do
                deallocate (rarray2)
                deallocate (rarray)

             elseif (elmtyp(1:7) == 'INTEGER') then
                buflen = bytes * grpsz
                do d = 1, elmndm
                   buflen = buflen * elmdms(d)
                end do
                allocate (iarray(elmdms(1), elmdms(2), elmdms(3), elmdms(4), elmdms(5)))
                !
                ierr = getelt(fds1, grpnam, elmnms(i), uidx1, usrord, buflen, iarray)
                if (ierr /= 0) goto 9997
                do j = 1, elmdms(1)
                   write (fdsdep, "(12I7)") iarray(j, :, :, :, :)
                end do

                deallocate (iarray)
             else
                buflen = bytes * grpsz
                do d = 1, elmndm
                   buflen = buflen * elmdms(d)
                end do
                allocate (barray(buflen))
                !
                ierr = getelt(fds1, grpnam, elmnms(i), uidx1, usrord, buflen, barray)
                if (ierr /= 0) goto 9997
                !
                write (fdsdep, *) barray
                !
                deallocate (barray)
             end if
          end if

       end do
       !
       ! ERROR HANDLING
       !
       fout = .false.
       !
9997   continue
       if (ierr /= 0) then
          write (*, *) 'Nefis error:', ierr
          ierr = neferr(0, error_string)
          write (*, *) trim(error_string)
       end if
       !
       ! deallocate arrays
       !
       if (associated(elmnms)) deallocate (elmnms)
       if (associated(barray)) deallocate (barray)
       if (associated(kcs)) deallocate (kcs)

       !call copygroup(fds1,fds2,grpnam,simidx,tgtidx,fout)
       !if (fout) goto 9999
       !
       ierr = clsnef(fds1)

       !
       !-----------------------------------------------------------
       ! end of READ DATA FROM SIM FILE
       !===========================================================
       !

       !
       ! ERROR HANDLING
       !
       if (fout) goto 9999
       errmsg = 'Operations completed successfully'
9998   continue
       write (lundia, '(1X,A)') errmsg
9999   continue
       !write(*,*) 'Close unit', fdsdep
       close (fdsdep)
    end subroutine trim2dep_fcn
