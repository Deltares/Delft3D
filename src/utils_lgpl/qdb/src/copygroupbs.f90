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
subroutine copygroupbs(fds1, fds2, fds3, grp, in_idx1, in_idx2, in_idx3, fout, mlim, nlim)
   !
   !=============================================================
   ! FUNCTIONAL DESCRIPTION
   !-------------------------------------------------------------
   !
   ! Generically copy all elements from group GRP in nefis file
   ! FDS1 to group GRP in nefis file FDS2 based on the size of FDS3.
   ! FDS2 = BIG / FDS1 = FDS3 = SMALL
   ! In case of a free dimension, the index IN_IDX1 is read from file
   ! FDS1 and stored as IN_IDX2 on file FDS2. The indices can be:
   !
   !    * positive: direct index number 1,2,3,..,N
   !    * negative: reversely counted index number -N,..,-3,-2,-1
   !    * zero    : next index number N+1
   !
   ! The zero option and positive value N+1 are only allow for
   ! IN_IDX2, i.e. as writing index.
   !
   !-------------------------------------------------------------
   ! end of FUNCTIONAL DESCRIPTION
   !=============================================================
   !
   !=============================================================
   ! DEFINE VARIABLES
   !-------------------------------------------------------------
   !
   implicit none
   !
   ! External INTEGER functions
   !
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
   character*(*), intent(in) :: grp
   character*16 :: grpnam
   character*16 :: grpdef
   character*16 :: celnam
   character*16 :: elmnam
   character*16 :: elmqty
   character*16 :: elmunt
   character*64 :: elmdes
   character*8 :: elmtyp
   character*16, pointer :: elmnms(:)
   character*1024 :: error_string
   !
   ! INTEGER variables
   !
   integer :: d
   integer :: i
   integer :: idx1
   integer, intent(in) :: in_idx1
   integer :: midx1
   integer :: idx2
   integer, intent(in) :: in_idx2
   integer, intent(in) :: in_idx3
   integer :: midx2
   integer :: idx3
   integer :: midx3
   integer :: grpsz
   integer :: buflen
   integer :: buflen2
   integer :: bytes
   integer :: numbyt
   integer :: elmndm
   integer :: elmdms(5)
   integer :: elmdms2(5)
   integer :: ierr
   integer, intent(in) :: fds1
   integer, intent(in) :: fds2
   integer, intent(in) :: fds3
   integer :: fdim
   integer :: grpndm
   integer :: grpdms(5)
   integer :: grpord(5)
   integer :: usrord(5)
   integer :: uidx1(3, 5)
   integer :: uidx2(3, 5)
   integer :: uidx3(3, 5)
   integer :: uidx(3, 5)
   integer :: nelems
   integer :: mmax
   integer :: nmax
   integer :: mmax2
   integer :: nmax2
   integer :: mlim(2)
   integer :: nlim(2)
   integer :: dim3, dim4, dim5
!        integer                          :: test(3,4)
!        integer                          :: test2(3,4,7)
   !
   ! LOGICAL variables
   !
   logical :: group_exists
   logical, intent(out) :: fout
   !
   ! POINTER variables
   !
   byte, pointer :: barray(:)
   integer, pointer :: iarray(:, :, :, :, :)
   integer, pointer :: iarray2(:, :, :, :, :)
   real, pointer :: rarray(:, :, :, :, :)
   real, pointer :: rarray2(:, :, :, :, :)
   integer, pointer :: kcs(:, :)
   integer, pointer :: kcsmask(:, :, :, :, :)
   !
   !
   !-------------------------------------------------------------
   ! end of DEFINE VARIABLES
   !=============================================================
   !
   fout = .true.
   nullify (elmnms)
   nullify (barray)
   nullify (iarray)
   nullify (iarray2)
   nullify (rarray)
   nullify (rarray2)

   !---------------------------------------------------------------
   !test = reshape( (/ 11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43 /), shape = (/3, 4/))
   !write(*,*) test(1,:)
   !test2 = spread( test, dim = 2, ncopies =7)
   !test2(1,:,1) = test(1,:)
   !write(*,*) test2(1,:,:)

   !------------------------------------------------------------------
   !Get N,M dimensions
   uidx(1, 1) = 1
   uidx(2, 1) = 1
   uidx(3, 1) = 1
   ierr = getelt(fds1, 'map-const', 'MMAX', uidx, 1, 4, mmax2)
   if (ierr /= 0) goto 9999
   !
   ierr = getelt(fds1, 'map-const', 'NMAX', uidx, 1, 4, nmax2)
   if (ierr /= 0) goto 9999
   !------------------------------------------------------------------

   ierr = getelt(fds3, 'map-const', 'MMAX', uidx, 1, 4, mmax)
   if (ierr /= 0) goto 9999
   !
   ierr = getelt(fds3, 'map-const', 'NMAX', uidx, 1, 4, nmax)
   if (ierr /= 0) goto 9999
   !------------------------------------------------------------------

   allocate (kcs(nmax, mmax))
   ierr = getelt(fds3, 'map-series', 'KCS', uidx, 1, 4 * nmax * mmax, kcs)
!    write(*,*) kcs(1:3,:)

!    kcs = merge(kcs-5, kcs, kcs .GT. 0)
!    write(*,*) kcs(1:3,:)

   idx1 = in_idx1
   idx2 = in_idx2
   idx3 = in_idx3
   !
   ! Check whether group already exists on target file. If the
   ! group exists, then skip the definition.
   !
   grpnam = grp
   ierr = inqdat(fds2, grpnam, grpdef)
   group_exists = (ierr == 0)
   !
   ! Get definition name of group.
   !
   ierr = inqdat(fds3, grpnam, grpdef)
   if (ierr /= 0) goto 9999
   !
   ! Get dimensions of the group definition and cell name.
   !
   grpndm = 5
   ierr = inqgrp(fds3, grpdef, celnam, grpndm, grpdms, grpord)
   if (ierr /= 0) goto 9999
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
   if (fdim /= 0) then
      if (idx3 == 0) then
         write (*, *) 'Group index 0 not allowed as reading index'
         goto 9999
      else
         !
         ! Obtain maximum used index of free group dimension
         !
         ierr = inqmxi(fds3, grpnam, midx3)
         !
         if (idx3 < 0) then
            !
            ! count back index
            !
            if (-idx3 > midx3) then
               write (*, '(A,I3,A,I3,A)') 'Negative group index ', idx3, &
                  & ' bigger than number of indices in use (', midx3, ')'
               goto 9999
            end if
            idx3 = midx3 + 1 + idx3
         else
            !
            ! direct index
            !
            if (idx3 > midx3) then
               write (*, '(A,I3,A,I3,A)') 'Group index ', idx3, &
                  & ' bigger than number of indices in use (', midx3, ')'
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
   ierr = inqfel(fds3, elmnam, elmtyp, elmqty, elmunt, elmdes, bytes, numbyt, elmndm, elmdms)
   !
   ! write(*,*) elmnam
   !
   do while (ierr == 0)
      nelems = nelems + 1
      elmndm = 5
      ierr = inqnel(fds3, elmnam, elmtyp, elmqty, elmunt, elmdes, bytes, numbyt, elmndm, elmdms)
      if (ierr /= 0) exit
      !
      ! write(*,*) elmnam
      !
   end do
   !
   ! write(*,*) 'Total number of elements = ',nelems
   !
   ! Inquire which elements form the cell/group.
   !
   allocate (elmnms(nelems))
   ierr = inqcel(fds3, celnam, nelems, elmnms)
   if (ierr /= 0) goto 9999
   !
   ! write(*,'(A)') elmnms(1:nelems)
   !
   ! Define the elements, cell, and group.
   !
   if (.not. group_exists) then
      !
      ! Define the elements.
      !
      do i = 1, nelems
         !
         ! write(*,*) elmnms(i)
         !
         elmndm = 5
         ierr = inqelm(fds3, elmnms(i), elmtyp, bytes, elmqty, elmunt, elmdes, elmndm, elmdms)
         if (ierr /= 0) goto 9999

!          elmdms2 = elmdms
         !write(*,*) elmdms
!             if (elmdms(1) == nmax .and. elmdms(2) == mmax) then
         !write(*,*) elmnms(i), elmtyp
!                elmdms2(1) = nmax2
!                elmdms2(2) = mmax2
!             endif

         ierr = defelm(fds2, elmnms(i), elmtyp, bytes, elmqty, elmunt, elmdes, elmndm, elmdms)
         if (ierr /= 0 .and. ierr /= 5007) goto 9999
         !
         ! Accept if element is already defined (may happen in another group)
         !
      end do
      !
      ! Define the cell.
      !
      ierr = defcel(fds2, celnam, nelems, elmnms)
      if (ierr /= 0) goto 9999
      !
      ! Define the group definition.
      !
      ierr = defgrp(fds2, grpdef, celnam, grpndm, grpdms, grpord)
      if (ierr /= 0) goto 9999
      !
      ! Create the group.
      !
      ierr = credat(fds2, grpnam, grpdef)
      if (ierr /= 0) goto 9999
      !
   end if
   !
   ! Initialize the group indices.
   !
   do i = 1, grpndm
      uidx1(1, i) = 1
      uidx1(2, i) = grpdms(i)
      uidx1(3, i) = 1
      uidx2(1, i) = 1
      uidx2(2, i) = grpdms(i)
      uidx2(3, i) = 1
      uidx3(1, i) = 1
      uidx3(2, i) = grpdms(i)
      uidx3(3, i) = 1
      usrord(i) = i
   end do
   !
   if (fdim /= 0) then
      !
      ! Obtain maximum used index of free group dimension
      !
      ierr = inqmxi(fds2, grpnam, midx2)
      !
      if (idx2 <= 0) then
         !
         ! count back index
         !
         if (-idx2 > midx2) then
            write (*, '(A,I3,A,I3,A)') 'Negative group index ', idx2, &
               & ' bigger than number of indices in use (', midx2, ')'
            goto 9999
         end if
         idx2 = idx2 + midx2 + 1
      else
         !
         ! direct index (one index bigger allowed: same effect as idx2=0)
         !
         if (idx2 > midx2 + 1) then
            write (*, '(A,I3,A,I3,A)') 'Group index ', idx2, &
              & ' bigger than number of indices in use (', midx2, ')'
            goto 9999
         end if
      end if
      !
      uidx1(1, fdim) = idx1
      uidx1(2, fdim) = idx1
      uidx2(1, fdim) = idx2
      uidx2(2, fdim) = idx2
      uidx3(1, fdim) = idx3
      uidx3(2, fdim) = idx3
   end if
   !
   grpsz = 1
   do i = 1, grpndm
      grpsz = grpsz * (uidx1(2, i) - uidx1(1, i) + 1)
   end do
   !
   ! Copy the elements.
   !
   do i = 1, nelems
      !
      ! write(*,*) elmnms(i)
      !
      elmndm = 5
      ierr = inqelm(fds3, elmnms(i), elmtyp, bytes, elmqty, elmunt, elmdes, elmndm, elmdms)
      if (ierr /= 0) goto 9999
      !
      buflen = bytes * grpsz
      do d = 1, elmndm
         elmdms(d) = max(elmdms(d), 1)
         buflen = buflen * elmdms(d)
      end do
      if (elmdms(1) == nmax .and. elmdms(2) == mmax) then
         if (elmtyp(1:4) == 'REAL') then
            write (*, *) 'REAL', elmnms(i)
            allocate (rarray(elmdms(1), elmdms(2), elmdms(3), elmdms(4), elmdms(5)))
            ierr = getelt(fds3, grpnam, elmnms(i), uidx3, usrord, buflen, rarray)
            if (ierr /= 0) goto 9999
            buflen2 = buflen / nmax / mmax * nmax2 * mmax2

            allocate (rarray2(nmax2, mmax2, elmdms(3), elmdms(4), elmdms(5)))

!               allocate(kcsmask(nmax, mmax, elmdms(3), elmdms(4), elmdms(5)))
!          do dim3=1,elmdms(3)
!             do dim4=1,elmdms(4)
!               do dim5=1,elmdms(5)
!                   kcsmask(:,:,dim3,dim4,dim5) = kcs(:,:)
!                        enddo
!                     enddo
!                   enddo

            ierr = getelt(fds1, grpnam, elmnms(i), uidx1, usrord, buflen2, rarray2)
            if (ierr /= 0) goto 9999
            !           rarray = merge( 4444.0, rarray, kcsmask .GT. 1)
            rarray(1:nmax, 1:mmax, :, :, :) = rarray2(nlim(1):nlim(2), mlim(1):mlim(2), :, :, :)
            ierr = putelt(fds2, grpnam, elmnms(i), uidx2, usrord, rarray)
            if (ierr /= 0) goto 9999
            deallocate (rarray)
            deallocate (rarray2)
!                           deallocate(kcsmask)
         elseif (elmtyp(1:7) == 'INTEGER') then
!               write (*,*) 'INT ', elmnms(i)
            allocate (iarray(elmdms(1), elmdms(2), elmdms(3), elmdms(4), elmdms(5)))
            ierr = getelt(fds3, grpnam, elmnms(i), uidx3, usrord, buflen, iarray)
            if (ierr /= 0) goto 9999
            buflen2 = buflen / nmax / mmax * nmax2 * mmax2
            allocate (iarray2(nmax2, mmax2, elmdms(3), elmdms(4), elmdms(5)))

!               allocate(kcsmask(nmax, mmax, elmdms(3), elmdms(4), elmdms(5)))
!          do dim3=1,elmdms(3)
!             do dim4=1,elmdms(4)
!               do dim5=1,elmdms(5)
!                   kcsmask(:,:,dim3,dim4,dim5) = kcs(:,:)
!                        enddo
!                     enddo
!                   enddo

            ierr = getelt(fds1, grpnam, elmnms(i), uidx1, usrord, buflen2, iarray2)
            if (ierr /= 0) goto 9999
            iarray(1:nmax, 1:mmax, :, :, :) = iarray2(nlim(1):nlim(2), mlim(1):mlim(2), :, :, :)
!                   iarray = merge(iarray2(nlim(1):nlim(2), mlim(1):mlim(2), :, :, :), iarray, kcsmask .GT. 0)
!                   iarray = merge( 8888, iarray, kcsmask .GT. 1)
            ierr = putelt(fds2, grpnam, elmnms(i), uidx2, usrord, iarray)
            if (ierr /= 0) goto 9999
            deallocate (iarray)
            deallocate (iarray2)
!                           deallocate(kcsmask)
         end if
      else
         allocate (barray(buflen))
         !
         ierr = getelt(fds3, grpnam, elmnms(i), uidx3, usrord, buflen, barray)
         if (ierr /= 0) goto 9999
         ierr = putelt(fds2, grpnam, elmnms(i), uidx2, usrord, barray)
         if (ierr /= 0) goto 9999
         deallocate (barray)
      end if
      !
      !
   end do
   !
   ! ERROR HANDLING
   !
   fout = .false.
   !
9999 continue
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
   if (associated(iarray)) deallocate (iarray)
   if (associated(iarray2)) deallocate (iarray2)
   if (associated(rarray)) deallocate (rarray)
   if (associated(rarray2)) deallocate (rarray2)
   if (associated(kcs)) deallocate (kcs)
   if (associated(kcsmask)) deallocate (kcsmask)

end subroutine copygroupbs
