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
subroutine copygroup(fds1, fds2, grp, in_idx1, in_idx2, fout)
   !
   !=============================================================
   ! FUNCTIONAL DESCRIPTION
   !-------------------------------------------------------------
   !
   ! Generically copy all elements from group GRP in nefis file
   ! FDS1 to group GRP in nefis file FDS2. In case of a free
   ! dimension, the index IN_IDX1 is read from file FDS1 and
   ! stored as IN_IDX2 on file FDS2. The indices can be:
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
   integer :: midx2
   integer :: grpsz
   integer :: buflen
   integer :: bytes
   integer :: numbyt
   integer :: elmndm
   integer :: elmdms(5)
   integer :: ierr
   integer, intent(in) :: fds1
   integer, intent(in) :: fds2
   integer :: fdim
   integer :: grpndm
   integer :: grpdms(5)
   integer :: grpord(5)
   integer :: usrord(5)
   integer :: uidx1(3, 5)
   integer :: uidx2(3, 5)
   integer :: nelems
   !
   ! LOGICAL variables
   !
   logical :: group_exists
   logical, intent(out) :: fout
   !
   ! BYTE variables
   !
   byte, pointer :: barray(:)
   !
   !-------------------------------------------------------------
   ! end of DEFINE VARIABLES
   !=============================================================
   !
   fout = .true.
   nullify (elmnms)
   nullify (barray)
   !
   idx1 = in_idx1
   idx2 = in_idx2
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
   ierr = inqdat(fds1, grpnam, grpdef)
   if (ierr /= 0) goto 9999
   !
   ! Get dimensions of the group definition and cell name.
   !
   grpndm = 5
   ierr = inqgrp(fds1, grpdef, celnam, grpndm, grpdms, grpord)
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
   ! Count number of elements to determine the maximum number of
   ! elements in the group.
   !
   nelems = 0
   elmndm = 5
   ierr = inqfel(fds1, elmnam, elmtyp, elmqty, elmunt, elmdes, bytes, numbyt, elmndm, elmdms)
   !
   ! write(*,*) elmnam
   !
   do while (ierr == 0)
      nelems = nelems + 1
      elmndm = 5
      ierr = inqnel(fds1, elmnam, elmtyp, elmqty, elmunt, elmdes, bytes, numbyt, elmndm, elmdms)
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
   ierr = inqcel(fds1, celnam, nelems, elmnms)
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
         ierr = inqelm(fds1, elmnms(i), elmtyp, bytes, elmqty, elmunt, elmdes, elmndm, elmdms)
         if (ierr /= 0) goto 9999
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
      ierr = inqelm(fds1, elmnms(i), elmtyp, bytes, elmqty, elmunt, elmdes, elmndm, elmdms)
      if (ierr /= 0) goto 9999
      !
      buflen = bytes * grpsz
      do d = 1, elmndm
         buflen = buflen * elmdms(d)
      end do
      allocate (barray(buflen))
      !
      ierr = getelt(fds1, grpnam, elmnms(i), uidx1, usrord, buflen, barray)
      if (ierr /= 0) goto 9999
      ierr = putelt(fds2, grpnam, elmnms(i), uidx2, usrord, barray)
      if (ierr /= 0) goto 9999
      !
      deallocate (barray)
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
end subroutine copygroup
