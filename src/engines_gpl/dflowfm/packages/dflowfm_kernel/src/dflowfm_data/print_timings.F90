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

module m_print_timings

   implicit none

   private

   public :: print_timings

contains

!>  print timing information to file
   subroutine print_timings(FNAM, dtime)
#ifdef HAVE_MPI
      use mpi
#endif
      use precision, only: dp
      use m_timer, only: numt, t, tcpu, numcgits, tnams, numtsteps
      use m_partitioninfo, only: jampi, ndomains, my_rank, DFM_COMM_DFMWORLD

      character(len=*), intent(in) :: FNAM !< file name
      real(kind=dp), intent(in) :: dtime !< time

      integer :: ierr
      integer :: i, j, lenstr

      logical :: Lexist

      integer, parameter :: ISTRLEN = 20
      integer :: MFILE

      integer, parameter :: Ntvarlist = 13
      integer, dimension(Ntvarlist), parameter :: itvarlist = (/1, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17/)

      real(kind=dp), dimension(:, :), allocatable :: t_max, t_ave, tcpu_max, tcpu_ave
      integer :: itsol_max

      integer :: jadoit

      character(len=128) :: FORMATSTRING, FORMATSTRINGINT, dum

      allocate (t_max(3, NUMT), t_ave(3, NUMT), tcpu_max(3, NUMT), tcpu_ave(3, NUMT))

      if (jampi == 1) then
#ifdef HAVE_MPI
         call mpi_reduce(t, t_max, 3 * NUMT, MPI_DOUBLE_PRECISION, MPI_MAX, 0, DFM_COMM_DFMWORLD, ierr)
         call mpi_reduce(t, t_ave, 3 * NUMT, MPI_DOUBLE_PRECISION, MPI_SUM, 0, DFM_COMM_DFMWORLD, ierr)
         t_ave = t_ave / dble(ndomains)
         call mpi_reduce(tcpu, tcpu_max, 3 * NUMT, MPI_DOUBLE_PRECISION, MPI_MAX, 0, DFM_COMM_DFMWORLD, ierr)
         call mpi_reduce(tcpu, tcpu_ave, 3 * NUMT, MPI_DOUBLE_PRECISION, MPI_SUM, 0, DFM_COMM_DFMWORLD, ierr)
         tcpu_ave = tcpu_ave / dble(ndomains)
#endif
      else
         t_ave = t
         tcpu_ave = tcpu
         t_max = t
         tcpu_max = tcpu
      end if

!     reduce number of iterations
      if (jampi == 1) then
#ifdef HAVE_MPI
         call mpi_reduce(numcgits, itsol_max, 1, MPI_INTEGER, MPI_MAX, 0, DFM_COMM_DFMWORLD, ierr)
#endif
         jadoit = 0
         if (my_rank == 0) jadoit = 1
      else
         itsol_max = numcgits
         jadoit = 1
      end if

      if (jadoit == 1) then
         inquire (FILE=FNAM, EXIST=Lexist)
         open (newunit=MFILE, FILE=trim(FNAM), access='APPEND')

         if (.not. Lexist) then
!           print header
            lenstr = 4
            do j = 1, ISTRLEN - lenstr
               write (MFILE, '(" ")', advance="no")
            end do
            write (MFILE, '(A)', advance="no") 'time'

            lenstr = 16
            do j = 1, ISTRLEN - lenstr
               write (MFILE, '(" ")', advance="no")
            end do
            write (MFILE, '(A)', advance="no") 'number_of_tsteps'

            do i = 1, Ntvarlist
               lenstr = len_trim(tnams(itvarlist(i)))
               do j = 1, ISTRLEN - (lenstr + 4)
                  write (MFILE, '(" ")', advance="no")
               end do
               write (MFILE, "(A, '_ave')", advance="no") trim(tnams(itvarlist(i)))

               do j = 1, ISTRLEN - (lenstr + 4)
                  write (MFILE, '(" ")', advance="no")
               end do
               write (MFILE, "(A, '_max')", advance="no") trim(tnams(itvarlist(i)))

               do j = 1, ISTRLEN - (lenstr + 8)
                  write (MFILE, '(" ")', advance="no")
               end do
               write (MFILE, "(A, '_CPU_ave')", advance="no") trim(tnams(itvarlist(i)))

               do j = 1, ISTRLEN - (lenstr + 8)
                  write (MFILE, '(" ")', advance="no")
               end do
               write (MFILE, "(A, '_CPU_max')", advance="no") trim(tnams(itvarlist(i)))
            end do

            lenstr = 8
            do j = 1, ISTRLEN - lenstr
               write (MFILE, '(" ")', advance="no")
            end do
            write (MFILE, "(A)", advance="no") 'cg_iters'
            write (MFILE, *)
         end if

!        make format strings
         write (dum, '(I0)') ISTRLEN
         FORMATSTRING = '(E'//trim(adjustl(dum))//'.5, $)'
         FORMATSTRINGINT = '(I'//trim(adjustl(dum))//',   $)'

!        write time
         write (MFILE, trim(FORMATSTRING)) dtime

!        write number of timesteps
         write (MFILE, trim(FORMATSTRINGINT)) numtsteps

!        wite timings
         do i = 1, Ntvarlist
            write (MFILE, FORMATSTRING) t_ave(3, itvarlist(i))
            write (MFILE, FORMATSTRING) t_max(3, itvarlist(i))
            write (MFILE, FORMATSTRING) tcpu_ave(3, itvarlist(i))
            write (MFILE, FORMATSTRING) tcpu_max(3, itvarlist(i))
         end do

!        write number of iterations
         write (MFILE, FORMATSTRINGINT) itsol_max

         write (MFILE, *)

         close (MFILE)
      end if

      if (allocated(t_max)) deallocate (t_max)
      if (allocated(t_ave)) deallocate (t_ave)
      if (allocated(tcpu_max)) deallocate (tcpu_max)
      if (allocated(tcpu_ave)) deallocate (tcpu_ave)

   end subroutine print_timings

end module m_print_timings
