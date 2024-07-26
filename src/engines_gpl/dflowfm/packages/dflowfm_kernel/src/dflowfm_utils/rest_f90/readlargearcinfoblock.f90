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

!>    read Arcinfo data and average it into a smaller array
      subroutine ReadLargeArcInfoBlock(MINP, Mfile, Nfile, istart, iend, jstart, jend, Marray, Narray, RMIS, istep, jstep, D)
         use m_missing
         implicit none

         integer, intent(in) :: Mfile, Nfile !< arcinfo dimensions
         integer, intent(in) :: istart, iend, jstart, jend !< block to be read in file-index numbering
         integer, intent(in) :: Marray, Narray !< sample data array dimensions
         integer, intent(inout) :: MINP !< input file unit number
         double precision, intent(in) :: RMIS !< missing value
         integer, intent(out) :: istep, jstep !< subblock sizes
         real, dimension(Marray, Narray), intent(inout) :: D !< sample data array

         real, dimension(:), allocatable :: dline

         integer :: iarray, jarray, ifile, jfile, ja3
         integer :: isub, jsub
         integer, dimension(:), allocatable :: num

         double precision :: af

         integer :: ierror
         character TEX * 16

         ierror = 1

!     compute subblock sizes
!      istep = max(Mfile/Marray,1)
!      jstep = max(Nfile/Narray,1)
         istep = max((iend - istart + 1) / Marray, 1)
         jstep = max((jend - jstart + 1) / Narray, 1)

!     allocate
         allocate (dline(Mfile))
         allocate (num(Marray))

         D = 0d0

         call readyy(' ', -1d0)
         call readyy('Reading Arcinfo file (press right mouse button to cancel)', 0d0)

!     read last lines outside block
         ifile = 1
         do jfile = Nfile, jstart + Narray * jstep, -1
            read (MINP, *, ERR=101, end=100) ! ( dum, ifile=1,Mfile )

!        check for right mouse button
            call halt3(ja3)
            if (ja3 == 3) then
!           fill remaining array elements with DMISS
               D = DMISS
               goto 1234
            end if

            af = dble(jfile - Nfile) / dble(jstart - Nfile)
            call readyy('Reading Arcinfo file (press right mouse button to cancel)', af)
            write (6, '(1H+"Reading Arcinfo file: ", F7.2, "% done")') 1d2 * af

         end do

         do jarray = Narray, 1, -1
!        read jstep lines and average
            dline = RMIS
            num = 0
            do jsub = jstep, 1, -1
               jfile = jstart - 1 + jsub + jstep * (jarray - 1)
!            if ( jfile.gt.Nfile ) cycle
               if (jfile > jend) cycle
               if (jfile < jstart) cycle

               dline = -1
               read (MINP, *, ERR=101, end=100) (dline(ifile), ifile=1, Mfile)

!           sum the sample values in a subcell
               do iarray = 1, Marray
                  do isub = 1, istep
                     ifile = istart - 1 + isub + istep * (iarray - 1)
!                   if ( ifile.gt.Mfile ) cycle
                     if (ifile > iend) cycle

                     if (dline(ifile) /= RMIS) then
                        num(iarray) = num(iarray) + 1
                        D(iarray, jarray) = D(iarray, jarray) + dline(ifile)
                     end if
                  end do
               end do
            end do

            af = dble(jfile - Nfile) / dble(jstart - Nfile)
            call readyy('Reading Arcinfo file (press right mouse button to cancel)', af)
            write (6, '(1H+"Reading Arcinfo file: ", F7.2, "% done")') 1d2 * af

!        divide by the number of samples in a subcell
            do iarray = 1, Marray
               if (num(iarray) > 0) then
                  D(iarray, jarray) = D(iarray, jarray) / dble(num(iarray))
               else
                  D(iarray, jarray) = DMISS
               end if
            end do

!        check for right mouse button
            call halt3(ja3)
            if (ja3 == 3) then
!           fill remaining array elements with DMISS
               D(1:Marray, jarray - 1:1:-1) = DMISS
               goto 1234
            end if

         end do ! do jarray=Narray,1,-1

         call readyy(' ', -1d0)

         call doclose(MINP)

         ierror = 0

         goto 1234

100      continue
         call EOFERROR(MINP)
101      continue
         write (TEX, '(2I8)') ifile, jfile
         call READERROR('ERROR READING ARC-INFO BLOCK IN COLNR, ROWNR :', TEX, MINP)

!     error handling
1234     continue

!     deallocate
         deallocate (dline)
         deallocate (num)

         return
      end subroutine ReadLargeArcInfoBlock
