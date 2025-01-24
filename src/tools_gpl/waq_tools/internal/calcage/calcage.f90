!!  Copyright (C)  Stichting Deltares, 2021-2024.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

! calcage.f90 --
!     Program to calculate the age from a "CART" model run.
!
!     Background:
!     Via the CART methodology we can determine the age of water
!     spreading out from a source. However, this involves the
!     ratio of the concentrations of two tracers, the water
!     source (WaterSrc) and age concentration (WaterAge).
!     To prevent divisions by zero and spurious results you
!     can specify a threshold for the water source concentration.
!     Below that threshold a missing value is used, instead of the
!     ratio.
!     This works fine for single point monitoring locations, but
!     not for monitoring areas, as in one segment belonging to that
!     area the concentration may be high enough whereas in another
!     segment it is not, so the value -999 is inserted. If you
!     then average the concentrations in all segments of the
!     monitoring area, then you get the wrong value.
!     The program "calcage" simply uses the water source and
!     age concentrations to recalculate the age. These
!     concentrations do not suffer from missing values.
!
!     Note: it works on history files and produces a new history
!     file.
!
!     Command-line arguments:
!     1. Name of the history file to be read
!     2. Name of the history file to be written (optional,
!        defaults to "calcage.his")
!     3. Threshold (optional, defaults to 0.0)
!
program calcage
   implicit none

   integer :: num_substances_transported, num_cells, time, ierr
   integer :: i, idxsrc, idxage
   integer :: inputfile, outputfile
   character(len=40), dimension(4) :: title
   character(len=20), dimension(:), allocatable :: syname, segname
   character(len=20) :: outname
   integer, dimension(:), allocatable :: segno
   real, dimension(:, :), allocatable :: conc
   real, dimension(:), allocatable :: age

   character(len=80) :: hisinp, hisout, string
   real :: threshold

   threshold = 0.0
   hisout = 'calcage.his'

   if (command_argument_count() >= 1) then
      call get_command_argument(1, hisinp)
   else
      write (*, *) 'Usage: calcage historyfile-in historyfile-out threshold'
      stop
   end if

   if (command_argument_count() >= 2) then
      call get_command_argument(2, hisout)
   end if

   if (command_argument_count() >= 3) then
      call get_command_argument(3, string)
      read (string, *) threshold
   end if

   write (*, *) 'Input history file:      ', trim(hisinp)
   write (*, *) 'Result written to:       ', trim(hisout)
   write (*, *) 'Concentration threshold: ', threshold

   open (newunit=inputfile, file=hisinp, access='stream', status='old', iostat=ierr)
   if (ierr /= 0) then
      write (*, *) 'Error opening input file', trim(hisinp), '! Does it exist?'
      stop
   end if

   read (inputfile) title
   read (inputfile) num_substances_transported, num_cells

   allocate (syname(num_substances_transported), segno(num_cells), segname(num_cells), conc(num_substances_transported, num_cells), age(num_cells))

   read (inputfile) syname
   read (inputfile) (segno(i), segname(i), i=1, num_cells)

   !
   ! Search for the combination WaterSrc and WaterAge
   !
   idxsrc = findloc(syname, 'WaterSrc', 1)
   idxage = findloc(syname, 'WaterAge', 1)

   if (idxsrc < 1 .or. idxage < 1) then
      write (*, *) 'Either "WaterSrc" or "WaterAge" not found!'
      write (*, *) '(Names are case-sensitive)'
      stop
   end if

   open (newunit=outputfile, file=hisout, access='stream')

   write (outputfile) title
   write (outputfile) 1, num_cells

   outname = 'Age (d)'
   write (outputfile) outname
   write (outputfile) (segno(i), segname(i), i=1, num_cells)

   do
      read (inputfile, iostat=ierr) time, conc
      if (ierr /= 0) then
         exit
      end if

      age = merge(conc(idxage, :) / (conc(idxsrc, :) + tiny(1.0)), -999.0, conc(idxsrc, :) > threshold)

      write (outputfile) time, age
   end do

   write (*, *) 'Done'

end program calcage

