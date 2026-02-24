!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
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

module m_generatepartitionmdufile

   implicit none(type, external)

   private

   public :: generate_partition_mdu_file

contains

   !> Generate *.mdu files for partitions from the main *.mdu file.
   subroutine generate_partition_mdu_file(filename_main, filename_partition)
      use unstruc_model, only: md_icgsolver, md_restartfile, md_mapfile, md_genpolygon, md_flowgeomfile, md_classmap_file, md_netfile, md_partitionfile, md_1dfiles, mess, level_error
      use string_module, only: strcmpi, str_lower

      ! Parameters
      character(len=*), intent(in) :: filename_main !< Filename of the main *.mdu file
      character(len=*), intent(in) :: filename_partition !< Filename of the partition *.mdu file to be generated

      ! Local variables
      logical :: in_numerics !< Flag to check if currently in [numerics] block
      logical :: in_geometry !< Flag to check if currently in [geometry] block
      logical :: icgsolver_present !< Flag to check if icgsolver keyword is present in input *.mdu file, to decide whether it needs to be added
      logical :: crossdeffile_present !< Flag to check if crossdeffile keyword is present in input *.mdu file, to decide whether it needs to be added
      logical, dimension(9) :: keyword_present !< Flags to check if keywords that need to be replaced are present in input *.mdu file, to decide whether find-and-replace is needed
      integer :: equal_pos !< Position of the equal sign in the line, to separate keyword from value
      integer :: stat !< Status of I/O operations
      integer :: unit_main !< Unit number for the main *.mdu file
      integer :: unit_partition !< Unit number for the partition *.mdu file

      character(len=500) :: mdu_line_main !< Line read from the main *.mdu file
      character(len=500) :: mdu_line_partition !< Line to be written to the partition *.mdu file
      character(len=500) :: keyword !< Keyword in the line, used for find-and-replace
      character(len=500) :: string_icgsolver !< String to hold the value of md_icgsolver for writing to the partition *.mdu file

      ! Open main and partition *.mdu files
      open (newunit=unit_main, file=filename_main, status="old", action="read", iostat=stat)
      if (stat /= 0) then
         call mess(LEVEL_ERROR, 'Error occured while opening the main MDU file')
         return
      end if
      open (newunit=unit_partition, file=filename_partition, status="replace", action="write", iostat=stat)
      if (stat /= 0) then
         call mess(LEVEL_ERROR, 'Error occured while opening the partition MDU file')
         return
      end if

      ! Initialization
      in_numerics = .false.
      in_geometry = .false.
      icgsolver_present = .false.
      crossdeffile_present = .false.
      keyword_present = .false.
      stat = 0

      do while (stat == 0)
         ! Readout line by line from old *.mdu file, until end of file is reached or an error occured (stat /= 0)
         read (unit_main, "(a)", iostat=stat) mdu_line_main
         if (stat /= 0) then
            exit
         end if

         ! In case icgsolver was not present in input *.mdu file, find-and-replace is not possible, so add icgsolver to partition *.mdu file
         if (strcmpi(mdu_line_main, '[numerics]', 10)) then
            in_numerics = .true.
         elseif (mdu_line_main(1:1) == '[' .and. in_numerics) then ! About to close [numerics] block
            if (.not. icgsolver_present) then
               write (string_icgsolver, "(I5)") md_icgsolver
               mdu_line_partition = "Icgsolver = "//trim(adjustl(string_icgsolver))//"          # Solver type , 1 = sobekGS_OMP, 2 = sobekGS_OMPthreadsafe, 3 = sobekGS, 4 = sobekGS + Saadilud, 5 = parallel/global Saad, 6 = parallel/Petsc, 7 = parallel/GS"
               write (unit_partition, "(a)") trim(mdu_line_partition)
            end if
            in_numerics = .false.
         end if

         ! In case crossdeffile was not present in input *.mdu file, find-and-replace is not possible, so add crossdeffile to partition *.mdu file
         if (strcmpi(mdu_line_main, '[geometry]', 10)) then
            in_geometry = .true.
         elseif (mdu_line_main(1:1) == '[' .and. in_geometry) then ! About to close [geometry] block
            if (.not. crossdeffile_present) then
               mdu_line_partition = "CrossDefFile = "//trim(md_1dfiles%cross_section_definitions)//"       # Cross section definitions file (*.ini)"
               write (unit_partition, "(a)") trim(mdu_line_partition)
            end if
            in_geometry = .false.
         end if

         ! Get keyword and convert to lower case for find-and-replace
         equal_pos = index(mdu_line_main, '=')
         keyword = mdu_line_main(1:equal_pos)
         call str_lower(keyword)

         ! Check if the keywords that need to be replaced are present
         keyword_present(1) = index(keyword, 'netfile') /= 0
         keyword_present(2) = index(keyword, 'icgsolver') /= 0
         if (len_trim(md_restartfile) > 0) then
            keyword_present(3) = index(keyword, 'restartfile') /= 0
         end if
         if (len_trim(md_mapfile) > 0) then
            keyword_present(4) = index(keyword, 'mapfile') /= 0
         end if
         if (md_genpolygon == 1) then
            keyword_present(5) = index(keyword, 'partitionfile') /= 0
         end if
         if (len_trim(md_flowgeomfile) > 0) then
            keyword_present(6) = index(keyword, 'flowgeomfile') /= 0
         end if
         if (len_trim(md_classmap_file) > 0) then
            keyword_present(7) = index(keyword, 'classmapfile') /= 0
         end if
         if (len_trim(md_1dfiles%structures) > 0) then
            keyword_present(8) = index(keyword, 'structurefile') /= 0
         end if
         if (len_trim(md_1dfiles%cross_section_definitions) > 0) then
            keyword_present(9) = index(keyword, 'crossdeffile') /= 0
         end if

         if (.not. any(keyword_present)) then
            write(unit_partition, "(a)") trim(mdu_line_main)
         else 
            if (keyword_present(1)) then ! Modify NetFile
               mdu_line_partition = trim(keyword)//" "//trim(md_netfile)//"        # *_net.nc"
               write (unit_partition, "(a)") trim(mdu_line_partition)
            else if (keyword_present(2)) then ! Modify IcgSolver
               write (string_icgsolver, "(I5)") md_icgsolver
               mdu_line_partition = trim(keyword)//" "//trim(adjustl(string_icgsolver))//"          # Solver type , 1 = sobekGS_OMP, 2 = sobekGS_OMPthreadsafe, 3 = sobekGS, 4 = sobekGS + Saadilud, 5 = parallel/global Saad, 6 = parallel/Petsc, 7 = parallel/GS"
               write (unit_partition, "(a)") trim(mdu_line_partition)
               icgsolver_present = .true.
            else if (keyword_present(3)) then ! Modify RestartFile
               mdu_line_partition = trim(keyword)//" "//trim(md_restartfile)//"       # Restart file, only from netcdf-file, hence: either *_rst.nc or *_map.nc"
               write (unit_partition, "(a)") trim(mdu_line_partition)
            else if (keyword_present(7)) then ! Modify ClassMapFile, must be before mapfile as we don't check on whole words
               mdu_line_partition = trim(keyword)//" "//trim(md_classmap_file)//"       # ClassMapFile name *.nc"
               write (unit_partition, "(a)") trim(mdu_line_partition)
            else if (keyword_present(4)) then ! Modify MapFile
               mdu_line_partition = trim(keyword)//" "//trim(md_mapfile)//"       # MapFile name *_map.nc"
               write (unit_partition, "(a)") trim(mdu_line_partition)
            else if (keyword_present(5)) then ! Modify PartitionFile
               mdu_line_partition = trim(keyword)//" "//trim(md_partitionfile)//"          # *_part.pol, polyline(s) x,y"
               write (unit_partition, "(a)") trim(mdu_line_partition)
            else if (keyword_present(6)) then ! Modify FlowGeomFile
               mdu_line_partition = trim(keyword)//" "//trim(md_flowgeomfile)//"       # FlowGeomFile name *.nc"
               write (unit_partition, "(a)") trim(mdu_line_partition)
            else if (keyword_present(8)) then ! Modify StructureFile
               mdu_line_partition = trim(keyword)//" "//trim(md_1dfiles%structures)//"       # Hydraulic structure file (*.ini)"
               write (unit_partition, "(a)") trim(mdu_line_partition)
            else if (keyword_present(9)) then ! Modify CrossSectionDefinitionsFile
               mdu_line_partition = trim(keyword)//" "//trim(md_1dfiles%cross_section_definitions)//"       # Cross section definitions file (*.ini)"
               write (unit_partition, "(a)") trim(mdu_line_partition)
            end if
         end if
      end do

      if (stat < 0) then ! End of file reached, close files
         close (unit_main)
         close (unit_partition)
      else if (stat > 0) then ! Error occured, close files and report error
         call mess(LEVEL_ERROR, 'Error occured while reading the main MDU file')
         close (unit_main)
         close (unit_partition)
      end if

   end subroutine generate_partition_mdu_file

end module m_generatepartitionmdufile
