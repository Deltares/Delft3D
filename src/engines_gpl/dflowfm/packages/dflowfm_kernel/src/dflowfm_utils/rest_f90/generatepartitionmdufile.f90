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

subroutine generatePartitionMDUFile(filename, filename_new)
   use unstruc_model
   use unstruc_messages
   use m_partitioninfo
   use string_module
   implicit none
   character(len=*), intent(in)  :: filename, filename_new
   integer                       :: k1, k2, k3, k4, k5, k6, k7, n
   character(len=500)            :: string, string_c, string_tmp, string_v
   integer                       :: ja_innumerics, ja_icgsolverset, lunold, lunnew

   open(newunit=lunold, file = filename, status ="old", action="read", err=999)
   open(newunit=lunnew, file = filename_new, status = "replace", action="write",err=999)

   k1 = 0; k2 = 0; k3 = 0; k4 = 0; k5 = 0; k6 = 0; k7 = 0; ja_innumerics = 0; ja_icgsolverset = 0
   do while (.true.)
      read(lunold, "(a)", err=999, end=1212) string
      n = index(string, '=')

      !> In case icgsolver was not present in input MDU, find-and-replace impossible, so make sure to add it, because it's required.
      if (strcmpi(string, '[numerics]', 10)) then
         ja_innumerics = 1
      elseif (string(1:1) == '[' .and. ja_innumerics == 1) then ! About to close [numerics]
         if (ja_icgsolverset == 0) then
            write(string_v, "(I5)") md_icgsolver
            string_tmp = "Icgsolver = "//trim(adjustl(string_v))//"          # Solver type , 1 = sobekGS_OMP, 2 = sobekGS_OMPthreadsafe, 3 = sobekGS, 4 = sobekGS + Saadilud, 5 = parallel/global Saad, 6 = parallel/Petsc, 7 = parallel/GS"
            write(lunnew, "(a)") trim(string_tmp)
         end if
         ja_innumerics = 0
      end if

      string_c = string(1:n)
      call str_lower(string_c)
      k1 = index(string_c, 'netfile')
      k2 = index(string_c, 'icgsolver')
      if (len_trim(md_restartfile) > 0) then
         k3 = index(string_c, 'restartfile')
      endif
      if (len_trim(md_mapfile) > 0) then
         k4 = index(string_c, 'mapfile')
      endif
      if (md_genpolygon .eq. 1) then
         k5 = index(string_c, 'partitionfile')
      endif
      if (len_trim(md_flowgeomfile) > 0) then
         k6 = index(string_c, 'flowgeomfile')
      endif
      if (len_trim(md_classmap_file) > 0) then
         k7 = index(string_c, 'classmapfile')
      endif

      if(k1==0 .and. k2==0 .and. k3==0 .and. k4==0 .and. k5==0 .and. k6==0 .and. k7==0) then ! Copy the whole row
         write(lunnew, "(a)") trim(string)
      else
         if (k1 .ne. 0) then      ! modify NetFile
           string_tmp = trim(string_c)//" "//trim(md_netfile)//"        # *_net.nc"
           write(lunnew, "(a)") trim(string_tmp)
         else if (k2 /= 0) then ! Modify icgsolver
            write(string_v, "(I5)") md_icgsolver
            string_tmp = trim(string_c)//" "//trim(adjustl(string_v))//"          # Solver type , 1 = sobekGS_OMP, 2 = sobekGS_OMPthreadsafe, 3 = sobekGS, 4 = sobekGS + Saadilud, 5 = parallel/global Saad, 6 = parallel/Petsc, 7 = parallel/GS"
            write(lunnew, "(a)") trim(string_tmp)
            ja_icgsolverset = 1
         else if (k3 /= 0) then ! Modify restart file name
            string_tmp = trim(string_c)//" "//trim(md_restartfile)//"       # Restart file, only from netcdf-file, hence: either *_rst.nc or *_map.nc"
            write(lunnew, "(a)") trim(string_tmp)
         else if (k7 /= 0) then ! Modify ClassMapFile. Must be before mapfile as we don't check on whole words
            string_tmp = trim(string_c)//" "//trim(md_classmap_file)//"       # ClassMapFile name *.nc"
            write(lunnew, "(a)") trim(string_tmp)
         else if (k4 /= 0) then ! Modify mapfile
            string_tmp = trim(string_c)//" "//trim(md_mapfile)//"       # MapFile name *_map.nc"
            write(lunnew, "(a)") trim(string_tmp)
         else if (k5 /= 0) then ! Modify Partitionfile
            string_tmp = trim(string_c)//" "//trim(md_partitionfile)//"          # *_part.pol, polyline(s) x,y"
            write(lunnew, "(a)") trim(string_tmp)
         else if (k6 /= 0) then ! Modify FlowGeomFile
            string_tmp = trim(string_c)//" "//trim(md_flowgeomfile)//"       # FlowGeomFile name *.nc"
            write(lunnew, "(a)") trim(string_tmp)
         endif
      endif
   enddo

1212 continue
   close(lunold)
   close(lunnew)
   return
999 call mess(LEVEL_ERROR, 'Error occurs when generating partition MDU files')
   close(lunold)
   close(lunnew)
   return
end subroutine generatePartitionMDUFile
