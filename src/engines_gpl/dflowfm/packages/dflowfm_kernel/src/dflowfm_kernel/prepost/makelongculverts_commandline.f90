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

!>  perform long culvert conversion from command line
subroutine makelongculverts_commandline()
   use unstruc_model
   use m_readstructures
   use string_module, only: strsplit
   use m_longculverts
   use unstruc_netcdf, only: unc_write_net, UNC_CONV_UGRID
   use system_utils
   use m_set_nod_adm

   implicit none

   character(len=1024) :: fnamesstring
   character(len=:), allocatable :: converted_fnamesstring
   character(len=:), allocatable :: converted_crsdefsstring
   character(len=:), allocatable :: tempstring_crsdef
   character(len=:), allocatable :: tempstring_fnames
   character(len=:), allocatable :: tempstring_netfile
   character(len=200), dimension(:), allocatable :: fnames
   character(len=IdLen) :: temppath, tempname, tempext

   integer :: istat, ifil

   if (len_trim(md_1dfiles%structures) > 0) then

      fnamesstring = md_1dfiles%structures
      call strsplit(fnamesstring, 1, fnames, 1)

      if (len_trim(md_1dfiles%cross_section_definitions) > 0) then
         call convertLongCulvertsAsNetwork(fnames(1), 0, md_culvertprefix, converted_fnamesstring, converted_crsdefsstring, istat, md_1dfiles%cross_section_definitions)
      else
         call convertLongCulvertsAsNetwork(fnames(1), 0, md_culvertprefix, converted_fnamesstring, converted_crsdefsstring, istat)
      end if
      do ifil = 2, size(fnames)
         call convertLongCulvertsAsNetwork(fnames(ifil), 1, md_culvertprefix, tempstring_fnames, tempstring_crsdef, istat)
         converted_crsdefsstring = trim(trim(converted_crsdefsstring)//', ')//tempstring_crsdef
         converted_fnamesstring = trim(trim(converted_fnamesstring)//', ')//tempstring_fnames
      end do
      deallocate (fnames)
      call setnodadm(0)
      call finalizeLongCulvertsInNetwork()

      call split_filename(md_netfile, temppath, tempname, tempext)
      tempname = trim(md_culvertprefix)//tempname
      tempstring_netfile = cat_filename(temppath, tempname, tempext)

      call unc_write_net(tempstring_netfile, janetcell=1, janetbnd=0, jaidomain=0, iconventions=UNC_CONV_UGRID)

      md_netfile = tempstring_netfile
      md_1dfiles%structures = converted_fnamesstring
      md_1dfiles%cross_section_definitions = converted_crsdefsstring
      converted_fnamesstring = trim(trim(md_culvertprefix)//md_ident)//'.mdu'
      call writeMDUFile(converted_fnamesstring, istat)
   end if

end subroutine makelongculverts_commandline
