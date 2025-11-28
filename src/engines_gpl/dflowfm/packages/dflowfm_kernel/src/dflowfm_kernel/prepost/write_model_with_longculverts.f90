!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2025.
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
module m_write_model_with_longculverts
    implicit none
    private
    public :: write_model_with_longculverts

contains

    subroutine write_model_with_longculverts()
        use unstruc_model, only: md_netfile, md_ident, writeMDUFile
        use m_globalparameters, only: t_filenames
        use messagehandling, only: IDLEN
        use system_utils, only: cat_filename, split_filename
        use unstruc_netcdf, only: unc_write_net, UNC_CONV_UGRID

        character(len=:), allocatable :: md_culvertprefix
        character(len=:), allocatable :: converted_fnamesstring
        character(len=:), allocatable :: tempstring_netfile
        character(len=IDLEN) :: temppath, tempname, tempext
        integer :: istat

        md_culvertprefix = 'converted_'

        ! Write converted netfile with prefix 'converted_'
        call split_filename(md_netfile, temppath, tempname, tempext)
        tempstring_netfile = cat_filename(temppath, trim(md_culvertprefix) // tempname, tempext)
        call unc_write_net(tempstring_netfile, janetcell=1, janetbnd=0, jaidomain=0, iconventions=UNC_CONV_UGRID)

        ! Overwrite netfile path in model definition.
        md_netfile = tempstring_netfile

        ! Write converted model definition file with prefix 'converted_'
        converted_fnamesstring = trim(trim(md_culvertprefix) // md_ident) // '.mdu'
        call writeMDUFile(converted_fnamesstring, istat)
    end subroutine write_model_with_longculverts

end module m_write_model_with_longculverts