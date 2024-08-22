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

!> Fourier Analysis, copied from Delft3D:
!! Opens and reads .fou file (md_foufile, specified in the mdu)
!! and prepares the fourier structure
subroutine flow_fourierinit()
   use precision, only: fp
   use m_fourier_analysis
   use m_update_fourier, only: fourier_save_dry_wet_mask
   use m_transport, only: NUMCONST, ISALT, ITEMP
   use unstruc_model, only: md_foufile, md_tunit, getoutputdir, md_fou_step
   use unstruc_files, only: defaultFilename
   use m_flow, only: kmxd
   use m_flowtimes, only: tstart_user, tstop_user, ti_his, dt_user
   use unstruc_channel_flow, only: network
   use m_oned_functions, only: set_ground_level_for_1d_nodes, set_max_volume_for_1d_nodes

   implicit none
   integer :: minp
   logical :: success
   real(kind=fp) :: ti_fou

   call oldfil(minp, md_foufile)

   call fouini(minp, success, md_tunit, 'S')

   FouOutputFile = trim(getoutputdir())//defaultFilename('fou')

   call alloc_fourier_analysis_arrays()

   select case (md_fou_step)
   case (1)
      ti_fou = -999.0_fp
   case (0)
      ti_fou = dt_user
   case (2)
      ti_fou = ti_his
   end select

   call reafou(minp, md_foufile, kmxd, NUMCONST, ISALT, ITEMP, tstart_user, tstop_user, ti_fou, success)

   if (fourierWithMask()) then
      call fourier_save_dry_wet_mask()
   end if

   if (network%loaded) then
      if (fourierWithFb() .or. fourierWithWdog() .or. fourierWithVog()) then
         ! If freeboard, waterdepth on ground or volume on ground is read from the *.fou file,
         ! then need to compute groundlevel for 1d node firstly.
         ! The groundlevel will be used to update freeboard, waterdepth on ground and volume on ground later.
         call set_ground_level_for_1d_nodes(network)
      end if
      if (fourierWithVog()) then
         ! If volume on ground is read from the *.fou file, then also need to compute maximal volume firstly.
         call set_max_volume_for_1d_nodes() ! set maximal volume, it will be used to update the volume on ground level for the output
      end if
   end if

   call doclose(minp)

end subroutine flow_fourierinit
