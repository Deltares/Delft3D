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
module m_netw

   use m_clearflowmodelinputs, only: clearflowmodelinputs
   use network_data
   use m_alloc
   implicit none

contains

   subroutine loadNetwork(filename, istat, jadoorladen)

      use unstruc_netcdf, only: unc_read_net, unc_write_net, md5_net_file
      use unstruc_messages
      use m_missing
      use gridoperations
      use m_network, only: admin_network
      use unstruc_channel_flow, only: network
      use m_qnerror
      use m_set_nod_adm

      implicit none

      character(*), intent(in) :: filename !< Name of file to be read (in current directory or with full path).
      integer, intent(out) :: istat !< Return status (0=success)
      integer, intent(in) :: jadoorladen

      integer :: iDumk
      integer :: iDuml

      ! double precision, allocatable, save :: zkold(:)

      integer :: K0, L0, NUMKN, NUMLN
      logical :: jawel

      call CLEARFLOWMODELINPUTS() ! TODO: Zou eigenlijk verplaatst moeten worden [AvD]

      inquire (file=filename, exist=jawel)
      if (.not. jawel) then
         call mess(LEVEL_WARN, 'could not open '''//trim(filename)//'''')
         return
      end if

      if (JADOORLADEN == 0) then
         K0 = 0
         L0 = 0
      else
         K0 = NUMK
         L0 = NUML
      end if

      ! New NetCDF net file
      call unc_read_net(filename, K0, L0, NUMKN, NUMLN, istat)
      call md5_net_file(L0 + 1, NUMLN)

      iDumk = 0
      iDuml = 0
      call admin_network(network, iDuml)

      if (istat == 0) then
         NUMK = K0 + NUMKN
         NUML = L0 + NUMLN
         call SETNODADM(0)
      else
         call qnerror('Error while loading network from '''//trim(filename)//''', please inspect the preceding diagnostic output.', ' ', ' ')
      end if
      call CLOSEWORLD() ! STITCH 0-360 FOR 0-360 GLOBE MODELS
      netstat = NETSTAT_CELLS_DIRTY
   end subroutine loadNetwork

end module m_netw
