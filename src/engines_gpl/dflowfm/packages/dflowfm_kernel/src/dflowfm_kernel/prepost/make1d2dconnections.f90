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

 subroutine make1D2Dconnections()
    use network_data, only: imake1d2dtype, searchRadius1D2DLateral, I1D2DTP_1TO1, I1D2DTP_1TON_EMB, I1D2DTP_1TON_LAT
    use geometry_module
    use gridoperations
    use m_samples
    use m_polygon
    use m_sferic, only: jsferic, jasfer3D
    use m_qnerror
    use m_reapol

    integer :: minp, ierr
    character FILNAM * 86
    minp = 0
    Filnam = '*.pliz, *.xyz'
    select case (imake1d2dtype)
    case (I1D2DTP_1TO1) ! 0: HK
       call FILEMENU(minp, FILNAM, ierr)
       if (ierr == -1) then
          ierr = make1D2Dinternalnetlinks(xpl(1:npl), ypl(1:npl), zpl(1:npl))
       else if (index(filnam, '.pliz') > 0) then
          call reapol(minp, 0)
          call make1D2Droofgutterpipes()
       else if (index(filnam, '.xyz') > 0) then
          call reasam(minp, 0)
          call make1D2Dstreetinletpipes()
       end if
       call doclose(minp)
    case (I1D2DTP_1TON_EMB)
       ierr = ggeo_make1D2Dembeddedlinks(jsferic, jasfer3D)
    case (I1D2DTP_1TON_LAT)
       ierr = ggeo_make1D2DRiverLinks(jsferic, jasfer3D, searchRadius1D2DLateral)
    case default
       call QNERROR('Invalid 1D2D algorithm selected. Please check your network parameters.', '', '')
    end select

 end subroutine make1D2Dconnections
