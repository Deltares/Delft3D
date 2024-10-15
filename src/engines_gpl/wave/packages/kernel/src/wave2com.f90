    subroutine wave2com (fof,sr)
    !----- GPL ---------------------------------------------------------------------
    !
    !  Copyright (C)  Stichting Deltares, 2011-2024.
    !
    !  This program is free software: you can redistribute it and/or modify
    !  it under the terms of the GNU General Public License as published by
    !  the Free Software Foundation version 3.
    !
    !  This program is distributed in the hope that it will be useful,
    !  but WITHOUT ANY WARRANTY; without even the implied warranty of
    !  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    !  GNU General Public License for more details.
    !
    !  You should have received a copy of the GNU General Public License
    !  along with this program.  If not, see <http://www.gnu.org/licenses/>.
    !
    !  contact: delft3d.support@deltares.nl
    !  Stichting Deltares
    !  P.O. Box 177
    !  2600 MH Delft, The Netherlands
    !
    !  All indications and logos of, and references to, "Delft3D" and "Deltares"
    !  are registered trademarks of Stichting Deltares, and remain the property of
    !  Stichting Deltares. All rights reserved.
    !
    !-------------------------------------------------------------------------------
    !
    !
    !!--description-----------------------------------------------------------------
    ! Head routine for calling transform_swan_physics
    !!--pseudo code and references--------------------------------------------------
    ! NONE
    !!--declarations----------------------------------------------------------------
    use swan_flow_grid_maps
    use swan_input
    use m_transform_wave_physics
    implicit none
    type (output_fields) :: fof
    type (swan_type)     :: sr
    ! global variables
    integer :: ierr

    call transform_wave_physics_sp(  fof%hs             ,fof%dir           ,fof%period         ,fof%depth          , &
                                   & fof%fx             ,fof%fy            ,fof%mx             ,fof%my             , &
                                   & fof%dissip(:,:,1)  ,fof%dissip(:,:,2) ,fof%dissip(:,:,3)                      , &
                                   & fof%mmax           ,fof%nmax          ,fof%hrms           ,fof%tp             , &
                                   & sr%grav            ,sr%swflux         ,sr%swdis                               , &
                                   & sr%gamma0          ,fof%wsbodyu       ,fof%wsbodyv        ,ierr )

    if (ierr < 0) then
        call wavestop(1, 'ERROR: gamma0 lies outside allowed range [1,20]')
    endif

    end subroutine wave2com
