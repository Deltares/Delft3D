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

 !> A complete single computational time step (init-perform-finalize).
 subroutine flow_single_timestep(key, iresult) ! do only 1 flow timestep
    use m_flow
    use m_flowgeom
    use m_flowtimes
    use unstruc_netcdf
    use m_xbeach_netcdf
    use m_timer
    use dfm_error
    use m_sedtrails_netcdf, only: sedtrails_write_stats
    implicit none

    integer :: key
    integer, intent(out) :: iresult !< Error status, DFM_NOERR==0 if successful.

    iresult = DFM_GENERICERROR

!V: At this moment we are at time <t>. When using the regular solver (i.e., <flow_solver>=1),
!the time step is advanced in <flow_run_single_timestep>. This means that the boundary conditions
!(constructed when calling <flow_init_single_timestep>) are at time <t>.  When using the
!implicit 1D solver, the boundary conditions are expected at time <t+1>. Hence, we advance
!the time here such that the boundary conditions are at <t+1>. This could be done somewhere
!else in the code, e.g., <flow_initimestep>. I think that here it is clearer.
    if (flow_solver == FLOW_SOLVER_SRE) then
       !V: During model initialization, the time advances 1 s. This is very annoying when using
       !an implicit solver with fixed time step. Here we take it out considering the case in
       !which the time step is set to 1 s. This should be done in a better way (not sure how).
       if ((time0 == 1d0) .and. (dts /= 1d0)) then
          time0 = 0d0
       end if
       time1 = time0 + dts
    end if

    call flow_init_single_timestep(iresult)
    if (iresult /= DFM_NOERR) then
       goto 888
    end if

    call flow_run_single_timestep(key, iresult)
    if (iresult /= DFM_NOERR .and. iresult /= DFM_TIMESETBACK) then
       goto 888
    end if

    call flow_finalize_single_timestep(iresult)
    if (iresult /= DFM_NOERR) then
       goto 888
    end if

    ! JRE avoid annoying dt_user interference
    ! This may induce slight inaccuracies when dts is relatively large
    call xbeach_write_stats(time1)
    call sedmor_write_stats(time1)
    if (jasedtrails > 0) then
       call sedtrails_write_stats(time1)
    end if
    iresult = DFM_NOERR
    return ! Return with success

888 continue
    ! Error
 end subroutine flow_single_timestep
