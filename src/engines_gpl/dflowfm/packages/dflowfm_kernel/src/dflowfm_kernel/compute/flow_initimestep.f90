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
!> Intialise flow timestep, also called once after flowinit.
module m_flow_initimestep
   use m_compute_wave_parameters, only: compute_wave_parameters
   use m_compute_wave_forcing_rhs, only: compute_wave_forcing_rhs
   use m_fillsystem_advec, only: fillsystem_advec
   use m_setumod, only: setumod
   use m_settaubxu_nowave, only: settaubxu_nowave
   use m_setship, only: setship
   use m_setkfs, only: setkfs
   use m_setdt, only: setdt
   use m_setcfuhi, only: setcfuhi
   use m_setbaptist, only: setbaptist
   use m_makeq1qaatstart
   use m_pillar_upd
   use m_heatu

   implicit none

contains

   subroutine flow_initimestep(jazws0, set_hu, use_u1, iresult)
      use precision, only: dp
      use m_bathyupdate, only: bathyupdate
      use m_advecdriver, only: advecdriver
      use timers
      use m_flowtimes
      use m_flow
      use m_flowgeom
      use unstruc_model, only: md_restartfile
      use unstruc_channel_flow
      use m_1d_structures, only: initialize_structures_actual_params, set_u0isu1_structures
      use m_nearfield, only: nearfield_mode, NEARFIELD_DISABLED, setNFEntrainmentMomentum
      use dfm_error
      use MessageHandling
      use m_partitioninfo
      use m_sethu
      use fm_external_forcings, only: calculate_wind_stresses, set_external_forcings_boundaries
      use m_wind, only: update_wind_stress_each_time_step
      use m_fm_icecover, only: update_icecover
      implicit none

      integer, intent(in) :: jazws0
      logical, intent(in) :: set_hu !< Flag for updating `hu` (.true.) or not (.false.) in subroutine `calculate_hu_au_and_advection_for_dams_weirs` (`sethu`).
      logical, intent(in) :: use_u1 !< Flag for using `u1` (.true.) or `u0` (.false.) in computing `taubxu` in subroutine `settaubxu_nowave`
      integer, intent(out) :: iresult !< Error status, DFM_NOERR==0 if succesful.
      integer :: ierror
      real(kind=dp), parameter :: MMPHR_TO_MPS = 1d-3 / 3600d0

      iresult = DFM_GENERICERROR

      call timstrt('Initialise timestep', handle_inistep)

      if (jazws0 == 0) s0 = s1 ! progress water levels

      call bathyupdate() ! only if jamorf == 1

      if (jazws0 == 1 .and. len_trim(md_restartfile) > 0) then
         ! ini timestep right after reading a rst file: prepare derived hs/ucx/etc.
         ! for writing of first map file snapshot, i.e., based on the s0/u0 values.
         hs = s0 - bl
      else
         hs = s1 - bl ! total water height
      end if

! due to tolerance in poshcheck, hs may be smaller than 0 (but larger than -1e-10)
      hs = max(hs, 0d0)

      if (nshiptxy > 0) then ! quick fix only for ships
         call setdt()
      end if

      tim1bnd = max(time0 + dts, tim1bnd)

      call timstrt('Set boundaries      ', handle_extra(38)) ! Start bnd
      call set_external_forcings_boundaries(tim1bnd, iresult) ! boundary forcings
      call timstop(handle_extra(38)) ! End bnd

      if (iresult /= DFM_NOERR) then
         write (msgbuf, *) ' Error found in EC-module '; call err_flush()
         if (jampi == 1) then
            write (msgbuf, *) 'Error occurs on one or more processes when setting external forcings on boundaries at time=', tim1bnd; 
            call err_flush()
            ! Terminate all MPI processes
            call abort_all()
         end if
         goto 888
      end if

      if (tlfsmo > 0d0) then
         alfsmo = (tim1bnd - tstart_tlfsmo_user) / tlfsmo
      end if

      call timstrt('u0u1        ', handle_extra(42)) ! Start u0u1
      if (jazws0 == 0) then
         u0 = u1 ! progress velocities
         call set_u0isu1_structures(network%sts)
      end if
      call timstop(handle_extra(42)) ! End u0u1

      advi = 0d0
      adve = 0d0

      call timstrt('Sethuau     ', handle_extra(39)) ! Start huau
      call calculate_hu_au_and_advection_for_dams_weirs(jazws0, set_hu)

      call setau() ! set au and cfuhi for conveyance after limited h upwind at u points
      call timstop(handle_extra(39)) ! End huau

      call timstrt('Setumod     ', handle_extra(43)) ! Start setumod
      call setumod(jazws0) ! set cell center velocities, should be here as prior to 2012 orso
      call timstop(handle_extra(43)) ! End setumod

      if (update_wind_stress_each_time_step > 0) then ! Update wind in each computational timestep
         call calculate_wind_stresses(time0, iresult)
         if (iresult /= DFM_NOERR) then
            return
         end if
      end if

      call timstrt('Set conveyance       ', handle_extra(44)) ! Start cfuhi
      call setcfuhi() ! set current related frictioncoefficient
      call timstop(handle_extra(44)) ! End cfuhi

      if (kmx == 0 .and. javeg > 0) then ! overwrite cfuhi in 2D with veg in plant areas
         call setbaptist()
      end if

      ! Calculate max bed shear stress amplitude and z0rou without waves
      if (jawave == 0) then
         call settaubxu_nowave(use_u1)
      end if

      ! Set wave parameters, adapted for present water depth/velocity fields
      if (jawave > 0) then
         taubxu = 0d0
         call compute_wave_parameters()
      end if

      call timstrt('Set structures actual parameters', handle_extra(45)) ! Start structactual
      call initialize_structures_actual_params(network%sts)
      call timstop(handle_extra(45)) ! Start structactual

      if (japillar == 1 .or. japillar == 3) then
         call pillar_upd()
      end if

      ! TIDAL TURBINES: Insert equivalent calls to updturbine and applyturbines here

      call timstrt('setdt', handle_extra(40)) ! Start setdt
      if (jazws0 == 0 .and. nshiptxy == 0) then
         call setdt() ! set computational timestep dt based on active hu's,
      end if
      call timstop(handle_extra(40)) ! End setdt

      ! Add wave model dependent wave force in RHS
      ! After setdt because surfbeat needs updated dts
      if (jawave > 0 .and. .not. flowwithoutwaves) then
         call compute_wave_forcing_RHS()
      end if

      if (nshiptxy > 0) then
         call setship() ! in initimestep
      end if

      if (nearfield_mode /= NEARFIELD_DISABLED .and. NFEntrainmentMomentum > 0) then
         !
         ! Update momentum exchange, based on current flow field
         call setNFEntrainmentMomentum()
      end if

      call timstrt('Compute advection term', handle_extra(41)) ! Start advec
      call advecdriver() ! advec limiting for depths below chkadvdp, so should be called after all source terms such as spiralforce
      call timstop(handle_extra(41)) ! End advec

      if (jazws0 == 1) then
         call makeq1qaAtStart()
         call setkfs()
      end if

      if (jaimplicit == 1) then
         call fillsystem_advec(ierror)
         if (ierror /= 0) goto 888
      end if

      if (jatem > 1 .and. jaheat_eachstep == 1) then
         call heatu(tim1bnd / 3600d0) ! from externalforcings
      end if
      call update_icecover()

      if (infiltrationmodel == DFM_HYD_INFILT_HORTON) then
         infiltcap0 = infiltcap / mmphr_to_mps
      end if

      call timstop(handle_inistep)

      iresult = DFM_NOERR

      return ! Return with success

888   continue
      ! Error
   end subroutine flow_initimestep

end module m_flow_initimestep
