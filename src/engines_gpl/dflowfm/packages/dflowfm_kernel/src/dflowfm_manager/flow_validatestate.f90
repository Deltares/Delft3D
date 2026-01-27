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

!> Validates the current flow state and returns whether simulation should be aborted.
!! Moreover, a final snapshot is written into the output files before aborting.
!!
!! Certain condtions cause the simulation to break: s01_max_err, u01_max_err, umag_max_er, ssc_max_err, dtavg_min_er, s01maxavg_min_err
!! Others simply show a warning and continue with the simulation: s1_max_warn, u1abs_max_warn, umag_max_warn
!! (see m_flow_validatestate_data for the variable definitions).
module m_flow_validatestate

   implicit none

   private

   public :: flow_validatestate, default_flow_validatestate, reset_flow_validatestate

contains

   subroutine flow_validatestate(iresult)
      use precision, only: dp
      use m_missing, only: dmiss_pos
      use m_flow_validatestate_data
      use m_flow_externaloutput_direct, only: flow_externaloutput_direct
      use precision, only: dp
      use messagehandling, only: LEVEL_WARN, msgbuf, mess, warn_flush
      use m_flow
      use m_flowgeom
      use m_flowparameters
      use m_flowtimes, only: time1, dnt
      use m_transport
      use dfm_error
      use m_get_ucx_ucy_eul_mag
      use m_missing, only: dmiss_neg

      integer, intent(out) :: iresult ! validation result status
      real(kind=dp) :: dtavg
      integer :: ksed, k, L
      real(kind=dp) :: s01maxavg_current
      real(kind=dp) :: s01maxavg
      real(kind=dp) :: s1_s0

      iresult = DFM_NOERR

      if (s01_max_err > 0.0_dp) then ! water level difference validation
         do k = 1, ndx
            s1_s0 = abs(s1(k) - s0(k))
            if (s1_s0 > s01_max_err) then
               call mess(LEVEL_WARN, 'water level change above threshold: (cell index, delta s[m]) = ', k, s1_s0)
               iresult = DFM_INVALIDSTATE
            end if
         end do
      end if

      if (u01_max_err > 0.0_dp) then ! velocity difference validation
         do L = 1, lnx
            if (abs(u1(L) - u0(L)) > u01_max_err) then
               call mess(LEVEL_WARN, 'velocity change above threshold: (flowlink index, delta u[m/s]) = ', L, abs(u1(L) - u0(L)))
               iresult = DFM_INVALIDSTATE
            end if
         end do
      end if

      if (umag_max_warn > 0.0_dp .or. umag_max_err > 0.0_dp) then ! velocity magnitude needed
         call getucxucyeulmag(ndkx, workx, worky, ucmag, jaeulervel, 1)
      end if

      if (umag_max_err > 0.0_dp) then ! velocity magnitude validation
         do k = 1, ndkx
            if (ucmag(k) > umag_max_err) then
               call mess(LEVEL_WARN, 'velocity magnitude above threshold: (cell index, ucmag[m/s]) = ', k, ucmag(k))
               iresult = DFM_INVALIDSTATE
            end if
         end do
      end if

      if (s1_max_warn > 0.0_dp) then ! water level warning
         do k = 1, ndx
            if (abs(s1(k)) > s1_max_warn) then
               call mess(LEVEL_WARN, 'water level s1 above threshold: (cell index, s[m]) = ', k, s1(k))
            end if
         end do
      end if

      if (u1abs_max_warn > 0.0_dp) then ! velocity component warning
         do L = 1, lnx
            if (abs(u1(L)) > u1abs_max_warn) then
               call mess(LEVEL_WARN, 'velocity u1 above threshold: (flowlink index, u[m/s]) = ', L, u1(L))
            end if
         end do
      end if

      if (umag_max_warn > 0.0_dp) then ! velocity magnitude warning
         do k = 1, ndkx
            if (ucmag(k) > umag_max_warn) then
               call mess(LEVEL_WARN, 'velocity magnitude above threshold: (cell index, ucmag[m/s]) = ', k, ucmag(k))
            end if
         end do
      end if

      window_current = mod(int(dnt), VALIDATESTATEWINDOWSIZE) + 1
      if (dnt < VALIDATESTATEWINDOWSIZE_double) then
         window_start = 1
         validatestate_window_length = dnt + 1.0_dp
      else
         window_start = mod(window_current, VALIDATESTATEWINDOWSIZE) + 1
         validatestate_window_length = VALIDATESTATEWINDOWSIZE_double
      end if

      if (dtavg_min_err > 0.0_dp) then ! smallest allowed timestep (in s), checked on a sliding average of several timesteps
         ! NOTE: this code below assumes that this routine is called once and exactly once every time step (i.e. in `dnt` rythm)
         dtavg_window(window_current) = time1
         dtavg = (time1 - dtavg_window(window_start)) / min(dnt + 1.0_dp, VALIDATESTATEWINDOWSIZE_double)

         ! Now ready for the actual dtavg_min_err check, but only do that once we have
         ! at least done dnt > VALIDATESTATEWINDOWSIZE time steps, to prevent the initial
         ! spin-up period to cause unwanted simulation breaks.
         if (dnt >= VALIDATESTATEWINDOWSIZE_double .and. dtavg < dtavg_min_err) then
            write (msgbuf, '(a,e11.4,a,e11.4,a)') 'Comp. time step average below threshold: ', dtavg, ' < ', dtavg_min_err, '.'
            call mess(LEVEL_WARN, msgbuf)
            iresult = DFM_INVALIDSTATE
         end if
      end if

      if (s01maxavg_min_err > 0.0_dp) then
         s01maxavg_current = dmiss_neg
         do k = 1, ndx
            s1_s0 = abs(s1(k) - s0(k))
            if (s1_s0 > s01maxavg_current) then
               s01maxavg_current = s1_s0
            end if
         end do

         s01maxavg_window(window_current) = s01maxavg_current

         s01maxavg = sum(s01maxavg_window) / validatestate_window_length
         if (s01maxavg < s01maxavg_min_err) then
            write (msgbuf, '(a)') 'Water level change below threshold MinWaterlevelChangeBreak'
            call mess(LEVEL_WARN, msgbuf)
            iresult = DFM_INVALIDSTATE
         end if
      end if

! Check on concentration values and crash
      if (ssc_max_err > 0.0_dp) then
         if (jased == 4 .and. ISED1 > 0) then ! to do: ease jased=4 req when HK arrays incorporated in constituents
            do ksed = ISED1, ISEDN
               do k = 1, ndx
                  if (constituents(ksed, k) > ssc_max_err) then
                     iresult = DFM_INVALIDSTATE
                     write (msgbuf, '(a,i5,a,i5,a,e11.4)') 'SSC above threshold: (cell index, fraction, SSC [kg/m3]) = ', k, ', ', ksed - ISED1 + 1, ', ', constituents(ksed, k)
                     call mess(LEVEL_WARN, msgbuf)
                  end if
               end do
            end do
         end if
      end if

      if (iresult /= DFM_NOERR) then
         call flow_externaloutput_direct() ! Last-minute save of emergency snapshot in map/his/rst
      end if

   end subroutine flow_validatestate

   subroutine default_flow_validatestate()
      use precision, only: dp
      use m_flow_validatestate_data

      s01_max_err = 0.0_dp ! max. water level change: off
      u01_max_err = 0.0_dp ! max. velocity change: off
      umag_max_err = 0.0_dp ! max. velocity: off
      ssc_max_err = 0.0_dp
      s01maxavg_min_err = 0.0_dp !< min. avg. water level change: off
      dtavg_min_err = 0.0_dp !< smallest allowed timestep, otherwise break: off
      s1_max_warn = 0.0_dp
      u1abs_max_warn = 0.0_dp
      umag_max_warn = 0.0_dp

      window_start = 0
      window_current = 0
      validatestate_window_length = 0.0_dp
   end subroutine default_flow_validatestate

   subroutine reset_flow_validatestate()
      use m_alloc, only: realloc
      use precision, only: dp
      use m_flow_validatestate_data

      if (dtavg_min_err > 0.0_dp) then
         call realloc(dtavg_window, VALIDATESTATEWINDOWSIZE, keepExisting=.false., fill=0.0_dp)
      end if
      if (s01maxavg_min_err > 0.0_dp) then
         call realloc(s01maxavg_window, VALIDATESTATEWINDOWSIZE, keepExisting=.false., fill=0.0_dp)
      end if
   end subroutine reset_flow_validatestate

end module m_flow_validatestate
