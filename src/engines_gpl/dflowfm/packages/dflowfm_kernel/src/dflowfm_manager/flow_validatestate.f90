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

!> Validates the current flow state and returns whether simulation should be aborted.
!! Moreover, a final snapshot is written into the output files before aborting.
!!
!! Validity is determined by s01max, u01max, umagmax and dtminbreak.
!! Also print a warning if water level or velocity > s01warn, u01warn, umagwarn
subroutine flow_validatestate(iresult)
   use precision, only: dp
   use unstruc_messages
   use m_flow
   use m_flowgeom
   use m_flowparameters
   use m_flowtimes
   use m_transport
   use dfm_error
   use m_get_ucx_ucy_eul_mag
   implicit none
   integer, intent(out) :: iresult ! validation result status
   real(kind=dp) :: dtavgwindow
   integer :: i, q, k

   iresult = DFM_NOERR

   q = 0

   if (s01max > 0d0) then ! water level difference validation
      do i = 1, ndx
         if (abs(s1(i) - s0(i)) > s01max) then
            call mess(LEVEL_WARN, 'water level change above threshold: (cell index, delta s[m]) = ', i, abs(s1(i) - s0(i)))
            q = 1
            exit
         end if
      end do
   end if

   if (u01max > 0d0) then ! velocity difference validation
      do i = 1, lnx
         if (abs(u1(i) - u0(i)) > u01max) then
            call mess(LEVEL_WARN, 'velocity change above threshold: (flowlink index, delta u[m/s]) = ', i, abs(u1(i) - u0(i)))
            q = 1
            exit
         end if
      end do
   end if

   if (umagwarn > 0d0 .or. umagmax > 0d0) then ! velocity magnitude needed
      call getucxucyeulmag(ndkx, workx, worky, ucmag, jaeulervel, 1)
   end if

   if (umagmax > 0d0) then ! velocity magnitude validation
      do i = 1, ndkx
         if (ucmag(i) > umagmax) then
            call mess(LEVEL_WARN, 'velocity magnitude above threshold: (cell index, ucmag[m/s]) = ', i, ucmag(i))
            q = 1
            exit
         end if
      end do
   end if

   if (s01warn > 0d0) then ! water level warning
      do i = 1, ndx
         if (abs(s1(i)) > s01warn) then
            call mess(LEVEL_WARN, 'water level s1 above threshold: (cell index, s[m]) = ', i, s1(i))
         end if
      end do
   end if

   if (u01warn > 0d0) then ! velocity component warning
      do i = 1, lnx
         if (abs(u1(i)) > u01warn) then
            call mess(LEVEL_WARN, 'velocity u1 above threshold: (flowlink index, u[m/s]) = ', i, u1(i))
         end if
      end do
   end if

   if (umagwarn > 0d0) then ! velocity magnitude warning
      do i = 1, ndkx
         if (ucmag(i) > umagwarn) then
            call mess(LEVEL_WARN, 'velocity magnitude above threshold: (cell index, ucmag[m/s]) = ', i, ucmag(i))
         end if
      end do
   end if

   if (dtminbreak > 0d0) then ! smallest allowed timestep (in s), checked on a sliding average of several timesteps
      ! NOTE: this code below assumes that this routine is called once and exactly once every time step (i.e. in `dnt` rythm)
      dtavgwindow = (time1 - tvalswindow(idtwindow_start)) / max(1d0, min(dble(NUMDTWINDOWSIZE), dnt))
      if (dnt < dble(NUMDTWINDOWSIZE)) then
         ! First few time steps: just store all time1's until array is full
         tvalswindow(int(dnt + 1)) = time1
      else
         ! Array is full already, overwrite the oldest element (i.e. at current idtwindow_start)
         ! and increment start index, cycling back to 1 if necessary.
         tvalswindow(idtwindow_start) = time1
         idtwindow_start = mod(idtwindow_start, NUMDTWINDOWSIZE) + 1
      end if

      ! Now ready for the actual dtminbreak check, but only do that once we have
      ! at least done dnt > NUMDTWINDOWSIZE time steps, to prevent the initial
      ! spin-up period to cause unwanted simulation breaks.
      if (dnt >= dble(NUMDTWINDOWSIZE) .and. dtavgwindow < dtminbreak) then
         write (msgbuf, '(a,e11.4,a,e11.4,a)') 'Comp. time step average below threshold: ', dtavgwindow, ' < ', dtminbreak, '.'
         call warn_flush() ! PENDING UNST-725, make this a warning instead of an error, because stopping will take place elsewhere in a clean way.
         q = 1
      end if
   end if

! Check on concentration values and crash
   if (sscmax > 0d0) then
      if (jased == 4 .and. ISED1 > 0) then ! to do: ease jased=4 req when HK arrays incorporated in constituents
         do i = ISED1, ISEDN
            do k = 1, ndx
               if (constituents(i, k) > sscmax) then
                  q = 1
                  write (msgbuf, '(a,i5,a,i5,a,e11.4)') 'SSC above threshold: (cell index, fraction, SSC [kg/m3]) = ', k, ', ', i - ISED1 + 1, ', ', constituents(i, k)
                  call warn_flush()
               end if
            end do
         end do
      end if
   end if

   if (q /= 0) then
      call flow_externaloutput_direct() ! Last-minute save of emergency snapshot in map/his/rst
      iresult = DFM_INVALIDSTATE
   end if

end subroutine flow_validatestate
