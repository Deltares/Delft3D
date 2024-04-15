module m_fm_read_hydro

implicit none

contains

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

subroutine part_readhydstep(hyd,itime,istat)
   use m_part_flow
   use m_part_geom, only: ba, ndx, lnx, lne2ln
   use m_part_times, only: time1
   use m_hydmod
   use timers

   implicit none

   type(t_hydrodynamics)        ,intent(inout) :: hyd           !< description of the hydrodynamics
   integer            ,intent(inout) :: itime
   integer            ,intent(inout) :: istat

   !  local declarations
   integer iseg, iq, L, K, iql, offset

   integer(4) ithndl              ! handle to time this subroutine
   data ithndl / 0 /
   if ( timon ) call timstrt( "part_readhydstep", ithndl )

   if (istat==0) then
      do iseg = 1, hyd%noseg
         vol0(iseg) = vol1(iseg)
         h0(iseg) = h1(iseg)
      end do
      do iq = 1, hyd%noq
         q0(iq) = q1(iq)
      end do
   end if

   itime = int(time1)
   call read_hyd_step_fm(hyd,itime,istat)
   if (nint(time1) /= itime) then
      istat = 99
   end if
   if (istat == 0) then
      do iseg = 1, ndx !hyd%noseg
         vol1(iseg) = real(hyd%volume(iseg),8)
         h1(iseg) = vol1(iseg)/ba(iseg)
      end do
      offset = hyd%nolay*lnx
      do K = 1, hyd%nolay
          do L = 1, lnx  ! horizontal
             iql = lne2ln(L)  !loop at boundary cells
             iq  = lne2ln(L) + (K-1)*lnx  !
             if (iql <= 0) cycle

             if (hyd%edge_type(iql) == 2 .or. hyd%edge_type(iql) == 22) then  ! upstream boundary?
                  q1(iq) = hyd%flow(iq)
             else
                  q1(iq) = -hyd%flow(iq)
             endif

          end do
      end do
      ! vertical exchanges:
      ! - ndx is the total number of segments
      ! - we have hyd%nosegl segments per layer
      ! - the number of interfaces between the layers is one less than the number of layers,
      !   hence the correction.
      !
      do K = 1,hyd%nolay-1
          do L = 1, hyd%nosegl  ! vertical
             iq  = offset + L + (K-1) * hyd%nosegl  !
             q1(iq) = hyd%flow(iq)
          end do
      end do
   end if

   if ( timon ) call timstop ( ithndl )

end subroutine part_readhydstep

!
! Wrapper for rdhydr from the "classic" reading routine
!
subroutine read_hyd_step_fm(hyd,itime,istat)
   use partmem, only: t_hydrodynamics, itstrtp, idelt, caltau, vol1, vol2, flow1, flow2m, vdiff1, tau1, salin1, temper1, flow2, rhowatc
   use m_part_geom, only: lnx
   use fileinfo
   use rdhydr_mod, only: rdhydr

   type(t_hydrodynamics), intent(inout) :: hyd
   integer, intent(in)        :: itime
   integer, intent(out)       :: istat

   integer, dimension(:), allocatable, save   :: cellpnt
   integer, dimension(:,:), allocatable, save :: flowpnt
  ! real, dimension(:), allocatable, save      :: vol1, vol2, flow1, vdiff1, tau1, salin1, temper1, rhowatc
   logical, save                              :: first = .true.
   logical                                    :: update
   integer                                    :: i

   if ( first ) then
      first = .false.
      allocate( vol1(hyd%noseg),     &
                vol2(hyd%noseg),     &
                tau1(hyd%noseg),     &
                salin1(hyd%noseg),   &
                temper1(hyd%noseg),  &
                rhowatc(hyd%noseg),  &
                vdiff1(hyd%noseg),   &
                flow1(hyd%noq)   ,   &
                flow2(hyd%noq)   ,   &
                flow2m(hyd%noq)          )
      allocate( cellpnt(hyd%noseg),  &
                flowpnt(hyd%noq,2)       )

      do i = 1,hyd%noseg
         cellpnt(i) = i
      enddo
      do i = 1,hyd%noq
         flowpnt(i,1) = i
         flowpnt(i,2) = 0 ! Ignore the "to" part
      enddo

      hyd%cnv_step_sec = -999 ! To properly initialise the reading procedure
   endif
                                               ! Was: lnx - AM
   call rdhydr( hyd%nmax, hyd%mmax,  hyd%noseg, hyd%noq,          hyd%noseg,  &
                hyd%noq,  itime,     itstrtp,   hyd%cnv_step_sec, hyd%volume, &
                hyd%vdf,  hyd%surf,  hyd%flow,  vol1,             vol2,       &
                flow1,    flow2m,    vdiff1,    update,           cellpnt,          flowpnt,    &
                hyd%tau,  tau1,      caltau,    hyd%sal,          salin1,     &
                hyd%tem,  temper1,   nfiles,    lunit,            fname,      &
                flow2,    rhowatc                                  )

   istat = 0 ! Assume it goes well
end subroutine read_hyd_step_fm

end module m_fm_read_hydro
