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

module m_addsorsin
   use m_reallocsrc, only: reallocsrc
   use m_missing, only: dmiss, dxymis
   use precision, only: dp

   implicit none

   private

   public :: addsorsin
   public :: addsorsin_from_polyline_file

contains

   !> Add a source(-sink) to the model based on geometry given in a polyline file.
   !!
   !! This subroutine is a wrapper around addsorsin, mainly taking care of reading the polyline file.
   subroutine addsorsin_from_polyline_file(polyline_file, name, z_source, z_sink, area, ierr)
      use dfm_error, only: DFM_NOERR, DFM_WRONGINPUT
      use m_filez, only: oldfil
      use m_polygon, only: xpl, ypl, zpl, npl, dzL
      use m_reapol, only: reapol
      use system_utils, only: split_filename
      use MessageHandling, only: IDLEN

      character(len=*), intent(in) :: polyline_file !< Name of the polyline file, either with x,y values only (*.pli), or including z-values (*.pliz).
      character(len=*), optional, intent(in) :: name !< Name of the source-sink. When not present, name is based on the polyline filename instead.
      real(kind=dp), dimension(:), optional, intent(in) :: z_source !< Vertical position of the source, Z-value(s) in m (1 for point or 2 for range).
      real(kind=dp), dimension(:), optional, intent(in) :: z_sink !< Vertical position of the source, Z-value(s) in m (1 for point or 2 for range).
      real(kind=dp), intent(in) :: area !< Area of the source/sink, in m2. Set to 0.0 for momentum-free point sources.
      integer, intent(out) :: ierr !< Error code, DFM_NOERR if no error occurred.

      integer :: istat
      integer :: pli_lun
      integer :: z_size ! Intended size of z_source_ and z_sink_ arrays, either 1 or 2.
      real(kind=dp), dimension(:), allocatable :: z_source_, z_sink_
      logical :: have_z_range
      character(len=0) :: path, ext
      character(len=IDLEN) :: name_

      ierr = DFM_WRONGINPUT

      call oldfil(pli_lun, polyline_file)
      call reapol(pli_lun, 0)

      if (npl == 0) then
         return
      end if

      have_z_range = allocated(dzl) .and. size(dzl) >= npl
      if (have_z_range) then
         z_size = 2
      else
         z_size = 1
      end if

      ! Either take the z-source values from input, or from polyline's last point.
      if (present(z_source)) then
         allocate (z_source_, source=z_source, stat=istat)
         if (istat /= 0) then
            return
         end if
      else
         allocate (z_source_(z_size), source=dmiss, stat=istat)
         if (istat /= 0) then
            return
         end if
         z_source_(1) = zpl(npl)

         if (have_z_range) then
            z_source_(2) = dzL(npl)
         end if
      end if

      ! Either take the z-sink values from input, or from polyline's first point.
      if (present(z_sink)) then
         allocate (z_sink_, source=z_sink, stat=istat)
         if (istat /= 0) then
            return
         end if
      else
         allocate (z_sink_(z_size), source=dmiss, stat=istat)
         if (istat /= 0) then
            return
         end if
         z_sink_(1) = zpl(1)
         if (have_z_range) then
            z_sink_(2) = dzL(1)
         end if
      end if

      if (present(name)) then
         name_ = name
      else
         call split_filename(polyline_file, path, name_, ext)
      end if

      ! Add the source/sink to the model based on prepared polyline data.
      call addsorsin(trim(name_), xpl(1:npl), ypl(1:npl), z_source_, z_sink_, area, ierr)

   end subroutine addsorsin_from_polyline_file

   !> Add a source-sink to the model.
   subroutine addsorsin(name, x_points, y_points, z_source, z_sink, area, ierr)
      use fm_external_forcings_data, only: numsrc, xsrc, ysrc, nxsrc, ksrc, zsrc, zsrc2, arsrc, cssrc, snsrc, srcname
      use m_GlobalParameters, only: INDTP_ALL

      use messagehandling, only: msgbuf, warn_flush
      use dfm_error, only: DFM_NOERR, DFM_WRONGINPUT
      use geometry_module, only: normalin
      use m_sferic, only: jsferic, jasfer3D
      use MessageHandling, only: IDLEN
      use m_find_flownode, only: find_nearest_flownodes

      character(len=*), intent(in) :: name !!< Name of the source/sink.
      real(kind=dp), dimension(:), intent(in) :: x_points !< x-coordinates of the source/sink (polyline from sink to source point).
      real(kind=dp), dimension(:), intent(in) :: y_points !< y-coordinates of the source/sink (polyline from sink to source point).
      real(kind=dp), dimension(:), intent(in) :: z_source !< Vertical position of the source, Z-value(s) in m (1 for point or 2 for range).
      real(kind=dp), dimension(:), intent(in) :: z_sink !< Vertical position of the source, Z-value(s) in m (1 for point or 2 for range).
      real(kind=dp), intent(in) :: area !< Area of the source/sink, in m2. Set to 0.0 for momentum-free point sources.
      integer, intent(out) :: ierr !< Error code, DFM_NOERR if no error occurred.

      integer :: kk, kk2, i, jakdtree, kdum(1)
      integer :: num_points
      character(len=IdLen) :: tmpname(1)

      ierr = DFM_WRONGINPUT

      num_points = size(x_points)
      if (num_points == 0) then
         return
      end if

      numsrc = numsrc + 1
      call reallocsrc(numsrc, num_points)

      ! set the coordinates of source/sink
      xsrc(numsrc, 1:num_points) = x_points(1:num_points)
      ysrc(numsrc, 1:num_points) = y_points(1:num_points)
      nxsrc(numsrc) = num_points
      kk = 0; kk2 = 0

      ! Store sink/source name for waq
      srcname(numsrc) = name

      ! call inflowcell(xpl(npl), ypl(npl), kk2) ! TO: Source
      tmpname(1) = name//' source'
      jakdtree = 0
      kdum(1) = 0
      if (xsrc(numsrc, num_points) /= dmiss) then
         call find_nearest_flownodes(1, xsrc(numsrc, num_points), ysrc(numsrc, num_points), tmpname(1), kdum(1), jakdtree, -1, INDTP_ALL); kk2 = kdum(1)
      end if

      ! Support point source/sinks in a single cell if polyline has just one point (npl==1)
      if (num_points == 1) then

         kk = 0 ! Only keep the source-side (kk2), and disable momentum discharge
         if (area /= dmiss .and. area /= 0.0_dp) then
            ! User specified an area for momentum discharge, but that does not apply to POINT sources.
            write (msgbuf, '(a,a,a,f8.2,a)') 'Source-sink ''', trim(name), ''' is a POINT-source. Nonzero area was specified: ', area, ', but area will be ignored (no momentum discharge).'
            call warn_flush()
         end if
         arsrc(numsrc) = 0.0_dp
      else ! Default: linked source-sink, with 2 or more polyline points
         ! call inflowcell(xpl(1) , ypl(1)  , kk) ! FROM: sink
         tmpname = name//' sink'
         kdum(1) = 0
         if (xsrc(numsrc, 1) /= dmiss) then
            call find_nearest_flownodes(1, xsrc(numsrc, 1), ysrc(numsrc, 1), tmpname(1), kdum(1), jakdtree, -1, INDTP_ALL); kk = kdum(1)
         end if

         if (kk /= 0 .or. kk2 /= 0) then
            arsrc(numsrc) = area
         end if
      end if

      if (kk == 0 .and. kk2 == 0) then
         write (msgbuf, '(a,a)') 'Source+sink is outside model area for ', trim(name)
         call warn_flush()
         ierr = DFM_NOERR
         goto 8888
      end if

      ksrc(1, numsrc) = kk
      zsrc(1, numsrc) = z_sink(1)
      zsrc2(1, numsrc) = z_sink(1)

      ksrc(4, numsrc) = kk2
      zsrc(2, numsrc) = z_source(1)
      zsrc2(2, numsrc) = z_source(1)

      if (kk > 0) then
         if (size(z_sink) == 2) then
            if (z_sink(2) /= dmiss) then
               zsrc2(1, numsrc) = z_sink(2)
            end if
         end if
         ! Determine angle (sin/cos) of 'from' link (=first segment of polyline)
         if (num_points > 1) then
            call normalin(xsrc(numsrc, 1), ysrc(numsrc, 1), xsrc(numsrc, 2), ysrc(numsrc, 2), cssrc(1, numsrc), snsrc(1, numsrc), xsrc(numsrc, 1), ysrc(numsrc, 1), jsferic, jasfer3D, dxymis)
         end if

         do i = 1, numsrc - 1
            if (ksrc(1, i) /= 0 .and. kk == ksrc(1, i)) then
               write (msgbuf, '(4a)') 'FROM point of ', trim(srcname(numsrc)), ' coincides with FROM point of ', trim(srcname(i)); call warn_flush()
            else if (ksrc(4, i) /= 0 .and. kk == ksrc(4, i)) then
               write (msgbuf, '(4a)') 'FROM point of ', trim(srcname(numsrc)), ' coincides with TO   point of ', trim(srcname(i)); call warn_flush()
            end if
         end do

      end if

      if (kk2 > 0) then
         if (size(z_source) == 2) then
            if (z_source(2) /= dmiss) then
               zsrc2(2, numsrc) = z_source(2)
            end if
         end if
         ! Determine angle (sin/cos) of 'to' link (=first segment of polyline)
         if (num_points > 1) then
            call normalin(xsrc(numsrc, num_points - 1), ysrc(numsrc, num_points - 1), xsrc(numsrc, num_points), ysrc(numsrc, num_points), cssrc(2, numsrc), snsrc(2, numsrc), xsrc(numsrc, num_points), ysrc(numsrc, num_points), jsferic, jasfer3D, dxymis)
         end if

         do i = 1, numsrc - 1
            if (ksrc(1, i) /= 0 .and. kk2 == ksrc(1, i)) then
               write (msgbuf, '(4a)') 'TO point of ', trim(srcname(numsrc)), ' coincides with FROM point of ', trim(srcname(i)); call warn_flush()
            else if (ksrc(4, i) /= 0 .and. kk2 == ksrc(4, i)) then
               write (msgbuf, '(4a)') 'TO point of ', trim(srcname(numsrc)), ' coincides with TO   point of ', trim(srcname(i)); call warn_flush()
            end if
         end do

      end if

      ierr = DFM_NOERR

8888  continue

   end subroutine addsorsin

end module m_addsorsin
