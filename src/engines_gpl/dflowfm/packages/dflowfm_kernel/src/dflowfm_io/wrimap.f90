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
module m_wrimap
contains
   subroutine wrimap(tim)
      use precision, only: dp
      use m_flow, only: jamapbnd
      use m_flowtimes
      use unstruc_netcdf
      use unstruc_model
      use unstruc_files, only: defaultFilename
      use m_fm_update_crosssections, only: fm_update_mor_width_mean_bedlevel
      use m_flowgeom, only: ndx2d, ndxi
      use Timers

      implicit none
      real(kind=dp), intent(in) :: tim

      ! locals
      integer :: ierr
      character(len=256) :: filnam
      real(kind=dp), save :: curtime_split = 0d0 ! Current time-partition that the file writer has open.
      integer :: ndx1d, ndims
      integer :: jabndnd

      ! Another time-partitioned file needs to start, reset iteration count (and file).
      if (ti_split > 0d0 .and. curtime_split /= time_split0) then
         mapids%id_tsp%idx_curtime = 0
         it_map = 0
         it_map_tec = 0
         curtime_split = time_split0
      end if

      if (md_mapformat == IFORMAT_NETCDF .or. md_mapformat == IFORMAT_NETCDF_AND_TECPLOT .or. md_mapformat == IFORMAT_UGRID) then !   NetCDF output
         call timstrt('wrimap inq ncid', handle_extra(81))
         if (mapids%ncid /= 0 .and. ((md_unc_conv == UNC_CONV_UGRID .and. mapids%id_tsp%idx_curtime == 0) .or. (md_unc_conv == UNC_CONV_CFOLD .and. it_map == 0))) then
            ierr = unc_close(mapids%ncid)
            mapids%ncid = 0
         end if
         call timstop(handle_extra(81))

         call timstrt('wrimap inq ndims', handle_extra(80))
         if (mapids%ncid /= 0) then ! reset stord ncid to zero if file not open
            ierr = nf90_inquire(mapids%ncid, ndims)
            if (ierr /= 0) mapids%ncid = 0
         end if
         call timstop(handle_extra(80))

         call timstrt('wrimap unc_create', handle_extra(82))
         if (mapids%ncid == 0) then
            if (ti_split > 0d0) then
               filnam = defaultFilename('map', timestamp=time_split0)
            else
               filnam = defaultFilename('map')
            end if
            ierr = unc_create(filnam, 0, mapids%ncid)
            if (ierr /= nf90_noerr) then
               call mess(LEVEL_WARN, 'Could not create map file.')
               mapids%ncid = 0
            end if
         end if
         call timstop(handle_extra(82))

         if (mapids%ncid /= 0) then
            if (md_unc_conv == UNC_CONV_UGRID) then
               ndx1d = ndxi - ndx2d
               if (ndx1d > 0 .and. stm_included) then
                  if (stmpar%morpar%moroutput%blave) then
                     call fm_update_mor_width_mean_bedlevel()
                  end if
               end if
               jabndnd = 0
               if (jamapbnd > 0) jabndnd = 1
               call unc_write_map_filepointer_ugrid(mapids, tim, jabndnd) ! wrimap
            else
               jabndnd = 0
               if (jamapbnd > 0) jabndnd = 1
               call unc_write_map_filepointer(mapids%ncid, tim, jabndnd) ! wrimap
            end if
         end if

         call timstrt('wrimap nf90_sync', handle_extra(83))
         if (unc_noforcedflush == 0) then
            ierr = nf90_sync(mapids%ncid) ! Flush file
         end if
         call timstop(handle_extra(83))
      end if

      if (md_mapformat == IFORMAT_TECPLOT .or. md_mapformat == IFORMAT_NETCDF_AND_TECPLOT) then ! TecPlot output
         !if (mtecfil /= 0 .and. it_map_tec == 0) then
         !   call doclose(mtecfil)
         !end if

         !if (it_map_tec == 0) then
         !     if (ti_split > 0d0) then
         !         filnam = defaultFilename('tec', timestamp=time_split0)
         !     else
         !         filnam = defaultFilename('tec')
         !     end if
         !   call newfil(mtecfil, filnam)
         !endif

         !call tecplot_out(mtecfil, tim, it_map_tec==0)

!      write grid in Tecplot format only once
         if (it_map_tec == 0) then
            filnam = defaultFilename('net.plt')
            call wrinet_tecplot(filnam) ! do not call findcells
         end if

!      write solution in Tecplot format
         filnam = defaultFilename('map.plt', timestamp=tim)
         call wrimap_tecplot(filnam)

         it_map_tec = it_map_tec + 1
      end if
   end subroutine wrimap
end module m_wrimap
