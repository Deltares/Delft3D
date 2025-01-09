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

!> Writes shapefiles, these shapefiles can be visualized in geographic information system (GIS) software
module m_unc_write_shp
   use m_get_netlinks_of_dryarea, only: get_netlinks_of_dryarea

   implicit none

   private

   public :: unc_write_shp

contains

   subroutine unc_write_shp()
#ifdef HAVE_SHAPELIB
      use m_flowparameters, only: jashp_crs, jashp_obs, jashp_weir, jashp_thd, jashp_gate, jashp_emb, jashp_fxw, jashp_src, jashp_pump, jashp_dry, jashp_genstruc, jashp_dambreak
      use unstruc_shapefile
      use m_monitoring_crosssections, only: ncrs, crs
      use m_observations_data, only: numobs, kobs
      use fm_external_forcings_data, only: nweirgen, ngategen, numsrc, ksrc, gate2cgen, L1cgensg, L2cgensg, npumpsg, L1pumpsg, L2pumpsg, ngenstru, genstru2cgen, weir2cgen, ndambreaksignals, L1dambreaksg, L2dambreaksg
      use m_thindams
      use m_sobekdfm, only: nbnd1d2d
      use m_fixedweirs, only: nfxw
      use unstruc_messages
      use m_partitioninfo, only: jampi, my_rank
      use unstruc_model, only: md_dryptsfile
      implicit none

      integer :: jawrite !< total number of objects to write; only write the file when larger than 0
      integer :: igen !< index indirection
      integer :: n !< loop index
      character(30) :: subdomain !< string containing "." in sequential mode and " on subdomain <i>." in parallel mode

      if (jampi == 0) then
         subdomain = '.'
      else
         write (subdomain, '(A,I0,A)') ' on subdomain ', my_rank, '.'
      end if

      ! cross sections
      if (jashp_crs > 0) then
         jawrite = ncrs
         do n = 1, ncrs
            if (crs(n)%path%lnx < 1) then
               jawrite = jawrite - 1
            end if
         end do
         if (jawrite > 0) then
            call unc_write_shp_crs()
         else
            call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for cross sections is written because no cross section is found'//trim(subdomain))
         end if
      end if

      ! observation stations
      if (jashp_obs > 0) then
         jawrite = numobs
         do n = 1, numobs
            if (kobs(n) <= 0) then
               jawrite = jawrite - 1
            end if
         end do
         if (jawrite > 0) then
            call unc_write_shp_obs()
         else
            call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for observation stations is written because no observation station is found'//trim(subdomain))
         end if
      end if

      ! weirs
      if (jashp_weir > 0) then
         if (nweirgen > 0 .and. allocated(weir2cgen)) then
            jawrite = nweirgen
            do n = 1, nweirgen
               igen = weir2cgen(n)
               if (L1cgensg(igen) > L2cgensg(igen)) then
                  jawrite = jawrite - 1
               end if
            end do
            if (jawrite > 0) then
               call unc_write_shp_weir()
            else
               call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for weirs is written because no weir is found'//trim(subdomain))
            end if
         else
            call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for weirs is written because no weir is found'//trim(subdomain))
         end if
      end if

      ! thin dams
      if (jashp_thd > 0) then
         jawrite = nthd
         do n = 1, nthd
            if (thd(n)%lnx < 1) then
               jawrite = jawrite - 1
            end if
         end do
         if (jawrite > 0) then
            call unc_write_shp_thd()
         else
            call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for thin dams is written because no thin dam is found'//trim(subdomain))
         end if
      end if
      ! gates
      if (jashp_gate > 0) then
         jawrite = ngategen
         do n = 1, ngategen
            igen = gate2cgen(n)
            if (L1cgensg(igen) > L2cgensg(igen)) then
               jawrite = jawrite - 1
            end if
         end do
         if (jawrite > 0) then
            call unc_write_shp_gate()
         else
            call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for gates is written because no gate is found'//trim(subdomain))
         end if
      end if

      ! embankments
      if (jashp_emb > 0) then
         if (nbnd1d2d > 0) then
            call unc_write_shp_emb()
         else
            call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for embankments is written because no embankment is found'//trim(subdomain))
         end if
      end if

      ! fixed weirs
      if (jashp_fxw > 0) then
         if (nfxw > 0) then
            call unc_write_shp_fxw()
         else
            call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for fixed weirs is written because no fixed weir is found'//trim(subdomain))
         end if
      end if

      ! source-sinks
      if (jashp_src > 0) then
         jawrite = numsrc
         do n = 1, numsrc
            if (ksrc(1, n) <= 0 .and. ksrc(4, n) <= 0) then
               jawrite = jawrite - 1
            end if
         end do
         if (jawrite > 0) then
            call unc_write_shp_src()
         else
            call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for source-sinks is written because no source-sink is found'//trim(subdomain))
         end if
      end if

      ! pumps
      if (jashp_pump > 0) then
         jawrite = npumpsg
         do n = 1, npumpsg
            if (L1pumpsg(n) > L2pumpsg(n)) then
               jawrite = jawrite - 1
            end if
         end do
         if (jawrite > 0) then
            call unc_write_shp_pump()
         else
            call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for pumps is written because no pump is found'//trim(subdomain))
         end if
      end if

      ! dry area
      if (jashp_dry > 0) then
         if (len_trim(md_dryptsfile) > 0) then
            call get_netlinks_of_dryarea()
            call unc_write_shp_dry()
         else
            call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for dry areas is written because no dry area is found'//trim(subdomain))
         end if
      end if

      ! general structures
      if (jashp_genstruc > 0) then
         if (ngenstru > 0 .and. allocated(genstru2cgen)) then
            jawrite = ngenstru
            do n = 1, ngenstru
               igen = genstru2cgen(n)
               if (L1cgensg(igen) > L2cgensg(igen)) then
                  jawrite = jawrite - 1
               end if
            end do
            if (jawrite > 0) then
               call unc_write_shp_genstruc()
            else
               call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for general structures is written because no general structure is found'//trim(subdomain))
            end if
         else
            call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for general structures is written because no general structure is found'//trim(subdomain))
         end if
      end if

      ! dam break
      if (jashp_dambreak > 0) then
         jawrite = ndambreaksignals
         do n = 1, ndambreaksignals
            if (L1dambreaksg(n) > L2dambreaksg(n)) then
               jawrite = jawrite - 1
            end if
         end do
         if (jawrite > 0) then
            call unc_write_shp_dambreak()
         else
            call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for dam breaks is written because no dam break is found'//trim(subdomain))
         end if
      end if
#endif
   end subroutine unc_write_shp

end module m_unc_write_shp
