!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2023.                                
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

 !> Writes shapefiles, these shapefiles can be visulaized in geographic information system (GIS) software
#ifdef HAVE_SHAPELIB
subroutine unc_write_shp()
    use m_flowparameters, only: jashp_crs, jashp_obs, jashp_weir, jashp_thd, jashp_gate, jashp_emb, jashp_fxw, jashp_src, jashp_pump, jashp_dry, jashp_genstruc
    use unstruc_shapefile
    use m_monitoring_crosssections, only: ncrs, crs
    use m_observations, only: numobs, kobs
    use m_flowexternalforcings, only: nweirgen, ngategen, numsrc, ksrc, gate2cgen, L1cgensg, L2cgensg, npumpsg, L1pumpsg, L2pumpsg, ngenstru, genstru2cgen, weir2cgen
    use m_thindams
    use m_sobekdfm, only: nbnd1d2d
    use m_fixedweirs, only: nfxw
    use unstruc_messages
    use m_partitioninfo, only: jampi, my_rank
    use unstruc_model  , only: md_dryptsfile
    implicit none
    integer :: jawrite, igen, n

    ! cross sections
    if (jashp_crs > 0) then
       if (jampi .eq. 0) then
          if (ncrs > 0) then
             call unc_write_shp_crs()
          else
             call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for cross sections is written because no cross section is found.')
          endif
       else
          if (ncrs > 0) then
             jawrite = ncrs
             do n = 1, ncrs
                if (crs(n)%PATH%LNX < 1) then
                   jawrite = jawrite - 1
                endif
             enddo
             if (jawrite > 0) then
                call unc_write_shp_crs()
             else
                call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for cross sections is written because no cross section is found on subdomain:', my_rank)
             endif
          else
             call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for cross sections is written because no cross section is found on subdomain:', my_rank)
          endif
       endif
    endif
    ! observation stations
    if (jashp_obs > 0) then
      if (jampi .eq. 0) then
          if (NUMOBS > 0) then
             call unc_write_shp_obs()
          else
             call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for observation stations is written because no observation station is found.')
          endif
       else
          if (NUMOBS > 0) then
             jawrite = NUMOBS
             do n = 1, NUMOBS
                if (kobs(n) <= 0) then
                   jawrite = jawrite - 1
                endif
             enddo
             if (jawrite > 0) then
                call unc_write_shp_obs()
             else
                call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for observation stations is written because no observation station is found on subdomain:', my_rank)
             endif
          else
             call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for observation stations is written because no observation station is found on subdomain:', my_rank)
          endif
       endif
    endif
    ! weirs
     if (jashp_weir > 0) then
       if (jampi .eq. 0) then
          if (nweirgen > 0 .and. allocated(weir2cgen)) then
             call unc_write_shp_weir()
          else
             call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for weirs is written because no weir is found.')
          endif
       else
          if (nweirgen > 0 .and. allocated(weir2cgen)) then
             jawrite = nweirgen
             do n = 1, nweirgen
                if (L1cgensg(n) > L2cgensg(n)) then
                   jawrite = jawrite - 1
                endif
             enddo
             if (jawrite > 0) then
                call unc_write_shp_weir()
             else
                call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for weirs is written because no weir is found on subdomain:', my_rank)
             endif
          else
             call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for weirs is written because no weir is found on subdomain:', my_rank)
          endif
       endif
    endif
    ! thin dams
    if (jashp_thd > 0) then
       if (jampi .eq. 0) then
          if (nthd > 0) then
             call unc_write_shp_thd()
          else
             call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for thin dams is written because no thin dam is found.')
          endif
       else
          if (nthd > 0) then
             jawrite = nthd
             do n = 1, nthd
                if (thd(n)%LNX < 1) then
                   jawrite = jawrite - 1
                endif
             enddo
             if (jawrite > 0) then
                call unc_write_shp_thd()
             else
                call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for thin dams is written because no thin dam is found on subdomain:', my_rank)
             endif
          else
             call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for thin dams is written because no thin dam is found on subdomain:', my_rank)
          endif
       endif
    endif
    ! gates
    if (jashp_gate > 0) then
       if (jampi .eq. 0) then
          if (ngategen > 0) then
             call unc_write_shp_gate()
          else
             call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for gates is written because no gate is found.')
          endif
       else
          if (ngategen > 0) then
             jawrite = ngategen
             do n = 1, ngategen
                igen = gate2cgen(n)
                if (L1cgensg(igen) > L2cgensg(igen)) then
                   jawrite = jawrite - 1
                endif
             enddo
             if (jawrite > 0) then
                call unc_write_shp_gate()
             else
                call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for gates is written because no gate is found on subdomain:', my_rank)
             endif
          else
             call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for gates is written because no gate is found on subdomain:', my_rank)
          endif
       endif
    endif
    ! embankments
    if (jashp_emb > 0) then
       if (nbnd1d2d > 0) then
          call unc_write_shp_emb()
       else
          if (jampi .eq. 0) then
             call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for embankments is written because no embankment is found.')
          else
             call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for embankments is written because no embankment is found on subdomain:', my_rank)
          endif
       endif
    endif
    ! fixed weirs
    if (jashp_fxw > 0) then
       if (nfxw > 0) then
          call unc_write_shp_fxw()
       else
          if (jampi .eq. 0) then
             call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for fixed weirs is written because no fixed weir is found.')
          else
             call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for fixed weirs is written because no fixed weir is found on subdomain:', my_rank)
          endif
       endif
    endif
    ! source-sinks
     if (jashp_src > 0) then
       if (jampi .eq. 0) then
          if (numsrc > 0) then
             call unc_write_shp_src()
          else
             call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for source-sinks is written because no source-sink is found.')
          endif
       else
          if (numsrc > 0) then
             jawrite = numsrc
             do n = 1, numsrc
                if (ksrc(1,n) <= 0 .and. ksrc(4,n) <= 0) then
                   jawrite = jawrite - 1
                endif
             enddo
             if (jawrite > 0) then
                call unc_write_shp_src()
             else
                call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for source-sinks is written because no source-sink is found on subdomain:', my_rank)
             endif
          else
             call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for source-sinks is written because no source-sink is found on subdomain:', my_rank)
          endif
       endif
     endif

     ! pumps
    if (jashp_pump > 0) then
       if (jampi .eq. 0) then
          if (npumpsg > 0) then
             call unc_write_shp_pump()
          else
             call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for pumps is written because no pump is found.')
          endif
       else
          if (npumpsg > 0) then
             jawrite = npumpsg
             do n = 1, npumpsg
                if (L1pumpsg(n) > L2pumpsg(n)) then
                   jawrite = jawrite - 1
                endif
             enddo
             if (jawrite > 0) then
                call unc_write_shp_pump()
             else
                call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for pumps is written because no pump is found on subdomain:', my_rank)
             endif
          else
             call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for pumps is written because no pump is found on subdomain:', my_rank)
          endif
       endif
    endif

    ! dry area
    if (jashp_dry > 0) then
       if (len_trim(md_dryptsfile) > 0) then
          call get_netlinks_of_dryarea()
          call unc_write_shp_dry()
       else
          if (jampi .eq. 0) then
             call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for dry areas is written because no dry area is found.')
          else
             call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for dry areas is written because no dry area is found on subdomain:', my_rank)
          endif
       endif
    endif

    ! general structures
    if (jashp_genstruc > 0) then
       if (jampi .eq. 0) then
          if (ngenstru > 0 .and. allocated(genstru2cgen)) then
             call unc_write_shp_genstruc()
          else
             call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for general structures is written because no gate is found.')
          endif
       else
          if (ngenstru > 0 .and. allocated(genstru2cgen)) then
             jawrite = ngenstru
             do n = 1, ngenstru
                igen = genstru2cgen(n)
                if (L1cgensg(igen) > L2cgensg(igen)) then
                   jawrite = jawrite - 1
                endif
             enddo
             if (jawrite > 0) then
                call unc_write_shp_genstruc()
             else
                call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for general structures is written because no general structure is found on subdomain:', my_rank)
             endif
          else
             call mess(LEVEL_WARN, 'SHAPEFILE: No shape file for general structures is written because no general structure is found on subdomain:', my_rank)
          endif
       endif
    endif



end subroutine unc_write_shp
#endif
