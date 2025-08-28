!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2025.
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

      subroutine write_hyd_init(hyd)

      ! write the time independent data from a hydrodynamics
      use m_logger_helper, only : get_log_unit_number
      use m_hydmod
      use m_write_waqgeom

      implicit none

      ! declaration of the arguments

      type(t_hydrodynamics)         :: hyd     ! description of the hydrodynamics

      ! local declarations

      integer             :: lunrep    ! report file
      integer             :: itime     ! time (dummy)
      character(len=20)   :: valnam(2) ! parameter name
      logical             :: success

      ! some init

      call get_log_unit_number(lunrep)

      if (hyd%geometry .eq. HYD_GEOM_CURVI) then
         ! grid table

         call write_lga ( hyd%file_lga, hyd%num_columns  , hyd%num_rows  , hyd%num_layers , hyd%nosegl, &
                          hyd%num_exchanges_u_dir    , hyd%num_exchanges_v_dir  , hyd%num_exchanges_z_dir  , hyd%lgrid )

         ! total grid table

         call write_lgt ( hyd%file_lgt, hyd%num_columns  , hyd%num_rows  , hyd%num_layers )

         ! cco file
         call write_cco ( hyd%file_cco, hyd%num_columns  , hyd%num_rows  , hyd%xdepth, hyd%ydepth, &
                          hyd%num_layers   )
      else if (hyd%geometry .eq. HYD_GEOM_UNSTRUC) then
         hyd%waqgeom%epsg = hyd%crs%epsg_code
         success =  write_waqgeom_file(hyd%file_geo%name, hyd%meta, hyd%crs, hyd%waqgeom, &
                                       hyd%edge_type, hyd%conv_type, hyd%conv_version)
         call write_bnd(hyd)
      endif

      ! pointer table

      call write_poi ( hyd%file_poi, hyd%num_exchanges   , hyd%num_exchanges_u_dir    , hyd%num_exchanges_v_dir  , hyd%num_exchanges_z_dir  , &
                       hyd%ipoint  )

      ! surf

      if (hyd%geometry .eq. HYD_GEOM_UNSTRUC) then
         write(lunrep,'(2a)') ' write horizontal surfaces file : ',trim(hyd%file_hsrf%name)
         call write_hsrf ( hyd%file_hsrf, hyd%num_cells, hyd%surf)
      endif
      if ( hyd%file_srf%name .ne. ' ' ) then
         write(lunrep,'(2a)') ' write surface areas file : ',trim(hyd%file_srf%name)
         call write_srf ( hyd%file_srf, hyd%num_columns  , hyd%num_rows  , hyd%nosegl, hyd%surf)
      endif

      ! depth

      if (hyd%geometry .eq. HYD_GEOM_CURVI) then
         if ( hyd%file_dps%name .ne. ' ' ) then
            write(lunrep,'(2a)') ' write depth file : ',trim(hyd%file_dps%name)
            call write_srf ( hyd%file_dps, hyd%num_columns  , hyd%num_rows  , hyd%nosegl, hyd%depth)
         endif
      endif

      ! attributes

      if ( hyd%file_atr%name .ne. ' ' ) then
         if (hyd%geometry .eq. HYD_GEOM_UNSTRUC) then
            hyd%atr_type = ATR_FM
         endif
         write(lunrep,'(2a)') ' write attributes file : ',trim(hyd%file_atr%name)
         call write_atr ( hyd )
      endif

      ! dispersion length

      itime     = 0
      valnam(1) = 'displen-from'
      valnam(2) = 'displen-to'
      write(lunrep,'(2a)') ' writing dispersion length file : ',trim(hyd%file_len%name)
      call write_data( hyd%file_len, itime, 1, hyd%num_exchanges_u_dir, hyd%num_exchanges_v_dir, hyd%num_exchanges_z_dir, 2, 1, 0, valnam, hyd%displen,0)

      return
      end
