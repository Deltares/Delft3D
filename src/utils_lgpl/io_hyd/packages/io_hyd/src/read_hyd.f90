!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
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

      subroutine read_hyd(hyd)
      ! read a hydrodynamic description file

      use m_logger_helper, only : get_log_unit_number, write_error_message
      use time_module
      use waq_file_utils_external, only : get_filepath_and_pathlen
      use m_hydmod
      use m_hyd_keys
      use rd_token       ! tokenized reading
      use m_string_utils, only: index_in_array
      use Ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_is_finite

      implicit none

      ! declaration of the arguments

      type(t_hydrodynamics)         :: hyd                    ! description of the hydrodynamics

      ! local declarations

      integer             :: lunhyd                 ! unit number hyd file
      integer             :: lunrep                 ! unit number report file
      integer             :: ilay                   ! index layers
      integer             :: i_desc                 ! index in description
      integer             :: i_wasteload            ! index in collection
      type(t_wasteload)   :: wasteload              ! one wasteload description
      integer             :: i_domain               ! index in collection
      type(t_domain)      :: domain                 ! one domain description
      integer             :: i_dd_bound             ! index in collection
      type(t_dd_bound)    :: dd_bound               ! one dd_bound description
      character(len=255)  :: line                   ! line buffer input file
      character(len=255)  :: ctoken                 ! line buffer input file
      integer             :: ierr                   ! error indicator
      logical             :: token_used             ! token_used
      integer             :: platform               ! computer platform
      integer             :: ft_dat                 ! type of the data files
      integer             :: i_swap                 ! variable used in swapping values
      character(len=256)  :: filpath                ! path to hyd file
      integer             :: pathlen                ! lentgth of path to hyd file
      integer             :: idummy                 ! idummy
      real                :: rdummy                 ! rdummy
      character           :: cdummy                 ! cdummy
      integer             :: itype                  ! itype
      integer             :: ierr2                  ! ierr2
      logical             :: lfound                 ! indication if command line argument was found
      integer             :: iy                     ! year
      integer             :: imo                    ! month
      integer             :: id                     ! day
      integer             :: ih                     ! hour
      integer             :: im                     ! minute
      integer             :: is                     ! second
      integer             :: idate                  ! date
      integer             :: itime                  ! time
      logical, parameter  :: untileol = .true.      ! read until the end of the line

      ft_dat = ft_bin
      call get_log_unit_number(lunrep)

      hyd%file_hyd%type = ft_asc
      call hyd%file_hyd%open()

      ! initialise tokenised reading
      ilun    = 0
      ilun(1) = hyd%file_hyd%unit
      lch (1) = hyd%file_hyd%name
      npos   = 1000
      cchar  = '#'
      ierr = 0

      hyd%description = ' '
      call get_filepath_and_pathlen ( hyd%file_hyd%name, filpath, pathlen)

      hyd%wasteload_coll%current_size = 0
      hyd%wasteload_coll%maxsize = 0
      hyd%domain_coll%current_size = 0
      hyd%domain_coll%maxsize = 0
      hyd%dd_bound_coll%current_size = 0
      hyd%dd_bound_coll%maxsize = 0
      hyd%file_com=t_file(' ',' ',0,FT_NEF,FILE_STAT_UNOPENED)
      hyd%file_dwq=t_file(' ',' ',0,FT_ASC,FILE_STAT_UNOPENED)
      hyd%file_vag=t_file(' ',' ',0,FT_ASC,FILE_STAT_UNOPENED)
      hyd%file_lga=t_file(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%file_cco=t_file(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%file_bnd=t_file(' ',' ',0,FT_ASC,FILE_STAT_UNOPENED)
      hyd%file_geo=t_file(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%file_vol=t_file(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%file_are=t_file(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%file_flo=t_file(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%file_poi=t_file(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%file_len=t_file(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%file_sal=t_file(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%file_tem=t_file(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%file_vdf=t_file(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%file_srf=t_file(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%file_hsrf=t_file(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%file_lgt=t_file(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%file_src=t_file(' ',' ',0,FT_ASC,FILE_STAT_UNOPENED)
      hyd%file_chz=t_file(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%file_tau=t_file(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%file_wlk=t_file(' ',' ',0,FT_ASC,FILE_STAT_UNOPENED)
      hyd%file_atr=t_file(' ',' ',0,FT_ASC,FILE_STAT_UNOPENED)
      hyd%file_dps=t_file(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%num_columns = 0
      hyd%num_rows = 0
      hyd%num_layers_grid = 1
      hyd%nosegl = 0
      hyd%num_cells = 0
      hyd%num_layers = 1
      hyd%num_exchanges_u_dir = 0
      hyd%num_exchanges_v_dir = 0
      hyd%num_exchanges_z_dir = 0
      hyd%num_exchanges_bottom_dir = 0
      hyd%num_exchanges  = 0

      hyd%zbot = ieee_value(hyd%zbot, ieee_quiet_nan)
      hyd%ztop = ieee_value(hyd%ztop, ieee_quiet_nan)

      ! loop over all the tokens in the file

      do

         ! if end of file the exit loop
         if ( gettoken( ctoken, idummy, rdummy, itype, ierr) .ne. 0 ) exit
         if (itype .ne. 1) then
             goto 900
         end if

         if ( ctoken .eq. task ) then

            ! task
            if ( gettoken( ctoken, ierr) .ne. 0 ) goto 900
            if ( ctoken == full_coupling ) then
               hyd%task = HYD_TASK_FULL
            elseif ( ctoken == coupling_per_domain ) then
               hyd%task = HYD_TASK_DDC
            else
               hyd%task = HYD_TASK_UNKNOWN
               write(lunrep,'(a)') ' warning unknown task in hydrodynamic file'
               write(lunrep,'(2a)') ' task =',trim(ctoken)
            endif

         elseif ( ctoken .eq. geometry ) then
            ! geometry
            if ( gettoken( ctoken, ierr) .ne. 0 ) goto 900
            if ( ctoken == curvilinear_grid ) then
               hyd%geometry = HYD_GEOM_CURVI
            elseif ( ctoken == unstructured ) then
               hyd%geometry = HYD_GEOM_UNSTRUC
            else
               hyd%geometry = HYD_GEOM_UNKNOWN
               write(lunrep,'(a)') ' warning unknown geometry in hydrodynamic file'
               write(lunrep,'(2a)') ' geometry =',trim(ctoken)
            endif

            ! layer type
            hyd%layer_type = HYD_LAYERS_SIGMA ! Always assume sigma layers, unless otherwise stated
            if ( gettoken( ctoken, ierr) .eq. 0 ) then
               if ( ctoken == z_layers ) then
                  hyd%layer_type = HYD_LAYERS_Z
               else
                  if ( puttoken( ctoken ) .ne. 0 ) goto 900
               end if
            else
               if ( puttoken( ctoken ) .ne. 0 ) goto 900
            end if
         elseif ( ctoken .eq. description ) then
            ! description
            i_desc = 0
            do
               ! look for end-description token
               if ( gettoken ( ctoken, ierr) .ne. 0 ) goto 900
               if ( ctoken == end_description ) exit
               ! it is a description line, store up to three
               i_desc = i_desc + 1
               if ( i_desc .le. 3 ) hyd%description(i_desc) = ctoken
            enddo

         elseif ( ctoken == reference_time ) then
            ! reference time
            if ( gettoken (hyd%hyd_ref, ierr) .ne. 0 ) goto 900
            ! convert to julian
            read (hyd%hyd_ref(1:8),'(i8)') idate
            read (hyd%hyd_ref(9:14),'(i6)') itime
            hyd%time_ref = julian_with_leapyears ( idate , itime )

         elseif ( ctoken == hydrodynamic_start_time ) then
            ! hydrodynamic start
            if ( gettoken(hyd%hyd_start, ierr) .ne. 0 ) goto 900

         elseif ( ctoken == hydrodynamic_stop_time) then
            ! hydrodynamic stop
            if ( gettoken(hyd%hyd_stop, ierr) .ne. 0 ) goto 900

         elseif ( ctoken == hydrodynamic_timestep) then
            ! hydrodynamic step
            if ( gettoken(hyd%hyd_step, ierr) .ne. 0 ) goto 900

         elseif ( ctoken == conversion_ref_time) then
            ! conversion reference time
            if ( gettoken(hyd%cnv_ref, ierr) .ne. 0 ) goto 900

         elseif ( ctoken == conversion_start_time) then
            ! conversion start time
            if ( gettoken(hyd%cnv_start, ierr) .ne. 0 ) goto 900

         elseif ( ctoken == conversion_stop_time) then
            ! conversion stop time
            if ( gettoken(hyd%cnv_stop, ierr) .ne. 0 ) goto 900

         elseif ( ctoken == conversion_timestep) then
            ! conversion step time
            if ( gettoken(hyd%cnv_step, ierr) .ne. 0 ) goto 900
            read(hyd%cnv_step,'(i4,i2,i2,i2,i2,i2)') iy,imo,id,ih,im,is
            if (iy .ne. 0 .or. imo .ne. 0 ) then
               write(lunrep,*) ' error conversion step has year or month, this is not supported'
               goto 900
            endif
            hyd%cnv_step_sec = id*86400+ih*3600+im*60+is

         elseif ( ctoken == grid_cells_first_direction) then
            ! grid cells first direction
            if ( gettoken(hyd%num_columns, ierr) .ne. 0 ) goto 900

         elseif ( ctoken == grid_cells_second_direction) then
            ! grid cells second direction
            if ( gettoken(hyd%num_rows, ierr) .ne. 0 ) goto 900

         elseif ( ctoken == number_hydrodynamic_layers) then
            ! number of hydrodynamic layers
            if ( gettoken(hyd%num_layers_grid, ierr) .ne. 0 ) goto 900

         elseif ( ctoken == number_water_quality_layers) then
            ! number of waq layers
            if ( gettoken(hyd%num_layers, ierr) .ne. 0 ) goto 900

         elseif ( ctoken == number_horizontal_exchanges) then
            ! number of horizontal exchanges
            if ( gettoken(hyd%num_exchanges_u_dir, ierr) .ne. 0 ) goto 900

         elseif ( ctoken == number_vertical_exchanges) then
            ! number of vertical exchanges
            if ( gettoken(hyd%num_exchanges_z_dir, ierr) .ne. 0 ) goto 900

         elseif ( ctoken == number_water_quality_segments_per_layer) then
            ! number of water quality segments per layer
            if ( gettoken(hyd%nosegl, ierr) .ne. 0 ) goto 900

         elseif ( ctoken == hydrodynamic_file) then
            ! com file
            if ( gettoken(hyd%file_com%name, ierr) .ne. 0 ) goto 900
            hyd%file_com%name = trim(filpath)//hyd%file_com%name

         elseif ( ctoken == aggregation_file) then
            ! dwq file
            if ( gettoken(hyd%file_dwq%name, ierr) .ne. 0 ) goto 900
            hyd%file_dwq%name = trim(filpath)//hyd%file_dwq%name

         elseif ( ctoken == grid_indices_file) then
            ! lga file
            if ( gettoken(hyd%file_lga%name, ierr) .ne. 0 ) goto 900
            hyd%file_lga%name = trim(filpath)//hyd%file_lga%name

         elseif ( ctoken == grid_coordinates_file) then
            ! cco file
            if ( gettoken(hyd%file_cco%name, ierr) .ne. 0 ) goto 900
            hyd%file_cco%name = trim(filpath)//hyd%file_cco%name

         elseif ( ctoken == boundaries_file) then
            ! bnd file (unstructured)
            if ( gettoken(hyd%file_bnd%name, ierr) .ne. 0 ) goto 900
            hyd%file_bnd%name = trim(filpath)//hyd%file_bnd%name

         elseif ( ctoken == waqgeom_file) then
            ! waqgeom file (unstructured)
            if ( gettoken(hyd%file_geo%name, ierr) .ne. 0 ) goto 900
            hyd%file_geo%name = trim(filpath)//hyd%file_geo%name

         elseif ( ctoken == volumes_file) then
            ! vol file
            if ( gettoken(hyd%file_vol%name, ierr) .ne. 0 ) goto 900
            hyd%file_vol%name = trim(filpath)//hyd%file_vol%name

         elseif ( ctoken == areas_file) then
            ! are file
            if ( gettoken(hyd%file_are%name, ierr) .ne. 0 ) goto 900
            hyd%file_are%name = trim(filpath)//hyd%file_are%name

         elseif ( ctoken == flows_file) then
            ! flo file
            if ( gettoken(hyd%file_flo%name, ierr) .ne. 0 ) goto 900
            hyd%file_flo%name = trim(filpath)//hyd%file_flo%name

         elseif ( ctoken == pointers_file) then
            ! poi file
            if ( gettoken(hyd%file_poi%name, ierr) .ne. 0 ) goto 900
            hyd%file_poi%name = trim(filpath)//hyd%file_poi%name

         elseif ( ctoken == lengths_file) then
            ! len file
            if ( gettoken(hyd%file_len%name, ierr) .ne. 0 ) goto 900
            hyd%file_len%name = trim(filpath)//hyd%file_len%name

         elseif ( ctoken == salinity_file) then
            ! sal file
            if ( gettoken(ctoken, ierr) .ne. 0 ) goto 900
            if ( ctoken.ne. 'none' ) then
               hyd%file_sal%name = trim(filpath)//ctoken
               hyd%sal_present = .true.
            else
               hyd%file_sal%name = ' '
               hyd%sal_present = .false.
            endif

         elseif ( ctoken == temperature_file) then
            ! tmp file
            if ( gettoken(ctoken, ierr) .ne. 0 ) goto 900
            if ( ctoken.ne. 'none' ) then
               hyd%file_tem%name = trim(filpath)//ctoken
               hyd%tem_present = .true.
            else
               hyd%file_tem%name = ' '
               hyd%tem_present = .false.
            endif

         elseif ( ctoken == vert_diffusion_file) then
            ! vdf file
            if ( gettoken(ctoken, ierr) .ne. 0 ) goto 900
            if ( ctoken.ne. 'none' ) then
               hyd%file_vdf%name = trim(filpath)//ctoken
               hyd%vdf_present = .true.
            else
               hyd%file_vdf%name = ' '
               hyd%vdf_present = .false.
            endif

         elseif ( ctoken == surfaces_file) then
            ! srf file
            if ( gettoken(hyd%file_srf%name, ierr) .ne. 0 ) goto 900
            hyd%file_srf%name = trim(filpath)//hyd%file_srf%name

         elseif ( ctoken == horizontal_surfaces_file) then
            ! hsrf file
            if ( gettoken(hyd%file_hsrf%name, ierr) .ne. 0 ) goto 900
            hyd%file_hsrf%name = trim(filpath)//hyd%file_hsrf%name

         elseif ( ctoken == total_grid_file) then
            ! lgt file
            if ( gettoken(hyd%file_lgt%name, ierr) .ne. 0 ) goto 900
            hyd%file_lgt%name = trim(filpath)//hyd%file_lgt%name

         elseif ( ctoken == discharges_file) then
            ! src file
            if ( gettoken(hyd%file_src%name, ierr) .ne. 0 ) goto 900
            hyd%file_src%name = trim(filpath)//hyd%file_src%name

         elseif ( ctoken == chezy_coefficients_file) then
            ! chz file
            if ( gettoken(hyd%file_chz%name, ierr) .ne. 0 ) goto 900
            hyd%file_chz%name = trim(filpath)//hyd%file_chz%name

         elseif ( ctoken == shear_stresses_file) then
            ! tau file
            if ( gettoken(ctoken, ierr) .ne. 0 ) goto 900
            if ( ctoken.ne. 'none' ) then
               hyd%file_tau%name = trim(filpath)//ctoken
               hyd%tau_present = .true.
            else
               hyd%file_tau%name = ' '
               hyd%tau_present = .false.
            endif

         elseif ( ctoken == walking_discharges_file) then
            ! wlk file
            if ( gettoken(hyd%file_wlk%name, ierr) .ne. 0 ) goto 900
            hyd%file_wlk%name = trim(filpath)//hyd%file_wlk%name

         elseif ( ctoken == attributes_file) then
            ! attrubutes file
            if ( gettoken(hyd%file_atr%name, ierr) .ne. 0 ) goto 900
            hyd%file_atr%name = trim(filpath)//hyd%file_atr%name

         elseif ( ctoken == depths_file) then
            ! depths file
            if ( gettoken(hyd%file_dps%name, ierr) .ne. 0 ) goto 900
            hyd%file_dps%name = trim(filpath)//hyd%file_dps%name

         elseif ( ctoken == z_layers_ztop) then
            ! ztop
            if ( gettoken( hyd%ztop, ierr) .ne. 0 ) goto 900

         elseif ( ctoken == z_layers_zbot) then
            ! ztop
            if ( gettoken( hyd%zbot, ierr) .ne. 0 ) goto 900

         elseif ( ctoken == hydrodynamic_layers) then
            ! hydrodynamic-layers
            allocate(hyd%hyd_layers(hyd%num_layers_grid))
            do ilay = 1 , hyd%num_layers_grid
               if ( gettoken(hyd%hyd_layers(ilay), ierr) .ne. 0 ) goto 900
            enddo
            ! end-hydrodynamic-layers
            if ( gettoken(ctoken, ierr) .ne. 0 ) goto 900

         elseif ( ctoken == water_quality_layers) then
            ! water-quality-layers
            allocate(hyd%waq_layers(hyd%num_layers))
            do ilay = 1 , hyd%num_layers
               if ( gettoken(hyd%waq_layers(ilay), ierr) .ne. 0 ) goto 900
            enddo
            ! end-water-quality-layers
            if ( gettoken(ctoken, ierr) .ne. 0 ) goto 900

         elseif ( ctoken == discharges) then
            ! discharges
            token_used = .true.
            do
               if ( token_used ) then
                  if ( gettoken(ctoken, idummy, rdummy, itype, ierr) .ne. 0 ) goto 900
               endif
               if ( ctoken == end_discharges ) exit

               ! a new wasteload
               if ( itype .eq. TYPE_INT ) then
                  wasteload%n    = idummy
               else
                  goto 900
               endif
               if ( gettoken(wasteload%m, ierr) .ne. 0 ) goto 900
               if ( gettoken(wasteload%k, ierr) .ne. 0 ) goto 900
               if ( gettoken(wasteload%name, ierr) .ne. 0 ) goto 900
               if ( gettoken(ctoken, ierr) .ne. 0 ) goto 900
               if ( ctoken == normal .or. ctoken == inlet .or. ctoken == outlet .or. ctoken == walking ) then
                  token_used = .true.
                  if ( ctoken == normal ) then
                     wasteload%type = DLWQ_WASTE_NORMAL
                  elseif ( ctoken == inlet ) then
                     wasteload%type = DLWQ_WASTE_INLET
                  elseif ( ctoken == outlet ) then
                     wasteload%type = DLWQ_WASTE_OUTLET
                  elseif ( ctoken == walking ) then
                     wasteload%type = DLWQ_WASTE_WALK
                  endif
               else
                  wasteload%type = DLWQ_WASTE_NORMAL
                  token_used = .false.
               endif
               wasteload%waqtype = ' '

               ! add to wasteload collection
               i_wasteload = hyd%wasteload_coll%add(wasteload)

            enddo

         elseif ( ctoken == domains) then
            ! domains
            do
               if ( gettoken(ctoken, ierr) .ne. 0 ) goto 900
               ! look for end-domains keyword
               if ( ctoken == end_domains ) exit

               ! key is domain name , read num_columns num_rows and dido file do not store dido file
               domain%name = ctoken
               if ( gettoken(domain%num_columns, ierr) .ne. 0 ) goto 900
               if ( gettoken(domain%num_rows, ierr) .ne. 0 ) goto 900
               if ( gettoken(ctoken, ierr) .ne. 0 ) goto 900

               ! add to domains collection
               i_domain = hyd%domain_coll%add(domain)
            enddo

         elseif ( ctoken == dd_boundaries) then
            ! dd-boundaries
            do
               if ( gettoken(ctoken, ierr) .ne. 0 ) goto 900

               ! look for end-dd-boundaries keyword
               if ( ctoken == end_dd_boundaries ) exit

               ! ctokenis domain name 1 , read m_begin1, n_begin1, m_end1, n_end1, domain name 2, m_begin2, n_begin2, m_end2, n_end2
               dd_bound%name1 = ctoken
               if (gettoken(dd_bound%m_begin1, ierr) .ne. 0 ) goto 900
               if (gettoken(dd_bound%n_begin1, ierr) .ne. 0 ) goto 900
               if (gettoken(dd_bound%m_end1, ierr)   .ne. 0 ) goto 900
               if (gettoken(dd_bound%n_end1, ierr)   .ne. 0 ) goto 900

               if (gettoken(dd_bound%name2, ierr)    .ne. 0 ) goto 900
               if (gettoken(dd_bound%m_begin2, ierr) .ne. 0 ) goto 900
               if (gettoken(dd_bound%n_begin2, ierr) .ne. 0 ) goto 900
               if (gettoken(dd_bound%m_end2, ierr)   .ne. 0 ) goto 900
               if (gettoken(dd_bound%n_end2, ierr)   .ne. 0 ) goto 900

               ! make sure the numbering is always increasing

               if ( dd_bound%m_begin1 .gt. dd_bound%m_end1 ) then
                  i_swap            = dd_bound%m_begin1
                  dd_bound%m_begin1 = dd_bound%m_end1
                  dd_bound%m_end1   = i_swap
               endif
               if ( dd_bound%n_begin1 .gt. dd_bound%n_end1 ) then
                  i_swap            = dd_bound%n_begin1
                  dd_bound%n_begin1 = dd_bound%n_end1
                  dd_bound%n_end1   = i_swap
               endif
               if ( dd_bound%m_begin2 .gt. dd_bound%m_end2 ) then
                  i_swap            = dd_bound%m_begin2
                  dd_bound%m_begin2 = dd_bound%m_end2
                  dd_bound%m_end2   = i_swap
               endif
               if ( dd_bound%n_begin2 .gt. dd_bound%n_end2 ) then
                  i_swap            = dd_bound%n_begin2
                  dd_bound%n_begin2 = dd_bound%n_end2
                  dd_bound%n_end2   = i_swap
               endif

               ! add to dd_bound collection

               i_dd_bound = hyd%dd_bound_coll%add(dd_bound)

            enddo

         elseif ( ctoken == file_created_by) then
            ! file-created-by string.
            if (gettoken(line, untileol, ierr) .ne. 0 ) goto 900
            hyd%created_by = line(1:80)

         elseif ( ctoken == file_creation_date) then
            ! file-creation-date
            if (gettoken(line, untileol, ierr) .ne. 0 ) goto 900
            hyd%creation_date = line(1:40)

         elseif ( ctoken == sink_sources) then
            ! sink-sources
            do
               if ( gettoken(ctoken, idummy, rdummy, itype, ierr) .ne. 0 ) goto 900
                  if(itype==1) then
                     ! look for end-domains keyword
                     if ( ctoken == end_sink_sources ) exit
                  endif
            enddo

         else
            ! unknown keyword, ignore until the end of the line
            if (gettoken(line, untileol, ierr) .ne. 0 ) goto 900

         endif

      enddo

      ! 2d then no vdf file

      if ( hyd%num_layers .le. 1 ) then
         hyd%file_vdf%name = ' '
         hyd%vdf_present = .false.
      endif

      ! unstructured set num_rows to 1

      if ( hyd%geometry .eq. HYD_GEOM_UNSTRUC ) then
         hyd%num_rows = 1
      endif

      ! check for ztop and zbot keywords

      if ( hyd%layer_type == HYD_LAYERS_Z ) then
         if ( .not. ieee_is_finite(hyd%zbot) .or. .not. ieee_is_finite(hyd%ztop) ) then
            call write_error_message('Error: hyd-file with z-layers should contain values for '// &
                     'keywords "z-layers-ztop" and "z-layers-zbot"')
         endif
         if ( hyd%zbot >=  hyd%ztop ) then
            call write_error_message('Error: the value for "z-layers-ztop" in the hyd-file ' // &
                     'should be larger than the value for "z-layers-bot"')
         endif
      endif

      return
 900  continue
      call write_error_message('error reading hyd file ('//trim(ctoken)//')')
      end subroutine read_hyd
