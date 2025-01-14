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

module m_flow_externaloutput
   use m_step_to_screen, only: step_to_screen
   use m_inctime_split, only: inctime_split

   implicit none

   private

   public :: flow_externaloutput

contains

   !> Write solution data to output files (map/his/restart/waq).
 !! Each output type has its own interval (see m_flowtimes),
 !! and output is only written if the current time tim exceeds the last
 !! written interval.
   subroutine flow_externaloutput(tim) ! give signals etc, write map, his etc
      use m_wrirst, only: wrirst
      use m_wricom, only: wricom
      use m_update_flowanalysis_parameters, only: updateFlowAnalysisParameters
      use m_unc_write_shp, only: unc_write_shp
      use m_flowtimes
      use m_flow
      use unstruc_model
      use unstruc_netcdf
      use unstruc_netcdf_map_class
      use m_xbeach_netcdf
      use waq
      use m_timer
      use m_partitioninfo, only: jampi, my_rank
      use m_flowparameters, only: jashp_crs, jashp_obs, jashp_weir, jashp_thd, jashp_gate, jashp_emb, jashp_fxw, jashp_src
      use m_flowgeom, only: ndx2d, ndxi, lnx1d
      use unstruc_channel_flow, only: network
      use m_oned_functions, only: updateFreeboard, updateDepthOnGround, updateVolOnGround, updateTotalInflow1d2d, updateTotalInflowLat, updateS1Gradient
      use m_structures, only: structure_parameters_rst
      use m_monitoring_runupgauges
      use Timers
      use system_utils, only: makedir
      use fm_statistical_output, only: out_variable_set_his
      use m_statistical_output, only: reset_statistical_output, finalize_average
      use m_wrimap
      use m_wribal
      use m_wrihistek
      use m_unc_write_his, only: unc_write_his
      use m_print_timings, only: print_timings

#ifdef _OPENMP
      use omp_lib
#endif

      real(kind=dp), intent(in) :: tim !< Current time, should in fact be time1, since all writers use s1, q1, etc.
      real(kind=dp) :: time_map_mpt
      real(dp) :: time_com_ctv
      real(kind=dp) :: runtime
      real(kind=dp) :: tem_dif

      call inctime_split(tim)

      if (ti_his > 0) then
         if (comparereal(tim, time_his, eps10) >= 0) then
            if (out_variable_set_his%count > 0) then
               call finalize_average(out_variable_set_his%statout)
            end if

            if (jampi == 0 .or. (jampi == 1 .and. my_rank == 0)) then
               call unc_write_his(tim) ! wrihis
            end if
            if (out_variable_set_his%count > 0) then
               call reset_statistical_output(out_variable_set_his%statout)
            end if
            if (num_rugs > 0) then
               ! needs to be done at exactly ti_his, but over all domains, so cannot go in wrihis
               call clear_runup_gauges()
            end if
            if (comparereal(time_his, ti_hise, eps10) == 0) then
               time_his = tstop_user + 1
            else
               tem_dif = (tim - ti_hiss) / ti_his
               time_his = max(ti_hiss + (floor(tem_dif + 0.001d0) + 1) * ti_his, ti_hiss)
               if (comparereal(time_his, ti_hise, eps10) == 1) then
                  ! next time_his would be beyond end of his-window, write one last his exactly at that end.
                  time_his = ti_hise
               end if
            end if
         end if
      end if

      if (.not. allocated(ti_mpt)) then
         allocate (ti_mpt(1), ti_mpt_rel(1)); ti_mpt(1) = 0; ti_mpt_rel(1) = 0
      end if

      call timstrt('call wrimap', handle_extra(77))
      if (ti_map > 0d0 .or. ti_mpt(1) > 0) then
         if (comparereal(tim, time_map, eps10) >= 0) then
            ! update for output, only for 1D
            if (network%loaded) then
               if (ndxi - ndx2d > 0) then
                  if (jamapFreeboard > 0) then
                     call updateFreeboard(network)
                  end if
                  if (jamapDepthOnGround > 0) then
                     call updateDepthOnGround(network)
                  end if
                  if (jamapVolOnGround > 0) then
                     call updateVolOnGround(network)
                  end if
                  ! NOTE: updateTotalInflow1d2d, updateTotalInflowLat done in flow_finalizesingletimestep().
               end if
            end if
            if (lnx1d > 0) then
               if (jamapS1Gradient > 0) then
                  call updateS1Gradient()
               end if
            end if

            if (jaeverydt == 0) then
               if (jamapFlowAnalysis > 0) then
                  ! update the cumulative flow analysis parameters, and also compute the right CFL numbers
                  call updateFlowAnalysisParameters()
               end if

               call wrimap(tim)

               if (jamapFlowAnalysis > 0) then
                  ! Reset the interval related flow analysis arrays
                  negativeDepths = 0
                  noiterations = 0
                  limitingTimestepEstimation = 0
                  flowCourantNumber = 0d0
               end if
            end if
            if (comparereal(time_map, ti_mape, eps10) == 0) then
               time_map = tstop_user + 1
            else
               tem_dif = (tim - ti_maps) / ti_map
               time_map = max(ti_maps + (floor(tem_dif + 0.001d0) + 1) * ti_map, ti_maps)
               ti_mpt_rel = ti_mpt - tim
               time_map_mpt = tim + minval(ti_mpt_rel, mask=ti_mpt_rel > 0)
               if (comparereal(time_map, time_map_mpt, eps10) == 1 .and. comparereal(tim, time_map_mpt, eps10) == -1) then
                  time_map = time_map_mpt
               end if

               if (comparereal(time_map, ti_mape, eps10) == 1) then
                  ! next time_map would be beyond end of map-window, write one last map exactly at that end.
                  time_map = ti_mape
               end if

            end if
         end if
      end if
      call timstop(handle_extra(77))

      call timstrt('call wriclm', handle_extra(78))
      if (ti_classmap > 0) then
         if (comparereal(tim, time_classmap, eps10) >= 0) then
            call write_map_classes_ugrid(m_incids, tim)
            if (comparereal(time_classmap, ti_classmape, eps10) == 0) then
               time_classmap = tstop_user + 1
            else
               tem_dif = (tim - ti_classmaps) / ti_classmap
               time_classmap = max(ti_classmaps + (floor(tem_dif + 0.001d0) + 1) * ti_classmap, ti_classmaps)

               if (comparereal(time_classmap, ti_classmape, eps10) == 1) then
                  ! next time_classmap would be beyond end of incr-window, write one last incr exactly at that end.
                  time_classmap = ti_classmape
               end if
            end if
         end if
      end if
      call timstop(handle_extra(78))

      ! Write com file
      if (jawave == 3) then
         !
         if (ti_com /= dt_user .or. ti_ctv(1) > 0) then
            !
            if (comparereal(tim, time_com, eps10) >= 0) then
               !
               call wricom(tim)
               !call mess(LEVEL_INFO,'com file written at t=', tim)
               !
               ! Update next com write instant
               if (comparereal(time_com, ti_come, eps10) == 0) then
                  time_com = tstop_user + 1
               else
                  tem_dif = (tim - ti_coms) / ti_com
                  if (isnan(tem_dif)) tem_dif = 0.0_hp
                  time_com = max(ti_coms + (floor(tem_dif + 0.001d0) + 1) * ti_com, ti_coms)
                  ti_ctv_rel = ti_ctv - tim
                  time_com_ctv = tim + minval(ti_ctv_rel, mask=ti_ctv_rel > 0)
                  if (comparereal(time_com, time_com_ctv, eps10) == 1 .and. comparereal(tim, time_com_ctv, eps10) == -1) then
                     time_com = time_com_ctv
                  end if
                  !
                  if (comparereal(time_com, ti_come, eps10) == 1) then
                     ! next time_com would be beyond end of com-window, write one last comfile exactly at that end.
                     time_com = ti_come
                  end if
               end if
            end if ! time_com
         else
            call wricom(tim) ! legacy behaviour, write at dt_user
            !call mess(LEVEL_INFO,'com file written at t=', tim)
         end if ! ti_com
      end if ! jawave

      if (ti_xls > 0) then
         if (tim >= time_xls) then
            call wrihistek(tim) ! wrihis xls
            call wribal()
            time_xls = tim + ti_xls
         end if
      end if

      call timstrt('call wrirst', handle_extra(76))
      if (ti_rst > 0) then
         if (comparereal(tim, time_rst, eps10) == 0) then
            ! Update structure parameters
            call structure_parameters_rst()
            call wrirst(tim)
            if (comparereal(time_rst, ti_rste, eps10) == 0) then
               time_rst = tstop_user + 1
            else
               tem_dif = (tim - ti_rsts) / ti_rst
               time_rst = max(ti_rsts + (floor(tem_dif + 0.001d0) + 1) * ti_rst, ti_rsts)
               if (comparereal(time_rst, ti_rste, eps10) == 1) then
                  ! We've come beyond the end time of restart window.
                  ! Write just a last one exactly on that end time (i.e. not at tstop_user).
                  time_rst = ti_rste
               end if
            end if
         end if
      end if
      call timstop(handle_extra(76))

      if (ti_waq > 0) then

         if (.not. wrwaqon) then
            call waq_wri_model_files()
            wrwaqon = .true.
         end if

         if (tim >= ti_waqs .and. tim <= ti_waqe + 0.1d0 .and. tim >= time_waq - 0.1d0) then
            call waq_wri_couple_files(tim)
            time_waq = time_waq + ti_waq
         end if

      end if

      if (ti_stat > 0) then
         if (tim >= time_stat) then
            call step_to_screen(); time_stat = tim + ti_stat
         end if
      else if (ti_stat < 0d0) then
!     base statistics output on wallclock time, if available
         if (jatimer > 0) then
            runtime = gettimer(1, ITOTAL)
            if (runtime > time_stat) then
               call step_to_screen()
               time_stat = runtime + abs(ti_stat)
            end if
         end if
      end if

   !! Write shape files at the initialization
      if (abs(tim - tstart_user) < 1d-10) then
#ifdef HAVE_SHAPELIB
         call unc_write_shp()
#else
         if (jashp_crs > 0 .or. jashp_obs > 0 .or. jashp_weir > 0 .or. jashp_thd > 0 .or. jashp_gate > 0 &
             .or. jashp_emb > 0 .or. jashp_fxw > 0 .or. jashp_src > 0) then
            call mess(LEVEL_WARN, 'Shapefile export is not supported yet on the current platform.')
         end if
#endif
      end if

      if ((jatimer == 1) .and. (ti_timings > 0)) then
         if (tim >= time_timings) then
!        output timings
            call makedir(trim(getoutputdir())) ! safety, no problem if it exists already.
            call print_timings(trim(getoutputdir())//trim(md_ident)//'_timings.txt', time1)

! the following code changes timings filename, which is unfortunate for post-processing
!         if (len_trim(md_timingsfile) == 0) then
!            filepostfix = ' '
!            if (jampi == 1) then
!               write(filepostfix, '(a,i0)') '_MPI', ndomains
!            end if
!#ifdef _OPENMP
!            numomp = omp_get_max_threads()
!            write(filepostfix, '(a,i0)') '_OMP', numomp
!#endif
!            md_timingsfile = trim(getoutputdir()) // trim(md_ident) // trim(filepostfix) // '_timings.txt'
!         end if
!         call print_timings(md_timingsfile, time1)

            time_timings = tim + ti_timings
!         call initimer()   ! reset timers
         end if
      end if

   end subroutine flow_externaloutput

end module m_flow_externaloutput
