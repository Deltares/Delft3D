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

module m_fetch_local_data
use m_kcir

   logical, allocatable :: calculate_for(:)
   integer, allocatable :: list_of_upwind_cells(:), number_of_upwind_cells(:)
   double precision, allocatable :: data_at_upwind_cells(:, :)
   double precision, allocatable :: fetch_temp(:, :)
end module m_fetch_local_data

!> calculates fetch length and depth based significant wave height and period
!! based on Hurdle, Stive formulae, tauwave based on Swart, taus = taubmx = taucur + tauwave, as in Delwaq
subroutine tauwavefetch(tim)
   use m_sediment, only: rlabda
   use m_flowgeom, only: ndx, ndxi, ndx2d
   use m_flow
   use m_waves, only: fetch, nwf, fetdp, uorb, twav, hwav
   use m_flowtimes
   use m_partitioninfo
   use timers
   use m_drawthis
   use m_getfetch, only: getfetch
   use m_hurdlestive, only: hurdlestive
   use m_ian_young_pt, only: ian_young_pt
   use m_tauwavehk, only: tauwavehk

   implicit none

   double precision, intent(in) :: tim

   integer :: error, cell
   integer, save :: total_nr_cells
   integer, external :: initialise_fetch_proc_data
   logical, external :: stop_fetch_computation
   logical, parameter :: call_from_tauwavefetch = .true.
   double precision :: U10, fetchL, fetchd, hsig, tsig, rsqrt2, dum

   if (.not. allocated(fetch) .or. size(fetch, 2) /= ndx) then
      nwf = 13
      call timstrt('Ext.forcings fetch', handle_fetch)

      if (allocated(fetch)) deallocate (fetch)
      allocate (fetch(nwf, ndx), stat=error)
      call aerr('fetch(nwf, ndx)', error, ndx * nwf)
      if (allocated(fetdp)) deallocate (fetdp)
      allocate (fetdp(nwf, ndx), stat=error)
      call aerr('fetdp(nwf, ndx)', error, ndx * nwf)

      if (jampi == 1 .and. use_fetch_proc == 0) then
         call reduce_int_sum(ndxi, total_nr_cells)
      else
         total_nr_cells = ndxi
      end if

      call timstop(handle_fetch)
      if (use_fetch_proc > 0) then
         error = initialise_fetch_proc_data()
      end if
   end if

   if (tim >= time_fetch) then

      do !  inifinite loop for the fetch proc
         if (use_fetch_proc > 0) then
            if (stop_fetch_computation(call_from_tauwavefetch)) then
               return
            end if
         end if

         if (use_fetch_proc == 1) then
            call send_s1_to_fetch_proc()
         end if

         time_fetch = max(tim, time_fetch + tifetch)
         if (tifetch == 0d0) time_fetch = 1d30

         if (use_fetch_proc == 0 .or. my_rank == fetch_proc_rank) then
            call calculate_fetch_values_for_all_wind_directions(total_nr_cells)
         end if

         if (use_fetch_proc == 1) then
            call get_fetch_values_from_fetch_proc()
         end if

         if (use_fetch_proc == 0 .or. my_rank /= fetch_proc_rank) then
            exit
         end if
      end do
   end if

   rsqrt2 = 1.0d0 / sqrt(2d0)
   do cell = 1, ndx2d
      Hwav(cell) = 0d0
      Twav(cell) = 0d0
      Uorb(cell) = 0d0
      rlabda(cell) = 0d0

      if (hs(cell) > 0.01d0) then

         call getfetch(cell, U10, FetchL, FetchD)
         if (FetchL > 0) then

            select case (jawave)
            case (1)
               call hurdlestive(U10, fetchL, fetchD, Hsig, Tsig)
            case (2)
               call ian_young_pt(U10, fetchL, fetchD, Hsig, Tsig)
            end select

            Hwav(cell) = Hsig * rsqrt2 ! Hwav === hrms
            Twav(cell) = Tsig
            call tauwavehk(Hwav(cell), Twav(cell), hs(cell), Uorb(cell), rlabda(cell), dum) ! basically now just a dispersion function with 2DH stokes drift magnitude
         end if
      end if

      select case (NDRAW(28))
      case (35)
         plotlin(cell) = fetchL
      case (36)
         plotlin(cell) = fetchD
      case (37)
         plotlin(cell) = Hsig
      case (38)
         plotlin(cell) = Tsig
      case (39)
         !  plotlin(k) = Taucur
      case (40)
         plotlin(cell) = uorb(cell)
      end select

   end do

   call get_phiwav_values()

   call copy_values_to_boundary_nodes()

end subroutine tauwavefetch

!> calculates fetch length and depth
subroutine calculate_fetch_values_for_all_wind_directions(total_nr_cells)
   use m_arrowsxy
   use m_netw
   use m_flowgeom
   use m_flow
   use m_flowtimes
   use timers
   use m_waves, only: nwf, fetch, fetdp
   use m_partitioninfo
   use m_gui
   use m_wearelt
   use m_missing, only: dmiss
   use m_sferic
   use m_fetch_local_data
   use m_set_col
   use m_cls1

   implicit none

   integer, intent(in) :: total_nr_cells

   integer :: cell, index_wind_direction, error
   integer :: nr_cells_done, nr_cells_done_red
   double precision :: wind_direction, u_wind, v_wind, xp, yp, vfw

   call timstrt('Ext.forcings fetch', handle_fetch)
   allocate (fetch_temp(2, ndx), stat=error)
   call aerr('fetch_temp(2, ndx)', error, ndx * 2)

   allocate (calculate_for(ndxi), stat=error)
   call aerr('calculate_for(ndxi)', error, ndxi)

   allocate (number_of_upwind_cells(0:ndxi), stat=error)
   call aerr('number_of_upwind_cells(0:ndxi)', error, ndxi + 1)
   allocate (list_of_upwind_cells(1), stat=error)
   allocate (data_at_upwind_cells(2, 1), stat=error)

   do index_wind_direction = 1, nwf
      fetch_temp = dmiss
      calculate_for = .true.

      wind_direction = twopi * (index_wind_direction - 1) / dble(nwf - 1)
      u_wind = cos(wind_direction)
      v_wind = sin(wind_direction)

      if (jagui > 0) then
         call cls1()
         call setcol(221)

         xp = 0.90 * x1 + 0.10 * x2
         yp = 0.15 * y1 + 0.85 * y2
         vfw = 0.1d0 * (x2 - x1) / 10d0 ! 10 m/s is 0.1*screen
         call arrowsxy(xp, yp, u_wind, v_wind, vfw)
      end if

      call search_starting_cells(u_wind, v_wind, nr_cells_done)

      if (jampi == 1 .and. use_fetch_proc == 0) then
         call update_ghosts(ITYPE_SaLL, 2, ndx, fetch_temp, error)
         call reduce_int_sum(nr_cells_done, nr_cells_done_red)
         nr_cells_done = nr_cells_done_red
      end if

      if (jagui > 0) call setcol(31)

      call make_list_of_upwind_cells(u_wind, v_wind)

      call calculate_fetch_values(nr_cells_done, total_nr_cells)

      do cell = 1, ndxi
         fetch(index_wind_direction, cell) = fetch_temp(1, cell)
         fetdp(index_wind_direction, cell) = fetch_temp(2, cell)
      end do

   end do

   deallocate (fetch_temp)
   deallocate (calculate_for)
   deallocate (number_of_upwind_cells)
   deallocate (list_of_upwind_cells)
   deallocate (data_at_upwind_cells)

   call timstop(handle_fetch)
end subroutine calculate_fetch_values_for_all_wind_directions

!< make a list of upwind cells for each cell for a given wind direction
subroutine make_list_of_upwind_cells(u_wind, v_wind)
   use m_flowgeom
   use m_fetch_local_data
   use m_alloc
   use m_qnerror

   implicit none

   double precision, intent(in) :: u_wind, v_wind

   character(1024) :: message2, message3
   integer :: cell, cell2, cell_link, index, link, error
   double precision :: cs, sn, prin, www

   number_of_upwind_cells = 0
   do cell = 1, ndxi
      if (calculate_for(cell)) then
         do cell_link = 1, nd(cell)%lnx
            link = abs(nd(cell)%ln(cell_link))
            cell2 = ln(1, link); if (cell2 == cell) cell2 = ln(2, link)
            if (kcs(cell2) == 2) then ! internal
               cs = u_wind * csu(link) + v_wind * snu(link)
               if (link /= nd(cell)%ln(cell_link)) cs = -cs

               if (cs > 0) then ! internal upwind cell
                  number_of_upwind_cells(cell) = number_of_upwind_cells(cell) + 1
               end if
            end if
         end do
      end if
   end do

   do cell = 1, ndxi
      number_of_upwind_cells(cell) = number_of_upwind_cells(cell) + number_of_upwind_cells(cell - 1)
   end do

   if (size(list_of_upwind_cells) < number_of_upwind_cells(ndxi)) then
      deallocate (list_of_upwind_cells)
      allocate (list_of_upwind_cells(number_of_upwind_cells(ndxi)), stat=error)
      call aerr('list_of_upwind_cells(number_of_upwind_cells(ndxi))', error, number_of_upwind_cells(ndxi))
   end if
   if (size(data_at_upwind_cells, 2) < number_of_upwind_cells(ndxi)) then
      deallocate (data_at_upwind_cells)
      allocate (data_at_upwind_cells(2, number_of_upwind_cells(ndxi)), stat=error)
      call aerr('data_at_upwind_cells(2,number_of_upwind_cells(ndxi))', error, 2 * number_of_upwind_cells(ndxi))
   end if
   list_of_upwind_cells = 0
   data_at_upwind_cells = 0d0

   do cell = 1, ndxi
      if (calculate_for(cell)) then
         index = 0
         do cell_link = 1, nd(cell)%lnx
            link = abs(nd(cell)%ln(cell_link))
            cell2 = ln(1, link); if (cell2 == cell) cell2 = ln(2, link)
            if (kcs(cell2) == 2) then ! internal
               cs = u_wind * csu(link) + v_wind * snu(link)
               if (link /= nd(cell)%ln(cell_link)) cs = -cs

               if (cs > 0) then ! internal upwind cell
                  index = index + 1
                  if (index > number_of_upwind_cells(cell) - number_of_upwind_cells(cell - 1)) then
                     write (message2, *) 'cell=', cell, ' cell_link=', cell_link
                     write (message3, *) 'index=', index, ' max=', number_of_upwind_cells(cell) - number_of_upwind_cells(cell - 1)
                     call qnerror('make_list_of_upwind_cells: error ', message2, message3)
                     return
                  end if
                  list_of_upwind_cells(number_of_upwind_cells(cell - 1) + index) = cell2
                  if (cs * cs < 1d0) then
                     sn = sqrt(1d0 - cs * cs)
                  else
                     sn = 0d0
                  end if
                  prin = dx(link) * cs
                  www = (cs + 0.05d0 * sn) * wu(link) / dx(link) ! some diffusion
                  data_at_upwind_cells(1, number_of_upwind_cells(cell - 1) + index) = www
                  data_at_upwind_cells(2, number_of_upwind_cells(cell - 1) + index) = prin
               end if
            end if
         end do
      end if
   end do

end subroutine make_list_of_upwind_cells

!< search cells that are starting points for the fetch length calculations
subroutine search_starting_cells(u_wind, v_wind, nr_cells_done)
   use m_netw
   use m_flowgeom
   use m_flow, only: s1, dxymis
   use m_flowtimes
   use timers
   use m_partitioninfo
   use m_gui
   use geometry_module, only: getdx, getdy, dbdistance, cross, normalout, normalin
   use m_missing, only: dmiss
   use m_sferic
   use m_fetch_local_data
   use m_alloc

   implicit none

   double precision, intent(in) :: u_wind, v_wind
   integer, intent(out) :: nr_cells_done

   integer :: jaopen, jacros
   integer :: cell, cell_link, link, index_cell_node, node1, node2, min_distance_node
   double precision :: sl, sm, xcr, ycr
   double precision :: prin, dist, min_distance, max_cell_size, wdep, xn, yn, crp, xnode1, ynode1, xnode2, ynode2

   nr_cells_done = 0
   do cell = 1, ndxi
      if (kcs(cell) /= 2) then
         calculate_for(cell) = .false.
         nr_cells_done = nr_cells_done + 1
         cycle
      end if
      if (jampi == 1 .and. use_fetch_proc == 0) then
         if (idomain(cell) /= my_rank) then
            calculate_for(cell) = .false.
            nr_cells_done = nr_cells_done + 1
            cycle
         end if
      end if

      node2 = netcell(cell)%nod(netcell(cell)%n)
      max_cell_size = 0d0
      do index_cell_node = 1, netcell(cell)%n
         node1 = netcell(cell)%nod(index_cell_node)
         max_cell_size = max(max_cell_size, dbdistance(xk(node1), yk(node1), xk(node2), yk(node2), jsferic, jasfer3D, dmiss))
         node2 = node1
      end do
      if (jsferic == 1) max_cell_size = max_cell_size * rd2dg / ra

      jaopen = 0
      do cell_link = 1, nd(cell)%lnx
         link = abs(nd(cell)%ln(cell_link))
         if (ln(1, link) > ndxi) then
            jaopen = 1
            exit
         end if
      end do

      min_distance_node = 0; min_distance = 1d10; 
      do index_cell_node = 1, netcell(cell)%n
         link = netcell(cell)%lin(index_cell_node)
         node1 = netcell(cell)%nod(index_cell_node)
         if (index_cell_node == netcell(cell)%n) then
            node2 = netcell(cell)%nod(1)
         else
            node2 = netcell(cell)%nod(index_cell_node + 1)
         end if
         wdep = s1(cell) - min(zk(node1), zk(node2))
         if (lnn(link) == 1 .or. wdep < 0.5d0 .or. kn(3, link) == 0 .or. jaopen == 1) then ! link shallow or closed => start fetch here
            call normalout(xk(node1), yk(node1), xk(node2), yk(node2), xn, yn, jsferic, jasfer3D, dmiss, dxymis)
            prin = u_wind * xn + v_wind * yn
            if (prin < 0d0) then ! if upwind
               crp = xn; xn = -yn; yn = crp
               crp = 0d0
               xnode1 = xk(node1) - 2 * max_cell_size * xn
               ynode1 = yk(node1) - 2 * max_cell_size * yn
               xnode2 = xk(node2) + 2 * max_cell_size * xn
               ynode2 = yk(node2) + 2 * max_cell_size * yn
               call cross(xnode1, ynode1, xnode2, ynode2, xzw(cell), yzw(cell), xzw(cell) - 1d4 * u_wind, yzw(cell) - 1d4 * v_wind, &
                          jacros, sl, sm, xcr, ycr, crp, jsferic, dmiss)
               if (jacros == 1) then
                  dist = dbdistance(xz(cell), yz(cell), xcr, ycr, jsferic, jasfer3D, dmiss)
                  if (dist < min_distance) then
                     min_distance = dist; min_distance_node = index_cell_node ! closest crossed upwind edge
                  end if
               end if
            end if
         end if
      end do

      if (min_distance_node > 0) then
         calculate_for(cell) = .false.
         if (jaopen == 1) then
            fetch_temp(1, cell) = 1d5
         else
            fetch_temp(1, cell) = min(min_distance, max_cell_size)
         end if
         fetch_temp(2, cell) = max(s1(cell) - bl(cell), .1d0)
         if (jagui > 0) then
            !CALL rCIRc(xz(k),yz(k) ) !, fetch(n,k))
            !call adddot(xz(k),yz(k),1d0)
         end if
         nr_cells_done = nr_cells_done + 1
      end if

   end do
end subroutine search_starting_cells

!< calculates fetch length and depth for a given wind direction
subroutine calculate_fetch_values(nr_cells_done, total_nr_cells)

   use m_flowgeom, only: ndxi, ndx, bl, xz, yz
   use m_flow
   use m_flowtimes
   use timers
   use m_partitioninfo
   use m_gui
   use m_missing, only: dmiss
   use m_fetch_local_data
   use m_qnerror

   implicit none

   integer, intent(inout) :: nr_cells_done
   integer, intent(in) :: total_nr_cells

   integer :: cell, index_upwind_cell, upwind_cell, nr_cells_done_red, nr_cells_done_prev_cycle, error
   double precision :: prin, fetch_length, fetch_depthw, sumw, www
   integer :: numcycles

   numcycles = 0
   do while (nr_cells_done < total_nr_cells)

      nr_cells_done_prev_cycle = nr_cells_done
      nr_cells_done = 0
      numcycles = numcycles + 1

      cell_loop: do cell = 1, ndxi
         if (calculate_for(cell)) then
            if (fetch_temp(1, cell) /= dmiss) then ! just in case, to be removed later
               call qnerror('cell is marked as done', ' ', ' ')
               calculate_for(cell) = .false.
               cycle cell_loop
            end if
            fetch_length = 0
            fetch_depthw = 0
            sumw = 0
            do index_upwind_cell = number_of_upwind_cells(cell - 1) + 1, number_of_upwind_cells(cell)
               upwind_cell = list_of_upwind_cells(index_upwind_cell)
               if (fetch_temp(1, upwind_cell) == dmiss) then
                  cycle cell_loop
               end if
               www = data_at_upwind_cells(1, index_upwind_cell)
               prin = data_at_upwind_cells(2, index_upwind_cell)
               fetch_length = fetch_length + www * (fetch_temp(1, upwind_cell) + prin)
               fetch_depthw = fetch_depthw + www * (fetch_temp(1, upwind_cell) + prin) * &
                              max(.1d0, 0.8d0 * fetch_temp(2, upwind_cell) + 0.2d0 * (s1(cell) - bl(cell)))
               sumw = sumw + www
            end do
            if (sumw > 0d0) then
               calculate_for(cell) = .false.
               fetch_temp(1, cell) = fetch_length / sumw
               fetch_temp(2, cell) = fetch_depthw / fetch_length
               nr_cells_done = nr_cells_done + 1
               if (jagui > 0) then
                  !CALL rCIRc(xz(k),yz(k) )
                  !call adddot(xz(k),yz(k),2d0)
                  call KCIR(xz(cell), yz(cell), 1d0)
               end if
            end if
         else
            nr_cells_done = nr_cells_done + 1
         end if

      end do cell_loop

      if (jampi == 1 .and. use_fetch_proc == 0) then
         call update_ghosts(ITYPE_SaLL, 2, ndx, fetch_temp, error)
         call reduce_int_sum(nr_cells_done, nr_cells_done_red)
         nr_cells_done = nr_cells_done_red
      end if

      if (nr_cells_done == nr_cells_done_prev_cycle) then
         call qnerror('connectivity issue in fetch', ' ', ' ')
         return
      end if
   end do

end subroutine calculate_fetch_values

!> get phiwav values
subroutine get_phiwav_values()
use m_waves, only : phiwav
   use m_flowgeom
   use m_flow
   use m_sferic, only: pi

   implicit none

   integer :: link, k1, k2
   double precision, dimension(:), allocatable :: wxc, wyc

   call realloc(wxc, ndx, keepExisting=.false.)
   call realloc(wyc, ndx, keepExisting=.false.)
   wxc = 0d0; wyc = 0d0
   do link = 1, lnx
      k1 = ln(1, link); k2 = ln(2, link)
      wxc(k1) = wxc(k1) + wcL(1, link) * wx(link)
      wxc(k2) = wxc(k2) + wcL(2, link) * wx(link)
      wyc(k1) = wyc(k1) + wcL(1, link) * wy(link)
      wyc(k2) = wyc(k2) + wcL(2, link) * wy(link)
   end do
   phiwav = atan2(wyc, wxc) * 180d0 / pi

end subroutine get_phiwav_values

!> copy values to boundary nodes
subroutine copy_values_to_boundary_nodes()
   use m_waves, only : phiwav, rlabda
   use m_flowgeom
   use m_flow
   use m_waves, only: uorb, twav, hwav

   implicit none
   integer :: node, kb, ki

   do node = 1, nbndz
      kb = kbndz(1, node)
      ki = kbndz(2, node)
      hwav(kb) = hwav(ki)
      twav(kb) = twav(ki)
      Uorb(kb) = uorb(ki)
      rlabda(kb) = rlabda(ki)
      phiwav(kb) = phiwav(ki)
   end do

   do node = 1, nbndu
      kb = kbndu(1, node)
      ki = kbndu(2, node)
      hwav(kb) = hwav(ki)
      twav(kb) = twav(ki)
      Uorb(kb) = uorb(ki)
      rlabda(kb) = rlabda(ki)
      phiwav(kb) = phiwav(ki)
   end do
end subroutine copy_values_to_boundary_nodes
