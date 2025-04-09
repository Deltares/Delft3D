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

module m_structure_parameters
   use precision_basics, only: dp

   implicit none
   private
   public :: structure_parameters
contains
! =================================================================================================
! =================================================================================================
   subroutine structure_parameters
      use m_flowgeom, only: ln, wu, bob
      use m_flow
      use m_structures
      use fm_external_forcings_data, only: ngenstru, db_link_actual_width 
      use m_partitioninfo
      use m_flowtimes
      use m_missing, only: dmiss
      use unstruc_channel_flow, only: network
      use m_1d_structures
      use m_compound
      use m_GlobalParameters
      use m_longculverts, only: nlongculverts, longculverts, newculverts
      use m_dambreak_breach, only: db_upstream_level, db_downstream_level

      integer :: i, n, L, Lf, La, ierr, k, ku, kd, istru, nlinks
      real(kind=dp) :: dir
      integer :: jaghost, idmn_ghost, jaghostexist
      real(kind=dp), save :: timprev = -1.0_dp
      real(kind=dp) :: timstep
      type(t_structure), pointer :: pstru
      type(t_compound), pointer :: pcmp

      if (jampi > 0) then
         if (.not. allocated(reducebuf)) then
            nreducebuf = npumpsg * NUMVALS_PUMP + ngatesg * NUMVALS_GATE + ncdamsg * NUMVALS_CDAM + ncgensg * NUMVALS_CGEN &
                         + ngategen * NUMVALS_GATEGEN + nweirgen * NUMVALS_WEIRGEN + ngenstru * NUMVALS_GENSTRU + ngenstru * NUMVALS_GENSTRU &
                         + n_db_signals * NUMVALS_DAMBREAK + network%sts%numUniWeirs * NUMVALS_UNIWEIR + network%sts%numOrifices * NUMVALS_ORIFGEN &
                         + network%sts%numCulverts * NUMVALS_CULVERT + network%sts%numBridges * NUMVALS_BRIDGE + network%cmps%count * NUMVALS_CMPSTRU &
                         + nlongculverts * NUMVALS_LONGCULVERT
            allocate (reducebuf(nreducebuf), stat=ierr)
            call aerr('reducebuf  ( nreducebuf )', ierr, nreducebuf); reducebuf = 0.0_dp
         end if
      end if

      if (ti_his <= 0) return
      ! in order to compute the cumulative discharge, we have to compute the time step (see update_values_on_cross_sections)
      if (timprev == -1.0_dp) then
         timstep = 0.0_dp
      else
         timstep = time1 - timprev
      end if
      !
      ! === Pumps
      !
      if (allocated(valpump)) then
         do n = 1, npumpsg
            valpump(1:NUMVALS_COMMON_PUMP, n) = 0.0_dp
            valpump(NUMVALS_COMMON_PUMP + 1:NUMVALS_PUMP, n) = dmiss
            if (allocated(pumpsWithLevels)) then
               istru = pumpsWithLevels(n)
            else
               istru = -1
            end if

            do L = L1pumpsg(n), L2pumpsg(n)
               Lf = kpump(3, L)
               La = abs(Lf)
               if (jampi > 0) then
                  call link_ghostdata(my_rank, idomain(ln(1, La)), idomain(ln(2, La)), jaghost, idmn_ghost)
                  if (jaghost == 1) cycle
               end if
               dir = 1.0_dp
               if (Ln(1, La) /= kpump(1, L)) then
                  dir = -1.0_dp
               end if
               call fill_valstruct_perlink(valpump(:, n), La, dir, ST_PUMP, istru, L - L1pumpsg(n) + 1)
            end do
         end do
      end if
      !
      ! === Gates
      !
      if (allocated(valgate)) then
         do n = 1, ngatesg
            valgate(:, n) = 0.0_dp
            do L = L1gatesg(n), L2gatesg(n)
               Lf = kgate(3, L)
               La = abs(Lf)
               if (jampi > 0) then
                  call link_ghostdata(my_rank, idomain(ln(1, La)), idomain(ln(2, La)), jaghost, idmn_ghost)
                  if (jaghost == 1) cycle
               end if
               dir = 1.0_dp
               ku = ln(1, La)
               kd = ln(2, La)
               if (Ln(1, La) /= kgate(1, L)) then
                  dir = -1.0_dp
                  ku = ln(2, La)
                  kd = ln(1, La)
               end if
               valgate(1, n) = valgate(1, n) + wu(La)
               valgate(2, n) = valgate(2, n) + q1(La) * dir
               valgate(3, n) = valgate(3, n) + s1(ku) * wu(La)
               valgate(4, n) = valgate(4, n) + s1(kd) * wu(La)
            end do
            if (jampi == 0) then
               if (valgate(1, n) == 0.0_dp) then
                  valgate(2, n) = dmiss
                  valgate(3, n) = dmiss
                  valgate(4, n) = dmiss
               else
                  valgate(3, n) = valgate(3, n) / valgate(1, n)
                  valgate(4, n) = valgate(4, n) / valgate(1, n)
               end if
            end if
         end do
      end if
      !
      ! === Dams
      !
      if (allocated(valcdam)) then
         do n = 1, ncdamsg
            valcdam(:, n) = 0.0_dp
            do L = L1cdamsg(n), L2cdamsg(n)
               Lf = kcdam(3, L)
               La = abs(Lf)
               if (jampi > 0) then
                  call link_ghostdata(my_rank, idomain(ln(1, La)), idomain(ln(2, La)), jaghost, idmn_ghost)
                  if (jaghost == 1) cycle
               end if
               dir = 1.0_dp
               ku = ln(1, La)
               kd = ln(2, La)
               if (Ln(1, La) /= kcdam(1, L)) then
                  dir = -1.0_dp
                  ku = ln(2, La)
                  kd = ln(1, La)
               end if
               valcdam(1, n) = valcdam(1, n) + wu(La)
               valcdam(2, n) = valcdam(2, n) + q1(La) * dir
               valcdam(3, n) = valcdam(3, n) + s1(ku) * wu(La)
               valcdam(4, n) = valcdam(4, n) + s1(kd) * wu(La)
            end do
            if (jampi == 0) then
               if (valcdam(1, n) == 0.0_dp) then
                  valcdam(2, n) = dmiss
                  valcdam(3, n) = dmiss
                  valcdam(4, n) = dmiss
               else
                  valcdam(3, n) = valcdam(3, n) / valcdam(1, n)
                  valcdam(4, n) = valcdam(4, n) / valcdam(1, n)
               end if
            end if
         end do
      end if
      !
      ! === General structures (from old ext file)
      !
      if (allocated(valcgen)) then
         do n = 1, ncgensg
            i = n
            valcgen(:, n) = 0.0_dp
            do L = L1cgensg(i), L2cgensg(i)
               Lf = kcgen(3, L)
               La = abs(Lf)
               if (jampi > 0) then
                  call link_ghostdata(my_rank, idomain(ln(1, La)), idomain(ln(2, La)), jaghost, idmn_ghost)
                  if (jaghost == 1) cycle
               end if
               dir = 1.0_dp
               ku = ln(1, La)
               kd = ln(2, La)
               if (Ln(1, La) /= kcgen(1, L)) then
                  dir = -1.0_dp
                  ku = ln(2, La)
                  kd = ln(1, La)
               end if
               valcgen(1, n) = valcgen(1, n) + wu(La)
               valcgen(2, n) = valcgen(2, n) + q1(La) * dir
               valcgen(3, n) = valcgen(3, n) + s1(ku) * wu(La)
               valcgen(4, n) = valcgen(4, n) + s1(kd) * wu(La)
            end do
            if (jampi == 0) then
               if (valcgen(1, n) == 0.0_dp) then
                  valcgen(2, n) = dmiss
                  valcgen(3, n) = dmiss
                  valcgen(4, n) = dmiss
               else
                  valcgen(3, n) = valcgen(3, n) / valcgen(1, n)
                  valcgen(4, n) = valcgen(4, n) / valcgen(1, n)
               end if
            end if
         end do
      end if
      !
      ! === Gates (new)
      !
      if (allocated(valgategen)) then
         valgategen(:, :) = 0.0_dp
         if (network%sts%numGates > 0) then
            do n = 1, network%sts%numGates
               associate (pstru => network%sts%struct(network%sts%gateIndices(n)))
                  do i = 1, pstru%numlinks
                     La = abs(pstru%linknumbers(i))
                     if (jampi > 0) then
                        call link_ghostdata(my_rank, idomain(ln(1, La)), idomain(ln(2, La)), jaghost, idmn_ghost)
                        if (jaghost == 1) cycle
                     end if
                     dir = sign(1.0_dp, dble(pstru%linknumbers(i)))
                     if (dir > 0) then
                        ku = ln(1, La)
                        kd = ln(2, La)
                     else
                        ku = ln(2, La)
                        kd = ln(1, La)
                     end if
                     valgategen(IVAL_WIDTH, n) = valgategen(IVAL_WIDTH, n) + wu(La)
                     if (hu(La) > epshu) then
                        valgategen(IVAL_WIDTHWET, n) = valgategen(IVAL_WIDTHWET, n) + wu(La)
                        valgategen(IVAL_DIS, n) = valgategen(IVAL_DIS, n) + q1(La) * dir
                     end if

                     if (hs(ku) > epshs) then
                        valgategen(IVAL_WIDTHUP, n) = valgategen(IVAL_WIDTHUP, n) + wu(La)
                        valgategen(IVAL_S1UP, n) = valgategen(IVAL_S1UP, n) + s1(ku) * wu(La)
                     end if
                     if (hs(kd) > epshs) then
                        valgategen(IVAL_WIDTHDN, n) = valgategen(IVAL_WIDTHDN, n) + wu(La)
                        valgategen(IVAL_S1DN, n) = valgategen(IVAL_S1DN, n) + s1(kd) * wu(La)
                     end if
                     if (hs(ku) > epshs .and. hs(kd) > epshs) then
                        valgategen(IVAL_WIDTHUPDN, n) = valgategen(IVAL_WIDTHUPDN, n) + wu(La)
                        valgategen(IVAL_HEAD, n) = valgategen(IVAL_HEAD, n) + (s1(ku) - s1(kd)) * wu(La)
                     end if

                     if (q1(La) >= 0.0_dp) then
                        k = ln(1, La)
                     else
                        k = ln(2, La)
                     end if
                     if (hs(k) > epshs) then
                        valgategen(IVAL_GATE_WIDTHWET, n) = valgategen(IVAL_GATE_WIDTHWET, n) + wu(La)
                        valgategen(IVAL_GATE_FLOWH, n) = valgategen(IVAL_GATE_FLOWH, n) + s1(k) * wu(La)
                     end if
                  end do
                  valgategen(IVAL_GATE_COUNT, n) = 1 ! rank contains the gate.
                  valgategen(IVAL_GATE_OPENW, n) = pstru%generalst%gateOpeningWidth
                  valgategen(IVAL_GATE_EDGEL, n) = pstru%generalst%gateLowerEdgeLevel_actual
                  valgategen(IVAL_GATE_SILLH, n) = pstru%generalst%zs_actual
                  if (jampi == 0) then
                     if (valgategen(IVAL_WIDTHUP, n) > 0) then
                        valgategen(IVAL_S1UP, n) = valgategen(IVAL_S1UP, n) / valgategen(IVAL_WIDTHUP, n)
                     else
                        valgategen(IVAL_S1UP, n) = dmiss
                     end if
                     if (valgategen(IVAL_WIDTHDN, n) > 0) then
                        valgategen(IVAL_S1DN, n) = valgategen(IVAL_S1DN, n) / valgategen(IVAL_WIDTHDN, n)
                     else
                        valgategen(IVAL_S1DN, n) = dmiss
                     end if
                     if (valgategen(IVAL_WIDTHWET, n) == 0.0_dp) then
                        valgategen(IVAL_DIS, n) = dmiss
                     end if
                     if (valgategen(IVAL_GATE_WIDTHWET, n) == 0.0_dp) then
                        valgategen(IVAL_GATE_FLOWH, n) = dmiss
                     else
                        valgategen(IVAL_GATE_FLOWH, n) = max(min(pstru%generalst%gateLowerEdgeLevel_actual - pstru%generalst%zs_actual, &
                                                                 valgategen(IVAL_GATE_FLOWH, n) / valgategen(IVAL_GATE_WIDTHWET, n) - pstru%generalst%zs_actual), 0.0_dp) ! flow through height is always positive
                     end if
                  end if
               end associate
            end do
         else
            do n = 1, ngategen
               i = gate2cgen(n)
               valgategen(:, n) = 0.0_dp
               do L = L1cgensg(i), L2cgensg(i)
                  Lf = kcgen(3, L)
                  La = abs(Lf)
                  if (jampi > 0) then
                     call link_ghostdata(my_rank, idomain(ln(1, La)), idomain(ln(2, La)), jaghost, idmn_ghost)
                     if (jaghost == 1) cycle
                  end if
                  dir = 1.0_dp
                  ku = ln(1, La)
                  kd = ln(2, La)
                  if (Ln(1, La) /= kcgen(1, L)) then
                     dir = -1.0_dp
                     ku = ln(2, La)
                     kd = ln(1, La)
                  end if
                  valgategen(IVAL_WIDTH, n) = valgategen(IVAL_WIDTH, n) + wu(La)
                  if (hu(La) > epshu) then
                     valgategen(IVAL_WIDTHWET, n) = valgategen(IVAL_WIDTHWET, n) + wu(La)
                     valgategen(IVAL_DIS, n) = valgategen(IVAL_DIS, n) + q1(La) * dir
                  end if

                  if (hs(ku) > epshs) then
                     valgategen(IVAL_WIDTHUP, n) = valgategen(IVAL_WIDTHUP, n) + wu(L)
                     valgategen(IVAL_S1UP, n) = valgategen(IVAL_S1UP, n) + s1(ku) * wu(L)
                  end if
                  if (hs(kd) > epshs) then
                     valgategen(IVAL_WIDTHDN, n) = valgategen(IVAL_WIDTHDN, n) + wu(L)
                     valgategen(IVAL_S1DN, n) = valgategen(IVAL_S1DN, n) + s1(kd) * wu(L)
                  end if
                  if (hs(ku) > epshs .and. hs(kd) > epshs) then
                     valgategen(IVAL_WIDTHUPDN, n) = valgategen(IVAL_WIDTHUPDN, n) + wu(L)
                     valgategen(IVAL_HEAD, n) = valgategen(IVAL_HEAD, n) + (s1(ku) - s1(kd)) * wu(L)
                  end if

                  k = kcgen(1, L); if (q1(La) < 0.0_dp) k = kcgen(2, L)
                  if (hs(k) > epshs) then
                     valgategen(IVAL_GATE_WIDTHWET, n) = valgategen(IVAL_GATE_WIDTHWET, n) + wu(La)
                     valgategen(IVAL_GATE_FLOWH, n) = valgategen(IVAL_GATE_FLOWH, n) + s1(k) * wu(La)
                  end if
               end do
               if (L1cgensg(i) <= L2cgensg(i)) then ! At least one flow link in this domain is affected by this structure.
                  valgategen(IVAL_GATE_COUNT, n) = 1 ! rank contains the gate.
                  valgategen(IVAL_GATE_OPENW, n) = zcgen(3 * i) ! id_gategen_openw.
                  valgategen(IVAL_GATE_EDGEL, n) = zcgen(3 * i - 1) ! id_gategen_edgel.
                  valgategen(IVAL_GATE_SILLH, n) = zcgen(3 * i - 2) ! id_gategen_sillh.
               end if
               if (jampi == 0) then
                  if (valgategen(IVAL_WIDTHUP, n) > 0) then
                     valgategen(IVAL_S1UP, n) = valgategen(IVAL_S1UP, n) / valgategen(IVAL_WIDTHUP, n)
                  else
                     valgategen(IVAL_S1UP, n) = dmiss
                  end if
                  if (valgategen(IVAL_WIDTHDN, n) > 0) then
                     valgategen(IVAL_S1DN, n) = valgategen(IVAL_S1DN, n) / valgategen(IVAL_WIDTHDN, n)
                  else
                     valgategen(IVAL_S1DN, n) = dmiss
                  end if
                  if (valgategen(IVAL_WIDTHWET, n) == 0.0_dp) then
                     valgategen(IVAL_DIS, n) = dmiss
                  end if
                  if (valgategen(IVAL_GATE_WIDTHWET, n) == 0.0_dp) then
                     valgategen(IVAL_GATE_FLOWH, n) = dmiss
                  else
                     valgategen(IVAL_GATE_FLOWH, n) = max(min(zcgen(3 * i - 1) - zcgen(3 * i - 2), valgategen(IVAL_GATE_FLOWH, n) / valgategen(IVAL_GATE_WIDTHWET, n) - zcgen(3 * i - 2)), 0.0_dp) ! flow through height is always positive
                  end if
               end if
            end do
         end if
      end if
      !
      ! === Weirs
      !
      if (allocated(valweirgen)) then
         if (network%sts%numWeirs > 0) then ! new weir
            do n = 1, nweirgen
               valweirgen(1:NUMVALS_WEIRGEN, n) = 0.0_dp
               istru = network%sts%weirIndices(n)
               pstru => network%sts%struct(istru)
               nlinks = pstru%numlinks
               jaghost = 0
               do L = 1, nlinks
                  Lf = pstru%linknumbers(L)
                  La = abs(Lf)
                  if (jampi > 0) then
                     call link_ghostdata(my_rank, idomain(ln(1, La)), idomain(ln(2, La)), jaghost, idmn_ghost)
                     if (jaghost == 1) cycle
                  end if
                  dir = sign(1.0_dp, dble(Lf))
                  call fill_valstruct_perlink(valweirgen(:, n), La, dir, ST_WEIR, istru, L)
               end do
               if (nlinks > 0 .and. jaghost == 0) then ! This assumes that each weir has only 1 link
                  call fill_valstruct_per_structure(valweirgen(:, n), ST_WEIR, istru, nlinks)
               end if
            end do
         else
            ! old weir, do not compute the new extra fileds
            do n = 1, nweirgen
               i = weir2cgen(n)
               valweirgen(1:NUMVALS_WEIRGEN, n) = 0.0_dp
               do L = L1cgensg(i), L2cgensg(i)
                  Lf = kcgen(3, L)
                  La = abs(Lf)
                  if (jampi > 0) then
                     call link_ghostdata(my_rank, idomain(ln(1, La)), idomain(ln(2, La)), jaghost, idmn_ghost)
                     if (jaghost == 1) cycle
                  end if
                  dir = 1.0_dp
                  if (Ln(1, La) /= kcgen(1, L)) then
                     dir = -1.0_dp
                  end if
                  call fill_valstruct_perlink(valweirgen(:, n), La, dir, ST_UNSET, 0, 0)
               end do
            end do
         end if
      end if

      !
      ! === Orifice
      !
      if (allocated(valorifgen)) then
         do n = 1, network%sts%numOrifices
            valorifgen(1:NUMVALS_ORIFGEN, n) = 0.0_dp
            istru = network%sts%orificeIndices(n)
            pstru => network%sts%struct(istru)
            nlinks = pstru%numlinks
            jaghost = 0
            do L = 1, nlinks
               Lf = pstru%linknumbers(L)
               La = abs(Lf)
               if (jampi > 0) then
                  call link_ghostdata(my_rank, idomain(ln(1, La)), idomain(ln(2, La)), jaghost, idmn_ghost)
                  if (jaghost == 1) cycle
               end if
               dir = sign(1.0_dp, dble(Lf))
               call fill_valstruct_perlink(valorifgen(:, n), La, dir, ST_ORIFICE, istru, L)
            end do
            if (nlinks > 0 .and. jaghost == 0) then ! This assumes that each orifice has only 1 link
               call fill_valstruct_per_structure(valorifgen(:, n), ST_ORIFICE, istru, nlinks)
            end if
         end do
      end if

      !
      ! === Bridge
      !
      if (allocated(valbridge)) then
         do n = 1, network%sts%numBridges
            valbridge(1:NUMVALS_BRIDGE, n) = 0.0_dp
            istru = network%sts%bridgeIndices(n)
            pstru => network%sts%struct(istru)
            nlinks = pstru%numlinks
            do L = 1, nlinks ! Currently bridges have always only 1 link.
               Lf = pstru%linknumbers(L)
               La = abs(Lf)
               if (jampi > 0) then
                  call link_ghostdata(my_rank, idomain(ln(1, La)), idomain(ln(2, La)), jaghost, idmn_ghost)
                  if (jaghost == 1) cycle
               end if
               dir = sign(1.0_dp, dble(Lf))
               call fill_valstruct_perlink(valbridge(:, n), La, dir, ST_BRIDGE, istru, L)
            end do
         end do
      end if

      !
      ! === Culvert
      !
      if (allocated(valculvert)) then
         do n = 1, network%sts%numCulverts
            valculvert(1:NUMVALS_CULVERT, n) = 0.0_dp
            istru = network%sts%culvertIndices(n)
            pstru => network%sts%struct(istru)
            nlinks = pstru%numlinks
            jaghost = 0
            do L = 1, nlinks
               Lf = pstru%linknumbers(L)
               La = abs(Lf)
               if (jampi > 0) then
                  call link_ghostdata(my_rank, idomain(ln(1, La)), idomain(ln(2, La)), jaghost, idmn_ghost)
                  if (jaghost == 1) cycle
               end if
               dir = sign(1.0_dp, dble(Lf))
               call fill_valstruct_perlink(valculvert(:, n), La, dir, ST_CULVERT, istru, L)
            end do

            if (nLinks > 0 .and. jaghost == 0) then ! This assumes that each culvert has only 1 link
               valculvert(IVAL_CL_CRESTL, n) = get_crest_level(pstru)
               valculvert(IVAL_CL_STATE, n) = dble(get_culvert_state(pstru))
               valculvert(IVAL_CL_EDGEL, n) = get_gle(pstru)
               valculvert(IVAL_CL_OPENH, n) = get_opening_height(pstru)
            end if
         end do
      end if

      !
      ! === Universal weir
      !
      if (allocated(valuniweir)) then
         do n = 1, network%sts%numuniweirs
            valuniweir(1:NUMVALS_UNIWEIR, n) = 0.0_dp
            istru = network%sts%uniweirIndices(n)
            pstru => network%sts%struct(istru)
            nlinks = pstru%numlinks
            jaghost = 0
            do L = 1, nlinks
               Lf = pstru%linknumbers(L)
               La = abs(Lf)
               if (jampi > 0) then
                  call link_ghostdata(my_rank, idomain(ln(1, La)), idomain(ln(2, La)), jaghost, idmn_ghost)
                  if (jaghost == 1) cycle
               end if
               dir = sign(1.0_dp, dble(Lf))
               call fill_valstruct_perlink(valuniweir(:, n), La, dir, ST_UNI_WEIR, istru, L)
            end do
            if (nLinks > 0 .and. jaghost == 0) then ! This assumes that each universal weir has only 1 link
               valuniweir(IVAL_UW_CRESTL, n) = get_crest_level(pstru)
            end if
         end do
      end if

      !
      ! == dambreak
      !
      if (allocated(valdambreak)) then
         do n = 1, n_db_signals
            ! valdambreak(NUMVALS_DAMBREAK,n) is the cumulative over time, we do not reset it to 0
            valdambreak(1:NUMVALS_DAMBREAK - 1, n) = 0.0_dp
            istru = dambreaks(n)
            do L = db_first_link(n), db_last_link(n)
               if (db_active_links(L) /= 1) then
                  cycle
               end if

               Lf = db_link_ids(3, L)
               La = abs(Lf)
               if (jampi > 0) then
                  call link_ghostdata(my_rank, idomain(ln(1, La)), idomain(ln(2, La)), jaghost, idmn_ghost)
                  if (jaghost == 1) cycle
               end if
               dir = 1.0_dp
               if (Ln(1, La) /= db_link_ids(1, L)) then
                  dir = -1.0_dp
               end if
               valdambreak(IVAL_WIDTH, n) = valdambreak(IVAL_WIDTH, n) + db_link_actual_width(L)
               valdambreak(IVAL_DB_CRESTW, n) = valdambreak(IVAL_DB_CRESTW, n) + db_link_actual_width(L)
               if (hu(La) > epshu) then
                  valdambreak(IVAL_WIDTHWET, n) = valdambreak(IVAL_WIDTHWET, n) + db_link_actual_width(L)
                  valdambreak(IVAL_DIS, n) = valdambreak(IVAL_DIS, n) + q1(La) * dir
                  valdambreak(IVAL_AREA, n) = valdambreak(IVAL_AREA, n) + au(La) ! flow area
               end if
            end do
            if (db_last_link(n) < db_first_link(n)) then ! NOTE: valdambreak(IVAL_DB_DISCUM,n) in a parallel simulation already gets values after mpi communication
               ! from the previous timestep. In the case that the dambreak does not exist on the current domain, it should
               ! not contribute to the cumulative discharge in the coming mpi communication so we set it to 0.
               valdambreak(IVAL_DB_DISCUM, n) = 0.0_dp
            else
               if (network%sts%struct(istru)%dambreak%width > 0.0_dp) then
                  valdambreak(IVAL_DB_CRESTH, n) = network%sts%struct(istru)%dambreak%crest_level
               else
                  valdambreak(1:NUMVALS_DAMBREAK - 1, n) = dmiss ! No breach started yet, set FillValue
                  La = abs(db_link_ids(3, breach_start_link(n)))
                  valdambreak(IVAL_DB_CRESTH, n) = bob(1, La) ! No breach started yet, use bob as 'crest'.
                  valdambreak(IVAL_DB_CRESTW, n) = 0.0_dp ! No breach started yet, set crest width to 0
                  cycle
               end if
               ! TODO: UNST-5102: code below needs checking: when dambreak #n not active in current partition,
               ! most values below *are* available (based on other partitions). And in the code ahead, a call to reduce_crs
               ! assumes that all values are present and will be sum-reduced in a flowlinkwidth-weighted manner.
               valdambreak(IVAL_S1UP, n) = db_upstream_level(n)
               valdambreak(IVAL_S1DN, n) = db_downstream_level(n)
               valdambreak(IVAL_HEAD, n) = valdambreak(IVAL_S1UP, n) - valdambreak(IVAL_S1DN, n)
               valdambreak(IVAL_VEL, n) = network%sts%struct(istru)%dambreak%normal_velocity
               valdambreak(IVAL_DB_JUMP, n) = network%sts%struct(istru)%dambreak%water_level_jump
               valdambreak(IVAL_DB_TIMEDIV, n) = network%sts%struct(istru)%dambreak%breach_width_derivative
               valdambreak(IVAL_DB_DISCUM, n) = valdambreak(IVAL_DB_DISCUM, n) + valdambreak(IVAL_DIS, n) * timstep ! cumulative discharge
            end if
         end do
      end if
      !
      ! === General structures (from new ext file)
      !
      if (allocated(valgenstru)) then
         if (network%sts%numGeneralStructures > 0) then
            do n = 1, ngenstru
               valgenstru(1:NUMVALS_GENSTRU, n) = 0.0_dp
               istru = network%sts%generalStructureIndices(n)
               pstru => network%sts%struct(istru)
               nlinks = pstru%numlinks
               jaghost = 0
               do L = 1, nlinks
                  Lf = pstru%linknumbers(L)
                  La = abs(Lf)
                  if (jampi > 0) then
                     call link_ghostdata(my_rank, idomain(ln(1, La)), idomain(ln(2, La)), jaghost, idmn_ghost)
                     if (jaghost == 1) cycle
                  end if
                  dir = sign(1.0_dp, dble(Lf))
                  call fill_valstruct_perlink(valgenstru(:, n), La, dir, ST_GENERAL_ST, istru, L)
               end do
               if (nlinks > 0 .and. jaghost == 0) then ! This assumes that each general structure has only 1 link
                  call fill_valstruct_per_structure(valgenstru(:, n), ST_GENERAL_ST, istru, nlinks)
               end if
            end do
         else
            ! old general structure, do not compute the new extra fields
            do n = 1, ngenstru
               i = genstru2cgen(n)
               valgenstru(1:NUMVALS_GENSTRU, n) = 0.0_dp
               do L = L1cgensg(i), L2cgensg(i)
                  Lf = kcgen(3, L)
                  La = abs(Lf)
                  if (jampi > 0) then
                     call link_ghostdata(my_rank, idomain(ln(1, La)), idomain(ln(2, La)), jaghost, idmn_ghost)
                     if (jaghost == 1) cycle
                  end if
                  dir = 1.0_dp
                  if (Ln(1, La) /= kcgen(1, L)) then
                     dir = -1.0_dp
                  end if
                  call fill_valstruct_perlink(valgenstru(:, n), La, dir, ST_UNSET, 0, 0)
               end do
               ! Fill in values for each structure if it exists on the current subdomain
               if (L1cgensg(i) <= L2cgensg(i)) then ! At least one flow link in this domain is affected by this structure.
                  valgenstru(NUMVALS_GENSTRU, n) = 1 ! rank contains the general structure.
                  valgenstru(IVAL_OPENW, n) = zcgen(3 * i) ! id_genstru_openw.
                  valgenstru(IVAL_EDGEL, n) = zcgen(3 * i - 1) ! id_genstru_edgel.
                  valgenstru(IVAL_CRESTL, n) = zcgen(3 * i - 2) ! id_genstru_cresth.
               end if
            end do
         end if
      end if

      !
      ! === compound structure
      !
      if (allocated(valcmpstru)) then
         if (network%cmps%count > 0) then
            do n = 1, network%cmps%count
               valcmpstru(1:NUMVALS_CMPSTRU, n) = 0.0_dp
               pcmp => network%cmps%compound(n)
               nlinks = pcmp%numlinks
               do L = 1, nlinks
                  Lf = pcmp%linknumbers(L)
                  La = abs(Lf)
                  if (jampi > 0) then
                     call link_ghostdata(my_rank, idomain(ln(1, La)), idomain(ln(2, La)), jaghost, idmn_ghost)
                     if (jaghost == 1) cycle
                  end if
                  dir = sign(1.0_dp, dble(Lf))
                  call fill_valstruct_perlink(valcmpstru(:, n), La, dir, ST_COMPOUND, 0, L)
               end do
            end do
         end if
      end if
      !
      ! === Long culvert
      !
      if (allocated(vallongculvert)) then
         do n = 1, nlongculverts
            vallongculvert(1:NUMVALS_LONGCULVERT, n) = 0.0_dp
            if (longculverts(n)%numlinks > 0) then ! This long culvert is valid on the current domain/subdomain
               ! fill in for the representative flow ilnk
               if (newculverts) then
                  La = abs(longculverts(n)%flowlinks(2)) ! We use the 2st link as a representative flow link
                  dir = sign(1.0_dp, dble(longculverts(n)%flowlinks(2)))
               else
                  La = abs(longculverts(n)%flowlinks(1))
                  dir = sign(1.0_dp, dble(longculverts(n)%flowlinks(1)))
               end if

               if (La > 0) then
                  if (jampi > 0) then
                     call link_ghostdata(my_rank, idomain(ln(1, La)), idomain(ln(2, La)), jaghost, idmn_ghost)
                     if (jaghost == 1) cycle
                  end if

                  call fill_valstruct_perlink(vallongculvert(:, n), La, dir, ST_LONGCULVERT, n, 0)
               end if
            end if
         end do
      end if

      !
      ! === Reduction of structur parameters for parallel
      !
      if (jampi > 0 .and. ti_his > 0) then
         nreducebuf = 0
         reducebuf = 0.0_dp
         n = 0
         if (npumpsg > 0 .and. allocated(valpump)) then
            call fill_reduce_buffer(valpump, npumpsg * NUMVALS_PUMP)
            n = 1
         end if
         if (ngatesg > 0 .and. allocated(valgate)) then
            call fill_reduce_buffer(valgate, ngatesg * NUMVALS_GATE)
            n = 1
         end if
         if (ncdamsg > 0 .and. allocated(valcdam)) then
            call fill_reduce_buffer(valcdam, ncdamsg * NUMVALS_CDAM)
            n = 1
         end if
         if (ncgensg > 0 .and. allocated(valcgen)) then
            call fill_reduce_buffer(valcgen, ncgensg * NUMVALS_CGEN)
            n = 1
         end if
         if (ngategen > 0 .and. allocated(valgategen)) then
            call fill_reduce_buffer(valgategen, ngategen * NUMVALS_GATEGEN)
            n = 1
         end if
         if (nweirgen > 0 .and. allocated(valweirgen)) then
            call fill_reduce_buffer(valweirgen, nweirgen * NUMVALS_WEIRGEN)
            n = 1
         end if
         if (ngenstru > 0 .and. allocated(valgenstru)) then
            call fill_reduce_buffer(valgenstru, ngenstru * NUMVALS_GENSTRU)
            n = 1
         end if
         if (n_db_signals > 0 .and. allocated(valdambreak)) then
            call fill_reduce_buffer(valdambreak, n_db_signals * NUMVALS_DAMBREAK)
            n = 1
         end if
         if (allocated(valuniweir) .and. network%sts%numUniWeirs > 0) then
            call fill_reduce_buffer(valuniweir, network%sts%numUniWeirs * NUMVALS_UNIWEIR)
            n = 1
         end if
         if (allocated(valorifgen) .and. network%sts%numOrifices > 0) then
            call fill_reduce_buffer(valorifgen, network%sts%numOrifices * NUMVALS_ORIFGEN)
            n = 1
         end if
         if (allocated(valculvert) .and. network%sts%numCulverts > 0) then
            call fill_reduce_buffer(valculvert, network%sts%numCulverts * NUMVALS_CULVERT)
            n = 1
         end if
         if (allocated(valbridge) .and. network%sts%numBridges > 0) then
            call fill_reduce_buffer(valbridge, network%sts%numBridges * NUMVALS_BRIDGE)
            n = 1
         end if
         if (allocated(valcmpstru) .and. network%cmps%count > 0) then
            call fill_reduce_buffer(valcmpstru, network%cmps%count * NUMVALS_CMPSTRU)
            n = 1
         end if
         if (allocated(vallongculvert) .and. nlongculverts > 0) then
            call fill_reduce_buffer(vallongculvert, nlongculverts * NUMVALS_LONGCULVERT)
            n = 1
         end if
         if (n == 1) then
            call reduce_crs(reducebuf, nreducebuf, 1)
         end if
      end if

      ! === Long culvert
      if (nlongculverts > 0 .and. allocated(vallongculvert)) then
         if (jampi > 0) then
            call substitute_reduce_buffer(vallongculvert, nlongculverts * NUMVALS_LONGCULVERT)
         end if
         do n = 1, nlongculverts
            call average_valstruct(vallongculvert(:, n), ST_LONGCULVERT, n)
         end do
      end if
      ! === Compound structure
      if (network%cmps%count > 0 .and. allocated(valcmpstru)) then
         if (jampi > 0) then
            call substitute_reduce_buffer(valcmpstru, network%cmps%count * NUMVALS_CMPSTRU)
         end if
         do n = 1, network%cmps%count
            pcmp => network%cmps%compound(n)
            nlinks = pcmp%numlinks
            call average_valstruct(valcmpstru(:, n), ST_COMPOUND, 0)
         end do
      end if

      ! === Bridge
      if (network%sts%numBridges > 0 .and. allocated(valbridge)) then
         if (jampi > 0) then
            call substitute_reduce_buffer(valbridge, network%sts%numBridges * NUMVALS_BRIDGE)
         end if
         do n = 1, network%sts%numBridges
            istru = network%sts%bridgeIndices(n)
            pstru => network%sts%struct(istru)
            nlinks = pstru%numlinks
            call average_valstruct(valbridge(:, n), ST_BRIDGE, istru)
         end do
      end if
      ! === Culvert
      if (network%sts%numCulverts > 0 .and. allocated(valculvert)) then
         if (jampi > 0) then
            call substitute_reduce_buffer(valculvert, network%sts%numCulverts * NUMVALS_CULVERT)
         end if
         do n = 1, network%sts%numCulverts
            istru = network%sts%culvertIndices(n)
            pstru => network%sts%struct(istru)
            nlinks = pstru%numlinks
            call average_valstruct(valculvert(:, n), ST_CULVERT, istru)
         end do
      end if
      ! === Orifice
      if (network%sts%numOrifices > 0 .and. allocated(valorifgen)) then
         if (jampi > 0) then
            call substitute_reduce_buffer(valorifgen, network%sts%numOrifices * NUMVALS_ORIFGEN)
         end if
         do n = 1, network%sts%numOrifices
            istru = network%sts%orificeIndices(n)
            pstru => network%sts%struct(istru)
            nlinks = pstru%numlinks
            call average_valstruct(valorifgen(:, n), ST_ORIFICE, istru)
         end do
      end if
      ! === Universal weir
      if (network%sts%numUniWeirs > 0 .and. allocated(valuniweir)) then
         if (jampi > 0) then
            call substitute_reduce_buffer(valuniweir, network%sts%numUniWeirs * NUMVALS_UNIWEIR)
         end if
         do n = 1, network%sts%numUniWeirs
            istru = network%sts%uniweirIndices(n)
            pstru => network%sts%struct(istru)
            nlinks = pstru%numlinks
            call average_valstruct(valuniweir(:, n), ST_UNI_WEIR, istru)
            if (valuniweir(IVAL_WIDTH, n) == 0) then
               valuniweir(IVAL_UW_CRESTL:NUMVALS_UNIWEIR, n) = dmiss
            end if
         end do
      end if

      ! === Dambreak
      if (jampi > 0 .and. ti_his > 0) then
         if (n_db_signals > 0 .and. allocated(valdambreak)) then
            call substitute_reduce_buffer(valdambreak, n_db_signals * NUMVALS_DAMBREAK)
         end if
      end if

      ! === General structure
      if (ngenstru > 0 .and. allocated(valgenstru)) then
         if (jampi > 0) then
            call substitute_reduce_buffer(valgenstru, ngenstru * NUMVALS_GENSTRU)
         end if

         if (network%sts%numGeneralStructures > 0) then ! new general structure
            do n = 1, ngenstru
               istru = network%sts%generalStructureIndices(n)
               pstru => network%sts%struct(istru)
               nlinks = pstru%numlinks
               call average_valstruct(valgenstru(:, n), ST_GENERAL_ST, istru)
            end do
         else ! Old general structure
            do n = 1, ngenstru
               call average_valstruct(valgenstru(:, n), ST_UNSET, 0)
               if (jampi > 0) then
                  if (valgenstru(NUMVALS_GENSTRU, n) > 1) then ! The structure lies on more than one partition
                     valgenstru(IVAL_OPENW, n) = valgenstru(IVAL_OPENW, n) / valgenstru(NUMVALS_GENSTRU, n) ! id_genstru_openw.
                     valgenstru(IVAL_EDGEL, n) = valgenstru(IVAL_EDGEL, n) / valgenstru(NUMVALS_GENSTRU, n) ! id_genstru_edgel.
                     valgenstru(IVAL_CRESTL, n) = valgenstru(IVAL_CRESTL, n) / valgenstru(NUMVALS_GENSTRU, n) ! id_genstru_cresth.
                  end if
               end if
            end do
         end if
      end if

      if (nweirgen > 0 .and. allocated(valweirgen)) then
         if (jampi > 0) then
            call substitute_reduce_buffer(valweirgen, nweirgen * NUMVALS_WEIRGEN)
         end if

         if (network%sts%numWeirs > 0) then ! new weir
            do n = 1, nweirgen
               istru = network%sts%weirIndices(n)
               pstru => network%sts%struct(istru)
               nlinks = pstru%numlinks
               call average_valstruct(valweirgen(:, n), ST_WEIR, istru)
            end do
         else ! old weir
            do n = 1, nweirgen
               call average_valstruct(valweirgen(:, n), ST_UNSET, 0)
               i = weir2cgen(n)
               if (L1cgensg(i) <= L2cgensg(i)) then ! At least one flow link in this domain is affected by this structure.
                  valweirgen(NUMVALS_WEIRGEN, n) = 1 ! rank contains the weir.
                  valweirgen(IVAL_CRESTW, n) = zcgen(3 * i) ! id_weirgen_crestw.
                  valweirgen(IVAL_CRESTL, n) = zcgen(3 * i - 2) ! id_weirgen_cresth.
               end if
            end do
         end if
      end if

      if (jampi > 0 .and. ti_his > 0) then
         if (ngategen > 0 .and. allocated(valgategen)) then
            call substitute_reduce_buffer(valgategen, ngategen * NUMVALS_GATEGEN)
            do n = 1, ngategen
               if (valgategen(IVAL_WIDTHUP, n) > 0) then
                  valgategen(IVAL_S1UP, n) = valgategen(IVAL_S1UP, n) / valgategen(IVAL_WIDTHUP, n)
               else
                  valgategen(IVAL_S1UP, n) = dmiss
               end if
               if (valgategen(IVAL_WIDTHDN, n) > 0) then
                  valgategen(IVAL_S1DN, n) = valgategen(IVAL_S1DN, n) / valgategen(IVAL_WIDTHDN, n)
               else
                  valgategen(IVAL_S1DN, n) = dmiss
               end if
               if (valgategen(IVAL_WIDTH, n) == 0.0_dp) then
                  valgategen(IVAL_GATE_OPENW, n) = dmiss
                  valgategen(IVAL_GATE_EDGEL, n) = dmiss
                  valgategen(IVAL_GATE_SILLH, n) = dmiss
               else
                  valgategen(IVAL_GATE_OPENW, n) = valgategen(IVAL_GATE_OPENW, n) / valgategen(IVAL_GATE_COUNT, n) !id_gategen_openw
                  valgategen(IVAL_GATE_EDGEL, n) = valgategen(IVAL_GATE_EDGEL, n) / valgategen(IVAL_GATE_COUNT, n) !id_gategen_edgel
                  valgategen(IVAL_GATE_SILLH, n) = valgategen(IVAL_GATE_SILLH, n) / valgategen(IVAL_GATE_COUNT, n) !id_gategen_sillh
               end if
               if (valgategen(IVAL_WIDTHWET, n) == 0.0_dp) then
                  valgategen(IVAL_DIS, n) = dmiss
               end if
               if (valgategen(IVAL_GATE_WIDTHWET, n) == 0.0_dp) then
                  valgategen(IVAL_GATE_FLOWH, n) = dmiss
               else
                  if (network%sts%numGates > 0) then
                     i = network%sts%gateIndices(n)
                  else
                     i = gate2cgen(n)
                  end if
                  valgategen(IVAL_GATE_FLOWH, n) = max(min(zcgen(3 * i - 1) - zcgen(3 * i - 2), valgategen(IVAL_GATE_FLOWH, n) / valgategen(IVAL_GATE_WIDTHWET, n) - zcgen(3 * i - 2)), 0.0_dp) ! flow through height is always positive
               end if
            end do
         end if

         if (ncgensg > 0 .and. allocated(valcgen)) then
            call substitute_reduce_buffer(valcgen, ncgensg * NUMVALS_CGEN)
            do n = 1, ncgensg
               if (valcgen(1, n) == 0.0_dp) then
                  valcgen(2, n) = dmiss
                  valcgen(3, n) = dmiss
                  valcgen(4, n) = dmiss
               else
                  valcgen(3, n) = valcgen(3, n) / valcgen(1, n)
                  valcgen(4, n) = valcgen(4, n) / valcgen(1, n)
               end if
            end do
         end if

         if (ncdamsg > 0 .and. allocated(valcdam)) then
            call substitute_reduce_buffer(valcdam, ncdamsg * NUMVALS_CDAM)
            do n = 1, ncdamsg
               if (valcdam(1, n) == 0.0_dp) then
                  valcdam(2, n) = dmiss
                  valcdam(3, n) = dmiss
                  valcdam(4, n) = dmiss
               else
                  valcdam(3, n) = valcdam(3, n) / valcdam(1, n)
                  valcdam(4, n) = valcdam(4, n) / valcdam(1, n)
               end if
            end do
         end if

         if (ngatesg > 0 .and. allocated(valgate)) then
            call substitute_reduce_buffer(valgate, ngatesg * NUMVALS_GATE)
            do n = 1, ngatesg
               if (valgate(1, n) == 0.0_dp) then
                  valgate(2, n) = dmiss
                  valgate(3, n) = dmiss
                  valgate(4, n) = dmiss
               else
                  valgate(3, n) = valgate(3, n) / valgate(1, n)
                  valgate(4, n) = valgate(4, n) / valgate(1, n)
               end if
            end do
         end if
      end if

      if (npumpsg > 0 .and. allocated(valpump)) then
         if (jampi > 0) then
            call substitute_reduce_buffer(valpump, npumpsg * NUMVALS_PUMP)
         end if
         do n = 1, npumpsg
            call average_valstruct(valpump(:, n), ST_UNSET, 0)

            do L = L1pumpsg(n), L2pumpsg(n)
               Lf = kpump(3, L)
               La = abs(Lf)
               jaghostexist = 0
               if (jampi > 0) then
                  call link_ghostdata(my_rank, idomain(ln(1, La)), idomain(ln(2, La)), jaghost, idmn_ghost)
                  if (jaghost == 1) then
                     jaghostexist = 1
                     cycle
                  end if
               end if
            end do
            if (jampi > 0) then
               valpump(NUMVALS_COMMON_PUMP + 1:NUMVALS_PUMP, n) = 0.0_dp
            end if
            if (L1pumpsg(n) <= L2pumpsg(n) .and. jaghostexist == 0) then
               if (allocated(pumpsWithLevels)) then
                  istru = pumpsWithLevels(n)
               else
                  istru = -1
               end if
               if (istru > 0) then ! TODO: UNST-2587: once all pump code is done, remove this temp IF.
                  pstru => network%sts%struct(istru)
                  valpump(IVAL_PP_CAP, n) = GetPumpCapacity(pstru)
                  valpump(IVAL_PP_DISDIR, n) = sign(1, pstru%pump%direction) * valpump(IVAL_DIS, n) ! Discharge w.r.t. pump direction (same sign as capacity)
                  valpump(IVAL_PP_STAG, n) = GetPumpStage(pstru)
                  if (valpump(IVAL_PP_STAG, n) < 0.0_dp) then
                     valpump(IVAL_PP_STAG, n) = dmiss ! Set to fill value if stage is irrelevant.
                  end if
                  if (pstru%pump%direction * pstru%pump%capacity(1) > 0) then
                     valpump(IVAL_PP_S1SUC, n) = valpump(IVAL_S1UP, n) ! water level at delivery side
                     valpump(IVAL_PP_S1DEL, n) = valpump(IVAL_S1DN, n) ! water level at suction side
                  else
                     valpump(IVAL_PP_S1SUC, n) = valpump(IVAL_S1DN, n)
                     valpump(IVAL_PP_S1DEL, n) = valpump(IVAL_S1UP, n)
                  end if
                  if (valpump(IVAL_WIDTHUPDN, n) > 0) then
                     valpump(IVAL_PP_HEAD, n) = valpump(IVAL_PP_S1DEL, n) - valpump(IVAL_PP_S1SUC, n) ! Pump head
                  else
                     valpump(IVAL_PP_HEAD, n) = dmiss
                  end if
                  valpump(IVAL_PP_RED, n) = GetPumpReductionFactor(pstru)
               end if
            end if
         end do
         if (jampi > 0) then
            call reduce_crs(valpump(NUMVALS_COMMON_PUMP + 1:NUMVALS_PUMP, 1:npumpsg), npumpsg, NUMVALS_PUMP - NUMVALS_COMMON_PUMP)
         end if
      end if

      !update timeprev
      timprev = time1

   end subroutine structure_parameters

   ! =================================================================================================
! =================================================================================================
   subroutine substitute_reduce_buffer(vals, nvals)
      use m_partitioninfo
      implicit none
      integer :: i
      integer, intent(in) :: nvals
      real(kind=dp), dimension(1:nvals) :: vals

      nreducebuf = nreducebuf - nvals
      do i = 1, nvals
         vals(i) = reducebuf(nreducebuf + i)
      end do

   end subroutine substitute_reduce_buffer

! =================================================================================================
! =================================================================================================
   subroutine fill_reduce_buffer(vals, nvals)
      use m_partitioninfo
      implicit none
      integer :: i
      integer, intent(in) :: nvals
      real(kind=dp), dimension(1:nvals) :: vals

      if (jampi == 0) then
         return
      end if

      do i = 1, nvals
         reducebuf(nreducebuf + i) = vals(i)
      end do
      nreducebuf = nreducebuf + nvals

   end subroutine fill_reduce_buffer

end module m_structure_parameters
