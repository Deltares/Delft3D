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

 subroutine furu_structures()
    use m_flow
    use m_flowgeom
    use m_flowtimes
    use m_flowparameters
    use m_general_structure
    use m_1d_structures
    use m_compound
    use m_Universal_Weir
    use m_cross_helper
    use m_culvert
    use m_bridge
    use m_oned_functions
    use unstruc_channel_flow
    use m_get_cz

    implicit none

    integer :: direction
    integer :: istru
    integer :: i
    integer :: k1
    integer :: k2
    integer :: kfu
    integer :: L
    integer :: L0
    integer :: mdown
    integer :: nstrucsg
    integer :: ncompound

    double precision :: as1
    double precision :: as2
    double precision :: cmustr
    double precision :: Cz
    double precision :: dpt
    double precision :: maxwidth1
    double precision :: maxwidth2
    double precision :: perimeter
    double precision :: wetdown
    double precision :: width

    logical :: firstiter
    logical :: SkipDimensionChecks

    type(t_structure), pointer :: pstru
    type(t_compound), pointer :: pcompound

    nstrucsg = network%sts%count
    do istru = 1, nstrucsg
       pstru => network%sts%struct(istru)
       if (pstru%type == ST_PUMP) then
          call computePump_all_links(pstru)
       else
          if (network%sts%struct(istru)%type == ST_GENERAL_ST) then
             SkipDimensionChecks = .not. changeStructureDimensions
             if (pstru%numlinks == 1) then
                L = abs(pstru%linknumbers(1))
                if (L <= lnx1D) then
                   if (network%adm%line2cross(L, 2)%c1 < 0) then
                      SkipDimensionChecks = .true.
                   end if
                end if
             end if
             call update_widths(pstru%generalst, pstru%numlinks, pstru%linknumbers, wu, SkipDimensionChecks)
          end if

          do L0 = 1, pstru%numlinks
             L = abs(pstru%linknumbers(L0))
             direction = sign(1, pstru%linknumbers(L0))
             if (hu(l) > 0) then
                k1 = ln(1, L)
                k2 = ln(2, L)

                select case (network%sts%struct(istru)%type)
                case (ST_GENERAL_ST)
                   firstiter = .true.
                   ! The upstream flow area is necessary for computing the upstream velocity height
                   ! For 1d the flow area is computed, using the upstream water depth
                   ! For 2D the flow area is computed, using the flow width WU and the waterdepth at the upstream grid cell
                   if (kcu(L) == 1) then
                      dpt = max(epshu, s1(k1) - bob0(1, L))
                      call GetCSParsFlow(network%adm%line2cross(L, 2), network%crs%cross, dpt, as1, perimeter, width, maxFlowWidth=maxwidth1)
                      dpt = max(epshu, s1(k2) - bob0(2, L))
                      call GetCSParsFlow(network%adm%line2cross(L, 2), network%crs%cross, dpt, as2, perimeter, width, maxFlowWidth=maxwidth2)
                      width = max(maxwidth1, maxwidth2)
                      wu(L) = width
                   else
                      as1 = (s1(k1) - bl(k1)) * wu(L)
                      as2 = (s1(k2) - bl(k2)) * wu(L)
                      width = wu(L)
                   end if
                   call getcz(hu(L), frcu(L), ifrcutp(L), Cz, L)
                   au(L) = pstru%au(L0)
                   call computeGeneralStructure(pstru%generalst, direction, L0, width, bob0(:, L), fu(L), ru(L), &
                                                au(L), as1, as2, width, kfu, s1(k1), s1(k2), q1(L), Cz, dx(L), dts, SkipDimensionChecks)
                case (ST_DAMBREAK)
                   continue
                case (ST_CULVERT)
                   if (s1(k1) > s1(k2)) then
                      mdown = k2
                      dpt = s1(k2) - bob0(2, L)
                   else
                      mdown = k1
                      dpt = s1(k1) - bob0(1, L)
                   end if

                   call GetCSParsFlow(network%adm%line2cross(L, 2), network%crs%cross, dpt, wetdown, perimeter, width)

                   wetdown = max(wetdown, 0.0001d0)
                   call computeculvert(pstru%culvert, fu(L), ru(L), au(L), width, kfu, cmustr, s1(k1), s1(k2), &
                                       q1(L), q1(L), pstru%u1(L0), pstru%u0(L0), dx(L), dts, wetdown)

                case (ST_UNI_WEIR)
                   fu(L) = pstru%fu(L0)
                   ru(L) = pstru%ru(L0)
                   au(L) = pstru%au(L0)
                   call computeUniversalWeir(pstru%uniweir, fu(L), ru(L), au(L), width, bob0(:, L), kfu, s1(k1), s1(k2), &
                                             q1(L), pstru%u1(L0), dx(L), dts, changeStructureDimensions)
                case (ST_BRIDGE)
                   dpt = max(epshu, s1(k1) - bob0(1, L))
                   call GetCSParsFlow(network%adm%line2cross(L, 2), network%crs%cross, dpt, as1, perimeter, width)
                   wu(L) = as1 / dpt
                   dpt = max(epshu, s1(k2) - bob0(2, L))
                   call GetCSParsFlow(network%adm%line2cross(L, 2), network%crs%cross, dpt, as2, perimeter, width)
                   ! WU(L) is the average width at the bridge (max of up/downstream side).
                   wu(L) = max(wu(L), as2 / dpt)
                   width = wu(L)
                   call ComputeBridge(pstru%bridge, fu(L), ru(L), au(L), width, kfu, s1(k1), s1(k2), pstru%u1(L0), dx(L), dts, &
                                      as1, as2, bob0(:, L), changeStructureDimensions)
                case (ST_LONGCULVERT)
                   ! NOTE: UNST-4328: long culverts are no actual structures, but rather just normal 1D flow links, no furu-step needed here.
                   continue
                case default
                   write (msgbuf, '(''Unsupported structure type'', i5)') network%sts%struct(istru)%type
                   call err_flush()
                end select

                ! store computed fu, ru and au in structure object. In case this structure
                ! is a part of a compound structure this data will be used in computeCompound
             else
                fu(L) = 0d0
                ru(L) = 0d0
                au(L) = 0d0
                if (pstru%type == ST_GENERAL_ST) then
                   pstru%generalst%fu(:, L0) = 0d0
                   pstru%generalst%ru(:, L0) = 0d0
                   pstru%generalst%au(:, L0) = 0d0
                   pstru%generalst%state(:, L0) = 0
                else if (pstru%type == ST_CULVERT) then
                   pstru%culvert%state = 0
                end if
             end if
             call set_fu_ru_structure(pstru, L0, fu(L), ru(L), au(L))
             call check_for_changes_on_structures(LEVEL_WARN, pstru, bob0(:, L))
          end do
       end if

    end do

    ! Compute FU, RA and AU for compound structures
    ncompound = network%cmps%Count
    do i = 1, ncompound
       pcompound => network%cmps%compound(i)
       do L0 = 1, pcompound%numlinks
          L = abs(pcompound%linknumbers(L0))
          if (hu(l) > 0) then
             call computeCompound(pcompound, network%sts%struct, L0, u0(L), teta(L), fu(L), ru(L), au(L))
          end if
       end do
    end do

 end subroutine furu_structures
