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

!> Computes and sets the widths and gate lower edge levels on each of the flow links
!! crossed by a general structure (gate/weir/true genstru).
!! This is now an extended version of SOBEK's setLineStructure, because it also enables
!! a sideways closing gate with two doors from the left and right side, where the partially
!! closed portions have gate flow, and the center open portion still only has normal weir
!! flow across the sill.
module m_update_zcgen_widths_and_heights

   implicit none

   private

   public :: update_zcgen_widths_and_heights

contains

   subroutine update_zcgen_widths_and_heights()
      use precision, only: dp
      use m_strucs
      use fm_external_forcings_data
      use m_flowgeom
      use m_structures
      implicit none

      real(kind=dp) :: crestwidth, totalWidth, closedWidth, closedGateWidthL, closedGateWidthR, help
      integer :: ng, L, L0, Lf

      do ng = 1, ncgensg ! Loop over general structures

         ! Crest level is the same across all crossed flow links. Possibly time-dependent:
         generalstruc(ng)%levelcenter = zcgen((ng - 1) * 3 + 1)

         ! 1: First determine total width of all genstru links (TODO: AvD: we should not recompute this every user time step)
         totalWidth = 0d0
         if (generalstruc(ng)%numlinks == 0) then
            cycle ! Only upon invalid input (see warnings in log about missing structure params)
         end if

         do L = L1cgensg(ng), L2cgensg(ng)
            L0 = L - L1cgensg(ng) + 1
            Lf = kcgen(3, L)
            generalstruc(ng)%widthcenteronlink(L0) = wu(Lf)
            totalWidth = totalWidth + wu(Lf)
         end do

         ! 2a: the desired crest width for this overall structure (hereafter, the open links for this genstru should add up to this width)
         !     Also: only for gates, the desired door opening width for this overall structure
         !           (should be smaller than crestwidth, and for this portion the open gate door is emulated by dummy very high lower edge level)
         if (cgen_type(ng) == ICGENTP_WEIR) then
            crestwidth = zcgen((ng - 1) * 3 + 3)
            if (crestwidth > totalwidth) then
               zcgen((ng - 1) * 3 + 3) = totalwidth
               crestwidth = totalwidth
               write (msgbuf, '(a,a,a,es12.5,a)') 'Weir ''', trim(cgen_ids(ng)), ''', crest width (re)set to ', totalwidth, '.'
               call warn_flush()
            end if
            closedGateWidthL = 0d0
            closedGateWidthR = 0d0
         else
            if (cgen_type(ng) == ICGENTP_GENSTRU) then
               !crestwidth = totalWidth ! No crest/sill-width setting for true general structure yet (not old ext, nor new ext)
               crestwidth = min(totalWidth, generalstruc(ng)%widthcenter)
               !crestwidth = zcgen((ng-1)*3+3) ! NOTE: AvD: this now comes from scalar attribute 'widthcenter', no timeseries yet.
               ! genstru: always IOPENDIR_SYMMETRIC (TODO: UNST-1935)
               closedGateWidthL = max(0d0, .5d0 * (crestwidth - zcgen((ng - 1) * 3 + 3)))
               closedGateWidthR = max(0d0, .5d0 * (crestwidth - zcgen((ng - 1) * 3 + 3)))
               !closedGateWidthL = 0d0 ! max(0d0, .5d0*(totalWidth - zcgen((ng-1)*3+3))) ! Default symmetric opening
               !closedGateWidthR = 0d0 ! max(0d0, .5d0*(totalWidth - zcgen((ng-1)*3+3)))
            end if
            if (cgen_type(ng) == ICGENTP_GATE) then
               ! For a gate: zcgen(3,ng) is limited to the door opening width, but we want to open all links
               ! *underneath* the two doors as well, (if lower_edge_level is still high enough above sill_level)
               crestwidth = min(totalWidth, gates(cgen2str(ng))%sill_width)
               if (gates(cgen2str(ng))%opening_direction == IOPENDIR_FROMLEFT) then
                  closedGateWidthL = max(0d0, crestwidth - zcgen((ng - 1) * 3 + 3))
                  closedGateWidthR = 0d0
               else if (gates(cgen2str(ng))%opening_direction == IOPENDIR_FROMRIGHT) then
                  closedGateWidthL = 0d0
                  closedGateWidthR = max(0d0, crestwidth - zcgen((ng - 1) * 3 + 3))
               else ! IOPENDIR_SYMMETRIC
                  closedGateWidthL = max(0d0, .5d0 * (crestwidth - zcgen((ng - 1) * 3 + 3)))
                  closedGateWidthR = max(0d0, .5d0 * (crestwidth - zcgen((ng - 1) * 3 + 3)))
               end if
            end if
            generalstruc(ng)%gateheightonlink(1:generalstruc(ng)%numlinks) = 1d10 ! As a start, gate door is open everywhere. Below, we will close part of the gate doors.
         end if

         ! 2b: Determine the width that needs to be fully closed on 'left' side
         ! close the line structure from the outside to the inside: first step increasing increments
         ! NOTE: closed means: fully closed because sill_width (crest_width) is smaller that totalwidth.
         !       NOT because of gate door closing: that is handled by closedGateWidthL/R and may still
         !       have flow underneath doors if they are up high enough.
         closedWidth = max(0d0, totalWidth - crestwidth) / 2d0 ! Intentionally symmetric: if crest/sill_width < totalwidth. Only gate door motion may have a direction, was already handled above.

         generalstruc(ng)%gateclosedfractiononlink = 0d0

         do L = L1cgensg(ng), L2cgensg(ng)
            L0 = L - L1cgensg(ng) + 1
            Lf = kcgen(3, L)

            if (closedWidth > 0d0) then
               help = min(generalstruc(ng)%widthcenteronlink(L0), closedWidth)
               generalstruc(ng)%widthcenteronlink(L0) = generalstruc(ng)%widthcenteronlink(L0) - help ! 0d0 if closed
               closedWidth = closedWidth - help
            end if

            if (closedWidth <= 0d0) then
               ! finished
               exit
            end if
         end do

         ! 2c: Determine the width that needs to be fully closed on 'right' side
         ! close the line structure from the outside to the inside: first step increasing increments
         ! NOTE: closed means: fully closed because sill_width (crest_width) is smaller that totalwidth.
         !       NOT because of gate door closing: that is handled by closedGateWidthL/R and may still
         !       have flow underneath doors if they are up high enough.
         closedWidth = max(0d0, totalWidth - crestwidth) / 2d0 ! Intentionally symmetric: if crest/sill_width < totalwidth. Only gate door motion may have a direction, was already handled above.
         do L = L2cgensg(ng), L1cgensg(ng), -1
            L0 = L - L1cgensg(ng) + 1
            Lf = kcgen(3, L)

            if (closedWidth > 0d0) then
               help = min(generalstruc(ng)%widthcenteronlink(L0), closedWidth)
               generalstruc(ng)%widthcenteronlink(L0) = generalstruc(ng)%widthcenteronlink(L0) - help ! 0d0 if closed
               closedWidth = closedWidth - help
            end if

            if (closedWidth <= 0d0) then
               ! finished
               exit
            end if
         end do

         ! 2d Determine the gateclosedfractionOnlink on the left side, using the widthcenteronlink
         do L = L1cgensg(ng), L2cgensg(ng)
            L0 = L - L1cgensg(ng) + 1
            Lf = kcgen(3, L)

            if ((cgen_type(ng) == ICGENTP_GATE .or. cgen_type(ng) == ICGENTP_GENSTRU) .and. closedGateWidthL > 0d0) then
               !if (closedGateWidthL > .5d0*wu(Lf)) then
               generalstruc(ng)%gateheightonlink(L0) = zcgen((ng - 1) * 3 + 2)
               help = min(generalstruc(ng)%widthcenteronlink(L0), closedGateWidthL)
               closedGateWidthL = closedGateWidthL - help
               !end if

               if (generalstruc(ng)%widthcenteronlink(L0) > 0d0) then
                  generalstruc(ng)%gateclosedfractiononlink(L0) = generalstruc(ng)%gateclosedfractiononlink(L0) + help / generalstruc(ng)%widthcenteronlink(L0)
               end if
            else

            end if

            if (closedGateWidthL <= 0d0) then
               ! finished
               exit
            end if
         end do

         ! 2e Determine the gateclosedfractionOnlink on the left side, using the widthcenteronlink
         closedWidth = max(0d0, totalWidth - crestwidth) / 2d0 ! Intentionally symmetric: if crest/sill_width < totalwidth. Only gate door motion may have a direction, was already handled above.
         do L = L2cgensg(ng), L1cgensg(ng), -1
            L0 = L - L1cgensg(ng) + 1
            Lf = kcgen(3, L)

            if ((cgen_type(ng) == ICGENTP_GATE .or. cgen_type(ng) == ICGENTP_GENSTRU) .and. closedGateWidthR > 0d0) then
               !if (closedGateWidthL > .5d0*wu(Lf)) then
               generalstruc(ng)%gateheightonlink(L0) = zcgen((ng - 1) * 3 + 2)
               help = min(generalstruc(ng)%widthcenteronlink(L0), closedGateWidthR)
               closedGateWidthR = closedGateWidthR - help
               !end if

               if (generalstruc(ng)%widthcenteronlink(L0) > 0d0) then
                  generalstruc(ng)%gateclosedfractiononlink(L0) = generalstruc(ng)%gateclosedfractiononlink(L0) + help / generalstruc(ng)%widthcenteronlink(L0)
               end if
            end if

            if (closedGateWidthR <= 0d0) then
               ! finished
               exit
            end if
         end do

         !if ( L2cgensg(ng) == L1cgensg(ng) ) then
         !   generalstruc(ng)%widthcenteronlink(L0) = min( wu(Lf), zcgen((ng-1)*3+3) )
         !endif

      end do ! 1,ngensg

   end subroutine update_zcgen_widths_and_heights

end module m_update_zcgen_widths_and_heights
