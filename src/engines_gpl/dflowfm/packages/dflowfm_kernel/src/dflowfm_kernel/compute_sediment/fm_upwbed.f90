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

module m_fm_upwbed

   implicit none

   private

   public :: fm_upwbed

contains

   ! Interpolate flownode-based vector (sx,sy) to edge-based vector (e_sn, e_st)
   subroutine fm_upwbed(lsedtot, sx, sy, sxtot, sytot, e_sn, e_st)
      use precision, only: dp
      use m_flowgeom, only: acl, snu, csu, kcu
      use m_flow, only: u1, epshu, hu
      use unstruc_messages
      use m_sediment, only: stmpar, jabndtreatment
      use sediment_basics_module
      use m_fm_erosed, only: link1, link1sign, tratyp, kfsed, link1sign2
      use m_fm_erosed, only: ln => ln_mor
      use m_fm_erosed, only: ndx => ndx_mor
      use m_fm_erosed, only: lnx => lnx_mor
      use m_fm_erosed, only: lnxi => lnxi_mor
      implicit none

      integer, intent(in) :: lsedtot !< number of sediment fractions
      real(kind=dp), dimension(Ndx, lsedtot), intent(in) :: sx, sy !< cell (flownode)-based quantity
      real(kind=dp), dimension(Ndx, lsedtot), intent(in) :: sxtot, sytot !< cell (flownode)-based fluxes
      real(kind=dp), dimension(Lnx, lsedtot), intent(out) :: e_sn, e_st !< edge (flowlink)-based quantity, normal and tangential components

      real(kind=dp) :: sutot1, sutot2

      integer :: k1, k2, Lf, l, lnxlnxi
      logical :: pure1d_mor
      logical :: upwindbedload

      upwindbedload = stmpar%morpar%mornum%upwindbedload
      pure1d_mor = stmpar%morpar%mornum%pure1d
      !if ( laterallyaveragedbedload ) then
      !   call mess(LEVEL_ERROR, 'upwbed: laterally averaged bedload not supported')
      !end if

      if (jabndtreatment == 0) then
         lnxlnxi = lnx
      else if (jabndtreatment == 1) then
         lnxlnxi = lnxi
      end if

      ! internal flowlinks (and boundary flowlinks if jabndtreatment==0 -- default)
      do Lf = 1, Lnxlnxi
         !     check if flowlink is active and if it connects two active sediment flownodes
         if (hu(Lf) > epshu) then
            !        find left and right neighboring flownodes
            k1 = ln(1, Lf)
            k2 = ln(2, Lf)

            do l = 1, lsedtot
               if (.not. has_bedload(tratyp(l))) cycle ! cycle if this fraction doesn't include bedload
               !
               ! check for active sediment cell
               if (kfsed(k1) * kfsed(k2) == 0) then
                  e_sn(Lf, l) = 0d0
                  e_st(Lf, l) = 0d0
                  cycle
               end if
               !
               if (pure1d_mor .and. abs(kcu(Lf)) == 1) then
                  ! on 1D links use the x-component which is the full vector
                  if (link1(k1) == Lf) then
                     sutot1 = sxtot(k1, l)
                  else
                     sutot1 = link1sign(k1) * sxtot(k1, l)
                  end if
                  if (link1(k2) == Lf) then
                     sutot2 = sxtot(k2, l)
                  else
                     sutot2 = link1sign(k2) * sxtot(k2, l)
                  end if

                  if (upwindbedload .or. Lf > Lnxi) then
                     ! upwind approximation (also at boundary cells for central scheme if jabndtreatment==0)
                     if (sutot1 > 0d0 .and. sutot2 > 0d0) then
                        e_sn(Lf, l) = link1sign2(Lf) * sx(k1, l)
                     else if (sutot1 < 0d0 .and. sutot2 < 0d0) then
                        e_sn(Lf, l) = link1sign2(Lf) * sx(k2, l)
                     else
                        e_sn(Lf, l) = 0.5d0 * (sx(k1, l) + sx(k2, l))
                     end if
                  else
                     ! central approximation
                     e_sn(Lf, l) = 0.5d0 * (sx(k1, l) + sx(k2, l))
                  end if
                  e_st(Lf, l) = 0d0
               else
                  ! project the fluxes in flowlink direction
                  sutot1 = csu(Lf) * sxtot(k1, l) + snu(Lf) * sytot(k1, l)
                  sutot2 = csu(Lf) * sxtot(k2, l) + snu(Lf) * sytot(k2, l)

                  if (upwindbedload .or. Lf > Lnxi) then
                     ! upwind approximation (also at boundary cells for central scheme if jabndtreatment==0)
                     if (sutot1 > 0d0 .and. sutot2 > 0d0) then
                        e_sn(Lf, l) = csu(Lf) * sx(k1, l) + snu(Lf) * sy(k1, l)
                     else if (sutot1 < 0d0 .and. sutot2 < 0d0) then
                        e_sn(Lf, l) = csu(Lf) * sx(k2, l) + snu(Lf) * sy(k2, l)
                     else
                        e_sn(Lf, l) = csu(Lf) * (acl(Lf) * sx(k1, l) + (1d0 - acl(Lf)) * sx(k2, l)) + snu(Lf) * (acl(Lf) * sy(k1, l) + (1d0 - acl(Lf)) * sy(k2, l))
                     end if
                  else
                     ! central approximation
                     e_sn(Lf, l) = csu(Lf) * (acl(Lf) * sx(k1, l) + (1d0 - acl(Lf)) * sx(k2, l)) + snu(Lf) * (acl(Lf) * sy(k1, l) + (1d0 - acl(Lf)) * sy(k2, l))
                  end if
                  e_st(Lf, l) = -snu(Lf) * (acl(Lf) * sx(k1, l) + (1d0 - acl(Lf)) * sx(k2, l)) + csu(Lf) * (acl(Lf) * sy(k1, l) + (1d0 - acl(Lf)) * sy(k2, l)) ! to check
               end if
            end do
         else ! dry
            do l = 1, lsedtot
               e_sn(Lf, l) = 0d0
               e_st(Lf, l) = 0d0
            end do
         end if
      end do

      if (jabndtreatment == 1) then
         ! boundary flowlinks processed separately
         do Lf = Lnxi + 1, Lnx
            ! outflow
            if (hu(Lf) > epshu .and. u1(Lf) <= 0d0) then
               ! find left and right neighboring flownodes
               k1 = ln(1, Lf) ! boundary node
               k2 = ln(2, Lf) ! internal node
               !
               do l = 1, lsedtot
                  if (.not. has_bedload(tratyp(l))) cycle ! cycle if this fraction doesn't include bedload
                  !
                  if (kfsed(k1) * kfsed(k2) == 0) then
                     e_sn(Lf, l) = 0d0
                     e_st(Lf, l) = 0d0
                     cycle
                  end if
                  !
                  if (pure1d_mor .and. kcu(Lf) == -1) then
                     if (link1(k2) == Lf) then
                        e_sn(Lf, l) = sx(k2, l)
                     else
                        e_sn(Lf, l) = link1sign(k2) * sx(k2, l) ! TODO: check
                     end if
                     e_st(Lf, l) = 0d0
                  else
                     e_sn(Lf, l) = csu(Lf) * sx(k2, l) + snu(Lf) * sy(k2, l)
                     e_st(Lf, l) = -snu(Lf) * sx(k2, l) + csu(Lf) * sy(k2, l)
                  end if
               end do
               ! cross-check next statements below with Bert
            else if (hu(Lf) <= epshu) then ! dry
               do l = 1, lsedtot
                  if (.not. has_bedload(tratyp(l))) cycle ! cycle if this fraction doesn't include bedload
                  !
                  e_sn(Lf, l) = 0d0
                  e_st(Lf, l) = 0d0
               end do
            else ! inflow and wet
               do l = 1, lsedtot
                  if (.not. has_bedload(tratyp(l))) cycle ! cycle if this fraction doesn't include bedload
                  !
                  if (kfsed(k1) * kfsed(k2) == 0) then
                     e_sn(Lf, l) = 0d0
                     e_st(Lf, l) = 0d0
                     cycle
                  end if
                  !
                  e_sn(Lf, l) = csu(Lf) * sx(k1, l) + snu(Lf) * sy(k1, l)
                  e_st(Lf, l) = -snu(Lf) * sx(k1, l) + csu(Lf) * sy(k1, l)
               end do
            end if
         end do
      end if
   end subroutine fm_upwbed

end module m_fm_upwbed
