!!  Copyright (C)  Stichting Deltares, 2021-2024.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

      subroutine renum_bnd(openbndsect_coll, ibnd, ibnd_new)

      ! function : renumber boundary in the sections

      ! (c) Deltares

      ! global declarations

      use hydmod                   ! module contains everything for the hydrodynamic description
      implicit none

      ! declaration of the arguments

      type(t_openbndsect_coll)               :: openbndsect_coll       ! collection of openbndsects
      integer                                :: ibnd                   ! boundary number (negative)
      integer                                :: ibnd_new               ! new boundary number (negative)

      ! local declarations

      integer                                :: no_sect                ! number of sections
      integer                                :: i_sect                 ! index of section
      integer                                :: no_bnd                 ! number of boundaries in section
      integer                                :: i_bnd                  ! index of boundary
      type(t_openbndsect), pointer           :: openbndsect            ! single section

      ! look for boundary number in the sections set new number

      no_sect = openbndsect_coll%cursize

      do i_sect = 1 , no_sect

         openbndsect => openbndsect_coll%openbndsect_pnts(i_sect)
         no_bnd = openbndsect%openbndlin_coll%cursize

         do i_bnd = 1 , no_bnd
            if ( openbndsect%openbndlin_coll%openbndlin_pnts(i_bnd)%ibnd .eq. ibnd ) then
               openbndsect%openbndlin_coll%openbndlin_pnts(i_bnd)%ibnd_new = ibnd_new
               goto 200
            endif
         enddo

      enddo
  200 continue

      return
      end
