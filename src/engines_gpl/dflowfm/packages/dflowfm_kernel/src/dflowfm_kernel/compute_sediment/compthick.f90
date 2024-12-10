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

module m_compthick

   implicit none

   private

   public :: compthick

contains

   subroutine compthick()
   !!--description-----------------------------------------------------------------
      !
      ! Function:   Compute new thickness of transport and exchange layer
      !
   !!--declarations----------------------------------------------------------------
      use precision
      use bedcomposition_module
      use m_flow, only: hs
      use m_flowgeom, only: ndx
      use unstruc_files, only: mdia
      use message_module
      use m_sediment, only: stmpar
      use m_bedform
      !
      implicit none
      !
      ! The following list of pointer parameters is used to point inside the gdp structure
      !
      integer, pointer :: ttlform
      integer, pointer :: telform
      real(fp), pointer :: ttlalpha
      real(fp), pointer :: ttlmin
      real(fp), dimension(:), pointer :: duneheight
      !
      ! Local variables
      !
      integer :: istat
      integer :: nm
      integer, pointer :: iunderlyr
      real(fp), dimension(:), pointer :: thtrlyr
      character(256) :: errorstr
      logical :: error
      !
   !! executable statements -------------------------------------------------------
      !
      ttlalpha => stmpar%morpar%ttlalpha
      ttlmin => stmpar%morpar%ttlmin
      ttlform => stmpar%morpar%ttlform
      telform => stmpar%morpar%telform
      duneheight => bfmpar%duneheight
      !
      istat = bedcomp_getpointer_integer(stmpar%morlyr, 'IUnderLyr', iunderlyr)
      if (istat /= 0) then
         errorstr = 'Memory problem in COMPTHICK'
         call write_error(errorstr, unit=mdia)
         error = .true.
         return
      end if
      !
      select case (iunderlyr)
      case (2)
         !
         istat = bedcomp_getpointer_realfp(stmpar%morlyr, 'ThTrLyr', thtrlyr)
         if (istat /= 0) then
            errorstr = 'Memory problem in COMPTHICK'
            call write_error(errorstr, unit=mdia)
            error = .true.
            return
         end if
         !
         ! Determine new transport layer thickness
         !
         select case (ttlform)
         case (2)
            !
            ! proportional to water depth
            !
            do nm = 1, ndx
               thtrlyr(nm) = max(ttlalpha * hs(nm), ttlmin)
            end do
         case (3)
            !
            ! proportional to dune height
            !
            do nm = 1, ndx
               thtrlyr(nm) = max(ttlalpha * duneheight(nm), ttlmin)
            end do
         case default
            !
            ! nothing to do: constant in time
            !
         end select
         !
         ! Determine new exchange layer thickness
         !
         select case (telform)
         case (1)
         case default
         end select
      case default
         !
         ! No active layers: nothing to do
         !
      end select
   end subroutine compthick

end module m_compthick
