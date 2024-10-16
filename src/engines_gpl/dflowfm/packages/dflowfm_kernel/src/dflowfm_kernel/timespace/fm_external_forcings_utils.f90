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

!> Utilities module with functions for initializing and updating external forcings.
module fm_external_forcings_utils
   use precision_basics, only: hp
   implicit none

contains

   !> Convert quantity id (from .ext file) to tracer name (split in generic qidname and specific tracer name).
   !! If the input qid is not tracer, then the same qid is returned (and no tracer name)
   subroutine get_tracername(qid, trname, qidname)
      use m_transportdata, only: DEFTRACER
      implicit none

      character(len=*), intent(in) :: qid      !< Original quantityid, e.g., 'tracerbndfluor'.
      character(len=*), intent(out) :: trname  !< The trimmed tracer name, e.g., 'fluor'.
      character(len=*), intent(out) :: qidname !< The base quantity name for further use in external forcing, e.g., 'tracerbnd'.

      trname = ''
      qidname = qid

      if (qid(1:9) == 'tracerbnd') then
         qidname = qid(1:9)
         if (len_trim(qid) > 9) then
            trname = trim(qid(10:))
         else
            trname = trim(DEFTRACER)
         end if
      else if (qid(1:13) == 'initialtracer') then
         qidname = qid(1:13)
         if (len_trim(qid) > 13) then
            trname = trim(qid(14:))
         else
            trname = trim(DEFTRACER)
         end if
      end if

      return
   end subroutine get_tracername

   !> Convert quantity id (from .ext file) to sediment fraction name (split in generic qidname and specific fraction name).
   !! If the input qid is no sediment fraction, then the same qid is returned (and no fraction name)
   subroutine get_sedfracname(qid, sfname, qidname)
      implicit none

      character(len=*), intent(in) :: qid        !< Original quantityid, e.g., 'sedfracbndsediment1'.
      character(len=*), intent(out) :: sfname    !< The trimmed tracer name, e.g., 'sediment1'.
      character(len=*), intent(inout) :: qidname !< The base quantity name for further use in external forcing, e.g., 'sedfracbnd'.

      sfname = ''

      if (index(qid, 'sedfracbnd') == 1) then
         qidname = qid(1:10)
         if (len_trim(qid) > 10) then
            sfname = trim(qid(11:))
         else
            sfname = trim('unknown_sediment_fraction')
         end if
      else if (index(qid, 'initialsedfrac') == 1) then
         qidname = qid(1:14)
         if (len_trim(qid) > 14) then
            sfname = trim(qid(15:))
         else
            sfname = trim('unknown_sediment_fraction')
         end if
      else if (index(qid, 'initialverticalsedfracprofile') == 1) then
         qidname = qid(1:29)
         if (len_trim(qid) > 29) then
            sfname = trim(qid(30:))
         else
            sfname = trim('unknown_sediment_fraction')
         end if
      else if (index(qid, 'initialverticalsigmasedfracprofile') == 1) then
         qidname = qid(1:34)
         if (len_trim(qid) > 34) then
            sfname = trim(qid(35:))
         else
            sfname = trim('unknown_sediment_fraction')
         end if
      end if
   end subroutine get_sedfracname

end module fm_external_forcings_utils
