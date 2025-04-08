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
   use m_fm_wq_processes_sub, only: get_waqinputname
   use precision_basics, only: hp
   implicit none
   private
   public :: split_qid
   public :: get_tracername
   public :: get_sedfracname
   public :: get_constituent_name

contains

   !> Split quantity id (qid) into base qid and specific qid.
   !! The base qid is the part of the qid that is common for all tracers of the same type.
   !! The specific qid is the part of the qid that is specific for a certain tracer.
   subroutine split_qid(qid, qid_base, qid_specific)
      use mass_balance_areas_routines, only: get_mbainputname

      character(len=*), intent(in) :: qid !< Original quantityid, e.g., 'tracerbndfluor'.
      character(len=*), intent(out) :: qid_base !< The base quantity name, e.g., 'tracerbnd'.
      character(len=*), intent(out) :: qid_specific !< The specific quantity name, e.g., 'fluor'.

      call get_tracername(qid, qid_specific, qid_base)
      ! As soon as the qid is different from the base qid, the qid is split and this routine is finished.
      if (qid_base /= qid) then
         return
      end if
      call get_sedfracname(qid, qid_specific, qid_base)
      if (qid_base /= qid) then
         return
      end if
      call get_waqinputname(qid, qid_specific, qid_base)
      if (qid_base /= qid) then
         return
      end if
      call get_mbainputname(qid, qid_specific, qid_base)
      if (qid_base /= qid) then
         return
      end if
      call get_constituent_name(qid, qid_specific, qid_base)
      if (qid_base /= qid) then
         return
      end if
   end subroutine split_qid

   !> Convert quantity id (from .ext file) to tracer name (split in generic qidname and specific tracer name).
   !! If the input qid is not tracer, then the same qid is returned (and no tracer name)
   subroutine get_tracername(qid, trname, qidname)
      use m_transportdata, only: DEFTRACER
      implicit none

      character(len=*), intent(in) :: qid !< Original quantityid, e.g., 'tracerbndfluor'.
      character(len=*), intent(out) :: trname !< The trimmed tracer name, e.g., 'fluor'.
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

      character(len=*), intent(in) :: qid !< Original quantityid, e.g., 'sedfracbndsediment1'.
      character(len=*), intent(out) :: sfname !< The trimmed tracer name, e.g., 'sediment1'.
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

   !> Convert quantity (from .ext file) to constituent name (split in generic base_quantity and specific constituent_name).
   !! If the original_quantity does not involve consituents, then the passed base_quantity is unchanged (and empty constituent name).
   !! For example: 'sourcesink_salinityDelta' -> 'sourcesink_constituentDelta', 'salinity'.
   !!
   !! This subroutine currently only covers source sinks, because they are the only external forcings that generalize on
   !! constituents. Other external forcings are handled in get_tracername, get_sedfracname, etc.
   subroutine get_constituent_name(original_quantity, constituent_name, base_quantity)
      use string_module, only: strcmpi
      implicit none

      character(len=*), intent(in) :: original_quantity !< Original quantity id, e.g., 'sourcesink_salinityDelta'.
      character(len=*), intent(out) :: constituent_name !< The trimmed constituent name, e.g., 'salinity', or 'sand', or 'fluor'. Empty '' if not a constituent.
      character(len=*), intent(out) :: base_quantity !< The base quantity name for further use in external forcing, e.g., 'sourcesink_constituentDelta'. Unchanged original_quantity if not a constituent.

      integer :: quantity_length
      integer :: index_prefix_end, index_suffix_start

      constituent_name = ''

      quantity_length = len_trim(original_quantity)
      index_prefix_end = min(len_trim('sourcesink_'), quantity_length)
      index_suffix_start = max(1, quantity_length - len_trim('Delta') + 1)

      if (strcmpi(original_quantity(1:index_prefix_end), 'sourcesink_') &
         .and. strcmpi(original_quantity(index_suffix_start:quantity_length), 'Delta')) then
         base_quantity = 'sourcesink_constituentDelta'
         constituent_name = original_quantity(index_prefix_end + 1:index_suffix_start - 1)
      end if

      return
   end subroutine get_constituent_name

end module fm_external_forcings_utils
