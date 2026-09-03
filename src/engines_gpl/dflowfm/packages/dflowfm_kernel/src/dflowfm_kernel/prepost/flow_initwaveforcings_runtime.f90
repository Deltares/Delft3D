!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
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

! This is the general hook-up to wave conditions for online wave coupling
module m_flow_initwaveforcings_runtime

   use precision, only: dp
   implicit none

   private
   integer, dimension(12) :: wave_items

   public :: flow_initwaveforcings_runtime
   public :: close_file_readers
   public :: open_file_readers

contains

   function close_file_reader(item_id) result(retval)
      use m_ec_support
      use m_ec_typedefs
      use netcdf
      use m_meteo, only: ecInstancePtr

      integer, intent(in) :: item_id
      integer :: ierror, i, j
      logical :: retval

      type(tEcItem), pointer :: item, source_item !< Item under consideration
      type(tEcFileReader), pointer             :: fileReaderPtr         !< helper pointer for a file reader
      retval = .true.

      item => ecSupportFindItem(ecInstancePtr, item_id)
      if (associated(item)) then
         do i=1, item%nConnections
            do j=1, item%connectionsPtr(i)%ptr%nSourceItems
               source_item => item%connectionsPtr(i)%ptr%sourceItemsPtr(j)%ptr         

               fileReaderPtr => ecSupportFindFileReader(ecInstancePtr, source_item%providerId)
               ierror = nf90_inquire(fileReaderPtr%fileHandle)
               if (ierror == nf90_noerr) then
                  ierror = nf90_close(fileReaderPtr%fileHandle)
               end if

               if (ierror /= nf90_noerr) then
                  retval = .false.
               end if      

            end do
         end do   
      end if

   end function close_file_reader

   function open_file_reader(item_id) result(retval)
      use m_ec_support
      use m_ec_typedefs
      use netcdf
      use m_meteo, only: ecInstancePtr

      integer, intent(in) :: item_id
      integer :: ierror, chunkSize, i, j
      logical :: retval
      type(tEcItem), pointer :: item, source_item !< Item under consideration
      type(tEcFileReader), pointer             :: fileReaderPtr         !< helper pointer for a file reader

      retval = .true.

      item => ecSupportFindItem(ecInstancePtr, item_id)
      if (associated(item)) then
         do i=1, item%nConnections
            do j=1, item%connectionsPtr(i)%ptr%nSourceItems
               source_item => item%connectionsPtr(i)%ptr%sourceItemsPtr(j)%ptr         

               fileReaderPtr => ecSupportFindFileReader(ecInstancePtr, source_item%providerId)
               chunkSize = 4096
               ierror = nf90_open(trim(fileReaderPtr%fileName), NF90_NOWRITE, fileReaderPtr%fileHandle, chunkSize)
               if (ierror /= nf90_noerr) then
                  retval = .false.
               end if      

            end do
         end do   
      end if

   end function open_file_reader

   function close_file_readers() result(retval)
      use m_meteo
      
      logical :: retval      
      integer :: i, item_id

      retval = .true.
      do i = 1, size(wave_items)
         item_id = wave_items(i)
         if (item_id /= ec_undef_int) then
            retval = retval .and. close_file_reader(item_id)
         end if
      end do
   end function close_file_readers

   function open_file_readers() result(retval)
      use m_meteo
      
      logical :: retval      
      integer :: i, item_id

      retval = .true.
      do i = 1, size(wave_items)
         item_id = wave_items(i)
         if (item_id /= ec_undef_int) then
            retval = retval .and. open_file_reader(item_id)
         end if
      end do
   end function open_file_readers

   function flow_initwaveforcings_runtime() result(retval)
      use m_flowparameters
      use m_flowtimes ! Two stages: 1 = collect elsets for which data is provided
      use m_flowgeom !             2 = add relations between elsets and their providers
      use unstruc_model
      use unstruc_files
      use timespace
      use m_missing
      use m_waves
      use m_alloc
      use m_meteo
      use timespace_parameters, only: OPERAND_OVERRIDE

      logical :: retval !< Whether init was successful or not

      integer :: ierr
      integer :: filetype_l
      integer :: method_l
      integer :: operand_l
      character(len=256) :: qid_l
      integer, allocatable :: kcw(:) ! mask array

      if (extfor_wave_initialized) then
         retval = .true.
         return
      end if

      wave_items = ec_undef_int

      filetype_l = 14 ! netcdf
      method_l = 7 ! only time interpolation, extrapolation allowed (online WAVE)
      operand_l = OPERAND_OVERRIDE
      kx = 1 ! default vectormax = 1
      !
      allocate (kcw(ndx), source=1, stat=ierr)
      call aerr('kcw(ndx)', ierr, ndx)

      qid_l = 'hrms'
      if (.not. allocated(hwavcom)) then
         allocate (hwavcom(ndx), stat=ierr)
         call aerr('hwavcom(ndx)', ierr, ndx)
         hwavcom = hwavuni
      end if
      success = ec_addtimespacerelation(qid_l, xz(1:ndx), yz(1:ndx), kcw, kx, md_wavefile, filetype_l, method_l, operand_l, quiet=.true.)
      if (.not. success) then
         !
         ! Most commonly, WAVE data has not been written to the com-file yet.
         ! Just try it the next timestep again
         !
         retval = .false.
         goto 888
      end if
      !
      if (jatpwav == TPWAVSMOOTH) then
         ! take smoothed peak wave period. Arjen: "Deze parameter is beter"
         qid_l = 'tps'
      elseif (jatpwav == TPWAVRELATIVE) then
         ! take relative peak wave period. Bas; scale factor required!!
         qid_l = 'rtp'
      else
         qid_l = 'tp'
      end if
      if (.not. allocated(twavcom)) then
         allocate (twavcom(ndx), stat=ierr)
         call aerr('twavcom(ndx)', ierr, ndx)
         twavcom = 0.0_dp
      end if
      success = ec_addtimespacerelation(qid_l, xz(1:ndx), yz(1:ndx), kcw, kx, md_wavefile, filetype_l, method_l, operand_l, quiet=.true.)
      !
      qid_l = 'dir'
      if (.not. allocated(phiwav)) then
         allocate (phiwav(ndx), stat=ierr)
         call aerr('phiwav(ndx)', ierr, ndx)
         phiwav = 0.0
      end if
      success = ec_addtimespacerelation(qid_l, xz(1:ndx), yz(1:ndx), kcw, kx, md_wavefile, filetype_l, method_l, operand_l, quiet=.true.)
      !
      qid_l = 'dissurf'
      if (.not. allocated(dsurf)) then
         allocate (dsurf(ndx), stat=ierr)
         call aerr('dsurf(ndx)', ierr, ndx)
         dsurf = 0.0
      end if
      success = ec_addtimespacerelation(qid_l, xz(1:ndx), yz(1:ndx), kcw, kx, md_wavefile, filetype_l, method_l, operand_l, quiet=.true.)
      !
      qid_l = 'diswcap'
      if (.not. allocated(dwcap)) then
         allocate (dwcap(ndx), stat=ierr)
         call aerr('dwcap(ndx)', ierr, ndx)
         dwcap = 0.0
      end if
      success = ec_addtimespacerelation(qid_l, xz(1:ndx), yz(1:ndx), kcw, kx, md_wavefile, filetype_l, method_l, operand_l, quiet=.true.)
      !
      qid_l = 'fx'
      if (.not. allocated(sxwav)) then
         allocate (sxwav(ndx), stat=ierr)
         call aerr('sxwav(ndx)', ierr, ndx)
         sxwav = 0.0
      end if
      success = ec_addtimespacerelation(qid_l, xz(1:ndx), yz(1:ndx), kcw, kx, md_wavefile, filetype_l, method_l, operand_l, quiet=.true.)
      !
      qid_l = 'fy'
      if (.not. allocated(sywav)) then
         allocate (sywav(ndx), stat=ierr)
         call aerr('sywav(ndx)', ierr, ndx)
         sywav = 0.0
      end if
      success = ec_addtimespacerelation(qid_l, xz(1:ndx), yz(1:ndx), kcw, kx, md_wavefile, filetype_l, method_l, operand_l, quiet=.true.)
      !
      qid_l = 'wsbu'
      if (.not. allocated(sbxwav)) then
         allocate (sbxwav(ndx), stat=ierr)
         call aerr('sbxwav(ndx)', ierr, ndx)
         sbxwav = 0.0
      end if
      success = ec_addtimespacerelation(qid_l, xz(1:ndx), yz(1:ndx), kcw, kx, md_wavefile, filetype_l, method_l, operand_l, quiet=.true.)
      !
      qid_l = 'wsbv'
      if (.not. allocated(sbywav)) then
         allocate (sbywav(ndx), stat=ierr)
         call aerr('sbywav(ndx)', ierr, ndx)
         sbywav = 0.0
      end if
      success = ec_addtimespacerelation(qid_l, xz(1:ndx), yz(1:ndx), kcw, kx, md_wavefile, filetype_l, method_l, operand_l, quiet=.true.)
      !
      qid_l = 'mx'
      if (.not. allocated(mxwav)) then
         allocate (mxwav(ndx), stat=ierr)
         call aerr('mxwav(ndx)', ierr, ndx)
         mxwav = 0.0
      end if
      success = ec_addtimespacerelation(qid_l, xz(1:ndx), yz(1:ndx), kcw, kx, md_wavefile, filetype_l, method_l, operand_l, quiet=.true.)
      !
      qid_l = 'my'
      if (.not. allocated(mywav)) then
         allocate (mywav(ndx), stat=ierr)
         call aerr('mywav(ndx)', ierr, ndx)
         mywav = 0.0
      end if
      success = ec_addtimespacerelation(qid_l, xz(1:ndx), yz(1:ndx), kcw, kx, md_wavefile, filetype_l, method_l, operand_l, quiet=.true.)
      !
      qid_l = 'ubot'
      if (.not. allocated(uorbwav)) then
         allocate (uorbwav(ndx), stat=ierr)
         call aerr('uorbwav(ndx)', ierr, ndx)
         uorbwav = 0.0
      end if
      success = ec_addtimespacerelation(qid_l, xz(1:ndx), yz(1:ndx), kcw, kx, md_wavefile, filetype_l, method_l, operand_l, quiet=.true.)
      !
      retval = success

888   continue
      if (retval) then
         wave_items = [item_hrms, item_tp, item_dir, item_fx, item_fy, item_wsbu, item_wsbv, item_mx, item_my, item_ubot, item_dissurf, item_diswcap]
      end if
      extfor_wave_initialized = retval ! Becomes .true. or .false., depending on whether the timespace relations have been created succesfully.

   end function flow_initwaveforcings_runtime

end module m_flow_initwaveforcings_runtime
