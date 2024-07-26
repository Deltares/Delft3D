!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
!
!  This library is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation version 2.1.
!
!  This library is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
!  You should have received a copy of the GNU Lesser General Public
!  License along with this library; if not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.

!
!

!> This module contains the read methods for the meteo files.
!! @author stef.hummel@deltares.nl
!! @author herman.kernkamp@deltares.nl
!! @author adri.mourits@deltares.nl
!! @author edwin.spee@deltares.nl
module m_ec_filereader_read
   use precision
   use string_module
   use m_ec_typedefs
   use m_ec_support
   use m_ec_message
   use m_ec_bcreader
   use mathconsts
   use time_module
   use string_module
   use m_ec_astro
   use m_alloc

   implicit none

   private

   public :: ecUniReadFirstLine
   public :: ecUniReadTimeSteps
   public :: ecUniReadBlock
   public :: ecBCReadBlock
   public :: ecFourierReadAll
   public :: ecFindInFile
   public :: ecSpiderwebAndCurviFindInFile
   public :: ecSpiderAndCurviAndArcinfoReadToBody
   public :: ecSpiderwebReadBlock
   public :: ecNetcdfGetTimeIndexByTime
   public :: ecArcinfoAndT3dReadBlock
   public :: ecCurviReadBlock

   public :: ecNetcdfReadVariable
   public :: ecNetcdfReadBlock
   public :: ecQhtableReadAll
   public :: ect3DFindInFile
   public :: ecNetcdfReadNextBlock
   public :: ecApplyCorrectionToCmp
   public :: ecSampleReadAll
   public :: ecParseARCinfoMask

   interface ecSampleReadAll
      module procedure ecSampleReadAll_from_fileReader
      module procedure ecSampleReadAll_from_lun
   end interface

   character(len=128) :: message

   contains

      ! =======================================================================

      !> Read the first line from a uni* file.
      function ecUniReadFirstLine(fileReaderPtr) result(rec)
         character(len=:), allocatable  :: rec           !< content of a line
         type(tEcFileReader), pointer :: fileReaderPtr !< intent(in)
         !
         integer :: istat !< status of read operation
         character(len=maxNameLen)      :: iomsg         !< io-message
         !
         iomsg = ""
         rewind(unit=fileReaderPtr%fileHandle, IOMSG = iomsg, IOSTAT = istat)
         if (istat /= 0) then
            call setECMessage("Rewind failed on " // trim(fileReaderPtr%fileName) // ". Error: " // trim(iomsg))
            return
         endif
         ! continue reading lines untill a data line is encountered
         do
            iomsg = ""
!           call GetLine(fileReaderPtr%fileHandle, rec, istat, iomsg=iomsg)
            call GetLine(fileReaderPtr%fileHandle, rec, istat)
            if (istat == 0) then
               call strip_comment(rec)
               if (len_trim(rec)>0) then
                  exit
               end if
            else
!              call setECMessage("  IOMessage: "//trim(iomsg))
               call setECMessage("File error in "//trim(fileReaderPtr%fileName))
               exit
            end if
         end do
      end function ecUniReadFirstLine

      ! =======================================================================

      !> Read the number of time steps of the next record from a uni* file.
      !! meteo1: readseries
      function ecUniReadTimeSteps(fileReaderPtr, time_steps) result(success)
         logical                                  :: success       !< function status
         type(tEcFileReader), pointer             :: fileReaderPtr !< intent(in)
         real(hp),                    intent(out) :: time_steps    !< number of time steps of length tEcTimeFrame%time_unit
         !
         character(:), allocatable :: rec   !< content of a line
         integer        :: istat !< status of read operation
         !
         success = .false.
         ! continue reading lines untill a data line is encountered
         do
            call GetLine(fileReaderPtr%fileHandle, rec, istat)
            if (istat == 0) then
               if (.not. (rec(1:1) == '*' .or. rec(1:1) == '#' .or. len_trim(rec) == 0)) then
                  read(rec, *, IOSTAT = istat) time_steps
                  if (istat == 0) then
                     ! Convert from minutes to seconds.
                     time_steps = time_steps * 60.0_hp
                     success = .true.
                  else
                     call setECMessage("ERROR: ec_filereader_read::ecUniReadTimeSteps: Read failure before end of file: "//trim(fileReaderPtr%fileName))
                     return
                  end if
               end if
            else
               call setECMessage("INFO: ec_filereader_read::ecUniReadTimeSteps: File end has been reached of: "//trim(fileReaderPtr%fileName))
               exit
            end if
         end do
      end function ecUniReadTimeSteps

      ! =======================================================================

      !> Read the next record from a uni* file.
      !! meteo1: readseries
      function ecUniReadBlock(fileReaderPtr, time_steps, values) result(success)
         logical                               :: success       !< function status
         type(tEcFileReader),    pointer       :: fileReaderPtr !< intent(in)
         real(hp),               intent(inout) :: time_steps    !< number of time steps of duration: MJD
         real(hp), dimension(:), intent(inout) :: values        !< read values
         !
         integer        :: n_values !< number of quantities in the file
         character(len=:), allocatable :: rec, rec0!< content of a line
         integer        :: istat    !< status of read operation
         integer        :: i        !< loop counter

         !
         success = .false.
         n_values = size(values)
         ! continue reading lines untill a data line is encountered
         do
            call GetLine(fileReaderPtr%fileHandle, rec, istat)
            rec0 = rec                                         ! preserve originally read line for error reporting
            if (istat == 0) then
               call strip_comment(rec)
               if (len_trim(rec)>0) then
                  read(rec, *, IOSTAT = istat) time_steps, ( values(i), i=1,n_values )
                  if (istat == 0) then
                     ! Convert from minutes to MJD
                     time_steps = fileReaderPtr%tframe%ec_refdate - fileReaderPtr%tframe%ec_timezone/24.0 + time_steps /1440.0
                     success = .true.
                  else
                     call setECMessage("Read failure before end of file: "//trim(fileReaderPtr%fileName))
                     call setECMessage("     string = '"//trim(rec0)//"'")
                     return
                  end if
                  exit
               end if
            else
               call setECMessage("File end has been reached of: "//trim(fileReaderPtr%fileName))
               exit
            end if
         end do
      end function ecUniReadBlock

      ! =======================================================================

      !> Read the next record from a *.bc file.
      function ecBCReadBlock(fileReaderPtr, time_steps, values) result(success)
         implicit none
         logical                               :: success       !< function status
         type(tEcFileReader),    pointer       :: fileReaderPtr !< intent(in)
         real(hp),               intent(inout) :: time_steps    !< number of time steps of duration: seconds
         real(hp), dimension(:), intent(inout) :: values        !< read values

         success = ecBCreadline(fileReaderPtr, values = values, time_steps = time_steps, eof = fileReaderPtr%end_of_data)
      end function ecBCReadBlock

      ! =======================================================================

      !> Read the next record from a Curvi file.
      !! meteo1: readarcinfoblock
      function ecCurviReadBlock(fileReaderPtr, handle, t0t1) result(success)
         use string_module
         !
         logical                         :: success       !< function status
         type(tEcFileReader), pointer    :: fileReaderPtr !< intent(in)
         integer,             intent(in) :: handle        !< file handle
         integer,             intent(in) :: t0t1          !< read into Field T0 or T1 (0,1)
         !
         integer                   :: n_cols     !< number of columns
         integer                   :: n_rows     !< number of rows
         type(tEcItem),    pointer :: item       !< Item containing the first Quantity
         character(len=maxNameLen) :: rec        !< helper variable
         real(hp)                  :: time_steps !< number of time steps from reference time of current data block
         integer                   :: i, j, k    !< loop counters
         character(len=maxNameLen) :: keyword    !< helper variable
         real(hp)                  :: timesteps  !< helper variable
         integer                   :: istat      !< status of read operation
         !
         success = .false.
         !
         n_cols = fileReaderPtr%items(1)%ptr%elementSetPtr%n_cols
         n_rows = fileReaderPtr%items(1)%ptr%elementSetPtr%n_rows
         ! Find the time specification line of the next block.
         keyword = 'TIME'
         rec = ecFindInFile(handle, keyword)
         if (len(trim(rec)) == 0) then
            call setECMessage("ERROR: ec_filereader_read::ecCurviReadBlock: Failed to find next 'TIME =' record in file: "//trim(fileReaderPtr%fileName))
            return
         end if
         ! Read and convert the timesteps to seconds.
         if (.not. ecGetTimesteps(rec, time_steps, .false.)) then
            return
         end if
         !
         ! ===== T0 =====
         if (t0t1 == 0) then
            ! Set the new time.
            timesteps = ecSupportThisTimeToMJD(fileReaderPtr%tframe, time_steps)
            do k=1, fileReaderPtr%nItems
               item => fileReaderPtr%items(k)%ptr
               item%sourceT0FieldPtr%timesteps = timesteps
            end do
            ! Set the new data.
            do k=1, fileReaderPtr%nItems
               item => fileReaderPtr%items(k)%ptr
               do i=n_rows, 1, -1
                  read(handle, *, IOSTAT = istat) (item%sourceT0FieldPtr%arr1dPtr((i-1)*n_cols+j), j=1, n_cols)
                  if(istat /= 0) then
                     call setECMessage("ec_filereader_read::ecCurviReadBlock: Read failure before end of file: "//trim(fileReaderPtr%fileName))
                     return
                  end if
               end do
            end do
         ! ===== T1 =====
         else if (t0t1 == 1) then
            ! Set the new time.
            timesteps = ecSupportThisTimeToMJD(fileReaderPtr%tframe, time_steps)
            do k=1, fileReaderPtr%nItems
               item => fileReaderPtr%items(k)%ptr
               item%sourceT1FieldPtr%timesteps = timesteps
            end do
            ! Set the new data.
            do k=1, fileReaderPtr%nItems
               item => fileReaderPtr%items(k)%ptr
               do i=n_rows, 1, -1
                  read(handle, *, IOSTAT = istat) (item%sourceT1FieldPtr%arr1dPtr((i-1)*n_cols+j), j=1, n_cols)
                  if(istat /= 0) then
                     call setECMessage("ec_filereader_read::ecCurviReadBlock: Read failure before end of file: "//trim(fileReaderPtr%fileName))
                     return
                  end if
               end do
            end do
         else
            call setECMessage("ERROR: ec_filereader_read::ecCurviReadBlock: Invalid Field specified.")
            return
         end if
         success = .true.
      end function ecCurviReadBlock

      ! =======================================================================

      !> Read the next record from a Arcinfo or Curvi file.
      !! meteo1: readarcinfoblock
      function ecArcinfoAndT3dReadBlock(fileReaderPtr, handle, t0t1, n_cols, n_rows, item1, item2, item3) result(success)
         use string_module
         logical                             :: success       !< function status
         type(tEcFileReader),     pointer    :: fileReaderPtr !< intent(in)
         integer,                 intent(in) :: handle        !< file handle
         integer,                 intent(in) :: t0t1          !< read into Field T0 or T1 (0,1)
         integer,                 intent(in) :: n_cols
         integer,                 intent(in) :: n_rows
         type(tEcItem),           pointer    :: item1         !< Item containing quantity1, intent(inout)
         type(tEcItem), optional, pointer    :: item2         !< Item containing quantity2, intent(inout)
         type(tEcItem), optional, pointer    :: item3         !< Item containing quantity3, intent(inout)
         !
         character(len=maxNameLen) :: rec        !< helper variable
         real(hp)                  :: time_steps !< number of time steps from reference time of current data block
         integer                   :: i, j       !< loop counter
         character(len=maxNameLen) :: keyword    !< helper variable
         integer                   :: istat      !< status of read operation

         !
         success = .false.
         keyword = 'TIME'
         ! Find the time specification line of the next block.
         rec = ecFindInFile(handle, keyword)
         if (len(trim(rec)) == 0) then
            call setECMessage("ERROR: ec_filereader_read::ecArcinfoAndT3dReadBlock: Reached end of file: "//trim(fileReaderPtr%fileName))
            return
         end if
         ! Read and convert the timesteps to seconds.
         if (.not. ecGetTimesteps(rec, time_steps, .false.)) then
            return
         else
            success = .true.
         end if
         !
         ! ===== T0 =====
         if (t0t1 == 0) then
            ! Set the new time.
            if (success) then
               item1%sourceT0FieldPtr%timesteps = ecSupportThisTimeToMJD(fileReaderPtr%tframe, time_steps)
               if (present(item2)) then
                  item2%sourceT0FieldPtr%timesteps = item1%sourceT0FieldPtr%timesteps
               end if
               if (present(item3)) then
                  item3%sourceT0FieldPtr%timesteps = item1%sourceT0FieldPtr%timesteps
               end if
            end if
            ! Set the new data.
            do i=n_rows, 1, -1
               read(handle, *, IOSTAT = istat) (item1%sourceT0FieldPtr%arr1dPtr((i-1)*n_cols+j), j=1, n_cols)
               if(istat /= 0) then
                  call setECMessage("ec_filereader_read::ecUniReadBlock: Read failure before end of file: "//trim(fileReaderPtr%fileName))
                  call setECMessage("     line = "//trim(rec))
                  success = .false.
                  return
               end if
            end do
            if (present(item2)) then
               do i=n_rows, 1, -1
                  read(handle, *, IOSTAT = istat) (item2%sourceT0FieldPtr%arr1dPtr((i-1)*n_cols+j), j=1, n_cols)
                  if(istat /= 0) then
                     call setECMessage("ec_filereader_read::ecUniReadBlock: Read failure before end of file: "//trim(fileReaderPtr%fileName))
                     call setECMessage("     line = "//trim(rec))
                     success = .false.
                     return
                  end if
               end do
            end if
            if (present(item3)) then
               do i=n_rows, 1, -1
                  read(handle, *, IOSTAT = istat) (item3%sourceT0FieldPtr%arr1dPtr((i-1)*n_cols+j), j=1, n_cols)
                  if(istat /= 0) then
                     call setECMessage("ec_filereader_read::ecUniReadBlock: Read failure before end of file: "//trim(fileReaderPtr%fileName))
                     call setECMessage("     line = "//trim(rec))
                     success = .false.
                     return
                  end if
               end do
            end if
         ! ===== T1 =====
         else if(t0t1 == 1) then
            ! Set the new time.
            if (success) then
               item1%sourceT1FieldPtr%timesteps = ecSupportThisTimeToMJD(fileReaderPtr%tframe, time_steps)
               if (present(item2)) then
                  item2%sourceT1FieldPtr%timesteps = item1%sourceT1FieldPtr%timesteps
               end if
               if (present(item3)) then
                  item3%sourceT1FieldPtr%timesteps = item1%sourceT1FieldPtr%timesteps
               end if
            end if
            ! Set the new data.
            do i=n_rows, 1, -1
               read(handle, *, IOSTAT = istat) (item1%sourceT1FieldPtr%arr1dPtr((i-1)*n_cols+j), j=1, n_cols)
               if(istat /= 0) then
                  call setECMessage("ec_filereader_read::ecUniReadBlock: Read failure before end of file: "//trim(fileReaderPtr%fileName))
                  call setECMessage("     line = "//trim(rec))
                  success = .false.
               return
               end if
            end do
            if (present(item2)) then
               do i=n_rows, 1, -1
                  read(handle, *, IOSTAT = istat) (item2%sourceT1FieldPtr%arr1dPtr((i-1)*n_cols+j), j=1, n_cols)
               if(istat /= 0) then
                  call setECMessage("ec_filereader_read::ecUniReadBlock: Read failure before end of file: "//trim(fileReaderPtr%fileName))
                  call setECMessage("     line = "//trim(rec))
                  success = .false.
               return
               end if
               end do
            end if
            if (present(item3)) then
               do i=n_rows, 1, -1
                  read(handle, *, IOSTAT = istat) (item3%sourceT1FieldPtr%arr1dPtr((i-1)*n_cols+j), j=1, n_cols)
                  if(istat /= 0) then
                     call setECMessage("ec_filereader_read::ecUniReadBlock: Read failure before end of file: "//trim(fileReaderPtr%fileName))
                     call setECMessage("     line = "//trim(rec))
                     success = .false.
                     return
                  end if
               end do
            end if
         else
            call setECMessage("ERROR: ec_filereader_read::ecArcinfoAndT3dReadBlock: Invalid Field specified.")
            success = .false.
         end if
      end function ecArcinfoAndT3dReadBlock

      ! =======================================================================
      ! TODO: we should really switch to newer spiderweb reader in
      ! ec_module\packages\ec_module\src\meteo\meteo_read.f90

      !> Read the next record from a spiderweb file.
      !! meteo1: reaspwtim
      function ecSpiderwebReadBlock(fileReaderPtr, item1, item2, item3, t0t1, n_cols, n_rows) result(success)
         logical                         :: success       !< function status
         type(tEcFileReader), pointer    :: fileReaderPtr !< intent(in)
         type(tEcItem),       pointer    :: item1         !< Item containing quantity1, intent(inout)
         type(tEcItem),       pointer    :: item2         !< Item containing quantity2, intent(inout)
         type(tEcItem),       pointer    :: item3         !< Item containing quantity3, intent(inout)
         integer,             intent(in) :: t0t1          !< read into Field T0 or T1 (0,1)
         integer,             intent(in) :: n_cols
         integer,             intent(in) :: n_rows
         !
         character(len=maxNameLen) :: rec        !< helper variable
         real(hp)                  :: time_steps !< number of time steps from reference time of current data block
         integer                   :: i, j       !< loop counter
         real(hp)                  :: x_spw_eye
         real(hp)                  :: y_spw_eye
         real(hp)                  :: p_drop_spw_eye
         character(len=maxNameLen) :: keyword
         integer                   :: istat      ! Reader status code
         !
         success = .true.                        ! Returns success, even when the file end is reached (see UNST-708)
         keyword = 'TIME'
         !
         rec = ecFindInFile(fileReaderPtr%fileHandle, 'TIME')
         if (len(trim(rec)) == 0) then
            call setECMessage("INFO: ec_filereader_read::ecSpiderwebReadBlock: Reached end of file: "//trim(fileReaderPtr%fileName))
            return
         end if
         ! Read and convert the timesteps to seconds.
         if (.not. ecGetTimesteps(rec, time_steps, .false.)) then
            success = .false.
            return
         endif
         !
         ! ===== T0 =====
         if (t0t1 == 0) then
            item1%sourceT0FieldPtr%timesteps = ecSupportThisTimeToMJD(fileReaderPtr%tframe, time_steps)
            item2%sourceT0FieldPtr%timesteps = ecSupportThisTimeToMJD(fileReaderPtr%tframe, time_steps)
            item3%sourceT0FieldPtr%timesteps = ecSupportThisTimeToMJD(fileReaderPtr%tframe, time_steps)
            rec = ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'x_spw_eye', .false.)
            if (len_trim(rec) == 0) then
               call setECMessage("ERROR: ec_filereader_read::ecSpiderwebReadBlock: Failed to find keyword in file: "//trim(fileReaderPtr%fileName), "x_spw_eye")
               success = .false.
               return
            end if

            read(rec, *, IOSTAT = istat) x_spw_eye
            if(istat /= 0) then
               call setECMessage("ERROR: ec_filereader_read::ecSpiderwebReadBlock: Failed to read keyword in file: "//trim(fileReaderPtr%fileName), "x_spw_eye")
               call setECMessage("     line = "//trim(rec))
               success = .false.
               return
            end if
            item1%sourceT0FieldPtr%x_spw_eye = x_spw_eye
            item2%sourceT0FieldPtr%x_spw_eye = x_spw_eye
            item3%sourceT0FieldPtr%x_spw_eye = x_spw_eye

            rec = ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'y_spw_eye', .false.)
            if (len_trim(rec) == 0) then
               call setECMessage("ERROR: ec_filereader_read::ecSpiderwebReadBlock: Failed to find keyword in file: "//trim(fileReaderPtr%fileName), "y_spw_eye")
               success = .false.
               return
            end if

            read(rec, *, IOSTAT = istat) y_spw_eye
            if(istat /= 0) then
               call setECMessage("ERROR: ec_filereader_read::ecSpiderwebReadBlock: Failed to read keyword in file: "//trim(fileReaderPtr%fileName), "y_spw_eye")
               call setECMessage("     line = "//trim(rec))
               success = .false.
               return
            end if
            item1%sourceT0FieldPtr%y_spw_eye = y_spw_eye
            item2%sourceT0FieldPtr%y_spw_eye = y_spw_eye
            item3%sourceT0FieldPtr%y_spw_eye = y_spw_eye

            rec = ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'p_drop_spw_eye', .false.)
            if (len_trim(rec) == 0) then
               call setECMessage("ERROR: ec_filereader_read::ecSpiderwebReadBlock: Failed to find keyword in file: "//trim(fileReaderPtr%fileName), "p_drop_spw_eye")
               success = .false.
               return
            end if

            read(rec, *, IOSTAT = istat) p_drop_spw_eye
            if(istat /= 0) then
               call setECMessage("ERROR: ec_filereader_read::ecSpiderwebReadBlock: Failed to read keyword in file: "//trim(fileReaderPtr%fileName), "p_drop_spw_eye")
               call setECMessage("     line = "//trim(rec))
               success = .false.
               return
            end if

            do i=1, n_rows-1
               read(fileReaderPtr%fileHandle, *, IOSTAT = istat) (item1%sourceT0FieldPtr%arr1dPtr(i*n_cols+j), j=1, n_cols-1)
               if(istat /= 0) then
                  call setECMessage("ec_filereader_read::ecSpiderwebReadBlock: Read failure before end of file: "//trim(fileReaderPtr%fileName))
                  call setECMessage("     line = "//trim(rec))
                  success = .false.
                  return
               end if
            end do
            do i=1, n_rows-1
               read(fileReaderPtr%fileHandle, *, IOSTAT = istat) (item2%sourceT0FieldPtr%arr1dPtr(i*n_cols+j), j=1, n_cols-1)
               if(istat /= 0) then
                  call setECMessage("ec_filereader_read::ecSpiderwebReadBlock: Read failure before end of file: "//trim(fileReaderPtr%fileName))
                  call setECMessage("     line = "//trim(rec))
                  success = .false.
                  return
               end if
            end do
            do i=1, n_rows-1
               read(fileReaderPtr%fileHandle, *, IOSTAT = istat) (item3%sourceT0FieldPtr%arr1dPtr(i*n_cols+j), j=1, n_cols-1)
               if(istat /= 0) then
                  call setECMessage("ec_filereader_read::ecSpiderwebReadBlock: Read failure before end of file: "//trim(fileReaderPtr%fileName))
                  call setECMessage("     line = "//trim(rec))
                  success = .false.
                  return
               end if
            end do
            ! Fill the central point. (the first row)
            item1%sourceT0FieldPtr%arr1dPtr(1:n_cols) = 0.0_hp
            item2%sourceT0FieldPtr%arr1dPtr(1:n_cols) = item2%sourceT0FieldPtr%arr1dPtr(n_cols+1:n_cols+n_cols)
            item3%sourceT0FieldPtr%arr1dPtr(1:n_cols) = p_drop_spw_eye
            ! Fill 360 degrees with values of 0 degrees. (copy first column into last column)
            do i=1, n_rows
               item1%sourceT0FieldPtr%arr1dPtr(i*n_cols) = item1%sourceT0FieldPtr%arr1dPtr(1+(i-1)*n_cols)
               item2%sourceT0FieldPtr%arr1dPtr(i*n_cols) = item2%sourceT0FieldPtr%arr1dPtr(1+(i-1)*n_cols)
               item3%sourceT0FieldPtr%arr1dPtr(i*n_cols) = item3%sourceT0FieldPtr%arr1dPtr(1+(i-1)*n_cols)
            end do
            ! Compensate for unit of pressure (mbar (= hpa) versus Pa)
            if ((index(item3%quantityPtr%units,'mbar') == 1) .or. (index(item3%quantityPtr%units,'hPa') == 1)) then
               do i=1, size(item3%sourceT0FieldPtr%arr1dPtr)
                  item3%sourceT0FieldPtr%arr1dPtr(i) = item3%sourceT0FieldPtr%arr1dPtr(i)*100.0_hp
               end do
            end if
         ! ===== T1 =====
         else if(t0t1 == 1) then
            item1%sourceT1FieldPtr%timesteps = ecSupportThisTimeToMJD(fileReaderPtr%tframe, time_steps)
            item2%sourceT1FieldPtr%timesteps = ecSupportThisTimeToMJD(fileReaderPtr%tframe, time_steps)
            item3%sourceT1FieldPtr%timesteps = ecSupportThisTimeToMJD(fileReaderPtr%tframe, time_steps)
            rec = ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'x_spw_eye', .false.)
            if (len_trim(rec) == 0) then
               call setECMessage("ERROR: ec_filereader_read::ecSpiderwebReadBlock: Failed to find keyword in file: "//trim(fileReaderPtr%fileName), "x_spw_eye")
               success = .false.
               return
            end if

            read(rec, *, IOSTAT = istat) x_spw_eye
            if(istat /= 0) then
               call setECMessage("ERROR: ec_filereader_read::ecSpiderwebReadBlock: Failed to read keyword in file: "//trim(fileReaderPtr%fileName), "x_spw_eye")
               call setECMessage("     line = "//trim(rec))
               success = .false.
               return
            end if

            item1%sourceT1FieldPtr%x_spw_eye = x_spw_eye
            item2%sourceT1FieldPtr%x_spw_eye = x_spw_eye
            item3%sourceT1FieldPtr%x_spw_eye = x_spw_eye
            rec = ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'y_spw_eye', .false.)
            if (len_trim(rec) == 0) then
               call setECMessage("ERROR: ec_filereader_read::ecSpiderwebReadBlock: Failed to find keyword in file: "//trim(fileReaderPtr%fileName), "y_spw_eye")
               success = .false.
               return
            end if

            read(rec, *, IOSTAT = istat) y_spw_eye
            if(istat /= 0) then
               call setECMessage("ERROR: ec_filereader_read::ecSpiderwebReadBlock: Failed to read keyword in file: "//trim(fileReaderPtr%fileName), "y_spw_eye")
               call setECMessage("     line = "//trim(rec))
               success = .false.
               return
            end if

            item1%sourceT1FieldPtr%y_spw_eye = y_spw_eye
            item2%sourceT1FieldPtr%y_spw_eye = y_spw_eye
            item3%sourceT1FieldPtr%y_spw_eye = y_spw_eye

            rec = ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'p_drop_spw_eye', .false.)
            if (len_trim(rec) == 0) then
               call setECMessage("ERROR: ec_filereader_read::ecSpiderwebReadBlock: Failed to find keyword in file: "//trim(fileReaderPtr%fileName), "p_drop_spw_eye")
               success = .false.
               return
            end if

            read(rec, *, IOSTAT = istat) p_drop_spw_eye
            if(istat /= 0) then
               call setECMessage("ERROR: ec_filereader_read::ecSpiderwebReadBlock: Failed to read keyword in file: "//trim(fileReaderPtr%fileName), "p_drop_spw_eye")
               call setECMessage("     line = "//trim(rec))
               success = .false.
               return
            end if

            do i=1, n_rows-1
               read(fileReaderPtr%fileHandle, *, IOSTAT = istat) (item1%sourceT1FieldPtr%arr1dPtr(i*n_cols+j), j=1, n_cols-1)
            if(istat /= 0) then
               call setECMessage("ec_filereader_read::ecSpiderwebReadBlock: Read failure before end of file: "//trim(fileReaderPtr%fileName))
               call setECMessage("     line = "//trim(rec))
               success = .false.
               return
            end if
            end do
            do i=1, n_rows-1
               read(fileReaderPtr%fileHandle, *, IOSTAT = istat) (item2%sourceT1FieldPtr%arr1dPtr(i*n_cols+j), j=1, n_cols-1)
               if(istat /= 0) then
                  call setECMessage("ec_filereader_read::ecSpiderwebReadBlock: Read failure before end of file: "//trim(fileReaderPtr%fileName))
                  call setECMessage("     line = "//trim(rec))
                  success = .false.
                  return
               end if
            end do
            do i=1, n_rows-1
               read(fileReaderPtr%fileHandle, *, IOSTAT = istat) (item3%sourceT1FieldPtr%arr1dPtr(i*n_cols+j), j=1, n_cols-1)
               if(istat /= 0) then
                  call setECMessage("ec_filereader_read::ecSpiderwebReadBlock: Read failure before end of file: "//trim(fileReaderPtr%fileName))
                  call setECMessage("     line = "//trim(rec))
                  success = .false.
                  return
               end if
            end do
            ! Fill the central point.
            item1%sourceT1FieldPtr%arr1dPtr(1:n_cols) = 0.0_hp
            item2%sourceT1FieldPtr%arr1dPtr(1:n_cols) = item2%sourceT1FieldPtr%arr1dPtr(n_cols+1:n_cols+n_cols)
            item3%sourceT1FieldPtr%arr1dPtr(1:n_cols) = p_drop_spw_eye
            ! Fill 360 degrees with values of 0 degrees.
            do i=1, n_rows
               item1%sourceT1FieldPtr%arr1dPtr(i*n_cols) = item1%sourceT1FieldPtr%arr1dPtr(1+(i-1)*n_cols)
               item2%sourceT1FieldPtr%arr1dPtr(i*n_cols) = item2%sourceT1FieldPtr%arr1dPtr(1+(i-1)*n_cols)
               item3%sourceT1FieldPtr%arr1dPtr(i*n_cols) = item3%sourceT1FieldPtr%arr1dPtr(1+(i-1)*n_cols)
            end do
            ! Compensate for unit of pressure (mbar, hPa versus Pa)
            if ((index(item3%quantityPtr%units,'mbar') == 1) .or. (index(item3%quantityPtr%units,'hPa') == 1)) then
               do i=1, size(item3%sourceT1FieldPtr%arr1dPtr)
                  item3%sourceT1FieldPtr%arr1dPtr(i) = item3%sourceT1FieldPtr%arr1dPtr(i)*100.0_hp
               end do
            end if
         else
            call setECMessage("ERROR: ec_filereader_read::ecSpiderwebReadBlock: Invalid Field specified.")
            success = .false.
         end if
      end function ecSpiderwebReadBlock

      ! =======================================================================
      !> Given the time, find the index of the time dimension in a netCDF filereader

      function ecNetcdfGetTimeIndexByTime(fileReaderPtr, time_mjd) result(ndx)
         integer                      :: ndx           !< read into Field T0 or T1 (0,1).
         type(tEcFileReader), pointer :: fileReaderPtr !< intent(in)
         real(hp), intent(in)         :: time_mjd
         ndx = ecSupportMJDToTimeIndex(fileReaderPtr%tframe, time_mjd)
      end function ecNetcdfGetTimeIndexByTime

      ! =======================================================================


      !> Read the next record from a NetCDF file.
      function ecNetcdfReadNextBlock(fileReaderPtr, item, t0t1, timesndx) result(success)
         use netcdf
         !
         logical                      :: success       !< function status
         type(tEcFileReader), pointer :: fileReaderPtr !< intent(in)
         type(tEcItem), intent(in)    :: item          !< Item containing quantity1, intent(inout)
         integer,       intent(in)    :: t0t1          !< read into Field T0 or T1 (0,1).
         integer,       intent(inout) :: timesndx      !< index of the time dimension to jump to in the netCDF file
         !
         type(tEcField), pointer                 :: fieldPtr         !< Field to update
         integer                                 :: ierror           !< return status of NetCDF method call
         integer                                 :: varid            !< NetCDF id of NetCDF variable
         integer                                 :: i, j, k          !< loop counters
         real(hp), dimension(:,:), allocatable   :: data_block       !< 2D slice of NetCDF variable's data
         integer                                 :: istat            !< allocation status
         real(hp)                                :: dmiss_nc         !< local netcdf missing

         real(hp)                                :: mintime, maxtime !< range of kernel times that can be requested from this netcdf reader
         logical                                 :: valid_field
         character(len=20)                       :: cnumber1         !< number converted to string for error message
         character(len=20)                       :: cnumber2         !< idem
         integer                                 :: ncol, col0, col1 !< bounding box and bounding box extent use to restrict reading a patch from a meteo-field from netCDF
         integer                                 :: nrow, row0, row1
         integer                                 :: nlay
         integer                                 :: Ndatasize

         integer                                 :: issparse   ! data in CRS format (1) or not (0)
         integer, dimension(:), pointer          :: ia         ! CRS sparsity pattern, startpointers
         integer, dimension(:), pointer          :: ja         ! CRS sparsity pattern, column numbers

         integer                                 :: n_cols, n_rows
         logical                                 :: has_time       ! flag if we (should) have a time axis
         logical                                 :: has_harmonics  ! flag if we have harmonics
         !
         success = .false.
         fieldPtr => null()
         has_time = (fileReaderPtr%tframe%nr_timesteps > 0)
         has_harmonics = associated(fileReaderPtr%hframe)

         dmiss_nc = item%quantityPtr%fillvalue
         varid = item%quantityPtr%ncid
         !
         !
         ! =============
         ! sanity checks
         ! =============
         !
         ! - Source T0 or T1 Field specified
         if (t0t1 == 0) then
            fieldPtr => item%sourceT0FieldPtr
         else if (t0t1 == 1) then
            fieldPtr => item%sourceT1FieldPtr
         else
            call setECMessage("Invalid source Field specified in ecNetcdfReadNextBlock.")
            return
         end if
         !
         ! - Check for the presence of times, indicating the presence of further data blocks.
         if (.not. has_time .and. .not. has_harmonics) then
            call setECMessage("Empty NetCDF time dimension and no harmonic components in "//trim(fileReaderPtr%filename)//".")
            return
         end if
         !
         ! - In case of harmonics, just always read at the start of the file.
         if (.not. has_time .and. has_harmonics) then
            timesndx = 1
         end if
         !
         ! ===================
         ! update source Field
         ! ===================
         ! - Determine if the timesteps of the field to be updated are still below the last time in the file

         if (has_time .and. timesndx > fileReaderPtr%tframe%nr_timesteps) then
            mintime = ecSupportTimeIndexToMJD(fileReaderPtr%tframe, 1)
            maxtime = ecSupportTimeIndexToMJD(fileReaderPtr%tframe, int(fileReaderPtr%tframe%nr_timesteps))
            call real2string(cnumber1, '(f12.2)', mintime)
            call real2string(cnumber2, '(f12.2)', maxtime)
            call setECMessage('   Valid range: ' // trim(cnumber1) // ' to ' // trim(cnumber2))
            call setECMessage("Data block requested outside valid time window in "//trim(fileReaderPtr%filename)//".")
            if (.true.) then                                      ! TODO : pass if extrapolation (constant value) is allowed here, now always allowed
               fieldPtr%timesteps = huge(fieldPtr%timesteps)      ! set time to infinity
               fieldPtr%timesndx = timesndx
            else
               return
            endif
         else
            col0 = fieldPtr%bbox(1)
            row0 = fieldPtr%bbox(2)
            col1 = fieldPtr%bbox(3)
            row1 = fieldPtr%bbox(4)
            nrow = row1 - row0 + 1
            ncol = col1 - col0 + 1
            nlay = item%elementSetPtr%n_layers

            Ndatasize = ncol * nrow

            n_cols = item%elementSetPtr%n_cols
            n_rows = item%elementSetPtr%n_rows
            issparse = fieldPtr%issparse
            if (issparse == 1) then
               ia => fieldPtr%ia
               ja => fieldPtr%ja
               Ndatasize = ia(n_rows+1)-1
            end if

            ! - Create storage for the field data if still unallocated and set to missing value
            if (.not.allocated(fieldPtr%arr1d)) then
               allocate(fieldPtr%arr1d(Ndatasize*max(nlay,1)), stat = istat)
               if (istat /= 0) then
                  call setECMessage("ERROR: ec_field::ecFieldCreate1dArray: Unable to allocate additional memory.")
                  write(message,'(a,i0,a,i0,a,i0,a,i0,a)') 'Failed to create storage for item ',item%id,': (',ncol,'x',nrow,'x',nlay,').'
                  call setECMessage(trim(message))
                  return
               else
                  fieldPtr%arr1d = ec_undef_hp
                  fieldPtr%arr1dPtr => fieldPtr%arr1d
               end if
            end if

            valid_field = (col1 == 0 .and. row1 == 0)
            do while (.not.valid_field)
               ! - Read a scalar data block.
               if (item%elementSetPtr%nCoordinates == 0) then
                  ierror = nf90_get_var(fileReaderPtr%fileHandle, varid, fieldPtr%arr1dPtr, start=(/timesndx/), count=(/1/))
                  if (ierror.ne.NF90_NOERR) then
                     call setECMessage("NetCDF:'"//trim(nf90_strerror(ierror))//"' in "//trim(fileReaderPtr%filename)//".")
                     return
                  end if
                  valid_field = (fieldPtr%arr1dPtr(1)/=dmiss_nc)
               end if      ! reading scalar data block
               !
               ! - Read a grid data block.
               valid_field = .false.
               !
               if (issparse /= 1) then
                  allocate(data_block(ncol, nrow), stat = istat)
                  if (istat /= 0) then
                     write(message,'(a,i0,a,i0,a)') 'Allocating temporary array of ',ncol,' x ',nrow,' elements.'
                     call setECMessage(trim(message))
                     call setECMessage("Allocation of data_block (data from NetCDF) failed.")
                     return
                  end if
               end if

               if (item%elementSetPtr%nCoordinates > 0) then
                  if ( issparse == 1 ) then
                     call read_data_sparse(fileReaderPtr%fileHandle, varid, n_cols, n_rows, item%elementSetPtr%n_layers, &
                                           timesndx, fileReaderPtr%relndx, ia, ja, Ndatasize, fieldPtr%arr1dPtr, ierror)
                     if (ecSupportNetcdfCheckError(ierror, 'Error reading quantity '//trim(item%quantityptr%name)//' from sparse data. ', fileReaderPtr%filename)) then
                        valid_field = .true.
                     else
                        return
                     endif
                  else
                     if (item%elementSetPtr%n_layers == 0) then
                        if (item%elementSetPtr%ofType == elmSetType_samples) then
                           if (has_harmonics) then
                              ierror = nf90_get_var(fileReaderPtr%fileHandle, varid, data_block, start=(/col0/), count=(/ncol/))
                           else
                              ierror = nf90_get_var(fileReaderPtr%fileHandle, varid, data_block, start=(/col0, timesndx/), count=(/ncol, 1/))
                           end if
                        else
                           if (has_harmonics) then
                              ierror = nf90_get_var(fileReaderPtr%fileHandle, varid, data_block, start=(/col0, row0/), count=(/ncol, nrow/))
                           else
                              ierror = nf90_get_var(fileReaderPtr%fileHandle, varid, data_block, start=(/col0, row0, timesndx/), count=(/ncol, nrow, 1/))
                           end if
                        end if
                        if (ierror /= 0) then
                           if (has_harmonics) then
                              call setECMessage("Error retrieving component data")
                           else
                              call setECMessage("Error retrieving time index ", timesndx)
                           end if
                           return
                        end if
                        ! copy data to source Field's 1D array, store (X1Y1, X1Y2, ..., X1Yn_rows, X2Y1, XYy2, ..., Xn_colsY1, ...)
                        do i=1, nrow
                           do j=1, ncol
                              fieldPtr%arr1dPtr( (i-1)*ncol +  j ) = data_block(j,i)
                           end do
                        end do
                        valid_field = .true.
                     else
                        ! copy data to source Field's 1D array, store (X1Y1, X1Y2, ..., X1Yn_rows, X2Y1, XYy2, ..., Xn_colsY1, ...)
                        do k=1, item%elementSetPtr%n_layers
                           if (has_harmonics) then
                              ierror = nf90_get_var(fileReaderPtr%fileHandle, varid, data_block, start=(/col0, row0, k/), count=(/ncol, nrow, 1/))
                           else
                              ierror = nf90_get_var(fileReaderPtr%fileHandle, varid, data_block, start=(/col0, row0, k, timesndx/), count=(/ncol, nrow, 1, 1/))
                           end if
                           do i=1, nrow
                              do j=1, ncol
                                 fieldPtr%arr1dPtr( (k-1)*ncol*nrow + (i-1)*ncol +  j ) = data_block(j,i)
                                 valid_field = .true.
                              end do
                           end do
                        end do
                     end if
                  end if
                  if (ierror /= 0) then
                     return
                  end if
               end if
               if (has_time .and. .not.valid_field) then
                  timesndx = timesndx+1
               else
                   valid_field = .true.  ! Make sure we exit immediately (in case of harmonics)
               end if
            end do         ! loop while fields invalid

            ! - Update the source Field's timesteps variable if applicable.
            if (has_time) then
               fieldPtr%timesteps = ecSupportTimeIndexToMJD(fileReaderPtr%tframe, timesndx)
               fieldPtr%timesndx = timesndx
            else if (has_harmonics) then
               if (t0t1 == 0) then
                  fieldPtr%timesteps = -ec_huge_hp
               else if (t0t1 == 1) then
                  fieldPtr%timesteps = ec_huge_hp
               end if
            end if
         endif

         ! - Apply the scale factor and offset
         if (item%quantityPtr%factor /= 1.0_hp .or. item%quantityPtr%offset /= 0.0_hp) then
            do i=1, size(fieldPtr%arr1dPtr)
               if ( fieldPtr%arr1dPtr(i) /= dmiss_nc ) then
                  fieldPtr%arr1dPtr(i) = fieldPtr%arr1dPtr(i) * item%quantityPtr%factor + item%quantityPtr%offset
               end if
            end do
         end if
         !
         ! - 5 - Deallocate temporary datablock
         if (allocated(data_block)) deallocate(data_block, stat = istat)
         !
         success = .true.
      end function ecNetcdfReadNextBlock

      ! =======================================================================

      !> Read all the data of one variable from a NetCDF file.
      !> This is used to read quantity amplitude data in case of harmonic data.
      !> Both T0 and T1 are created and filled. T1 holds the amplitude data
      !> while T0 will be later used to hold calculated values each timestep.
      function ecNetcdfReadVariable(fileReaderPtr, item) result(success)
         use netcdf
         !
         logical                      :: success       !< function status
         type(tEcFileReader), pointer :: fileReaderPtr !< intent(in)
         type(tEcItem), intent(in)    :: item          !< Item containing quantity1, intent(inout)
         !
         integer                      :: timesndx
         !
         timesndx = 1 ! Dummy time index.
         success = ecNetcdfReadNextBlock(fileReaderPtr, item, 0, timesndx)
         if (success) then
            success = ecNetcdfReadNextBlock(fileReaderPtr, item, 1, timesndx)
         end if
      end function ecNetcdfReadVariable

      ! =======================================================================

      !> Read the next record from a NetCDF file.
      !! meteo1: readnetcdfblock
      ! TODO: cleanup: lastReadTime, TimeFrame usage, time conversions, remove hardcoded asumption of file content and structure
      function ecNetcdfReadBlock(fileReaderPtr, item1, t0t1, n) result(success)
         use netcdf
         !
         logical                         :: success       !< function status
         type(tEcFileReader), pointer    :: fileReaderPtr !< intent(in)
         type(tEcItem),       pointer    :: item1         !< Item containing quantity1, intent(inout)
         integer                         :: t0t1          !< read into Field T0 or T1 (0,1). -1: choose yourself and return where you put it.
         integer,             intent(in) :: n             !< dimension of quantity to read
         !
         integer                             :: i             !< loop counter
         integer                             :: ierror        !< return value of function calls
         integer                             :: iddim_time    !< id as obtained from NetCDF
         integer                             :: idvar_time    !< id as obtained from NetCDF
         integer                             :: idvar_q       !< id as obtained from NetCDF
         integer                             :: ntimes        !< number of times on the NetCDF file
         integer                             :: read_index    !< index of field to read
         real(hp)                            :: tim           !< instantanious time
         real(hp), dimension(:), allocatable :: times         !< time array read from NetCDF
         character(NF90_MAX_NAME)            :: string        !< to catch NetCDF messages
         !
         success = .false.
         !
         ierror = nf90_sync(fileReaderPtr%fileHandle)
         ierror = nf90_inq_dimid(fileReaderPtr%fileHandle, 'time', iddim_time); success = ecSupportNetcdfCheckError(ierror, "inq_dimid time"    , fileReaderPtr%fileName)
         ierror = nf90_inquire_dimension(fileReaderPtr%fileHandle, iddim_time, string, ntimes); success = ecSupportNetcdfCheckError(ierror, "inq_dim time", fileReaderPtr%fileName)
         ierror = nf90_inq_varid(fileReaderPtr%fileHandle, 'time', idvar_time); success = ecSupportNetcdfCheckError(ierror, "inq_varid time", fileReaderPtr%fileName)
         !
         !
         ! varid: First compare name with standard_names
         idvar_q = -1
         do i=1,size(fileReaderPtr%standard_names)
            if (fileReaderPtr%standard_names(i)==item1%quantityPtr%name) then
               idvar_q = i
               exit
            endif
         enddo
         !
         ! varid not found: compare name with variable_names
         if (idvar_q == -1) then
            do i=1,size(fileReaderPtr%variable_names)
               if (fileReaderPtr%variable_names(i)==item1%quantityPtr%name) then
                  idvar_q = i
                  exit
               endif
            enddo
         endif
         !
         ! varid not found: get it via nf90_inq_varid
         if (idvar_q == -1) then
            ierror = nf90_inq_varid(fileReaderPtr%fileHandle, item1%quantityPtr%name, idvar_q); success = ecSupportNetcdfCheckError(ierror, "inq_varid "//item1%quantityPtr%name, fileReaderPtr%fileName)
         endif
         !
         ! TODO: replace times by filereaderPtr%tframe%times
         allocate (times(ntimes), stat=ierror)
         if (ierror /= 0) then
            call setECMessage("Allocation error in ec_filereader_read::ecNetcdfReadBlock.")
            return
         endif
         ! Parse refdate and tunit to reconstruct mjd from netcdf timestep vector
         string = '' ! NetCDF does not completely overwrite a string, so re-initialize.
         if (.not. ecSupportNetcdfCheckError(nf90_get_att(fileReaderPtr%fileHandle, idvar_time, "units", string), "obtain units", fileReaderPtr%fileName)) return
         if (.not. ecSupportTimestringToUnitAndRefdate(string, fileReaderPtr%tframe%ec_timestep_unit, fileReaderPtr%tframe%ec_refdate, &
                                                              tzone = fileReaderPtr%tframe%ec_timezone)) return
         ierror = nf90_get_var(fileReaderPtr%fileHandle, idvar_time, times, start=(/1/), count=(/ntimes/)); success = ecSupportNetcdfCheckError(ierror, "get_var time", fileReaderPtr%fileName)
         !
         ! Search in times the first time bigger than lastReadTime
         !
         read_index = -1
         if (comparereal(fileReaderPtr%lastReadTime,ec_undef_hp) == 0) then
            !
            ! No data read at all. Force reading for the first time
            read_index = 1
         else
            do i=1, ntimes
               if (comparereal(times(i),fileReaderPtr%lastReadTime) == 1) then
                  read_index = i
                  exit
               endif
            enddo
         endif
         if (read_index > 0) then
            if (t0t1 < 0) then
               if (comparereal(item1%sourceT0FieldPtr%timesteps,0.0_hp) == -1) then
                  t0t1 = 0
               elseif (comparereal(item1%sourceT1FieldPtr%timesteps,0.0_hp) == -1) then
                  t0t1 = 1
               elseif (comparereal(item1%sourceT0FieldPtr%timesteps,item1%sourceT1FieldPtr%timesteps) /= 1) then
                  t0t1 = 0
               else
                  t0t1 = 1
               endif
            endif
            !
            ! T0
            if (t0t1==0) then   ! JRE: needed to be changed to mjd because of use in ecItemUpdateSourceItem
               tim=fileReaderPtr%tframe%ec_refdate + times(read_index) * ecSupportTimeUnitConversionFactor(fileReaderPtr%tframe%ec_timestep_unit) / 86400.0_hp - fileReaderPtr%tframe%ec_timezone / 24.0_hp
               item1%sourceT0FieldPtr%timesteps = tim
               ierror = nf90_get_var(fileReaderPtr%fileHandle, idvar_q, item1%sourceT0FieldPtr%arr1dPtr, start=(/ 1, read_index /), count = (/ n, 1 /))
               success = ecSupportNetcdfCheckError(ierror, "get_var "//item1%quantityPtr%name, fileReaderPtr%fileName)
            ! ===== T1 =====
            else if(t0t1==1) then
               tim=fileReaderPtr%tframe%ec_refdate + times(read_index) * ecSupportTimeUnitConversionFactor(fileReaderPtr%tframe%ec_timestep_unit) / 86400.0_hp - fileReaderPtr%tframe%ec_timezone / 24.0_hp
               item1%sourceT1FieldPtr%timesteps = tim
               ierror = nf90_get_var(fileReaderPtr%fileHandle, idvar_q, item1%sourceT1FieldPtr%arr1dPtr, start=(/ 1, read_index /), count = (/ n, 1 /))
               success = ecSupportNetcdfCheckError(ierror, "get_var "//item1%quantityPtr%name, fileReaderPtr%fileName)
            else
               call setECMessage("ecNetcdfReadBlock: Invalid Field specified.")
            endif
            fileReaderPtr%lastReadTime = times(read_index)
         else
            success = .false.
         endif
         deallocate (times, stat=ierror)
      end function ecNetcdfReadBlock

      ! =======================================================================

      !> Read the Qhtable file.
      function ecQhtableReadAll(fileReaderPtr, discharges, waterlevels, nr_rows) result(success)
         logical                                                     :: success       !< function status
         type(tEcFileReader),                            pointer     :: fileReaderPtr !< intent(in)
         real(hp),            dimension(:), allocatable, intent(out) :: discharges    !<
         real(hp),            dimension(:), allocatable, intent(out) :: waterlevels   !<
         integer,                                        intent(out) :: nr_rows       !<
         !
         integer        :: istat !< status of operation
         character(:), allocatable :: rec   !< content of a line
         logical        :: eof   !< end-of_file mark
         !
         success = .true.
         nr_rows = 0
         !
         allocate(discharges(10), waterlevels(10), STAT = istat)
         if (istat /= 0) then
            call setECMessage("ERROR: ec_filereader_read::ecQhtableReadAll: Unable to allocate additional memory.")
            return
         end if
         !
         if (fileReaderPtr%ofType == provFile_qhtable) then
            rewind(unit=fileReaderPtr%fileHandle)
         endif
         !
         do
            if (fileReaderPtr%ofType == provFile_bc) then
               if (.not.ecBCReadLine(fileReaderPtr, recout=rec, eof=eof)) then
                  ! TODO (RL): insert real message handling/reporting here (deltarescommon message)
                  if (eof) then           ! legitimate way to exit, data simply ended
                     istat = 0
                  else                    ! reading failed but not eof! something wrong
                     istat = -666
                     success = .false.
                  endif
                  return
               endif
            endif
            if (fileReaderPtr%ofType == provFile_qhtable) then
               call GetLine(fileReaderPtr%fileHandle, rec, istat)
            endif
            if (istat == 0) then
               if (.not. (rec(1:1) == '*' .or. len_trim(rec) == 0)) then
                  if (nr_rows == size(discharges)) then
                     call realloc(discharges, nr_rows + 10, STAT = istat, keepExisting = .true.)
                     if (istat /= 0) then
                        call setECMessage("ERROR: ec_filereader_read::ecQhtableReadAll: Unable to allocate additional memory.")
                        success = .false.
                        exit
                     end if
                     call realloc(waterlevels, nr_rows + 10, STAT = istat, keepExisting = .true.)
                     if (istat /= 0) then
                        call setECMessage("ERROR: ec_filereader_read::ecQhtableReadAll: Unable to allocate additional memory.")
                        success = .false.
                        exit
                     end if
                  end if
                  nr_rows = nr_rows + 1
                  read (rec, *, iostat = istat) discharges(nr_rows), waterlevels(nr_rows)
                  if (istat /= 0) then
                      success = .false.
                      call setECMessage("ERROR: ec_filereader_read::ecQhtableReadAll: Cannot find two numbers in line: "//trim(rec)//" in file: "//	trim(fileReaderPtr%FILENAME))
                      exit
                  end if
               end if
            else
               exit
            end if
         end do
      end function ecQhtableReadAll

      ! =======================================================================

      !> Read the Fourier file, transforming components into periods.
      !! meteo1: readfourierdims, readfouriercompstim
      function ecFourierReadAll(fileReaderPtr, periods, components, magnitudes, phases, nPeriods) result(success)
         logical                                                     :: success       !< function status
         type(tEcFileReader),                            pointer     :: fileReaderPtr !< intent(in)
         real(hp),            dimension(:), allocatable, intent(out) :: periods       !< periods in minutes, directly or converted from components
         character(len=8),    dimension(:), allocatable, intent(out) :: components    !< astro component names
         real(hp),            dimension(:), allocatable, intent(out) :: magnitudes    !< seed values for the magnitudes of the Fourier components
         real(hp),            dimension(:), allocatable, intent(out) :: phases        !< seed values for the phases of the Fourier components (in deg, output in rad)
         integer,                                        intent(out) :: nPeriods      !< number of periods

         !
         integer                   :: istat     !< status of allocation operation
         character(:), allocatable :: rec       !< content of a line
         integer                   :: i1        !< start index of first word
         integer                   :: i2        !< stop index of first word
         character(len=maxNameLen) :: component !< helper variable, when converting from component to period
         logical                   :: eof       !< true if the end of file was reached
         logical                   :: is_astro  !< true if an astronomical component has been parsed

         integer                   :: MAXCMP = 100
         !
         success = .true.
         nPeriods = 0
         !
         allocate(periods(10), components(10), magnitudes(10), phases(10), STAT = istat)
         if (istat /= 0) then
            call setECMessage("ERROR: ec_filereader_read::ecFourierReadAll: Unable to allocate additional memory.")
            return
         end if

         if (fileReaderPtr%ofType == provFile_fourier) then
            rewind(unit=fileReaderPtr%fileHandle)
         endif
         !

         if (fileReaderPtr%ofType == provFile_bc) then
            if (allocated(fileReaderPtr%bc%quantity%astro_component)) deallocate (fileReaderPtr%bc%quantity%astro_component)
            if (allocated(fileReaderPtr%bc%quantity%astro_amplitude)) deallocate (fileReaderPtr%bc%quantity%astro_amplitude)
            if (allocated(fileReaderPtr%bc%quantity%astro_phase)) deallocate (fileReaderPtr%bc%quantity%astro_phase)
            allocate (fileReaderPtr%bc%quantity%astro_component(MAXCMP))
            allocate (fileReaderPtr%bc%quantity%astro_amplitude(MAXCMP))
            allocate (fileReaderPtr%bc%quantity%astro_phase(MAXCMP))
         endif

         is_astro=.false.
         do
            if (fileReaderPtr%ofType == provFile_bc) then
               if (.not.ecBCReadLine(fileReaderPtr, recout=rec, eof=eof)) then
                  ! TODO (RL): insert real message handling/reporting here (deltarescommon message)
                  istat = -666
                  success = eof        ! if reading failed, allow only if eof
                  exit
               else
                  istat = 0
               endif
            endif
            if (fileReaderPtr%ofType == provFile_fourier) then
               call GetLine(fileReaderPtr%fileHandle, rec, istat)
            endif
            if (istat == 0) then
               if (.not. (rec(1:1) == '*' .or. len_trim(rec) == 0)) then
                  if (nPeriods == size(periods)) then
                     call realloc(periods, nPeriods + 10, STAT = istat, keepExisting = .true.)
                     if (istat == 0) call realloc(components, nPeriods + 10, STAT = istat, keepExisting = .true.)
                     if (istat == 0) call realloc(magnitudes, nPeriods + 10, STAT = istat, keepExisting = .true.)
                     if (istat == 0) call realloc(phases, nPeriods + 10, STAT = istat, keepExisting = .true.)
                     if (istat /= 0) then
                        call setECMessage("ERROR: ec_filereader_read::ecFourierReadAll: Unable to allocate additional memory.")
                        success = .false.
                        return
                     end if
                  end if
                  nPeriods = nPeriods + 1
                  call remove_leading_spaces(rec) ! Prevents scientific notation number to be identified as a component name.
                  call find_first_word(rec, i1, i2)
                  if ((i1 == 0 .and. i2 == 0) .or. (i1 > 3)) then
                     ! period found
                     read (rec, *, IOSTAT = istat) periods(nPeriods), magnitudes(nPeriods), phases(nPeriods)
                     if(istat /= 0) then
                        call setECMessage("ec_filereader_read::ecUniReadBlock: Read failure before end of file: "//trim(fileReaderPtr%fileName))
                        call setECMessage("     line = "//trim(rec))
                        success = .false.
                        return
                     end if
                     !
                     if(is_astro) then
                        call setECMessage("ERROR: mixed astro-components/harmonic components encountered.")
                        success = .false.
                        return
                     end if
                     ! Perform transformations, which are handled by subroutine asc for components.
                     if (.not. (comparereal(periods(nPeriods), 0.0_hp) == 0)) then
                        ! if a bc-structure is associated to the filereader (i.e. we are reading from a BC-file),
                        ! inspect the 'timeunit' in which info was stored on the contents of the 'component'-column
                        if (associated(fileReaderPtr%bc)) then
                           select case (fileReaderPtr%bc%timeunit)
                              case ('SECOND')
                                 periods(nPeriods) = 2.0_hp * pi_hp / (periods(nPeriods)/60.0)
                              case ('MINUTE')
                                 periods(nPeriods) = 2.0_hp * pi_hp / (periods(nPeriods))
                              case ('HOUR')
                                 periods(nPeriods) = 2.0_hp * pi_hp / (periods(nPeriods)*60.0)
                              case ('PERSECOND')
                                 periods(nPeriods) = 2.0_hp * pi_hp * (periods(nPeriods)/60.0)
                              case ('PERMINUTE')
                                 periods(nPeriods) = 2.0_hp * pi_hp * (periods(nPeriods))
                              case ('PERHOUR')
                                 periods(nPeriods) = 2.0_hp * pi_hp * (periods(nPeriods)*60.0)
                              case ('RADPERSECOND')
                                 periods(nPeriods) = periods(nPeriods)/60.0
                              case ('RADPERMINUTE')
                                 periods(nPeriods) = periods(nPeriods)
                              case ('RADPERHOUR')
                                 periods(nPeriods) = periods(nPeriods)*60.0
                              case default                                       ! old setting
                                 periods(nPeriods) = 2.0_hp * pi_hp / periods(nPeriods)
                           end select
                        else
                           periods(nPeriods) = 2.0_hp * pi_hp / periods(nPeriods)
                        end if
                     end if
                     phases(nPeriods) = phases(nPeriods)*degrad_hp
                     is_astro = .false.
                  else
                     ! component found
                     read (rec, *, IOSTAT = istat) component, magnitudes(nPeriods), phases(nPeriods)
                     if(istat /= 0) then
                        call setECMessage("ec_filereader_read::ecUniReadBlock: Read failure before end of file: "//trim(fileReaderPtr%fileName))
                        call setECMessage("     line = "//trim(rec))
                        success = .false.
                        return
                     end if
                     !
                     components(nPeriods) = trim(component)
                     phases(nPeriods) = phases(nPeriods)*degrad_hp
                     ! store the original component parameters, read from file, into a bc%quantity
                     if (fileReaderPtr%ofType == provFile_bc) then
                        fileReaderPtr%bc%quantity%astro_component(nPeriods) = components(nPeriods)
                        fileReaderPtr%bc%quantity%astro_amplitude(nPeriods) = magnitudes(nPeriods)
                        fileReaderPtr%bc%quantity%astro_phase(nPeriods) = phases(nPeriods)
                     endif
                     is_astro = .true.
                  end if
               end if
            else
               exit
            end if
         end do

         ! truncate the period amplitude and phase arrays to the actual sizes (processed components)
         if(is_astro) then
            deallocate(periods)
            call realloc(components, nPeriods, STAT = istat, keepExisting = .true.)
         else
            deallocate(components)
            call realloc(periods, nPeriods, STAT = istat, keepExisting = .true.)
         end if
         if (istat == 0) call realloc(magnitudes, nPeriods, STAT = istat, keepExisting = .true.)
         if (istat == 0) call realloc(phases, nPeriods, STAT = istat, keepExisting = .true.)
         if (istat /= 0) then
            call setECMessage("ERROR: ec_filereader_read::ecFourierReadAll: Unable to allocate actual memory (components).")
            success = .false.
            return
         end if

         ! truncate the period amplitude and phase arrays to the actual sizes (original components in bc-instance)
         if (fileReaderPtr%ofType == provFile_bc) then
            call realloc(fileReaderPtr%bc%quantity%astro_component, nPeriods, STAT = istat, keepExisting = .true.)
            call realloc(fileReaderPtr%bc%quantity%astro_amplitude, nPeriods, STAT = istat, keepExisting = .true.)
            call realloc(fileReaderPtr%bc%quantity%astro_phase, nPeriods, STAT = istat, keepExisting = .true.)
            if (istat /= 0) then
               call setECMessage("ERROR: ec_filereader_read::ecFourierReadAll: Unable to allocate actual memory (original components).")
               success = .false.
               return
            endif
         endif

      end function ecFourierReadAll

      ! =======================================================================

      !> Read the file from the current line untill a line containing the keyword is found and read.
      !! meteo1.f90: reaspwheader
      function ecFindInFile(minp, keyword) result(rec)
         character(maxFileNameLen)                 :: rec
         integer                      , intent(in) :: minp    !< IO unit number
         character(*)                 , intent(in) :: keyword !< keyword to find
         !
         ! locals
         integer                                   :: istat !< status of read operation
         character(maxFileNameLen)                     :: rec_small
         character(maxFileNameLen)                     :: keyword_small
         !
         ! body
         rec = ' '
         keyword_small = keyword
         call small(keyword_small, len(keyword_small))
         do
            ! Infinite read loop until keyword found or EOF
            read(minp, '(a)', IOSTAT = istat) rec
            rec_small = rec
            call small(rec_small, len(rec_small))
            if (istat == 0) then
               !if (.not. (rec_small(1:1) == '*' .or. rec_small(1:1) == '#' .or. len_trim(rec_small) == 0)) then
               if (.not. (rec_small(1:1) == '*'                            .or. len_trim(rec_small) == 0)) then
                  if (index(rec_small, trim(keyword_small)) /= 0) then
                     exit ! Jump out of do-loop
                  end if
               end if
            else
               call setECMessage("INFO: ec_filereader_read::ecFindInFile: File end has been reached.")
               rec = ' '
               exit ! Jump out of do-loop
            end if
         enddo
      end function ecFindInFile

      ! =======================================================================

      !> In a spiderweb or curvi file, find the value curresponding to the specified keyword.
      !! meteo1.f90: reaspwheader
      function ecSpiderwebAndCurviFindInFile(minp, keyword, do_rewind) result(answer)
         character(len=maxFileNameLen)                     :: answer
         integer,                   intent(in)             :: minp      !< IO unit number
         character(len=*),          intent(in)             :: keyword   !< keyword to find
         logical, optional,         intent(in)             :: do_rewind !< rewind file before search
         !
         character(len=maxFileNameLen+20)                  :: rec         !< content of read line
         integer                                           :: indx        !< helper index variable
         integer                                           :: indxComment !< position in string of comments
         !
         answer = ' '
         if (present(do_rewind)) then
            if (do_rewind) then
               rewind(unit = minp)
            end if
         else
            rewind(unit = minp)
         end if
         !
         rec = ecFindInFile(minp, keyword)
         indx = index(rec, '=')
         indxComment = index(rec, '#')
         if (indx /= 0) then
            if (indxComment /= 0) then
               answer = adjustl(rec(indx+1:indxComment - 1))
            else
               answer = adjustl(rec(indx+1:))
            endif
         else
            call setECMessage("ERROR: ec_filereader_read::ecSpiderwebAndCurviFindInFile: Failed to read an existing line.")
            answer = ' '
         end if
      end function ecSpiderwebAndCurviFindInFile

      ! =======================================================================

      !> In a t3D file, find the list of values following the specified keyword.
      function ect3DFindInFile(minp, keyword, do_rewind) result(answer)
         character(len=1000)        :: answer
         integer,                   intent(in) :: minp      !< IO unit number
         character(len=*),          intent(in) :: keyword   !< keyword to find
         logical, optional,         intent(in) :: do_rewind !< rewind file before search
         !
         character(len=maxNameLen) :: word
         character(len=maxNameLen) :: rec     !< content of read line
         integer                   :: indx    !< helper index variable
         !
         answer = ''
         word = keyword
         if (present(do_rewind)) then
            if (do_rewind) then
               rewind(unit = minp)
            end if
         else
            rewind(unit = minp)
         end if
         !
         rec = ecFindInFile(minp, word)
         indx = index(rec, '=')
         answer = rec(indx+1:)
      end function ect3DFindInFile

      ! =======================================================================

      !> Rewind and then read past the header of a spiderweb file, putting it is a state suitable for calling ecSpiderwebReadBlock.
      function ecSpiderAndCurviAndArcinfoReadToBody(minp) result(success)
         logical             :: success
         integer, intent(in) :: minp !< IO unit number
         !
         character(len=20) :: answer
         !
         success = .false.
         rewind(unit=minp)
         answer = ecFindInFile(minp, "TIME")
         if (len_trim(answer) > 0) then
            success = .true.
            backspace(minp) ! We wanted to read to the end of the header.
         end if
      end function ecSpiderAndCurviAndArcinfoReadToBody

      !> Reads a sample file (*.xyz) given a tEcFileReader, into allocatable arrays.
      !! \see ecSampleReadAll_from_lun
      function ecSampleReadAll_from_fileReader(fileReaderPtr, xs, ys, zs, nSamples, kx) result(success)
         logical                                                       :: success       !< function status
         type(tEcFileReader),                              pointer     :: fileReaderPtr !< intent(in)
         real(hp),            dimension(:),   allocatable, intent(out) :: xs            !< list of x-coordinates of all samples
         real(hp),            dimension(:),   allocatable, intent(out) :: ys            !< list of y-coordinates of all samples
         real(hp),            dimension(:,:), allocatable, intent(out) :: zs            !< list of z-values of all samples
         integer,                                          intent(out) :: nSamples      !< number of samples
         integer,                                          intent(out) :: kx            !< number of vector components in each sample value (1 for scalars)

         success = ecSampleReadAll_from_lun(fileReaderPtr%fileHandle, fileReaderPtr%filename, xs, ys, zs, nSamples, kx)

      end function ecSampleReadAll_from_fileReader


      !> Reads a sample file (*.xyz) given an already opened logical unit number, into allocatable arrays.
      function ecSampleReadAll_from_lun(msam, filename, xs, ys, zs, nSamples, kx) result(success)
         logical                                                         :: success       !< function status
         integer,                                          intent(inout) :: msam          !< logical unit number (to already opened file)
         character(len=*),                                 intent(in   ) :: filename      !< Name of the file (for messaging only)
         real(hp),            dimension(:),   allocatable, intent(  out) :: xs            !< list of x-coordinates of all samples
         real(hp),            dimension(:),   allocatable, intent(  out) :: ys            !< list of y-coordinates of all samples
         real(hp),            dimension(:,:), allocatable, intent(  out) :: zs            !< list of z-values of all samples
         integer,                                          intent(  out) :: nSamples      !< number of samples
         integer,                                          intent(  out) :: kx            !< number of vector components in each sample value (1 for scalars)

         double precision :: xx, yy, zz
         double precision :: dmiss_dflt = -999d0   ! Use default missing value for this 'old' sample file type
         double precision :: xymis_dflt = -999d0   !
         character(len=:), allocatable :: rec
         character(len=maxMessageLen) :: tex
         integer                      :: istat


         success = .true.

         nSamples = 0

         rewind(msam)
11       read (msam,*, end = 31)
            nSamples = nSamples + 1
         goto 11
31       continue
         kx = 1 ! TODO: AvD: scan and support vector_max > 1
         call realloc(xs,        nSamples,    keepExisting = .false.)
         call realloc(ys,        nSamples,    keepExisting = .false.)
         call realloc(zs, (/ kx, nSamples /), keepExisting = .false.)

         rewind(msam)
! TODO: this reader does not yet have all functionality that reasam() in dflowfm kernel has (comments *, PHAROS filetype, ...)
         nSamples = 0
10       continue
         call GetLine(msam, rec, istat)
         if (istat /= 0) goto 30
         read (rec,*,end = 40, err = 40) xx,yy,zz

         if (  xx .ne. xymis_dflt .and. yy .ne. xymis_dflt .and. &
               zz .ne. dmiss_dflt .and. zz .ne. 999.999d0 .and. &
               .not.(isnan(xx) .or. isnan(yy) .or. isnan(zz)) ) then
            nSamples = nSamples + 1
            xs(nSamples)    = xx
            ys(nSamples)    = yy
            zs(kx,nSamples) = zz
         endif
         goto 10

40       continue
         success = .false.
         write(tex,'(a,a,a,i0,a)') "ERROR: ec_filereader_read::ecSampleReadAll: read error in file '", trim(filename), "' on line ", nSamples+1, "."
         call setECMessage(trim(tex))
         return

30       continue
!        ! TODO: Sample cleaning below not necessary in EC module?
!        write(tex,'(i10)') ns
!        call readyy('sorting '//trim(tex)//' samples points',0d0)
!        if (ns .gt. 1) then
!           call tidysamples(xs,ys,zs,ipsam,ns,mxsam,mysam)
!           call get_samples_boundingbox()
!           ipstat = ipstat_ok
!        end if
         return
      end function ecSampleReadAll_from_lun

!!==============================================================================
!
!function read_spv_block(unitnr, p_conv, xwind, ywind, press, num_rows, num_columns, tread, ipart) result(success)
!   !
!   ! Read block in meteo_on_flow_grid file
!   !
!   ! result
!   logical                    :: success
!   !
!   ! arguments
!   !
!   integer, intent(in)        :: unitnr
!   integer, intent(in)        :: num_rows
!   integer, intent(in)        :: num_columns
!   real(hp), dimension(:,:), intent(out) :: xwind, ywind, press
!   real(hp), intent(in)       :: p_conv
!   real(hp), intent(out)      :: tread
!   integer, intent(in)        :: ipart ! request 'uvp' for all components,
!                                       ! 'u', 'v', or 'p' for one component
!   !
!   ! locals
!   !
!   integer                    :: i
!   integer                    :: ierr
!   integer                    :: j
!   integer                    :: loc_is
!   character(132)             :: rec
!   !
!   ! read line with 'TIME = ... minutes ' or a Headerline
!   !
!   read (unitnr, '(a)') rec
!   if (index(rec, 'HEADER') .gt. 0) then
!      ! skip header-lines
!      do
!        read (unitnr, '(a)', iostat=ierr) rec
!        if (ierr /= 0) then
!           call setECMessage("ERROR: error reading header of meteo-file")
!           success = .false.
!           return
!        endif
!        i = index(rec, 'END OF HEADER')
!        if (i .gt. 0) then
!           !
!           ! extra read so rec is always 'TIME = ... minutes ' after this if-do-if-block
!           !
!           read (unitnr, '(a)') rec
!           exit
!        endif
!      enddo
!   endif
!   !
!   loc_is  = index(rec, '=')
!   if (loc_is .gt. 0) then
!      read (rec(loc_is+1:), *) tread
!   else
!      call setECMessage('Could not find time in meteo-file')
!      success = .false.
!      return
!   endif
!   !
!   ! Loop over the first dimension in flow
!   !
!   if (ipart == 1 .or. ipart == -1) then
!      if ( size(xwind,1) .ne. num_rows .or. size(xwind,2) .ne. num_columns) then
!         call setECMessage('READ_SPV_BLOCK: wrong sizes xwind')
!         success = .false.
!         return
!      endif
!      do j = 1,num_rows
!         read(unitnr,*,end = 100, err=101) ( xwind(j,i), i = 1,num_columns )
!      enddo
!   endif
!   if (ipart == 2 .or. ipart == -1) then
!      if ( size(ywind,1) .ne. num_rows .or. size(ywind,2) .ne. num_columns ) then
!         call setECMessage('READ_SPV_BLOCK: wrong sizes ywind')
!         success = .false.
!         return
!      endif
!      do j = 1,num_rows
!         read(unitnr,*,end = 100, err=102) ( ywind(j,i), i = 1,num_columns )
!      enddo
!   endif
!   if (ipart == 3 .or. ipart == -1) then
!      if ( size(press,1) .ne. num_rows .or. size(press,2) .ne. num_columns ) then
!         call setECMessage('READ_SPV_BLOCK: wrong sizes press')
!         success = .false.
!         return
!      endif
!      do j = 1,num_rows
!         read(unitnr,*,end = 100, err=103) ( press(j,i), i = 1,num_columns )
!      enddo
!      !
!      ! Conversion of pressure to Pa (N/m2). If already Pa, p_conv = 1.0_hp
!      !
!      if (p_conv .ne. 1d0) then
!         press(:,:) = press(:,:) * p_conv
!      endif
!   endif
!   !
!   success = .true.
!   return
!100 continue
!   call setECMessage('Unexpected end of file in meteo_on_flow_grid file')
!   success = .false.
!   return
!101 continue
!   call setECMessage('Error reading wind u-field')
!   success = .false.
!   return
!102 continue
!   call setECMessage('Error reading wind v-field')
!   success = .false.
!   return
!103 continue
!   call setECMessage('Error reading pressure field')
!   success = .false.
!   return
!end function read_spv_block
!!
!function ec_grib_open(unitnr, filename) result(success)
!   !
!   ! wrapper around pbopen
!   !
!   ! result
!   logical                    :: success
!   !
!   ! arguments
!   !
!   integer,      intent(out) :: unitnr
!   character(*), intent(in)  :: filename
!   !
!   ! locals
!   !
!   integer                   :: ierr
!   character(maxFileNameLen) :: filename2
!   !
!   if (index(filename,'?') > 0) then
!      success = expand_wildcard(1, filename, filename2)
!   else
!      filename2 = filename
!   endif
!   !
!#  if defined GRIB
!      call pbopen(unitnr, filename2, 'r', ierr)
!#  else
!      unitnr = -1
!      ierr   = -4
!#  endif
!   select case (ierr)
!   case (0)
!      success = .true.
!   case (-1)
!      call setECMessage("ERROR: grib open: could not open file")
!   case (-2)
!      call setECMessage("ERROR: grib open: invalid filename")
!   case (-3)
!      call setECMessage("ERROR: grib open: invalid open mode")
!   case (-4)
!      call setECMessage("ERROR: grib lib not linked")
!   case default
!      call setECMessage("ERROR: grib open: unknown error")
!   end select
!   !
!end function ec_grib_open
!!
!function read_grib(unitnr, filename, filenr, xwind, ywind, press, num_rows, num_columns, tread, meta) result(success)
!   !
!   ! Read wind and pressure from grib file
!   !
!   ! result
!   logical                            :: success
!   !
!   ! arguments
!   !
!   integer                            :: unitnr
!   character(maxFileNameLen)          :: filename
!   integer                            :: filenr
!   integer                            :: num_rows       ! out at initialization, otherwise in
!   integer                            :: num_columns       ! out at initialization, otherwise in
!   type(tGrib_data), pointer, optional :: meta
!   real(hp), dimension(:,:)           :: xwind, ywind, press
!   real(hp)                           :: txwind, tywind, tpress, tread
!   !
!   ! locals
!   !
!   character(maxFileNameLen)          :: filename2
!   logical, parameter                 :: debug = .false.
!   logical                            :: initialize ! use for initialization
!   integer                            :: iyear, imonth, iday, ihour, imin
!   integer                            :: bufsz1, bufsz2
!   integer                            :: ifldtp   ! Type of the field values acording the WMO table 2,
!!    used are:  001   Pressure Pa
!!               033   u-component of wind m/s
!!               034   v-component of wind m/s
!!               124   Momentum flux (stress) u component N/m^2 (not yet implemented)
!!               125   Momentum flux (stress) v component N/m^2 (not yet implemented)
!   integer                             :: ierr
!   integer                             :: iword       ! number of elements from inbuff that contain coded data (return from gribex not used)
!   integer                             :: lenout
!   integer                             :: i, n, m
!   integer                             :: wildcard
!   integer , dimension(:), allocatable :: buffer
!   real(sp), dimension(:), allocatable :: zbuf4
!   integer                             :: isec0(2)    ! integer info from section 0 of GRIB file (identification)
!   integer                             :: isec1(1024) ! array holding section 1 of GRIB data (time/date)
!   integer                             :: isec2(1024) ! array holding section 2 of GRIB data (grid definition)
!   integer                             :: isec3(2)    ! info from section 3 of GRIB file (bitmap section)
!   integer                             :: isec4(512)  ! info from section 4 of GRIB file (field values)
!   real(sp)                            :: zsec2(512)  ! data from section 2 of the GRIB file
!   real(sp)                            :: zsec3(2)    ! data from section 3 of the GRIB file
!   logical                             :: found_u, found_v, found_p
!   character(1)                        :: request     ! 'J' only decodes identification, 'D' decodes all
!   !
!   ! body
!   !
!   success = .false.
!   initialize = (present(meta))
!
!   txwind  = dble(EC_MISSING_VALUE)
!   tywind  = dble(EC_MISSING_VALUE)
!   tpress  = dble(EC_MISSING_VALUE)
!   !
!   if (unitnr < 0) return ! work-around
!   !
!   if (initialize) then
!      request  = 'J'
!      bufsz1 = 4 * 999 * 999
!      bufsz2 = 1
!   else
!      request  = 'D'
!      bufsz1 = 4 * num_columns * num_rows
!      bufsz2 = bufsz1
!   endif
!   allocate(buffer(bufsz1), zbuf4(bufsz2), stat=ierr)
!   if (ierr /= 0) then
!      call setECMessage("allocate problem in read_grib")
!      return
!   endif
!   !
!   found_u = .false.
!   found_v = .false.
!   found_p = .false.
!   outer: do while (.not. (found_u .and. found_v .and. found_p))
!      !
!      ! read from file and check error status
!      !
!#     if defined GRIB
!         call pbgrib(unitnr, buffer, bufsz1, lenout, ierr)
!#     else
!         success = .false.
!         call setECMessage("ERROR: grib lib not linked")
!         exit outer
!#     endif
!      select case (ierr)
!      case (0)
!         success = .true.
!      case (-1)
!         if (filenr == -1) then
!            call setECMessage("end of file")
!            exit
!         else
!#           if defined GRIB
!               call pbclose(unitnr, ierr)
!#           endif
!            unitnr = -1
!            inner: do i = filenr+1, 9
!               wildcard = index(filename, '?')
!               filename2 = filename
!               write(filename2(wildcard:wildcard), '(i1)') i
!               inquire(file=trim(filename2), exist=success)
!               if (success) then
!                  success = ec_grib_open(unitnr, filename2)
!                  if (.not. success) exit outer
!#                 if defined GRIB
!                     call pbgrib(unitnr, buffer, bufsz1, lenout, ierr)
!#                 endif
!                  if (ierr == 0) then
!                     exit inner
!                  else
!                     call setECMessage("too much trouble with wildcards in gribfiles")
!                     exit outer
!                  endif
!               endif
!            enddo inner
!            if (.not. success) then
!               call setECMessage("too much trouble with wildcards in gribfiles")
!               exit outer
!            endif
!         endif
!      case (-2)
!         call setECMessage("error in file handling")
!         exit
!      case (-3)
!         call setECMessage("size of buffer array too small to hold product")
!         exit
!      case default
!         call setECMessage("unknown error in grib_grid")
!         exit
!      end select
!      !
!      ! decoded grib data
!      !
!#     if defined GRIB
!         if (debug) call grsdbg(1)
!#     endif
!      ierr = 0
!      call gribex(isec0, isec1, isec2, zsec2, isec3, zsec3, isec4, zbuf4, bufsz2, buffer, lenout, iword, request, ierr)
!      select case (ierr)
!      case (-4)
!         if (debug) write (*,*) "grib decode warning: Bit-map encountered"
!      case (-6)
!         if (debug) write (*,*) "grib decode warning: ECMWF pseudo-grib data encountered."
!      case (0)
!         success = .true.
!      case (1:999)
!         call setECMessage("decoding error in read_grib", ierr)
!         success = .false.
!         exit
!      case default
!         if (debug) write (*,*) "grib decode warning number", ierr
!      end select
!      !
!      ! fill metadata
!      !
!      if (initialize) then
!          call fill_grib_metadata(num_rows, num_columns, meta, isec2)
!          exit
!      endif
!      !
!      ! file time tread
!      !
!      iyear  = (isec1(21)-1)*100+isec1(10)
!      imonth = isec1(11)
!      iday   = isec1(12)
!      ihour  = isec1(13) + isec1(16)
!      imin   = isec1(14)
!      tread  = real(1440 * (iday -1) + 60 * ihour + imin, hp)
!      !
!      ! fill wind or pressure
!      !
!      ifldtp = isec1(6)
!      select case (ifldtp)
!      case (1)
!         if (debug) write (*,*) 'found pressure'
!         do m = 1, num_columns
!             do n = 1, num_rows
!                press(n, m) = zbuf4(m+(n-1)*num_columns)
!             enddo
!         enddo
!         tpress = tread
!         found_p = .true.
!      case (33)
!         if (debug) write (*,*) 'found u-wind'
!         do m = 1, num_columns
!             do n = 1, num_rows
!                xwind(n,m) = zbuf4(m+(n-1)*num_columns)
!             enddo
!         enddo
!         txwind = tread
!         found_u = .true.
!      case (34)
!         if (debug) write (*,*) 'found v-wind'
!         do m = 1, num_columns
!             do n = 1, num_rows
!                ywind(n,m) = zbuf4(m+(n-1)*num_columns)
!             enddo
!         enddo
!         tywind = tread
!         found_v = .true.
!      case (124:125)
!         if (debug) write (*,*) 'found wind stress'
!      case default
!         if (debug) write (*,*) 'unknown entry: ', ifldtp
!      end select
!   enddo outer
!   !
!   if ((tpress /= txwind .or. txwind /= tywind) .and. success .and. .not. initialize) then
!      write (*,*) 'different times for pressure and x, y wind: ', tpress, txwind, tywind
!      success = .false.
!   endif
!   !
!   deallocate(buffer, zbuf4)
!end function read_grib
!!
!subroutine fill_grib_metadata(num_rows, num_columns, meta, isec2)
!   !
!   ! arguments
!   !
!   integer                  :: num_rows, num_columns
!   type(tGrib_data)         :: meta
!   integer, dimension(1024) :: isec2
!   !
!   ! locals
!   !
!   integer                  :: iang1y, iangy, iang1x, iangx
!   integer                  :: igdtp
!   !
!   ! body
!   !
!   num_rows = isec2(3)
!   num_columns = isec2(2)
!   !
!   if ( isec2(7) .gt. isec2(4) ) then
!      iang1y = isec2(4)
!      iangy  = isec2(7)
!   else
!      iang1y = isec2(7)
!      iangy  = isec2(4)
!   endif
!   if (isec2(5) .gt. 180000) isec2(5) = isec2(5) - 360000
!   if (isec2(8) .gt. 180000) isec2(8) = isec2(8) - 360000
!   if ( isec2(8) .gt. isec2(5) ) then
!      iang1x = isec2(5)
!      iangx  = isec2(8)
!   else
!      iang1x = isec2(8)
!      iangx  = isec2(5)
!   endif
!   meta%dx = (real (iangx - iang1x, hp) * 1D-3) / real (num_columns - 1, hp)
!   meta%dy = (real (iangy - iang1y, hp) * 1D-3) / real (num_rows - 1, hp)
!   meta%x0 = real (iang1x, hp) * 1D-3
!   meta%y0 = real (iang1y, hp) * 1D-3
!   !
!   igdtp  = isec2(1)
!   if (igdtp .eq. 10) then
!      meta%latsp = real(isec2(13), hp) * 1D-3
!      meta%lonsp = real(isec2(14), hp) * 1D-3
!   else
!      meta%latsp = 0D0
!      meta%lonsp = 0D0
!   endif
!   !
!end subroutine fill_grib_metadata


      ! =======================================================================

      !> Add corr. to astro/harmonic components
      function ecApplyCorrectionToCmp(instancePtr, corFileReaderPtr) result(success)
      use m_ec_support
         logical                      :: success           !< function status
         type(tEcInstance),   pointer :: instancePtr       !< intent(in)
         type(tEcFileReader), pointer :: corFileReaderPtr  !< intent(inout)
         !
         integer                             :: nPeriods          !< number of periods
         real(hp), dimension(:), allocatable :: periods           !< Fourier components transformed into periods
         character(len=8), dimension(:), allocatable :: components
         real(hp), dimension(:), allocatable :: magnitudes        !< seed values for the magnitudes of the Fourier components
         real(hp), dimension(:), allocatable :: phases            !< seed values for the phases of the Fourier components
         type(tEcFileReader) , pointer     :: cmpFileReaderPtr    !< related file reader (with components)

         character(len=8), pointer         :: cmpcomponent(:), corcomponent(:)             !< raw data from input, stored in the bc%quantity
         real(hp), pointer                 :: cmpamplitude(:), coramplitude(:)
         real(hp), pointer                 :: cmpphase(:), corphase(:)

         real(hp), pointer                 :: cmpamplitude_result_T0(:)
         real(hp), pointer                 :: cmpphase_result_T0(:)
         real(hp), pointer                 :: cmpamplitude_result_T1(:)
         real(hp), pointer                 :: cmpphase_result_T1(:)

         integer           ::  icmp, ncmp, icor, ncor, iitem
         logical           ::  cmpfound
         !
         success = .true.

         if (corFileReaderPtr%bc%func==BC_FUNC_HARMOCORR) then
            cmpFileReaderPtr => ecSupportFindRelatedBCBlock(instancePtr, corFileReaderPtr, BC_FUNC_HARMONIC)
         elseif (corFileReaderPtr%bc%func==BC_FUNC_ASTROCORR) then
            cmpFileReaderPtr => ecSupportFindRelatedBCBlock(instancePtr, corFileReaderPtr, BC_FUNC_ASTRO)
         endif

         if (.not. associated(cmpFileReaderPtr)) then
            ! TODO: message: no related component
            success = .false.
            return
         endif

         if (ecFourierReadAll(corFileReaderPtr, periods, components, magnitudes, phases, nPeriods)) then

            do iitem = 1, cmpFileReaderPtr%nItems
               if (cmpFileReaderPtr%items(iitem)%ptr%role == itemType_source) then           ! source items
                  select case (cmpFileReaderPtr%items(iitem)%ptr%quantityptr%name)
                     case('magnitude')
                        cmpamplitude_result_T0 => cmpFileReaderPtr%items(iitem)%ptr%sourceT0FieldPtr%arr1d
                        cmpamplitude_result_T1 => cmpFileReaderPtr%items(iitem)%ptr%sourceT1FieldPtr%arr1d
                     case('phase')
                        cmpphase_result_T0 => cmpFileReaderPtr%items(iitem)%ptr%sourceT0FieldPtr%arr1d
                        cmpphase_result_T1 => cmpFileReaderPtr%items(iitem)%ptr%sourceT1FieldPtr%arr1d
                  end select
               endif
            enddo

            cmpcomponent => cmpFileReaderPtr%bc%quantity%astro_component
            cmpamplitude => cmpFileReaderPtr%bc%quantity%astro_amplitude
            cmpphase => cmpFileReaderPtr%bc%quantity%astro_phase
            corcomponent => corFileReaderPtr%bc%quantity%astro_component
            coramplitude => corFileReaderPtr%bc%quantity%astro_amplitude
            corphase => corFileReaderPtr%bc%quantity%astro_phase

            ncmp = size(cmpcomponent)                       ! number of components
            ncor = size(corcomponent)                       ! number of corrections
            do icor = 1,  ncor
               cmpfound = .false.
               do icmp = 1,  ncmp
                  if (trim(corcomponent(icor))==trim(cmpcomponent(icmp))) then
                     cmpamplitude_result_T0(icmp)=cmpamplitude(icmp)*coramplitude(icor)
                     cmpphase_result_T0(icmp)=cmpphase(icmp)+corphase(icor)
                     cmpamplitude_result_T1(icmp)=cmpamplitude(icmp)*coramplitude(icor)
                     cmpphase_result_T1(icmp)=cmpphase(icmp)+corphase(icor)
                     cmpfound = .true.
                     cycle
                  endif
               enddo                ! components
               if(.not.cmpfound) then
                  ! TODO: think of a meaningfull error message if correcting a non-existing component
                  success = .false.
               endif
            enddo                   ! corrections
         else
            ! TODO: message
            success = .false.
         end if
      end function ecApplyCorrectionToCmp

      function ecParseARCinfoMask(maskfilname, mask, fileReaderPtr) result(success)
         use m_ec_typedefs
         implicit none
         logical                         :: success
         character(len=256), intent(in)  :: maskfilname
         type(tEcMask),      intent(out) :: mask
         type(tEcFileReader),pointer     :: fileReaderPtr

         integer                        :: fmask
         integer                        :: iostat
         character(len=:), allocatable  :: rec
         logical                        :: jamaskinit
         integer                        :: i

         success = .false.

         if (.not.ecSupportOpenExistingFile(fmask, maskfilname)) then
            call setECMessage('Cannot open maskfile '//trim(maskfilname))
            return
         endif

         mask%mrange = fileReaderPtr%items(1)%ptr%elementSetPtr%n_cols     ! NB. implicitly assume that all items on this filereader are either based on
         mask%nrange = fileReaderPtr%items(1)%ptr%elementSetPtr%n_rows     !     the same elementset, or elementsets with the same number of rows and cols
         mask%mmin   = 1
         mask%nmin   = 1

         jamaskinit = .false.
         iostat = 0
         i = 0
         do while(iostat==0)
            call GetLine(fmask, rec, iostat)
            if (len_trim(rec)==0) cycle
            if (iostat/=0) cycle
            rec = adjustl(rec)
            call str_upper(rec)
            if (index('%*!#',rec(1:1))+index('//',rec(1:2)) > 0 ) cycle
            if (index(rec,'=')>0) then
               if (index(rec,'N_COLS')>0) then
                   read(rec(index(rec,'=')+1:len_trim(rec)),*,iostat=iostat) mask%mrange
               elseif (index(rec,'N_ROWS')>0) then
                   read(rec(index(rec,'=')+1:len_trim(rec)),*,iostat=iostat) mask%nrange
               elseif (index(rec,'XLL')>0) then
                   read(rec(index(rec,'=')+1:len_trim(rec)),*,iostat=iostat) mask%mmin
               elseif (index(rec,'YLL')>0) then
                   read(rec(index(rec,'=')+1:len_trim(rec)),*,iostat=iostat) mask%nmin
               endif
            else                 ! Line of values expected
                if (.not.jamaskinit) then
                   if ((mask%mrange>0) .and. (mask%nrange>0)) then
                      if (allocated(mask%msk)) deallocate (mask%msk)
                      allocate(mask%msk(mask%mrange*mask%nrange))
                      jamaskinit = .true.
                   else
                      call setECMessage('At least one of the mask dimensions in '//trim(maskfilname)//' is smaller than 1.')
                      return
                   endif
                endif
                i = i + 1
                ! NB. Mask is stored in a 1D-array (n_rows*n_cols) row-by-row from the last row to the first,
                !     identically to the curvi and arcinfo data, so that data 1d-array matches the mask array elementwise
                read(rec,*,iostat=iostat) mask%msk((mask%nrange-i)*mask%mrange+1:(mask%nrange-i+1)*mask%mrange)
            endif
         enddo          ! reading maskfile
         mask%num_columns = mask%mmin + mask%mrange - 1
         mask%num_rows = mask%nmin + mask%nrange - 1
         success = .true.
         close(fmask)
      end function ecParseARCinfoMask

       subroutine strip_comment(rec)
          implicit none
          character(len=*), intent(inout) :: rec
          integer                         :: reclen, commentpos
          reclen = len_trim(rec)                                  ! deal with various comment delimiters
          commentpos = index(rec,'//')
          if (commentpos>0) reclen = min(reclen,commentpos-1)
          commentpos = index(rec,'%')
          if (commentpos>0) reclen = min(reclen,commentpos-1)
          commentpos = index(rec,'#')
          if (commentpos>0) reclen = min(reclen,commentpos-1)
          commentpos = index(rec,'*')
          if (commentpos>0) reclen = min(reclen,commentpos-1)
          commentpos = index(rec,'!')
          if (commentpos>0) reclen = min(reclen,commentpos-1)
          rec=rec(1:reclen)
       end subroutine strip_comment

!     read data and store in CRS format
      subroutine read_data_sparse(filehandle, varid, n_cols, n_rows, n_layers, timesndx, relndx, ia, ja, Ndatasize, arr1d, ierror)
         use netcdf
         use netcdf_utils, only: ncu_get_att
         use io_ugrid

         implicit none

         integer,                        intent(in)    :: filehandle  !< filehandle
         integer,                        intent(in)    :: varid       !< variable id
         integer,                        intent(in)    :: n_cols      !< number of columns in input
         integer,                        intent(in)    :: n_rows      !< number of rows in input
         integer,                        intent(in)    :: n_layers    !< number of layers in input
         integer,                        intent(in)    :: timesndx    !< time index
         integer,                        intent(in)    :: relndx      !< realization index in an ensemble
         integer,          dimension(:), intent(in)    :: ia          !< CRS sparsity pattern, startpointers
         integer,          dimension(:), intent(in)    :: ja          !< CRS sparsity pattern, column numbers
         integer,                        intent(in)    :: Ndatasize   !< dimension of sparse data
         double precision, dimension(:), intent(inout) :: arr1d       !< CRS data
         integer,                        intent(out)   :: ierror      !< error (!=0) or not (0)

         double precision, dimension(:), allocatable   :: data_block  ! work array for reading

         integer,          dimension(:), allocatable   :: mcolmin, mcolmax
         integer,          dimension(:), allocatable   :: nrowmax

         integer                                       :: Ndata
         integer                                       :: mcol, nrow
         integer                                       :: nrowmin
         integer                                       :: i, j, k
         integer                                       :: istart, iend
         integer                                       :: ndims
         integer                                       :: ierr
         integer                                       :: Nreadrow      !< number of rows read at once
         character(len=:), allocatable                 :: standard_name
         character(len=64)                             :: stringBuffer
         integer, allocatable                          :: start(:), cnt(:)

         ierror = 1
         allocate(character(len=0) :: standard_name)

         Nreadrow = n_rows

!        compute number of data blocks
         Ndata = ceiling(dble(n_rows)/dble(nreadrow))

!        allocate data block
         allocate(data_block(n_cols*nreadrow))

!        allocate data block mrowmin, mrowmax, nrowmax arrays
         allocate(mcolmin(Ndata))
         mcolmin = n_cols
         allocate(mcolmax(Ndata))
         mcolmax = 1
         allocate(nrowmax(Ndata))

!        get bounding box around datablock
         j = 0
         do nrowmin=1,n_rows,nreadrow
            j = j+1

            nrowmax(j) = min(nrowmin+nreadrow-1, n_rows)

            do nrow=nrowmin,nrowmax(j)
               istart = ia(nrow)
               iend = ia(nrow+1)-1
               if ( iend.ge.istart ) then
                  mcolmin(j) = min(mcolmin(j), ja(istart))
                  mcolmax(j) = max(mcolmax(j), ja(iend))
               end if
            end do
         end do

         ierror = nf90_inquire_variable(filehandle, varid, ndims=ndims)
         allocate(start(ndims), cnt(ndims))
         start = 1
         cnt = 1

!        loop over layers
         do k=1, max(n_layers,1)

!           loop over rows
            j = 0
            do nrowmin=1,n_rows,nreadrow
               j = j+1

               if ( mcolmax(j).ge.mcolmin(j) ) then
!                 read data
                  start(1:2)   = (/ mcolmin(j), nrowmin /)
                  start(ndims) = timesndx
                  if (relndx>0 .and. ndims>=4) then
                     start(3) = relndx
                  endif
                  if ( n_layers /= 0 ) then
                     start(ndims-1) = k
                  end if
                  cnt(1:2) = (/mcolmax(j)-mcolmin(j)+1, nrowmax(j)-nrowmin+1 /)
                  ierror = nf90_get_var(fileHandle, varid, data_block, start=start, count=cnt)

                  if ( ierror /= 0 ) then
                     standard_name = ''
                     ierr = ncu_get_att(fileHandle, varid, 'standard_name', standard_name)
                     if (ierr /= 0) then
                        write(stringBuffer,*) 'varid = ', varid
                        call setECMessage("Read error in read_data_sparse for " // trim(stringBuffer))
                     else
                        call setECMessage("Read error in read_data_sparse for " // trim(standard_name))
                     end if
                     goto 1234
                  endif

                  do nrow=nrowmin,nrowmax(j)
                     do i=ia(nrow),ia(nrow+1)-1
                        mcol = ja(i)
                        arr1d(i + (k-1)*Ndatasize) = data_block(mcol-mcolmin(j)+1 + (mcolmax(j)-mcolmin(j)+1)*(nrow-nrowmin))
                     end do
                  end do
               end if
            end do
         end do

         ierror = 0

 1234    continue

!        deallocate
         deallocate(standard_name)
         if ( allocated(data_block) ) deallocate(data_block)
         if ( allocated(mcolmin)    ) deallocate(mcolmin)
         if ( allocated(mcolmax)    ) deallocate(mcolmax)
         if ( allocated(start)      ) deallocate(start)
         if ( allocated(cnt)        ) deallocate(cnt)

         return
      end subroutine read_data_sparse

   end module m_ec_filereader_read






