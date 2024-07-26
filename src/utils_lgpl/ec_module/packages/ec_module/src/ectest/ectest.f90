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
!-------------------------------------------------------------------------------
!  
!  
program ectest
   ! adri.mourits@deltares.nl
   use precision
   use gdp
   use m_ec_module
   use m_ec_provider ! only ecSetFileReaderProperties, this must be outside the EC-module, exactly like a kernel.
   use time_module
   !
   implicit none
   !
   ! locals
   !
   integer :: i
   integer :: istat
   integer :: j
   integer :: ij
   integer :: mnmax
   integer :: provid
   integer :: testnr
   logical :: success
   character(len=100) :: new_error_message
   character(len=10) :: string
   character(len=*), parameter :: errorstr = 'First commandline argument should be an integer (1..5)'
   character(len=8) :: reference_date !< Gregorian yyyymmdd string
   integer :: yyyymmdd !< Gregorian date as an integer
   !
   integer :: fileReaderId  !< helper variable
   integer :: quantityId    !< helper variable
   integer :: elementSetId  !< helper variable
   integer :: fieldId       !< helper variable
   integer :: itemId        !< helper variable
   integer :: connectionId  !< helper variable
   integer :: converterId   !< helper variable
   integer :: windspeed     !< Item for wind speed
   integer :: winddirection !< Item for wind direction
   integer :: windU         !< Item for wind U
   integer :: windV         !< Item for wind V
   !
!   logical ectest_rr_cf
!   external ectest_rr_cf
   !
   !! executable statements -------------------------------------------------------
   !
   reference_date = '19901231'
   read(reference_date, *) yyyymmdd
   reference_time = ymd2jul(yyyymmdd)
   !
   num_columns = 0
   num_rows = 0
   write(*,*) 'ECTEST start . . .'
   !
   ! Define the test grid
   !
   if (command_argument_count() > 0) then
      call getarg(1, string)
      read(string, '(i1)', iostat=istat) testnr
      if (istat /= 0 .or. testnr < 1 .or. testnr > 5) stop errorstr
   else
      stop errorstr
   endif
   if (testnr == 4) then
!      success = ectest_rr_cf()
      stop 'klaar met RR-CF test.'
   else if (testnr == 5) then
      ! Construct and initialize a new Instance of the EC-module
      success = ecCreateInstance(ecInstancePtr)
      if (.not. success) stop 'Failed to create a new EC-module instance.'
      ! Construct a new FileReader
      fileReaderId = ecCreateFileReader(ecInstancePtr)
      ! Initialize the new FileReader.
      success = ecSetFileReaderProperties(ecInstancePtr, fileReaderId, provFile_unimagdir, '..\ectest_input.wnd', yyyymmdd)
      if (.not. success) stop 'Failed to initialize the FileReader.'
      ! Done
      stop 'klaar met tables-test.'
   else if (testnr == 1) then
      num_rows   = 2
      num_columns   = 3
      mnmax  = num_columns*num_rows
      num_layers_grid   = 1
      sferic = .false.
                      allocate(kcs  (mnmax), STAT = istat)
      if (istat == 0) allocate(x    (mnmax), STAT = istat)
      if (istat == 0) allocate(y    (mnmax), STAT = istat)
      if (testnr == 1) then
         if (istat == 0) allocate(uwind(mnmax), STAT = istat)
         if (istat == 0) allocate(vwind(mnmax), STAT = istat)
         if (istat == 0) allocate(patm (mnmax), STAT = istat)
      endif
   else
      num_columns   = 201
      num_rows   = 173
      mnmax  = num_columns*num_rows
      num_layers_grid   = 1
      sferic = .true.
                      allocate(kcs  (mnmax), STAT = istat)
      if (istat == 0) allocate(x    (mnmax), STAT = istat)
      if (istat == 0) allocate(y    (mnmax), STAT = istat)
      if (istat == 0) allocate(uwind2d(num_rows,num_columns), STAT = istat)
      if (istat == 0) allocate(vwind2d(num_rows,num_columns), STAT = istat)
      if (istat == 0) allocate(patm2d (num_rows,num_columns), STAT = istat)
   endif
   
   ! ==========================================================================
   
   if (istat /= 0) then
      write(*,*) "ERROR: ectest: Unable to allocate additional memory"
      stop
   endif
   !
   kcs = 1
   do i=1, num_columns
      do j=1, num_rows
         ij = i + num_columns*(j-1)
         x(ij) = real(i,fp) * real(j,fp)
         y(ij) = real(i,fp) * real(j,fp) * 10.0_fp
      enddo
   enddo
   !
   ! Define test time parameters
   !
   curtim = reference_time + (0.0_hp / 60.0_hp / 24.0_hp)
   if (testnr == 1) then
      dt     = 0.1_hp / 60.0_hp / 60.0_hp / 24.0_hp ! 6 seconds as a delta Julian Day Number
      tstop  = reference_time + dt*100.0_hp
   else if (testnr == 2) then
      dt     = 1.0_hp / 60.0_hp / 24.0_hp ! 1 minute as a delta Julian Day Number
      tstop  = reference_time + dt*20.0_hp
   else if (testnr == 3) then
      dt     = 1440.0_hp / 60.0_hp / 24.0_hp ! 1440 minutes as a delta Julian Day Number
      tstop  = reference_time + dt*3.0_hp
   else
      dt     = 10.0_hp / 60.0_hp / 24.0_hp ! 10 minutes as a delta Julian Day Number
      tstop  = reference_time + dt*20.0_hp
   endif
   !
   ! EC init
   !
   ! Construct and initialize a new Instance of the EC-module
   success = ecCreateInstance(ecInstancePtr)
   if (.not. success) stop 'Failed to create a new EC-module instance.'
   !
   ! Construct the source Items by creating a new FileReader.
   !
   ! Construct a new FileReader
   fileReaderId = ecCreateFileReader(ecInstancePtr)
   ! Initialize the new FileReader.
   if (testnr == 1 .or. testnr == 2) then
      success = ecSetFileReaderProperties(ecInstancePtr, fileReaderId, provFile_unimagdir, 'ectest_input.wnd', yyyymmdd)
      if (.not. success) stop 'Failed to initialize the FileReader.'
   else
      !!!!! call ectest_readMeteo(testnr)
      stop 'The requested FileReader has not been implemented yet.'
   end if
   !
   ! Construct the target Items
   !
   ! ===== wind u =====
   quantityId = ecCreateQuantity(ecInstancePtr)
   if (.not. (ecSetQuantityName(ecInstancePtr, quantityId, 'wind u') .and. &
              ecSetQuantityUnits(ecInstancePtr, quantityId, ' '))) then
      success = .false.
   end if
   elementSetId = ecCreateElementSet(ecInstancePtr)
   if (.not. (ecSetElementSetType(ecInstancePtr, elementSetId, elmSetType_scalar))) then
      success = .false.
   end if
   fieldId = ecCreateField(ecInstancePtr)
   if (.not. (ecSetField1dArray(ecInstancePtr, fieldId, x))) then
      success = .false.
   end if
   itemId = ecCreateItem(ecInstancePtr)
   if (.not. (ecSetItemRole(ecInstancePtr, itemId, itemType_target) .and. &
              ecSetItemQuantity(ecInstancePtr, itemId, quantityId) .and. &
              ecSetItemElementSet(ecInstancePtr, itemId, elementSetId) .and. &
              ecSetItemTargetField(ecInstancePtr, itemId, fieldId))) then
      success = .false.
   end if
   if (success) then
      windU = itemId
   else
      stop 'Failed to create a new Item for wind u.'
   end if
   ! ===== wind v =====
   quantityId = ecCreateQuantity(ecInstancePtr)
   if (.not. (ecSetQuantityName(ecInstancePtr, quantityId, 'wind v') .and. &
              ecSetQuantityUnits(ecInstancePtr, quantityId, ' '))) then
      success = .false.
   end if
   elementSetId = ecCreateElementSet(ecInstancePtr)
   if (.not. (ecSetElementSetType(ecInstancePtr, elementSetId, elmSetType_scalar))) then
      success = .false.
   end if
   fieldId = ecCreateField(ecInstancePtr)
   if (.not. (ecSetField1dArray(ecInstancePtr, fieldId, y))) then
      success = .false.
   end if
   itemId = ecCreateItem(ecInstancePtr)
   if (.not. (ecSetItemRole(ecInstancePtr, itemId, itemType_target) .and. &
              ecSetItemQuantity(ecInstancePtr, itemId, quantityId) .and. &
              ecSetItemElementSet(ecInstancePtr, itemId, elementSetId) .and. &
              ecSetItemTargetField(ecInstancePtr, itemId, fieldId))) then
      success = .false.
   end if
   if (success) then
      windV = itemId
   else
      stop 'Failed to create a new Item for wind v.'
   end if
   !
   ! Connect source and target Items
   !
   ! Construct a new Converter.
   converterId = ecCreateConverter(ecInstancePtr)
   ! Initialize the new Converter.
   success = ecSetConverterType(ecInstancePtr, converterId, convType_unimagdir)
   if (.not. success) stop 'Unable to set Converter type.'
   success = ecSetConverterOperand(ecInstancePtr, converterId, operand_replace)
   if (.not. success) stop 'Unable to set Converter operand.'
   ! Construct a new Connection.
   connectionId = ecCreateConnection(ecInstancePtr)
   success = ecSetConnectionConverter(ecInstancePtr, connectionId, converterId)
   ! Initialize the new Connection.
   windspeed = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'windspeed')
   success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, windspeed)
   if (.not. success) stop 'Unable to add windspeed as a source Item to the Connection.'   
   winddirection = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'winddirection')
   success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, winddirection)
   if (.not. success) stop 'Unable to add winddirection as a source Item to the Connection.'   
   success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, windU)
   if (.not. success) stop 'Unable to add windU as a target Item to the Connection.'
   success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, windV)
   if (.not. success) stop 'Unable to add windV as a target Item to the Connection.'
   success = ecAddItemConnection(ecInstancePtr, windU, connectionId)
   if (.not. success) stop 'Unable to add a Connection to windU.'
   success = ecAddItemConnection(ecInstancePtr, windV, connectionId)
   if (.not. success) stop 'Unable to add a Connection to windV.'
   !
   ! Enough input for the full simulation period?
   !
   success = ecCheckTimeFrameTstartTstop(ecInstancePtr, fileReaderId, curtim, tstop)
   if (.not. success) stop 'The test models time domain exceeds the FileReaders time domain.'
   !
   ! calculation
   !
   do
      success = ecGetValues(ecInstancePtr, windU, curtim)
      if (.not. success) stop 'Unable to get a new value for windU.'
      write(*,*) 'New wind U value: ', x(1)
      success = ecGetValues(ecInstancePtr, windV, curtim)
      if (.not. success) stop 'Unable to get a new value for windV.'
      write(*,*) 'New wind V value: ', y(1)
      curtim = curtim + dt
      if (curtim >= tstop) exit
   end do
   !
   ! EC finish
   !
   success = ecFreeInstance(ecInstancePtr)
   if (.not. success) stop 'Unable to correctly and completely free the EC-module instance.'
   !
   ! free test
   !
   if (testnr == 1) then
      deallocate(uwind, STAT = istat)
      deallocate(vwind, STAT = istat)
      deallocate(patm , STAT = istat)
   else if (testnr == 2 .or. testnr == 3) then
      deallocate(uwind2d, STAT = istat)
      deallocate(vwind2d, STAT = istat)
      deallocate(patm2d , STAT = istat)
   endif

   write(*,*) 'ECTEST . . . finished'
end program ectest
