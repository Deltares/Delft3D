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

!> This module contains all the methods for the datatype tEcQuantity.
!! @author arjen.markus@deltares.nl
!! @author adri.mourits@deltares.nl
!! @author stef.hummel@deltares.nl
!! @author edwin.spee@deltares.nl
!! @author robert.leander@deltares.nl
module m_ec_quantity
   use m_ec_typedefs
   use m_ec_message
   use m_ec_support
   use m_ec_alloc
   
   implicit none
   
   private
   
   public :: ecQuantityCreate
   public :: ecQuantityFree1dArray
   public :: ecQuantitySet
   public :: ecQuantitySetUnitsFillScaleOffsetFromNcidVarid
   public :: ecQuantitySetName
   public :: ecQuantitySetTimeint
   
   contains
      
      ! =======================================================================
      
      !> Construct a new Quantity with the specified id.
      !! Failure is indicated by returning a null pointer.
      function ecQuantityCreate(quantityId) result(quantityPtr)
         type(tEcQuantity), pointer    :: quantityPtr !< the new Quantity, intent(out)
         integer,           intent(in) :: quantityId  !< unique Quantity id
         !
         integer :: istat !< allocate() status
         !
         ! allocation
         allocate(quantityPtr, stat = istat)
         if (istat /= 0) then
            call setECMessage("ERROR: ec_quantity::ecQuantityCreate: Unable to allocate additional memory.")
            quantityPtr => null()
            return
         end if
         ! initialization
         quantityPtr%id = quantityId
         quantityPtr%name = ' '
         quantityPtr%units = ' '
      end function ecQuantityCreate
      
      ! =======================================================================
      
      !> Frees a 1D array of tEcQuantityPtrs, after which the quantityPtr is deallocated.
      function ecQuantityFree1dArray(quantityPtr, nQuantities) result (success)
         logical                                     :: success     !< function status
         type(tEcQuantityPtr), dimension(:), pointer :: quantityPtr !< intent(inout)
         integer, intent(inout)                      :: nQuantities !< number of Quantities
         !
         integer :: i     !< loop counter
         integer :: istat !< deallocate() status
         !
         success = .true.
         !
         if (.not. associated(quantityPtr)) then
            call setECMessage("WARNING: ec_quantity::ecQuantityFree1dArray: Dummy argument quantityPtr is already disassociated.")
         else
            ! Free(nothing to do) and deallocate all tEcQuantityPtrs in the 1d array.
            do i=1, nQuantities
               deallocate(quantityPtr(i)%ptr, stat = istat)
               if (istat /= 0) success = .false.
            end do
            ! Finally deallocate the tEcQuantityPtr(:) pointer.
            if (success) then
               deallocate(quantityPtr, stat = istat)
               if (istat /= 0) success = .false.
            end if
         end if
         nQuantities = 0
      end function ecQuantityFree1dArray
      
      ! =======================================================================
      
       function ecQuantitySet(instancePtr, quantityId, name,                     &
                                                       units,                    &
                                                       vectormax,                &
                                                       periodic, constant,       &
                                                       factor,                   &
                                                       offset,                   &
                                                       fillvalue,                &
                                                       timeint,                  &
                                                       ncid                      &
                                                     ) result(success)

         logical                               :: success     !< function status
         type(tEcInstance), pointer            :: instancePtr !< intent(in)
         integer,                   intent(in) :: quantityId  !< unique Quantity id

         character(len=*),optional :: name
         character(len=*),optional :: units
         integer         ,optional :: vectormax
         logical         ,optional :: periodic, constant
         real(hp)        ,optional :: factor
         real(hp)        ,optional :: offset
         real(hp)        ,optional :: fillvalue
         integer         ,optional :: timeint
         integer         ,optional :: ncid
         !
         type(tEcQuantity), pointer :: quantityPtr !< Quantity corresponding to quantityId
         !
         success = .false.
         quantityPtr => null()
         !
         quantityPtr => ecSupportFindQuantity(instancePtr, quantityId)
         if (.not.associated(quantityPtr)) then
            call setECMessage("ERROR: ec_quantity::ecQuantitySet: Cannot find a Quantity with the supplied id.")
         end if
         if (present(name)) then 
            if (len_trim(name) > maxNameLen) then
               call setECMessage("ERROR: ec_quantity::ecQuantitySetName: The new name string is too long, unable to change name.")
            else
               quantityPtr%name = name  
            end if
         end if

         if (present(units)) quantityPtr%units = units  
         if (present(vectormax)) quantityPtr%vectormax = vectormax  
         if (present(periodic)) quantityPtr%periodic = periodic  
         if (present(constant)) quantityPtr%constant = constant  
         if (present(factor)) quantityPtr%factor = factor  
         if (present(offset)) quantityPtr%offset = offset  
         if (present(fillvalue)) quantityPtr%fillvalue = fillvalue  
         if (present(timeint)) quantityPtr%timeint = timeint  
         if (present(ncid)) quantityPtr%ncid = ncid  
         success = .true.
         
      end function ecQuantitySet
      
      !> Change the Units, Fillvalue, Scalefactor and Offset shift of the Quantity corresponding to quantityId
      !> obtained from the variable varid in the netcdf file ncid 
      !> all in try-catch fashion: if not available, leave empty or use default
      function ecQuantitySetUnitsFillScaleOffsetFromNcidVarid(instancePtr, quantityId, ncid, varid) result(success)
         use netcdf
         use netcdf_utils, only: ncu_get_att
         use string_module
         use physicalconsts, only : CtoKelvin
         use io_ugrid 

         implicit none
         logical                               :: success     !< function status
         type(tEcInstance), pointer            :: instancePtr !< intent(in)
         integer,                   intent(in) :: quantityId  !< unique Quantity id
         integer,                   intent(in) :: ncid        !< id of the nc-file 
         integer,                   intent(in) :: varid       !< id of the variable
                                                              !< order: new = (old*scale) + offset
         character(len=:), allocatable  :: units
         integer  :: ierr
         real(hp) :: add_offset, scalefactor, fillvalue
         
         allocate(character(len=0) :: units)
         
         success = .false.
         add_offset = 0.d0
         scalefactor = 1.d0
         fillvalue = ec_undef_hp

         units = ''
         if (ncu_get_att(ncid, varid, 'units', units)==NF90_NOERR) then 
            call str_upper(units) ! make units attribute case-insensitive 
            if (.not.(ecQuantitySet(instancePtr, quantityId, units=units))) then
                deallocate(units)
                return
            end if
         end if

         ierr = nf90_get_att(ncid, varid, '_FillValue', fillvalue)
         if (ierr==NF90_NOERR) then
            if (.not.(ecQuantitySet(instancePtr, quantityId, fillvalue=fillvalue))) then
               call setECMessage("Unable to set fillValue for quantity ", quantityId)
               return
            end if
         end if

         ierr = nf90_get_att(ncid, varid, 'scale_factor', scalefactor)
         if (ierr==NF90_NOERR) then
            if (.not.(ecQuantitySet(instancePtr, quantityId, factor=scalefactor))) then
               call setECMessage("Unable to set scale factor for quantity ", quantityId)
               return
            end if
         end if

         ierr = nf90_get_att(ncid, varid, 'add_offset', add_offset)
         if (ierr /= NF90_NOERR) add_offset = 0.0_hp

         if (units == 'K' .or. units == 'KELVIN') then
            ! convert Kelvin to degrees Celsius (kernel expects degrees Celsius)
            add_offset = add_offset - CtoKelvin
         end if

         if (add_offset /= 0.0_hp) then
            if (.not.(ecQuantitySet(instancePtr, quantityId, offset=add_offset))) then
               call setECMessage("Unable to set offset for quantity ", quantityId)
               return
            end if
         end if

        deallocate(units)
        success = .true.

      end function ecQuantitySetUnitsFillScaleOffsetFromNcidVarid
     
      ! =======================================================================
      
      !> Change the name of the Quantity corresponding to quantityId.
      function ecQuantitySetName(instancePtr, quantityId, newName) result(success)
         logical                               :: success     !< function status
         type(tEcInstance), pointer            :: instancePtr !< intent(in)
         integer,                   intent(in) :: quantityId  !< unique Quantity id
         character(*),              intent(in) :: newName     !< new name of the Quantity
         !
         type(tEcQuantity), pointer :: quantityPtr !< Quantity corresponding to quantityId
         character(len=maxNameLen)  :: name        !< new name of the Quantity, converted to the correct length
         !
         success = .false.
         quantityPtr => null()
         !
         if (len_trim(newName) > maxNameLen) then
            call setECMessage("ERROR: ec_quantity::ecQuantitySetName: The new name string is too long, unable to change name.")
         else
            name = newName
         end if
         quantityPtr => ecSupportFindQuantity(instancePtr, quantityId)
         if (associated(quantityPtr)) then
            quantityPtr%name = name
            success = .true.
         else
            call setECMessage("ERROR: ec_quantity::ecQuantitySetName: Cannot find a Quantity with the supplied id.")
         end if
      end function ecQuantitySetName

      ! =======================================================================
      
      !> Change the timeinterpolation type of the Quantity corresponding to quantityId.
      function ecQuantitySetTimeint(instancePtr, quantityId, timeint, periodic, constant) result(success)
         logical                               :: success     !< function status
         type(tEcInstance), pointer            :: instancePtr !< intent(in)
         integer,                   intent(in) :: quantityId  !< unique Quantity id
         integer,                   intent(in) :: timeint     !< new vectormax of the Quantity
         logical, optional,         intent(in) :: periodic    !< periodic time function?
         logical, optional,         intent(in) :: constant    !< constant value?
         !
         type(tEcQuantity), pointer :: quantityPtr !< Quantity corresponding to quantityId
         !
         success = .false.
         quantityPtr => null()
         !
         quantityPtr => ecSupportFindQuantity(instancePtr, quantityId)
         if (associated(quantityPtr)) then
            quantityPtr%timeint = timeint
            if (present(periodic)) then
                   quantityPtr%periodic = periodic
            endif
            if (present(constant)) then
                   quantityPtr%constant = constant
            endif
            success = .True.
         else
            call setECMessage("ERROR: ec_quantity::ecQuantitySetTimeint: Cannot find a Quantity with the supplied id.")
         end if
      end function ecQuantitySetTimeint
      
end module m_ec_quantity
