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
function ectest_rr_cf() result (success)
   use precision
   use ec_module
   !
   implicit none
   !
   ! result
   !
   logical                 :: success
   !
   ! arguments
   !
   ! -- none --
   !
   ! locals
   !
   type(tECHandle)         :: ECHandle   ! (so not using ectest_gdp anymore)
   logical                 :: ECPrivate
   integer                 :: ntstop
   real(hp)                :: dt, curtim
   !
   integer                 :: provId, targetId, qECItemId, istat
   integer                 :: nr_ids_S, nr_ids_T
   integer                 :: gridECItemIdS, gridECItemIdT
   real(fp), dimension(:), pointer :: results, values_in
   character(len=24), dimension(:), pointer :: IDs_S, IDs_T
   !
   ! body
   !
   nr_ids_S = 4
   nr_ids_T = 3
   !
                   allocate(IDs_S(nr_IDs_S), STAT = istat)
   if (istat == 0) allocate(IDs_T(nr_IDs_T), STAT = istat)
   if (istat == 0) allocate(results(nr_ids_T), STAT = istat)
   if (istat == 0) allocate(values_in(nr_ids_S), STAT = istat)
   if (istat /= 0) then
      write(*,*) "ERROR: ectest: Unable to allocate additional memory"
      stop
   endif
   !
   IDs_S = (/'loc1  ', 'locB  ', 'locIII', 'test  '/)
   IDs_T = (/'test  ', 'locIII', 'loc1  '/)
   !
   dt     = 10.0_hp
   ntstop = 20
   curtim = 0.0_hp
   !
   ! EC init
   !
   ECPrivate  = .true.
   success  = create(ECHandle, ECPrivate)
   call checkResult(ECHandle, success)
   !
   gridECItemIdS = addElementSet(ECHandle, IDs_S)
   if (gridECItemIdS == 0) call checkResult(ECHandle)
   !
   gridECItemIdT = addElementSet(ECHandle, IDs_T)
   if (gridECItemIdT == 0) call checkResult(ECHandle)
   !
   targetId  = addQuantity(ECHandle, 'discharge')
   qECItemId = addECItem(ECHandle, targetId, gridECItemIdT)
   success   = addECItemField(ECHandle, qECItemId, results)
   !
   provId   = addProvider(ECHandle, provType_PutById)
   if (provId == 0) call checkResult(ECHandle)
   success  = initProvider(ECHandle, provId, 'discharge', gridECItemId = gridECItemIdS)
   call checkResult(ECHandle, success)
   if (.not. success) return
   !
   success = ECCheck(ECHandle, curtim)
   call ECDump(ECHandle)
   !
   curtim   = curtim + dt
   values_in = 0d0
   values_in(1) = 9.81d0
   success  = putValues(ECHandle, provId, curtim, values_in)
   !
   curtim   = curtim + dt
   values_in(4) = 3.14d0
   success  = putValues(ECHandle, provId, curtim, values_in)
   !
   curtim   = curtim - 0.5d0 * dt
   success = getVal(ECHandle, qECItemId, curtim, results)
   !
   write (*,'(a,3f8.3)') 'cf gets from rr the value ', results
   !
   curtim   = curtim + 0.5d0 * dt
   success = getVal(ECHandle, qECItemId, curtim, results)
   !
   write (*,'(a,3f8.3)') 'cf gets from rr the value ', results
   
end function ectest_rr_cf

