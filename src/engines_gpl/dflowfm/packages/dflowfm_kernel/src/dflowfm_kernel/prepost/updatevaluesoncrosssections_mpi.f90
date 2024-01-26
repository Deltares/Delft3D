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

!> Sums all monitored data on all cross sections, including time-integrated values.
!! for sequential/non-MPI models: stored in crs()%sumvalcur/sumvalcum
!! for parallel/MPI models: stored in sumvalcur_tmp, and needs later mpi_allreduce:
!! @see updateValuesOnCrossSections_mpi
subroutine updateValuesOnCrossSections_mpi(tim1)
   use m_monitoring_crosssections
   use m_partitioninfo
   use m_timer
   use m_transport , only: NUMCONST_MDU
   use m_sediment, only: jased, stmpar
   use m_alloc
   use mpi
   use m_flowtimes, only: tstart_user, ti_his
   implicit none
   double precision                 :: tim1, timtot
   integer                          :: iv, icrs, ierror

   ! This routine can now be called any time, but will only do the update
   ! of sumval* when necessary:
   
   !TODO see UNST-5429
   if (tlastupd_sumval == tim1 )then 
     return
   end if
   
   tlastupd_sumval = tim1

   timtot = tim1 - tstart_user
   if (timtot == 0) then
   timtot = 1 ! So that the first time we don't divide by zero, avoids if condition in loop.
   endif
   
   ! Allocate separate arrays to store sum
    if (.not. allocated(sumvalcur_global)) then
       allocate(sumvalcur_global(nval,ncrs))
       sumvalcur_global = 0d0      
    endif
    if (.not. allocated(sumvalcum_global)) then
       allocate(sumvalcum_global(nval,ncrs))
       sumvalcum_global = 0d0      
    endif
    
   ! Sum current and cumulative values across MPI partitions
   if ( jatimer.eq.1 ) call starttimer(IOUTPUTMPI)  
   ! TODO: see UNST-5429
    call mpi_allreduce(sumvalcum_local, sumvalcum_global,nval*ncrs,mpi_double_precision,mpi_sum,DFM_COMM_DFMWORLD,ierror)
    call mpi_allreduce(sumvalcur_local, sumvalcur_global,nval*ncrs,mpi_double_precision,mpi_sum,DFM_COMM_DFMWORLD,ierror)
   if ( jatimer.eq.1 ) call stoptimer(IOUTPUTMPI)

   ! Update values of crs object
   do icrs=1,ncrs
   
      crs(icrs)%sumvalcur(1:nval) = sumvalcur_global(1:nval,icrs)
      crs(icrs)%sumvalcum(1:nval) = sumvalcum_global(1:nval,icrs)
      crs(icrs)%sumvalavg(1:nval) = sumvalcum_global(1:nval,icrs) / timtot / max(sumvalcum_timescale(1:nval),1d0) 
      
   enddo
   
   return 
   
end subroutine updateValuesOnCrossSections_mpi
