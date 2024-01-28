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
!! for parallel/MPI models: stored in sumvalcur_local, and needs later mpi_allreduce:
!! @see updateValuesOnCrossSections_mpi
subroutine updateValuesOnCrossSections(tim1)
use m_monitoring_crosssections
use m_missing
use m_transport , only: NUMCONST_MDU
use m_partitioninfo, only: jampi
use m_sediment, only: jased, stmpar

implicit none
    double precision, intent(in) :: tim1 !< Current (new) time

    double precision,                 save        :: timprev = -1d0
    double precision,                 save        :: timstart
    double precision                              :: timstep, timtot
    integer                                       :: iv, icrs

    ! This routine can now be called any time, but will only do the update
    ! of sumval* when necessary:
    if (tlastupd_sumval == tim1) then
       return
    end if

    if(nval == 0) then
        nval  = 5 + NUMCONST_MDU 
        if( jased == 4 .and. stmpar%lsedtot > 0 ) then
           nval = nval + stmpar%lsedtot + 1      
           if( stmpar%lsedsus > 0 ) then
              nval = nval + stmpar%lsedsus + 1
           endif
        endif
    endif

    if (.not. allocated(sumvalcum_timescale)) then
       allocate(sumvalcum_timescale(nval))
       sumvalcum_timescale = 1d0
    endif

    if (.not. allocated(sumvalcur_local)) then
       allocate(sumvalcur_local(nval,ncrs))
       sumvalcur_local = 0d0
    endif
    

    if (timprev == -1d0) then
        timstep  = 0d0
        timstart = tim1 ! Generally tstart_user
        timtot   = 0d0
    else
        timstep = tim1 - timprev
        timtot  = tim1 - timstart
    end if

!   compute cross-section data for all cross-sections
    call sumvalueOnCrossSections(sumvalcur_local, nval)

    if (jampi == 0) then
      tlastupd_sumval = tim1
      ! NOTE: when jampi==1, the cross section sumvals on GUI screen are *not* correct, except at each ti_his interval.
      do icrs=1,ncrs
         do iv = 1, nval ! Nu nog "5+ Numconst" standaard grootheden, in buitenlus
            crs(icrs)%sumvalcur(iv) = sumvalcur_local(iv,icrs)
            crs(icrs)%sumvalcum(iv) = crs(icrs)%sumvalcum(iv) + max(sumvalcum_timescale(iv),1d0)*timstep*sumvalcur_local(iv,icrs)
            if (timtot > 0d0) then
                crs(icrs)%sumvalavg(iv) = crs(icrs)%sumvalcum(iv)/timtot/max(sumvalcum_timescale(iv),1d0)
            else
                crs(icrs)%sumvalavg(iv) = crs(icrs)%sumvalcur(iv)
            end if
          end do
      end do
    else
        
    if (.not. allocated(sumvalcum_local)) then
       allocate(sumvalcum_local(nval,ncrs))
       sumvalcum_local = 0d0      
    endif
    
        do icrs=1,ncrs
         do iv = 1, nval 
           ! if jampi = 1 we only update crs(icrs)%sumvalcur and crs(icrs)%sumvalcum every user timestep  
           sumvalcum_local(iv,icrs) = sumvalcum_local(iv,icrs) +   max(sumvalcum_timescale(iv),1d0)*timstep*sumvalcur_local(iv,icrs)
         enddo
        enddo
    endif

    timprev = tim1
end subroutine updateValuesOnCrossSections
