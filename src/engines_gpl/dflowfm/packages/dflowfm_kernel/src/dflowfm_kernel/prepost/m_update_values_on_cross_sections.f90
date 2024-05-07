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

module m_update_values_on_cross_sections

   use stdlib_kinds, only: dp

   implicit none
   
   private
   
   public :: update_values_on_cross_sections
   
   real(dp) :: time_of_last_reset = -1.0_dp
   real(dp) :: time_of_last_call

contains

!> Updates all monitored data on all cross sections, including time-integrated values
subroutine update_values_on_cross_sections(do_reduce)
   use m_monitoring_crosssections, only: nval, ncrs, crs_values, crs_timescales, crs
   use precision, only: comparereal
   use m_flowtimes, only: time1
   
   logical, intent(in) :: do_reduce

   real(dp)       :: time_since_last_reset, time_since_last_call
   integer        :: iv, icrs

   if (ncrs < 1) then
      return
   end if

   if (comparereal(time_of_last_reset, -1.0_dp) == 0) then
      call initialise_cross_section_integrals
   end if

   call integrate_over_cross_section_flowlinks(do_reduce)
   
   time_since_last_call  = time1 - time_of_last_call
   
   if (comparereal(time_since_last_reset, 0.0_dp) == 0) then
      time_since_last_reset = 1.0_dp ! So that the first time after resetting, we don't divide by zero in calculating the average
   else
      time_since_last_reset = time1 - time_of_last_reset
   end if

   ! Update values of crs object
   do icrs = 1, ncrs
      do iv = 1, nval
         crs(icrs)%sumvalcur(iv) = crs_values(iv,icrs)
         crs(icrs)%sumvalcum(iv) = crs(icrs)%sumvalcum(iv) + max(crs_timescales(iv),1.0_dp) * time_since_last_call * crs_values(iv,icrs)
         crs(icrs)%sumvalavg(iv) = crs(icrs)%sumvalcum(iv) / time_since_last_reset / max(crs_timescales(iv),1.0_dp)
       end do
   end do

   time_of_last_call = time1
   
end subroutine update_values_on_cross_sections

!> Initialise memory for cross-section flowlink integrals
subroutine initialise_cross_section_integrals
   use m_monitoring_crosssections, only: nval, ncrs, crs_values, crs_timescales, crs
   use m_transport, only: NUMCONST_MDU
   use m_sediment, only: jased, stmpar
   use m_flowtimes, only: time1
   
   integer :: icrs

   nval  = 5 + NUMCONST_MDU 
   if (jased == 4 .and. stmpar%lsedtot > 0) then
      nval = nval + stmpar%lsedtot + 1      
      if (stmpar%lsedsus > 0) then
         nval = nval + stmpar%lsedsus + 1
      end if
   end if

   allocate(crs_timescales(nval))
   crs_timescales = 1.0_dp

   allocate(crs_values(nval, ncrs))
   crs_values = 0.0_dp
   
   do icrs = 1, ncrs
      crs(icrs)%sumvalcur = 0.0_dp
      crs(icrs)%sumvalcum = 0.0_dp
      crs(icrs)%sumvalavg = 0.0_dp
   end do
   
   time_of_last_reset = time1
   time_of_last_call  = time1
   
end subroutine initialise_cross_section_integrals

!> Integrate monitored values over the flowlinks of each cross-section
!! Optionally includes reduction across processes in case of a parallel model
subroutine integrate_over_cross_section_flowlinks(do_reduce)
   use m_monitoring_crosssections, only: ncrs, crs_values, crs_timescales, crs, IPNT_Q1C, IPNT_AUC, IPNT_S1A, IPNT_HUA, IPNT_U1A
   use m_flowgeom, only: ln, wu, wu_mor
   use m_flow, only: q1, au, s1, hu, Lbot, Ltop
   use m_transport, only: NUMCONST_MDU, constituents
   use m_sediment, only: jased, stmpar, sedtra
   use m_partitioninfo, only: jampi

   implicit none
   
   logical, intent(in) :: do_reduce

   integer  :: icrs, i, Lf, L, k1, k2, IP, num, LL, IPTOT, lsed
   real(dp) :: val

   crs_values = 0.0_dp

   do icrs = 1, ncrs
      do i = 1, crs(icrs)%path%lnx
         
         Lf = crs(icrs)%path%ln(i)
         if (Lf == 0) then
            cycle ! Closed wall
         end if
         
         L  = abs(Lf)
         k1 = ln(1,L)
         k2 = ln(2,L)

         crs_values(IPNT_Q1C,icrs) = crs_values(IPNT_Q1C,icrs) + real(sign(1, Lf),dp) * q1(L)        ! discharge
         crs_values(IPNT_AUC,icrs) = crs_values(IPNT_AUC,icrs) + au(L)                               ! area
         ! NOTE: IPNT_U1A is now not included.
         crs_values(IPNT_S1A,icrs) = crs_values(IPNT_S1A,icrs) + 0.5_dp*( s1(k1) + s1(k2) ) * au(L)  ! weigted waterlevel
         crs_values(IPNT_HUA,icrs) = crs_values(IPNT_HUA,icrs) + hu(L) * au(L)                       ! upwind waterdepth

         IP = IPNT_HUA
         do num = 1, NUMCONST_MDU
            IP = IP + 1
            do LL = Lbot(L), Ltop(L)
               k1 = ln(1,LL); k2 = ln(2,LL)
               crs_values(IP,icrs) = crs_values(IP,icrs) + real(sign(1, Lf),dp) * (max(q1(LL),0.0_dp) * constituents(num,k1) &
                                                                                 + min(q1(LL),0.0_dp) * constituents(num,k2))
            end do
         end do

         if (jased == 4) then 
            if (stmpar%lsedtot > 0) then ! todo, loop korter tot lsedsus.
               IP = IPNT_HUA + NUMCONST_MDU + 1 ! TODO: mourits/dam_ar: check whether all uses of NUMCONST versus NUMCONST_MDU are now correct.
               IPTOT = IP
               do lsed = 1, stmpar%lsedtot  ! sum of bed load 
                  IP = IP + 1
                  val = sedtra%e_sbn(L,lsed) * wu_mor(L) * real(sign(1, Lf),dp)
                  crs_values(IPTOT,icrs) = crs_values(IPTOT,icrs) + val   ! sum of bed load on crosssections
                  crs_values(IP   ,icrs) = crs_values(IP   ,icrs) + val   ! bed load on crosssections per fraction
               end do
            end if
            if( stmpar%lsedsus > 0 ) then
               IP = IP + 1
               IPTOT = IP
               do lsed = 1,stmpar%lsedsus ! sum of suspended load 
                  IP = IP + 1
                  val = sedtra%e_ssn(L,lsed) * wu(L) * real(sign(1, Lf),dp)
                  crs_values(IPTOT,icrs) = crs_values(IPTOT,icrs) + val   ! sum of suspended load on crosssections
                  crs_values(IP   ,icrs) = crs_values(IP   ,icrs) + val   ! suspended load on crosssections per fraction
               end do
            end if
         end if
         
      end do
   end do

   if (jased == 4) then 
      if (stmpar%lsedtot > 0) then
         IP = IPNT_HUA + NUMCONST_MDU + 1
         crs_timescales(IP) = stmpar%morpar%morfac
         do lsed = 1, stmpar%lsedtot
            IP = IP + 1
            crs_timescales(IP) = stmpar%morpar%morfac
         end do    
      end if
      if (stmpar%lsedsus > 0) then
         IP = IP + 1
         crs_timescales(IP) = stmpar%morpar%morfac
         do lsed = 1,stmpar%lsedsus
            IP = IP + 1
            crs_timescales(IP) = stmpar%morpar%morfac
         end do    
      end if
   end if
   
   if (jampi == 1 .and. do_reduce) then
      call reduce_cross_section_flowlink_integrals()
   endif

   ! Calculate cross section area-averaged quantities
   do icrs = 1, ncrs
      if (crs_values(IPNT_AUC,icrs) > 0) then
         crs_values(IPNT_U1A,icrs) = crs_values(IPNT_Q1C,icrs) / crs_values(IPNT_AUC,icrs) ! average velocity
         crs_values(IPNT_S1A,icrs) = crs_values(IPNT_S1A,icrs) / crs_values(IPNT_AUC,icrs) ! average waterlevel
         crs_values(IPNT_HUA,icrs) = crs_values(IPNT_HUA,icrs) / crs_values(IPNT_AUC,icrs) ! average waterdepth
      end if
   end do

end subroutine integrate_over_cross_section_flowlinks

!> Reduce cross-section flowlink-integrated data
subroutine reduce_cross_section_flowlink_integrals
   use m_monitoring_crosssections, only: nval, ncrs, crs_values
   use m_partitioninfo
   use m_timer
#ifdef HAVE_MPI
   use mpi
#endif
   
   implicit none
   
   integer  :: ierror
    
#ifdef HAVE_MPI
   if (jatimer == 1) call starttimer(IOUTPUTMPI)
   call mpi_allreduce(mpi_in_place, crs_values, nval*ncrs, mpi_double_precision, mpi_sum, DFM_COMM_DFMWORLD, ierror)
   if (jatimer == 1) call stoptimer(IOUTPUTMPI)
#endif
   
end subroutine reduce_cross_section_flowlink_integrals

end module m_update_values_on_cross_sections