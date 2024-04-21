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

!> compute cross-section data, summed across all flow links for each cross-section.
!! In parallel models, only summed across flow links in own domain.
!! @see updateValuesOnCrossSections @see updateValuesOnCrossSections_mpi
subroutine sumvalueOnCrossSections(resu, numvals)
    use m_flow
    use m_flowgeom
    use m_monitoring_crosssections
    use m_partitioninfo
    use m_timer
    use m_transport, only: NUMCONST_MDU, ISALT, ITEMP, ISED1, ITRA1, constituents
    use m_sediment, only: jased, stmpar, sedtra


    implicit none
    integer, intent(in)           :: numvals             !< Which values to sum (1=discharge)
    double precision, intent(out) :: resu(numvals,ncrs)  !< cross-section data, note: ncrs from module m_monitoring_crosssections

    integer                       :: i, Lf, L, k1, k2, IP, num, LL, IPTOT
    integer                       :: icrs
    double precision              :: val
    integer                       :: lsed

    if ( ncrs.lt.1 ) return   ! nothing to do

    resu = 0d0

    do icrs=1,ncrs
       do i=1,crs(icrs)%path%lnx
           Lf = crs(icrs)%path%ln(i)
           if (Lf == 0) cycle ! Closed wall
           L  = abs(Lf)
           k1 = ln(1,L); k2 = ln(2,L)

           resu(IPNT_Q1C,icrs) = resu(IPNT_Q1C,icrs) + dble(sign(1, Lf)) * q1(L)                     ! discharge

           resu(IPNT_AUC,icrs) = resu(IPNT_AUC,icrs) + au(L)                                         ! area

           ! NOTE: IPNT_U1A is now not included.

           resu(IPNT_S1A,icrs) = resu(IPNT_S1A,icrs) + 0.5d0*( s1(k1) + s1(k2) ) * au(L)             ! weigted waterlevel

           resu(IPNT_HUA,icrs) = resu(IPNT_HUA,icrs) + hu(L) * au(L)                                 ! upwind waterdepth

           IP = IPNT_HUA
           do num = 1,NUMCONST_MDU
              IP = IP + 1
              do LL = Lbot(L), Ltop(L)
                 k1 = ln(1,LL); k2 = ln(2,LL)
                 resu(IP,icrs) = resu(IP,icrs) + dble(sign(1, Lf)) * ( max(q1(LL),0d0) * constituents(num,k1) &
                                                                     + min(q1(LL),0d0) * constituents(num,k2) )
              enddo
           enddo
 
           if( jased == 4 ) then 
              if ( stmpar%lsedtot > 0 ) then ! todo, loop korter tot lsedsus.
                 IP = IPNT_HUA + NUMCONST_MDU + 1 ! TODO: mourits/dam_ar: check whether all uses of NUMCONST versus NUMCONST_MDU are now correct.
                 IPTOT = IP
                 do lsed = 1,stmpar%lsedtot  ! sum of bed load 
                    IP = IP + 1
                    val = sedtra%e_sbn(L,lsed) * wu_mor(L) * dble(sign(1, Lf))
                    resu(IPTOT,icrs) = resu(IPTOT,icrs) + val   ! sum of bed load on crosssections
                    resu(IP,icrs) = resu(IP,icrs) + val         ! bed load on crosssections per fraction
                 enddo
              endif
              if( stmpar%lsedsus > 0 ) then
                 IP = IP + 1
                 IPTOT = IP
                 do lsed = 1,stmpar%lsedsus ! sum of suspended load 
                    IP = IP + 1
                    val = sedtra%e_ssn(L,lsed) * wu(L) * dble(sign(1, Lf))
                    resu(IPTOT,icrs) = resu(IPTOT,icrs) + val   ! sum of suspended load on crosssections
                    resu(IP,icrs) = resu(IP,icrs) + val         ! suspended load on crosssections per fraction
                 enddo
              endif
           endif
       end do
    end do   ! do icrs=1,ncrs

    if( jased == 4 ) then 
       if ( stmpar%lsedtot > 0 ) then
          IP = IPNT_HUA + NUMCONST_MDU + 1
          sumvalcum_timescale(IP) = stmpar%morpar%morfac
          do lsed = 1,stmpar%lsedtot
             IP = IP + 1
             sumvalcum_timescale(IP) = stmpar%morpar%morfac
          enddo    
       endif
       if( stmpar%lsedsus > 0 ) then
          IP = IP + 1;
          sumvalcum_timescale(IP) = stmpar%morpar%morfac
          do lsed = 1,stmpar%lsedsus
             IP = IP + 1
             sumvalcum_timescale(IP) = stmpar%morpar%morfac
          enddo    
       endif
    endif

    if (jampi == 0 ) then
      ! NOTE: if jampi==1, it is incorrect to compute quantities that require division by AU values
      ! since these are not mpi_reduced yet. So, don't compute them at all in parallel runs.
      do icrs=1,ncrs
         if (resu(IPNT_AUC,icrs) > 0) then
             resu(IPNT_U1A,icrs) = resu(IPNT_Q1C,icrs) / resu(IPNT_AUC,icrs)                              ! average velocity
             resu(IPNT_S1A,icrs) = resu(IPNT_S1A,icrs) / resu(IPNT_AUC,icrs)                              ! average waterlevel
             resu(IPNT_HUA,icrs) = resu(IPNT_HUA,icrs) / resu(IPNT_AUC,icrs)                              ! average waterdepth
         endif
      end do
    endif

!!   BEGIN DEBUG
!    do icrs=1,ncrs
!       write(6,"('icrs=', I0, ', my_rank=', I0, ', Q=', G15.5)") icrs, my_rank, resu(2,icrs)
!    end do
!!   END DEBUG

end subroutine sumvalueOnCrossSections
