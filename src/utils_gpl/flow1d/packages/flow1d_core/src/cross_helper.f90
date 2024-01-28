module m_cross_helper
!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.             
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
!-------------------------------------------------------------------------------

   use m_network
   use M_newcross
   use m_CrossSections
   use m_tables
   
   implicit none
   
   public getBobs
   public getConveyance
   public getCrossDischarge
   
   private
   
contains

   function getbobs(network, ilink) result(res)
      type(t_network), intent(in) :: network
      integer, intent(in) :: ilink
      double precision, dimension(2) ::res
      
      type (t_CrossSection), pointer     :: cross1
      type (t_CrossSection), pointer     :: cross2 
      double precision :: factor

      if (network%adm%line2cross(ilink, 2)%c1 < 0) then
         ! no cross section on this branch
         res = huge(1d0)
         return
      endif

      cross1 => network%crs%cross(network%adm%line2cross(ilink, 1)%c1)
      cross2 => network%crs%cross(network%adm%line2cross(ilink, 1)%c2)
      factor = network%adm%line2cross(ilink,1)%f
      res(1) = getBob(cross1, cross2, factor)      

      cross1 => network%crs%cross(network%adm%line2cross(ilink, 3)%c1)
      cross2 => network%crs%cross(network%adm%line2cross(ilink, 3)%c2)
      factor = network%adm%line2cross(ilink, 3)%f
      res(2) = getBob(cross1, cross2, factor)      

   end function getbobs   

! =================================================================================================
! =================================================================================================
   subroutine getConveyance(network, dpt, u1L, q1L, s1L, L, perim_sub, flowarea_sub, conv, cz_sub, cz, flowArea, wetPerimeter, factor_time_interpolation)
      use m_CrossSections     , only: t_CSType, CS_TABULATED, CS_YZ_PROF
      
      implicit none
      type(t_network),                intent(in   )    :: network                      !< Network data structure
      double precision,               intent(in   )    :: dpt                          !< Water depth 
      double precision,               intent(in   )    :: u1L                          !< Flow velocity 
      double precision,               intent(in   )    :: q1L                          !< Discharge 
      double precision,               intent(in   )    :: s1L                          !< Upstream water level 
      double precision,               intent(  out)    :: conv                         !< Conveyance 
      integer,                        intent(in   )    :: L                            !< Link number 
      double precision, dimension(3), intent(in   )    :: flowarea_sub                 !< Flow area in subsections (for ZW-river cross sections) 
      double precision, dimension(3), intent(in   )    :: perim_sub                    !< Wet perimeter in subsections (for ZW-river cross sections)
      double precision, dimension(3), intent(  out)    :: cz_sub                       !< Chezy value in subsections (for ZW-river cross sections)
      double precision,               intent(  out)    :: cz                           !< Chezy value 
      double precision,               intent(in   )    :: flowArea                     !< Flow area
      double precision,               intent(in   )    :: wetPerimeter                 !< Wet perimeter 
      double precision,               intent(in   )    :: factor_time_interpolation    !< Factor for interpolation of time dependent conveyance tables
                                                                                       !< conveyance = (1-factor)*conv1 + factor*conv2

      integer                        :: i , n
      double precision, parameter    :: eps = 1d-3               !< accuracy parameter for determining wetperimeter == 0d0
      double precision               :: r, cz1, cz2
      double precision               :: f
      integer                        :: igrid
      integer                        :: ibranch
      logical                        :: YZ_conveyance 
      double precision               :: chainage
      type(t_CSType), pointer        :: cross
      
      n = network%adm%line2cross(L, 2)%c1
      if ( n <= 0) then
         ! no cross section defined on L
         conv = 45d0* flowarea_sub(1) * sqrt(flowarea_sub(1) / perim_sub(1))
         cz = 45d0
         return
      else
         ! for YZ profiles CalcCSParsFlow computes the conveyance
         yz_conveyance = network%crs%cross(n)%crosstype == CS_YZ_PROF
      endif
      
      if (yz_conveyance) then
         call getYZConveyance(network%adm%line2cross(L, 2), network%crs%cross, dpt, u1L, cz, conv, factor_time_interpolation)
         cz_sub(1)   = cz
         cz_sub(2:3) = 0.0d0

      else
         igrid   = network%adm%lin2grid(L)
         ibranch = network%adm%lin2ibr(L)
         chainage = network%brs%branch(ibranch)%uPointsChainages(network%adm%lin2local(L))
         conv = 0d0
         if (perim_sub(1)> 0.0d0) then
            do i = 1, 3
               if (perim_sub(i) > eps .and. flowarea_sub(i) > 0.0d0) then
                  r = flowarea_sub(i)/perim_sub(i)
                  cross => network%crs%cross(network%adm%line2cross(L, 2)%c1)%tabdef
                  cz1 = getFrictionValue(network%rgs, network%spdata, cross, ibranch, i, igrid, s1L, q1L, u1L, r, dpt, chainage)
                  cross => network%crs%cross(network%adm%line2cross(L, 2)%c2)%tabdef
                  cz2 = getFrictionValue(network%rgs, network%spdata, cross, ibranch, i, igrid, s1L, q1L, u1L, r, dpt, chainage)
                  ! Compute weighting of left and right cross section on this grid point.
                  ! Note: friction coefficient was already interpolated onto this grid point inside getFrictionValue.
                  f = network%adm%line2cross(L, 2)%f
                  cz_sub(i) = (1.0d0 - f) * cz1     + f * cz2
                  conv = conv + cz_sub(i) * flowarea_sub(i) * sqrt(flowarea_sub(i) / perim_sub(i))
               else
                  cz_sub(i) = 0
               endif
            enddo
         endif
         ! compute average chezy 
         if (flowArea > 1d-10) then
            cz = conv/(flowArea*sqrt(flowArea/wetPerimeter))
         else 
            cz = 0d0
         endif
         
      endif
      !        criteria to satisfy the criteria  in normup i.e cz(m)*cz(m)*wet
      if (cz * cz * flowArea < 1.0d0) then
         conv = sqrt(flowArea * flowArea / wetPerimeter)
      endif

   end subroutine getConveyance

   
   subroutine getCrossDischarge(perim_sub, flowarea_sub, cz_sub, q1_local, q_sub)
      ! Get discharges per reachsubsegment - based on plqsec() by J.Kuipers 
      implicit none
      double precision, dimension(3), intent(in) :: flowarea_sub, cz_sub, perim_sub
      double precision,               intent(in) :: q1_local
      double precision, dimension(3), intent(out) :: q_sub  
      double precision, dimension(3)             :: r_sub  
      integer                                    :: isec
      double precision                           :: acrtot
      double precision                           :: qacrtot
      double precision, parameter                :: eps_perim = 1d-3               ! accuracy wetted perimeter == 0d0
      double precision, parameter                :: eps_acrtot = 1d-10            ! accuracy weighted area parameter == 0d0
      double precision, parameter                :: eps_area = 1d-6               ! accuracy area == 0d0
      
      acrtot = 0.d0
      do isec = 1,3
          if (perim_sub(isec) > eps_perim) then
              r_sub(isec) = flowarea_sub(isec)/perim_sub(isec)
              acrtot = acrtot+flowarea_sub(isec)*cz_sub(isec) * sqrt(r_sub(isec))
          endif
      enddo
      if (acrtot > eps_acrtot) then
         qacrtot = q1_local/acrtot
         q_sub = 0.d0
         do isec = 1, 3
            if (flowarea_sub(isec) > eps_area) then
               q_sub(isec) = flowarea_sub(isec)*cz_sub(isec) *sqrt(r_sub(isec))*qacrtot
            endif
         enddo
      endif      
   end subroutine getCrossDischarge
   
   
end module m_cross_helper
