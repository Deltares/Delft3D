subroutine ua_vt(facas,      facsk,        sws,      h,   &
               & hrms,       tp,           ag,       urms,&
               & ua)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
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
!!--description-----------------------------------------------------------------
! computes velocity asymmetry due to waves according to
! PhD van Thiel 2009
! -
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
   use precision
   use mathconsts
   !
   implicit none
   !
   include 'RF.inc'
   !
   real(fp), intent(in)     :: facas
   real(fp), intent(in)     :: facsk
   integer, intent(in)      :: sws
   real(fp), intent(in)     :: h
   real(fp), intent(in)     :: hrms
   real(fp), intent(in)     :: tp
   real(fp), intent(in)     :: ag
   real(fp), intent(in)     :: urms
   real(fp), intent(out)    :: ua
   !
   ! Locals
   !                             
   integer(fp)              :: nh
   integer(fp)              :: nt
   integer(fp)              :: ih0
   integer(fp)              :: ih1
   integer(fp)              :: it0
   integer(fp)              :: it1
   real(fp)                 :: dh
   real(fp)                 :: dt
   real(fp)                 :: p
   real(fp)                 :: q
   real(fp)                 :: h0
   real(fp)                 :: t0
   real(fp)                 :: sk
   real(fp)                 :: as
   real(fp)                 :: f0, f1, f2, f3
   !
   dh = 0.03_fp
   dt = 1.25_fp
   nh = floor(0.99_fp/dh)
   nt = floor(50.0_fp/dt)

   ! compute dimensionless wave height and wave period in each grid point..
   h0 = min(nh*dh,max(dh,min(hrms,h)/max(h,1e-5_fp)))
   t0 = min(nt*dt,max(dt,tp*sqrt(ag/max(h,1e-5_fp))))       ! xb uses trep here

   ! estimate Sk, As and ua by interpolating table values
   ih0=floor(h0/dh)
   it0=floor(t0/dt)
   ih1=min(ih0+1,nh)
   it1=min(it0+1,nt)
   p=(h0-ih0*dh)/dh
   q=(t0-it0*dt)/dt

   f0=(1-p)*(1-q)
   f1=p*(1-q)
   f2=q*(1-p)
   f3=p*q
   !
   ! Skewness and asymmetry
   sk = f0*RF(1,ih0,it0)+f1*RF(1,ih1,it0)+ f2*RF(1,ih0,it1)+f3*RF(1,ih1,it1)
   as = f0*RF(2,ih0,it0)+f1*RF(2,ih1,it0)+ f2*RF(2,ih0,it1)+f3*RF(2,ih1,it1)
   !
   ! Sediment advection velocity from Skewness and Asymmetry
   ua = sws*(facsk*sk-facas*as)*urms   ! urms, not uorb
   
end subroutine ua_vt