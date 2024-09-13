SUBROUTINE vvvORD2sub(vvv, v1, guu, gvv, kfv, nm, k, nst, icx, icy, nmlb, nmub, kmax, kfvTOT)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011.                                     
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
!  Ndryact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  $Id: My_intersec.f90 
!  $HeadURL:
!!--description-----------------------------------------------------------------
!
!   Function: Compute v at u point with second order also close to cut boundary. 
!             For TYPEfreeSLIP=0 and exact then v1inu1 could be used. 
!             But not for Hartmann 
!
!
!   Author: Alberto Canestrelli
!
!--declarations----------------------------------------------------------------
!
  use precision
!
  implicit none
!
! global variables
!
    real(fp), dimension(nmlb:nmub,kmax)                            , intent(in)    :: v1
    real(fp), dimension(nmlb:nmub)                                 , intent(in)    :: guu    
    real(fp), dimension(nmlb:nmub)                                 , intent(in)    :: gvv
    integer , dimension(nmlb:nmub)                                 , intent(in)    :: kfv    
    integer                                                        , intent(in)    :: nm    
    integer                                                        , intent(in)    :: nst
    integer                                                        , intent(in)    :: icx
    integer                                                        , intent(in)    :: icy
    integer                                                        , intent(in)    :: nmlb
    integer                                                        , intent(in)    :: nmub
    integer                                                        , intent(in)    :: kmax    
    integer                                                        , intent(in)    :: kfvTOT
    real(fp)                                                       , intent(OUT)   :: vvv
!
!
! local variables
!
  integer  :: kfABOVE
  integer  :: kfBELOW
  integer  :: kfRIGHT
  integer  :: kfLEFT
  integer  :: k
  integer  :: kDOWN
  integer  :: kUP
  integer  :: nmu 
  integer  :: nmuu  
  integer  :: ndm 
  integer  :: ndmu
  integer  :: nm1  
  integer  :: nm2  
  integer  :: nm3  
  integer  :: nm4   
  real(fp) :: len1
  real(fp) :: len2  
  real(fp) :: vvvUP
  real(fp) :: vvvDOWN  
  real(fp) :: vvv1
  real(fp) :: vvv2
!
! executable statements -------------------------------------------------------
!    
    nmu  = nm  + icx
    ndm  = nm  - icy
    ndmu = ndm + icx        
    if (kfvTOT==3) then
       kDOWN = 1
       kUP   = 1
       kfABOVE = kfv(nmu)+kfv(nm)
       if (kfABOVE==2) then ! two active u-points are above
          nm1 = nmu
          nm2 = nm   
          vvvUP    = (v1(nm1,k)+v1(nm2,k))*0.5_fp
       elseif (kfABOVE==1) then !1 active point below
          if (kfv(nmu)==1) then !one active above on the right
             nm3 = nmu
             nm4 = nm3+icx
          else !if (kfv(nm))==1) then !one active above on the left
             nm3 = nm
             nm4 = nm3-icx 
          endif 
          if (kfv(nm4)==0) then
             write(*,*) 'case for vvvSECord not implemented/considered' 
             !pause !to be commented, it is fine to continue it is only first order
             kUP = 0
          else
             len1 = gvv(nm3)*0.5_fp
             len2 = len1 + gvv(nm4)*0.5_fp
             vvvUP = (v1(nm3,k)-v1(nm4,k))/len2*len1+v1(nm3,k)               
          endif
       endif
       kfBELOW = kfv(ndmu)+kfv(ndm)
       if (kfABOVE/=2.and.kfBELOW==2) then ! two active u-points are below
          nm1 = ndmu
          nm2 = ndm        
          vvvDOWN    = (v1(nm1,k)+v1(nm2,k))*0.5_fp               
       elseif (kfBELOW==1) then !1 active point below    
          if (kfv(ndmu)==1) then !one active below on the right
             nm3 = ndmu
             nm4 = nm3+icx     
          else !if (kfv(nmd))==1) then !one active below on the left
             nm3 = ndm
             nm4 = nm3-icx  
          endif            
          if (kfv(nm4)==0) then
             write(*,*) 'case for vvvSECord not implemented/considered' 
             !pause !to be commented, it is fine to continue it is only first order
             kDOWN = 0
          else
             len1 = gvv(nm3)*0.5_fp
             len2 = len1 + gvv(nm4)*0.5_fp
             vvvDOWN = (v1(nm3,k)-v1(nm4,k))/len2*len1+v1(nm3,k)               
          endif               
       endif          
       vvv = (kUP*vvvUP+kDOWN*vvvDOWN)/max(1,kDOWN+kUP)
       !    
    elseif (kfvTOT==2) then
       kfABOVE = kfv(nmu)  + kfv(nm)    
       kfBELOW = kfv(ndmu) + kfv(ndm)        
       kfRIGHT = kfv(nmu)  + kfv(ndmu)
       kfLEFT  = kfv(nm)   + kfv(ndm)       
       if (kfABOVE==2) then !water is above
          nm1  = nm
          nm2  = nm+icy     
          nm3  = nmu
          nm4  = nmu+icy                
          len1 = guu(nm1)*0.5_fp
          len2 = guu(nm2) 
          vvv1 = (v1(nm3,k)-v1(nm4,k))/len2*len1+v1(nm3,k)   
          vvv2 = (v1(nm1,k)-v1(nm2,k))/len2*len1+v1(nm1,k) 
          vvv  = (vvv1+vvv2)*0.5_fp
       elseif (kfBELOW==2) then !water is below
          nm1  = ndm
          nm2  = ndm - icy
          nm3  = ndmu
          nm4  = ndmu - icy
          len1 = guu(nm)*0.5_fp
          len2 = guu(ndm) 
          vvv1 = (v1(nm3,k)-v1(nm4,k))/len2*len1+v1(nm3,k)  
          vvv2 = (v1(nm1,k)-v1(nm2,k))/len2*len1+v1(nm1,k) 
          vvv  = (vvv1+vvv2)*0.5_fp 
       elseif (kfRIGHT==2) then !water is right
          nm1  = nmu
          nm2  = nmu + icx
          nm3  = ndmu
          nm4  = ndmu + icx
          len1 = gvv(nmu)*0.5_fp
          len2 = len1 + gvv(nm2)*0.5_fp
          vvv1 = (v1(nm3,k)-v1(nm4,k))/len2*len1+v1(nm3,k) 
          vvv2 = (v1(nm1,k)-v1(nm2,k))/len2*len1+v1(nm1,k) 
          vvv  = (vvv1+vvv2)*0.5_fp 
       elseif (kfLEFT==2) then !water is left
          nm1  = nm
          nm2  = nm - icx
          nm3  = ndm
          nm4  = ndm - icx
          len1 = gvv(nm)*0.5_fp
          len2 = gvv(nm-icx)*0.5_fp
          vvv1 = (v1(nm3,k)-v1(nm4,k))/len2*len1+v1(nm3,k) 
          vvv2 = (v1(nm1,k)-v1(nm2,k))/len2*len1+v1(nm1,k) 
          vvv  = (vvv1+vvv2)*0.5_fp 
       endif
    else
       write(*,*) 'case for vvvSECord not implemented/considered' 
       !pause !to be commented, it is fine to continue it is only first order
       vvv  = (  v1(ndm, k)*kfv(ndm) + v1(ndmu, k)*kfv(ndmu) &
            &  + v1(nm , k)*kfv(nm)  + v1(nmu , k)*kfv(nmu)   ) / kfvTOT 
    endif
    !         
end subroutine vvvORD2sub
