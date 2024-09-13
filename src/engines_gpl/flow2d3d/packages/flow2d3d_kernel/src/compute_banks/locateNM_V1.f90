subroutine locateMN_V1(m,n,Npsi,Neta,x,y,mOUT,nOUT,mmax,nmaxus, gdp)  
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
!   Function:   Finds the location (mOUT,nOUT) of a given point given a initial SURFACE cell (n,m)
!               and a search direction (vector). The location is found on the grid for U1.
!               In fact U1 is at the  center of the cells, and the nodes are the locations of V1.
!               Accordingly, note that the location of the point is compared 
!               with PSIcorU1 and ETAcorU1 (that are the location of velocity V1, 
!               i.e. the grid points for the grid of V1) 
!               
!!--declarations----------------------------------------------------------------
!
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    real(fp), dimension(:,:), pointer :: PSIcorU1
    real(fp), dimension(:,:), pointer :: ETAcorU1
    real(fp), dimension(:,:), pointer :: PSIx
    real(fp), dimension(:,:), pointer :: PSIy
    real(fp), dimension(:,:), pointer :: ETAx
    real(fp), dimension(:,:), pointer :: ETAy
!
! global variables
!
!   (none)
!
! local variables
!
  integer, intent(OUT)       :: mOUT,nOUT 
  integer, intent(IN)        :: m,n
  integer, intent(IN)        :: mmax,nmaxus
  integer                    :: k
  integer                    :: mFAR,mCLOSE,nFAR,nCLOSE 
  integer                    :: signNPSIint,signNETAint
!
  real(fp), intent(IN)       :: Npsi,Neta,x,y
  real(fp)                   :: psi,eta
  real(fp)                   :: signNPSIreal,signNETAreal
!
! executable statements -------------------------------------------------------
! 
    PSIcorU1 => gdp%gdimbound%PSIcorU1
    ETAcorU1 => gdp%gdimbound%ETAcorU1
    PSIx     => gdp%gdimbound%PSIx
    PSIy     => gdp%gdimbound%PSIy
    ETAx     => gdp%gdimbound%ETAx
    ETAy     => gdp%gdimbound%ETAy
   signNPSIreal = sign(1._fp,Npsi)  
   signNETAreal = sign(1._fp,Neta)  
   signNPSIint = NINT(signNPSIreal)
   signNETAint = NINT(signNETAreal)    
   psi =  (x*PSIx(2,2)+y*PSIy(2,2))*signNPSIreal     
   eta =  (x*ETAx(2,2)+y*ETAy(2,2))*signNETAreal
!
   if (comparereal(Npsi,0._fp).ne.0) then  
         ! do k=0-max(-signNPSIint,0),2000*signNPSIint,signNPSIint 
       !  do k=-1,2000*signNPSIint,signNPSIint !2000 is a high value never reached.   it actually first want to check in the cell itself,so I start from 0-max(signNPSIint,0)
      do k=0-max(-2*signNPSIint,0),2000*signNPSIint,signNPSIint
!
         mFAR   = m+k                    ! mFAR is the value of m far from mBI
         mCLOSE = m+k-signNPSIint        ! mCLOSE is the value of m close to mBI
         if (max(mFAR,mCLOSE).gt.mmax-1.or.min(mFAR,mCLOSE).lt.0) then
            write(*,*) 'search in locateMN_v1 failed at (n,m)=',n,m
            call d3stop(1, gdp)
         endif
         if ((comparereal(psi,PSIcorU1(n,mFAR)*signNPSIreal).le.0).and.(comparereal(psi,PSIcorU1(n,mCLOSE)*signNPSIreal).ge.0)) then
            if (signNPSIint.gt.0) then
               mOUT = mFAR
            else
               mOUT = mCLOSE
            endif
            exit
         endif
      enddo
   else
      if (signNPSIint.gt.0) then
         mOUT = m
      else
         mOUT = m-1
      endif
   endif
!
!  search along eta
!
   if (comparereal(Neta,0._fp).ne.0) then
      do k=signNETAint,2000*signNETAint,signNETAint !2000 is a high value never reached. When signNETAint negative it actually first check the cell itself, it is ok although it is extra work it will never find it there
         nFAR   = n+k                    ! nFAR is the value of n far from nBI
         nCLOSE = n+k-signNETAint        ! nCLOSE is the value of n close to nBI
         if (max(nFAR,nCLOSE).gt.nmaxus.or.min(nFAR,nCLOSE).lt.1) then
            write(*,*) 'search in locateMN_v1 failed at (n,m)=',n,m
            call d3stop(1, gdp)
         endif
         if ((comparereal(eta,ETAcorU1(nFAR,mOUT)*signNETAreal).le.0).and.(comparereal(eta,ETAcorU1(nCLOSE,mOUT)*signNETAreal).ge.0)) then
            if (signNETAint.gt.0) then
               nOUT = nFAR
            else
               nOUT = nCLOSE
            endif
            exit
         endif
      enddo
   else
      nOUT = n
   endif
!
RETURN
END
