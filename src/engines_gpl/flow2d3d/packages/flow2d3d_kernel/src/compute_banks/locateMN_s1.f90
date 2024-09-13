subroutine locateMN_s1(m,n,Npsi,Neta,x,y,mOUT,nOUT,mmax,nmaxus, gdp)  
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
!   Function:   Finds the location (mOUT,nOUT) of a given point (x,y) given a initial SURFACE cell (n,m)
!               and a search direction (vector). The search is performed  in the dp grid 
!               (i.e. the grid having as cell centers the dp points, and as nodes the surface points.
!               
!!--declarations----------------------------------------------------------------
!
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    real(fp), dimension(:,:), pointer :: PSIx
    real(fp), dimension(:,:), pointer :: PSIy
    real(fp), dimension(:,:), pointer :: ETAx
    real(fp), dimension(:,:), pointer :: ETAy
    real(fp), dimension(:,:), pointer :: psiG
    real(fp), dimension(:,:), pointer :: etaG
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
    PSIx => gdp%gdimbound%PSIx
    PSIy => gdp%gdimbound%PSIy
    ETAx => gdp%gdimbound%ETAx
    ETAy => gdp%gdimbound%ETAy
    psiG => gdp%gdimbound%psiG
    etaG => gdp%gdimbound%etaG
   signNPSIreal = sign(1._fp,Npsi) !-Npsi, I go opposite to the normal (toward the wet region)
   signNETAreal = sign(1._fp,Neta) !-Neta, I go opposite to the normal (toward the wet region)
   signNPSIint = NINT(signNPSIreal)
   signNETAint = NINT(signNETAreal)    
   psi =  (x*PSIx(2,2)+y*PSIy(2,2))*signNPSIreal     
   eta =  (x*ETAx(2,2)+y*ETAy(2,2))*signNETAreal
!
!  search along psi
!
   if (comparereal(Npsi,0._fp).ne.0) then  !note I had to put k=signNPSIint,... instead of k=0,... cause otherwise when porosity =0.5 it could happened once that the point was a ghost point and here it was found in the dry dp-centered cell instead of the wet part.
      do k=signNPSIint,2000*signNPSIint,signNPSIint !0-max(+signNPSIint,0),2000*signNPSIint,signNPSIint !2000 is a high value never reached.   it actually first want to check in the cell itself,so I start from 0-max(signNPSIint,0)
         mFAR   = m+k                    ! mFAR is the value of m far from mBI
         mCLOSE = m+k-signNPSIint        ! mCLOSE is the value of m close to mBI
         if (max(mFAR,mCLOSE).gt.mmax.or.min(mFAR,mCLOSE).lt.1) then
            write(*,*) 'search in locateMN_s1 failed at (n,m)=',n,m
            call d3stop(1, gdp)
         endif
         if ((comparereal(psi,psiG(n,mFAR)*signNPSIreal).le.0).and.(comparereal(psi,psiG(n,mCLOSE)*signNPSIreal).ge.0)) then
            if (signNPSIint.gt.0) then
               mOUT = mFAR
            else
               mOUT = mCLOSE
            endif
            exit
         endif
      enddo
   else
      mOUT = m
   endif
!
!  search along eta
!
   if (comparereal(Neta,0._fp).ne.0) then  
      do k=signNETAint,2000*signNETAint,signNETAint  !0-max(+signNETAint,0),2000*signNETAint,signNETAint !2000 is a high value never reached. When signNETAint negative it actually first check the cell itself, it is ok although it is extra work it will never find it there
         nFAR   = n+k                    ! nFAR is the value of n far from nBI
         nCLOSE = n+k-signNETAint        ! nCLOSE is the value of n close to nBI
         if (max(nFAR,nCLOSE).gt.nmaxus.or.min(nFAR,nCLOSE).lt.1) then
            write(*,*) 'search in locateMN_s1 failed at (n,m)=',n,m
            call d3stop(1, gdp)
         endif
         if ((comparereal(eta,etaG(nFAR,m)*signNETAreal).le.0).and.(comparereal(eta,etaG(nCLOSE,m)*signNETAreal).ge.0)) then
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
