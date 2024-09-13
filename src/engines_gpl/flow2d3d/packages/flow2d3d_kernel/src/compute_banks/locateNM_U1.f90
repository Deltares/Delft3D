subroutine locateMn_U1(m,n,Npsi,Neta,x,y,mOUT,nOUT,mmax,nmaxus, gdp)  
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
!               and a search direction (vector). The location is found on the grid for V1.
!               In fact V1 is at the  center of the cells, and the nodes are the locations of U1.
!               Accordingly, note that the location of the point is compared 
!               with PSIcorV1 and ETAcorV1 (that are the location of velocity U1, 
!               i.e. the grid points for the grid of U1) 
!               
!!--declarations----------------------------------------------------------------
!
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    real(fp), dimension(:,:), pointer :: PSIcorV1
    real(fp), dimension(:,:), pointer :: ETAcorV1
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
    PSIcorV1 => gdp%gdimbound%PSIcorV1
    ETAcorV1 => gdp%gdimbound%ETAcorV1
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
!  note: if I ever wanna do it for non uniform mesh, note that in the circular anular case 
!  the cell with zero area has dpsi and deta copied from the periodic cell. Think about it. alse psix and psiy are not made periodic
!  note also that xcor0 is made periodic, but  PSIcor0 and ETAcor0 are not since they are computed in PLIC_VOF_INIT with the xcor0,ycor0 of the zero area cell
   if (comparereal(Neta,0._fp).ne.0) then  
     ! do k=0-max(-signNETAint,0),2000*signNETAint,signNETAint
      do k=0-max(-2*signNETAint,0),2000*signNETAint,signNETAint !2000 is a high value never reached. When signNETAint negative it actually first check the cell itself, it is ok although it is extra work it will never find it there
         nFAR   = n+k                    ! nFAR is the value of n far from nBI
         nCLOSE = n+k-signNETAint        ! nCLOSE is the value of n close to nBI
         if (max(nFAR,nCLOSE).gt.nmaxus-1.or.min(nFAR,nCLOSE).lt.0) then
            write(*,*) 'search in locateMN_U1 failed at (n,m)=',n,m
            call d3stop(1, gdp)
         endif
         if ((comparereal(eta,ETAcorV1(nFAR,m)*signNETAreal).le.0).and.(comparereal(eta,ETAcorV1(nCLOSE,m)*signNETAreal).ge.0)) then
            if (signNETAint.gt.0) then
               nOUT = nFAR
            else
               nOUT = nCLOSE
            endif
            exit
         endif
      enddo
   else
      if (signNETAint.gt.0) then
         nOUT = n
      else
         nOUT = n-1
      endif
   endif
!
   if (comparereal(Npsi,0._fp).ne.0) then  
      do k=signNPSIint,2000*signNPSIint,signNPSIint !2000 is a high value never reached.   it actually first want to check in the cell itself,so I start from 0-max(signNPSIint,0)
         mFAR   = m+k                    ! mFAR is the value of m far from mBI
         mCLOSE = m+k-signNPSIint        ! mCLOSE is the value of m close to mBI
         if (max(mFAR,mCLOSE).gt.mmax.or.min(mFAR,mCLOSE).lt.1) then
            write(*,*) 'search in locateMN_u1 failed at (n,m)=',n,m
            call d3stop(1, gdp)
         endif
         if ((comparereal(psi,PSIcorV1(nOUT,mFAR)*signNPSIreal).le.0).and.(comparereal(psi,PSIcorV1(nOUT,mCLOSE)*signNPSIreal).ge.0)) then
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
RETURN
END
