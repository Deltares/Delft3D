subroutine PER_GRIDgeometry(gdp)
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
!   Function:  extrapolate time-independent GRID property at periodic boundary 
!               
!    Author: Alberto Canestrelli
!
!!--declarations----------------------------------------------------------------
!
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                 , pointer :: nrPER
    real(fp), dimension(:,:), pointer :: xG
    real(fp), dimension(:,:), pointer :: xcorU1
    real(fp), dimension(:,:), pointer :: xcorV1
    real(fp), dimension(:,:), pointer :: yG
    real(fp), dimension(:,:), pointer :: ycorU1
    real(fp), dimension(:,:), pointer :: ycorV1
    real(fp), dimension(:,:), pointer :: PSIx
    real(fp), dimension(:,:), pointer :: PSIy
    real(fp), dimension(:,:), pointer :: ETAx
    real(fp), dimension(:,:), pointer :: ETAy
    integer, dimension(:)   , pointer :: mPH_ext
    integer, dimension(:)   , pointer :: nPH_ext
    integer, dimension(:)   , pointer :: mPQ_ext
    integer, dimension(:)   , pointer :: nPQ_ext
    integer, dimension(:)   , pointer :: mPH_int
    integer, dimension(:)   , pointer :: mPQ_int
    integer, dimension(:)   , pointer :: mPH_intint
    integer, dimension(:)   , pointer :: mPQ_intint
    integer, dimension(:)   , pointer :: nPH_int
    integer, dimension(:)   , pointer :: nPQ_int
    integer, dimension(:)   , pointer :: nPH_intint
    integer, dimension(:)   , pointer :: nPQ_intint
    integer, dimension(:)   , pointer :: mPH_extext
    integer, dimension(:)   , pointer :: mPQ_extext
    integer, dimension(:)   , pointer :: nPH_extext
    integer, dimension(:)   , pointer :: nPQ_extext
    real(fp), dimension(:,:), pointer :: dpsi
    real(fp), dimension(:,:), pointer :: deta
    logical                 , pointer :: perCIRC
    integer                 , pointer :: shiftHM
    integer                 , pointer :: shiftHN
    integer                 , pointer :: shiftQM
    integer                 , pointer :: shiftQN
    logical                 , pointer :: twoCELLSperiod
    real(fp), dimension(:,:), pointer :: xcor0
    real(fp), dimension(:,:), pointer :: ycor0
!
! global variables
! 
!
! local variables
!
  integer                    :: k 
!
! executable statements -------------------------------------------------------
!
    nrPER          => gdp%gdimbound%nrPER
    xG             => gdp%gdimbound%xG
    xcorU1         => gdp%gdimbound%xcorU1
    xcorV1         => gdp%gdimbound%xcorV1
    yG             => gdp%gdimbound%yG
    ycorU1         => gdp%gdimbound%ycorU1
    ycorV1         => gdp%gdimbound%ycorV1
    PSIx           => gdp%gdimbound%PSIx
    PSIy           => gdp%gdimbound%PSIy
    ETAx           => gdp%gdimbound%ETAx
    ETAy           => gdp%gdimbound%ETAy
    mPH_ext        => gdp%gdimbound%mPH_ext
    nPH_ext        => gdp%gdimbound%nPH_ext
    mPQ_ext        => gdp%gdimbound%mPQ_ext
    nPQ_ext        => gdp%gdimbound%nPQ_ext
    mPH_int        => gdp%gdimbound%mPH_int
    mPQ_int        => gdp%gdimbound%mPQ_int
    mPH_intint     => gdp%gdimbound%mPH_intint
    mPQ_intint     => gdp%gdimbound%mPQ_intint
    nPH_int        => gdp%gdimbound%nPH_int
    nPQ_int        => gdp%gdimbound%nPQ_int
    nPH_intint     => gdp%gdimbound%nPH_intint
    nPQ_intint     => gdp%gdimbound%nPQ_intint
    mPH_extext     => gdp%gdimbound%mPH_extext
    mPH_ext        => gdp%gdimbound%mPH_ext
    mPQ_extext     => gdp%gdimbound%mPQ_extext
    nPH_extext     => gdp%gdimbound%nPH_extext
    nPQ_extext     => gdp%gdimbound%nPQ_extext
    dpsi           => gdp%gdimbound%dpsi
    deta           => gdp%gdimbound%deta
    perCIRC        => gdp%gdimbound%perCIRC
    shiftHM        => gdp%gdimbound%shiftHM
    shiftHN        => gdp%gdimbound%shiftHN
    shiftQM        => gdp%gdimbound%shiftQM
    shiftQN        => gdp%gdimbound%shiftQN
    twoCELLSperiod => gdp%gdimbound%twoCELLSperiod
    xcor0          => gdp%gdimbound%xcor0
    ycor0          => gdp%gdimbound%ycor0
      do k=0,nrPER+1  !increased of 1 both side. Note that xcorU1 and xcorV1 are shifted of 1
         !
         ! note: in the case of a straight channel nothing has to be done. In the circular cartesian case, LchanPERprojX and LchanPERprojY are zero and with the following I have the right values in kcs==2
         !
         if (perCIRC) then
            xG    (nPQ_ext(k)          ,mPQ_ext(k))           = xG    (nPH_int(k)          ,mPH_int(k))           !- LchanPERprojX 
            yG    (nPQ_ext(k)          ,mPQ_ext(k))           = yG    (nPH_int(k)          ,mPH_int(k))           !- LchanPERprojY
            !note: here shiftHM and shiftQM are inverted for xcor0 and ycor0 (compared to what i do with the u velcity)
            ! for xcorU1,ycorU1 it always uses shiftHM (not sure it works with periodic Bc along y)
            xcor0 (nPQ_ext(k)+shiftHN  ,mPQ_ext(k)+shiftHM)   = xcor0 (nPH_int(k)+shiftHN  ,mPH_int(k)+shiftHM)   !- LchanPERprojX !xcorU1/ycorU1 are shifted (location of v(n,m) is at xcorU1(n,m-1),ycor(n,m-1)
            ycor0 (nPQ_ext(k)+shiftHN  ,mPQ_ext(k)+shiftHM)   = ycor0 (nPH_int(k)+shiftHN  ,mPH_int(k)+shiftHM)
            xcorU1(nPQ_ext(k)+shiftHN  ,mPQ_ext(k)-1+shiftHM)   = xcorU1(nPH_int(k)+shiftHN  ,mPH_int(k)-1+shiftHM)   !- LchanPERprojX !xcorU1/ycorU1 are shifted (location of v(n,m) is at xcorU1(n,m-1),ycor(n,m-1)
            ycorU1(nPQ_ext(k)+shiftHN  ,mPQ_ext(k)-1+shiftHM)   = ycorU1(nPH_int(k)+shiftHN  ,mPH_int(k)-1+shiftHM)   !- LchanPERprojY
            xcorV1(nPQ_ext(k)-1+shiftHN  ,mPQ_ext(k)+shiftHM)   = xcorV1(nPH_int(k)-1+shiftHN  ,mPH_int(k)+shiftHM)   !- LchanPERprojX !xcorV1/ycorV1 are shifted (location of u(n,m) is at xcorV1(n-1,m),ycor(n-1,m)
            ycorV1(nPQ_ext(k)-1+shiftHN  ,mPQ_ext(k)+shiftHM)   = ycorV1(nPH_int(k)-1+shiftHN  ,mPH_int(k)+shiftHM)   !- LchanPERprojY
            xG    (nPH_ext(k)          ,mPH_ext(k))           = xG    (nPQ_int(k)          ,mPQ_int(k))           !+ LchanPERprojX
            yG    (nPH_ext(k)          ,mPH_ext(k))           = yG    (nPQ_int(k)          ,mPQ_int(k))           !+ LchanPERprojY 
            xcor0 (nPH_ext(k)+shiftQN  ,mPH_ext(k)+shiftQM)   = xcor0 (nPQ_int(k)+shiftQN  ,mPQ_int(k)+shiftQM) 
            ycor0 (nPH_ext(k)+shiftQN  ,mPH_ext(k)+shiftQM)   = ycor0 (nPQ_int(k)+shiftQN  ,mPQ_int(k)+shiftQM) 
            xcorU1(nPH_ext(k)+shiftHN  ,mPH_ext(k)-1+shiftHM)   = xcorU1(nPQ_int(k)+shiftHN  ,mPQ_int(k)-1+shiftHM)   !+ LchanPERprojX
            ycorU1(nPH_ext(k)+shiftHN  ,mPH_ext(k)-1+shiftHM)   = ycorU1(nPQ_int(k)+shiftHN  ,mPQ_int(k)-1+shiftHM)   !+ LchanPERprojY 
            xcorV1(nPH_ext(k)-1+shiftQN  ,mPH_ext(k)+shiftQM)   = xcorV1(nPQ_int(k)-1+shiftQN  ,mPQ_int(k)+shiftQM)   !+ LchanPERprojX 
            ycorV1(nPH_ext(k)-1+shiftQN  ,mPH_ext(k)+shiftQM)   = ycorV1(nPQ_int(k)-1+shiftQN  ,mPQ_int(k)+shiftQM)   !+ LchanPERprojY 
         endif
!
         PSIx(nPQ_ext(k),mPQ_ext(k)) = PSIx(nPH_int(k),mPH_int(k))      
         PSIy(nPQ_ext(k),mPQ_ext(k)) = PSIy(nPH_int(k),mPH_int(k))   
         ETAx(nPH_ext(k),mPH_ext(k)) = ETAx(nPQ_int(k),mPQ_int(k))      
         ETAy(nPH_ext(k),mPH_ext(k)) = ETAy(nPQ_int(k),mPQ_int(k)) 
!
         dpsi(nPQ_ext(k),mPQ_ext(k)) = dpsi(nPH_int(k),mPH_int(k)) 
         deta(nPQ_ext(k),mPQ_ext(k)) = deta(nPH_int(k),mPH_int(k))
         dpsi(nPH_ext(k),mPH_ext(k)) = dpsi(nPQ_int(k),mPQ_int(k))
         deta(nPH_ext(k),mPH_ext(k)) = deta(nPQ_int(k),mPQ_int(k))
         ! compute DpsiG in the internal cell first (its wrong) then make it periodic
!        note: in the case of a straight channel nothing has to be done. In the circular cartesian case, LchanPERprojX and LchanPERprojY are zero and with the following I have the right values in kcs==2
         !I dont think its necesary to do stuff wiht DpsiG and DetaG.because for the periodic straight channel they are not used
         ! Only concern:anular periodi cartesian: parker and young can give wrong values in the halo(i dont think NaN maybe just wrong value since dpsiG should be just halved cause of zero area cells)
         ! but then the normals are made pariodic so I should be fine
         !
         !
!            n = nPH_int(k)
!            m = mPH_int(k)
!            DpsiG(n,m) = sqrt((xG(n,m+1)-xG(n,m))**2+(yG(n,m+1)-yG(n,m))**2) !m+1 is ok since xG and yG have already made periodic
!            DetaG(n,m)  = sqrt((xG(n+1,m)-xG(n,m))**2+(yG(n+1,m)-yG(n,m))**2) !n+1 is ok since xG and yG have already made periodic
!            n = nPQ_int(k)
!            m = mPQ_int(k)
!            DpsiG(n,m) = sqrt((xG(n,m+1)-xG(n,m))**2+(yG(n,m+1)-yG(n,m))**2) !m+1 is ok since xG and yG have already made periodic
!            DetaG(n,m)  = sqrt((xG(n+1,m)-xG(n,m))**2+(yG(n+1,m)-yG(n,m))**2) !n+1 is ok since xG and yG have already made periodic
  
!
         if (twoCELLSperiod) then
            if (perCIRC) then
               xG    (nPQ_extext(k)          ,mPQ_extext(k))           = xG    (nPH_intint(k)          ,mPH_intint(k))           !- LchanPERprojX 
               yG    (nPQ_extext(k)          ,mPQ_extext(k))           = yG    (nPH_intint(k)          ,mPH_intint(k))           !- LchanPERprojY
               xcorU1(nPQ_extext(k)+shiftQN  ,mPQ_extext(k)+shiftQM-1) = xcorU1(nPH_intint(k)+shiftQN  ,mPH_intint(k)+shiftQM-1) !- LchanPERprojX !xcorU1/ycorU1 are shifted (location of v(n,m) is at xcorU1(n,m-1),ycor(n,m-1)
               ycorU1(nPQ_extext(k)+shiftQN  ,mPQ_extext(k)+shiftQM-1) = ycorU1(nPH_intint(k)+shiftQN  ,mPH_intint(k)+shiftQM-1) !- LchanPERprojY
            !   xcor0 (nPQ_extext(k)+shiftQN  ,mPQ_extext(k)+shiftQM-1) = xcor0 (nPH_intint(k)+shiftQN  ,mPH_intint(k)+shiftQM-1) !- LchanPERprojX !xcorU1/ycorU1 are shifted (location of v(n,m) is at xcorU1(n,m-1),ycor(n,m-1)
            !   ycor0 (nPQ_extext(k)+shiftQN  ,mPQ_extext(k)+shiftQM-1) = ycor0 (nPH_intint(k)+shiftQN  ,mPH_intint(k)+shiftQM-1) !- LchanPERprojY
             !  xcorV1(nPQ_extext(k)+shiftQN-1,mPQ_extext(k)+shiftQM)   = xcorV1(nPH_intint(k)+shiftQN-1,mPH_intint(k)+shiftQM)   !- LchanPERprojX !xcorV1/ycorV1 are shifted (location of u(n,m) is at xcorV1(n-1,m),ycor(n-1,m)
             !  ycorV1(nPQ_extext(k)+shiftQN-1,mPQ_extext(k)+shiftQM)   = ycorV1(nPH_intint(k)+shiftQN-1,mPH_intint(k)+shiftQM)   !- LchanPERprojY
               xG    (nPH_extext(k)          ,mPH_extext(k))           = xG    (nPQ_intint(k)          ,mPQ_intint(k))           !+ LchanPERprojX
               yG    (nPH_extext(k)          ,mPH_extext(k))           = yG    (nPQ_intint(k)          ,mPQ_intint(k))           !+ LchanPERprojY 
               xcorU1(nPH_extext(k)+shiftHN  ,mPH_extext(k)+shiftHM-1) = xcorU1(nPQ_intint(k)+shiftHN  ,mPQ_intint(k)+shiftHM-1) !+ LchanPERprojX
               ycorU1(nPH_extext(k)+shiftHN  ,mPH_extext(k)+shiftHM-1) = ycorU1(nPQ_intint(k)+shiftHN  ,mPQ_intint(k)+shiftHM-1) !+ LchanPERprojY 
            !   xcorV1(nPH_extext(k)+shiftHN-1,mPH_extext(k)+shiftHM)   = xcorV1(nPQ_intint(k)+shiftHN-1,mPQ_intint(k)+shiftHM)   !+ LchanPERprojX 
            !   ycorV1(nPH_extext(k)+shiftHN-1,mPH_extext(k)+shiftHM)   = ycorV1(nPQ_intint(k)+shiftHN-1,mPQ_intint(k)+shiftHM)   !+ LchanPERprojY     
             !  xcor0 (nPH_extext(k)+shiftHN  ,mPH_extext(k)+shiftHM-1) = xcor0 (nPQ_intint(k)+shiftHN  ,mPQ_intint(k)+shiftHM-1) !+ LchanPERprojX
             !  ycor0 (nPH_extext(k)+shiftHN  ,mPH_extext(k)+shiftHM-1) = ycor0 (nPQ_intint(k)+shiftHN  ,mPQ_intint(k)+shiftHM-1) !+ LchanPERprojY                          
            endif
   !
            PSIx(nPQ_extext(k),mPQ_extext(k)) = PSIx(nPH_intint(k),mPH_intint(k))      
            PSIy(nPQ_extext(k),mPQ_extext(k)) = PSIy(nPH_intint(k),mPH_intint(k))   
            ETAx(nPH_extext(k),mPH_extext(k)) = ETAx(nPQ_intint(k),mPQ_intint(k))      
            ETAy(nPH_extext(k),mPH_extext(k)) = ETAy(nPQ_intint(k),mPQ_intint(k)) 
            
            dpsi(nPQ_extext(k),mPQ_extext(k)) = dpsi(nPH_intint(k),mPH_intint(k)) !these should not be used ever
            deta(nPQ_extext(k),mPQ_extext(k)) = deta(nPH_intint(k),mPH_intint(k))
            dpsi(nPH_extext(k),mPH_extext(k)) = dpsi(nPQ_intint(k),mPQ_intint(k))
            deta(nPH_extext(k),mPH_extext(k)) = deta(nPQ_intint(k),mPQ_intint(k))
         endif
!         
      enddo

      !LOWER VERTICES !(to be removed I added it above)
      k = nrPER+1
      if (perCIRC) then 
         xcor0 (nPQ_ext(k)+shiftHN  ,mPQ_ext(k)+shiftHM)   = xcor0 (nPH_int(k)+shiftHN  ,mPH_int(k)+shiftHM)   !- LchanPERprojX !xcorU1/ycorU1 are shifted (location of v(n,m) is at xcorU1(n,m-1),ycor(n,m-1)
         ycor0 (nPQ_ext(k)+shiftHN  ,mPQ_ext(k)+shiftHM)   = ycor0 (nPH_int(k)+shiftHN  ,mPH_int(k)+shiftHM)          
         xcor0 (nPH_ext(k)+shiftQN  ,mPH_ext(k)+shiftQM)   = xcor0 (nPQ_int(k)+shiftQN  ,mPQ_int(k)+shiftQM) 
         ycor0 (nPH_ext(k)+shiftQN  ,mPH_ext(k)+shiftQM)   = ycor0 (nPQ_int(k)+shiftQN  ,mPQ_int(k)+shiftQM)
      endif
      if (twoCELLSperiod) then
         if (perCIRC) then           
          !  xcor0 (nPQ_extext(k)+shiftQN  ,mPQ_extext(k)+shiftQM-1) = xcor0 (nPH_intint(k)+shiftQN  ,mPH_intint(k)+shiftQM-1) !- LchanPERprojX !xcorU1/ycorU1 are shifted (location of v(n,m) is at xcorU1(n,m-1),ycor(n,m-1)
          !  ycor0 (nPQ_extext(k)+shiftQN  ,mPQ_extext(k)+shiftQM-1) = ycor0 (nPH_intint(k)+shiftQN  ,mPH_intint(k)+shiftQM-1) !- LchanPERprojY          
          !  xcor0 (nPH_extext(k)+shiftHN  ,mPH_extext(k)+shiftHM-1) = xcor0 (nPQ_intint(k)+shiftHN  ,mPQ_intint(k)+shiftHM-1) !+ LchanPERprojX
          !  ycor0 (nPH_extext(k)+shiftHN  ,mPH_extext(k)+shiftHM-1) = ycor0 (nPQ_intint(k)+shiftHN  ,mPQ_intint(k)+shiftHM-1) !+ LchanPERprojY                          
         endif
      endif
!
RETURN
end subroutine PER_GRIDgeometry
