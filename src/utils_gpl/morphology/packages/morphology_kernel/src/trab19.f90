!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
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
!> computes sediment transport according to the transport formula of Van Thiel / Van Rijn (2008)
subroutine trab19(u         ,v         ,hrms      ,rlabda    ,teta      ,h         ,tp        , &
                & d50       ,d15       ,d90       ,npar      ,par       ,dzbdt     ,vicmol    , &
                & poros     ,chezy     ,dzdx      ,dzdy      ,sbotx     ,sboty     ,cesus     , &
                & ua        ,va        ,ubot      ,kwtur     ,ubot_from_com )
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    use sed_support_routines, only: calculate_critical_velocities, calculate_velocity_asymmetry, calculate_urms
    !
    implicit none
!
! Arguments
!
    logical                  , intent(in)    :: ubot_from_com
    integer                  , intent(in)    :: npar
    real(fp)                 , intent(in)    :: chezy
    real(fp)                 , intent(in)    :: d15
    real(fp)                 , intent(in)    :: d50
    real(fp)                 , intent(in)    :: d90
    real(fp)                 , intent(in)    :: dzbdt    !<  Erosion/sedimentation velocity
    real(fp)                 , intent(in)    :: dzdx   
    real(fp)                 , intent(in)    :: dzdy   
    real(fp)                                 :: h
    real(fp)                                 :: hrms
    real(fp)                 , intent(in)    :: kwtur    !<  Breaker induced turbulence
    real(fp), dimension(npar), intent(in)    :: par
    real(fp)                 , intent(in)    :: poros
    real(fp)                 , intent(in)    :: rlabda   
    real(fp)                 , intent(in)    :: teta     
    real(fp)                                 :: tp     
    real(fp)                 , intent(in)    :: ubot   
    real(fp)                 , intent(in)    :: u
    real(fp)                 , intent(in)    :: v
    real(fp)                 , intent(in)    :: vicmol
    !
    real(fp)                 , intent(out)   :: sbotx
    real(fp)                 , intent(out)   :: sboty
    real(fp)                 , intent(out)   :: cesus
    real(fp)                 , intent(out)   :: ua
    real(fp)                 , intent(out)   :: va
    !
    ! Local variables
    !
    real(fp), parameter            :: DTOL = 1e-6_fp
    real(fp), parameter            :: ONETHIRD = 1.0_fp/3.0_fp
    
    integer                        :: waveform
    integer                        :: dilatancy
    integer                        :: sws
    integer                        :: lws
    integer                        :: bedslpeffini
    real(fp)                       :: ag
    real(fp)                       :: delta
    real(fp)                       :: facua
    real(fp)                       :: facas
    real(fp)                       :: facsk
    real(fp)                       :: smax
    real(fp)                       :: pormax
    real(fp)                       :: cmax
    real(fp)                       :: reposeangle
    real(fp)                       :: rheea
    real(fp)                       :: cf 
    real(fp)                       :: utot   
    real(fp)                       :: uamag   
    real(fp)                       :: phi      
    real(fp)                       :: b2   
    real(fp)                       :: ucrw  
    real(fp)                       :: ucrc   
    real(fp)                       :: dster   
    real(fp)                       :: ucr
    real(fp)                       :: urms       
    real(fp)                       :: urms2
    real(fp)                       :: ucrb, ucrs, asb, ass, term1, ceqb, ceqs
    real(fp)                       :: cmax2h
    !
    !
    !! executable statements -------------------------------------------------------
    !
    !
    !     Initialize Transports to zero
    !
    sbotx = 0.0_fp
    sboty = 0.0_fp
    ua    = 0.0_fp
    va    = 0.0_fp
    cesus = 0.0_fp
    utot = sqrt(u**2 + v**2)
    if ( utot < DTOL .or. h > 200.0_fp .or. h < 0.01_fp ) return
    !
    !     Initialisations
    ag = par(1)
    delta = par(4)
    facua = par(11)
    facas = par(12)
    facsk = par(13)
    waveform = int(par(14))
    sws = int(par(15))
    lws = int(par(16))
    dilatancy = int(par(17))
    rheea = par(18)
    pormax = par(19)
    bedslpeffini = int(par(20))
    smax = par(21)
    reposeangle = par(22)
    cmax = par(23)
    !
    ! limit input parameters to sensible values
    !
    facua = max(min(facua,1.0_fp),0.0_fp)
    facas = max(min(facas,1.0_fp),0.0_fp)
    facsk = max(min(facsk,1.0_fp),0.0_fp)
    if (.not. (waveform==1 .or. waveform==2)) waveform=2            ! van Thiel default
    if (.not. (lws==1)) lws = 1                                     ! default always on, not used now (don't have relaxation parameters)
    if (.not. (sws==1 .or. sws==0)) sws = 1                         ! default on
    if (.not. (dilatancy==1 .or. dilatancy==0)) dilatancy = 0       ! default off
    rheea = max(min(rheea,2.0_fp),0.75_fp)
    pormax = max(min(pormax,0.6_fp),poros)
    if (.not. (bedslpeffini==0 .or. bedslpeffini==1 .or. bedslpeffini==2)) bedslpeffini=0       
    smax = max(min(smax,3.0_fp),-1.0_fp)
    if (smax<0.0_fp) smax=huge(0.0_fp)*1.0e-20_fp
    reposeangle = max(min(reposeangle,45.0_fp),30.0_fp)
    cmax = max(min(cmax,1.0_fp),0.0_fp)
    !
    cf = ag / chezy / chezy
    !
    call calculate_urms(hrms, tp, h, ag, ubot_from_com, ubot, kwtur, urms, urms2)
    !
    ! velocity asymmetry
    !
    call calculate_velocity_asymmetry(waveform, facas, facsk, sws, h, hrms, rlabda, ag, tp, urms, uamag)
    !
    !     Velocity magnitude
    !
    phi = reposeangle*degrad ! Angle of internal friction
    dster=(delta*ag/1e-12_fp)**onethird*d50        ! 1e-12 = nu**2
    !
    if(d50<=0.0005_fp) then
       Ucrc=0.19_fp*d50**0.1_fp*log10(4.0_fp*h/d90)                           !Shields
       Ucrw=0.24_fp*(delta*ag)**0.66_fp*d50**0.33_fp*tp**0.33_fp              !Komar and Miller (1975)
    else if(d50<=0.002_fp) then
       Ucrc=8.5_fp*d50**0.6_fp*log10(4.0_fp*h/d90)                            !Shields
       Ucrw=0.95_fp*(delta*ag)**0.57_fp*d50**0.43_fp*tp**0.14_fp                  !Komar and Miller (1975)
    else if(d50>0.002_fp) then
       Ucrc=1.3_fp*sqrt(delta*ag*d50)*(h/d50)**(0.5_fp*onethird)            !Maynord (1978) --> also Neill (1968) where 1.3_fp = 1.4_fp
       Ucrw=0.95_fp*(delta*ag)**0.57_fp*d50**0.43_fp*tp**0.14_fp                  !Komar and Miller (1975)
    end if
    B2 = utot/max(utot+sqrt(urms2),5e-3_fp)
    Ucr = B2*Ucrc + (1.0_fp-B2)*Ucrw                                           !Van Rijn 2007 (Bed load transport paper)
    !
    call calculate_critical_velocities(dilatancy, bedslpeffini, dzbdt, ag, vicmol, d15, poros, pormax, rheea, delta, u, v, &
    dzdx, dzdy, dtol, phi, ucr, ucrb, Ucrs)
   !
   ! transport parameters
   Asb=0.015_fp*h*(d50/h)**1.2_fp/(delta*ag*d50)**0.75_fp                         !bed load coefficent
   Ass=0.012_fp*d50*dster**(-0.6_fp)/(delta*ag*d50)**1.2_fp                       !suspended load coeffient
   !
   ! Van Rijn use Peak orbital flow velocity --> 0.64 corresponds to 0.4 coefficient regular waves Van Rijn (2007)
   term1=utot**2+0.64_fp*sws*urms2
   ! reduce sediment suspensions for (inundation) overwash conditions with critical flow velocities
   term1=min(term1,smax*ag/max(cf,1e-10_fp)*d50*delta)
   term1=sqrt(term1)
   !
   ceqb = 0.0_fp                                                                     !initialize ceqb
   ceqs = 0.0_fp                                                                     !initialize ceqs
   !
   if(term1>Ucrb .and. h>dtol) then
      ceqb=Asb*(term1-Ucrb)**1.5_fp
   end if
   if(term1>Ucrs .and. h>dtol) then
      ceqs=Ass*(term1-Ucrs)**2.4_fp
   end if
   !
   cmax2h = cmax*h/2.0_fp
   ceqb  = min(ceqb,   cmax2h)               ! maximum equilibrium bed concentration
   cesus = min(ceqs,   cmax2h)/h             ! m2/s/m*s/m = [-], and times rhosol in eqtran
   ua = uamag*cos(teta*degrad)
   va = uamag*sin(teta*degrad)
   sbotx = (u+ua)*ceqb                       ! m2/s
   sboty = (v+va)*ceqb
   !
  999 continue
end subroutine trab19
