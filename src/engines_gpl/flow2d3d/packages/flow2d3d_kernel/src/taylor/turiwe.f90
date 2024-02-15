subroutine turiwe(kmxdt     ,kmxt      ,h0        ,scale     ,bvav      , &
                & xkh       ,utg       ,vtg       ,ratiow    ,kbed      , &
                & ktop      ,ktgcd     ,ktgcu     ,tke       ,zlw       , &
                & tlw       ,ttkiw     ,tiwtk     ,luniwe    ,gdp       )
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
!
!    Function: Transfer of turbulent kinetic energy into internal
!              wave energy; this transfer is the distributed loss
!              TTKIW for TKE.
!              The IW's return the depth integral of TTKIW to TKE
!              either by wave breaking or by wave-induced
!              shearing
!              Transfer rates are based on analytical expressions
!              for excitation of interfacial waves (density jump)
!
! Definitions: KTGCU is KTG-level of highest critical layer
!              (nearest to water surface).
!              KTGCD is KTG-level of lowest critical layer
!              (nearest to the bed).
!
!              Notice that the mechanism of this vertical re-
!              distribution of TKE is not diffusion but due to
!              vertical transport of IWE by its vertical group
!              velocity.
!              TTKIW is the distributed TKE dissipation in
!                                                    [m^2/s^3]
!              TIWTK is the distributed TKE production in
!                                                    [m^2/s^3].
!
!              In subsequent calls to TKEIWE within one call to
!              subroutine Taylor, all contributions are added to
!              respectively TTKIW or TIWTK.
!
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)               , pointer :: ag
    real(fp)               , pointer :: argsnh
    real(fp)               , pointer :: argexp
    real(fp)               , pointer :: epsbv2
    real(fp)               , pointer :: clu
    real(fp)               , pointer :: clw
    real(fp)               , pointer :: ckw
    logical                , pointer :: iwedia
!
! Global variables
!
    integer, intent(in)            :: kbed
                                   !!  KTG-value of upper level strat.
                                   !!  layer
    integer, intent(in)            :: kmxdt !  Description and declaration in dimens.igs
    integer, intent(in)            :: kmxt !  Description and declaration in dimens.igs
    integer, intent(in)            :: ktgcd
                                   !!  KTG-value of upper most critical
                                   !!  level
    integer, intent(in)            :: ktgcu
                                   !!  KTG-value of lowest critical level
    integer, intent(in)            :: ktop
                                   !!  KTG-value of lower level strat.
                                   !!  layer
    integer, intent(in)            :: luniwe
                                   !!  Unit number of IWE diagnostic file
                                   !!  Unit number for diagnostic reports of
                                   !!  this subprogram as well as called
                                   !!  subroutines
    real(fp), intent(in)               :: bvav
                                   !!  Mean bouyancy frequency
    real(fp), intent(in)               :: h0
                                   !!  Water depth
    real(fp), intent(in)               :: ratiow
                                   !!  Ratio between depth-averaged pot. and
                                   !!  kin. energy in internal waves and kin
                                   !!  energy of vertical motions
    real(fp), intent(in)               :: scale
                                   !!  Length scale for non-dimensionalising
    real(fp)        :: xkh
                                   !!  Wave number magnitude normalized by
                                   !!  SCALE
    real(fp), dimension(0:kmxdt) :: tiwtk
                                   !!  Transfer rate of IWE to TKE
    real(fp), dimension(0:kmxdt), intent(in) :: tke
                                   !!  Interpolated TKE
    real(fp), dimension(0:kmxdt), intent(in) :: tlw
                                   !!  Integral time scale of vertical
                                   !!  turbulent motions
    real(fp), dimension(0:kmxdt) :: ttkiw
                                   !!  Transfer rate of TKE to IWE
    real(fp), dimension(0:kmxdt), intent(in) :: utg
                                   !!  Interpolated u-velocity component
    real(fp), dimension(0:kmxdt), intent(in) :: vtg
                                   !!  Interpolated v-velocity component
    real(fp), dimension(0:kmxdt), intent(in) :: zlw
                                   !!  Integral length scale of turbulence
                                   !!  in z/w-direction
!
!
! Local variables
!
    integer                        :: ktg
    real(fp)                       :: delro
    real(fp)                       :: dh
    real(fp)                       :: dz
    real(fp)                       :: dztg
    real(fp)                       :: epsscl               ! Scaling factor for dissipation [m^2/s^3] 
    real(fp)                       :: factor
    real(fp)                       :: gg
    real(fp)                       :: h1                   ! Thickness of neutrally-stratified top layer 
    real(fp)                       :: h2                   ! Thickness of neutrally-stratified bed layer 
    real(fp)                       :: omeg                 ! Angular frequency of IW with respect to ground 
    real(fp)                       :: omegn                ! Angular frequency of IW with respect to local fluid 
    real(fp)                       :: pinf                 ! Transfer rate, divived by int. time scale, of TKE to IWE 
    real(fp)                       :: rtg                  ! Normalized profile of vert. IW compo- nent outside stably-stratified layer 
    real(fp)                       :: rtgvar
    real(fp)                       :: sumtti               ! Depth integral of TKE to IWE transfer 
    real(fp)                       :: t3                   ! Cubed intrinsic angular frequency of interfacial IW's 
    real(fp)                       :: th1
    real(fp)                       :: th2
    real(fp)                       :: tit                  ! Homogenously distributed conversion of IWE into TKE 
    real(fp)                       :: ts1
    real(fp)                       :: ts2
    real(fp)                       :: tt
    real(fp)                       :: tti                  ! Z-dependent conversion rate of TKE into IWE 
    real(fp)                       :: xkh1                 ! Product XKH*H1 
    real(fp)                       :: xkh2                 ! Product XKH*H2 
    real(fp)                       :: z
    real(fp)                       :: zlu                  ! Integral length scale of turbulence in x/u-direction 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    argsnh   => gdp%gdiwearr%argsnh
    argexp   => gdp%gdiwearr%argexp
    epsbv2   => gdp%gdiwearr%epsbv2
    clu      => gdp%gdiwearr%clu
    clw      => gdp%gdiwearr%clw
    ckw      => gdp%gdiwearr%ckw
    iwedia   => gdp%gdiwearr%iwedia
    ag       => gdp%gdphysco%ag
    !
    gg = ag/(scale*bvav**2)
    epsscl = (bvav**3)*(scale**2)
    !
    factor = gg*sqrt(ckw)*(clw/clu)**2
    !
    dz = h0/kmxt
    dztg = dz/scale
    h1 = dztg*(ktgcu - 1)
    h2 = dztg*(kmxt - ktgcd - 1)
    dh = dz*(kbed - ktop)
    delro = (1. + 2*epsbv2)*(bvav**2)*dh
    factor = factor*delro/16.
    !
    sumtti = 0.0
    !
    do ktg = 1, kmxt - 1
       if (ktg<ktgcu .or. ktgcd<ktg) then
          zlu = clu*zlw(ktg)/clw
          xkh = 1./zlu
          xkh1 = min(argexp, xkh*h1)
          xkh2 = min(argexp, xkh*h2)
          ts1 = 2.0
          ts2 = 2.0
          if (1.E-3<xkh1 .and. xkh1<argsnh) then
             ts1 = (sinh(2*xkh1) - 2*xkh1)/(sinh(xkh1))**2
          else
             ts1 = 0.0
          endif
          if (1.E-3<xkh2 .and. xkh2<argsnh) then
             ts2 = (sinh(2*xkh2) - 2*xkh2)/(sinh(xkh2))**2
          else
             ts2 = 0.0
          endif
          !
          th1 = tanh(xkh1)
          th2 = tanh(xkh2)
          tt = th1*th2/(th1 + th2)
          t3 = tt**3
          !
          omeg = 0.0
          omegn = xkh*sqrt(utg(ktg)**2 + vtg(ktg)**2) - omeg
          pinf = 1./(1. + (omegn*tlw(ktg))**2)
          !
          if (ktg<ktgcu) then
             z = ktg*dztg
             if (1.E-3<xkh1 .and. xkh1<argsnh) then
                rtg = sinh(xkh*z)/sinh(xkh1)
             elseif (xkh1>argsnh) then
                rtg = exp(xkh*(z - h1))
             else
                rtg = 0.0
             endif
          else
             z = (kmxt - ktg)*dztg
             if (1.E-3<xkh2 .and. xkh2<argsnh) then
                rtg = sinh(xkh*z)/sinh(xkh2)
             elseif (xkh2>argsnh) then
                rtg = exp(xkh*(z - h2))
             else
                rtg = 0.0
             endif
          endif
          !
          ! TKE transferred to IWE and conversion into physical units:
          !
          rtgvar = rtg**2
          tti = factor*rtgvar*(ts1 + ts2)*t3*pinf
          tti = tti*sqrt(tke(ktg))*epsscl*ratiow
          ttkiw(ktg) = ttkiw(ktg) + tti
          sumtti = sumtti + tti*dz
       endif
    enddo
    !
    ! IWE to TKE transfer in [m^2/s^3]:
    !
    tit = sumtti/(dz*(1 + ktgcd - ktgcu))
    do ktg = 1, kmxt - 1
       if (ktgcu<=ktg .and. ktg<=ktgcd) then
          tiwtk(ktg) = tiwtk(ktg) + tit
       endif
    enddo
    !
    ! Diagnostics:
    !
    if (iwedia) then
       write (luniwe, *)
       write (luniwe, '(a)') ' TURIWE                          :'
       write (luniwe, '(a,1x,g9.2)') ' Upper level critical layers  [m]:',      &
                                   & h0*(1. - ktgcu/real(kmxt,sp))
       write (luniwe, '(a,1x,g9.2)') ' Lower level critical layers  [m]:',      &
                                   & h0*(1. - ktgcd/real(kmxt,sp))
       write (luniwe, '(a,1x,g9.2)') ' With IWE to TKE rate   [m^2/s^3]:', tit
    endif
end subroutine turiwe
