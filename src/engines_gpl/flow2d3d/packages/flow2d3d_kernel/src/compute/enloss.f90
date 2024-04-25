subroutine enloss(ag        ,d1        ,eweir     ,hkruin    ,hov       , &
                & qunit     ,qvolk     ,toest     ,vov       , &
                & ewbov     ,ewben     ,wsben     ,dte       ,dtefri    )
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
! Function: Determines additional energy loss due to weir.
!           Energy loss dependent on velocity perpendicular
!           to weir (Schonfeld, 1955).
!           Subroutine based on subroutine ENLOSS in WAQUA.
!           Energyloss based on Carnot relation for
!           decellerated flow VOV < 0.25 m/s
!           or on "Tables" VOV > 0.5 m/s or
!           average for 0.25 < VOV < 0.50 m/s.
!           In 2003 the improved formulations described in
!           "Beoordeling nieuwe overlaatroutines WAQUA"
!           WL-report Z3063, have been implemented and tested.
!
!!--pseudo code and references--------------------------------------------------
!
! "Weergave van extra energieverlies in RIVCUR."
!  (J.H.A. Wybenga, report Deltares Q779 voor RWS/RIZA, 1989).
! "Energieverliezen door overlaten: een gewijzigde berekeningsprocedure voor
!  WAQUA-rivieren versie."
!  (H. Vermaas, verslag onderzoek Deltares Q92, 1987)
! "Beoordeling nieuwe overlaatroutines WAQUA"
!  (J. van Kester, verslag Z3063, juli 2001)
!
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Global variables
!
    real(fp)    , intent(in) :: ag     !  Description and declaration in esm_alloc_real.f90
    real(fp)                 :: d1     !!  Distance between crest and downstream depth
    real(fp)    , intent(out):: dte    !!  Subgrid energy loss due to weir
    real(fp)    , intent(in) :: dtefri !!  Energy loss due to friction
    real(fp)    , intent(in) :: ewben  !!  Excess waterdepth downstream
    real(fp)    , intent(in) :: ewbov  !!  Excess waterdepth upstream
    real(fp)                 :: eweir  !!  Energy level at weir
    real(fp)    , intent(in) :: hkruin !!  Crest height (downward positive).
    real(fp)                 :: hov    !!  Total water depth at crest weir
    real(fp)                 :: qunit  !!  Discharge at weir crest
    real(fp)                 :: qvolk  !!  Maximum discharge (super critical flow)
    real(fp)    , intent(in) :: vov    !!  Velocity at crest of weir
    real(fp)    , intent(in) :: wsben  !!  Downstream water level
    character(4)             :: toest  !!  State weir:
                                       !!  volk = perfect weir
                                       !!  onvo = imperfect weir
!
! Local variables
!
    real(fp) :: dtecar
    real(fp) :: dteonv
    real(fp) :: dtetab
    real(fp) :: dtevol
    real(fp) :: qqv
    real(fp) :: tabel
    real(fp) :: theta
    real(fp) :: vben
    real(fp) :: wsov
!
!! executable statements -------------------------------------------------------
!
    !
    ! Determine energy loss
    !
    qqv = qunit/qvolk
    !
    ! Imperfect weir (sub critical flow)
    !
    if ((wsben + hkruin + d1)==0.0) then
       !
       ! Dry bed downstream, could perhaps also check on qunit==0.0
       !
       dteonv = 0.0
    elseif (abs(vov)<=0.25) then
       !
       ! Energy loss according Carnot law
       ! WSBEN+HKRUIN+D1 := H0 (S0+DPS)
       !
       dteonv = (vov - qunit/(wsben + hkruin + d1))**2/(2.*ag)
    elseif (abs(vov)>0.25 .and. abs(vov)<0.5) then
       !
       ! Weigthing between Carnot and Tables
       ! WSBEN+HKRUIN+D1 := H0 (S0+DPS)
       !
       dtecar = (vov - qunit/(wsben + hkruin + d1))**2/(2.*ag)
       dtetab = tabel(d1, eweir, qunit, qvolk)
       theta = (abs(vov) - 0.25)/0.25
       dteonv = (1 - theta)*dtecar + theta*dtetab
    elseif (abs(vov)>=0.5) then
       !
       ! Energy loss according to Tables from Vermaas
       !
       dteonv = tabel(d1, eweir, qunit, qvolk)
    else
    endif
    !
    ! Determine energy loss for free weir flow
    !
    dtevol = ewbov - ewben - dtefri
    !
    ! Determine if it is free weir flow or submerged weir flow
    !
    if (dtevol*qqv**2>=dteonv) then
       !
       ! It is a free weir flow
       !
       toest = 'volk'
    else
       !
       ! It is a submerged weir flow
       !
       toest = 'onvo'
    endif
    !
    ! Energy loss
    !
    if (toest=='volk') then
       !
       ! It is a free weir flow
       !
       dte = dtevol
    elseif (toest=='onvo') then
       !
       ! It is a submerged weir flow
       !
       dte = dteonv
    else
    endif
end subroutine enloss
