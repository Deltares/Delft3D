subroutine bedform(nmmax     ,dps       ,s1        ,u         ,v         , &
                 & kfs       ,kfu       ,kfv       ,kmax      , &
                 & icx       ,icy       ,cvalu0    ,cvalv0    ,gdp       )
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
! Calculate bedform height and length.
!
! Routine is called for U/V-direction
! respectively.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use flow2d3d_timers
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp 
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                              , pointer :: bedformheighttype
    integer                              , pointer :: bedformlengthtype
    integer                              , pointer :: i50
    integer                              , pointer :: i90
    logical                              , pointer :: spatial_bedform
    real(fp), dimension(:)               , pointer :: bedformD50
    real(fp), dimension(:)               , pointer :: bedformD90
    real(fp)                             , pointer :: thetacdune
    real(fp), dimension(:)               , pointer :: hdpar
    real(fp), dimension(:)               , pointer :: duneheightequi
    real(fp), dimension(:)               , pointer :: duneheight
    real(fp), dimension(:)               , pointer :: dunelength
    real(fp), dimension(:)               , pointer :: ldpar
    real(fp), dimension(:,:)             , pointer :: dxx
    real(fp), dimension(:)               , pointer :: rhosol
    real(fp)                             , pointer :: ag
    real(fp)                             , pointer :: rhow
    real(fp)                             , pointer :: vicmol
    real(fp)                             , pointer :: eps
!
! Global variables
!
    integer                                           , intent(in)  :: nmmax   !  Description and declaration in dimens.igs
    integer                                           , intent(in)  :: icx
    integer                                           , intent(in)  :: icy
    integer                                           , intent(in)  :: kmax    !  Description and declaration in dimens.igs
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfs     !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfu     !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfv     !  Description and declaration in esm_alloc_int.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: dps     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: s1      !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: u       !!  EULARIAN U-velocities at old time level
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: v       !!  EULARIAN V-velocities at old time level
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: cvalu0  ! Roughness in U-direction        
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: cvalv0  ! Roughness in V-direction        
!
! Local variables
!
    integer  :: nm
    integer  :: nmd
    integer  :: ndm
    integer  :: ken
    real(fp) :: depth
    real(fp) :: uuub
    real(fp) :: vvvb
    real(fp) :: umodb
    real(fp) :: dstar
    real(fp) :: thetac
    real(fp) :: ucbsv2
    real(fp) :: ubsvg2
    real(fp) :: rcgrn
    real(fp) :: rkgrn
    real(fp) :: u2dhb
    real(fp) :: tsp
    real(fp) :: t1
    real(fp) :: t2
    real(fp) :: t3
    real(fp) :: t4
    real(fp) :: mu
    real(fp) :: chezy
    real(fp) :: theta
    real(fp) :: d50
    real(fp) :: d90
    real(fp) :: relden
!
!! executable statements -------------------------------------------------------
!
    bedformD50              => gdp%gdbedformpar%bedformD50
    bedformD90              => gdp%gdbedformpar%bedformD90
    spatial_bedform         => gdp%gdbedformpar%spatial_bedform
    bedformheighttype       => gdp%gdbedformpar%bedformheighttype
    bedformlengthtype       => gdp%gdbedformpar%bedformlengthtype
    hdpar                   => gdp%gdbedformpar%hdpar
    duneheightequi          => gdp%gdbedformpar%duneheightequi
    duneheight              => gdp%gdbedformpar%duneheight
    dunelength              => gdp%gdbedformpar%dunelength
    ldpar                   => gdp%gdbedformpar%ldpar
    thetacdune              => gdp%gdbedformpar%thetacdune
    dxx                     => gdp%gderosed%dxx
    rhosol                  => gdp%gdsedpar%rhosol
    i50                     => gdp%gdmorpar%i50
    i90                     => gdp%gdmorpar%i90
    ag                      => gdp%gdphysco%ag
    rhow                    => gdp%gdphysco%rhow
    vicmol                  => gdp%gdphysco%vicmol
    eps                     => gdp%gdconst%eps
    !
    do nm = 1, nmmax
       if (kfs(nm)>0) then
          !
          !  Initialisation of depth and velocity.
          !
          nmd   = nm - icx
          ndm   = nm - icy
          !
          depth = max(s1(nm) + real(dps(nm),fp), 0.0_fp)
          !
          ! Calculate EQUILIBRIUM dune heights
          !
          if (bedformheighttype <= 2) then
             !
             ! Velocity in zeta point
             !
             uuub = 0.5_fp*(u(nm,kmax)*kfu(nm) + u(nmd,kmax)*kfu(nmd))
             vvvb = 0.5_fp*(v(nm,kmax)*kfv(nm) + v(ndm,kmax)*kfv(ndm))
             !
             ! Depth-average velocity (similar as in TAUBOT)
             !
             umodb   = sqrt(uuub**2 + vvvb**2)
             u2dhb   = umodb
             !
             ! Get d50 and d90 if defined.
             !
             if (associated(gdp%gderosed%dxx))  then 
                d50 = dxx(nm, i50)
                d90 = dxx(nm, i90)
                relden = (rhosol(1)-rhow)/rhow ! assume density equal for all sediment fractions
             else
                if (spatial_bedform) then
                   d50 = bedformD50(nm)
                   d90 = bedformD90(nm)
                else
                   d50 = bedformD50(1)
                   d90 = bedformD90(1)
                endif
                relden = 1.65_fp
             endif
             !
             ! Dimensionless Particle Parameter
             !
             dstar = d50*(ag*relden/vicmol**2)**(1.0_fp/3.0_fp)
             !
             ! Calculate critical bed-shear velocity (Shields)
             !
             if ( thetacdune < 0.0_fp ) then
                if (dstar<=4.0_fp) then
                   thetac = 0.24_fp/dstar
                elseif (dstar<=10.0_fp) then
                   thetac = 0.14_fp*dstar**( - 0.64_fp)
                elseif (dstar<=20.0_fp) then
                   thetac = 0.04_fp*dstar**( - 0.10_fp)
                elseif (dstar<=150.0_fp) then
                   thetac = 0.013_fp*dstar**(0.29_fp)
                else
                   thetac = 0.055_fp
                endif
             else
                thetac = thetacdune
             endif
             !
             ucbsv2 = ag*relden*d50*thetac   
             ken = kfu(nm) + kfu(nmd) + kfv(nm) + kfv(ndm)
             chezy = (kfu(nmd)*cvalu0(nmd) + kfu(nm)*cvalu0(nm) &
                  & + kfv(ndm)*cvalv0(ndm) + kfv(nm)*cvalv0(nm))/ken
             !
             rkgrn  = 3.0_fp*d90
             rcgrn  = 18.0_fp*log10(12.0_fp*depth/rkgrn)
             ubsvg2 = ag*u2dhb**2/rcgrn**2
             !
             if (bedformheighttype == 1) then   
                !
                ! Van Rijn 1984.
                !
                ! Calculate Transport Parameter
                ! 
                tsp    = (ubsvg2 - ucbsv2)/ucbsv2
                !
                if (tsp<=0.0_fp .or. tsp>=25.0_fp) then
                   duneheightequi(nm) = 0.0
                else
                   t1  = 0.11_fp*depth
                   t2  = (d50/depth)**(0.3_fp)
                   t3  = 1.0_fp - exp( - 0.5_fp*tsp)
                   t4  = 25.0_fp - tsp
                   duneheightequi(nm) = hdpar(1)*t1*t2*t3*t4
                endif
             elseif (bedformheighttype == 2) then
                !
                ! Fredsoe for Meyer-Peter & Mueller 1948
                ! Avoid division by zero (using eps) when theta or mu is zero
                !
                mu    = (chezy/rcgrn)**1.5_fp
                theta = u2dhb**2 / ((chezy**2)*relden*d50)
                t1    = hdpar(1) * 24.0_fp / 63.0_fp
                t4    = max(theta*mu,eps)
                t2    = thetac/t4
                t3    = max(1.0_fp-t2,0.0_fp)
                duneheightequi(nm) = max(t1*t3*depth,0.0_fp)
             endif
          elseif (bedformheighttype == 3) then
             !
             ! Fredsoe for Engelund & Hansen 1967
             !
             duneheightequi(nm) = hdpar(1)*depth/5.0_fp
          elseif (bedformheighttype == 4) then
             !
             ! Power relation -- can be used for verification.
             ! 
             duneheightequi(nm) = hdpar(1)*(abs(depth)**hdpar(2));
          endif
          !
          ! Calculate dune lengths.
          !
          if (bedformlengthtype == 1) then
             !
             ! Van Rijn 1984
             !
             dunelength(nm) = 7.3_fp*depth
          elseif (bedformlengthtype == 2) then
             !
             ! Power relation
             !
             dunelength(nm) = ldpar(1)*(abs(depth)**ldpar(2))
          endif
       else
          !
          ! dry point: keep dune height constant
          !
          duneheightequi(nm) = duneheight(nm)
       endif
    enddo
end subroutine bedform
