subroutine z_dwnvel(nmmax     ,kmax      ,icx     ,kcs       ,kfu       , &
                  & kfv       ,kcu       ,kcv     ,s1        ,dps       , &
                  & u0eul     ,v0eul     ,uuu     ,vvv       ,umod      , &
                  & zumod     ,dzs1      ,hu      ,hv        ,kfsed     , &
                  & kfsmin    ,kfsmax    ,kfumin  ,kfumax    ,kfvmin    , &
                  & kfvmax    ,dzu1      ,dzv1    ,z0ucur    ,z0vcur    , &
                  & vonkar    ,gdp     )
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
! Calculate velocity components and magnitude at the
! zeta points based on velocity in the bottom
! computational layer
! 
! Fully rewritten for z-layer
!
! Note: uses downwind velocity at any internal point,
! uses internal velocity at any open boundary, uses
! half of internal velocity in direction of any
! closed boundary or dry point.
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)                             , pointer :: eps
    type (mornumericstype)               , pointer :: mornum
    logical                              , pointer :: ztbml
!
! Global variables
!
    integer                                           , intent(in)  :: icx
    integer                                           , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                           , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kcv    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfsed  !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfv    !  Description and declaration in esm_alloc_int.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: hv     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: u0eul
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: v0eul
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: dzs1   !  See rjdim.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfsmin !  Description and declaration in iidim.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfsmax !  Description and declaration in iidim.f90
    !
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(out) :: umod
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(out) :: uuu
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(out) :: vvv
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(out) :: zumod
    !   
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfumin !  Description and declaration in iidim.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfumax !  Description and declaration in iidim.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfvmin !  Description and declaration in iidim.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfvmax !  Description and declaration in iidim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: dzu1   !  Description and declaration in rjdim.f90 
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: dzv1   !  Description and declaration in rjdim.f90 
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: z0ucur
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: z0vcur
    real(fp)  , intent(in)  :: vonkar
    !
    ! Local variables
    !
    integer :: kmin
    integer :: ksed
    integer  :: k
    integer  :: kn
    integer  :: kmxs
    integer  :: kmx
    integer  :: kmx_u1
    integer  :: kmx_u2
    integer  :: kmx_v1
    integer  :: kmx_v2
    integer  :: ndm
    integer  :: ndmd
    integer  :: ndmu
    integer  :: nm
    integer  :: nm_u1
    integer  :: nm_u2
    integer  :: nm_v1
    integer  :: nm_v2
    integer  :: nmd
    integer  :: nmu
    integer  :: num
    integer  :: numd
    real(fp) :: cc
    real(fp) :: h1
    real(fp) :: ufac
    real(fp) :: uu
    real(fp) :: vfac
    real(fp) :: vv
    real(fp) :: zu   
    real(fp) :: zv
    real(fp) :: u1   
    real(fp) :: v1
    real(fp) :: u2   
    real(fp) :: v2
    real(fp) :: z0cur_u
    real(fp) :: z0cur_v
    real(fp) :: zumod_u1
    real(fp) :: zumod_u2
    real(fp) :: zvmod_v1
    real(fp) :: zvmod_v2
    real(fp) :: ustarc_u1
    real(fp) :: vstarc_v1
    real(fp) :: ustarc_u2
    real(fp) :: vstarc_v2
    real(fp) :: ustarc
    real(fp) :: vstarc
    real(fp) :: thick1
    real(fp) :: thick2
!
!! executable statements -------------------------------------------------------
!
    eps                 => gdp%gdconst%eps
    mornum              => gdp%gdmorpar%mornum
    ztbml               => gdp%gdzmodel%ztbml
    !
    do nm = 1, nmmax
       !
       if (kfsed(nm) == 0) then
          uuu  (nm) = 0.0_fp
          vvv  (nm) = 0.0_fp
          umod (nm) = 0.0_fp
          zumod(nm) = 0.0_fp
          cycle
       endif
       !
       nmd  = nm  - icx
       numd = nmd + 1
       ndmd = nmd - 1
       num  = nm  + 1
       ndm  = nm  - 1
       nmu  = nm  + icx
       ndmu = nmu - 1
       !
       uu = 0.0_fp
       vv = 0.0_fp
       h1 = s1(nm) + real(dps(nm),fp)
       !
       ! to find kmx and zumod
       !
       thick1 = 0.0_fp
       do k = kfsmin(nm),kfsmax(nm)
          thick1 = thick1 + dzs1(nm, k)
          kmxs = k
          if (thick1>=0.05_fp*h1 .or. thick1>=0.05_fp) then
             exit
          endif  
       enddo
       thick2 = 0.0_fp
       if (kfsmax(nm) /= kfsmin(nm) ) then
          do k = kfsmin(nm), kmxs-1
            thick2 = thick2 + dzs1(nm, k)
          enddo
          thick2  = thick2+ dzs1(nm, kmxs)*0.5_fp
          zumod(nm) = thick2
       else
          zumod(nm) = h1*0.368_fp
       endif
       !
       !
       ufac = 0.5_fp
       vfac = 0.5_fp
       if (kcs(nm) == 1) then
          !
          ! Internal point
          ! Set velocity in U direction.
          !
          nm_u1 = nm
          nm_u2 = nmd
          !
          ! Set velocity in V direction.
          !
          nm_v1 = nm
          nm_v2 = ndm
       elseif (kcu(nm) + kcu(nmd) == 1) then
          !
          ! Open boundary (kcs(nm)==2) in v-direction
          !
          ! Set velocity in U direction.
          !
          nm_u1 = nm
          nm_u2 = nmd
          ufac  = 1.0_fp
          !
          ! Set velocity in V direction.
          !
          if (kcu(nm) == 1) then
             !
             ! Open boundary at left-hand side
             !
             nm_v1 = nmu
             nm_v2 = ndmu
          else
             !
             ! Open boundary at right-hand side
             !
             nm_v1 = nmd
             nm_v2 = ndmd
          endif
       else
          !
          ! Open boundary (kcs(nm)==2) in u-direction
          !
          ! Set velocity in U direction.
          !
          if (kcv(nm) == 1) then
             !
             ! Open boundary at lower side
             !
             nm_u1 = num
             nm_u2 = numd
          else
             !
             ! Open boundary at upper side
             !
             nm_u1 = ndm
             nm_u2 = ndmd
          endif
          !
          ! Set velocity in V direction.
          !
          nm_v1 = nm
          nm_v2 = ndm
          vfac  = 1.0_fp
       endif
       !
       ! U-direction
       !
       zu        = 0.0_fp
       uu        = 0.0_fp
       ustarc_u1 = 0.0_fp
       ustarc_u2 = 0.0_fp
       !
       ! kmx_u1 here is for z layers: counting from the bed.
       !
       kmx_u1   = 0
       kmx_u2   = 0
       zumod_u1 = 0.0_fp
       zumod_u2 = 0.0_fp
       !
       if ((nm_u1 > 0) .and. (kfumin(nm_u1)<kfumax(nm_u1))) then
          do k = kfumin(nm_u1),kfumax(nm_u1)
             zu = zu + dzu1(nm_u1,k)
             kmx_u1 = k
             if (zu > 0.05_fp*hu(nm_u1)) then
                exit
             endif
          enddo
       endif
       zu = 0.0_fp
       if ((nm_u2 > 0) .and. (kfumin(nm_u2)<kfumax(nm_u2))) then
          do k = kfumin(nm_u2),kfumax(nm_u2)
             zu = zu + dzu1(nm_u2,k)
             kmx_u2 = k
             if (zu > 0.05_fp*hu(nm_u2)) then
                exit
             endif
          enddo
       endif
       kn      = max(1, kfu(nm_u1) + kfu(nm_u2))
       z0cur_u   = (  kfu(nm_u1)*z0ucur(nm_u1) + kfu(nm_u2)*z0ucur(nm_u2))/kn
       if ((nm_u1>0) .and. (kmx_u1>0)) then
          u1 = kfu(nm_u1) * u0eul(nm_u1,kmx_u1)
          do k = kfumin(nm_u1), kmx_u1-1
            zumod_u1 = zumod_u1 + dzu1(nm, k)
          enddo
          zumod_u1  = zumod_u1+ dzu1(nm,kmx_u1)*0.5_fp
          if(zumod_u1 > 0.0_fp) then
             ustarc_u1= u1 * vonkar / log(1.0_fp + zumod_u1/z0ucur(nm_u1))
          else 
             ustarc_u1= 0.0_fp
          endif
       endif
       if ((nm_u2>0) .and. (kmx_u2>0)) then
          u2 = kfu(nm_u2) * u0eul(nm_u2,kmx_u2)
          do k = kfumin(nm_u1), kmx_u2-1
            zumod_u2 = zumod_u2 + dzu1(nm, k)
          enddo
          zumod_u2  = zumod_u2+ dzu1(nm, kmx_u2)*0.5_fp
          if(zumod_u2 > 0.0_fp) then
             ustarc_u2= u2*vonkar/log(1.0 + zumod_u2/z0ucur(nm_u2))
          else
             ustarc_u2= 0.0_fp 
          endif
       endif
       ustarc = (ustarc_u1+ustarc_u2) * ufac
       if (z0cur_u >0.0_fp) then
          uu = ustarc * log(1.0_fp+zumod(nm)/z0cur_u) / vonkar
       else
          uu = 0.0_fp
       endif
       !
       ! V-direction
       !
       zv        = 0.0_fp
       vv        = 0.0_fp
       vstarc_v1 = 0.0_fp
       vstarc_v2 = 0.0_fp
       kmx_v1    = 0
       kmx_v2    = 0
       zvmod_v1  = 0.0_fp
       zvmod_v2  = 0.0_fp    
       if ((nm_v1>0) .and. (kfvmin(nm_v1)<kfvmax(nm_v1))) then
          do k = kfvmin(nm_v1),kfvmax(nm_v1)
             zv = zv + dzv1(nm_v1,k)
             kmx_v1 = k
             if (zv > 0.05_fp*hv(nm_v1)) then
               exit
             endif
          enddo
       endif
       zv = 0.0_fp
       if ((nm_v2>0) .and. (kfvmin(nm_v2)<kfvmax(nm_v2))) then
          do k = kfvmin(nm_v2),kfvmax(nm_v2)
             zv = zv + dzv1(nm_v2,k)
             kmx_v2 = k
             if (zv > 0.05_fp*hv(nm_v2)) then
               exit
             endif
          enddo
       endif
       kn      = max(1, kfv(nm_v1) + kfv(nm_v2))
       z0cur_v = (kfv(nm_v1)*z0vcur(nm_v1) + kfv(nm_v2)*z0vcur(nm_v2)) / kn
       if ((nm_v1>0) .and. (kmx_v1>0)) then          
          v1 = kfv(nm_v1) * v0eul(nm_v1,kmx_v1)
          do k = kfvmin(nm_v1), kmx_v1-1
            zvmod_v1 = zvmod_v1 + dzv1(nm, k)
          enddo
          zvmod_v1  = zvmod_v1 + dzv1(nm,kmx_v1)*0.5_fp
          if(zvmod_v1 > 0.0_fp) then
             vstarc_v1= v1 * vonkar / log(1.0_fp + zvmod_v1/z0vcur(nm_v1))
          else
             vstarc_v1= 0.0_fp 
          endif
       endif
       if ((nm_v2>0) .and. (kmx_v2>0)) then          
          v2 = kfv(nm_v2) * v0eul(nm_v2,kmx_v2)
          do k = kfvmin(nm_v2), kmx_v2-1
            zvmod_v2 = zvmod_v2 + dzv1(nm, k)
          enddo
          zvmod_v2  = zvmod_v2 + dzv1(nm,kmx_v2)*0.5_fp
          if(zvmod_v2 > 0.0_fp) then
             vstarc_v2= v2 * vonkar / log(1.0_fp + zvmod_v2/z0vcur(nm_v2))
          else
             vstarc_v2= 0.0_fp
          endif
       endif
       vstarc = vfac * (vstarc_v1+vstarc_v2)
       if (z0cur_v >0.0_fp) then
          vv = vstarc * log(1.0_fp + zumod(nm)/z0cur_v) / vonkar
       else
          vv = 0.0_fp
       endif
       !
       if (mornum%maximumwaterdepth) then
          !
          ! prevent any increase in velocity due to a decrease in water depth
          !
          if (kfu(nm_u1)==1) h1 = max(h1,hu(nm_u1))
          if (kfu(nm_u2)==1) h1 = max(h1,hu(nm_u2))
          if (kfv(nm_v1)==1) h1 = max(h1,hv(nm_v1))
          if (kfv(nm_v2)==1) h1 = max(h1,hv(nm_v2))
       endif
       !
       ! h1 will not be zero, because of cycle statement when kfsed==0
       !
       !uu = uu/h1
       !vv = vv/h1
       !
       if (abs(uu) < eps) then
          uu = 0.0_fp
       endif
       if (abs(vv) < eps) then
          vv = 0.0_fp
       endif
       !
       ! Calculate resultant velocity magnitude and height
       !
       umod(nm) = (uu*uu + vv*vv)**0.5
       !if (kmax > 1) then
       !   zumod(nm) = h1 * (1.0+sig(kmx))
       !else
       !
       !do k = kfsmax(nm), 1, -1
       !   cc  = (1.0 + siglc(k))*h1
       !   kmx = k
       !   if (cc>=0.05*h1 .or. cc>=0.05) then
       !      exit
       !   endif         
       !enddo
       !
       !
       !
       !if (kfsmax(nm) /= kfsmin(nm) ) then
       !   zumod(nm) = h1 * (1.0+siglc(kmx))
       !else
       !   !
       !   ! (1+h1/z0rou)**(z0rou/h1)*exp(-1)*(z0rou+h1)-z0rou
       !   ! equals approximately exp(-1)*h1 if h1>>z0rou
       !   ! exp(-1) is approx 0.368
       !   !
       !   zumod(nm) = h1*0.368
       !endif
       uuu(nm) = uu
       vvv(nm) = vv
    enddo
end subroutine z_dwnvel
