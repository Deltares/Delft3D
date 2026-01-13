subroutine limadv(         hdt       ,icx       ,icy       , &
              & j         ,nmmaxj    ,nmmax     ,guu       , &
              & norow     ,irocol    , &
              & kcs       ,kfs       ,kfsice    ,kfssnw    , &
              & u_ice     ,psm       , &
              & ps0       ,psx       ,psxx      , &
              & psy       ,psyy      ,psxy      , &
              & zf0       ,zfx       ,zfy       ,zbet      , & 
              & zfm       ,zfxx      ,zfyy      ,zfxy      , &
              & zalg      ,zalg1     ,zalg1q    ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2013.                                     
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
!  $Id: limadv.f90 64423 2019-07-24 11:30:09Z goede $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20190705_ice_modelling/src/engines_gpl/flow2d3d/packages/kernel/src/compute_ice/limadv.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Computes advection for sea ice components
!              Taken from LIM3 (see routine LIM_ADV_X.F90 and LIM_ADV_Y.F90)
!
! Method used: Prather second order scheme that advects tracers
!              but also theirquadratic forms. The method preserves
!              tracer structures by conserving second order moments.
! 
!!--pseudo code and references--------------------------------------------------
!                 Prather, 1986, JGR, 91, D6. 6671-6681.
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    use timers
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                 , pointer :: lundia
!
! Global variables
!
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: guu       !!  Description and declaration in esm_alloc_real.f90
integer                                                               :: icx       !!  Increment in the X-dir., if ICX= NMAX
                                                                                   !!  then computation proceeds in the X-
                                                                                   !!  dir. If icx=1 then computation pro-
                                                                                   !!  ceeds in the Y-dir.
integer                                                               :: icy       !!  Increment in the Y-dir. (see ICX)
integer, dimension(5, norow)                                          :: irocol
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: psm, ps0
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: psx , psy          ! 1st moments
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: psxx, psyy, psxy   ! 2nd moments
!
real(fp)                     , intent(in)                             :: hdt
integer, dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kcs 
integer, dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kfs 
integer, dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kfsice
integer, dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kfssnw
integer                                                               :: j
integer                                                               :: nmmax
integer                                                               :: nmmaxj
integer                                                               :: norow
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: u_ice
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: zf0, zfx , zfy , zbet, &   ! 2D workspace
                                                                       &  zfm, zfxx, zfyy, zfxy, &   !  -      -
                                                                       &  zalg, zalg1, zalg1q        !  -      -                 
!
! Local variables
!
    integer            :: ddb
    integer            :: ic
    integer            :: icxy
    integer            :: nm
    integer            :: nmd
    integer            :: nmu
    real(fp)           :: zs1max, zslpmax, ztemp, zin0    ! temporary scalars
    real(fp)           :: zs1new, zalf , zalfq , zbt           !    -         -  
    real(fp)           :: zs2new, zalf1, zalf1q, zbt1          !    -         - 
    real(fp)           :: rzero, rone

!
!! executable statements -------------------------------------------------------
!
    lundia      => gdp%gdinout%lundia
    !
    ! INITIALISATION
    !
    ddb  = gdp%d%ddbound
    icxy = max(icx, icy)
    !
    rzero  = 0.0_fp
    rone   = 1.0_fp
    !
    ! Initialize work arrays
    !
    zalg   = 0.0_fp    
    zalg1  = 0.0_fp   
    zalg1q = 0.0_fp  
    zbet   = 0.0_fp 
    zf0    = 0.0_fp 
    zfm    = 0.0_fp 
    zfx    = 0.0_fp 
    zfxx   = 0.0_fp
    zfy    = 0.0_fp 
    zfyy   = 0.0_fp
    zfxy   = 0.0_fp 
    !
    !------------------------------------------------------------------------------!
    ! Initialization
    !------------------------------------------------------------------------------!
    !
    do nm = 1, nmmax
       if (kcs(nm)*kfs(nm) == 1)  then
          zslpmax = MAX( rzero, ps0(nm) )
          zs1max  = 1.5 * zslpmax
          zs1new  = MIN( zs1max, MAX( -zs1max, psx(nm) ) )
          zs2new  = MIN(  2.0 * zslpmax - 0.3334 * ABS( zs1new ),      &
             &            MAX( ABS( zs1new ) - zslpmax, psxx(nm) )  )
          zin0    = ( 1.0 - MAX( rzero, sign ( rone, -zslpmax) ) ) * kfsice(nm)
          !
          ps0 (nm) = zslpmax
          psx (nm) = zs1new      * zin0
          psxx(nm) = zs2new      * zin0
          psy (nm) = psy (nm) * zin0
          psyy(nm) = psyy(nm) * zin0
          psxy(nm) = MIN( zslpmax, MAX( -zslpmax, psxy(nm) ) ) * zin0
       endif
    enddo
    !
    !------------------------------------------------------------------------------!
    !  Calculate fluxes and moments between boxes i<-->i+1
    !  for Flux from i to i+1 WHEN u GT 0
    !------------------------------------------------------------------------------!
    !
    do nm = 1, nmmax
       if (kcs(nm)*kfs(nm) == 1)  then
          zbet(nm)  =  MAX( rzero, SIGN( rone, u_ice(nm) ) )
          zalf         =  MAX( rzero, u_ice(nm) ) * hdt * guu(nm) / psm(nm)
          zalfq        =  zalf * zalf
          zalf1        =  1.0 - zalf
          zalf1q       =  zalf1 * zalf1
          !
          zfm (nm)  =  zalf  *   psm (nm)
          zf0 (nm)  =  zalf  * ( ps0 (nm) + zalf1 * ( psx(nm) + (zalf1 - zalf) * psxx(nm) )  )
          zfx (nm)  =  zalfq * ( psx (nm) + 3.0 * zalf1 * psxx(nm) )
          zfxx(nm)  =  zalf  *   psxx(nm) * zalfq
          zfy (nm)  =  zalf  * ( psy (nm) + zalf1 * psxy(nm) )
          zfxy(nm)  =  zalfq *   psxy(nm)
          zfyy(nm)  =  zalf  *   psyy(nm)
          !
          !  Readjust moments remaining in the box.
          psm (nm)  =  psm (nm) - zfm(nm)
          ps0 (nm)  =  ps0 (nm) - zf0(nm)
          psx (nm)  =  zalf1q * ( psx(nm) - 3.0 * zalf * psxx(nm) )
          psxx(nm)  =  zalf1  * zalf1q * psxx(nm)
          psy (nm)  =  psy (nm) - zfy(nm)
          psyy(nm)  =  psyy(nm) - zfyy(nm)
          psxy(nm)  =  zalf1q * psxy(nm)
       endif
    enddo
    !
    !------------------------------------------------------------------------------!
    !  Calculate fluxes and moments between boxes i<-->i+1
    !  for Flux from i+1 to i when u LT 0.
    !------------------------------------------------------------------------------!
    !
    do nm = 1, nmmax
       nmu = nm + icx
       if (kcs(nm)*kfs(nm) == 1 .and. kfs(nmu)*kfsice(nmu) == 1)  then
          zalf          = MAX( rzero, -u_ice(nm) ) * hdt * guu(nm) / psm(nmu) 
          zalg  (nm) = zalf
          zalfq         = zalf * zalf
          zalf1         = 1.0 - zalf
          zalg1 (nm) = zalf1
          zalf1q        = zalf1 * zalf1
          zalg1q(nm) = zalf1q
          !
          zfm   (nm) = zfm (nm) + zalf  *   psm (nmu)
          zf0   (nm) = zf0 (nm) + zalf  * ( ps0 (nmu) - zalf1 * ( psx(nmu) - (zalf1 - zalf ) * psxx(nmu) ) )
          zfx   (nm) = zfx (nm) + zalfq * ( psx (nmu) - 3.0 * zalf1 * psxx(nmu) )
          zfxx  (nm) = zfxx(nm) + zalf  *   psxx(nmu) * zalfq
          zfy   (nm) = zfy (nm) + zalf  * ( psy (nmu) - zalf1 * psxy(nmu) )
          zfxy  (nm) = zfxy(nm) + zalfq *   psxy(nmu)
          zfyy  (nm) = zfyy(nm) + zalf  *   psyy(nmu)
       endif
    enddo
    !
    ! Readjust moments remaining in the box.
    !
    do nm = 1, nmmax
       if (kcs(nm)*kfs(nm) == 1)  then
          nmd = nm - icx
          zbt  =       zbet(nmd)
          zbt1 = 1.0 - zbet(nmd)
          !
          psm (nm) = zbt * psm(nm) + zbt1 * ( psm(nm) - zfm(nmd) )
          ps0 (nm) = zbt * ps0(nm) + zbt1 * ( ps0(nm) - zf0(nmd) )
          psx (nm) = zalg1q(nmd) * ( psx(nm) + 3.0 * zalg(nmd) * psxx(nm) )
          psxx(nm) = zalg1 (nmd) * zalg1q(nmd) * psxx(nm)
          psy (nm) = zbt * psy (nm) + zbt1 * ( psy (nm) - zfy (nmd) )
          psyy(nm) = zbt * psyy(nm) + zbt1 * ( psyy(nm) - zfyy(nmd) )
          psxy(nm) = zalg1q(nmd) * psxy(nm)
       endif
    enddo
    !
    !------------------------------------------------------------------------------!
    ! Put the temporary moments into appropriate neighboring boxes.
    ! for Flux from i to i+1 IF u GT 0.
    !------------------------------------------------------------------------------!
    !
    do nm = 1, nmmax
       nmd = nm - icx
       if (kcs(nm)*kfs(nm) == 1 .and. kfs(nmd)*kfsice(nmd) == 1)  then
          zbt  =       zbet(nmd)
          zbt1 = 1.0 - zbet(nmd)
          psm(nm)  = zbt * ( psm(nm) + zfm(nmd) ) + zbt1 * psm(nm)
          zalf        = zbt * zfm(nmd) / psm(nm)
          zalf1       = 1.0 - zalf
          ztemp       = zalf * ps0(nm) - zalf1 * zf0(nmd)
          !
          ps0 (nm) = zbt * ( ps0(nm) + zf0(nmd) ) + zbt1 * ps0(nm)
          psx (nm) = zbt * ( zalf * zfx(nmd) + zalf1 * psx(nm) + 3.0 * ztemp ) + zbt1 * psx(nm)
          psxx(nm) = zbt * ( zalf * zalf * zfxx(nmd) + zalf1 * zalf1 * psxx(nm)                               &
             &                + 5.0 * ( zalf * zalf1 * ( psx (nm) - zfx(nmd) ) - ( zalf1 - zalf ) * ztemp )  )   &
             &                                                + zbt1 * psxx(nm)
          psxy(nm) = zbt * ( zalf * zfxy(nmd) + zalf1 * psxy(nm)             &
             &                + 3.0 * (- zalf1*zfy(nmd)  + zalf * psy(nm) ) )   &
             &                                                + zbt1 * psxy(nm)
          psy (nm) = zbt * ( psy (nm) + zfy (nmd) ) + zbt1 * psy (nm)
          psyy(nm) = zbt * ( psyy(nm) + zfyy(nmd) ) + zbt1 * psyy(nm)
       endif
    enddo
    !
    !------------------------------------------------------------------------------!
    ! Put the temporary moments into appropriate neighboring boxes.
    ! for Flux from i+1 to i IF u LT 0.
    !------------------------------------------------------------------------------!
    !
    do nm = 1, nmmax
       if (kcs(nm)*kfs(nm) == 1)  then
          zbt  =       zbet(nm)
          zbt1 = 1.0 - zbet(nm)
          psm(nm)  = zbt * psm(nm)  + zbt1 * ( psm(nm) + zfm(nm) )
          zalf        = zbt1 * zfm(nm) / psm(nm)
          zalf1       = 1.0 - zalf
          ztemp       = - zalf * ps0(nm) + zalf1 * zf0(nm)
          !
          ps0(nm)  = zbt * ps0 (nm) + zbt1 * ( ps0(nm) + zf0(nm) )
          psx(nm)  = zbt * psx (nm) + zbt1 * ( zalf * zfx(nm) + zalf1 * psx(nm) + 3.0 * ztemp )
          psxx(nm) = zbt * psxx(nm) + zbt1 * ( zalf * zalf * zfxx(nm)  + zalf1 * zalf1 * psxx(nm)  &
             &                                      + 5.0 *( zalf * zalf1 * ( - psx(nm) + zfx(nm) )      &
             &                                      + ( zalf1 - zalf ) * ztemp ) )
          psxy(nm) = zbt * psxy(nm) + zbt1 * (  zalf * zfxy(nm) + zalf1 * psxy(nm)  &
             &                                      + 3.0 * ( zalf1 * zfy(nm) - zalf * psy(nm) )  )
          psy(nm)  = zbt * psy (nm)  + zbt1 * ( psy (nm) + zfy (nm) )
          psyy(nm) = zbt * psyy(nm)  + zbt1 * ( psyy(nm) + zfyy(nm) )
       endif
    enddo
    !
end subroutine limadv