subroutine limtrp(         hdt       ,icx       ,icy       , &
              & j         ,nmmaxj    ,nmmax     , &
              & gsqs      ,guu       ,gvv       , &
              & norow     ,irocol    ,kcs       ,kfs       , &
              & u_ice     ,v_ice     ,h_ice     ,h_snow    ,a_ice     , &
              & kfsice    ,kfssnw    , &
              & sxice     ,sxsn      ,sxa       , &
              & zs0ice    ,zs0sn     ,zs0a      , &
              & vol_ice   ,vol_snow  ,zsm       , &
              & zf0       ,zfx       ,zfy       ,zbet      , & 
              & zfm       ,zfxx      ,zfyy      ,zfxy      , &
              & zalg      ,zalg1     ,zalg1q    ,guu2      ,gvv2      ,gdp       )
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
!  $Id: limtrp.f90 64423 2019-07-24 11:30:09Z goede $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20190705_ice_modelling/src/engines_gpl/flow2d3d/packages/kernel/src/compute_ice/limtrp.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Computes advection process for sea ice (NB. diffusion is omitted yet.)
!              Taken from LIM3 (see routine LIM_TRP.F90)
!
! Method used: variables included in the process are scalar
!              other values are considered as second order.
!              For advection, a second order Prather scheme is used.
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
integer                                                               :: icx       !!  Increment in the X-dir., if ICX= NMAX
                                                                                   !!  then computation proceeds in the X-
                                                                                   !!  dir. If icx=1 then computation pro-
                                                                                   !!  ceeds in the Y-dir.
integer                                                               :: icy       !!  Increment in the Y-dir. (see ICX)
integer, dimension(5, norow)                                          :: irocol
integer                                                               :: j
integer, dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kfsice
integer, dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kfssnw
integer                                                               :: nmax
integer                                                               :: nmmax
integer                                                               :: nmmaxj
integer                                                               :: norow
integer, dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kcs
integer, dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kfs
!
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: a_ice
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: h_ice
real(fp)                     , intent(in)                             :: hdt
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: h_snow
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: guu       !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: gvv       !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: gsqs      !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub,5)                          :: sxice, sxsn, sxa ! contains field to be advected and 1st and 2nd moments for ice, snow and ice concentration
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: u_ice
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: v_ice
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: vol_ice
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: vol_snow
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: zsm
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: zs0ice, zs0sn, zs0a
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: guu2, gvv2
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: zf0, zfx , zfy , zbet, &   ! 2D workspace
                                                                       & zfm, zfxx, zfyy, zfxy, &   !  -      -
                                                                       & zalg, zalg1, zalg1q        !  -      -                 
!
! Local variables
!
    integer            :: ic
    integer            :: nm, nmd, nmu, ndm, num
    real(fp)           :: zs1max, zslpmax, ztemp, zin0    ! temporary scalars
    real(fp)           :: zs1new, zalf , zalfq , zbt           !    -         -  
    real(fp)           :: zs2new, zalf1, zalf1q, zbt1          !    -         - 
    real(fp)           :: rzero, rone
    real(fp)           :: gmin, gmean, gmeani, gmax
    logical            :: existing_ice

!
!! executable statements -------------------------------------------------------
!
    lundia      => gdp%gdinout%lundia
    !
    ! INITIALISATION
    !
    rzero  = 0.0_fp
    rone   = 1.0_fp
    !
    !------------------------------------------------------------------------------!
    ! Initialization
    !------------------------------------------------------------------------------!
    !
    vol_ice  = 0.0_fp
    vol_snow = 0.0_fp
    zs0ice  = 0.0_fp
    zs0sn   = 0.0_fp
    zs0a    = 0.0_fp
    !
    !------------------------------------------------------------------------------!
    ! Check whether ice is available
    !------------------------------------------------------------------------------!
    !
    existing_ice = .false.
    do nm = 1, nmmax
       if (kfs(nm) == 1 .and. kfsice(nm) .eq. 1) existing_ice = .true.
    enddo
    !
    if (.not. existing_ice) then
       goto 1234
    endif   
    !
    !------------------------------------------------------------------------------!
    ! Scaling 
    !------------------------------------------------------------------------------!
    !
    gmax = -1.0
    gmin = 10.0**14
    do nm = 1, nmmax
       if (kfs(nm) == 1)  then
           if (gmax .lt. gsqs(nm)) gmax=gsqs(nm)
           if (gmin .gt. gsqs(nm)) gmin=gsqs(nm)
       endif
    enddo
    gmean  = 0.5_fp *(gmin+gmax)   
    gmeani = 1.0_fp / gmean 
    !
    !------------------------------------------------------------------------------!
    !  Calculate ice volume, snow volume and ice arera
    !------------------------------------------------------------------------------!
    !
    do nm = 1, nmmax
       if (kfs(nm) == 1)  then
          ! compute volume of ice per unit area
          vol_ice(nm)  =  h_ice(nm)  * a_ice(nm)
          vol_snow(nm) =  h_snow(nm) * a_ice(nm)
          !
          zs0ice(nm) =  vol_ice(nm)  * gsqs(nm) * gmeani   ! Ice volume.
          zs0sn (nm) =  vol_snow(nm) * gsqs(nm) * gmeani   ! Snow volume.
          zs0a  (nm) =  a_ice(nm)    * gsqs(nm) * gmeani   ! Ice area
       endif
    enddo
    !
    !------------------------------------------------------------------------------!
    !  Calculate advection of ice fields (ice volume, snow volume, area volume)
    !------------------------------------------------------------------------------!
    !
    ! initialize area ZSM
    do nm = 1, nmmax
       if (kfs(nm) == 1)  then
          zsm(nm)  = gsqs(nm) * gmeani
          guu2(nm) = guu(nm)  * gmeani
          gvv2(nm) = gvv(nm)  * gmeani
       endif
    enddo
    !
    ! for ice volume in x-direction
    !
    call limadv(           hdt       ,icx       ,icy       , &
              & j         ,nmmaxj    ,nmmax     ,guu2      , &
              & norow     ,irocol    , &
              & kcs       ,kfs       ,kfsice    ,kfssnw    , &
              & u_ice     ,zsm       , &
              & zs0ice    ,sxice(j,1),sxice(j,2), &
              & sxice(j,3),sxice(j,4),sxice(j,5), &
              & zf0       ,zfx       ,zfy       ,zbet      , & 
              & zfm       ,zfxx      ,zfyy      ,zfxy      , &
              & zalg      ,zalg1     ,zalg1q    ,gdp       )
    !
    ! for ice volume in y-direction
    !
    call limadv(           hdt       ,icy       ,icx       , &
              & j         ,nmmaxj    ,nmmax     ,gvv2      , &
              & norow     ,irocol    , &
              & kcs       ,kfs       ,kfsice    ,kfssnw    , &
              & v_ice     ,zsm       , &
              & zs0ice    ,sxice(j,3),sxice(j,4), &
              & sxice(j,1),sxice(j,2),sxice(j,5), &
              & zf0       ,zfy       ,zfx       ,zbet      , & 
              & zfm       ,zfyy      ,zfxx      ,zfxy      , &
              & zalg      ,zalg1     ,zalg1q    ,gdp       )
    !
    ! initialize area ZSM
    do nm = 1, nmmax
       if (kfs(nm) == 1)  then
          zsm(nm) = gsqs(nm) * gmeani
       endif
    enddo
    !
    ! for snow volume in x-direction
    !
    call limadv(           hdt       ,icx       ,icy       , &
              & j         ,nmmaxj    ,nmmax     ,guu2      , &
              & norow     ,irocol    , &
              & kcs       ,kfs       ,kfsice    ,kfssnw    , &
              & u_ice     ,zsm       , &
              & zs0sn     ,sxsn(j,1) ,sxsn(j,2) , &
              & sxsn(j,3) ,sxsn(j,4) ,sxsn(j,5) , &
              & zf0       ,zfx       ,zfy       ,zbet      , & 
              & zfm       ,zfxx      ,zfyy      ,zfxy      , &
              & zalg      ,zalg1     ,zalg1q    ,gdp       )
    !
    ! for snow volume in y-direction
    !
    call limadv(           hdt       ,icy       ,icx       , &
              & j         ,nmmaxj    ,nmmax     ,gvv2      , &
              & norow     ,irocol    , &
              & kcs       ,kfs       ,kfsice    ,kfssnw    , &
              & v_ice     ,zsm       , &
              & zs0sn     ,sxsn(j,3) ,sxsn(j,4) , &
              & sxsn(j,1) ,sxsn(j,2) ,sxsn(j,5) , &
              & zf0       ,zfy       ,zfx       ,zbet      , & 
              & zfm       ,zfyy      ,zfxx      ,zfxy      , &
              & zalg      ,zalg1     ,zalg1q    ,gdp       )
    !
    ! initialize area ZSM
    !
    do nm = 1, nmmax
       if (kfs(nm) == 1)  then
          zsm(nm) = gsqs(nm) * gmeani
       endif
    enddo
    !
    ! for area volume in x-direction
    !
    call limadv(           hdt       ,icx       ,icy       , &
              & j         ,nmmaxj    ,nmmax     ,guu2      , &
              & norow     ,irocol    , &
              & kcs       ,kfs       ,kfsice    ,kfssnw    , &
              & u_ice     ,zsm       , &
              & zs0a      ,sxa(j,1)  ,sxa(j,2)  , &
              & sxa(j,3)  ,sxa(j,4)  ,sxa(j,5)  , &
              & zf0       ,zfx       ,zfy       ,zbet      , & 
              & zfm       ,zfxx      ,zfyy      ,zfxy      , &
              & zalg      ,zalg1     ,zalg1q    ,gdp       )
    !
    ! for area volume in y-direction
    !
    call limadv(           hdt       ,icy       ,icx       , &
              & j         ,nmmaxj    ,nmmax     ,gvv2      , &
              & norow     ,irocol    , &
              & kcs       ,kfs       ,kfsice    ,kfssnw    , &
              & v_ice     ,zsm       , &
              & zs0a      ,sxa(j,3)  ,sxa(j,4)  , &
              & sxa(j,1)  ,sxa(j,2)  ,sxa(j,5)  , &
              & zf0       ,zfy       ,zfx       ,zbet      , & 
              & zfm       ,zfyy      ,zfxx      ,zfxy      , &
              & zalg      ,zalg1     ,zalg1q    ,gdp       )
    !
    !------------------------------------------------------------------------------!
    !  Recover the properties from their contents
    !------------------------------------------------------------------------------!
    !
    do nm = 1, nmmax
       if (kfs(nm) == 1)  then
          vol_ice (nm) = zs0ice(nm) * gmean / gsqs(nm)
          vol_snow(nm) = zs0sn (nm) * gmean / gsqs(nm)
          a_ice   (nm) = zs0a  (nm) * gmean / gsqs(nm)
          !
          if (a_ice(nm) .gt. 1e-6_fp .and. a_ice(nm) .lt. 1.0_fp) then
             h_ice (nm) = vol_ice (nm) / a_ice (nm)
             h_snow(nm) = vol_snow(nm) / a_ice (nm)
          else if (a_ice(nm) .ge. 1.0_fp) then
             a_ice(nm) = 1.0_fp  
             h_ice (nm) = a_ice(nm) * h_ice (nm) 
             h_snow(nm) = a_ice(nm) * h_snow(nm) 
          else if (vol_ice(nm) .gt. 1e-8 .and. a_ice(nm) .le. 1e-6_fp) then
             a_ice(nm) = 1e-6_fp  
             h_ice (nm) = vol_ice (nm) / a_ice (nm)
             h_snow(nm) = vol_snow(nm) / a_ice (nm)
          endif
       endif
    enddo
    !
    !------------------------------------------------------------------------------!
    !  Check for very large or negative ice thickness
    !------------------------------------------------------------------------------!
    !
    do nm = 1, nmmax
       if (kfs(nm) == 1 .and. vol_ice(nm) .gt. 1e-8)  then
          if (h_ice(nm) .gt. 10.0_fp ) then
              a_ice(nm) = a_ice(nm) * h_ice (nm) / 10.0_fp
              h_ice(nm) = 10.0_fp
          endif     
          !
          if (h_ice(nm) .lt. -1e-10_fp .or. a_ice(nm) .lt. -1e-10_fp) then
             h_ice(nm) = 1e-10_fp
             a_ice(nm) = 1e-10_fp
          endif     
       endif
    enddo
    !
1234 continue        
end subroutine limtrp
