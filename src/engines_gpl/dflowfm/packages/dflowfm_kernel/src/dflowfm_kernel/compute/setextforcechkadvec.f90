!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! 
! 

 subroutine setextforcechkadvec()
 use m_flow
 use m_flowparameters, only: trshcorio
 use m_flowgeom
 use m_netw
 use MessageHandling
 use m_alloc
 use m_wind
 use m_sferic
 use m_xbeach_data, only: Fx, Fy, swave, Lwave, hminlw
 use m_fm_icecover, only: ice_p, fm_ice_update_press, ice_apply_pressure, ice_reduce_waves, ice_af, ice_apply_friction, ice_frctp, ice_frcuni, FRICT_AS_DRAG_COEFF

 implicit none

 integer          :: L,LL, Lb, Lt, k1,k2, kt1, kt2
 double precision :: dptot, tidp, trshcorioi, fmax, floc, dzt, dztm, alf
 double precision :: GradHinUc
 double precision :: p1, p2, wfac, Dzk
 double precision :: uixL, uiyL, uL, vL, uxL, uyL, duxL, duyL, duL, cdi, ice_shear

 trshcorioi = 1d0/trshcorio

 
if (jawind > 0) then

    if (kmx == 0) then
        do LL = 1,lnx
           if ( hu(LL) > 0 ) then
               wfac = 1d0
               if (jawindpartialdry == 1) then
                  Dzk  = abs( zk(lncn(1,LL)) - zk(lncn(2,LL)) )
                  if (Dzk > 0d0) then
                     wfac = min( 1d0, hu(LL) / Dzk )
                  endif
               endif
               ! wdsu/huvli = [(m^2/s^2)*m^-1]
               if (jawindhuorzwsbased == 0) then
                  adve(LL) = adve(LL) - wdsu(LL)*wfac/hu(LL)
               else
                  adve(LL) = adve(LL) - wdsu(LL)*wfac*huvli(LL)
               endif
           endif
        enddo

    else

        do LL  = 1,lnx
           if (hu(LL) > 0d0) then
               wfac = 1d0
               if (jawindpartialdry == 1) then
                  Dzk  = abs( zk(lncn(1,LL)) - zk(lncn(2,LL)) )
                  if (Dzk > 0d0) then
                     wfac = min( 1d0, hu(LL) / Dzk )
                  endif
               endif

               Lt = Ltop(LL)
               ! adve(Lt) = adve(Lt) - wdsu(LL) / max( toplayminthick, hu(Lt) - hu(Lt-1)  )

               alf = 1d0
               if (jawindhuorzwsbased == 0) then
                  dzt = hu(Lt) - hu(Lt-1)
               else
                  kt1 = ktop( ln(1,LL) ) ; kt2 = ktop( ln(2,LL) )
                  dzt = acL(LL)*(zws(kt1) - zws(kt1-1)) + (1d0-acL(LL))*(zws(kt2) - zws(kt2-1))
               endif
               if ( Lbot(LL) < Lt ) then
                  dztm  =  hu(Lt-1) - hu(Lt-2)
                  !if ( dzt < 0.8d0*dztm ) then
                  if ( dzt < 0.05d0 ) then
                     alf   =  dzt / ( dzt + dztm )
                     adve(Lt-1) = adve(Lt-1) - (1d0-alf)*wdsu(LL)*wfac / dztm
                  endif
               endif
               adve(Lt) = adve(Lt) - alf*wdsu(LL)*wfac / dzt
           endif
        enddo

    endif

 endif

 if (ice_apply_friction) then
    do LL = 1,lnx
       if ( hu(LL) > 0 ) then
          Lt = Ltop(LL)

          ! for the moment the ice is always stagnant
          uixL = 0d0
          uiyL = 0d0

          ! flow velocity
          uL  = U1(Lt)
          vL  =  v(Lt)
          uxL = uL*csu(LL) - vL*snu(LL)
          uyL = uL*snu(LL) + vL*csu(LL)
          
          ! velocity difference
          duxL = uixL - uxL
          duyL = uiyL - uyL
          duL    = sqrt( duxL*duxL + duyL*duyL )

          ! get drag coefficient
          if (ice_frctp == FRICT_AS_DRAG_COEFF) then
             cdi = ice_frcuni
          else
             cdi = 0d0
          endif

          wfac = 0.5d0 * (ice_af(ln(1,LL)) + ice_af(ln(2,LL)))
          ice_shear = wfac * cdi * duL * ( duxL*csu(LL) + duyL*snu(LL) )  ! * rhomean?
          
          if (kmx > 0) then
             alf = 1d0
             if (jawindhuorzwsbased == 0) then
                dzt = hu(Lt) - hu(Lt-1)
             else
                kt1 = ktop( ln(1,LL) )
                kt2 = ktop( ln(2,LL) )
                dzt = acL(LL)*(zws(kt1) - zws(kt1-1)) + (1d0-acL(LL))*(zws(kt2) - zws(kt2-1))
             endif
             if ( Lbot(LL) < Lt ) then
                dztm  =  hu(Lt-1) - hu(Lt-2)
                if ( dzt < 0.05d0 ) then
                   alf   =  dzt / ( dzt + dztm )
                   adve(Lt-1) = adve(Lt-1) - (1d0-alf) * ice_shear / dztm
                endif
             endif
             adve(Lt) = adve(Lt) - alf * ice_shear / dzt
             
          elseif (jawindhuorzwsbased == 0) then
             adve(LL) = adve(LL) - ice_shear / hu(LL)
          else
             adve(LL) = adve(LL) - ice_shear * huvli(LL)
          endif
       endif
    enddo
 endif

 if ((jawave==3.or.jawave==6) .and. .not. flowWithoutWaves) then
     ! if a SWAN computation is performed, add wave forces to adve
     ! This part is mainly based on the wave forces formulation (wsu) of Delft3D (cucnp.f90)

    if ( kmx.eq.0 ) then  ! 2D
       do L  = 1,lnx
          wfac = 1d0
          if (ice_reduce_waves) then
             wfac = wfac * (1.0d0 - 0.5d0 * (ice_af(ln(1,L)) + ice_af(ln(2,L))))
          endif
          adve(L) = adve(L) - wfac * wavfu(L)
       enddo
    else
       do LL  = 1,lnx
          wfac = 1d0
          if (ice_reduce_waves) then
             wfac = wfac * (1.0d0 - 0.5d0 * (ice_af(ln(1,LL)) + ice_af(ln(2,LL))))
          endif
          call getLbotLtop(LL,Lb,Lt)
          do L=Lb,Lt
             adve(L) = adve(L) - wfac * wavfu(L)           ! Dimensions [m/s^2]
          end do
       enddo
    end if
 endif

! JRE
 if (jawave .eq. 4) then                              ! wave forcing from XBeach
    if (lwave==1)  then
         if (kmx==0) then
            do L  = 1,Lnx
               wfac = 1d0
               if (ice_reduce_waves) then
                  wfac = wfac * (1.0d0 - 0.5d0 * (ice_af(ln(1,L)) + ice_af(ln(2,L))))
               endif
               adve(L) = adve(L) - wfac * wavfu(L)
            enddo
         else
            do L=1,lnx
               wfac = 1d0
               if (ice_reduce_waves) then
                  wfac = wfac * (1.0d0 - 0.5d0 * (ice_af(ln(1,L)) + ice_af(ln(2,L))))
               endif
               call getLbotLtop(L, Lb,Lt)
               if (Lt<Lb) cycle
               do LL = Lb, Lt
                  adve(LL) = adve(LL) - wfac * wavfu(LL)
               enddo
            enddo
         endif
    endif
 endif

 if (ice_apply_pressure) then
    call fm_ice_update_press(ag)
 endif

 if (japatm > 0 .or. jatidep > 0 .or. ice_apply_pressure) then
    do L  = 1,lnx
       if ( hu(L) > 0 ) then
          k1     = ln(1,L) ; k2 = ln(2,L)

          dptot = 0.0d0
          if (japatm > 0) then
             dptot  = dptot + (patm(k2)-patm(k1))*dxi(L)/rhomean
          endif
          if (ice_apply_pressure) then
             dptot  = dptot + (ice_p(k2)-ice_p(k1))*dxi(L)/rhomean
          endif
          if (jatidep > 0 .or. jaselfal > 0) then
             if (jatidep == 1) then 
                tidp = ( tidep(1,k2) - tidep(1,k1) )*dxi(L)
             else
                tidp = tidef(L)
             endif
             if ( hu(L) < trshcorio) then
                tidp = tidp*hu(L)*trshcorioi
             endif
             dptot = dptot - tidp
             if (jatidep == 1) then  ! todo: check if you now get desired outputting if jatidep==2
                tidef(L) = tidp      ! add to tidal forces
             endif
          endif
          
          if (kmx == 0) then
             adve(L) = adve(L) + dptot
          else
             do LL = Lbot(L), Ltop(L)
                adve(LL) = adve(LL) + dptot
             enddo
          endif
       endif
    enddo

    if ( jatidep>0 .or. jaselfal>0 .and. kmx.eq.0 ) then
       call comp_GravInput()
    end if

 endif

 ! Anti-creep
 if( kmx < 2 .and. jacreep == 1) then           ! A warning due to kmx<2 and anticreep on
     call mess(LEVEL_INFO, 'Error : Anti-creep must be switched off in a 1d/2d model!')
 endif

 if ( idensform > 0) then                                     ! Baroclinic pressure
    if ( jacreep == 1) then
       dsalL = 0d0
       dtemL = 0d0
       do L = 1,lnx
          if (hu(L) > 0d0) then
             call anticreep( L )
          endif
       enddo

     else

       call addbaroclinicpressure()

       if (abs(jabaroctimeint) == 2) then
           rho0 = rho                                           ! save rho
       else if (abs(jabaroctimeint) == 5) then
           if (jarhoxu > 0) then
              rho = rho0                                        ! restore rho
           endif
       endif
       jabaroctimeint = abs(jabaroctimeint)                     ! flag as initialised

    end if
 endif

 if ( jasecflow > 0 ) then                                         ! Secondary Flow

    do LL = 1,lnx
       call getcz( hu(LL), frcu(LL), ifrcutp(LL), czusf(LL), LL)   ! calculating chezy coefficient on the flow links
    enddo

    !
    if (kmx < 2) then
       call linkstocenterstwodoubles( czssf, czusf )               ! converting chezy cofficient to the flow nodes
       if( spirbeta > 0.0d0 ) then
          call get_spiralforce()
       endif
    else
       !call linkstocenterstwodoubles( czssf, czusf )
       call get_spiral3d()                                           ! compute equivalent secondary flow intensity
    endif
 end if

 if ( jaFrcInternalTides2D.gt.0 .and. kmx.eq.0 ) then   ! internal tides friction (2D only)
    call add_InternalTidesFrictionForces()
 end if

 if (chkadvd > 0) then                       ! niet droogtrekken door advectie, stress of wind (allen in adve)

    if (kmx == 0) then

       do L  = 1,lnx

          if ( hu(L) > 0 ) then
             k1      = ln(1,L) ; k2 = ln(2,L)

             if     (hs(k1)  < 0.5d0*hs(k2) ) then
                if (adve(L)  < 0 .and. hs(k1) < chkadvd ) then
                    adve(L)  = adve(L)*hs(k1) / chkadvd ! ; nochkadv = nochkadv + 1
                endif
             else if (hs(k2) < 0.5d0*hs(k1) ) then
                if (adve(L)  > 0 .and. hs(k2) < chkadvd ) then
                    adve(L)  = adve(L)*hs(k2) / chkadvd ! ; nochkadv = nochkadv + 1
                endif
             endif
          endif

       enddo

    else

       do LL  = 1,lnx
          if (hu(LL) > 0d0) then
             call getLbotLtop(LL,Lb,Lt)
             k1   = ln(1,LL) ; k2 = ln(2,LL)
              if      (hs(k1) < 0.5d0*hs(k2) ) then
                 do L = Lb, Lt
                   if (adve(L)  < 0 .and. hs(k1) < chkadvd ) then
                       adve(L)  = adve(L)*hs(k1) / chkadvd ! ; nochkadv = nochkadv + 1
                   endif
                 enddo
              else if (hs(k2) < 0.5d0*hs(k1) ) then
                 do L = Lb, Lt
                   if (adve(L)  > 0 .and. hs(k2) < chkadvd) then
                       adve(L)  = adve(L)*hs(k2) / chkadvd ! ; nochkadv = nochkadv + 1
                   endif
                 enddo
              endif
          endif
      enddo

    endif

    endif

   end subroutine setextforcechkadvec
