!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2023.                                
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

subroutine heatun(n, timhr, qsno)
use m_flow
use m_flowgeom
use m_sferic
use m_itdate
use unstruc_model
use m_flowtimes
use m_heatfluxes
 use m_transport, only: constituents, itemp

implicit none

double precision, intent (in) :: timhr, qsno
integer         , intent (in) :: n

integer          :: i, k, kb, kt, k2, L, LL, j, j2, ncols, lunadh = 0 , jafree = 0 ! D3D
double precision :: rlon, rlat, sc, qsn, qsu, qsnom, presn, tairn, twatn, twatK, rhumn, cloun, windn
double precision :: ce, ch, qwmx, qahu, tl, Qcon, Qeva, Qlong, sg, pvtamx, pvtwmx, pvtahu, delvap
double precision :: dexp, zlo, zup, explo, expup, ratio, rcpiba, qheat, atot

double precision :: w(20), Qtot, Qfree, b, gred, wfree, Qfrcon, Qfreva, rhoa0, rhoa10

double precision :: prair=0.7d0, pr2=.49d0, xnuair=16d-6, cfree=0.14d0

double precision :: rdry=287.05d-2 , rvap=461.495d-2 , evafac = 1d0

double precision :: hlc, arn, wxL, wyL, uL, vL, uxL, uyL, bak2, twatb

double precision :: qsunsoil, qwatsoil, watsoiltransfer, rdtsdz, soiltemprev, pvtamxB, pvtwmxB


presn   = 1d-2*paver                 ! Air pressure (mbar)
rhumn   = 1d-2*backgroundhumidity    ! ( )
cloun   = 1d-2*backgroundcloudiness  ! ( )
ce      = Dalton                     ! Dalton  number = 1.50e-3 (Gill, 1982)           evaporative flux
ch      = Stanton                    ! Stanton number = 1.45e-3 (Friehe&Schmitt, 1976) convective heat flux
qsu     = 0d0
qsnom   = qsno
call getlink1(n,L)
if (relativewind > 0d0) then
    wxL = wx(L) - relativewind*ucx(ktop(n))
    wyL = wy(L) - relativewind*ucy(ktop(n))
else
    wxL = wx(L)
    wyL = wy(L)
endif
windn  = sqrt( wxL*wxL + wyL*wyL )

call getkbotktop(n,kb,kt)

twatn = constituents(itemp, kt)
if (surftempsmofac > 0d0) then
   arn    = ba(n)
   twatn  = twatn*arn 
   do LL  = 1,nd(n)%lnx
      L   = iabs( nd(n)%ln(LL) )
      k2  = ln(1,L) + ln(2,L) - n
      if (hs(k2) > epshstem) then
         bak2  = surftempsmofac*ba(k2)
         twatn = twatn + constituents(itemp, ktop(k2))*bak2
         arn   = arn   + bak2
      endif
   enddo
   if (arn  > 0d0) then
      twatn = twatn / arn
   endif
endif

tairn = tair(n)

if (jatem == 3) then                 ! excess

   hlc    = 4.48d0 + 0.049d0 * twatn + fwind * ( 3.5d0 + 2.05d0*windn ) * ( 1.12d0 + 0.018d0*twatn + 0.00158d0*twatn**2 )

   qheat  = -hlc*(twatn-tairn)
   rcpiba = rcpi*ba(n)
   heatsrc0(kt) = heatsrc0(kt) + qheat*rcpiba  ! fill heat source array

   if (jamapheatflux > 0 .or. jahisheatflux > 0) then          ! todo, only at mapintervals
      Qtotmap(n) = qheat
   endif

else if (jatem == 5) then

   rhumn  = min(1d0, max(0d0, 1d-2*rhum(n) ) )
   cloun  = min(1d0, max(0d0, 1d-2*clou(n) ) )

   if (japatm > 0) then
       presn = 1d-2*patm(n)
   endif
                             ! Solar radiation restricted by presence of clouds and reflection of water surface (albedo)
   if (jasol == 1) then      ! Measured solar radiation qradin specified in .tem file
      qsu = qrad(n) * (1d0-albedo)
   else                      ! Calculate solar radiation from cloud coverage specified in file
      if (jsferic == 1) then
          call qsun_nominal(xz(n), yz(n), timhr, qsnom)
      endif
      if (qsnom > 0d0) then
         qsu   = qsnom * (1d0 - 0.40d0*cloun - 0.38d0*cloun*cloun) * (1d0-albedo)
      else
         qsu   = 0d0
      endif
   endif

   rcpiba = rcpi*ba(n)
   qsn    = qsu*rcpiba

   if (qsn > 0d0 ) then

      if (kmx > 0) then     ! distribute incoming radiation over water column
         ! zab  = (min(0.5d0*hs(n) ,Secchidepth)) / 1.7d0

         if (Secchidepth2 > 0d0) then
            j2 = 2
         else
            j2 = 1
         endif

         do j=j2,1,-1

            if (j==1 .and. jaSecchisp > 0) then
               zab(1) = Secchisp(n) / 1.7d0
            endif

            zlo   = 0d0
            explo = 1d0

            do k  = kt,kb,-1
               zup     = zlo ; expup   = explo
               zlo     = zws(kt) - zws(k-1)
               ratio   = zlo/zab(j)
               if (ratio   > 4d0) then !  .or. k.eq.kb) then
                  explo    = 0.0
               else
                  explo    = exp(-ratio)
               endif
               dexp        = expup-explo
               if (dexp > 0d0) then
                  heatsrc0(k) = heatsrc0(k) + sfr(j)*qsn*dexp
               else
                  exit
               endif
            enddo
         enddo

      else
         heatsrc0(n) = heatsrc0(n) + qsn
      endif

   endif

   if (kmx > 0 .and. Soiltempthick > 0) then
       if (qsn > 0d0) then
          qsunsoil = qsu*explo
       else
          qsunsoil = 0d0
       endif
       watsoiltransfer = 1d0/(0.5d0*Soiltempthick)           ! thermalcond sand = 0.15 -> 4 for dry -> saturated, [W/mK]
       twatb           = constituents(itemp, kb)
       qwatsoil        = watsoiltransfer*( twatb - tbed(n) )
       heatsrc0(kb)    = heatsrc0(kb) - rcpiba*qwatsoil
       rdtsdz          = rcpi*dts/Soiltempthick
       tbed(n)         = ( tbed(n) + rdtsdz*( qsunsoil + watsoiltransfer* twatb )  ) / ( 1d0 + watsoiltransfer*rdtsdz )
   endif


   ! PVTWMX = PVapour at TWater and MaX relative humidity
   ! PVTAMX = PVapour at TAir   and MaX relative humidity
   pvtamx = 10d0**((0.7859d0+0.03477d0*tairn)/(1d0+0.00412d0*tairn)) ! saturation pressure of water vapour in air remote (ewl)
   pvtwmx = 10d0**((0.7859d0+0.03477d0*twatn)/(1d0+0.00412d0*twatn)) ! and near water surface (ew); eq.(A.12):

   !pvtamxB = 6.1121d0*exp( (18.678d0 - (tairn/234.5d0))*(tairn/(257.14d0+tairn) ) )  ! Buck
   !pvtwmxB = 6.1121d0*exp( (18.678d0 - (twatn/234.5d0))*(twatn/(257.14d0+twatn) ) )  ! Buck

   pvtahu = rhumn*pvtamx                                             ! vapour pressure in air remote (eal)

   qwmx   = (0.62d0*pvtwmx)/(presn - 0.38d0*pvtwmx)                  ! specific humidity of air remote and
   qahu   = (0.62d0*pvtahu)/(presn - 0.38d0*pvtahu)                  ! saturated air near water surface; eq.(A.9)+(A.10):

   tl     = 2.5d6 - 2.3d3*twatn                                      ! latent heat tl; eq.(A.19.b): (J/kg)

   if (Stanton < 0) then                                             ! if specified negative, use windspeed dependent Cd coeff
       ch = abs(Stanton)*cdwcof(L)
   endif
   if (Dalton < 0) then                                              ! if specified negative, use windspeed dependent Cd coeff
       ce = abs(Dalton )*cdwcof(L)
   endif

   delvap = qwmx-qahu               ! D3D, both positive and negative evaporation, cannot be correct
   if (jadelvappos == 1) then
      delvap = max(0d0, delvap)     ! DPM, DFM This must be positive, otherwise heat is pumped into water
   endif                            ! causing air to cool down below prescribed temperature, immedia. and

   Qeva   = -ce*rhoair*windn*delvap*tl                            ! heat loss of water by evaporation eq.(A.19.a); Dalton number is ce:

   Qcon   = -ch*rcpa*windn*(twatn-tairn)                          ! heat loss of water by convection eq.(A.23); Stanton number is ch:

   twatK  =  twatn + tkelvn
   if (jalongwave > 0) then
      Qlong = em * (longwave(n) - stf*(twatK**4))                   ! difference between prescribed long wave downward flux and calculated upward flux
   else
      Qlong  = -em*stf*(twatK**4)*(0.39d0-0.05d0*sqrt(pvtahu))       ! heat loss by effective infrared back radiation hl, restricted by
      Qlong  =  Qlong*(1d0 - 0.6d0*cloun**2 )                        !  presence of clouds and water vapour in air; eq.(A.22):
   endif

   Qfree  = 0d0 ; Qfrcon = 0d0 ; Qfreva = 0d0                     ! Contribution by free convection:
   rhoa0  = ((presn-pvtwmx)/rdry + pvtwmx/rvap) / (Twatn + Tkelvn)
   rhoa10 = ((presn-pvtahu)/rdry + pvtahu/rvap) / (Tairn + Tkelvn)
   if (jaroro > 0) then
      if (jaroro == 2) then
         roair(n) = rhoa0
      else if (jaroro == 3) then
         roair(n) = rhoa10
      else if (jaroro == 4) then
         roair(n) = 0.5d0*(rhoa10+rhoa0)
      endif
   endif
   gred   = 2d0*ag*(rhoa10-rhoa0)/(rhoa0+rhoa10)
   if (gred > 0d0) then                                           ! Ri= (gred/DZ)/ (du/dz)2, Ri>0.25 stable
       wfree  =  gred*xnuair/pr2
       wfree  =  cfree*wfree**0.33333333d0
       Qfrcon = min(0d0, -rcpa*wfree*(twatn-tairn)*evafac )                   ! Free convective sensible heat loss:
       Qfreva = min(0d0, -wfree*(qwmx-qahu)*tl*evafac*(rhoa0+rhoa10)*0.5d0 )  ! Free convective latent/evaporation heat loss:
       Qfree  =  Qfrcon + Qfreva
   endif

   qheat = Qeva + Qcon + Qlong + Qfree                            ! net heat flux [W/m^2] into water, solar radiation excluded:
   if (jaevap > 0) then
      evap(n) = (Qeva+Qfreva)/(tL*rhomean)                        ! (J/sm2)/(J/kg)/kg/m3) = (m/s)
   endif

   heatsrc0(kt) = heatsrc0(kt) + qheat*rcpiba                     ! fill heat source array

   if (jamapheatflux > 0 .or. jahisheatflux > 0) then ! todo, only at mapintervals
      Qsunmap(n)   = Qsu
      Qevamap(n)   = Qeva
      Qconmap(n)   = Qcon
      Qlongmap(n)  = Qlong
      Qfrevamap(n) = Qfreva
      Qfrconmap(n) = Qfrcon
      Qtotmap(n)   = Qsu + qheat
   endif

  !if (ti_xls > 0) then

  Atot  = 0d0                        ! these 2 lines outside loop
  w     = 0d0                        ! array of spatially averaged output

  b     = ba(n)                   ! Spatially averaged time series output :
  atot  = atot + b                ! Total area
  w( 1) = timhr/24d0              ! Time in days
  w( 2) = w( 2) + b*tairn         ! tair
  w( 3) = w( 3) + b*constituents(itemp, kt)      ! Twatn, SST
  if (soiltempthick > 0d0) then
     w( 4) = w( 4) + b*tbed(n)       ! tbed
  endif
  w( 5) = w( 5) + b*(qsu + qheat) ! qtot
  w( 6) = w( 6) + b*Qsu           ! Qsun
  w( 7) = w( 7) + b*Qlong         ! QLw
  w( 8) = w( 8) + b*Qcon          ! Qcon
  w( 9) = w( 9) + b*Qeva          ! Qeva
  w(10) = w(10) + b*Qfrcon        ! Qfreecon
  w(11) = w(11) + b*Qfreva        ! Qfree
  w(12) = w(12) + b*windn         ! wind
  w(13) = w(13) + b*rhumn         ! rhum
  w(14) = w(14) + b*cloun         ! clou
  w(15) = w(15) + b*presn         ! pres

  ncols = 15
  if (Atot  > 0d0) then
      w(2:ncols) = w(2:ncols) / Atot
  endif
  Qsunav = w(6) ; Qlongav = w(7) ; Qconav = w(8) ; Qevaav = w(9) ; Qfrconav = w(10) ; Qfrevaav = w(11)

     !if (lunadh == 00) then
     !    call newfil(lunadh, trim(md_ident)//'Spatially_Averaged_Heatfluxes.tek')
     !    write(lunadh,'(a)') '* column  1: minut :'
     !    write(lunadh,'(a)') '* column  2: T_air :'
     !    write(lunadh,'(a)') '* column  3: SST   :'
     !    write(lunadh,'(a)') '* column  4: T_bed :'
     !    write(lunadh,'(a)') '* column  5: Q_tot :'
     !    write(lunadh,'(a)') '* column  6: Qsun  : input solar radiation   '
     !    write(lunadh,'(a)') '* column  7: Qlw   : back radiation'
     !    write(lunadh,'(a)') '* column  8: Qcon  : convection'
     !    write(lunadh,'(a)') '* column  9: Qeva  : evaporation'
     !    write(lunadh,'(a)') '* column 10: Qfrconav : free convection'
     !    write(lunadh,'(a)') '* column 11: Qfrevaav : free evaporation'
     !    write(lunadh,'(a)') '* column 12: Wind  :'
     !    write(lunadh,'(a)') '* column 13: Rhum  :'
     !    write(lunadh,'(a)') '* column 14: Clou  :'
     !    write(lunadh,'(a)') '* column 15: Pres  :'
     !    write(lunadh,'(a)') 'BL01'
     !    write(lunadh,'(a)') '105125   15 '
     !endif
     !write(lunadh,'(20(1x,g12.6))') ( w(i), i = 1,ncols )
  !endif

endif

end subroutine heatun
