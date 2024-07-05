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

! fill observation stations array
subroutine fill_valobs()
   use m_flow
   use m_flowtimes
   use m_transport
   use m_fm_wq_processes, only: kbx, wqbot, waqoutputs
   use m_flowgeom
   use m_observations
   use m_sediment
   use m_waves, only: hwav, twav, phiwav, rlabda, uorb, ustokes
   use m_xbeach_data, only: R
   use m_ship
   use Timers
   use m_alloc
   use fm_statistical_output, only: model_is_3d

   implicit none

   integer :: i, ii, j, kk, k, kb, kt, klay, L, LL, Lb, Lt, LLL, k1, k2, k3, LLa, n, nlayb, nrlay, nlaybL, nrlayLx
   integer :: link_id_nearest
   integer :: kmx_const, kk_const, nlyrs
   double precision :: wavfac
   double precision :: dens, prsappr, drhodz, rhomea 
   double precision :: ux,uy,um
   double precision, allocatable :: wa(:,:)
   double precision, allocatable :: frac(:,:)
   double precision, allocatable :: poros(:)
   double precision, external    :: setrhofixedp
   double precision, allocatable :: ueux(:)
   double precision, allocatable :: ueuy(:)

   kmx_const = kmx
   nlyrs     = 0

   if (timon) call timstrt ( "fill_valobs", handle_extra(55))
   !
   if (.not. allocated(ueux)) then
      call realloc(ueux,ndkx,keepExisting=.false.,fill=0d0)
      call realloc(ueuy,ndkx,keepExisting=.false.,fill=0d0)
   endif
   !
   if (jawave>0) then
      if (jahissigwav==0) then
         wavfac = 1d0
      else
         wavfac = sqrt(2d0)
      endif
      if (allocated(wa)) deallocate(wa)
      allocate(wa(1:2,1:max(kmx,1)))
   endif
   
   call getucxucyeulmag(ndkx, ueux, ueuy, ucmag, jaeulervel, jahisvelocity)
   if (jahistaucurrent>0) then
      if (jawave==0 .or. flowWithoutWaves) then
         ! fill taus
         call gettaus(1,1)

         ! get vector comps
         if (kmx==0) then
            do k = 1, ndx
               workx(k) = taus(k)*ueux(k)/max(ucmag(k),1d-4)
               worky(k) = taus(k)*ueuy(k)/max(ucmag(k),1d-4)
            enddo
         else
            do k = 1, ndx
               call getkbotktop(k,kb,kt)
               ux = ueux(kb); uy = ueuy(kb)
               um = max(hypot(ux,uy),1d-4)
               workx(k) = taus(k)*ux/um  
               worky(k) = taus(k)*uy/um   
            enddo
         endif
      else
         call gettauswave(jawaveswartdelwaq)
      endif
   endif
   !
   if (stm_included .and. jased>0) then
      if (stmpar%morlyr%settings%iunderlyr==2) then
         if (allocated(frac) ) deallocate(frac)
         allocate( frac(stmpar%lsedtot,1:stmpar%morlyr%settings%nlyr) )
         frac = dmiss
         if (allocated(poros) ) deallocate(poros)
         allocate( poros(1:stmpar%morlyr%settings%nlyr) )
         poros=dmiss
      endif
   endif


   valobs = DMISS
   do i = 1,numobs+nummovobs
      k = max(kobs(i),1)
      link_id_nearest = lobs(i)
      if ( kobs(i) > 0 ) then  ! rely on reduce_kobs to have selected the right global flow nodes

         if ( model_is_3D() ) then
            call getkbotktop(k,kb,kt)
            call getlayerindices(k, nlayb, nrlay)
            call reconstructucz(k)
         else
            kb = k
            kt = k
            nlayb = 1
         end if

         if (jawave > 0 .and. .not. flowWithoutWaves) then
            wa = 0d0
            call linkstocentercartcomp(k,ustokes,wa)      ! wa now 2*1 value or 2*1 vertical slice
         endif

!        store values in valobs work array
         valobs(i,:)        = dmiss   ! Intended to have dmiss on inactive layers for output.
                                      ! It is taken care of in subroutine reduce_valobs for parallel computation.
         valobs(i,IPNT_S1)  = s1(k)
         if (nshiptxy > 0) then
             if ( allocated(zsp) ) then
                valobs(i,IPNT_S1)  = valobs(i,IPNT_S1) + zsp(k)
             endif
         endif

         valobs(i,IPNT_HS)  = s1(k) - bl(k)

         valobs(i,IPNT_BL)  = bl(k)

         valobs(i,IPNT_CMX) = cmxobs(i)
         if (jawind > 0) then
            valobs(i,IPNT_wx) = 0d0
            valobs(i,IPNT_wy) = 0d0
            do LL=1,nd(k)%lnx
               LLL = iabs(nd(k)%ln(LL))
               k1 = ln(1,LLL) ; k2 = ln(2,LLL)
               k3 = 1 ; if( nd(k)%ln(LL) > 0 ) k3 = 2
               valobs(i,IPNT_wx) = valobs(i,IPNT_wx) + wx(LLL) * wcL(k3,LLL)
               valobs(i,IPNT_wy) = valobs(i,IPNT_wy) + wy(LLL) * wcL(k3,LLL)
            enddo
         endif
         if (jaPATM > 0 .and. allocated(patm)) then
            valobs(i,IPNT_PATM)  = PATM(k)
         ENDIF

         if ( jawave.eq.4 .and. allocated(R) ) then
            valobs(i,IPNT_WAVER) = R(k)
         end if

         if (jawave>0 .and. allocated(hwav)) then
            valobs(i,IPNT_WAVEH) = hwav(k)*wavfac
            valobs(i,IPNT_WAVET) = twav(k)
            if (.not. flowWithoutWaves) then
               valobs(i,IPNT_WAVED) = modulo(270d0 - phiwav(k), 360d0)  ! Direction from
               valobs(i,IPNT_WAVEL) = rlabda(k)
               valobs(i,IPNT_WAVEU) = uorb(k)
            endif
         endif

         if (jahistaucurrent>0) then
            valobs(i,IPNT_TAUX) = workx(k)
            valobs(i,IPNT_TAUY) = worky(k)
         endif

         if (stm_included .and. jased>0) then
            do j = IVAL_SBCX1, IVAL_SBCXN
               ii = j-IVAL_SBCX1+1
               valobs(i,IPNT_SBCX1+ii-1)=sedtra%sbcx(k,ii)
            enddo
            do j = IVAL_SBCY1, IVAL_SBCYN
               ii = j-IVAL_SBCY1+1
               valobs(i,IPNT_SBCY1+ii-1)=sedtra%sbcy(k,ii)
            enddo
            do j = IVAL_SSCX1, IVAL_SSCXN
               ii = j-IVAL_SSCX1+1
               valobs(i,IPNT_SSCX1+ii-1)=sedtra%sscx(k,ii)
            enddo
            do j = IVAL_SSCY1, IVAL_SSCYN
               ii = j-IVAL_SSCY1+1
               valobs(i,IPNT_SSCY1+ii-1)=sedtra%sscy(k,ii)
            enddo
            if (jawave>0 .and. .not. flowWithoutWaves) then
               do j = IVAL_SBWX1, IVAL_SBWXN
                  ii = j-IVAL_SBWX1+1
                  valobs(i,IPNT_SBWX1+ii-1)=sedtra%sbwx(k,ii)
               enddo
               do j = IVAL_SBWY1, IVAL_SBWYN
                  ii = j-IVAL_SBWY1+1
                  valobs(i,IPNT_SBWY1+ii-1)=sedtra%sbwy(k,ii)
               enddo
               do j = IVAL_SSWX1, IVAL_SSWXN
                  ii = j-IVAL_SSWX1+1
                  valobs(i,IPNT_SSWX1+ii-1)=sedtra%sswx(k,ii)
               enddo
               do j = IVAL_SSWY1, IVAL_SSWYN
                  ii = j-IVAL_SSWY1+1
                  valobs(i,IPNT_SSWY1+ii-1)=sedtra%sswy(k,ii)
               enddo
            endif
            !
            valobs(i,IPNT_TAUB) = sedtra%taub(k)  ! contains tausmax or Soulsby-Clarke shear stresses
            ! bed composition
            if (stmpar%morlyr%settings%iunderlyr==1) then
               do j = IVAL_BODSED1, IVAL_BODSEDN
                  ii = j-IVAL_BODSED1+1
                  valobs(i,IPNT_BODSED1+ii-1) = stmpar%morlyr%state%bodsed(ii,k)
               enddo
               valobs(i,IPNT_DPSED) = stmpar%morlyr%state%dpsed(k)
            elseif (stmpar%morlyr%settings%iunderlyr==2) then
               nlyrs=stmpar%morlyr%settings%nlyr
               do l = 1, stmpar%lsedtot
                  if (stmpar%morlyr%settings%iporosity==0) then
                     dens = stmpar%sedpar%cdryb(l)
                  else
                     dens = stmpar%sedpar%rhosol(l)
                  endif
                  do n = 1, stmpar%morlyr%settings%nlyr
                     if (stmpar%morlyr%state%thlyr(n,k)>0.0_fp) then  ! lyrfrac
                        frac(l, n) = stmpar%morlyr%state%msed(l, n, k)/(dens*stmpar%morlyr%state%svfrac(n, k) * &
                                     stmpar%morlyr%state%thlyr(n, k))
                     else
                        frac(l, n) = 0d0
                     endif
                  enddo
               enddo
               !
               if (stmpar%morlyr%settings%iporosity>0) then
                  poros = 1d0-stmpar%morlyr%state%svfrac(:,k)
               endif
               !
               do klay = 1, nlyrs
                  do j=IVAL_MSED1,IVAL_MSEDN
                     ii = j-IVAL_MSED1+1
                     valobs(i,IPNT_MSED1+(ii-1)*nlyrs+klay-1) = stmpar%morlyr%state%msed(ii, klay, k)
                  end do
                  !
                  do j=IVAL_LYRFRAC1,IVAL_LYRFRACN
                     ii = j-IVAL_LYRFRAC1+1
                     valobs(i,IPNT_LYRFRAC1+(ii-1)*nlyrs+klay-1) = frac(ii,klay)
                  end do
                  !
                  valobs(i,IPNT_POROS+klay-1) = poros(klay)
                  valobs(i,IPNT_THLYR+klay-1) = stmpar%morlyr%state%thlyr(klay, k)
               end do
            endif
            !
            do j = IVAL_FRAC1, IVAL_FRACN
               ii = j-IVAL_FRAC1+1
               valobs(i,IPNT_FRAC1+ii-1) = sedtra%frac(k,ii)
            enddo
            valobs(i,IPNT_MUDFRAC)  = sedtra%mudfrac(k)
            valobs(i,IPNT_SANDFRAC) = sedtra%sandfrac(k)
            !
            if (stmpar%morpar%flufflyr%iflufflyr>0 .and. stmpar%lsedsus>0) then
               do j = IVAL_MFLUFF1, IVAL_MFLUFFN
                  ii = j-IVAL_MFLUFF1+1
                  valobs(i,IPNT_MFLUFF1+ii-1) = stmpar%morpar%flufflyr%mfluff(ii,k)
               enddo
            endif
            !
            do j = IVAL_FIXFAC1, IVAL_FIXFACN
               ii = j-IVAL_FIXFAC1+1
               valobs(i,IPNT_FIXFAC1+ii-1)=sedtra%fixfac(k,ii)
            enddo
            !
            do j = IVAL_HIDEXP1, IVAL_HIDEXPN
               ii = j-IVAL_HIDEXP1+1
               valobs(i,IPNT_HIDEXP1+ii-1)=sedtra%hidexp(k,ii)
            enddo
            !
            if (stmpar%lsedsus>0) then
               do j = IVAL_SOUR1, IVAL_SOURN
                  ii = j-IVAL_SOUR1+1
                  valobs(i,IPNT_SOUR1+ii-1)=sedtra%sourse(k,ii)
               enddo
               do j = IVAL_SINK1, IVAL_SINKN
                  ii = j-IVAL_SINK1+1
                  valobs(i,IPNT_SINK1+ii-1)=sedtra%sinkse(k,ii)
               enddo
            endif
         endif
         !
         if ( IVAL_WQB1 > 0 ) then
            do j=IVAL_WQB1,IVAL_WQBN
               ii = j-IVAL_WQB1+1
               valobs(i,IPNT_WQB1+ii-1) = wqbot(ii,kb)
            end do
         end if

         if ( model_is_3D() ) then
            valobs(i,IPNT_UCXQ) = ucx(k)
            valobs(i,IPNT_UCYQ) = ucy(k)
         endif

         do kk=kb,kt
            klay = kk-kb+nlayb

            if ( model_is_3D() ) then
               valobs(i,IPNT_ZCS+klay-1) = 0.5d0*( zws(kk)+zws(kk-1) )
            endif

            if (jahisvelocity>0 .or. jahisvelvec>0) then
               valobs(i,IPNT_UCX+klay-1) = ueux(kk)
               valobs(i,IPNT_UCY+klay-1) = ueuy(kk)
            endif

            if (jawave > 0 .and. .not. flowWithoutWaves) then
               if (hs(k)>epshu) then
                  if (kmx==0) then
                     kk_const = 1
                  else
                     kk_const = klay
                  endif
                  valobs(i,IPNT_UCXST+klay-1) = wa(1,kk_const)
                  valobs(i,IPNT_UCYST+klay-1) = wa(2,kk_const)
               endif
            endif

            if ( model_is_3D() ) then
               valobs(i,IPNT_UCZ+klay-1)  = ucz(kk)
            end if
            if ( jasal > 0 ) then
               valobs(i,IPNT_SA1+klay-1)  = constituents(isalt, kk)
            end if
            if ( jatem > 0 ) then
               valobs(i,IPNT_TEM1+klay-1) = constituents(itemp, kk)
            end if
            if ((jasal > 0 .or. jatem > 0 .or. jased > 0) .and. jahisrho > 0) then
               if ( density_is_pressure_dependent() ) then
                  valobs(i,IPNT_RHOP+klay-1) = setrhofixedp(kk, 0d0)
                  valobs(i,IPNT_RHO +klay-1) = rho(kk)
               else
                  valobs(i,IPNT_RHOP+klay-1) = rho(kk)
               end if
            end if
            if (jahisvelocity > 0) then
               valobs(i,IPNT_UMAG+klay-1) = ucmag(kk)
            end if
            valobs(i,IPNT_QMAG+klay-1) = 0.5d0*(squ(kk)+sqi(kk))
            
            if (kmx==0) then
               kmx_const = 1     ! to make numbering below work
            end if

            if ( IVAL_TRA1 > 0 ) then
               do j=IVAL_TRA1,IVAL_TRAN
                  ii = j-IVAL_TRA1+1
                  valobs(i,IPNT_TRA1+(ii-1)*kmx_const+klay-1) = constituents(ITRA1+ii-1, kk)
               end do
            end if

            if ( IVAL_HWQ1 > 0 ) then
               do j=IVAL_HWQ1,IVAL_HWQN
                  ii = j-IVAL_HWQ1+1
                  valobs(i,IPNT_HWQ1+(ii-1)*kmx_const+klay-1) = waqoutputs(ii,kk-kbx+1)
               end do
            end if

            if ( IVAL_WQB3D1 > 0 ) then
               do j=IVAL_WQB3D1,IVAL_WQB3DN
                  ii = j-IVAL_WQB3D1+1
                  valobs(i,IPNT_WQB3D1+(ii-1)*kmx_const+klay-1) = wqbot(ii,kk)
               end do
            end if

            if ( IVAL_SF1 > 0 ) then
               do j=IVAL_SF1,IVAL_SFN
                  ii = j-IVAL_SF1+1
                  valobs(i,IPNT_SF1+(ii-1)*kmx_const+klay-1) = constituents(ISED1+ii-1, kk)
               end do
            end if

            if (kmx==0 .and. IVAL_WS1 >0) then
               do j=IVAL_WS1,IVAL_WSN
                  ii = j-IVAL_WS1+1
                  valobs(i,IPNT_WS1+(ii-1)*kmx_const+klay-1) = mtd%ws(kk, ii)   ! 1:lsedsus
               end do
            end if

            if ( jased > 0 .and. .not. stm_included) then
               valobs(i,IPNT_SED+klay-1) = sed(1, kk)
            end if
            valobs(i,IPNT_CMX) = max( valobs(i,IPNT_UCX), sqrt( ucx(kk)**2 + ucy(kk)**2 )  )
         end do
         valobs(i,IPNT_SMX) = max( smxobs(i), s1(k) )

         if ( model_is_3D() ) then
            call getkbotktop(k, kb, kt)
            call getlayerindices(k, nlayb, nrlay)
            do kk = kb-1, kt
               klay = kk - kb + nlayb + 1
               valobs(i, IPNT_ZWS+klay-1) = zws(kk)
               if (iturbulencemodel >= 2) then
                  valobs(i, IPNT_VICWWS + klay - 1) = vicwws(kk)
               end if
               if ((jasal > 0 .or. jatem > 0 .or. jased > 0) .and. jahisrho > 0) then
                  if (zws(kt) - zws(kb-1) > epshu .and. kk > kb-1 .and. kk < kt ) then
                     if ( density_is_pressure_dependent() ) then
                        prsappr = ag*rhomean*( zws(kt) - zws(kk) )  
                        drhodz  = ( setrhofixedp(kk+1,prsappr) - setrhofixedp(kk,prsappr) ) / max(0.5d0*(zws(kk+1) - zws(kk-1)),epshs)
                     else 
                        drhodz  = ( rho(kk+1) - rho(kk)                                   ) / max(0.5d0*(zws(kk+1) - zws(kk-1)),epshs) 
                     end if
                     rhomea  = 0.5d0*( rho(kk+1) + rho(kk) )
                     valobs(i,IPNT_BRUV+klay-1) = -ag*drhodz/rhomea
                  end if
               end if
               if ( IVAL_WS1 > 0 ) then
                  do j=IVAL_WS1,IVAL_WSN
                     ii = j-IVAL_WS1+1
                     valobs(i,IPNT_WS1+(ii-1)*(kmx+1)+klay-1) =mtd%ws(kb+klay-2, ii)
                  end do
               end if
               if ( IVAL_SEDDIF1 > 0 ) then
                  do j=IVAL_SEDDIF1,IVAL_SEDDIFN
                     ii = j-IVAL_SEDDIF1+1
                     valobs(i,IPNT_SEDDIF1+(ii-1)*(kmx+1)+klay-1) =mtd%seddif(ii, kb+klay-2)
                  end do
               end if
            end do
            
            call getLbotLtop(link_id_nearest, Lb, Lt)
            call getlayerindicesLmax(link_id_nearest, nlaybL, nrlayLx)
            do L = Lb-1, Lt
               klay = L-Lb+nlaybL+1
               valobs(i, IPNT_ZWU + klay-1) = min(bob(1,link_id_nearest), bob(2,link_id_nearest)) + hu(L)
               if (iturbulencemodel >= 2) then
                   valobs(i, IPNT_VICWWU + klay-1) = vicwwu(L)
               end if
               if (iturbulencemodel >= 3) then
                   valobs(i, IPNT_TKIN + klay-1) = turkin1(L)
                   valobs(i, IPNT_TEPS + klay-1) = tureps1(L)
               end if
               if (idensform > 0 .and. jaRichardsononoutput > 0) then
                   valobs(i, IPNT_RICH + klay-1) = rich(L)
               end if
            end do
         end if

!        Rainfall
         if (jarain > 0 .and. jahisrain > 0) then
            valobs(i,IPNT_RAIN) = rain(k)
         end if
         
         if (allocated(airdensity) .and. jahis_airdensity > 0) then
            valobs(i,IPNT_AIRDENSITY) = airdensity(k)
         end if

!        Infiltration
         if ((infiltrationmodel == DFM_HYD_INFILT_CONST .or. infiltrationmodel == DFM_HYD_INFILT_HORTON) .and. jahisinfilt > 0) then
            valobs(i,IPNT_INFILTCAP) = infiltcap(k)*1d3*3600d0 ! m/s -> mm/hr
            if (ba(k) > 0d0) then
               valobs(i,IPNT_INFILTACT) = infilt(k)/ba(k)*1d3*3600d0 ! m/s -> mm/hr
            else
               valobs(i,IPNT_INFILTACT) = 0d0
            end if
         end if

!        Heatflux
         if (jatem > 0 .and. jahisheatflux > 0) then
            call getlink1(k,LL)
            if ( jawind > 0 ) then
               valobs(i,IPNT_WIND) = sqrt(wx(LL)*wx(LL) + wy(LL)*wy(LL))
            end if

            if ( jatem > 1 ) then   ! also heat modelling involved
               valobs(i,IPNT_TAIR) = Tair(k)
            end if

            if (jatem == 5 .and. allocated(Rhum) .and. allocated(Clou) ) then
               valobs(i,IPNT_RHUM) = Rhum(k)
               valobs(i,IPNT_CLOU) = Clou(k)
            endif

            if (jatem == 5) then
               valobs(i,IPNT_QSUN) = Qsunmap(k)
               valobs(i,IPNT_QEVA) = Qevamap(k)
               valobs(i,IPNT_QCON) = Qconmap(k)
               valobs(i,IPNT_QLON) = Qlongmap(k)
               valobs(i,IPNT_QFRE) = Qfrevamap(k)
               valobs(i,IPNT_QFRC) = Qfrconmap(k)
            endif

            if (jatem > 1) then
               valobs(i,IPNT_QTOT) = Qtotmap(k)
            end if
         end if
      else
         valobs(i,:) = DMISS
      end if
   end do

!  No need to copy empty layers from top anymore, they have been filled with dmiss

   if (allocated(wa)) deallocate(wa)

   if (timon) call timstop (handle_extra(55))
   return
   end subroutine fill_valobs
