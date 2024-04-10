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

   implicit none

   integer :: i, ii, j, kk, k, kb, kt, klay, L, LL, Lb, Lt, LLL, k1, k2, k3, LLa, n, nlayb, nrlay, nlaybL, nrlayLx
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
      if ( kobs(i).gt.0 ) then  ! rely on reduce_kobs to have selected the right global flow nodes

         if ( kmx.gt.0 ) then
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
         valobs(:,i)        = dmiss   ! Intended to have dmiss on inactive layers for output.
                                      ! It is taken care of in subroutine reduce_valobs for parallel computation.
         valobs(IPNT_S1,i)  = s1(k)
         if (nshiptxy > 0) then
             if ( allocated(zsp) ) then
                valobs(IPNT_S1,i)  = valobs(IPNT_S1,i) + zsp(k)
             endif
         endif

         valobs(IPNT_HS,i)  = s1(k) - bl(k)

         valobs(IPNT_BL,i)  = bl(k)

         valobs(IPNT_CMX,i) = cmxobs(i)
         if (jawind > 0) then
            valobs(IPNT_wx,i) = 0d0
            valobs(IPNT_wy,i) = 0d0
            do LL=1,nd(k)%lnx
               LLL = iabs(nd(k)%ln(LL))
               k1 = ln(1,LLL) ; k2 = ln(2,LLL)
               k3 = 1 ; if( nd(k)%ln(LL) > 0 ) k3 = 2
               valobs(IPNT_wx,i) = valobs(IPNT_wx,i) + wx(LLL) * wcL(k3,LLL)
               valobs(IPNT_wy,i) = valobs(IPNT_wy,i) + wy(LLL) * wcL(k3,LLL)
            enddo
         endif
         if (jaPATM > 0 .and. allocated(patm)) then
            valobs(IPNT_PATM,i)  = PATM(k)
         ENDIF

         if ( jawave.eq.4 .and. allocated(R) ) then
            valobs(IPNT_WAVER,i) = R(k)
         end if

         if (jawave>0 .and. allocated(hwav)) then
            valobs(IPNT_WAVEH,i) = hwav(k)*wavfac
            valobs(IPNT_WAVET,i) = twav(k)
            if (.not. flowWithoutWaves) then
               valobs(IPNT_WAVED,i) = modulo(270d0 - phiwav(k), 360d0)  ! Direction from
               valobs(IPNT_WAVEL,i) = rlabda(k)
               valobs(IPNT_WAVEU,i) = uorb(k)
            endif
         endif

         if (jahistaucurrent>0) then
            valobs(IPNT_TAUX,i) = workx(k)
            valobs(IPNT_TAUY,i) = worky(k)
         endif

         if (stm_included .and. jased>0) then
            do j = IVAL_SBCX1, IVAL_SBCXN
               ii = j-IVAL_SBCX1+1
               valobs(IPNT_SBCX1+ii-1,i)=sedtra%sbcx(k,ii)
            enddo
            do j = IVAL_SBCY1, IVAL_SBCYN
               ii = j-IVAL_SBCY1+1
               valobs(IPNT_SBCY1+ii-1,i)=sedtra%sbcy(k,ii)
            enddo
            do j = IVAL_SSCX1, IVAL_SSCXN
               ii = j-IVAL_SSCX1+1
               valobs(IPNT_SSCX1+ii-1,i)=sedtra%sscx(k,ii)
            enddo
            do j = IVAL_SSCY1, IVAL_SSCYN
               ii = j-IVAL_SSCY1+1
               valobs(IPNT_SSCY1+ii-1,i)=sedtra%sscy(k,ii)
            enddo
            if (jawave>0 .and. .not. flowWithoutWaves) then
               do j = IVAL_SBWX1, IVAL_SBWXN
                  ii = j-IVAL_SBWX1+1
                  valobs(IPNT_SBWX1+ii-1,i)=sedtra%sbwx(k,ii)
               enddo
               do j = IVAL_SBWY1, IVAL_SBWYN
                  ii = j-IVAL_SBWY1+1
                  valobs(IPNT_SBWY1+ii-1,i)=sedtra%sbwy(k,ii)
               enddo
               do j = IVAL_SSWX1, IVAL_SSWXN
                  ii = j-IVAL_SSWX1+1
                  valobs(IPNT_SSWX1+ii-1,i)=sedtra%sswx(k,ii)
               enddo
               do j = IVAL_SSWY1, IVAL_SSWYN
                  ii = j-IVAL_SSWY1+1
                  valobs(IPNT_SSWY1+ii-1,i)=sedtra%sswy(k,ii)
               enddo
            endif
            !
            valobs(IPNT_TAUB,i) = sedtra%taub(k)  ! contains tausmax or Soulsby-Clarke shear stresses
            ! bed composition
            if (stmpar%morlyr%settings%iunderlyr==1) then
               do j = IVAL_BODSED1, IVAL_BODSEDN
                  ii = j-IVAL_BODSED1+1
                  valobs(IPNT_BODSED1+ii-1,i) = stmpar%morlyr%state%bodsed(ii,k)
               enddo
               valobs(IPNT_DPSED,i) = stmpar%morlyr%state%dpsed(k)
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
                     valobs(IPNT_MSED1+(ii-1)*nlyrs+klay-1,i) = stmpar%morlyr%state%msed(ii, klay, k)
                  end do
                  !
                  do j=IVAL_LYRFRAC1,IVAL_LYRFRACN
                     ii = j-IVAL_LYRFRAC1+1
                     valobs(IPNT_LYRFRAC1+(ii-1)*nlyrs+klay-1,i) = frac(ii,klay)
                  end do
                  !
                  valobs(IPNT_POROS+klay-1,i) = poros(klay)
                  valobs(IPNT_THLYR+klay-1,i) = stmpar%morlyr%state%thlyr(klay, k)
               end do
            endif
            !
            do j = IVAL_FRAC1, IVAL_FRACN
               ii = j-IVAL_FRAC1+1
               valobs(IPNT_FRAC1+ii-1,i) = sedtra%frac(k,ii)
            enddo
            valobs(IPNT_MUDFRAC,i)  = sedtra%mudfrac(k)
            valobs(IPNT_SANDFRAC,i) = sedtra%sandfrac(k)
            !
            if (stmpar%morpar%flufflyr%iflufflyr>0 .and. stmpar%lsedsus>0) then
               do j = IVAL_MFLUFF1, IVAL_MFLUFFN
                  ii = j-IVAL_MFLUFF1+1
                  valobs(IPNT_MFLUFF1+ii-1,i) = stmpar%morpar%flufflyr%mfluff(ii,k)
               enddo
            endif
            !
            do j = IVAL_FIXFAC1, IVAL_FIXFACN
               ii = j-IVAL_FIXFAC1+1
               valobs(IPNT_FIXFAC1+ii-1,i)=sedtra%fixfac(k,ii)
            enddo
            !
            do j = IVAL_HIDEXP1, IVAL_HIDEXPN
               ii = j-IVAL_HIDEXP1+1
               valobs(IPNT_HIDEXP1+ii-1,i)=sedtra%hidexp(k,ii)
            enddo
            !
            if (stmpar%lsedsus>0) then
               do j = IVAL_SOUR1, IVAL_SOURN
                  ii = j-IVAL_SOUR1+1
                  valobs(IPNT_SOUR1+ii-1,i)=sedtra%sourse(k,ii)
               enddo
               do j = IVAL_SINK1, IVAL_SINKN
                  ii = j-IVAL_SINK1+1
                  valobs(IPNT_SINK1+ii-1,i)=sedtra%sinkse(k,ii)
               enddo
            endif
         endif
         !
         if ( IVAL_WQB1.gt.0 ) then
            do j=IVAL_WQB1,IVAL_WQBN
               ii = j-IVAL_WQB1+1
               valobs(IPNT_WQB1+ii-1,i) = wqbot(ii,kb)
            end do
         end if

         if (kmx>0) then
            valobs(IPNT_UCXQ,i) = ucx(k)
            valobs(IPNT_UCYQ,i) = ucy(k)
         endif

         do kk=kb,kt
            klay = kk-kb+nlayb

            if (kmx > 0) then
               valobs(IPNT_ZCS+klay-1,i) = 0.5d0*( zws(kk)+zws(kk-1) )
            endif

            if (jahisvelocity>0 .or. jahisvelvec>0) then
               valobs(IPNT_UCX+klay-1,i) = ueux(kk)
               valobs(IPNT_UCY+klay-1,i) = ueuy(kk)
            endif

            if (jawave > 0 .and. .not. flowWithoutWaves) then
               if (hs(k)>epshu) then
                  if (kmx==0) then
                     kk_const = 1
                  else
                     kk_const = klay
                  endif
                  valobs(IPNT_UCXST+klay-1,i) = wa(1,kk_const)
                  valobs(IPNT_UCYST+klay-1,i) = wa(2,kk_const)
               endif
            endif

            if ( kmx>0 ) then
               valobs(IPNT_UCZ+klay-1,i)  = ucz(kk)
            end if
            if ( jasal.gt.0 ) then
               valobs(IPNT_SA1+klay-1,i)  = constituents(isalt, kk)
            end if
            if ( jatem.gt.0 ) then
               valobs(IPNT_TEM1+klay-1,i) = constituents(itemp, kk)
            end if
            if ((jasal > 0 .or. jatem > 0 .or. jased > 0) .and. jahisrho > 0) then
               valobs(IPNT_RHOP+klay-1,i) = setrhofixedp(kk, 0d0)
               if ( density_is_pressure_dependent() ) then
                  valobs(IPNT_RHO+klay-1,i) = rho(kk)
               endif
            end if
            if (jahisvelocity > 0) then
               valobs(IPNT_UMAG+klay-1,i) = ucmag(kk)
            end if
            valobs(IPNT_QMAG+klay-1,i) = 0.5d0*(squ(kk)+sqi(kk))
            
            if (kmx==0) then
               kmx_const = 1     ! to make numbering below work
            end if

            if ( IVAL_TRA1.gt.0 ) then
               do j=IVAL_TRA1,IVAL_TRAN
                  ii = j-IVAL_TRA1+1
                  valobs(IPNT_TRA1+(ii-1)*kmx_const+klay-1,i) = constituents(ITRA1+ii-1, kk)
               end do
            end if

            if ( IVAL_HWQ1.gt.0 ) then
               do j=IVAL_HWQ1,IVAL_HWQN
                  ii = j-IVAL_HWQ1+1
                  valobs(IPNT_HWQ1+(ii-1)*kmx_const+klay-1,i) = waqoutputs(ii,kk-kbx+1)
               end do
            end if

            if ( IVAL_WQB3D1.gt.0 ) then
               do j=IVAL_WQB3D1,IVAL_WQB3DN
                  ii = j-IVAL_WQB3D1+1
                  valobs(IPNT_WQB3D1+(ii-1)*kmx_const+klay-1,i) = wqbot(ii,kk)
               end do
            end if

            if ( IVAL_SF1.gt.0 ) then
               do j=IVAL_SF1,IVAL_SFN
                  ii = j-IVAL_SF1+1
                  valobs(IPNT_SF1+(ii-1)*kmx_const+klay-1,i) = constituents(ISED1+ii-1, kk)
               end do
            end if

            if (kmx==0 .and. IVAL_WS1 .gt. 0) then
               do j=IVAL_WS1,IVAL_WSN
                  ii = j-IVAL_WS1+1
                  valobs(IPNT_WS1+(ii-1)*kmx_const+klay-1,i) = mtd%ws(kk, ii)   ! 1:lsedsus
               end do
            end if

            if ( jased.gt.0 .and. .not. stm_included) then
               valobs(IPNT_SED+klay-1,i) = sed(1, kk)
            end if
            valobs(IPNT_CMX,i) = max( valobs(IPNT_UCX,i), sqrt( ucx(kk)**2 + ucy(kk)**2 )  )
         end do
         valobs(IPNT_SMX,i) = max( smxobs(i), s1(k) )

         if ( kmx.gt.0 ) then
            call getkbotktop(k, kb, kt)
            call getlayerindices(k, nlayb, nrlay)
            do kk = kb-1, kt
               klay = kk - kb + nlayb + 1
               valobs(IPNT_ZWS+klay-1,i) = zws(kk)
               if ((jasal > 0 .or. jatem > 0 .or. jased > 0) .and. jahisrho > 0) then
                  if (zws(kt) - zws(kb-1) > epshu .and. kk > kb-1 .and. kk < kt ) then
                     if ( density_is_pressure_dependent() ) then
                        prsappr = ag*rhomean*( zws(kt) - zws(kk) )  
                        drhodz  = ( setrhofixedp(kk+1,prsappr) - setrhofixedp(kk,prsappr) ) / max(0.5d0*(zws(kk+1) - zws(kk-1)),epshs)
                     else 
                        drhodz  = ( rho(kk+1) - rho(kk)                                   ) / max(0.5d0*(zws(kk+1) - zws(kk-1)),epshs) 
                     endif
                     rhomea  = 0.5d0*( rho(kk+1) + rho(kk) )
                     valobs(IPNT_BRUV+klay-1,i) = -ag*drhodz/rhomea
                  endif
               end if
            enddo

            call getlink1(k,LL)
            call getLbotLtop(LL,Lb,Lt)
            call getlayerindicesLmax(LL, nlaybL, nrlayLx)
            do L = Lb-1, Lt
               klay = L-Lb+nlaybL+1
               if (layertype == 2) then
                  valobs(IPNT_ZWU+klay-1,i) = min(bob(1,LL),bob(2,LL)) + hu(L)
               else
                  valobs(IPNT_ZWU+klay-1,i) = min(bob(1,LL),bob(2,LL)) + hu(L)
               end if

               if ( IVAL_WS1.gt.0 ) then
                  do j=IVAL_WS1,IVAL_WSN
                     ii = j-IVAL_WS1+1
                     valobs(IPNT_WS1+(ii-1)*(kmx+1)+klay-1,i) =mtd%ws(kb+klay-2, ii)
                  end do
               end if

               if ( IVAL_SEDDIF1.gt.0 ) then
                  do j=IVAL_SEDDIF1,IVAL_SEDDIFN
                     ii = j-IVAL_SEDDIF1+1
                     valobs(IPNT_SEDDIF1+(ii-1)*(kmx+1)+klay-1,i) =mtd%seddif(ii, kb+klay-2)
                  end do
               end if
            enddo
            if ( iturbulencemodel.ge.2 ) then
               valobs(IPNT_VICWW:IPNT_VICWW+kmx,i) = 0d0
            endif
            if ( iturbulencemodel.ge.3 ) then
               valobs(IPNT_TKIN:IPNT_TKIN+kmx,i) = 0d0
               valobs(IPNT_TEPS:IPNT_TEPS+kmx,i) = 0d0
            endif
            if (idensform > 0 .and. jaRichardsononoutput > 0) then
               valobs(IPNT_RICH:IPNT_RICH+kmx,i) = 0d0
            endif
            do LL = 1,nd(k)%lnx
               LLa = iabs(nd(k)%ln(LL))
               call getLbotLtop(LLa,Lb,Lt)
               k1 = ln0(1,LLa) ; k2 = ln0(2,LLa)
               k3 = 1 ; if( nd(k)%ln(LL) > 0 ) k3 = 2
               do L = Lb-1,Lt
                  klay = L-Lb+2
                  if ( iturbulencemodel.ge.2 ) then
                     valobs(IPNT_VICWW + klay-1,i) = valobs(IPNT_VICWW + klay-1,i) + vicwwu(L) * wcL(k3,LLa)
                  end if
                  if ( iturbulencemodel.ge.3 ) then
                     valobs(IPNT_TKIN  + klay-1,i) = valobs(IPNT_TKIN  + klay-1,i) + turkin1(L) * wcL(k3,LLa)
                     valobs(IPNT_TEPS  + klay-1,i) = valobs(IPNT_TEPS  + klay-1,i) + tureps1(L) * wcL(k3,LLa)
                  endif
                  if (idensform > 0 .and. jaRichardsononoutput > 0) then
                     valobs(IPNT_RICH + klay-1,i) = valobs(IPNT_RICH + klay-1,i) + rich(L) * wcL(k3,LLa)
                  endif
               enddo
            enddo

            if (iturbulencemodel.ge.2) then
               call reorder_valobs_array(kmx+1,valobs(IPNT_VICWW:IPNT_VICWW+kmx,i), kb, kt, nlayb, dmiss)
            endif
            if (iturbulencemodel.ge.3) then
               call reorder_valobs_array(kmx+1,valobs(IPNT_TKIN:IPNT_TKIN+kmx,i), kb, kt, nlayb, dmiss)
               call reorder_valobs_array(kmx+1,valobs(IPNT_TEPS:IPNT_TEPS+kmx,i), kb, kt, nlayb, dmiss)
            endif
            if (idensform > 0 .and. jaRichardsononoutput > 0) then
               call reorder_valobs_array(kmx+1,valobs(IPNT_RICH:IPNT_RICH+kmx,i), kb, kt, nlayb, dmiss)
            endif
         endif

!        Rainfall
         if (jarain > 0 .and. jahisrain > 0) then
            valobs(IPNT_RAIN,i) = rain(k)
         end if
         
         if (allocated(airdensity) .and. jahis_airdensity > 0) then
            valobs(IPNT_AIRDENSITY,i) = airdensity(k)
         end if

!        Infiltration
         if ((infiltrationmodel == DFM_HYD_INFILT_CONST .or. infiltrationmodel == DFM_HYD_INFILT_HORTON) .and. jahisinfilt > 0) then
            valobs(IPNT_INFILTCAP,i) = infiltcap(k)*1d3*3600d0 ! m/s -> mm/hr
            if (ba(k) > 0d0) then
               valobs(IPNT_INFILTACT,i) = infilt(k)/ba(k)*1d3*3600d0 ! m/s -> mm/hr
            else
               valobs(IPNT_INFILTACT,i) = 0d0
            end if
         end if

!        Heatflux
         if (jatem > 0 .and. jahisheatflux > 0) then
            call getlink1(k,LL)
            if ( jawind.gt.0 ) then
               valobs(IPNT_WIND,i) = sqrt(wx(LL)*wx(LL) + wy(LL)*wy(LL))
            end if

            if ( jatem.gt.1 ) then   ! also heat modelling involved
               valobs(IPNT_TAIR,i) = Tair(k)
            end if

            if (jatem == 5 .and. allocated(Rhum) .and. allocated(Clou) ) then
               valobs(IPNT_RHUM,i) = Rhum(k)
               valobs(IPNT_CLOU,i) = Clou(k)
            endif

            if (jatem == 5) then
               valobs(IPNT_QSUN,i) = Qsunmap(k)
               valobs(IPNT_QEVA,i) = Qevamap(k)
               valobs(IPNT_QCON,i) = Qconmap(k)
               valobs(IPNT_QLON,i) = Qlongmap(k)
               valobs(IPNT_QFRE,i) = Qfrevamap(k)
               valobs(IPNT_QFRC,i) = Qfrconmap(k)
            endif

            if (jatem > 1) then
               valobs(IPNT_QTOT,i) = Qtotmap(k)
            end if
         end if
      else
         valobs(:,i) = DMISS
      end if
   end do

!  No need to copy empty layers from top anymore, they have been filled with dmiss

   if (allocated(wa)) deallocate(wa)

   if (timon) call timstop (handle_extra(55))
   return
   end subroutine fill_valobs
