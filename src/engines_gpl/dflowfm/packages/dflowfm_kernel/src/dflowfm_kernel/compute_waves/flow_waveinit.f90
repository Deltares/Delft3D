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

   subroutine flow_waveinit
   use m_flow
   use m_flowgeom
   use m_waves
   use m_xbeach_data
   use m_xbeach_avgoutput
   use m_xbeach_readkey
   use m_xbeach_filefunctions
   use m_xbeach_errorhandling
   use m_xbeach_paramsconst
   use m_xbeach_netcdf
   use M_SAMPLES
   use m_missing
   use m_alloc
   use m_sferic, only: jsferic, jasfer3D
   use m_polygon, only: NPL, xpl, ypl, zpl
   use m_ec_basic_interpolation, only: triinterp2
   use m_flowexternalforcings, only: transformcoef
   use dfm_error

   implicit none

   integer      :: ierr
   integer      :: minp0, jdla, nm, ibnd, kb, ki

   ierr = DFM_NOERR

   call realloc(uin, nbndu, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('uin  (nbndu)', ierr, nbndu)
   call realloc(vin, nbndu, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('vin  (nbndu)', ierr, nbndu)
   call realloc(u1rm, nbndu, stat=ierr, keepExisting=.false., fill=0d0)   ! remember u1 state
   call aerr('u1rm  (nbndu)', ierr, nbndu)

   call realloc(ktb, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('ktb  (ndx)', ierr, ndx)
   call realloc(ust_mag, ndkx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('ust_mag  (ndkx)', ierr, ndkx)
   call realloc(fwav_mag, ndkx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('fwav_mag  (ndkx)', ierr, ndkx)
   call realloc(ustx_cc, ndkx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('ustx_cc  (ndkx)', ierr, ndkx)
   call realloc(usty_cc, ndkx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('usty_cc  (ndkx)', ierr, ndkx)

   if (jawave == 3 .or. jawave == 4 .or. jawave == 6 .or. jawave == 7) then
      call realloc(wavfu, lnkx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('wavfu  (lnkx)', ierr, lnkx)
      call realloc(wavfv, lnkx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('wavfv  (lnkx)', ierr, lnkx)
      call realloc(sxwav, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('sxwav  (ndx)', ierr, ndx)
      call realloc(sywav, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('sywav  (ndx)', ierr, ndx)
      if(jawave /= 7 .or. waveforcing == 3) then
          call realloc(sbxwav, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
          call aerr('sbxwav  (ndx)', ierr, ndx)
          call realloc(sbywav, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
          call aerr('sbywav  (ndx)', ierr, ndx)
      endif
   endif

   if (jawave == 3 .or. jawave == 7) then
      call realloc(wavmubnd, lnkx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('wavmubnd  (lnkx)', ierr, lnkx)
      call realloc(uorbwav, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('uorbwav  (ndx)', ierr, ndx)
      call realloc(wlenwav, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('wlenwav  (ndx)', ierr, ndx)

      call realloc(mxwav, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('mxwav(ndx)', ierr, ndx)
      call realloc(mywav, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('mywav(ndx)', ierr, ndx)
      if(jawave /= 7 .or. waveforcing == 3) then
          call realloc(dsurf, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
          call aerr('dsurf(ndx)', ierr, ndx)
          call realloc(dwcap, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
          call aerr('dwcap(ndx)', ierr, ndx)
      endif
      if(jawave == 7 .and. waveforcing == 2) then
          call realloc(distot, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
          call aerr('distot(ndx)', ierr, ndx)
      endif
   endif
   !
   if  (jawave > 0) then
      call realloc( hwavcom,   ndx, stat=ierr, keepExisting = .false., fill = hwavuni)
      call aerr   ('hwavcom   (ndx)', ierr, ndx)
   endif
   !
   ! Ugly, fix with allocate9basic andsoon
   if  (jawave == 6) then
      call realloc( hwav,   ndx, stat=ierr, keepExisting = .false., fill = hwavuni)
      call aerr   ('hwav   (ndx)', ierr, ndx)
      call realloc( twav,   ndx, stat=ierr, keepExisting = .false., fill = twavuni)
      call aerr   ('twav   (ndx)', ierr, ndx)
      call realloc( rlabda,  ndx,  stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr   ('rlabda  (ndx)',     ierr, ndx)
      call realloc( uorb,    ndx,  stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr   ('uorb    (ndx)',     ierr, ndx)
   endif

   if (jawave .eq. 4) then
      if (trim(instat)=='stat' .or. &
          trim(instat)=='stat_table') then
         call allocstatsolverarrays(0,ierr)
      endif
      !
      if (single_dir>0) then
         call allocstatsolverarrays(1,ierr)
      endif
      !
      call realloc(ee0, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('ee0  (ntheta,ndx)', ierr, ntheta*ndx)
      call realloc(ee1, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('ee1  (ntheta,ndx)', ierr, ntheta*ndx)
      call realloc(cwav, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('cwav  (ndx)', ierr, ndx)
      call realloc(cgwav, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('cgwav  (ndx)', ierr, ndx)
      call realloc(kwav, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('kwav  (ndx)', ierr, ndx)
      call realloc(nwav, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('nwav  (ndx)', ierr, ndx)
      call realloc(ctheta, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('ctheta  (ntheta,ndx)', ierr, ntheta*ndx)
      call realloc(sigmwav, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('sigmwav  (ndx)', ierr, ndx)
      call realloc(sigt, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('sigt  (ntheta,ndx)', ierr, ntheta*ndx)
      call realloc(horadvec, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('horadvec  (ntheta,ndx)', ierr, ntheta*ndx)
      call realloc(thetaadvec, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('thetaadvec  (ntheta,ndx)', ierr, ntheta*ndx)
      call realloc(rhs, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('rhs  (ntheta,ndx)', ierr, ntheta*ndx)
      call realloc(rrhoradvec, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('rrhoradvec  (ntheta,ndx)', ierr, ntheta*ndx)
      call realloc(rrthetaadvec, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('rrthetaadvec  (ntheta,ndx)', ierr, ntheta*ndx)
      call realloc(rr, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('rr  (ntheta,ndx)', ierr, ntheta*ndx)
      call realloc(H, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('H  (ndx)', ierr, ndx)
      call realloc(fw, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('fw  (ndx)', ierr, ndx)
      call realloc(E, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('E  (ndx)', ierr, ndx)
      call realloc(DR, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('DR  (ndx)', ierr, ndx)
      call realloc(R, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('R  (ndx)', ierr, ndx)
      call realloc(rr, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('rr  (ntheta,ndx)', ierr, ntheta*ndx)
      call realloc(Sxx, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('Sxx  (ndx)', ierr, ndx)
      call realloc(Syy, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('Syy  (ndx)', ierr, ndx)
      call realloc(Sxy, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('Sxy  (ndx)', ierr, ndx)
      call realloc(Fx, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('Fx  (lnx)', ierr, lnx)
      call realloc(Fy, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('Fy  (lnx)', ierr, lnx)
      call realloc(Fx_cc, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('Fx_cc  (ndx)', ierr, ndx)
      call realloc(Fy_cc, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('Fy_cc  (ndx)', ierr, ndx)

      call realloc(dhsdx, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('dhsdx  (ndx)', ierr, ndx)
      call realloc(dhsdy, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('dhsdy  (ndx)', ierr, ndx)

      call realloc(thetamean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('thetamean  (ndx)', ierr, ndx)
      call realloc(Qb, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('Qb  (ndx)', ierr, ndx)
      call realloc(D, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('D  (ndx)', ierr, ndx)
      call realloc(Df, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('Df  (ndx)', ierr, ndx)
      call realloc(Dtot, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('Dtot  (ndx)', ierr, ndx)
      call realloc(BR, ndx, stat=ierr, keepExisting = .false., fill = beta)
      call aerr('BR  (ndx)', ierr, ndx)
      call realloc(bi, nbndw, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('bi  (nbndw)', ierr, nbndw)
      !
      call realloc(Tbore, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('Tbore  (ndx)', ierr, ndx)
      call realloc(hhw, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('hhw  (ndx)', ierr, ndx)
      call realloc(hdisp, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('hdisp  (ndx)', ierr, ndx)
      call realloc(hstokes, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('hstokes  (ndx)', ierr, ndx)
      !
      call realloc(xbducxdx, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('xbducxdx  (ndx)', ierr, ndx)
      call realloc(xbducydx, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('xbducydx  (ndx)', ierr, ndx)
      call realloc(xbducxdy, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('xbducxdy  (ndx)', ierr, ndx)
      call realloc(xbducydy, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('xbducydy  (ndx)', ierr, ndx)
      !
      if (trim(absgentype)=='abs_2d') then
         call realloc(dbetadx, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('dbetadx  (ndx)', ierr, ndx)
         call realloc(dbetady, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('dbetady  (ndx)', ierr, ndx)
      endif
      !
      if (single_dir>0) then
         call realloc(cgwav_s, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('cgwav_s  (ndx)', ierr, ndx)
         call realloc(cwav_s, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('cwav_s  (ndx)', ierr, ndx)
         call realloc(ctheta_s, (/ntheta_s, ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('ctheta_s  (ntheta_s,ndx)', ierr, ntheta_s*ndx)
         call realloc(ee_s, (/ntheta_s,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('ee_s  (ntheta_s,ndx)', ierr, ntheta_s*ndx)
         call realloc(hhws, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('hhws  (ndx)', ierr, ndx)
         call realloc(ucxws, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('ucxws  (ndx)', ierr, ndx)
         call realloc(ucyws, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('ucyws  (ndx)', ierr, ndx)
      endif
      !
      if (wci>0) then
         call realloc(km, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('km  (ndx)', ierr, ndx)
         call realloc(umwci, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('umwci  (ndx)', ierr, ndx)
         call realloc(vmwci, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('vmwci  (ndx)', ierr, ndx)
         call realloc(hhwwci, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('hhwwci  (ndx)', ierr, ndx)
      endif
      !
      !if (windmodel .eq. 0) then
         call realloc(L1, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('L1  (ndx)', ierr, ndx)
         call realloc(Ltemp, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('Ltemp  (ndx)', ierr, ndx)
         call realloc(L0, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('L0  (ndx)', ierr, ndx)
      !endif

      !if (windmodel .eq. 1) then
      !   call realloc(tt1, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      !   call aerr('tt1  (ntheta,ndx)', ierr, ntheta*ndx)
      !   call realloc(cwavt, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      !   call aerr('cwavt  (ntheta,ndx)', ierr, ntheta*ndx)
      !   call realloc(cgwavt, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      !   call aerr('cgwavt  (ntheta,ndx)', ierr, ntheta*ndx)
      !   call realloc(kwavt, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      !   call aerr('kwavt  (ntheta,ndx)', ierr, ntheta*ndx)
      !   call realloc(nwavt, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      !   call aerr('nwavt  (ntheta,ndx)', ierr, ntheta*ndx)
      !   call realloc(horadvec2, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      !   call aerr('horadvec2  (ntheta,ndx)', ierr, ntheta*ndx)
      !   call realloc(thetaadvec2, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      !   call aerr('thetaadvec2  (ntheta,ndx)', ierr, ntheta*ndx)
      !   call realloc(L0t, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      !   call aerr('L0t  (ntheta,ndx)', ierr, ntheta*ndx)
      !   call realloc(L1t, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      !   call aerr('L1t  (ntheta,ndx)', ierr, ntheta*ndx)
      !   call realloc(Ltempt, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      !   call aerr('Ltempt  (ntheta,ndx)', ierr, ntheta*ndx)
      !   call realloc(ma, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      !   call aerr('ma  (ntheta,ndx)', ierr, ntheta*ndx)
      !   call realloc(mb, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      !   call aerr('mb  (ntheta,ndx)', ierr, ntheta*ndx)
      !   call realloc(wmagcc, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      !   call aerr('wmagcc  (ndx)', ierr, ntheta*ndx)
      !   call realloc(windspreadfac, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      !   call aerr('windspreadfac (ntheta,ndx)', ierr, ntheta*ndx)
      !   call realloc(wsorE, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      !   call aerr('wsorE  (ntheta,ndx)', ierr, ntheta*ndx)
      !   call realloc(wsorT, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      !   call aerr('wsorT  (ntheta,ndx)', ierr, ntheta*ndx)
      !   call realloc(egradcg, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      !   call aerr('egradcg  (ntheta,ndx)', ierr, ntheta*ndx)
      !   call realloc(ddT, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      !   call aerr('ddT  (ndx)', ierr, ntheta*ndx)
      !   call realloc(SwE, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      !   call aerr('SwE  (ndx)', ierr, ntheta*ndx)
      !   call realloc(SwT, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      !   call aerr('SwT  (ndx)', ierr, ntheta*ndx)
      !endif

      ! handle wave friction, has to be post-poned until here because of unavailability of ndx
      if (wavefricfile .ne. ' ') then
         call check_file_exist(wavefricfile)   ! if not, program will exit here
         call writelog('lws','(a,a,a)','Warning: wave friction coefficient values from file ''',&
            trim(wavefricfile), &
            ''' will be used in computation')
         call oldfil(minp0, wavefricfile)
         call reasam(minp0, 0)
         !
         jdla = 1
         fw  = dmiss
         CALL triinterp2(xz, yz, fw, ndx,JDLA, &
            XS, YS, ZS, NS, dmiss, jsferic, jins, jasfer3D, NPL, MXSAM, MYSAM, XPL, YPL, ZPL, transformcoef)
         !
         ! mirror boundary cells if undefined if equal to dmiss
         do ibnd = lnxi+1, lnx            ! loop over boundary flow links
            kb = ln(1,ibnd)      ! point outside net
            ki = ln(2,ibnd)      ! point inside net
            if (fw(kb) == dmiss) then
               fw(kb) = fw(ki)
            endif
         enddo
         ! if node value still equal to dmiss (values are not defined on flow nodes) - throw error
         do nm = 1, ndx  ! loop over flow nodes
            if (fw(nm) == dmiss) then
               call xbeach_errorhandler()
            endif
         enddo
         call delsam(-1)
         call doclose(minp0)

      else
         fw = wavefricval
      endif

      if (jaavgwavquant .eq. 1) then            ! arrays for statistical output wave quantities
         call xbeach_allocateaverages()
      end if
   end if
   end subroutine flow_waveinit
