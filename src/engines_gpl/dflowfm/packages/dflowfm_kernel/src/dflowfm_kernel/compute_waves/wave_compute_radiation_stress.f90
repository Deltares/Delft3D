subroutine wave_compute_radiation_stress()   
   use m_waves
   use m_flow, only: hs, zws, ucx, ucy, ustb, ustbcw
   use m_flowgeom, only: ndx, bl, nd, ln, wcl
   use m_flowparameters, only: epshu
   use m_physcoef
   
   implicit none  
 
   logical                                     :: udominant
   integer                                     :: k, kk, kb, kt, k1
   integer                                     :: L, LL
   double precision                            :: hsk, zph, hrms
   double precision                            :: cwru, cwrv, sigma
   double precision                            :: diru, ustar, rs
   double precision, external                  :: sinhsafei
   double precision, dimension(:), allocatable :: Sj
   !
   if (allocated(Sj)) then
      deallocate(Sj)
      allocate(Sj(1:ndx))
      Sj=0d0
   endif   

   do k = 1,ndx
      hsk = hs(k)          
      if (hsk.gt.epshu) then
         hrms = max(hwav(k), gammax*hsk)
         ee = 1d0/8d0*rhomean*ag*hrms**2
         Sj(k) = kw(k)*ee*sinhsafei(2.*kw(k)*hsk)/rhomean     ! wave induced kinematic pressure
      endif
   enddo
   !
   ! Shear components
   Sjxx = 0
   Sjyy = 0
   Sjxy = 0
   do k=1,ndx
      if (hwav(k)<1d-3) continue
      call getkbotktop(k, kb, kt)
      do kk = kb,kt
         zph = 0.5*(zws(kk)-zws(kk-1))-bl(k)
         Sjxx(kk)  = Sj(k)*((kwx(k)*cosh(kw(k)*(zph))/kw(k))**2 - (sinh(kw(k)*(zph)))**2)  ! (u2-w2)
         Sjxy(kk)  = Sj(k)*(kwx(k)*kwy(k)*(cosh(kw(k)*(zph))/kw(k))**2)                    ! (uv, vu) This is to check
         Sjyy(kk)  = Sj(k)*((kwy(k)*cosh(kw(k)*(zph))/kw(k))**2 - (sinh(kw(k)*(zph)))**2)  ! (v2-w2)
      enddo
   enddo
   !
   ! Normal components
   call get_wave_amplitude_gradient()
   do k = 1, ndx
      if (hwav(k)<1d-3) continue
      if (hs(k)>epshu) then
         !
         sigma = sqrt(ag*kw(k)*tanh(kw(k)*hsk))    ! or 2*pi/twav
         !
         ! get cell centre ustar
         udominant = sqrt(ucx(kb)**2+ucy(kb)**2)>uorb(k)
         if (udominant) then
            diru = atan2(ucy(kb),ucx(kb))
            ustar = 0d0
            do L = 1, nd(k)%lnx
               LL = iabs(nd(k)%ln(L))
               k1 = ln(1,LL)
               if (k1==k) then
                  ustar = ustar + wcl(1,LL)*(ustbcw(LL) + ustb(LL))
               else
                  ustar = ustar + wcl(2,LL)*(ustbcw(LL) + ustb(LL))
               endif
            enddo
         endif
         !
         call getkbotktop(k, kb, kt)
         do kk = kb, kt
            zph = 0.5*(zws(kk)-zws(kk-1))-bl(k)
            !
            !     For random wave in Klopman (1994)
            rs       =  -0.25d0/kw(k)/kw(k)*ag*hwav(k)*sinh(2d0*kw(k)*zph)*sinhsafei(kw(k)*hsk)*(kwx(k)*dasgdx(k) + kwy(k)*dasgdy(k)) 
            ! 
            !     Empirical formula for vertical radiation stress (Nielsen, 1997)
            if (udominant) then
               cwru = 1d0+144d0*ustar*sqrt(abs(cos(diru)))*zph/(0.5d0*hwav(k)*sigma*hsk) 
               cwrv = 1d0+144d0*ustar*sqrt(abs(sin(diru)))*zph/(0.5d0*hwav(k)*sigma*hsk)
            else
               cwru = 1d0
               cwrv = 1d0
            endif
            !
            !  Vertical radiation stress (uw) according to Groeneweg (1999)
            Sjxw(kk) =  cwru*kwx(k)*rs   
            Sjyw(kk) =  cwrv*kwy(k)*rs 
         enddo
      endif
   enddo

end subroutine wave_compute_radiation_stress