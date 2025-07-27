! calculate wave-induced mixing coefficient
subroutine get_mixing_nguyen(LL, kmxx, ustbcw, deltau, Kbr, Kfr)     
   
   ! calculate Kbr, Kfr in LINK positions
   use m_waves
   use m_flow, only: hu
   use m_flowgeom
   use m_flowparameters, only: alfa_uchi_mix, awmx
   use m_physcoef

   implicit none
   
   integer, intent(in)              :: LL
   integer, intent(in)              :: kmxx
   double precision, intent(in)     :: deltau
   double precision, intent(in)     :: ustbcw
   double precision, dimension(kmxx), intent(out)    :: Kbr
   double precision, dimension(kmxx), intent(out)    :: Kfr
   ! Local vars
   integer                          :: k1, k2
   integer                          :: Lb, Lt, L
   double precision                 :: ac1, ac2
   double precision                 :: dis1, dis2, surdisLL
   double precision, dimension(kmxx) :: fb
   double precision                 :: huLL, huL, Hrms, wl, z, zz, cb

   fb  = 0d0       ! safety
   Kbr = 0d0
   Kfr = 0d0
   cb = 1./14.0    ! breaking-induced mixing coefficient (Apotsos et al (2007))
   !
   call getLbotLtop(LL,Lb,Lt)
   if (Lt<Lb) return     
   !
   huLL = hu(LL)        
   k1 = ln(1,LL); k2=ln(2,LL)
   ac1 = acl(LL); ac2 = 1d0-ac1
   hrms = ac1*hwav(k1)+ac2*hwav(k2)
   if (hrms<1d-2) return
   hrms = max(hrms,gammax*huLL)
   hrms = awmx*hrms
   wl = ac1*rlabda(k1)+ac2*rlabda(k2)
   !
   call uchiyama_u(LL, hrms, wl, alfa_uchi_mix, fb) 
   !
   ! Get surface dissipation
   call wave_fillsurdis(k1,dis1)
   call wave_fillsurdis(k2,dis2)
   surdisLL = ac1*dis1 + ac2*dis2
   !
   ! Mixing distribution from breaking
   !visc_br(i,k) = cb*((((1-alphar)*Dw(i)+Dr(i))/rho)**(1./3)) &
   !         *Hrms(i)*d(i)*fwb(i,k)
   do L = Lb-1, Lt
      Kbr(L) = cb*hrms*huLL*fb(L)*(surdisLL/rhomean)**(1d0/3d0)
   enddo
   !
   ! Mixing distribution from bottom dissipation
   do L  = Lb-1, Lt
      huL = hu(L)
      z = max(huL,deltau)
      zz = z*( 1d0 - z/ huLL )  
      Kfr(L) = vonkar*zz*ustbcw
   enddo

1234 continue
     return
   
end subroutine get_mixing_nguyen 