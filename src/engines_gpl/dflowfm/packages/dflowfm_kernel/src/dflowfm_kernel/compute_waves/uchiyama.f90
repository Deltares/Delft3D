subroutine uchiyama_u(L, hrms, rlabda, alfa, fb)
   ! Determines uchiyama profile per vertical profile in link positions
   use m_flow, only: kmx, hu, hs      

   implicit none
   
   integer,                            intent(in)  :: L        ! link number 
   double precision,                   intent(in)  :: hrms     ! wave height
   double precision,                   intent(in)  :: rlabda   ! wave length
   double precision,                   intent(in)  :: alfa     ! alfa_uchi for breaking or mixing
   double precision, dimension(kmx+1), intent(out) :: fb       ! vertical distribution in link positions   
   ! Local variables
   integer          :: LL, Lb, Lt
   double precision :: hoverL, wps, fbsum
   
   ! Get vertical administration (works from bottom to top!)
   call getLbotLtop(L,Lb,Lt)
   if (Lt<Lb) then
      fb = 0d0
      return
   endif      
   ! wave penetration length
   wps = 1d0/hrms   
   ! determine regime
   hoverL = hs(L)/max(rlabda,0.1d0)
   fbsum = 0d0   
   if (hoverL<1d0/20d0) then    ! shallow, type I
      do LL = Lb+1, Lt          
         fb(LL) = 1d0-tanh((0.5d0*wps*(hu(LL)+hu(LL-1)))**4)
         fbsum  = fbsum + fb(LL)*(hu(LL)-hu(LL-1))
      enddo     
   else if (hoverL>0.5d0) then  ! deep, type III
      do LL = Lb+1, Lt
         fb(LL) = cosh(0.5d0*wps*(hu(LL)+hu(LL-1)))
         fbsum  = fbsum + fb(LL)*(hu(LL)-hu(LL-1))
      enddo         
   else                         ! intermediate, type II
      do LL = Lb+1, Lt
         fb(LL) = 1d0-tanh((0.5d0*wps*(hu(LL)+hu(LL-1)))**2)
         fbsum  = fbsum + fb(LL)*(hu(LL)-hu(LL-1))
      enddo       
   endif   
   fb = fb/fbsum   

   return   
   end subroutine uchiyama_u
!   
  subroutine uchiyama_s(k, hrms, rlabda, alfa, fb)
   ! Determines uchiyama profile per vertical profile in cc positions
   use m_flow       , only: kmx, hs, zws, s1
   
   implicit none
   
   integer,                            intent(in)  :: k        ! node number 
   double precision,                   intent(in)  :: hrms     ! wave height
   double precision,                   intent(in)  :: rlabda   ! wave length
   double precision,                   intent(in)  :: alfa     ! alfa_uchi for breaking or mixing
   double precision, dimension(kmx),   intent(out) :: fb       ! vertical distribution in cc positions
   
   ! Local variables
   integer          :: kk, kb, kt
   double precision :: hoverL, wps, fbsum
   
   ! Get vertical administration (works from bottom to top!)
   call getkbotktop(k,kb,kt)
   if (kt<kb) then
      fb = 0d0
      return
   endif   
   ! wave penetration length
   wps = 1d0/(alfa*hrms)
   ! determine regime
   hoverL = hs(k)/max(rlabda,0.1d0)
   fbsum = 0d0
 
   if (hoverL<1d0/20d0) then    ! shallow, type I
      do kk = kb+1, kt
         fb(kk) = 1d0-tanh(wps*(s1(k)+0.5d0*(zws(kk)+zws(kk-1))))**4
         fbsum  = fbsum + fb(kk)*(zws(kk)-zws(kk-1))
      enddo
   else if (hoverL>0.5d0) then  ! deep, type III
      do kk = kb+1, kt
         fb(kk) = cosh(wps*(s1(k)+0.5d0*(zws(kk)+zws(kk-1))))
         fbsum  = fbsum + fb(kk)*(zws(kk)-zws(kk-1))
      enddo   
   else                         ! intermediate, type II
      do kk = kb+1, kt
         fb(kk) = 1d0-tanh(wps*(s1(k)+0.5d0*(zws(kk)+zws(kk-1))))**2
         fbsum  = fbsum + fb(kk)*(zws(kk)-zws(kk-1))
      enddo       
   endif   
   fb = fb/fbsum   

   return   
end subroutine uchiyama_s