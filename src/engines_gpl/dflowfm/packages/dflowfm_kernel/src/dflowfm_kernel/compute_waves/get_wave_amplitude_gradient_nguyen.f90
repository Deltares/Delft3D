subroutine get_wave_amplitude_gradient_nguyen()
   use m_waves
   use m_flowgeom
   use m_alloc
   
   implicit none
   
   integer :: L,LL,Lb,Lt,k1,k2,dasgdn
   double precision, allocatable, dimension(:) :: asg
   
   call realloc(asg,ndx,keepexisting=.true.,fill=0d0)
   
   asg = 0.5d0*hwav
   
    dasgdx = 0d0; dasgdy = 0d0
    do LL = 1,lnx
       call getLbotLtop(LL,Lb,Lt)
       do L  = Lb, Lt
          k1 = ln(1,L)
          k2 = ln(2,L)
          dasgdn = dxi(LL)*( asg(k2) - asg(k1) )
          dasgdx(k1) = dasgdx(k1) + wcx1(LL)*dasgdn
          dasgdy(k1) = dasgdy(k1) + wcy1(LL)*dasgdn
          dasgdx(k2) = dasgdx(k2) + wcx2(LL)*dasgdn
          dasgdy(k2) = dasgdy(k2) + wcy2(LL)*dasgdn
       enddo
    enddo
   
end subroutine get_wave_amplitude_gradient_nguyen