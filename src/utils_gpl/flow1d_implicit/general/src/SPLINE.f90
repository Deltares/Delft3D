subroutine spline(x ,y ,n ,hx ,hy ,hn ,y2 ,u )
!
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Getij Analyse Module
!
! Programmer:         J.Kuipers
!
! Module:             SPLINE (SPLINE functie)
!
! Module description: A new series will be made using cubic
!                     spline interpolation
!                     [Numerical Recepies par 3.3]
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 1  x                 I  arguments of given series
! 2  y                 I  functie values of given series
! 3  n                 I  number of given points
! 4  hx                I  arguments of resulting series
! 5  hy                O  functie values of resulting series
! 6  hn                O  number of resulting points
! 7  y2                   scratch array
! 8  u                    scratch array
!=======================================================================
!

   integer          n       ,hn
   real             x(0:n-1)  ,y(0:n-1) ,hx(0:hn-1) ,hy(0:hn-1)
   double precision y2(0:n-1) ,u(0:n-1)
!
!     Declaration of local variables:
!
   integer          klo     ,khi    ,i    ,k
   real             sig     ,p
   double precision yp1     ,ypn    ,h    ,a   ,b   ,qn   ,un
!
   yp1 = 0.
   ypn = 0.
!
!     Tijdelijke reeksen maken
!
!     Bepalen van y2
   y2(0) = 0.0
   u(0)  = 0.0
   if (yp1 .le. 0.99e30) then
      y2(0) = -0.5
      u(0)  = (3.0/(x(1) - x(0))) * ((y(1) - y(0)) /&
      &(x(1) - x(0)) - yp1)
   endif

   do i = 1,n - 2
      sig   = (x(i) - x(i-1)) / (x(i+1) - x(i-1))
      p     = sig * y2(i-1) + 2.
      y2(i) = (sig - 1) / p
      u(i)  = (6. * ((y(i+1)-y(i)) / (x(i+1)-x(i)) - (y(i)-y(i-1)) /&
      &(x(i)-x(i-1))) / (x(i+1)-x(i-1)) - sig * u(i-1)) / p
   enddo
!
   qn = 0.
   un = 0.
   if (ypn .le. 0.99e30) then
      qn = 0.5
      un = (3./(x(n-1)-x(n-2)))*(ypn-(y(n-1)-y(n-2))/(x(n-1)-x(n-2)))
   endif
   y2(n-1) = (un-qn*u(n-2))/(qn*y2(n-2)+1.0)
!
   do i = n - 2,0,-1
      y2(i) = y2(i) * y2(i+1) + u(i)
   enddo

!     Spline
   do i = 0, hn-1
      klo = 0
      khi = n - 1
!
      do while (khi - klo .gt. 1)
         k = (khi + klo) / 2
         if (x(k) .gt. hx(i)) then
            khi = k
         else
            klo = k
         endif
      enddo
!
      h     = x(khi) - x(klo)
      if (h .eq. 0.0) stop 'Foute waarde XA in SPLINE'
      a     = (x(khi) - hx(i)) / h
      b     = (hx(i) - x(klo)) / h
      hy(i) = a*y(klo) + b*y(khi) +&
      &((a**3-a)*y2(klo)+(b**3-b)*y2(khi))*(h**2)/6.0
   enddo

end
