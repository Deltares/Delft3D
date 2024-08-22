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

 subroutine carrier(time1)
    implicit none
    double precision :: time1

    double precision :: J0(100), J1(100), A1(100), A3(100), ahh
    double precision :: h0, T0, s, etinbr, dc, tol, etabr, etain, a, a2
    double precision :: omega, sg, osg, osg2, rl0, c, dt, t, uold, u, x, xster, hh, uu, xx
    double precision :: pi, g
    integer :: ic1, ic2, nt, ic, iter, it
    double precision :: bessj0, bessj1
    common / signal / ahh

    !open(1,file='carrier.inp')
    !open(2,file='carrier.out')
    !open(3,file='carrier.env')
    !open(4,file='carrier.u')
    !open(5,file='carrier.tx')

    h0 = 5
    T0 = 32
    s = .04
    etinbr = 0.5d0 ! eta in /eta br
    dc = .09906
    ic1 = 1
    ic2 = 100
    nt = 21
    tol = .0001

    pi = 4.*atan(1.)
    g = 9.81d0

    etabr = 1./sqrt(128.) / (pi**3) * s**2.5d0 * T0**2.5d0 * g**1.25d0 * h0**(-.25)
    etain = etinbr * etabr
    A = etain * pi / sqrt(.125 * s * T0 * sqrt(g / h0))

! write(*,*)' eta in     = ',etain
! write(*,*)' A/ eta in  = ',A/etain
! write(*,*)' eta br     = ',etabr
! write(*,*)' etain/etabr= ',etain/etabr

    omega = 2.*pi / T0
    sg = s * g
    osg = omega / sg
    osg2 = 2.*osg

    rl0 = T0 * sqrt(sg)

    do ic = ic1, ic2
       C = ic * dc
       J0(ic) = BESSj0(osg2 * C)
       J1(ic) = BESSj1(osg2 * C)
       A1(ic) = A * g / C * J1(ic)
    end do

    A2 = osg
    dt = T0 / (Nt - 1)
    t = -dt
    t = time1
    it = 1
    ! do it=1,Nt
    ! t=t+dt
    A3(it) = omega * t
    do ic = ic1, ic2
       C = ic * dc
       do iter = 1, 100
          uold = u
          u = A1(ic) * cos(A2 * u - A3(it))
          if (abs(uold - u) < tol) exit
       end do
       x = .5 * u * u / sg + C * C / sg - A / s * J0(ic) * sin(A2 * u - A3(it))

       xster = x * 4./(sg * T0 * T0)

       hh = (C * C / g - s * x) / A
       uu = u / (A * omega / s)

       xx = 125d0 - x / 2

       ahh = a * hh
       if (ic == ic1) then
          call movabs(xx, ahh)
       else
          call lnabs(xx, ahh)
       end if

!       write(2,'(2f10.4)')xster,(C*C/g-s*x)/A
!       write(3,'(2f10.4)')xster,(A/pi*sqrt(.5*s*T0*g/C))/A
!       write(4,'(2f10.4)')xster,u/(A*omega/s)

!       if (ic.eq.ic1) write(5,'(2f10.4)')t,x
    end do
    ! enddo

 end subroutine carrier
