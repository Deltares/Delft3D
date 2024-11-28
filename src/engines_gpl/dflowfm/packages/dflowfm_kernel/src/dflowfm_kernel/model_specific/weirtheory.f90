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

module m_weirtheory

implicit none

private

public :: weirtheory

contains

subroutine weirtheory(zupstream, zdownstream, crestheight, zcrestperfect, zminsub, zcrest, &
                      qweir, uupstream, ucrest, udownstream, regime, qfree)
   double precision :: zupstream, zdownstream, crestheight, zcrestperfect, zminsub, zcrest, &
      qweir, uupstream, ucrest, udownstream, qfree
   double precision :: pi, g, d, z1, h1, p, q, cosfi, fi, zc1, zc2, zc3, &
      res1, res2, res3, z2, z2critical, h2, u1, u2, u3, qd, ff, z3, z3critical, &
      h3, fz2, z3inp, z2a, fz2a, z2b, fz2b, z2c, fz2c, za, zb, fa, fb, zc, fc, &
      fr1, fr2, fr3
   character(len=*) :: regime

! input variables:
!   zupstream     : upstream water level
!   zdownstream   : downstream water level
!   crestheight   : crestheight
! outputvariables :
!   zcrestperfect : waterlevel on crest for critical flow
!   zminsub       : minimum down stream waterlevel for submerged flow
!   zcrest        : waterlevel on crest
!   qweir         : discharge/m
!   uupstream     : upstream velocity
!   ucrest        : velocity on crest
!   udownstream   : downstream velocity        :

! open(5,file='weirtheory.dia')

   qweir = 0; uupstream = 0; ucrest = 0; udownstream = 0

   regime = 'subcritical'

   if (zupstream < zdownstream) return

   pi = 4.0d0 * atan(1.0d0)
   g = 9.81d0
   d = crestheight
   z1 = zupstream
   h1 = z1 + d

! compute critical depth on crest by solving: z^3-3*h1^z+2*h1^2*zupstream=0
! equation is solved analytically method of Gardano

   p = 3.0d0 * (h1**2)
   q = 2.0d0 * (h1**2) * z1
   cosfi = z1 / h1
   fi = acos(cosfi)
   zc1 = -2.0d0 * h1 * cos(fi / 3.0d0)
   zc2 = -2.0d0 * h1 * cos(fi / 3.0d0 + 2.0d0 * pi / 3.0d0)
   zc3 = -2.0d0 * h1 * cos(-fi / 3.0d0 + 2.0d0 * pi / 3.0d0)

   if (zc3 < 0) return
! write(5,'(3e14.5)') zc1,zc2,zc3
   res1 = zc1**3 - p * zc1 + q
   res2 = zc2**3 - p * zc2 + q
   res3 = zc3**3 - p * zc3 + q
! write(5,'(3d14.5)') res1,res2,res3

   z2 = zc3
   z2critical = z2
   h2 = z2 + d
   u2 = sqrt(g * z2)
   qd = z2 * u2

   qfree = qd

! compute maximum down stream water level for perfect weir or minimum water level for submerged weir
! for this the equation F=q^2/h3+0.5*g*h3^2 is solved analytically with Gardano

   ff = qd * u2 + 0.5d0 * g * h2 * h2
   p = 2.0d0 * ff / g
   q = 2.0d0 * qd**2 / g
   cosfi = 0.5 * q / sqrt((p / 3.0d0)**3)
   cosfi = max(-1d0, min(cosfi, 1d0))
   fi = acos(cosfi)
   zc1 = -2.0d0 * sqrt(p / 3.0d0) * cos(fi / 3.0d0)
   zc2 = -2.0d0 * sqrt(p / 3.0d0) * cos(fi / 3.0d0 + 2.0d0 * pi / 3.0d0)
   zc3 = -2.0d0 * sqrt(p / 3.0d0) * cos(-fi / 3.0d0 + 2.0d0 * pi / 3.0d0)
!write(5,'(3e14.5)') zc1-d,zc2-d,zc3-d
   res1 = zc1**3 - p * zc1 + q
   res2 = zc2**3 - p * zc2 + q
   res3 = zc3**3 - p * zc3 + q
!write(5,'(3d14.5)') res1,res2,res3

   z3 = zc2 - d
   z3critical = z3
   zminsub = z3
   zcrestperfect = z2critical
   h3 = d + z3

   fz2 = 4.0d0 * (z2 - z1) * h1**2 * z2 * (h3 - z2) - (h3**2 - h2**2) * (z2**2 - h1**2) * h3

!write(5,'(d15.5)') fz2

! compute subcritical weir by solving:
! 4.0d0*(z2-z1)*h1**2*z2*(h3-z2)-(h3**2-h2**2)*(z2**2-h1**2)*h3=0

   z3inp = zdownstream

   if (z3inp < z3critical) then
      qweir = qd
      zcrest = zcrestperfect
      uupstream = qd / h1
      ucrest = qd / zcrest
      h3 = zdownstream + crestheight
      udownstream = qd / h3
      regime = 'supercritical'
      return
   end if

   z3 = z3inp
   h3 = z3inp + d
   z2a = z2
   fz2a = 4.0d0 * (z2 - z1) * h1**2 * z2 * (h3 - z2) - (h3**2 - h2**2) * (z2**2 - h1**2) * h3
   z2b = z3inp
   h2 = z2b + d
   z2 = z2b
   fz2b = 4.0d0 * (z2 - z1) * h1**2 * z2 * (h3 - z2) - (h3**2 - h2**2) * (z2**2 - h1**2) * h3
   z2c = z1
   z2 = z2c
   h2 = z2c + d
   fz2c = 4.0d0 * (z2 - z1) * h1**2 * z2 * (h3 - z2) - (h3**2 - h2**2) * (z2**2 - h1**2) * h3
! write(5,'(3d14.5)') fz2a,fz2b,fz2c

! apply bisection

   if (fz2a * fz2b < 0) then
      za = z2a
      zb = z2b
      fa = fz2a
      fb = fz2b
      zc = 0.5 * (z2a + z2b)
      z2 = zc
      h2 = zc + d
      fc = 4.0d0 * (z2 - z1) * h1**2 * z2 * (h3 - z2) - (h3**2 - h2**2) * (z2**2 - h1**2) * h3
   else if (fz2b * fz2c < 0) then
      za = z2b
      zb = z2c
      fa = fz2b
      fb = fz2c
      zc = 0.5 * (za + zb)
      z2 = zc
      h2 = zc + d
      fc = 4.0d0 * (z2 - z1) * h1**2 * z2 * (h3 - z2) - (h3**2 - h2**2) * (z2**2 - h1**2) * h3
   end if

   do while (abs(fc) > 1.0d-10)
      if (fa * fc < 0) then
         zb = zc
         fb = fc
         zc = 0.5 * (za + zb)
         z2 = zc
         h2 = zc + d
         fc = 4.0d0 * (z2 - z1) * h1**2 * z2 * (h3 - z2) - (h3**2 - h2**2) * (z2**2 - h1**2) * h3
      else if (fb * fc < 0) then
         za = zc
         fa = fc
         zc = 0.5 * (za + zb)
         z2 = zc
         h2 = zc + d
         fc = 4.0d0 * (z2 - z1) * h1**2 * z2 * (h3 - z2) - (h3**2 - h2**2) * (z2**2 - h1**2) * h3
      end if
   end do

! write(5,'('' water levels:'',3d15.5)') z1,z2,z3
   h1 = z1 + d
   h2 = z2 ! +d
   h3 = z3 + d
   qd = sqrt((2.0d0 * g * (z2 - z1) * h1**2 * z2**2) / (z2**2 - h1**2))
! qdb=sqrt(0.5d0*g*(h3**2-h2**2)*z2*h3/(h3-z2))

! write(5,'('' discharge/m:'',d15.5)') qd

   u1 = qd / h1; u2 = qd / h2; u3 = qd / h3
! write(5,'('' velocities:'',3d15.5)') u1,u2,u3
   fr1 = u1 / sqrt(g * h1); fr2 = u2 / sqrt(g * h2); fr3 = u3 / sqrt(g * h3)
! write(5,'('' Froude numbers:'',3d15.5)') fr1,fr2,fr3

   zcrest = z2
   qweir = qd
   uupstream = u1
   ucrest = u2
   udownstream = u3

   return

end subroutine weirtheory

end module m_weirtheory
