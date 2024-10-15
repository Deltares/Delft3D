!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2022.
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
module m_fm1dimp_update_bc
   implicit none
contains
!> Updates the boundary conditions. The FM variables are updated in <flow_initimestep>
! and here we put them into the table that uses SRE.
   subroutine fm1dimp_update_bc(iresult, time1)

      use m_flow, only: au
      use m_f1dimp, only: f1dimppar
      use fm_external_forcings_data
!pointer
      integer, pointer :: table_length
      integer, pointer :: maxtab

!input
      double precision, intent(in) :: time1 !t^{n+1}

!output
      integer, intent(out) :: iresult !< Error status, DFM_NOERR==0 if succesful.

!local
      integer :: k, ktab, nq, n, L
      integer :: table_number

!point
      table_length => f1dimppar%table_length
      maxtab => f1dimppar%maxtab

!!
!! CALC
!!

      iresult = 0

      do k = 1, f1dimppar%nhstat
         table_number = f1dimppar%hbdpar(3, k)
         !x (time)
         ktab = table_number * table_length - 1
         f1dimppar%table(ktab) = time1
         f1dimppar%table(ktab + 1) = time1 + 1d0 !It does not matter, as the query time will be <time1>
         !y (var)
         ktab = maxtab * table_length + table_number * table_length - 1
         f1dimppar%table(ktab) = zbndz(k)
         f1dimppar%table(ktab + 1) = zbndz(k)
      end do
!   q

      do k = 1, f1dimppar%nqstat
         table_number = f1dimppar%qbdpar(3, k) !< table number after the ones of <hbdpar>
         !x (time)
         ktab = table_number * table_length - 1
         f1dimppar%table(ktab) = time1
         f1dimppar%table(ktab + 1) = time1 + 1d0 !It does not matter, as the query time will be <time1>
         !y (var)
         ktab = maxtab * table_length + table_number * table_length - 1

         !FM1DIMP2DO: properly understand what happens!
         !In <setau>, the discharge read in BC is converted into a
         !normalized discharge. Here we reconvert it to the actual
         !discharge.
         nq = k
         n = L1qbnd(nq) !, L2qbnd(nq)
         L = kbndu(3, n)
         f1dimppar%table(ktab) = zbndq(k) * au(L)
         f1dimppar%table(ktab + 1) = zbndq(k) * au(L)
      end do

!f1dimppar%table=(/ 0d0,86400d0,0d0,10000d0,1.00666656855963d0,1.00666656855963d0,100d0,100d0 /)
!                     call INTTAB (ntab(1,itab), ntab(4,itab),
!     +                            table(ntab(3,itab)),
!     +                            table(ntab(2,itab)),
!     +                            dble(watlev),qh    )

!zbndz

   end subroutine fm1dimp_update_bc

end module m_fm1dimp_update_bc
