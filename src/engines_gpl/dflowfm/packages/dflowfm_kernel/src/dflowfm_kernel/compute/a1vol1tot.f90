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

module m_a1vol1tot

implicit none

private

public :: a1vol1tot

contains

 subroutine a1vol1tot()
    use m_flowgeom
    use m_flow
    use m_partitioninfo
    use m_flowtimes
    use precision_basics
    implicit none

    double precision, dimension(1) :: dum

    integer :: k

    if (jampi /= 1) then
       a1tot = sum(a1(1:ndxi))
       vol1tot = sum(vol1(1:ndxi))
       if (interceptionmodel == DFM_HYD_INTERCEPT_LAYER) then
          vol1icept = sum(bare(1:ndxi) * InterceptHs(1:ndxi))
       end if
    else
       a1tot = 0d0
       vol1tot = 0d0
       vol1icept = 0d0

       do k = 1, Ndxi
          if (idomain(k) == my_rank) then
             a1tot = a1tot + a1(k)
             vol1tot = vol1tot + vol1(k)
             if (interceptionmodel == DFM_HYD_INTERCEPT_LAYER) then
                vol1icept = vol1icept + bare(k) * InterceptHs(k)
             end if
          end if
       end do

       !a1tot     = sum(a1  (1:ndxi), mask=idomain(1:Ndxi).eq.my_rank)
       !vol1tot   = sum(vol1(1:ndxi), mask=idomain(1:Ndxi).eq.my_rank)

!   begin debug
!    call reduce_double_sum(vol1tot)
!   end debug
    end if

    if (jagrw > 0) then
       if (volgrw > 0 .and. volgrwini <= 0) then
          volgrwini = volgrw
       end if
    end if

    if (comparereal(time1, tstart_user, eps10) == 0) then
       volcur(IDX_VOLTOT) = vol1tot
       volcur(IDX_STOR) = vol1tot
       vol1ini = vol1tot

!   vol1ini needs to be global
       if (jampi == 1) then
          call reduce_double_sum(1, (/vol1ini/), dum)
          vol1ini = dum(1)
       end if
    end if

! begin debug
!  if ( my_rank.eq.0 ) write(6,*) 'vol1tot =', vol1tot
! end debug

 end subroutine a1vol1tot

end module m_a1vol1tot
