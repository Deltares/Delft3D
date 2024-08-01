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

 subroutine setzminmax()
    use m_netw
    use m_flowgeom
    use m_flow
    use m_flowparameters
    use m_sediment, only: stm_included
    implicit none

    integer :: i, ki, kb, kt, itrac, isf

    if (kmx > 0) then ! 2D, set dummy values
       do i = 1, nbnds
          ki = kbnds(2, i)
          call getkbotktop(ki, kb, kt)
          zminmaxs(i) = zws(kb - 1)
          zminmaxs(i + nbnds) = zws(kt)
       end do
       do i = 1, nbndTM
          ki = kbndTM(2, i)
          call getkbotktop(ki, kb, kt)
          zminmaxTM(i) = zws(kb - 1)
          zminmaxTM(i + nbndTM) = zws(kt)
       end do
       do i = 1, nbnduxy
          ki = kbnduxy(2, i)
          call getkbotktop(ki, kb, kt)
          zminmaxuxy(i) = zws(kb - 1)
          zminmaxuxy(i + nbnduxy) = zws(kt)
       end do
       do i = 1, nbndu
          ki = kbndu(2, i)
          call getkbotktop(ki, kb, kt)
          zminmaxu(i) = zws(kb - 1)
          zminmaxu(i + nbndu) = zws(kt)
       end do
       do itrac = 1, numtracers
          do i = 1, nbndtr(itrac)
             ki = bndtr(itrac)%k(2, i)
             call getkbotktop(ki, kb, kt)
             bndtr(itrac)%zminmax(i) = zws(kb - 1)
             bndtr(itrac)%zminmax(i + nbndtr(itrac)) = zws(kt)
          end do
       end do
       do i = 1, nbndsd
          ki = kbndsd(2, i)
          call getkbotktop(ki, kb, kt)
          zminmaxsd(i) = zws(kb - 1)
          zminmaxsd(i + nbndsd) = zws(kt)
       end do

       if (jased == 4 .and. stm_included) then
          do isf = 1, numfracs
             do i = 1, nbndsf(isf)
                ki = bndsf(isf)%k(2, i)
                call getkbotktop(ki, kb, kt)
                bndsf(isf)%zminmax(i) = zws(kb - 1)
                bndsf(isf)%zminmax(i + nbndsf(isf)) = zws(kt)
             end do
          end do
       end if
    else ! For a kmx=0 model: set min and max to bedlevel and waterlevel respectively
       do i = 1, nbnds
          ki = kbnds(2, i)
          zminmaxs(i) = bl(ki)
          zminmaxs(i + nbnds) = s1(ki)
       end do
       do i = 1, nbndTM
          ki = kbndTM(2, i)
          zminmaxTM(i) = bl(ki)
          zminmaxTM(i + nbndTM) = s1(ki)
       end do
       do i = 1, nbnduxy
          ki = kbnduxy(2, i)
          zminmaxuxy(i) = bl(ki)
          zminmaxuxy(i + nbnduxy) = s1(ki)
       end do
       do i = 1, nbndu
          ki = kbndu(2, i)
          zminmaxu(i) = bl(ki)
          zminmaxu(i + nbndu) = s1(ki)
       end do
       do itrac = 1, numtracers
          do i = 1, nbndtr(itrac)
             ki = bndtr(itrac)%k(2, i)
             bndtr(itrac)%zminmax(i) = bl(ki)
             bndtr(itrac)%zminmax(i + nbndtr(itrac)) = s1(ki)
          end do
       end do
       do i = 1, nbndsd
          ki = kbndsd(2, i)
          zminmaxsd(i) = bl(ki)
          zminmaxsd(i + nbndsd) = s1(ki)
       end do

       if (jased == 4 .and. stm_included) then
          do isf = 1, numfracs
             do i = 1, nbndsf(isf)
                ki = bndsf(isf)%k(2, i)
                bndsf(isf)%zminmax(i) = bl(ki)
                bndsf(isf)%zminmax(i + nbndsf(isf)) = s1(ki)
             end do
          end do
       end if
    end if
 end subroutine setzminmax
