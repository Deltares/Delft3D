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

module m_setsorsin

implicit none

private

public :: setsorsin

contains

 !> Compute and set source and sink values for the 'intake-outfall' structures.
 subroutine setsorsin()
    use m_flow
    use m_flowtimes, only: dts
    use m_transport, only: NUMCONST, constituents
    use MessageHandling, only: LEVEL_WARN, msgbuf, mess
    use m_partitioninfo, only: jampi, reduce_srsn
    use m_get_kbot_ktop

    integer :: n, kk, k, kb, kt, kk2, ku, numvals, L
    double precision :: qsrck, qsrckk, dzss
    double precision :: frac = 0.5d0 ! cell volume fraction that can at most be extracted in one step

    srsn = 0d0; vcsrc = 0d0
    do n = 1, numsrc
       kk = ksrc(1, n) ! 2D pressure cell nr, From side, 0 = out of all, -1 = in other domain, > 0, own domain
       kk2 = ksrc(4, n) ! 2D pressure cell nr, To   side, 0 = out of all, -1 = in other domain, > 0, own domain
       qsrc(n) = qstss((1 + numconst) * (n - 1) + 1)
       if (kk > 0) then ! FROM point
          if (kmx > 0) then
             call getkbotktop(kk, kb, kt)
             if (zsrc(1, n) == dmiss) then
                k = kb; ku = kt
             else
                do k = kb, kt
                   if (zws(k) > zsrc(1, n) .or. k == kt) then
                      exit
                   end if
                end do
                if (zsrc2(1, n) == dmiss) then
                   ku = k
                else
                   do ku = kb, kt
                      if (zws(ku) > zsrc2(1, n) .or. ku == kt) then
                         exit
                      end if
                   end do
                end if
             end if
          else
             k = kk; kt = kk; ku = kk ! in 2D, volume cell nr = pressure cell nr
          end if
          ksrc(2, n) = k ! store kb of src
          ksrc(3, n) = ku !
          if (qsrc(n) > 0) then ! Reduce if flux pos

             do k = ksrc(2, n), kt
                srsn(1, n) = srsn(1, n) + vol1(k)
                do L = 1, numconst
                   srsn(1 + L, n) = srsn(1 + L, n) + constituents(L, k) * vol1(k)
                end do
                ksrc(3, n) = k
                if (frac * srsn(1, n) / dts > abs(qsrc(n))) then
                   exit
                end if
             end do
             if (srsn(1, n) > 0d0) then
                do L = 1, numconst
                   srsn(1 + L, n) = srsn(1 + L, n) / srsn(1, n)
                end do
             end if
             do k = ksrc(2, n), ksrc(3, n)
                !if (jasal > 0) constituents(isalt,k) = srsn(1+isalt,n)
                !if (jatem > 0) constituents(itemp,k) = srsn(1+itemp,n)
                do L = 1, numconst
                   constituents(L, k) = srsn(L + 1, n)
                end do
             end do

          end if
       end if

       if (kk2 > 0) then ! TO point
          if (kmx > 0) then
             call getkbotktop(kk2, kb, kt)
             if (zsrc(2, n) == dmiss) then
                k = kb; ku = kt
             else
                do k = kb, kt
                   if (zws(k) > zsrc(2, n) .or. k == kt) then
                      exit
                   end if
                end do
                if (zsrc2(2, n) == dmiss) then
                   ku = k
                else
                   do ku = kb, kt
                      if (zws(ku) > zsrc2(2, n) .or. ku == kt) then
                         exit
                      end if
                   end do
                end if
             end if
          else
             k = kk2; kt = kk2; ku = kk2 ! in 2D, volume cell nr = pressure cell nr
          end if
          ksrc(5, n) = k
          ksrc(6, n) = ku
          if (qsrc(n) < 0) then ! Reduce if flux neg

             do k = ksrc(5, n), kt
                srsn(1 + numconst + 1, n) = srsn(1 + numconst + 1, n) + vol1(k)
                do L = 1, numconst
                   srsn(1 + numconst + 1 + L, n) = srsn(1 + numconst + 1 + L, n) + constituents(L, k) * vol1(k)
                end do
                ksrc(6, n) = k
                if (frac * srsn(1 + numconst + 1, n) / dts > abs(qsrc(n))) then
                   exit
                end if
             end do
             if (srsn(1 + numconst + 1, n) > 0d0) then
                do L = 1, numconst
                   srsn(1 + numconst + 1 + L, n) = srsn(1 + numconst + 1 + L, n) / srsn(1 + numconst + 1, n)
                end do
             end if
             do k = ksrc(5, n), ksrc(6, n)
                !if (jasal > 0) constituents(isalt,k) = srsn(1+numconst+1+isalt,n)
                !if (jatem > 0) constituents(itemp,k) = srsn(1+numconst+1+itemp,n)
                do L = 1, numconst
                   constituents(L, k) = srsn(1 + numconst + 1 + L, n)
                end do
             end do

          end if
       end if

    end do

    if (jampi > 0) then
       numvals = 2 * (1 + numconst)
       call reduce_srsn(numvals, numsrc, srsn)
    end if

    jamess = 0
    do n = 1, numsrc
       qsrc(n) = qstss((numconst + 1) * (n - 1) + 1)
       do L = 1, numconst
          ccsrc(L, n) = qstss((numconst + 1) * (n - 1) + L + 1)
       end do

       kk = ksrc(1, n) ! 2D pressure cell nr
       qsrck = qsrc(n)
       if (kk /= 0 .and. qsrck > 0) then ! Extract FROM 1
          if (frac * srsn(1, n) / dts < abs(qsrck)) then
             qsrck = frac * srsn(1, n) / dts; jamess(n) = 1
          end if
       end if

       kk2 = ksrc(4, n) ! 2D pressure cell nr
       if (kk2 /= 0 .and. qsrck < 0) then ! Extract From 2
          if (frac * srsn(1 + numconst + 1, n) / dts < abs(qsrck)) then
             qsrck = -frac * srsn(1 + numconst + 1, n) / dts; jamess(n) = 2
          end if
       end if

       qsrc(n) = qsrck

       if (kk * kk2 /= 0) then ! Coupled stuff
          if (qsrck > 0) then ! FROM k to k2
             do L = 1, numconst
                ccsrc(L, n) = ccsrc(L, n) + srsn(1 + L, n)
             end do
          else if (qsrck < 0) then ! FROM k2 to k
             do L = 1, numconst
                ccsrc(L, n) = ccsrc(L, n) + srsn(1 + numconst + 1 + L, n)
             end do
          end if
       end if

       if (kk > 0) then ! FROM Point
          qsrckk = qsrc(n)
          qin(kk) = qin(kk) - qsrckk ! add to 2D pressure cell nr
          do k = ksrc(2, n), ksrc(3, n)
             if (kmx > 0) then
                dzss = zws(ksrc(3, n)) - zws(ksrc(2, n) - 1)
                if (dzss > epshs) then
                   qsrck = qsrckk * (zws(k) - zws(k - 1)) / dzss
                else
                   qsrck = qsrckk / (ksrc(3, n) - ksrc(2, n) + 1)
                end if
                qin(k) = qin(k) - qsrck
             end if
          end do
       end if

       if (kk2 > 0) then ! TO Point
          qsrckk = qsrc(n)
          qin(kk2) = qin(kk2) + qsrckk ! add to 2D pressure cell nr
          do k = ksrc(5, n), ksrc(6, n)
             if (kmx > 0) then
                dzss = zws(ksrc(6, n)) - zws(ksrc(5, n) - 1)
                if (dzss > epshs) then
                   qsrck = qsrckk * (zws(k) - zws(k - 1)) / dzss
                else
                   qsrck = qsrckk / (ksrc(6, n) - ksrc(5, n) + 1)
                end if
                qin(k) = qin(k) + qsrck
             end if
          end do
       end if

    end do

    do n = 1, numsrc
       if (jamess(n) == 1) then
          write (msgbuf, *) 'Extraction flux larger than cell volume at point 1 of : ', trim(srcname(n))
          call mess(LEVEL_WARN, msgbuf)
       else if (jamess(n) == 2) then
          write (msgbuf, *) 'Extraction flux larger than cell volume at point 2 of : ', trim(srcname(n))
          call mess(LEVEL_WARN, msgbuf)
       end if
    end do

 end subroutine setsorsin

end module m_setsorsin
