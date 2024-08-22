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

     subroutine SECTR(X, Y, TIJ, mmax, nmax, imax, &
                      merr, NUMI, &
                      NUMSPL, NUMPX, NTYP, MN12, XI, YI, XJ, YJ)
        use unstruc_colors
        use unstruc_messages
        use unstruc_display

        implicit none
        integer :: mmax, nmax, imax
        double precision, dimension(mmax, nmax), intent(inout) :: X, Y
        double precision, dimension(mmax, nmax), intent(out) :: TIJ
        integer, intent(out) :: merr, numi, numspl, numpx
        integer, dimension(imax) :: NTYP
        integer, dimension(imax, 3), intent(out) :: MN12
        double precision, dimension(imax), intent(out) :: XI, YI, XJ, YJ

!      INTEGER :: NTYP(IMAX), MN12(IMAX,3)
        character TEX1 * 4, TEX2 * 4
        double precision :: crp, ti, tj, xspc, yspc
        integer :: mcs, ncs, i, j, numpi, j2, ionbenoemd, numpj, numcro, &
                   L, jachange, icount, JK, maxm, maxn, jjlast, jj, iilast, ii
        integer :: jadubbel
        JADUBBEL = 0

        call CHECKSPL(X, Y, mmax, nmax, MCS, NCS)

5       continue
        call NUMS(X, mmax, nmax, NUMSPL, NUMPX)
        if (NUMSPL < 4) then
           call QNERROR('You Need 4 Splines or More to Create a Grid', ' ', ' ')
           merr = 1
           return
        else if (NUMSPL > IMAX) then
           write (TEX1, '(I4)') NUMSPL
           write (TEX2, '(I4)') IMAX
           call QNERROR('Number of Splines Larger than IMAX', TEX1, TEX2)
           call QNERROR('REDUCE THE NUMBER OF SPLINES', ' ', ' ')
           MERR = 1
           return
        end if

        call NULARR(TIJ, MMAX, NMAX)
        call INULAR(NTYP, IMAX)
        NTYP(1) = 1

        if (JADUBBEL >= 1) then
!        VERDUBBEL AANTAL STEUNPUNTEN ALS
           do I = 1, NUMSPL
              call NUMPold(X, mmax, nmax, I, NUMPI)
              call GETIJ(X, XI, mmax, nmax, imax, I, I, 1, NUMPI)
              call GETIJ(Y, YI, mmax, nmax, imax, I, I, 1, NUMPI)
              call SPLINE(XI, NUMPI, XJ)
              call SPLINE(YI, NUMPI, YJ)
              do J = 2 * NUMPI - 1, 2, -2
                 J2 = 1 + J / 2
                 X(I, J) = X(I, J2)
                 Y(I, J) = Y(I, J2)
              end do
              do J = 1, NUMPI - 1
                 TI = J - 0.5
                 J2 = 2 * J
                 call SPLINT(XI, XJ, NUMPI, TI, X(I, J2))
                 call SPLINT(YI, YJ, NUMPI, TI, Y(I, J2))
              end do
           end do
           call NUMS(X, mmax, nmax, NUMSPL, NUMPX)
        end if

        IONBENOEMD = 0
6       continue
        do I = 1, NUMSPL
           call READYY(' ', 0.01d0 + 0.3d0 * dble(I - 1) / dble(NUMSPL))
           do J = I + 1, NUMSPL
              call NUMPold(X, mmax, nmax, I, NUMPI)
              call NUMPold(X, mmax, nmax, J, NUMPJ)
              call GETIJ(X, XI, mmax, nmax, imax, I, I, 1, NUMPI)
              call GETIJ(Y, YI, mmax, nmax, imax, I, I, 1, NUMPI)
              call GETIJ(X, XJ, mmax, nmax, imax, J, J, 1, NUMPJ)
              call GETIJ(Y, YJ, mmax, nmax, imax, J, J, 1, NUMPJ)
              call SECT3r(XI, YI, XJ, YJ, imax, CRP, &
                          NUMPI, NUMPJ, NUMCRO, TI, TJ, XSPc, YSPc)
              if (NUMCRO == 1) then
                 if (NTYP(I) * NTYP(J) == 1) then
!                 al gelijk benoemd
                    if (NUMPX > NMAX / 2) then
                       call plotSpline(X(i, :), Y(i, :), numpi, NCOLDN)
                       call plotSpline(X(j, :), Y(j, :), numpj, NCOLRN)
                       call QNERROR(' ', ' ', 'Spaghetty; spline both in m- and n-direction')
                       MERR = MERR + 1
                       return
                    else
                       JADUBBEL = JADUBBEL + 1
                       call mess(LEVEL_DEBUG, 'SPLINE SUPPORT POINTS DOUBLED')
                       goto 5
                    end if
                 else if (NTYP(I) == 0 .and. NTYP(J) == 0) then
                    call mess(LEVEL_DEBUG, ' BOTH UNDEFINED YET')
                 else if (NTYP(J) == 0) then
                    NTYP(J) = -NTYP(I)
                    if (CRP * NTYP(I) < 0) then
                       call mess(LEVEL_DEBUG, ' SWITCHED J')
                       call SWITCH(X, Y, mmax, nmax, J, NUMPJ)
                       TJ = dble(NUMPJ) - 1 - TJ
                    end if
                 else if (NTYP(I) == 0) then
                    NTYP(I) = -NTYP(J)
                    if (CRP * NTYP(J) > 0) then
                       call mess(LEVEL_DEBUG, ' SWITCHED I')
                       call SWITCH(X, Y, mmax, nmax, I, NUMPI)
                       TI = dble(NUMPI) - 1 - TI
                    end if
                 end if
                 TIJ(I, J) = TI
                 TIJ(J, I) = TJ
              else if (NUMCRO >= 2) then
                 if (NUMPX > NMAX / 2) then
                    call plotSpline(X(i, :), Y(i, :), numpi, NCOLDN)
                    call plotSpline(X(j, :), Y(j, :), numpj, NCOLRN)
                    call QNERROR(' ', ' ', '2 splines appear to intersect more than once; modify splines')
                    MERR = MERR + 1
                    return
                 else
                    JADUBBEL = JADUBBEL + 1
                    goto 5
                 end if
              end if
           end do
        end do

        do I = 1, NUMSPL
           call NUMPold(X, mmax, nmax, I, NUMPI)
           if (NTYP(I) == 0) then
              IONBENOEMD = IONBENOEMD + 1
!           IF (IONBENOEMD .GT. NUMSPL) THEN
              if (IONBENOEMD > 1000) then
                 call plotSpline(X(i, :), Y(i, :), numpi, NCOLDN)
                 call QNERROR(' ', ' ', 'ONE OF THE SPLINES CANNOT BE ATTACHED IN THE GRID')
                 MERR = MERR + 1
                 return
              end if
              goto 6
           end if
        end do

!     sorteren op type, eerst de horizontalen (N = CONSTANT)
        do I = 1, NUMSPL
           if (NTYP(I) == -1) then
              do L = I + 1, NUMSPL
                 if (NTYP(L) == 1) then
                    call CHAROW(X, mmax, nmax, I, L, NUMPX)
                    call CHAROW(Y, mmax, nmax, I, L, NUMPX)
                    call CHAROW(TIJ, mmax, nmax, I, L, NUMSPL)
                    call CHACOL(TIJ, mmax, nmax, I, L, NUMSPL)
                    NTYP(I) = 1
                    NTYP(L) = -1
                    exit
                 end if
              end do
           end if
        end do

        do I = 1, NUMSPL
           if (NTYP(I) == 1) NUMI = I
        end do

59      continue
!     Sorteer de M
        JACHANGE = 0
        ICOUNT = 0
        do I = 1, NUMI
!        CALL READYY(' ',0.35 + 0.65*REAL(I-1)/REAL(NUMSPL-1) )
           do J = NUMI + 1, NUMSPL
              call NUMPold(X, mmax, nmax, I, NUMPI)
              call NUMPold(X, mmax, nmax, J, NUMPJ)
              if (TIJ(I, J) /= 0) then
                 do JK = J + 1, NUMSPL
                    if (TIJ(I, JK) /= 0) then
                       if (TIJ(I, J) > TIJ(I, JK)) then
                          call CHAROW(X, mmax, nmax, J, JK, NUMPX)
                          call CHAROW(Y, mmax, nmax, J, JK, NUMPX)
                          call CHAROW(TIJ, mmax, nmax, J, JK, NUMSPL)
                          call CHACOL(TIJ, mmax, nmax, J, JK, NUMSPL)
                          JACHANGE = 1
                          ICOUNT = ICOUNT + 1
                          if (ICOUNT > NUMSPL) then
                             call plotSpline(X(i, :), Y(i, :), numpi, NCOLDN)
                             call plotSpline(X(j, :), Y(j, :), numpj, NCOLRN)
                             call QNERROR(' ', ' ', 'PROBLEM IN SPLINE ORDERING, MODIFY SPLINES')
                             MERR = MERR + 1
                          end if
                          goto 59
                       end if
                    end if
                 end do

              end if
           end do
        end do

79      continue
        ICOUNT = 0
!     Sorteer de N
        do I = NUMI + 1, NUMSPL
!        CALL READYY(' ',0.35 + 0.65*REAL(I-1)/REAL(NUMSPL-1) )
           do J = 1, NUMI
              call NUMPold(X, mmax, nmax, I, NUMPI)
              call NUMPold(X, mmax, nmax, J, NUMPJ)
              if (TIJ(I, J) /= 0) then
                 do JK = J + 1, NUMI
                    if (TIJ(I, JK) /= 0) then
                       if (TIJ(I, J) > TIJ(I, JK)) then
                          call CHAROW(X, mmax, nmax, J, JK, NUMPX)
                          call CHAROW(Y, mmax, nmax, J, JK, NUMPX)
                          call CHAROW(TIJ, mmax, nmax, J, JK, NUMSPL)
                          call CHACOL(TIJ, mmax, nmax, J, JK, NUMSPL)
                          JACHANGE = 1
                          ICOUNT = ICOUNT + 1
                          if (ICOUNT > NUMSPL) then
                             call plotSpline(X(i, :), Y(i, :), numpi, NCOLDN)
                             call plotSpline(X(j, :), Y(j, :), numpj, NCOLRN)
                             call QNERROR(' ', ' ', 'PROBLEM IN SPLINE ORDERING, MODIFY SPLINES')
                             MERR = MERR + 1
                          end if
                          goto 79
                       end if
                    end if
                 end do
              end if
           end do
        end do
        if (JACHANGE == 1) goto 59

!     Initialiseer ranking, start en eind, 1,2,3
        do I = 1, NUMSPL
           MN12(I, 1) = 0
           MN12(I, 2) = 0
           MN12(I, 3) = 0
        end do

!     CALL SHOWADM(TIJ,MMAX,NMAX)

!     Eerst alles ranken in N richting
        do i = 1, NUMI
           do J = NUMI + 1, NUMSPL
              MAXN = 0
              JJLAST = 1
              do JJ = 1, I
                 if (TIJ(J, JJ) /= 0) then
                    MAXN = MN12(JJLAST, 1) + 1
                    JJLAST = JJ
                 end if
              end do

              MN12(J, 2) = MAXN
           end do

           MAXN = 0
           do J = NUMI + 1, NUMSPL
              if (TIJ(J, I) /= 0) MAXN = max(MN12(J, 2), MAXN)
           end do

           MN12(I, 1) = MAXN
        end do

!     Dan alles ranken in M richting
        do I = NUMI + 1, NUMSPL
           do J = 1, NUMI
              MAXM = 0
              IILAST = NUMI + 1
              do II = NUMI + 1, I
                 if (TIJ(J, II) /= 0) then
                    MAXM = MN12(IILAST, 1) + 1
                    IILAST = II
                 end if
              end do

              MN12(J, 3) = MAXM
           end do
           MAXM = 0
           do J = 1, NUMI
              if (TIJ(J, I) /= 0) MAXM = max(MN12(J, 3), MAXM)
           end do
           MN12(I, 1) = MAXM
        end do

        do I = 1, NUMSPL
           MN12(I, 2) = 0
           MN12(I, 3) = 0
        end do

!     Daarna per spline begin- en eindpunt tellen, eerst N = constant
        do I = 1, NUMI
           do J = NUMI + 1, NUMSPL
              if (TIJ(I, J) /= 0) then
                 if (MN12(I, 2) == 0) MN12(I, 2) = MN12(J, 1)
                 MN12(I, 3) = MN12(J, 1)
              end if
           end do
        end do

!     Dan M = constant
        do I = NUMI + 1, NUMSPL
           do J = 1, NUMI
              if (TIJ(I, J) /= 0) then
                 if (MN12(I, 2) == 0) MN12(I, 2) = MN12(J, 1)
                 MN12(I, 3) = MN12(J, 1)
              end if
           end do
        end do

        call READYY(' ', 0.95d0)

        do I = 1, NUMSPL
           write (msgbuf, *) I, (MN12(I, J), J=1, 3)
           call dbg_flush()
        end do

        return
     end subroutine sectr
