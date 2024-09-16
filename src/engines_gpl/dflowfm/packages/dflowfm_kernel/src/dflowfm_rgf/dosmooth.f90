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

      subroutine DOSMOOTH(NFLD)
         use m_gridsettings
         use m_grid
         use unstruc_colors
         use unstruc_messages
         use m_smeerfunctie
         use m_drawthis
         use m_grid_block
         use m_readyy
         use m_isitu
         implicit none
         integer :: nfld
         double precision, allocatable :: XH(:, :), YH(:, :)

         integer :: MD, ND, M1, M2, N1, N2, JS, I, J, K, JA1, JA2
         double precision :: R1, R2, R3, FR, XX, YY, X21, X22, Y21, Y22, X41, X42, Y41, Y42, &
            A, B, TV1, TV2

         allocate (XH(MMAX, NMAX), YH(MMAX, NMAX))

         if (NDRAW(8) == 0) call READYY('SMOOTHING GRID', 0d0)
!      CALL ISITU(      X,      Y,     MC,   NC,    IJC,  IJYES)!!!Oud
         ! Deze routine dosmooth wordt alleen uit editgridlineblock aangeroepen
         ! met de xc, ijyes, etc. uit m_grid. Diezelfde m_grid wordt in isitu gebruikt
         ! dus hoeven het niet meer door te geven.
         call ISITU()
         call PUTARR(Xc, XH, MMAX, NMAX)
         call PUTARR(Yc, YH, MMAX, NMAX)

         MD = MB(2) - MB(1)
         ND = NB(2) - NB(1)

         M1 = MB(3)
         N1 = NB(3)
         M2 = MB(4)
         N2 = NB(4)

         JS = 1
         do K = 1, ITSMO

            if (MD == 0 .and. NFLD == 9) then
!           verticale linemodes

               do I = 1, MC
                  do J = 2, NC - 1
                     if (J > N1 .and. J < N2) then
                        if (IJC(I, J) == 10 .or. IJC(I, J) == 2 .or. IJC(I, J) == 4) then
                           R1 = sqrt((XH(I, J) - XH(I, J - 1))**2 + &
                                     (YH(I, J) - YH(I, J - 1))**2)
                           R2 = sqrt((XH(I, J) - XH(I, J + 1))**2 + &
                                     (YH(I, J) - YH(I, J + 1))**2)

                           if ((M2 - M1) /= 0) then
                              call SMEERFUNCTIE(I, J, MB(1), J, FR, 1, 0)
                           else
                              FR = 1d0
                           end if

                           if (JS == 1) then
                              if (R1 > R2) then
                                 R3 = (R1 - R2) / 2
                                 A = FR * CSMO * R3 / R1
                                 XX = XH(I, J) + A * (XH(I, J - 1) - Xc(I, J))
                                 YY = YH(I, J) + A * (YH(I, J - 1) - Yc(I, J))
                              else
                                 R3 = (R2 - R1) / 2
                                 A = FR * CSMO * R3 / R2
                                 XX = XH(I, J) + A * (XH(I, J + 1) - Xc(I, J))
                                 YY = YH(I, J) + A * (YH(I, J + 1) - Yc(I, J))
                              end if
                           else
                              A = 0.1
                              if (R1 < R2) then
                                 XX = XH(I, J) + A * (XH(I, J - 1) - Xc(I, J))
                                 YY = YH(I, J) + A * (YH(I, J - 1) - Yc(I, J))
                              else
                                 XX = XH(I, J) + A * (XH(I, J + 1) - Xc(I, J))
                                 YY = YH(I, J) + A * (YH(I, J + 1) - Yc(I, J))
                              end if
                           end if
                           Xc(I, J) = XX
                           Yc(I, J) = YY
                        end if
                     end if
                  end do
               end do

            else if (ND == 0 .and. NFLD == 9) then
!           horizontale linemodes

               do I = 2, MC - 1
                  do J = 1, NC
                     if (I > M1 .and. I < M2) then
                        if (IJC(I, J) == 10 .or. IJC(I, J) == 1 .or. IJC(I, J) == 3) then
                           R1 = (XH(I, J) - XH(I - 1, J))**2 + &
                                (YH(I, J) - YH(I - 1, J))**2
                           R2 = (XH(I, J) - XH(I + 1, J))**2 + &
                                (YH(I, J) - YH(I + 1, J))**2

                           if ((N2 - N1) /= 0) then
                              call SMEERFUNCTIE(I, J, I, NB(1), FR, 0, 1)
                           else
                              FR = 1
                           end if

                           if (JS == 1) then
                              if (R1 > R2) then
                                 R3 = (R1 - R2) / 2
                                 if (abs(R1) < 1d-8) then
                                    A = 0.5d0
                                 else
                                    A = FR * CSMO * R3 / R1
                                 end if
                                 XX = XH(I, J) + A * (XH(I - 1, J) - Xc(I, J))
                                 YY = YH(I, J) + A * (YH(I - 1, J) - Yc(I, J))
                              else
                                 R3 = (R2 - R1) / 2
                                 if (abs(R2) < 1d-8) then
                                    A = 0.5d0
                                 else
                                    A = FR * CSMO * R3 / R2
                                 end if
                                 XX = XH(I, J) + A * (XH(I + 1, J) - Xc(I, J))
                                 YY = YH(I, J) + A * (YH(I + 1, J) - Yc(I, J))
                              end if
                           else
                              A = 0.1
                              if (R1 < R2) then
                                 XX = XH(I, J) + A * (XH(I - 1, J) - Xc(I, J))
                                 YY = YH(I, J) + A * (YH(I - 1, J) - Yc(I, J))
                              else
                                 XX = XH(I, J) + A * (XH(I + 1, J) - Xc(I, J))
                                 YY = YH(I, J) + A * (YH(I + 1, J) - Yc(I, J))
                              end if
                           end if
                           Xc(I, J) = XX
                           Yc(I, J) = YY
                        end if
                     end if
                  end do
               end do

            else if (NFLD == 17) then
!           blockmode

               B = CSMO
               A = 1 - B
               do I = 1, MC
                  do J = 1, NC
                     if (I >= M1 .and. I <= M2 .and. &
                         J >= N1 .and. J <= N2) then
                        if (IJC(I, J) == 10) then
                           Xc(I, J) = A * XH(I, J) + B * (XH(I - 1, J) + XH(I + 1, J)) / 4 &
                                      + B * (XH(I, J - 1) + XH(I, J + 1)) / 4
                           Yc(I, J) = A * YH(I, J) + B * (YH(I - 1, J) + YH(I + 1, J)) / 4 &
                                      + B * (YH(I, J - 1) + YH(I, J + 1)) / 4
                        else if (IJC(I, J) >= 1 .and. IJC(I, J) <= 4) then
                           if (IJC(I, J) == 1) then
                              XX = A * XH(I, J) + B * &
                                   (XH(I - 1, J) + XH(I + 1, J) + XH(I, J + 1)) / 3
                              YY = A * YH(I, J) + B * &
                                   (YH(I - 1, J) + YH(I + 1, J) + YH(I, J + 1)) / 3
                           else if (IJC(I, J) == 3) then
                              XX = A * XH(I, J) + B * &
                                   (XH(I - 1, J) + XH(I + 1, J) + XH(I, J - 1)) / 3
                              YY = A * YH(I, J) + B * &
                                   (YH(I - 1, J) + YH(I + 1, J) + YH(I, J - 1)) / 3
                           else if (IJC(I, J) == 2) then
                              XX = A * XH(I, J) + B * &
                                   (XH(I, J - 1) + XH(I, J + 1) + XH(I - 1, J)) / 3
                              YY = A * YH(I, J) + B * &
                                   (YH(I, J - 1) + YH(I, J + 1) + YH(I - 1, J)) / 3
                           else if (IJC(I, J) == 4) then
                              XX = A * XH(I, J) + B * &
                                   (XH(I, J - 1) + XH(I, J + 1) + XH(I + 1, J)) / 3
                              YY = A * YH(I, J) + B * &
                                   (YH(I, J - 1) + YH(I, J + 1) + YH(I + 1, J)) / 3
                           end if
                           call MOVABS(XH(I, J), YH(I, J))
                           call LNABS(XX, YY)
                           if (IJC(I, J) == 1 .or. IJC(I, J) == 3) then
                              X21 = XH(I - 1, J)
                              Y21 = YH(I - 1, J)
                              X22 = XH(I + 1, J)
                              Y22 = YH(I + 1, J)
                           elseif (IJC(I, J) == 2 .or. IJC(I, J) == 4) then
                              X21 = XH(I, J - 1)
                              Y21 = YH(I, J - 1)
                              X22 = XH(I, J + 1)
                              Y22 = YH(I, J + 1)
                           end if
                           call ORTPRO2(XH(I, J), YH(I, J), X21, Y21, &
                                        XX, YY, X41, Y41, TV1, JA1)
                           call ORTPRO2(XH(I, J), YH(I, J), X22, Y22, &
                                        XX, YY, X42, Y42, TV2, JA2)
                           if (JA1 == 1 .and. JA2 == 1) then
                              if (TV2 > TV1) then
                                 Xc(I, J) = X42
                                 Yc(I, J) = Y42
                              else
                                 Xc(I, J) = X41
                                 Yc(I, J) = Y41
                              end if
                           else if (JA1 == 1) then
                              Xc(I, J) = X41
                              Yc(I, J) = Y41
                           else if (JA2 == 1) then
                              Xc(I, J) = X42
                              Yc(I, J) = Y42
                           else
                              Xc(I, J) = (X41 + X42) / 2
                              Yc(I, J) = (Y41 + Y42) / 2
                              write (msgbuf, *) 'BLOCK VORM VERLIES'; call dbg_flush()
                           end if
                        end if
                     end if
                  end do
               end do

            end if

            call PUTARR(Xc, XH, MMAX, NMAX)
            call PUTARR(Yc, YH, MMAX, NMAX)
            if (NDRAW(8) == 0) then
               call READYY(' ', dble(K) / dble(ITSMO))
            else
               call TEKGRD(Xc, Yc, mmax, nmax, M1, N1, M2, N2, NCOLDG, NDRAW(38), -1, mc) ! key=-1 is unknown (but unused anyway)
            end if

         end do

         call PUTARR(XH, Xc, MMAX, NMAX)
         call PUTARR(YH, Yc, MMAX, NMAX)
         deallocate (XH, YH)
         if (NDRAW(8) == 0) call READYY(' ', -1d0)

         return
      end subroutine dosmooth
