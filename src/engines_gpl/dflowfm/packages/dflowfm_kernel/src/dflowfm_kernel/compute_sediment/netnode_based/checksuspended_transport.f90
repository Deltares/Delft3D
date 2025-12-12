!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2025.
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

module m_checksuspended_transport
   use m_check_einstein_garcia2, only: check_einstein_garcia2

   implicit none

   private

   public :: checksuspended_transport

contains

   subroutine checksuspended_transport()
      use precision, only: dp
      use m_check_einstein_garcia, only: check_einstein_garcia
      use m_filez, only: doclose, newfil

      real(kind=dp) :: hsk
      real(kind=dp) :: ucr, ueff, Ucur, Pmob, sster, wster, ws
      real(kind=dp) :: aref, Tmob, crefa, ustar, rouse, sqcf, z0k, dks, hdune, qssevr84
      real(kind=dp) :: qsseqcheck, eincheck, eincheck2
      real(kind=dp) :: D50, D90, dstar, ag, sag, vonkar, ee, rhomean, rhosed, sqsgd50, temp, vismol, c1, c2
      real(kind=dp) :: Accr, rhodelta, wschk, ff, hf, df, D50a, hska

      integer :: j, i, mout, nx = 4

      ag = 9.81_dp
      sag = sqrt(ag)
      vonkar = 0.41_dp
      ee = exp(1.0_dp)

      ucur = 1.0_dp ! depth avaraged flow velocity
      ueff = ucur ! effective velocity, possibly plus wave contribution

      call newfil(mout, 'rvrcheck.xyz')
      write (mout, *) ' Depth , D50   , Refcon   , Qsc Numerical, Qsc vR84_D50 ' ! QscNumerical/Refcon, Tau'

      hska = 1.5_dp
      ff = 1.3_dp
      hf = 1.0_dp / (hska * ff**(nx - 1))
      D50a = 0.000062_dp
      df = 1.0_dp / (D50a * ff**(nx - 1))
      do i = 1, nx
         !D50 = D50a*ff**(i-1)
         if (i == 1) then
            D50 = 0.000062
         end if
         if (i == 2) then
            D50 = 0.0002
         end if
         if (i == 3) then
            D50 = 0.0006
         end if
         if (i == 4) then
            D50 = 0.002
         end if
         do j = 1, nx
            ! hsk = hska*ff**(j-1)
            if (j == 1) then
               hsk = 1.0_dp
            end if
            if (j == 2) then
               hsk = 5.0_dp
            end if
            if (j == 3) then
               hsk = 20.0_dp
            end if
            if (j == 4) then
               hsk = 40.0_dp
            end if

            d90 = 2.0_dp * d50 ! grainsize
            dks = 3.0_dp * d90 ! nikuradse
            z0k = dks / 30.0_dp ! z0
            sqcf = vonkar / log(hsk / (ee * z0k)) ! sqrt(g)/C  ( )
            ustar = sqcf * Ucur ! ustar
            hdune = 0.0_dp
            aref = max(dks, hdune) ! reference height is max of (nikuradse and half dune height) (m)

            rhosed = 2650.0_dp
            rhomean = 1000.0_dp
            rhodelta = (rhosed - rhomean) / rhomean ! rhodelta = (s-1), s=rhosed/rhomean
            sqsgd50 = sqrt(rhodelta * ag * D50)
            Temp = 20.0_dp
            vismol = 4.0_dp / (20.0_dp + Temp) * 1.0e-5_dp ! Van rijn, 1993
            Sster = D50 / (4 * vismol) * sqsgd50
            c1 = 1.06_dp * tanh(0.064_dp * Sster * exp(-7.5_dp / Sster**2))
            c2 = 0.22_dp * tanh(2.34_dp * Sster**(-1.18_dp) * exp(-0.0064_dp * Sster**2))
            wster = c1 + c2 * Sster
            ws = wster * sqsgd50

            dstar = D50 * ((rhodelta * ag) / (vismol * vismol))**(1.0_dp / 3.0_dp)
            Wschk = 16.17_dp * D50 * D50 / (1.80e-5_dp + sqrt(12.12 * D50 * D50 * D50)) ! Ferguson,Church 2006) Wikipedia sand fall velocity

            if (D50 <= 0.0005_dp) then ! calculate treshold velocity Ucr, formula (12)
               Accr = 0.19_dp * D50**0.1_dp
            else ! if(D50<0.05d0) then                                       ! Dano see what happens with coarse material
               Accr = 8.50_dp * D50**0.6_dp
            end if
            Ucr = Accr * log10(4.0_dp * hsk / D90)

            Pmob = (Ueff - Ucr) / Ucr
            Tmob = (Ueff * Ueff - Ucr * Ucr) / (Ucr * Ucr) ! Mobility parameter T ( )

            if (Tmob > 0.0_dp) then

               rouse = ws / (vonkar * ustar)
               !deltaa = aref/hsk
               !call einstein_garcia(deltaa,rouse,dj1,dj2)                      ! einstein integrals following garcia 2008
               !garciaeinstein = dj1*log(hsk/z0k) + dj2                       ! garcia 2008(2-219) ( )
               !garciaeinstein = max(0d0,garciaeinstein)
               crefa = 0.015_dp * (D50 / aref) * (Tmob**1.5_dp) / (Dstar**0.3_dp) ! dimensionless reference concentration ( ), (book vRijn 1993, (7.3.31) )
               if (crefa > 0.65_dp) then
                  crefa = 0.65_dp ! max ref concentration ( )               or (book Garcia 2008, (2-226) )
               end if
               !qsseq = (crefa*ustar*hsk/vonkar)*garciaeinstein               ! equilibrium suspended transport, ( ). (m/s) . (m) =  ( m2/s) )
               !sseq  = qsseq/ ( max(ucur,1d-2)*hsk )                         ! ( ) dimensionless equilibrium suspended sediment concentration

               call check_einstein_garcia(aref, hsk, z0k, rouse, eincheck) ! numerical check einstein integrals slow, height is already in eincheck

               call check_einstein_garcia2(aref, hsk, z0k, rouse, eincheck2) ! numerical check einstein integrals fast, height is already in eincheck

               qsseqcheck = (crefa * ustar / vonkar) * eincheck ! (conclusion : inaccuracy of einstein_garcia is about 10-20 % => improve if have time )

               qssevr84 = 0.012_dp * Ucur * D50 * Pmob**2.4_dp * Dstar**(-0.6_dp) ! boek vanrijn (7.3.46), or 2007b

               write (mout, '(7F12.8)') hsk, D50, crefa, qsseqcheck, qssevr84 !,  qsseqcheck/ crefa, rhomean*ustar**2

               !vr84rel    = qssevr84 / qsseqcheck

               !qsseqrel   = qsseq    / qsseqcheck

               !caver = crefa*dj1                                             ! just checking
               !if (caver > 0) then
               !   effic = qsseq / (caver*ucur*hsk)                              ! just checking
               !   bav   = crefa /  caver                                        ! just checking
               !   botsu = sbeq  /   sseq                                        ! just checking
               !endif

            end if

         end do
      end do

      call doclose(mout)

   end subroutine checksuspended_transport

end module m_checksuspended_transport
