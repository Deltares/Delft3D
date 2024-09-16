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

 subroutine setbaptist()
    use m_flow
    use m_flowgeom
    use m_get_cz
    implicit none
    integer :: L, k1, k2
    double precision :: ap, Cz, Czb, Czr, rnL, diaL, stemhL, gamhg, Cda, areastem, umag, fac, facL, Cdaleaf

    do L = 1, lnx
       k1 = ln(1, L); k2 = ln(2, L)

       rnL = 0.5d0 * (rnveg(k1) + rnveg(k2))

       if (hu(L) > 0 .and. rnL > densvegminbap) then ! overwrite cfuhi on veg locations with 2D Baptist
          if (jaBaptist <= 2) then ! compute Baptist on board
             call getcz(hu(L), frcu(L), ifrcutp(L), Czb, L) ! standard Chezy coeff
             if (diaveg(k1) > 0 .and. diaveg(k2) > 0) then
                diaL = 0.5d0 * (diaveg(k1) + diaveg(k2))
             else
                diaL = max(diaveg(k1), diaveg(k2))
             end if
             if (stemheight(k1) > 0 .and. stemheight(k2) > 0) then
                stemhL = 0.5d0 * (stemheight(k1) + stemheight(k2))
             else
                stemhL = max(stemheight(k1), stemheight(k2))
             end if
             stemhL = min(stemhL, hu(L))
             areastem = diaL * stemhL
             if (jaCdvegsp == 1) then
                if (Cdvegsp(k1) > 0 .and. Cdvegsp(k2) > 0) then
                   Cdveg = 0.5d0 * (Cdvegsp(k1) + Cdvegsp(k2))
                else
                   Cdveg = max(Cdvegsp(k1), Cdvegsp(k2))
                end if
             end if
             Cda = Cdveg * areastem
             if (uchistem > 0d0 .and. expchistem < 0d0) then
                umag = sqrt(u1(L)**2 + v(L)**2)
                if (umag > 0d0) then
                   fac = (umag / uchistem)**expchistem
                else
                   fac = 1d0
                end if
                Cda = Cda * fac
                !if ( leafarea(k1) > 0 .and. leafarea(k2) > 0) then
                !   arealeaf = 0.5d0*( leafarea(k1) + leafarea(k2) )
                !else
                !   arealeaf = max( leafarea(k1), leafarea(k2) )
                !endif
                if (uchileaf > 0d0 .and. expchileaf < 0d0) then
                   if (umag > 0d0) then
                      facL = (umag / uchileaf)**expchileaf
                   else
                      facL = 1d0
                   end if
                   Cdaleaf = Cdleaf * arealeaf * facL
                   Cda = Cda + Cdaleaf
                end if
             end if

             gamhg = 0.5d0 * Cda * rnL / ag ! gamma*h/g
             ap = gamhg + 1d0 / (Czb * Czb)
             ! ap     = gamhg + (1d0/Czb*Czb)                    ! old=wrong
             Czr = sqrt(1d0 / ap)

             if (stemhL < hu(L) - 0.01d0) then
                Czr = Czr + (sag / vonkar) * log(hu(L) / stemhL) ! resulting Baptist Chezy
             end if
             if (jaBaptist == 1) then
                cfuhi(L) = ag / (Czr * Czr * hu(L)) ! use as is, wrong morpho ?
             else if (jaBaptist == 2) then
                Cz = Czr * sqrt(1d0 + gamhg * Czb * Czb)
                cfuhi(L) = ag / (Cz * Cz * hu(L)) ! better for morfo
                alfav(L) = ag * (1d0 / (Czr * Czr) - 1d0 / (Cz * Cz)) / hu(L)
             end if
          else if (jaBaptist == 3) then ! by biologists through Python
             cfuhi(L) = cfuveg(L) / hu(L)
             alfav(L) = alfaveg(L) / hu(L)
          end if
       end if
    end do

 end subroutine setbaptist
