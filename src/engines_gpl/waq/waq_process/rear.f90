!!  Copyright (C)  Stichting Deltares, 2012-2025.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.
module m_rear
   use m_waq_precision

   implicit none

contains

   subroutine rear(process_space_real, fl, ipoint, increm, num_cells, &
                   noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
                   num_exchanges_z_dir, num_exchanges_bottom_dir)
      use m_zerome
      use m_logger_helper, only: stop_with_error, get_log_unit_number
      use m_extract_waq_attribute

      !>\file
      !>       Reaeration of carbon dioxide and oxygen

      implicit none

      real(kind=real_wp) :: process_space_real(*), FL(*)
      integer(kind=int_wp) :: ipoint(*), increm(*), num_cells, noflux, &
                              iexpnt(4, *), iknmrk(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir
      !
      !     Local declarations
      !
      integer(kind=int_wp) :: lunrep
      integer(kind=int_wp) :: ifrear, ikmrk1, ikmrk2, iseg, iflux
      integer(kind=int_wp) :: ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8, ip9, ip10, &
                              ip11, ip12, ip13, ip14, ip15, ip16, ip17, ip18, ip19, ip20, &
                              ip21, ip22, ip23, ip24, ip25, ip26, ip27, ip28
      real(kind=real_wp) :: sal, b_enha, sc, sc20, klrear, totdep, &
                            reartc, rearrc, hcrt, veloc, vwind, temp, &
                            oxsat, o2, temp20, tmpcf, depth, fl1, &
                            fcover, maxrrc, rearkl, satperc, minrrc, delt, rain
      real(kind=real_wp) :: a, b1, b2, c1, &
                            c2, d1, d2, d3, d4, d5, &
                            a_o, b_o, c_o, d_o, e_o, &
                            a_co, b_co, c_co, d_co, e_co

      !   PBo3: hard coded coefficients for salt water options Wannikhof
      !     Parameters for Schmidt number calculation (Wanninkhoff and Guerin)
      !     D1-4Os = oxygen Wannikhof (seawater)
      !     D1-4Cs = CO2 Wannikhof (seawater)

      parameter(a_o=1920.4, &
                b_o=-135.6, &
                c_o=5.2122, &
                d_o=-0.10939, &
                e_o=0.00093777, &
                a_co=2116.8, &
                b_co=-136.25, &
                c_co=4.7353, &
                d_co=-0.092307, &
                e_co=0.0007555)

      ip1 = ipoint(1)
      ip2 = ipoint(2)
      ip3 = ipoint(3)
      ip4 = ipoint(4)
      ip5 = ipoint(5)
      ip6 = ipoint(6)
      ip7 = ipoint(7)
      ip8 = ipoint(8)
      ip9 = ipoint(9)
      ip10 = ipoint(10)
      ip11 = ipoint(11)
      ip12 = ipoint(12)
      ip13 = ipoint(13)
      ip14 = ipoint(14)
      ip15 = ipoint(15)
      ip16 = ipoint(16)
      ip17 = ipoint(17)
      ip18 = ipoint(18)
      ip19 = ipoint(19)
      ip20 = ipoint(20)
      ip21 = ipoint(21)
      ip22 = ipoint(22)
      ip23 = ipoint(23)
      ip24 = ipoint(24)
      ip25 = ipoint(25)
      ip26 = ipoint(26)
      ip27 = ipoint(27)
      ip28 = ipoint(28)

      !
      iflux = 0
      do iseg = 1, num_cells
         call extract_waq_attribute(1, iknmrk(iseg), ikmrk1)
         if (ikmrk1 == 1) then

            !         Compute saturation percentage for all layers

            o2 = process_space_real(ip1)
            oxsat = process_space_real(ip10)
            satperc = o2 / oxsat * 100
            process_space_real(ip28) = satperc

            call extract_waq_attribute(2, iknmrk(iseg), ikmrk2)
            if ((ikmrk2 == 0) .or. (ikmrk2 == 1)) then
               !
               depth = process_space_real(ip2)
               temp = process_space_real(ip3)
               veloc = process_space_real(ip4)
               vwind = process_space_real(ip5)
               ifrear = process_space_real(ip6) + 0.5
               rearkl = process_space_real(ip7)
               reartc = process_space_real(ip8)
               delt = process_space_real(ip9)
               sal = process_space_real(ip11)
               totdep = process_space_real(ip12)
               fcover = process_space_real(ip13)
               maxrrc = process_space_real(ip14)
               minrrc = process_space_real(ip15)
               rain = process_space_real(ip16)
               a = process_space_real(ip17)
               b1 = process_space_real(ip18)
               b2 = process_space_real(ip19)
               c1 = process_space_real(ip20)
               c2 = process_space_real(ip21)
               d1 = process_space_real(ip22)
               d2 = process_space_real(ip23)
               d3 = process_space_real(ip24)
               d4 = process_space_real(ip25)
               d5 = process_space_real(ip26)

               if (depth < 1e-30) call zerome('DEPTH in REAR')

               !     JvG, 1 May 2002
               !     Current formulation was not valid for layered schematisations
               !     Correct by using the methodology as follows:
               !     a) compute surface transfer coefficient in m/day per method
               !     b) compute flux by multiplying with (surface) deficit
               !     c) convert to volumetric flux by using the (surface) layer thickness

               select case (ifrear)
               case (0)
                  !
                  !         0. Unscaled user input coefficient in 1/day
                  !
                  rearrc = rearkl * totdep

               case (1)
                  !
                  !         1. User input coefficient in m/day
                  !
                  rearrc = rearkl

               case (2)
                  !
                  !         2. Churchill [1962]
                  !
                  rearrc = 0.0
                  if (veloc > 1e-30) &
                     rearrc = 5.026 * (veloc**0.969) / (totdep**0.673)

               case (3)
                  !
                  !         3. O'Connor - Dobbins [1958]
                  !
                  rearrc = 0.0
                  if (veloc > 1e-30) &
                     rearrc = 3.863 * (veloc**0.5) / (totdep**0.5)

               case (4)
                  !
                  !         4. Scaled version of O'Connor - Dobbins [1958]
                  !
                  rearrc = 0.0
                  if (veloc > 1e-30) &
                     rearrc = 3.863 * (veloc**0.5) / (totdep**0.5) * rearkl

               case (5)
                  !
                  !         5. Owens - Edwards - Gibb [1964]
                  !
                  rearrc = 0.0
                  if (veloc > 1e-30) &
                     rearrc = 5.322 * (veloc**0.67) / (totdep**0.85)

               case (6)
                  !
                  !         6. Langbien - Durum [1967]
                  !
                  rearrc = 0.0
                  if (veloc > 1e-30) &
                     rearrc = 11.23 * veloc / (totdep**0.333)

               case (7)
                  !
                  !         7. Van Pagee[1978] and Delvigne [1980]
                  !
                  if (veloc > 1e-30) then
                     rearrc = (rearkl * 0.065 * vwind**2 + &
                               3.86 * sqrt(veloc / totdep))
                  else
                     rearrc = rearkl * 0.065 * vwind**2
                  end if

               case (8)
                  !
                  !         8. Thackston - Krenkel [1966]
                  !
                  call get_log_unit_number(lunrep)
                  write (lunrep, *) &
                     ' Reaeration formula 8 has not been implemented'
                  write (*, *) ' Reaeration formula 8 has not been implemented'
                  call stop_with_error()

               case (9)
                  !
                  !         9. DBS
                  !
                  rearrc = (0.30 + rearkl * 0.028 * vwind**2)

               case (10)
                  !
                  !        10. Wanninkhof Oxygen
                  !
                  if (sal > 5.0) then
                     sc = a_o + b_o * temp + c_o * temp**2 + d_o * temp**3 + e_o * temp**4
                     sc20 = a_o + b_o * 20.0 + c_o * 20.0**2 + d_o * 20.0**3 + e_o * 20.0**4
                  else
                     sc = d1 + d2 * temp + d3 * temp**2 + d4 * temp**3 + d5 * temp**4
                     sc20 = d1 + d2 * 20.0 + d3 * 20.0**2 + d4 * 20.0**3 + d5 * 20.0**4
                  end if
                  klrear = 0.31 * vwind**2 * (sc / sc20)**(-0.5) * 24./100.
                  rearrc = klrear

               case (11)
                  !
                  !        11. Wanninkhof CO2
                  !
                  if (sal > 5.0) then
                     sc = a_co + b_co * temp + c_co * temp**2 + d_co * temp**3 + e_co * temp**4
                     sc20 = a_co + b_co * 20.0 + c_co * 20.0**2 + d_co * 20.0**3 + e_co * 20.0**4
                  else
                     sc = d1 + d2 * temp + d3 * temp**2 + d4 * temp**3 + d5 * temp**4
                     sc20 = d1 + d2 * 20.0 + d3 * 20.0**2 + d4 * 20.0**3 + d5 * 20.0**4
                  end if
                  b_enha = 2.5 * (.5246 + 1.6256e-2 * temp + 4.9946e-4 * temp**2)
                  klrear = (b_enha + 0.31 * vwind**2) * (sc / sc20)**(-0.5) * 24./100.
                  rearrc = klrear

               case (12)

                  !     Note this option is not included in the process documentation!
                  !
                  !         12. Hybride formulation using O'Connor - Dobbins [1958]
                  !             and Owens - Edwards - Gibb [1964]
                  !
                  rearrc = 0.0
                  hcrt = 3.93 / 5.32 * totdep**0.35
                  if (veloc > 1e-30) then
                     if (veloc < hcrt**6) then
                        rearrc = 3.93 * (veloc**0.5) / (totdep**0.5)
                     else
                        rearrc = 5.32 * (veloc**0.67) / (totdep**0.85)
                     end if
                  end if
                  rearrc = max(minrrc, rearrc)

               case (13)
                  !
                  !        13. Guerin O2  - only fresh water
                  !            Guerin CO2 - only fresh water
                  !            Guerin CH4 - only fresh water
                  !
                  sc = d1 + d2 * temp + d3 * temp**2 + d4 * temp**3 + d5 * temp**4
                  sc20 = d1 + d2 * 20.0 + d3 * 20.0**2 + d4 * 20.0**3 + d5 * 20.0**4

                  klrear = (a * exp(b1 * vwind**b2) + c1 * rain**c2) * (sc / sc20)**(-0.67)
                  rearrc = klrear

                  reartc = 1.0
                  !
               case default
                  call get_log_unit_number(lunrep)
                  write (lunrep, *) ' Invalid option for reaeration formula'
                  write (*, *) ' Invalid option for reaeration formula'
                  call stop_with_error()
               end select

               process_space_real(IP27) = rearrc / depth

               !     Calculation of rearation flux ( M.L-1.DAY)
               !     negatieve zuurstof wordt 0 gemaakt i.v.m. deficiet berekening!
               !     Wanninkhof, don't use temperature dependency

               o2 = max(o2, 0.0)
               if (ifrear == 10 .or. ifrear == 11) then
                  fl1 = rearrc * (oxsat - o2) / depth
               else
                  temp20 = temp - 20.0
                  tmpcf = reartc**temp20
                  if (rearrc <= maxrrc) then
                     rearrc = rearrc * tmpcf
                  end if
                  fl1 = min(1.0 / delt, rearrc * (1 - fcover) / depth) * (oxsat - o2)
               end if

               !     Limitation of FL(1) to amount of oxygen present
               if (fl1 < 0.0) then
                  fl1 = max(-1.*o2 / delt, fl1)
               end if
               fl(1 + iflux) = fl1

            end if
         end if
         !
         iflux = iflux + noflux
         ip1 = ip1 + increm(1)
         ip2 = ip2 + increm(2)
         ip3 = ip3 + increm(3)
         ip4 = ip4 + increm(4)
         ip5 = ip5 + increm(5)
         ip6 = ip6 + increm(6)
         ip7 = ip7 + increm(7)
         ip8 = ip8 + increm(8)
         ip9 = ip9 + increm(9)
         ip10 = ip10 + increm(10)
         ip11 = ip11 + increm(11)
         ip12 = ip12 + increm(12)
         ip13 = ip13 + increm(13)
         ip14 = ip14 + increm(14)
         ip15 = ip15 + increm(15)
         ip16 = ip16 + increm(16)
         ip17 = ip17 + increm(17)
         ip18 = ip18 + increm(18)
         ip19 = ip19 + increm(19)
         ip20 = ip20 + increm(20)
         ip21 = ip21 + increm(21)
         ip22 = ip22 + increm(22)
         ip23 = ip23 + increm(23)
         ip24 = ip24 + increm(24)
         ip25 = ip25 + increm(25)
         ip26 = ip26 + increm(26)
         ip27 = ip27 + increm(27)
         ip28 = ip28 + increm(28)
         !
      end do
      !
      return
      !
   end

end module m_rear
