!!  Copyright (C)  Stichting Deltares, 2012-2026.
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
module m_sedim
   use m_waq_precision

   implicit none

contains

   subroutine sedim(process_space_real, fl, ipoint, increm, num_cells, &
                    noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
                    num_exchanges_z_dir, num_exchanges_bottom_dir)
      !>\file
      !>       Sedimentation routine used for IMx
      !
      !     Description of the module :
      !
      !        General water quality module for DELWAQ:
      !        SEDIMENTATION FORMULATIONS
      !        MODULE VALID FOR IM, IM2, IM3
      !

      use m_extract_waq_attribute
      use bottomSet !  Module with definition of the waterbottom segments

      real(kind=real_wp) :: process_space_real(*), fl(*)
      integer(kind=int_wp) :: ipoint(*), increm(*), num_cells, noflux, iexpnt(4, *), &
                              iknmrk(*), num_exchanges_u_dir, num_exchanges_v_dir, &
                              num_exchanges_z_dir, num_exchanges_bottom_dir

      real(kind=real_wp) :: mindep, minde2, depth, depth2

      real(kind=real_wp) :: psedmin

      real(kind=real_wp), parameter :: seconds_per_day = 86400.0

      integer :: ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8, ip9, ip10, &
                 ip11, ip12, ip13, ip14, ip15, ip16, ip17, &
                 in1, in2, in3, in4, in5, in6, in7, in8, in9, in10, &
                 in11, in12, in13, in14, in15, in16, in17
      integer :: ifrom, ito, ip, ipv, ipn, ipq, iq, iseg, iflux, ikmrkv, ikmrkn, iwa1, iwa2, ikmrk1, ikmrk2, ik, iwater
      real(kind=real_wp) :: conc, zersed, vsed, tau, tcrsed, delt, psed, alpha, p, pmax, maxsed, potsed
      real(kind=real_wp) :: flowrate, volume, surf

      ip1 = ipoint(1) ! Concentration inorganic matter
      ip2 = ipoint(2) ! Zeroth-order flux -- at all useful?
      ip3 = ipoint(3) ! Sedimentation velocity
      ip4 = ipoint(4) ! Bottom shear stress
      ip5 = ipoint(5) ! Critical shear stress for sedimentation
      ip6 = ipoint(6) ! Depth of the segments
      ip7 = ipoint(7) ! Time step (for limiting the deposition flux)
      ip8 = ipoint(8) ! Minimum depth for sedimentation/resuspension -- obsolete!
      ip9 = ipoint(9) ! Fraction going directly to layer S2
      ip10 = ipoint(10) ! Fraction total inorganic matter (TIM) in layer S2
      ip11 = ipoint(11) ! Maximum allowable fraction (TIM) in layer S2
      ip12 = ipoint(12) ! Minimum sedimentation probability (force sedimentation) -- useful?
      ip13 = ipoint(13) ! (exchange) sedimentation velocity per exchange
      ip14 = ipoint(14) ! (output) Sedimentation probability (used for adsorbed substances)
      ip15 = ipoint(15) ! (output) Sedimentation flux to layer S1
      ip16 = ipoint(16) ! (output) Sedimentation flux to layer S2
      ip17 = ipoint(17) ! (additional velocity) Sedimentation velocity

      in1 = increm(1)
      in2 = increm(2)
      in3 = increm(3)
      in4 = increm(4)
      in5 = increm(5)
      in6 = increm(6)
      in7 = increm(7)
      in8 = increm(8)
      in9 = increm(9)
      in10 = increm(10)
      in11 = increm(11)
      in12 = increm(12)
      in13 = increm(13)
      in14 = increm(14)
      in15 = increm(15)
      in16 = increm(16)
      in17 = increm(17)

      iflux = 0
      do iseg = 1, num_cells

         !     zero output
         process_space_real(ip14) = 0.0
         process_space_real(ip15) = 0.0
         process_space_real(ip16) = 0.0

         !     sedimentation towards the bottom
         call extract_waq_attribute(1, iknmrk(iseg), ikmrk1)
         if (ikmrk1 == 1) then
            call extract_waq_attribute(2, iknmrk(iseg), ikmrk2)
            if ((ikmrk2 == 0) .or. (ikmrk2 == 3)) then
               !
               conc = max(0.0, process_space_real(ip1))
               zersed = process_space_real(ip2)
               vsed = max(0.0, process_space_real(ip3)) ! Avoid inadvertent source if VSED negative (Delft3D-35562)
               tau = process_space_real(ip4)
               tcrsed = process_space_real(ip5)
               depth = process_space_real(ip6)
               delt = process_space_real(ip7)
               mindep = process_space_real(ip8)
               alpha = process_space_real(ip9)
               p = process_space_real(ip10)
               pmax = process_space_real(ip11)
               psedmin = process_space_real(ip12)

               !***********************************************************************
               !**** Processes connected to the SEDIMENTATION
               !***********************************************************************

               !     if fraction IM1 in second layer P > PMAX then ALPHA = 0 meaning no sedimentations towards S2
               if (p >= pmax) then
                  alpha = 0.0
               end if

               !     Calculate sedimenation probability
               if (tau == -1.0) then
                  psed = 1.0
               elseif (tcrsed < 1e-20) then
                  psed = 0.0
               else
                  !         comparison with critical shear stress
                  psed = max(0.0, (1.0 - tau / tcrsed))
               end if
               psed = max(psedmin, psed)

               !     Calculate potential sedimentation fluxes
               !     No sedimentation when depth below min depth
               if (depth < mindep) then
                  maxsed = 0.0
                  fl(1 + iflux) = 0.0
                  fl(2 + iflux) = 0.0
               else
                  potsed = zersed + (vsed * conc) * psed

                  !        limit sedimentation to available mass
                  maxsed = min(potsed, conc / delt * depth)

                  !        convert sedimentation to flux
                  fl(1 + iflux) = maxsed * (1.-alpha) / depth
                  fl(2 + iflux) = maxsed * alpha / depth
               end if

               !     Output of calculated sedimentation rate
               process_space_real(ip14) = psed
               process_space_real(ip15) = maxsed * (1.-alpha)
               process_space_real(ip16) = maxsed * alpha
               !
            end if
         end if
         !
         iflux = iflux + noflux
         ip1 = ip1 + in1
         ip2 = ip2 + in2
         ip3 = ip3 + in3
         ip4 = ip4 + in4
         ip5 = ip5 + in5
         ip6 = ip6 + in6
         ip7 = ip7 + in7
         ip8 = ip8 + in8
         ip9 = ip9 + in9
         ip10 = ip10 + in10
         ip11 = ip11 + in11
         ip12 = ip12 + in12
         ip14 = ip14 + in14
         ip15 = ip15 + in15
         ip16 = ip16 + in16
      end do
      !
      ip1 = ipoint(1)
      ip6 = ipoint(6)
      ip8 = ipoint(8)
      ip15 = ipoint(15)

      !.....Exchange loop over the horizontal direction
      do iq = 1, num_exchanges_u_dir + num_exchanges_v_dir

         process_space_real(ip17) = 0.0

         ip17 = ip17 + in17

      end do

      ip13 = ip13 + (num_exchanges_u_dir + num_exchanges_v_dir) * in13

      !.....Exchange loop over the vertical direction
      do iq = num_exchanges_u_dir + num_exchanges_v_dir + 1, num_exchanges_u_dir + num_exchanges_v_dir + num_exchanges_z_dir + num_exchanges_bottom_dir

         ifrom = iexpnt(1, iq)
         ito = iexpnt(2, iq)

         if (ifrom > 0 .and. ito > 0) then

            !           Find first characteristic of from- and to-segments
            call extract_waq_attribute(1, iknmrk(ifrom), ikmrkv)
            call extract_waq_attribute(1, iknmrk(ito), ikmrkn)
            if (ikmrkv == 1 .and. ikmrkn == 3) then

               !               Bottom-water exchange: ZERO FLUX TO ALSO BE ABLE TO USE OLD PDFs
               !               Velocity does not need to be set (happens in TRASED)

               !               maxsed = process_space_real (ip11+(ifrom-1)*in11)
               !               conc   = max (1e-20, process_space_real(ip1+(ifrom-1)*in1) )
               !               process_space_real(ip17) = maxsed/seconds_per_day/conc
               fl(1 + (ifrom - 1) * noflux) = 0.0

            elseif (ikmrkv == 1 .and. ikmrkn == 1) then

               !               Water-water exchange
               !rs             note: sedimentation between water layers: no taucr correction,
               !rs             only conversion from 1/d to 1/s. For the record:
               !rs             scu (s) and aux-timer (d) are therefore fixed!

               depth = process_space_real(ip6 + (ifrom - 1) * in6)
               depth2 = process_space_real(ip6 + (ito - 1) * in6)
               mindep = process_space_real(ip8 + (ifrom - 1) * in8)
               minde2 = process_space_real(ip8 + (ito - 1) * in8)
               if (depth > mindep .and. depth2 > minde2) then
                  process_space_real(ip17) = process_space_real(ip13) / seconds_per_day
               else
                  process_space_real(ip17) = 0.0
               end if
            else
               process_space_real(ip17) = 0.0
            end if

         end if

         ip13 = ip13 + in13
         ip17 = ip17 + in17

      end do

      !     Handle velocity to the delwaq-g bottom

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

      do ik = 1, coll%current_size

         iwa1 = coll%set(ik)%fstwatsed
         iwa2 = coll%set(ik)%lstwatsed

         do iq = iwa1, iwa2
            iwater = iexpnt(1, iq)

            conc = max(0.0, process_space_real(ip1 + (iwater - 1) * in1))
            zersed = process_space_real(ip2 + (iwater - 1) * in2)
            vsed = max(0.0, process_space_real(ip3 + (iwater - 1) * in3))
            tau = process_space_real(ip4 + (iwater - 1) * in4)
            tcrsed = process_space_real(ip5 + (iwater - 1) * in5)
            depth = process_space_real(ip6 + (iwater - 1) * in6)
            delt = process_space_real(ip7 + (iwater - 1) * in7)
            mindep = process_space_real(ip8 + (iwater - 1) * in8)

            !           Calculate sedimenation probability

            if (tau == -1.0) then
               psed = 1.0
            elseif (tcrsed < 1e-20) then
               psed = 0.0
            else
               !               comparison with critical shear stress
               psed = max(0.0, (1.0 - tau / tcrsed))
            end if

            !           Calculate the potential sedimentation fluxes
            !           No sedimentation below a minimum depth

            if (depth < mindep) then
               maxsed = 0.0
            else
               potsed = zersed + (vsed * conc) * psed

               !              sediment maximally the available amount
               maxsed = min(potsed, conc / delt * depth)

            end if

            if (conc > 1.e-10) then
               process_space_real(ip17 + (iq - 1) * in17) = maxsed / seconds_per_day / conc
            end if

         end do
      end do
      !
      return
   end subroutine sedim
end module m_sedim
