!!  Copyright (C)  Stichting Deltares, 2021-2024.
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

      subroutine agr_poi_exp( ipnt  , ipnt_b, input_hyd, output_hyd, ipnt_q)

      ! function : set agggregation pointer for the pointers, expand case

      ! (c) DELFT HYDRAULICS

      ! global declarations

      use m_hydmod
      implicit none

      ! declaration of the arguments

      integer              :: ipnt(*)                              ! aggregation pointer
      integer              :: ipnt_b(*)                            ! aggregation pointer boundaries
      type(t_hydrodynamics)          :: input_hyd                            ! description of the input hydrodynamics
      type(t_hydrodynamics)          :: output_hyd                           ! description of the output hydrodynamics
      integer              :: ipnt_q(*)                            ! new exchange pointer

      ! local declarations

      integer              :: iq, iq_n
      integer              :: ip1,ip2,ip3,ip4
      integer              :: noq_o
      integer              :: noq1_o
      integer              :: noq2_o
      integer              :: noq3_o
      integer              :: noq_n
      integer              :: noq1_n
      integer              :: noq2_n
      integer              :: noq3_n

      integer              :: iq_n_prev
      integer              :: ip1_prev
      integer              :: ip2_prev
      integer              :: ip3_prev
      integer              :: ip4_prev
      integer              :: iq_prev
      integer              :: iq_n_prev_n

      integer, allocatable :: iq_o(:)
      logical, allocatable :: iq_reset(:)

      ! some init

      noq1_o = input_hyd%noq1
      noq2_o = input_hyd%noq2
      noq3_o = input_hyd%noq3
      noq_o  = input_hyd%noq

      noq1_n = output_hyd%noseg
      noq2_n = output_hyd%noseg
      noq3_n = output_hyd%noseg - output_hyd%nosegl
      noq_n  = noq1_n + noq2_n + noq3_n

      output_hyd%noq1 = noq1_n
      output_hyd%noq2 = noq2_n
      output_hyd%noq3 = noq3_n
      output_hyd%noq  = noq_n

      allocate(iq_o(noq_n))
      allocate(iq_reset(noq_n))
      iq_o     = 0
      iq_reset = .false.

      ! loop over the original pointers

      do iq = 1 , noq_o
         ip1   = input_hyd%ipoint(1,iq)
         ip2   = input_hyd%ipoint(2,iq)
         ip3   = input_hyd%ipoint(3,iq)
         ip4   = input_hyd%ipoint(4,iq)
         if ( ip1 .gt. 0 ) then
            ip1 = ipnt(ip1)
         elseif ( ip1 .lt. 0 ) then
            ip1 = -ipnt_b(-ip1)
         endif
         if ( ip2 .gt. 0 ) then
            ip2 = ipnt(ip2)
         elseif ( ip2 .lt. 0 ) then
            ip2 = -ipnt_b(-ip2)
         endif
         if ( ip3 .gt. 0 ) then
            ip3 = ipnt(ip3)
         elseif ( ip3 .lt. 0 ) then
            ip3 = -ipnt_b(-ip3)
         endif
         if ( ip4 .gt. 0 ) then
            ip4 = ipnt(ip4)
         elseif ( ip4 .lt. 0 ) then
            ip4 = -ipnt_b(-ip4)
         endif

         ! if ip1 equals ip2 then the exchange is not used anymore

         if ( ip1 .ne. ip2 .and. .not. ( ip1 .eq. 0 .or. ip2 .eq. 0 ) .and. .not. (ip1 .lt. 0 .and. ip2 .lt. 0 ) ) then

            ! the exchange number can be determined from the new segment numbers

            if ( iq .le. noq1_o ) then
               if ( ip1 .gt. 0 ) then
                  if ( output_hyd%ipoint(1,ip1) .eq. 0 ) then
                     iq_n = ip1
                  else                                                    ! ddcoupling exception
                     iq_n = ip2 - 1
                  endif
               else
                  iq_n = ip2 - 1
               endif
            elseif ( iq .le. noq1_o+noq2_o ) then
               if ( ip1 .gt. 0 ) then
                  if ( output_hyd%ipoint(1,ip1+noq1_n) .eq. 0 ) then
                     iq_n = ip1 + noq1_n
                  else

                     ! ddcoupling exception

                     iq_n = ip2 - output_hyd%nmax + noq1_n

                     ! also reset previous point in this dd coupling

                     iq_n_prev = ip1+noq1_n
                     ip1_prev  = output_hyd%ipoint(1,iq_n_prev)
                     ip2_prev  = output_hyd%ipoint(2,iq_n_prev)
                     ip3_prev  = output_hyd%ipoint(3,iq_n_prev)
                     ip4_prev  = output_hyd%ipoint(4,iq_n_prev)
                     iq_prev   = iq_o(iq_n_prev)

                     iq_n_prev_n                      = ip2_prev - output_hyd%nmax + noq1_n
                     ipnt_q(iq_prev)                  = iq_n_prev_n
                     output_hyd%ipoint(1,iq_n_prev_n) = ip1_prev
                     output_hyd%ipoint(2,iq_n_prev_n) = ip2_prev
                     output_hyd%ipoint(3,iq_n_prev_n) = ip3_prev
                     output_hyd%ipoint(4,iq_n_prev_n) = ip4_prev
                     iq_o(iq_n_prev_n)                = iq_prev
                     iq_reset(iq_n_prev)              = .true.

                  endif
               else
                  iq_n = ip2 - output_hyd%nmax + noq1_n
               endif
            else
               if ( ip1 .gt. 0 ) then
                  iq_n = ip1 + noq1_n + noq2_n
               else
                  iq_n = ip2 - output_hyd%nmax*output_hyd%mmax + noq1_n + noq2_n
               endif
            endif

            ipnt_q(iq)   = iq_n
            output_hyd%ipoint(1,iq_n) = ip1
            output_hyd%ipoint(2,iq_n) = ip2
            output_hyd%ipoint(3,iq_n) = ip3
            output_hyd%ipoint(4,iq_n) = ip4
            iq_o(iq_n)   = iq

         else
            ipnt_q(iq)   = 0
         endif

      enddo

      ! reset the not used dd pointer

      do iq_n = 1 , noq_n
         if ( iq_reset(iq_n) ) then
            output_hyd%ipoint(1,iq_n) = 0
            output_hyd%ipoint(2,iq_n) = 0
            output_hyd%ipoint(3,iq_n) = 0
            output_hyd%ipoint(4,iq_n) = 0
         endif
      enddo

      ! set the q pointer of the not used dd side for part anyhow

      do iq = 1 , noq_o
         ip1   = input_hyd%ipoint(1,iq)
         ip2   = input_hyd%ipoint(2,iq)
         ip3   = input_hyd%ipoint(3,iq)
         ip4   = input_hyd%ipoint(4,iq)
         if ( ip1 .gt. 0 ) then
            ip1 = ipnt(ip1)
         elseif ( ip1 .lt. 0 ) then
            ip1 = -ipnt_b(-ip1)
         endif
         if ( ip2 .gt. 0 ) then
            ip2 = ipnt(ip2)
         elseif ( ip2 .lt. 0 ) then
            ip2 = -ipnt_b(-ip2)
         endif
         if ( ip3 .gt. 0 ) then
            ip3 = ipnt(ip3)
         elseif ( ip3 .lt. 0 ) then
            ip3 = -ipnt_b(-ip3)
         endif
         if ( ip4 .gt. 0 ) then
            ip4 = ipnt(ip4)
         elseif ( ip4 .lt. 0 ) then
            ip4 = -ipnt_b(-ip4)
         endif

         ! if ip1 and ip2 eq 0 then this probably a non used dd exchange

         if ( ip1 .eq. 0 .and. ip2 .eq. 0 ) then

            ! look for the -1 or +1 pointer for the location of the exchange

            iq_n = 0
            if ( iq .le. noq1_o ) then
               if ( ip3 .gt. 0 ) then
                  iq_n = ip3 + 1
               elseif ( ip4 .gt. 0 ) then
                  iq_n = ip4 - 2
               endif
            elseif ( iq .le. noq1_o+noq2_o ) then
               if ( ip3 .gt. 0 ) then
                  iq_n = ip3 + output_hyd%nmax + noq1_n
               else
                  iq_n = ip4 - 2*output_hyd%nmax + noq1_n
               endif
            endif

            ipnt_q(iq)   = iq_n

         endif

      enddo
      return
      end
