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

      subroutine agr_poi_reg( ipnt  , ipnt_b, input_hyd, output_hyd, ipnt_q)

      ! function : set agggregation pointer for the pointers, regular case

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

      integer              :: iq, iq_n, iq2
      integer              :: iqstart, iqstop
      integer              :: ip1,ip2,ip3,ip4
      integer              :: noq_o
      integer              :: noq1_o
      integer              :: noq2_o
      integer              :: noq3_o
      integer              :: noq_n
      integer              :: noq1_n
      integer              :: noq2_n
      integer              :: noq3_n
      logical              :: found

      ! some init

      noq1_o = input_hyd%num_exchanges_u_dir
      noq2_o = input_hyd%num_exchanges_v_dir
      noq3_o = input_hyd%num_exchanges_z_dir
      noq_o  = input_hyd%num_exchanges

      noq1_n = output_hyd%num_cells
      noq2_n = output_hyd%num_cells
      noq3_n = output_hyd%num_cells - output_hyd%nosegl
      noq_n  = noq1_n + noq2_n + noq3_n

      output_hyd%num_exchanges_u_dir = noq1_n
      output_hyd%num_exchanges_v_dir = noq2_n
      output_hyd%num_exchanges_z_dir = noq3_n
      output_hyd%num_exchanges  = noq_n

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
                  iq_n = ip1
               else
                  iq_n = ip2 - 1
               endif
            elseif ( iq .le. noq1_o+noq2_o ) then
               if ( ip1 .gt. 0 ) then
                  iq_n = ip1 + noq1_n
               else
                  iq_n = ip2 - output_hyd%num_rows + noq1_n
               endif
            else
               if ( ip1 .gt. 0 ) then
                  iq_n = ip1 + noq1_n + noq2_n
               else
                  iq_n = ip2 - output_hyd%num_rows*output_hyd%num_columns + noq1_n + noq2_n
               endif
            endif


            ipnt_q(iq)   = iq_n
            output_hyd%ipoint(1,iq_n) = ip1
            output_hyd%ipoint(2,iq_n) = ip2
            output_hyd%ipoint(3,iq_n) = ip3
            output_hyd%ipoint(4,iq_n) = ip4
         else
            ipnt_q(iq)   = 0
         endif

      enddo

      ! look for +1 and -1 pointers

      do iq = 1 , noq_n
         ip1   = output_hyd%ipoint(1,iq)
         ip2   = output_hyd%ipoint(2,iq)
         ip3   = output_hyd%ipoint(3,iq)
         ip4   = output_hyd%ipoint(4,iq)
         if ( ip3 .eq. ip1 ) then
            found = .false.
            if ( iq .gt. noq1_n+noq2_n ) then
               iqstart = noq1_n + noq2_n + 1
               iqstop  = noq_n
            elseif ( iq .gt. noq1_n ) then
               iqstart = noq1_n + 1
               iqstop  = noq1_n + noq2_n
            else
               iqstart = 1
               iqstop  = noq1_n
            endif
            do iq2 = iq - 1 , iqstart , -1
               if ( output_hyd%ipoint(2,iq2) .eq. ip1 ) then
                  ip3 = output_hyd%ipoint(1,iq2)
                  found = .true.
                  exit
               endif
            enddo
            if ( .not. found ) then
               do iq2 = iq + 1 , iqstop
                  if ( output_hyd%ipoint(2,iq2) .eq. ip1 ) then
                     ip3 = output_hyd%ipoint(1,iq2)
                     found = .true.
                     exit
                  endif
               enddo
            endif
            if ( found ) then
               output_hyd%ipoint(3,iq) = ip3
            else
               output_hyd%ipoint(3,iq) = 0
            endif
         endif
         if ( ip4 .eq. ip2 ) then
            found = .false.
            if ( iq .gt. noq1_n+noq2_n ) then
               iqstart = noq1_n + noq2_n + 1
               iqstop  = noq_n
            elseif ( iq .gt. noq1_n ) then
               iqstart = noq1_n + 1
               iqstop  = noq1_n + noq2_n
            else
               iqstart = 1
               iqstop  = noq1_n
            endif
            do iq2 = iq + 1 , iqstop
               if ( output_hyd%ipoint(1,iq2) .eq. ip2 ) then
                  ip4 = output_hyd%ipoint(2,iq2)
                  found = .true.
                  exit
               endif
            enddo
            if ( .not. found ) then
               do iq2 = iq - 1 , iqstart , -1
                  if ( output_hyd%ipoint(1,iq2) .eq. ip2 ) then
                     ip4 = output_hyd%ipoint(2,iq2)
                     found = .true.
                     exit
                  endif
               enddo
            endif
            if ( found ) then
               output_hyd%ipoint(4,iq) = ip4
            else
               output_hyd%ipoint(4,iq) = 0
            endif
         endif

      enddo

      return
      end
