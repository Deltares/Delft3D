!!  Copyright (C)  Stichting Deltares, 2021-2025.
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

      subroutine agr_poi( ipnt  , noq_o , noq1_o  , noq2_o, noq3_o, & 
                         ip_o  , noq_n , noq1_n  , noq2_n, noq3_n, & 
                         ip_n  , ipnt_q, ipnt_b  )

      implicit none

      integer           :: ipnt(*)        ! aggregation pointer
      integer           :: noq_o          !
      integer           :: noq1_o
      integer           :: noq2_o
      integer           :: noq3_o
      integer           :: ip_o(4,noq_o)  ! original exchange pointer
      integer           :: noq_n          !
      integer           :: noq1_n
      integer           :: noq2_n
      integer           :: noq3_n
      integer           :: ip_n(4,noq_o)  ! new exchange pointer
      integer           :: ipnt_q(noq_o)  ! new exchange pointer
      integer           :: ipnt_b(*)      ! aggregation pointer boundaries

      ! local declarations

      logical           :: found
      integer, dimension(:), allocatable   :: iq1selectno, iq2selectno
      integer, dimension(:,:), allocatable :: iq1select, iq2select
      integer           :: num_cells, num_boundary_conditions, ips, ip1, ip2, ip3, ip4, iq, iq1, iq2, iq_n, iq_o
      integer           :: maxexchanges, iqstop, iqstart


      ! some init

      noq_n  = 0
      noq1_n = 0
      noq2_n = 0
      noq3_n = 0

      !
      ! Determine the "lookup" table for accelerating the procedure to eliminate
      ! duplicate exchanges
      !
      ip1   = maxval(ip_o)
      ip2   = -minval(ip_o)
      num_cells = maxval(ipnt(1:ip1))
      if (ip2>0) then
         num_boundary_conditions = maxval(ipnt_b(1:ip2))
      else
         num_boundary_conditions = 0
      endif

      allocate( iq1selectno(-num_boundary_conditions:num_cells), iq2selectno(-num_boundary_conditions:num_cells) )

      iq1selectno = 0
      do iq = 1, noq_o
         ip1 = ip_o(1,iq)

         if ( ip1 .gt. 0 ) then
            ip1 = ipnt(ip1)
            ip2 = ip_o(2,iq)
            if ( ip2 .gt. 0 ) then
               ip2 = ipnt(ip2)
            end if
            if ( ip2 .eq. 0 .or. ip2 .eq. ip1) then
               ip1 = 0
            end if
         elseif ( ip1 .lt. 0 ) then
            ip1 = -ipnt_b(-ip1)
         endif

         if ( ip1 /= 0 ) then
            iq1selectno(ip1)  = iq1selectno(ip1) + 1
         endif
         if ( ip2 /= 0 ) then
            iq1selectno(ip2)  = iq1selectno(ip2) + 1
         endif
      enddo

      maxexchanges = maxval( iq1selectno )
      iq1selectno  = 0

      allocate( iq1select(maxexchanges,-num_boundary_conditions:num_cells) )

      do iq = 1, noq_o
         ip1 = ip_o(1,iq)

         if ( ip1 .gt. 0 ) then
            ip1 = ipnt(ip1)
            ip2 = ip_o(2,iq)
            if ( ip2 .gt. 0 ) then
               ip2 = ipnt(ip2)
            end if
            if ( ip2 .eq. 0 .or. ip2 .eq. ip1) then
               ip1 = 0
            end if
         elseif ( ip1 .lt. 0 ) then
            ip1 = -ipnt_b(-ip1)
         endif

         if ( ip1 /= 0 ) then
            iq1selectno(ip1)   = iq1selectno(ip1) + 1
            ips                = iq1selectno(ip1)
            iq1select(ips,ip1) = iq
         endif
         if ( ip2 /= 0 ) then
            iq1selectno(ip2)   = iq1selectno(ip2) + 1
            ips                = iq1selectno(ip2)
            iq1select(ips,ip2) = iq
         endif
      enddo

      ipnt_q = 0

      do iq = 1 , noq_o
         ip1   = ip_o(1,iq)
         ip2   = ip_o(2,iq)
         ip3   = ip_o(3,iq)
         ip4   = ip_o(4,iq)
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

         ! if ip1 equals ip2 then the exchange is not used anymore, keep the negative pointers in the horizontal
         ! but not from negative to negative this will interfere with ddcouple

         if ( ( ip1 .ne. ip2 .and. .not. ( ip1 .eq. 0 .or. ip2 .eq. 0 )  .and. .not. (ip1 .lt. 0 .and. ip2 .lt. 0)) .or. & 
             ( (ip1 .lt. 0 .or. ip2 .lt. 0) .and. iq .le. noq1_o+noq2_o .and. .not. (ip1 .lt. 0 .and. ip2 .lt. 0)) ) then

            ! see if new exchange is unique

            found = .false.
            if ( iq .gt. noq1_o+noq2_o ) then
               iqstart = noq1_n + noq2_n + 1
            elseif ( iq .gt. noq1_o ) then
               iqstart = noq1_n + 1
            else
               iqstart = 1
            endif

            do ips = 1 , iq1selectno(ip1)
               iq_o = iq1select(ips,ip1)
               iq_n = ipnt_q(iq_o)

               ! This filters out the exchanges that have not been examined yet
               if ( iq_n >= iqstart .and. iq_o /= iq ) then
                  if ( ip1 .eq. ip_n(1,iq_n) .and. ip2 .eq. ip_n(2,iq_n) ) then
                     ipnt_q(iq) =  iq_n
                     found = .true.
                     exit
                  elseif ( ip2 .eq. ip_n(1,iq_n) .and. ip1 .eq. ip_n(2,iq_n) ) then
                     ipnt_q(iq) = -iq_n
                     found = .true.
                     exit
                  endif
               endif
            enddo

            ! if not found then add pointer

            if ( .not. found ) then
               noq_n = noq_n + 1
               if ( iq .gt. noq1_o+noq2_o ) then
                  noq3_n = noq3_n + 1
               elseif ( iq .gt. noq1_o ) then
                  noq2_n = noq2_n + 1
               else
                  noq1_n = noq1_n + 1
               endif
               ip_n(1,noq_n) = ip1
               ip_n(2,noq_n) = ip2
               ip_n(3,noq_n) = ip3
               ip_n(4,noq_n) = ip4
               ipnt_q(iq)    = noq_n
            endif
         else
            ipnt_q(iq)    = 0
         endif

      enddo

      ! Redo the hash table, now that the duplicates have been removed

      deallocate( iq1select )

      iq1selectno = 0
      iq2selectno = 0

      do iq_n = 1,noq_n
         ip1 = ip_n(1,iq_n)
         ip2 = ip_n(2,iq_n)

         iq1selectno(ip1)   = iq1selectno(ip1) + 1
         iq2selectno(ip2)   = iq2selectno(ip2) + 1
      enddo

      maxexchanges = max( maxval( iq1selectno ), maxval( iq2selectno ) )
      iq1selectno  = 0
      iq2selectno  = 0

      allocate( iq1select(maxexchanges,-num_boundary_conditions:num_cells), iq2select(maxexchanges,-num_boundary_conditions:num_cells) )

      do iq_n = 1,noq_n
         ip1 = ip_n(1,iq_n)
         ip2 = ip_n(2,iq_n)

         iq1selectno(ip1)   = iq1selectno(ip1) + 1
         ips                = iq1selectno(ip1)
         iq1select(ips,ip1) = iq_n

         iq2selectno(ip2)   = iq2selectno(ip2) + 1
         ips                = iq2selectno(ip2)
         iq2select(ips,ip2) = iq_n
      enddo

      ! look for +1 and -1 pointers

      do iq = 1 , noq_n
         ip1   = ip_n(1,iq)
         ip2   = ip_n(2,iq)
         ip3   = ip_n(3,iq)
         ip4   = ip_n(4,iq)

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

            do ips = iq2selectno(ip1) , 1 , -1
               iq2 = iq2select(ips,ip1)
               if ( iq2 >= iqstart .and. iq2 <= iq-1 ) then
                  if ( ip_n(2,iq2) .eq. ip1 ) then
                     ip3 = ip_n(1,iq2)
                     found = .true.
                     exit
                  endif
               endif
            enddo

            if ( .not. found ) then
               do ips = 1 , iq2selectno(ip1)
                  iq2 = iq2select(ips,ip1)
                  if ( iq2 >= iq+1 .and. iq2 <= iqstop ) then
                     if ( ip_n(2,iq2) .eq. ip1 ) then
                        ip3 = ip_n(1,iq2)
                        found = .true.
                        exit
                     endif
                  endif
               enddo
            endif
            if ( found ) then
               ip_n(3,iq) = ip3
            else
               ip_n(3,iq) = 0
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
            do ips = 1 , iq1selectno(ip2)
               iq2 = iq1select(ips,ip2)
               if ( iq2 >= iq+1 .and. iq2 <= iqstop ) then
                  if ( ip_n(1,iq2) .eq. ip2 ) then
                     ip4 = ip_n(2,iq2)
                     found = .true.
                     exit
                  endif
               endif
            enddo
            if ( .not. found ) then
               do ips = iq1selectno(ip2) , 1 , -1
                  iq2 = iq1select(ips,ip2)
                  if ( iq2 >= iqstart .and. iq2 <= iq-1 ) then
                     if ( ip_n(1,iq2) .eq. ip2 ) then
                        ip4 = ip_n(2,iq2)
                        found = .true.
                        exit
                     endif
                  endif
               enddo
            endif
            if ( found ) then
               ip_n(4,iq) = ip4
            else
               ip_n(4,iq) = 0
            endif
         endif

      enddo

      return
      end
