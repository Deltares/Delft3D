!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2026.
!
!  This program is free software: you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation version 3.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------
!
!

      subroutine makind
!
!          Create indices for Nefis file
!
!          Table R2: index in ITEMS is r2_iin
!          Table R3: index in ITEMS is inpuii
!          Table R3: index in PROCS is inpupi
!          Table R4: index in ITEMS is outpii
!          Table R4: index in PROCS is outppi

!          Include data structures for tables
      use m_waqpb_data
      use m_string_utils

      integer icnsb, iinpu, ioutp, iitem, iproc

      do 10 icnsb = 1,ncnsb
          iitem = index_in_array(r2_sid(icnsb),itemid(:nitem))
          if ( iitem .le. 0 ) then
              write(*,*) 'Error: unknown item ', r2_sid(icnsb)
              stop 'MAKIND: BUG 001'
          endif
          r2_iin(icnsb) = iitem-1
   10 continue

      do 20 iinpu = 1,ninpu
          iitem = index_in_array(inpuit(iinpu),itemid(:nitem))
          if ( iitem .le. 0 ) then
              write(*,*) 'Error: unknown item ', inpuit(iinpu)
              stop 'MAKIND: BUG 002'
          endif
          inpuii(iinpu) = iitem-1

          iproc = index_in_array(inpupr(iinpu),procid(:nproc))
          if ( iitem .le. 0 ) then
              write(*,*) 'Error: unknown item ', inpupr(iinpu)
              stop 'MAKIND: BUG 003'
          endif
          inpupi(iinpu) = iproc-1
   20 continue

      do 30 ioutp = 1,noutp
          iitem = index_in_array(outpit(ioutp),itemid(:nitem))
          if ( iitem .le. 0 ) then
              write(*,*) 'Error: unknown item ', outpit(ioutp)
              stop 'MAKIND: BUG 004'
          endif
          outpii(ioutp) = iitem-1

          iproc = index_in_array(outppr(ioutp),procid(:nproc))
          if ( iitem .le. 0 ) then
              write(*,*) 'Error: unknown item ', outppr(ioutp)
              stop 'MAKIND: BUG 005'
          endif
          outppi(ioutp) = iproc-1
   30 continue

      return
      end
