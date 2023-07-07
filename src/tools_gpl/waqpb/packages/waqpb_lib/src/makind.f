!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2023.                                
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
c
c     Create indices for Nefis file
c
c     Table R2: index in ITEMS is r2_iin
c     Table R3: index in ITEMS is inpuii
c     Table R3: index in PROCS is inpupi
c     Table R4: index in ITEMS is outpii
c     Table R4: index in PROCS is outppi

c     Include data structures for tables
      include 'data.inc'

      integer icnsb, iinpu, ioutp, iitem, iproc

      do 10 icnsb = 1,ncnsb
          call zoek (r2_sid(icnsb),nitem,itemid,10,iitem)
          if ( iitem .le. 0 ) stop 'MAKIND: BUG 001'
          r2_iin(icnsb) = iitem-1
   10 continue

      do 20 iinpu = 1,ninpu
          call zoek (inpuit(iinpu),nitem,itemid,10,iitem)
          if ( iitem .le. 0 ) stop 'MAKIND: BUG 002'
          inpuii(iinpu) = iitem-1

          call zoek (inpupr(iinpu),nproc,procid,10,iproc)
          if ( iproc .le. 0 ) stop 'MAKIND: BUG 003'
          inpupi(iinpu) = iproc-1
   20 continue

      do 30 ioutp = 1,noutp
          call zoek (outpit(ioutp),nitem,itemid,10,iitem)
          if ( iitem .le. 0 ) stop 'MAKIND: BUG 004'
          outpii(ioutp) = iitem-1

          call zoek (outppr(ioutp),nproc,procid,10,iproc)
          if ( iproc .le. 0 ) stop 'MAKIND: BUG 005'
          outppi(ioutp) = iproc-1
   30 continue

      return
      end
