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

      subroutine cldept
      include 'data.inc'

      integer ioffse, aantal, ifort , iproc , istoc , ivelo , idisp ,
     j        iitem , iinpu , ioutp , ioutf , i

c     Table P3: remove obsolete lines by checking P4

      do 10 ifort = 1,nfort
   10 fort_i(ifort) = 0
      do 20 iproc = 1,nproc
          call zoek (procfo(iproc),nfort,fortid,10,ifort)
          if ( ifort .le. 0 ) then
              write (*,*) procfo(iproc)
              stop ' FORT table WRONG!!!'
          endif
          fort_i(ifort) = 1
   20 continue
      call clrcar (nfort , fort_i, fortid )
      call updind (nfort , fort_i)

c     Table P2: remove obsolete items by checking R2 t/m R8

      do 60 iitem = 1,nitem
          item_i(iitem) = 0
          itemse(iitem) = ' '
          itemex(iitem) = ' '
   60 continue
c     Check R2
      do 65 i = 1,ncnsb
          call zoek (r2_sid(i),nitem,itemid,10,iitem)
          if ( iitem .le. 0 ) stop ' R2 table WRONG!!!'
          item_i(iitem) = 1
          itemse(iitem) = 'x'
   65 continue
c     Check R3
      do 70 iinpu = 1,ninpu
          call zoek (inpuit(iinpu),nitem,itemid,10,iitem)
          if ( iitem .le. 0 ) stop ' ITEM table WRONG!!!'
          item_i(iitem) = 1
          if ( inpusx(iinpu) .eq. 1 ) then
              itemse(iitem) = 'x'
          else
              itemex(iitem) = 'x'
          endif
   70 continue
c     Check R4
      do 80 ioutp = 1,noutp
          call zoek (outpit(ioutp),nitem,itemid,10,iitem)
          if ( iitem .le. 0 ) stop ' ITEM table WRONG!!!'
          item_i(iitem) = 1
          if ( outpsx(ioutp) .eq. 1 ) then
              itemse(iitem) = 'x'
          else
              itemex(iitem) = 'x'
          endif
   80 continue
c     Check R5
      do 90 ioutf = 1,noutf
          call zoek (outffl(ioutf),nitem,itemid,10,iitem)
          if ( iitem .le. 0 ) stop ' ITEM table WRONG!!!'
          item_i(iitem) = 1
          itemse(iitem) = 'x'
   90 continue
c     Check R6
      do 95 istoc = 1,nstoc
          call zoek (stocsu(istoc),nitem,itemid,10,iitem)
          if ( iitem .le. 0 ) stop ' ITEM table WRONG!!!'
          item_i(iitem) = 1
   95 continue
c     Check R7
      do 100 ivelo = 1,nvelo
          call zoek (velosu(ivelo),nitem,itemid,10,iitem)
          if ( iitem .le. 0 ) stop ' ITEM table WRONG!!!'
          item_i(iitem) = 1
  100 continue
c     Check R8
      do 110 idisp = 1,ndisp
          call zoek (dispsu(idisp),nitem,itemid,10,iitem)
          if ( iitem .le. 0 ) stop ' ITEM table WRONG!!!'
          item_i(iitem) = 1
  110 continue
      call clrcar (nitem , item_i, itemid )
      call clrcar (nitem , item_i, itemun )
      call clrcar (nitem , item_i, itemnm )
      call clrcar (nitem , item_i, itemse )
      call clrcar (nitem , item_i, itemex )
      call clrrar (nitem , item_i, itemde )
      call clrcar (nitem , item_i, itemwk )
      call clrcar (nitem , item_i, itemag )
      call clrcar (nitem , item_i, itemda )
      call clrcar (nitem , item_i, itemgr )
      call updind (nitem , item_i)

      return
      end
