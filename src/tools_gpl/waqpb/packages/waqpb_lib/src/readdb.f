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

      subroutine readdb ( lu_inp, lu_mes )
      use m_zoek

      integer    lu_inp, lu_mes
      include 'data.inc'

c     Local declarations

      character*255 c255
      character*10 chkcnf(nconfm),c10
      character*1  swicnf(nconfm),c1dum
      integer      jndex , iproc , iconf , ipos  , ihulp , idum(1)

c     Read database containing Processes Library

c     Read Table P1

      open ( newunit = lu_inp , file = 'grpsub.csv')
      read ( lu_inp , * )
      nsgrp = 0
    5 if ( nsgrp+1 .gt. nsgrpm ) stop 'dimension NSGRPM'
      read ( lu_inp , * , end = 6  )
     j  sgrpid(nsgrp+1), sgrpnm(nsgrp+1)
      nsgrp = nsgrp + 1
      goto 5
    6 close (lu_inp)
      write (lu_mes,'(i5,'' lines read from GRPSUB.CSV'')') nsgrp

c     Read Table P2

      open ( newunit = lu_inp , file = 'items.csv' )
      read ( lu_inp , * )
      nitem = 0
   10 if ( nitem+1 .gt. nitemm ) stop 'dimension NITEMM'
      read ( lu_inp , * , end = 11 )
     j  itemid(nitem+1), itemse(nitem+1), itemex(nitem+1),
     j  itemde(nitem+1), itemun(nitem+1), itemnm(nitem+1),
     j  itemag(nitem+1), itemda(nitem+1), itemwk(nitem+1),
     j  itemgr(nitem+1)
      nitem = nitem + 1
      item_i(nitem) = nitem
      goto 10
   11 close (lu_inp)
      write (lu_mes,'(i5,'' lines read from ITEMS.CSV'')') nitem

c     Read Table P3

      open ( newunit = lu_inp , file = 'fortran.csv' )
      read ( lu_inp , * )
      nfort = 0
   15 if ( nfort+1 .gt. nfortm ) stop 'dimension NFORTM'
      read ( lu_inp , * , end = 16 )
     j  fortid(nfort+1)
      nfort = nfort + 1
      fort_i(nfort) = nfort
      goto 15
   16 close (lu_inp)
      write (lu_mes,'(i5,'' lines read from FORTRAN.CSV'')') nfort

c     Read Table P4

      open ( newunit = lu_inp , file = 'proces.csv' )
      read ( lu_inp , * )
      nproc = 0
   20 if ( nproc+1 .gt. nprocm ) stop 'dimension NprocM'
      read ( lu_inp , * , end = 21 )
     j  procid(nproc+1), procco(nproc+1), procfo(nproc+1),
     j  procnm(nproc+1)
      nproc = nproc + 1
      proc_i(nproc) = nproc
      goto 20
   21 close (lu_inp)
      write (lu_mes,'(i5,'' lines read from PROCES.CSV'')') nproc

c     Read table P5

      open ( newunit = lu_inp , file = 'config.csv' )
      read ( lu_inp , * )
      nconf = 0
  100 if ( nconf+1 .gt. nconfm ) stop 'dimension NconfM'
      read ( lu_inp , * , end = 101 )
     j      confid(nconf+1),confnm(nconf+1)
      nconf = nconf + 1
      goto 100
  101 close (lu_inp)
      write (lu_mes,'(i5,'' lines read from CONFIG.CSV'')') nconf

c     Read table R1

      write (lu_mes,'(5x,'' processing file CON_PRO.CSV ...'')')
      open ( newunit = lu_inp , file = 'con_pro.csv' )
      read ( lu_inp , * ) c10,(chkcnf(iconf),iconf=1,nconf)
c     check consistency beween Config and Con_pro files
      do 110 iconf = 1,nconf
          call zoek (chkcnf(iconf),1,confid(iconf),10,jndex)
          if (jndex.ne.1) STOP 'Inconsistent Config and Con_pro files'
  110 continue
  120 continue
      do 121 iconf=1,nconf
  121 swicnf(iconf) = ' '
c     read ( lu_inp , * , end=123) c10,(swicnf(iconf),iconf=1,nconf)
      read ( lu_inp , * , end=123) c10
      backspace ( lu_inp )
c     Some parsing necessary
      read ( lu_inp , '(a)' ) c255
c     write ( lu_mes , * ) 'Line: [',c255
      ipos = 0
      do 105 iconf = 1,nconf
c         write ( lu_mes , * ) 'Conf: ',iconf
c         write ( lu_mes , * ) 'Ipos in: ',ipos
          ihulp = index (c255(ipos+1:),',')
c         write ( lu_mes , * ) 'Index: ',ihulp
          if ( ihulp .le. 0 ) then
              swicnf(iconf) = ' '
          else
              ipos = ipos + ihulp
c             write ( lu_mes , * ) 'Ipos uit: ',ipos
              if ( c255(ipos+1:ipos+1) .eq. 'A' ) then
                  swicnf(iconf) = 'A'
c                 write ( lu_mes , * ) 'SWITCH ON '
              else
                  swicnf(iconf) = ' '
c                 write ( lu_mes , * ) 'SWITCH OFF '
              endif
          endif
  105 continue
      call zoek (c10,nproc,procid,10,iproc)
      if (iproc.le.0) then
      write (lu_mes,'(''Unknown process '',a10,'' in Con_pro file'')')
     j           c10
          goto 120
      endif
      do 122 iconf = 1,nconf
          if ( swicnf(iconf) .eq. 'A' ) then
              conpro(iconf,iproc) = .true.
          else
              conpro(iconf,iproc) = .false.
          endif
  122 continue
      goto 120
  123 close (lu_inp)

c     Read table R2

      open ( newunit = lu_inp , file = 'con_sub.csv' )
      read ( lu_inp , * )
      ncnsb = 0
  130 if ( ncnsb+1 .gt. ncnsbm ) stop 'dimension ncnsb'
      read ( lu_inp , * , end = 131 )
     j      r2_cid(ncnsb+1),r2_siD(ncnsb+1)
      ncnsb = ncnsb + 1
      goto 130
  131 close (lu_inp)
      write (lu_mes,'(i5,'' lines read from CON_SUB.CSV'')') ncnsb

c     Read table R3

      open ( newunit = lu_inp , file = 'inputs.csv' )
      read ( lu_inp , * )
      ninpu = 0
   40 if ( ninpu+1 .gt. ninpum ) stop 'dimension NinpuM'
      read ( lu_inp , * , end = 41 )
     j  inpupr(ninpu+1), inpuit(ninpu+1), inpunm(ninpu+1),
     j  inpude(ninpu+1), inpudo(ninpu+1), inpusx(ninpu+1)
      ninpu = ninpu + 1
      inpu_i(ninpu) = ninpu
      goto 40
   41 close (lu_inp)
      write (lu_mes,'(i5,'' lines read from INPUTS.CSV'')') ninpu

C     Sort table R3

      call sorts2 ( inpupr, inpuit, inpunm, inpude, inpudo,
     j              inpusx, ninpu , .true., .true. )
      write (lu_mes,'('' INPUTS.CSV sorted'')')


c     Read table R4

      open ( newunit = lu_inp , file = 'outputs.csv' )
      read ( lu_inp , * )
      noutp = 0
   50 if ( noutp+1 .gt. noutpm ) stop 'dimension NoutpM'
      read ( lu_inp , * , end = 51 )
     j  outppr(noutp+1), outpit(noutp+1), outpnm(noutp+1),
     j  outpdo(noutp+1), outpsx(noutp+1)
      noutp = noutp + 1
      outp_i(noutp) = noutp
      goto 50
   51 close (lu_inp)
      write (lu_mes,'(i5,'' lines read from OUTPUTS.CSV'')') noutp

C     Sort table R4

      call sorts2 ( outppr, outpit, outpnm, c1dum , outpdo,
     j              outpsx, noutp , .false., .true. )
      write (lu_mes,'('' OUTPUTS.CSV sorted'')')

c     Read table R5

      open ( newunit = lu_inp , file = 'outpflx.csv' )
      read ( lu_inp , * )
      noutf = 0
   60 if ( noutf+1 .gt. noutfm ) stop 'dimension NoutfM'
      read ( lu_inp , * , end = 61 )
     j  outfpr(noutf+1), outffl(noutf+1), outfnm(noutf+1),
     j  outfdo(noutf+1)
      noutf = noutf + 1
      outf_i(noutf) = noutf
      goto 60
   61 close (lu_inp)
      write (lu_mes,'(i5,'' lines read from OUTPFLX.CSV'')') noutf

C     Sort table R5

      call sorts2 ( outfpr, outffl, outfnm, c1dum , outfdo,
     j              idum  , noutf , .false., .false. )
      write (lu_mes,'('' OUTPFLX.CSV sorted'')')

c     Read table R6

      open ( newunit = lu_inp , file = 'stochi.csv' )
      read ( lu_inp , * )
      nstoc = 0
   70 if ( nstoc+1 .gt. nstocm ) stop 'dimension NstocM'
      read ( lu_inp , * , end = 71 )
     j  stocfl(nstoc+1), stocsu(nstoc+1), stocsc(nstoc+1)
      nstoc = nstoc + 1
      stoc_i(nstoc) = nstoc
      goto 70
   71 close (lu_inp)
      write (lu_mes,'(i5,'' lines read from STOCHI.CSV'')') nstoc

C     Sort table R6

      call sortst ( stocfl, stocsu, stocsc, nstoc )
      write (lu_mes,'('' STOCHI.CSV sorted'')')

c     Read table R7

      open ( newunit = lu_inp , file = 'velocs.csv' )
      read ( lu_inp , * )
      nvelo = 0
   90 if ( nvelo+1 .gt. nvelom ) stop 'dimension NveloM'
      read ( lu_inp , * , end = 91 )
     j  veloit(nvelo+1), velosu(nvelo+1), velosc(nvelo+1)
      nvelo = nvelo + 1
      velo_i(nvelo) = nvelo
      goto 90
   91 close (lu_inp)
      write (lu_mes,'(i5,'' lines read from VELOCS.CSV'')') nvelo

C     Sort table R7

      call sortst ( veloit, velosu, velosc, nvelo )
      write (lu_mes,'('' VELOCS.CSV sorted'')')

c     Read table R8

      open ( newunit = lu_inp , file = 'disps.csv' )
      read ( lu_inp , * )
      ndisp = 0
   80 if ( ndisp+1 .gt. ndispm ) stop 'dimension NdispM'
      read ( lu_inp , * , end = 81 )
     j  dispit(ndisp+1), dispsu(ndisp+1), dispsc(ndisp+1)
      ndisp = ndisp + 1
      disp_i(ndisp) = ndisp
      goto 80
   81 close (lu_inp)
      write (lu_mes,'(i5,'' lines read from DISPS.CSV'')') ndisp

C     Sort table R8

      call sortst ( dispit, dispsu, dispsc, ndisp )
      write (lu_mes,'('' DISPS.CSV sorted'')')

c     Read table R9

      open ( newunit = lu_inp , file = 'table5.csv' )
      read ( lu_inp , * , end = 202 )
      nmodv = 0
  200 if ( nmodv+1 .gt. nmodvm ) stop 'dimension NmodvM'
      read ( lu_inp , * , end = 201 )
     j  modvci(nmodv+1), modvit(nmodv+1)
      nmodv = nmodv + 1
      goto 200
  201 close (lu_inp)
      write (lu_mes,'(i5,'' lines read from TABLE5.CSV'')') nmodv
  202 continue

c     Table old_items

      open ( newunit = lu_inp , file = 'old_items.csv' )
      read ( lu_inp , * , end = 302 )
      n_old_items = 0
  300 if ( n_old_items+1 .gt. n_old_items_max ) stop 'dimension n_old_items_max'
      i = n_old_items + 1
      read ( lu_inp , * , end = 301 )
     j   old_items_old_name(i),
     j   old_items_new_name(i),
     j   old_items_old_default(i),
     j   old_items_configuration(i),
     j   old_items_serial(i),
     j   old_items_action_type(i)
      n_old_items = n_old_items + 1
      goto 300
  301 close (lu_inp)
      write (lu_mes,'(i5,'' lines read from old_items.csv'')') n_old_items
  302 continue

      return
      end

      subroutine writdb ( lu )
      integer lu
      include 'data.inc'

      integer iproc, iconf, i
      character*10 c10
      character*1  swicnf(nconfm)

c     Table P1

      open ( newunit = lu , file = 'grpsub.csv' )
      write (lu,'(''sgrpid,sgrpnm'')')
      if ( nsgrp .gt. 0 )
     jwrite (lu,'(''"'',a30,''","'',a50,''"'')')
     j  (sgrpid(i),sgrpnm(i),i=1,nsgrp)
      close (lu)

c     Table P2

      open ( newunit = lu , file = 'items.csv' )
      write (lu,'(''itemid,itemse,itemex,itemde,itemun,itemnm,'',
     j        ''itemag,itemda,itemwk,itemgr'')')
      if ( nitem .gt. 0 )
     jwrite (lu,'(''"'',a10,''","'',a1,''","'',a1,''",'',g15.6,
     j            '',"'',a20,''","'',a50,''","'',a10,''","'',a10,
     j            ''","'',a1,''","'',a30,''"'')')
     j  (itemid(i),itemse(i),itemex(i),itemde(i),itemun(i),
     j   itemnm(i),itemag(i),itemda(i),itemwk(i),itemgr(i),i=1,nitem)
      close (lu)

c     Table P3

      open ( newunit = lu , file = 'fortran.csv' )
      write (lu,'(''fortid'')')
      if ( nfort .gt. 0 )
     jwrite (lu,'(''"'',a10,''"'')') (fortid(i),i=1,nfort)
      close (lu)

c     Table P4

      open ( newunit = lu , file = 'proces.csv' )
      write (lu,'(''procid,procco,procfo,procnm'')')
      if ( nproc .gt. 0 )
     jwrite (lu,'(''"''a10,''",'',i3,'',"'',a10,''","'',a50,''"'')')
     j  (procid(i),procco(i),procfo(i),procnm(i),i=1,nproc)
      close (lu)

c     Table P5

      open ( newunit = lu , file = 'config.csv' )
      write ( lu ,'(''confid,confnm'')')
      if ( nconf .gt. 0 )
     jwrite (lu,'(''"'',a10,''","'',a50,''"'')')
     j  (confid(i),confnm(i),i=1,nconf)
      close (lu)

c     Table R1

      open ( newunit = lu , file = 'con_pro.csv' )
      c10 = 'Config:'
      write ( lu , '(''"'',a10,''"'',99('',"'',a10,''"''))')
     j    c10,(confid(i),i=1,nconf)
      do 300 iproc=1,nproc
          do 290 iconf = 1,nconf
              if ( conpro(iconf,iproc) ) then
                  swicnf(iconf) = 'A'
              else
                  swicnf(iconf) = ' '
              endif
  290     continue
          write ( lu , '(''"'',a10,''"'',99('','',a1))' )
     j    procid(iproc),(swicnf(iconf),iconf=1,nconf)
  300 continue
      close (lu)

c     Table R2

      open ( newunit = lu , file = 'con_sub.csv' )
      write ( lu ,'(''r2_cid,r2_sid'')')
      if ( ncnsb .gt. 0 )
     jwrite (lu,'(''"'',a10,''","'',a10,''"'')')
     j  (r2_cid(i),r2_sid(i),i=1,ncnsb)
      close (lu)

c     Table R3

      open ( newunit = lu , file = 'inputs.csv' )
      write (lu,'(''inpupr,inpuit,inpunm,inpude,inpudo,inpusx'')')
      if ( ninpu .gt. 0 )
     jwrite (lu,'(''"'',a10,''","'',a10,''",'',i4,'',"'',a1,
     j            ''","'',a1,''",'',i1)')
     j  (inpupr(i),inpuit(i),inpunm(i),inpude(i),inpudo(i),inpusx(i),
     j   i=1,ninpu)
      close (lu)

c     Table R4

      open ( newunit = lu , file = 'outputs.csv' )
      write (lu,'(''outppr,outpit,outpnm,outpdo,outpsx'')')
      if ( noutp .gt. 0 )
     jwrite (lu,'(''"'',a10,''","'',a10,''",'',i4,'',"'',a1,
     j            ''",'',i1)')
     j  (outppr(i),outpit(i),outpnm(i),outpdo(i),outpsx(i),i=1,noutp)
      close (lu)

c     Table R5

      open ( newunit = lu , file = 'outpflx.csv' )
      write (lu,'(''outfpr,outffl,outfnm,outfdo'')')
      if ( noutf .gt. 0 )
     jwrite (lu,'(''"'',a10,''","'',a10,''",'',i4,'',"'',a1,''"'')')
     j  (outfpr(i),outffl(i),outfnm(i),outfdo(i),i=1,noutf)
      close (lu)

c     Table R6

      open ( newunit = lu , file = 'stochi.csv' )
      write (lu,'(''stocfl,stocsu,stocsc'')')
      if ( nstoc .gt. 0 )
     jwrite (lu,'(''"'',a10,''","'',a10,''",'',f10.5)')
     j  (stocfl(i),stocsu(i),stocsc(i),i=1,nstoc)
      close (lu)

c     Table R7

      open ( newunit = lu , file = 'velocs.csv' )
      write (lu,'(''veloit,velosu,velosc'')')
      if ( nvelo .gt. 0 )
     jwrite (lu,'(''"''a10,''","'',a10,''",'',f10.5)')
     j  (veloit(i),velosu(i),velosc(i),i=1,nvelo)
      close (lu)

c     Table R8

      open ( newunit = lu , file = 'disps.csv' )
      write (lu,'(''dispit,dispsu,dispsc'')')
      if ( ndisp .gt. 0 )
     jwrite (lu,'(''"''a10,''","'',a10,''",'',f10.5)')
     j  (dispit(i),dispsu(i),dispsc(i),i=1,ndisp)
      close (lu)

c     Table old_items

      open ( newunit = lu , file = 'old_items.csv' )
      write (lu,'(''old_name,new_name,old_default,configuration,serial,action_type'')')
      if ( n_old_items .gt. 0 )
     jwrite (lu,'(''"'',a10,''","'',a10,''",'',g15.6,'',"'',a10,''",'',i10,'','',i10)')
     j  (old_items_old_name(i),
     j   old_items_new_name(i),
     j   old_items_old_default(i),
     j   old_items_configuration(i),
     j   old_items_serial(i),
     j   old_items_action_type(i),
     j   i=1,n_old_items)
      close (lu)

      return
      end
