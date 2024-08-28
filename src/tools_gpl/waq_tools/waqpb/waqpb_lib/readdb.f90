!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
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

subroutine readdb(lu_inp, lu_mes)
    use m_validate_input, only: validate_names, validate_units
    use m_waqpb_data
    use m_string_utils, only: index_in_array, string_equals

    integer  :: lu_inp  !< Logical unit number for input
    integer  :: lu_mes  !< Logical unit number for messages (logging)

    character(len=255) c255
    character(len=10) chkcnf(nconfm),c10
    character(len=1)  swicnf(nconfm),c1dum
    integer      jndex , iproc , iconf , ipos  , ihulp , idum(1), error


    !Read database containing Processes Library

    !Read Table P1
    open(newunit = lu_inp, file='grpsub.csv')
    read(lu_inp, *)
    nsgrp = 0
  5 if (nsgrp+1>nsgrpm) stop 'dimension NSGRPM'
    read(lu_inp, * , end = 6) sgrpid(nsgrp+1), sgrpnm(nsgrp+1)
    nsgrp = nsgrp + 1
    goto 5
  6 close(lu_inp)
    write(lu_mes,'(i5,'' lines read from GRPSUB.CSV'')') nsgrp


    !Read Table P2
    open(newunit=lu_inp, file='items.csv')
    read(lu_inp, *)
    nitem = 0
 10 if (nitem+1>nitemm) stop 'dimension NITEMM'
    read(lu_inp, * , end = 11) &
        itemid(nitem+1), itemse(nitem+1), itemex(nitem+1), &
        itemde(nitem+1), itemun(nitem+1), itemnm(nitem+1), &
        itemag(nitem+1), itemda(nitem+1), itemwk(nitem+1), &
        itemgr(nitem+1)
    call validate_units(itemun(nitem+1), lu_mes)
    nitem = nitem + 1
    item_i(nitem) = nitem
    goto 10
 11 close(lu_inp)
    write(lu_mes,'(i5,'' lines read from ITEMS.CSV'')') nitem


    !Read Table P3
    open(newunit=lu_inp, file='fortran.csv')
    read(lu_inp, *)
    nfort = 0
 15 if (nfort+1>nfortm) stop 'dimension NFORTM'
    read(lu_inp, * , end = 16) &
        fortid(nfort+1)
    nfort = nfort + 1
    fort_i(nfort) = nfort
    goto 15
 16 close(lu_inp)
    write(lu_mes,'(i5,'' lines read from FORTRAN.CSV'')') nfort
    call validate_names(fortid(1:nfort), lu_mes)


    !Read Table P4
    open(newunit=lu_inp, file='proces.csv')
    read(lu_inp, *)
    nproc = 0
 20 if (nproc+1>nprocm) stop 'dimension NprocM'
    read(lu_inp, * , end = 21) &
        procid(nproc+1), procco(nproc+1), procfo(nproc+1), &
        procnm(nproc+1)
    nproc = nproc + 1
    proc_i(nproc) = nproc
    goto 20
 21 close(lu_inp)
    write(lu_mes,'(i5,'' lines read from PROCES.CSV'')') nproc
    call validate_names(procfo(1:nproc), lu_mes)


    !Read table P5
    open(newunit=lu_inp, file='config.csv')
    read(lu_inp, *)
    nconf = 0
100 if (nconf+1>nconfm) stop 'dimension NconfM'
    read(lu_inp, * , end = 101) &
            confid(nconf+1),confnm(nconf+1)
    nconf = nconf + 1
    goto 100
101 close(lu_inp)
    write(lu_mes,'(i5,'' lines read from CONFIG.CSV'')') nconf


    !Read table R1
    write(lu_mes,'(5x,'' processing file CON_PRO.CSV ...'')')
    open(newunit=lu_inp, file='con_pro.csv')
    read(lu_inp, *) c10,(chkcnf(iconf),iconf=1,nconf)
    !Check consistency beween Config and Con_pro files
    do 110 iconf = 1,nconf
        if (.not. string_equals(chkcnf(iconf), confid(iconf))) then
            STOP 'Inconsistent Config and Con_pro files'
        end if
110 continue
120 continue
    do iconf=1,nconf
        swicnf(iconf) = ' '
    end do
    read(lu_inp, * , end=123) c10
    backspace ( lu_inp)
    !Some parsing necessary
    read(lu_inp, '(a)') c255
    ipos = 0
    do 105 iconf = 1,nconf
        ihulp = index (c255(ipos+1:),',')
        if (ihulp .le. 0) then
            swicnf(iconf) = ' '
        else
            ipos = ipos + ihulp
            if (c255(ipos+1:ipos+1) .eq. 'A') then
                swicnf(iconf) = 'A'
            else
                swicnf(iconf) = ' '
            endif
        endif
105 continue
    iproc = index_in_array(c10,procid)
    if (iproc.le.0) then
        write(lu_mes,'(''Unknown process '',a10,'' in Con_pro file'')') &
                c10
        goto 120
    endif
    do 122 iconf = 1,nconf
        if (swicnf(iconf) .eq. 'A') then
            conpro(iconf,iproc) = .true.
        else
            conpro(iconf,iproc) = .false.
        endif
122 continue
    goto 120
123 close(lu_inp)


    !Read table R2
    open(newunit=lu_inp, file='con_sub.csv')
    read(lu_inp, *)
    ncnsb = 0
130 if (ncnsb+1>ncnsbm) stop 'dimension ncnsb'
    read(lu_inp, * , end = 131) &
            r2_cid(ncnsb+1),r2_sid(ncnsb+1)
    ncnsb = ncnsb + 1
    goto 130
131 close(lu_inp)
    write(lu_mes,'(i5,'' lines read from CON_SUB.CSV'')') ncnsb


    !Read table R3
    open(newunit=lu_inp, file='inputs.csv')
    read(lu_inp, *)
    ninpu = 0
 40 if (ninpu+1>ninpum) stop 'dimension NinpuM'
    read(lu_inp, * , end = 41) &
        inpupr(ninpu+1), inpuit(ninpu+1), inpunm(ninpu+1), &
        inpude(ninpu+1), inpudo(ninpu+1), inpusx(ninpu+1)
    ninpu = ninpu + 1
    inpu_i(ninpu) = ninpu
    goto 40
 41 close(lu_inp)
    write(lu_mes,'(i5,'' lines read from INPUTS.CSV'')') ninpu
    !Sort table R3
    call sorts2 ( inpupr, inpuit, inpunm, inpude, inpudo, &
                  inpusx, ninpu , .true., .true.)
    write(lu_mes,'('' INPUTS.CSV sorted'')')


    !Read table R4
    open(newunit=lu_inp, file='outputs.csv')
    read(lu_inp, *)
    noutp = 0
 50 if (noutp+1>noutpm) stop 'dimension NoutpM'
    read(lu_inp, * , end = 51) &
        outppr(noutp+1), outpit(noutp+1), outpnm(noutp+1), &
        outpdo(noutp+1), outpsx(noutp+1)
    noutp = noutp + 1
    outp_i(noutp) = noutp
    goto 50
 51 close(lu_inp)
    write(lu_mes,'(i5,'' lines read from OUTPUTS.CSV'')') noutp
    !Sort table R4
    call sorts2 ( outppr, outpit, outpnm, c1dum , outpdo, &
                  outpsx, noutp , .false., .true.)
    write(lu_mes,'('' OUTPUTS.CSV sorted'')')


    !Read table R5
    open(newunit=lu_inp, file='outpflx.csv')
    read(lu_inp, *)
    noutf = 0
 60 if (noutf+1>noutfm) stop 'dimension NoutfM'
    read(lu_inp, * , end = 61) &
        outfpr(noutf+1), outffl(noutf+1), outfnm(noutf+1), &
        outfdo(noutf+1)
    noutf = noutf + 1
    outf_i(noutf) = noutf
    goto 60
 61 close(lu_inp)
    write(lu_mes,'(i5,'' lines read from OUTPFLX.CSV'')') noutf
    !Sort table R5
    call sorts2( outfpr, outffl, outfnm, c1dum , outfdo, &
                idum  , noutf , .false., .false.)
    write(lu_mes,'('' OUTPFLX.CSV sorted'')')


    !Read table R6
    open(newunit=lu_inp, file='stochi.csv')
    read(lu_inp, *)
    nstoc = 0
 70 if (nstoc+1>nstocm) stop 'dimension NstocM'
    read(lu_inp, * , end = 71) &
        stocfl(nstoc+1), stocsu(nstoc+1), stocsc(nstoc+1)
    nstoc = nstoc + 1
    stoc_i(nstoc) = nstoc
    goto 70
 71 close(lu_inp)
    write(lu_mes,'(i5,'' lines read from STOCHI.CSV'')') nstoc
    !Sort table R6
    call sortst ( stocfl, stocsu, stocsc, nstoc)
    write(lu_mes,'('' STOCHI.CSV sorted'')')


    !Read table R7
    open(newunit=lu_inp, file='velocs.csv')
    read(lu_inp, *)
    nvelo = 0
 90 if (nvelo+1>nvelom) stop 'dimension NveloM'
    read(lu_inp, * , end = 91) &
        veloit(nvelo+1), velosu(nvelo+1), velosc(nvelo+1)
    nvelo = nvelo + 1
    velo_i(nvelo) = nvelo
    goto 90
 91 close(lu_inp)
    write(lu_mes,'(i5,'' lines read from VELOCS.CSV'')') nvelo
    !Sort table R7
    call sortst ( veloit, velosu, velosc, nvelo)
    write(lu_mes,'('' VELOCS.CSV sorted'')')


    !Read table R8
    open(newunit=lu_inp, file='disps.csv')
    read(lu_inp, *)
    ndisp = 0
 80 if (ndisp+1>ndispm) stop 'dimension NdispM'
    read(lu_inp, * , end = 81) &
      dispit(ndisp+1), dispsu(ndisp+1), dispsc(ndisp+1)
    ndisp = ndisp + 1
    disp_i(ndisp) = ndisp
    goto 80
 81 close(lu_inp)
    write(lu_mes,'(i5,'' lines read from DISPS.CSV'')') ndisp
    !Sort table R8
    call sortst ( dispit, dispsu, dispsc, ndisp)
    write(lu_mes,'('' DISPS.CSV sorted'')')


    !Read table R9
    open(newunit=lu_inp, file='table5.csv')
    read(lu_inp, * , end = 202)
    nmodv = 0
200 if (nmodv+1>nmodvm) stop 'dimension NmodvM'
    read(lu_inp, * , end = 201) &
        modvci(nmodv+1), modvit(nmodv+1)
    nmodv = nmodv + 1
    goto 200
201 close(lu_inp)
    write(lu_mes,'(i5,'' lines read from TABLE5.CSV'')') nmodv
202 continue
    !Table old_items
    open(newunit=lu_inp, file='old_items.csv')
    read(lu_inp, * , end = 302)
    n_old_items = 0
300 if (n_old_items+1>n_old_items_max) stop 'dimension n_old_items_max'
    i = n_old_items + 1
    read(lu_inp, * , end = 301) &
        old_items_old_name(i), &
        old_items_new_name(i), &
        old_items_old_default(i), &
        old_items_configuration(i), &
        old_items_serial(i), &
        old_items_action_type(i)
    n_old_items = n_old_items + 1
    goto 300
301 close(lu_inp)
    write(lu_mes,'(i5,'' lines read from old_items.csv'')') n_old_items
302 continue
    return
end subroutine readdb


subroutine writdb(lu)
    use m_waqpb_data
    integer :: lu !< logical unit number

    integer iproc, iconf, i
    character(len=10) c10
    character(len=1)  swicnf(nconfm)

    !Table P1
    open(newunit=lu, file='grpsub.csv')
    write(lu,'(''sgrpid,sgrpnm'')')
    if (nsgrp>0) then
        write(lu,'(''"'',a30,''","'',a50,''"'')') (sgrpid(i),sgrpnm(i),i=1,nsgrp)
    end if
    close(lu)

    !Table P2
    open(newunit=lu, file='items.csv')
    write(lu,'(''itemid,itemse,itemex,itemde,itemun,itemnm,'', &
            ''itemag,itemda,itemwk,itemgr'')')
    if (nitem>0) then
        write(lu,'(''"'',a10,''","'',a1,''","'',a1,''",'',g15.6, &
                    '',"'',a20,''","'',a50,''","'',a10,''","'',a10, &
                    ''","'',a1,''","'',a30,''"'')') &
                    (itemid(i), itemse(i), itemex(i), itemde(i), itemun(i), &
                    itemnm(i), itemag(i), itemda(i), itemwk(i), itemgr(i),  i=1, nitem)
    end if
    close(lu)


    !Table P3
    open(newunit=lu, file='fortran.csv')
    write(lu,'(''fortid'')')
    if (nfort>0) then
        write(lu,'(''"'',a10,''"'')') (fortid(i),i=1,nfort)
    end if
    close(lu)


    !Table P4
    open(newunit=lu, file='proces.csv')
    write(lu,'(''procid,procco,procfo,procnm'')')
    if (nproc>0) then
        write(lu,'(''"''a10,''",'',i3,'',"'',a10,''","'',a50,''"'')') &
            (procid(i),procco(i),procfo(i),procnm(i),i=1,nproc)
    end if
    close(lu)


    !Table P5
    open(newunit=lu, file='config.csv')
    write(lu,'(''confid,confnm'')')
    if (nconf>0)  then
        write(lu,'(''"'',a10,''","'',a50,''"'')') &
            (confid(i),confnm(i),i=1,nconf)
    end if
    close(lu)


    !Table R1
    open(newunit=lu, file='con_pro.csv')
    c10 = 'Config:'
    write(lu, '(''"'',a10,''"'',99('',"'',a10,''"''))') &
            c10,(confid(i),i=1,nconf)
    do 300 iproc=1,nproc
        do 290 iconf = 1,nconf
            if (conpro(iconf,iproc)) then
                swicnf(iconf) = 'A'
            else
                swicnf(iconf) = ' '
            endif
290     continue
        write(lu, '(''"'',a10,''"'',99('','',a1))') &
          procid(iproc),(swicnf(iconf),iconf=1,nconf)
300 continue
    close(lu)


    !Table R2
    open(newunit=lu, file='con_sub.csv')
    write(lu,'(''r2_cid,r2_sid'')')
    if (ncnsb>0) then
        write(lu,'(''"'',a10,''","'',a10,''"'')') &
              (r2_cid(i),r2_sid(i),i=1,ncnsb)
    end if
    close(lu)


    !Table R3
    open(newunit=lu, file='inputs.csv')
    write(lu,'(''inpupr,inpuit,inpunm,inpude,inpudo,inpusx'')')
    if (ninpu>0) then
        write(lu,'(''"'',a10,''","'',a10,''",'',i4,'',"'',a1, &
                ''","'',a1,''",'',i1)') &
                (inpupr(i),inpuit(i),inpunm(i),inpude(i),inpudo(i),inpusx(i), &
                i=1,ninpu)
    end if
    close(lu)


    !Table R4
    open(newunit=lu, file='outputs.csv')
    write(lu,'(''outppr,outpit,outpnm,outpdo,outpsx'')')
    if (noutp>0) then
        write(lu,'(''"'',a10,''","'',a10,''",'',i4,'',"'',a1, &
                ''",'',i1)') &
                (outppr(i),outpit(i),outpnm(i),outpdo(i),outpsx(i),i=1,noutp)
    end if
    close(lu)


    !Table R5
    open(newunit=lu, file='outpflx.csv')
    write(lu,'(''outfpr,outffl,outfnm,outfdo'')')
    if (noutf>0) then
        write(lu,'(''"'',a10,''","'',a10,''",'',i4,'',"'',a1,''"'')') &
             (outfpr(i),outffl(i),outfnm(i),outfdo(i),i=1,noutf)
    end if
    close(lu)


    !Table R6
    open(newunit=lu, file='stochi.csv')
    write(lu,'(''stocfl,stocsu,stocsc'')')
    if (nstoc>0) then
        write(lu,'(''"'',a10,''","'',a10,''",'',f10.5)') &
                    (stocfl(i),stocsu(i),stocsc(i),i=1,nstoc)
    end if
    close(lu)


    !Table R7
    open(newunit=lu, file='velocs.csv')
    write(lu,'(''veloit,velosu,velosc'')')
    if (nvelo>0) then
        write(lu,'(''"''a10,''","'',a10,''",'',f10.5)') &
          (veloit(i),velosu(i),velosc(i),i=1,nvelo)
    end if
    close(lu)


    !Table R8
    open(newunit=lu, file='disps.csv')
    write(lu,'(''dispit,dispsu,dispsc'')')
    if (ndisp>0) then
        write(lu,'(''"''a10,''","'',a10,''",'',f10.5)') &
            (dispit(i),dispsu(i),dispsc(i),i=1,ndisp)
    end if
    close(lu)


    !Table old_items
    open(newunit=lu, file='old_items.csv')
    write(lu,'(''old_name,new_name,old_default,configuration,serial,action_type'')')
    if (n_old_items>0) then
        write(lu,'(''"'',a10,''","'',a10,''",'',g15.6,'',"'',a10,''",'',i10,'','',i10)') &
                (old_items_old_name(i), &
                old_items_new_name(i), &
                old_items_old_default(i), &
                old_items_configuration(i), &
                old_items_serial(i), &
                old_items_action_type(i), &
                i=1,n_old_items)
    end if
    close(lu)
end subroutine writdb