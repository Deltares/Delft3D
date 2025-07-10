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

!     
!          DATA STRUCTURE FOR PROCESSES LIBRARY MANAGEMENT
!     
!          Reference: bespreekverslag van Gils 3-9-1998 R3252

      module m_waqpb_data
      integer      nsubsm, nitemm, nfortm, nprocm, nfluxm, & 
                  nconfm, ninpum, noutpm, noutfm, nmodvm, & 
                  nstocm, nvelom, ndispm, nsgrpm, ncnsbm
      parameter   (nsubsm=3000, nitemm=30000, nfortm=800, nmodvm=4000, & 
                  nprocm=3000, nfluxm=   1, nconfm=20, ninpum=30000, & 
                  noutpm=15000, noutfm=6000, nstocm=9000, nvelom=3000, & 
                  ndispm=3000, nsgrpm=20  , ncnsbm=4000)
      integer      nsubs , nitem , nfort , nproc , nflux , & 
                  nconf , ninpu , noutp , noutf , nstoc , & 
                  nvelo , ndisp , nsgrp , ncnsb , nmodv , & 
                  ncnpr
      common /dims/ & 
                  nsubs , nitem , nfort , nproc , nflux , & 
                  nconf , ninpu , noutp , noutf , nstoc , & 
                  nvelo , ndisp , nsgrp , ncnsb , nmodv , & 
                  ncnpr

!          Substances groups table P1

      character(len=30) sgrpid(nsgrpm)
      character(len=50) sgrpnm(nsgrpm)

      common /sgrp/ sgrpid, sgrpnm

!          Substances table obsolete, included in P2

      character(len=10) subsid(nsubsm)
!          character*2  subswk(nsubsm)
!          character*10 subsgr(nsubsm)
!          character*20 subsun(nsubsm)
!          character*50 subsnm(nsubsm)

      common /subs/ subsid
!          common /subs/ subswk, subsid, subsgr, subsun, subsnm

!          Items table P2

      character(len=1)  itemse(nitemm), itemex(nitemm)
      character(len=10) itemid(nitemm)
      character(len=20) itemun(nitemm)
      character(len=50) itemnm(nitemm)
      real         itemde(nitemm)
      character(len=1)  itemwk(nitemm)
      character(len=10) itemag(nitemm), & 
                  itemda(nitemm)
      character(len=30) itemgr(nitemm)
      integer      item_i(nitemm)

      common /item/ itemse, itemex, itemid, itemun, itemnm, & 
                   itemde, itemwk, itemag, itemda, itemgr, & 
                   item_i

!          FORTRAN subroutines table P3

      character(len=10) fortid(nfortm)
      integer      fort_i(nfortm)

      common /fort/ fortid, fort_i

!          Processes table P4

      character(len=10) procid(nprocm), procfo(nprocm)
      character(len=50) procnm(nprocm)
      integer      procco(nprocm), proc_i(nprocm)

      common /proc/ procid, procfo, procnm, procco, proc_i

!          Fluxes table obsolete, included in P2

!          character*10 fluxid(nfluxm)
!          character*20 fluxun(nfluxm)
!          character*50 fluxnm(nfluxm)
!     
!          common /flux/ fluxid, fluxun, fluxnm

!          Configurations table P5

      character(len=10) confid(nconfm)
      character(len=50) confnm(nconfm)

      common /conf/ confid, confnm

!          internal license on conf

      integer      liconf(nconfm)

      common /lconf/ liconf

!          Configurations and processes matrix R1

      logical      conpro(nconfm,nprocm)
      integer      icnpro(nconfm*nprocm)
      character(len=10) r1_cid(nconfm*nprocm)
      character(len=10) r1_pid(nconfm*nprocm)

      common /copr/ conpro,icnpro, r1_cid, r1_pid

!          Configurations and substances table R2

      character(len=10) r2_cid(ncnsbm)
      character(len=10) r2_sid(ncnsbm)
      integer      r2_iin(ncnsbm)

      common /tabr2/ r2_cid,r2_sid,r2_iin

!          Input items table R3

      character(len=10) inpupr(ninpum), inpuit(ninpum)
      character(len=1)  inpude(ninpum), inpudo(ninpum)
      integer      inpunm(ninpum), inpusx(ninpum), & 
                  inpu_i(ninpum)
      integer      inpuii(ninpum), inpupi(ninpum)

      common /inpu/ inpupr, inpuit, inpude, inpudo, inpunm, & 
                   inpusx, inpu_i, inpuii, inpupi

!          Output items table R4

      character(len=10) outppr(noutpm), outpit(noutpm)
      character(len=1)  outpdo(noutpm)
      integer      outpnm(noutpm), outpsx(noutpm), & 
                  outp_i(noutpm)
      integer      outpii(noutpm), outppi(noutpm)

      common /outp/ outppr, outpit, outpdo, outpnm, outpsx, outp_i, & 
                   outpii, outppi

!          Output fluxes table R5

      character(len=10) outfpr(noutfm), outffl(noutfm)
      character(len=1)  outfdo(noutfm)
      integer      outfnm(noutfm), outf_i(noutfm)

      common /outf/ outfpr, outffl, outfdo, outfnm, outf_i

!          Stochi lines table R6

      character(len=10) stocfl(nstocm), stocsu(nstocm)
      real         stocsc(nstocm)
      integer      stoc_i(nstocm)

      common /stoc/ stocfl, stocsu, stocsc, stoc_i

!          Velocity lines table R7

      character(len=10) veloit(nvelom), velosu(nvelom)
      real         velosc(nvelom)
      integer      velo_i(nvelom)

      common /velo/ veloit, velosu, velosc, velo_i

!          Dispersion lines table R8

      character(len=10) dispit(ndispm), dispsu(ndispm)
      real         dispsc(ndispm)
      integer      disp_i(ndispm)

      common /disp/ dispit, dispsu, dispsc, disp_i

!          Modelled variables table R9

      character(len=10) modvci(nmodvm), modvit(nmodvm)

      common /modv/ modvit, modvci

!          old_items

      integer, parameter :: n_old_items_max = 1000                       ! maximum number of old items
      integer            :: n_old_items                                  ! number of old items
      character(len=10)  :: old_items_old_name(n_old_items_max)          ! old name (if equal to new name then use old_default if target serial is less then
      character(len=10)  :: old_items_new_name(n_old_items_max)          ! new name
      real               :: old_items_old_default(n_old_items_max)       ! old default value
      character(len=10)  :: old_items_configuration(n_old_items_max)     ! (only use this new name if a specific configuration is used?)
      integer            :: old_items_serial(n_old_items_max)            ! the proces definition serial number up to where this old name, old default was used
      integer            :: old_items_action_type(n_old_items_max)       ! process rename, process parameter rename, default value change

      common /old_items/ n_old_items, & 
                        old_items_old_name, & 
                        old_items_new_name, & 
                        old_items_old_default, & 
                        old_items_configuration, & 
                        old_items_serial, & 
                        old_items_action_type
      end module m_waqpb_data
