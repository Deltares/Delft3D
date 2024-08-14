!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

!
!

   !
   ! ========================================================================================
   !
   subroutine inipointers_erosed()
      use m_fm_erosed
      use m_flowgeom, only: lnx
      implicit none
      integer :: ierr

      if (.not. stm_included) return

      ! mtd: Pointer to dummies to fill later
      dzbdt => mtd%dzbdt
      seddif => mtd%seddif
      blchg => mtd%blchg
      sed => mtd%sed
      ws => mtd%ws
      uau => mtd%uau

      ! stmpar
      lsed => stmpar%lsedsus
      lsedtot => stmpar%lsedtot

      ! sedpar
      nmudfrac => stmpar%sedpar%nmudfrac
      max_mud_sedtyp => stmpar%sedpar%max_mud_sedtyp
      cmpupdfrac => stmpar%sedpar%cmpupdfrac
      rhosol => stmpar%sedpar%rhosol
      cdryb => stmpar%sedpar%cdryb
      logseddia => stmpar%sedpar%logseddia
      logsedsig => stmpar%sedpar%logsedsig
      sedd10 => stmpar%sedpar%sedd10
      sedd50 => stmpar%sedpar%sedd50
      sedd90 => stmpar%sedpar%sedd90
      sedd50fld => stmpar%sedpar%sedd50fld
      dstar => stmpar%sedpar%dstar
      taucr => stmpar%sedpar%taucr
      tetacr => stmpar%sedpar%tetacr
      mudcnt => stmpar%sedpar%mudcnt
      pmcrit => stmpar%sedpar%pmcrit
      nseddia => stmpar%sedpar%nseddia
      sedtyp => stmpar%sedpar%sedtyp
      tratyp => stmpar%sedpar%tratyp
      anymud => stmpar%sedpar%anymud
      sedtrcfac => stmpar%sedpar%sedtrcfac
      bsskin => stmpar%sedpar%bsskin
      thcmud => stmpar%sedpar%thcmud
      tpsnumber => stmpar%sedpar%tpsnumber
      dss => stmpar%sedpar%dss
      !
      max_mud_sedtyp => stmpar%sedpar%max_mud_sedtyp
      min_dxx_sedtyp => stmpar%sedpar%min_dxx_sedtyp
      flocmod => stmpar%sedpar%flocmod
      nflocpop => stmpar%sedpar%nflocpop
      nflocsizes => stmpar%sedpar%nflocsizes
      floclist => stmpar%sedpar%floclist
      tbreakup => stmpar%sedpar%tbreakup
      tfloc => stmpar%sedpar%tfloc

      ! morpar
      thresh => stmpar%morpar%thresh
      sus => stmpar%morpar%sus
      suscorfac => stmpar%morpar%suscorfac
      bed => stmpar%morpar%bed
      susw => stmpar%morpar%susw
      sedthr => stmpar%morpar%sedthr
      bedw => stmpar%morpar%bedw
      i10 => stmpar%morpar%i10
      i15 => stmpar%morpar%i15
      i50 => stmpar%morpar%i50
      i90 => stmpar%morpar%i90
      nxx => stmpar%morpar%nxx
      xx => stmpar%morpar%xx
      multi => stmpar%morpar%multi
      eqmbcsand => stmpar%morpar%eqmbcsand
      eqmbcmud => stmpar%morpar%eqmbcmud
      factcr => stmpar%morpar%factcr
      factsd => stmpar%morpar%factsd
      ihidexp => stmpar%morpar%ihidexp
      asklhe => stmpar%morpar%asklhe
      mwwjhe => stmpar%morpar%mwwjhe
      ffthresh => stmpar%morpar%thresh
      morfac => stmpar%morpar%morfac
      varyingmorfac => stmpar%morpar%varyingmorfac
      morft => stmpar%morpar%morft
      hydrt => stmpar%morpar%hydrt
      espir => stmpar%morpar%espir
      epspar => stmpar%morpar%epspar
      camax => stmpar%morpar%camax
      aksfac => stmpar%morpar%aksfac
      rdc => stmpar%morpar%rdc
      iopkcw => stmpar%morpar%iopkcw
      oldmudfrac => stmpar%morpar%oldmudfrac
      sinkf => stmpar%morpar%flufflyr%sinkf
      sourf => stmpar%morpar%flufflyr%sourf
      iflufflyr => stmpar%morpar%flufflyr%iflufflyr
      depfac => stmpar%morpar%flufflyr%depfac
      mfluff => stmpar%morpar%flufflyr%mfluff
      alfabs => stmpar%morpar%alfabs
      alfabn => stmpar%morpar%alfabn
      wetslope => stmpar%morpar%wetslope
      avaltime => stmpar%morpar%avaltime
      duneavalan => stmpar%morpar%duneavalan
      dryslope => stmpar%morpar%dryslope
      hswitch => stmpar%morpar%hswitch
      dzmaxdune => stmpar%morpar%dzmaxdune
      ashld => stmpar%morpar%ashld
      bshld => stmpar%morpar%bshld
      cshld => stmpar%morpar%cshld
      dshld => stmpar%morpar%dshld
      alfpa => stmpar%morpar%alfpa
      thcrpa => stmpar%morpar%thcrpa
      islope => stmpar%morpar%islope
      ti_sedtrans => stmpar%morpar%ti_sedtrans
      tmor => stmpar%morpar%tmor
      tcmp => stmpar%morpar%tcmp
      itmor => stmpar%morpar%itmor
      bedupd => stmpar%morpar%bedupd
      neglectentrainment => stmpar%morpar%neglectentrainment
      dzmax => stmpar%morpar%dzmax
      hmaxth => stmpar%morpar%hmaxth
      thetsd => stmpar%morpar%thetsd
      eulerisoglm => stmpar%morpar%eulerisoglm
      l_suscor => stmpar%morpar%l_suscor
      bermslopetransport => stmpar%morpar%bermslopetransport
      bermslopebed => stmpar%morpar%bermslopebed
      bermslopesus => stmpar%morpar%bermslopesus
      bermslope => stmpar%morpar%bermslope
      bermslopefac => stmpar%morpar%bermslopefac
      bermslopegamma => stmpar%morpar%bermslopegamma
      bermslopedepth => stmpar%morpar%bermslopedepth

      ! trapar
      iform => stmpar%trapar%iform
      par => stmpar%trapar%par
      npar => stmpar%trapar%npar
      max_integers => stmpar%trapar%max_integers
      max_reals => stmpar%trapar%max_reals
      max_strings => stmpar%trapar%max_strings
      dll_function => stmpar%trapar%dll_function
      dll_handle => stmpar%trapar%dll_handle
      dll_integers => stmpar%trapar%dll_integers
      dll_reals => stmpar%trapar%dll_reals
      dll_strings => stmpar%trapar%dll_strings
      dll_usrfil => stmpar%trapar%dll_usrfil

      ! sedtra
      aks => sedtra%aks
      bc_mor_array => sedtra%bc_mor_array
      dbodsd => sedtra%dbodsd
      dcwwlc => sedtra%dcwwlc
      dm => sedtra%dm
      dg => sedtra%dg
      dgsd => sedtra%dgsd
      dxx => sedtra%dxx
      e_dzdn => sedtra%e_dzdn
      e_dzdt => sedtra%e_dzdt
      epsclc => sedtra%epsclc
      epswlc => sedtra%epswlc
      fixfac => sedtra%fixfac
      frac => sedtra%frac
      kfsed => sedtra%kfsed
      kmxsed => sedtra%kmxsed
      mudfrac => sedtra%mudfrac
      sandfrac => sedtra%sandfrac
      hidexp => sedtra%hidexp
      rsdqlc => sedtra%rsdqlc
      rsedeq => sedtra%rsedeq
      sbcx => sedtra%sbcx
      sbcy => sedtra%sbcy
      e_sbcn => sedtra%e_sbcn
      e_sbct => sedtra%e_sbct
      e_sbn => sedtra%e_sbn
      e_sbt => sedtra%e_sbt
      e_ssn => sedtra%e_ssn
      e_sst => sedtra%e_sst
      e_sbnc => sedtra%e_sbnc
      e_sbtc => sedtra%e_sbtc
      e_ssnc => sedtra%e_ssnc
      e_scrn => sedtra%e_scrn
      e_scrt => sedtra%e_scrt
      sbwx => sedtra%sbwx
      sbwy => sedtra%sbwy
      sscx => sedtra%sscx
      sscy => sedtra%sscy
      e_sbwn => sedtra%e_sbwn
      e_sbwt => sedtra%e_sbwt
      sddflc => sedtra%sddflc
      sswx => sedtra%sswx
      sswy => sedtra%sswy
      e_sswn => sedtra%e_sswn ! add correction part later on e_scrn, e_scrt
      e_sswt => sedtra%e_sswt
      sxtot => sedtra%sxtot
      sytot => sedtra%sytot
      sbxcum => sedtra%sbxcum
      sbycum => sedtra%sbycum
      ssxcum => sedtra%ssxcum
      ssycum => sedtra%ssycum
      sinkse => sedtra%sinkse
      sourse => sedtra%sourse
      sour_im => sedtra%sour_im
      srcmax => sedtra%srcmax
      taub => sedtra%taub
      taurat => sedtra%taurat
      ust2 => sedtra%ust2
      umod => sedtra%umod
      uuu => sedtra%uuu
      vvv => sedtra%vvv
      wslc => sedtra%wslc
      zumod => sedtra%zumod
      rca => sedtra%rca
      statqnt => sedtra%statqnt

      allocate (q_zeta(2, lnx), stat=ierr)
      q_zeta = 0d0

   end subroutine inipointers_erosed
