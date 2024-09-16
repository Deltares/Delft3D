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

 subroutine flow_allocflow() ! initialise flow model time independent parameters
    use m_netw, only: kn
    use m_flowgeom
    use m_flow
    use m_flowtimes
    use m_missing
    use unstruc_model
    use m_netw, only: numk, numl
    use m_alloc
    use m_waves
    use fm_external_forcings_data, only: nbndw
    use m_sediment
    use m_ship
    use m_sferic
    use m_partitioninfo
    use m_integralstats
    use unstruc_channel_flow
    use m_bedform
    use m_fm_erosed, only: ucxq_mor, ucyq_mor, hs_mor
    use m_hydrology, only: jadhyd, alloc_hydrology, init_hydrology
    use m_qnerror
    use m_get_zlayer_indices
    use m_get_zlayer_indices_bobL

    implicit none
    integer :: ierr, n, k, mxn, j, kk, LL, L, k1, k2, k3, n1, n2, n3, n4, kb1, kb2, numkmin, numkmax, kbc1, kbc2
    integer :: nlayb, nrlay, nlayb1, nrlay1, nlayb2, nrlay2, Lb, Lt, mx, ltn, mpol, Lt1, Lt2, Ldn
    integer :: laybed, laytop, nrlayL, Lf, kuni, kb
    integer :: nlayb1L, nrlay1L, nlayb2L, nrlay2L
    integer :: ndx1d

    double precision :: zmn, zmx, dzm ! for 3D
    double precision :: gf, w1, w2, w3, zbt, zbb, dzb, gfi, gfk
    logical :: jawel

    if (ndx == 0) return

! if ( jampi.eq.1 ) then
!!   synchronise bed level
!    call update_ghosts(ITYPE_SALL, 1, Ndx, bl, ierror)
! end if

    call ilowercase(md_netfile) ! INTERACTOR!

! node related
    if (allocated(s0)) then
       deallocate (s0, s1, a0, a1, hs, s00, cfs)
    end if
    if (allocated(negativeDepths)) then
       deallocate (negativeDepths, negativeDepths_cum, noIterations, noIterations_cum, &
                   limitingTimestepEstimation, limitingTimestepEstimation_cum, flowCourantNumber)
    end if

    allocate (s0(ndx), s1(ndx), stat=ierr)
    call aerr('s0  (ndx) , s1  (ndx)', ierr, 2 * ndx); s0 = 0; s1 = 0
    allocate (a0(ndx), a1(ndx), stat=ierr)
    call aerr('a0  (ndx) , a1  (ndx)', ierr, 2 * ndx); a0 = 0; a1 = 0
    allocate (hs(ndx), s00(ndx), stat=ierr)
    call aerr('hs  (ndx) , s00 (ndx)', ierr, 2 * ndx); hs = 0; s00 = 0
    allocate (cfs(ndx), stat=ierr)
    call aerr('cfs (ndx)', ierr, ndx); cfs = 0

    if (jamapFlowAnalysis > 0) then
       allocate (negativeDepths(ndx), stat=ierr)
       call aerr('negativeDepths (ndx)', ierr, ndx)
       negativeDepths = 0
       allocate (negativeDepths_cum(ndx), stat=ierr)
       call aerr('negativeDepths_cum (ndx)', ierr, ndx)
       negativeDepths_cum = 0
       allocate (noIterations(ndx), stat=ierr)
       call aerr('noIterations (ndx)', ierr, ndx)
       noIterations = 0
       allocate (noIterations_cum(ndx), stat=ierr)
       call aerr('noIterations_cum (ndx)', ierr, ndx)
       noIterations_cum = 0
       allocate (limitingTimestepEstimation(ndx), stat=ierr)
       call aerr('limitingTimestepEstimation (ndx)', ierr, ndx)
       limitingTimestepEstimation = 0
       allocate (limitingTimestepEstimation_cum(ndx), stat=ierr)
       call aerr('limitingTimestepEstimation_cum (ndx)', ierr, ndx)
       limitingTimestepEstimation_cum = 0
       allocate (flowCourantNumber(ndx), stat=ierr)
       call aerr('flowCourantNumber (ndx)', ierr, ndx)
       flowCourantNumber = 0d0
    end if

    if (allocated(kbot)) then
       deallocate (kbot, ktop, ktop0, kmxn, Lbot, Ltop, kmxL)
    end if
    allocate (kbot(ndx), stat=ierr)
    call aerr('kbot (ndx)', ierr, ndx)
    allocate (ktop(ndx), stat=ierr)
    call aerr('ktop (ndx)', ierr, ndx)
    allocate (ktop0(ndx), stat=ierr)
    call aerr('ktop0(ndx)', ierr, ndx)
    allocate (kmxn(ndx), stat=ierr)
    call aerr('kmxn (ndx)', ierr, ndx)
    allocate (Lbot(Lnx), stat=ierr)
    call aerr('Lbot (Lnx)', ierr, Lnx)
    allocate (Ltop(Lnx), stat=ierr)
    call aerr('Ltop (Lnx)', ierr, Lnx)
    allocate (kmxL(Lnx), stat=ierr)
    call aerr('kmxL (Lnx)', ierr, Lnx)

    if (allocated(ustb)) deallocate (ustb, ustw)
    allocate (ustb(lnx), stat=ierr)
    call aerr('ustb  (lnx)', ierr, lnx); ustb = 0
    allocate (ustw(lnx), stat=ierr)
    call aerr('ustw  (lnx)', ierr, lnx); ustw = 0

    if (allocated(laydefnr)) deallocate (laydefnr, laytyp, laymx)
    allocate (laydefnr(ndx), stat=ierr)
    call aerr('laydefnr(ndx)', ierr, ndx)
    allocate (laytyp(mxlaydefs), stat=ierr)
    call aerr('laytyp(mxlaydefs)', ierr, mxlaydefs)
    allocate (laymx(mxlaydefs), stat=ierr)
    call aerr('laymx(mxlaydefs)', ierr, mxlaydefs)

    if (layertype >= 2) then
       if (allocated(nlaybn)) deallocate (nlaybn, nrlayn)
       allocate (nlaybn(ndx), stat=ierr); nlaybn = 0
       call aerr('nlaybn(ndx)', ierr, ndx)
       allocate (nrlayn(ndx), stat=ierr); nrlayn = 0
       call aerr('nrlayn(ndx)', ierr, ndx)
    end if

    do k = 1, Ndx
       kbot(k) = k
       ktop(k) = k
       kmxn(k) = 1
    end do
    do L = 1, Lnx
       Lbot(L) = L
       Ltop(L) = L
       kmxL(L) = 1
    end do

    if (kmx > 0) then

       numkmin = int(1d8); numkmax = -numkmin
       do Lf = Lnx1D + 1, Lnx ! we only need netnode nrs in 2D, todo: trim to numkmin
          L = ln2lne(Lf)
          if (kn(3, L) == 2) then
             numkmin = min(numkmin, kn(1, L), kn(2, L))
             numkmax = max(numkmax, kn(1, L), kn(2, L))
          end if
       end do

       numkmax = numk
       if (allocated(kbotc)) then
          deallocate (kbotc, kmxc)
       end if
       allocate (kbotc(numkmax), stat=ierr) ! may also be numkmin -> numkmax
       call aerr('kbotc(numkmax)', ierr, numkmax)
       allocate (kmxc(numkmax), stat=ierr)
       call aerr('kmxc (numkmax)', ierr, numkmax)

       kbot = 1; ktop = 1; kmxn = 1
       Lbot = 1; Ltop = 1; kmxL = 1
       kbotc = 1; kmxc = 1

       mxlays = kmx; numvertdis = 3 ! mxlayz = 6
       mxlaydefs = numvertdis; mx = 0; 
       laydefnr = 1

       if (layertype == 3) then
          inquire (file=md_vertplizfile, exist=jawel)
          if (jawel) then
             call oldfil(mpol, md_vertplizfile)
          else
             call qnerror('vertical_layering.pliz not found, switch back to sigma', ' ', ' ')
             layertype = 1
          end if
       end if

       if (layertype == 1 .or. layertype == 4) then ! all sigma
          mxlaydefs = 1
          laytyp(1) = 1
          laymx(1) = kmx
          if (layertype == 4) then
             if (allocated(dkx)) deallocate (dkx, sdkx)
             allocate (sdkx(ndx), dkx(ndx))
          end if
       else if (layertype == 2) then ! all z
          mxlaydefs = 1
          laytyp(1) = 2

          if (zlaybot == dmiss) then
             zmn = bl(1)
             do n = 2, ndx
                zmn = min(bl(n), zmn)
             end do
          else
             zmn = zlaybot
          end if

          if (jampi == 1) then
             call reduce_double_min(zmn)
          end if

          if (iStrchType >= 0) then
             if (zlaytop == dmiss) then
                zmx = sini
             else
                zmx = zlaytop
             end if
          else
             if (Floorlevtoplay == dmiss) then
                zmx = sini
             else
                if (jaorgFloorlevtoplaydef == 1) then
                   zmx = Floorlevtoplay
                else
                   if (dztop == dmiss) then
                      zmx = Floorlevtoplay
                   else
                      zmx = Floorlevtoplay + dztop
                   end if
                end if
             end if
          end if

          if (dztopuniabovez == dmiss) then
             zbt = zmn
          else
             zbt = max(zmn, dztopuniabovez)
          end if

          if (dztop == dmiss) then
             dzm = (zmx - zbt) / mxlayz
          else
             dzm = dztop; mxlayz = (zmx - zbt) / dzm
             if (numtopsig > 0 .and. janumtopsiguniform == 1) then
                mxlayz = max(mxlayz, numtopsig)
             end if
          end if

          kuni = mxlayz; mx = kuni
          if (zbt > zmn) then ! count extra layers needed to fill out till bottom

             zbb = zbt; dzb = dzm
             do while (zbb > zmn .and. mx < kmxx - 1)
                dzb = dzb * sigmagrowthfactor
                zbb = zbb - dzb
                mx = mx + 1
             end do

          end if

          dzm = max(dzm, 1d-2)
          ! toplayminthick = 0.01d0  ! 0.5d0*dzm
          mxlayz = mx
          kmx = mx ! repair code
          laymx(1) = mx
       else if (layertype == 3) then ! combination in polygons
          call polygonlayering(mpol)
       end if
       do k = 1, mxlaydefs
          mx = max(mx, laymx(k))
       end do
       if (allocated(zslay)) deallocate (zslay, dzslay)
       allocate (zslay(0:mx, mxlaydefs), stat=ierr) ! nr of layer distributions
       allocate (dzslay(0:mx, mxlaydefs), stat=ierr); dzslay = 0d0

       if (iStrchType == STRCH_USER) then
          do j = 1, mxlaydefs
             mx = laymx(j)
             do k = 1, mx
                dzslay(k, j) = laycof(k) / 100d0
             end do
          end do

       elseif (iStrchType == STRCH_EXPONENT) then
          gfi = 1d0 / laycof(2)
          gf = laycof(3)
          do j = 1, mxlaydefs
             mx = laymx(j)
             k1 = laycof(1) * mx
             gfk = gfi**k1
             if (gfk == 1d0) then
                gfi = 1d0
                dzslay(1, j) = 1d0 / mx
             else
                dzslay(1, j) = (1d0 - gfi) / (1d0 - gfk) * laycof(1)
             end if
             do k = 2, k1
                dzslay(k, j) = dzslay(k - 1, j) * gfi
             end do
             gfk = gf**(kmx - k1)
             if (k1 < kmx) then
                if (gfk == 1d0) then
                   gf = 1d0
                   dzslay(k1 + 1, j) = 1d0 / mx
                else
                   dzslay(k1 + 1, j) = (1d0 - gf) / (1d0 - gfk) * (1d0 - laycof(1))
                end if
                do k = k1 + 2, mx
                   dzslay(k, j) = dzslay(k - 1, j) * gf
                end do
             end if
          end do
       else
          do j = 1, mxlaydefs
             mx = laymx(j)
             do k = 1, mx
                dzslay(k, j) = 1d0 / mx
             end do
          end do
       end if

       do j = 1, mxlaydefs
          mx = laymx(j)
          if (laytyp(j) == 1) then

             zslay(0, j) = 0d0
             do k = 1, mx
                zslay(k, j) = zslay(k - 1, j) + dzslay(k, j)
             end do

          else if (laytyp(j) == 2) then

             if (allocated(zslay)) deallocate (zslay)
             allocate (zslay(0:mx, mxlaydefs), stat=ierr) ! nr of layer distributions

             if (iStrchType >= 0) then
                zslay(0, j) = zmn
                do k = 1, mx
                   zslay(k, j) = zslay(k - 1, j) + dzslay(k, j) * (zmx - zmn)
                end do
             else
                zslay(0, j) = zmn; zslay(mx, j) = zmx
                do k = mx - 1, mx - kuni, -1
                   zslay(k, j) = zslay(k + 1, j) - dzm
                end do

                dzb = dzm
                do k = mx - kuni - 1, 1, -1 ! Carefull attention by Julien, kuni minus one
                   dzb = dzb * sigmagrowthfactor
                   zslay(k, j) = zslay(k + 1, j) - dzb
                end do
             end if
          end if
       end do

       kk = Ndx
       do n = 1, ndx

          kbot(n) = 0
          kk = kk + 1 ! spoke cell for everyone

          Ldn = laydefnr(n)
          if (Ldn >= 1) then
             if (laytyp(Ldn) == 1) then
                mx = laymx(Ldn)
                kmxn(n) = mx
             else if (laytyp(Ldn) == 2) then
                call getzlayerindices(n, nlayb, nrlay)
                kmxn(n) = nrlay
                ! mx  = laymx(Ldn)
                ! do k = 1,mx
                !   if ( zslay(k,Ldn) > bl(n) ) then
                !       kmxn(n) = mx - k + 1
                !       exit
                !   endif
                ! enddo
             end if
          end if

       end do

       kk = Ndx
       do n = 1, ndx ! Count ndkx + set kbot array

          kbot(n) = 0
          kk = kk + 1

          Ldn = laydefnr(n)
          if (Ldn == 0) then
             k1 = indlaynod(1, n); k2 = indlaynod(2, n); k3 = indlaynod(3, n)
             w1 = wflaynod(1, n); w2 = wflaynod(2, n); w3 = wflaynod(3, n)
             kmxn(n) = max(1, nint(w1 * kmxn(k1) + w2 * kmxn(k2) + w3 * kmxn(k3)))

             ! kmxn(n) = max( kmxn(k1), kmxn(k2), kmxn(k3) )
          end if

          do k = 1, kmxn(n)
             kk = kk + 1
             if (k == 1) then
                kbot(n) = kk
             end if
          end do
       end do
       ndkx = kk

       LL = Lnx ! Stapelen vanaf grondlaag
       do L = 1, lnx
          n1 = ln(1, L); n2 = ln(2, L)
          kmxL(L) = min(kmxn(n1), kmxn(n2)) ! 30-04       !  kmxL(L) = max ( kmxn(n1), kmxn(n2) )

          if (jaZlayeratubybob == 1 .and. kmxL(L) > numtopsig) then
             call getzlayerindicesbobL(n1, nlayb1, nrlay1, min(bob(1, L), bob(2, L)))
             call getzlayerindicesbobL(n2, nlayb2, nrlay2, min(bob(1, L), bob(2, L)))
             kmxL(L) = min(nrlay1, nrlay2)
          end if

          if (abs(kcu(L)) == 2) then
             n3 = lncn(1, L); n4 = lncn(2, L)
             kmxc(n3) = max(kmxc(n3), kmxL(L))
             kmxc(n4) = max(kmxc(n4), kmxL(L))
          end if

          do k = 0, kmxL(L)
             LL = LL + 1
          end do
       end do
       Lnkx = LL

       call realloc(ln, (/2, Lnkx/))
       call realloc(lncn, (/2, Lnkx/))

       LL = Lnx ! Stapelen vanaf grondlaag

       kk = numk ! setup cornerpoint admin
       do n = numkmin, numkmax
          do k = 0, kmxc(n)
             kk = kk + 1
             if (k == 1) then
                kbotc(n) = kk
             end if
          end do
       end do
       call realloc(ucnx, kk)
       call realloc(ucny, kk)

       do L = 1, lnx
          n1 = ln(1, L); n2 = ln(2, L)
          n3 = lncn(1, L); n4 = lncn(2, L)

          Lt1 = 0; Lt2 = 0
          if (laydefnr(n1) > 0 .and. laydefnr(n2) > 0) then
             Lt1 = laytyp(laydefnr(n1)); Lt2 = laytyp(laydefnr(n2))
          end if

          if (Lt1 == 2 .and. Lt2 == 2) then

             call getzlayerindices(n1, nlayb1, nrlay1) ! connection to be made at bedcell of highest adjacent cell
             call getzlayerindices(n2, nlayb2, nrlay2)
             kb1 = max(0, nlayb2 - nlayb1)
             kb2 = max(0, nlayb1 - nlayb2)

             if (jaZlayeratubybob == 1 .and. kmxL(L) > numtopsig) then ! min(nrlay1,nrlay2) > numtopsig ) then ! connection to be made at first cell above local bob
                ! but not if you are in numtopsig country
                call getzlayerindicesbobL(n1, nlayb1L, nrlay1L, min(bob(1, L), bob(2, L)))
                call getzlayerindicesbobL(n2, nlayb2L, nrlay2L, min(bob(1, L), bob(2, L)))
                kb1 = nlayb1L - nlayb1
                kb2 = nlayb2L - nlayb2

             end if

             laybed = max(nlayb1, nlayb2)
             laytop = min(nlayb1 + nrlay1, nlayb2 + nrlay2) ! should be identical for n1,n2,n3,n4
             nrlayL = laytop - laybed + 1

             kbc1 = kmxc(n3) - nrlayL
             kbc2 = kmxc(n4) - nrlayL

          else
             kb1 = 0; kb2 = 0 ! linking starts at kbot(n1) + kb1 on left and at kbot(n2) + kb2 on right
             kbc1 = 0; kbc2 = 0
          end if

          do k = 0, kmxL(L) ! 1 extra below bedlayer k = 1
             LL = LL + 1
             if (k == 1) then
                Lbot(L) = LL
             end if
             if (k > 0) then
                ln(1, LL) = kbot(n1) + kb1
                ln(2, LL) = kbot(n2) + kb2
                kb1 = kb1 + 1; kb1 = min(kb1, kmxn(n1))
                kb2 = kb2 + 1; kb2 = min(kb2, kmxn(n2))

                if (abs(kcu(L)) == 2) then
                   lncn(1, LL) = kbotc(n3) + kbc1
                   lncn(2, LL) = kbotc(n4) + kbc2
                   kbc1 = kbc1 + 1; kbc1 = min(kbc1, kmxc(n3))
                   kbc2 = kbc2 + 1; kbc2 = min(kbc2, kmxc(n4))
                end if

             end if
          end do
       end do

       call realloc(ln0, (/2, Lnkx/))
       ln0 = ln

       do LL = 1, lnx ! only checking
          Lb = Lbot(LL)
          Lt = Lb + kmxL(LL) - 1
          n1 = ln(1, LL); n2 = ln(2, LL)
          do L = Lb, Lt
             k1 = ln(1, L); k2 = ln(2, L)
             if (k1 > kbot(n1) + kmxn(n1) - 1) then
                ln(1, L) = k1
             end if
             if (k2 > kbot(n2) + kmxn(n2) - 1) then
                ln(2, L) = k2
             end if
          end do
       end do

    else
       ndkx = ndx
       Lnkx = Lnx
    end if

    if (allocated(ucx)) then ! basic flow arrays
       deallocate (ucx, ucy, uqcx, uqcy, ucxq, ucyq, qin, vih, dvxc, dvyc, squ, sqi, sq, sqa, vol0, vol1, vol1_f, volerror)
    end if
    allocate (ucx(ndkx), ucy(ndkx), stat=ierr)
    call aerr('ucx (ndkx) , ucy (ndkx)', ierr, 2 * ndkx); ucx = 0; ucy = 0
    allocate (uqcx(ndkx), uqcy(ndkx), stat=ierr)
    call aerr('uqcx(ndkx) , uqcy(ndkx)', ierr, 2 * ndkx); uqcx = 0; uqcy = 0
    allocate (ucxq(ndkx), ucyq(ndkx), stat=ierr)
    call aerr('ucxq(ndkx) , ucyq(ndkx)', ierr, 2 * ndkx); ucxq = 0; ucyq = 0
    if (jamapucvec == 1 .or. jamapucmag == 1 .or. jahisvelocity == 1 .or. len_trim(md_foufile) > 0 .or. allocated(map_classes_ucmag)) then
       call realloc(ucmag, ndkx, keepExisting=.false.)
    end if
    allocate (qin(ndkx), vih(ndkx), stat=ierr)
    call aerr('qin (ndkx) , vih (ndkx)', ierr, 2 * ndkx); qin = 0; vih = 0
    allocate (dvxc(ndkx), dvyc(ndkx), stat=ierr)
    call aerr('dvxc(ndkx) , dvyc(ndkx)', ierr, 2 * ndkx); dvxc = 0; dvyc = 0
    allocate (squ(ndkx), stat=ierr)
    call aerr('squ (ndkx)', ierr, ndkx); squ = 0
    allocate (sqi(ndkx), stat=ierr)
    call aerr('sqi (ndkx)', ierr, ndkx); sqi = 0
    allocate (sq(ndkx), stat=ierr)
    call aerr('sq  (ndkx)', ierr, ndx); sq = 0
    allocate (sqa(ndkx), stat=ierr)
    call aerr('sqa (ndkx)', ierr, ndx); sq = 0
    allocate (vol0(ndkx), stat=ierr)
    call aerr('vol0(ndkx)', ierr, ndkx); vol0 = 0
    allocate (vol1(ndkx), stat=ierr)
    call aerr('vol1(ndkx)', ierr, ndkx); vol1 = 0
    allocate (vol1_f(ndkx), stat=ierr)
    call aerr('vol1_f(ndkx)', ierr, ndkx); vol1_f = 0
    allocate (volerror(ndkx), stat=ierr)
    call aerr('volerror(ndkx)', ierr, ndx); volerror = 0

    if (stm_included .or. bfm_included .or. bfmpar%lfbedfrmrou .or. jatrt > 0) then
       allocate (ucxq_mor(1:ndkx), ucyq_mor(1:ndkx), hs_mor(1:ndkx), stat=ierr)
       ucxq_mor = 0d0; ucyq_mor = 0d0; hs_mor = 0d0
       allocate (ucx_mor(1:ndkx), ucy_mor(1:ndkx), stat=ierr)
       ucx_mor = 0d0; ucy_mor = 0d0
    end if

    if (lnxi > 0 .and. kmx == 0) then
       call realloc(uc1D, ndx, keepExisting=.false., fill=0d0, stat=ierr)
       call aerr('uc1D(ndx)', ierr, ndx)
       call realloc(u1Du, lnx, keepExisting=.false., fill=0d0, stat=ierr)
       call aerr('u1Du(lnx)', ierr, lnx)
       if (japure1D >= 3) then
          call realloc(alpha_mom_1D, ndx, keepExisting=.false., fill=0d0, stat=ierr)
          call aerr('alpha_mom_1D', ierr, ndx)
          call realloc(alpha_ene_1D, ndx, keepExisting=.false., fill=0d0, stat=ierr)
          call aerr('alpha_ene_1D', ierr, ndx)
          call realloc(q1D, (/2, lnx/), keepExisting=.false., fill=0d0, stat=ierr)
          call aerr('q1D(2,lnx)', ierr, lnx)
          call realloc(au1D, (/2, lnx/), keepExisting=.false., fill=0d0, stat=ierr)
          call aerr('au1D(2,lnx)', ierr, lnx)
          call realloc(wu1D, (/2, lnx/), keepExisting=.false., fill=0d0, stat=ierr)
          call aerr('wu1D(2,lnx)', ierr, lnx)
          call realloc(sar1D, (/2, lnx/), keepExisting=.false., fill=0d0, stat=ierr)
          call aerr('sar1D(2,lnx)', ierr, lnx)
          call realloc(volu1D, lnx, keepExisting=.false., fill=0d0, stat=ierr)
          call aerr('volu1D(lnx)', ierr, lnx)
       end if
    end if

    if (allocated(dtcell)) then
       deallocate (dtcell)
    end if
    if (kmx > 0) then
       allocate (dtcell(ndkx), stat=ierr)
       call aerr('dtcell(ndkx)', ierr, ndkx); dtcell(:) = 0d0
    else
       allocate (dtcell(ndx), stat=ierr)
       call aerr('dtcell(ndx)', ierr, ndx); dtcell(:) = 0d0
    end if

    ! for 1D only
    if (network%loaded) then
       ndx1d = ndxi - ndx2d
       if (ndx1d > 0) then
          call realloc(time_wetground, ndx, keepExisting=.false., fill=0d0, stat=ierr)
          call aerr('time_wetground(ndx)', ierr, ndx)

          call realloc(freeboard, ndx1d, keepExisting=.false., fill=dmiss, stat=ierr)
          call aerr('freeboard(ndxi-ndx2d)', ierr, ndx1d)

          call realloc(hsOnGround, ndx1d, keepExisting=.false., fill=0d0, stat=ierr)
          call aerr('hsOnGround(ndxi-ndx2d)', ierr, ndx1d)

          call realloc(volOnGround, ndx1d, keepExisting=.false., fill=0d0, stat=ierr)
          call aerr('volOnGround(ndxi-ndx2d)', ierr, ndx1d)

          call realloc(qCur1d2d, ndx, keepExisting=.false., fill=0d0, stat=ierr)
          call aerr('qCur1d2d(ndx)', ierr, ndx)

          call realloc(vTot1d2d, ndx, keepExisting=.false., fill=0d0, stat=ierr)
          call aerr('vTot1d2d(ndx)', ierr, ndx)

          call realloc(qCurLat, ndx, keepExisting=.false., fill=0d0, stat=ierr)
          call aerr('qCurLat(ndx)', ierr, ndx)

          call realloc(vTotLat, ndx, keepExisting=.false., fill=0d0, stat=ierr)
          call aerr('vTotLat(ndx)', ierr, ndx)
       end if
       if (lnx1d > 0) then
          call realloc(s1Gradient, lnx, keepExisting=.false., fill=dmiss, stat=ierr)
          call aerr('s1Gradient', ierr, lnx)
       end if
    end if

    if (kmx > 0 .and. (ja_timestep_auto == 3 .or. ja_timestep_auto == 4)) then
       if (allocated(squ2D)) deallocate (squ2d)
       allocate (squ2D(ndkx), stat=ierr)
       call aerr('squ2D(ndkx)', ierr, ndkx); squ2D = 0
    end if

    if (ja_timestep_auto == 1 .and. ja_timestep_nostruct > 0) then
       if (allocated(squcor)) deallocate (squcor)
       allocate (squcor(ndx), stat=ierr)
       call aerr('squcor(ndx)', ierr, ndx); squcor = 0
    end if

    if (icorio == 7 .or. icorio == 8 .or. icorio == 27 .or. icorio == 28) then
       if (allocated(hus)) deallocate (hus)
       allocate (hus(ndkx), stat=ierr)
       call aerr('hus(ndkx)', ierr, ndkx); hus = 0
    end if
    if (kmx > 0) then
       if (allocated(ucz)) deallocate (ucz)
       allocate (ucz(ndkx), stat=ierr)
       call aerr('ucz (ndkx)', ierr, ndkx); ucz = 0
    end if

    if (allocated(rho)) deallocate (rho)
    allocate (rho(ndkx), stat=ierr)
    call aerr('rho (ndkx)', ierr, ndkx); rho = rhomean

    if (stm_included) then
       if (allocated(rhowat)) deallocate (rhowat)
       allocate (rhowat(ndkx), stat=ierr)
       call aerr('rhowat (ndkx)', ierr, ndkx); rhowat = rhomean
    end if

    if (jasal > 0 .or. jatem > 0 .or. jased > 0 .or. stm_included) then
       if (abs(jabaroctimeint) >= 2) then
          if (jacreep == 1 .or. abs(jabaroctimeint) == 3 .or. abs(jabaroctimeint) == 4) then
             if (allocated(dpbdx0)) deallocate (dpbdx0)
             allocate (dpbdx0(lnkx), stat=ierr)
             call aerr('dpbdx0 (lnkx)', ierr, lnkx); dpbdx0 = 0d0
          end if

          if (jacreep /= 1 .and. abs(jabaroctimeint) == 2 .or. abs(jabaroctimeint) == 5) then
             if (allocated(rho0)) deallocate (rho0)
             allocate (rho0(ndkx), stat=ierr)
             call aerr('rho0 (ndkx)', ierr, ndkx); rho0 = rhomean
          end if

       end if

       if (jabarocterm >= 4) then
          if (allocated(rvdn)) deallocate (rvdn, grn)
          allocate (rvdn(ndkx), grn(ndkx), stat=ierr); rvdn = 0d0; grn = 0d0
          call aerr('rvdn(ndkx), grn(ndkx)', ierr, 2 * ndkx)

          if (jarhointerfaces == 1) then
             if (allocated(rhosww)) deallocate (rhosww)
             allocate (rhosww(ndkx), stat=ierr)
             call aerr('rhosww(ndkx)', ierr, ndkx); rhosww = 0d0
          end if
       end if

    end if

    if (kmx > 0) then
       if (allocated(zws)) deallocate (zws, zws0, ww1, qw)
       allocate (ww1(ndkx), stat=ierr)
       call aerr('ww1 (ndkx)', ierr, ndkx); ww1 = 0
       allocate (qw(ndkx), stat=ierr)
       call aerr('qw  (ndkx)', ierr, ndkx); qw = 0

       allocate (zws(ndkx), stat=ierr)
       call aerr('zws (ndkx)', ierr, ndkx); zws = 0
       allocate (zws0(ndkx), stat=ierr)
       call aerr('zws0(ndkx)', ierr, ndkx); zws0 = 0

       do n1 = 1, ndx
          Ldn = laydefnr(n1)
          Ltn = laytyp(Ldn)
          kb = kbot(n1)
          zws(kb - 1) = bl(n1) - zwsbtol
          if (Ltn == 2 .and. keepzlayeringatbed == 1 .and. kmxn(n1) > numtopsig) then
             call getzlayerindices(n1, nlayb1, nrlay1)
             zws(kb - 1) = zslay(nlayb1 - 1, Ldn)
          end if
       end do

    end if

    kmxd = max(1, kmx)

    if (allocated(workx)) then
       deallocate (workx, worky, work0, work1)
    end if
    allocate (workx(ndkx), stat=ierr)
    call aerr('workx (ndkx)', ierr, ndkx); workx = 0
    allocate (worky(ndkx), stat=ierr)
    call aerr('worky (ndkx)', ierr, ndkx); worky = 0
    allocate (work0(0:max(kmx, 1), max(ndx, lnx)), stat=ierr)
    call aerr('work0 (0:max(kmx,1),max(ndx,lnx))', ierr, max(kmx + 1, 1) * max(ndx, lnx)); work0 = 0
    allocate (work1(max(kmx, 1), max(ndx, lnx)), stat=ierr)
    call aerr('work1 (max(kmx,1),max(ndx,lnx))', ierr, max(kmx, 1) * max(ndx, lnx)); work1 = 0

!    Secondary Flow
    if (jasecflow > 0) then
       if (allocated(spirint)) deallocate (spirint)
       allocate (spirint(ndx), stat=ierr)
       call aerr('spirint( ndx )', ierr, ndx); spirint = 0
       if (allocated(czusf)) deallocate (czusf)
       allocate (czusf(lnx), stat=ierr)
       call aerr('czusf  ( lnx )', ierr, lnx); czusf = 0
       if (allocated(czssf)) deallocate (czssf)
       allocate (czssf(ndx), stat=ierr)
       call aerr('czssf  ( ndx )', ierr, ndx); czssf = 0
       if (kmx == 0) then
          if (allocated(spircrv)) then
             deallocate (spircrv, spirint, spirsrc, spirfx, spirfy, spirucm, ht_xx, ht_xy)
          end if
          allocate (spircrv(ndx), stat=ierr)
          call aerr('spircrv( ndx )', ierr, ndx); spircrv = 0
          allocate (spirsrc(ndx), stat=ierr)
          call aerr('spirsrc( ndx )', ierr, ndx); spirsrc = 0
          allocate (spirfx(ndx), stat=ierr)
          call aerr('spirfx ( ndx )', ierr, ndx); spirfx = 0
          allocate (spirfy(ndx), stat=ierr)
          call aerr('spirfy ( ndx )', ierr, ndx); spirfy = 0
          allocate (spirucm(ndx), stat=ierr)
          call aerr('spirucm( ndx )', ierr, ndx); spirucm = 0
          allocate (ht_xx(ndx), stat=ierr)
          call aerr('ht_xx  ( ndx )', ierr, ndx); ht_xx = 0
          allocate (ht_xy(ndx), stat=ierr)
          call aerr('ht_xy  ( ndx )', ierr, ndx); ht_xy = 0
       else
          if (allocated(spiratx)) then
             deallocate (spiratx, spiraty)
          end if
          allocate (spiratx(ndx), stat=ierr)
          call aerr('spiratx( ndx )', ierr, ndx); spiratx = 0
          allocate (spiraty(ndx), stat=ierr)
          call aerr('spiraty( ndx )', ierr, ndx); spiraty = 0
       end if
    end if

    if (jadhyd == 1) then
       call alloc_hydrology() ! allocate the hydrology module (for spatial input reading in flow_flowinit())
    end if

    if (jabarrieradvection == 3) then
       if (allocated(struclink)) then
          deallocate (struclink)
       end if
       allocate (struclink(lnx), stat=ierr)
       call aerr('struclink( lnx )', ierr, lnx); struclink = 0
    end if

    if (limtypmom == 6) then
       if (allocated(ducxdx)) deallocate (ducxdx, ducxdy)
       allocate (ducxdx(ndkx), stat=ierr)
       call aerr('ducxdx (ndkx)', ierr, ndkx); ducxdx = 0d0
       allocate (ducxdy(ndkx), stat=ierr)
       call aerr('ducxdy (ndkx)', ierr, ndkx); ducxdy = 0d0

       if (allocated(ducydx)) deallocate (ducydx, ducydy)
       allocate (ducydx(ndkx), stat=ierr)
       call aerr('ducydx (ndkx)', ierr, ndkx); ducydx = 0d0
       allocate (ducydy(ndkx), stat=ierr)
       call aerr('ducydy (ndkx)', ierr, ndkx); ducydy = 0d0
    end if

    if (limtypsa == 6) then
       if (allocated(dsadx)) deallocate (dsadx, dsady)
       allocate (dsadx(ndkx), stat=ierr)
       call aerr('dsadx (ndkx)', ierr, ndkx); dsadx = 0
       allocate (dsady(ndkx), stat=ierr)
       call aerr('dsady (ndkx)', ierr, ndkx); dsady = 0
    end if

! if (allocated (dudx) ) then
!    deallocate (dudx,dudy,dvdx,dvdy,rsi,uc3rsi,rsiexact)
!    deallocate (dsdx,dsdy)
! endif
!    Secondary Flow
! if (kmx < 2) then
!     allocate ( dudx (ndkx) , stat = ierr)
!     call aerr('dudx (ndkx)', ierr, ndkx) ; dudx  = 0
!     allocate ( dsdx (ndkx) , stat = ierr)
!     call aerr('dsdx (ndkx)', ierr, ndkx) ; dsdx  = 0
!     allocate ( dsdy (ndkx) , stat = ierr)
!     call aerr('dsdy (ndkx)', ierr, ndkx) ; dsdx  = 0
!     allocate ( dudy (ndkx) , stat = ierr)
!     call aerr('dudy (ndkx)', ierr, ndkx) ; dudy  = 0
!     allocate ( dvdx (ndkx) , stat = ierr)
!     call aerr('dvdx (ndkx)', ierr, ndkx) ; dudx  = 0
!     allocate ( dvdy (ndkx) , stat = ierr)
!     call aerr('dvdy (ndkx)', ierr, ndkx) ; dudy  = 0
!     allocate ( rsi (ndkx) , stat = ierr)
!     call aerr('rsi (ndkx)', ierr, ndkx) ; rsi  = 0
!     allocate ( rsiexact (ndkx) , stat = ierr)
!     call aerr('rsiexact (ndkx)', ierr, ndkx) ; rsiexact  = 0
!     allocate ( uc3rsi (ndkx) , stat = ierr)
!     call aerr('uc3rsi (ndkx)', ierr, ndkx) ; uc3rsi  = 0
! endif

    ! Anti-creep
    if (jacreep == 1 .and. (jasal > 0 .or. jatem > 0 .or. jased > 0 .and. jased < 4)) then
       if (allocated(dsalL)) then
          deallocate (dsalL, dtemL)
       end if
       if (kmx >= 2) then
          allocate (dsalL(lnkx), stat=ierr)
          call aerr('dsalL(lnkx)', ierr, lnkx); dsalL = 0
          allocate (dtemL(lnkx), stat=ierr)
          call aerr('dtemL(lnkx)', ierr, lnkx); dtemL = 0
       end if
    end if

    if (jsferic == 0) then
       jatidep = 0; jaselfal = 0
    else if (jatidep > 0 .or. jaselfal > 0) then
       if (allocated(tidep)) deallocate (tidep)
       if (allocated(tidef)) deallocate (tidef)
       if (allocated(s1init)) deallocate (s1init)
       if (jaselfal > 0) then
!      also store SAL potential
          allocate (tidep(2, ndx), stat=ierr)
          call aerr('tidep (2,ndx)', ierr, 2 * ndx); tidep = 0
          if (jaSELFALcorrectWLwithIni == 1) then
             allocate (s1init(Ndx), stat=ierr)
             call aerr('s1init(Ndx)', ierr, Ndx)
             s1init = 0d0
          end if
       else
          allocate (tidep(1, ndx), stat=ierr)
          call aerr('tidep (1,ndx)', ierr, ndx); tidep = 0
       end if

       allocate (tidef(Lnx), stat=ierr)
       call aerr('tidef(Lnx)', ierr, Lnx)
       tidef = 0d0

    end if

    if (kmx > 0) then ! 7 turbulence arrays (0:kmx)
       if (allocated(turkin0)) then
          deallocate (turkin0, turkin1, tureps0, tureps1, vicwwu, vicwws)
       end if

       allocate (turkin0(Lnkx), stat=ierr)
       call aerr('turkin0  (Lnkx)', ierr, Lnkx); turkin0 = epstke
       allocate (turkin1(Lnkx), stat=ierr)
       call aerr('turkin1  (Lnkx)', ierr, Lnkx); turkin1 = epstke
       allocate (tureps0(Lnkx), stat=ierr)
       call aerr('tureps0  (Lnkx)', ierr, Lnkx); tureps0 = epseps
       allocate (tureps1(Lnkx), stat=ierr)
       call aerr('tureps1  (Lnkx)', ierr, Lnkx); tureps1 = epseps
       allocate (vicwwu(Lnkx), stat=ierr)
       call aerr('vicwwu   (Lnkx)', ierr, Lnkx); vicwwu = 0d0
       allocate (vicwws(ndkx), stat=ierr)
       call aerr('vicwws   (ndkx)', ierr, ndkx); vicwws = 0d0

       if (allocated(turkinepsws)) then
          deallocate (turkinepsws)
       end if
       allocate (turkinepsws(2, ndkx), stat=ierr)
       call aerr('turkinepsws (2,ndkx)', ierr, ndkx); turkinepsws = 0

       if (allocated(sqcu)) then
          deallocate (sqcu, tqcu, eqcu)
       end if
       allocate (sqcu(Ndkx), stat=ierr)
       call aerr('sqcu(Ndkx)', ierr, ndkx); sqcu = 0
       allocate (tqcu(Ndkx), stat=ierr)
       call aerr('tqcu(Ndkx)', ierr, ndkx); tqcu = 0
       allocate (eqcu(Ndkx), stat=ierr)
       call aerr('eqcu(Ndkx)', ierr, ndkx); eqcu = 0
    end if

    call realloc(z0ucur, lnx, stat=ierr, keepExisting=.false., fill=epsz0) !1d-10
    call aerr('z0ucur(lnx)', ierr, lnx)
    call realloc(z0urou, lnx, stat=ierr, keepExisting=.false., fill=epsz0) ! 1d-10
    call aerr('z0urou(lnx)', ierr, lnx)
    call realloc(taus, ndx, stat=ierr, keepExisting=.false., fill=0d0)
    call aerr('taus    (ndx)', ierr, ndx)
    call realloc(taubxu, lnx, stat=ierr, keepExisting=.false., fill=0d0) ! Always needs to be allocated, even if jawave == 0, used in gettau()
    call aerr('taubxu(lnx)', ierr, lnx)
    call realloc(taubu, lnx, stat=ierr, keepExisting=.false., fill=0d0)
    call aerr('taubu(lnx)', ierr, lnx)

    ! link related
    if (allocated(cfuhi)) deallocate (cfuhi)
    if (allocated(frcu)) deallocate (frcu)
    if (allocated(ifrcutp)) deallocate (ifrcutp)
    if (allocated(u0)) deallocate (u0)
    if (allocated(u1)) deallocate (u1)
    if (allocated(q1)) deallocate (q1)
    if (allocated(qa)) deallocate (qa)
    if (allocated(map_fixed_weir_energy_loss)) deallocate (map_fixed_weir_energy_loss)
    if (allocated(v)) deallocate (v)
    if (allocated(ucxu)) deallocate (ucxu)
    if (allocated(ucyu)) deallocate (ucyu)
    if (allocated(hu)) deallocate (hu)
    if (allocated(huvli)) deallocate (huvli)
    if (allocated(au)) deallocate (au)
    if (allocated(au_nostrucs)) deallocate (au_nostrucs)
    if (allocated(viu)) deallocate (viu)
    if (allocated(vicLu)) deallocate (vicLu)
    if (allocated(suu)) deallocate (suu)
    if (allocated(advi)) deallocate (advi)
    if (allocated(adve)) deallocate (adve)
    if (allocated(plotlin)) deallocate (plotlin)
    if (allocated(frcu_bkp)) deallocate (frcu_bkp)
    if (allocated(frcu_mor)) deallocate (frcu_mor)

    allocate (cfuhi(lnx), stat=ierr) ! hk: hier stond + 1, heb ik weggehaald
    call aerr('cfuhi(lnx)', ierr, lnx); cfuhi = 0
    allocate (frcu(lnx), stat=ierr)
    call aerr('frcu (lnx)', ierr, lnx); frcu = dmiss
    if (jacali == 1) then
       allocate (frcu_bkp(lnx), stat=ierr)
       call aerr('frcu_bkp (lnx)', ierr, lnx); frcu_bkp = dmiss
    end if
    allocate (frcu_mor(lnx), stat=ierr)
    call aerr('frcu_mor (lnx)', ierr, lnx); frcu_mor = dmiss
    allocate (ifrcutp(lnx), stat=ierr)
    call aerr('ifrcutp(lnx)', ierr, lnx); ifrcutp = abs(ifrctypuni)

    allocate (u0(lnkx), stat=ierr)
    call aerr('u0   (lnkx)', ierr, lnkx); u0 = 0
    allocate (u1(lnkx), stat=ierr)
    call aerr('u1   (lnkx)', ierr, lnkx); u1 = 0
    allocate (q1(lnkx), stat=ierr)
    call aerr('q1   (lnkx)', ierr, lnkx); q1 = 0
    allocate (qa(lnkx), stat=ierr)
    call aerr('qa   (lnkx)', ierr, lnkx); qa = 0
    allocate (map_fixed_weir_energy_loss(lnkx), stat=ierr)
    call aerr(' map_fixed_weir_energy_loss(lnkx)', ierr, lnkx); map_fixed_weir_energy_loss(:) = 0
    allocate (v(lnkx), stat=ierr)
    call aerr('v    (lnkx)', ierr, lnkx); v = 0
    allocate (ucxu(lnkx), stat=ierr)
    call aerr('ucxu (lnkx)', ierr, lnkx); ucxu = 0
    allocate (ucyu(lnkx), stat=ierr)
    call aerr('ucyu (lnkx)', ierr, lnkx); ucxu = 0
    allocate (hu(lnkx), stat=ierr)
    call aerr('hu   (lnkx)', ierr, lnkx); hu = 0
    allocate (huvli(lnkx), stat=ierr)
    call aerr('huvli(lnkx)', ierr, lnkx); huvli = 0
    allocate (au(lnkx), stat=ierr)
    call aerr('au   (lnkx)', ierr, lnkx); au = 0
    allocate (au_nostrucs(lnkx), stat=ierr)
    call aerr('au_nostrucs   (lnkx)', ierr, lnkx); au_nostrucs = 0
    allocate (viu(lnkx), stat=ierr)
    call aerr('viu  (lnkx)', ierr, lnkx); viu = 0
    allocate (vicLu(lnkx), stat=ierr)
    call aerr('vicLu(lnkx)', ierr, lnkx); vicLu = 0
    allocate (suu(lnkx), stat=ierr)
    call aerr('suu  (lnkx)', ierr, lnkx); suu = 0
    allocate (advi(lnkx), stat=ierr)
    call aerr('advi (lnkx)', ierr, lnkx); advi = 0
    allocate (adve(lnkx), stat=ierr)
    call aerr('adve (lnkx)', ierr, lnkx); adve = 0
    allocate (plotlin(max(lnkx, ndkx)), stat=ierr)
    call aerr('plotlin(max(lnkx,ndkx))', ierr, lnkx); plotlin = 0

    if (jafrculin > 0) then
       if (allocated(frculin)) deallocate (frculin)
       allocate (frculin(lnx), stat=ierr)
       call aerr('frculin (lnx)', ierr, ndx); frculin = dmiss
    end if

    if (network%loaded .or. stm_included) then
       if (allocated(u_to_umain)) deallocate (u_to_umain)
       allocate (u_to_umain(lnkx), stat=ierr)
       call aerr('u_to_umain   (lnkx)', ierr, lnkx); u_to_umain = 1d0

       if (allocated(q1_main)) deallocate (q1_main)
       allocate (q1_main(lnkx), stat=ierr)
       call aerr('q1_main   (lnkx)', ierr, lnkx); q1_main = 0
    end if

    if (jacali == 1) then
       if (allocated(cfclval)) deallocate (cfclval)
       allocate (cfclval(numl), stat=ierr)
       call aerr('cfclval(numl)', ierr, numl); cfclval = 0
    end if

    if (jatrt == 1) then
       if (allocated(cftrt)) deallocate (cftrt)
       allocate (cftrt(numl, 3), stat=ierr)
       call aerr('cftrt(numl,3)', ierr, numl); cftrt = 0
    end if

    if (jamap_chezy_elements > 0) then
       if (allocated(czs)) deallocate (czs)
       allocate (czs(ndx), stat=ierr)
       call aerr('czs(ndx)', ierr, ndx); czs = 0
    end if
    if (jamap_chezy_links > 0) then
       if (allocated(czu)) deallocate (czu)
       allocate (czu(lnx), stat=ierr)
       call aerr('czu(lnx)', ierr, lnx); czu = 0
    end if

    if (jarhoxu > 0 .or. jased > 0) then
       if (allocated(rhou)) deallocate (rhou)
       allocate (rhou(lnkx), stat=ierr)
       call aerr('rhou (lnkx)', ierr, lnkx); rhou = rhomean
    end if

    ! m_integralstats
    if (is_numndvals > 0) then
       call realloc(is_maxvalsnd, (/is_numndvals, ndx/), keepExisting=.false., fill=0d0)
       call realloc(is_sumvalsnd, (/is_numndvals, ndx/), keepExisting=.false., fill=0d0)
       call realloc(is_valnamesnd, is_numndvals, keepExisting=.false., fill='')
    end if

    ! solving related
    if (allocated(fu)) then
       deallocate (fu, ru, bb, dd)
    end if

    allocate (bb(ndx), stat=ierr)
    call aerr('bb   (ndx )', ierr, ndx); bb = 0
    allocate (dd(ndx), stat=ierr)
    call aerr('dd   (ndx )', ierr, ndx); dd = 0
    allocate (fu(lnkx), stat=ierr)
    call aerr('fu   (lnkx)', ierr, ndx); fu = 0
    allocate (ru(lnkx), stat=ierr)
    call aerr('ru   (lnkx)', ierr, ndx); ru = 0

    if (jasal > 0 .or. kmx > 0) then
       if (allocated(sa1)) deallocate (sa1)
       allocate (sa1(ndkx), stat=ierr)
       call aerr('sa1 (ndkx)', ierr, ndkx); sa1 = salini

       if (allocated(sam0)) deallocate (sam0, sam1, same)
       allocate (sam0(ndkx), sam1(ndkx), same(ndkx)); sam0 = 0; sam1 = 0; same = 0
    end if

    if (jatem > 0) then
       if (allocated(tem1)) deallocate (tem1)
       allocate (tem1(ndkx), stat=ierr)
       call aerr('tem1(ndkx) ', ierr, ndkx)
       tem1 = temini
       if (allocated(heatsrc)) deallocate (heatsrc, heatsrc0)
       allocate (heatsrc(ndkx), heatsrc0(ndkx), stat=ierr)
       call aerr('heatsrc(ndkx), heatsrc0(ndkx)', ierr, ndkx)
       heatsrc = 0d0
       heatsrc0 = 0d0

       if (jatem > 1) then ! also heat modelling involved
          if (allocated(tair)) deallocate (tair, rhum, clou)
          allocate (tair(ndx), rhum(ndx), clou(ndx), stat=ierr)
          call aerr('tair(ndx), rhum(ndx), clou(ndx)', ierr, 3 * ndx)
          tair = backgroundairtemperature
          rhum = backgroundhumidity
          clou = backgroundcloudiness
          if (allocated(qrad)) deallocate (qrad)
          allocate (qrad(ndx), stat=ierr)
          call aerr('qrad(ndx)', ierr, ndx)
          qrad = 0d0
          if (allocated(longwave)) deallocate (longwave)
          if (Soiltempthick > 0) then
             if (allocated(tbed)) deallocate (tbed)
             allocate (tbed(ndx), stat=ierr)
             call aerr('tbed(ndx)', ierr, ndx)
             tbed = temini
          end if

       end if

       if ((jamapheatflux > 0 .or. jahisheatflux > 0) .and. jatem > 1) then
          if (allocated(Qtotmap)) deallocate (Qtotmap)
          allocate (Qtotmap(ndx), stat=ierr)
          call aerr('Qtotmap(ndx)', ierr, ndx)
          Qtotmap = 0d0
       end if

       if (jatem == 5) then ! save cd coeff if heat modelling also involved
          if (allocated(cdwcof)) deallocate (cdwcof)
          allocate (cdwcof(lnx), stat=ierr)
          call aerr('cdwcof(lnx)', ierr, lnx)
          cdwcof = 0d0

          if (jaroro > 1) then ! save rhoair for windstress
             if (allocated(roair)) deallocate (roair)
             allocate (roair(ndx), stat=ierr)
             call aerr('roair(ndx)', ierr, ndx)
             roair = rhoair
          end if

          if (jamapheatflux > 0 .or. jahisheatflux > 0) then ! his or map output
             if (allocated(qsunmap)) deallocate (Qsunmap, Qevamap, Qconmap, Qlongmap, Qfrevamap, Qfrconmap)
             allocate (Qsunmap(ndx), stat=ierr)
             call aerr('Qsunmap(ndx)', ierr, ndx)
             Qsunmap = 0d0
             allocate (Qevamap(ndx), stat=ierr)
             call aerr('Qevamap(ndx)', ierr, ndx)
             Qevamap = 0d0
             allocate (Qconmap(ndx), stat=ierr)
             call aerr('Qconmap(ndx)', ierr, ndx)
             Qconmap = 0d0
             allocate (Qlongmap(ndx), stat=ierr)
             call aerr('Qlongmap(ndx)', ierr, ndx)
             Qlongmap = 0d0
             allocate (Qfrevamap(ndx), stat=ierr)
             call aerr('Qfrevamap(ndx)', ierr, ndx)
             Qfrevamap = 0d0
             allocate (Qfrconmap(ndx), stat=ierr)
             call aerr('Qfrconmap(ndx)', ierr, ndx)
             Qfrconmap = 0d0
          end if
       end if
    end if

    if (jased > 0 .and. jased < 4) then
       if (allocated(sed)) deallocate (sed, grainlay)
       allocate (sed(mxgr, ndkx), stat=ierr)
       call aerr('sed (mxgr,ndkx)', ierr, ndkx * mxgr)

       if (allocated(sdupq)) deallocate (sdupq)
       allocate (sdupq(mxgr, ndkx), stat=ierr)
       call aerr('sdupq(mxgr,ndkx)', ierr, ndkx * mxgr)
       sdupq = 0d0

       if (jaceneqtr == 1) then ! cell centre equilibrium transport concentration
          mxn = ndx
          if (allocated(blinc)) deallocate (blinc)
          allocate (blinc(ndx), stat=ierr)
          call aerr('blinc(ndx)', ierr, ndx); blinc = 0d0
       else ! cell corner equilibrium transport concentration
          mxn = numk
          if (allocated(sedi)) deallocate (sedi)
          allocate (sedi(mxgr, ndx), stat=ierr)
          call aerr('sedi(mxgr,ndx)', ierr, ndx * mxgr); sedi = 0d0
       end if
       allocate (grainlay(mxgr, mxn), stat=ierr)
       call aerr('grainlay(mxgr,mxn)', ierr, mxgr * mxn); grainlay = 0d0

       if (kmx > 0 .and. jased > 0 .and. jased < 4) then
          if (allocated(ustbc)) deallocate (ustbc)
          allocate (ustbc(mxn), stat=ierr)
          call aerr('ustbc(mxn)', ierr, mxn); ustbc = 0d0
       end if

    end if

    if (idensform > 0 .and. jaRichardsononoutput > 0) then
       if (allocated(rich)) deallocate (rich)
       allocate (rich(lnkx), stat=ierr)
       call aerr('rich(lnkx)', ierr, ndkx); rich = 0d0
    else
       jaRichardsononoutput = 0
    end if

    if (ti_waq > 0) then
       call realloc(q1waq, lnkx, keepExisting=.false., fill=0d0, stat=ierr)
       if (kmx > 0) then
          call realloc(qwwaq, ndkx, keepExisting=.false., fill=0d0, stat=ierr)
       end if
    end if

    if (itstep == 4) then ! explicit time-step
       if (allocated(sqwave)) deallocate (sqwave)
       allocate (sqwave(ndx), stat=ierr)
       call aerr('sqwave (ndx)', ierr, ndx); sqwave = 0
    end if

    if (infiltrationmodel /= DFM_HYD_NOINFILT) then
       call realloc(infilt, ndx, keepExisting=.false., fill=0d0, stat=ierr)

       if (infiltrationmodel == DFM_HYD_INFILT_CONST) then
          call realloc(infiltcap, ndx, keepExisting=.false., fill=infiltcapuni, stat=ierr)
       end if
    end if

    if (jagrw > 0) then
       if (allocated(sgrw0)) deallocate (sgrw0, sgrw1, pgrw, bgrw)
       allocate (sgrw0(ndx), stat=ierr)
       call aerr('sgrw0(ndx)', ierr, ndx); sgrw0 = 0d0
       allocate (sgrw1(ndx), stat=ierr)
       call aerr('sgrw1(ndx)', ierr, ndx); sgrw1 = 0d0
       allocate (pgrw(ndx), stat=ierr)
       call aerr('pgrw (ndx)', ierr, ndx); pgrw = 0d0
       allocate (bgrw(ndx), stat=ierr)
       call aerr('bgrw (ndx)', ierr, ndx)
       if (h_aquiferuni > 0d0) then
          bgrw = bl - h_aquiferuni
       else
          bgrw = bgrwuni
       end if

    end if

    if (jarain > 0) then
       call realloc(rain, ndx, keepExisting=.false., fill=0d0, stat=ierr)
    end if

    if (jaevap > 0) then
       call realloc(evap, ndx, keepExisting=.false., fill=0d0, stat=ierr)
    end if

    if (jaQext > 0) then
       call realloc(qext, ndkx, keepExisting=.false., fill=0d0, stat=ierr)
       call aerr('qext(ndkx)', ierr, ndkx)
       call realloc(qextreal, ndkx, keepExisting=.false., fill=0d0, stat=ierr)
       call aerr('qextreal(ndkx)', ierr, ndkx)
       call realloc(vextcum, ndkx, keepExisting=.false., fill=0d0, stat=ierr)
       call aerr('vextcum(ndkx)', ierr, ndkx)
    end if

    if (nshiptxy > 0) then
       if (allocated(zsp0)) deallocate (zsp0)
       allocate (zsp0(numk), stat=ierr)
       call aerr('zsp0(numk)', ierr, numk); zsp0 = 0d0

       if (allocated(zspc)) deallocate (zspc)
       allocate (zspc(numk), stat=ierr)
       call aerr('zspc(numk)', ierr, numk); zspc = 0d0

       if (allocated(zspc0)) deallocate (zspc0)
       allocate (zspc0(numk), stat=ierr); zspc0 = 0d0
       call aerr('zspc0(numk)', ierr, numk)

       if (allocated(v0ship)) deallocate (v0ship, v1ship, qinship)
       allocate (v0ship(ndx), v1ship(ndx), qinship(ndx), stat=ierr); v0ship = 0d0; v1ship = 0d0; qinship = 0d0
       call aerr('v0ship(ndx), v1ship(ndx), qinship(ndx)', ierr, ndx)

       call realloc(shL, 2, keepExisting=.false., fill=0d0, stat=ierr)
       call aerr('shL(2)', ierr, 2)
       call realloc(shB, 2, keepExisting=.false., fill=0d0, stat=ierr)
       call aerr('shB(2)', ierr, 2)
       call realloc(shd, 2, keepExisting=.false., fill=0d0, stat=ierr)
       call aerr('shd(2)', ierr, 2)
       call realloc(stuw, 2, keepExisting=.false., fill=0d0, stat=ierr)
       call aerr('stuw(2)', ierr, 2)
       call realloc(fstuw, 2, keepExisting=.false., fill=0d0, stat=ierr)
       call aerr('fstuw(2)', ierr, 2)
       call realloc(stuwmx, 2, keepExisting=.false., fill=0d0, stat=ierr)
       call aerr('stuwmx(2)', ierr, 2)
       call realloc(roer, 2, keepExisting=.false., fill=0d0, stat=ierr)
       call aerr('roer(2)', ierr, 2)
       call realloc(froer, 2, keepExisting=.false., fill=0d0, stat=ierr)
       call aerr('froer(2)', ierr, 2)
       call realloc(roermx, 2, keepExisting=.false., fill=0d0, stat=ierr)
       call aerr('roermx(2)', ierr, 2)
    end if

    if (janudge == 1) then
       call realloc(nudge_tem, Ndkx, fill=DMISS)
       call realloc(nudge_sal, Ndkx, fill=DMISS)
       call realloc(zcs, Ndkx)
       call realloc(nudge_time, Ndx, fill=DMISS)
       call realloc(nudge_rate, Ndx, fill=DMISS)
    end if

 end subroutine flow_allocflow
