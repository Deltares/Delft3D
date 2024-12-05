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
!>  override bobs along pliz's, jadykes == 0: only heights, 1 = also dyke attributes
subroutine setfixedweirs()
   use precision, only: dp
   use m_netw
   use m_flowgeom
   use m_flow
   use m_missing
   use m_alloc
   use unstruc_model
   use timespace
   use unstruc_messages
   use m_fixedweirs
   use kdtree2Factory
   use m_sferic
   use m_polygon
   use m_partitioninfo, only: jampi, sdmn
   use messagehandling
   use string_module, only: strsplit
   use system_utils, only: FILESEP
   use geometry_module, only: dbdistance, CROSSinbox, dcosphi, duitpl, normalout
   use unstruc_caching
   use m_1d2d_fixedweirs, only: find_1d2d_fixedweirs
   use m_readyy
   use m_wall_clock_time
   use m_find_crossed_links_kdtree2

   implicit none

   integer :: k, kk, n1, n2, n, L, LL, jacros, minp, kint, ierr, nh, nhh, i, Lf
   integer :: jaweir, Lastfoundk, kf, kL, Lnt, nna, nnb, k3, k4
   integer, allocatable :: ihu(:)
   real(kind=dp) :: SL, SM, XCR, YCR, CRP, Xa, Ya, Xb, Yb, zc, zh, zhu, zhd, af, dz1, dz2, xn, yn, adjacentbob, cosphi, sig, bobL
   real(kind=dp), allocatable :: csh(:), snh(:), zcrest(:), dzsillu(:), dzsilld(:), crestlen(:), taludu(:), taludd(:), vegetat(:), ztoeu(:), ztoed(:)
   integer, allocatable :: iweirtyp(:)
   integer, allocatable :: ifirstweir(:)

   real(kind=dp), dimension(:), allocatable :: dSL
   integer, dimension(:), allocatable :: iLink
   integer, dimension(:), allocatable :: iLcr ! link crossed yes no
   integer, dimension(:), allocatable :: iPol

   integer :: iL, numcrossedLinks, ii, LLL, LLLa, nx
   integer :: mout, jatabellenboekorvillemonte
   integer :: ierror

   integer :: jakdtree = 1
   character(len=5) :: sd
   character(len=200), allocatable :: fnames(:)
   integer, allocatable :: start_npl_for_files(:)
   integer :: jadoorladen, ifil
   real(kind=dp) :: t0, t1, t_extra(2, 10), BLmn
   character(len=128) :: mesg

   integer, parameter :: KEEP_PLI_NAMES = 1
   integer :: number_of_plis

   if (len_trim(md_fixedweirfile) == 0) then
      ifixedweirscheme = 0
      return
   end if

   jatabellenboekorvillemonte = 0
   if (ifixedweirscheme == 8) jatabellenboekorvillemonte = 1
   if (ifixedweirscheme == 9) jatabellenboekorvillemonte = 2

   call readyy('Setfixedweirs', 0d0)

   allocate (ihu(lnx)); ihu = 0
   allocate (csh(lnx)); csh = 0d0
   allocate (snh(lnx)); snh = 0d0
   allocate (zcrest(lnx)); zcrest = -1000d0 ! starting from a low value
   allocate (dzsillu(lnx)); dzsillu = 0d0
   allocate (dzsilld(lnx)); dzsilld = 0d0
   allocate (ztoeu(lnx)); ztoeu = 1000d0 ! starting from a high value
   allocate (ztoed(lnx)); ztoed = 1000d0 ! starting from a high value
   allocate (crestlen(lnx)); crestlen = 3d0
   allocate (taludu(lnx)); taludu = 4d0
   allocate (taludd(lnx)); taludd = 4d0
   allocate (vegetat(lnx)); vegetat = 0d0
   allocate (iweirtyp(lnx)); iweirtyp = 0
   allocate (ifirstweir(lnx)); ifirstweir = 1 ! added to check whether fixed weir data is set for the first time at a net link (1=true, 0=false)

   call wall_clock_time(t0)
   t_extra(1, 1) = t0

   ! Load fixed weirs polygons from file.
   ! --------------------------------------------------------------------
   if (len_trim(md_fixedweirfile) > 0) then
      call strsplit(md_fixedweirfile, 1, fnames, 1)
      allocate (start_npl_for_files(size(fnames) + 1))
      jadoorladen = 0
      number_of_plis = 0
      do ifil = 1, size(fnames)
         start_npl_for_files(ifil) = npl + 1
         call oldfil(minp, fnames(ifil))
         N1 = index(fnames(ifil), FILESEP, .true.)
         !  fix for Linux-prepared input on Windows
         if (N1 == 0) then
            N1 = index(fnames(ifil), char(47), .true.)
         end if

         sd = ''
         if (jampi == 1) then
            sd = '_'//trim(sdmn)
         end if

         N2 = index(fnames(ifil), '.', .true.)
         if (N2 == 0) then
            N2 = len_trim(fnames(ifil))
         else
            N2 = N2 - 1
         end if
         if (jawriteDFMinterpretedvalues > 0) then
            call newfil(mout, trim(getoutputdir())//'DFM_interpreted_fxwvalues_'//fnames(ifil) (n1 + 1:n2)//trim(sd)//'.xyz')
            write (mout, '(a)') '* xu yu crest(bob) width(wu) xk3 yk3 xk4 yk4'
         end if
         call reapol_nampli(minp, jadoorladen, KEEP_PLI_NAMES, number_of_plis)
         jadoorladen = 1
      end do

      call wall_clock_time(t_extra(2, 1))
      call wall_clock_time(t_extra(1, 2))
      call pol_to_flowlinks(xpl, ypl, zpl, npl, nfxw, fxw)
      call wall_clock_time(t_extra(2, 2))

      start_npl_for_files(size(fnames) + 1) = npl + 1
      call check_fixed_weirs_parameters_against_limits()

      deallocate (fnames)
   end if

   kint = max(lnxi / 1000, 1)

   call wall_clock_time(t_extra(1, 3))
   allocate (iLink(Lnx))
   allocate (iLcr(Lnx)); Ilcr = 0
   allocate (ipol(Lnx))
   allocate (dSL(Lnx))
   if (cacheRetrieved()) then
      ierror = 0
      call copyCachedFixedWeirs(npl, xpl, ypl, numcrossedLinks, iLink, iPol, dSL, success)
   else
      success = .false.
   end if
   if (.not. success) then
      call find_crossed_links_kdtree2(treeglob, NPL, XPL, YPL, 2, Lnx, 2, numcrossedLinks, iLink, iPol, dSL, ierror)
      call cacheFixedWeirs(npl, xpl, ypl, numcrossedLinks, iLink, iPol, dSL)
   end if
   call wall_clock_time(t_extra(2, 3))

   call wall_clock_time(t_extra(1, 4))
   if (ierror == 0) then
      do iL = 1, numcrossedlinks
         L = iLink(il)
         iLcr(L) = 1
      end do
   else
      n = 0; Lastfoundk = 0
      do L = 1, lnxi

         if (mod(L, kint) == 0) then
            AF = dble(L) / dble(lnxi)
            call readyy('Setfixedweirs', af)
         end if

         n1 = ln(1, L); n2 = ln(2, L)
         xa = xz(n1); ya = yz(n1)
         xb = xz(n2); yb = yz(n2)

         iloop: do i = 1, 2

            if (i == 1) then
               if (Lastfoundk == 0) cycle
               kf = max(1, Lastfoundk - 100)
               kL = min(npl - 1, Lastfoundk + 100)
            else
               kf = 1
               kL = npl - 1
            end if

            Lastfoundk = 0
            do k = kf, kL

               if (xpl(k) /= dmiss .and. xpl(k + 1) /= dmiss) then
                  call CROSSinbox(XPL(k), YPL(k), XPL(k + 1), YPL(k + 1), Xa, Ya, Xb, Yb, jacros, SL, SM, XCR, YCR, CRP, jsferic, dmiss)

                  if (jacros == 1) then
                     Lastfoundk = k
                     n = n + 1
                     ilink(n) = L
                     ipol(n) = k
                     dsl(n) = SL
                     iLcr(L) = 1
                     exit iloop
                  end if
               end if
            end do

         end do iloop

      end do
      numcrossedlinks = n
   end if
   call wall_clock_time(t_extra(2, 4))

   call wall_clock_time(t1)
   write (mesg, "('fixed weirs with kdtree2, elapsed time: ', G15.5, 's.')") t1 - t0
   call mess(LEVEL_INFO, trim(mesg))
   write (mesg, "('fixed weirs: read files,  elapsed time: ', G15.5, 's.')") t_extra(2, 1) - t_extra(1, 1)
   call mess(LEVEL_INFO, trim(mesg))
   write (mesg, "('fixed weirs: pol_to_flowlinks, elapsed time: ', G15.5, 's.')") t_extra(2, 2) - t_extra(1, 2)
   call mess(LEVEL_INFO, trim(mesg))
   write (mesg, "('fixed weirs: find_crossed_links, elapsed time: ', G15.5, 's.')") t_extra(2, 3) - t_extra(1, 3)
   call mess(LEVEL_INFO, trim(mesg))
   write (mesg, "('fixed weirs: attributes,  elapsed time: ', G15.5, 's.')") t_extra(2, 4) - t_extra(1, 4)
   call mess(LEVEL_INFO, trim(mesg))

   nh = 0
   do iL = 1, numcrossedlinks

      L = ilink(iL)
      k = ipol(iL)
      SL = dsl(iL)
      n1 = ln(1, L); n2 = ln(2, L)

      if (kcu(L) == 1 .or. kcu(L) == 5) then
         cycle ! UNST-2226: test code for forbidding fixed weirs on 1D
      end if

      zc = sl * zpL(k + 1) + (1d0 - sl) * zpL(k)

      if (abs(kcu(L)) == 2) then
         bobL = min(bob(1, L), bob(2, L))
      else
         bobL = max(bob(1, L), bob(2, L))
      end if

      ! if ( (zc > bobL .and. zc > zcrest(L)) .or. ( (ifixedweirscheme == 8 .or. ifixedweirscheme == 9) .and. ifirstweir(L) == 1) ) then   ! For Villemonte and Tabellenboek fixed weirs under bed level are also possible

      if ((zc > bobL .and. zc > zcrest(L)) .or. ifirstweir(L) == 1) then ! For Villemonte and Tabellenboek fixed weirs under bed level are also possible

         ! Set whether this is the first time that for this link weir values are set:
         ! As a result, only the first fixed weir under the bed level is used
         ifirstweir(L) = 0

         bob(1, L) = max(bob(1, L), zc); bob(2, L) = max(bob(2, L), zc)

         if (kcu(L) /= 2 .and. kcu(L) /= 1) then
            cycle ! weirs only on regular links
         end if

         jaweir = 1 ! 0
         if (ifixedweirscheme > 0) then
            if (jakol45 == 0) then ! no dzl or dzr specified
               ! jaweir = 1
            else
               dz1 = sl * dzL(k + 1) + (1d0 - sl) * dzL(k)
               dz2 = sl * dzR(k + 1) + (1d0 - sl) * dzR(k)

               ! if (min (dz1,dz2) >= sillheightmin) then  ! weir if sufficiently high and regular link
               !   jaweir = 1
               !elseif (ifixedweirscheme == 8 .or. ifixedweirscheme == 9) then
               ! For Villemonte and Tabellenboek weirs with low sills are also applied, in order to be consistent with Simona
               !   jaweir = 1
               !endif

               if (jaconveyance2D >= 1) then ! now set adjacent bobs of netlinks | sufficiently perpendicular to fixedweir to local ground level
                  do i = 1, 2
                     n1 = lncn(i, L)
                     do kk = 1, nmk(n1) !          |         |
                        Lnt = nod(n1)%lin(kk) ! ---------o---------o-------fixedweir
                        Lf = lne2ln(Lnt) !          |         |
                        if (Lf == 0) cycle
                        if (iLcr(abs(Lf)) == 1) cycle
                        nna = kn(1, Lnt)
                        nnb = kn(2, Lnt)
                        xa = xk(nna); ya = yk(nna)
                        xb = xk(nnb); yb = yk(nnb)

                        COSPHI = DCOSPHI(Xpl(k), Ypl(k), xpl(k + 1), ypl(k + 1), xa, ya, xb, yb, jsferic, jasfer3D, dxymis)
                        if (abs(cosphi) < 0.5d0) then
                           if (nna /= n1) then
                              nhh = nna
                              nna = nnb
                              nnb = nhh
                           end if ! na is now basepoint
                           xa = xk(nna); ya = yk(nna)
                           xb = xk(nnb); yb = yk(nnb)
                           call duitpl(Xpl(k), Ypl(k), xpl(k + 1), ypl(k + 1), xa, ya, xb, yb, sig, jsferic)
                           adjacentbob = dmiss
                           if (sig > 0) then
                              if (dz2 > 3d0 .and. dz1 < 3d0) then ! kade at other side deeper than 3 m
                                 adjacentbob = zc - dz1 ! then set kade ground level
                              end if
                           else
                              if (dz1 > 3d0 .and. dz2 < 3d0) then
                                 adjacentbob = zc - dz2
                              end if
                           end if

                           if (Lf > 0 .and. adjacentbob /= dmiss) then
                              if (jaconveyance2D >= 1) then
                                 if (lncn(1, Lf) == n1) then
                                    bob(1, Lf) = adjacentbob
                                 else
                                    bob(2, Lf) = adjacentbob
                                 end if
                              else
                                 bob(1, Lf) = adjacentbob
                                 bob(2, Lf) = adjacentbob
                              end if
                              !nl1 = ln(1,Lf) ; nl2 = ln(2,Lf)
                              !bl(nl1) = min(bl(nl1), adjacentbob )
                              !bl(nl2) = min(bl(nl2), adjacentbob ) ! still needs to be done
                           end if

                        end if

                     end do

                  end do !1,2
               end if
            end if
         end if

         if (jaweir > 0) then ! set weir treatment
            ihu(L) = k
            call normalout(XPL(k), YPL(k), XPL(k + 1), YPL(k + 1), xn, yn, jsferic, jasfer3D, dmiss, dxymis)

            k3 = lncn(1, L); k4 = lncn(2, L)
            wu(L) = dbdistance(xk(k3), yk(k3), xk(k4), yk(k4), jsferic, jasfer3D, dmiss) ! set 2D link width

            wu(L) = wu(L) * abs(xn * csu(L) + yn * snu(L)) ! projected length of fixed weir

            if (jakol45 == 2) then ! use local type definition
               !
               ! recompute ground heights if zc is larger than previous crest levels
               !
               if (zc > zcrest(L)) then
                  dzsillu(L) = zc - ztoeu(L)
                  dzsilld(L) = zc - ztoed(L)
               end if
               !
               zcrest(L) = zc
               zhu = (1d0 - sl) * dzl(k) + sl * dzl(k + 1) ! ground height left
               zhd = (1d0 - sl) * dzr(k) + sl * dzr(k + 1) ! ground height right
               crestlen(L) = (1d0 - sl) * dcrest(k) + sl * dcrest(k + 1) ! crest length
               taludu(L) = (1d0 - sl) * dtl(k) + sl * dtl(k + 1) ! talud at ln(1,L)
               taludd(L) = (1d0 - sl) * dtr(k) + sl * dtr(k + 1) ! talud at ln(2,L)
               vegetat(L) = (1d0 - sl) * dveg(k) + sl * dveg(k + 1) ! vegetation on fixed weir
               iweirtyp(L) = iweirt(k) ! type of weir
               if (iweirt(k) == 1) then
                  iadv(L) = 24; jatabellenboekorvillemonte = 1 !  Tabellenboek
               else if (iweirt(k) == 2) then
                  iadv(L) = 25; jatabellenboekorvillemonte = 1 !  Villemonte
               else
                  iadv(L) = 21 !  Subgrid, ifixedweirscheme = 6 or 7
               end if
               !
               ! If link is reversed, exchange ground height levels and taluds
               !
               if (xn * csu(L) + yn * snu(L) < 0d0) then ! check left/right
                  zh = taludd(L); taludd(L) = taludu(L); taludu(L) = zh
                  zh = zhd; zhd = zhu; zhu = zh
               end if
               !
               ! lowest toe is applied
               !
               if (zc - zhu < ztoeu(L)) then
                  ztoeu(L) = zc - zhu
                  dzsillu(L) = zcrest(L) - ztoeu(L)
               end if
               if (zc - zhd < ztoed(L)) then
                  ztoed(L) = zc - zhd
                  dzsilld(L) = zcrest(L) - ztoed(L)
               end if
             !! write (msgbuf,'(a,2i5,7f10.3)') 'Projected fixed weir', L, iweirtyp(L), zc, bobL, dzsillu(L), dzsilld(L),crestlen(L),taludu(L),taludd(L); call msg_flush()
            else ! use global type definition
               if (ifixedweirscheme == 7) then
                  iadv(L) = 23 !  Rajaratnam
               else if (ifixedweirscheme == 8) then
                  iadv(L) = 24 !  Tabellenboek
                  dzsillu(L) = max(0.0d0, zc - blu(L)); dzsilld(L) = max(0.0d0, zc - blu(L)) ! if not specified then estimate
                  zcrest(L) = zc
               else if (ifixedweirscheme == 9) then
                  iadv(L) = 25 !  Villemonte
                  dzsillu(L) = max(0.0d0, zc - blu(L)); dzsilld(L) = max(0.0d0, zc - blu(L)) ! if not specified then estimate
                  zcrest(L) = zc
               else
                  iadv(L) = 21 !  Ifixedweirscheme 6
               end if
            end if

            ! 21 = Ifixedweirscheme 6
            ! 22 = General structure
            ! 23 = Rajaratnam
            ! 24 = Tabellenboek
            ! 25 = Villemonte

            if (jawriteDFMinterpretedvalues > 0) then
               write (mout, '(8(f24.4))') xu(L), yu(L), bob(1, L), fixedweircontraction * wu(L), xk(k3), yk(k3), xk(k4), yk(k4)
            end if
         else
            nh = nh + 1 ! just raised bobs
         end if
      else
         if (ifirstweir(L) == 0 .and. jakol45 == 2) then !  .and. (ifixedweirscheme == 8 .or. ifixedweirscheme == 9) ) then   !  only for fixed weirs under the bed level for Tabellenboek or Villemonte and not for the first time that a fixed weir is set on this link
            ! check for larger ground height height values if at this link already a fixed weir exist

            !
            ! check whether crestlevel is higher
            !
            if (zc > zcrest(L)) then
               zcrest(L) = zc
             !! write (msgbuf,'(a,i5,f10.3)') 'Higher crest level: ', L,  zcrest(L); call msg_flush()
            end if
            if (jakol45 /= 0) then
               call normalout(XPL(k), YPL(k), XPL(k + 1), YPL(k + 1), xn, yn, jsferic, jasfer3D, dmiss, dxymis) ! test EdG
               zhu = (1d0 - sl) * dzl(k) + sl * dzl(k + 1)
               zhd = (1d0 - sl) * dzR(k) + sl * dzR(k + 1)
               if (xn * csu(L) + yn * snu(L) < 0d0) then ! check left/right
                  zh = zhd; zhd = zhu; zhu = zh
               end if
               !
               ! Check whether toe is lower. If so, also adjust toe level and the ground height
               ! If ground height is smaller than 1 cm, then this neglected
               !
               if (zc - zhu < ztoeu(L) .and. zhu > 0.01) then
                  ztoeu(L) = zc - zhu
                  dzsillu(L) = zcrest(L) - ztoeu(L)
            !! write (msgbuf,'(a,i5,f10.3)') 'Larger sill up:     ', L,  dzsillu(L); call msg_flush()
               end if
               if (zc - zhd < ztoed(L) .and. zhd > 0.01) then
                  ztoed(L) = zc - zhd
                  dzsilld(L) = zcrest(L) - ztoed(L)
            !! write (msgbuf,'(a,i5,f10.3)') 'Larger sill down:   ', L, dzsilld(L); call msg_flush()
               end if
            end if
         end if
      end if
    !! write (msgbuf,'(a,2i5,7f10.3)') 'Projected fixed weir', L, iweirtyp(L), zcrest(L), ztoeu(L), dzsillu(L),ztoed(L),dzsilld(L),taludu(L),taludd(L); call msg_flush()

   end do
   if (jawriteDFMinterpretedvalues > 0) then
      call doclose(mout)
   end if

   if (jakol45 == 2 .and. sillheightmin > 0d0) then ! when a minimum threshold is specified
      ! and toe heights are known, and agreed upon
      do L = 1, lnxi
         if (ihu(L) > 0) then ! when flagged as weir

            do ii = 1, 2 ! loop over adjacent cells
               k = 0
               if (ii == 1 .and. dzsillu(L) < sillheightmin .and. dzsilld(L) > sillheightmin .or. &
                   ii == 2 .and. dzsilld(L) < sillheightmin .and. dzsillu(L) > sillheightmin) then
                  k = ln(ii, L)
               end if
               if (k > 0) then ! flatland on node k
                  nx = nd(k)%lnx
                  do LL = 1, nx ! loop over all attached links
                     LLL = nd(k)%ln(LL); LLLa = abs(LLL)
                     if (LLLa == L) then
                        LLLa = LL - 1; if (LLLa == 0) LLLa = nx ! left of weir link
                        LLLa = abs(nd(k)%ln(LLLa))
                        if (ihu(LLLa) == 0) then ! if not already marked as weir
                           bob(1, LLLa) = max(zcrest(L), bob(1, LLLa)) ! raise both bobs
                           bob(2, LLLa) = max(zcrest(L), bob(2, LLLa)) ! raise both bobs
                        end if
                        LLLa = LL + 1; if (LLLa > nx) LLLa = 1 ! right of weir link
                        LLLa = abs(nd(k)%ln(LLLa))
                        if (ihu(LLLa) == 0) then ! if not already marked as weir
                           bob(1, LLLa) = max(zcrest(L), bob(1, LLLa)) ! raise both bobs
                           bob(2, LLLa) = max(zcrest(L), bob(2, LLLa)) ! raise both bobs o
                        end if
                     end if
                  end do
               end if
            end do
         end if
      end do

      BL = 1d9
      do L = 1, lnx ! switch off weirs that do not need weir treatment
         if (ihu(L) > 0 .and. (dzsillu(L) < sillheightmin .or. dzsilld(L) < sillheightmin)) then
            ihu(L) = 0; iadv(L) = iadvec
            if (slopedrop2D > 0d0) then
               iadv(L) = 8
            end if
         end if
         BLmn = min(bob(1, L), bob(2, L)) ! and reset BL to lowest attached link
         n1 = ln(1, L); n2 = ln(2, L)
         BL(n1) = min(BL(n1), BLmn)
         BL(n2) = min(BL(n2), BLmn)
      end do

   end if

   nfxw = 0
   do L = 1, lnxi
      if (ihu(L) > 0) then
         nfxw = nfxw + 1 ! TODO: HK: incorrect/inconsistent use of nfxw: upon reading the pliz file it is nr of polylines, now it becomes the total number of flow links crossed by a fixed weir.

         if (iadv(L) == 21) then
            call setfixedweirscheme3onlink(L)
            if (ifixedweirscheme == 7) then
               iadv(L) = 23
            end if
         end if

      end if
   end do

   if (nfxw > 0) then
      if (allocated(lnfxw)) deallocate (nfxwL, lnfxw)
      if (allocated(weirdte)) deallocate (weirdte)
      if (allocated(shlxw)) deallocate (shlxw, shrxw, crestlevxw, crestlxw, taludlxw, taludrxw, vegxw, iweirtxw)
      allocate (nfxwL(Lnx), stat=ierr)
      call aerr('nfxwL(Lnx)', ierr, lnx)

      call realloc(weirdte, nfxw, keepExisting=.false., fill=0d0, stat=ierr)
      call aerr('weirdte', ierr, nfxw)
      allocate (lnfxw(nfxw), stat=ierr)
      call aerr('lnfxw(nfxw)', ierr, nfxw)
      allocate (shlxw(nfxw), stat=ierr) ! Tabellenboek / Villemonte parameters)
      call aerr('shlxw(nfxw)', ierr, nfxw)
      allocate (shrxw(nfxw), stat=ierr)
      call aerr('shrxw(nfxw)', ierr, nfxw)
      allocate (crestlevxw(nfxw), stat=ierr)
      call aerr('crestlevxw(nfxw)', ierr, nfxw)
      allocate (crestlxw(nfxw), stat=ierr)
      call aerr('crestlxw(nfxw)', ierr, nfxw)
      allocate (taludlxw(nfxw), stat=ierr)
      call aerr('taludlxw(nfxw)', ierr, nfxw)
      allocate (taludrxw(nfxw), stat=ierr)
      call aerr('taludrxw(nfxw)', ierr, nfxw)
      allocate (vegxw(nfxw), stat=ierr)
      call aerr('vegxw(nfxw)', ierr, nfxw)
      allocate (iweirtxw(nfxw), stat=ierr)
      call aerr('iweirtxw(nfxw)', ierr, nfxw)
   end if

   nfxw = 0
   do L = 1, lnxi
      if (ihu(L) > 0) then
         nfxw = nfxw + 1
         lnfxw(nfxw) = L
         nfxwL(L) = nfxw
         crestlevxw(nfxw) = zcrest(L)
         shlxw(nfxw) = dzsillu(L)
         if (ifixedweirscheme == 8 .or. ifixedweirscheme == 9) then
            shlxw(nfxw) = max(0.1d0, shlxw(nfxw)) !  in case of the Tabellenboek and Villemonte the ground height left should be at least 0.1 m, as in Simona and Delft3D-FLOW
         end if
         shrxw(nfxw) = dzsilld(L)
         if (ifixedweirscheme == 8 .or. ifixedweirscheme == 9) then
            shrxw(nfxw) = max(0.1d0, shrxw(nfxw)) !  in case of the Tabellenboek and Villemonte the ground height right should be at least 0.1 m, as in Simona and Delft3D-FLOW
         end if
         crestlxw(nfxw) = crestlen(L)
         taludlxw(nfxw) = taludu(L)
         taludrxw(nfxw) = taludd(L)
         vegxw(nfxw) = vegetat(L)
         iweirtxw(nfxw) = iweirtyp(L)
      end if
   end do

   deallocate (ihu, csh, snh, zcrest, dzsillu, dzsilld, crestlen, taludu, taludd, vegetat, iweirtyp, ztoeu, ztoed)
   if (jatabellenboekorvillemonte == 0 .and. jashp_fxw == 0 .and. allocated(shlxw)) then
      deallocate (shlxw, shrxw, crestlevxw, crestlxw, taludlxw, taludrxw, vegxw, iweirtxw)
   end if

   do i = 1, nfxw
      L = lnfxw(i)
      if (L > 0) then
         wu(L) = wu(L) * fixedweircontraction ! TODO: EdG/HK: this will be wrong if MULTIPLE fixed weirs are snapped onto the same flow link (repeated multiplication by fixedweircontraction)
      end if
   end do

   call doclose(minp)

   if (nfxw > 0) then
      call mess(LEVEL_INFO, 'Number of flow Links with fixed weirs :: ', nfxw)
   end if
   if (nh > 0) then
      call mess(LEVEL_INFO, 'Number of flow Links with highlines :: ', nh)
   end if

   call readyy(' ', -1d0)

   if (ifixedweirscheme1D2D > 0) then
      call find_1d2d_fixedweirs(iLink, numcrossedLinks)
   end if

1234 continue

! deallocate
   if (jakdtree == 1) then
      if (allocated(iLink)) deallocate (iLink)
      if (allocated(iPol)) deallocate (iPol)
      if (allocated(dSL)) deallocate (dSL)
   end if

contains

   subroutine check_fixed_weirs_parameters_against_limits()
      use precision, only: dp

      real(kind=dp), parameter :: GROUND_HEIGHT_MINIMUM = -500.0d0
      real(kind=dp), parameter :: GROUND_HEIGHT_MAXIMUM = 500.0d0

      real(kind=dp), parameter :: SLOPE_MINIMUM = -1.0d-8
      real(kind=dp), parameter :: SLOPE_MAXIMUM = 1000.0d0

      real(kind=dp), parameter :: CREST_LEVEL_MAXIMUM = 10000.0d0
      real(kind=dp), parameter :: CREST_LEVEL_MINIMUM = -10000.0d0

      logical :: inside_limits
      integer :: line
      integer :: file_number
      integer :: pli_number
      integer :: pli_first_line
      integer :: location
      character(:), allocatable :: pli_name
      character(:), allocatable :: file_name

      character(len=*), parameter :: DZL_name = 'Ground height left'
      character(len=*), parameter :: DZR_name = 'Ground height right'
      character(len=*), parameter :: DTL_name = 'Slope left'
      character(len=*), parameter :: DTR_name = 'Slope right'
      character(len=*), parameter :: CREST_LEVEL_name = 'Crest level'

      file_number = 1
      pli_number = 1
      file_name = fnames(1)
      pli_name = nampli(1)
      inside_limits = .true.
      pli_first_line = start_npl_for_files(1) - 1

      do line = start_npl_for_files(1), npl
         if (line > start_npl_for_files(file_number + 1)) then
            file_number = file_number + 1
            file_name = fnames(file_number)
         end if
         if (xpl(line) == dmiss) then
            pli_number = pli_number + 1
            pli_first_line = line
            if (pli_number > size(nampli)) then
               pli_name = ' '
            else
               pli_name = nampli(pli_number)
            end if
         else
            location = line - pli_first_line
            if (allocated(DZL)) then
               inside_limits = inside_limits .and. is_value_inside_limits(DZL(line), &
                                                                          GROUND_HEIGHT_MINIMUM, GROUND_HEIGHT_MAXIMUM, DZL_name, file_name, pli_name, location)
            end if
            if (allocated(DZR)) then
               inside_limits = inside_limits .and. is_value_inside_limits(DZR(line), &
                                                                          GROUND_HEIGHT_MINIMUM, GROUND_HEIGHT_MAXIMUM, DZR_name, file_name, pli_name, location)
            end if
            if (allocated(DTL)) then
               inside_limits = inside_limits .and. is_value_inside_limits(DTL(line), &
                                                                          SLOPE_MINIMUM, SLOPE_MAXIMUM, DTL_name, file_name, pli_name, location)
            end if
            if (allocated(DTR)) then
               inside_limits = inside_limits .and. is_value_inside_limits(DTR(line), &
                                                                          SLOPE_MINIMUM, SLOPE_MAXIMUM, DTR_name, file_name, pli_name, location)
            end if
            if (allocated(ZPL)) then
               inside_limits = inside_limits .and. is_value_inside_limits(ZPL(line), &
                                                                          CREST_LEVEL_MINIMUM, CREST_LEVEL_MAXIMUM, CREST_LEVEL_name, file_name, pli_name, location)
            end if
         end if
      end do
      if (.not. inside_limits) then
         call mess(LEVEL_WARN, &
                   'Some fixed weirs have values outside of limits. See messages above. This may give problems when writing the shape file.')
      end if

   end subroutine check_fixed_weirs_parameters_against_limits

!> check_value_and_write_message
   logical function is_value_inside_limits(value_to_be_checked, min_limit, max_limit, value_name, file_name, pli_name, location)
      use precision, only: dp

      real(kind=dp), intent(in) :: value_to_be_checked !< value_to_be_checked
      real(kind=dp), intent(in) :: min_limit !< min_limit
      real(kind=dp), intent(in) :: max_limit !< max_limit
      character(len=*), intent(in) :: value_name !< value_name
      character(len=*), intent(in) :: file_name !< file_name
      character(len=*), intent(in) :: pli_name !< pli_name
      integer, intent(in) :: location !< location

      is_value_inside_limits = .true.
      if (value_to_be_checked < min_limit) then
         is_value_inside_limits = .false.
         write (msgbuf, '(a,d13.5,a,d13.5,a)') trim(value_name), value_to_be_checked, ' is smaller than the minimum limit ', min_limit, '.'
         call mess(LEVEL_WARN, msgbuf)
      end if

      if (value_to_be_checked > max_limit) then
         is_value_inside_limits = .false.
         write (msgbuf, '(a,d13.5,a,d13.5,a)') trim(value_name), value_to_be_checked, ' is larger than the maximum limit ', max_limit, '.'
         call mess(LEVEL_WARN, msgbuf)
      end if

      if (.not. is_value_inside_limits) then
         write (msgbuf, '(2a,i0,5a)') trim(value_name), ' is located at line ', location, ' in PLI ', trim(pli_name), &
            ' of file ', trim(file_name), '.'
         call mess(LEVEL_WARN, msgbuf)
      end if

   end function is_value_inside_limits

end subroutine setfixedweirs
