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

 subroutine setprofs1D()
    use precision, only: dp

    use m_closeto1dnetlink, only: closeto1dnetlink
    use m_readprofilesloc
    use m_readprofilesdef
    use m_flowgeom
    use m_flow
    use UNSTRUC_MODEL
    use m_netw
    use m_profiles
    use m_missing
    use unstruc_messages
    use m_partitioninfo
    use stdlib_sorting, only: sort_index
    use geometry_module, only: dbdistance
    use m_sferic, only: jsferic, jasfer3D
    use m_samples
    use m_qnerror
    use m_set_branch_lc

    implicit none
    integer :: ierr, MINP, LS, L, K, IBR, LL, LA, K1, K2, KA, KB, NRL, KK, ja, ium
    real(kind=dp) :: XL, ALFA
    logical :: jawel
    character(len=256) :: fnam
    integer, allocatable :: LSAM(:) ! sample K IS ON NET LINK LSAM
    integer, allocatable :: NSbr(:) ! nr of profiles on branch

    integer :: NSBRMX ! MX NR OF PROFILES ON BRANCH
    integer, allocatable :: IDX(:) ! INDEX ARR, SIZE = NSBRMX
    integer, allocatable :: KLH(:), KLHH(:) ! INDEX  ARR, + SORTED BY IDX
    real(kind=dp), allocatable :: XLH(:) ! LENGTH ARR
    real(kind=dp), allocatable :: ZLH(:) ! VALUE  ARR

    type tKBSAM !< TEMP
       integer, allocatable :: KS(:) !< successive SAMPLE nrs ON BRANCH
    end type tKBSAM
    type(TKBSAM), dimension(:), allocatable :: KBSAM ! ARRAY OF SAMPLES PER BRANCH

    real(kind=dp), dimension(:), allocatable :: XLLin
    real(kind=dp), dimension(:), allocatable :: XLsam ! link and sample line distances

    real(kind=dp), dimension(:), allocatable :: distsam ! distance from sample to link
    integer, dimension(:), allocatable :: iconnsam ! globally connected branch number associated with sample

    real(kind=dp), dimension(:), allocatable :: zkk, wkk ! help interpolate zk in profiles if dmiss

    real(kind=dp) :: XLS, YLS, XLB, DXB, dum, ZA, ZB, wa, wb, zul, wul
    integer :: kn3now

    if (jampi /= 1) then
       if (lnx1D == 0) return
    end if

    fnam = trim(md_proflocfile)
    inquire (file=trim(fnam), exist=jawel)

    if (jawel) then

       call oldfil(minp, fnam)

       call readprofilesloc(minp) ! read profloc

       call readprofilesdef(ja) ! read profdef

       if (ja == 0) then
          call qnerror(' Profs not ok. ', ' ', ' ')
          return
       end if

       if (nproflocs > 0) then

          call SETBRANCH_LC(ium)

          if (jampi == 1) then
             call global_netbranch_numbering()
          end if

          if (allocated(xllin)) then
             deallocate (xllin)
          end if
          ierr = 0
          allocate (XLLIN(numL), stat=ierr)
          call aerr('XLLIN(numL)', ierr, numL)

          allocate (XLSAM(Nproflocs), stat=ierr)
          call aerr('XLSAM(Nproflocs)  ', ierr, Nproflocs)

          allocate (LSAM(Nproflocs), stat=ierr)
          call aerr('LSAM(Nproflocs)   ', ierr, Nproflocs)

          if (jampi == 1) then
             allocate (distsam(Nproflocs), stat=ierr)
             call aerr('distsam(Nproflocs)', ierr, Nproflocs)

             allocate (iconnsam(Nproflocs), stat=ierr)
             call aerr('iconnsam(Nproflocs)', ierr, Nproflocs)
          end if

          xlsam = 0d0
          distsam = 1d99
          iconnsam = 0
          if (Lnx1D > 0) then
             do ibr = 1, mxnetbr ! SET UP BRANCH DISTANCE COORDINATE
                if (jampi == 0) then
                   XLB = 0d0
                else
                   XLB = netbr(ibr)%doff
                end if
                do LL = 1, netbr(ibr)%NX
                   L = netbr(ibr)%ln(LL); LA = abs(L)
                   if (L > 0) then
                      k1 = kn(1, La); k2 = kn(2, LA)
                   else
                      k2 = kn(1, La); k1 = kn(2, LA)
                   end if
                   dxB = dbdistance(xk(k1), yk(k1), xk(k2), yk(k2), jsferic, jasfer3D, dmiss)
                   XLB = XLB + dxB
                   XLLIN(LA) = xLB
                end do
             end do

             do K = 1, nproflocs ! SET UP BRANCH DISTANCE COORDINATE OF SAMPLE POINTS
                if (jampi == 0) then
                   call CLOSETO1Dnetlink(Xpr(K), Ypr(K), LS, XLS, YLS, dum, 1)
                else
                   call CLOSETO1Dnetlink(Xpr(K), Ypr(K), LS, XLS, YLS, distsam(k), 1)
                   if (LS > 0) then
                      ibr = LC(LS)
                      iconnsam(k) = netbr(ibr)%iconn
                   end if
                end if

                if (LS == 0) then
                   cycle
                end if

                NRL = NRLB(LS)
                K1 = K1BR(NRL)
                if (K1 == KN(1, LS)) then ! K1 = FIRST IN BRANCH, K2 = SECOND
                   K2 = KN(2, LS)
                else
                   K2 = K1
                   K1 = KN(2, LS)
                end if

                XLSAM(K) = XLLIN(LS) - DBDISTANCE(XLS, YLS, XK(K2), YK(K2), jsferic, jasfer3D, dmiss)

                LSAM(K) = LS

                !DO L = 1,NUML
                !   IF (LC(L) == LC(LS)) THEN
                !      LF = LNE2LN(L)
                !      PROF1D(1,LF) = ZS(K)
                !   ENDIF
                !ENDDO

             end do

          end if

! parallel: reduce XLSAM and the connected branch numbers
          if (jampi == 1) then
             call reduce_xlsam(Nproflocs, xlsam, distsam, iconnsam)
!       else
!          write(6,*) (xlsam(k), k=1,Nproflocs)
          end if

          if (Lnx1D > 0) then
             do ibr = 1, mxnetbr ! SET UP BRANCH AGAIN, NOW WITH LINK POSITIONS
                if (jampi == 0) then
                   XLB = 0d0
                else
                   XLB = netbr(ibr)%doff
                end if
                do LL = 1, netbr(ibr)%NX
                   L = netbr(ibr)%ln(LL); LA = abs(L)
                   if (L > 0) then
                      k1 = kn(1, La); k2 = kn(2, LA)
                   else
                      k2 = kn(1, La); k1 = kn(2, LA)
                   end if
                   dxB = dbdistance(xk(k1), yk(k1), xk(k2), yk(k2), jsferic, jasfer3D, dmiss)
                   XLB = XLB + dxB
                   XLLIN(LA) = xLB - 0.5d0 * DXB
                end do
             end do

             allocate (NSBR(MXNETBR), STAT=IERR); NSBR = 0
             call AERR('NSBR (MXNETBR)', IERR, MXNETBR)
             allocate (KBSAM(MXNETBR), STAT=IERR)
             call AERR('KBSAM(MXNETBR)', IERR, MXNETBR)

             do K = 1, Nproflocs ! COUNT NR OF SAMPLES PER BRANCH
                L = LSAM(K)
                IBR = LC(L)
                if (jampi == 0) then
                   NSBR(IBR) = NSBR(IBR) + 1
                else
                   !            parallel: add profile to all branches that are in the corresponding connected branch
                   do ibr = 1, mxnetbr
                      if (iconnsam(k) == netbr(ibr)%iconn) then
                         NSBR(IBR) = NSBR(IBR) + 1
                      end if
                   end do
                end if
             end do

             do IBR = 1, MXNETBR ! ALLOC BRANCH SAMPLES BACKREF.
                if (NSBR(IBR) > 0) then
                   allocate (KBSAM(IBR)%KS(NSBR(IBR)))
                end if
             end do

             NSBR = 0; NSBRMX = 0
             do K = 1, Nproflocs ! REFER BACK TO SAMPLES ON BRANCH
                L = LSAM(K)
                IBR = LC(L)
                if (jampi == 0) then
                   NSBR(IBR) = NSBR(IBR) + 1
                   KBSAM(IBR)%KS(NSBR(IBR)) = K
                   NSBRMX = max(NSBRMX, NSBR(IBR))
                else
                   do ibr = 1, mxnetbr
                      if (iconnsam(k) == netbr(ibr)%iconn) then
                         NSBR(IBR) = NSBR(IBR) + 1
                         KBSAM(IBR)%KS(NSBR(IBR)) = K
                         NSBRMX = max(NSBRMX, NSBR(IBR))
                      end if
                   end do
                end if
             end do

             allocate (KLH(NSBRMX), XLH(NSBRMX), ZLH(NSBRMX), IDX(NSBRMX), KLHH(NSBRMX))

             do IBR = 1, MXNETBR ! ORDER SAMPLES ON BRANCH AND INTERP LINKS INTO IT

                if (NSBR(IBR) > 0) then ! ER ZITTEN PROFIELEN OP
                   do KK = 1, NSBR(IBR)
                      K = KBSAM(IBR)%KS(KK)
                      XLH(KK) = XLSAM(K)
                      !ZLH(KK)  = Zpr(K)
                      KLH(KK) = K
                   end do
                   call sort_index(XLH(1:NSBR(IBR)), IDX(1:NSBR(IBR)))
                   do KK = 1, NSBR(IBR) ! NU GESORTEERD NAAR AFSTAND
                      !ZLHH(KK) = ZLH(IDX(KK))
                      KLHH(KK) = KLH(IDX(KK))
                   end do

                   K1 = 0; K2 = 1
                   do LL = 1, netbr(ibr)%NX
                      ! NOTE: vulnerability: netbr(:)%ln(:) contains NETlinks (see SETBRANCH_LC()), but it is used below as FLOWlinks
                      !       Not a problem as long as *no* netlinks are discarded during geominit. (Then: numl1d == lnx1d.)
                      LA = abs(NETBR(IBR)%LN(LL))
                      XL = XLLIN(LA)
                      do while (XL > XLH(K2) .and. K2 < NSBR(IBR))
                         K2 = K2 + 1; K1 = K1 + 1
                      end do

                      if (XL > XLH(K2)) then
                         K1 = K2
                      end if

                      if (K1 == 0) then ! IN FIRST SEGMENT, VALUE IS THAT OF K1
                         ALFA = 0d0
                      else if (K1 == NSBR(IBR)) then ! IN LAST  SEGMENT, VALUE IS THAT OF K2
                         ALFA = 1d0
                      else ! IN BETWEEN, REGULAR INTERPOLATION
                         ALFA = (XL - XLH(K1)) / (XLH(K2) - XLH(K1))
                      end if
                      if (K1 == 0) then
                         KA = KLHH(K2)
                      else
                         KA = KLHH(K1)
                      end if
                      if (K1 == NSBR(IBR)) then
                         KB = KLHH(K1)
                      else
                         KB = KLHH(K2)
                      end if
                      KA = NPR(KA); KB = NPR(KB)
                      if (profiles1D(ka)%ityp <= 3 .and. profiles1D(ka)%ityp == profiles1D(kb)%ityp) then ! identical simple profs are interpolated immediately
                         PROF1D(1, LA) = (1d0 - alfa) * profiles1D(ka)%width + alfa * profiles1D(kb)%width
                         PROF1D(2, LA) = (1d0 - alfa) * profiles1D(ka)%height + alfa * profiles1D(kb)%height
                         PROF1D(3, LA) = PROFILES1D(KA)%ITYP
                      else ! POINTEREN VOOR YZPROF OR MIXED PROFILE TYPES
                         PROF1D(1, LA) = -KA
                         PROF1D(2, LA) = -KB
                         PROF1D(3, LA) = ALFA
                      end if

                      !call mess('profile interpolation ready',nproflocs)                                     ! EN NU IS DE INTERPOLATIE KLAAR

                   end do

                end if

             end do

             deallocate (IBN, LIB, K1BR, NRLB, KLH, XLH, ZLH, IDX, KLHH)

             if (jainterpolatezk1D > 0) then

                allocate (zkk(numk), wkk(numk))
                do kn3now = 6, 1, -5
                   wkk = 0d0; zkk = 0d0
                   do L = 1, lnx1D
                      if (abs(kcu(L)) == 1 .and. kn(3, ln2lne(L)) == kn3now) then ! regular 1D links
                         KA = PROF1D(1, L)
                         KB = PROF1D(2, L)
                         if (KA < 0 .and. KB < 0) then ! for which profile pointering exists
                            ALFA = PROF1D(3, L)
                            ZA = profiles1D(-KA)%ZMIN
                            ZB = profiles1D(-KB)%ZMIN
                            WA = profiles1D(-KA)%width
                            WB = profiles1D(-KB)%width
                            zuL = (1d0 - ALFA) * ZA + ALFA * ZB ! z on these links
                            wuL = 1d0 ! (1d0-ALFA)*WA + ALFA*WB  ! z on these links
                            LL = abs(ln2lne(L))
                            k = kn(1, LL)
                            zkk(k) = zkk(k) + zul * wuL
                            wkk(k) = wkk(k) + wuL
                            k = kn(2, LL)
                            zkk(k) = zkk(k) + zul * wuL
                            wkk(k) = wkk(k) + wuL
                         end if
                      end if
                   end do
                   do k = 1, numk
                      if (zk(k) == dmiss .or. zk(k) == zkuni) then
                         if (wkk(k) /= 0) then
                            zk(k) = zkk(k) / wkk(k)
                         end if
                      end if
                   end do
                end do
                deallocate (zkk, wkk)
             end if

          end if ! if ( Lnx1D.gt.0 ) then
       end if ! if (nproflocs > 0) then

!   parallel: reduce nonlin
       if (jampi == 1) then
          call reduce_key(nonlin)
       end if

       call restoresam()
       deallocate (XLLIN, XLSAM)
       if (Lnx1D > 0) then
          if (allocated(NSBR)) deallocate (NSBR)
          if (allocated(KBSAM)) deallocate (KBSAM)
       end if
       deallocate (xpr, ypr, zpr, npr)

       if (jampi == 1) then
          if (allocated(distsam)) deallocate (distsam)
          if (allocated(iconnsam)) deallocate (iconnsam)
       end if

    end if

    !call duikerstoprofs()

 end subroutine setprofs1D
