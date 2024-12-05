module m_setucxcuy_leastsquare
   use precision, only: dp

   real(kind=dp), dimension(:, :, :), allocatable :: AtWAiAtW ! Matrix for each flow node
   integer, dimension(:), allocatable :: ireconstu
   integer, dimension(:), allocatable :: ireconstz

contains

   ! ==============================================================================================
   ! ==============================================================================================
   subroutine reconst2ndini()
      use precision, only: dp
      use m_flow
      use m_flowgeom
      use m_netw

      implicit none
      integer :: i, j, k1, k2, kk, L, L1, L2, L3, m, mm, n1, n2, ierr
      integer :: L1a, L2a, L3a, k3, nmax
      real(kind=dp) :: cof0, xwall, ywall, scale, cs, sn, Deltxu, Deltyu
      integer, parameter :: mmax = 64
      real(kind=dp), dimension(9, 9) :: AtWA, AtWAi
      real(kind=dp), dimension(9, mmax) :: AtW
      real(kind=dp), dimension(mmax, 9) :: Amat
      real(kind=dp), dimension(mmax) :: Wmat
      real(kind=dp), dimension(9) :: svec
      integer, dimension(lnx) :: LDone
      integer, dimension(mxwalls) :: LwDone

      if (allocated(AtWAiAtW)) deallocate (AtWAiAtW)
      allocate (AtWAiAtW(9, mmax, lnx), stat=ierr); AtWAiAtW = 0d0
      if (allocated(ireconstu)) deallocate (ireconstu)
      allocate (ireconstu(lnx)); ireconstu = 0
      if (allocated(ireconstz)) deallocate (ireconstz)
      allocate (ireconstz(ndx)); ireconstz = 0

      scale = 1d0
      ireconstu = 0
      ireconstz = 0

      do L = 1, lnxi
         m = 0
         AtWA = 0d0; AtWAi = 0d0; AtW = 0d0
         Amat = 0d0
         Wmat = 0d0
         LDone = 0
         LwDone = 0
         k1 = ln(1, L); k2 = ln(2, L)

         do L1 = 1, nd(k1)%nwx
            L1a = abs(nd(k1)%nw(L1))
            if (LwDone(L1a) == 1) cycle
            m = m + 1
            n1 = walls(2, L1a)
            n2 = walls(3, L1a)
            xwall = (xk(n1) + xk(n2)) * 0.5d0
            ywall = (yk(n1) + yk(n2)) * 0.5d0
            Deltxu = xwall - xu(L)
            Deltyu = ywall - yu(L)
            cs = -walls(8, L1a)
            sn = walls(7, L1a)
            Amat(m, 1) = cs
            Amat(m, 2) = sn
            Amat(m, 3) = cs * Deltxu
            Amat(m, 4) = sn * Deltxu
            Amat(m, 5) = cs * Deltyu
            Amat(m, 6) = sn * Deltyu
            Amat(m, 7) = cs * Deltxu * Deltyu
            Amat(m, 8) = sn * Deltxu * Deltyu
            cof0 = sqrt(Deltxu**2 + Deltyu**2)
            if (cof0 > 1d-10) then
               Wmat(m) = 1d0 / cof0
            else
               Wmat(m) = 1d0
            end if
            LwDone(L1a) = 1
         end do

         do L2 = 1, nd(k2)%nwx
            L2a = abs(nd(k2)%nw(L2))
            if (LwDone(L2a) == 1) cycle
            m = m + 1
            n1 = walls(2, L2a)
            n2 = walls(3, L2a)
            xwall = (xk(n1) + xk(n2)) * 0.5d0
            ywall = (yk(n1) + yk(n2)) * 0.5d0
            Deltxu = xwall - xu(L)
            Deltyu = ywall - yu(L)
            cs = -walls(8, L2a)
            sn = walls(7, L2a)
            Amat(m, 1) = cs
            Amat(m, 2) = sn
            Amat(m, 3) = cs * Deltxu
            Amat(m, 4) = sn * Deltxu
            Amat(m, 5) = cs * Deltyu
            Amat(m, 6) = sn * Deltyu
            Amat(m, 7) = cs * Deltxu * Deltyu
            Amat(m, 8) = sn * Deltxu * Deltyu
            cof0 = sqrt(Deltxu**2 + Deltyu**2)
            if (cof0 > 1d-10) then
               Wmat(m) = 1d0 / cof0
            else
               Wmat(m) = 1d0
            end if
            LwDone(L2a) = 1
         end do

         do L1 = 1, nd(k1)%lnx
            L1a = abs(nd(k1)%ln(L1))
            if (L1a == L) cycle
            k3 = ln(1, L1a) + ln(2, L1a) - k1
            do L3 = 1, nd(k3)%lnx
               L3a = abs(nd(k3)%ln(L3))
               if (LDone(L3a) == 1) cycle
               m = m + 1
               cs = csu(L3a)
               sn = snu(L3a)
               Deltxu = xu(L3a) - xu(L)
               Deltyu = yu(L3a) - yu(L)
               Amat(m, 1) = cs
               Amat(m, 2) = sn
               Amat(m, 3) = cs * Deltxu
               Amat(m, 4) = sn * Deltxu
               Amat(m, 5) = cs * Deltyu
               Amat(m, 6) = sn * Deltyu
               Amat(m, 7) = cs * Deltxu * Deltyu
               Amat(m, 8) = sn * Deltxu * Deltyu
               cof0 = sqrt(Deltxu**2 + Deltyu**2)
               if (cof0 > 1.0d-10) then
                  Wmat(m) = 1d0 / cof0
               else
                  Wmat(m) = 1d0
               end if
               LDone(L3a) = 1
            end do
            do L3 = 1, nd(k3)%nwx
               L3a = abs(nd(k3)%nw(L3))
               if (LwDone(L3a) == 1) cycle
               m = m + 1
               n1 = walls(2, L3a)
               n2 = walls(3, L3a)
               xwall = (xk(n1) + xk(n2)) * 0.5d0
               ywall = (yk(n1) + yk(n2)) * 0.5d0
               Deltxu = xwall - xu(L)
               Deltyu = ywall - yu(L)
               cs = -walls(8, L3a)
               sn = walls(7, L3a)
               Amat(m, 1) = cs
               Amat(m, 2) = sn
               Amat(m, 3) = cs * Deltxu
               Amat(m, 4) = sn * Deltxu
               Amat(m, 5) = cs * Deltyu
               Amat(m, 6) = sn * Deltyu
               Amat(m, 7) = cs * Deltxu * Deltyu
               Amat(m, 8) = sn * Deltxu * Deltyu
               cof0 = sqrt(Deltxu**2 + Deltyu**2)
               if (cof0 > 1d-10) then
                  Wmat(m) = 1d0 / cof0
               else
                  Wmat(m) = 1d0
               end if
               LwDone(L3a) = 1
            end do
         end do

         do L2 = 1, nd(k2)%lnx
            L2a = abs(nd(k2)%ln(L2))
            if (L2a == L) cycle
            k3 = ln(1, L2a) + ln(2, L2a) - k2
            do L3 = 1, nd(k3)%lnx
               L3a = abs(nd(k3)%ln(L3))
               if (LDone(L3a) == 1) cycle
               m = m + 1
               cs = csu(L3a)
               sn = snu(L3a)
               Deltxu = xu(L3a) - xu(L)
               Deltyu = yu(L3a) - yu(L)
               Amat(m, 1) = cs
               Amat(m, 2) = sn
               Amat(m, 3) = cs * Deltxu
               Amat(m, 4) = sn * Deltxu
               Amat(m, 5) = cs * Deltyu
               Amat(m, 6) = sn * Deltyu
               Amat(m, 7) = cs * Deltxu * Deltyu
               Amat(m, 8) = sn * Deltxu * Deltyu
               cof0 = sqrt(Deltxu**2 + Deltyu**2)
               if (cof0 > 1d-10) then
                  Wmat(m) = 1d0 / cof0
               else
                  Wmat(m) = 1d0
               end if
               LDone(L3a) = 1
            end do
            do L3 = 1, nd(k3)%nwx
               L3a = abs(nd(k3)%nw(L3))
               if (LwDone(L3a) == 1) cycle
               m = m + 1
               n1 = walls(2, L3a)
               n2 = walls(3, L3a)
               xwall = (xk(n1) + xk(n2)) * 0.5d0
               ywall = (yk(n1) + yk(n2)) * 0.5d0
               Deltxu = xwall - xu(L)
               Deltyu = ywall - yu(L)
               cs = -walls(8, L3a)
               sn = walls(7, L3a)
               Amat(m, 1) = cs
               Amat(m, 2) = sn
               Amat(m, 3) = cs * Deltxu
               Amat(m, 4) = sn * Deltxu
               Amat(m, 5) = cs * Deltyu
               Amat(m, 6) = sn * Deltyu
               Amat(m, 7) = cs * Deltxu * Deltyu
               Amat(m, 8) = sn * Deltxu * Deltyu
               cof0 = sqrt(Deltxu**2 + Deltyu**2)
               if (cof0 > 1d-10) then
                  Wmat(m) = 1d0 / cof0
               else
                  Wmat(m) = 1d0
               end if
               LwDone(L3a) = 1
            end do
         end do

         nmax = 8
         if (m < 8 .and. m >= 6) then
            nmax = 6
            print *, 'm < 8 .and. m >= 6', L
            ireconstu(L) = 1
            cycle
         elseif (m < 6) then
            print *, 'm < 6', m, L
            ireconstu(L) = 1
            cycle
         end if

         do j = 1, m
            do i = 1, nmax
               AtW(i, j) = Amat(j, i) * Wmat(j)
            end do
         end do
         do j = 1, nmax
            do i = 1, nmax
               AtWA(i, j) = 0d0
               do mm = 1, m
                  AtWA(i, j) = AtWA(i, j) + AtW(i, mm) * Amat(mm, j)
               end do
            end do
         end do
         !
         ! === Lagrangian multiplier
         !
         nmax = nmax + 1
         AtWA(nmax, :) = 0d0
         AtWA(:, nmax) = 0d0
         AtWA(nmax, 1) = csu(L)
         AtWA(nmax, 2) = snu(L)
         AtWA(1, nmax) = csu(L)
         AtWA(2, nmax) = snu(L)
         m = m + 1
         AtW(nmax, :) = 0d0
         Atw(:, m) = 0d0
         Atw(nmax, m) = 1d0

         AtWAi = AtWA
         svec = 0d0 ! dummy vector
         call gaussj1(AtWAi, nmax, nmax, svec, 1, 1, ierr)
         if (ierr /= 0) then
            ireconstu(L) = 1
            cycle
         end if

         do j = 1, m
            do i = 1, nmax
               AtWAiAtW(i, j, L) = 0d0
               do kk = 1, nmax
                  AtWAiAtW(i, j, L) = AtWAiAtW(i, j, L) + AtWAi(i, kk) * AtW(kk, j)
               end do
            end do
         end do
      end do

   end subroutine reconst2ndini

   ! ==============================================================================================
   ! ==============================================================================================
   subroutine reconst2nd()
      use precision, only: dp
      use m_flow
      use m_flowgeom
      use m_netw
      use m_get_Lbot_Ltop
      use m_links_to_centers, only: links_to_centers

      implicit none
      integer :: i, j, k1, k2, L, L1, L2, L3, m
      integer :: L1a, L2a, L3a, L3b, L3t, k3, nmax, LL, Lb, Lt, kk1, kk2
      integer, parameter :: mmax = 64
      real(kind=dp), dimension(mmax) :: bvec
      real(kind=dp), dimension(9) :: xvec
      integer, dimension(lnx) :: LDone
      integer, dimension(mxwalls) :: LwDone
      real(kind=dp), dimension(:), allocatable :: uxu, uyu

      if (.not. allocated(uxu)) then
         allocate (uxu(lnkx), uyu(lnkx)); uxu = 0d0; uyu = 0d0
      end if

      ucx = 0d0; ucy = 0d0

      if (kmx == 0) then !2D
         do L = 1, lnxi
            if (hu(L) < epshu) then
               v(L) = 0d0
               cycle
            end if

            k1 = ln(1, L); k2 = ln(2, L)
            if (ireconstu(L) == 1) then
               call perotnode2d(k1)
               call perotnode2d(k2)
               v(L) = (1d0 - acl(L)) * (-snu(L) * ucx(k1) + csu(L) * ucy(k1)) + acl(L) * (-snu(L) * ucx(k2) + csu(L) * ucy(k2))
               ireconstz(k1) = 1
               ireconstz(k2) = 1
               cycle
            end if
            m = 0
            bvec = 0d0
            LDone = 0
            LwDone = 0

            do L1 = 1, nd(k1)%nwx
               L1a = abs(nd(k1)%nw(L1))
               if (LwDone(L1a) == 1) cycle
               m = m + 1
               bvec(m) = 0d0
               LwDone(L1a) = 1
            end do

            do L2 = 1, nd(k2)%nwx
               L2a = abs(nd(k2)%nw(L2))
               if (LwDone(L2a) == 1) cycle
               m = m + 1
               bvec(m) = 0d0
               LwDone(L2a) = 1
            end do

            do L1 = 1, nd(k1)%lnx
               L1a = abs(nd(k1)%ln(L1))
               if (L1a == L) cycle
               k3 = ln(1, L1a) + ln(2, L1a) - k1
               do L3 = 1, nd(k3)%lnx
                  L3a = abs(nd(k3)%ln(L3))
                  if (LDone(L3a) == 1) cycle
                  m = m + 1
                  bvec(m) = u1(L3a)
                  LDone(L3a) = 1
               end do
               do L3 = 1, nd(k3)%nwx
                  L3a = abs(nd(k3)%nw(L3))
                  if (LwDone(L3a) == 1) cycle
                  m = m + 1
                  bvec(m) = 0d0
                  LwDone(L3a) = 1
               end do
            end do

            do L2 = 1, nd(k2)%lnx
               L2a = abs(nd(k2)%ln(L2))
               if (L2a == L) cycle
               k3 = ln(1, L2a) + ln(2, L2a) - k2
               do L3 = 1, nd(k3)%lnx
                  L3a = abs(nd(k3)%ln(L3))
                  if (LDone(L3a) == 1) cycle
                  m = m + 1
                  bvec(m) = u1(L3a)
                  LDone(L3a) = 1
               end do
               do L3 = 1, nd(k3)%nwx
                  L3a = abs(nd(k3)%nw(L3))
                  if (LwDone(L3a) == 1) cycle
                  m = m + 1
                  bvec(m) = 0d0
                  LwDone(L3a) = 1
               end do
            end do

            nmax = 8
            if (m < 8 .and. m >= 6) then
               nmax = 6
            end if
            !
            ! === Lagrangian multiplier
            !
            nmax = nmax + 1
            m = m + 1
            bvec(m) = u1(L)

            do i = 1, 2 !nmax
               xvec(i) = 0d0
               do j = 1, m
                  xvec(i) = xvec(i) + AtWAiAtW(i, j, L) * bvec(j)
               end do
            end do
            uxu(L) = xvec(1)
            uyu(L) = xvec(2)
            v(L) = -snu(L) * uxu(L) + csu(L) * uyu(L)
         end do
         do L = lnxi + 1, lnx
            uxu(L) = csu(L) * u1(L)
            uyu(L) = snu(L) * u1(L)
         end do

      else ! 3D =================================

         do L = 1, lnxi
            call getLbotLtop(L, Lb, Lt)
            k1 = ln(1, L); k2 = ln(2, L)
            do LL = Lb, Lt
               if (ireconstu(L) == 1) then
                  kk1 = ln(1, LL); kk2 = ln(2, LL)
                  call perotnode3d(k1, kk1)
                  call perotnode3d(k2, kk2)
                  v(LL) = (1d0 - acl(L)) * (-snu(L) * ucx(kk1) + csu(L) * ucy(kk1)) + acl(L) * (-snu(L) * ucx(kk2) + csu(L) * ucy(kk2))
                  ireconstz(k1) = 1
                  ireconstz(k2) = 1
                  cycle
               end if
               m = 0
               bvec = 0d0
               LDone = 0
               LwDone = 0

               do L1 = 1, nd(k1)%nwx
                  L1a = abs(nd(k1)%nw(L1))
                  if (LwDone(L1a) == 1) cycle
                  m = m + 1
                  bvec(m) = 0d0
                  LwDone(L1a) = 1
               end do

               do L2 = 1, nd(k2)%nwx
                  L2a = abs(nd(k2)%nw(L2))
                  if (LwDone(L2a) == 1) cycle
                  m = m + 1
                  bvec(m) = 0d0
                  LwDone(L2a) = 1
               end do

               do L1 = 1, nd(k1)%lnx
                  L1a = abs(nd(k1)%ln(L1))
                  if (L1a == L) cycle
                  k3 = ln(1, L1a) + ln(2, L1a) - k1
                  do L3 = 1, nd(k3)%lnx
                     L3a = abs(nd(k3)%ln(L3))
                     if (LDone(L3a) == 1) cycle
                     m = m + 1
                     call getLbotLtop(L3a, L3b, L3t)
                     bvec(m) = u1(L3b + LL - Lb)
                     LDone(L3a) = 1
                  end do
                  do L3 = 1, nd(k3)%nwx
                     L3a = abs(nd(k3)%nw(L3))
                     if (LwDone(L3a) == 1) cycle
                     m = m + 1
                     bvec(m) = 0d0
                     LwDone(L3a) = 1
                  end do
               end do

               do L2 = 1, nd(k2)%lnx
                  L2a = abs(nd(k2)%ln(L2))
                  if (L2a == L) cycle
                  k3 = ln(1, L2a) + ln(2, L2a) - k2
                  do L3 = 1, nd(k3)%lnx
                     L3a = abs(nd(k3)%ln(L3))
                     if (LDone(L3a) == 1) cycle
                     m = m + 1
                     call getLbotLtop(L3a, L3b, L3t)
                     bvec(m) = u1(L3b + LL - Lb)
                     LDone(L3a) = 1
                  end do
                  do L3 = 1, nd(k3)%nwx
                     L3a = abs(nd(k3)%nw(L3))
                     if (LwDone(L3a) == 1) cycle
                     m = m + 1
                     bvec(m) = 0d0
                     LwDone(L3a) = 1
                  end do
               end do

               nmax = 8
               if (m < 8 .and. m >= 6) then
                  nmax = 6
               end if
               !
               ! === Lagrangian multiplier
               !
               nmax = nmax + 1
               m = m + 1
               bvec(m) = u1(LL)

               do i = 1, 2 !nmax
                  xvec(i) = 0d0
                  do j = 1, m
                     xvec(i) = xvec(i) + AtWAiAtW(i, j, L) * bvec(j)
                  end do
               end do
               uxu(LL) = xvec(1)
               uyu(LL) = xvec(2)
               v(LL) = -snu(L) * uxu(LL) + csu(L) * uyu(LL)
            end do
         end do
         do L = lnxi + 1, lnx
            call getLbotLtop(L, Lb, Lt)
            do LL = Lb, Lt
               uxu(LL) = csu(L) * u1(LL)
               uyu(LL) = snu(L) * u1(LL)
            end do
         end do
      end if

      ucx = 0d0
      ucy = 0d0
      call links_to_centers(ucx, uxu)
      call links_to_centers(ucy, uyu)

      do L = 1, lnxi
         if (hu(L) < epshu) then
            v(L) = 0d0
            cycle
         end if
         k1 = ln(1, L); k2 = ln(2, L)
         if (ireconstu(L) == 1) then
            call perotnode2d(k1)
            call perotnode2d(k2)
            v(L) = acl(L) * (-snu(L) * ucx(k1) + csu(L) * ucy(k1)) + (1d0 - acl(L)) * (-snu(L) * ucx(k2) + csu(L) * ucy(k2))
            ireconstz(k1) = 1
            ireconstz(k2) = 1
            cycle
         end if
      end do

   end subroutine reconst2nd

! =================================================================================================
! =================================================================================================
   subroutine perotnode2d(k)
      use m_flow
      use m_flowgeom

      implicit none
      integer :: k, L, L1, La

      ucx(k) = 0d0
      ucy(k) = 0d0
      do L1 = 1, nd(k)%lnx
         L = nd(k)%ln(L1); La = abs(L)
         if (L < 0) then
            ucx(k) = ucx(k) + wcx1(La) * u1(La)
            ucy(k) = ucy(k) + wcy1(La) * u1(La)
         else
            ucx(k) = ucx(k) + wcx2(La) * u1(La)
            ucy(k) = ucy(k) + wcy2(La) * u1(La)
         end if
      end do

   end subroutine perotnode2d

   ! ==============================================================================================
   ! ==============================================================================================
   subroutine perotnode3d(kk, k)
      use m_flow
      use m_flowgeom
      use m_get_kbot_ktop
      use m_get_Lbot_Ltop

      implicit none
      integer, intent(in) :: kk, k
      integer :: kb, kt, L, L1, La, Lb, Lt, LL

      call getkbotktop(kk, kb, kt)
      ucx(k) = 0d0
      ucy(k) = 0d0
      do L1 = 1, nd(kk)%lnx
         L = nd(kk)%ln(L1)
         La = abs(L)
         call getLbotLtop(La, Lb, Lt)
         LL = Lb + k - kb
         if (L < 0) then
            ucx(k) = ucx(k) + wcx1(La) * u1(LL)
            ucy(k) = ucy(k) + wcy1(La) * u1(LL)
         else
            ucx(k) = ucx(k) + wcx2(La) * u1(LL)
            ucy(k) = ucy(k) + wcy2(La) * u1(LL)
         end if
      end do

   end subroutine perotnode3d

   ! ==============================================================================================
   ! ==============================================================================================
   subroutine gaussj1(a, n, np, b, m, mp, ierr)
      use precision, only: dp

      implicit none
      integer :: m, mp, n, np, ierr
      integer :: i, icol, irow, j, k, l, ll
      integer, dimension(np) :: indxc, indxr, ipiv
      real(kind=dp) :: big, dum, pivinv
      real(kind=dp), dimension(np, np) :: a
      real(kind=dp), dimension(np, mp) :: b

      ierr = 0
      do j = 1, n
         ipiv(j) = 0
      end do
      do i = 1, n
         big = 0d0
         do j = 1, n
            if (ipiv(j) /= 1) then
               do k = 1, n
                  if (ipiv(k) == 0) then
                     if (abs(a(j, k)) >= big) then
                        big = abs(a(j, k))
                        irow = j
                        icol = k
                     end if
                  elseif (ipiv(k) > 1) then
                     !print*, 'singular matrix in gaussj'
                     ierr = 1
                     return
                  end if
               end do
            end if
         end do
         ipiv(icol) = ipiv(icol) + 1
         if (irow /= icol) then
            do l = 1, n
               dum = a(irow, l)
               a(irow, l) = a(icol, l)
               a(icol, l) = dum
            end do
            do l = 1, m
               dum = b(irow, l)
               b(irow, l) = b(icol, l)
               b(icol, l) = dum
            end do
         end if
         indxr(i) = irow
         indxc(i) = icol
         if (abs(a(icol, icol)) < 1d-10) then
            !print*, 'singular matrix in gaussj'
            ierr = 1
            return
         end if
         pivinv = 1d0 / a(icol, icol)
         a(icol, icol) = 1d0
         do l = 1, n
            a(icol, l) = a(icol, l) * pivinv
         end do
         do l = 1, m
            b(icol, l) = b(icol, l) * pivinv
         end do
         do ll = 1, n
            if (ll /= icol) then
               dum = a(ll, icol)
               a(ll, icol) = 0d0
               do l = 1, n
                  a(ll, l) = a(ll, l) - a(icol, l) * dum
               end do
               do l = 1, m
                  b(ll, l) = b(ll, l) - b(icol, l) * dum
               end do
            end if
         end do
      end do
      do l = n, 1, -1
         if (indxr(l) /= indxc(l)) then
            do k = 1, n
               dum = a(k, indxr(l))
               a(k, indxr(l)) = a(k, indxc(l))
               a(k, indxc(l)) = dum
            end do
         end if
      end do

   end subroutine gaussj1

   ! ==============================================================================================
   ! ==============================================================================================
   subroutine qrdcmp(a, n, np, c, d, sing)
      use precision, only: dp

      implicit none
      integer :: n, np
      integer :: i, j, k
      real(kind=dp) :: scale, sigma, sum, tau
      real(kind=dp), dimension(np, np) :: a
      real(kind=dp), dimension(n) :: c, d
      logical sing

      sing = .false.
      do k = 1, n - 1
         scale = 0d0
         do i = k, n
            scale = max(scale, abs(a(i, k)))
         end do
         if (scale == 0d0) then
            sing = .true.
            c(k) = 0d0
            d(k) = 0d0
         else
            do i = k, n
               a(i, k) = a(i, k) / scale
            end do
            sum = 0d0
            do i = k, n
               sum = sum + a(i, k)**2
            end do
            sigma = sign(sqrt(sum), a(k, k))
            a(k, k) = a(k, k) + sigma
            c(k) = sigma * a(k, k)
            d(k) = -scale * sigma
            do j = k + 1, n
               sum = 0d0
               do i = k, n
                  sum = sum + a(i, k) * a(i, j)
               end do
               tau = sum / c(k)
               do i = k, n
                  a(i, j) = a(i, j) - tau * a(i, k)
               end do
            end do
         end if
      end do
      d(n) = a(n, n)
      if (d(n) == 0d0) sing = .true.

   end subroutine qrdcmp

end module m_setucxcuy_leastsquare
