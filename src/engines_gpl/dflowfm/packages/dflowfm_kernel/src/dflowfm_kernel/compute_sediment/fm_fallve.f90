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

module m_fm_fallve

   implicit none

   private

   public :: fm_fallve

contains

   subroutine fm_fallve()
   !!--description-----------------------------------------------------------------
      !
      !    Function: Relation between sediment concentration
      !              and vertical fall velocity. Model for
      !              hindered settling.
      !              Fall velocity at layer interfaces.
   !!--declarations----------------------------------------------------------------
      use precision
      use m_physcoef, only: ee, ag, sag, vonkar, backgroundsalinity, backgroundwatertemperature, vismol
      use m_sediment, only: stmpar, mtd, sed
      use m_flowtimes, only: time1
      use m_flowgeom, only: ndx, ln, bl, wcl, lnx
      use m_flow, only: iturbulencemodel, kmx, zws, ucxq, ucyq, ucz, s1, z0urou, ucx_mor, ucy_mor
      use m_flowparameters, only: jasal, jatem, epshs, epsz0
      use m_transport, only: constituents, isalt, itemp
      use m_turbulence, only: turkinepsws, rhowat
      use sediment_basics_module, only: SEDTYP_CLAY
      use morphology_data_module
      use message_module, only: write_error
      use unstruc_files, only: mdia
      use m_alloc
      use m_fm_erosed, only: taub, sedtyp
      use flocculation, only: get_tshear_tdiss
      use m_get_kbot_ktop
      !
      implicit none
      !
      real(fp), dimension(:), pointer :: localpar
      real(fp), dimension(:, :), pointer :: ws
      real(fp), pointer :: csoil
      real(fp), dimension(:), pointer :: rhosol
      real(fp), dimension(:, :), pointer :: dss
      real(fp), dimension(:), pointer :: sedd50
      real(fp), dimension(:), pointer :: sedd50fld

      character(256), dimension(:), pointer :: dll_usrfil
      character(256), dimension(:), pointer :: dll_function
      integer(pntrsize), dimension(:), pointer :: dll_handle
      integer, dimension(:), pointer :: iform_settle
      real(fp), dimension(:, :), pointer :: par_settle
      !
      integer, pointer :: npar
      integer, pointer :: max_integers
      integer, pointer :: max_reals
      integer, pointer :: max_strings
      integer, dimension(:), pointer :: dll_integers
      real(hp), dimension(:), pointer :: dll_reals
      character(256), dimension(:), pointer :: dll_strings
      !
      ! Global variables
      integer, pointer :: lsed
      !
      ! Local variables
      !
      integer :: k, kk, k1, k2, L, ll, i, istat, kb, kt
      logical :: error

      real(kind=dp) :: cclay
      real(kind=dp) :: chezy
      real(kind=dp) :: ctot
      real(kind=dp) :: h0
      real(kind=dp) :: rhoint
      real(kind=dp) :: salint
      real(kind=dp) :: temint
      real(kind=dp) :: tka
      real(kind=dp) :: tkb
      real(kind=dp) :: tkt
      real(kind=dp) :: tshear
      real(kind=dp) :: tur_eps
      real(kind=dp) :: tur_k
      real(kind=dp) :: u
      real(kind=dp) :: um
      real(kind=dp) :: v
      real(kind=dp) :: vm
      real(kind=dp) :: w
      real(kind=dp) :: wsloc
      real(kind=dp), dimension(:), allocatable :: z0rou
      real(kind=dp) :: thick

      character(256) :: errmsg
   !!
   !!! executable statements -------------------------------------------------------
   !!
      call realloc(z0rou, ndx, keepExisting=.false., fill=0d0)

      csoil => stmpar%sedpar%csoil
      rhosol => stmpar%sedpar%rhosol
      dss => stmpar%sedpar%dss
      sedd50 => stmpar%sedpar%sedd50
      sedd50fld => stmpar%sedpar%sedd50fld

      npar => stmpar%trapar%npar
      dll_usrfil => stmpar%trapar%dll_usrfil_settle
      dll_function => stmpar%trapar%dll_function_settle
      dll_handle => stmpar%trapar%dll_handle_settle
      iform_settle => stmpar%trapar%iform_settle
      par_settle => stmpar%trapar%par_settle

      max_integers => stmpar%trapar%max_integers_settle
      max_reals => stmpar%trapar%max_reals_settle
      max_strings => stmpar%trapar%max_strings_settle
      dll_integers => stmpar%trapar%dll_integers_settle
      dll_reals => stmpar%trapar%dll_reals_settle
      dll_strings => stmpar%trapar%dll_strings_settle

      lsed => stmpar%lsedsus
      ws => mtd%ws
      !
      allocate (localpar(npar), stat=istat)

      ! Calculate roughness height at cell centres
      do L = 1, lnx
         k1 = ln(1, L); k2 = ln(2, L)
         z0rou(k1) = z0rou(k1) + wcl(1, L) * z0urou(L) ! set for all cases in setcfuhi/getustbcfuhi
         z0rou(k2) = z0rou(k2) + wcl(2, L) * z0urou(L)
      end do

      do k = 1, ndx
         if (s1(k) - bl(k) < epshs) cycle
         !
         h0 = s1(k) - bl(k)
         chezy = sag * log(h0 / ee / max(epsz0, z0rou(k))) / vonkar ! consistency with getczz0
         !
         ! compute depth-averaged velocities
         !
         if (kmx > 0) then ! 3D
            um = 0d0
            vm = 0d0
            call getkbotktop(k, kb, kt)
            do kk = kb, kt
               thick = zws(kk) - zws(kk - 1)
               um = um + thick / h0 * ucxq(kk)
               vm = vm + thick / h0 * ucyq(kk)
            end do
         else
            um = ucxq(k)
            vm = ucyq(k)
         end if
         !
         ! loop over the interfaces in the vertical
         !
         if (kmx > 0) then ! 3D
            call getkbotktop(k, kb, kt)
         else ! 2D
            kb = k
            kt = k + 1
         end if
         !
         do kk = kb, kt - 1
            ! HK: is this better than first establish fallvelocity in a cell, next interpolate to interfaces?

            if (kmx > 0) then ! 3D
               tka = zws(kk + 1) - zws(kk) ! thickness above
               tkb = zws(kk) - zws(kk - 1) ! thickness below
               tkt = tka + tkb
               if (jasal > 0) then
                  salint = max(0.0_fp, (tka * constituents(isalt, kk + 1) + tkb * constituents(isalt, kk)) / tkt)
               else
                  salint = backgroundsalinity
               end if
               !                !
               if (jatem > 0) then
                  temint = (tka * constituents(itemp, kk + 1) + tkb * constituents(itemp, kk)) / tkt
               else
                  temint = backgroundwatertemperature
               end if
               !
               rhoint = (tka * rhowat(kk + 1) + tkb * rhowat(kk)) / tkt
               !
               u = (tka * ucx_mor(kk + 1) + tkb * ucx_mor(kk)) / tkt ! x component
               v = (tka * ucy_mor(kk + 1) + tkb * ucy_mor(kk)) / tkt ! y component
               w = (tka * ucz(kk + 1) + tkb * ucz(kk)) / tkt ! z component

               if (iturbulencemodel == 3) then ! k-eps
                  tur_k = turkinepsws(1, kk)
               else
                  tur_k = -999.0d0
               end if
               if (iturbulencemodel == 3) then
                  tur_eps = turkinepsws(2, kk)
               else
                  tur_eps = -999.0d0
               end if
               if (iturbulencemodel == 3) then ! k-eps
                  call get_tshear_tdiss(tshear, tur_eps, rhoint, tke=tur_k)
               else
                  call get_tshear_tdiss(tshear, tur_eps, rhoint, taub=taub(k), waterdepth=h0, localdepth=s1(k) - zws(kk), vonkar=vonkar)
               end if

            else ! 2D
               if (jasal > 0) then
                  salint = max(0d0, constituents(isalt, k))
               else
                  salint = backgroundsalinity
               end if
               !             !
               if (jatem > 0) then
                  temint = constituents(itemp, k)
               else
                  temint = backgroundwatertemperature
               end if
               !
               rhoint = rhowat(k)
               !
               u = ucx_mor(k) ! x component
               v = ucy_mor(k) ! y component
               w = -999d0 ! z component
               !
               tur_k = -999d0
               tur_eps = -999d0
               call get_tshear_tdiss(tshear, tur_eps, rhoint, taub=taub(k), waterdepth=h0, vonkar=vonkar)
            end if
            !
            ctot = 0d0
            cclay = 0d0
            do ll = 1, lsed
               ctot = ctot + sed(ll, kk)
               if (sedtyp(ll) == SEDTYP_CLAY) cclay = cclay + sed(ll, kk)
            end do
            !
            do ll = 1, lsed
               !
               do i = 1, npar
                  localpar(i) = par_settle(i, ll)
               end do
               !
               ! Pass to DLL, decoupling because of treatment per layer interface
               !
               if (max_reals < WS_MAX_RP) then
                  write (errmsg, '(a,a,a)') 'Insufficient space to pass real values to settling routine.'
                  call write_error(errmsg, unit=mdia)
                  error = .true.
                  return
               end if
               dll_reals(WS_RP_TIME) = real(time1, hp)
               dll_reals(WS_RP_ULOC) = real(u, hp)
               dll_reals(WS_RP_VLOC) = real(v, hp)
               dll_reals(WS_RP_WLOC) = real(w, hp)
               dll_reals(WS_RP_SALIN) = real(salint, hp)
               dll_reals(WS_RP_TEMP) = real(temint, hp)
               dll_reals(WS_RP_RHOWT) = real(rhoint, hp)
               dll_reals(WS_RP_CFRCB) = real(sed(ll, kk), hp)
               dll_reals(WS_RP_CTOT) = real(ctot, hp)
               dll_reals(WS_RP_KTUR) = real(tur_k, hp)
               dll_reals(WS_RP_EPTUR) = real(tur_eps, hp)
               if (sedd50(ll) < 0.0_fp) then
                  dll_reals(WS_RP_D50) = real(sedd50fld(k), hp)
               else
                  dll_reals(WS_RP_D50) = real(sedd50(ll), hp)
               end if
               dll_reals(WS_RP_DSS) = real(dss(k, ll), hp)
               dll_reals(WS_RP_RHOSL) = real(rhosol(ll), hp)
               dll_reals(WS_RP_CSOIL) = real(csoil, hp)
               dll_reals(WS_RP_GRAV) = real(ag, hp)
               dll_reals(WS_RP_VICML) = real(vismol, hp)
               dll_reals(WS_RP_WDEPT) = real(h0, hp)
               dll_reals(WS_RP_UMEAN) = real(um, hp)
               dll_reals(WS_RP_VMEAN) = real(vm, hp)
               dll_reals(WS_RP_CHEZY) = real(chezy, hp)
               dll_reals(WS_RP_SHTUR) = real(tshear, hp)
               dll_reals(WS_RP_CCLAY) = real(cclay, hp)
               !
               if (max_integers < WS_MAX_IP) then
                  write (errmsg, '(a,a,a)') 'Insufficient space to pass integer values to settling routine.'
                  call write_error(errmsg, unit=mdia)
                  error = .true.
                  return
               end if
               dll_integers(WS_IP_NM) = kk
               dll_integers(WS_IP_ISED) = ll
               !
               if (max_strings < WS_MAX_SP) then
                  write (errmsg, '(a,a,a)') 'Insufficient space to pass strings to settling routine.'
                  call write_error(errmsg, unit=mdia)
                  error = .true.
                  return
               end if
               dll_strings(WS_SP_USRFL) = dll_usrfil(ll)
               !             !
               call eqsettle(dll_function(ll), dll_handle(ll), max_integers, max_reals, max_strings, &
                           & dll_integers, dll_reals, dll_strings, mdia, iform_settle(ll),  &
                           & localpar, npar, wsloc, error)
               if (error) then
                  write (errmsg, '(a,a,a)') 'fm_fallve:: Error from eqsettle.'
                  call write_error(errmsg, unit=mdia)
                  error = .true.
                  return
               end if
               !
               ws(kk, ll) = wsloc
            end do ! ll
         end do ! kk
         if (kmx > 1) then
            do ll = 1, lsed
               ws(kb - 1, ll) = ws(kb, ll) ! to check
            end do ! ll
         end if
      end do ! k

      deallocate (localpar, stat=istat)
   end subroutine fm_fallve

end module m_fm_fallve
