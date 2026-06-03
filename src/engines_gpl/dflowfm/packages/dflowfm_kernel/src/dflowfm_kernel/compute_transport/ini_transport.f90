!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
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

!> initialize transport, set the enumerators
module m_ini_transport
   use precision, only: dp

   implicit none(type, external)

   private

   public :: ini_transport

contains

   subroutine ini_transport()
      use m_alloc_transport, only: alloc_transport
      use m_transport
      use m_flowparameters
      use m_sediment
      use m_physcoef
      use fm_external_forcings_data
      use string_module
      use Messagehandling
      use m_flow, only: kmx
      use m_find_name, only: find_name

      use m_fm_wq_processes
      use m_alloc
      use unstruc_model, only: md_thetav_waq
      use m_bedform, only: bfmpar

      character(len=8) :: str
      character(len=256) :: msg

      integer :: i, itrace, ised, isf, ifrac, isys
      logical :: allocate_transport

      numconst = 0
      isalt = 0
      itemp = 0
      ioxy = 0
      ised1 = 0
      isedn = 0
      ispir = 0
      itra1 = 0
      itran = 0

      if (jasal /= 0) then
         numconst = numconst + 1
         isalt = numconst
      end if

      if (temperature_model /= TEMPERATURE_MODEL_NONE) then
         numconst = numconst + 1
         itemp = numconst
      end if

      if (jased /= 0) then
         if (mxgr > 0) then
            numconst = numconst + 1
            ised1 = numconst

            numconst = numconst + mxgr - 1
            isedn = numconst
         end if
      end if

      if (jasecflow > 0 .and. jaequili == 0 .and. kmx == 0) then
         numconst = numconst + 1
         ispir = numconst
      end if

      if (numtracers > 0) then
         numconst = numconst + 1
         itra1 = numconst
         numconst = numconst + numtracers - 1
         itran = numconst
         
         ! Lookup oxygen tracer index, if present.
         do i = itra1, itran
            if (trim(str_tolower(const_names(i))) == 'oxy') then
               ioxy = i
            end if
         end do
      end if

      select case (jatransportautotimestepdiff)
      case (0)
!     limitation of diffusion
         jalimitdiff = 1
         jalimitdtdiff = 0
      case (1)
!     limitation of transport time step due to diffusion
         jalimitdiff = 0
         jalimitdtdiff = 1
      case (2)
!     no limitation of transport time step due to diffusion, and no limitation of diffusion
         jalimitdiff = 0
         jalimitdtdiff = 0
      case (3)
!     only for 2D, implicit
         jalimitdiff = 3
         jalimitdtdiff = 0
      case default ! as 0
         jalimitdiff = 1
         jalimitdtdiff = 0
      end select

      !Allocate arrays for transport, if necessary.
      allocate_transport = .false.
      if (numconst > 0 .or. bfmpar%lfbedfrm) then
         allocate_transport = .true.
      end if
      if (jased > 0 .and. stm_included) then
         if (stmpar%morlyr%settings%active_layer_diffusion > 0) then !`morlyr` is undefined if `jased=0`, but it is associated, so you cannot check `associated`.
            allocate_transport = .true.
         end if
      end if
      if (allocate_transport) then
         call alloc_transport(.false.)
      end if

      if (isalt > 0) then
         if (javasal == 6) then
            thetavert(isalt) = 0.0_dp ! Ho explicit
         else
            thetavert(isalt) = tetav ! Central implicit
         end if
         const_names(isalt) = 'salt'
      end if

      if (itemp > 0) then
         if (javatem == 6) then
            thetavert(itemp) = 0.0_dp ! Ho explicit
         else
            thetavert(itemp) = tetav ! Central implicit  0.55d0
         end if
         const_names(itemp) = 'temperature'
      end if

      if (ised1 > 0) then
         if (javased == 6) then
            thetavert(ised1:isedn) = 0.0_dp
         else
            thetavert(ised1:isedn) = tetav
         end if
         if (.not. stm_included) then ! Andere naamgeving in flow_sedmorinit, fracties van sed file
            do i = ised1, isedn
               ised = i - ised1 + 1
               write (str, "(I0)") ised
               const_names(i) = 'sediment_'//trim(str)
            end do
         else
            !
            maserrsed = 0.0_dp ! initialise mass error counter
            !
            ! Map fraction names from sed to constituents (moved from ini_transport)
            !
            do i = ised1, isedn
               ised = i - ised1 + 1
               const_names(i) = trim(stmpar%sedpar%NAMSED(sedtot2sedsus(ised))) ! JRE - netcdf output somehow does not tolerate spaces in varnames?
            end do
            !
            !   Map sfnames to const_names
            !
            if (numfracs > 0) then
               do isf = 1, stmpar%lsedsus ! dimension of sed constituents
                  ifrac = find_name(sfnames, const_names(isf + ised1 - 1))
                  if (ifrac > 0) then
                     ifrac2const(ifrac) = isf + ised1 - 1
                  else
                     call mess(LEVEL_WARN, 'ini_transport(): fraction '//trim(const_names(isf + ised1 - 1))//' has a neumann bc assigned. If that is not what was intended, check fraction name in the sedfracbnd definition.')
                  end if
               end do
            end if ! numfracs
         end if ! stm_included
      end if ! ised

      if (jasecflow > 0 .and. jaequili == 0 .and. kmx == 0) then
         const_names(ispir) = 'secondary_flow_intensity'
      end if

      if (itra1 > 0) then
         do i = itra1, itran
            itrace = i - itra1 + 1

!        set name
            if (trim(trnames(itrace)) /= '') then
               const_names(i) = trim(trnames(itrace))
               const_units(i) = trim(trunits(itrace))
            else
               write (str, "(I0)") itrace
               const_names(i) = 'tracer_'//trim(str)
            end if

            itrac2const(itrace) = i

         end do
      end if

      if (numconst > 0) then
         call mess(LEVEL_INFO, 'List of constituents defined in the model')
         do i = 1, numconst
            write (msg, '(I8,1X,A)') i, const_names(i)
            call mess(LEVEL_INFO, msg)
         end do
      end if

      if (numwqbots > 0) then
         call mess(LEVEL_INFO, 'List of water quality bottom variables defined in the model')
         do i = 1, numwqbots
            write (msg, '(I8,1X,A)') i, wqbotnames(i)
            call mess(LEVEL_INFO, msg)
         end do
      end if

      if (jawaqproc > 0) then
!     fill administration for WAQ substances with fall velocities, and thetavert
         do isys = 1, num_substances_transported
            i = itrac2const(isys2trac(isys))
            isys2const(isys) = i
            iconst2sys(i) = isys

            thetavert(i) = md_thetav_waq
         end do
      end if

!   iconst_cur = min(numconst,itra1)
      iconst_cur = min(numconst, 1)

!  local timestepping
      time_dtmax = -1.0_dp ! cfl-numbers not evaluated
      nsubsteps = 1
      ndeltasteps = 1
      jaupdatehorflux = 1
      numnonglobal = 0

!  sediment advection velocity
      jaupdateconst = 1

      if (stm_included) then
         if (stmpar%lsedsus > 0) then
            noupdateconst = 0
            do i = ised1, isedn
               jaupdateconst(i) = 0
               noupdateconst(i) = 1
            end do
         end if
      end if

      return
   end subroutine ini_transport

end module m_ini_transport
