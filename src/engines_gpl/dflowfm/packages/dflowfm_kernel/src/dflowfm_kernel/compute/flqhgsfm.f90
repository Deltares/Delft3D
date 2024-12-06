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

module m_flqhgsfm

   implicit none

contains

   subroutine flqhgsfm(m, teken, husb, hdsb, uu, zs, wstr, w2, wsd, zb2, ds1, ds2, &
                       dg, cgf, cgd, cwf, cwd, mugf, lambda, strdamf, jarea, ds)
      use precision, only: dp
      use m_flgsfurufm
      use m_flgsd3fm
      use m_flgsd2fm
      use m_flgsareafm
      use m_flccgsfm
      use m_physcoef, only: ag
      use m_dpsequfm

      implicit none
!
! Global variables
!
      integer :: m
      logical, intent(in) :: jarea
      real(kind=dp) :: cgd
      real(kind=dp) :: cgf
      real(kind=dp) :: cwd
      real(kind=dp), intent(in) :: cwf
      real(kind=dp) :: dg
      real(kind=dp) :: ds
      real(kind=dp) :: ds1
      real(kind=dp) :: ds2
      real(kind=dp) :: hdsb
      real(kind=dp) :: husb
      real(kind=dp) :: lambda
      real(kind=dp) :: mugf
      real(kind=dp) :: rhoast = 1d0
      real(kind=dp) :: strdamf
      real(kind=dp) :: teken
      real(kind=dp), intent(in) :: uu
      real(kind=dp) :: w2
      real(kind=dp) :: wsd
      real(kind=dp) :: wstr
      real(kind=dp) :: zb2
      real(kind=dp) :: zs
!
!
! Local variables
!
      integer :: formno
      logical :: imag
      real(kind=dp) :: cgd2
      real(kind=dp) :: cgda
      real(kind=dp) :: cgfa
      real(kind=dp) :: cwfa
      real(kind=dp) :: dc
      real(kind=dp) :: dlim
      real(kind=dp) :: elu
      real(kind=dp) :: hd1
      real(kind=dp) :: hs1
      real(kind=dp) :: mugfa
      real(kind=dp) :: velhght, tr
!
!
!! executable statements -------------------------------------------------------
!
      !
      !
      !=======================================================================
      !                      Deltares
      !                One-Two Dimensional Modelling System
      !                           S O B E K
      !
      ! Subsystem:          Flow Module
      !
      ! Programmer:         J.Kuipers
      !
      ! Module:             FLQHGS (FLow QH relation for General Structure)
      !
      ! Module description: The QH-relationship for a general structure
      !                     will be transformed to a linearized equation
      !
      !                     In this subroutine for given upstream and down-
      !                     stream water levels and upstream velocity
      !                     the flow condition will be determined.
      !
      !
      ! Parameters:
      ! NR NAME              IO DESCRIPTION
      ! 16 cgd               I  Contraction coefficient for drowned gate flow
      ! 15 cgf               I  Contraction coefficient for gate flow
      ! 18 cwd               I  Contraction coefficient for drowned weir flow.
      ! 17 cwf               I  Contraction coefficient for free weir flow.
      ! 13 dg                I  Gate opening height.
      ! 22 ds                O  Water level immediately downstream the gate.
      ! 11 ds1               I  Delta s1 general structure.
      ! 12 ds2               I  Delta s2 general structure.
      !  4 hdsb              I  Downstream water level.
      !  3 husb              I  Upstream water level.
      ! 21 jarea             I  If True then claculate only area
      !  1 m                 I  Grid index of structure
      ! 20 lambda            I  Extra resistance
      ! 19 mugf              I  Vertical contraction coefficient for free
      !                         gate flow.
      ! 14 rhoast            I  Ratio of density right and left of structure
      !  2 teken             I  Flow direction (+1/-1).
      !  5 uu                I  Upstream velocity.
      !  8 w2                I  Width at right side of structure.
      !  9 wsd               I  Width structure right or left side.
      !  7 wstr              I  Width at centre of structure.
      ! 10 zb2               I  Bed level at right side of structure.
      !  6 zs                I  Bed level at centre of structure.
      !
      ! Subprogram calls:
      ! NAME     DESCRIPTION
      ! flccgs   FLow contraction coefficients for general structure
      ! flgsd2   FLow general structure depth sill 2nd order equation
      ! flgsd3   FLow general structure depth sill 3rd order equation
      ! flgsfuru FLow general structure calculate FU and RU
      !=======================================================================
      !     Include Pluvius data space
      !     Declaration of parameters:
      !     Declaration of local variables:
      !     Function declaration:
      !
      !     Compute upstream velocity height and energy level
      !

      velhght = uu * uu / (2.0d0 * ag)
      elu = husb + velhght
      hs1 = elu - zs
      !
      tr = 1d-4
      if (hs1 < tr .or. wstr < tr .or. dg < tr .or. min(cgf, cgd, cwf, cwd) <= 0.) then !  & dg<.0001) then !hk: or gate closed

         formno = 0; return

      else
         !
         !        Compute critical water depth at the
         !        sill, dc and water depth at the sill,ds
         !
         dlim = hs1 * (wstr / w2 * 2./3.*sqrt(2./3.))**(2.0 / 3.0)
         hd1 = max(hdsb, zb2 + dlim * 0.9d0)
         !
         dc = 2.0d0 / 3.0d0 * hs1
         !
         !        Calculate ds by solving third order algebraic equation
         !

         call flgsd3fm(wsd, wstr, zs, w2, zb2, ds1, ds2, elu, hd1, rhoast, cwd, ds, &
                   & lambda)

         !
         if (ds >= dc) then ! waterheight on crest larger than critical height on crest
            if (dg >= ds) then
               !
               !              - drowned weir -
               !
               formno = 2
            else
               !
               !              - gate flow -
               !
               formno = 3
               !
               !              adapt coefficients on basis of Ds & Cwd
               !
               call flccgsfm(dg, ds, cgd, cgf, cwd, mugf, cgda, cgfa, mugfa)
            end if
         else
            !
            !           Adapt Cwf coefficient
            !
            if (cwf < cwd) then
               if (dpsequfm(dc, 0.0d0, 1.0d-20)) then
                  cwfa = cwf
               else
                  cwfa = max(ds / dc * cwd, cwf)
               end if
            elseif (ds > 0.0d0) then
               cwfa = min(dc / ds * cwd, cwf)
            else
               cwfa = cwf
            end if
            !
            if (dg >= dc) then
               !
               !              - free weir -
               !
               formno = 1
               ds = dc
            else
               !
               !              - gate flow -
               !
               formno = 3
               !
               !              adapt coefficients on basis of Dc & Cwf
               !
               call flccgsfm(dg, dc, cgd, cgf, cwfa, mugf, cgda, cgfa, mugfa)
            end if
         end if
         !
         !        In case of gate flow determine type of gate flow
         !        (drowned or free)
         !
         if (formno == 3) then
            dc = mugfa * dg
            !
            !      Cgd for second order equation = Cgd' * Mu'
            !
            cgd2 = cgda * mugfa
            !
            call flgsd2fm(wsd, wstr, zs, w2, zb2, dg, ds1, ds2, elu, hd1, rhoast,   &
                      & cgd2, imag, ds, lambda)
            !
            if (imag) then
               !
               !              - free gate -
               !
               formno = 3
               ds = dc
            elseif (ds <= dc) then
               !
               !              - free gate -
               !
               formno = 3
               !
               !             Adapt coefficients
               !
               if (cgda > cgfa) then
                  if (.not. dpsequfm(dc, 0.0d0, 1.0d-20)) then
                     cgfa = max(ds / dc * cgda, cgfa)
                  end if
               elseif (ds > 0.0d0) then
                  cgfa = min(dc / ds * cgda, cgfa)
               else
               end if
               ds = dc
               !TEM          WRITE (11,*) 'cgfa,mugfa',cgfa,mugfa
            else
               !
               !             - drowned gate -
               !
               formno = 4
            end if
         end if
         !
         !
      end if
      !
      !TEM    WRITE (11,*) 'formno,ds,dc,dg',formno,ds,dc,dg
      !
      !       The flowe condition is known so calculate
      !       the linearization coefficients FU and RU
      !
      if (jarea) then
         call flgsareafm(formno, m, husb, velhght, zs, ds, dg, wstr)
      else
         call flgsfurufm(formno, m, teken, husb, hdsb, velhght, zs, ds, dg, dc, wstr, &
                         cwfa, cwd, mugfa, cgfa, cgda, strdamf, lambda)
      end if
   end subroutine flqhgsfm

end module m_flqhgsfm
