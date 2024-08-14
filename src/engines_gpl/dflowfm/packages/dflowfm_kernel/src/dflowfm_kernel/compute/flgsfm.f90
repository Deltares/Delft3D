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

subroutine flgsfm(n, ng, L, firstiter, jarea)
   use m_flowgeom
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
   ! use cpluv
   ! use m_strucs
   ! use ident

   use m_strucs
   use m_flow

   implicit none
!
! Global variables
!
   integer, intent(in) :: n !< general structure point n
   integer, intent(in) :: ng !< is a member of general structure sigal ng
   integer, intent(in) :: L !< Flow link number, signed! If L < 0 then flow link is in opposite direction than structure left-right orientation.
   logical, intent(in) :: firstiter
   logical :: jarea

!
!
! Local variables
!
   integer :: il, ir, k1, k2, kL, kR, m, Lf
   integer :: L0, Lb, Lt, LL, kk, iup
   logical :: velheight
   double precision :: cgd, cgf
   double precision :: crest
   double precision :: cwd, cwf
   double precision :: dg, ds, ds1, ds2
   double precision :: hdsb, husb
   double precision :: lambda, mugf
   double precision :: relax
   double precision :: rholeft, rhoright
   double precision :: strdamf
   double precision :: teken, tekenstr
   double precision :: ud, uu
   double precision :: w2, wsd, wstr
   double precision :: zb2, zs
   double precision :: gateloweredgelevel, gatedoorheight
   double precision :: DsL, au0, au1, au2, au3
   double precision :: gatefraction
   double precision :: hhi(3), zbi(3), zti(3)

!
!! executable statements -------------------------------------------------------
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
   ! Module:             FLGS (FLow General Structure)
   !
   ! Module description: In subroutine FLGS the QH-relationship for a
   !                     general structure will be transformed to a
   !                     linearized equation
   !
   !
   ! Parameters:
   ! NR NAME              IO DESCRIPTION
   !  2 il                I  Grid point on left side of structure (lower
   !                         index).
   !  3 ir                I  Grid point on right side of structure (upper
   !                         index).
   !  8 jarea             I  If True then claculate only area
   !  1 m                 I  Grid index of structure
   !  4 istru             I  Number of structure.
   !  7 firstiter         I  True in case of first iteration step.

   ! Subprogram calls:
   ! NAME    DESCRIPTION
   ! flgtar  FLow get General sTructure ARguments
   ! flupdg  FLow UP/Downstream near General structure
   ! errmsg  generate ERRer MeSsaGe to log file
   ! flqhgs  FLow QH relation for General Structure
   !=======================================================================
   !     Include Pluvius data space
   !     Include identifiers of objects
   !
   !     Declaration of parameters:
   !
   !
   !     Declaration of local variables:
   !
   !
   !
   !TEM  WRITE (11,*) 'Call structure',istru,'(',m,il,ir,istru,')'

   Lf = abs(L)
   ! NOTE: Since a single general structure may be crossed by multiple flow links,
   ! pay attention to proper directions: structure parameters are typically determined
   ! by the structure's left-right direction, whereas upwinding and furu-computations
   ! are typically in the flow link's 1-2 direction.
   k1 = ln(1, Lf); k2 = ln(2, Lf) ! 1 -> 2 flow link direction
   kL = kcgen(1, n); kR = kcgen(2, n) ! L -> R structure direction

   m = L
   il = k1
   ir = k2
   L0 = n - L1cgensg(ng) + 1

   zs = min(bob(1, Lf), bob(2, Lf)) ! == zcgen(3*ng - 2) crest/silllevel
   zbi(1) = zs
   gateloweredgelevel = generalstruc(ng)%gateheightonlink(L0) ! == zcgen(3*ng - 1) under gate door and infinity in open part.
   gatefraction = generalstruc(ng)%gateclosedfractiononlink(L0)
   DsL = s1(k2) - s1(k1)

   ! TODO: RTC: AvD/Herman: hier ook wu'tjes en zb1-tjes etc gaan zetten, voordat we de flupd/flgtar-subroutines gaan callen?
   ! Velheight is always true for river structures
   ! velheight = istrtyp(7, istru)==1
   velheight = .true.

   relax = 1.0d0
   au(Lf) = 0d0; fu(Lf) = 0d0; ru(Lf) = 0d0
   !
   ! ng instead of istru

   dg = gateloweredgelevel - zs

   call flupdofm(m, il, ir, ng, velheight, rholeft, rhoright, crest, husb, hdsb, &
                 uu, ud, teken, relax)

   gatedoorheight = 0d0

   tekenstr = teken * sign(1, L) ! if flow link abs(L) is in opposite orientation to the structure's orientation, then negate the just computed upwind (flow) teken.

   if (kmx > 0) then
      call getLbotLtop(Lf, Lb, Lt)
      ff3(:, :) = 0d0
   end if

   if (husb > zs) then ! in all three tests of this type we do not have risc of immediate drying after opening, (sills usually above bed)
      zbi(1) = zs ! so we do not have the regular husb-zs>epshu, that would make a structure first overflowing too epshu dependent
      call flgtarfm(ng, L0, wu(Lf), bl(kL), bl(kR), tekenstr, zs, wstr, w2, wsd, zb2, dg, ds1, ds2, cgf, cgd, &
                    cwf, cwd, mugf, lambda, strdamf, gatedoorheight)
      u1(Lf) = rusav(1, n) - fusav(1, n) * DsL; u0(Lf) = u1(Lf); q1(Lf) = ausav(1, n) * u1(Lf)
      call flqhgsfm(Lf, teken, husb, hdsb, uu, zs, wstr, w2, wsd, zb2, ds1, ds2, dg, &
                    cgf, cgd, cwf, cwd, mugf, lambda, strdamf, jarea, ds)
      fusav(1, n) = fu(Lf); rusav(1, n) = ru(Lf); ausav(1, n) = au(Lf) * gatefraction
   else
      fusav(1, n) = 0d0
      rusav(1, n) = 0d0
      ausav(1, n) = 0d0
   end if

   if (gatedoorheight > 0d0) then ! now add water overflowing top of gate
      zs = gateloweredgelevel + gatedoorheight
      zbi(2) = zs
      if (husb > zs) then ! husb = upwind waterlevel instead of height
         dg = 1d9 ! sky is the limit, this gate fully open
         u1(Lf) = rusav(2, n) - fusav(2, n) * dsL; u0(Lf) = u1(Lf); q1(Lf) = ausav(2, n) * u1(Lf)
         call flgtarfm(ng, L0, wu(Lf), bl(kL), bl(kR), tekenstr, zs, wstr, w2, wsd, zb2, dg, ds1, ds2, cgf, cgd, &
                       cwf, cwd, mugf, lambda, strdamf, gatedoorheight)
         call flqhgsfm(Lf, teken, husb, hdsb, uu, zs, wstr, w2, wsd, zb2, ds1, ds2, dg, &
                       cgf, cgd, cwf, cwd, mugf, lambda, strdamf, jarea, ds)
         fusav(2, n) = fu(Lf); rusav(2, n) = ru(Lf); ausav(2, n) = au(Lf) * gatefraction

      else
         fusav(2, n) = 0d0
         rusav(2, n) = 0d0
         ausav(2, n) = 0d0
      end if
   else
      fusav(2, n) = 0d0
      rusav(2, n) = 0d0
      ausav(2, n) = 0d0
   end if

   zs = min(bob(1, Lf), bob(2, Lf)) ! == zcgen(3*ng - 2) crest/silllevel
   if (husb > zs .and. (1d0 - gatefraction) > 1d-9) then ! and add flow around the tip of the floating gate (e.g. for SVKW)
      zbi(3) = zs ! 1d-9 prevents unneccesary evaluation
      dg = huge(1d0)
      u1(Lf) = rusav(3, n) - fusav(3, n) * dsL; u0(Lf) = u1(Lf); q1(Lf) = ausav(3, n) * u1(Lf)
      call flgtarfm(ng, L0, wu(Lf), bl(kL), bl(kR), tekenstr, zs, wstr, w2, wsd, zb2, dg, ds1, ds2, cgf, cgd, &
                    cwf, cwd, mugf, lambda, strdamf, gatedoorheight)
      call flqhgsfm(Lf, teken, husb, hdsb, uu, zs, wstr, wstr, wstr, zb2, ds1, ds2, dg, & ! no width variation here, 3 times wstr,
                    cgf, cgd, cwf, cwd, mugf, lambda, strdamf, jarea, ds) ! easy to see in the call
      fusav(3, n) = fu(Lf); rusav(3, n) = ru(Lf); ausav(3, n) = au(Lf) * (1d0 - gatefraction)
   else
      fusav(3, n) = 0d0
      rusav(3, n) = 0d0
      ausav(3, n) = 0d0
   end if

   au(Lf) = ausav(1, n) + ausav(2, n) + ausav(3, n)

   if (au(Lf) > 0d0) then
      fu(Lf) = (fusav(1, n) * ausav(1, n) + fusav(2, n) * ausav(2, n) + fusav(3, n) * ausav(3, n)) / au(Lf)
      ru(Lf) = (rusav(1, n) * ausav(1, n) + rusav(2, n) * ausav(2, n) + rusav(3, n) * ausav(3, n)) / au(Lf)
      if (kmx > 0) then
         if (jastructurelayersactive == 1) then ! some layers are more equal than others

            if (ausav(1, n) > 0) then
               hhi(1) = ausav(1, n) / (gatefraction * wstr)
               zti(1) = zbi(1) + hhi(1)
            end if
            if (ausav(2, n) > 0) then
               hhi(2) = ausav(2, n) / (gatefraction * wstr)
               zti(2) = zbi(2) + hhi(2)
            end if
            if (ausav(3, n) > 0) then
               hhi(3) = ausav(3, n) / ((1d0 - gatefraction) * wstr)
               zti(3) = zbi(3) + hhi(3)
            end if

            if (u1(Lf) > 0) then
               iup = 1
            else if (u1(Lf) < 0) then
               iup = 2
            else if (s1(k1) > s1(k2)) then
               iup = 1
            else
               iup = 2
            end if
            ff3(:, 0) = 0d0
            do LL = Lb, Lt
               kk = ln(iup, LL)
               if (ausav(1, n) > 0) ff3(1, LL - Lb + 1) = max(0d0, min(zti(1), zws(kk)) - zbi(1)) / hhi(1)
               if (ausav(2, n) > 0) ff3(2, LL - Lb + 1) = max(0d0, min(zti(2), zws(kk)) - zbi(2)) / hhi(2)
               if (ausav(3, n) > 0) ff3(3, LL - Lb + 1) = max(0d0, min(zti(3), zws(kk)) - zbi(3)) / hhi(3)
            end do

            au0 = 0d0
            do LL = Lb, Lt
               au1 = ausav(1, n) * (ff3(1, LL - Lb + 1) - ff3(1, LL - Lb))
               au2 = ausav(2, n) * (ff3(2, LL - Lb + 1) - ff3(2, LL - Lb))
               au3 = ausav(3, n) * (ff3(3, LL - Lb + 1) - ff3(3, LL - Lb))
               au(LL) = au1 + au2 + au3
               if (au(LL) > 0) then
                  fu(LL) = (fusav(1, n) * au1 + fusav(2, n) * au2 + fusav(3, n) * au3) / au(LL)
                  ru(LL) = (rusav(1, n) * au1 + rusav(2, n) * au2 + rusav(3, n) * au3) / au(LL)
               else
                  fu(LL) = 0d0
                  ru(LL) = 0d0
               end if
            end do
         else ! default: all layers are equal
            do LL = Lb, Lt
               fu(LL) = fu(Lf); ru(LL) = ru(Lf)
               au(LL) = au(Lf) * (hu(LL) - hu(LL - 1)) / (hu(Lt) - hu(Lb - 1))
            end do
         end if

      end if
   else
      fu(Lf) = 0d0
      ru(Lf) = 0d0
   end if

   if (au(Lf) == 0d0) then
      hu(Lf) = 0d0
      if (kmx > 0) then
         au(Lb:Lt) = 0d0
      end if
   end if

   ! TEMP = laatste statement
   ! strhis(15, istru) = ds + crest     ! waterlevel on crest
end subroutine flgsfm
