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

subroutine flgtarfm(ng, L0, wuL, bl1, bl2, teken, zs, wstr, w2, wsd, zb2, dg, ds1, ds2, cgf, & ! fromgeneral
                    cgd, cwf, cwd, mugf, lambda, strdamf, gatedoorheight)
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
   ! use cpluv
   use m_strucs
   use m_missing
   implicit none
!
! Global variables
!
   integer :: ng
   integer, intent(in) :: L0 !< counter for the current flow link under genstru #ng (1:ncgen for each separate genstru)
   double precision, intent(in) :: wuL !< wu of this flow link.
   double precision, intent(in) :: bl1 !< bl of nod1
   double precision, intent(in) :: bl2 !< bl of nod2
   double precision, intent(out) :: cgd
   double precision, intent(out) :: cgf
   double precision, intent(out) :: cwd
   double precision, intent(out) :: cwf
   double precision :: dg
   double precision, intent(out) :: ds1
   double precision, intent(out) :: ds2
   double precision :: lambda
   double precision, intent(out) :: mugf
   double precision :: strdamf
   double precision, intent(in) :: teken !< Flow direction, w.r.t. the structure's orientation. So: based on both upwind *and* flow-link<-->str-pli crossing.
   double precision :: w2
   double precision, intent(out) :: wsd
   double precision, intent(out) :: wstr
   double precision, intent(out) :: gatedoorheight
   double precision :: zb2
   double precision :: zs
!
!
! Local variables
!
   double precision :: help
   double precision :: w1
   double precision :: wsdl
   double precision :: wsdr
   double precision :: zb1
   double precision :: zbsl
   double precision :: zbsr
!
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
   ! Programmer:         J.Brouwer
   !
   ! Module:             FLGTAR (FLow get General sTructure ARguments)
   !
   ! Module description: Parameters for the general structure are extracted
   !                     from the structures module.
   !
   !
   ! Parameters:
   ! NR NAME              IO DESCRIPTION
   ! 13 cgd               O  Correction coefficient for drowned gate flow.
   ! 12 cgf               O  Correction coefficient for free gate flow.
   ! 15 cwd               O  Correction coefficient for drowned weir flow.
   ! 14 cwf               O  Correction coefficient for free weir flow.
   !  9 dg                O  Gate opening height.
   ! 10 ds1               O  Delta s1 general structure.
   ! 11 ds2               O  Delta s2 general structure.
   !  1 istru             I  Number of structure.
   ! 17 lambda            O  Extra resistance
   ! 16 mugf              O  Contraction coefficient for free gate flow.
   !  3 tekenstr          I  Flow direction, w.r.t. structure orientation (+/-).
   !  6 w2                O  Width at right side of structure.
   !  7 wsd               O  Width structure right or left side.
   !  5 wstr              O  Width at centre of structure.
   !  8 zb2               O  Bed level at right side of structure.
   !  4 zs                O  Bed level at centre of structure.
   !=======================================================================
   !     Include Pluvius data space
   !
   !     Declaration of parameters:
   !
   !
   !     Declaration of local variables:
   !
   !
   !     Fetch parameters from structure info array
   !

   if (generalstruc(ng)%numlinks <= 1) then ! Structure crosses just one link, use user-specified widths ! TODO: AvD: can this be merged with else block, also incase of timeseries and RTC (why only do that for numlinks>1 ?)
      w1 = min(wuL, generalstruc(ng)%widthleftW1)
      wsdl = min(wuL, generalstruc(ng)%widthleftWsdl)
!    wstr = generalstruc(ng)%widthcenter
      wstr = min(wuL, generalstruc(ng)%widthcenteronlink(L0)) ! Possible realtime-controlled, even when crossing one link, so use widthcenteronlink here already.
      wsdr = min(wuL, generalstruc(ng)%widthrightWsdr)
      w2 = min(wuL, generalstruc(ng)%widthrightW2)
   else ! Structure crosses more than one link: nonsensible to use single width left/right etc. same for all links. Use center linkwidth instead (i.e., typically wu(Lf))
      ! TODO: UNST-695: Support sideways movement/closing of a gate.
      w1 = min(wuL, generalstruc(ng)%widthcenteronlink(L0)) !widthleftW1
      wsdl = min(wuL, generalstruc(ng)%widthcenteronlink(L0)) !widthleftWsdl
      ! wstr = generalstruc(ng)%widthcenter
      wstr = min(wuL, generalstruc(ng)%widthcenteronlink(L0))
      wsdr = min(wuL, generalstruc(ng)%widthcenteronlink(L0)) !widthrightWsdr
      w2 = min(wuL, generalstruc(ng)%widthcenteronlink(L0)) !widthrightW2
   end if

   ! zs   = generalstruc(ng)%levelcenter        ! comes from ec
   zb1 = max(bl1, generalstruc(ng)%levelleftZb1)
   zbsl = max(bl1, generalstruc(ng)%levelleftZbsl)
   zbsr = max(bl2, generalstruc(ng)%levelrightZbsr)
   zb2 = max(bl2, generalstruc(ng)%levelrightZb2)
   ! dg   = generalstruc(ng)%gateheight - zs    ! also comes from ec
   lambda = generalstruc(ng)%extraresistance
   strdamf = generalstruc(ng)%dynstructext
   gatedoorheight = generalstruc(ng)%gatedoorheight

   !if (strdamf< - 0.5D0) strdamf = dynstructext
   !if (lambda < - 0.5D0) lambda = extra_resist_genstruc

   !
   !     Determine cgf, cgd, cwf, cwd, mugf
   !     (flow direction dependent)
   !
   if (teken > 0.0d0) then
      cgf = generalstruc(ng)%pos_freegateflowcoeff
      cgd = generalstruc(ng)%pos_drowngateflowcoeff
      cwf = generalstruc(ng)%pos_freeweirflowcoeff
      cwd = generalstruc(ng)%pos_drownweirflowcoeff
      mugf = generalstruc(ng)%pos_contrcoeffreegate
   else
      cgf = generalstruc(ng)%neg_freegateflowcoeff
      cgd = generalstruc(ng)%neg_drowngateflowcoeff
      cwf = generalstruc(ng)%neg_freeweirflowcoeff
      cwd = generalstruc(ng)%neg_drownweirflowcoeff
      mugf = generalstruc(ng)%neg_contrcoeffreegate
   end if
   !
   !     Determine flow direction dependent parameters
   !
   if (teken > 0.0d0) then
      wsd = wsdr
      ds1 = zs - zbsr
      ds2 = zbsr - zb2
   else
      wsd = wsdl
      ds1 = zs - zbsl
      ds2 = zbsl - zb1
      help = w1
      w1 = w2
      w2 = help
      help = zb1
      zb1 = zb2
      zb2 = help
   end if
end subroutine flgtarfm
