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

   !>  pointer data
module m_fm_erosed
   use precision
   use bedcomposition_module
   use morphology_data_module
   use sediment_basics_module
   use m_sediment, only: stmpar, sedtra, stm_included, mtd
   use m_ini_noderel
   use m_flowgeom, only: tnode

   implicit none

   integer :: ndx_mor !< copy of <ndx> for morphodynamics
   integer :: ndxi_mor !< copy of <ndxi> for morphodynamics
   integer :: lnx_mor !< copy of <lnx> for morphodynamics
   integer :: lnxi_mor !< copy of <lnxi> for morphodynamics
   integer :: ndkx_mor !< copy of <ndkx> for morpho. <ndkx> cannot be changed for preallocating because it is used for writing.
   integer, dimension(:, :), allocatable :: ln_mor !< copy of <ln> for morphodynamics. Kept allocatable rather than pointer for consistency with the original

   type(tnode), allocatable :: nd_mor(:) !< copy of <nd> for morphodynamics

   integer, dimension(:), pointer :: link1 => null()
   integer, dimension(:), pointer :: link1sign => null()
   integer, dimension(:), pointer :: link1sign2 => null() !sign of the link for FM1DIMP. It should converge with <link1sign>, but I am not sure of the use of <link1sign>.
   !<link1sign2> is 1 when a positive velocity is in the direction of increasing chainage and -1 when it has the opposite direction.
   logical :: link1_initialized = .false.

   real(fp), dimension(:, :), pointer :: seddif !< Sediment diffusion
   real(fp), dimension(:, :), pointer :: sed !< Sediment concentration work array
   real(fp), dimension(:), pointer :: blchg !< Bed level change (> 0 = sedimentation, < 0 = erosion)
   real(fp), dimension(:), pointer :: dzbdt !< Bed level change time rate
   real(fp), dimension(:), pointer :: uau !< velocity asymmetry
   real(fp), dimension(:, :), pointer :: ws !< settling velocity

   real(fp), dimension(:), pointer :: ucxq_mor
   real(fp), dimension(:), pointer :: ucyq_mor
   real(fp), dimension(:), pointer :: hs_mor

   real(fp), dimension(:, :), pointer :: q_zeta

   !     stmpar
   integer, pointer :: lsed
   integer, pointer :: lsedtot

   !     sedpar
   integer, pointer :: nmudfrac
   logical, dimension(:), pointer :: cmpupdfrac
   real(fp), dimension(:), pointer :: rhosol
   real(fp), dimension(:), pointer :: cdryb
   real(fp), dimension(:, :, :), pointer :: logseddia
   real(fp), dimension(:), pointer :: logsedsig
   real(fp), dimension(:), pointer :: sedd10
   real(fp), dimension(:), pointer :: sedd50
   real(fp), dimension(:), pointer :: sedd90
   real(fp), dimension(:), pointer :: sedd50fld
   real(fp), dimension(:), pointer :: dstar
   real(fp), dimension(:), pointer :: taucr
   real(fp), dimension(:), pointer :: tetacr
   real(fp), dimension(:), pointer :: mudcnt
   real(fp), dimension(:), pointer :: pmcrit
   integer, dimension(:), pointer :: nseddia
   integer, dimension(:), pointer :: sedtyp
   integer, dimension(:), pointer :: tratyp
   logical, pointer :: anymud
   real(fp), dimension(:), pointer :: sedtrcfac
   logical, pointer :: bsskin
   real(fp), dimension(:), pointer :: thcmud
   real(fp), dimension(:), pointer :: tpsnumber
   real(fp), dimension(:, :), pointer :: dss

   integer, pointer :: max_mud_sedtyp
   integer, pointer :: min_dxx_sedtyp
   integer, pointer :: flocmod
   integer, pointer :: nflocpop
   integer, pointer :: nflocsizes
   integer, dimension(:, :), pointer :: floclist
   real(fp), pointer :: tbreakup
   real(fp), pointer :: tfloc

   ! morpar
   real(fp), pointer :: thresh
   real(fp), pointer :: sus
   real(fp), pointer :: suscorfac
   real(fp), pointer :: bed
   real(fp), pointer :: susw
   real(fp), pointer :: sedthr
   real(fp), pointer :: bedw
   integer, pointer :: i10
   integer, pointer :: i15
   integer, pointer :: i50
   integer, pointer :: i90
   integer, pointer :: nxx
   real(fp), dimension(:), pointer :: xx
   logical, pointer :: multi
   real(fp), pointer :: factcr
   real(fp), pointer :: factsd
   integer, pointer :: ihidexp
   real(fp), pointer :: asklhe
   real(fp), pointer :: mwwjhe
   real(fp), pointer :: ffthresh
   real(fp), pointer :: morfac
   logical, pointer :: varyingmorfac
   real(hp), pointer :: morft
   real(hp), pointer :: hydrt
   real(fp), pointer :: espir
   logical, pointer :: epspar
   real(fp), pointer :: camax
   real(fp), pointer :: aksfac
   real(fp), pointer :: rdc
   integer, pointer :: iopkcw
   logical, pointer :: oldmudfrac
   real(fp), dimension(:, :), pointer :: sinkf
   real(fp), dimension(:, :), pointer :: sourf
   integer, pointer :: iflufflyr
   real(fp), dimension(:, :), pointer :: depfac
   real(fp), dimension(:, :), pointer :: mfluff
   logical, pointer :: bedupd
   real(fp), pointer :: tmor
   real(fp), pointer :: tcmp
   integer, pointer :: itmor
   integer, pointer :: islope
   real(fp), pointer :: dzmax
   real(fp), pointer :: hmaxth
   real(fp), dimension(:), pointer :: thetsd
   logical, pointer :: eqmbcsand
   logical, pointer :: eqmbcmud
   logical, pointer :: eulerisoglm
   logical, pointer :: l_suscor
   logical, pointer :: bermslopetransport !< switch on bermslope transport or not
   logical, pointer :: bermslopebed !< include bermslope nudging on bedload
   logical, pointer :: bermslopesus !< include bermslope nudging on suspended load
   real(fp), pointer :: bermslope !< slope to be nudged toward
   real(fp), pointer :: bermslopefac !< calibration factor
   real(fp), pointer :: bermslopegamma !< bermslope nudging applied for region where Hrms/h>bermslopegamma
   real(fp), pointer :: bermslopedepth !< minimum depth to apply nudging
   real(fp), pointer :: ti_sedtrans !< sediment transport computation start time in TUnits

   ! trapar
   integer, dimension(:), pointer :: iform
   real(fp), dimension(:, :), pointer :: par
   integer, pointer :: npar
   integer, pointer :: max_integers
   integer, pointer :: max_reals
   integer, pointer :: max_strings
   character(256), dimension(:), pointer :: dll_function
   integer(pntrsize), dimension(:), pointer :: dll_handle
   integer, dimension(:), pointer :: dll_integers
   real(hp), dimension(:), pointer :: dll_reals
   character(256), dimension(:), pointer :: dll_strings
   character(256), dimension(:), pointer :: dll_usrfil

   ! sedtra
   real(fp), dimension(:, :), pointer :: aks
   real(fp), dimension(:), pointer :: bc_mor_array
   real(fp), dimension(:, :), pointer :: dbodsd
   real(fp), dimension(:), pointer :: dcwwlc
   real(fp), dimension(:), pointer :: dm
   real(fp), dimension(:), pointer :: dg
   real(fp), dimension(:), pointer :: dgsd
   real(fp), dimension(:, :), pointer :: dxx
   real(fp), dimension(:), pointer :: e_dzdn
   real(fp), dimension(:), pointer :: e_dzdt
   real(fp), dimension(:), pointer :: epsclc
   real(fp), dimension(:), pointer :: epswlc
   real(fp), dimension(:, :), pointer :: fixfac
   real(fp), dimension(:, :), pointer :: frac
   integer, dimension(:), pointer :: kfsed
   integer, dimension(:, :), pointer :: kmxsed
   real(fp), dimension(:), pointer :: mudfrac
   real(fp), dimension(:), pointer :: sandfrac
   real(fp), dimension(:, :), pointer :: hidexp
   real(fp), dimension(:), pointer :: rsdqlc
   real(fp), dimension(:, :), pointer :: sbcx
   real(fp), dimension(:, :), pointer :: sbcy
   real(fp), dimension(:, :), pointer :: e_sbcn
   real(fp), dimension(:, :), pointer :: e_sbct
   real(fp), dimension(:, :), pointer :: sbwx
   real(fp), dimension(:, :), pointer :: sbwy
   real(fp), dimension(:, :), pointer :: e_sbwn
   real(fp), dimension(:, :), pointer :: e_sbwt
   real(fp), dimension(:, :), pointer :: e_sbt
   real(fp), dimension(:, :), pointer :: e_sbn
   real(fp), dimension(:, :), pointer :: e_ssn
   real(fp), dimension(:, :), pointer :: e_sst
   real(fp), dimension(:, :), pointer :: e_sbtc
   real(fp), dimension(:, :), pointer :: e_sbnc
   real(fp), dimension(:, :), pointer :: e_ssnc
   real(fp), dimension(:, :), pointer :: e_scrn
   real(fp), dimension(:, :), pointer :: e_scrt
   real(fp), dimension(:), pointer :: sddflc
   real(fp), dimension(:, :), pointer :: sswx
   real(fp), dimension(:, :), pointer :: sswy
   real(fp), dimension(:, :), pointer :: sscx
   real(fp), dimension(:, :), pointer :: sscy
   real(fp), dimension(:, :), pointer :: e_sswn
   real(fp), dimension(:, :), pointer :: e_sswt
   real(fp), dimension(:, :), pointer :: sxtot
   real(fp), dimension(:, :), pointer :: sytot
   real(fp), dimension(:, :), pointer :: sbxcum
   real(fp), dimension(:, :), pointer :: sbycum
   real(fp), dimension(:, :), pointer :: ssxcum
   real(fp), dimension(:, :), pointer :: ssycum
   real(fp), dimension(:, :), pointer :: sinkse
   real(fp), dimension(:, :), pointer :: sourse
   real(fp), dimension(:, :), pointer :: sour_im
   real(fp), dimension(:, :), pointer :: srcmax
   real(fp), dimension(:), pointer :: taub
   real(fp), dimension(:, :), pointer :: taurat
   real(fp), dimension(:), pointer :: ust2
   real(fp), dimension(:), pointer :: umod
   real(fp), dimension(:), pointer :: uuu
   real(fp), dimension(:), pointer :: vvv
   real(fp), dimension(:), pointer :: wslc
   real(fp), dimension(:), pointer :: zumod
   real(fp), dimension(:, :), pointer :: rsedeq
   real(fp), pointer :: alfabs
   real(fp), pointer :: alfabn
   real(fp), pointer :: wetslope
   real(fp), pointer :: avaltime
   real(fp), pointer :: dryslope
   logical, pointer :: duneavalan
   real(fp), pointer :: hswitch
   real(fp), pointer :: dzmaxdune
   real(fp), pointer :: ashld
   real(fp), pointer :: bshld
   real(fp), pointer :: cshld
   real(fp), pointer :: dshld
   real(fp), pointer :: alfpa
   real(fp), pointer :: thcrpa
   logical, pointer :: neglectentrainment
   real(fp), dimension(:, :), pointer :: rca
   real(fp), dimension(:, :), pointer :: statqnt

end module m_fm_erosed
