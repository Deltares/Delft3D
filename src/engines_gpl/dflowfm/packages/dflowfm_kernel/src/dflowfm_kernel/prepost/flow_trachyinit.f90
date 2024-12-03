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

!! Initialise trachytope module containing FM data for trachytopes.
!! Note that arrays are dimensioned on the number of net links
!! This was done so that roughness characteristics can be specified
!! not related to the location of open boundaries, which are not
!! yet available at the time the model is constructed.
!! Besides that certain sediment transport formulae require a
!! Cf at the flow node, which can only accurately be deterimined if the
!! values at net links are known.
subroutine flow_trachyinit()
   use grid_dimens_module
   use network_data, only: numl, lne, xk, yk, kn, lnn
   use unstruc_model ! (contains md_ptr)
   use m_flowparameters
   use m_flowgeom
   use m_physcoef, only: ifrctypuni
   use m_flow, only: kmx, zslay, z0urou
   use m_flowtimes, only: dt_user
   use m_trachy ! (FM module containing trachy data structure)
   use precision, only: dp
   use m_rdtrt ! (contains dimtrt)
   use m_trtrou ! (contains chktrt)
   use unstruc_files, only: mdia
   use m_bedform, only: bfmpar
   use m_sediment
   use unstruc_messages
   use trachytopes_data_module, only: TRACHY_UNDEFINED, TRACHY_NOT_IN_SUBDOMAIN ! = -99999, -77777
   use m_monitoring_crosssections, only: crs, ncrs
   use m_observations_data, only: namobs, numobs
   use kdtree2Factory
   use m_missing
   use m_sferic, only: jsferic, jasfer3D
   use geometry_module, only: dbdistance, half
   use m_vegetation, only: jabaptist
   !
   implicit none
   !
   integer, pointer :: ntrtcrs
   integer, pointer :: ntrtobs
   !
   real(kind=dp) :: xE, yE, xF, yF, x, y, dist
   real(kind=dp), parameter :: dtol_trachy = 1d-4 !< tolerance for distance in finding net-link numbers based on xuL,yuL
   !
   real(kind=dp), dimension(:), allocatable :: xuL !< xu points on net-links
   real(kind=dp), dimension(:), allocatable :: yuL !< yu points on net-links
   !
   integer :: istat
   integer :: itt
   integer :: k, kL, kR, k1, k2
   integer :: icrs
   integer :: iobs
   integer :: itrt
   integer :: itrtcrs
   integer :: itrtobs
   integer :: L
   integer :: LF
   integer :: ddbval = 0
   integer :: threshold_abort_current

   real(kind=dp) :: dummy_tunit = 1d0
   !
   logical :: lftrto
   logical :: error
   logical :: trt_in_arl = .false. ! is trachytope in arl definitions (used for error logging of discharge and waterlevel dependent trachytopes)
   !
   character(256), pointer :: flnmD50
   character(256), pointer :: flnmD90
   logical, pointer :: lfbedfrmrou
   !
   ! Pointers to module vars
   !
   flnmD50 => bfmpar%flnmD50
   flnmD90 => bfmpar%flnmD90
   lfbedfrmrou => bfmpar%lfbedfrmrou
   !
   if (allocated(sig)) deallocate (sig)
   if (allocated(umag)) deallocate (umag)
   if (allocated(z0rou)) deallocate (z0rou)
   if (allocated(dx_trt)) deallocate (dx_trt)
   if (allocated(hu_trt)) deallocate (hu_trt)
   if (allocated(kcu_trt)) deallocate (kcu_trt)

   kmaxtrt = max(kmx, 1)
   allocate (sig(kmaxtrt)); sig = 0d0
   allocate (umag(ndx)); umag = 0d0
   allocate (z0rou(ndx)); z0rou = 0d0
   !
   ! Allocate arrays for moving data from flow links to net links
   !
   allocate (hu_trt(numl))
   allocate (kcu_trt(numl)) ! TODO: alle deallocs oplossen, zodra FM ook een flow_finalize heeft.
   allocate (dx_trt(numl))
   !
   !
   ! Temporariliy set threshold for abort to LEVEL_FATAL
   threshold_abort_current = threshold_abort
   threshold_abort = LEVEL_FATAL
   !
   z0rou = 0d0
   do L = 1, lnx
      k1 = ln(1, L); k2 = ln(2, L)
      z0rou(k1) = z0rou(k1) + wcl(1, L) * z0urou(L)
      z0rou(k2) = z0rou(k2) + wcl(2, L) * z0urou(L)
   end do
   !
   z0rou = max(z0rou, epsz0)
   !
   ! Delft3D       sig        FM      slay        at centre cell (FM)         conversion FM to Delft3D style
   !             0 = top             1 = top
   do k = 1, kmx ! k = 1         -1/6      j=kmx      1                                       sig(1) = 1.0 - 0.5*slay(kmx)   - 0.5*slay(kmx-1)
      sig(k) = 1.0 - 0.5 * zslay(kmx - k + 1, 1) - 0.5 * zslay(kmx - k, 1) ! k = 2         -1/2      j= 2      2/3    0.5*slay(j) + 0.5*slay(j-1)       sig(2) = 1.0 - 0.5*slay(kmx-1) - 0.5*slay(kmx-1-1)
   end do ! k = 3 = kmx   -5/6      j= 1      1/3                                      sig(3) = 1.0 - 0.5*slay(kmx-2) - 0.5*slay(kmx-2-1)
   !                         j= 0       0                                       sig(k) = 1.0 - 0.5*slay(kmx-k+1) - 0.5*slay(kmx-k)
   !             -1 = bed             0 = bed

   error = .false.

   ! If trachytopes not defined return
   !if (jatrt == 0) return

   ! Construct a default griddim struct (dimension is lnx = number of flow links )  ! (better dimension is numl based on number of net links)
   ! (Contrary to the morphology routine call (where the dimension is ndxi)
   ! [TO DO: make uniform, or adjust griddim structure to allow both flow nodes, net links (& flow-links)?]
   call simplegrid_dimens(trachy_griddim, numl, 1)

   ! Construct memory structure for trachytopes on flow links
   call inittrachy(trachy_fl, 1, istat)

   ! Read dimensions of trachytope memory structure
   call dimtrt(mdia, error, trachy_fl, trtdef_ptr, &
             & trachy_griddim)
   if (error) then
      call SetMessage(LEVEL_FATAL, 'flow_trachyinit:: Error reading trachytope dimensions (dimtrt)')
   end if

   call rdtrt(mdia, error, lftrto, dt_user, & !lftrto = jatrt (read twice, in unstruc_model and rdtrt), so always true after rdtrt
            & kmaxtrt, itimtt, trachy_fl, &
            & trachy_griddim, 0.1_fp, trtdef_ptr, .false., &
            & ddbval, dummy_tunit)
   if (error) then
      call SetMessage(LEVEL_FATAL, 'flow_trachyinit:: Error reading trachytopes (rdtrt)')
   end if

   ! Initialise kcu_trt
   kcu_trt = 1
   do L = 1, numl
      trachy_fl%dir(1)%kcu_trt(L) = kcu_trt(L) ! Copy here to be able to pass on to chktrt. TODO: choose which kcu_trt should remain (or both).
   end do

   ! Check if trachytopes are defined
   call chktrt(mdia, error, trachy_griddim, &
             & trachy_fl, flnmD50, flnmD50, lfbedfrmrou, stm_included, ddbval)
   if (error) then
      call SetMessage(LEVEL_FATAL, 'unstruc::flow_trachyinit - Error reading trachytope definitions')
   end if

   allocate (xuL(numL), yuL(numL)) ! can we refer to tree instead ?
   xuL = DMISS
   yuL = DMISS
   do L = 1, numL
      k1 = kn(1, L)
      k2 = kn(2, L)
      call half(xk(k1), yk(k1), xk(k2), yk(k2), xuL(L), yuL(L), jsferic, jasfer3D)
   end do

   ! Update neighboring links and distance weighting
   do L = 1, numl ! loop is safe for 1D/1D-2D
      kL = lne(1, L) !flow node neighbouring net-link on from-side  (also for 1D, and 1D-2D)
      kR = lne(2, L) !flow node neighbouring net-link on to-side    (also for 1D, and 1D-2D)
      if (kL == 0 .and. kR == 0) cycle ! special case for dry areas
      !
      if (kL /= 0 .and. kR /= 0) then

         trachy_fl%dir(1)%lin(1, L) = abs(kL)
         trachy_fl%dir(1)%lin(2, L) = abs(kR)
         if (lne2ln(L) > 0) then
            ! net link corresponds to flow link
            trachy_fl%dir(1)%acLin(L) = acl(lne2ln(L))
         else
            ! for example: net link at thin dam internally in the domain
            trachy_fl%dir(1)%acLin(L) = dbdistance(xz(kL), yz(kL), xuL(L), xuL(L), jsferic, jasfer3D, dmiss) / &
                                        dbdistance(xz(kL), yz(kL), xz(kR), yz(kR), jsferic, jasfer3D, dmiss)
         end if
      else
         ! net link is on closed boundary (Note: 1D network should not get here)
         trachy_fl%dir(1)%lin(1, L) = max(abs(kR), abs(kL))
         trachy_fl%dir(1)%lin(2, L) = max(abs(kR), abs(kL))
         trachy_fl%dir(1)%acLin(L) = 1.0
      end if
   end do
   !
   ! determine if umag is needed.
   !
   update_umag = .false.
   itrt = 0
   do while ((.not. update_umag) .and. (itrt < trachy_fl%gen%ntrt))
      itrt = itrt + 1
      ! The statement could be optimized a bit more to be evaluated only if such area definitions exist.
      if ((trachy_fl%gen%ittdef(itrt, 2) == 103) .or. (trachy_fl%gen%ittdef(itrt, 2) == 104) .or. (trachy_fl%gen%ittdef(itrt, 2) == 155)) then ! if Van Rijn roughness predictor or Struiksma roughness predictor or Vaestila vegetation roughness
         update_umag = .true.
      end if
   end do
   !
   itrt = 0
   do while ((.not. trachy_resistance) .and. (itrt < trachy_fl%gen%ntrt))
      itrt = itrt + 1
      ! The statement could be optimized a bit more to be evaluated only if such area definitions exist (see above).
      if ((trachy_fl%gen%ittdef(itrt, 2) == 154) .or. (trachy_fl%gen%ittdef(itrt, 2) == 155) .or. (trachy_fl%gen%ittdef(itrt, 2) == 156)) then ! if Baptist type 154
         trachy_resistance = .true.
      end if
   end do
   if (trachy_resistance .and. (jabaptist >= 2)) then
      call mess(LEVEL_ERROR, 'Trachytopes and Vegetationmodelnr >= 2 cannot be used in the same simulation', mdia)
   end if
   !
   ! Connection to FM definitions
   !
   if (ifrctypuni == 0) then
      rouflo = 'CHEZ'
   elseif (ifrctypuni == 1) then
      rouflo = 'MANN'
   elseif (ifrctypuni == 2) then
      rouflo = 'WHIT'
   elseif (ifrctypuni == 3) then
      rouflo = 'WHIT'
   else
      call SetMessage(LEVEL_FATAL, 'Unsupported friction type specified in combination with trachytopes')
   end if

   do L = 1, numl
      kL = lne(1, L); kR = lne(2, L)
      if (kL == 0 .and. kR == 0) cycle
      LF = lne2ln(L)
      if (LF > 0) then
         !link is on flow-link
         dx_trt(L) = dx(LF)
      else
         ! link is not on flow-link --> closed boundary
         ! get x,y on midpoint of edge
         xE = xuL(L)
         yE = yuL(L)
         ! get x,y on midpoint of neighbouring flow node
         xF = xz(trachy_fl%dir(1)%lin(1, L))
         yF = yz(trachy_fl%dir(1)%lin(1, L))
         dx_trt(L) = 2.0 * dbdistance(xE, yE, xF, yF, jsferic, jasfer3D, dmiss) ! determine distance (as if it were a flow node)    (dx - oppervlakte/net link lengte ?) (test 2. cel-gemiddelde ruwheid) (3. veel cellen ... )
      end if
   end do

   do itt = 1, trachy_fl%dir(1)%nttaru

      if (trachy_fl%dir(1)%ittaru(itt, 4) == TRACHY_MISSING_VALUE) then
         x = trachy_fl%dir(1)%rttxyz(itt, 1)
         y = trachy_fl%dir(1)%rttxyz(itt, 2)
         ! z=trachy_fl%dir(1)%rttxyz(itt,3) (not used).

         ! fill query vector
         call make_queryvector_kdtree(treeglob, x, y, jsferic)

         ! find nearest link
         call kdtree2_n_nearest(treeglob%tree, treeglob%qv, 1, treeglob%results)

         ! get link number
         L = treeglob%results(1)%idx

         ! check distance
         dist = treeglob%results(1)%dis
         ! dist = dbdistance(xuL(L),yuL(L),x,y)  (alternatively)

         if (dist < dtol_trachy .and. lnn(L) > 0) then
            trachy_fl%dir(1)%ittaru(itt, 4) = L !(net link number)
         else
            trachy_fl%dir(1)%ittaru(itt, 4) = TRACHY_NOT_IN_SUBDOMAIN
         end if
      end if
   end do

   ! check cross-section names and link to local FM cross-section index
   ntrtcrs => trachy_fl%gen%ntrtcrs
   do itrtcrs = 1, ntrtcrs
      do icrs = 1, ncrs
         if (trim(trachy_fl%gen%crs(itrtcrs)%name) == trim(crs(icrs)%name)) then
            trachy_fl%gen%crs(itrtcrs)%id = icrs
         end if
      end do

      itrt = trachy_fl%gen%ittdef(trachy_fl%gen%crs(itrtcrs)%itrt, 1)

      trt_in_arl = .false.
      itt = 0
      do while ((.not. trt_in_arl) .and. (itt < trachy_fl%dir(1)%nttaru))
         itt = itt + 1
         if (trachy_fl%dir(1)%ittaru(itt, 3) == itrt) then ! if trachytope is included in .arl file
            trt_in_arl = .true.
         end if
      end do

      if ((trachy_fl%gen%crs(itrtcrs)%id == TRACHY_UNDEFINED) .and. trt_in_arl) then
         call SetMessage(LEVEL_FATAL, 'Error reading trachytopes: Cross-section does not exist in "'//trim(trachy_fl%gen%md_ttdfile)//'": '//trim(trachy_fl%gen%crs(itrtcrs)%rec132))
      end if
   end do

   ! check observation-station names and link to local FM observation-station index
   ntrtobs => trachy_fl%gen%ntrtobs
   do itrtobs = 1, ntrtobs
      do iobs = 1, numobs
         if (trim(trachy_fl%gen%obs(itrtobs)%name) == trim(namobs(iobs))) then
            trachy_fl%gen%obs(itrtobs)%id = iobs
         end if
      end do

      itrt = trachy_fl%gen%ittdef(trachy_fl%gen%obs(itrtobs)%itrt, 1)

      trt_in_arl = .false.
      itt = 0
      do while ((.not. trt_in_arl) .and. (itt < trachy_fl%dir(1)%nttaru))
         itt = itt + 1
         if (trachy_fl%dir(1)%ittaru(itt, 3) == itrt) then ! if trachytope is included in .arl file
            trt_in_arl = .true.
         end if
      end do

      if ((trachy_fl%gen%obs(itrtobs)%id == TRACHY_UNDEFINED) .and. trt_in_arl) then
         call SetMessage(LEVEL_FATAL, 'Error reading trachytopes: Observation station does not exist in "'//trim(trachy_fl%gen%md_ttdfile)//'": '//trim(trachy_fl%gen%obs(itrtobs)%rec132))
      end if
   end do

   if (allocated(xuL)) deallocate (xuL)
   if (allocated(yuL)) deallocate (yuL)
   threshold_abort = threshold_abort_current

end subroutine flow_trachyinit
