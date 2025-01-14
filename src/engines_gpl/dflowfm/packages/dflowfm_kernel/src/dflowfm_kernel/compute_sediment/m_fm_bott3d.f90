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

!> Module with subroutines for bed level update.
module m_fm_bott3d

   use m_fm_adjust_bedload, only: fm_adjust_bedload
   use m_duneaval, only: duneaval
   use m_compthick, only: compthick
   use m_collectcumultransports, only: collectcumultransports
   use m_bndmorlyr, only: bndmorlyr
   use m_bermslopenudging, only: bermslopenudging
   use precision

   implicit none

   private !Prevent used modules from being exported
   public :: fm_bott3d

contains

   !< Computes suspended sediment transport correction
   !! vector for sand sediment fractions
   !! Computes depth integrated suspended sediment
   !! transport vector for output to map file
   !! Computes change in BODSED based on source and sink
   !! terms calculated in EROSED, and new concentrations.
   !! Calculates new mixing layer thickness based on
   !! change in BODSED values
   !! Calculates new depth values based on changes
   !! in bottom sediment.
   !! Includes erosion of dry points and associated
   !! bathymetry changes
   subroutine fm_bott3d()

   !!
   !! Declarations
   !!

      use Messagehandling
      use message_module, only: writemessages, write_error
      use precision
      use precision_basics
      use bedcomposition_module
      use sediment_basics_module
      use m_flowgeom, only: ndxi, ndx
      use m_flowparameters, only: eps10, jawave
      use fm_external_forcings_data, only: nopenbndsect
      use m_flowtimes, only: dts, tstart_user, time1, tfac, ti_sed, ti_seds, handle_extra
      use unstruc_files, only: mdia
      use m_fm_erosed, only: mtd, tmor, bc_mor_array, lsedtot, e_ssn, bermslopetransport, duneavalan, bedw, bed, dbodsd, e_sbcn, e_sbct, e_sbwn, e_sswn, e_sswt, lsed, morfac, stmpar, susw, tcmp, sbcx, sbcy, morft, ucxq_mor, ucyq_mor, blchg, e_sbwt, hs_mor, hydrt, sbwx, sbwy, sscx, sscy, sswx, sswy
      use m_sediment, only: kcsmor
      use m_partitioninfo, only: jampi, ITYPE_Sall, update_ghosts
      use m_fm_morstatistics, only: morstats, morstatt0
      use m_tables, only: interpolate
      use Timers
      use m_reconstruct_sed_transports

      implicit none

   !!
   !! Local parameters
   !!

      real(kind=dp), parameter :: DAY2SEC = 86400.0d0 !< seconds in a day
      real(kind=dp), parameter :: H2SEC = 3600.0d0 !< seconds in an hour
      logical, parameter :: AVALANCHE_ON = .true.
      logical, parameter :: AVALANCHE_OFF = .false.
      logical, parameter :: SLOPECOR_ON = .true.
      logical, parameter :: SLOPECOR_OFF = .false.

   !!
   !! Local variables
   !!

      logical :: error

      integer :: ierror, ll

      real(kind=dp) :: dtmor
      real(kind=dp) :: timhr

      logical, pointer :: cmpupd

   !!
   !! Point
   !!

      cmpupd => stmpar%morpar%cmpupd

   !!
   !! Execute
   !!
      call timstrt('Bott3d_call   ', handle_extra(89))

      dtmor = dts * morfac
      error = .false.
      timhr = time1 / H2SEC
      blchg(:) = 0d0
      e_ssn(:, :) = 0d0

      call fm_suspended_sand_correction()

      call fm_total_face_normal_suspended_transport()
      !
      ! Add equilibrium berm slope adjustment
      !
      if (bermslopetransport) then
         call bermslopenudging(error)
         if (error) then
            write (errmsg, '(a)') 'fm_bott3d::bermslopenudging returned an error. Check your inputs.'
            call write_error(errmsg, unit=mdia)
         end if
      end if
      call timstop(handle_extra(89))
      !
      ! BEGIN: Moved parts from `fm_erosed`
      !
      call timstrt('Erosed_call   ', handle_extra(88))
      if (bed > 0.0_fp) then
         call fm_adjust_bedload(e_sbcn, e_sbct, AVALANCHE_ON, SLOPECOR_ON)
      end if
      !
      !See: UNST-7367
      call apply_nodal_point_relation()
      !
      ! Bed-slope and sediment availability effects for
      ! wave-related bed load transport
      !
      if (bedw > 0.0_fp .and. jawave > 0) then
         call fm_adjust_bedload(e_sbwn, e_sbwt, AVALANCHE_OFF, SLOPECOR_ON)
      end if
      !
      ! Sediment availability effects for
      ! wave-related suspended load transport
      !
      if (susw > 0.0_fp .and. jawave > 0) then
         call fm_adjust_bedload(e_sswn, e_sswt, AVALANCHE_OFF, SLOPECOR_OFF)
      end if
      !
      if (duneavalan) then
         call duneaval(error)
         if (error) then
            write (errmsg, '(a)') 'fm_bott3d::duneavalan returned an error. Check your inputs.'
            call write_error(errmsg, unit=mdia)
         end if
      end if
      !
      call sum_current_wave_transport_links()
      !
      call timstop(handle_extra(88))
      !
      ! END: Moved parts from `fm_erosed`
      !
      !
      ! if bed composition computations have started
      !
      call timstrt('Bott3d_call   ', handle_extra(89))
      !
      if (time1 >= tstart_user + tcmp * tfac) then ! tmor/tcmp in tunit since start of computations, time1 in seconds since reference date

         call fm_bed_boundary_conditions(timhr)

         call fm_change_in_sediment_thickness(dtmor)

         call fluff_burial(stmpar%morpar%flufflyr, dbodsd, lsed, lsedtot, 1, ndxi, dts, morfac)

         call fm_dry_bed_erosion(dtmor)

         !See: UNST-7368
         if (jampi > 0) then
            call update_ghosts(ITYPE_Sall, lsedtot, Ndx, dbodsd, ierror)
         end if

         call fm_apply_mormerge()

         do ll = 1, lsedtot
            dbodsd(ll, :) = dbodsd(ll, :) * kcsmor
         end do

         call reconstructsedtransports() ! reconstruct cell centre transports for morstats and cumulative st output
         call collectcumultransports() ! Always needed, written on last timestep of simulation
         call fm_exclude_cmpupdfrac() ! Conditionally exclude specific fractions from erosion and sedimentation

         if (stmpar%morpar%moroutput%morstats .and. ti_sed > 0d0) then
            call morstats(dbodsd, hs_mor, ucxq_mor, ucyq_mor, sbcx, sbcy, sbwx, sbwy, sscx, sscy, sswx, sswy)
         end if
         !
         ! Apply erosion and sedimentation to bookkeeping system
         !
         if (cmpupd) then
            !
            ! Determine new thickness of transport layer
            !
            call compthick()
            !
            ! Update layers and obtain the depth change
            !
            !See: UNST-7369
            if (updmorlyr(stmpar%morlyr, dbodsd, blchg, mtd%messages) /= 0) then
               call writemessages(mtd%messages, mdia)
               write (errmsg, '(a,a,a)') 'fm_bott3d :: updmorlyr returned an error.'
               call write_error(errmsg, unit=mdia)
               error = .true.
               return
            else
               call writemessages(mtd%messages, mdia)
            end if
            call lyrdiffusion(stmpar%morlyr, dtmor)
            !
            ! Apply composition boundary conditions
            !
            call bndmorlyr(lsedtot, timhr, nopenbndsect, bc_mor_array, stmpar)
         end if
      end if ! time1>tcmp

      if (time1 >= tstart_user + tmor * tfac) then
         !
         ! Increment morphological time
         ! Note: dtmor in seconds, morft in days!
         !
         morft = morft + dtmor / DAY2SEC
         if (morfac > 0d0) hydrt = hydrt + dts / DAY2SEC
         if (stmpar%morpar%moroutput%morstats) then
            if (comparereal(time1, ti_seds, eps10) >= 0) morstatt0 = morft
         end if
         !
         call fm_blchg_no_cmpupd() !Compute bed level changes without actually updating the bed composition
         !
         call fm_apply_bed_boundary_condition(dtmor, timhr)

      else
         !
         ! if morphological computations haven't started yet
         !
         blchg(1:ndx) = 0d0

      end if ! time1<tmor

      call fm_update_bed_level(dtmor)

      !
      call timstop(handle_extra(89))

   end subroutine fm_bott3d

   !< Calculate suspended sediment transport correction vector (for SAND)
   !! Note: uses GLM velocities, consistent with DIFU
   !!
   !! Correct suspended sediment transport rates by estimating the
   !! quantity of suspended sediment transported in the grid cells below
   !! Van Rijn's reference height (aks) and making a vector of this in the
   !! opposite direction to the suspended sediment transport.
   !!
   !! Ensure suspended sediment correction arrays and suspended sediment
   !! vector arrays are blank
   subroutine fm_suspended_sand_correction()

   !!
   !! Declarations
   !!

      use precision
      use precision_basics
      use sediment_basics_module
      use m_debug
      use m_flow, only: u1, kmx, hu
      use m_flowgeom, only: ln, lnx, lnxi, acl, wu_mor
      use m_transport, only: fluxhortot, ised1, constituents, numconst
      use m_fm_erosed, only: aks, e_scrn, e_scrt, fixfac, kfsed, lsed, l_suscor, rca, suscorfac, sus, tratyp
      use m_partitioninfo, only: jampi, itype_u, update_ghosts
      use m_get_Lbot_Ltop

      implicit none

   !!
   !! Local variables
   !!

      integer :: ierror
      integer :: l, ll, Lx, Lf, k1, k2
      integer :: Lb, Lt, ka, kf1, kf2, ac1, ac2

      real(kind=dp) :: cavg
      real(kind=dp) :: cavg1
      real(kind=dp) :: cavg2
      real(kind=dp) :: ceavg
      real(kind=dp) :: cumflux
      real(kind=dp) :: aksu
      real(kind=dp) :: apower
      real(kind=dp) :: cflux
      real(kind=dp) :: dz
      real(kind=dp) :: dzup
      real(kind=dp) :: r1avg
      real(kind=dp) :: z
      real(kind=dp) :: zktop

   !!
   !! Execute
   !!

      e_scrn(:, :) = 0d0
      e_scrt(:, :) = 0d0

      !
      ! calculate corrections
      !
      if (sus /= 0d0 .and. l_suscor) then
         !
         ! suspension transport correction vector only for 3D
         !
         if (kmx > 0) then
            !
            if (jampi > 0) then
               call update_ghosts(ITYPE_U, NUMCONST, lnx, fluxhortot, ierror)
            end if
            !
            do l = 1, lsed
               ll = ISED1 - 1 + l
               if (tratyp(l) == TRA_COMBINE) then
                  !
                  ! Determine aks
                  !
                  do Lx = 1, lnx
                     if (wu_mor(Lx) == 0d0) cycle
                     ac1 = acL(Lx)
                     ac2 = 1d0 - ac1
                     k1 = ln(1, Lx); k2 = ln(2, Lx)
                     call getLbotLtop(Lx, Lb, Lt)
                     if (Lt < Lb) cycle
                     !
                     ! try new approach - should be smoother
                     ! don't worry about direction of the flow
                     ! use concentration at velocity point=average of the
                     ! two adjacent concentrations
                     ! use aks height at velocity point = average of the
                     ! two adjacent aks values
                     !
                     ! note correction vector only computed for velocity
                     ! points with active sediment cells on both sides
                     !
                     if (kfsed(k1) * kfsed(k2) > 0) then ! bring sedthr into account
                        cumflux = 0.0_fp
                        !
                        ! Determine reference height aks in vel. pt.
                        !
                        if (Lx > lnxi) then ! boundary link, take inner point value
                           aksu = aks(k2, l)
                        else
                           aksu = ac1 * aks(k1, l) + ac2 * aks(k2, l)
                        end if
                        !
                        ! work up through layers integrating transport flux
                        ! below aksu, according to Bert's new implementation
                        !
                        zktop = 0d0
                        ka = 0
                        if (kmx == 1) then
                           if (aksu > hu(Lx)) then
                              ka = 0
                           else
                              ka = Lt
                           end if
                        else
                           do Lf = Lb, Lt
                              zktop = hu(Lf)
                              dz = hu(Lf) - hu(Lf - 1)
                              !
                              ! if layer contains aksu
                              !
                              if (aksu <= zktop) then
                                 ka = Lf
                                 if (Lf /= Lt) then
                                    dzup = hu(Lf + 1) - hu(Lf)
                                 end if
                                 ! the contribution of this layer is computed below
                                 exit
                              else
                                 cumflux = cumflux + fluxhortot(ll, Lf)
                              end if
                           end do
                        end if
                        !
                        if (ka == 0) then
                           ! aksu larger than water depth, so all done
                        elseif (ka == Lt) then
                           ! aksu is located in top layer; use simple flux
                           ! approximation assuming uniform flux
                           cumflux = cumflux + fluxhortot(ll, ka) * (aksu - hu(Lt - 1)) / dz ! kg/s
                        else
                           ! aksu is located in a layer below the top layer
                           !
                           ! Get reference concentration at aksu
                           !
                           if (Lx > lnxi) then ! boundary link, take inner point value
                              ceavg = rca(k2, l)
                           else
                              ceavg = ac1 * rca(k1, l) + ac2 * rca(k2, l) ! Perot average
                           end if
                           !
                           ! Get concentration in layer above this layer
                           !
                           kf1 = ln(1, ka + 1); kf2 = ln(2, ka + 1)
                           r1avg = ac1 * constituents(ll, kf1) + ac2 * constituents(ll, kf2)
                           !
                           ! If there is a significant concentration gradient, and significant
                           ! reference concentration
                           !
                           if (ceavg > r1avg * 1.1d0 .and. ceavg > 0.05d0) then
                              !
                              ! Compute Rouse number based on reference concentration and
                              ! concentration of the layer above it. Make sure that Rouse number
                              ! differs significantly from 1, and that it is not too large.
                              ! Note: APOWER = - Rouse number
                              !
                              ! The Rouse profile equation
                              !
                              !            [ a*(H-z) ]^R
                              ! c(z) = c_a*[ ------- ]
                              !            [ z*(H-a) ]
                              !
                              ! is here approximated by
                              !
                              ! c(z) = c_a*(a/z)^R = c_a*(z/a)^-R
                              !
                              z = zktop + dzup / 2.0_fp
                              apower = log(max(r1avg / ceavg, 1d-5)) / log(z / aksu)
                              if (apower > -1.05d0 .and. apower <= -1.0d0) then ! you have decide on the eq to -1.0
                                 apower = -1.05d0
                              elseif (apower > -1.0d0 .and. apower < -0.95d0) then
                                 apower = -0.95d0
                              end if
                              apower = min(max(-10.0d0, apower), 10.0d0)
                              !
                              ! Compute the average concentration cavg between the reference
                              ! height a and the top of the current layer (bottom of layer above) z.
                              !           /zktop                           zktop                       zktop
                              ! cavg*dz = | c(z) dz = c_a/(-R+1)*(z/a)^(-R+1)*a | = c_a/(-R+1)*a^R*z^(-R+1) |
                              !          /a                                     a                           a
                              !
                              cavg1 = (ceavg / (apower + 1.0d0)) * (1d0 / aksu)**apower
                              cavg2 = zktop**(apower + 1.0d0) - aksu**(apower + 1.0d0)
                              cavg = cavg1 * cavg2 ! kg/m3/m
                              !
                              ! The corresponding effective suspended load flux is
                              !
                              cflux = u1(ka) * cavg * dz * wu_mor(Lx)
                              !
                              ! Increment the correction by the part of the suspended load flux
                              ! that is in excess of the flux computed above, but never opposite.
                              !
                              if (fluxhortot(ll, ka) > 0.0d0 .and. cflux > 0.0d0) then
                                 cumflux = cumflux + max(0.0d0, fluxhortot(ll, ka) - cflux)
                              elseif (fluxhortot(ll, ka) < 0.0d0 .and. cflux < 0.0_fp) then
                                 cumflux = cumflux + min(fluxhortot(ll, ka) - cflux, 0.0d0)
                                 !else
                                 !   cumflux = cumflux + fluxhortot(ll,ka)    ! don't correct in aksu layer
                              end if
                           end if
                        end if
                        e_scrn(Lx, l) = -suscorfac * cumflux / wu_mor(Lx)
                        !
                        ! bedload will be reduced in case of sediment transport
                        ! over a non-erodible layer (no sediment in bed) in such
                        ! a case, the suspended sediment transport vector must
                        ! also be reduced.
                        !
                        if (e_scrn(Lx, l) > 0.0d0 .and. Lx <= lnxi) then
                           e_scrn(Lx, l) = e_scrn(Lx, l) * fixfac(k1, l) ! outgoing (cumflux<0)
                        else
                           e_scrn(Lx, l) = e_scrn(Lx, l) * fixfac(k2, l) ! take inner point fixfac on bnd
                        end if
                     else
                        e_scrn(Lx, l) = 0.0d0
                     end if
                  end do ! lnx
               end if ! tratyp == TRA_COMBINE
            end do ! l
         end if ! kmx>0; end of correction for bed/total load
      end if ! sus /= 0.0

   end subroutine fm_suspended_sand_correction

   !< Distribute sediment transport at a 1D node connected to more than
   !! one branch (e.g., a bifurcation). This is done by applying a closure
   !! relation (the nodal point relation)
   subroutine apply_nodal_point_relation()
      use precision, only: dp

   !!
   !! Declarations
   !!

      use Messagehandling
      use message_module, only: writemessages, write_error
      use unstruc_channel_flow, only: network, t_branch, t_node, nt_LinkNode
      use m_flowgeom, only: nd, wu_mor
      use m_flow, only: u1, qa
      use m_flowparameters, only: flow_solver, FLOW_SOLVER_FM
      use m_fm_erosed, only: lsedtot, e_sbcn, e_sbct
      use m_sediment, only: stmpar
      use m_ini_noderel, only: get_noderel_idx
      use m_tables, only: interpolate
      use morphology_data_module, only: t_nodefraction, t_noderelation

      implicit none

   !!
   !! Local variables
   !!

      logical :: error

      integer :: inod, j, istat, ised, ifrac, k1, k3, nrd_idx, L

      integer, dimension(:), allocatable :: branInIDLn !< ID of Incoming Branch (If there is only one) (nnod)

      integer, dimension(:, :, :), allocatable :: sb_dir !< direction of transport at node (nnod, lsedtot, nbr) (-1 = incoming or no transport, +1 = outgoing)

      real(fp), dimension(:), allocatable :: qb_out !< sum of outgoing discharge at 1d node
      real(fp), dimension(:), allocatable :: width_out !< sum of outgoing main channel widths

      real(fp), dimension(:, :), allocatable :: sb_in !< sum of incoming sediment transport at 1d node

      real(kind=dp) :: ldir
      real(kind=dp) :: faccheck
      real(kind=dp) :: expQ
      real(kind=dp) :: expW
      real(kind=dp) :: facQ
      real(kind=dp) :: facW
      real(kind=dp) :: qb1d, wb1d, sb1d
      real(kind=dp) :: sbrratio, qbrratio, Qbr1, Qbr2

      type(t_nodefraction), pointer :: pFrac
      type(t_noderelation), pointer :: pNodRel
      type(t_node), pointer :: pnod

   !!
   !! Allocate and initialize
   !!

      istat = 0
      if (istat == 0) allocate (qb_out(network%nds%Count), stat=istat)
      if (istat == 0) allocate (width_out(network%nds%Count), stat=istat)
      if (istat == 0) allocate (sb_in(network%nds%Count, lsedtot), stat=istat)
      if (istat == 0) allocate (sb_dir(network%nds%Count, lsedtot, network%nds%maxnumberofconnections), stat=istat)
      if (istat == 0) allocate (branInIDLn(network%nds%Count), stat=istat)

      qb_out(:) = 0d0; width_out(:) = 0d0; sb_in(:, :) = 0d0; sb_dir(:, :, :) = 1
      BranInIDLn(:) = 0

   !!
   !! Execute
   !!

      !
      ! Determine incoming discharge and transport at nodes
      !
      do inod = 1, network%nds%Count
         pnod => network%nds%node(inod)
         if (pnod%numberofconnections > 1) then
            k3 = pnod%gridnumber
            do j = 1, nd(k3)%lnx
               L = abs(nd(k3)%ln(j))
               Ldir = sign(1, nd(k3)%ln(j))
               !
               wb1d = wu_mor(L)
               !
               if (u1(L) * Ldir < 0d0) then
                  ! Outgoing discharge
                  qb1d = -qa(L) * Ldir ! replace with junction advection: to do WO
                  width_out(inod) = width_out(inod) + wb1d
                  qb_out(inod) = qb_out(inod) + qb1d
                  do ised = 1, lsedtot
                     sb_dir(inod, ised, j) = -1 ! set direction to outgoing
                  end do
               else
                  ! Incoming discharge
                  if (branInIDLn(inod) == 0) then
                     branInIDLn(inod) = L
                  else
                     branInIDLn(inod) = -444 ! multiple incoming branches
                  end if
               end if
            end do
         end if
      end do
      !
      ! Apply nodal relations to transport
      !
      do inod = 1, network%nds%Count
         pnod => network%nds%node(inod)
         if (pnod%numberofconnections == 1) cycle
         if (pnod%nodeType == nt_LinkNode) then ! connection node
            k1 = pnod%gridnumber
            do j = 1, nd(k1)%lnx
               L = abs(nd(k1)%ln(j))
               Ldir = sign(1, nd(k1)%ln(j))
               !
               wb1d = wu_mor(L)
               do ised = 1, lsedtot
                  sb1d = e_sbcn(L, ised) * Ldir ! first compute all outgoing sed. transport.
                  if (flow_solver == FLOW_SOLVER_FM .or. pnod%numberofconnections == 2) then !standard
                     !V: In the standard scheme, at the <e_sbcn> of the outgoing links we have the upwind transport, i.e.,
                     !part of the transport in the junction node. By summing over all of them we have the total transport at
                     !the junction node, which we then redistribute.
                     !We apply this to the standard scheme and to the nodes with only 2 connections, as in this second case
                     !we have not modified the link direction and the same logic applies as for the standard scheme.
                     ! this works for one incoming branch TO DO: WO
                     if (sb_dir(inod, ised, j) == -1) then
                        sb_in(inod, ised) = sb_in(inod, ised) + max(-wb1d * sb1d, 0.0_fp) ! outgoing transport is negative
                     end if
                  else !FM1DIMP
                     !V: In the FM1DIMP scheme at <e_sbcn> of the incoming links we have the upwind transport, i.e., the transport
                     !in the ghost cell for multivaluedness of each branch. By summing over all of them we have the total
                     !transport incoming to the junction, which we want to redistribute.
                     if (sb_dir(inod, ised, j) == 1) then
                        sb_in(inod, ised) = sb_in(inod, ised) + wb1d * sb1d ! incoming transport is positive
                     end if
                  end if
               end do
            end do
         end if
      end do
      !
      ! Determining sediment redistribution
      !
      ! loop over sediment fractions
      do ised = 1, lsedtot

         ! mor%nrd%nFractions = or 1 (One for All Fractions) or lsedtot (One for Every Fraction)
         iFrac = min(ised, stmpar%nrd%nFractions)

         pFrac => stmpar%nrd%nodefractions(iFrac)

         do inod = 1, network%nds%Count
            pnod => network%nds%node(inod)
            if (pnod%nodeType == nt_LinkNode) then ! connection node

               facCheck = 0.d0

               if (pnod%numberofconnections == 1) cycle

               ! loop over branches and determine redistribution of incoming sediment
               k3 = pnod%gridnumber
               do j = 1, nd(k3)%lnx
                  L = abs(nd(k3)%ln(j))
                  Ldir = sign(1, nd(k3)%ln(j))
                  qb1d = -qa(L) * Ldir
                  wb1d = wu_mor(L)

                  ! Get Nodal Point Relation Data
                  nrd_idx = get_noderel_idx(inod, pFrac, pnod%gridnumber, branInIDLn(inod), pnod%numberofconnections)

                  pNodRel => pFrac%noderelations(nrd_idx)

                  if (sb_dir(inod, ised, j) == -1) then ! is outgoing

                     if (qb_out(inod) > 0.0_fp) then

                        if (pNodRel%Method == 'function') then

                           expQ = pNodRel%expQ
                           expW = pNodRel%expW

                           facQ = (qb1d / qb_out(inod))**expQ
                           facW = (wb1d / width_out(inod))**expW

                           facCheck = facCheck + facQ * facW

                           e_sbcn(L, ised) = -Ldir * facQ * facW * sb_in(inod, ised) / wu_mor(L)

                        elseif (pNodRel%Method == 'table') then

                           facCheck = 1.0d0

                           if (L == pNodRel%BranchOut1Ln) then
                              Qbr1 = qb1d
                              Qbr2 = qb_out(inod) - qb1d
                           elseif (L == pNodRel%BranchOut2Ln) then
                              Qbr1 = qb_out(inod) - qb1d
                              Qbr2 = qb1d
                           else
                              call SetMessage(LEVEL_FATAL, 'Unknown Branch Out (This should never happen!)')
                           end if

                           QbrRatio = Qbr1 / Qbr2

                           SbrRatio = interpolate(pNodRel%Table, QbrRatio)

                           if (L == pNodRel%BranchOut1Ln) then
                              e_sbcn(L, ised) = -Ldir * SbrRatio * sb_in(inod, ised) / (1 + SbrRatio) / wu_mor(L)
                              e_sbct(L, ised) = 0.0
                           elseif (L == pNodRel%BranchOut2Ln) then
                              e_sbcn(L, ised) = -Ldir * sb_in(inod, ised) / (1 + SbrRatio) / wu_mor(L)
                              e_sbct(L, ised) = 0.0
                           end if

                        else
                           call SetMessage(LEVEL_FATAL, 'Unknown Nodal Point Relation Method Specified')
                        end if

                     else
                        e_sbcn(L, ised) = 0.0_fp
                        e_sbct(L, ised) = 0.0
                     end if

                  end if

               end do ! Branches

               ! Correct Total Outflow
               if ((facCheck /= 1.0_fp) .and. (facCheck > 0.0_fp)) then
                  ! loop over branches and correct redistribution of incoming sediment
                  do j = 1, nd(k3)%lnx
                     L = abs(nd(k3)%ln(j))
                     if (sb_dir(inod, ised, j) == -1) then
                        e_sbcn(L, ised) = e_sbcn(L, ised) / facCheck
                     end if
                  end do ! Branches
               end if !`facCheck`
            end if
         end do ! Nodes
      end do ! Fractions

   !!
   !! Deallocate
   !!

      if (istat == 0) deallocate (qb_out, stat=istat)
      if (istat == 0) deallocate (width_out, stat=istat)
      if (istat == 0) deallocate (sb_in, stat=istat)
      if (istat == 0) deallocate (sb_dir, stat=istat)
      if (istat == 0) deallocate (BranInIDLn, stat=istat)

      if (istat /= 0) then
         error = .true.
         write (errmsg, '(a)') 'fm_bott3d::error deallocating memory.'
         call mess(LEVEL_FATAL, errmsg)
      end if

   end subroutine apply_nodal_point_relation

   !> Apply morphodynamic boundary condition on bed level
   subroutine fm_bed_boundary_conditions(timhr)
      use precision, only: dp

   !!
   !! Declarations
   !!

      use Messagehandling
      use message_module, only: writemessages, write_error
      use sediment_basics_module
      use m_flowparameters, only: flowWithoutWaves, jawaveswartdelwaq
      use m_flowgeom, only: wu_mor, ln
      use m_flow, only: u1
      use m_fm_erosed, only: lsedtot, bc_mor_array, cdryb, rhosol, nmudfrac, tratyp, e_sbn
      use m_sediment, only: stmpar
      use morphology_data_module, only: bedbndtype
      use table_handles, only: handletype, gettabledata
      use m_flowtimes, only: julrefdat
      use m_partitioninfo, only: idomain, jampi, my_rank, reduce_sum
      use fm_external_forcings_data, only: nopenbndsect
      use m_get_tau

      implicit none

   !!
   !! I/O
   !!

      real(kind=dp), intent(in) :: timhr

   !!
   !! Local variables
   !!

      integer :: jb, nto, ib, lm, k2, L, li, nm, nxmx
      integer :: icond
      integer :: jawaveswartdelwaq_local
      integer :: lsedbed

      real(kind=dp) :: tausum2(1)
      real(kind=dp) :: alfa_dist
      real(kind=dp) :: alfa_mag
      real(kind=dp) :: sbsum
      real(kind=dp) :: taucurc
      real(kind=dp) :: czc
      real(kind=dp) :: rate

      real(kind=dp), dimension(lsedtot) :: bc_sed_distribution

      character(len=256) :: msg

      type(handletype), pointer :: bcmfile
      type(bedbndtype), dimension(:), pointer :: morbnd

   !!
   !! Allocate and initialize
   !!

      bcmfile => stmpar%morpar%bcmfile
      morbnd => stmpar%morpar%morbnd

   !!
   !! Execute
   !!

      if (flowWithoutWaves) then
         jawaveswartdelwaq_local = 0
      else
         jawaveswartdelwaq_local = jawaveswartdelwaq
      end if

      nto = nopenbndsect

      !
      ! Bed boundary conditions: transport condition
      !
      !See: UNST-7370
      do jb = 1, nto ! no of open bnd sections
         icond = morbnd(jb)%icond
         if (icond == 4 .or. icond == 5) then
            !
            ! Open boundary with transport boundary condition:
            ! Get data from table file
            !
            call gettabledata(bcmfile, morbnd(jb)%ibcmt(1), &
               & morbnd(jb)%ibcmt(2), morbnd(jb)%ibcmt(3), &
               & morbnd(jb)%ibcmt(4), bc_mor_array, &
               & timhr, julrefdat, msg)
            !
            ! Prepare loop over boundary points
            !
            tausum2(1) = 0d0
            do ib = 1, morbnd(jb)%npnt
               lm = morbnd(jb)%lm(ib)
               k2 = morbnd(jb)%nxmx(ib)
               if (jampi == 1) then
                  if (.not. (idomain(k2) == my_rank)) cycle ! internal cells at boundary are in the same domain as the link
               end if
               if (u1(lm) < 0.0d0) cycle
               call get_tau(k2, taucurc, czc, jawaveswartdelwaq_local)
               tausum2(1) = tausum2(1) + taucurc**2 ! sum of the shear stress squared
            end do ! the distribution of bedload is scaled with square stress
            ! for avoiding instability on BC resulting from uniform bedload
            ! in combination with non-uniform cells.
            li = 0
            do l = 1, lsedtot
               sbsum = 0d0
               !
               ! bed load transport only for fractions with bedload component
               !
               if (.not. has_bedload(tratyp(l))) cycle
               li = li + 1
               !
               do ib = 1, morbnd(jb)%npnt
                  lm = morbnd(jb)%lm(ib)
                  k2 = morbnd(jb)%nxmx(ib)
                  if (jampi == 1) then
                     if (.not. (idomain(k2) == my_rank)) cycle
                  end if
                  sbsum = sbsum + bc_mor_array(li) * wu_mor(lm) ! sum the total bedload flux throughout boundary
               end do
               bc_sed_distribution(li) = sbsum
            end do

            ! do MPI reduce step for bc_sed_distribution and tausum2
            if (jampi == 1) then
               call reduce_sum(1, tausum2)
               call reduce_sum(lsedtot, bc_sed_distribution)
            end if

            do ib = 1, morbnd(jb)%npnt
               alfa_dist = morbnd(jb)%alfa_dist(ib)
               alfa_mag = morbnd(jb)%alfa_mag(ib)
               !                idir_scalar = morbnd(jb)%idir(ib)
               nm = morbnd(jb)%nm(ib)
               nxmx = morbnd(jb)%nxmx(ib)
               lm = morbnd(jb)%lm(ib)
               !
               ! If the computed transport is directed outward, do not
               ! impose the transport rate (at outflow boundaries the
               ! "free bed level boundary" condition is imposed. This
               ! check is carried out for each individual boundary point.
               !
               ! Detect the case based on the value of nxmx.
               !
               if (u1(lm) < 0.0d0) cycle ! check based on depth averaged velocity value
               !
               ! The velocity/transport points to the left and top are part
               ! of this cell. nxmx contains by default the index of the
               ! neighbouring grid cell, so that has to be corrected. This
               ! correction is only carried out locally since we need the
               ! unchanged nxmx value further down for the bed level updating
               !
               li = 0
               lsedbed = lsedtot - nmudfrac
               do l = 1, lsedtot
                  !
                  ! bed load transport only for fractions with bedload component
                  !
                  if (.not. has_bedload(tratyp(l))) cycle
                  li = li + 1
                  !
                  if (morbnd(jb)%ibcmt(3) == lsedbed) then
                     call get_tau(ln(2, lm), taucurc, czc, jawaveswartdelwaq_local)
                     if (tausum2(1) > 0d0 .and. wu_mor(lm) > 0d0) then ! fix cutcell
                        rate = bc_sed_distribution(li) * taucurc**2 / wu_mor(lm) / tausum2(1)
                     else
                        rate = bc_mor_array(li)
                     end if
                  elseif (morbnd(jb)%ibcmt(3) == 2 * lsedbed) then
                     rate = bc_mor_array(li) + &
                        & alfa_dist * (bc_mor_array(li + lsedbed) - bc_mor_array(li))
                  end if
                  rate = alfa_mag * rate
                  !
                  if (icond == 4) then
                     !
                     ! transport including pores
                     !
                     rate = rate * cdryb(l)
                  else
                     !
                     ! transport excluding pores
                     !
                     rate = rate * rhosol(l)
                  end if
                  !
                  ! impose boundary condition
                  !
                  !                   if (idir_scalar == 1) then
                  e_sbn(lm, l) = rate
                  !                   else
                  !                      sbvv(nxmx, l) = rate
                  !                   endif
               end do ! l (sediment fraction)
            end do ! ib (boundary point)
         end if ! icond = 4 or 5 (boundary with transport condition)
      end do ! jb (open boundary)

   end subroutine fm_bed_boundary_conditions

   !> Compute change in bed level `dbodsd`
   subroutine fm_change_in_sediment_thickness(dtmor)
      use precision, only: dp

   !!
   !! Declarations
   !!

      use sediment_basics_module
      use m_flowgeom, only: bai_mor, ndxi, bl, wu, wu_mor, xz, yz, ndx
      use m_flow, only: kmx, s1, vol1
      use m_fm_erosed, only: dbodsd, lsedtot, cdryb, tratyp, e_sbn, sus, neglectentrainment, duneavalan, bed, bedupd, e_scrn, iflufflyr, kmxsed, sourf, sourse, mfluff, ndxi_mor
      use m_fm_erosed, only: nd => nd_mor, sedtyp, depfac, max_mud_sedtyp
      use m_sediment, only: avalflux, ssccum
      use m_flowtimes, only: dts, dnt
      use m_transport, only: fluxhortot, ised1, sinksetot, sinkftot
      use unstruc_files, only: mdia
      use m_get_kbot_ktop
      use m_get_Lbot_Ltop

      implicit none

   !!
   !! I/O
   !!

      real(kind=dp), intent(in) :: dtmor

   !!
   !! Local variables
   !!

      integer, parameter :: BEDCHANGEMESSMAX = 50

      logical :: bedload

      integer :: j, l, ii, il, nm, ll, lt, kb, kt, lb, lf, k
      integer :: bedchangemesscount
      integer :: lstart

      real(kind=dp) :: trndiv
      real(kind=dp) :: sedflx
      real(kind=dp) :: eroflx
      real(kind=dp) :: flux
      real(kind=dp) :: dhmax
      real(kind=dp) :: dsdnm
      real(kind=dp) :: h1
      real(kind=dp) :: sumflux
      real(kind=dp) :: thick1

   !!
   !! Execute
   !!

      lstart = ised1 - 1
      bedload = .false.

      !
      ! Update quantity of bottom sediment
      !
      dbodsd(:, :) = 0d0
      !
      ! compute change in bodsed (dbodsd)
      !
      bedchangemesscount = 0
      do l = 1, lsedtot
         bedload = tratyp(l) == TRA_BEDLOAD
         j = lstart + l ! constituent index
         !
         ! loop over internal (ndxi) nodes - don't update the boundary nodes
         !
         do nm = 1, Ndxi_mor
            trndiv = 0d0
            sedflx = 0d0
            eroflx = 0d0
            !FM1DIMP2DO: I do not like this, but I cannot think of a better way.
            !The added flownodes at junctions are after the boundary ghost nodes.
            !We have to skip the boundaries but loop over the added flownodes.
            if ((nm > ndxi) .and. (nm < ndx + 1)) then
               cycle
            end if
            if (sus /= 0d0 .and. .not. bedload) then
               if (neglectentrainment) then
                  !
                  ! mass balance based on transport fluxes only: entrainment and deposition
                  ! do not lead to erosion/sedimentation.
                  !
                  sumflux = 0d0
                  if (kmx > 0) then
                     do ii = 1, nd(nm)%lnx
                        LL = nd(nm)%ln(ii)
                        Lf = abs(LL)
                        call getLbotLtop(Lf, Lb, Lt)
                        if (Lt < Lb) cycle
                        flux = 0d0
                        do iL = Lb, Lt
                           flux = flux + fluxhortot(j, iL)
                        end do
                        !See: UNST-7371
                        call fm_sumflux(LL, sumflux, flux)
                     end do
                  else
                     do ii = 1, nd(nm)%lnx
                        LL = nd(nm)%ln(ii)
                        Lf = abs(LL)

                        flux = fluxhortot(j, Lf)
                        call fm_sumflux(LL, sumflux, flux)
                     end do
                  end if
                  trndiv = trndiv + sumflux * bai_mor(nm)
               else
                  !
                  ! mass balance includes entrainment and deposition
                  !
                  if (tratyp(l) == TRA_COMBINE) then
                     !
                     ! l runs from 1 to lsedtot, kmxsed is defined for 1:lsed
                     ! The first lsed fractions are the suspended fractions,
                     ! so this is correct
                     !
                     k = kmxsed(nm, l)
                  else
                     call getkbotktop(nm, kb, kt)
                     k = kb
                  end if
                  thick1 = vol1(k) * bai_mor(nm)
                  ! no fluff, everything to bed layer
                  if (iflufflyr == 0) then
                     sedflx = sinksetot(j, nm) * bai_mor(nm) + ssccum(l, nm) ! kg/s/m2
                  else
                     !
                     ! Update sedflx icw fluff layer
                     !
                     ! 1. update fluff layer mass
                     mfluff(l, nm) = mfluff(l, nm) + dts * (sinkftot(j, nm) * bai_mor(nm) - sourf(l, nm) * thick1)
                     !
                     ! 2. sand to bed layer
                     sedflx = sinksetot(j, nm) * bai_mor(nm)
                     !
                     if (sedtyp(l) == SEDTYP_SAND) then
                        sedflx = sedflx + ssccum(l, nm)
                     elseif (sedtyp(l) <= max_mud_sedtyp) then
                        ! 3. if silt/clay and drying, mass to fluff layer
                        if (iflufflyr == 1) then
                           mfluff(l, nm) = mfluff(l, nm) + ssccum(l, nm)
                        else ! iflufflyr == 2
                           mfluff(l, nm) = mfluff(l, nm) + (1.0_fp - depfac(l, nm)) * ssccum(l, nm)
                           sedflx = sedflx + depfac(l, nm) * ssccum(l, nm)
                        end if
                     end if
                  end if
                  ssccum(l, nm) = 0d0
                  eroflx = sourse(nm, l) * thick1 ! mass conservation, different from D3D
                  !
                  ! add suspended transport correction vector
                  !
                  sumflux = 0d0
                  do ii = 1, nd(nm)%lnx
                     LL = nd(nm)%ln(ii)
                     Lf = abs(LL)
                     flux = e_scrn(Lf, l) * wu(Lf)
                     call fm_sumflux(LL, sumflux, flux)
                  end do
                  trndiv = trndiv + sumflux * bai_mor(nm)
               end if
            end if
            if (bed /= 0.0d0) then
               sumflux = 0d0
               do ii = 1, nd(nm)%lnx
                  LL = nd(nm)%ln(ii)
                  Lf = abs(LL)
                  flux = e_sbn(Lf, l) * wu_mor(Lf)
                  call fm_sumflux(LL, sumflux, flux)
               end do
               trndiv = trndiv + sumflux * bai_mor(nm)
            end if
            !
            if (duneavalan) then ! take fluxes out of timestep restriction
               sumflux = 0d0 ! drawback: avalanching fluxes not included in total transports
               do ii = 1, nd(nm)%lnx
                  LL = nd(nm)%ln(ii)
                  Lf = abs(LL)
                  flux = avalflux(Lf, l) * wu_mor(Lf)
                  call fm_sumflux(LL, sumflux, flux)
               end do
               trndiv = trndiv + sumflux * bai_mor(nm)
            end if
            !
            dsdnm = (trndiv + sedflx - eroflx) * dtmor
            !
            ! Warn if bottom changes are very large,
            ! depth change NOT LIMITED
            !
            dhmax = 0.05d0
            h1 = max(0.01d0, s1(nm) - bl(nm))
            if (abs(dsdnm) > dhmax * h1 * cdryb(1) .and. bedupd) then
               !
               ! Only write bed change warning when bed updating is true
               ! (otherwise no problem)
               ! Limit the number of messages with BEDCHANGEMESSMAX
               !
               bedchangemesscount = bedchangemesscount + 1
               if (bedchangemesscount <= BEDCHANGEMESSMAX) then
                  write (mdia, '(a,f5.1,a,i0,a,i0,a,f10.0,a,f10.0)') &
                     & '*** WARNING Bed change exceeds ', dhmax * 100.0d0, ' % of waterdepth after ', int(dnt),  &
                     & ' timesteps, flow node = (', nm, ') at x=', xz(nm), ', y=', yz(nm)
               end if
            end if
            !
            ! Update dbodsd value at nm
            !
            dbodsd(l, nm) = dbodsd(l, nm) + dsdnm
         end do ! nm
      end do ! l

      if (bedchangemesscount > BEDCHANGEMESSMAX) then
         write (mdia, '(12x,a,i0,a)') 'Bed change messages skipped (more than ', BEDCHANGEMESSMAX, ')'
         write (mdia, '(12x,2(a,i0))') 'Total number of Bed change messages for timestep ', int(dnt), ' : ', bedchangemesscount
      end if

   end subroutine fm_change_in_sediment_thickness

   !> Redistribute erosion of wet cell next to dry cell to the dry cell
   !! to consider some sort of bank or beach erosion
   subroutine fm_dry_bed_erosion(dtmor)
      use precision, only: dp

   !!
   !! Declarations
   !!

      use m_flowgeom, only: bai_mor, bl, wu_mor, ba
      use m_flow, only: s1, hs
      use m_flowparameters, only: epshs
      use m_fm_erosed, only: lsedtot, kfsed, dbodsd, fixfac, frac, hmaxth, sedthr, thetsd, e_sbn
      use m_fm_erosed, only: ndxi => ndxi_mor
      use m_fm_erosed, only: nd => nd_mor
      use m_fm_erosed, only: ln => ln_mor

      implicit none

   !!
   !! I/O
   !!

      real(kind=dp), intent(in) :: dtmor

   !!
   !! Local variables
   !!

      integer :: l, nm, k1, k2, knb, ll, lf

      real(kind=dp) :: bamin
      real(kind=dp) :: dv
      real(kind=dp) :: thet
      real(kind=dp) :: totdbodsd
      real(kind=dp) :: totfixfrac

   !!
   !! Execute
   !!

      !
      ! Re-distribute erosion near dry and shallow points to allow erosion
      ! of dry banks
      !
      do nm = 1, ndxi
         !
         ! If this is a cell in which sediment processes are active then ...
         !
         if (kfsed(nm) /= 1 .or. (s1(nm) - bl(nm)) < epshs .or. thetsd(nm) <= 0) cycle ! check whether sufficient as condition
         !
         totdbodsd = 0d0
         do l = 1, lsedtot
            totdbodsd = totdbodsd + real(dbodsd(l, nm), hp)
         end do
         !
         ! If this is a cell where erosion is occuring (accretion is not
         ! distributed to dry points) then...
         !
         if (totdbodsd < 0d0) then
            !
            ! Note: contrary to the previous implementation, this new
            ! implementation erodes the sediment from nm and
            ! re-distributes the eroded volume based on the composition
            ! of the neighbouring cells, replenishing the sediment volume
            ! at grid point nm with sediment of a different composition
            ! than that what was eroded. This new implementation is mass
            ! conserving per fraction. Furthermore, re-distribution takes
            ! place only in case of net TOTAL erosion, i.e. not of
            ! individual fractions.
            !
            bamin = ba(nm)
            totfixfrac = 0d0
            !
            do L = 1, nd(nm)%lnx
               k1 = ln(1, abs(nd(nm)%ln(L))); k2 = ln(2, abs(nd(nm)%ln(L)))
               if (k2 == nm) then
                  knb = k1
               else
                  knb = k2
               end if
               !
               ! evaluate whether dry cell, and calculate totfixfac value for cell
               !
               if (kfsed(knb) == 0 .and. bl(knb) > bl(nm)) then
                  bamin = min(bamin, ba(knb))
                  do ll = 1, lsedtot
                     totfixfrac = totfixfrac + fixfac(knb, ll) * frac(knb, ll)
                  end do
               end if
            end do
            !
            ! Re-distribute THET % of erosion in nm to surrounding cells
            ! THETSD is a user-specified maximum value, range 0-1
            !
            if (totfixfrac > 1d-7) then
               !
               ! Compute local re-distribution factor THET
               !
               if (hmaxth > sedthr) then
                  thet = (hs(nm) - sedthr) / (hmaxth - sedthr) * thetsd(nm)
                  thet = min(thet, thetsd(nm))
               else
                  thet = thetsd(nm)
               end if
               !
               ! Combine some constant factors in variable THET
               ! Note: TOTDBODSD<0.0 and thus THET>0.0 !
               !
               thet = -bamin * totdbodsd * thet / totfixfrac
               !
               do ll = 1, lsedtot
                  !
                  ! update dbodsd values in this cell and surrounding cells
                  ! adjust bedload transport rates to include this erosion
                  ! process.
                  !
                  do L = 1, nd(nm)%lnx
                     k1 = ln(1, abs(nd(nm)%ln(L))); k2 = ln(2, abs(nd(nm)%ln(L)))
                     Lf = abs(nd(nm)%ln(L))
                     ! cutcells
                     if (wu_mor(Lf) == 0d0) cycle
                     !
                     if (k2 == nm) then
                        knb = k1
                     else
                        knb = k2
                     end if
                     if (kfsed(knb) == 0 .and. bl(knb) > bl(nm)) then
                        dv = thet * fixfac(knb, ll) * frac(knb, ll)
                        dbodsd(ll, knb) = dbodsd(ll, knb) - dv * bai_mor(knb)
                        dbodsd(ll, nm) = dbodsd(ll, nm) + dv * bai_mor(nm)
                        e_sbn(Lf, ll) = e_sbn(Lf, ll) + dv / (dtmor * wu_mor(Lf)) * sign(1d0, nd(nm)%ln(L) + 0d0)
                     end if
                  end do ! L
               end do ! ll
            end if ! totfixfrac > 1.0e-7
         end if ! totdbodsd < 0.0
      end do ! nm

   end subroutine fm_dry_bed_erosion

   !>Update `dbodsd` considering mormerge
   subroutine fm_apply_mormerge()

   !!
   !! Declarations
   !!

      use m_sediment, only: stmpar, mergebodsed, jamormergedtuser
      use m_flowtimes, only: time1, time_user
      use m_flowgeom, only: ndxi
      use m_flowparameters, only: eps10
      use m_fm_erosed, only: lsedtot, dbodsd
      use m_partitioninfo, only: jampi, my_rank, DFM_COMM_DFMWORLD
      use m_mormerge_mpi, only: update_mergebuffer

      implicit none

   !!
   !! Local variables
   !!

      logical :: jamerge

      integer :: ll, nm, ii

   !!
   !! Execute
   !!

      !
      ! Modifications for running parallel conditions (mormerge)
      !
      !FM1DIMP2DO: The 1D implicit solver does not yet support mormerge. This should be dealt with.
      !
      if (stmpar%morpar%multi) then
         jamerge = .false.
         if (jamormergedtuser > 0) then
            mergebodsed = mergebodsed + dbodsd
            dbodsd(:, :) = 0d0
            if (comparereal(time1, time_user, eps10) >= 0) then
               jamerge = .true.
            end if
         else
            mergebodsed = dbodsd
            dbodsd(:, :) = 0d0
            jamerge = .true.
         end if
         if (jamerge) then
            ii = 0
            do ll = 1, lsedtot
               do nm = 1, ndxi
                  ii = ii + 1
                  stmpar%morpar%mergebuf(ii) = real(mergebodsed(ll, nm), hp)
               end do
            end do
            call update_mergebuffer(stmpar%morpar%mergehandle, ndxi * lsedtot, stmpar%morpar%mergebuf, &
                                    jampi, my_rank, DFM_COMM_DFMWORLD)

            ii = 0
            do ll = 1, lsedtot
               do nm = 1, ndxi
                  ii = ii + 1
                  dbodsd(ll, nm) = real(stmpar%morpar%mergebuf(ii), fp)
               end do
            end do
            mergebodsed(:, :) = 0d0
         end if
      end if

   end subroutine fm_apply_mormerge

   !> Apply bed boundary condition
   subroutine fm_apply_bed_boundary_condition(dtmor, timhr)
      use precision, only: dp

   !!
   !! Declarations
   !!

      use Messagehandling
      use message_module, only: writemessages, write_error
      use morphology_data_module, only: bedbndtype
      use table_handles, only: handletype, gettabledata
      use m_sediment, only: stmpar
      use m_flow, only: u1
      use m_flowtimes, only: julrefdat
      use m_flowgeom, only: bl
      use m_fm_erosed, only: blchg, bc_mor_array

      implicit none

   !!
   !! I/O
   !!

      real(kind=dp), intent(in) :: dtmor
      real(kind=dp), intent(in) :: timhr

   !!
   !! Local variables
   !!

      integer :: nto, jb, ib, nm, nxmx, lm
      integer :: icond

      real(kind=dp) :: alfa_dist
      real(kind=dp) :: alfa_mag
      real(kind=dp) :: rate

      character(len=256) :: msg

      type(handletype), pointer :: bcmfile
      type(bedbndtype), dimension(:), pointer :: morbnd

   !!
   !! Allocate and initialize
   !!

      bcmfile => stmpar%morpar%bcmfile
      morbnd => stmpar%morpar%morbnd

   !!
   !! Execute
   !!

      !
      ! Bed boundary conditions
      !
      nto = size(morbnd, 1)
      do jb = 1, nto
         icond = morbnd(jb)%icond
         !
         ! In case of an open boundary with bed level condition
         ! described by time series: get data from table file
         !
         if (icond == 2 .or. icond == 3 .or. icond == 6 .or. icond == 7) then
            call gettabledata(bcmfile, morbnd(jb)%ibcmt(1), &
               & morbnd(jb)%ibcmt(2), morbnd(jb)%ibcmt(3), &
               & morbnd(jb)%ibcmt(4), bc_mor_array, &
               & timhr, julrefdat, msg)
            if (msg /= ' ') then
               call setmessage(LEVEL_FATAL, msg)
               return
            end if
         end if
         !
         ! Prepare loop over boundary points
         !
         do ib = 1, morbnd(jb)%npnt
            alfa_dist = morbnd(jb)%alfa_dist(ib)
            alfa_mag = morbnd(jb)%alfa_mag(ib)**2
            nm = morbnd(jb)%nm(ib)
            nxmx = morbnd(jb)%nxmx(ib)
            lm = morbnd(jb)%lm(ib)
            !
            ! Bed change in open boundary point
            ! Any boundary condition is changed into a "free bed level
            ! boundary" if the computed transport is directed outward.
            !
            ! Detect the case based on the value of nxmx. In case of a
            ! diagonal water level boundary, there will be two separate
            ! entries in the morbnd structure. The sum of alfa_mag(ib)**2
            ! will be equal to 1.
            !
            icond = morbnd(jb)%icond
            if (u1(lm) < 0d0) icond = 0 ! to do: 3d
            !
            select case (icond)
            case (0, 4, 5)
               !
               ! outflow or free boundary (0)
               ! or prescribed transport with pores (4)
               ! or prescribed transport without pores (5)
               !
               blchg(nm) = blchg(nm) + blchg(nxmx) * alfa_mag
            case (1)
               !
               ! fixed bed level: no update
               !
               ! blchg(nm) = blchg(nm) + 0.0 * alfa_mag
            case (2)
               !
               ! prescribed depth
               ! temporarily store "bed levels" in variable "rate"
               !
               if (morbnd(jb)%ibcmt(3) == 1) then
                  rate = bc_mor_array(1)
               elseif (morbnd(jb)%ibcmt(3) == 2) then
                  rate = bc_mor_array(1) + &
                     & alfa_dist * (bc_mor_array(2) - bc_mor_array(1))
               end if
               !
               blchg(nm) = blchg(nm) + (real(-bl(nm), fp) - rate) * alfa_mag
            case (3)
               !
               ! prescribed depth change rate
               !
               if (morbnd(jb)%ibcmt(3) == 1) then
                  rate = bc_mor_array(1)
               elseif (morbnd(jb)%ibcmt(3) == 2) then
                  rate = bc_mor_array(1) + &
                     & alfa_dist * (bc_mor_array(2) - bc_mor_array(1))
               end if
               !
               blchg(nm) = blchg(nm) - rate * alfa_mag * dtmor
            case (6)
               !
               ! prescribed bed level
               ! temporarily store "bed levels" in variable "rate"
               !
               if (morbnd(jb)%ibcmt(3) == 1) then
                  rate = bc_mor_array(1)
               elseif (morbnd(jb)%ibcmt(3) == 2) then
                  rate = bc_mor_array(1) + &
                     & alfa_dist * (bc_mor_array(2) - bc_mor_array(1))
               end if
               !
               blchg(nm) = blchg(nm) + (real(-bl(nm), fp) + rate) * alfa_mag
            case (7)
               !
               ! prescribed bed level change rate
               !
               if (morbnd(jb)%ibcmt(3) == 1) then
                  rate = bc_mor_array(1)
               elseif (morbnd(jb)%ibcmt(3) == 2) then
                  rate = bc_mor_array(1) + &
                     & alfa_dist * (bc_mor_array(2) - bc_mor_array(1))
               end if
               !
               blchg(nm) = blchg(nm) + rate * alfa_mag * dtmor
            end select
         end do ! ib (boundary point)
      end do ! jb (open boundary)

   end subroutine fm_apply_bed_boundary_condition

   !< Update concentrations in water column to conserve mass because of bottom update
   !! This needs to happen in work array sed, not constituents, because of copying back and forth later on
   subroutine fm_update_concentrations_after_bed_level_update()
      use precision, only: dp

      use m_flow, only: kmx, hs
      use m_flowgeom, only: ndx
      use m_transport, only: constituents, itra1, itran, isalt
      use m_sediment, m_sediment_sed => sed
      use m_fm_erosed, only: blchg
      use m_flowparameters, only: epshs, jasal
      use m_get_kbot_ktop

      implicit none

   !!
   !! Local variables
   !!

      integer :: k, ll, kb, kt, kk, itrac

      real(kind=dp) :: hsk
      real(kind=dp) :: ddp

   !!
   !! Execute
   !!

      if (kmx == 0) then
         do k = 1, ndx
            hsk = hs(k)
            ! After review, botcrit as a parameter is a really bad idea, as it causes concentration explosions if chosen poorly or blchg is high.
            ! Instead, allow bottom level changes up until 5% of the waterdepth to influence concentrations
            ! This is in line with the bed change messages above. Above that threshold, change the concentrations as if blchg==0.95hs
            if (hsk < epshs) cycle
            botcrit = 0.95 * hsk
            ddp = hsk / max(hsk - blchg(k), botcrit)
            do ll = 1, stmpar%lsedsus
               m_sediment_sed(ll, k) = m_sediment_sed(ll, k) * ddp
            end do !ll
            !
            if (jasal > 0) then
               constituents(isalt, k) = constituents(isalt, k) * ddp
            end if !jasal>0
            !
            if (ITRA1 > 0) then
               do itrac = ITRA1, ITRAN
                  constituents(itrac, k) = constituents(itrac, k) * ddp
               end do
            end if !ITRA1>0
         end do !k
      else !kmx==0
         do ll = 1, stmpar%lsedsus ! works for sigma only
            do k = 1, ndx
               hsk = hs(k)
               if (hsk < epshs) cycle
               botcrit = 0.95 * hsk
               ddp = hsk / max(hsk - blchg(k), botcrit)
               call getkbotktop(k, kb, kt)
               do kk = kb, kt
                  m_sediment_sed(ll, kk) = m_sediment_sed(ll, kk) * ddp
               end do !kk
            end do !k
         end do !ll
         !
         if (jasal > 0) then
            do k = 1, ndx
               hsk = hs(k)
               if (hsk < epshs) cycle
               botcrit = 0.95 * hsk
               call getkbotktop(k, kb, kt)
               do kk = kb, kt
                  constituents(isalt, kk) = constituents(isalt, kk) * hsk / max(hsk - blchg(k), botcrit)
               end do !kk
            end do !k
         end if !jasal>0
         !
         if (ITRA1 > 0) then
            do itrac = ITRA1, ITRAN
               do k = 1, ndx
                  hsk = hs(k)
                  if (hsk < epshs) cycle
                  botcrit = 0.95 * hsk
                  call getkbotktop(k, kb, kt)
                  do kk = kb, kt
                     constituents(itrac, kk) = constituents(itrac, kk) * hsk / max(hsk - blchg(k), botcrit)
                  end do !kk
               end do !k
            end do !itrac
         end if !ITRA1>0
         !
      end if !kmx==0

   end subroutine fm_update_concentrations_after_bed_level_update

   !> Compute total face normal suspended transport
   subroutine fm_total_face_normal_suspended_transport()

      use m_flowgeom, only: lnx, wu_mor
      use m_fm_erosed, only: e_ssn, lsed, e_scrn
      use m_transport, only: fluxhortot, ISED1
      use m_get_Lbot_Ltop

      implicit none

   !!
   !! Local variables
   !!

      integer :: ll, j, L, Lb, Lt, iL, lstart

   !!
   !! Execute
   !!
      lstart = ISED1 - 1
      do ll = 1, lsed
         j = lstart + ll ! constituent index
         do L = 1, lnx
            e_ssn(L, ll) = 0d0
            if (wu_mor(L) == 0d0) cycle
            call getLbotLtop(L, Lb, Lt)
            if (Lt < Lb) cycle
            do iL = Lb, Lt
               e_ssn(L, ll) = e_ssn(L, ll) + fluxhortot(j, iL) / max(wu_mor(L), 1d-3) ! timestep transports per layer [kg/s/m]
            end do
            e_ssn(L, ll) = e_ssn(L, ll) + e_scrn(L, ll) ! bottom layer correction
         end do
      end do

   end subroutine fm_total_face_normal_suspended_transport

   !> Summation of current-related and wave-related transports on links
   subroutine sum_current_wave_transport_links()

      use sediment_basics_module
      use m_fm_erosed, only: lsedtot, e_sbn, e_sbt, e_sbcn, e_sbwn, e_sswn, tratyp, e_sbct, e_sbwt, e_sswt
      use m_fm_erosed, only: lnx => lnx_mor

      implicit none

   !!
   !! Local variables
   !!

      integer :: nm, l

   !!
   !! Execute
   !!

      e_sbn(:, :) = 0d0
      e_sbt(:, :) = 0d0
      do l = 1, lsedtot
         if (has_bedload(tratyp(l))) then
            do nm = 1, lnx
               e_sbn(nm, l) = e_sbcn(nm, l) + e_sbwn(nm, l) + e_sswn(nm, l)
               e_sbt(nm, l) = e_sbct(nm, l) + e_sbwt(nm, l) + e_sswt(nm, l)
            end do
         end if
      end do

   end subroutine sum_current_wave_transport_links

   !> Conditionally exclude specific fractions from erosion and sedimentation
   !! exclude specific fractions if cmpupdfrac has been set.
   subroutine fm_exclude_cmpupdfrac()

      use m_fm_erosed, only: lsedtot, cmpupdfrac, stmpar, dbodsd

      implicit none

   !!
   !! Local variables
   !!

      integer :: l

      logical, pointer :: cmpupd

   !!
   !! Point
   !!

      cmpupd => stmpar%morpar%cmpupd

   !!
   !! Execute
   !!

      if (cmpupd) then
         do l = 1, lsedtot
            if (.not. cmpupdfrac(l)) then
               dbodsd(l, :) = 0.0_fp
            end if
         end do !l
      end if !cmpupd

   end subroutine fm_exclude_cmpupdfrac

   !> If there is no composition update, compute bed level changes
   !! without actually updating the bed composition. If there is
   !! composition update, bed level changes have already been determined
   subroutine fm_blchg_no_cmpupd()

      use m_flowgeom, only: ndx
      use m_fm_erosed, only: lsedtot, blchg, stmpar, dbodsd, cdryb

      implicit none

   !!
   !! Local variables
   !!

      integer :: ll, nm

      logical, pointer :: cmpupd

   !!
   !! Point
   !!

      cmpupd => stmpar%morpar%cmpupd

   !!
   !! Execute
   !!

      if (.not. cmpupd) then
         blchg(:) = 0d0
         do ll = 1, lsedtot
            do nm = 1, ndx
               blchg(nm) = blchg(nm) + dbodsd(ll, nm) / cdryb(ll)
            end do
         end do
      end if

   end subroutine fm_blchg_no_cmpupd

   !> Update bottom elevation
   subroutine fm_update_bed_level(dtmor)
      use precision, only: dp

   !!
   !! Declarations
   !!

      use Messagehandling
      use message_module, only: writemessages, write_error
      use m_flowgeom, only: ndx, bl_ave, bl, bl_ave0
      use m_fm_erosed, only: bedupd, blchg, stmpar
      use m_dad, only: dad_included
      use m_fm_update_crosssections, only: fm_update_crosssections
      use morphology_data_module, only: bedbndtype
      use fm_external_forcings_data, only: nopenbndsect
      use m_fm_dredge, only: fm_dredge

      implicit none

   !!
   !! I/O
   !!

      real(kind=dp), intent(in) :: dtmor

   !!
   !! Local variables
   !!

      logical :: error

      integer :: nm, jb, ib
      integer :: icond

      type(bedbndtype), dimension(:), pointer :: morbnd

   !!
   !! Point
   !!

      morbnd => stmpar%morpar%morbnd

   !!
   !! Execute
   !!

      if (bedupd) then
         !
         if (dad_included) then
            do nm = 1, ndx
               bl_ave(nm) = bl_ave(nm) + blchg(nm)
            end do
         end if
         !
         call fm_update_crosssections(blchg) ! blchg gets updated for 1d cross-sectional profiles in this routine
         !
         call fm_update_bl()
         !
         ! Free morpho boundaries get Neumann update
         !
         do jb = 1, nopenbndsect
            icond = morbnd(jb)%icond
            if (icond == 0) then
               do ib = 1, morbnd(jb)%npnt
                  bl(morbnd(jb)%nm(ib)) = bl(morbnd(jb)%nxmx(ib))
                  blchg(morbnd(jb)%nm(ib)) = blchg(morbnd(jb)%nxmx(ib)) ! needed below
               end do
            end if
         end do
         !
         call fm_update_concentrations_after_bed_level_update()
         !
         call fm_correct_water_level()
         !
         ! Remember erosion velocity for dilatancy
         !
         call fm_erosion_velocity(dtmor)
         !
         ! Dredging and Dumping
         !
         if (dad_included) then
            !
            bl_ave0 = bl_ave ! backup average bed level before dredging, needed to compute bed level change due to dredging
            !
            call fm_dredge(error)
            if (error) then
               call mess(LEVEL_FATAL, 'Error in fm_bott3d :: fm_dredge returned an error.')
               return
            end if
            !
            do nm = 1, ndx
               blchg(nm) = bl_ave(nm) - bl_ave0(nm) ! get average bed level change
            end do
            !
            call fm_update_crosssections(blchg) ! update 1d cross-sections after dredging (updates bl for 1D).
            !
            call fm_update_bl()
            !
         end if
      end if !bedupd

   end subroutine fm_update_bed_level

   !> Maximize and minimize water level
   subroutine fm_correct_water_level()

      use m_flow, only: s0, s1, hs
      use m_flowgeom, only: ndx, bl
      use m_fm_erosed, only: blchg
      use m_flowparameters, only: epshs

      implicit none

   !!
   !! Local variables
   !!

      integer :: nm

   !!
   !! Execute
   !!

      do nm = 1, ndx
         ! note: if kcs(nm)=0 then blchg(nm)=0.0
         ! should change to following test because blchg may be small
         ! due to truncation errors
         !
         s1(nm) = max(s1(nm), bl(nm))
         s0(nm) = max(s0(nm), bl(nm))
         !
         ! if dry cells are eroded then bring water level down to
         ! bed or maximum water level in surrounding wet cells
         ! (whichever is higher)
         !
         if (hs(nm) < epshs) then
            s1(nm) = s1(nm) + blchg(nm)
            s0(nm) = s0(nm) + blchg(nm)
         end if
      end do

   end subroutine fm_correct_water_level

   !> Update bed level based on bed level change
   subroutine fm_update_bl()

      use m_flowgeom, only: ndx, bl
      use m_fm_erosed, only: blchg

      implicit none

   !!
   !! Local variables
   !!

      integer :: nm

   !!
   !! Execute
   !!

      do nm = 1, Ndx
         bl(nm) = bl(nm) + blchg(nm)
      end do

   end subroutine fm_update_bl

   subroutine fm_erosion_velocity(dtmor)
      use precision, only: dp

      use m_flowgeom, only: ndx
      use m_fm_erosed, only: blchg, dzbdt

      implicit none

   !!
   !! I/O
   !!

      real(kind=dp), intent(in) :: dtmor

   !!
   !! Local variables
   !!

      integer :: nm

   !!
   !! Execute
   !!

      if (dtmor > 0d0) then
         do nm = 1, ndx
            dzbdt(nm) = blchg(nm) / dtmor
         end do
      else
         dzbdt(:) = 0d0
      end if

   end subroutine fm_erosion_velocity

   subroutine fm_sumflux(LL, sumflux, flux)
      use precision, only: dp

   !!
   !! Declarations
   !!

      implicit none

   !!
   !! I/O
   !!

      integer, intent(in) :: LL

      real(kind=dp), intent(in) :: flux
      real(kind=dp), intent(inout) :: sumflux

   !!
   !! Execute
   !!

      if (LL > 0) then ! inward
         sumflux = sumflux + flux
      else ! outward
         sumflux = sumflux - flux
      end if

   end subroutine fm_sumflux

end module m_fm_bott3d
