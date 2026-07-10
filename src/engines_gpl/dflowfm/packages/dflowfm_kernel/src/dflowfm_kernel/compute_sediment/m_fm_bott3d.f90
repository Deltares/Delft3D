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
      use m_flowparameters, only: EPS10, jawave
      use fm_external_forcings_data, only: nopenbndsect
      use m_flowtimes, only: dts, tstart_user, time1, tfac, ti_sed, ti_seds, handle_extra
      use unstruc_files, only: mdia
      use m_fm_erosed, only: mtd, tmor, bc_mor_array, lsedtot, e_ssn, bermslopetransport, duneavalan, &
                             bedw, bed, dbodsd, e_sbcn, e_sbct, e_sbwn, e_sswn, e_sswt, lsed, morfac, &
                             stmpar, susw, tcmp, sbcx, sbcy, morft, ucxq_mor, ucyq_mor, blchg, e_sbwt,&
                             hs_mor, hydrt, sbwx, sbwy, sscx, sscy, sswx, sswy, sedd50, taub, rhosol, &
                             hidexp
      use m_sediment, only: kcsmor
      use m_partitioninfo, only: jampi, ITYPE_Sall, update_ghosts
      use m_fm_morstatistics, only: morstats, morstatt0
      use m_tables, only: interpolate
      use Timers
      use m_reconstruct_sed_transports
      use m_waveconst
      use m_physcoef, only: ag, rhomean
      use m_bedform, only: bfmpar
      use m_physcoef, only: dynroughveg
      implicit none

   !!
   !! Local parameters
   !!

      real(kind=dp), parameter :: DAY2SEC = 86400.0_dp !< seconds in a day
      real(kind=dp), parameter :: H2SEC = 3600.0_dp !< seconds in an hour
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
      real(kind=dp) :: sbtot(ndx,stmpar%lsedtot)
      real(fp), dimension(:), pointer :: dunelength_tmp

   !!
   !! Point
   !!

      associate (&
         cmpupd => stmpar%morpar%cmpupd &
         )

      if (associated(bfmpar%dunelength)) then
         dunelength_tmp => bfmpar%dunelength
      else
         allocate(dunelength_tmp(1:ndx))
         dunelength_tmp = 1.0e10_fp
      end if

   !!
   !! Execute
   !!
      call timstrt('Bott3d_call   ', handle_extra(89))

      dtmor = dts * morfac
      error = .false.
      timhr = time1 / H2SEC
      blchg(:) = 0.0_dp
      e_ssn(:, :) = 0.0_dp

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
      if (bedw > 0.0_fp .and. jawave > NO_WAVES) then
         call fm_adjust_bedload(e_sbwn, e_sbwt, AVALANCHE_OFF, SLOPECOR_ON)
      end if
      !
      ! Sediment availability effects for
      ! wave-related suspended load transport
      !
      if (susw > 0.0_fp .and. jawave > NO_WAVES) then
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

         if (stmpar%morpar%moroutput%morstats .and. ti_sed > 0.0_dp) then
            call morstats(dbodsd, hs_mor, ucxq_mor, ucyq_mor, sbcx, sbcy, sbwx, sbwy, sscx, sscy, sswx, sswy)
         end if
         !
         ! Apply erosion and sedimentation to bookkeeping system
         !
         if (cmpupd) then
            !
            ! Diffuse fractions in active layer
            !
            if (stmpar%morlyr%settings%active_layer_diffusion > ACTIVE_LAYER_DIFFUSION_OFF) then
               call fm_diffusion_active_layer()
            end if
            !
            ! Determine new thickness of transport layer
            !
            call compthick()
            ! 
            ! Compute mobile fractions
            ! 
            if (stmpar%morlyr%settings%imobility > MOBILITY_OFF) then 
               call compmobile(stmpar%morlyr, ag, sedd50, taub, rhosol, rhomean, hidexp)
            endif    
            ! 
            if (stmpar%morlyr%settings%crslyr) then 
               !
               ! Compute average bed load transport in cel
               ! 
               sbtot(:, :) = hypot(sbcx(:, :), sbcy(:, :)) !sbtot(:, :) = sqrt(sbcx(:, :) * sbcx(:, :) + sbcy(:, :) * sbcy(:, :))
            endif 
            !
            ! Update layers and obtain the depth change
            !
            !See: UNST-7369
            if (updmorlyr(stmpar%morlyr, dbodsd, blchg, dunelength_tmp, sbtot, dtmor, mtd%messages) /= 0) then
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

      if (.not. associated(bfmpar%dunelength)) then
         deallocate(dunelength_tmp)
      end if

      if (time1 >= tstart_user + tmor * tfac) then
         !
         ! Increment morphological time
         ! Note: dtmor in seconds, morft in days!
         !
         morft = morft + dtmor / DAY2SEC
         if (morfac > 0.0_dp) then
            hydrt = hydrt + dts / DAY2SEC
         end if
         if (stmpar%morpar%moroutput%morstats) then
            if (comparereal(time1, ti_seds, EPS10) >= 0) then
               morstatt0 = morft
            end if
         end if
         !
         call fm_blchg_no_cmpupd() !Compute bed level changes without actually updating the bed composition
         !
         call fm_apply_bed_boundary_condition(dtmor, timhr)
         !
         if (dynroughveg > 0) then
            call determine_linkbased_cumblchg()
         end if

      else
         !
         ! if morphological computations haven't started yet
         !
         blchg(1:ndx) = 0.0_dp

      end if ! time1<tmor

      call fm_update_bed_level(dtmor)

      !
      call timstop(handle_extra(89))
   end associate
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
   !! Local parameters
   !!

      real(kind=dp), parameter :: AKSU_THRESHOLD_FOR_SCORR_COMPUTATION = 1e-10_dp !< Threshold for aksu value below which don't compute the near bed suspended transport

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

      e_scrn(:, :) = 0.0_dp
      e_scrt(:, :) = 0.0_dp

      !
      ! calculate corrections
      !
      if (sus /= 0.0_dp .and. l_suscor) then
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
                     if (wu_mor(Lx) == 0.0_dp) then
                        cycle
                     end if
                     ac1 = acL(Lx)
                     ac2 = 1_dp - ac1
                     k1 = ln(1, Lx)
                     k2 = ln(2, Lx)
                     call getLbotLtop(Lx, Lb, Lt)
                     if (Lt < Lb) then
                        cycle
                     end if
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
                        if (aksu < AKSU_THRESHOLD_FOR_SCORR_COMPUTATION) then
                           cycle
                        end if
                        !
                        ! work up through layers integrating transport flux below aksu
                        !
                        zktop = 0.0_dp
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
                           kf1 = ln(1, ka + 1)
                           kf2 = ln(2, ka + 1)
                           r1avg = ac1 * constituents(ll, kf1) + ac2 * constituents(ll, kf2)
                           !
                           ! If there is a significant concentration gradient, and significant
                           ! reference concentration
                           !
                           if (ceavg > r1avg * 1.1_dp .and. ceavg > 0.05_dp) then
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
                              apower = log(max(r1avg / ceavg, 1.0e-5_dp)) / log(z / aksu)
                              if (apower > -1.05_dp .and. apower <= -1.0_dp) then ! you have decide on the eq to -1.0
                                 apower = -1.05_dp
                              elseif (apower > -1.0_dp .and. apower < -0.95_dp) then
                                 apower = -0.95_dp
                              end if
                              apower = min(max(-10.0_dp, apower), 10.0_dp)
                              !
                              ! Compute the average concentration cavg between the reference
                              ! height a and the top of the current layer (bottom of layer above) z.
                              !           /zktop                           zktop                       zktop
                              ! cavg*dz = | c(z) dz = c_a/(-R+1)*(z/a)^(-R+1)*a | = c_a/(-R+1)*a^R*z^(-R+1) |
                              !          /a                                     a                           a
                              !
                              cavg1 = (ceavg / (apower + 1.0_dp)) * (1.0_dp / aksu)**apower
                              cavg2 = zktop**(apower + 1.0_dp) - aksu**(apower + 1.0_dp)
                              cavg = cavg1 * cavg2 ! kg/m3/m
                              !
                              ! The corresponding effective suspended load flux is
                              !
                              cflux = u1(ka) * cavg * dz * wu_mor(Lx)
                              !
                              ! Increment the correction by the part of the suspended load flux
                              ! that is in excess of the flux computed above, but never opposite.
                              !
                              if (fluxhortot(ll, ka) > 0.0_dp .and. cflux > 0.0_dp) then
                                 cumflux = cumflux + max(0.0_dp, fluxhortot(ll, ka) - cflux)
                              elseif (fluxhortot(ll, ka) < 0.0_dp .and. cflux < 0.0_fp) then
                                 cumflux = cumflux + min(fluxhortot(ll, ka) - cflux, 0.0_dp)
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
                        if (e_scrn(Lx, l) > 0.0_dp .and. Lx <= lnxi) then
                           e_scrn(Lx, l) = e_scrn(Lx, l) * fixfac(k1, l) ! outgoing (cumflux<0)
                        else
                           e_scrn(Lx, l) = e_scrn(Lx, l) * fixfac(k2, l) ! take inner point fixfac on bnd
                        end if
                     else
                        e_scrn(Lx, l) = 0.0_dp
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
   !!
   !! 
   !                                                
   !                  [] flow node                  
   !                  ^  flow link                  
   !                                                
   !                                           []   
   !                                          /3    
   !                                       2 /      
   !                                       ^/       
   !                                       /        
   !  discharge                     1     /         
   !  -------->       _______[]_____^___[]          
   !                         1          2\          
   !                                      \ 3       
   !                                       \^       
   !                                        \       
   !                                         \      
   !                                          \     
   !                                           []   
   !                                           4    
   !
   !Sediment transport is computed at flow nodes (i.e., cell centres). As there is a flow node at
   !junctions, sediment transport is computed at the junction point. In the sketch above, flow 
   !node 2. When upwinding, the sediment transport at flow links is computed. The transport at
   !flow node 1 is projected to flow link 1. The transport at flow node 2 is projected at flow links
   !2 and 3. With these transports at links it is already possible to compute the fluxes and the bed
   !level update. However, in one-dimensional simulations the grid is usually too coarse to properly 
   !capture the angle at the bifurcation. For this reason, we want to directly control the 
   !distribution of sediment from flow node 2 to flow links 2 and 3 by means of a nodal point relation.
   !To this end, the transports first computed at flow links 2 and 3 after upwinding (which come
   !from the transport at flow node 2) are summed to compute the total transport that exits the
   !flow node, and this is the transport redistributed by the nodal point relation.
   !
   subroutine apply_nodal_point_relation()

   !!
   !! Declarations
   !!

      use precision, only: dp
      use Messagehandling, only: LEVEL_FATAL, LEVEL_INFO, mess, errmsg
      use message_module, only: writemessages, write_error
      use unstruc_channel_flow, only: t_branch, t_node, nt_LinkNode
      use m_fm_erosed, only: lsedtot, e_sbcn, e_sbct
      use m_ini_noderel, only: get_noderel_idx
      use m_tables, only: interpolate
      use morphology_data_module, only: t_nodefraction, t_noderelation

      implicit none

   !!
   !! Local variables
   !!

      integer :: j, ised, L, kinod

      integer :: n_junctions !< Number of junctions in the domain (i.e., number of geometry nodes with more than two connections)
      integer, dimension(:), allocatable :: idx_junctions !< Indices of the geometry nodes which are junctions [n_junctions]
      real(kind=dp), dimension(:,:), allocatable :: water_discharge_out !< Water discharge out of each branch for a junction [n_junctions,maxnumberofconnections]
      real(kind=dp), dimension(:), allocatable :: total_width_out ![n_junctions]
      real(kind=dp), dimension(:,:), allocatable :: width_out ![n_junctions,maxnumberofconnections]
      integer, dimension (:,:), allocatable :: links_out ![n_junctions,maxnumberofconnections]
      integer, dimension (:,:), allocatable :: link_dir_out ![n_junctions,link]
      integer, dimension (:), allocatable :: n_links_out ![n_junctions]
      integer, dimension (:), allocatable :: n_links_in ![n_junctions]
      integer, dimension (:,:), allocatable :: links_in ![n_junctions]
      integer, dimension (:), allocatable :: flownode_junction ![n_junctions]
      real(fp), dimension(:, :), allocatable :: total_sediment_transport_out !< sum of incoming sediment transport at 1d node                                                      
      real(fp), dimension(:), allocatable :: total_water_discharge_out !< sum of outgoing discharge at 1d node
      real(kind=dp), dimension(:,:), allocatable :: width_in !< Width at incoming branches [n_junctions,maxnumberofconnections]
      real(kind=dp), dimension(:,:), allocatable :: u_in !< Velocity at incoming branches [n_junctions,maxnumberofconnections]
      real(kind=dp), dimension(:,:), allocatable :: q_main_in !< Main-channel discharge at incoming branches [n_junctions,maxnumberofconnections]
      real(kind=dp), dimension(:,:), allocatable :: u_to_main_in !< Velocity conversion factor at incoming branches [n_junctions,maxnumberofconnections]
      real(kind=dp), dimension(:,:), allocatable :: u_out !< Velocity at outgoing branches [n_junctions,maxnumberofconnections]
      real(kind=dp), dimension(:,:), allocatable :: q_main_out !< Main-channel discharge at outgoing branches [n_junctions,maxnumberofconnections]
      real(kind=dp), dimension(:,:), allocatable :: u_to_main_out !< Velocity conversion factor at outgoing branches [n_junctions,maxnumberofconnections]
      real(kind=dp), dimension(:,:), allocatable :: bl_out !< Downstream bottom level at outgoing branches [n_junctions,maxnumberofconnections]
      real(kind=dp) :: faccheck

      type(t_noderelation), pointer :: pNodRel
      
      integer :: number_prints_switch_nodal_point_relation
      integer, parameter :: MAX_NUMBER_PRINTS_SWITCH_NODAL_POINT_RELATION = 10
   !!
   !! Execute
   !!
      call nodal_point_relation_data( & 
         total_water_discharge_out, total_width_out, total_sediment_transport_out,idx_junctions,& !output
         n_junctions,n_links_out,links_out,link_dir_out,width_out,water_discharge_out,flownode_junction,& !output
         n_links_in,links_in,width_in,u_in,q_main_in,u_to_main_in,u_out,q_main_out,u_to_main_out,bl_out) !output
      !
      ! Determining sediment redistribution
      !
      ! loop over sediment fractions
      do ised = 1, lsedtot

         do kinod = 1, n_junctions
             
            call get_nodal_point_relation_parameters(&
                pNodRel, & !output
                flownode_junction,n_links_out,n_links_in,links_in,ised,kinod) !input
            
            facCheck = 0._dp
            
            do j = 1, n_links_out(kinod)
               L = links_out(kinod,j) 
   
               if (total_water_discharge_out(kinod) > 0.0_fp) then
               
                  if (pNodRel%Method == 'function') then
                     call nodal_point_relation_function(&
                         facCheck,e_sbcn(L, ised),& !output
                         pNodRel,link_dir_out,width_out,total_width_out,water_discharge_out,& !input
                         total_water_discharge_out,total_sediment_transport_out,kinod,j,ised) !input  
                     e_sbct(L, ised) = 0.0                     
                  elseif (pNodRel%Method == 'table') then
                     call nodal_point_relation_table(&
                        facCheck,e_sbcn(L, ised),& !output
                        pNodRel,links_out,link_dir_out,width_out,water_discharge_out,& !input
                        total_water_discharge_out,total_sediment_transport_out,kinod,j,ised) !input      
                     e_sbct(L, ised) = 0.0                     
                  elseif (pNodRel%Method == 'bollapittaluga') then
                      if (n_links_out(kinod) /= 2 .OR. n_links_in(kinod) /= 1 .and. number_prints_switch_nodal_point_relation <= MAX_NUMBER_PRINTS_SWITCH_NODAL_POINT_RELATION) then
                         number_prints_switch_nodal_point_relation = number_prints_switch_nodal_point_relation + 1
                         call mess(LEVEL_INFO, 'There must be 2 out branches and 1 in branch to use the nodal point relation `BollaPittaluga`. Now there are:')
                         call mess(LEVEL_INFO, 'Out branches: ',n_links_out(kinod))
                         call mess(LEVEL_INFO, ' In branches: ',n_links_in(kinod))
                         call mess(LEVEL_INFO, 'Switching to `function`.')
                         if (number_prints_switch_nodal_point_relation == MAX_NUMBER_PRINTS_SWITCH_NODAL_POINT_RELATION) then
                            call mess(LEVEL_INFO, 'Maximum number of prints reached for nodal point relation warning. No more warnings will be printed.')
                         end if
                         
                         call nodal_point_relation_function(&
                           facCheck,e_sbcn(L, ised),& !output
                           pNodRel,link_dir_out,width_out,total_width_out,water_discharge_out,& !input
                           total_water_discharge_out,total_sediment_transport_out,kinod,j,ised) !input  
                      else 
                         call nodal_point_relation_BollaPittaluga(&
                         e_sbcn(L,ised), & !output
                         pNodRel,total_sediment_transport_out(kinod, ised),links_out(kinod,:),& !input
                         width_in(kinod,:),width_out(kinod,:),u_in(kinod,:),q_main_in(kinod,:),u_to_main_in(kinod,:),& !input
                         u_out(kinod,:),q_main_out(kinod,:),u_to_main_out(kinod,:),bl_out(kinod,:),ised) !input
                      endif          
                      e_sbct(L, ised) = 0.0       
                  else
                     write (errmsg, '(a,a)') 'Unknown Nodal Point Relation Method Specified at node: ' // trim(pNodRel%node)
                     call mess(LEVEL_FATAL, errmsg)
                  end if
               
               else
                  e_sbcn(L, ised) = 0.0_fp
                  e_sbct(L, ised) = 0.0
               end if
      
            end do ! Branches
   
            !
            ! Correct Total Outflow
            !
            if ((facCheck /= 1.0_fp) .and. (facCheck > 0.0_fp)) then
               ! loop over branches and correct redistribution of incoming sediment
               do j = 1, n_links_out(kinod)
                  L = links_out(kinod,j) 
                  e_sbcn(L, ised) = e_sbcn(L, ised) / facCheck
               end do ! Branches
            end if !`facCheck`
            
         end do ! Nodes
      end do ! Fractions

   end subroutine apply_nodal_point_relation

   !> Apply morphodynamic boundary condition on bed level
   subroutine fm_bed_boundary_conditions(timhr)
      use precision, only: dp
      use m_waveconst
   !!
   !! Declarations
   !!

      use Messagehandling
      use message_module, only: writemessages, write_error
      use sediment_basics_module
      use m_flowparameters, only: flow_without_waves, jawaveswartdelwaq
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

      if (flow_without_waves) then
         jawaveswartdelwaq_local = WAVE_WAQ_SHEAR_STRESS_HYD
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
            tausum2(1) = 0.0_dp
            do ib = 1, morbnd(jb)%npnt
               lm = morbnd(jb)%lm(ib)
               k2 = morbnd(jb)%nxmx(ib)
               if (jampi == 1) then
                  if (.not. (idomain(k2) == my_rank)) then
                     cycle ! internal cells at boundary are in the same domain as the link
                  end if
               end if
               if (u1(lm) < 0.0_dp) then
                  cycle
               end if
               call get_tau(k2, taucurc, czc, jawaveswartdelwaq_local)
               tausum2(1) = tausum2(1) + taucurc**2 ! sum of the shear stress squared
            end do ! the distribution of bedload is scaled with square stress
            ! for avoiding instability on BC resulting from uniform bedload
            ! in combination with non-uniform cells.
            li = 0
            do l = 1, lsedtot
               sbsum = 0.0_dp
               !
               ! bed load transport only for fractions with bedload component
               !
               if (.not. has_bedload(tratyp(l))) then
                  cycle
               end if
               li = li + 1
               !
               do ib = 1, morbnd(jb)%npnt
                  lm = morbnd(jb)%lm(ib)
                  k2 = morbnd(jb)%nxmx(ib)
                  if (jampi == 1) then
                     if (.not. (idomain(k2) == my_rank)) then
                        cycle
                     end if
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
               if (u1(lm) < 0.0_dp) then
                  cycle ! check based on depth averaged velocity value
               end if
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
                  if (.not. has_bedload(tratyp(l))) then
                     cycle
                  end if
                  li = li + 1
                  !
                  if (morbnd(jb)%ibcmt(3) == lsedbed) then
                     call get_tau(ln(2, lm), taucurc, czc, jawaveswartdelwaq_local)
                     if (tausum2(1) > 0.0_dp .and. wu_mor(lm) > 0.0_dp) then ! fix cutcell
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
      use m_flowgeom, only: bai_mor, ndxi, bl, wu, wu_mor, xz, yz
      use m_flow, only: kmx, s1, vol1
      use m_fm_erosed, only: dbodsd, lsedtot, cdryb, tratyp, e_sbn, sus, neglectentrainment, duneavalan, bed, bedupd, e_scrn, iflufflyr, kmxsed, sourf, sourse, mfluff, ndxi_mor
      use m_fm_erosed, only: nd => nd_mor, sedtyp, depfac, max_mud_sedtyp, ndx => ndx_mor
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
      dbodsd(:, :) = 0.0_dp
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
            trndiv = 0.0_dp
            sedflx = 0.0_dp
            eroflx = 0.0_dp
            !FM1DIMP2DO: I do not like this, but I cannot think of a better way.
            !The added flownodes at junctions are after the boundary ghost nodes.
            !We have to skip the boundaries but loop over the added flownodes.
            if ((nm > ndxi) .and. (nm < ndx + 1)) then
               cycle
            end if
            if (sus /= 0.0_dp .and. .not. bedload) then
               if (neglectentrainment) then
                  !
                  ! mass balance based on transport fluxes only: entrainment and deposition
                  ! do not lead to erosion/sedimentation.
                  !
                  sumflux = 0.0_dp
                  if (kmx > 0) then
                     do ii = 1, nd(nm)%lnx
                        LL = nd(nm)%ln(ii)
                        Lf = abs(LL)
                        call getLbotLtop(Lf, Lb, Lt)
                        if (Lt < Lb) then
                           cycle
                        end if
                        flux = 0.0_dp
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
                     ! 3. in case of drying cell, assign mass to the appropriate layer (fluff/bed)
                     if (ssccum(l, nm) > 0.0_fp) then
                        if (sedtyp(l) <= max_mud_sedtyp) then
                           ! if silt/clay some mass may go to fluff layer
                           if (iflufflyr == 1 .or. mfluff(l, nm) < 0.0_fp) then
                              ! all mass to fluff layer
                              mfluff(l, nm) = mfluff(l, nm) + ssccum(l, nm) * dts
                           else ! iflufflyr == 2
                              ! part to fluff layer, part to bed layer
                              mfluff(l, nm) = mfluff(l, nm) + (1.0_fp - depfac(l, nm)) * ssccum(l, nm) * dts
                              sedflx = sedflx + depfac(l, nm) * ssccum(l, nm)
                           end if
                        else
                           ! for sand all mass to bed layer
                           sedflx = sedflx + ssccum(l, nm)
                        end if
                     end if
                  end if
                  ssccum(l, nm) = 0.0_dp
                  eroflx = sourse(nm, l) * thick1 ! mass conservation, different from D3D
                  !
                  ! add suspended transport correction vector
                  !
                  sumflux = 0.0_dp
                  do ii = 1, nd(nm)%lnx
                     LL = nd(nm)%ln(ii)
                     Lf = abs(LL)
                     flux = e_scrn(Lf, l) * wu(Lf)
                     call fm_sumflux(LL, sumflux, flux)
                  end do
                  trndiv = trndiv + sumflux * bai_mor(nm)
               end if
            end if
            if (bed /= 0.0_dp) then
               sumflux = 0.0_dp
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
               sumflux = 0.0_dp ! drawback: avalanching fluxes not included in total transports
               do ii = 1, nd(nm)%lnx
                  LL = nd(nm)%ln(ii)
                  Lf = abs(LL)
                  flux = avalflux(Lf, l) * wu_mor(Lf)   ! kg m-1 s-1 m
                  call fm_sumflux(LL, sumflux, flux)
               end do
               trndiv = trndiv + sumflux * bai_mor(nm)  ! kg s-1
            end if
            !
            dsdnm = (trndiv + sedflx - eroflx) * dtmor  ! kg m-2
            !
            ! Warn if bottom changes are very large,
            ! depth change NOT LIMITED
            !
            dhmax = 0.05_dp
            h1 = max(0.01_dp, s1(nm) - bl(nm))
            if (abs(dsdnm) > dhmax * h1 * cdryb(1) .and. bedupd) then
               !
               ! Only write bed change warning when bed updating is true
               ! (otherwise no problem)
               ! Limit the number of messages with BEDCHANGEMESSMAX
               !
               bedchangemesscount = bedchangemesscount + 1
               if (bedchangemesscount <= BEDCHANGEMESSMAX) then
                  write (mdia, '(a,f5.1,a,i0,a,i0,a,f10.0,a,f10.0)') &
                     & '*** WARNING Bed change exceeds ', dhmax * 100.0_dp, ' % of waterdepth after ', int(dnt),  &
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
         if (kfsed(nm) /= 1 .or. (s1(nm) - bl(nm)) <= epshs .or. thetsd(nm) <= 0) then
            cycle ! check whether sufficient as condition
         end if
         !
         totdbodsd = 0.0_dp
         do l = 1, lsedtot
            totdbodsd = totdbodsd + real(dbodsd(l, nm), hp)
         end do
         !
         ! If this is a cell where erosion is occuring (accretion is not
         ! distributed to dry points) then...
         !
         if (totdbodsd < 0.0_dp) then
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
            totfixfrac = 0.0_dp
            !
            do L = 1, nd(nm)%lnx
               k1 = ln(1, abs(nd(nm)%ln(L)))
               k2 = ln(2, abs(nd(nm)%ln(L)))
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
            if (totfixfrac > 1.0e-7_dp) then
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
                     k1 = ln(1, abs(nd(nm)%ln(L)))
                     k2 = ln(2, abs(nd(nm)%ln(L)))
                     Lf = abs(nd(nm)%ln(L))
                     ! cutcells
                     if (wu_mor(Lf) == 0.0_dp) then
                        cycle
                     end if
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
                        e_sbn(Lf, ll) = e_sbn(Lf, ll) + dv / (dtmor * wu_mor(Lf)) * sign(1.0_dp, nd(nm)%ln(L) + 0.0_dp)
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
      use m_flowparameters, only: EPS10
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
            dbodsd(:, :) = 0.0_dp
            if (comparereal(time1, time_user, EPS10) >= 0) then
               jamerge = .true.
            end if
         else
            mergebodsed = dbodsd
            dbodsd(:, :) = 0.0_dp
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
            mergebodsed(:, :) = 0.0_dp
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
            if (u1(lm) < 0.0_dp) then
               icond = 0 ! to do: 3d
            end if
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
   subroutine fm_update_concentrations_after_bed_level_update()
      use precision, only: dp

      use m_flow, only: kmx, hs
      use m_flowgeom, only: ndx
      use m_transport, only: constituents, itra1, itran, isalt, ised1
      use m_sediment, only: botcrit, stmpar
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
            if (hsk <= epshs) then
               cycle
            end if
            botcrit = 0.95 * hsk
            ddp = hsk / max(hsk - blchg(k), botcrit)
            do ll = 1, stmpar%lsedsus
               constituents(ised1 + ll - 1, k) = constituents(ised1 + ll - 1, k) * ddp
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
      else !kmx>0
         do ll = 1, stmpar%lsedsus ! works for sigma only
            do k = 1, ndx
               hsk = hs(k)
               if (hsk <= epshs) then
                  cycle
               end if
               botcrit = 0.95 * hsk
               ddp = hsk / max(hsk - blchg(k), botcrit)
               call getkbotktop(k, kb, kt)
               do kk = kb, kt
                  constituents(ised1 + ll - 1, kk) = constituents(ised1 + ll - 1, kk) * ddp
               end do !kk
            end do !k
         end do !ll
         !
         if (jasal > 0) then
            do k = 1, ndx
               hsk = hs(k)
               if (hsk <= epshs) then
                  cycle
               end if
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
                  if (hsk <= epshs) then
                     cycle
                  end if
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
            e_ssn(L, ll) = 0.0_dp
            if (wu_mor(L) == 0.0_dp) then
               cycle
            end if
            call getLbotLtop(L, Lb, Lt)
            if (Lt < Lb) then
               cycle
            end if
            do iL = Lb, Lt
               e_ssn(L, ll) = e_ssn(L, ll) + fluxhortot(j, iL) / max(wu_mor(L), 1.0e-3_dp) ! timestep transports per layer [kg/s/m]
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

      e_sbn(:, :) = 0.0_dp
      e_sbt(:, :) = 0.0_dp
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
         blchg(:) = 0.0_dp
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
         if (hs(nm) <= epshs) then
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

      if (dtmor > 0.0_dp) then
         do nm = 1, ndx
            dzbdt(nm) = blchg(nm) / dtmor
         end do
      else
         dzbdt(:) = 0.0_dp
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

   !> Apply diffusion to sediment mass in the active layer.
   subroutine fm_diffusion_active_layer()
      use precision, only: dp
      use m_fm_advec_diff_2d, only: fm_advec_diff_2d
      use m_fm_erosed, only: lsedtot, tratyp, stmpar
      use m_flowgeom, only: lnx, ndx
      use sediment_basics_module, only: has_bedload
      use m_alloc, only: realloc
      use m_sediment, only: aldiff_links
      use m_turbulence, only: BACKGROUND_DIFFUSION_OFF

      real(kind=dp), dimension(1), parameter :: ACTIVE_LAYER_BACKGROUND_DIFFUSION_FACTOR = [BACKGROUND_DIFFUSION_OFF] !< background diffusion factor [-]. It cannot be a `parameter` because it is `inout` in `comp_fluxhor3D` because it is optional.
      integer, parameter :: LIMITER_TYPE = 4 !< It should be made equal to a parameter inside, for instance, `m_flowparameters`.
   !!
   !! I/O
   !!

   !!
   !! Local variables
   !!
      real(kind=dp), dimension(:), allocatable :: uadv
      real(kind=dp), dimension(:), allocatable :: qadv
      real(kind=dp), dimension(:), allocatable :: sour
      real(kind=dp), dimension(:), allocatable :: sink

      integer :: l
      integer :: ierror

   !!
   !! Execute
   !!

      ierror = 0

      call realloc(uadv, lnx, keepExisting=.false., fill=0.0_dp)
      call realloc(qadv, lnx, keepExisting=.false., fill=0.0_dp)
      call realloc(sour, ndx, keepExisting=.false., fill=0.0_dp)
      call realloc(sink, ndx, keepExisting=.false., fill=0.0_dp)

      do l = 1, lsedtot
         if (has_bedload(tratyp(l))) then
            call fm_advec_diff_2d(stmpar%morlyr%state%msed(l, 1, :), uadv, qadv, sour, sink, aldiff_links, ACTIVE_LAYER_BACKGROUND_DIFFUSION_FACTOR, LIMITER_TYPE, ierror)
         end if
      end do

   end subroutine fm_diffusion_active_layer

   subroutine determine_linkbased_cumblchg()
      use m_sediment, only: cumes
      use m_fm_erosed, only: blchg
      use m_flowgeom, only: lnx, ln, acl

      implicit none

      integer :: L, k1, k2
      real(kind=dp) :: ac1, ac2

      do L = 1, lnx
         k1 = ln(1, L)
         k2 = ln(2, L)
         ac1 = acl(L)
         ac2 = 1.0_dp - ac1
         cumes(L) = cumes(L) + ac1 * (blchg(k1)) + ac2 * (blchg(k2))
      end do

   end subroutine determine_linkbased_cumblchg

   !> Apply the nodal point relation by Bolla and Pittaluga et al. (2003) to compute the sediment transport 
   !rate at the node of a bifurcation. The relation is applied at the junction flownode of a bifurcation, 
   !which has one incoming and two outgoing branches. The sediment transport rate at the node is computed 
   !based on the sediment transport rates and widths of the three branches, as well as the bed levels of the
   !nodes downstream of the outgoing branches. The relation also requires a parameter `alpha_BP` which is 
   !provided in the input file and can be set by the user.
   !
   subroutine nodal_point_relation_BollaPittaluga(&
      sq_sb,&
      pNodRel_in,Q_sa,links_out_kinod,&
      width_in_kinod,width_out_kinod,u_in_kinod,q_main_in_kinod,u_to_main_in_kinod,&
      u_out_kinod,q_main_out_kinod,u_to_main_out_kinod,bl_out_kinod,ised)

   use precision, only: dp
   use morphology_data_module, only: t_noderelation
   use m_fm_adjust_bedload, only: compute_ftheta
   
   implicit none

   ! Output
   real(kind=dp), intent(out) :: sq_sb
   
   ! Input
   type(t_noderelation), target, intent(in) :: pNodRel_in !< Nodal-point relation settings for this junction and sediment fraction.
   real(kind=dp), intent(in) :: Q_sa !< Total incoming sediment transport to redistribute over the outgoing branches.
   integer, dimension(:), intent(in) :: links_out_kinod !< Outgoing link indices for this junction.
   real(kind=dp), dimension(:), intent(in) :: width_in_kinod !< Widths of the incoming branches at this junction.
   real(kind=dp), dimension(:), intent(in) :: width_out_kinod !< Widths of the outgoing branches at this junction.
   real(kind=dp), dimension(:), intent(in) :: u_in_kinod !< Velocities on the incoming branches.
   real(kind=dp), dimension(:), intent(in) :: q_main_in_kinod !< Main-channel discharges on the incoming branches.
   real(kind=dp), dimension(:), intent(in) :: u_to_main_in_kinod !< Factors converting incoming-branch velocity to main-channel velocity.
   real(kind=dp), dimension(:), intent(in) :: u_out_kinod !< Velocities on the outgoing branches.
   real(kind=dp), dimension(:), intent(in) :: q_main_out_kinod !< Main-channel discharges on the outgoing branches.
   real(kind=dp), dimension(:), intent(in) :: u_to_main_out_kinod !< Factors converting outgoing-branch velocity to main-channel velocity.
   real(kind=dp), dimension(:), intent(in) :: bl_out_kinod !< Bed levels at the downstream nodes of the outgoing branches.
   integer, intent(in) :: ised !< Sediment-fraction index.

   ! Local variables

   ! `_a` = incoming branch
   ! `_b` = first outgoing branch
   ! `_c` = second outgoing branch
   ! `_y` = transverse direction between the two outgoing branches
   real(kind=dp) :: dbl_dy !< transverse bed slope between the two outgoing branches
   real(kind=dp) :: B_a, B_b, B_c !< widths of the incoming branch, and the two outgoing branches, respectively
   real(kind=dp) :: D_a, D_b, D_c, D_abc !< flow depths of the incoming branch, and the two outgoing branches, and a representative flow depth for the node, respectively
   real(kind=dp) :: Q_a, Q_b, Q_c, Q_y !< discharges of the incoming branch, and the two outgoing branches, and the transverse discharge respectively
   real(kind=dp) :: Q_sy, Q_sb !< sediment transport rates in the transverse direction and at the outgoing branch of interest, respectively
   real(kind=dp) :: sq_sa, sq_sy !< specific sediment transport
   real(kind=dp) :: L_a !< representative length scale for the node, taken as `alpha_BP` times the width of the incoming branch
   real(kind=dp) :: u, v !< flow velocity in the incoming branch, and transverse flow velocity at the node, respectively
   real(kind=dp) :: ftheta !< correction factor for the transverse sediment transport, which accounts for the effect of bed slope on sediment transport. 
   
   type(t_noderelation), pointer :: pNodRel

   real(kind=dp), parameter :: U_THRESH = 1.0e-5_dp ! threshold velocity for computing `D` to avoid numerical issues when `u` is close to zero. 
   real(kind=dp), parameter :: Q_THRESH = 1.0e-5_dp ! threshold dischare for computing `D` to avoid numerical issues when `q` is close to zero. 
                                                    !    Note that the problem is that if `q` is zero, `D` is zero and `v` is infinite. Hence, 
                                                    !    we only apply the threshold when computing `D`.
   
   pNodRel => pNodRel_in

   !Equations as in: 
   !Chavarrias, V. (2026) "Implementation of Bolla Pittaluga et al. (2003) nodal point relation: Review of the relation", Deltares internal memo.

   dbl_dy = (bl_out_kinod(1) - bl_out_kinod(2)) / ((width_out_kinod(1) + width_out_kinod(2)) / 2.0_dp) !Equation (18). In Bolla-Pittaluga et al. (2003): "The transverse bed slope is calculated in terms of the difference between bed elevations at the inlet of channels b and c".
   
   B_a = width_in_kinod(1)
   B_b = width_out_kinod(1)
   B_c = width_out_kinod(2)
   
   !Flow depth. `s1-bl` could be used, but we stay closer to the original formulation of Bolla and Pittaluga (2003) by using `q/(u*B)` 
   !Note that their cross-section is rectangular. We take values from the main channel. 
   D_a=max(q_main_in_kinod(1),Q_THRESH)/max(u_in_kinod(1) * u_to_main_in_kinod(1),U_THRESH)/B_a
   D_b=max(q_main_out_kinod(1),Q_THRESH)/max(u_out_kinod(1) * u_to_main_out_kinod(1),U_THRESH)/B_b
   D_c=max(q_main_out_kinod(2),Q_THRESH)/max(u_out_kinod(2) * u_to_main_out_kinod(2),U_THRESH)/B_c
   
   !Total discharge. We take values from the main channel.
   Q_a=q_main_in_kinod(1)
   Q_b=q_main_out_kinod(1)
   Q_c=q_main_out_kinod(2)
      
   L_a=pNodRel%alpha_BP*B_a
   
   !u=Q_a/D_a/B_a
   u = max(u_in_kinod(1) * u_to_main_in_kinod(1),U_THRESH) !we later divide by `u`
    

   Q_y=Q_b-Q_a*(B_b/(B_b+B_c)) !Equation (13) (rework of Equation (16) in Bolla-Pittaluga et al. (2003)).
   D_abc=0.5*((D_b+D_c)/2+D_a) !Equation (12) (Equation (17) in Bolla-Pittaluga et al. (2003)).
   v=Q_y/D_abc/L_a !Equation (11) (part of Equation (15) in Bolla-Pittalugaet al. (2003)).
   
   call compute_ftheta(ftheta,ised,links_out_kinod(1))
   
   sq_sa=Q_sa/B_a !make per unit width.
   sq_sy=sq_sa*(v/u-dbl_dy/ftheta) !Equation (1) (Equation (15) in Bolla-Pittaluga et al. (2003)).
   Q_sy=sq_sy*L_a !make total. 
   
   Q_sb=Q_sa+Q_sy !Equation (16) (implicit in Bolla-Pittaluga et al. (2003)).
   sq_sb=Q_sb/B_b !make per unit width.

   end subroutine nodal_point_relation_BollaPittaluga

   subroutine nodal_point_relation_data( &
   total_water_discharge_out, total_width_out, total_sediment_transport_out,idx_junctions,n_junctions,n_links_out,links_out,link_dir_out,width_out,water_discharge_out,flownode_junction,n_links_in,links_in,&
   width_in,u_in,q_main_in,u_to_main_in,u_out,q_main_out,u_to_main_out,bl_out)

   !Modules
   use precision, only: dp
   use unstruc_channel_flow, only: network, t_branch, t_node, nt_LinkNode
   use m_flowgeom, only: wu_mor, bl
   use m_flow, only: u1, qa, q1_main, u_to_umain
   use m_fm_erosed, only: lsedtot, e_sbcn, nd_mor, ln_mor
   use m_flowparameters, only: flow_solver, FLOW_SOLVER_FM
   use Messagehandling, only: LEVEL_FATAL, mess, errmsg

   !Output                                                      
   real(fp), dimension(:), allocatable, intent(out) :: total_water_discharge_out !< Total water discharge that exits the geometry (flow) node at the junction and must be redistributed over the downstream branches. [n_junctions]
   real(fp), dimension(:), allocatable, intent(out) :: total_width_out !< Total width that exits the geometry (flow) node at the junction. [n_junctions]
   real(fp), dimension(:, :), allocatable, intent(out) :: total_sediment_transport_out !< Total sediment transport that exits the geometry (flow) node at the junction and must be redistributed over the downstream branches. [n_junctions, lsedtot]
   integer, dimension(:), allocatable, intent(out) :: idx_junctions !< Indices of junctions in the network. [n_junctions]
   integer, intent(out) :: n_junctions !< Number of junctions in the network.
   integer, dimension (:,:), allocatable, intent(out) :: links_out !< Link index of the outgoing branches. [n_junctions,maxnumberofconnections]
   integer, dimension (:,:), allocatable, intent(out) :: link_dir_out !< Direction of the outgoing links. [n_junctions,maxnumberofconnections]
   integer, dimension (:), allocatable, intent(out) :: n_links_out !< Number of outgoing links for each junction. [n_junctions]
   integer, dimension (:,:), allocatable, intent(out) :: links_in !< Link index of the incoming branches. [n_junctions,maxnumberofconnections]
   integer, dimension (:), allocatable, intent(out) :: n_links_in !< Number of incoming links for each junction. [n_junctions]
   real(fp), dimension (:,:), allocatable, intent(out) :: width_out !< Width of the outgoing links. [n_junctions,maxnumberofconnections]
   real(fp), dimension (:,:), allocatable, intent(out) :: water_discharge_out !< Water discharge of the outgoing links. [n_junctions,maxnumberofconnections]
   integer, dimension (:), allocatable, intent(out) :: flownode_junction !< Flow node indices for each junction. [n_junctions]
   real(kind=dp), dimension (:,:), allocatable, intent(out) :: width_in !< Width at incoming branches. [n_junctions,maxnumberofconnections]
   real(kind=dp), dimension (:,:), allocatable, intent(out) :: u_in !< Velocity at incoming branches. [n_junctions,maxnumberofconnections]
   real(kind=dp), dimension (:,:), allocatable, intent(out) :: q_main_in !< Main-channel discharge at incoming branches. [n_junctions,maxnumberofconnections]
   real(kind=dp), dimension (:,:), allocatable, intent(out) :: u_to_main_in !< Velocity conversion factor at incoming branches. [n_junctions,maxnumberofconnections]
   real(kind=dp), dimension (:,:), allocatable, intent(out) :: u_out !< Velocity at outgoing branches. [n_junctions,maxnumberofconnections]
   real(kind=dp), dimension (:,:), allocatable, intent(out) :: q_main_out !< Main-channel discharge at outgoing branches. [n_junctions,maxnumberofconnections]
   real(kind=dp), dimension (:,:), allocatable, intent(out) :: u_to_main_out !< Velocity conversion factor at outgoing branches. [n_junctions,maxnumberofconnections]
   real(kind=dp), dimension (:,:), allocatable, intent(out) :: bl_out !< Downstream bottom level at outgoing branches. [n_junctions,maxnumberofconnections]
         
   !Locals
   real(kind=dp) :: qb1d !< flow discharge considering direction. 
   real(kind=dp) :: wb1d !< width.
   real(kind=dp) :: sb1d !< sediment transport rate per unit width considering direction.
   type(t_node), pointer :: pnod !< pointer to the node structure of the geometry node at the junction.
   logical :: error !< flag to indicate if an error occurred during execution.
   integer :: node_out !< first flownode of the outgoing branch. 
   integer :: inod !< counter of geometry nodes.
   integer :: ilink !< counter of links connected to the geometry node.
   integer :: link_junction !< link connected to geometry node.
   integer :: link_dir !< direction of the link connected to the geometry node. 
   integer :: flownode_idx !< index of flownode at junction node.
   integer :: ised !< counter of sediment fractions.
   integer :: istat !< status of memory allocation.
   integer :: n_links !< number of outgoing links for this junction
   integer :: sb_dir !< direction of transport at geometry (junction) node
                                                            !!  1: Sediment enters the flow node.
                                                            !! -1: Sediment exits the flow node.                       
   !                                                        
   !                               sb_dir                   
   !                                                        
   !                                               []       
   !                                              /         
   !                                          -1 /          
   !                                           ^/           
   !                                           /            
   !      discharge                      1    /             
   !      -------->        _______[]_____^___[]             
   !                                          \              
   !                                           \-1          
   !                                            \^           
   !                                             \           
   !                                              \          
   !                                               \         
   !                                                []       
   !        

   !Allocate
   istat = 0
   if (istat == 0) then
      allocate (total_water_discharge_out(network%nds%Count), stat=istat)
   end if
   if (istat == 0) then
      allocate (total_width_out(network%nds%Count), stat=istat)
   end if
   if (istat == 0) then
      allocate (total_sediment_transport_out(network%nds%Count, lsedtot), stat=istat)
   end if
   if (istat == 0) then
      allocate (idx_junctions(network%nds%Count), stat=istat)
   end if
   if (istat == 0) then
      allocate (n_links_out(network%nds%Count), stat=istat)
   end if
   if (istat == 0) then
      allocate (links_out(network%nds%Count,network%nds%maxnumberofconnections), stat=istat)
   end if
   if (istat == 0) then
      allocate (link_dir_out(network%nds%Count,network%nds%maxnumberofconnections), stat=istat)
   end if
   if (istat == 0) then
      allocate (width_out(network%nds%Count,network%nds%maxnumberofconnections), stat=istat)
   end if
   if (istat == 0) then
      allocate (water_discharge_out(network%nds%Count,network%nds%maxnumberofconnections), stat=istat)
   end if
   if (istat == 0) then
      allocate (links_in(network%nds%Count,network%nds%maxnumberofconnections), stat=istat)
   end if
   if (istat == 0) then
      allocate (n_links_in(network%nds%Count), stat=istat)
   end if
   if (istat == 0) then
      allocate (flownode_junction(network%nds%Count), stat=istat)
   end if
   if (istat == 0) then
      allocate (width_in(network%nds%Count,network%nds%maxnumberofconnections), stat=istat)
   end if
   if (istat == 0) then
      allocate (u_in(network%nds%Count,network%nds%maxnumberofconnections), stat=istat)
   end if
   if (istat == 0) then
      allocate (q_main_in(network%nds%Count,network%nds%maxnumberofconnections), stat=istat)
   end if
   if (istat == 0) then
      allocate (u_to_main_in(network%nds%Count,network%nds%maxnumberofconnections), stat=istat)
   end if
   if (istat == 0) then
      allocate (u_out(network%nds%Count,network%nds%maxnumberofconnections), stat=istat)
   end if
   if (istat == 0) then
      allocate (q_main_out(network%nds%Count,network%nds%maxnumberofconnections), stat=istat)
   end if
   if (istat == 0) then
      allocate (u_to_main_out(network%nds%Count,network%nds%maxnumberofconnections), stat=istat)
   end if
   if (istat == 0) then
      allocate (bl_out(network%nds%Count,network%nds%maxnumberofconnections), stat=istat)
   end if


   if (istat /= 0) then
      error = .true.
      write (errmsg, '(a)') 'fm_bott3d::error deallocating memory.'
      call mess(LEVEL_FATAL, errmsg)
   end if

   !Initialize
   total_water_discharge_out(:) = 0.0_dp
   total_width_out(:) = 0.0_dp 
   total_sediment_transport_out(:, :) = 0.0_dp 
   width_out = 0.0_dp
   water_discharge_out = 0.0_dp
   n_links_out=0
   n_links_in=0
   links_out=0
   links_in=0
   flownode_junction=0
   width_in=0.0_dp
   u_in=0.0_dp
   q_main_in=0.0_dp
   u_to_main_in=0.0_dp
   u_out=0.0_dp
   q_main_out=0.0_dp
   u_to_main_out=0.0_dp
   bl_out=0.0_dp
   n_links=0
   n_junctions=0

   do inod = 1, network%nds%Count
      pnod => network%nds%node(inod)
      if (pnod%numberofconnections == 1) then
         cycle
      end if
      if (pnod%nodeType == nt_LinkNode) then ! connection node
         n_junctions=n_junctions+1
         idx_junctions(n_junctions)=inod
         flownode_idx = pnod%gridnumber
         flownode_junction(n_junctions)=flownode_idx

         do ilink = 1, nd_mor(flownode_idx)%lnx 
            link_junction = abs(nd_mor(flownode_idx)%ln(ilink))
            link_dir = sign(1, nd_mor(flownode_idx)%ln(ilink))
            wb1d = wu_mor(link_junction)
            
            if (u1(link_junction) * link_dir < 0.0_dp) then
               ! Outgoing discharge
               n_links_out(n_junctions)=n_links_out(n_junctions)+1 
               n_links=n_links_out(n_junctions) 
               links_out(n_junctions,n_links)=link_junction
               link_dir_out(n_junctions,n_links)=link_dir
               width_out(n_junctions,n_links)=wb1d
               u_out(n_junctions,n_links)=u1(link_junction)
               q_main_out(n_junctions,n_links)=q1_main(link_junction)
               u_to_main_out(n_junctions,n_links)=u_to_umain(link_junction)
               qb1d = -qa(link_junction) * link_dir ! replace with junction advection: to do WO
               water_discharge_out(n_junctions,n_links) = qb1d
               total_width_out(n_junctions) = total_width_out(n_junctions) + wb1d
               total_water_discharge_out(n_junctions) = total_water_discharge_out(n_junctions) + qb1d
               sb_dir = -1
               ! Find the node connected to this outgoing link that is not the junction node.
               if (ln_mor(1,link_junction) == flownode_idx) then
                  node_out = ln_mor(2,link_junction)
               else
                  node_out = ln_mor(1,link_junction)
               end if
               bl_out(n_junctions,n_links)=bl(node_out)
            else
               ! Incoming discharge
               n_links_in(n_junctions)=n_links_in(n_junctions)+1 
               n_links=n_links_in(n_junctions)
               links_in(n_junctions,n_links)=link_junction
               width_in(n_junctions,n_links)=wb1d
               u_in(n_junctions,n_links)=u1(link_junction)
               q_main_in(n_junctions,n_links)=q1_main(link_junction)
               u_to_main_in(n_junctions,n_links)=u_to_umain(link_junction)
               sb_dir = 1
            end if
            
            do ised = 1, lsedtot
               sb1d = e_sbcn(link_junction, ised) * link_dir ! first compute all outgoing sed. transport.
               if (flow_solver == FLOW_SOLVER_FM .or. pnod%numberofconnections == 2) then !standard
                  !V: In the standard scheme, at the <e_sbcn> of the outgoing links we have the upwind transport, i.e.,
                  !part of the transport in the junction node. By summing over all of them we have the total transport at
                  !the junction node, which we then redistribute.
                  !We apply this to the standard scheme and to the nodes with only 2 connections, as in this second case
                  !we have not modified the link direction and the same logic applies as for the standard scheme.
                  ! this works for one incoming branch TO DO: WO
                  if (sb_dir == -1) then
                     total_sediment_transport_out(n_junctions, ised) = total_sediment_transport_out(n_junctions, ised) + max(-wb1d * sb1d, 0.0_fp) ! outgoing transport is negative
                  end if
               else !FM1DIMP
                  !V: In the FM1DIMP scheme at <e_sbcn> of the incoming links we have the upwind transport, i.e., the transport
                  !in the ghost cell for multivaluedness of each branch. By summing over all of them we have the total
                  !transport incoming to the junction, which we want to redistribute.
                  if (sb_dir == 1) then
                     total_sediment_transport_out(n_junctions, ised) = total_sediment_transport_out(n_junctions, ised) + wb1d * sb1d ! incoming transport is positive
                  end if
               end if
            end do
         end do
      end if
   end do

   end subroutine nodal_point_relation_data

   subroutine nodal_point_relation_function(facCheck,sediment_transport_rate,pNodRel,link_dir_out,width_out,total_width_out,water_discharge_out,total_water_discharge_out,total_sediment_transport_out,kinod,j,ised)   

   use precision, only: dp
   use morphology_data_module, only: t_noderelation

   real(kind=dp), intent(inout) :: facCheck

   real(kind=dp), intent(out) :: sediment_transport_rate

   type(t_noderelation), target, intent(in) :: pNodRel
   integer, dimension (:,:), allocatable, intent(in) :: link_dir_out ![n_junctions,link]
   real(kind=dp), dimension(:,:), allocatable, intent(in) :: water_discharge_out ![n_junctions,maxnumberofconnections]
   real(kind=dp), dimension(:), allocatable, intent(in) :: total_width_out ![n_junctions]
   real(kind=dp), dimension(:,:), allocatable, intent(in) :: width_out ![n_junctions,maxnumberofconnections]
   real(fp), dimension(:), allocatable, intent(in) :: total_water_discharge_out !< sum of outgoing discharge at 1d node
   real(fp), dimension(:, :), allocatable, intent(in) :: total_sediment_transport_out !< sum of incoming sediment transport at 1d node
   integer, intent(in) :: kinod
   integer, intent(in) :: j
   integer, intent(in) :: ised

   !Local variables
   real(kind=dp) :: facQ
   real(kind=dp) :: facW
         
   facQ = (water_discharge_out(kinod, j)  / total_water_discharge_out(kinod))**pNodRel%expQ
   facW = (width_out(kinod, j) / total_width_out(kinod))**pNodRel%expW

   facCheck = facCheck + facQ * facW

   sediment_transport_rate = -link_dir_out(kinod,j) * facQ * facW * total_sediment_transport_out(kinod, ised) / width_out(kinod, j)

   end subroutine nodal_point_relation_function

   !> Retrieve the nodal point relation structure for a given junction node and sediment fraction.
   !! This subroutine determines the correct nodal point relation (t_noderelation) for a specified
   !! junction node (flownode_junction), sediment fraction (ised), and node index (kinod), based on
   !! the number of incoming and outgoing links and their indices. It is used to apply the appropriate
   !! closure relation for sediment transport redistribution at network junctions.
   !!
   subroutine get_nodal_point_relation_parameters(&
      pNodRel, & !output
      flownode_junction,n_links_out,n_links_in,links_in,ised,kinod) !input

   use morphology_data_module, only: t_nodefraction, t_noderelation
   use m_sediment, only: stmpar
   use m_ini_noderel, only: get_noderel_idx

   implicit none

   !Output
   type(t_noderelation), pointer, intent(out) :: pNodRel !< pointer to the nodal point relation structure for the given junction node and sediment fraction

   !Input
   integer, intent(in) :: flownode_junction(:) !< array of junction node indices
   integer, intent(in) :: n_links_out(:) !< array of number of outgoing links per junction node
   integer, intent(in) :: n_links_in(:) !< array of number of incoming links per junction node
   integer, intent(in) :: links_in(:,:) !< array of incoming link indices per junction node
   integer, intent(in) :: ised !< sediment fraction index
   integer, intent(in) :: kinod !< index of the current junction node in the arrays

   !Local variables
   integer :: iFrac
   integer :: nrd_idx
   type(t_nodefraction), pointer :: pFrac

   !Execute   
   iFrac = min(ised, stmpar%nrd%nFractions)
   pFrac => stmpar%nrd%nodefractions(iFrac)
   nrd_idx = get_noderel_idx(pFrac, flownode_junction(kinod), n_links_out(kinod),n_links_in(kinod),links_in(kinod,1))
   pNodRel => pFrac%noderelations(nrd_idx)
               
   end subroutine get_nodal_point_relation_parameters
      
   !> Compute sediment transport rate for a junction node using a tabulated nodal point relation.
   !! This subroutine applies a user-defined table to redistribute sediment transport at a network
   !! junction based on the ratio of outgoing branch discharges. It is used for bifurcations where
   !! the distribution of sediment is specified by a lookup table (pNodRel%Table).
   !!
   subroutine nodal_point_relation_table(&
      facCheck,sediment_transport_rate,& !output
      pNodRel,links_out,link_dir_out,width_out,water_discharge_out,& !input
      total_water_discharge_out,total_sediment_transport_out,kinod,j,ised) !input

   use precision, only: dp
   use morphology_data_module, only: t_noderelation
   use m_tables, only: interpolate
   use Messagehandling, only: SetMessage, LEVEL_FATAL

   implicit none

   ! Arguments
   real(kind=dp), intent(inout) :: facCheck !< Accumulator for normalization factor (set to 1.0 for table method)
   real(kind=dp), intent(out) :: sediment_transport_rate !< Computed sediment transport rate for the branch
   type(t_noderelation), target, intent(in) :: pNodRel !< Nodal point relation structure containing the table
   integer, dimension (:,:), allocatable, intent(in) :: links_out ![kinod, j] Indices of the outgoing links for each junction node
   integer, dimension(:,:), allocatable, intent(in) :: link_dir_out ![kinod, j] Direction of the outgoing link (1 or -1)
   real(kind=dp), dimension(:,:), allocatable, intent(in) :: width_out ![kinod, j] Width of the outgoing branch
   real(kind=dp), dimension(:,:), allocatable, intent(in) :: water_discharge_out ![kinod, j] Water discharge in the outgoing branch
   real(kind=dp), dimension(:), allocatable, intent(in) :: total_water_discharge_out ![kinod] Total outgoing water discharge at the junction node
   real(kind=dp), dimension(:, :), allocatable, intent(in) :: total_sediment_transport_out ![kinod, ised] Total sediment transport to be redistributed at the junction node for sediment fraction `ised`
   integer, intent(in) :: kinod !< Index of the current junction node
   integer, intent(in) :: j !< Index of the current branch
   integer, intent(in) :: ised !< Sediment fraction index

   ! Local variables
   real(kind=dp) :: Qbr1, Qbr2, QbrRatio, SbrRatio
      
   facCheck = 1.0_dp

   if (links_out(kinod,j) == pNodRel%BranchOut1Ln) then
      Qbr1 = water_discharge_out(kinod, j) 
      Qbr2 = total_water_discharge_out(kinod) - water_discharge_out(kinod, j) 
   elseif (links_out(kinod,j) == pNodRel%BranchOut2Ln) then
      Qbr1 = total_water_discharge_out(kinod) - water_discharge_out(kinod, j) 
      Qbr2 = water_discharge_out(kinod, j) 
   else
      call SetMessage(LEVEL_FATAL, 'Unknown Branch Out (This should never happen!)')
   end if

   QbrRatio = Qbr1 / Qbr2

   SbrRatio = interpolate(pNodRel%Table, QbrRatio)

   if (links_out(kinod,j) == pNodRel%BranchOut1Ln) then
      sediment_transport_rate = -link_dir_out(kinod,j) * SbrRatio * total_sediment_transport_out(kinod, ised) / (1 + SbrRatio) / width_out(kinod, j)
   elseif (links_out(kinod,j) == pNodRel%BranchOut2Ln) then
      sediment_transport_rate = -link_dir_out(kinod,j) * total_sediment_transport_out(kinod, ised) / (1 + SbrRatio) / width_out(kinod, j)
   end if
                        
   end subroutine nodal_point_relation_table
                           
end module m_fm_bott3d
