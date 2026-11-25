!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2026.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!-------------------------------------------------------------------------------

!> Support wave-group transport at absorbing-generating (ABSGEN) boundaries.
!! Compute depth-averaged inputs to the boundary closure and distribute its
!! prescribed transport over the layers while preserving vertical shear.
module m_absgen_3d
   use precision, only: dp

   implicit none(type, external)

   private
   public :: accumulate_absgen_boundary_mean
   public :: apply_absgen_barotropic_increment
   public :: compute_absgen_depth_averaged_velocity
   public :: compute_absgen_friction_acceleration
   public :: compute_absgen_incident_velocity
   public :: compute_absgen_reflected_velocity
   public :: relax_absgen_boundary_means

contains

   !> Accumulate width-weighted velocities for one owned boundary face.
   !! The caller sums these local contributions across MPI ranks using the
   !! same global section IDs on each rank.
   pure subroutine accumulate_absgen_boundary_mean(is_owned, boundary_section, face_width, normal_velocity, &
                                                    tangential_velocity, weighted_u, weighted_v, boundary_length)
      logical, intent(in) :: is_owned !< Whether this rank owns the face.
      integer, intent(in) :: boundary_section !< Global ID within the arrays.
      real(kind=dp), intent(in) :: face_width !< Horizontal face width (m).
      real(kind=dp), intent(in) :: normal_velocity !< Inward velocity (m/s).
      real(kind=dp), intent(in) :: tangential_velocity !< Along-face velocity (m/s).
      real(kind=dp), intent(inout) :: weighted_u(:) !< Width times normal velocity (m^2/s).
      real(kind=dp), intent(inout) :: weighted_v(:) !< Width times tangential velocity (m^2/s).
      real(kind=dp), intent(inout) :: boundary_length(:) !< Sum of face widths (m).

      if (.not. is_owned) then
         return
      end if

      weighted_u(boundary_section) = weighted_u(boundary_section) + face_width * normal_velocity
      weighted_v(boundary_section) = weighted_v(boundary_section) + face_width * tangential_velocity
      boundary_length(boundary_section) = boundary_length(boundary_section) + face_width
   end subroutine accumulate_absgen_boundary_mean

   !> Apply a depth-uniform velocity increment while preserving layer shear.
   !! The wet-layer transport (m^3/s) equals total wet area times the target
   !! velocity, up to rounding. If the total wet area is zero, return the
   !! target velocity uniformly.
   pure subroutine apply_absgen_barotropic_increment(layer_area, current_velocity, target_depth_averaged_velocity, target_velocity)
      real(kind=dp), intent(in) :: layer_area(:) !< Wetted face area per layer (m^2).
      real(kind=dp), intent(in) :: current_velocity(:) !< Input layer normal velocity (m/s).
      real(kind=dp), intent(in) :: target_depth_averaged_velocity !< Prescribed mean velocity (m/s).
      real(kind=dp), intent(out) :: target_velocity(:) !< Corrected layer normal velocity (m/s).

      real(kind=dp) :: current_depth_averaged_velocity
      real(kind=dp) :: total_area
      real(kind=dp) :: transport_correction

      total_area = sum(max(layer_area, 0.0_dp))
      if (total_area <= tiny(total_area)) then
         target_velocity = target_depth_averaged_velocity
         return
      end if

      current_depth_averaged_velocity = &
         dot_product(max(layer_area, 0.0_dp), current_velocity) / total_area
      target_velocity = current_velocity + &
                        target_depth_averaged_velocity - current_depth_averaged_velocity

      ! Remove the final floating-point transport residual without changing shear.
      transport_correction = target_depth_averaged_velocity - &
                             dot_product(max(layer_area, 0.0_dp), target_velocity) / total_area
      target_velocity = target_velocity + transport_correction
   end subroutine apply_absgen_barotropic_increment

   !> Compute a thickness-weighted cell velocity without modifying flow state.
   !! Pass one more interface than velocity sample, ordered bottom to top.
   !! Return zero for a dry column or inconsistent input sizes.
   pure subroutine compute_absgen_depth_averaged_velocity(interface_elevation, layer_velocity_x, layer_velocity_y, &
                                                          depth_averaged_velocity_x, depth_averaged_velocity_y)
      real(kind=dp), intent(in) :: interface_elevation(:) !< Layer interface elevations (m).
      real(kind=dp), intent(in) :: layer_velocity_x(:) !< Layer x velocities (m/s).
      real(kind=dp), intent(in) :: layer_velocity_y(:) !< Layer y velocities (m/s).
      real(kind=dp), intent(out) :: depth_averaged_velocity_x !< Mean x velocity (m/s).
      real(kind=dp), intent(out) :: depth_averaged_velocity_y !< Mean y velocity (m/s).

      integer :: layer
      real(kind=dp) :: layer_thickness
      real(kind=dp) :: total_thickness

      depth_averaged_velocity_x = 0.0_dp
      depth_averaged_velocity_y = 0.0_dp
      if (size(interface_elevation) /= size(layer_velocity_x) + 1 .or. &
          size(layer_velocity_x) /= size(layer_velocity_y)) then
         return
      end if

      total_thickness = 0.0_dp
      do layer = 1, size(layer_velocity_x)
         layer_thickness = max(interface_elevation(layer + 1) - interface_elevation(layer), 0.0_dp)
         total_thickness = total_thickness + layer_thickness
         depth_averaged_velocity_x = depth_averaged_velocity_x + layer_thickness * layer_velocity_x(layer)
         depth_averaged_velocity_y = depth_averaged_velocity_y + layer_thickness * layer_velocity_y(layer)
      end do

      if (total_thickness > tiny(total_thickness)) then
         depth_averaged_velocity_x = depth_averaged_velocity_x / total_thickness
         depth_averaged_velocity_y = depth_averaged_velocity_y / total_thickness
      else
         depth_averaged_velocity_x = 0.0_dp
         depth_averaged_velocity_y = 0.0_dp
      end if
   end subroutine compute_absgen_depth_averaged_velocity

   !> Compute the signed normal acceleration term for quadratic friction.
   !! Use velocities relative to Stokes drift. The boundary closure subtracts
   !! this term from the inward normal acceleration.
   pure function compute_absgen_friction_acceleration(friction_coefficient, normal_velocity, tangential_velocity, &
                                                      normal_stokes_velocity, tangential_stokes_velocity) result(acceleration)
      real(kind=dp), intent(in) :: friction_coefficient !< Depth-scaled friction coefficient (1/m).
      real(kind=dp), intent(in) :: normal_velocity !< Inward depth-mean velocity (m/s).
      real(kind=dp), intent(in) :: tangential_velocity !< Along-face depth-mean velocity (m/s).
      real(kind=dp), intent(in) :: normal_stokes_velocity !< Inward Stokes drift (m/s).
      real(kind=dp), intent(in) :: tangential_stokes_velocity !< Along-face Stokes drift (m/s).
      real(kind=dp) :: acceleration !< Signed friction acceleration term (m/s^2).

      real(kind=dp) :: relative_normal_velocity
      real(kind=dp) :: relative_tangential_velocity

      relative_normal_velocity = normal_velocity - normal_stokes_velocity
      relative_tangential_velocity = tangential_velocity - tangential_stokes_velocity
      acceleration = friction_coefficient * &
                     sqrt(relative_normal_velocity**2 + relative_tangential_velocity**2) * &
                     relative_normal_velocity
   end function compute_absgen_friction_acceleration

   !> Relax globally accumulated boundary means without changing empty sections.
   !! Divide each weighted sum by its section length. Blend the resulting
   !! velocity into the stored mean with weight dt / (cats * Trep), where
   !! cats is the relaxation timescale in representative wave periods.
   pure subroutine relax_absgen_boundary_means(weighted_u, weighted_v, boundary_length, relaxation_factor, mean_u, mean_v)
      real(kind=dp), intent(in) :: weighted_u(:) !< Section sum of widths times normal velocity (m^2/s).
      real(kind=dp), intent(in) :: weighted_v(:) !< Section sum of widths times tangential velocity (m^2/s).
      real(kind=dp), intent(in) :: boundary_length(:) !< Total section width (m).
      real(kind=dp), intent(in) :: relaxation_factor !< Weight of the new sample.
      real(kind=dp), intent(inout) :: mean_u(:) !< Running normal mean (m/s).
      real(kind=dp), intent(inout) :: mean_v(:) !< Running tangential mean (m/s).

      integer :: boundary_section
      real(kind=dp) :: instantaneous_u
      real(kind=dp) :: instantaneous_v

      do boundary_section = 1, size(boundary_length)
         if (boundary_length(boundary_section) <= tiny(1.0_dp)) then
            cycle
         end if

         instantaneous_u = weighted_u(boundary_section) / boundary_length(boundary_section)
         instantaneous_v = weighted_v(boundary_section) / boundary_length(boundary_section)
         mean_u(boundary_section) = relaxation_factor * instantaneous_u + &
                                    (1.0_dp - relaxation_factor) * mean_u(boundary_section)
         mean_v(boundary_section) = relaxation_factor * instantaneous_v + &
                                    (1.0_dp - relaxation_factor) * mean_v(boundary_section)
      end do
   end subroutine relax_absgen_boundary_means

   !> Recover the signed velocity along the incident wave direction (m/s).
   !! Use a cosine floor to bound the reconstructed amplitude near grazing incidence.
   pure function compute_absgen_incident_velocity(normal_velocity, tangential_velocity, &
                                                   min_normal_direction_cosine) result(signed_velocity)
      real(kind=dp), intent(in) :: normal_velocity !< Inward incident velocity (m/s).
      real(kind=dp), intent(in) :: tangential_velocity !< Along-face incident velocity (m/s).
      real(kind=dp), intent(in) :: min_normal_direction_cosine !< Positive cosine floor.

      real(kind=dp) :: signed_velocity
      real(kind=dp) :: direction_cosine
      real(kind=dp) :: velocity_magnitude

      velocity_magnitude = hypot(normal_velocity, tangential_velocity)
      if (velocity_magnitude <= tiny(1.0_dp)) then
         signed_velocity = 0.0_dp
         return
      end if

      direction_cosine = abs(normal_velocity) / velocity_magnitude
      signed_velocity = normal_velocity / max(direction_cosine, min_normal_direction_cosine)
   end function compute_absgen_incident_velocity

   !> Solve for the signed reflected normal velocity (m/s) in the ABSGEN closure.
   !! Inward folding gives cos(alpha) = abs(ur) / hypot(ur, tangential_residual).
   !! Substituting in ur = forcing * cos(alpha) / (1 + cos(alpha)) gives
   !! abs(forcing) = hypot(ur, tangential_residual) + abs(ur) on the nonzero branch.
   !! At or below the tangential threshold the reflected normal velocity is zero.
   pure function compute_absgen_reflected_velocity(forcing, tangential_residual) result(normal_velocity)
      real(kind=dp), intent(in) :: forcing !< Signed characteristic forcing (m/s).
      real(kind=dp), intent(in) :: tangential_residual !< Reflected along-face velocity (m/s).

      real(kind=dp) :: normal_velocity
      real(kind=dp) :: forcing_magnitude, tangential_magnitude

      forcing_magnitude = abs(forcing)
      tangential_magnitude = abs(tangential_residual)
      normal_velocity = 0.0_dp
      if (forcing_magnitude > tangential_magnitude) then
         normal_velocity = sign(0.5_dp * (forcing_magnitude - tangential_magnitude) * &
                                (1.0_dp + tangential_magnitude / forcing_magnitude), forcing)
      end if
   end function compute_absgen_reflected_velocity

end module m_absgen_3d
