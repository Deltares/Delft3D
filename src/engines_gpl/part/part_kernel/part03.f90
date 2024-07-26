!!  Copyright (C)  Stichting Deltares, 2012-2024.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.
module part03_mod
    use m_stop_exit

    !
contains
    subroutine part03 (lgrid, volume, flow, dx, dy, &
            num_rows, num_columns, mnmaxk, lgrid2, velo, &
            layt, area, depth, dps, locdep, &
            zlevel, zmodel, laytop, laytopp, laybot, &
            pagrid, aagrid, tcktot, ltrack, flow2m, &
            lgrid3, vol1, vol2, vel1, vel2)
        !
        !
        !                   Deltares
        !
        !                        d e l p a r    v3.10
        !
        !
        !     system administration : r.j. vos
        !
        !
        !     created               : july 1991, by l.postma
        !
        !
        !     function              : computes velocities
        !
        !
        !     note                  : none.
        !
        !
        !     logical unit numbers  : none.
        !
        !
        !     functions   called    : none.
        !
        !     i0      integer     1       local   grid help variable
        !     i1      integer     1       local   grid help variable
        !     i2      integer     1       local   grid help variable
        !     i3      integer     1       local   grid help variable
        !     i4      integer     1       local   grid help variable
        !     sum     real        1       local   summed help variable
        !     vx      real        1       local   velocity in x direction
        !     vy      real        1       local   velocity in y direction
        !
        use m_waq_precision    ! single/double precision
        use timers
        implicit none

        !     parameters

        !     kind         function         name                     Description

        integer(int_wp), intent(in) :: num_rows                   !< first dimension lgrid
        integer(int_wp), intent(in) :: num_columns                   !< second dimension lgrid
        integer(int_wp), intent(in) :: mnmaxk                 !< total size of 3D matrix
        integer(int_wp), intent(in) :: layt                   !< number of layers
        integer(int_wp), intent(in) :: lgrid (:, :)            !< active grid indices matrix
        integer(int_wp), intent(in) :: lgrid2(:, :)            !< total grid indices matrix
        real   (real_wp), intent(in) :: dx    (:)              !< x distance of grid cell
        real   (real_wp), intent(in) :: dy    (:)              !< y distance of grid cell
        real   (real_wp), intent(in) :: volume(:)              !< volumes
        real   (real_wp), intent(in) :: flow  (*)              !< flows
        real   (real_wp), intent(in) :: area  (:)              !< horizontal surface area
        real   (real_wp), intent(out) :: depth (:)              !< total water depth (from current surface to bottom)
        real   (real_wp), intent(out) :: velo  (:)              !< velocities in 3D
        real   (real_wp), intent(in) :: dps   (:)              !< bed depth w.r.t. reference (fixed in time!)
        real   (real_wp), intent(out) :: locdep(:, :)            !< depth per layer
        real   (real_wp), intent(out) :: zlevel(:)
        logical, intent(in) :: zmodel
        integer(int_wp), intent(inout) :: laytop(:, :)            !< highest active layer in z-layer model
        integer(int_wp), intent(inout) :: laytopp(:, :)           !< highest active layer in z-layer model of previous time step
        integer(int_wp), intent(inout) :: laybot(:, :)            !< deepest active layer in z-layer model
        integer(int_wp), intent(in) :: pagrid(:, :, :)          !< potentially active z-layer segments grid matrix
        integer(int_wp), intent(inout) :: aagrid(:, :, :)          !< actually active z-layer segments grid matrix
        real   (real_wp), intent(in) :: tcktot(:)
        logical, intent(in) :: ltrack
        real   (real_wp), intent(in) :: flow2m  (*)            !< flows next time level on matrix
        integer(int_wp), pointer :: lgrid3(:, :)        !< original grid (conc array)
        real   (sp), pointer :: vol1   (:)           !< first volume record
        real   (sp), pointer :: vol2   (:)           !< second volume record
        real   (sp), pointer :: vel1  (:)            ! velocity begin hydr step
        real   (sp), pointer :: vel2  (:)            ! velocity end hydr step

        real (sp) :: default = 999.999

        !     local scalars

        integer(int_wp)  i0       !    grid help variable
        integer(int_wp)  i1       !    grid help variable
        integer(int_wp)  i2       !    grid help variable
        integer(int_wp)  i3       !    grid help variable
        integer(int_wp)  i4       !    grid help variable
        real   (real_wp)  sum      !    summed help variable
        real   (real_wp)  vx       !    velocity in x direction
        real   (real_wp)  vy       !    velocity in y direction
        integer(int_wp)  i03d, i33d, i43d
        integer(int_wp)  ilay
        integer(int_wp)  iseg
        real   (dp)  dplay

        integer(4) ithndl              ! handle to time this subroutine
        data       ithndl / 0 /
        if (timon) call timstrt("part03", ithndl)
        !
        !     determine actually active segments in z-layer models
        !
        if (zmodel) then
            do i2 = 2, num_columns
                do i1 = 2, num_rows
                    i0 = lgrid(i1, i2)
                    if (i0  >  0) then
                        laytopp(i1, i2) = laytop(i1, i2)
                        laytop(i1, i2) = laybot(i1, i2)
                        do ilay = 1, laybot(i1, i2)
                            if (pagrid(i1, i2, ilay)==1) then
                                i03d = i0 + (ilay - 1) * num_rows * num_columns
                                if (volume(i03d)>0.0) then
                                    aagrid(i1, i2, ilay) = 1
                                    if (laytop(i1, i2)==laybot(i1, i2)) then
                                        laytop(i1, i2) = ilay
                                    endif
                                end if
                            end if
                        end do
                    end if
                end do
            end do
        end if
        !
        !     initialisation
        !
        velo = 0.0 ! whole array assignment
        depth = 0.0 ! whole array assignment
        locdep = 0.0 ! whole array assignment
        !
        !     loop over the segments
        !
        do i2 = 2, num_columns
            do i1 = 2, num_rows
                i0 = lgrid(i1, i2)
                iseg = lgrid3(i1, i2)
                if (i0  >  0) then
                    if(area(i0)==(0.0)) then
                        write (*, *) ' Area zero in active segment'
                        call stop_exit(1)
                    endif
                    do ilay = 1, layt
                        if (zmodel) then
                            if (aagrid(i1, i2, ilay)==0) cycle
                        end if
                        i03d = i0 + (ilay - 1) * num_rows * num_columns

                        !                 magnitude of the velocities
                        vy = flow(i03d) / volume(i03d) * dy(i0)
                        vx = flow(i03d + mnmaxk) / volume(i03d) * dx(i0)

                        !                 calculate sum; value >= 0
                        sum = vx**2 + vy**2
                        i3 = lgrid2(i1 - 1, i2)
                        i33d = i3 + (ilay - 1) * num_rows * num_columns
                        if (i3  >  0) then
                            vy = flow(i33d) / volume(i03d) * dy(i0)
                            sum = sum + vy**2
                        endif
                        i4 = lgrid2(i1, i2 - 1)
                        i43d = i4 + (ilay - 1) * num_rows * num_columns
                        if (i4  >  0) then
                            vx = flow(i43d + mnmaxk) / volume(i03d) * dx(i0)
                            sum = sum + vx**2
                        endif
                        velo(i03d) = sqrt(sum / 2.0)
                        dplay = volume(i03d) / area(i0)
                        if (ilay == 1) then
                            vel1(iseg) = velo(i03d)

                            ! next time level
                            if(vol2(iseg) > 0.0001) then
                                vy = flow2m(i03d) / vol2(iseg) * dy(i0)
                                vx = flow2m(i03d + mnmaxk) / vol2(iseg) * dx(i0)
                            else
                                vy = 0.0
                                vx = 0.0
                            endif
                            !
                            !                   calculate sum; value >= 0
                            !
                            sum = vx**2 + vy**2
                            !
                            i3 = lgrid2(i1 - 1, i2)
                            i33d = i3 + (ilay - 1) * num_rows * num_columns
                            if (i3  >  0) then
                                if(vol2(iseg) > 0.0001) then
                                    vy = flow2m(i33d) / vol2(iseg) * dy(i0)
                                else
                                    vy = 0.0
                                endif
                                sum = sum + vy**2
                            endif
                            !
                            i4 = lgrid2(i1, i2 - 1)
                            i43d = i4 + (ilay - 1) * num_rows * num_columns
                            if (i4  >  0) then
                                if(vol2(iseg) > 0.0001) then
                                    vx = flow2m(i43d + mnmaxk) / vol2(iseg) * dx(i0)
                                else
                                    vx = 0.0
                                endif
                                sum = sum + vx**2
                            endif
                            vel2(iseg) = sqrt(sum / 2.0)
                            locdep(i0, ilay) = dplay
                        else
                            locdep(i0, ilay) = locdep(i0, ilay - 1) + dplay
                        end if

                        !
                        ! As not all segment in the water column are actually
                        ! active in the case of z-layers, store the last
                        ! local depth in the total depth - the last update
                        ! is automatically the total depth for that water
                        ! column.
                        !
                        depth(i0) = locdep(i0, ilay)
                    end do
                end if
            end do
        end do
        !
        !     determine z coordinate free water surface
        !     totdep = total depth (free water surface - bottom)
        !     dps    = bathymetry (w.r.t. reference level) in water level points
        !
        !     note that zlevel is only used for calculating the vertical
        !     position(za) of particles in case of storing the particle tracks
        !     (itrack=1).
        !
        if (ltrack) then
            do i2 = 1, num_columns
                do i1 = 1, num_rows
                    i0 = lgrid(i1, i2)
                    if (i0  >  0) then
                        zlevel(i0) = locdep(i0, layt) - dps(i0)
                    endif
                enddo
            enddo
        else
            zlevel = default  ! whole array assignment
        endif
        !
        !     end of subroutine
        !
        if (timon) call timstop (ithndl)
        return
        !
    end subroutine
end module
