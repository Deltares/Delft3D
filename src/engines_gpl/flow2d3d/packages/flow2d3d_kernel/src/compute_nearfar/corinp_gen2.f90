subroutine corinp_gen2(error, gdp)
!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
!
!  This program is free software: you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation version 3.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------
!  $Id: corinp_gen2.f90 67957 2020-11-16 11:45:07Z platzek $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160128_34357_NearField_Coupling/src/engines_gpl/flow2d3d/packages/kernel/src/compute_nearfar/corinp_gen2.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Reads input needed for the coupling of Corjet/Cortime/Cormix
!              with Delft3d-Flow
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
!
    use precision
    use properties
    use string_module
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    ! They replace the  include igd / include igp lines
    !
    integer                      , pointer :: lundia
    integer                      , pointer :: itnfli
    integer                      , pointer :: lstsc
    integer                      , pointer :: no_dis
    integer                      , pointer :: no_amb_max
    integer       ,dimension(:)  , pointer :: no_amb
    integer       ,dimension(:)  , pointer :: const_operator
    real(fp)                     , pointer :: momrelax
    real(fp)      ,dimension(:)  , pointer :: x_diff
    real(fp)      ,dimension(:)  , pointer :: y_diff
    real(fp)      ,dimension(:,:), pointer :: x_amb
    real(fp)      ,dimension(:,:), pointer :: y_amb
    real(fp)      ,dimension(:)  , pointer :: x_intake
    real(fp)      ,dimension(:)  , pointer :: y_intake
    real(fp)      ,dimension(:)  , pointer :: z_intake
    real(fp)      ,dimension(:)  , pointer :: q_diff
    real(fp)      ,dimension(:,:), pointer :: const_diff
    real(fp)      ,dimension(:)  , pointer :: d0
    real(fp)      ,dimension(:)  , pointer :: h0
    real(fp)      ,dimension(:)  , pointer :: sigma0
    real(fp)      ,dimension(:)  , pointer :: theta0
    character(256),dimension(:)  , pointer :: base_path
    character(256),dimension(:,:), pointer :: basecase
    character(256)               , pointer :: filename
    real(fp)                     , pointer :: hdt
    type(tree_data)              , pointer :: cosumofile_ptr
!
! Global variables
!
    logical, intent(out)   :: error
!
! Local variables
!
    integer                             :: luntmp
    integer, external                   :: newlun
    integer                             :: i
    integer                             :: j
    integer                             :: idis
    integer                             :: istat
    integer                             :: no_dis_read
    real(sp)                            :: version
    real(hp), dimension(:), allocatable :: r_input
    character(1)                        :: slash
    character(300)                      :: cdummy
    character(300)                      :: errmsg
    type(tree_data), pointer            :: cosumoblock_ptr
    type(tree_data), pointer            :: data_ptr
    type(tree_data), pointer            :: node_ptr
!
!! executable statements -------------------------------------------------------
!
    lundia         => gdp%gdinout%lundia
    itnfli         => gdp%gdinttim%itnfli
    lstsc          => gdp%d%lstsc
    no_dis         => gdp%gdnfl%no_dis
    no_amb_max     => gdp%gdnfl%no_amb_max
    no_amb         => gdp%gdnfl%no_amb
    const_operator => gdp%gdnfl%const_operator
    momrelax       => gdp%gdnfl%momrelax
    x_diff         => gdp%gdnfl%x_diff
    y_diff         => gdp%gdnfl%y_diff
    x_amb          => gdp%gdnfl%x_amb
    y_amb          => gdp%gdnfl%y_amb
    x_intake       => gdp%gdnfl%x_intake
    y_intake       => gdp%gdnfl%y_intake
    z_intake       => gdp%gdnfl%z_intake
    q_diff         => gdp%gdnfl%q_diff
    const_diff     => gdp%gdnfl%const_diff
    d0             => gdp%gdnfl%d0
    h0             => gdp%gdnfl%h0
    sigma0         => gdp%gdnfl%sigma0
    theta0         => gdp%gdnfl%theta0
    base_path      => gdp%gdnfl%base_path
    basecase       => gdp%gdnfl%basecase
    filename       => gdp%gdnfl%infile
    hdt            => gdp%gdnumeco%hdt
    !
    allocate(r_input(max(2,gdp%d%lstsc)), stat=istat)
    if (gdp%arch=='win32' .or. gdp%arch=='win64') then
       slash = '\'
    else
       slash = '/'
    endif
    !
    if (.not.associated(gdp%gdnfl%cosumofile_ptr)) then
       !
       ! Create Cosumo input tree
       !
       write(lundia,'(3a)') "Reading file '", trim(filename), "' ..."
       call tree_create( 'Cosumo input', cosumofile_ptr )
       call tree_put_data( cosumofile_ptr, transfer(trim(filename),node_value), 'STRING' )
       !
       ! Put file in input tree
       !
       call prop_file('xml',trim(filename),cosumofile_ptr,istat)
       if (istat /= 0) then
          select case (istat)
          case(1)
             errmsg = FILE_NOT_FOUND // trim(filename)
             call write_error(errmsg, unit=lundia)
          case(3)
             errmsg = PREMATURE_EOF // trim(filename)
             call write_error(errmsg, unit=lundia)
          case default
             errmsg = FILE_READ_ERROR // trim(filename)
             call write_error(errmsg, unit=lundia)
          endselect
          nullify(gdp%gdnfl%cosumofile_ptr)
          error = .true.
          return
       endif
       !
       ! Store the file data(-pointer) in GDP
       !
       gdp%gdnfl%cosumofile_ptr => cosumofile_ptr
    else
       !
       ! File already read: reuse stored pointer
       !
       cosumofile_ptr => gdp%gdnfl%cosumofile_ptr
    endif
    !
    call tree_get_node_by_name( cosumofile_ptr, 'cosumo', cosumoblock_ptr )
    if (.not.associated(cosumoblock_ptr)) then
       write(lundia, '(a)') "ERROR: Tag '<COSUMO>' not found"
       error = .true.
       return
    endif
    version     = -999.9
    no_dis_read = 0
    do i=1, size(cosumoblock_ptr%child_nodes)
       if (tree_get_name(cosumoblock_ptr%child_nodes(i)%node_ptr) == "settings") then
          no_dis_read = no_dis_read + 1
       endif
    enddo
    if (no_dis_read /= no_dis) then
       write(lundia,'(a,i0)') "ERROR: Unexpected number of discharges read: ", no_dis_read
       error = .true.
    endif
    !
    call prop_get(cosumofile_ptr, 'COSUMO/fileVersion', version)
    if (comparereal(version, 0.3_sp) /= 0) then
       write(lundia,'(a,f5.2)') "ERROR: Unexpected FileVersion number read: ", version
       error = .true.
    endif
    !
    momrelax = -999.0_fp
    call prop_get(cosumofile_ptr, 'COSUMO/momentumRelaxation', momrelax)
    if (comparereal(momrelax, 0.0_fp) == 1) then
       !
       ! The prescribed momentum relaxation parameter N should adhere to:
       ! dt < N*dt                   , i.e. N >= 1.0
       !      N*dt < 0.5*itnfli*dt   , i.e. N <= 0.5*itnfli
       if (momrelax < 1.0_fp) then
           momrelax = 1.0_fp
           write(lundia,'(a,f5.2,a)') "Message: Limited nearfield Momentum Relaxation to the minimum value of 1.0."
       endif
       if (momrelax > 0.5_fp*real(itnfli,fp)) then
           momrelax = 0.5_fp*real(itnfli,fp)
           write(lundia,'(a,f5.2,a)') "Message: Limited nearfield Momentum Relaxation to the maximum value of 0.5*(the nearfield-coupling interval ITNFLI)."
       endif
       write(lundia,'(a,f5.2,a)') "Message: Nearfield Momentum Relaxation = 1/(", momrelax, "*dt)"
    else
       !
       ! No value prescribed: use the default of N = 5, i.e. Trelax = 5*dt.
       !
       momrelax = 5.0_fp
    endif
    !
    idis = 0
    do i=1, size(cosumoblock_ptr%child_nodes)
       node_ptr => cosumoblock_ptr%child_nodes(i)%node_ptr
       if (tree_get_name(node_ptr) /= "settings") cycle
       idis = idis + 1
       !
       ! Read position diffusor
       !
       r_input = -999.0_fp
       call prop_get(node_ptr, 'data/xydiff', r_input, 2)
       x_diff(idis) = r_input(1)
       y_diff(idis) = r_input(2)
       !
       ! Read positions ambient conditions
       !
       no_amb(idis) = 0
       nullify(data_ptr)
       call tree_get_node_by_name(cosumoblock_ptr%child_nodes(i)%node_ptr, 'data', data_ptr )
       if (.not. associated(data_ptr)) then
          write(lundia,'(a)') "ERROR: <settings> block does not contain a <data> block."
          error = .true.
          cycle
       endif
       do j=1, size(data_ptr%child_nodes)
          if (tree_get_name(data_ptr%child_nodes(j)%node_ptr) == "xyambient") then
             no_amb(idis) = no_amb(idis) + 1
             r_input = -999.0_fp
             call prop_get(data_ptr%child_nodes(j)%node_ptr, 'xyambient', r_input, 2)
             x_amb(idis,no_amb(idis)) = r_input(1)
             y_amb(idis,no_amb(idis)) = r_input(2)
          endif
       enddo
       !
       ! Read intake location
       !
       r_input = -999.0_fp
       call prop_get(node_ptr, 'data/xyintake', r_input, 2)
       x_intake(idis) = r_input(1)
       y_intake(idis) = r_input(2)
       !
       ! Read discharge characteristics
       !
       call prop_get(node_ptr, 'data/discharge/m3s', q_diff(idis))
       cdummy = ' '
       call prop_get(node_ptr, 'data/discharge/constituentsoperator', cdummy)
       call str_lower(cdummy)
       if (cdummy == "absolute") then
          const_operator(idis) = NFLCONSTOPERATOR_ABS
       elseif (cdummy == "excess") then
          const_operator(idis) = NFLCONSTOPERATOR_EXC
       else
          write(lundia,'(a)') "ERROR: '<settings> / <data> / <discharge> / <constituentsOperator>' expected with value 'excess' or 'absolute'"
          error = .true.
       endif
       r_input = -999.0_fp
       call prop_get(node_ptr, 'data/discharge/constituents', r_input, lstsc)
       do j=1,lstsc
          const_diff(idis,j) = r_input(j)
       enddo
       !
       ! Read remainder of cormix general input
       !
       call prop_get(node_ptr, 'data/d0', d0(idis))
       call prop_get(node_ptr, 'data/h0', h0(idis))
       call prop_get(node_ptr, 'data/theta0', theta0(idis))
       call prop_get(node_ptr, 'data/sigma0', sigma0(idis))
       call prop_get(node_ptr, 'comm/ff2nfdir', base_path(idis))
       call prop_get(node_ptr, 'comm/ffrundir', basecase(idis,1))
       if (trim(basecase(idis,1)) == "rundir") then
          call getcwd(cdummy)
          basecase(idis,1) = trim(cdummy)//slash
          write(*,'(3a)') "corinp_gen2: 'rundir' substituted by '", trim(basecase(idis,1)), "'"
       endif
    enddo
    !
    ! Delete file info
    ! This will force rereading the Cosumo file at the next Cosumo calculation
    !
    call tree_destroy(gdp%gdnfl%cosumofile_ptr)
    !
    deallocate(r_input, stat=istat)
end subroutine corinp_gen2
