module sync_flm
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
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
!  
!  
!!--description-----------------------------------------------------------------
! Organizes the communication between two Delft3D-FLOW executables for a 2 layer
! fluid mud model: one doing the calculation for the (3D) water phase on top and
! the other doing calculation for the 2D fluid mud layer below.
! This module is the interface between DelftIO and Delft3D-FLOW.
! Method used: Communication via DelftIO
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use dio_2dfield_rw
    implicit none
!
! Derived Type definitions
!
    type comminfotype
       type (dio2dftype) :: uset
       type (dio2dftype) :: vset
       type (dio2dftype) :: czuset
       type (dio2dftype) :: czvset
       type (dio2dftype) :: wsset
       type (dio2dftype) :: entrset
       type (dio2dftype) :: rsedset
       type (dio2dftype) :: sset
    end type comminfotype
!
! Module variables
!
    type (comminfotype) :: comminfo
!
contains
!
!
!
!==============================================================================
subroutine syncom_init(mudlay, densin, mlb, mub, nlb, nub)
! global data structure definition and access functions
    implicit none
!
! Global variables
!
    integer, intent (in)  :: mlb
    integer, intent (in)  :: mub
    integer, intent (in)  :: nlb
    integer, intent (in)  :: nub
    logical, intent (out) :: densin
    logical, intent (in)  :: mudlay
!
! Local variables
!
    integer :: msize
    integer :: nsize
!
!! executable statements -------------------------------------------------------
!
    ! Call DioInit in both modes. The routine reads the dioinit.ini configuration file
    ! This filename may be specified as an argument to DioInit
    call dioinit
    ! Size of arrays defined with: GDP%D%Nlb:GDP%D%Nub (Nlb/Mlb is negative):
    nsize = nub - nlb + 1
    msize = mub - mlb + 1
    !
    if (.not.mudlay) then
       !
       ! Define outgoing sets for the top (flow) layer
       !
       comminfo%uset    = dio2dfdefine('dio-U'   , dio_plt_double, nsize, msize)
       comminfo%vset    = dio2dfdefine('dio-V'   , dio_plt_double, nsize, msize)
       comminfo%sset    = dio2dfdefine('dio-S'   , dio_plt_double, nsize, msize)
       comminfo%czuset  = dio2dfdefine('dio-CZU' , dio_plt_double, nsize, msize)
       comminfo%czvset  = dio2dfdefine('dio-CZV' , dio_plt_double, nsize, msize)
       comminfo%rsedset = dio2dfdefine('dio-RSED', dio_plt_double, nsize, msize)
       comminfo%wsset   = dio2dfdefine('dio-WS'  , dio_plt_double, nsize, msize)
       !
       ! Define ingoing sets for the top (flow) layer
       !
       comminfo%entrset = dio2dfgetdataset('dio-Entr')
    else
       !
       ! Define outgoing sets for the bottom (fluid mud) layer
       !
       comminfo%entrset = dio2dfdefine('dio-Entr', dio_plt_double, nsize, msize)
       !
       ! Define ingoing sets for the bottom (fluid mud) layer
       !
       comminfo%wsset   = dio2dfgetdataset('dio-WS')
       comminfo%uset    = dio2dfgetdataset('dio-U')
       comminfo%vset    = dio2dfgetdataset('dio-V')
       comminfo%sset    = dio2dfgetdataset('dio-S')
       comminfo%czuset  = dio2dfgetdataset('dio-CZU')
       comminfo%czvset  = dio2dfgetdataset('dio-CZV')
       comminfo%rsedset = dio2dfgetdataset('dio-RSED')
    endif
    !! If run as fluid mud system sediment concentration and
    !! density should be decoupled, that is, densin = .false.
    !! This only applies to the top (flow/suspension) layer (mudlay = .false.)
    if (.not.mudlay) then
       densin = .false.
    endif
end subroutine syncom_init
!
!
!
!==============================================================================
subroutine syncom(mudlay, timnow, itstrt, itstop, num_layers_grid, u0, usus, v0, vsus, &
                & cfurou, czusus, cfvrou, czvsus, r0, rsed, lstsci, lsal,   &
                & ltem, wstau, wssus, entr, s0, sepsus, mlb, mub, nlb, nub)
    implicit none
!
! PARAMETER definitions
!
    integer, parameter :: parallel = 0, sequential = 1
!
! Global variables
!
    integer                                            , intent (in)  :: mlb
    integer                                            , intent (in)  :: mub
    integer                                            , intent (in)  :: nlb
    integer                                            , intent (in)  :: nub
    integer                                            , intent (in)  :: itstop
    integer                                            , intent (in)  :: itstrt
    integer                                            , intent (in)  :: num_layers_grid
    integer                                            , intent (in)  :: lsal
    integer                                            , intent (in)  :: lstsci
    integer                                            , intent (in)  :: ltem
    logical                                            , intent (in)  :: mudlay
    real(fp)                                           , intent (in)  :: timnow
    real(fp), dimension(nlb:nub, mlb:mub, 3)           , intent (in)  :: cfurou
    real(fp), dimension(nlb:nub, mlb:mub, 3)           , intent (in)  :: cfvrou
    real(fp), dimension(nlb:nub, mlb:mub)              , intent (out) :: czusus
    real(fp), dimension(nlb:nub, mlb:mub)              , intent (out) :: czvsus
    real(fp), dimension(nlb:nub, mlb:mub)                             :: entr
    real(fp), dimension(nlb:nub, mlb:mub)              , intent (out) :: rsed
    real(fp), dimension(nlb:nub, mlb:mub)              , intent (in)  :: s0
    real(fp), dimension(nlb:nub, mlb:mub)              , intent (out) :: sepsus
    real(fp), dimension(nlb:nub, mlb:mub)              , intent (out) :: wssus
    real(fp), dimension(nlb:nub, mlb:mub)              , intent (in)  :: wstau
    real(fp), dimension(nlb:nub, mlb:mub, num_layers_grid, lstsci), intent (in)  :: r0
    real(fp), dimension(nlb:nub, mlb:mub, num_layers_grid)        , intent (in)  :: u0
    real(fp), dimension(nlb:nub, mlb:mub, num_layers_grid)        , intent (out) :: usus
    real(fp), dimension(nlb:nub, mlb:mub, num_layers_grid)        , intent (in)  :: v0
    real(fp), dimension(nlb:nub, mlb:mub, num_layers_grid)        , intent (out) :: vsus
!
! Local variables
!
    integer                                        :: current_step
    integer                                        :: last_step
    integer                                        :: process_mode
    real(hp)         , dimension(nlb:nub, mlb:mub) :: cfurou2
    real(hp)         , dimension(nlb:nub, mlb:mub) :: cfvrou2
    real(hp)         , dimension(nlb:nub, mlb:mub) :: entr_dbl
    real(hp)         , dimension(nlb:nub, mlb:mub) :: r0_dbl
    real(hp)         , dimension(nlb:nub, mlb:mub) :: s0_dbl
    real(hp)         , dimension(nlb:nub, mlb:mub) :: u0kmax
    real(hp)         , dimension(nlb:nub, mlb:mub) :: v0kmax
    real(hp)         , dimension(nlb:nub, mlb:mub) :: ws_dbl
    real(hp), pointer, dimension(:, :)             :: loc_ptr_czu
    real(hp), pointer, dimension(:, :)             :: loc_ptr_czv
    real(hp), pointer, dimension(:, :)             :: loc_ptr_entr
    real(hp), pointer, dimension(:, :)             :: loc_ptr_rsed
    real(hp), pointer, dimension(:, :)             :: loc_ptr_s
    real(hp), pointer, dimension(:, :)             :: loc_ptr_u
    real(hp), pointer, dimension(:, :)             :: loc_ptr_v
    real(hp), pointer, dimension(:, :)             :: loc_ptr_ws
!
!! executable statements -------------------------------------------------------
!
    process_mode = parallel
    !
    current_step = 2*(timnow - itstrt) ! times 2 since actual stepping is by half steps
    last_step    = 2*(itstop - itstrt)
    !
    if (process_mode==parallel) then
       !
       ! Solve the top (flow) and bottom (fluid mud) layers at the same time
       !
       if (mudlay) then
          !
          ! This is the bottom (fluid mud) layer.
          !
          if (current_step==1) then
             if (dio2dfget(comminfo%czuset, loc_ptr_czu) .and.                  &
               & dio2dfget(comminfo%czvset, loc_ptr_czv)) then
                czusus = loc_ptr_czu
                czvsus = loc_ptr_czv
             else
                write (*, '(a)') 'ERROR while getting CZusus or CZvsus'
                call cstop(3, '      DelftIO communication failed')
             endif
          endif
          !
          if (dio2dfget(comminfo%uset, loc_ptr_u) .and.                         &
            & dio2dfget(comminfo%vset, loc_ptr_v)) then
             usus(:, :, 1) = loc_ptr_u
             vsus(:, :, 1) = loc_ptr_v
          else
             write (*, '(a)') 'ERROR while getting Usus or Vsus'
             call cstop(3, '      DelftIO communication failed')
          endif
          if (dio2dfget(comminfo%sset, loc_ptr_s)) then
             sepsus = loc_ptr_s
          else
             write (*, '(a)') 'ERROR while getting SEPSUS'
             call cstop(3, '      DelftIO communication failed')
          endif
          if (dio2dfget(comminfo%rsedset, loc_ptr_rsed)) then
             rsed = loc_ptr_rsed
          else
             write (*, '(a)') 'ERROR while getting Rsed'
             call cstop(3, '      DelftIO communication failed')
          endif
          if (dio2dfget(comminfo%wsset, loc_ptr_ws)) then
             wssus = loc_ptr_ws
          else
             write (*, '(a)') 'ERROR while getting wssus'
             call cstop(3, '      DelftIO communication failed')
          endif
          if (current_step>1) then
             entr_dbl = entr
             call dio2dfput(comminfo%entrset, entr_dbl)
          else
             entr = 0.
          endif
       else
          !
          ! This is the top (flow/suspension) layer.
          !
          if (current_step==1) then
             cfurou2 = cfurou(:, :, 2)
             ! Select chezy field; rouflo is not checked
             cfvrou2 = cfvrou(:, :, 2)
             call dio2dfput(comminfo%czuset, cfurou2)
             call dio2dfput(comminfo%czvset, cfvrou2)
          endif
          u0kmax = u0(:, :, num_layers_grid)       ! Select flow field of bottom layer
          v0kmax = v0(:, :, num_layers_grid)
          call dio2dfput(comminfo%uset, u0kmax)
          call dio2dfput(comminfo%vset, v0kmax)
          s0_dbl = s0
          call dio2dfput(comminfo%sset, s0_dbl)
          r0_dbl = r0(:, :, num_layers_grid, max(lsal, ltem) + 1)
          call dio2dfput(comminfo%rsedset, r0_dbl)
          ws_dbl = wstau(:, :)
          call dio2dfput(comminfo%wsset, ws_dbl)
          if (current_step>1) then
             if (dio2dfget(comminfo%entrset, loc_ptr_entr)) then
                entr = loc_ptr_entr
             else
                write (*, '(a)') 'ERROR while getting Entr'
                call cstop(3, '      DelftIO communication failed')
             endif
          endif
       endif
    elseif (process_mode==sequential) then
       !
       ! Solve Flow Sed and Flow Mud in a parallel way
       write (*, '(a)') 'ERROR Fluid Mud Sequential mode not implemented yet.'
       call cstop(3, '      DelftIO communication failed')
    else
       write (*, '(a)')                                                         &
              &'ERROR Fluid Mud: Parallel nor sequential process mode selected.'
       call cstop(3, '      DelftIO communication failed')
    endif
    !
end subroutine syncom



end module sync_flm
