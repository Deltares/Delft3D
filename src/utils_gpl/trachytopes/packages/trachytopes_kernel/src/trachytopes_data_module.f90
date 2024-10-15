module trachytopes_data_module
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
!  
!  
!!--module description----------------------------------------------------------
!
! This module defines the data structures for the trachytopes      
!
!!--module declarations---------------------------------------------------------

use precision
use handles, only:handletype
use properties, only:tree_data

! 
! public data 
!
implicit none 
integer, parameter, public :: TRACHY_NOT_IN_SUBDOMAIN = -77777     ! link not in subdomain
integer, parameter, public :: TRACHY_MISSING_VALUE    = -88888     ! missing value 
integer, parameter, public :: TRACHY_UNDEFINED        = -99999     ! not defined
integer, parameter, public :: TRACHY_WATERLEVEL_TYPE  = 1
integer, parameter, public :: TRACHY_DISCHARGE_TYPE   = 2
!
! public data types
!
public trachy_type_gen
public trachy_type_dir
public trachy_type

!
! public routines
!
public inittrachy
public clrtrachy

!
!integer, parameter, public :: RP_TIME  =  1
!integer, parameter, public :: RP_EFUMN =  2
!
type trachy_type_gen
    !
    ! doubles
    !
    real(fp) :: alf_area_ser !  averaging coefficient serial Chezy for areas
    real(fp) :: trtminh      !  minimum waterdepth in roughness formulations
    real(fp) :: dttrt        !  time-step for updating trachytopes roughness
    !
    ! reals
    !
    !
    ! integers
    !
    integer  :: iarea_avg !  indicator which type of averaging is used
                          !  for the area roughnesses
                          !  1: averaging based on Nikuradse k
                          !  2: averaging based on serial and parallel
                          !     Chezy C
    integer  :: max_cl    !  maximum recursion limit for nested definitions
    integer  :: nroupa    !  maximum number of roughness parameters per
                          !  trachytope definition
    integer  :: ntrt      !  number of trachytope definitions
    integer  :: nodir     !  number of directional input (1: unstructured, 2: structured (curvilinear))
    integer  :: ntrtnrm   !  number of normal trachytope definitions
    integer  :: ntrtcrs   !  number of discharge dependent trachytope definitions
    integer  :: ntrtobs   !  number of waterlevel dependent trachytope definitions
    integer  :: n_q           !  number of discharge dependent table entries
    integer  :: n_zs          !  number of waterlevel dependent table entries
    real(fp), dimension(:)    , pointer :: table_zs      ! waterlevel array 
    real(fp), dimension(:)    , pointer :: table_q       ! discharge array 
    real(fp), dimension(:,:)  , pointer :: rttdef_zs     ! waterlevel dependent coeffient table for trachytopes
    real(fp), dimension(:,:)  , pointer :: rttdef_q      ! discharge dependent coeffient table for trachytopes
    real(fp), dimension(:,:)  , pointer :: rttdef_zs_slope  ! slope of coeffient table depending on waterlevel for trachytopes
    real(fp), dimension(:,:)  , pointer :: rttdef_q_slope   ! slope of coeffient table depending on discharge for trachytopes
    real(fp), dimension(:,:)  , pointer :: rttdef_zs_cross  ! cross of coeffient table depending on waterlevel for trachytopes
    real(fp), dimension(:,:)  , pointer :: rttdef_q_cross   ! cross of coeffient table depending on discharge for trachytopes
    real(fp), dimension(:)    , pointer :: ittdef_zs     ! waterlevel dependent eq def table for trachytopes for error checking 
    real(fp), dimension(:)    , pointer :: ittdef_q      ! discharge dependent eq def table for trachytopes for error checking 
    !
    ! pointers
    !
    integer , dimension(:,:)  , pointer :: ittdef !  Trachytope definitions (integer data)
    real(fp), dimension(:,:)  , pointer :: rttdef !  Trachytope definitions (real data)
    real(fp), dimension(:)    , pointer :: vegh2d !  Vegetation height on 2D grid (coming from WAQ)
    real(fp), dimension(:)    , pointer :: vden2d !  Vegetation density (nD/m2) on 2D grid (coming from WAQ)
    real(fp), dimension(:)    , pointer :: fraccu_list !  Relative fraction (used for combined definitions)
    integer , dimension(:)    , pointer :: itrt_list   !  Roughness definition or roughness definition
    ! 
    ! logicals
    !
    logical  :: flsedprop_rqrd    !  flag indicating whether one (or more) of the (alluvial)
                                  !  trachytope  definitions require sediment properties
    !
    ! characters
    !
    character(256)                     :: md_ttdfile ! Name of trachytopes definition file. 
    
    !
    ! types     
    !
    type(trachy_crs_obs_name), dimension(:),  pointer :: crs   !  Names of crossections for discharge-dependent roughnesses
    type(trachy_crs_obs_name), dimension(:),  pointer :: obs   !  Names of observation-station for waterlevel-dependent roughnesses

    
end type trachy_type_gen

type trachy_crs_obs_name
    !
    character(64)                 :: name                  ! Name 
    integer                       :: id                    ! ID to connect with arrays in FM and Delft3D
    character(132)                :: rec132                ! Record string from ttd-file for error messages
    real(fp)                      :: val                   ! Actual discharge or waterlevel value
    integer                       :: itrt                  ! index to link to table of all trachtopes definitions 
    integer                       :: idx_start             ! start index for interpolation in q_table / zs_table 
    integer                       :: idx_end               ! end index for interpolation in q_table / zs_table 
    integer                       :: nropars               ! number of roughness parameters
    ! 
end type trachy_crs_obs_name


type trachy_type_dir
    !
    ! doubles
    !
    !
    ! reals
    !
    !
    ! integers
    !
    integer  :: nttaru    !  number of roughness records in M direction
    !
    ! pointers
    !
    integer , dimension(:,:)  , pointer :: ittaru  !  Trachytope areas in M-direction (integer data)
    integer , dimension(:)    , pointer :: kcu_trt !  Net link property    ! In Delft3D: kcu = 1 (active), kcu = 0 (inactive=thindam), kcu = -1 (Halo area), >= 3 (dd-coupling)
    real(fp), dimension(:)    , pointer :: rgcalu  !  Calibration matrix for trachytope roughness
                                                   !  in M-direction
    real(fp), dimension(:)    , pointer :: rttaru  !  Trachytope areas in M-direction (real data)
    real(fp), dimension(:,:)  , pointer :: rttxyz  !  Trachytope x,y,z coordinates at velocity point  (z coordinate is not used presently)
    real(fp), dimension(:,:)  , pointer :: rttfu   !  Trachytope resistance 3D in M-direction
    !
    ! extra grid info. (move to ??)
    ! 
    integer , dimension(:,:)   , pointer :: lin      !  neighboring flow elements (1:2,L)
    real(fp), dimension(:)     , pointer :: acLin    !  weight factor (Value on link is acl(L)*V(ln(1,L)) + (1d0-acl(L))*V(ln(2,L)) ? 
    real(fp), dimension(:)     , pointer :: blu_trt  !  bed level in u point  
    real(fp), dimension(:)     , pointer :: zsu_prev !  water level in u point from previous function call
    ! 
    ! logicals
    !
    !
    ! characters
    !
    character(256)                     :: md_arlfile ! Name of trachytopes area definition file. 
    !
    !

end type trachy_type_dir


type trachy_type
    !
    ! types
    !
    type(trachy_type_gen)                 , pointer :: gen   !  General part of trachytopes
    type(trachy_type_dir) , dimension(:)  , pointer :: dir   !  Directional part of the roughness (1d for unstructured, 2d for structured/curvilinear)
    !
    !    
end type trachy_type


contains
!
!
!
!============================================================================== 
subroutine inittrachy(gdtrachy, nodir, istat)
!!--description-----------------------------------------------------------------
!
! NONE
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    implicit none
    !
    type(trachy_type), intent(inout) :: gdtrachy
    integer                          :: nodir
    integer                          :: istat    
    integer                          :: j
!
! Global variables
!
!! executable statements -------------------------------------------------------
!
    !
    ! General part 
    ! 
    allocate(gdtrachy%gen        , stat = istat)
    !
    nullify(gdtrachy%gen%vegh2d)
    nullify(gdtrachy%gen%vden2d)
    nullify(gdtrachy%gen%ittdef)
    nullify(gdtrachy%gen%rttdef)
    !
    gdtrachy%gen%nodir = nodir
    gdtrachy%gen%flsedprop_rqrd = .false. 
    !
    ! Directional part 
    ! 
    allocate(gdtrachy%dir(nodir) , stat = istat)
    !
    do j = 1, nodir
      nullify(gdtrachy%dir(j)%ittaru)
      nullify(gdtrachy%dir(j)%rgcalu)
      nullify(gdtrachy%dir(j)%rttaru)
      nullify(gdtrachy%dir(j)%rttxyz)
      nullify(gdtrachy%dir(j)%rttfu)
      nullify(gdtrachy%dir(j)%lin)
      nullify(gdtrachy%dir(j)%acLin)
      nullify(gdtrachy%dir(j)%blu_trt)
      nullify(gdtrachy%dir(j)%zsu_prev)
      nullify(gdtrachy%dir(j)%kcu_trt)
    end do
    !   
end subroutine inittrachy
!
!
!
!============================================================================== 
!
!
!============================================================================== 
subroutine clrtrachy(istat, gdtrachy)
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    implicit none
    !
    type(trachy_type),   target :: gdtrachy
!
! Global variables
!
    integer,intent(out) :: istat
    integer             :: j
!
!! executable statements -------------------------------------------------------
!
    if (associated(gdtrachy%dir)) then
       do j = 1, size(gdtrachy%dir)
          if (associated(gdtrachy%dir(j)%ittaru))     deallocate(gdtrachy%dir(j)%ittaru,      STAT = istat)
          if (associated(gdtrachy%dir(j)%rgcalu))     deallocate(gdtrachy%dir(j)%rgcalu,      STAT = istat)
          if (associated(gdtrachy%dir(j)%rttaru))     deallocate(gdtrachy%dir(j)%rttaru,      STAT = istat)
          if (associated(gdtrachy%dir(j)%rttxyz))     deallocate(gdtrachy%dir(j)%rttxyz,      STAT = istat)
          if (associated(gdtrachy%dir(j)%rttfu))      deallocate(gdtrachy%dir(j)%rttfu ,      STAT = istat)
          if (associated(gdtrachy%dir(j)%lin))        deallocate(gdtrachy%dir(j)%lin   ,      STAT = istat)
          if (associated(gdtrachy%dir(j)%acLin))      deallocate(gdtrachy%dir(j)%acLin ,      STAT = istat)
          if (associated(gdtrachy%dir(j)%blu_trt))    deallocate(gdtrachy%dir(j)%blu_trt ,    STAT = istat)
          if (associated(gdtrachy%dir(j)%kcu_trt))    deallocate(gdtrachy%dir(j)%kcu_trt ,    STAT = istat)
          if (associated(gdtrachy%dir(j)%zsu_prev))   deallocate(gdtrachy%dir(j)%zsu_prev,    STAT = istat)
       enddo
       deallocate(gdtrachy%dir, STAT = istat)
    endif
    !!
    if (associated(gdtrachy%gen%vegh2d))     deallocate (gdtrachy%gen%vegh2d , STAT = istat)
    if (associated(gdtrachy%gen%vden2d))     deallocate (gdtrachy%gen%vden2d , STAT = istat)
    if (associated(gdtrachy%gen%ittdef))     deallocate (gdtrachy%gen%ittdef , STAT = istat)
    if (associated(gdtrachy%gen%rttdef))     deallocate (gdtrachy%gen%rttdef , STAT = istat)
    if (associated(gdtrachy%gen%itrt_list))  deallocate (gdtrachy%gen%itrt_list , STAT = istat)
    if (associated(gdtrachy%gen%fraccu_list)) deallocate (gdtrachy%gen%fraccu_list , STAT = istat)    
    if (associated(gdtrachy%gen))            deallocate (gdtrachy%gen        , STAT = istat)
    !
end subroutine clrtrachy
!
!
!

end module trachytopes_data_module
