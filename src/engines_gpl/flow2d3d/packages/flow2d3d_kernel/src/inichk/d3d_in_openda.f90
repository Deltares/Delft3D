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
module m_openda_quantities
! quantity-id's : in (from Delft3D to openDA)

integer, parameter :: waterlevel = 1
integer, parameter :: x_velocity = 2   
integer, parameter :: y_velocity = 3
integer, parameter :: x_discharge = 4
integer, parameter :: y_discharge = 5



!quantity-id's : out (from openDA to Delft3D)
integer, parameter :: src_discharge = 6
integer, parameter :: windu         = 7
integer, parameter :: windv         = 8
integer, parameter :: bound_HQ      = 9   !gives one value per timestep: both end A and end B
                                         

integer, parameter :: bound_temp    = 10  !gives one value per timestep: both end A and end B
integer, parameter :: bound_salt    = 11  !gives one value per timestep: both end A and end B
integer, parameter :: bound_astroH  = 12  !gives one value per timestep: both end A and end B

integer, parameter :: windgu         = 13
integer, parameter :: x_windgu       = 14
integer, parameter :: y_windgu       = 15
integer, parameter :: windgv         = 16
integer, parameter :: x_windgv       = 17
integer, parameter :: y_windgv       = 18

integer, parameter :: max_quantity_ids = 18
integer, parameter :: max_location_ids = 200


! possible operation for modifying the boundary values
integer, parameter :: oper_set      = 1
integer, parameter :: oper_add      = 2
integer, parameter :: oper_multiply = 3


end module m_openda_quantities

!-----------------------------------

module m_openda_olv

use d3d_olv_class

type(OLVHandle), save :: openda_olv_handle

end module m_openda_olv

!-----------------------------------

module m_openda_exchange_items

use m_openda_quantities

logical :: l_ei(max_location_ids,max_quantity_ids)

logical :: doLogging = .false.

double precision :: ei_val(max_location_ids,max_quantity_ids)
integer          :: ei_oper(max_location_ids,max_quantity_ids)

integer          :: ei_locations_per_quantity(max_quantity_ids)

contains 

!------------------------

  subroutine set_openda_buffer(vals, nvals, location_id,quantity_id, operation)
! set the value of an exchange-item. This routine is typically called
! by an SE_setvalues routine (from OUTSIDE delft3D!) for a certain instance and exchange item (e.g. wind)
! In the case of forcings, the value can be a multiplier (1 + epsilon)
! Note that the actual adjustment INSIDE delft3d is not yet performed here.
! for the wind example, this is done in incmeteo with a call to get_openda_buffer.
! The multiplication is performed in this routine get_openda_buffer.

! For a specific boundary, the location_id is used (can be found by counting in the BND-file)
! for other boundaries, the l_ei remains false so they do not change. 
 
  implicit none
  
  integer, intent(in)                            :: location_id   !   location identifier
  integer, intent(in)                            :: quantity_id   !   quantity identifier
  integer, intent(in)                            :: nvals        
  double precision, dimension(nvals), intent(in) :: vals          !   values to be set
  integer, intent(in)                            :: operation     !   operation: oper_multiply, oper_add, oper_set
  
!------ local variables
  integer                                        :: iloc
  integer                                        :: ngrid  
  
  if (doLogging) then
     print *, 'set_openda_buffer, loc-id=', location_id, ', q_id=', quantity_id, ', val=', vals(1), ', oper:', operation
     ! flush is no longer a standard intrinsic Fortran routine
     ! call flush(6)
  endif
  
  if (nvals .eq. 1) then !standard situation
     l_ei(location_id,quantity_id) = .true.
     ei_val(location_id,quantity_id) = vals(1)
     ei_oper(location_id,quantity_id) = operation
  else
      if (quantity_id /= windgu .and. quantity_id /=windgv) then
        print *,'ERROR: set_openda_buffer: number of noise parameters should be 1 if no noise grid is present'
        call throwexception()
      endif
      ! specific situation of a noise grid. The array vals in fact consists of triples
      ! (xloc, yloc, value)
      ! we  can use ei_val, to store these values. We need to store the xloc, yloc in some empty rows 
      ! of ei_val. 
      ! Be careful: we demand that nvals/3 <= max_location_ids!
      ngrid = nvals/3
      if (ngrid*3 /= nvals) then
         print *,'ERROR set_openda_buffer: providing noise grid: number of grid parameters is inconsistent with meta information'
         l_ei(1:max_location_ids,quantity_id) = .false.
      endif
      if (ngrid > max_location_ids) then
          print *,'ERROR set_openda_buffer: noise grid is too large. Increase the allowed number of locations.'
          l_ei(1:max_location_ids,quantity_id) = .false.
      endif
      do iloc = 1, ngrid
         l_ei(iloc, quantity_id:quantity_id+2) = .true. ! set flag no true for both value and the locations
         ei_val(iloc, quantity_id) = vals(iloc*3)
         ei_val(iloc, quantity_id+1) = vals(iloc*3-2)   !quantity_id+1 is location for x_windgu or x_windgv
         ei_val(iloc, quantity_id+2) = vals(iloc*3-1)   !quantity_id+2 is location for y_windgu or y_windgv
       
         ei_oper(iloc, quantity_id) = operation      
         ei_oper(iloc, quantity_id+1) = oper_set      ! the locations are always set by openda
         ei_oper(iloc, quantity_id+2) = oper_set      !    
      enddo
      ei_locations_per_quantity(quantity_id) = ngrid
  endif

  end subroutine set_openda_buffer
!---------------------------------------------------

  function get_openda_buffersize(str_quantity) result (bsize)
  !result
  integer bsize

  character(*) , intent (in) :: str_quantity  
  
  if (str_quantity .eq. 'windgu') then
     bsize = ei_locations_per_quantity(windgu)
  elseif (str_quantity .eq. 'windgv') then   
     bsize = ei_locations_per_quantity(windgv)
  else
     bsize = 1 
  endif  
    
  end function get_openda_buffersize
!---------------------------------------------------  

  subroutine get_openda_buffer(str_quantity, loc_from_d3d, dim1, dim2, qarray)
  
  use precision
  
  implicit none
  
  integer, intent(in)    :: dim1, dim2, loc_from_d3d
  character(*) , intent (in) :: str_quantity
  real(fp)    , dimension(dim1, dim2), target, intent(out) :: qarray 
  
  ! locals
  integer          :: location_id, quantity_id
  double precision :: org_value
  


  select case (str_quantity)
       
  case ('windu')
    location_id = 1
    quantity_id = windu 
  case ('windv') 
    location_id = 1
    quantity_id = windv 
  case ('windgu') 
    location_id = loc_from_d3d
    quantity_id = windgu 
  case ('windgv') 
    location_id = loc_from_d3d
    quantity_id = windgv 
  case ('x_windgu') 
    location_id = loc_from_d3d
    quantity_id = x_windgu 
  case ('x_windgv') 
    location_id = loc_from_d3d
    quantity_id = x_windgv 
  case('y_windgu') 
    location_id = loc_from_d3d
    quantity_id = y_windgu 
  case('y_windgv')
    location_id = loc_from_d3d
    quantity_id = y_windgv 
  case('bound_HQ') 
    location_id = loc_from_d3d
    quantity_id = bound_HQ
  case('bound_temp') 
    location_id = loc_from_d3d
    quantity_id = bound_temp 
  case('bound_salt') 
     location_id = loc_from_d3d
     quantity_id = bound_salt 
  case('bound_astroH') 
     location_id = loc_from_d3d
     quantity_id = bound_astroH 
  case default 
     location_id = -1
     quantity_id = -1
  endselect
  
  !print *, 'get_openda_buffer, loc-id=', location_id, ', q_id=', quantity_id
  !call flush(6)

  if (location_id == -1 .or. quantity_id == -1) then
       print *, 'EI get_openda_buffer, INVALID ITEM: loc-id=', location_id, ', q_id=', quantity_id
  else
    if (l_ei(location_id,quantity_id)) then
       org_value = qarray(1,1)
       select case (ei_oper(location_id,quantity_id))
       case(oper_set)
           qarray = ei_val(location_id, quantity_id) 
       case(oper_add)
           qarray = qarray + ei_val(location_id, quantity_id) 
       case(oper_multiply)
           qarray = qarray * ei_val(location_id, quantity_id) 
       case default
          print *, 'get_openda_buffer: UNKNOWN OPERATION type: ', ei_oper(location_id,quantity_id)
       endselect
       if (doLogging) then
          print *, 'EI adjusted, loc-id=', location_id, ', q_id=', quantity_id, ', was: ', org_value, ', is:', qarray(1,1)
!          call flush(6)
       endif
    endif
  endif
  
  end subroutine get_openda_buffer

! ------------------


   subroutine openda_buffer_initialize
 
   implicit none
   
   l_ei = .false.  
   ei_locations_per_quantity = 0

   end subroutine openda_buffer_initialize
   
!--------------------------
end module m_openda_exchange_items

!================================================================================


subroutine compute_secundary_state(gdp       )
!!--description-----------------------------------------------------------------
!
!    Function: compute secundary state variables; called from the 'setstate'routine

!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use meteo
    use flow_tables
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    include 'fsm.i'
    integer(pntrsize)                    , pointer :: wrka1
    integer(pntrsize)                    , pointer :: wrka2
    integer(pntrsize)                    , pointer :: wrka3
    integer(pntrsize)                    , pointer :: wrkb1
    integer(pntrsize)                    , pointer :: wrkb2
    integer(pntrsize)                    , pointer :: wrkb3
    integer(pntrsize)                    , pointer :: wrkb4
    integer(pntrsize)                    , pointer :: zwork
    integer(pntrsize)                    , pointer :: alfas
    integer(pntrsize)                    , pointer :: areau
    integer(pntrsize)                    , pointer :: areav
    integer(pntrsize)                    , pointer :: bruvai
    integer(pntrsize)                    , pointer :: c
    integer(pntrsize)                    , pointer :: cdwlsu
    integer(pntrsize)                    , pointer :: cdwlsv
    integer(pntrsize)                    , pointer :: cdwzbu
    integer(pntrsize)                    , pointer :: cdwzbv
    integer(pntrsize)                    , pointer :: cdwztu
    integer(pntrsize)                    , pointer :: cdwztv
    integer(pntrsize)                    , pointer :: cfurou
    integer(pntrsize)                    , pointer :: cfvrou
    integer(pntrsize)                    , pointer :: cvalu0
    integer(pntrsize)                    , pointer :: cvalv0
    integer(pntrsize)                    , pointer :: dddeta
    integer(pntrsize)                    , pointer :: dddksi
    integer(pntrsize)                    , pointer :: deltau
    integer(pntrsize)                    , pointer :: deltav
    integer(pntrsize)                    , pointer :: dfu
    integer(pntrsize)                    , pointer :: dfv
    integer(pntrsize)                    , pointer :: diapl
    integer(pntrsize)                    , pointer :: dicuv
    integer(pntrsize)                    , pointer :: dicww
    integer(pntrsize)                    , pointer :: dis
    integer(pntrsize)                    , pointer :: disch
    integer(pntrsize)                    , pointer :: discum
    integer(pntrsize)                    , pointer :: dpd
    integer(pntrsize)                    , pointer :: dps
    integer(pntrsize)                    , pointer :: dpu
    integer(pntrsize)                    , pointer :: dpv
    integer(pntrsize)                    , pointer :: dzdeta
    integer(pntrsize)                    , pointer :: dzdksi
    integer(pntrsize)                    , pointer :: enstro
    integer(pntrsize)                    , pointer :: eroll0
    integer(pntrsize)                    , pointer :: eroll1
    integer(pntrsize)                    , pointer :: ewabr0
    integer(pntrsize)                    , pointer :: ewabr1
    integer(pntrsize)                    , pointer :: ewave0
    integer(pntrsize)                    , pointer :: ewave1
    integer(pntrsize)                    , pointer :: grmasu
    integer(pntrsize)                    , pointer :: grmasv
    integer(pntrsize)                    , pointer :: grmsur
    integer(pntrsize)                    , pointer :: grmsvr
    integer(pntrsize)                    , pointer :: grfacu
    integer(pntrsize)                    , pointer :: grfacv
    integer(pntrsize)                    , pointer :: gsqs
    integer(pntrsize)                    , pointer :: guu
    integer(pntrsize)                    , pointer :: guv
    integer(pntrsize)                    , pointer :: gvu
    integer(pntrsize)                    , pointer :: gvv
    integer(pntrsize)                    , pointer :: hkru
    integer(pntrsize)                    , pointer :: hkrv
    integer(pntrsize)                    , pointer :: hrms
    integer(pntrsize)                    , pointer :: hu
    integer(pntrsize)                    , pointer :: hu0
    integer(pntrsize)                    , pointer :: hv
    integer(pntrsize)                    , pointer :: hv0
    integer(pntrsize)                    , pointer :: patm
    integer(pntrsize)                    , pointer :: porosu
    integer(pntrsize)                    , pointer :: porosv
    integer(pntrsize)                    , pointer :: qu
    integer(pntrsize)                    , pointer :: qv
    integer(pntrsize)                    , pointer :: qxk
    integer(pntrsize)                    , pointer :: qyk
    integer(pntrsize)                    , pointer :: r0
    integer(pntrsize)                    , pointer :: r1
    integer(pntrsize)                    , pointer :: rho
    integer(pntrsize)                    , pointer :: rhowat
    integer(pntrsize)                    , pointer :: rich
    integer(pntrsize)                    , pointer :: rlabda
    integer(pntrsize)                    , pointer :: rnpl
    integer(pntrsize)                    , pointer :: rob
    integer(pntrsize)                    , pointer :: rtur0
    integer(pntrsize)                    , pointer :: rtur1
    integer(pntrsize)                    , pointer :: s0
    integer(pntrsize)                    , pointer :: s1
    integer(pntrsize)                    , pointer :: sig
    integer(pntrsize)                    , pointer :: sumrho
    integer(pntrsize)                    , pointer :: taubmx
    integer(pntrsize)                    , pointer :: taubpu
    integer(pntrsize)                    , pointer :: taubpv
    integer(pntrsize)                    , pointer :: taubsu
    integer(pntrsize)                    , pointer :: taubsv
    integer(pntrsize)                    , pointer :: teta
    integer(pntrsize)                    , pointer :: thick
    integer(pntrsize)                    , pointer :: tp
    integer(pntrsize)                    , pointer :: u0
    integer(pntrsize)                    , pointer :: u1
    integer(pntrsize)                    , pointer :: ubrlsu
    integer(pntrsize)                    , pointer :: ubrlsv
    integer(pntrsize)                    , pointer :: umean
    integer(pntrsize)                    , pointer :: umnflc
    integer(pntrsize)                    , pointer :: umnldf
    integer(pntrsize)                    , pointer :: uorb
    integer(pntrsize)                    , pointer :: v0
    integer(pntrsize)                    , pointer :: v1
    integer(pntrsize)                    , pointer :: vicuv
    integer(pntrsize)                    , pointer :: vmean
    integer(pntrsize)                    , pointer :: vmnflc
    integer(pntrsize)                    , pointer :: vmnldf
    integer(pntrsize)                    , pointer :: volum0
    integer(pntrsize)                    , pointer :: volum1
    integer(pntrsize)                    , pointer :: vortic
    integer(pntrsize)                    , pointer :: w1
    integer(pntrsize)                    , pointer :: w10mag
    integer(pntrsize)                    , pointer :: windsu
    integer(pntrsize)                    , pointer :: windsv
    integer(pntrsize)                    , pointer :: windu
    integer(pntrsize)                    , pointer :: windv
    integer(pntrsize)                    , pointer :: ws
    integer(pntrsize)                    , pointer :: wsu
    integer(pntrsize)                    , pointer :: wsv
    integer(pntrsize)                    , pointer :: xcor
    integer(pntrsize)                    , pointer :: ycor
    integer(pntrsize)                    , pointer :: z0ucur
    integer(pntrsize)                    , pointer :: z0vcur
    integer(pntrsize)                    , pointer :: z0urou
    integer(pntrsize)                    , pointer :: z0vrou
    integer(pntrsize)                    , pointer :: drhodx
    integer(pntrsize)                    , pointer :: drhody
    integer(pntrsize)                    , pointer :: dzs0
    integer(pntrsize)                    , pointer :: dzs1
    integer(pntrsize)                    , pointer :: dzu0
    integer(pntrsize)                    , pointer :: dzu1
    integer(pntrsize)                    , pointer :: dzv0
    integer(pntrsize)                    , pointer :: dzv1
    integer(pntrsize)                    , pointer :: res
    integer(pntrsize)                    , pointer :: fact
    integer(pntrsize)                    , pointer :: rl
    integer(pntrsize)                    , pointer :: xj
    integer(pntrsize)                    , pointer :: p1
    integer(pntrsize)                    , pointer :: p0
    integer(pntrsize)                    , pointer :: w0
    integer(pntrsize)                    , pointer :: s00
    integer(pntrsize)                    , pointer :: gud
    integer(pntrsize)                    , pointer :: gvd
    integer(pntrsize)                    , pointer :: kcs
    integer(pntrsize)                    , pointer :: kcu
    integer(pntrsize)                    , pointer :: kcv
    integer(pntrsize)                    , pointer :: kfs
    integer(pntrsize)                    , pointer :: kfu
    integer(pntrsize)                    , pointer :: kfv
    integer(pntrsize)                    , pointer :: kspu
    integer(pntrsize)                    , pointer :: kspv
    integer(pntrsize)                    , pointer :: kfumin
    integer(pntrsize)                    , pointer :: kfvmin
    integer(pntrsize)                    , pointer :: kfsmin
    integer(pntrsize)                    , pointer :: kfumax
    integer(pntrsize)                    , pointer :: kfvmax
    integer(pntrsize)                    , pointer :: kfsmax
    integer(pntrsize)                    , pointer :: kfumx0
    integer(pntrsize)                    , pointer :: kfvmx0
    integer(pntrsize)                    , pointer :: kfsmx0
    integer(pntrsize)                    , pointer :: kfumn0
    integer(pntrsize)                    , pointer :: kfvmn0
    integer(pntrsize)                    , pointer :: kfsmn0
    integer(pntrsize)                    , pointer :: kfsz0
    integer(pntrsize)                    , pointer :: kfsz1
    integer(pntrsize)                    , pointer :: kfuz0
    integer(pntrsize)                    , pointer :: kfuz1
    integer(pntrsize)                    , pointer :: kfvz0
    integer(pntrsize)                    , pointer :: kfvz1
    integer(pntrsize)                    , pointer :: kcscut
    integer(pntrsize)                    , pointer :: evap
    integer(pntrsize)                    , pointer :: precip
    integer(pntrsize)                    , pointer :: sour
    integer(pntrsize)                    , pointer :: sink
    integer(pntrsize)                    , pointer :: xz
    integer(pntrsize)                    , pointer :: yz
    integer                              , pointer :: nmax
    integer                              , pointer :: mmax
    integer                              , pointer :: nlb
    integer                              , pointer :: nub
    integer                              , pointer :: mlb
    integer                              , pointer :: mub
    integer                              , pointer :: nmlb
    integer                              , pointer :: nmub
    integer                              , pointer :: ddbound
    integer                              , pointer :: nmaxus
    integer                              , pointer :: kmax
    integer                              , pointer :: nmaxd
    integer                              , pointer :: jstart
    integer                              , pointer :: nmmaxj
    integer                              , pointer :: nmmax
    integer                              , pointer :: lsts
    integer                              , pointer :: lstsc
    integer                              , pointer :: lstsci
    integer                              , pointer :: lsal
    integer                              , pointer :: lsed
    integer                              , pointer :: ltem
    integer                              , pointer :: lsecfl
    integer                              , pointer :: lsec
    integer                              , pointer :: ltur
    integer                              , pointer :: kc
    integer                              , pointer :: nsrc
    real(fp)                             , pointer :: timhr
    real(fp)                             , pointer :: timmin
    real(fp)                             , pointer :: timnow
    integer                              , pointer :: julday
    real(fp)                             , pointer :: cp
    real(fp)                             , pointer :: rhum
    real(fp)                             , pointer :: tair
    real(fp), dimension(:)               , pointer :: rhumarr
    real(fp), dimension(:)               , pointer :: tairarr
    real(fp), dimension(:)               , pointer :: clouarr
    real(fp), dimension(:)               , pointer :: swrfarr
    logical                              , pointer :: rhum_file
    logical                              , pointer :: tair_file
    logical                              , pointer :: clou_file
    logical                              , pointer :: prcp_file
    logical                              , pointer :: swrf_file
    real(fp)                             , pointer :: morfac
    integer                              , pointer :: morfacpar
    integer                              , pointer :: morfacrec
    integer                              , pointer :: morfactable
    type (handletype)                    , pointer :: morfacfile
    logical                              , pointer :: densin
    logical                              , pointer :: varyingmorfac
    integer                              , pointer :: iro
    integer                              , pointer :: irov
    real(fp)                             , pointer :: rhow
    real(fp)                             , pointer :: ag
    real(fp)                             , pointer :: z0
    real(fp)                             , pointer :: z0v
    logical                              , pointer :: wind
    logical                              , pointer :: temp
    logical                              , pointer :: wave
    logical                              , pointer :: struct
    logical                              , pointer :: cdwstruct
    logical                              , pointer :: htur2d
    logical                              , pointer :: zmodel
    logical                              , pointer :: nonhyd
    logical                              , pointer :: roller
    logical                              , pointer :: lftrto
    logical                              , pointer :: veg3d
    include 'tri-dyn.igd'
    real(fp)      , dimension(:)         , pointer :: rhosol
    integer                              , pointer :: lturi
    real(fp)                             , pointer :: grdang
    real(fp)                             , pointer :: saleqs
    real(fp)                             , pointer :: temeqs
    character*4                          , pointer :: rouflo
    character*4                          , pointer :: rouwav
    character*8                          , pointer :: dischy
    integer                                        :: nmaxddb
    integer                                        :: nst


    integer                              , pointer :: lundia
!
    integer                              , pointer :: nfltyp


    integer                              , pointer :: ktemp
    real(fp)                             , pointer :: anglat
    real(fp)                             , pointer :: anglon
    logical                              , pointer :: sferic    
    integer                              , pointer :: keva

!
! Local variables
!
    integer                            :: icx
    integer                            :: icy
    integer                            :: ierr
    integer                            :: itype
    integer                            :: ifirst_dens !  Flag to initialize the water density array

    character(256)                     :: restid_old
 
    integer, dimension(:), allocatable :: kcucopy
    integer, dimension(:), allocatable :: kcvcopy
    real(fp), dimension(1)             :: value
    character(8)                       :: stage       ! First or second half time step 
                                                      ! Stage = 'both' means that in F0ISF1 the layering administration
                                                      ! is copied for both the U- and the V-direction
!
!! executable statements -------------------------------------------------------
!
    wrka1               => gdp%gdaddress%wrka1
    wrka2               => gdp%gdaddress%wrka2
    wrka3               => gdp%gdaddress%wrka3
    wrkb1               => gdp%gdaddress%wrkb1
    wrkb2               => gdp%gdaddress%wrkb2
    wrkb3               => gdp%gdaddress%wrkb3
    wrkb4               => gdp%gdaddress%wrkb4
    zwork               => gdp%gdaddress%zwork
    nmax                => gdp%d%nmax
    mmax                => gdp%d%mmax
    nlb                 => gdp%d%nlb
    nub                 => gdp%d%nub
    mlb                 => gdp%d%mlb
    mub                 => gdp%d%mub
    nmlb                => gdp%d%nmlb
    nmub                => gdp%d%nmub
    ddbound             => gdp%d%ddbound
    nmaxus              => gdp%d%nmaxus
    kmax                => gdp%d%kmax
    nmaxd               => gdp%d%nmaxd
    jstart              => gdp%d%jstart
    nmmaxj              => gdp%d%nmmaxj
    nmmax               => gdp%d%nmmax
    lsts                => gdp%d%lsts
    lstsc               => gdp%d%lstsc
    lstsci              => gdp%d%lstsci
    lsal                => gdp%d%lsal
    lsed                => gdp%d%lsed
    ltem                => gdp%d%ltem
    lsecfl              => gdp%d%lsecfl
    lsec                => gdp%d%lsec
    ltur                => gdp%d%ltur
    kc                  => gdp%d%kc
    nsrc                => gdp%d%nsrc
    timhr               => gdp%gdinttim%timhr
    timmin              => gdp%gdinttim%timmin
    timnow              => gdp%gdinttim%timnow
    julday              => gdp%gdinttim%julday
    cp                  => gdp%gdheat%cp
    rhum                => gdp%gdheat%rhum
    tair                => gdp%gdheat%tair
    rhumarr             => gdp%gdheat%rhumarr
    tairarr             => gdp%gdheat%tairarr
    clouarr             => gdp%gdheat%clouarr
    swrfarr             => gdp%gdheat%swrfarr
    rhum_file           => gdp%gdheat%rhum_file
    tair_file           => gdp%gdheat%tair_file
    clou_file           => gdp%gdheat%clou_file
    prcp_file           => gdp%gdheat%prcp_file
    swrf_file           => gdp%gdheat%swrf_file
    morfac              => gdp%gdmorpar%morfac
    morfacpar           => gdp%gdmorpar%morfacpar
    morfacrec           => gdp%gdmorpar%morfacrec
    morfactable         => gdp%gdmorpar%morfactable
    morfacfile          => gdp%gdmorpar%morfacfile
    densin              => gdp%gdmorpar%densin
    varyingmorfac       => gdp%gdmorpar%varyingmorfac
    iro                 => gdp%gdphysco%iro
    irov                => gdp%gdphysco%irov
    rhow                => gdp%gdphysco%rhow
    ag                  => gdp%gdphysco%ag
    z0                  => gdp%gdphysco%z0
    z0v                 => gdp%gdphysco%z0v
    wind                => gdp%gdprocs%wind
    temp                => gdp%gdprocs%temp
    wave                => gdp%gdprocs%wave
    struct              => gdp%gdprocs%struct
    cdwstruct           => gdp%gdprocs%cdwstruct
    htur2d              => gdp%gdprocs%htur2d
    zmodel              => gdp%gdprocs%zmodel
    nonhyd              => gdp%gdprocs%nonhyd
    roller              => gdp%gdprocs%roller
    lftrto              => gdp%gdprocs%lftrto
    veg3d               => gdp%gdprocs%veg3d
    alfas               => gdp%gdr_i_ch%alfas
    areau               => gdp%gdr_i_ch%areau
    areav               => gdp%gdr_i_ch%areav
    bruvai              => gdp%gdr_i_ch%bruvai
    c                   => gdp%gdr_i_ch%c
    cdwlsu              => gdp%gdr_i_ch%cdwlsu
    cdwlsv              => gdp%gdr_i_ch%cdwlsv
    cdwzbu              => gdp%gdr_i_ch%cdwzbu
    cdwzbv              => gdp%gdr_i_ch%cdwzbv
    cdwztu              => gdp%gdr_i_ch%cdwztu
    cdwztv              => gdp%gdr_i_ch%cdwztv
    cfurou              => gdp%gdr_i_ch%cfurou
    cfvrou              => gdp%gdr_i_ch%cfvrou
    cvalu0              => gdp%gdr_i_ch%cvalu0
    cvalv0              => gdp%gdr_i_ch%cvalv0
    dddeta              => gdp%gdr_i_ch%dddeta
    dddksi              => gdp%gdr_i_ch%dddksi
    deltau              => gdp%gdr_i_ch%deltau
    deltav              => gdp%gdr_i_ch%deltav
    dfu                 => gdp%gdr_i_ch%dfu
    dfv                 => gdp%gdr_i_ch%dfv
    diapl               => gdp%gdr_i_ch%diapl
    dicuv               => gdp%gdr_i_ch%dicuv
    dicww               => gdp%gdr_i_ch%dicww
    dis                 => gdp%gdr_i_ch%dis
    disch               => gdp%gdr_i_ch%disch
    discum              => gdp%gdr_i_ch%discum
    dpd                 => gdp%gdr_i_ch%dpd
    dps                 => gdp%gdr_i_ch%dps
    dpu                 => gdp%gdr_i_ch%dpu
    dpv                 => gdp%gdr_i_ch%dpv
    dzdeta              => gdp%gdr_i_ch%dzdeta
    dzdksi              => gdp%gdr_i_ch%dzdksi
    enstro              => gdp%gdr_i_ch%enstro
    eroll0              => gdp%gdr_i_ch%eroll0
    eroll1              => gdp%gdr_i_ch%eroll1
    ewabr0              => gdp%gdr_i_ch%ewabr0
    ewabr1              => gdp%gdr_i_ch%ewabr1
    ewave0              => gdp%gdr_i_ch%ewave0
    ewave1              => gdp%gdr_i_ch%ewave1
    grmasu              => gdp%gdr_i_ch%grmasu
    grmasv              => gdp%gdr_i_ch%grmasv
    grmsur              => gdp%gdr_i_ch%grmsur
    grmsvr              => gdp%gdr_i_ch%grmsvr
    grfacu              => gdp%gdr_i_ch%grfacu
    grfacv              => gdp%gdr_i_ch%grfacv
    gsqs                => gdp%gdr_i_ch%gsqs
    guu                 => gdp%gdr_i_ch%guu
    guv                 => gdp%gdr_i_ch%guv
    gvu                 => gdp%gdr_i_ch%gvu
    gvv                 => gdp%gdr_i_ch%gvv
    hkru                => gdp%gdr_i_ch%hkru
    hkrv                => gdp%gdr_i_ch%hkrv
    hrms                => gdp%gdr_i_ch%hrms
    hu                  => gdp%gdr_i_ch%hu
    hu0                 => gdp%gdr_i_ch%hu0
    hv                  => gdp%gdr_i_ch%hv
    hv0                 => gdp%gdr_i_ch%hv0
    patm                => gdp%gdr_i_ch%patm
    porosu              => gdp%gdr_i_ch%porosu
    porosv              => gdp%gdr_i_ch%porosv
    qu                  => gdp%gdr_i_ch%qu
    qv                  => gdp%gdr_i_ch%qv
    qxk                 => gdp%gdr_i_ch%qxk
    qyk                 => gdp%gdr_i_ch%qyk
    r0                  => gdp%gdr_i_ch%r0
    r1                  => gdp%gdr_i_ch%r1
    rho                 => gdp%gdr_i_ch%rho
    rhowat              => gdp%gdr_i_ch%rhowat
    rich                => gdp%gdr_i_ch%rich
    rlabda              => gdp%gdr_i_ch%rlabda
    rnpl                => gdp%gdr_i_ch%rnpl
    rob                 => gdp%gdr_i_ch%rob
    rtur0               => gdp%gdr_i_ch%rtur0
    rtur1               => gdp%gdr_i_ch%rtur1
    s0                  => gdp%gdr_i_ch%s0
    s1                  => gdp%gdr_i_ch%s1
    sig                 => gdp%gdr_i_ch%sig
    sumrho              => gdp%gdr_i_ch%sumrho
    taubmx              => gdp%gdr_i_ch%taubmx
    taubpu              => gdp%gdr_i_ch%taubpu
    taubpv              => gdp%gdr_i_ch%taubpv
    taubsu              => gdp%gdr_i_ch%taubsu
    taubsv              => gdp%gdr_i_ch%taubsv
    teta                => gdp%gdr_i_ch%teta
    thick               => gdp%gdr_i_ch%thick
    tp                  => gdp%gdr_i_ch%tp
    u0                  => gdp%gdr_i_ch%u0
    u1                  => gdp%gdr_i_ch%u1
    ubrlsu              => gdp%gdr_i_ch%ubrlsu
    ubrlsv              => gdp%gdr_i_ch%ubrlsv
    umean               => gdp%gdr_i_ch%umean
    umnflc              => gdp%gdr_i_ch%umnflc
    umnldf              => gdp%gdr_i_ch%umnldf
    uorb                => gdp%gdr_i_ch%uorb
    v0                  => gdp%gdr_i_ch%v0
    v1                  => gdp%gdr_i_ch%v1
    vicuv               => gdp%gdr_i_ch%vicuv
    vmean               => gdp%gdr_i_ch%vmean
    vmnflc              => gdp%gdr_i_ch%vmnflc
    vmnldf              => gdp%gdr_i_ch%vmnldf
    volum0              => gdp%gdr_i_ch%volum0
    volum1              => gdp%gdr_i_ch%volum1
    vortic              => gdp%gdr_i_ch%vortic
    w1                  => gdp%gdr_i_ch%w1
    w10mag              => gdp%gdr_i_ch%w10mag
    windsu              => gdp%gdr_i_ch%windsu
    windsv              => gdp%gdr_i_ch%windsv
    windu               => gdp%gdr_i_ch%windu
    windv               => gdp%gdr_i_ch%windv
    ws                  => gdp%gdr_i_ch%ws
    wsu                 => gdp%gdr_i_ch%wsu
    wsv                 => gdp%gdr_i_ch%wsv
    xcor                => gdp%gdr_i_ch%xcor
    ycor                => gdp%gdr_i_ch%ycor
    z0ucur              => gdp%gdr_i_ch%z0ucur
    z0vcur              => gdp%gdr_i_ch%z0vcur
    z0urou              => gdp%gdr_i_ch%z0urou
    z0vrou              => gdp%gdr_i_ch%z0vrou
    drhodx              => gdp%gdr_i_ch%drhodx
    drhody              => gdp%gdr_i_ch%drhody
    dzs0                => gdp%gdr_i_ch%dzs0
    dzs1                => gdp%gdr_i_ch%dzs1
    dzu0                => gdp%gdr_i_ch%dzu0
    dzu1                => gdp%gdr_i_ch%dzu1
    dzv0                => gdp%gdr_i_ch%dzv0
    dzv1                => gdp%gdr_i_ch%dzv1
    res                 => gdp%gdr_i_ch%res
    fact                => gdp%gdr_i_ch%fact
    rl                  => gdp%gdr_i_ch%rl
    xj                  => gdp%gdr_i_ch%xj
    p1                  => gdp%gdr_i_ch%p1
    p0                  => gdp%gdr_i_ch%p0
    w0                  => gdp%gdr_i_ch%w0
    s00                 => gdp%gdr_i_ch%s00
    gud                 => gdp%gdr_i_ch%gud
    gvd                 => gdp%gdr_i_ch%gvd
    kcs                 => gdp%gdr_i_ch%kcs
    kcu                 => gdp%gdr_i_ch%kcu
    kcv                 => gdp%gdr_i_ch%kcv
    kfs                 => gdp%gdr_i_ch%kfs
    kfu                 => gdp%gdr_i_ch%kfu
    kfv                 => gdp%gdr_i_ch%kfv
    kspu                => gdp%gdr_i_ch%kspu
    kspv                => gdp%gdr_i_ch%kspv
    kfumin              => gdp%gdr_i_ch%kfumin
    kfvmin              => gdp%gdr_i_ch%kfvmin
    kfsmin              => gdp%gdr_i_ch%kfsmin
    kfumax              => gdp%gdr_i_ch%kfumax
    kfvmax              => gdp%gdr_i_ch%kfvmax
    kfsmax              => gdp%gdr_i_ch%kfsmax
    kfumx0              => gdp%gdr_i_ch%kfumx0
    kfvmx0              => gdp%gdr_i_ch%kfvmx0
    kfsmx0              => gdp%gdr_i_ch%kfsmx0
    kfumn0              => gdp%gdr_i_ch%kfumn0
    kfvmn0              => gdp%gdr_i_ch%kfvmn0
    kfsmn0              => gdp%gdr_i_ch%kfsmn0
    kfsz0               => gdp%gdr_i_ch%kfsz0
    kfsz1               => gdp%gdr_i_ch%kfsz1
    kfuz0               => gdp%gdr_i_ch%kfuz0
    kfuz1               => gdp%gdr_i_ch%kfuz1
    kfvz0               => gdp%gdr_i_ch%kfvz0
    kfvz1               => gdp%gdr_i_ch%kfvz1
    kcscut              => gdp%gdr_i_ch%kcscut
    rhosol              => gdp%gdsedpar%rhosol
    lturi               => gdp%gdtricom%lturi
    grdang              => gdp%gdtricom%grdang
    saleqs              => gdp%gdtricom%saleqs
    temeqs              => gdp%gdtricom%temeqs
    rouflo              => gdp%gdtricom%rouflo
    rouwav              => gdp%gdtricom%rouwav
    dischy              => gdp%gdtricom%dischy

    lundia              => gdp%gdinout%lundia
    
    nfltyp              => gdp%gdtricom%nfltyp
    
    evap                => gdp%gdr_i_ch%evap
    precip              => gdp%gdr_i_ch%precip
    ktemp               => gdp%gdtricom%ktemp
    anglat              => gdp%gdtricom%anglat
    anglon              => gdp%gdtricom%anglon
    sferic              => gdp%gdtricom%sferic
    keva                => gdp%gdtricom%keva    
    sour                => gdp%gdr_i_ch%sour    
    sink                => gdp%gdr_i_ch%sink
    xz                  => gdp%gdr_i_ch%xz
    yz                  => gdp%gdr_i_ch%yz
    !
    ! First repair some output variables due to 
    ! inconsistent usage
    !
    if (gdp%gdinttim%ithisi .eq.0) gdp%gdtricom%ithisc = -1
    !
    nmaxddb = nmax + 2*gdp%d%ddbound
    allocate(kcucopy(nmlb:nmub))
    allocate(kcvcopy(nmlb:nmub))
    call copykcuv(i(kcu), kcucopy, gdp)
    call copykcuv(i(kcv), kcvcopy, gdp)
    ! 
    ! First foisf1 since s0 etc have to be known from the beginning.
    ! 
    ! F0ISF1: copy old (1) in new arrays (0)
    ! N.B.:
    ! check on stability not in initialisation
    ! herefore NST = -1
    ! Note:
    ! HU0 and HV0 obtain their values for the first time in F0ISF1 
    !
    if (.true.) then
    nst = -1
    stage = 'both'
    call f0isf1(stage     ,dischy    ,nst       ,zmodel    ,jstart    , &
              & nmmax     ,nmmaxj    ,nmax      ,kmax      ,lstsci    , &
              & ltur      ,nsrc      ,i(kcu)    ,i(kcv)    ,i(kcs)    , &
              & i(kfs)    ,i(kfu)    ,i(kfv)    ,i(kfsmin) ,i(kfsmax) , &
              & i(kfumin) ,i(kfumax) ,i(kfvmin) ,i(kfvmax) ,i(kfsmn0) , &
              & i(kfumn0) ,i(kfvmn0) ,i(kfsmx0) ,i(kfumx0) ,i(kfvmx0) , &
              & i(kfsz0)  ,i(kfuz0)  ,i(kfvz0)  , &
              & i(kfsz1)  ,i(kfuz1)  ,i(kfvz1)  , &
              & r(s0)     ,r(s1)     ,r(u0)     , &
              & r(u1)     ,r(v0)     ,r(v1)     ,r(volum0) ,r(volum1) , &
              & r(r0)     ,r(r1)     ,r(rtur0)  ,r(rtur1)  ,r(disch)  , &
              & r(discum) ,r(hu)     ,r(hv)     ,r(dzu1)   ,r(dzv1)   , &
              & r(dzs1)   ,r(dzu0)   ,r(dzv0)   ,r(dzs0)   ,r(qxk)    , &
              & r(qyk)    ,r(s00)    ,r(w0)     , &
              & r(w1)     ,r(p0)     ,r(p1)     ,r(hu0)    ,r(hv0)    , &
              & r(ewabr0) ,r(ewabr1) , &
              & r(ewave0) ,r(ewave1) ,r(eroll0) ,r(eroll1) ,roller    , &
              & gdp       )

    endif
    ! 
    ! Now also recompute dps, dpu, dpv
    !
    icx = nmaxddb
    icy = 1
    call caldps(nmmax     ,nfltyp    ,icx       , &
                 & icy       ,i(kcs)    ,r(dpd)    ,d(dps)    ,gdp       )
    !
    call caldpu(lundia    ,mmax      ,nmaxus    ,kmax      , &
                 & zmodel    , &
                 & i(kcs)    ,i(kcu)    ,i(kcv)    , &
                 & i(kspu)   ,i(kspv)   ,r(hkru)   ,r(hkrv)   , &
                 & r(umean)  ,r(vmean)  ,r(dpd)    ,r(dpu)    ,r(dpv)    , &
                 & d(dps)    ,r(dzs1)   ,r(u1)     ,r(v1)     ,r(s1)     , &
                 & r(thick)  ,gdp       )
    !
    ! CHKDRY: set initial depth at velocity points
    ! define mask arrays for velocity points
    ! initialize QXK and QYK arrays
    ! subroutine parameter(7) = ICX := NMAX
    ! subroutine parameter(8) = ICY := 1
    ! ONLY FOR SIGMA LAYER MODEL 
    ! FOR Z_MODEL CALL Z_CHKDRY
    !
    restid_old = gdp%gdrestart%restid
    gdp%gdrestart%restid = 'STATE'
    if (.not.zmodel) then
       icx = nmaxddb
       icy = 1
       call chkdry(jstart    ,nmmaxj    ,nmmax     ,kmax      ,lsec      , &
                 & lsecfl    ,lstsci    ,ltur      ,icx       ,icy       , &
                 & i(kcu)    ,i(kcv)    ,i(kcs)    ,i(kfu)    , &
                 & i(kfv)    ,i(kfs)    ,i(kspu)   ,i(kspv)   ,r(dpu)    , &
                 & r(dpv)    ,r(hu)     ,r(hv)     ,r(hkru)   ,r(hkrv)   , &
                 & r(thick)  ,r(s1)     ,d(dps)    ,r(u1)     ,r(v1)     , &
                 & r(umean)  ,r(vmean)  ,r(r1)     ,r(rtur1)  ,r(guu)    , &
                 & r(gvv)    ,r(qxk)    ,r(qyk)    ,gdp       )
    else
       icx = nmaxddb
       icy = 1
       call z_chkdry(jstart    ,nmmaxj    ,nmmax     ,kmax      ,lstsci    , &
                   & ltur      ,icx       ,icy       ,i(kcu)    , &
                   & i(kcv)    ,i(kcs)    ,i(kfu)    ,i(kfv)    ,i(kfs)    , &
                   & i(kspu)   ,i(kspv)   ,i(kfuz1)  ,i(kfvz1)  ,i(kfsz1)  , &
                   & i(kfumin) ,i(kfumax) ,i(kfvmin) ,i(kfvmax) ,i(kfsmin) , &
                   & i(kfsmax) ,r(dpu)    ,r(dpv)    ,r(hu)     ,r(hv)     , &
                   & r(hkru)   ,r(hkrv)   ,r(s1)     ,d(dps)    ,r(u1)     , &
                   & r(v1)     ,r(umean)  ,r(vmean)  ,r(r1)     ,r(rtur1)  , &
                   & r(guu)    ,r(gvv)    ,r(qxk)    ,r(qyk)    ,r(dzu1)   , &
                   & r(dzv1)   ,r(dzs1)   ,r(sig)    ,gdp       )
    endif
    !
    gdp%gdrestart%restid = restid_old
    !
    ! Convert the coordinates of the fixed gate using DPU/DPV as reference
    ! Initialise the porosity factor POROSU/V (== 1). Initialisation
    ! of porosity may not be skipped (later initialisation maybe moved to
    ! other routines)
    !
    call inicdw(lundia    ,nmax      ,mmax      ,nmaxus    ,kmax      , &
              & i(kspu)   ,i(kspv)   ,r(dpu)    ,r(dpv)    , &
              & r(porosu) ,r(porosv) ,r(cdwztu) ,r(cdwzbu) ,r(cdwztv) , &
              & r(cdwzbv) ,gdp       )
    if (cdwstruct) then
       !
       ! Define KSPU/V and POROSU/V for CDW type of structure (fixed gate with
       ! - OPTIONALLY - layers with enhanced friction below it). 
       ! Array SIG is passed on twice; the first one represents the SIGma coordinates
       ! (zmodel == .FALSE.) the second represent the Z-coordinates (zmodel == .TRUE.). 
       ! This is a trick to enable CDWKAD routine to be used for both coordinate types.
       ! Work array ZWORK has the length of 5*KMAX
       !
       call cdwkad(nmmax     ,kmax      ,zmodel    ,i(kspu)   ,i(kfsmax) , &
                 & i(kfsmin) ,i(kfumax) ,i(kfumin) ,r(sig)    ,r(thick)  , &
                 & r(sig)    ,r(zwork)  ,r(zwork+kmax)  ,r(zwork+2*kmax) , &
                 & r(dpu)    ,r(hu)     ,r(dzu1)   ,r(porosu) ,r(ubrlsu) , &
                 & r(cdwztu) ,r(cdwzbu) ,r(cdwlsu) ,gdp       )
       call cdwkad(nmmax     ,kmax      ,zmodel    ,i(kspv)   ,i(kfsmax) , &
                 & i(kfsmin) ,i(kfvmax) ,i(kfvmin) ,r(sig)    ,r(thick)  , &
                 & r(sig)    ,r(zwork)  ,r(zwork+kmax)  ,r(zwork+2*kmax) , &
                 & r(dpv)    ,r(hv)     ,r(dzv1)   ,r(porosv) ,r(ubrlsv) , &
                 & r(cdwztv) ,r(cdwzbv) ,r(cdwlsv) ,gdp       )
    endif
    !
    ! Compute volumes and areas
    !
    call inivol(jstart    ,nmmaxj    ,nmmax     ,kmax      ,zmodel    , &
              & i(kcs)    ,i(kcu)    ,i(kcv)    ,i(kfsmin) ,i(kfsmax) , &
              & i(kfumin) ,i(kfumax) ,i(kfvmin) ,i(kfvmax) ,r(thick)  , &
              & r(s1)     ,d(dps)    ,r(gsqs)   ,r(guu)    ,r(gvv)    , &
              & r(hu)     ,r(hv)     ,r(dzs1)   ,r(dzu1)   ,r(dzv1)   , &
              & r(volum1) ,r(porosu) ,r(porosv) ,r(areau)  ,r(areav)  ,gdp       )
    !
    ! DENS  : compute densities 
    !
    ifirst_dens = 1
    call dens(jstart    ,nmmaxj    ,nmmax     ,kmax       ,lstsci    , &
            & lsal      ,ltem      ,lsed      ,i(kcs)     ,saleqs    ,temeqs    , &
            & densin    ,zmodel    ,r(thick)  ,r(r0)      ,r(rho)    , &
            & r(sumrho) ,r(rhowat) ,rhosol    ,ifirst_dens,gdp       )
    !
    ! Z_DENGRA: compute DRHODX/DRHODY terms (only in Z-MODEL)
    !
    if (zmodel) then
       icx = nmaxddb
       icy = 1
       call z_dengra(jstart    ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                   & icy       ,i(kfsz1)  ,i(kfumin) ,i(kfumax) ,i(kfvmin) , &
                   & i(kfvmax) ,i(kfu)    ,i(kfv)    , &
                   & r(rho)    ,r(gvu)    ,r(guv)    ,r(drhodx) , &
                   & r(drhody) ,r(dzu1)   ,r(dzv1)   ,gdp       )
    endif
    !
    ! TRTROU: calculate rougness due to trachytopes.
    !         called for U/V-direction.
    !
    if (lftrto) then
       call trtrou(lundia    ,nmax      ,mmax      ,nmaxus    ,kmax      , &
                 & r(cfurou) ,rouflo    ,.true.    ,r(guu)    ,r(gvu)    , &
                 & r(hu)     ,i(kcu)    ,r(u1)     ,r(v1)     ,r(sig)    , &
                 & r(z0urou) ,r(deltau) ,1         ,gdp       )
       call trtrou(lundia    ,nmax      ,mmax      ,nmaxus    ,kmax      , &
                 & r(cfvrou) ,rouflo    ,.true.    ,r(gvv)    ,r(guv)    , &
                 & r(hv)     ,i(kcv)    ,r(v1)     ,r(u1)     ,r(sig)    , &
                 & r(z0vrou) ,r(deltav) ,2         ,gdp       )
    endif
    !
    ! INITAU: calculate inital roughness heights Z0U(V)ROU
    ! for HU and HV use work array WRKB1 for this purpose
    ! subroutine parameter(5) = ICX := NMAX and := 1    (second call)
    !
    icx = nmaxddb
    icy = 1
    call initau(jstart    ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
              & rouflo    ,zmodel    , &
              & i(kcs)    ,i(kcu)    ,i(kfu)    ,i(kspu)   , &
              & r(s1)     ,r(dpu)    ,r(umean)  ,r(wrkb1)  ,d(dps)    , &
              & r(cfurou) ,r(z0urou) ,gdp       )
    icx = 1
    icy = nmaxddb
    call initau(jstart    ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
              & rouflo    ,zmodel    , &
              & i(kcs)    ,i(kcv)    ,i(kfv)    ,i(kspv)   , &
              & r(s1)     ,r(dpv)    ,r(vmean)  ,r(wrkb2)  ,d(dps)    , &
              & r(cfvrou) ,r(z0vrou) ,gdp       )
    !
    ! EULER: calculate adjusted velocities for mass flux
    ! NOTE: Array SIG has a different meaning (ZK) in case
    ! of ZMODEL
    !
    icx = nmaxddb
    icy = 1
    call euler(jstart    ,nmmax     ,nmmaxj    ,kmax      ,icx       , &
             & i(kcu)    ,i(kcv)    ,i(kfu)    ,i(kfv)    ,i(kfumax) , &
             & i(kfumin) ,i(kfvmax) ,i(kfvmin) ,r(dzu1)   ,r(dzv1)   , &
             & r(u1)     ,r(wrkb3)  ,r(v1)     ,r(wrkb4)  , &
             & r(grmasu) ,r(grmasv) ,r(hu)     ,r(hv)     , &
             & r(tp)     ,r(hrms)   ,r(sig)    ,r(thick)  ,r(teta)   , &
             & r(grmsur) ,r(grmsvr) ,r(grfacu) ,r(grfacv) ,gdp       )
    !
    ! TAUBOT: calculate bottom stress coefficients
    ! to calculate tau_bottom values using local values
    ! For HU and HV use work array WRKB1 for this purpose
    ! For adjusted velocities use work array WRKB3 (U1) and
    ! WRKB4 (V1)
    ! For calculation of TAUBMX use work array WRKA1 resp.
    ! WRKA2
    ! For Chezy coeff. use work array WRKA4 resp. WRKA5 (used
    ! in DETVIC)
    ! subroutine parameter(5) = ICX := NMAX and := 1    (second call)
    ! subroutine parameter(6) = ICY := 1    and := NMAX (second call)
    !
    ! TAUBOT is called here with kcu/v instead of kfu/v, to ensure that
    ! also the temporary dry points contain a relevant cfurou(nm,1) value.
    ! These values are used when the point becomes wet.
    ! kcu/v is used inside TAUBOT as weight factor to calculate v(u) 
    ! in u(v) points. Therefore, kcu/v should not contain the value 
    ! 2 (open boundary) or 3 (dd boundary). That's why the (cleaned)
    ! copies of kcu/v are used.
    !

    !VORTECH test 24.02.10: skip taubot
    !  26.02.10: skipping taubot is wrong, since taubpu etc are computed and needed for u's etc.
    ! VORTECH test 26.02: Do NOT use kcu. Then cfurou will NOT be computed and an old 
    ! value will be used; that old value may not be very wrong.
    ! Addendum: it IS very wrong, but that is the way Delft3D works. It is accepted that
    ! it is difficult to put a value in a recently dry point, but the value is now dependent
    ! of the last restart, the more recent, the 'better'.
    !
    if (.true.) then
       icx = nmaxddb
       icy = 1
       if (.not. zmodel) then
          call upwhu(jstart    ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                   & zmodel    ,i(kcs)    ,i(kcu)    ,i(kspu)   ,d(dps)    , &
                   & r(s1)     ,r(dpu)    ,r(umean)  ,r(wrkb1)  ,gdp       )
       endif
       call taubot(jstart    ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                 & icy       ,rouflo    ,rouwav    ,kcucopy   ,kcvcopy   , &
                 & i(kfumin) ,i(kfumax) ,i(kspu)   ,i(kcs)    ,i(kcscut) , &
                 & d(dps)    ,r(s1)     ,r(wrkb3)  ,r(wrkb4)  , &
                 & r(guu)    ,r(xcor)   ,r(ycor)   ,r(rho)    , &
                 & r(taubpu) ,r(taubsu) ,r(wrka1)  ,r(dis)    ,r(rlabda) , &
                 & r(teta)   ,r(uorb)   ,r(tp)     ,r(wsu)    ,r(wsv)    , &
                 & r(grmasu) ,r(dfu)    ,r(deltau) ,r(hrms)   , &
                 & r(cfurou) ,r(z0urou) ,r(wrkb1)  ,r(dzu1)   ,r(sig)    , &
                 & r(z0ucur) ,r(cvalu0) ,r(grmsur) ,r(grfacu) ,gdp       )
       icx = 1
       icy = nmaxddb
       if (.not. zmodel) then
          call upwhu(jstart    ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                   & zmodel    ,i(kcs)    ,i(kcv)    ,i(kspv)   ,d(dps)    , &
                   & r(s1)     ,r(dpv)    ,r(vmean)  ,r(wrkb2)  ,gdp       )
       endif
       call taubot(jstart    ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                 & icy       ,rouflo    ,rouwav    ,kcvcopy   ,kcucopy   , &
                 & i(kfvmin) ,i(kfvmax) ,i(kspv)   ,i(kcs)    ,i(kcscut) , &
                 & d(dps)    ,r(s1)     ,r(wrkb4)  ,r(wrkb3)  , &
                 & r(gvv)    ,r(ycor)   ,r(xcor)   ,r(rho)    , &
                 & r(taubpv) ,r(taubsv) ,r(wrka2)  ,r(dis)    ,r(rlabda) , &
                 & r(teta)   ,r(uorb)   ,r(tp)     ,r(wsv)    ,r(wsu)    , &
                 & r(grmasv) ,r(dfv)    ,r(deltav) ,r(hrms)   , &
                 & r(cfvrou) ,r(z0vrou) ,r(wrkb2)  ,r(dzv1)   ,r(sig)    , &
                 & r(z0vcur) ,r(cvalv0) ,r(grmsvr) ,r(grfacv) ,gdp       )
    endif 
    icx = nmaxddb
    icy = 1
    call caltmx(jstart    ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
              & icy       ,zmodel    ,i(kfu)    ,i(kfv)    ,i(kfs)    , &
              & i(kfuz1)  ,i(kfvz1)  ,i(kfsmin) ,r(wrka1)  ,r(wrka2)  , &
              & r(taubmx) ,r(hu)     ,r(hv)     ,d(dps)    ,r(s1)     , &
              & gdp       )
    if (htur2d .or. irov>0 .or. zmodel) then
       !
       ! Check horizontal Eddy Viscosity and Diffusivity
       !
       itype = 1
       call chkvic(lundia    ,jstart    ,nmmaxj    ,nmmax     ,kmax      , &
                 & icx       ,icy       ,timnow    ,i(kfs)    ,i(kfu)    , &
                 & i(kfv)    ,i(kcs)    ,lstsci    ,r(guv)    ,r(gvu)    , &
                 & r(vicuv)  ,r(dicuv)  ,itype     ,i(kfsmin) ,i(kfsmax) , &
                 & gdp       )
    endif
    if (htur2d) then
       !
       ! HLES/Smagorinsky with bottom friction
       ! Calculate fluctuating velocity components using lp filter
       !
       call lpfluc(jstart    ,nmmaxj    ,nmmax     ,i(kfu)    ,i(kfv)    , &
                 & r(umean)  ,r(vmean)  ,r(umnldf) ,r(vmnldf) ,r(umnflc) , &
                 & r(vmnflc) ,gdp       )
       !
       ! Calculate Turbulent Kinetic Energy production due to velocity
       ! fluctuation
       ! wrka3 is used to store the result (S2) to be used in DETVIC
       !
       icx = nmaxddb
       icy = 1
       call protke(jstart    ,nmmaxj    ,nmmax     ,icx       ,icy       , &
                 & i(kfs)    ,i(kfu)    ,i(kfv)    ,i(kcs)    ,r(umnflc) , &
                 & r(vmnflc) ,r(guu)    ,r(gvv)    ,r(wrka1)  ,r(wrka2)  , &
                 & r(wrka3)  ,gdp       )
       !
       ! Calculate subgridscale eddy viscosity/diffusivity
       ! CVALU0 and CVALV0 contain actual 2D-chezy values
       ! WRKA3 contains TKE production (S2)
       ! result is put in vicuv/dicuv in layer kmax+2
       !
       icx = nmaxddb
       icy = 1
       call detvic(lundia    ,jstart    ,nmmaxj    ,nmmax     ,kmax      , &
                 & icx       ,icy       ,i(kfs)    ,i(kfu)    , &
                 & i(kfv)    ,i(kcs)    ,d(dps)    ,r(s1)     ,r(umean)  , &
                 & r(vmean)  ,r(cvalu0) ,r(cvalv0) ,r(guv)    ,r(gvu)    , &
                 & r(gsqs)   ,r(wrka3)  ,r(vicuv)  ,r(dicuv)  , &
                 & gdp       )
    endif
    !
    ! To avoid problems with GPP, arrays VORTIC and ENSTRO are always
    ! computed and stored in HIS and MAP files even when HLES is not
    ! activated. These arrays were meant for post-processing only
    !
    call c_vort(mmax      ,nmax      ,kmax      ,nmaxus    ,i(kcs)    ,i(kfu)    , &
              & i(kfv)    ,r(u1)     ,r(v1)     ,r(gud)    ,r(gvd)    , &
              & r(vortic) ,r(enstro) ,r(wrkb1)  ,gdp       )
    !
    ! INITUR: calculate initial turbulent energy and/or turbulent
    ! dissipation depending on the value of lturi
    ! subroutine parameter(5) = ICX := NMAX
    ! subroutine parameter(6) = ICY := 1
    !
    
    ! VORtech : skip!
    ! VORtech: really? note the rtur0/rtur1 initialisation is rather difficult!
    goto 9554
    
    if (lturi /= 0) then
       if (.not.zmodel) then
          icx = nmaxddb
          icy = 1
          call initur(jstart    ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                    & icy       ,ltur      ,lturi     ,r(rtur1)  , &
                    & r(s1)     ,d(dps)    ,r(hu)     ,r(hv)     ,r(u1)     , &
                    & r(v1)     ,r(thick)  ,r(windsu) ,r(windsv) ,r(z0urou) , &
                    & r(z0vrou) ,i(kfu)    ,i(kfv)    ,i(kfs)    ,i(kcs)    , &
                    & r(wrkb1)  ,r(wrkb2)  ,r(bruvai) ,r(rich)   ,r(rho)    , &
                    & gdp       )
       else
          icx = nmaxddb
          icy = 1
          call z_initur(jstart    ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                      & icy       ,ltur      ,lturi     ,i(kfu)    ,i(kfv)    , &
                      & i(kfs)    ,i(kcs)    ,i(kfumin) ,i(kfumax) ,i(kfvmin) , &
                      & i(kfvmax) ,i(kfsmin) ,i(kfsmax) ,r(rtur1)  , &
                      & r(s1)     ,d(dps)    ,r(u1)     ,r(v1)     ,r(windsu) , &
                      & r(windsv) ,r(z0urou) ,r(z0vrou) ,r(wrkb1)  ,r(wrkb2)  , &
                      & r(dzu1)   ,r(dzv1)   ,r(dzs1)   ,r(bruvai) ,r(rich)   , &
                      & r(rho)    ,gdp       )
       endif
    endif
    !
9554 continue
    
    !VORtech: call incmeteo first since time is reset when states are swapped.
         call incmeteo(timhr     , grdang   , &
                & r (windu ),r (windv ),r (patm  ), &
                & i (kcs   ),r (alfas ), &
                & r (windsu),r (windsv),r (w10mag), gdp)
    
    ! VORtech: call heatu in order to have a good value of evap.
         if (ktemp > 0) then
          icx = nmaxddb
          icy = 1

          call heatu(ktemp     ,anglat    ,sferic    ,timhr     ,keva      , &
                   & ltem      ,lstsci    ,icx       ,icy       , &
                   & nmmax     ,kmax      ,i(kfs)    ,i(kfsmx0) ,i(kfsmax) , &
                   & i(kfsmin) ,i(kspu)   ,i(kspv)   ,r(dzs0)   ,r(dzs1)   , &
                   & r(sour)   ,r(sink)   ,r(r0)     ,r(evap)   ,d(dps)    , &
                   & r(s0)     ,r(s1)     ,r(thick)  ,r(w10mag) ,r(patm)   , &
                   & r(xcor)   ,r(ycor)   ,r(gsqs)   ,r(xz)     ,r(yz)     , &
                   & anglon    ,gdp       )
     endif
    
    
    
    ! DERSIG: computes transformation coefficients for the sigma trans-
    ! formation: DZDKSI, DZDETA, DDDKSI, DDDETA
    ! subroutine parameter(4) = ICX := NMAX
    ! subroutine parameter(5) = ICY := 1
    !
    if (.not.zmodel) then
       icx = nmaxddb
       icy = 1
       call dersig(jstart    ,nmmaxj    ,nmmax     ,icx       ,icy       , &
                 & i(kfu)    ,i(kfv)    ,r(dpd)    ,r(s1)     ,r(dddksi) , &
                 & r(dddeta) ,r(dzdksi) ,r(dzdeta) ,gdp       )
    endif
    !
    ! (Rigid) 3D Vegetation Model
    !
    if (veg3d) then
       call updveg3d(mmax      ,nmax      ,kmax      ,r(sig)    ,r(thick)  , &
                   & d(dps)    ,i(kfs)    ,r(s0)     ,r(u1)     ,r(v1)     , &
                   & r(diapl)  ,r(rnpl)   ,gdp       )
    endif
    if (varyingmorfac) then
       !
       ! Varying MorFac
       ! First update of MorFac must be done before the first call to postpr
       !
       call flw_gettabledata(morfacfile ,morfactable,            &
                           & morfacpar  , 1         , morfacrec, &
                           & value(1:1) , timhr     , julday,    gdp )

       morfac = value(1)
    endif
    !
 9999 continue
    deallocate(kcucopy, stat=ierr)
    deallocate(kcvcopy, stat=ierr)
    !
    ! recompute time variables (especially timmin)
    !
    call setcurrentdatetime(timnow, gdp)
    !
end subroutine compute_secundary_state
