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
!-------------------------------------------------------------------------------
module m_rdstm
use morphology_data_module
use bedcomposition_module
use precision
use m_rdsed
use m_rdmor
use m_rdtrafrm
!
private
!
public stmtype
!
public rdstm
public clrstm

type stmtype
    real(fp)                                               :: fwfac
    type(sedpar_type)                        , pointer     :: sedpar
    type(morpar_type)                        , pointer     :: morpar
    type(bedcomp_data)                       , pointer     :: morlyr
    type(trapar_type)                        , pointer     :: trapar
    type(t_nodereldata)                      , pointer     :: nrd
    integer                                                :: lsedsus
    integer                                                :: lsedtot
    real(fp)      , dimension(:), allocatable              :: facdss
    real(fp)      , dimension(:,:), allocatable            :: ws
    character(20) , dimension(:), allocatable              :: namcon
end type stmtype

contains

!> Read sediment transport and morphology data from filsed, filemor and filtrn
!! (and files referenced therein).
subroutine rdstm(stm, griddim, filsed, filmor, filtrn, &
               & lundia, lsal, ltem, ltur, lsec, lfbedfrm, &
               & julrefday, dtunit, nambnd, error)
    use grid_dimens_module
    use sediment_basics_module, only: TRA_ADVDIFF
    use properties ! includes tree_structures
    use m_ini_noderel ! for node relation definitions
    !
    implicit none
    !
    integer                   , parameter    :: NPARDEF = 20
!
! Arguments
!
    type(stmtype)               , intent(out) :: stm
    type(griddimtype)   , target, intent(in)  :: griddim
    character(*)                , intent(in)  :: filsed
    character(*)                , intent(in)  :: filmor
    character(*)                , intent(in)  :: filtrn
    integer                     , intent(in)  :: lundia
    integer                     , intent(in)  :: lsal
    integer                     , intent(in)  :: ltem
    integer                     , intent(in)  :: ltur
    integer                     , intent(in)  :: lsec
    logical                     , intent(in)  :: lfbedfrm
    integer                     , intent(in)  :: julrefday
    character(20) , dimension(:), intent(in)  :: nambnd
    character(*)                , intent(in)  :: dtunit
    logical                     , intent(out) :: error
!
! Local variables
!
    logical                                  :: cmpupdall
    logical                                  :: cmpupdany
    integer                                  :: istat
    integer                                  :: lstsci
    integer                                  :: nto
    integer                                  :: nmaxus
    integer                                  :: nmlb
    integer                                  :: nmub
    integer                                  :: l
    type(tree_data)               , pointer  :: morfil_tree
    type(tree_data)               , pointer  :: sedfil_tree
    integer, dimension(2,NPARDEF)            :: ipardef
    real(fp), dimension(NPARDEF)             :: rpardef
!
!! executable statements -------------------------------------------------------
!
    error = .false.
    !
    allocate(stm%sedpar , stat = istat)
    allocate(stm%morpar , stat = istat)
    allocate(stm%trapar , stat = istat)
    allocate(stm%morlyr , stat = istat)
    allocate(stm%nrd    , stat = istat)
    !
    call nullsedpar(stm%sedpar)
    call nullmorpar(stm%morpar)
    call nulltrapar(stm%trapar)
    istat = initmorlyr (stm%morlyr)
    !
    call tree_create  ( "Sediment input", sedfil_tree )
    call tree_create  ( "Morphology input", morfil_tree )
    !
    nmaxus = griddim%nmax
    nmlb   = griddim%nmlb
    nmub   = griddim%nmub
    nto    = size(nambnd,1)
    !
    ! Open filsed file and determine the number of sediment fractions
    ! lsedsus (only fractions that included suspended transport advection diffusion solver)
    ! and lsedtot (total number of fractions). Fill names and sediment types in sedpar.
    ! Keep sediment file information in sedfil_tree.
    !
    call count_sed(lundia, error, stm%lsedsus, stm%lsedtot, filsed, &
                 & stm%sedpar, sedfil_tree)
    if (error) return
    !
    lstsci = max(0,lsal,ltem) + stm%lsedsus
    !
    allocate(stm%facdss(stm%lsedsus), stat = istat)
    allocate(stm%namcon(lstsci+ltur), stat = istat)
    !
    if (lsal>0) then
       stm%namcon(lsal) = 'SALINITY'
    endif
    if (ltem>0) then
       stm%namcon(ltem) = 'TEMPERATURE'
    endif
       do l = 1, stm%lsedsus
       stm%namcon(max(0,lsal,ltem) + l) = stm%sedpar%namsed(l)
    enddo
    !
    ! Read sediment and transport parameters
    !
    ! facdss set by rdsed
    ! iopsus set by rdsed
    ! sedpar filled by rdsed
    ! trapar set by rdtrafrm
    ! sedfil_tree NEEDS TO BE SET
    !
    ! Sediment input has been placed in input_tree in subroutine count_sed
    ! get pointer
    !
    call initrafrm(lundia, error, stm%lsedtot, stm%trapar)
    if (error) return
    !
    call rdsed  (lundia, error, lsal, ltem, stm%lsedsus, &
               & stm%lsedtot, lstsci, ltur, stm%namcon, &
               & stm%morpar%iopsus, nmlb, nmub, filsed, &
               & sedfil_tree, stm%sedpar, stm%trapar, griddim)
    if (error) return
    ! 
    !  For 1D branches read the node relation definitions
    !
    call ini_noderel(stm%nrd, stm%sedpar, stm%lsedtot)
    !     
    ! Read morphology parameters
    !
    ! morpar filled by rdmor
    ! morlyr filled by rdmor
    ! morfil_tree set by rdmor
    ! fwfac set by rdmor
    !
    call rdmor  (lundia, error, filmor, lsec, stm%lsedtot, &
               & stm%lsedsus, nmaxus, nto, lfbedfrm, nambnd, julrefday, morfil_tree, &
               & stm%sedpar, stm%morpar, stm%fwfac, stm%morlyr, &
               & griddim)
    if (error) return
    !
    ! Some other parameters are transport formula specific. Use the value
    ! historically specified in mor file as default.
    !
    ipardef = 0
    rpardef = 0.0_fp
    ! Van Rijn (1993)
    call setpardef(ipardef, rpardef, NPARDEF, -1, 1, stm%morpar%iopsus)
    call setpardef(ipardef, rpardef, NPARDEF, -1, 2, stm%morpar%aksfac)
    call setpardef(ipardef, rpardef, NPARDEF, -1, 3, stm%morpar%rwave)
    call setpardef(ipardef, rpardef, NPARDEF, -1, 4, stm%morpar%rdc)
    call setpardef(ipardef, rpardef, NPARDEF, -1, 5, stm%morpar%rdw)
    call setpardef(ipardef, rpardef, NPARDEF, -1, 6, stm%morpar%iopkcw)
    call setpardef(ipardef, rpardef, NPARDEF, -1, 7, stm%morpar%epspar) ! explicitly not for Delft3D-FLOW ... ?
    ! Van Rijn (2007)
    call setpardef(ipardef, rpardef, NPARDEF, -2, 1, stm%morpar%iopsus)   ! jre
    call setpardef(ipardef, rpardef, NPARDEF, -2, 2, stm%morpar%pangle)
    call setpardef(ipardef, rpardef, NPARDEF, -2, 3, stm%morpar%fpco)
    call setpardef(ipardef, rpardef, NPARDEF, -2, 4, stm%morpar%subiw)
    call setpardef(ipardef, rpardef, NPARDEF, -2, 5, stm%morpar%epspar)
    ! SANTOSS copy of Van Rijn (2007)
    call setpardef(ipardef, rpardef, NPARDEF, -4, 1, stm%morpar%iopsus)
    call setpardef(ipardef, rpardef, NPARDEF, -4, 2, stm%morpar%pangle)
    call setpardef(ipardef, rpardef, NPARDEF, -4, 3, stm%morpar%fpco)
    call setpardef(ipardef, rpardef, NPARDEF, -4, 4, stm%morpar%subiw)
    call setpardef(ipardef, rpardef, NPARDEF, -4, 5, stm%morpar%epspar)
    !
    call rdtrafrm(lundia, error, filtrn, stm%lsedtot, &
                & ipardef, rpardef, NPARDEF, stm%trapar, &
                & stm%morpar%moroutput%sedpar, &
                & stm%sedpar%sedtyp, stm%sedpar%sedblock, &
                & griddim, julrefday, stm%sedpar%max_mud_sedtyp)
    if (error) return
    !
    ! update tratyp based on the transport formula selected
    ! switch off the bed load component when a transport formula based on
    ! entrainment and deposition terms is selected (Partheniades-Krone or
    ! user defined).
    !
    do l = 1, stm%lsedsus
       if (stm%trapar%iform(l) == -3 .or. stm%trapar%iform(l) == 21) then
          stm%sedpar%tratyp(l) = TRA_ADVDIFF
       endif
    enddo
    !--------------------------------------------------------------------------
    !
    ! Echo sediment and transport parameters
    !
    call echosed(lundia, error, stm%lsedsus, stm%lsedtot, &
               & stm%morpar%iopsus, stm%sedpar, stm%trapar, stm%morpar%cmpupd)
    if (error) return
    !
    ! Echo morphology parameters
    !
    cmpupdall = all(stm%sedpar%cmpupdfrac)
    cmpupdany = any(stm%sedpar%cmpupdfrac)
    call echomor(lundia, error, lsec, stm%lsedtot, nto, &
               & nambnd, stm%sedpar, stm%morpar, dtunit, cmpupdall, cmpupdany)
    !
    ! we should deallocate sedfil_tree, morfil_tree but
    ! we can't deallocate sedfil_tree since parts are referenced from stm%sedpar
end subroutine rdstm

function clrstm(stm) result(istat)
!!--description-----------------------------------------------------------------
!
! Read sediment transport and morphology data from filsed, filemor and filtrn
! (and files referenced therein).
!
!!--declarations----------------------------------------------------------------
    use morphology_data_module
    use m_ini_noderel
    implicit none
!
! Arguments
!
    type(stmtype)   , intent(inout) :: stm
    integer                         :: istat
!
! Local variables
!
!   NONE
!
!! executable statements -------------------------------------------------------
!
    istat = 0
    if (associated(stm%sedpar)) then
        call clrsedpar(istat, stm%sedpar)
        deallocate(stm%sedpar, STAT = istat)
    endif
    if (associated(stm%morpar) .and. istat==0) then
        call clrmorpar(istat, stm%morpar)
        deallocate(stm%morpar, STAT = istat)
    endif
    if (associated(stm%trapar) .and. istat==0) then
        call clrtrapar(istat, stm%trapar)
        deallocate(stm%trapar, STAT = istat)
    endif
    if (associated(stm%morlyr) .and. istat==0) then
        istat = clrmorlyr(stm%morlyr)
        deallocate(stm%morlyr, STAT = istat)
    endif
    if (associated(stm%nrd) .and. istat==0) then
        call clr_noderel(istat, stm%nrd)
        deallocate(stm%nrd, STAT = istat)
    endif
end function clrstm

end module m_rdstm
