subroutine rdsedmortra(lundia    ,error     ,lsal      ,ltem      ,lsed      , &
                     & lsedtot   ,lstsci    ,ltur      ,namcon    ,iopsus    , &
                     & mmax      ,nmax      ,nmaxus    ,nmmax     ,nto       , &
                     & nambnd    ,lsec      ,tstart    ,tunit     ,gdp       )
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
!!--description-----------------------------------------------------------------
!
! Read sediment, morphology and transport parameters from the input files
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    use sediment_basics_module, only: TRA_ADVDIFF
    use m_rdmor
    use m_rdsed
    use m_rdtrafrm
    !
    use globaldata
    !
    implicit none
    !
    type(globdat)             ,target        :: gdp
    integer                   , parameter    :: NPARDEF = 20
    logical                   , pointer      :: lfbedfrm
    real(hp)                  , pointer      :: morft
    real(hp)                  , pointer      :: morft0
!
! Global variables
!
    integer                                  , intent(in)  :: lsal    !< constituent index of salinity (0 if not included)
    integer                                  , intent(in)  :: lsed    !< number of suspended sediment fractions
    integer                                  , intent(in)  :: lsedtot !< total number of sediment fractions
    integer                                  , intent(in)  :: lstsci  !< total number of constituents (including secondary flow)
    integer                                  , intent(in)  :: ltem    !< constituent index of temperature (0 if not included)
    integer                                  , intent(in)  :: ltur    !< number of turbulent quantities
    integer                                                :: lundia  !< unit number of log file
    logical                                  , intent(out) :: error   !< flag indicating whether an error occurred
    character(20) , dimension(lstsci + ltur)               :: namcon  !< names of the constituents and turbulent quantities
    integer                                                :: iopsus  !<
    integer                                  , intent(in)  :: mmax    !< number of grid cells in M direction
    integer                                  , intent(in)  :: nmax    !< number of grid cells in N direction
    integer                                  , intent(in)  :: nmaxus  !< number of grid cells in N direction communicated to user
    integer                                  , intent(in)  :: nmmax   !< total number of grid cells
    integer                                  , intent(in)  :: nto     !< number of open boundaries
    integer                                  , intent(in)  :: lsec    !< secondary flow flag
    real(fp)                                 , intent(in)  :: tstart  !< start time
    real(fp)                                 , intent(in)  :: tunit   !< time unit
    character(20) , dimension(nto)                         :: nambnd  !< names of open boundaries
!
! Local variables
!
    logical                                  :: cmpupdall
    logical                                  :: cmpupdany
    integer                                  :: i
    real(fp)                                 :: fwfacmor
    character(256)                           :: filmor
    character(256)                           :: filsed
    character(256)                           :: filtrn
    character(256)                           :: string
    type(tree_data)               , pointer  :: mor_ptr
    type(tree_data)               , pointer  :: sed_ptr
    integer, dimension(2,NPARDEF)            :: ipardef
    real(fp), dimension(NPARDEF)             :: rpardef
!
!! executable statements -------------------------------------------------------
!
    morft               => gdp%gdmorpar%morft
    morft0              => gdp%gdmorpar%morft0
    lfbedfrm            => gdp%gdbedformpar%lfbedfrm
    !    
    error = .false.
    !
    if (morft == 0.0_hp) then
        !
        ! if the morphological start time is not set to some positive value due
        ! to restart from trim-file, then make sure that the morphological start
        ! time corresponds to the hydrodynamic start time. This includes TStart!
        !
        morft  = real(tstart*tunit,hp)/86400.0_hp
        morft0 = morft
    endif
    !
    ! Read name of default transport formula
    !
    filtrn = ' '
    call prop_get_string(gdp%mdfile_ptr,'*','TraFrm',filtrn)
    !
    call initrafrm(lundia    ,error     ,lsedtot   ,gdp%gdtrapar)
    if (.not.error) then
    !
    ! Read name of sediment input file
    !
    filsed = ' '
    call prop_get_string(gdp%mdfile_ptr, '*', 'Filsed', filsed)
    !
    ! Sediment input has been placed in input_tree in subroutine dimsedconst
    ! get pointer
    !
    call tree_get_node_by_name( gdp%input_tree, 'Sediment Input', sed_ptr )
    !
    ! Read data from that file
    !
    call rdsed(lundia    ,error     ,lsal      ,ltem      ,lsed      , &
             & lsedtot   ,lstsci    ,ltur      ,namcon    ,iopsus    , &
             & gdp%d%nmlb,gdp%d%nmub,filsed    ,sed_ptr   , &
             & gdp%gdsedpar,gdp%gdtrapar, gdp%griddim)
    endif
    if (.not.error) then
    !
    ! Read name of morphology input file
    !
    filmor = ' '
    call prop_get_string(gdp%mdfile_ptr, '*', 'Filmor', filmor)
    !
    ! Create Morphology branch in input tree
    !
    call tree_create_node(gdp%input_tree, 'Morphology Input', mor_ptr )
    call tree_put_data(mor_ptr, transfer(trim(filmor),node_value), 'STRING' )
    !
    ! Read data from that file
    !
    call rdmor(lundia     ,error     ,filmor    ,lsec      ,lsedtot    , &
             & lsed       ,nmaxus     ,nto      ,lfbedfrm  , &
             & nambnd     ,gdp%gdinttim%julday  ,mor_ptr   ,gdp%gdsedpar, &
             &gdp%gdmorpar,fwfacmor  ,gdp%gdmorlyr, gdp%griddim)
    endif
    if (.not.error) then
    !
    ! FWFac is independent of sediment transport. Use the value historically
    ! specified in mor file only if it hasn't been specified in the mdf file.
    !
    string = ' '
    call prop_get(gdp%mdfile_ptr, '*', 'FWFac' , string)
    if (string == ' ') then
       gdp%gdnumeco%fwfac = fwfacmor
    endif
    !
    ! Some other parameters are transport formula specific. Use the value
    ! historically specified in mor file as default.
    !
    ipardef = 0
    rpardef = 0.0_fp
    ! Van Rijn (1993)
    call setpardef(ipardef, rpardef, NPARDEF, -1, 1, gdp%gdmorpar%iopsus)
    call setpardef(ipardef, rpardef, NPARDEF, -1, 2, gdp%gdmorpar%aksfac)
    call setpardef(ipardef, rpardef, NPARDEF, -1, 3, gdp%gdmorpar%rwave)
    call setpardef(ipardef, rpardef, NPARDEF, -1, 4, gdp%gdmorpar%rdc)
    call setpardef(ipardef, rpardef, NPARDEF, -1, 5, gdp%gdmorpar%rdw)
    call setpardef(ipardef, rpardef, NPARDEF, -1, 6, gdp%gdmorpar%iopkcw)
    ! for backward compatibility gdp%gdmorpar%epspar should NOT be copied to par 7 of Van Rijn 1993 (-1)
    ! Van Rijn (2007)
    call setpardef(ipardef, rpardef, NPARDEF, -2, 1, gdp%gdmorpar%iopsus)
    call setpardef(ipardef, rpardef, NPARDEF, -2, 2, gdp%gdmorpar%pangle)
    call setpardef(ipardef, rpardef, NPARDEF, -2, 3, gdp%gdmorpar%fpco)
    call setpardef(ipardef, rpardef, NPARDEF, -2, 4, gdp%gdmorpar%subiw)
    call setpardef(ipardef, rpardef, NPARDEF, -2, 5, gdp%gdmorpar%epspar)
    ! SANTOSS copy of Van Rijn (2007)
    call setpardef(ipardef, rpardef, NPARDEF, -4, 1, gdp%gdmorpar%iopsus)
    call setpardef(ipardef, rpardef, NPARDEF, -4, 2, gdp%gdmorpar%pangle)
    call setpardef(ipardef, rpardef, NPARDEF, -4, 3, gdp%gdmorpar%fpco)
    call setpardef(ipardef, rpardef, NPARDEF, -4, 4, gdp%gdmorpar%subiw)
    call setpardef(ipardef, rpardef, NPARDEF, -4, 5, gdp%gdmorpar%epspar)
    !
    call rdtrafrm(lundia    ,error     ,filtrn    ,lsedtot   , &
                & ipardef   ,rpardef   ,NPARDEF   ,gdp%gdtrapar, &
                & gdp%gdmorpar%moroutput%sedpar, &
                & gdp%gdsedpar%sedtyp  ,gdp%gdsedpar%sedblock  , &
                & gdp%griddim, gdp%gdinttim%julday, gdp%gdsedpar%max_mud_sedtyp)
    endif
    if (.not.error) then
       !
       ! update tratyp based on the transport formula selected
       ! switch off the bed load component when Partheniades-Krone is used.
       !
       do i = 1, lsed
          if (gdp%gdtrapar%iform(i) == -3) then
             gdp%gdsedpar%tratyp(i) = TRA_ADVDIFF
          endif     
       enddo    
       !
       !--------------------------------------------------------------------------
       if (gdp%gdprocs%flmd2l) then
          if (gdp%gdmorpar%bedupd) then
              call prterr(lundia, 'U190', 'Bed level updating automatically switched off for 2-layer fluid mud computation')
              gdp%gdmorpar%bedupd = .false.
          endif
          if (gdp%gdmorpar%cmpupd) then
              call prterr(lundia, 'U190', 'Bed composition updating automatically switched off for 2-layer fluid mud computation')
              gdp%gdmorpar%cmpupd = .false.
          endif
       endif
       !--------------------------------------------------------------------------
       !
       ! Echo sediment and transport parameters
       !
       call echosed(lundia    ,error     ,lsed      ,lsedtot   , &
                  & iopsus    ,gdp%gdsedpar, gdp%gdtrapar, gdp%gdmorpar%cmpupd)
    endif
    if (.not.error) then
       !
       ! Echo morphology parameters
       !
       cmpupdall = all(gdp%gdsedpar%cmpupdfrac)
       cmpupdany = any(gdp%gdsedpar%cmpupdfrac)
       call echomor(lundia    ,error     ,lsec      ,lsedtot   ,nto        , &
                  & nambnd    ,gdp%gdsedpar, gdp%gdmorpar, gdp%gdexttim%tunitstr, cmpupdall, cmpupdany)
    endif
    if (.not.error) then
       !
       ! Read scour and echo parameters
       !
       call rdscour(lundia    ,error     ,nmmax     ,gdp       )
    endif
    if (.not.error) then
       !
       ! If either Van Rijn 2004 transport formula (iform = -2) or the extended
       ! SANTOSS version (iform = -4) is used, switch on the bed roughness height
       ! predictor. By default this predictor is set to the Van Rijn 2004
       ! formulations; give a warning if this has been set to a different predictor
       ! by the user.
       !
       do i = 1, lsedtot
          if (gdp%gdtrapar%iform(i) == -2 .or. gdp%gdtrapar%iform(i) == -4) then
             if (gdp%gdbedformpar%bdfrpt /= 0) then
                call prterr(lundia, 'U190', 'Van Rijn (2007) or SANTOSS transport formula combined with different bedform roughness predictor')
             endif
             gdp%gdbedformpar%lfbedfrmrou = .true.
             exit
          endif
       enddo
    endif
    
    if (error) call d3stop(1, gdp)
end subroutine rdsedmortra
