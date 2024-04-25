subroutine postpr_hdt(nst, gdp)
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
!    Function: 
!              - Call POSTPR without having to define all arguments in the
!                main routine. This routine is called in TRISOL for half
!                time steps.
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use flow2d3d_timers
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                             , pointer :: lundia
    integer                             , pointer :: lunprt
    real(fp)                            , pointer :: rhow
    integer                             , pointer :: ktemp
    real(fp)                            , pointer :: grdang
    character(19)                       , pointer :: prsmap
    character(21)                       , pointer :: selmap
    character(23)                       , pointer :: prshis
    character(23)                       , pointer :: selhis
    character(256)                      , pointer :: comfil  !!  Communication file name
    character(256)                      , pointer :: runid   !!  Run identification code for the current simulation (used to determine the names of the in- /output files used by the system)
    character(256)                      , pointer :: trifil  !!  File name for TRISULA NEFIS output files (tri"h/m"-"casl""labl".dat/def)
    character(5)                        , pointer :: versio  !!  Version nr. of the current package
    integer                             , pointer :: iphisc  ! Current time counter for printing history data 
    integer                             , pointer :: itcomc  ! Current time counter for the communication file 
    integer                             , pointer :: itcur   ! Current time counter for the communication file, where starting point depend on CYCLIC 
    integer                             , pointer :: itdroc  ! Current time counter for the drogue data file 
    integer                             , pointer :: ithisc  ! Current time counter for the history file 
    integer                             , pointer :: itimc   ! Current time step counter for 2D system 
    integer                             , pointer :: itmapc  ! Current time counter for the map file 
    integer                             , pointer :: itrstc  ! Current time counter for the restart file. Start writing after first interval is passed. Last time will always be written to file for ITRSTI > 0 
    integer                             , pointer :: npmap   ! Current array counter for printing map data 
    integer                             , pointer :: ntcur   ! Total number of timesteps on comm. file (to write to) 
    real(fp)                            , pointer :: dtsec   ! DT in seconds 
!
! Arguments
!
    integer                                       :: nst           ! Current time step counter 
!
! Local variables
!
    logical                                       :: error         ! Flag=TRUE if an error is encountered 
!
!! executable statements -------------------------------------------------------
!
    lundia              => gdp%gdinout%lundia
    lunprt              => gdp%gdinout%lunprt
    rhow                => gdp%gdphysco%rhow
    ktemp               => gdp%gdtricom%ktemp
    grdang              => gdp%gdtricom%grdang
    prsmap              => gdp%gdtricom%prsmap
    selmap              => gdp%gdtricom%selmap
    prshis              => gdp%gdtricom%prshis
    selhis              => gdp%gdtricom%selhis
    comfil              => gdp%gdtricom%comfil
    runid               => gdp%runid
    trifil              => gdp%gdtricom%trifil
    versio              => gdp%gdtricom%versio
    iphisc              => gdp%gdtricom%iphisc
    itcomc              => gdp%gdtricom%itcomc
    itcur               => gdp%gdtricom%itcur
    itdroc              => gdp%gdtricom%itdroc
    ithisc              => gdp%gdtricom%ithisc
    itimc               => gdp%gdtricom%itimc
    itmapc              => gdp%gdtricom%itmapc
    itrstc              => gdp%gdtricom%itrstc
    npmap               => gdp%gdtricom%npmap
    ntcur               => gdp%gdtricom%ntcur
    dtsec               => gdp%gdtricom%dtsec
    !
    ! Call postpr
    !
    error = .false.
    !
    call psemnefis
    call timer_start(timer_postpr, gdp)
    call postpr(lundia    ,lunprt    ,error     ,versio    ,comfil    , &
              & trifil    ,runid     ,prsmap    ,prshis    ,selmap    , &
              & selhis    ,rhow      ,grdang    ,dtsec     , &
              & nst+1     ,iphisc    ,npmap     ,itcomc    ,itimc     , &
              & itcur     ,ntcur     ,ithisc    ,itmapc    ,itdroc    , &
              & itrstc    ,ktemp     ,.true.    ,gdp       )
    call timer_stop(timer_postpr, gdp)
    call vsemnefis
    !
    !if (error) return
  end subroutine postpr_hdt
