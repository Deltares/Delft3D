subroutine shearsettle(dll_function, dll_handle, max_integers, max_reals, max_strings , &
                     & dll_integers, dll_reals , dll_strings , lundia   , iform_settle, &
                     & parloc      , npar      , wsloc       , ifirst_settle   , error  )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2016.                                
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
!  $Id$
!  $HeadURL$
!!--description-----------------------------------------------------------------
!
! Compute fall velocity of mud based on Parameterised flocculation model
! Winterwerp 2004
!
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts, only: pi, ee
    use sediment_basics_module, only: dgravel, dsand, SEDTYP_CLAY, SEDTYP_SAND
    use morphology_data_module
    use message_module, only: write_error
    !
    implicit none
!
! Global variables
!
    character(256)                                          , intent(in)  :: dll_function
    integer(pntrsize)                                       , intent(in)  :: dll_handle
    !
    integer                                                 , intent(in)  :: max_integers
    integer                                                 , intent(in)  :: max_reals
    integer                                                 , intent(in)  :: max_strings
    integer            , dimension(max_integers)            , intent(in)  :: dll_integers
    real(hp)           , dimension(max_reals)               , intent(in)  :: dll_reals
    character(256)     , dimension(max_strings)             , intent(in)  :: dll_strings
    !
    integer                                                               :: lundia
    integer                                                 , intent(in)  :: iform_settle
    integer                                                 , intent(in)  :: npar
    real(fp)           , dimension(npar)                    , intent(in)  :: parloc
    real(fp)                                                , intent(out) :: wsloc
    logical                                                 , intent(out) :: error
    
!
! Local variables
!
    real(fp)                              :: ag
    real(fp)                              :: dss
    real(fp)                              :: rhocf
    real(fp)                              :: rhosol
    real(fp)                              :: wss
    ! real(fp)                              :: salint
    ! real(fp)                              :: silint
    real(fp)                              :: rets
    real(fp)                              :: retco
    real(fp)                              :: sasim
    real(fp)                              :: shrset
    real(fp)                              :: cfvic
    real(fp)                              :: csoil
    real(fp)                              :: buoyan
    real(fp)                              :: phiclayint
    real(fp)                              :: phisandint
    real(fp)                              :: clayint
    real(fp)                              :: sandint
    real(fp)                              :: rhoint
    real(fp)                              :: SluSettParam1
    real(fp)                              :: SluSettParam2
    real(fp)                              :: Shearsettle_w_opt
    real(fp)                              :: phisim
    character(256)                        :: errmsg
    integer                               :: ifirst_settle ! Flag to initialize the Shearsettle message
!
! executable statements -------------------------------------------------------
!
    error      = .false.
    retco      = 2.0_fp
    sasim      = 0.6_fp
    shrset     = 1.0_fp
    SluSettParam1 = 1.0_fp
    SluSettParam2 = 2.0_fp
    phiclayint = real(dll_reals(WS_RP_PHICL),fp)   
    phisandint = real(dll_reals(WS_RP_PHISA),fp) 
    rhosol     = real(dll_reals(WS_RP_RHOSL),fp)
    dss        = real(dll_reals(WS_RP_DSS  ),fp)
    ag         = real(dll_reals(WS_RP_GRAV ),fp)
    rhocf      = real(dll_reals(WS_RP_RHOCF),fp)
    cfvic      = real(dll_reals(WS_RP_CFVIC),fp)
    !
    clayint    = real(dll_reals(WS_RP_CLYINT),fp)
    sandint    = real(dll_reals(WS_RP_SNDINT),fp)
    phisim     = real(dll_reals(WS_RP_PHISIM),fp)
    Shearsettle_w_opt = real(dll_reals(WS_RP_FOROPT),fp)
    !
    rhoint  = real(dll_reals(WS_RP_RHOWT),fp)
    !temint  = real(dll_reals(WS_RP_TEMP ),fp)
    !salint  = real(dll_reals(WS_RP_SALIN),fp)
    !d50     = real(dll_reals(WS_RP_D50  ),fp)
    !ctot    = real(dll_reals(WS_RP_CTOT ),fp)
    !ctotab  = real(dll_reals(WS_RP_CTOTA),fp)
    !csoil   = real(dll_reals(WS_RP_CSOIL),fp)
    !phiclay = real(dll_reals(WS_RP_PHICL),fp)
    !phisand = real(dll_reals(WS_RP_PHISA),fp)
    !shear   = real(dll_reals(WS_RP_SHR  ),fp)
    !
    csoil  = real(dll_reals(WS_RP_CSOIL),fp)  ! cref, cgel.
    !
    ! CarrierFluid is waterClay
    !
    if (Shearsettle_w_opt == 1.0_fp)then
       wss = (SluSettParam1/18.0_fp)*((rhosol-rhoint) * ag * dss**2 / (cfvic*rhocf))  ! version 1. Arno's formula
       if (ifirst_settle==1) then
          write(errmsg,'(a,a,a)') ' Settling velocity formula of Talmon is used!'
          call write_error(errmsg, unit=lundia)     
          ifirst_settle = 0
       endif
    elseif (Shearsettle_w_opt == 2.0_fp)then
       wss = (SluSettParam1/18.0_fp)*((rhosol-rhocf) * ag * dss**2 / (cfvic*rhocf))  ! version 2. Han's formula
       if (ifirst_settle==1) then
          write(errmsg,'(a,a,a)') ' Settling velocity formula of Winterwerp is used!'
          call write_error(errmsg, unit=lundia)     
          ifirst_settle = 0
       endif
    else
       write(errmsg,'(a,a,a)') ' Settling velocity formula is not defined!  ERROR!!!'
       call write_error(errmsg, unit=lundia)     
       return
    endif
    !
    ! Hindered settling
    !
    if (Shearsettle_w_opt == 1.0_fp)then
       !version1: Arno's formula
       rets = max( 0.0_fp , (1.0_fp - clayint/csoil - sandint/rhosol))  !return flow 
       rets = rets* (1.0_fp - (sandint/rhosol)/phisim)**SluSettParam2
    elseif (Shearsettle_w_opt == 2.0_fp)then
       !version 2: Han's formula
       rets = max( 0.0_fp , (1.0_fp - clayint/csoil - sandint/rhosol))**SluSettParam2  !return flow  
    else
       write(errmsg,'(a,a,a)') ' Settling velocity formula is not defined!  ERROR!!!'
       call write_error(errmsg, unit=lundia)     
       return
    endif
    wsloc   = wss * rets

    ! rets    = max( 0.0_fp , (1.0_fp-phisandint/sasim) )  ! return flow
    ! buoyan  = max( 0.0_fp , (1.0_fp-(phiclayint+phisandint)) )
    ! wsloc   = wss * buoyan * rets**SluSettParam2
    
    ! for test 8-2-2018
    ! ws = 0.0001_fp 
    !
 end subroutine shearsettle
