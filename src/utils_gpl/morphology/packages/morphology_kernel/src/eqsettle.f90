subroutine eqsettle(dll_function, dll_handle, max_integers, max_reals, max_strings, &
                  & dll_integers, dll_reals, dll_strings, lundia, iform_settle, &
                  & parloc, npar, wsloc, error)
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
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts, only: pi, ee
    use sediment_basics_module, only: dgravel, dsand
    use morphology_data_module
    use flocculation, only: macro_floc_settling_manning, micro_floc_settling_manning, floc_manning, &
        & macro_floc_settling_chassagne, micro_floc_settling_chassagne, floc_chassagne
    use message_module, only: write_error
    use iso_c_binding, only: c_char
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
    integer(pntrsize)           :: error_ptr
    integer(pntrsize), external :: perf_function_fallve
    real(hp)                    :: ws_dll
    character(256)              :: errmsg
    character(256)              :: message        ! Contains message from shared library
    character(kind=c_char)      :: message_c(257) ! C- version of "message", including C_NULL_CHAR
                                                  ! Calling perf_function_eqtran with "message" caused problems
                                                  ! Solved by using "message_c"
    integer                     :: i
    integer                     :: l = 0
    logical                     :: apply_hinset
    real(fp)                    :: rhow
    real(fp)                    :: rhosol
    real(fp)                    :: temint
    real(fp)                    :: salint
    real(fp)                    :: dss
    real(fp)                    :: ag
    real(fp)                    :: d50
    real(fp)                    :: cclay
    real(fp)                    :: ctot
    real(fp)                    :: csoil
    real(fp)                    :: s
    real(fp)                    :: tdiss
    real(fp)                    :: tshear
    real(fp)                    :: vonkar
    real(fp)                    :: settling_flux
    real(fp)                    :: macro_frac  ! macro floc fraction
    real(fp)                    :: ws_macro
    real(fp)                    :: ws_micro
    real(fp)                    :: vcmol
    real(fp)                    :: coefw
    real(fp)                    :: ffloc
    real(fp)                    :: cgel
    real(fp)                    :: hinset
    real(fp)                    :: fhulp
    real(fp)                    :: efloc
    real(fp)                    :: ffloc0
    real(fp)                    :: a
    real(fp)                    :: b
    real(fp)                    :: cflocc
    real(fp)                    :: enhfac ! settling enhancement factor in salinity function
    real(fp)                    :: ws0
    real(fp)                    :: wsm
    real(fp)                    :: salmax
    real(fp)                    :: sedtc
    real(fp)                    :: gamflc
    real(fp)                    :: npow
    real(fp)                    :: d_micro
    real(fp)                    :: ustar_macro
!
!! executable statements -------------------------------------------------------
!
    error = .false.
    apply_hinset = .false.
    select case (iform_settle)
    case (WS_FORM_FUNCTION_SALTEMCON)
       salint = real(dll_reals(WS_RP_SALIN),fp)
       temint = real(dll_reals(WS_RP_TEMP ),fp)
       ctot   = real(dll_reals(WS_RP_CTOT ),fp)
       salmax = parloc(1)
       ws0    = parloc(2)
       wsm    = parloc(3)
       sedtc  = parloc(4)
       cflocc = parloc(5)
       npow   = parloc(6)
       !
       ! salinity effect
       !
       if (salint<salmax .and. salmax>0.0_fp) then
          enhfac = wsm/ws0
          a = 1.0_fp + enhfac
          b = enhfac - 1.0_fp
          wsloc = 0.5_fp * ws0 * (a-b*cos(pi*salint/salmax))
       else
          wsloc = wsm
       endif
       !
       ! temperature effect
       !
       wsloc = wsloc * sedtc**(temint - 20.0_fp)
       !
       ! simple flocculation effect
       !
       wsloc = wsloc * min((ctot/cflocc)**npow, 1.0_fp)
       apply_hinset = .true.

    case (WS_FORM_FUNCTION_DSS, WS_FORM_FUNCTION_DSS_2004)
       rhow   = real(dll_reals(WS_RP_RHOWT),fp)
       rhosol = real(dll_reals(WS_RP_RHOSL),fp)
       temint = real(dll_reals(WS_RP_TEMP ),fp)
       salint = real(dll_reals(WS_RP_SALIN),fp)
       dss    = real(dll_reals(WS_RP_DSS  ),fp)
       ag     = real(dll_reals(WS_RP_GRAV ),fp)
       d50    = real(dll_reals(WS_RP_D50  ),fp)
       ctot   = real(dll_reals(WS_RP_CTOT ),fp)
       csoil  = real(dll_reals(WS_RP_CSOIL),fp)
       salmax = parloc(1)
       gamflc = parloc(2)
       !
       s = rhosol / rhow
       !
       ! Molecular viscosity vcmol computed according to Van Rijn (2004) sediment tranport
       ! vicmol only matches this value if temperature is not explicitly modeled.
       !
       vcmol = 4.0e-5_fp / (20.0_fp + temint)
       if (dss < 1.5_fp*dsand) then
          wsloc = (s-1.0_fp) * ag * dss**2/(18.0_fp*vcmol)
       elseif (dss < 0.5_fp*dgravel) then
          if (dss < 2.0_fp*dsand) then
             coefw = (-2.9912_fp/dsand) * dss + 15.9824_fp
          else
             coefw = 10.0_fp
          endif
          wsloc = coefw * vcmol / dss                           &
                       & * (sqrt(1.0_fp + (s-1.0_fp)*ag*dss**3  &
                       & / (100.0_fp*vcmol**2)) - 1.0_fp)
       else
          wsloc = 1.1_fp * sqrt((s-1.0_fp)*ag*dss)
       endif
       !
       ffloc = 1.0_fp
       if (  iform_settle == WS_FORM_FUNCTION_DSS_2004 &
           & .and. d50 < dsand                         &
           & .and. salint > 0.0_fp                     ) then
          !
          ! Hindered settling (Van Rijn, 2004)
          !
          cgel = 0.65_fp * rhosol * min(d50/dsand , 1.0_fp)
          hinset = max(0.0_fp , (1.0 - max(0.0_fp, 0.65_fp*ctot)/cgel))
          !
          ! Flocculation (Van Rijn, 2004)
          !
          if (salint >= 0.01_fp .and. salmax>0.0_fp) then
             fhulp = max(4.0_fp+log10(2.0_fp*max(1.0e-6_fp,ctot)/cgel) , 1.0_fp)
             efloc = min(max(dsand/d50-1.0_fp , 1.0_fp) , 3.0_fp)
             ffloc0 = max(min(fhulp**efloc , 10.0_fp) , 1.0_fp)
             ffloc = (ffloc0-1.0_fp) * min(1.0_fp,salint/salmax) + 1.0_fp
             !
             ! Calibration parameter for flocculation
             !
             ffloc = ffloc * gamflc
             !
             ffloc = max(min(ffloc , 10.0_fp) , 1.0_fp)
          endif
       else
          ! call hinset_Richardson_and_Zaki()
          hinset = max(0.0_fp , (1.0_fp - max(0.0_fp , ctot)/csoil)) 
       endif
       wsloc = ffloc * wsloc * hinset**5
       apply_hinset = .false.

    case (WS_FORM_MANNING_DYER_MACRO)
       !
       ! Settling velocity for macro flocs according Manning and Dyer
       !
       cclay  = real(dll_reals(WS_RP_CCLAY),fp) * 1000.0_fp
       tshear = real(dll_reals(WS_RP_SHTUR),fp)
       call macro_floc_settling_manning( cclay, tshear, wsloc )
       apply_hinset = .true.

    case (WS_FORM_MANNING_DYER_MICRO)
       !
       ! Settling velocity for micro flocs according Manning and Dyer
       !
       tshear = real(dll_reals(WS_RP_SHTUR),fp)
       call micro_floc_settling_manning( tshear, wsloc )
       apply_hinset = .true.

    case (WS_FORM_MANNING_DYER)
       !
       ! Settling velocity based on flocculation model by Manning and Dyer
       !
       cclay  = real(dll_reals(WS_RP_CCLAY),fp) * 1000.0_fp ! convert kg/m3 to g/m3
       tshear = real(dll_reals(WS_RP_SHTUR),fp)
       call floc_manning( cclay, tshear, wsloc, macro_frac, ws_macro, ws_micro )
       apply_hinset = .true.

    case (WS_FORM_CHASSAGNE_SAFAR_MACRO)
       !
       ! Settling velocity for macro flocs according Chassagne and Safar
       !
       cclay  = real(dll_reals(WS_RP_CCLAY),fp) * 1000.0_fp ! convert kg/m3 to g/m3
       ag     = real(dll_reals(WS_RP_GRAV ),fp)
       tshear = real(dll_reals(WS_RP_SHTUR),fp)
       tdiss  = real(dll_reals(WS_RP_EPTUR),fp)
       rhow   = real(dll_reals(WS_RP_RHOWT),fp)
       vcmol  = real(dll_reals(WS_RP_VICML),fp)
       d_micro      = parloc(1)
       ustar_macro  = parloc(2)
       call macro_floc_settling_chassagne( cclay, tshear, tdiss, ag, vcmol, rhow, d_micro, ustar_macro, wsloc )
       apply_hinset = .true.

    case (WS_FORM_CHASSAGNE_SAFAR_MICRO)
       !
       ! Settling velocity for macro flocs according Chassagne and Safar
       !
       ag     = real(dll_reals(WS_RP_GRAV ),fp)
       tshear = real(dll_reals(WS_RP_SHTUR),fp)
       tdiss  = real(dll_reals(WS_RP_EPTUR),fp)
       rhow   = real(dll_reals(WS_RP_RHOWT),fp)
       vcmol  = real(dll_reals(WS_RP_VICML),fp)
       call micro_floc_settling_chassagne( tshear, tdiss, ag, vcmol, rhow, wsloc )
       apply_hinset = .true.

    case (WS_FORM_CHASSAGNE_SAFAR)
       !
       ! Settling velocity based on flocculation model by Chassagne and Safar
       !
       cclay  = real(dll_reals(WS_RP_CCLAY),fp) * 1000.0_fp ! convert kg/m3 to g/m3
       ag     = real(dll_reals(WS_RP_GRAV ),fp)
       tshear = real(dll_reals(WS_RP_SHTUR),fp)
       tdiss  = real(dll_reals(WS_RP_EPTUR),fp)
       rhow   = real(dll_reals(WS_RP_RHOWT),fp)
       vcmol  = real(dll_reals(WS_RP_VICML),fp)
       d_micro      = parloc(1)
       ustar_macro  = parloc(2)
       call floc_chassagne( cclay, tshear, tdiss, ag, vcmol, rhow, d_micro, ustar_macro, wsloc, macro_frac, ws_macro, ws_micro )
       apply_hinset = .true.

    case (WS_FORM_USER_ROUTINE)
       !
       ! Settling velocity routine supplied by the user in a DLL
       !
       ! Initialisation of output variables of user defined settling velocity routine
       !
       ws_dll    = 0.0_hp
       message     = ' '
       do i=1,256
          message_c(i) = message(i:i)
       enddo
       message_c(257) = C_NULL_CHAR
       !
       ! psem/vsem is used to be sure this works fine in DD calculations
       !
       call psemlun
       error_ptr = 0
       error_ptr = perf_function_fallve(dll_handle          , dll_function          , &
                                        dll_integers        , max_integers          , &
                                        dll_reals           , max_reals             , &
                                        dll_strings         , max_strings           , &
                                        ws_dll              , message_c)
       message = transfer(message_c(1:256), message)
       call vsemlun
       if (error_ptr /= 0) then
          write(errmsg,'(a,a,a)') 'Cannot find function "',trim(dll_function),'" in dynamic library.'
          call write_error(errmsg, unit=lundia)
          error = .true.
          return
       endif
       if (message /= ' ') then
          write (errmsg,'(a,a,a)') '*** ERROR Message from user defined settling velocity routine ',trim(dll_function),' :'
          call write_error(errmsg, unit=lundia)
          call write_error(message, unit=lundia)
          call write_error(' ', unit=lundia)
          error = .true.
          return
       endif
       !
       ! Output parameters
       !
       wsloc = real(ws_dll,fp)
       apply_hinset = .false.

    case default
       errmsg = 'Settling formula not recognized'
       call write_error(errmsg, unit=lundia)
       error = .true.
       return
    end select
    
    if (apply_hinset) then
        call hinset_Richardson_and_Zaki()
    endif

    contains

    subroutine hinset_Richardson_and_Zaki()
       !
       ! hindered settling Richardson and Zaki/Mehta
       !
       ctot   = real(dll_reals(WS_RP_CTOT ),fp)
       csoil  = real(dll_reals(WS_RP_CSOIL),fp) ! TODO: in Van Rijn (2005) csoil = cgel/0.65, can we make this consistent?
       hinset = max(0.0_fp , (1.0_fp - max(0.0_fp , ctot)/csoil))
       wsloc = wsloc * hinset**5
    end subroutine hinset_Richardson_and_Zaki
end subroutine eqsettle
