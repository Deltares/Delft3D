module flow2d3d_timers
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
! Adding a timer:
! 1) Add an identification name (module parameter)
!    Example:
!    integer, parameter :: timer_new          = 12
! 2) Increase numtimers (module parameter)
!    Example:
!    integer, parameter :: numtimers          = 12
! 3) Add the name/description of the timer (module subroutine timers_init)
!    Maximum of 20 characters
!    Example:
!    gdp%gdtimers%names(timer_new)       = 'My own new timer'
! 4) Add an explanation (module subroutine timers_finish)
!    Example:
!    write(lundia,'(a)') '|Momentum eq.   : Part of Simulation ("UZD")|'
! 5) Add in the source code calls to timer_start and timer_stop around the
!    code you want to time
!    Example:
!    call timer_start(timer_new, gdp)
!    ... code to be timed ...
!    call timer_stop(timer_new, gdp)
! Remarks/restrictions:
! - The subroutines containing calls to timer_start/timer_stop must contain
!   the line:
!   use flow2d3d_timers
! - The subroutines containing calls to timer_start/timer_stop must contain
!   the pointer gdp to the Global Data.
! - These timers can only be used inside FLOW;
!   not in the mapper, hydra executive etc.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
use precision
implicit none
!
! array usedcp has dimensions (numtimers,4)
! The following 4 parameters should be used to point to the correct array position
!
integer, parameter :: starttime     =  1
integer, parameter :: sumtime       =  2
integer, parameter :: starttime_cpu =  3
integer, parameter :: sumtime_cpu   =  4
!
integer, parameter :: timer_total               = 1
integer, parameter :: timer_init                = timer_total + 1
integer, parameter :: timer_simulation          = timer_init + 1
integer, parameter :: timer_close               = timer_simulation + 1
integer, parameter :: timer_uzd                 = timer_close + 1
integer, parameter :: timer_sud                 = timer_uzd + 1
integer, parameter :: timer_difu                = timer_sud + 1
integer, parameter :: timer_turbulence          = timer_difu + 1
integer, parameter :: timer_3dmor               = timer_turbulence + 1
integer, parameter :: timer_waitdd              = timer_3dmor + 1
integer, parameter :: timer_wait                = timer_waitdd + 1
!
! Extra timers (output on keyword 'AddTim'):
!
! *** in tricom ***
integer, parameter :: timer_tricomtot           = timer_wait + 1    
integer, parameter :: timer_d3dflowinit         = timer_tricomtot + 1
integer, parameter :: timer_timeintegr          = timer_d3dflowinit + 1
integer, parameter :: timer_step2screen         = timer_timeintegr + 1
integer, parameter :: timer_postpr              = timer_step2screen + 1
integer, parameter :: timer_trisol              = timer_postpr + 1
integer, parameter :: timer_tricom_rest         = timer_trisol + 1
! *** in trisol ***
integer, parameter :: timer_trisol_ini          = timer_tricom_rest + 1
integer, parameter :: timer_trisol_fluidmud     = timer_trisol_ini + 1
integer, parameter :: timer_nodal_factor        = timer_trisol_fluidmud + 1
integer, parameter :: timer_incmeteo            = timer_nodal_factor + 1
integer, parameter :: timer_incbc               = timer_incmeteo + 1
integer, parameter :: timer_incrbc              = timer_incbc + 1
integer, parameter :: timer_incbcc              = timer_incrbc + 1
integer, parameter :: timer_incdis              = timer_incbcc + 1
integer, parameter :: timer_culver              = timer_incdis + 1
integer, parameter :: timer_trisol_heat         = timer_culver + 1
integer, parameter :: timer_filterstr           = timer_trisol_heat + 1
integer, parameter :: timer_sousin              = timer_filterstr + 1
integer, parameter :: timer_sourmu              = timer_sousin + 1
integer, parameter :: timer_turclo              = timer_sourmu + 1
integer, parameter :: timer_chkvic              = timer_turclo + 1
integer, parameter :: timer_iwe00               = timer_chkvic + 1
integer, parameter :: timer_dengra              = timer_iwe00 + 1
integer, parameter :: timer_tfzeta              = timer_dengra + 1
integer, parameter :: timer_cdwkad              = timer_tfzeta + 1
integer, parameter :: timer_hydkad              = timer_cdwkad + 1
integer, parameter :: timer_updbar              = timer_hydkad + 1
integer, parameter :: timer_1stadi              = timer_updbar + 1
integer, parameter :: timer_massfl              = timer_1stadi + 1
integer, parameter :: timer_euler               = timer_massfl + 1
integer, parameter :: timer_orbvel              = timer_euler + 1
integer, parameter :: timer_upwhu               = timer_orbvel + 1
integer, parameter :: timer_taubot              = timer_upwhu + 1
integer, parameter :: timer_caltmx              = timer_taubot + 1
integer, parameter :: timer_trisol_hles         = timer_caltmx + 1
integer, parameter :: timer_secrhs              = timer_trisol_hles + 1
integer, parameter :: timer_discha              = timer_secrhs + 1
integer, parameter :: timer_heatu               = timer_discha + 1
integer, parameter :: timer_thahbc              = timer_heatu + 1
integer, parameter :: timer_trakad              = timer_thahbc + 1
integer, parameter :: timer_fallve              = timer_trakad + 1
integer, parameter :: timer_erosed              = timer_fallve + 1
integer, parameter :: timer_tritra              = timer_erosed + 1
integer, parameter :: timer_tratur              = timer_tritra + 1
integer, parameter :: timer_tur2d               = timer_tratur + 1
integer, parameter :: timer_forfil              = timer_tur2d + 1
integer, parameter :: timer_drotim              = timer_forfil + 1
integer, parameter :: timer_dersig              = timer_drotim + 1
integer, parameter :: timer_dens                = timer_dersig + 1
integer, parameter :: timer_bott3d              = timer_dens + 1
integer, parameter :: timer_f0isf1              = timer_bott3d + 1
integer, parameter :: timer_2ndadi              = timer_f0isf1 + 1
integer, parameter :: timer_calksc              = timer_2ndadi + 1
integer, parameter :: timer_trtrou              = timer_calksc + 1
integer, parameter :: timer_wphys               = timer_trtrou + 1
integer, parameter :: timer_cvort               = timer_wphys + 1
integer, parameter :: timer_nonhyd              = timer_cvort + 1
integer, parameter :: timer_trisol_rest         = timer_nonhyd + 1
! *** in adi ***
integer, parameter :: timer_checku              = timer_trisol_rest + 1
integer, parameter :: timer_1stuzd              = timer_checku + 1
integer, parameter :: timer_1stsud              = timer_1stuzd + 1
integer, parameter :: timer_drychk              = timer_1stsud + 1
integer, parameter :: timer_comvol              = timer_drychk + 1
integer, parameter :: timer_2nduzd              = timer_comvol + 1
integer, parameter :: timer_2ndsud              = timer_2nduzd + 1
! *** in sud ***
integer, parameter :: timer_sud_cucnp           = timer_2ndsud + 1
integer, parameter :: timer_sud_cucbp           = timer_sud_cucnp + 1
integer, parameter :: timer_sud_rowsc           = timer_sud_cucbp + 1
integer, parameter :: timer_sud_solve           = timer_sud_rowsc + 1
integer, parameter :: timer_sud_wangpre         = timer_sud_solve + 1
integer, parameter :: timer_sud_gwsslv          = timer_sud_wangpre + 1
integer, parameter :: timer_sud_wangback        = timer_sud_gwsslv + 1
integer, parameter :: timer_sud_cucdp           = timer_sud_wangback + 1
integer, parameter :: timer_sud_veldisch        = timer_sud_cucdp + 1
integer, parameter :: timer_sud_rest            = timer_sud_veldisch + 1
! *** in uzd ***
integer, parameter :: timer_uzd_ini             = timer_sud_rest + 1
integer, parameter :: timer_uzd_momsol          = timer_uzd_ini + 1
integer, parameter :: timer_uzd_rhs             = timer_uzd_momsol + 1
integer, parameter :: timer_uzd_eloss           = timer_uzd_rhs + 1
integer, parameter :: timer_uzd_stress          = timer_uzd_eloss + 1
integer, parameter :: timer_uzd_shrwav          = timer_uzd_stress + 1
integer, parameter :: timer_uzd_dismmt          = timer_uzd_shrwav + 1
integer, parameter :: timer_uzd_vih             = timer_uzd_dismmt + 1
integer, parameter :: timer_uzd_advdiffv        = timer_uzd_vih + 1
integer, parameter :: timer_uzd_bouncond        = timer_uzd_advdiffv + 1
integer, parameter :: timer_uzd_lhs             = timer_uzd_bouncond + 1
integer, parameter :: timer_uzd_rowsc           = timer_uzd_lhs + 1
integer, parameter :: timer_uzd_solve1          = timer_uzd_rowsc + 1
integer, parameter :: timer_uzd_solve2          = timer_uzd_solve1 + 1
integer, parameter :: timer_uzd_solve3u         = timer_uzd_solve2 + 1
integer, parameter :: timer_uzd_solve4u         = timer_uzd_solve3u + 1
integer, parameter :: timer_uzd_solve5v         = timer_uzd_solve4u + 1
integer, parameter :: timer_uzd_solve6v         = timer_uzd_solve5v + 1
integer, parameter :: timer_uzd_umean           = timer_uzd_solve6v + 1
integer, parameter :: timer_uzd_rest            = timer_uzd_umean + 1
! *** in tritra ***
integer, parameter :: timer_1stdifu             = timer_uzd_rest + 1
integer, parameter :: timer_2nddifu             = timer_1stdifu + 1
integer, parameter :: timer_tritra_rest         = timer_2nddifu + 1
! *** in difu ***
integer, parameter :: timer_difu_ini            = timer_tritra_rest + 1
integer, parameter :: timer_difu_horadv         = timer_difu_ini + 1
integer, parameter :: timer_difu_hordiff        = timer_difu_horadv + 1
integer, parameter :: timer_difu_vertadv        = timer_difu_hordiff + 1
integer, parameter :: timer_difu_vertdiff       = timer_difu_vertadv + 1
integer, parameter :: timer_difu_difws          = timer_difu_vertdiff + 1
integer, parameter :: timer_difu_bounopen       = timer_difu_difws + 1
integer, parameter :: timer_difu_sourcesink     = timer_difu_bounopen + 1
integer, parameter :: timer_difu_secbou         = timer_difu_sourcesink + 1
integer, parameter :: timer_difu_lhs            = timer_difu_secbou + 1
integer, parameter :: timer_difu_rowsc          = timer_difu_lhs + 1
integer, parameter :: timer_difu_solve1         = timer_difu_rowsc + 1
integer, parameter :: timer_difu_solve2         = timer_difu_solve1 + 1
integer, parameter :: timer_difu_solve3         = timer_difu_solve2 + 1
integer, parameter :: timer_difu_solve4u        = timer_difu_solve3 + 1
integer, parameter :: timer_difu_solve5u        = timer_difu_solve4u + 1
integer, parameter :: timer_difu_solve6v        = timer_difu_solve5u + 1
integer, parameter :: timer_difu_solve7v        = timer_difu_solve6v + 1
integer, parameter :: timer_difu_rest           = timer_difu_solve7v + 1
! *** in cucnp ***
integer, parameter :: timer_cucnp_ini           = timer_difu_rest + 1
integer, parameter :: timer_cucnp_momsol        = timer_cucnp_ini + 1
integer, parameter :: timer_cucnp_rhs           = timer_cucnp_momsol + 1
integer, parameter :: timer_cucnp_eloss         = timer_cucnp_rhs + 1
integer, parameter :: timer_cucnp_stress        = timer_cucnp_eloss + 1
integer, parameter :: timer_cucnp_shrwav        = timer_cucnp_stress + 1
integer, parameter :: timer_cucnp_dismmt        = timer_cucnp_shrwav + 1
integer, parameter :: timer_cucnp_advdiffv      = timer_cucnp_dismmt + 1
integer, parameter :: timer_cucnp_vih           = timer_cucnp_advdiffv + 1
integer, parameter :: timer_cucnp_rowsc         = timer_cucnp_vih + 1
integer, parameter :: timer_cucnp_lhs           = timer_cucnp_rowsc + 1
integer, parameter :: timer_cucnp_vihsec        = timer_cucnp_lhs + 1
! *** in parallel routines ***
integer, parameter :: timer_dfexchg             = timer_cucnp_vihsec + 1
integer, parameter :: timer_dfreduce            = timer_dfexchg + 1
! Total number of timers
integer, parameter :: numtimers                 = timer_dfreduce
contains
!
!
!
!==============================================================================
subroutine timers_init (gdp)
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
!
! Local variables
!
    integer :: istat
!
!! executable statements -------------------------------------------------------
!
                    allocate (gdp%gdtimers%usedcp(numtimers,4), stat = istat)
    if (istat == 0) allocate (gdp%gdtimers%names (numtimers)  , stat = istat)
    if (istat /= 0) then
       call prterr(gdp%gdinout%lundia, 'U021', 'timers_init: memory alloc error')
       call d3stop(1, gdp)
    endif
    gdp%gdtimers%names(timer_total)            = 'Total'
    gdp%gdtimers%names(timer_init)             = 'Initialization'
    gdp%gdtimers%names(timer_simulation)       = 'Simulation'
    gdp%gdtimers%names(timer_close)            = 'Close and stop'
    gdp%gdtimers%names(timer_uzd)              = 'Momentum eq.'
    gdp%gdtimers%names(timer_sud)              = 'Continuity eq.'
    gdp%gdtimers%names(timer_difu)             = 'Transport eq.'
    gdp%gdtimers%names(timer_turbulence)       = 'Turbulence'
    gdp%gdtimers%names(timer_3dmor)            = '3D Morphology'
    gdp%gdtimers%names(timer_waitdd)           = 'Wait (dd   module)'
    gdp%gdtimers%names(timer_wait)             = 'Wait (ext. modules)'
    !
    ! starttime = -1.0 denotes that the related timer is not started
    !
    ! *** in tricom ***
    gdp%gdtimers%names(timer_tricomtot)        = 'Tricom total'
    gdp%gdtimers%names(timer_d3dflowinit)      = 'd3dflow_init'
    gdp%gdtimers%names(timer_timeintegr)       = 'Time integration'
    gdp%gdtimers%names(timer_step2screen)      = 'step2screen'
    gdp%gdtimers%names(timer_postpr)           = 'postprocessing'
    gdp%gdtimers%names(timer_trisol)           = 'Trisol'
    gdp%gdtimers%names(timer_tricom_rest)      = 'Tricom_rest'
    ! *** in trisol ***
    gdp%gdtimers%names(timer_trisol_ini)       = 'trisol_ini'
    gdp%gdtimers%names(timer_trisol_fluidmud)  = 'trisol_fluidmud'
    gdp%gdtimers%names(timer_nodal_factor)     = 'nodal_factor'
    gdp%gdtimers%names(timer_incmeteo)         = 'incmeteo'
    gdp%gdtimers%names(timer_incbc)            = 'incbc'
    gdp%gdtimers%names(timer_incrbc)           = 'incrbc'
    gdp%gdtimers%names(timer_incbcc)           = 'incbcc'
    gdp%gdtimers%names(timer_incdis)           = 'incdis'
    gdp%gdtimers%names(timer_culver)           = 'culvert'
    gdp%gdtimers%names(timer_trisol_heat)      = 'trisol_heat'
    gdp%gdtimers%names(timer_filterstr)        = 'filterstructures'
    gdp%gdtimers%names(timer_sousin)           = 'sousin'
    gdp%gdtimers%names(timer_sourmu)           = 'sourmu'
    gdp%gdtimers%names(timer_turclo)           = 'turclo'
    gdp%gdtimers%names(timer_chkvic)           = 'chkvic'
    gdp%gdtimers%names(timer_iwe00)            = 'iwe00'
    gdp%gdtimers%names(timer_dengra)           = 'dengra'
    gdp%gdtimers%names(timer_tfzeta)           = 'tfzeta'
    gdp%gdtimers%names(timer_cdwkad)           = 'cdwkad'
    gdp%gdtimers%names(timer_hydkad)           = 'hydkad'
    gdp%gdtimers%names(timer_updbar)           = 'updbar'
    gdp%gdtimers%names(timer_1stadi)           = '1st adi'
    gdp%gdtimers%names(timer_massfl)           = 'massfl'
    gdp%gdtimers%names(timer_euler)            = 'euler'
    gdp%gdtimers%names(timer_orbvel)           = 'orbvel'
    gdp%gdtimers%names(timer_upwhu)            = 'upwhu'
    gdp%gdtimers%names(timer_taubot)           = 'taubot'
    gdp%gdtimers%names(timer_caltmx)           = 'caltmx'
    gdp%gdtimers%names(timer_trisol_hles)      = 'trisol_hles'
    gdp%gdtimers%names(timer_secrhs)           = 'secrhs'
    gdp%gdtimers%names(timer_discha)           = 'discha'
    gdp%gdtimers%names(timer_heatu)            = 'heatu'
    gdp%gdtimers%names(timer_thahbc)           = 'thahbc'
    gdp%gdtimers%names(timer_trakad)           = 'trakad'
    gdp%gdtimers%names(timer_fallve)           = 'fallve'
    gdp%gdtimers%names(timer_erosed)           = 'erosed'
    gdp%gdtimers%names(timer_tritra)           = 'tritra'
    gdp%gdtimers%names(timer_tratur)           = 'tratur'
    gdp%gdtimers%names(timer_tur2d)            = 'tur2d'
    gdp%gdtimers%names(timer_forfil)           = 'forfil'
    gdp%gdtimers%names(timer_drotim)           = 'drotim'
    gdp%gdtimers%names(timer_dersig)           = 'dersig'
    gdp%gdtimers%names(timer_dens)             = 'dens'
    gdp%gdtimers%names(timer_bott3d)           = 'bott3d'
    gdp%gdtimers%names(timer_f0isf1)           = 'f0isf1'
    gdp%gdtimers%names(timer_2ndadi)           = '2nd adi'
    gdp%gdtimers%names(timer_calksc)           = 'calksc'
    gdp%gdtimers%names(timer_trtrou)           = 'trtrou'
    gdp%gdtimers%names(timer_wphys)            = 'wphys'
    gdp%gdtimers%names(timer_cvort)            = 'c_vort'
    gdp%gdtimers%names(timer_nonhyd)           = 'non-hydrostatic'
    gdp%gdtimers%names(timer_trisol_rest)      = 'trisol_rest'
    ! *** in adi ***
    gdp%gdtimers%names(timer_checku)           = 'checku'
    gdp%gdtimers%names(timer_1stuzd)           = '1st_uzd'
    gdp%gdtimers%names(timer_1stsud)           = '1st_sud'
    gdp%gdtimers%names(timer_drychk)           = 'drychk'
    gdp%gdtimers%names(timer_comvol)           = 'comvol'
    gdp%gdtimers%names(timer_2nduzd)           = '2nd_uzd'
    gdp%gdtimers%names(timer_2ndsud)           = '2nd_sud'
    ! *** in sud ***
    gdp%gdtimers%names(timer_sud_cucnp)        = 'sud_cucnp'
    gdp%gdtimers%names(timer_sud_cucbp)        = 'sud_cucbp'
    gdp%gdtimers%names(timer_sud_rowsc)        = 'sud_rowsc'
    gdp%gdtimers%names(timer_sud_solve)        = 'sud_solve'
    gdp%gdtimers%names(timer_sud_wangpre)      = 'sud_wang(pre)'
    gdp%gdtimers%names(timer_sud_gwsslv)       = 'sud_gwsslv'
    gdp%gdtimers%names(timer_sud_wangback)     = 'sud_wang(back subst)'
    gdp%gdtimers%names(timer_sud_cucdp)        = 'sud_cucdp'
    gdp%gdtimers%names(timer_sud_veldisch)     = 'sud_veldisch'
    gdp%gdtimers%names(timer_sud_rest)         = 'sud_rest'
    ! *** in uzd ***
    gdp%gdtimers%names(timer_uzd_ini)          = 'uzd initialisation'
    gdp%gdtimers%names(timer_uzd_momsol)       = 'uzd_momsol'
    gdp%gdtimers%names(timer_uzd_rhs)          = 'uzd_rhs'
    gdp%gdtimers%names(timer_uzd_eloss)        = 'uzd_energy loss'
    gdp%gdtimers%names(timer_uzd_stress)       = 'uzd stress'
    gdp%gdtimers%names(timer_uzd_shrwav)       = 'uzd_shear wave'
    gdp%gdtimers%names(timer_uzd_dismmt)       = 'uzd_disch momentum'
    gdp%gdtimers%names(timer_uzd_vih)          = 'uzd_horizontal visc'
    gdp%gdtimers%names(timer_uzd_advdiffv)     = 'uzd_vert adv-diff'
    gdp%gdtimers%names(timer_uzd_bouncond)     = 'uzd_boundary cond'
    gdp%gdtimers%names(timer_uzd_lhs)          = 'uzd_lhs'
    gdp%gdtimers%names(timer_uzd_rowsc)        = 'uzd_rowscale'
    gdp%gdtimers%names(timer_uzd_solve1)       = 'uzd_solve1'
    gdp%gdtimers%names(timer_uzd_solve2)       = 'uzd_solve2'
    gdp%gdtimers%names(timer_uzd_solve3u)      = 'uzd_solve3u'
    gdp%gdtimers%names(timer_uzd_solve4u)      = 'uzd_solve4u'
    gdp%gdtimers%names(timer_uzd_solve5v)      = 'uzd_solve5v'
    gdp%gdtimers%names(timer_uzd_solve6v)      = 'uzd_solve6v'
    gdp%gdtimers%names(timer_uzd_umean)        = 'uzd_umean'
    gdp%gdtimers%names(timer_uzd_rest)         = 'uzd_rest'
    ! *** in tritra ***
    gdp%gdtimers%names(timer_1stdifu)          = '1stdifu'
    gdp%gdtimers%names(timer_2nddifu)          = '2nddifu'
    gdp%gdtimers%names(timer_tritra_rest)      = 'tritra_rest'
    ! *** in difu ***
    gdp%gdtimers%names(timer_difu_ini)         = 'difu_initialisation'
    gdp%gdtimers%names(timer_difu_horadv)      = 'difu_horadv'
    gdp%gdtimers%names(timer_difu_hordiff)     = 'difu_hordiff'
    gdp%gdtimers%names(timer_difu_vertadv)     = 'difu_vertadv'
    gdp%gdtimers%names(timer_difu_vertdiff)    = 'difu_vertdiff'
    gdp%gdtimers%names(timer_difu_difws)       = 'difu_difws'
    gdp%gdtimers%names(timer_difu_bounopen)    = 'difu_bounopen'
    gdp%gdtimers%names(timer_difu_sourcesink)  = 'difu_sourcesink'    
    gdp%gdtimers%names(timer_difu_secbou)      = 'difu_secbou'
    gdp%gdtimers%names(timer_difu_lhs)         = 'difu_lhs'
    gdp%gdtimers%names(timer_difu_rowsc)       = 'difu_rowsc'
    gdp%gdtimers%names(timer_difu_solve1)      = 'difu_solve1'
    gdp%gdtimers%names(timer_difu_solve2)      = 'difu_solve2'
    gdp%gdtimers%names(timer_difu_solve3)      = 'difu_solve3'
    gdp%gdtimers%names(timer_difu_solve4u)     = 'difu_solve4u'
    gdp%gdtimers%names(timer_difu_solve5u)     = 'difu_solve5u'
    gdp%gdtimers%names(timer_difu_solve6v)     = 'difu_solve6v'
    gdp%gdtimers%names(timer_difu_solve7v)     = 'difu_solve7v'
    gdp%gdtimers%names(timer_difu_rest)        = 'difu_rest'
    ! *** in cucnp ***
    gdp%gdtimers%names(timer_cucnp_ini)        = 'cucnp_initialisation'
    gdp%gdtimers%names(timer_cucnp_momsol)     = 'cucnp_momsol'
    gdp%gdtimers%names(timer_cucnp_rhs)        = 'cucnp_rhs'
    gdp%gdtimers%names(timer_cucnp_eloss)      = 'cucnp_energy loss'
    gdp%gdtimers%names(timer_cucnp_stress)     = 'cucnp stress'
    gdp%gdtimers%names(timer_cucnp_shrwav)     = 'cucnp_shear wave'
    gdp%gdtimers%names(timer_cucnp_dismmt)     = 'cucnp_disch momentum'
    gdp%gdtimers%names(timer_cucnp_advdiffv)   = 'cucnp_vert adv-diff'
    gdp%gdtimers%names(timer_cucnp_vih)        = 'cucnp_hor viscosity'
    gdp%gdtimers%names(timer_cucnp_rowsc)      = 'cucnp_rowscale'
    gdp%gdtimers%names(timer_cucnp_lhs)        = 'cucnp_lhs'
    gdp%gdtimers%names(timer_cucnp_vihsec)     = 'cucnp_vihsec'
    ! *** in parallel routines ***
    gdp%gdtimers%names(timer_dfexchg)          = 'dfexchg'
    gdp%gdtimers%names(timer_dfreduce)         = 'dfreduce'
    !
    ! starttime = -1.0 denotes that the related timer is not started
    !
    gdp%gdtimers%usedcp(:, starttime    ) = -1.0_hp
    gdp%gdtimers%usedcp(:, sumtime      ) =  0.0_hp
    gdp%gdtimers%usedcp(:, starttime_cpu) =  0.0_hp
    gdp%gdtimers%usedcp(:, sumtime_cpu  ) =  0.0_hp
    gdp%gdtimers%initialized = .true.
end subroutine timers_init
!
!
!
!==============================================================================
subroutine timer_start (timnum, gdp)
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
!
! Global variables
!
    integer, intent(in) :: timnum
!
! Local variables
!
   integer(long)                     :: tcount
   integer(long)                     :: trate
   integer(long)                     :: tmax
   real(sp)                          :: cputim
   real(hp), dimension(:,:), pointer :: usedcp
   character(80)                     :: message
!
!! executable statements -------------------------------------------------------
!
    ! When using DD, timer_start/timerstop calls occur after closing diagnosis
    ! file. Use logical initialized to recognize that situation
    !
    if (.not. gdp%gdtimers%initialized) return
    !
    usedcp => gdp%gdtimers%usedcp
    if ( usedcp(timnum,starttime) > 0.0_hp ) then
       !
       ! Only show a warning when there is no (more serious) error
       !
       if (gdp%errorcode == 0) then
          write (message,'(3a)') 'Timer_start: Timer ', trim(gdp%gdtimers%names(timnum)), ' has already been started'
          call prterr(gdp%gdinout%lundia, 'U190', trim(message))
       endif
    endif
    call system_clock(tcount, trate, tmax)
    call cpu_time(cputim)
    usedcp(timnum,starttime_cpu) = real(cputim,hp)
    usedcp(timnum,starttime)     = real(tcount,hp) / real(trate,hp)
end subroutine timer_start
!
!
!
!==============================================================================
function timer_sum (timnum, gdp)
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
!
! Global variables
!
    integer, intent(in) :: timnum
!
! Local variables
!
   integer(long)                     :: tcount
   integer(long)                     :: trate
   integer(long)                     :: tmax
   real(hp)                          :: timer_sum
   real(hp), dimension(:,:), pointer :: usedcp
   character(80)                     :: message
!
!! executable statements -------------------------------------------------------
!
    ! When using DD, timer_start/timerstop calls occur after closing diagnosis
    ! file. Use logical initialized to recognize that situation
    !
    if (.not. gdp%gdtimers%initialized) then
       timer_sum = 0.0_hp
       return
    endif
    !
    usedcp => gdp%gdtimers%usedcp
    if ( usedcp(timnum,starttime) < 0.0_hp ) then
       timer_sum   = usedcp(timnum,sumtime)
    else
       call system_clock(tcount, trate, tmax)
       timer_sum   = usedcp(timnum,sumtime) &
                   & + (real(tcount,hp)/real(trate,hp) - usedcp(timnum,starttime))
    endif
end function timer_sum
!
!
!
!==============================================================================
subroutine timer_stop (timnum, gdp)
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
!
! Global variables
!
    integer, intent(in) :: timnum
!
! Local variables
!
   integer(long)                     :: tcount
   integer(long)                     :: trate
   integer(long)                     :: tmax
   real(sp)                          :: cputim
   real(hp), dimension(:,:), pointer :: usedcp
   character(80)                     :: message
!
!! executable statements -------------------------------------------------------
!
    ! When using DD, timer_start/timerstop calls occur after closing diagnosis
    ! file. Use logical initialized to recognize that situation
    !
    if (.not. gdp%gdtimers%initialized) return
    !
    usedcp => gdp%gdtimers%usedcp
    if ( usedcp(timnum,starttime) < 0.0_hp ) then
       !
       ! Only show a warning when there is no (more serious) error
       !
       if (gdp%errorcode == 0) then
          write (message,'(3a)') 'Timer_stop: Timer ', trim(gdp%gdtimers%names(timnum)), ' has not been started'
          call prterr(gdp%gdinout%lundia, 'U190', trim(message))
       endif
       call system_clock(tcount, trate, tmax)
       call cpu_time(cputim)
       usedcp(timnum,starttime_cpu) = real(cputim,hp)
       usedcp(timnum,starttime)     = real(tcount,hp) / real(trate,hp)
    endif
    call system_clock(tcount, trate, tmax)
    call cpu_time(cputim)
    usedcp(timnum,sumtime_cpu)   = usedcp(timnum,sumtime_cpu) &
                                 & + (real(cputim,hp) - usedcp(timnum,starttime_cpu))
    usedcp(timnum,starttime_cpu) = 0.0_hp
    usedcp(timnum,sumtime)   = usedcp(timnum,sumtime) &
                             & + (real(tcount,hp)/real(trate,hp) - usedcp(timnum,starttime))
    if (timnum==timer_wait) then
       write(*,'(a,f15.3,a)') "Waited: ", real(tcount,hp)/real(trate,hp) - usedcp(timnum,starttime), " sec"
    endif
    usedcp(timnum,starttime) = -1.0_hp
end subroutine timer_stop
!
!
!
!==============================================================================
subroutine timers_finish (gdp)
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
!
! Local variables
!
   integer                                :: i
   integer                                :: istat
   integer                      , pointer :: lundia
   integer                      , pointer :: nmax
   integer                      , pointer :: mmax
   integer                      , pointer :: kmax
   integer                      , pointer :: lmax
   integer                                :: timesteps
   real(hp)                               :: simper
   real(hp)     , dimension(:,:), pointer :: usedcp
   real(hp)     , dimension(4)            :: timetot
   character(20), dimension(:)  , pointer :: names
   character(80)                          :: message
   logical                      , pointer :: addtim
!
!! executable statements -------------------------------------------------------
!
    if (.not. gdp%gdtimers%initialized) then
       write (message,'(a)') 'Timers_finish: Timers not initialized; can not produce timer output.'
       call prterr(gdp%gdinout%lundia, 'U190', trim(message))
       return
    endif
    !
    if (gdp%gdprognm%prognm /= 'TRISIM') then
       !
       ! Timer information is only interesting for TRISIM (calculation part of FLOW)
       ! deallocate and return in all other modes
       !
       if (associated(gdp%gdtimers%usedcp)) deallocate(gdp%gdtimers%usedcp)
       nullify(gdp%gdtimers%usedcp)
       if (associated(gdp%gdtimers%names)) deallocate(gdp%gdtimers%names)
       nullify(gdp%gdtimers%names)
       return
    endif
    nmax    => gdp%d%nmaxus
    mmax    => gdp%d%mmax
    kmax    => gdp%d%kmax
    lmax    => gdp%d%lmax
    lundia  => gdp%gdinout%lundia
    usedcp  => gdp%gdtimers%usedcp
    names   => gdp%gdtimers%names
    timesteps = gdp%gdinttim%itstop - gdp%gdinttim%itstrt
    addtim  => gdp%gdflwpar%flwoutput%addtim
    !
    timetot = 0
    !
    do i = 1, numtimers
       if ( usedcp(i,starttime) > 0.0_hp ) then
          !
          ! Only show a warning when there is no (more serious) error
          !
          if (gdp%errorcode == 0) then
             write (message,'(3a)') 'Timer_finish: Timer ', trim(gdp%gdtimers%names(i)), ' has not been stopped'
             call prterr(lundia, 'U190', trim(message))
          endif
          call timer_stop(i, gdp)
       endif
       !
       ! Location 'startime' in array usedcp is used to store the percentage of the total time
       !
       usedcp(i,starttime)     = (usedcp(i,sumtime)     / usedcp(timer_total,sumtime))     * 100.0_hp
       usedcp(i,starttime_cpu) = (usedcp(i,sumtime_cpu) / usedcp(timer_total,sumtime_cpu)) * 100.0_hp
    enddo
    write(lundia,'(a)') 'Performance timers:'
    write(lundia,'(a)') '|---------------------------------------------------------------------|'
    write(lundia,'(a)') '|Timer name           |       wall clock      |       CPU time        |'
    write(lundia,'(a)') '|                     |-----------------------|-----------------------|'
    write(lundia,'(a)') '|                     |       sec     |  %    |       sec     |  %    |'
    write(lundia,'(a)') '|---------------------------------------------------------------------|'
    do i = 2, 4
       write(lundia,111) names(i), &
            &       usedcp(i,sumtime)    , usedcp(i,starttime)    ,  &
            &       usedcp(i,sumtime_cpu), usedcp(i,starttime_cpu)
    enddo
    write(lundia,'(a)') '|                     |------------- +|----- +|------------- +|----- +|'
    write(lundia,111) names(timer_total), &
         &       usedcp(timer_total,sumtime)    , usedcp(timer_total,starttime)    , &
         &       usedcp(timer_total,sumtime_cpu), usedcp(timer_total,starttime_cpu)
    write(lundia,'(a)') '|---------------------------------------------|-----------------------|'
    do i = 5, 9
       write(lundia,111) names(i), &
            &       usedcp(i,sumtime)    , usedcp(i,starttime)    , &
            &       usedcp(i,sumtime_cpu), usedcp(i,starttime_cpu)
    enddo
    write(lundia,'(a)') '|---------------------------------------------|-----------------------|'
    do i = 10, 11
       write(lundia,111) names(i), &
            &       usedcp(i,sumtime)    , usedcp(i,starttime)    , &
            &       usedcp(i,sumtime_cpu), usedcp(i,starttime_cpu)
    enddo
    if (addtim) then
       !
       ! Additional timings output
       !
       do i = timer_tricomtot, numtimers
          if (i == timer_tricomtot) then 
               write(lundia, '(a)') '|---------------------------------------------------------------------|'
               write(lundia, '(a)') '| *** In tricom ***                                                   |'
                write(lundia, '(a)') '|---------------------------------------------------------------------|'
          elseif (i == timer_trisol_ini) then
               timetot = 0
               write(lundia, '(a)') '|---------------------------------------------------------------------|'
               write(lundia, '(a)') '| *** In trisol ***                                                   |'
               write(lundia, '(a)') '|---------------------------------------------------------------------|'
          elseif (i == timer_checku) then
               write(lundia, 111) 'Total in trisol:    ', timetot(sumtime), timetot(starttime), &
                                                      & timetot(sumtime_cpu), timetot(starttime_cpu)
               timetot = 0
               write(lundia, '(a)') '|---------------------------------------------------------------------|'
               write(lundia, '(a)') '| *** In adi ***                                                      |'
               write(lundia, '(a)') '|---------------------------------------------------------------------|'
          elseif (i == timer_sud_cucnp) then
               write(lundia, 111) 'Total in adi:       ', timetot(sumtime), timetot(starttime), &
                                                      & timetot(sumtime_cpu), timetot(starttime_cpu)
               timetot = 0
               write(lundia, '(a)') '|---------------------------------------------------------------------|'
               write(lundia, '(a)') '| *** In sud ***                                                      |'
               write(lundia, '(a)') '|---------------------------------------------------------------------|'
          elseif (i == timer_uzd_ini) then
               write(lundia, 111) 'Total in sud:       ', timetot(sumtime), timetot(starttime), &
                                                      & timetot(sumtime_cpu), timetot(starttime_cpu)
               timetot = 0
               write(lundia, '(a)') '|---------------------------------------------------------------------|'
               write(lundia, '(a)') '| *** In uzd ***                                                      |'
               write(lundia, '(a)') '|---------------------------------------------------------------------|'
          elseif (i == timer_1stdifu) then
               write(lundia, 111) 'Total in uzd:       ', timetot(sumtime), timetot(starttime), &
                                                      & timetot(sumtime_cpu), timetot(starttime_cpu)
               timetot = 0
               write(lundia, '(a)') '|---------------------------------------------------------------------|'
               write(lundia, '(a)') '| *** In tritra ***                                                   |'
               write(lundia, '(a)') '|---------------------------------------------------------------------|'
          elseif (i == timer_difu_ini) then
               write(lundia, 111) 'Total in tritra:    ', timetot(sumtime), timetot(starttime), &
                                                      & timetot(sumtime_cpu), timetot(starttime_cpu)
               timetot = 0
               write(lundia, '(a)') '|---------------------------------------------------------------------|'
               write(lundia, '(a)') '| *** In difu ***                                                     |'
               write(lundia, '(a)') '|---------------------------------------------------------------------|'
          elseif (i == timer_cucnp_ini) then
               write(lundia, 111) 'Total in difu:      ', timetot(sumtime), timetot(starttime), &
                                                      & timetot(sumtime_cpu), timetot(starttime_cpu)
               timetot = 0
               write(lundia, '(a)') '|---------------------------------------------------------------------|'
               write(lundia, '(a)') '| *** In cucnp ***                                                    |'
               write(lundia, '(a)') '|---------------------------------------------------------------------|'
          elseif (i == timer_dfexchg) then
               write(lundia, 111) 'Total in cucnp:     ', timetot(sumtime), timetot(starttime), &
                                                      & timetot(sumtime_cpu), timetot(starttime_cpu)
               timetot = 0
               write(lundia, '(a)') '|---------------------------------------------------------------------|'
               write(lundia, '(a)') '| *** In parallel routines ***                                        |'
               write(lundia, '(a)') '|---------------------------------------------------------------------|'
          endif
          write(lundia,111) names(i), &
               &       usedcp(i,sumtime)    , usedcp(i,starttime)    , &
               &       usedcp(i,sumtime_cpu), usedcp(i,starttime_cpu)
          timetot(sumtime) = timetot(sumtime) + usedcp(i,sumtime)
          timetot(starttime) = timetot(starttime) + usedcp(i,starttime)
          timetot(sumtime_cpu) = timetot(sumtime_cpu) + usedcp(i,sumtime_cpu)
          timetot(starttime_cpu) = timetot(starttime_cpu) + usedcp(i,starttime_cpu)
       enddo
       write(lundia, 111) 'Total in parallel:  ', timetot(sumtime), timetot(starttime), &
                                                      & timetot(sumtime_cpu), timetot(starttime_cpu)
    endif
    write(lundia,'(a)') '|---------------------------------------------------------------------|'
    if (lmax <= 0) then
       write(lundia,'(a)') '| Performance = CPU time / (TimeSteps*MMAX*NMAX*KMAX)                 |'
    else
       write(lundia,'(a)') '| Performance = CPU time / (TimeSteps*MMAX*NMAX*KMAX*LMAX)            |'
    endif
    write(lundia,'(a,i12,41x,a)') '|   TimeSteps   :', timesteps, '|'
    write(lundia,'(a,i12,41x,a)') '|   MMAX        :', mmax, '|'
    write(lundia,'(a,i12,41x,a)') '|   NMAX        :', nmax, '|'
    write(lundia,'(a,i12,41x,a)') '|   KMAX        :', kmax, '|'
    if (lmax <= 0) then
       lmax = 1
    else
       write(lundia,'(a,i12,41x,a)') '|   LMAX        :', lmax, '|'
    endif
    if (timesteps > 0 .and. nmax > 0 .and. mmax > 0 .and. kmax > 0) then
       simper = usedcp(timer_total,sumtime_cpu) &
              & / (  real(timesteps,hp) * real(nmax,hp) * real(mmax,hp) &
              &    * real(kmax,hp)      * real(lmax,hp)                )
       write(lundia,'(a,e12.5,a,35x,a)') '|   Performance :', simper, ' [sec]', '|'
    endif
    write(lundia,'(a)') '|---------------------------------------------------------------------|'
    write(lundia,'(a)') '|Explanation:                                                         |'
    write(lundia,'(a)') '|                                                                     |'
    write(lundia,'(a)') '|Initialization : Everything up to the first time step                |'
    write(lundia,'(a)') '|Simulation     : All time steps                                      |'
    write(lundia,'(a)') '|Close and stop : Everything after the last time step                 |'
    write(lundia,'(a)') '|Total          : FLOW for this subdomain,                            |'
    write(lundia,'(a)') '|                 excluding allocation/deallocation of global data    |'
    write(lundia,'(a)') '|                                                                     |'
    write(lundia,'(a)') '|Momentum eq.   : Part of Simulation ("UZD")                          |'
    write(lundia,'(a)') '|Continuity eq. : Part of Simulation ("SUD")                          |'
    write(lundia,'(a)') '|Transport eq.  : Part of Simulation ("DIFU")                         |'
    write(lundia,'(a)') '|                 Only relevant when constituents are modelled.       |'
    write(lundia,'(a)') '|Turbulence     : Part of Simulation ("TURCLO, TRATUR, TUR2D")        |'
    write(lundia,'(a)') '|3D Morphology  : Part of Simulation ("EROSED, BOTT3D")               |'
    write(lundia,'(a)') '|                 Only relevant when sediments are modelled.          |'
    write(lundia,'(a)') '|Wait (dd)      : Part of Initialization, Simulation and Closing.     |'
    write(lundia,'(a)') '|                 ("NXTSTP, NXTDRY")                                  |'
    write(lundia,'(a)') '|                 Only relevant when DomainDecomposition is used.     |'
    write(lundia,'(a)') '|                 Communication with and execution of the "mappers".  |'
    write(lundia,'(a)') '|                 Mappers are separate threads, possibly in another   |'
    write(lundia,'(a)') '|                 executable, possibly on another node. They copy     |'
    write(lundia,'(a)') '|                 data from one subdomain to another.                 |'
    write(lundia,'(a)') '|                 FLOWs must wait while the mappers are executing,    |'
    write(lundia,'(a)') '|                 mappers must wait while the FLOWs are executing.    |'
    write(lundia,'(a)') '|Wait (ext)     : Part of Initialization, Simulation and Closing.     |'
    write(lundia,'(a)') '|                 ("SYNC, _TO_, _FROM_")                              |'
    write(lundia,'(a)') '|                 Communication with and execution of all "external"  |'
    write(lundia,'(a)') '|                 modules:                                            |'
    write(lundia,'(a)') '|                 Online WAVES, Online WAQ, Online RTC, Fluid Mud.    |'
    write(lundia,'(a)') '|                                                                     |'
    if (addtim) then
        write(lundia,'(a)') '|Additional timers have been added in the following routines:         |'
        write(lundia,'(a)') '|                 trisol                                              |'
        write(lundia,'(a)') '|                 adi                                                 |'
        write(lundia,'(a)') '|                 sud                                                 |'
        write(lundia,'(a)') '|                 uzd                                                 |'
        write(lundia,'(a)') '|                 tritra                                              |'
        write(lundia,'(a)') '|                 difu                                                |'
        write(lundia,'(a)') '|                 cucnp                                               |'
        write(lundia,'(a)') '|                 parallel routines                                   |'
        write(lundia,'(a)') '|The Delft3D-FLOW source code is needed to interpret these timers     |' 
    endif
    write(lundia,'(a)') '|---------------------------------------------------------------------|'
    write(lundia,*)
    !
    if (associated(gdp%gdtimers%usedcp)) deallocate(gdp%gdtimers%usedcp, stat = istat)
    nullify (gdp%gdtimers%usedcp)
    if (associated(gdp%gdtimers%names))  deallocate(gdp%gdtimers%names, stat = istat)
    nullify (gdp%gdtimers%names)
    !
    ! When using DD, timer_start/timerstop calls occur after closing diagnosis
    ! file. Use logical initialized to recognize that situation
    !
    gdp%gdtimers%initialized = .false.
 111 format('|',a,' | ',2(f12.2,'  |',f5.1,'  | '))
end subroutine timers_finish

end module flow2d3d_timers
