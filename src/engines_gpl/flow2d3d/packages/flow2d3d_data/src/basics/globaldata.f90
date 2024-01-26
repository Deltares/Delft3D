module globaldata
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
! This module contains the definition of the
! Global data structure. This module is necessary
! whenever the GDP pointer enters a subroutine via
! the argument list.
! usage: include globdat.igd
! NOTE: The contents of this module was formerly the
! contents of globdat.igd itself (without the use
! of a module). The module is necessary since GDP
! is used in the Fluidmud module.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use tree_structures
    use dio_plt_rw, only:dioplttype
    use handles
    use message_module
    !use ec_typedefs
    use bedcomposition_module, only:bedcomp_data
    use dredge_data_module, only: dredge_type
    use morphology_data_module, only:morpar_type, sedpar_type, moroutputtype, &
                              & mornumericstype, bedbndtype, cmpbndtype, &
                              & trapar_type, sedtra_type
    use grid_dimens_module, only: griddimtype
    !
    ! dio_plt_rw is only used for coup.igs.
    ! Both the DelftIO modules and the tree_structure modules contain
    ! the entity prop_get_string, causing a compile error
    ! This is solved by the "only:dioplttype" addition
    !
    implicit none
    !
    include 'address.igs'
    include 'adv2d.igs'
    include 'bcdat.igs'
    include 'bedformpar.igs'
    include 'betaro.igs'
    include 'bubble.igs'
    include 'const.igs'
    include 'constd.igs'
    include 'coup.igs'
    include 'datusr.igs'
    include 'discharge.igs'
    include 'dimens.igs'
    include 'veg3d.igs'
    include 'exttim.igs'
    include 'flwpar.igs'
    include 'fmtbcc.igs'
    include 'fmtbct.igs'
    include 'fmtdis.igs'
    include 'fourier.igs'
    include 'heat.igs'
    include 'htur2d.igs'
    include 'inout.igs'
    include 'inttim.igs'
    include 'iwearr.igs'
    include 'iwepar.igs'
    include 'keywtd.igs'
    include 'luntmp.igs'
    include 'massbal.igs'
    include 'mudcoe.igs'
    include 'nfl.igs'
    include 'numeco.igs'
    include 'physco.igs'
    include 'pointrs.igs'
    include 'procs.igs'
    include 'prognm.igs'
    include 'r-i-ch.igs'
    include 'rdpara.igs'
    include 'rivpro.igs'
    include 'scour.igs'
    include 'sdu.igs' 
    include 'sobek.igs'
    include 'stations.igs'
    include 'tfzeta.igs'
    include 'tmpfil.igs'
    include 'trachy.igs'
    include 'turcoe.igs'
    include 'usrpar.igs'
    include 'zmodel.igs'
    include 'nonhyd.igs'
    !
    include 'cline.igs'
    include 'culver.igs'
    include 'f0isf1.igs'
    include 'incbc.igs'
    include 'incbcc.igs'
    include 'incwav.igs'
    include 'inibcc.igs'
    include 'inibct.igs'
    include 'inidis.igs'
    include 'postpr.igs'
    include 'restart.igs'
    include 'rtc.igs'
    include 'snel.igs'
    include 'timers.igs'
    include 'tricom.igs'
    include 'trisol.igs'
    include 'u_ppr.igs'
    include 'updbcc.igs'
    include 'updbct.igs'
    include 'upddis.igs'
    include 'waqpar.igs'
    include 'wrirst.igs'
    include 'wrline.igs'
    include 'z_initcg.igs'
    include 'iofiles.igs'
    include 'dddata.igs'
    include 'dfparall.igs'
    !
    !
    ! Derived Type definitions
    !
    type globdat
       type (gd_address)  , pointer :: gdaddress
       type (gd_adv2d)     , pointer :: gdadv2d
       type (gd_bcdat)    , pointer :: gdbcdat
       type (gd_bedformpar), pointer :: gdbedformpar
       type (gd_betaro)   , pointer :: gdbetaro
       type (gd_bubble)   , pointer :: gdbubble
       type (gd_const)    , pointer :: gdconst
       type (gd_constd)   , pointer :: gdconstd
       type (gd_coup)     , pointer :: gdcoup
       type (gd_datusr)   , pointer :: gddatusr
       type (gd_discharge), pointer :: gddischarge
       type (gd_dimens)   , pointer :: d
       type (griddimtype) , pointer :: griddim
       type (gd_veg3d)    , pointer :: gdveg3d
       type (gd_exttim)   , pointer :: gdexttim
       type (gd_flwpar)   , pointer :: gdflwpar
       type (gd_fmtbcc)   , pointer :: gdfmtbcc
       type (gd_fmtbct)   , pointer :: gdfmtbct
       type (gd_fmtdis)   , pointer :: gdfmtdis
       type (gd_fourier)  , pointer :: gdfourier
       type (gd_heat)     , pointer :: gdheat
       type (gd_htur2d)   , pointer :: gdhtur2d
       type (gd_inout)    , pointer :: gdinout
       type (gd_inttim)   , pointer :: gdinttim
       type (gd_iwearr)   , pointer :: gdiwearr
       type (gd_iwepar)   , pointer :: gdiwepar
       type (gd_keywtd)   , pointer :: gdkeywtd
       type (gd_luntmp)   , pointer :: gdluntmp
       type (gd_massbal)  , pointer :: gdmassbal
       type (morpar_type) , pointer :: gdmorpar
       type (gd_mudcoe)   , pointer :: gdmudcoe
       type (gd_nfl)      , pointer :: gdnfl
       type (gd_numeco)   , pointer :: gdnumeco
       type (gd_physco)   , pointer :: gdphysco
       type (gd_pointrs)  , pointer :: gdpointrs
       type (gd_procs)    , pointer :: gdprocs
       type (gd_prognm)   , pointer :: gdprognm
       type (gd_r_i_ch)   , pointer :: gdr_i_ch
       type (gd_rdpara)   , pointer :: gdrdpara
       type (gd_rivpro)   , pointer :: gdrivpro
       type (gd_scour)    , pointer :: gdscour
       type (sedpar_type) , pointer :: gdsedpar
       type (gd_sdu)      , pointer :: gdsdu
       type (gd_sobek)    , pointer :: gdsobek
       type (gd_stations) , pointer :: gdstations
       type (gd_tfzeta)   , pointer :: gdtfzeta
       type (gd_tmpfil)   , pointer :: gdtmpfil
       type (gd_trachy)   , pointer :: gdtrachy
       type (gd_turcoe)   , pointer :: gdturcoe
       type (message_stack), pointer :: messages
       type (bedcomp_data), pointer :: gdmorlyr
       type (gd_usrpar)   , pointer :: gdusrpar
       type (gd_zmodel)   , pointer :: gdzmodel
       type (gd_nonhyd)   , pointer :: gdnonhyd
       type (gd_waqpar)   , pointer :: gdwaqpar
       !
       type (sv_cline)    , pointer :: gdcline
       type (sv_culver)   , pointer :: gdculver
       type (dredge_type) , pointer :: gddredge
       type (trapar_type) , pointer :: gdtrapar
       type (sedtra_type) , pointer :: gderosed
       type (sv_f0isf1)   , pointer :: gdf0isf1
       type (sv_incbc)    , pointer :: gdincbc
       type (sv_incbcc)   , pointer :: gdincbcc
       type (sv_incwav)   , pointer :: gdincwav
       type (sv_inibcc)   , pointer :: gdinibcc
       type (sv_inibct)   , pointer :: gdinibct
       type (sv_inidis)   , pointer :: gdinidis
       type (sv_postpr)   , pointer :: gdpostpr
       type (sv_restart)  , pointer :: gdrestart
       type (sv_rtc)      , pointer :: gdrtc
       type (sv_snel)     , pointer :: gdsnel
       type (timerstype)  , pointer :: gdtimers
       type (sv_tricom)   , pointer :: gdtricom
       type (sv_trisol)   , pointer :: gdtrisol
       type (sv_u_ppr)    , pointer :: gdu_ppr
       type (sv_updbcc)   , pointer :: gdupdbcc
       type (sv_updbct)   , pointer :: gdupdbct
       type (sv_upddis)   , pointer :: gdupddis
       type (sv_wrirst)   , pointer :: gdwrirst
       type (sv_wrline)   , pointer :: gdwrline
       type (sv_z_initcg) , pointer :: gdz_initcg
       type (iofile), dimension(:), pointer :: iofiles
       !
       ! DD data fields
       !
       type (dddata)      , pointer :: dd
       !
       ! runid (copy); needed to debug DD models
       !
       character(256)     , pointer :: runid
       character(6)       , pointer :: uniqueid ! 6 capitals, randomly set in initialization phase
       !
       ! data fields for parallel Delft3D-FLOW
       !
       type(dfparalltype) , pointer :: gdparall
       type(dfparalltype) , pointer :: iopartit
       !
       ! ec-module
       !
       !type(tECHandle)              :: gd_ECHandle
       integer                      :: gridECItemId  = -1
       integer                      :: patmECItemId  = -1
       integer                      :: uwindECItemId = -1
       integer                      :: vwindECItemId = -1
       !
       ! arch is currently 'win32', 'win64' or 'linux'
       !
       character(10)      , pointer :: arch
       !
       ! errorcode = 0 : No errors
       !             1 : Error in TRIPOI
       !             2 : Error in TRICOM
       !
       integer            , pointer :: errorcode
       !
       ! pointer to tree structure containing "all" input
       !
       type(tree_data)    , pointer :: input_tree
       !
       ! mdfile_ptr is a pointer to a sub-tree of input_tree,
       ! containing the full contents of the mdf-file.
       ! On deallocation, only input_tree has to be deallocated.
       !
       type(tree_data)    , pointer :: mdfile_ptr
    end type globdat
end module globaldata
