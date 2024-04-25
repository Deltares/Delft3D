subroutine initflwpar(gdp)
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
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! local parameters
    !
    integer  :: istat
!
!! executable statements -------------------------------------------------------
!
    istat = 0
    allocate (gdp%gdflwpar%flwoutput, stat = istat)
    allocate (gdp%gdflwpar%fbcrfile , stat = istat) 
    !
    gdp%gdflwpar%flwoutput%cntcflmsg   = 0
    gdp%gdflwpar%flwoutput%maxcflmsg   = 100
    !
    gdp%gdflwpar%flwoutput%iteroutputsteps = 0
    gdp%gdflwpar%flwoutput%air             = .false.
    gdp%gdflwpar%flwoutput%addtim          = .false.
    gdp%gdflwpar%flwoutput%cflmsg          = .false.
    gdp%gdflwpar%flwoutput%chezy           = .false.
    gdp%gdflwpar%flwoutput%cumdifuflux     = .false.
    gdp%gdflwpar%flwoutput%difuflux        = .false.
    gdp%gdflwpar%flwoutput%dredge_map      = .false.
    gdp%gdflwpar%flwoutput%halfdt          = .false.
    gdp%gdflwpar%flwoutput%hisbar          = .false.
    gdp%gdflwpar%flwoutput%hisdis          = .false.
    gdp%gdflwpar%flwoutput%kf_minmax       = .false.
    gdp%gdflwpar%flwoutput%layering        = .false.
    gdp%gdflwpar%flwoutput%momentum        = .false.
    gdp%gdflwpar%flwoutput%roughness       = .false.
    gdp%gdflwpar%flwoutput%temperature     = .false.
    gdp%gdflwpar%flwoutput%vortic          = .false.
    gdp%gdflwpar%flwoutput%veuler          = .true.
    gdp%gdflwpar%flwoutput%waveqnt         = .false.
    gdp%gdflwpar%flwoutput%z0cur           = .false.
    gdp%gdflwpar%flwoutput%z0rou           = .false.
    !
    nullify(gdp%gdflwpar%fluxu)
    nullify(gdp%gdflwpar%fluxuc)
    nullify(gdp%gdflwpar%fluxv)
    nullify(gdp%gdflwpar%fluxvc)
    nullify(gdp%gdflwpar%fluxw)
    nullify(gdp%gdflwpar%fluxwc)
    !
    nullify(gdp%gdflwpar%mom_m_velchange)
    nullify(gdp%gdflwpar%mom_m_densforce)
    nullify(gdp%gdflwpar%mom_m_flowresist)
    nullify(gdp%gdflwpar%mom_m_corioforce)
    nullify(gdp%gdflwpar%mom_m_visco)
    nullify(gdp%gdflwpar%mom_m_pressure)
    nullify(gdp%gdflwpar%mom_m_tidegforce)
    nullify(gdp%gdflwpar%mom_m_windforce)
    nullify(gdp%gdflwpar%mom_m_bedforce)
    nullify(gdp%gdflwpar%mom_m_waveforce)
    nullify(gdp%gdflwpar%mom_m_convec)
    nullify(gdp%gdflwpar%mom_m_xadvec)
    !
    nullify(gdp%gdflwpar%mom_n_velchange)
    nullify(gdp%gdflwpar%mom_n_densforce)
    nullify(gdp%gdflwpar%mom_n_flowresist)
    nullify(gdp%gdflwpar%mom_n_corioforce)
    nullify(gdp%gdflwpar%mom_n_visco)
    nullify(gdp%gdflwpar%mom_n_pressure)
    nullify(gdp%gdflwpar%mom_n_tidegforce)
    nullify(gdp%gdflwpar%mom_n_windforce)
    nullify(gdp%gdflwpar%mom_n_bedforce)
    nullify(gdp%gdflwpar%mom_n_waveforce)
    nullify(gdp%gdflwpar%mom_n_convec)
    nullify(gdp%gdflwpar%mom_n_xadvec)
    !
    nullify(gdp%gdflwpar%fcrbnd) 
    !
    gdp%gdflwpar%fbccorrection = .false.
    !
    gdp%gdflwpar%fbcrfilnam    = ' '
end subroutine initflwpar
