subroutine initbedformpar(gdp)
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
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp 
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    character(256), pointer                   :: flbdfh
    !
    logical, pointer                          :: lfbedfrm
    logical, pointer                          :: lfbedfrmout
    logical, pointer                          :: lfbedfrmrou
    logical, pointer                          :: lfbedfrmCFL
    logical, pointer                          :: lfbedfrmADV
    logical, pointer                          :: lfbdfmor
    !
    integer, pointer                          :: bedformheighttype
    integer, pointer                          :: bedformlengthtype
    integer, pointer                          :: bdfrpt
    integer, pointer                          :: bdfrlxtype
    !
    real(fp), pointer                         :: bdfC_Hn
    real(fp), pointer                         :: bdfC_Hp
    real(fp), pointer                         :: bdfGmin
    real(fp), pointer                         :: bdfHmax
    real(fp), pointer                         :: bdfL_Hc
    real(fp), pointer                         :: bdfL_Hp
    real(fp), pointer                         :: bdfPmax
    real(fp), pointer                         :: bedformL_H
    real(fp), pointer                         :: bedformT_H
    real(fp), pointer                         :: bdfuni
    real(fp), pointer                         :: thetacdune
    !
    integer  :: istat
!
!! executable statements -------------------------------------------------------
!
    
    flbdfh                  => gdp%gdbedformpar%flbdfh
    !
    lfbedfrm                => gdp%gdbedformpar%lfbedfrm
    lfbedfrmout             => gdp%gdbedformpar%lfbedfrmout
    lfbedfrmrou             => gdp%gdbedformpar%lfbedfrmrou
    lfbedfrmCFL             => gdp%gdbedformpar%lfbedfrmCFL
    lfbedfrmADV             => gdp%gdbedformpar%lfbedfrmADV
    lfbdfmor                => gdp%gdbedformpar%lfbdfmor
    !
    bedformheighttype       => gdp%gdbedformpar%bedformheighttype
    bedformlengthtype       => gdp%gdbedformpar%bedformlengthtype
    bdfrpt                  => gdp%gdbedformpar%bdfrpt
    bdfrlxtype              => gdp%gdbedformpar%bdfrlxtype
    !
    bdfC_Hn                 => gdp%gdbedformpar%bdfC_Hn
    bdfC_Hp                 => gdp%gdbedformpar%bdfC_Hp
    bdfGmin                 => gdp%gdbedformpar%bdfGmin
    bdfHmax                 => gdp%gdbedformpar%bdfHmax
    bdfL_Hc                 => gdp%gdbedformpar%bdfL_Hc
    bdfL_Hp                 => gdp%gdbedformpar%bdfL_Hp
    bdfPmax                 => gdp%gdbedformpar%bdfPmax
    bedformL_H              => gdp%gdbedformpar%bedformL_H
    bedformT_H              => gdp%gdbedformpar%bedformT_H 
    bdfuni                  => gdp%gdbedformpar%bdfuni
    thetacdune              => gdp%gdbedformpar%thetacdune
    !
    ! Initialize all variables
    !
    nullify(gdp%gdbedformpar%duneheight)
    nullify(gdp%gdbedformpar%duneheightequi)
    nullify(gdp%gdbedformpar%dunelength)
    nullify(gdp%gdbedformpar%qbedformx)
    nullify(gdp%gdbedformpar%qbedformy)
    nullify(gdp%gdbedformpar%ubedform)
    nullify(gdp%gdbedformpar%hdpar)
    nullify(gdp%gdbedformpar%ldpar)
    nullify(gdp%gdbedformpar%kdpar)
    nullify(gdp%gdbedformpar%cdpar)
    nullify(gdp%gdbedformpar%rksr)
    nullify(gdp%gdbedformpar%rksmr)
    nullify(gdp%gdbedformpar%rksd)
    nullify(gdp%gdbedformpar%bedformD50)
    nullify(gdp%gdbedformpar%bedformD90)
    !
    flbdfh      = ' '
    !
    lfbedfrm    = .false.
    lfbedfrmout = .false.
    lfbedfrmrou = .false.
    lfbdfmor    = .false.
    lfbedfrmADV = .true.    
    lfbedfrmCFL = .false.    
    !
    bedformheighttype = 0
    bedformlengthtype = 0
    bdfrpt     = 0
    bdfrlxtype = 0
    !
    bdfC_Hn    =    5.5_fp
    bdfC_Hp    =    3.5_fp
    bdfGmin    =    0.2_fp
    bdfHmax    =    5.0_fp
    bdfL_Hc    =    1.0_fp
    bdfL_Hp    =    2.5_fp
    bdfPmax    =    3.0_fp
    bedformL_H =    0.0_fp    
    bedformT_H =    1.0_fp
    bdfuni     =    0.0_fp
    thetacdune = -999.0_fp
    !
end subroutine initbedformpar
