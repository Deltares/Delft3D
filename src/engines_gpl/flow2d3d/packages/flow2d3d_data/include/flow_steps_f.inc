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
!  $Id$
!  $HeadURL$$
!-------------------------------------------------------------------------------
    integer, external :: nxtstp
!
    integer, external :: nxtdry
!
!
! Define all steps as integer parameters
!
    integer, parameter :: d3dflow_init = 0
    integer, parameter :: d3dflowmap_init = 1
!
    integer, parameter :: d3dflow_inittimestep = 2
    integer, parameter :: d3dflowmap_inittimestep = 3
!
    integer, parameter :: d3dflow_build_u = 4
    integer, parameter :: d3dflowmap_build_u = 5
    integer, parameter :: d3dflow_solve_u = 6
    integer, parameter :: d3dflowmap_check_u = 7
!
    integer, parameter :: d3dflow_build_v = 8
    integer, parameter :: d3dflowmap_build_v = 9
    integer, parameter :: d3dflow_solve_v = 10
    integer, parameter :: d3dflowmap_check_v = 11
!
    integer, parameter :: d3dflow_finishstage1 = 14
    integer, parameter :: d3dflowmap_finishstage1 = 15
!
    integer, parameter :: d3dflow_expl_turb = 40
    integer, parameter :: d3dflowmap_expl_turb = 41
!
    integer, parameter :: d3dflow_build_conc = 26
    integer, parameter :: d3dflowmap_build_conc = 27
    integer, parameter :: d3dflow_solve_conc = 30
    integer, parameter :: d3dflowmap_check_conc = 31
!
    integer, parameter :: d3dflow_finishtimestep = 96
    integer, parameter :: d3dflowmap_finishtimestep = 97
!
    integer, parameter :: d3dflow_finish = 98
    integer, parameter :: d3dflowmap_finish = 99
!
    integer, parameter :: d3dflow_build_adi_zeta = 60
    integer, parameter :: d3dflowmap_build_adi_zeta = 61
!
    integer, parameter :: d3dflow_solve_adi_zeta = 64
    integer, parameter :: d3dflowmap_check_adi_zeta = 65
!
    integer, parameter :: d3dflow_build_adi_conc = 68
    integer, parameter :: d3dflowmap_build_adi_conc = 69
!
    integer, parameter :: d3dflow_solve_adi_conc = 72
    integer, parameter :: d3dflowmap_check_adi_conc = 73
!
    integer, parameter :: d3dflow_check_sud_dry = 76
    integer, parameter :: d3dflowmap_check_sud_dry = 77
!
    integer, parameter :: d3dflow_check_adi_dry = 85
    integer, parameter :: d3dflowmap_check_adi_dry = 86
!
    integer, parameter :: d3dflow_solve_wang = 78
    integer, parameter :: d3dflow_finish_wang = 80
    integer, parameter :: d3dflowmap_finish_wang = 79
!
    integer, parameter :: d3dflow_sediment = 81
    integer, parameter :: d3dflowmap_sediment = 82
!
    integer, parameter :: d3dflow_bottom3d = 83
    integer, parameter :: d3dflowmap_bottom3d = 84
!
    integer, parameter :: d3dflow_build_2dad = 87
    integer, parameter :: d3dflowmap_build_2dad = 88
!
    integer, parameter :: d3dflow_solve_2dad = 89
    integer, parameter :: d3dflowmap_check_2dad = 90
!
    integer, parameter :: d3dflow_roller_uv = 91
    integer, parameter :: d3dflowmap_roller_uv = 92
!
    integer, parameter :: d3dflow_nr_steps = 100
!
    integer, parameter :: noneighbors = 777     ! repeated in flow_nextstep.f90
