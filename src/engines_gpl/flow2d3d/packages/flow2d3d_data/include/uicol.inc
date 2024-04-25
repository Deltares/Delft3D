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
    integer :: stdfor
    integer :: stdbck
    integer :: mnufor
    integer :: mnubck
    integer :: inpfor
    integer :: inpbck
    integer :: errfor
    integer :: errbck
    integer :: lblfor
    integer :: lblbck
    integer :: linfor
    integer :: linbck
    integer :: topfor
    integer :: topbck
    integer :: hlpfor
    integer :: hlpbck
    integer :: botfor
    integer :: botbck
    integer :: keyfor
    integer :: keybck
    integer :: wndfor
    integer :: wndbck
    integer :: shafor
    integer :: shabck
    !
    ! menu indication
    !
    integer, dimension(4) :: npos
    !
    common /forbck/ stdfor, stdbck, mnufor, mnubck, inpfor, inpbck, errfor,     &
                  & errbck, lblfor, lblbck, linfor, linbck, topfor, topbck,     &
                  & hlpfor, hlpbck, botfor, botbck, keyfor, keybck, wndfor,     &
                  & wndbck, shafor, shabck, npos
