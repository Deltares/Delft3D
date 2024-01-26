module rdsec_module
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

contains

function rdsec(iuni      ,parread   ,convar    ,nsect     ,distance  , &
             & waveheight,period    ,direction ,dirspread , &
             & cbuf      ,turn      ,rccnt     )
    implicit none
!
! Global variables
!
    integer                    , intent(in)  :: convar
    integer                                  :: iuni
    integer                    , intent(out) :: nsect
    integer                    , intent(in)  :: parread
    integer                                  :: rccnt
    integer                                  :: rdsec
    integer                    , intent(out) :: turn
    real         , dimension(:), intent(out) :: distance
    real         , dimension(:), intent(out) :: waveheight
    real         , dimension(:), intent(out) :: period
    real         , dimension(:), intent(out) :: direction
    real         , dimension(:), intent(out) :: dirspread
    character(37), dimension(:), intent(out) :: cbuf
!
! Local variables
!
    integer           :: cindx
    integer           :: retval
    integer           :: sec
    integer, external :: skcomc
    real              :: dd
    real              :: dist
    real              :: hs
    real              :: pdir
    real              :: per
!
!! executable statements -------------------------------------------------------
!
    cindx = 1
    if (convar==1) then
       !        constant segment
       nsect = 1
       if (parread==2) then
          rccnt = rccnt + skcomc(iuni)
          !           hs, per, pdir, dd
          rccnt = rccnt + 1
          read (iuni, *, err = 1002, end = 1009) hs, per, pdir, dd
          distance  (1) = 0.0
          waveheight(1) = hs
          period    (1) = per
          direction (1) = pdir
          dirspread (1) = dd
          cbuf(cindx) = ' '
       else
          rccnt = rccnt + skcomc(iuni)
          !           filename
          rccnt = rccnt + 1
          read (iuni, *, err = 1002, end = 1009) cbuf(cindx)
          distance  (1) = 0.0
          waveheight(1) = 0.0
          period    (1) = 0.0
          direction (1) = 0.0
          dirspread (1) = 0.0
       endif
    else
       !        variable segment
       rccnt = rccnt + skcomc(iuni)
       rccnt = rccnt + 1
       read (iuni, *, err = 1002, end = 1009) nsect, turn
       do sec = 1, nsect
          if (parread==2) then
             rccnt = rccnt + skcomc(iuni)
             !              dist, hs, per, pdir, dd
             rccnt = rccnt + 1
             read (iuni, *, err = 1002, end = 1009) dist, hs, per, pdir, dd
             distance  (sec) = dist
             waveheight(sec) = hs
             period    (sec) = per
             direction (sec) = pdir
             dirspread (sec) = dd
             cbuf(cindx)     = ' '
          else
             rccnt = rccnt + skcomc(iuni)
             !              filename
             rccnt = rccnt + 1
             read (iuni, *, err = 1002, end = 1009) dist, cbuf(cindx)
             distance  (sec) = dist
             waveheight(sec) = 0.0
             period    (sec) = 0.0
             direction (sec) = 0.0
             dirspread (sec) = 0.0
          endif
          cindx = cindx + 1
       enddo
    endif
    retval = 0
    rdsec  = retval
    goto 99999
    !        read
 1002 continue
    retval = 4
    rdsec = retval
    goto 99999
    !        eof
 1009 continue
    retval = 6
    rdsec = retval
99999 continue
end function rdsec

end module rdsec_module
