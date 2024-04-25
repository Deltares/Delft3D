subroutine wrpara(filnm1    ,rdwr      ,xpar      ,ipar      ,l         , &
                & error     )
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
! reading of special parameters from nefis
! communication file
! -
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Local parameters
!
    integer, parameter :: nelmx = 8
!
! Global variables
!
    integer              :: error
    integer              :: ipar
    integer, intent(in)  :: l
    logical              :: rdwr
    real                 :: xpar
    character(*)         :: filnm1
!
! Local variables
!
    integer                            :: i
    integer      , dimension(1)        :: ip
    integer      , dimension(6, nelmx) :: elmdms  ! element description array cointaining info about the
    integer      , dimension(nelmx)    :: nbytsg  ! number of bytes of each element type
    real         , dimension(1)        :: xp
    character(10), dimension(nelmx)    :: elmunt
    character(16)                      :: grpnam
    character(16), dimension(nelmx)    :: elmnms
    character(16), dimension(nelmx)    :: elmqty
    character(16), dimension(nelmx)    :: elmtps
    character(64), dimension(nelmx)    :: elmdes
    !
    data grpnam/'PARAMS'/, elmnms/'AG', 'RHOW', 'DT', 'NFLTYP', 'TSCALE',       &
       & 'IT01', 'IT02', 'ITLEN'/,                                              &
        & elmdes/'Acceleration of gravity                         ',             &
        & 'Density of water                                ',                    &
        & 'Timestep FLOW                                ',                    &
        & 'Dry point proc. 0 = NO  1 = MEAN  2 = MAX  3 = MIN         ',         &
        & 'Basic unit of time, expressed in seconds                   ',         &
        & 'Reference date                                  ',                    &
        & 'Reference time                                  ',                    &
        & 'Length of tide cycle ; stand alone and no wave 0          '/,         &
       & elmqty/nelmx*' '/, elmunt/'[  M/S2 ]', '[ KG/M3 ]', '[ TUNIT ]',       &
        & '[   -   ]', '[   S   ]', '[ YYMMDD]', '[ HHMMSS]', '[ TSCALE]'/,      &
       & elmtps/3*'REAL', 'INTEGER', 'REAL', 3*'INTEGER'/, nbytsg/8*4/
!
!! executable statements -------------------------------------------------------
!
    !
    do i = 1, nelmx
       call filldm(elmdms    ,i         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
    enddo
    if (elmtps(l)=='INTEGER') then
       if (rdwr) ip(1) = ipar
       call putgti(filnm1    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
                 & elmqty(1) ,elmunt(1) ,elmdes(1) ,elmtps    ,nbytsg    , &
                 & elmnms(l) ,1         ,rdwr      ,error     ,ip        )
       if (.not.rdwr) ipar = ip(1)
    else
       if (rdwr) xp(1) = xpar
       call putgtr(filnm1    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
                 & elmqty(1) ,elmunt(1) ,elmdes(1) ,elmtps    ,nbytsg    , &
                 & elmnms(l) ,1         ,rdwr      ,error     ,xp        )
       if (.not.rdwr) xpar = xp(1)
    endif
end subroutine wrpara
