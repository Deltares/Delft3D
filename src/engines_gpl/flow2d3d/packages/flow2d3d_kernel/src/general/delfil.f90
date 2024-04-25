subroutine delfil(runid     ,filmd     ,gdp       )
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
!    Function: Deletes all the temporary (unformatted) files
!              which where created by the simulation program.
!              These files are respectively :
!                   TMP_//runid//.bch
!                   TMP_//runid//.bct
!                   TMP_//runid//.grd
!                   TMP_//runid//.bcc
!                   TMP_//runid//.td
!                   TMP_//runid//.dis
!                   TMP_//runid//.dry
!                   TMP_//runid//.tem
!                   TMP_//runid//.eva
!                   TMP_//runid//.wnd
!                   TMP_refinement
!                   com-//runid//.srctmp (old name)
!                   TMP_com-//runid//.src (new name)
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use string_module
    use dfparall 
    use properties
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer , pointer :: itis
    integer , pointer :: lundia
    integer , pointer :: lunscr
    logical , pointer :: reusetmp !  TRUE when temporary files will be reused if possible 
!
! Global variables
!
    character(*)   , intent(in) :: filmd    !! File name for MD FLOW file 
    character(*)   , intent(in) :: runid    !!  Run identification code for the current simulation
!
! Local variables
!
    integer                        :: lrid    ! Length of character string runid 
    integer                        :: lunout  ! Unit number for file to write out- put to (:=LUNDIA or LUNSCR) 
    logical                        :: opend   ! Flag to check if file is open 
    character(12)                  :: fildef
    character(256)                 :: filnam  ! String contaning complete file name "TMP_RUNID.extension" 
    character(256)                 :: filrd   ! File name read from Md-file/flow file 
!
!! executable statements -------------------------------------------------------
!
    lundia      => gdp%gdinout%lundia
    lunscr      => gdp%gdinout%lunscr
    itis        => gdp%gdrdpara%itis
    reusetmp    => gdp%gdtmpfil%reusetmp
    !
    ! Unit number to write output to
    !
    lunout = lunscr
    inquire (lundia, opened = opend)
    if (opend) lunout = lundia
    !
    ! Define length of runid
    !
    call remove_leading_spaces(runid     ,lrid      )
    !
    if (.not. reusetmp) then
        !
        ! Filcco
        !
        filrd = fildef
        call prop_get(gdp%mdfile_ptr,'*','Filcco',filrd)
        filnam = 'TMP_' // runid(:lrid) // '.grd'
        if (filnam/=filrd .and. inode == master) call rmdel(filnam    ,gdp       )
        ! 
        ! append node number to file name in case of parallel computing within single-domain case 
        ! 
        if ( parll ) then
           write(filnam(8+lrid+1:8+lrid+4),'(a,i3.3)') '-',inode 
        endif
        if (filnam/=filrd) call rmdel(filnam    ,gdp       )
        !
        ! Fildry
        !
        filrd = fildef
        call prop_get(gdp%mdfile_ptr,'*','Fildry',filrd)
        filnam = 'TMP_' // runid(:lrid) // '.dry'
        if ( parll ) then
           write(filnam(8+lrid+1:8+lrid+4),'(a,i3.3)') '-',inode 
        endif
        if (filnam/=filrd) call rmdel(filnam    ,gdp       )
        !
        ! Filtd
        !
        filrd = fildef
        call prop_get(gdp%mdfile_ptr,'*','Filtd',filrd)
        filnam = 'TMP_' // runid(:lrid) // '.td'
        if ( parll ) then
           write(filnam(7+lrid+1:7+lrid+4),'(a,i3.3)') '-',inode 
        endif
        if (filnam/=filrd) call rmdel(filnam    ,gdp       )
        !
        ! FilbcH
        !
        filrd = fildef
        call prop_get(gdp%mdfile_ptr,'*','FilbcH',filrd)
        filnam = 'TMP_' // runid(:lrid) // '.bch'
        if (filnam/=filrd .and. inode==master) call rmdel(filnam    ,gdp       )
        !
        ! FilbcT
        !
        filrd = fildef
        call prop_get(gdp%mdfile_ptr,'*','FilbcT',filrd)
        filnam = 'TMP_' // runid(:lrid) // '.bct'
        if (filnam/=filrd .and. filrd/=' ') call rmdel(filnam    ,gdp       )
        !
        ! FilbcQ
        !
        filrd = fildef
        call prop_get(gdp%mdfile_ptr,'*','FilbcQ',filrd)
        filnam = 'TMP_' // runid(:lrid) // '.bcq'
        if (filnam/=filrd .and. filrd/=' ') call rmdel(filnam    ,gdp       )
        !
        ! FilbcC
        !
        filrd = fildef
        call prop_get(gdp%mdfile_ptr,'*','FilbcC',filrd)
        filnam = 'TMP_' // runid(:lrid) // '.bcc'
        if (filnam/=filrd .and. filrd/=' ') call rmdel(filnam    ,gdp       )
        !
        ! Fildis
        !
        filrd = fildef
        call prop_get(gdp%mdfile_ptr,'*','Fildis',filrd)
        filnam = 'TMP_' // runid(:lrid) // '.dis'
        if (filnam/=filrd .and. filrd/=' ') call rmdel(filnam    ,gdp       )
        !
        ! Filtmp
        !
        filrd = fildef
        call prop_get(gdp%mdfile_ptr,'*','Filtmp',filrd)
        filnam = 'TMP_' // runid(:lrid) // '.tem'
        if (filnam/=filrd .and. filrd/=' ') call rmdel(filnam    ,gdp       )
        !
        ! Fileva
        !
        filrd = fildef
        call prop_get(gdp%mdfile_ptr,'*','Fileva',filrd)
        filnam = 'TMP_' // runid(:lrid) // '.eva'
        if (filnam/=filrd .and. filrd/=' ') call rmdel(filnam    ,gdp       )
        !
        ! Filwnd
        !
        filrd = fildef
        call prop_get(gdp%mdfile_ptr,'*','Filwnd',filrd)
        filnam = 'TMP_' // runid(:lrid) // '.wnd'
        if (filnam/=filrd .and. inode==master) call rmdel(filnam    ,gdp       )
    endif
    !
    ! Temporary file created (Tricom.f90) when leaving online visualisation 
    ! during simulation
    ! close the file and delete it using routine RMDEL
    !
    filnam = 'TMP_VisuOL_closed'
    if (inode == master) call rmdel(filnam, gdp)
    !
    ! Temporary file created (mapper_config.cpp) by mappers to check refinement
    !
    filnam = 'TMP_refinement'
    if (inode == master) call rmdel(filnam, gdp)
    !
    ! Temporary file possibly created when writing WAQ input files
    !
    filnam = 'TMP_com-' // runid(:lrid) // '.src'
    if (inode == master) call rmdel(filnam, gdp)
    ! old name
    filnam = 'com-' // runid(:lrid) // '.srctmp'
    if (inode == master) call rmdel(filnam, gdp)
end subroutine delfil
