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

subroutine read_hyd_step(hyd, itime, iend)

    ! global declarations

    use m_srstop
    use m_hydmod
    implicit none

    ! decalration of arguments

    type(t_hydrodynamics) :: hyd           ! description of the hydrodynamics
    integer :: itime         ! relative time in file
    integer :: iend          ! end of file indicator

    ! local decalrations

    integer :: i             ! loop counter
    integer :: ierr          ! error indicator

    ! for volume check on end of file

    call hyd%file_vol%open()
    read(hyd%file_vol%unit, iostat = iend) itime, (hyd%volume(i), i = 1, hyd%noseg)
    !     write(*,*) 'iend:',iend
    if (iend /= 0) return

    ! for the rest read

    call hyd%file_are%open()
    read(hyd%file_are%unit, iostat = ierr) itime, (hyd%area(i), i = 1, hyd%noq)
    if (ierr /= 0) then
        write(*, *) 'ERROR: reading are file: ', hyd%file_are%unit, trim(hyd%file_are%name)
        call srstop(1)
    endif

    call hyd%file_flo%open()
    read(hyd%file_flo%unit, iostat = ierr) itime, (hyd%flow(i), i = 1, hyd%noq)
    if (ierr /= 0) then
        write(*, *) 'ERROR: reading flo file: ', hyd%file_flo%unit, trim(hyd%file_flo%name)
        call srstop(1)
    endif

    if (hyd%sal_present) then
        call hyd%file_sal%open()
        read(hyd%file_sal%unit, iostat = ierr) itime, (hyd%sal(i), i = 1, hyd%noseg)
        if (ierr /= 0) then
            write(*, *) 'ERROR: reading sal file: ', hyd%file_sal%unit, trim(hyd%file_sal%name)
            call srstop(1)
        endif
    endif

    if (hyd%tem_present) then
        call hyd%file_tem%open()
        read(hyd%file_tem%unit, iostat = ierr) itime, (hyd%tem(i), i = 1, hyd%noseg)
        if (ierr /= 0) then
            write(*, *) 'ERROR: reading tem file: ', hyd%file_tem%unit, trim(hyd%file_tem%name)
            call srstop(1)
        endif
    endif

    if (hyd%tau_present) then
        call hyd%file_tau%open()
        read(hyd%file_tau%unit, iostat = ierr) itime, (hyd%tau(i), i = 1, hyd%noseg)
        if (ierr /= 0) then
            write(*, *) 'ERROR: reading tau file: ', hyd%file_tau%unit, trim(hyd%file_tau%name)
            call srstop(1)
        endif
    endif

    if (hyd%vdf_present) then
        call hyd%file_vdf%open()
        read(hyd%file_vdf%unit, iostat = ierr) itime, (hyd%vdf(i), i = 1, hyd%noseg)
        if (ierr /= 0) then
            write(*, *) 'ERROR: reading vdf file: ', hyd%file_vdf%unit, trim(hyd%file_vdf%name)
            call srstop(1)
        endif
    endif

    return
end subroutine read_hyd_step
