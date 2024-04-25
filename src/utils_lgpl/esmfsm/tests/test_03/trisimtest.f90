!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
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
subroutine trisimtest (context_id, fsm_flags)
    use precision     ! pntrsize, used in fsm.i
    implicit none
    !
    include 'fsm.i'
!
! Parameters
!
    integer       , intent(in)  :: context_id
    integer       , intent(in)  :: fsm_flags
!
! Local variables
!
    integer                :: fsmstatus
    integer(kind=pntrsize) :: mkpnt

    fsmstatus = fsmini (context_id, fsm_flags)

    mkpnt = makptr ("bufc", chtyp, 10)
    if (mkpnt==0) then
           stop "Cannot allocate memory"
    endif
    ! Initialize requested memory space
    call chnull (chbuf(mkpnt),10)
!    mkpnt = relptr ("bufc")
    if (mkpnt==0) then
           stop "Cannot free memory"
    endif

    mkpnt = makptr ("bufr", rtyp, 10)
    if (mkpnt==0) then
           stop "Cannot allocate memory"
    endif
    ! Initialize requested memory space
    call rnull (rbuf(mkpnt),10    )
!    mkpnt = relptr ("bufr")
    if (mkpnt==0) then
           stop "Cannot free memory"
    endif

    mkpnt = makptr ("bufi", ityp, 10)
    if (mkpnt==0) then
           stop "Cannot allocate memory"
    endif
    ! Initialize requested memory space
    call inull (ibuf(mkpnt),10)
!    mkpnt = relptr ("bufi")
    if (mkpnt==0) then
           stop "Cannot free memory"
    endif

end subroutine trisimtest
