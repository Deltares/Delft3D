!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2022.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! $Id$
! $HeadURL$

module m_f1dimp
   use m_f1dimp_data
   public f1dimppar
   !
   ! flow 1d implicit
   !
   logical                           :: f1dimp_initialized=.false.   
   type(f1dimppar_type), target      :: f1dimppar         !< flow 1d implicit parameters
   !
   
    contains
!
!BEGIN reallocate_fill
!
    
subroutine reallocate_fill(val,idx_mask,idxi,idxf)

use m_alloc

implicit none

double precision, dimension(:), allocatable, intent(inout) :: val
integer, dimension(idxf), intent(in) :: idx_mask
integer, intent(in) :: idxi, idxf

integer :: k


call realloc(val,idxf)

do k=idxi+1,idxf
    val(k)=val(idx_mask(k))
enddo

end subroutine reallocate_fill
    
!
!END reallocate_fill_int
!

!
!BEGIN reallocate_fill_int
!

!FM1DIMP2DO: There must be a better way to do it. This is just the same but for integers
    
subroutine reallocate_fill_int(val,idx_mask,idxi,idxf)

use m_alloc

implicit none

integer, dimension(:), allocatable, intent(inout) :: val
integer, dimension(idxf), intent(in) :: idx_mask
integer, intent(in) :: idxi, idxf

integer :: k


call realloc(val,idxf)

do k=idxi+1,idxf
    val(k)=val(idx_mask(k))
enddo

end subroutine reallocate_fill_int
    
!
!END reallocate_fill_int
!

!
!BEGIN reallocate_fill_pointer
!

!FM1DIMP2DO: There must be a better way to do it. This is just the same but for integers
    
subroutine reallocate_fill_pointer(val,idx_mask,idxi,idxf)

use m_alloc

implicit none

real(fp), dimension(:), pointer, intent(inout) :: val
integer, dimension(idxf), intent(in) :: idx_mask
integer, intent(in) :: idxi, idxf

integer :: k


call reallocp(val,idxf)

do k=idxi+1,idxf
    val(k)=val(idx_mask(k))
enddo

end subroutine reallocate_fill_pointer
    
!
!END reallocate_fill_pointer
!

!
!BEGIN reallocate_fill_int_pointer
!
    
subroutine reallocate_fill_int_pointer(val,idx_mask,idxi,idxf)

use m_alloc

implicit none

integer, dimension(:), pointer, intent(inout) :: val
integer, dimension(idxf), intent(in) :: idx_mask
integer, intent(in) :: idxi, idxf

integer :: k


call reallocp(val,idxf)

do k=idxi+1,idxf
    val(k)=val(idx_mask(k))
enddo

end subroutine reallocate_fill_int_pointer
    
!
!END reallocate_fill_int_pointer
!

!
!BEGIN default_fm1dimp
!

subroutine default_fm1dimp()

use m_f1dimp_data

implicit none

f1dimppar%omega=0.5d0 !check sensible value
f1dimppar%psi=0.5d0
f1dimppar%theta=1.0d0 !It is rewriten if steady flow is chosen anyhow
f1dimppar%epsh=1.0d-5  
f1dimppar%epsq=1.0d-5  
f1dimppar%flitmx=100 
f1dimppar%epsqrl=1.0d-10
f1dimppar%lambda=0 
f1dimppar%relstr=1.0d0
f1dimppar%dhstru=1.0d-5
f1dimppar%cflpse=1000.0d0
f1dimppar%iterbc=100
f1dimppar%resid=1.0d-8 !check sensible value
f1dimppar%overlp=0 !change to summerdiketransitionheight -> check if <network%csdefinitions%cs(1)%summerdike> allocated?
f1dimppar%lconv=.true. !the input is converted to logical by calling soipar? setting to true we can break the simulation in FM code to handle the messages
f1dimppar%omcfl=0.9d0 !default in SRE
f1dimppar%dhtyp=0.1d0 !default in SRE
f1dimppar%exrstp=0.0d0 !default in SRE

end subroutine default_fm1dimp

!
!END default_fm1dimp
!

!
!BEGIN reallocate_fill_manual_3
!

subroutine reallocate_fill_manual_3(val,val_o,idx_mask,idxi,idxf,idxf1,idxf2)

implicit none

double precision, dimension(:,:,:), intent(inout) :: val
double precision, dimension(:,:,:), intent(in) :: val_o
integer, dimension(idxf), intent(in) :: idx_mask
integer, intent(in) :: idxi, idxf, idxf1, idxf2

integer :: k, k1, k2

    !copy
do k=1,idxi
    do k1=1,idxf1 !lsedtot
       do k2=1,idxf2 !nlyr
           val(k1,k2,k)=val_o(k1,k2,k)
       enddo !k2
    enddo !k1
enddo !k

    !fill
do k=idxi+1,idxf    
    do k1=1,idxf1 !lsedtot
       do k2=1,idxf2 !nlyr
            val(k1,k2,k)=val_o(k1,k2,idx_mask(k))
       enddo !k2
    enddo !k1
enddo !kl

end subroutine reallocate_fill_manual_3

!
!END reallocate_fill_manual_3
!

!
!BEGIN reallocate_fill_manual_2
!

subroutine reallocate_fill_manual_2(val,val_o,idx_mask,idxi,idxf,idxf1)

implicit none

double precision, dimension(:,:), intent(inout) :: val
double precision, dimension(:,:), intent(in) :: val_o
integer, dimension(idxf), intent(in) :: idx_mask
integer, intent(in) :: idxi, idxf, idxf1

integer :: k, k1

    !copy
do k=1,idxi
    do k1=1,idxf1 !lsedtot, nlyr
        val(k1,k)=val_o(k1,k)
    enddo !k1
enddo !k

    !fill
do k=idxi+1,idxf    
    do k1=1,idxf1 !lsedtot, nlyr
        val(k1,k)=val_o(k1,idx_mask(k))
    enddo !k1
enddo !kl

end subroutine reallocate_fill_manual_2

!
!END reallocate_fill_manual_2
!

end module m_f1dimp
