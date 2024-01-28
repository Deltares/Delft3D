subroutine bewvuf(ierrs     ,kcmp      ,mxkc      ,inaam     ,knaam     , &
                & jnaam     ,w         ,v0u       ,fr        ,v         , &
                & f         )
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
!    Function: calculates V0U() and FR()
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    implicit none
!
! Global variables
!
    integer                                       :: ierrs !!  Number of error messages
    integer                         , intent(in)  :: kcmp
    integer                         , intent(in)  :: mxkc
    integer     , dimension(mxkc*16), intent(in)  :: jnaam !!  Help var.
    character(8), dimension(0:kcmp) , intent(in)  :: inaam !!  Name of the referenced components
    character(8), dimension(mxkc)   , intent(in)  :: knaam !!  Names of all components
    real(hp)    , dimension(15)     , intent(in)  :: v     !!  Help var. to calculate V0U()
    real(hp)    , dimension(25)     , intent(in)  :: f     !!  Help var. to calculate FR()
    real(hp)    , dimension(kcmp)                 :: fr    !!  Amplitude factors for the referenced
                                                           !!  components
    real(hp)    , dimension(kcmp)                 :: v0u   !!  Astronomical arguments of the
                                                           !!  referenced components
    real(hp)    , dimension(kcmp)                 :: w     !!  Angular velocity of the referenced
                                                           !!  components
!
! Local variables
!
    integer  :: ia1
    integer  :: ia2
    integer  :: iar
    integer  :: ie1
    integer  :: ie2
    integer  :: iex
    integer  :: ikomp
    integer  :: j
    integer  :: kw
    integer  :: kx
    integer  :: mh
    integer  :: mp
    integer  :: mp1
    integer  :: ms
    integer  :: mt
    real(hp) :: dhalf   ! Value for 0.5 in SIGN function 
    real(hp) :: pix2
    real(hp) :: s1
    real(hp) :: s2
!
!! executable statements -------------------------------------------------------
!
    pix2 = 8.0d0*atan(1.0d0)
    dhalf = 0.5d0
    !
    ! loop over given components
    !
    do ikomp = 1, kcmp
       !
       ! loop over the elements of kompbes
       !
       do j = 1, mxkc
          !
          ! test on name of present component
          !
          if (inaam(ikomp)==knaam(j)) then
             !
             ! compute angular velocity
             !
             mt = jnaam(16*j - 15)
             ms = jnaam(16*j - 14)
             mp = jnaam(16*j - 13)
             mh = jnaam(16*j - 12)
             mp1 = jnaam(16*j - 11)
             w(ikomp) = mt*15.0d0 + ms*0.54901653d0 + mp*0.0046418333d0 +       &
                      & mh*0.04106864d0 + mp1*0.0000019610393d0
             w(ikomp) = (w(ikomp)*pix2)/360.0d0
             !
             ! compute v0+u
             !
             v0u(ikomp) = (jnaam(16*j - 8)*pix2)/4.0d0
             do kw = 1, 7
                kx = 16*j - 16 + kw
                v0u(ikomp) = v0u(ikomp) + v(kw)*jnaam(kx)
             enddo
             ie1 = jnaam(16*j - 7)
             if (ie1/=0) then
                ia1 = abs(ie1)
                s1 = real(ie1/ia1, hp)
                v0u(ikomp) = v0u(ikomp) + s1*v(ia1)
                ie2 = jnaam(16*j - 6)
                if (ie2/=0) then
                   ia2 = abs(ie2)
                   s2 = real(ie2/ia2, hp)
                   v0u(ikomp) = v0u(ikomp) + s2*v(ia2)
                endif
             endif
             v0u(ikomp) = mod(v0u(ikomp), pix2)                                 &
                        & - pix2*(sign(dhalf, v0u(ikomp)) - dhalf)
             !
             ! compute f
             !
             fr(ikomp) = 1.0d0
             iex = jnaam(16*j - 5)
             if (iex/=0) then
                iar = jnaam(16*j - 4)
                fr(ikomp) = (f(iar))**iex
                iex = jnaam(16*j - 3)
                if (iex/=0) then
                   iar = jnaam(16*j - 2)
                   fr(ikomp) = fr(ikomp)*(f(iar))**iex
                   iex = jnaam(16*j - 1)
                   if (iex/=0) then
                      iar = jnaam(16*j)
                      fr(ikomp) = fr(ikomp)*(f(iar))**iex
                   endif
                endif
             endif
             exit
          endif
          if (j>=mxkc) then
             ierrs = ierrs + 1
             write (*, '(a,a,a)') '*** ERROR Component ', inaam(ikomp),         &
                                 & ' not in internal component base'
             exit
          endif
       enddo
    enddo
end subroutine bewvuf
