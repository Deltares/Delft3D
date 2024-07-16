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

!> Performs a single computational timestep, calling <SOFLOW> of Sobek-RE
    
subroutine SOFLOW_wrap(time1)   

use m_f1dimp
!use m_flow, only: s1, u1 !this subroutine is not in flow_kernel and cannot use this module

implicit none

!
!input
!

!integer, intent(in) :: ndx
!integer, intent(in) :: lnx
!integer, intent(in) :: lnx1d
!integer, intent(in) :: juer

double precision, intent(in) :: time1

!double precision, dimension(ndx), intent(in) :: s0
!double precision, dimension(ndx), intent(in) :: umag
!double precision, dimension(lnx), intent(in) :: au
!double precision, dimension(lnx), intent(in) :: wu

!
!output
!
!double precision, intent(out) :: h
!double precision, intent(out) :: u

!double precision, dimension(ndx), intent(out) :: s1
!double precision, dimension(lnx), intent(out) :: u1
!double precision, dimension(ndx) :: s1
!double precision, dimension(lnx) :: u1


!
!pointer
!

logical                                  , pointer :: lconv                   
logical                                  , pointer :: steady    
                                         
integer                                  , pointer :: flitmx                 
integer                                  , pointer :: iterbc                 
integer                                  , pointer :: ngrid   
integer                                  , pointer :: ngridm   
integer                                  , pointer :: nbran   
integer                                  , pointer :: maxlev
integer                                  , pointer :: nnode
integer                                  , pointer :: nhstat
integer                                  , pointer :: nqstat
integer                                  , pointer :: maxtab
integer                                  , pointer :: ntabm
integer                                  , pointer :: nbrnod
integer                                  , pointer :: juer

integer, dimension(:)                    , pointer :: nlev
integer, dimension(:)                    , pointer :: numnod

integer, dimension(:,:)                  , pointer :: branch
integer, dimension(:,:)                  , pointer :: bfrict
integer, dimension(:,:)                  , pointer :: hbdpar
integer, dimension(:,:)                  , pointer :: qbdpar
integer, dimension(:,:)                  , pointer :: ntab
integer, dimension(:,:)                  , pointer :: node
integer, dimension(:,:)                  , pointer :: nodnod

real                                     , pointer :: g
real                                     , pointer :: psi                    
real                                     , pointer :: theta                  
real                                     , pointer :: epsh                   
real                                     , pointer :: epsq                   
real                                     , pointer :: rhow                   
real                                     , pointer :: omega                  
real                                     , pointer :: epsqrl                 
real                                     , pointer :: lambda                 
real                                     , pointer :: relstr                 
real                                     , pointer :: dhstru                 
real                                     , pointer :: cflpse                               
real                                     , pointer :: overlp                 
real                                     , pointer :: omcfl                  
real                                     , pointer :: dhtyp                  
real                                     , pointer :: exrstp     

real, dimension(:)                       , pointer :: table
real, dimension(:)                       , pointer :: x

real, dimension(:,:)                     , pointer :: bfricp
real, dimension(:,:)                     , pointer :: wft
real, dimension(:,:)                     , pointer :: aft
real, dimension(:,:)                     , pointer :: wtt
real, dimension(:,:)                     , pointer :: att
real, dimension(:,:)                     , pointer :: of
real, dimension(:,:)                     , pointer :: waoft


double precision                         , pointer :: time
double precision                         , pointer :: dtf
double precision                         , pointer :: resid

double precision, dimension(:,:)         , pointer :: hpack
double precision, dimension(:,:)         , pointer :: qpack
double precision, dimension(:,:)         , pointer :: hlev

!debug
integer                                  , pointer :: fm1dimp_debug_k1
integer                                  , pointer :: debug_wr

!local
!integer                              :: N
integer                              :: swaoft


!
!f1dimp variables
!

!#BEGIN# MOVE TO CONVERSION ROUTINE FOR EVERY TIME STEP

!<SOFLOW> input
f1dimppar%istep=1 
!<itim> only used for writing to file when error
!f1dimppar%itim(1)=20000101 
!f1dimppar%itim(2)=00000000
f1dimppar%time=time1
f1dimppar%dtf=1.0d0 !as we do steady, it is overwritten

!#END# MOVE TO CONVERSION ROUTINE FOR EVERY TIME STEP

!<flwpar> variables
g      => f1dimppar%g
psi    => f1dimppar%psi
theta  => f1dimppar%theta
epsh   => f1dimppar%epsh
epsq   => f1dimppar%epsq
rhow   => f1dimppar%rhow
omega  => f1dimppar%omega
epsqrl => f1dimppar%epsqrl
lambda => f1dimppar%lambda
relstr => f1dimppar%relstr
dhstru => f1dimppar%dhstru
cflpse => f1dimppar%cflpse
resid  => f1dimppar%resid
overlp => f1dimppar%overlp
omcfl  => f1dimppar%omcfl
dhtyp  => f1dimppar%dhtyp
exrstp => f1dimppar%exrstp
flitmx => f1dimppar%flitmx

!<SOFLOW> variables
time   => f1dimppar%time
dtf    => f1dimppar%dtf
steady => f1dimppar%steady

!dimensions
ngrid  => f1dimppar%ngrid
ngridm => f1dimppar%ngridm
nbran  => f1dimppar%nbran
maxlev => f1dimppar%maxlev
nnode  => f1dimppar%nnode 
nhstat => f1dimppar%nhstat 
nqstat => f1dimppar%nqstat
maxtab => f1dimppar%maxtab
ntabm  => f1dimppar%ntabm
nbrnod => f1dimppar%nbrnod
nlev   => f1dimppar%nlev

!dependent on branch
branch => f1dimppar%branch
bfrict => f1dimppar%bfrict

!dependent on gridpoints 
bfricp => f1dimppar%bfricp
hpack  => f1dimppar%hpack
qpack  => f1dimppar%qpack
x      => f1dimppar%x
waoft  => f1dimppar%waoft 

!cross-sectional shape
wft  => f1dimppar%wft 
aft  => f1dimppar%aft 
wtt  => f1dimppar%wtt 
att  => f1dimppar%att 
of   => f1dimppar%of  
hlev => f1dimppar%hlev

!boundary conditions
hbdpar => f1dimppar%hbdpar
qbdpar => f1dimppar%qbdpar

!tables
table  => f1dimppar%table
ntab   => f1dimppar%ntab

!dependent on node
node   => f1dimppar%node
numnod => f1dimppar%numnod
nodnod => f1dimppar%nodnod

!debug
debug_wr         => f1dimppar%debug_wr
fm1dimp_debug_k1 => f1dimppar%fm1dimp_debug_k1

!other
juer             => f1dimppar%juer

debug_wr=0

if (debug_wr>0) then
write(42,*) 'SOFLOW_wrap'
write(42,*) fm1dimp_debug_k1
write(42,*) 'q1'
write(42,*) qpack(:,1)
write(42,*) 'q3'
write(42,*) qpack(:,3)
write(42,*) 'h1'
write(42,*) hpack(:,1)
write(42,*) 'h3'
write(42,*) hpack(:,3)
write(42,*) 'waoft3'
write(42,*) waoft(:,3)

write(42+fm1dimp_debug_k1,*) 'hlev'
write(42+fm1dimp_debug_k1,*) hlev
endif

call SOFLOW( &
!<flwpar> input
        &   g      , psi    , theta  , epsh   , epsq   , &
        &   rhow   , omega  , epsqrl , lambda , relstr , &
        &   dhstru , cflpse , resid  , overlp , omcfl  , &
        &   dhtyp  , exrstp , flitmx                   , &
!<SOFLOW> input
        &   time   , dtf    , steady                   , &
!dimensions 
        &   ngrid  , ngridm , nbran  , maxlev , nnode  , &
        &   nhstat , nqstat , maxtab , ntabm  , nbrnod , &
        &   nlev                                       , &
!dependent on branch
        &   branch , bfrict                            , &
!dependent on gridpoints 
        &   bfricp , hpack  , qpack  ,x       , waoft  , & 
!cross-sectional shape
        &   wft    , aft    ,wtt     ,att     , of     , & 
        &   hlev                                       , &
!boundary conditions
        &   hbdpar , qbdpar                            , &
!tables
        &   table  , ntab                              , &
!dependent on node
        &   node   , numnod ,nodnod                    , &
!debug 
        &   debug_wr                                   , &
!units  
        &   juer                                       , &
!close
        &)
    
!FM1DIMP2DO: remove debug
fm1dimp_debug_k1=fm1dimp_debug_k1+1 !FM1DIMP2DO: remove debug variables

if (debug_wr>0) then
write(42,*) 'SOFLOW'
write(42,*) fm1dimp_debug_k1
write(42,*) 'q1'
write(42,*) qpack(:,1)
write(42,*) 'q3'
write(42,*) qpack(:,3)
write(42,*) 'h1'
write(42,*) hpack(:,1)
write(42,*) 'h3'
write(42,*) hpack(:,3)
write(42,*) 'waoft3'
write(42,*) waoft(:,3)
endif

end subroutine SOFLOW_wrap