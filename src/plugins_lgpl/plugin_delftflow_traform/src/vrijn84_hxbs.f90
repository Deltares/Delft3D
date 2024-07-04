subroutine vrijn84_hxbs(dll_integers, max_integers, &
                  dll_reals   , max_reals   , &
                  dll_strings , max_strings , &
                  sbc_total, sbc  , sbcu, sbcv, sbwu, sbwv     , &
                  equi_conc, cesus, ssus, sswu, sswv, t_relax  , &
                  error_message_c  )
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS: 'VRIJN84_HXBS' :: VRIJN84_HXBS
!!--copyright-------------------------------------------------------------------
! Copyright (c) 2005-2024, WL | Delft Hydraulics. All rights reserved.
!!--disclaimer------------------------------------------------------------------
! This code is part of the Delft3D software system. WL|Delft Hydraulics has
! developed c.q. manufactured this code to its best ability and according to the
! state of the art. Nevertheless, there is no express or implied warranty as to
! this software whether tangible or intangible. In particular, there is no
! express or implied warranty as to the fitness for a particular purpose of this
! software, whether tangible or intangible. The intellectual property rights
! related to this software code remain with WL|Delft Hydraulics at all times.
! For details on the licensing agreement, we refer to the Delft3D software
! license and any modifications to this license, if applicable. These documents
! are available upon request.
!!--version information---------------------------------------------------------
! $W.Ottevanger$
! $April_07$
! $Revision$
!!--description-----------------------------------------------------------------
!
! Computes sediment transport according to
! van Rijn 1984 including correction due to hiding and exposure
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
use iso_c_binding, only: c_char
implicit none
!
! Local constants
! Interface is in high precision
!
integer        , parameter :: hp   = kind(1.0d0)
!
! Global variables
!
integer                                    , intent(in)  :: max_integers
integer                                    , intent(in)  :: max_reals
integer                                    , intent(in)  :: max_strings
integer           , dimension(max_integers), intent(in)  :: dll_integers
real(hp)          , dimension(max_reals)   , intent(in)  :: dll_reals
real(hp)                                   , intent(out) :: sbc           ! Unit [m3/m/s]
real(hp)                                   , intent(out) :: sbcu          ! Unit [m3/m/s]
real(hp)                                   , intent(out) :: sbcv          ! Unit [m3/m/s]
real(hp)                                   , intent(out) :: sbwu          ! Unit [m3/m/s]
real(hp)                                   , intent(out) :: sbwv          ! Unit [m3/m/s]
real(hp)                                   , intent(out) :: cesus
real(hp)                                   , intent(out) :: ssus
real(hp)                                   , intent(out) :: sswu
real(hp)                                   , intent(out) :: sswv
real(hp)                                   , intent(out) :: t_relax
character(len=256), dimension(max_strings) , intent(in)  :: dll_strings
character(kind=c_char)                     , intent(out) :: error_message_c(*) ! not empty: echo and stop run
logical                                    , intent(out) :: equi_conc     ! equilibrium concentration near bedlevel
logical                                    , intent(out) :: sbc_total     ! Sediment bedlevel concentration  (scalar)
!
! Local variables for error message
!
character(len=256) :: error_message
!
! Initialize the output argument error_message since it MUST have value ' ' to continue the calculation.
error_message = ' '
!
call core_function() ! Core function call 
!
call message2c(error_message, error_message_c)

contains
    
!
! Core function definition 
!
subroutine core_function()
!
! Local variables
!
integer            :: kode
integer            :: ntrsi
integer			   :: lundia 

real(hp)           :: acal
real(hp)           :: ag     ! gravity acceleration
real(hp)           :: c
real(hp)           :: cc
real(hp)           :: cf
real(hp)           :: chezy
real(hp)           :: d10
real(hp)           :: d50
real(hp)           :: d90
real(hp)           :: delta  ! relative density of sediment particle
real(hp)           :: di50
real(hp)           :: dss
real(hp)           :: dstar
real(hp)           :: h
real(hp)           :: hidexp
real(hp)           :: hrms
real(hp)           :: mudfrac
real(hp)           :: rhosol
real(hp)           :: rhowat
real(hp)           :: rk
real(hp)           :: rlabda
real(hp)           :: teta
real(hp)           :: timsec
real(hp)           :: tp
real(hp)           :: u
real(hp)           :: umod
real(hp)           :: uorb
real(hp)           :: utot
real(hp)           :: uuu
real(hp)           :: v
real(hp)           :: vster
real(hp)           :: vvv
real(hp)           :: ws     ! settling velocity
real(hp)           :: zumod
character(len=256) :: runid
character(len=80)  :: line
logical, save      :: firsttime = .true.
logical, save      :: original = .true.
integer, save      :: ihidexp = 1
real(hp), save     :: rhidexp = 1.0
!
! Local variables
!
    integer        :: i 
    real(hp)       :: a
    real(hp)       :: ah
    real(hp), save :: alf1
    real(hp)       :: beta   ! lowest level of integration interval over vertical
    real(hp)       :: ca
    real(hp)       :: del
    real(hp)       :: dster
    real(hp)       :: fc
    real(hp)       :: ff     ! coriolis coefficient
    real(hp)       :: g      ! gravity acceleration
    real(hp)       :: psi
    real(hp)       :: rhos   ! density of sediment
    real(hp)       :: rhow   ! density of water
    real(hp)       :: rkap
    real(hp)       :: rksc
    real(hp), save :: rmuc
    real(hp)       :: rnu    ! laminar viscosity of water
    real(hp)       :: t      ! time in seconds
    real(hp)       :: tbc
    real(hp)       :: tbce
    real(hp)       :: tbcr
	real(hp)       :: tbcrhx
    real(hp)       :: thetcr
    real(hp)       :: uster
    real(hp)       :: zc
    real(hp)       :: shld 
	real(hp)	   :: d50tot  ! D50 van het totale mengsel
!
!! extract array variables -----------------------------------------------------
!
kode    = dll_integers( 1)
!
timsec  = dll_reals( 1)
u       = dll_reals( 2)
v       = dll_reals( 3)
utot    = dll_reals( 4)
uuu     = dll_reals( 5)
vvv     = dll_reals( 6)
umod    = dll_reals( 7)
zumod   = dll_reals( 8)
h       = dll_reals( 9)
c       = dll_reals(10)
hrms    = dll_reals(11)
tp      = dll_reals(12)
teta    = dll_reals(13)
rlabda  = dll_reals(14)
uorb    = dll_reals(15)
d50     = dll_reals(16)
dss     = dll_reals(17)
dstar   = dll_reals(18)
d10     = dll_reals(19)
d90     = dll_reals(20)
mudfrac = dll_reals(21)
hidexp  = dll_reals(22)
ws      = dll_reals(23)
rhosol  = dll_reals(24)
rhowat  = dll_reals(25)
!
runid   = dll_strings( 1)
!
!! executable statements -------------------------------------------------------
!
sbc_total     = .true.
equi_conc     = .false.    

    lundia = 12345
	if (firsttime) then 
	  firsttime = .false. 
	    write(*,*) 'Van Rijn transport formula (including hiding exposure) -- Version 0.3 (bedload = bedload+suspended)'
	    open (newunit=lundia, file='vr84hx.par')
		read (lundia,*) line
		read (line,*) rmuc  !Constant ripple factor 
        if (rmuc < 0.0) then
           write(*,*) 'Ripple factor computed using D90 of complete mixture and local Chezy roughness'
	    else
		   write(*,*) 'Constant ripple factor', rmuc
        endif
	    read (lundia,*) line
		read (line,*) alf1  !Calibration coefficient a_1 (See Flow Manual p. B-50)
		write(*,*) 'Calibration Coefficient a_1 (See Flow Manual p. B-50)', alf1
		read (lundia,*) line
		read (line,*) original !Use original van Rijn (TRUE) or adjustment Kleinhans (FALSE)
			if (original) then
				write(*,*) 'Using original Van Rijn 84'
			else
		        write(*,*) 'Using adjusted Van Rijn 84, (Kleinhans, Van Rijn 2002)'
			endif
	    read (lundia,*) line
		read (line,*) ihidexp  !Hiding exposure coefficient (See Flow Manual p. 11-11)
!                                                    1 (default): none
!                                                    2          : Egiazaroff
!                                                    3          : Ashida & Michiue, modified Egiazaroff
!                                                    4          : Soehngen, Kellermann, Loy
!                                                    5          : Wu, Wang, Jia
		select case (ihidexp)	
			case (1)
				write(*,*) 'Hiding Exposure Formulation: none'
			case (2)
				write(*,*) 'Hiding Exposure Formulation: Egiazaroff'
			case (3)
				write(*,*) 'Hiding Exposure Formulation: Ashida & Michiue, modified Egiazaroff'
			case (4)
				write(*,*) 'Hiding Exposure Formulation: Soehngen, Kellermann, Loy'
			    read (lundia,*) line
				read (line,*) rhidexp  
				write(*,*) 'ASKLHE', rhidexp
			case (5)
				write(*,*) 'Hiding Exposure Formulation: Wu, Wang, Jia'
			    read (lundia,*) line
				read (line,*) rhidexp  
				write(*,*) 'MWWJHE', rhidexp
			case default
				write(*,*) 'Hiding Exposure Formulation: none'
		end select
	    close (lundia)
	endif 
		select case (ihidexp)	
			case (2)
				!hidexp = (log(19.0)/(log(19.0)+log(d50/d50tot)))**2
				!d50/d50tot = 10**(log(19.0)/sqrt(hidexp)-log(19.0))
				d50tot = d50/(10**(log(19.0)/sqrt(hidexp)-log(19.0)))
			case (4)
				d50tot = hidexp**(1.0/rhidexp)*d50
			case default
				error_message = 'D50 of complete mixture on the basis of hiding-exposure not possible'
                return 
		end select

	ntrsi = 1
    sbc = 0.0
    ssus = 0.0
    !
    if (kode== - 1) then
       return
    endif

    g    = 9.81 !par(1)  !
    rhow = rhowat  !par(2)
    rhos = rhosol  !par(3)
    del = (rhos - rhow)/rhow
    
	if (c <	21.65459 .or. utot<1.E-3) then
       return
    endif
    !
	a = 12.0*h/(10**(c/18.0))  
    
	
	dster = dstar*d50tot/d50 !Bereken D* van het complete mengsel 
	
    !
    if (rmuc < 0.0) then
		rmuc = ((c/18.0)/log10(12.*h/d90))**2
    endif
	!rmuc wordt anders ingelezen in vr84hx.par

    !fc = .24*(log10(12.*h/rksc))**( - 2)
	fc = .24*(c/18.0)**( - 2)

    tbc = .125*rhow*fc*utot**2
    tbce = rmuc*tbc


	!!!Bereken kritische shields waarde
	!!!--------------------------------
	if (dster<=4.) then
       shld = 0.240/dster
    elseif (dster<=10.) then
       shld = 0.140/dster**0.64
    elseif (dster<=20.) then
       shld = 0.040/dster**0.10
    elseif (dster<=150.) then
       shld = 0.013*dster**0.29
    else
       shld = 0.055
    endif
	!!!--------------------------------


    thetcr = shld  
	tbcrhx = (rhos - rhow)*g*d50*hidexp*thetcr    
	tbcr = (rhos - rhow)*g*d50*thetcr
    t = (tbce - tbcrhx)/tbcr   !TOEGEVOEGD: hiding exposure correction. 
    !
    if (t<.000001) t = .000001
    ca = .015*alf1*d50/a*t**1.5/dster**.3
    rkap = .4
    !
    uster = sqrt(.125*fc)*utot
    zc = 0.
    beta = 1. + 2.*(ws/uster)**2
    beta = min(beta, 1.5_hp)
    psi = 2.5*(ws/uster)**0.8*(ca/0.65)**0.4
    if (uster>0.) zc = ws/rkap/uster/beta + psi
    if (zc>20.) zc = 20.
    ah = a/h
    fc = 0.
    if (abs(zc - 1.2)>1.E-4) then
       fc = (ah**zc - ah**1.2)/(1. - ah)**zc/(1.2 - zc)
    else
       fc = -(ah/(1. - ah))**1.2*log(ah)
    endif
    ff = fc
    ssus = ff*utot*h*ca
    !
    
	if (original) then 
		if (t<3.) then
			sbc = 0.053*(del)**0.5*sqrt(g)*d50**1.5*dster**( - 0.3)*t**2.1
	    else
			sbc = 0.100*(del)**0.5*sqrt(g)*d50**1.5*dster**( - 0.3)*t**1.5
	    endif
	else 
		sbc = 0.100*(del)**0.5*sqrt(g)*d50**1.5*dster**( - 0.3)*t**1.5
	endif
	sbc = sbc + ssus
	ssus = 0.0_hp
!
! Not used output
!
sbcu    = 0.0_hp
sbcv    = 0.0_hp
sbwu    = 0.0_hp
sbwv    = 0.0_hp
cesus   = 0.0_hp
sswu    = 0.0_hp
sswv    = 0.0_hp
t_relax = 0.0_hp

end subroutine core_function

end subroutine vrijn84_hxbs
