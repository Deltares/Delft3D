!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling system
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          SOBEK
!
! Programmer:         S.L. van der Woude
!
! Module:             Sobek constants
!
! Module description: Include file with sobek constants
!
!
!
! Pre condition:
!
! Post condition:
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
!=======================================================================

!
!***********************************************************************
! CVS log information:
!
! $Id: sobcon.i,v 1.9 1999/03/15 14:08:22 kuipe_j Exp $
!
! History:
! $Log: sobcon.i,v $
! Revision 1.9  1999/03/15  14:08:22  kuipe_j
! devided segmentes added
!
! Revision 1.8  1998/05/25  19:16:45  kuipe_j
! Wendy structures
!
! Removed on 8-6-96
!
!***********************************************************************
!
!     Definition of the individual constants
!
!
!                          Normal gridpoint
integer    cgrdcl
!                          Structure cell
integer    cstrcl
!                          Internal node
integer    cintnd
!                          H-boundary
integer    chbou
!                          Q-boundary
integer    cqbou
!                          QH-boundary
integer    cqhbou
!                          HQ-boundary
integer    chqbou
!                          No layer defined
integer    cnlayd
!                          Layer defined
integer    cylayd
!                          Reduction function straight
integer    crdstr
!                          Sinus reduction function
integer    crdsin
!                          Exchange in gridpoint
integer    cexigp
!                          Exchange between gridpoint
integer    cexbgp
!                          Exchange in node
integer    cexind
!                          Exchange from Qlat to segment
integer    cexqlt
!                          Exchange in seperated segment
integer    cexbsc
!                          Positive pointer
integer    cpopnt
!                          Negative pointer
integer    cnepnt
!                          No parallel sections
integer    cnopar
!                          Main channel
integer    cmainc
!                          Sub section 1
integer    csubs1
!                          Sub section 2
integer    csubs2
!                          Equally over actual ws
integer    ceqows
!                          Proportional to local depth
integer    cprodp
!                          Tabulated cross section
integer    ccrtab
!                          Circel cross section
integer    ccrcir
!                          Sedredge cross section
integer    ccrsed
!                          Only main channel
integer    c1sec
!                          Main channel + sub section 1
integer    c2sec
!                          Main channel + sub section 1 and 2
integer    c3sec
!                          Disable slot generation
integer    csldis
!                          Enable slot generation
integer    cslena
!                          Simple weir
integer    csweir
!                          Advanced weir
integer    caweir
!                          Data base structure
integer    cdtbst
!                          Pump
integer    cpump
!                          General structure
integer    cgenst
!                          Culvert
integer    cclvrt
!                          Sluice with underflow gate
integer    cslund
!                          Open Flume
integer    cflume
!                          Gated culvert with pressure flow
integer    cculpr
!                          Sluice with bottom hinged gate
integer    cslubo
!                          Sluice with overflow/underflow gate
integer    csovun
!                          Bridge piers
integer    cbridg
!                          Abutment
integer    cabutm
!                          Structure in branch
integer    cstbra
!                          Lateral structure
integer    cstlat
!                          Not controlled
integer    cnocon
!                          No trigger
integer    cnotrg
!                          Time trigger
integer    ctitrg
!                          Hydraulic trigger
integer    chytrg
!                          Hydraulic and/or timetrigger
integer    candor
!                          Pump controlled upward
integer    cpmpup
!                          Pump controlled downward
integer    cpmpdw
!                          Time controller
integer    ctimcn
!                          Time controller absolute times
integer    ctmabs
!                          Time controller relative times zero
integer    ctmrl0
!                          Time controller relative times value
integer    ctmrlv
!                          PID controller
integer    cpidcn
!                          Hydraulic controller
integer    chydcn
!                          Interval controller
integer    cintcn
!                          Control parameter: Crest height
integer    ccpcrh
!                          Control parameter: Crest width
integer    ccpcrw
!                          Control parameter: Gate opening heigth
integer    ccpgat
!                          Water level controlled
integer    cconh
!                          Discharge controlled
integer    cconq
!                          Head difference controlled
integer    cconhd
!                          Velocity controlled
integer    cconu
!                          Stream direction
integer    cconsd
!                          Pressure difference
integer    cconpd
!                          Fixed interval
integer    cintfx
!                          Variable interval
integer    cintvr
!                          Fixed deadband
integer    cdbdfx
!                          Deadband as percentage of discharge
integer    cdbdpq
!                          Off trigger as function of time
integer    ctrgof
!                          On trigger as function of time
integer    ctrgon
!                          Trigger on water level
integer    ctrgh
!                          Trigger on head difference
integer    ctrghd
!                          Trigger on discharge
integer    ctrgdi
!                          Trigger on gate height
integer    ctrggh
!                          Trigger on crest height
integer    ctrgch
!                          Trigger on crest width
integer    ctrgcw
!                          Trigger on level of retention basin
integer    ctrghlat
!                          Trigger on pressure difference
integer    ctrgpd
!                          Or trigger
integer    ctrgor
!                          And trigger
integer    ctrgan
!                          Equal dispersion of structure
integer    cdsequ
!                          Different dispersion of structure (1)
integer    cdsdf1
!                          Different dispersion of structure (2)
integer    cdsdf2
!                          Chezy value constant
integer    cfrchc
!                          Chezy value function of Q
integer    cfrchq
!                          Chezy value function of h
integer    cfrchh
!                          Manning
integer    cfrman
!                          Strickler-1  (Kn)
integer    cfrskn
!                          Strickler-2  (Ks)
integer    cfrsks
!                          Nikuradze
integer    cfrnik
!                          Engelund predictor
integer    cfreng
!                          No wind friction defined
integer    cnwndf
!                          Wind friction defined
integer    cywndf
!                          boundary = function of time
integer    cbftim
!                          boundary = function of h or Q
integer    cbfhoq
!                          boundary = fourier function
integer    cbfour
!                          boundary = tidal components
integer    cbtidl
!                          Qlat     = function of time
integer    cqlftm
!                          Qlat     = function of water level
integer    cqlfh
!                          Qlat     = function of structure
integer    cqlfst
!                          Qlat     = function of connected station
integer    cqlret
!                          Qlat     = function of retentiebekken
integer    cqlcon
!                          Point discharge in 1 gridpoint
integer    cpd1gp
!                          Point discharge in 1 gridcell
integer    cpd1gc
!                          Traject discharge in 1 gridcell
integer    ctd1gc
!                          Traject discharge in more gridcells
integer    ctdmgc
!                          Salt boundary user specified
integer    csbusr
!                          Salt boundary Thatcher-Harleman
integer    csbthh
!                          Salt boundary Zero-flux
integer    csbflx
!                          Morphological boundary S=f(t)
integer    cmbsft
!                          Morphological boundary S=f(Q)
integer    cmbsfq
!                          Morphological boundary z=f(t)
integer    cmbzft
!                          Proportional distribution function
integer    cdbpro
!                          Lineair distribution function
integer    cdblin
!                          Ratio distribution function
integer    cdbrat
!                          Power distribution function
integer    cdbpow
!                          Engelund & Hansen transport formula
integer    ctrfeh
!                          Meyer-Peter & Muller transport formula
integer    ctrfmm
!                          Ackers & White transport formula
integer    ctrfaw
!                          Van Rijn transport formula
integer    ctrfvr
!                          Parker & Klingemann transport formula
integer    ctrfpk
!                          User defined transport formula
integer    ctrfud
!                          Ripple factor is constant
integer    crfcon
!                          Ripple factor must be calculated
integer    crfcal
!                          Dispersion function 1
integer    cds1fu
!                          Dispersion function 2
integer    cds2fu
!                          Dispersion Thatcher-Harleman
integer    cdsthh
!                          Dispersion Empirical
integer    cdsemp
!                          Dispersion is function of time
integer    cdsftm
!                          Dispersion is function of place
integer    cdsfpl
!                          No periodical function
integer    ctbnpf
!                          Periodical function
integer    ctbpfu
!                          Table Interpolate continuously
integer    ctbico
!                          Table Interpolate discrete
integer    ctbidi
!                          Delta check value
real       cdchk
!                          Value to add or distract to avoid devision
!                          by zero
real       cdval
!
!     Assign values to constants
!
!
!     grid cells types
!
parameter (cgrdcl = 1,&
&cstrcl = 2&
&)
!
!     node types
!
parameter (cintnd = 1,&
&chbou  = 2,&
&cqbou  = 3,&
&cqhbou = 4,&
&chqbou = 5&
&)
!
!     Fixed layer constants
!
parameter (cnlayd = 0,&
&cylayd = 1&
&)
!
!     Type of reduction function for fixed layers
!
parameter (crdstr = 1,&
&crdsin = 2&
&)
!
!     exchange type for water quality
!
parameter (cexigp = 1,&
&cexbgp = 2,&
&cexind = 3,&
&cexqlt = 4,&
&cexbsc = 5&
&)
!
!     pointer types
!
parameter (cpopnt = 1,&
&cnepnt = -1&
&)
!
!     section types
!
parameter (cnopar = 0,&
&cmainc = 1,&
&csubs1 = 2,&
&csubs2 = 3&
&)
!
!     Morphology adaption options
!
parameter (ceqows = 1,&
&cprodp = 2&
&)
!
!     cross section types
!
parameter (ccrtab = 1,&
&ccrcir = 2,&
&ccrsed = 3&
&)
!
!     sections types
!
parameter (c1sec  = 0,&
&c2sec  = 1,&
&c3sec  = 2&
&)
!
!     Slot disabled or enabled
!
parameter (csldis = 0,&
&cslena = 1&
&)
!
!     Structure types
!
parameter (csweir = 1,&
&caweir = 2,&
&cdtbst = 3,&
&cpump  = 4,&
&cgenst = 5,&
!     Wendy structures added
&cclvrt = 6,&
&cslund = 7,&
&cflume = 8,&
&cculpr = 9,&
&cslubo = 10,&
&csovun = 11,&
&cbridg = 12,&
&cabutm = 13&
&)
!
!     Place of structure
!
parameter (cstbra = 1,&
&cstlat = 2&
&)
!
!     trigger types
!
parameter (cnotrg = 0,&
&ctitrg = 1,&
&chytrg = 2,&
&candor = 3&
&)
!
!     Control directions for pump
!
parameter (cpmpup = -1,&
&cpmpdw = 1&
&)
!
!     controller types
!
parameter (cnocon = 0,&
&ctimcn = 1,&
&chydcn = 2,&
&cintcn = 3,&
&cpidcn = 4&
&)
!
!     Time controller
!
parameter (ctmabs = 0,&
&ctmrl0 = 1,&
&ctmrlv = 2&
&)
!
!     control parameter types
!
parameter (ccpcrh = 1,&
&ccpcrw = 2,&
&ccpgat = 3&
&)
!
!     controlled variables
!
parameter (cconh  = 1,&
&cconq  = 2,&
&cconhd = 3,&
&cconu  = 4,&
&cconsd = 5,&
&cconpd = 6&
&)
!
!     Interval types
!
parameter (cintfx = 1,&
&cintvr = 2&
&)
!
!     Deadband types
!
parameter (cdbdfx = 1,&
&cdbdpq = 2&
&)
!
!     Trigger off, trigger on
!
parameter (ctrgof = 0,&
&ctrgon = 1&
&)
!
!     Hydraulic trigger types
!
parameter (ctrgh  = 1,&
&ctrghd = 2,&
&ctrgdi = 3,&
&ctrggh = 4,&
&ctrgch = 5,&
&ctrgcw = 6,&
&ctrghlat = 7,&
&ctrgpd = 8&
&)
!
!     Trigger or, trigger and
!
parameter (ctrgor = 0,&
&ctrgan = 1&
&)
!
!     Dispersion for structures
!
parameter (cdsequ = 1,&
&cdsdf1 = 2,&
&cdsdf2 = 3&
&)
!
!     Friction types
!
parameter (cfrchc = 1,&
&cfrchq = 2,&
&cfrchh = 3,&
&cfrman = 4,&
&cfrskn = 5,&
&cfrsks = 6,&
&cfrnik = 7,&
&cfreng = 8&
&)
!
!     Wind disabled, enabled
!
parameter (cnwndf = 0,&
&cywndf = 1&
&)
!
!     boundary function types
!
parameter (cbftim = 1,&
&cbfhoq = 2,&
&cbfour = 3,&
&cbtidl = 4&
&)
!
!     Lateral discharge types
!
parameter (cqlftm = 1,&
&cqlfh  = 2,&
&cqlfst = 3,&
&cqlcon = 4,&
&cqlret = 5&
&)
!
!     Lateral discharge location types
!
parameter (cpd1gp = 1,&
&cpd1gc = 2,&
&ctd1gc = 3,&
&ctdmgc = 4&
&)
!
!     Salt boundary types
!
parameter (csbusr = 1,&
&csbthh = 2,&
&csbflx = 3&
&)
!
!     Morphology boundary types
!
parameter (cmbsft = 1,&
&cmbsfq = 2,&
&cmbzft = 3&
&)
!
!     Sediment distribution functions
!
parameter (cdbpro = 1,&
&cdblin = 2,&
&cdbrat = 3,&
&cdbpow = 4&
&)
!
!     Sediment transport formulas
!
parameter (ctrfeh = 1,&
&ctrfmm = 2,&
&ctrfaw = 3,&
&ctrfvr = 4,&
&ctrfpk = 5,&
&ctrfud = 6&
&)
!
!     Ripple factor constant/calculation
!
parameter (crfcon = 0,&
&crfcal = 1&
&)
!
!     Dispersion function options
!
parameter (cds1fu = 1,&
&cds2fu = 2,&
&cdsthh = 3,&
&cdsemp = 4&
&)
!
!     Dispersion function types
!
parameter (cdsftm = 1,&
&cdsfpl = 2&
&)
!
!     Table period indicators
!
parameter (ctbnpf = 0,&
&ctbpfu = 1&
&)
!
!     Table interpolation options
!
parameter (ctbico = 0,&
&ctbidi = 1&
&)
!
!     Values for epsequ (delta check)
!
parameter (cdchk = 1.E-26 ,&
&cdval = 1.E-25&
&)
