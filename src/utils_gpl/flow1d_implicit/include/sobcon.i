c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling system
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          SOBEK
c
c Programmer:         S.L. van der Woude
c
c Module:             Sobek constants
c
c Module description: Include file with sobek constants
c
c
c
c Pre condition:
c
c Post condition:
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c=======================================================================

c
c***********************************************************************
c CVS log information:
c
c $Id: sobcon.i,v 1.9 1999/03/15 14:08:22 kuipe_j Exp $
c
c History:
c $Log: sobcon.i,v $
c Revision 1.9  1999/03/15  14:08:22  kuipe_j
c devided segmentes added
c
c Revision 1.8  1998/05/25  19:16:45  kuipe_j
c Wendy structures
c
c Removed on 8-6-96
c
c***********************************************************************
c
c     Definition of the individual constants
c
c
c                          Normal gridpoint
      integer    cgrdcl
c                          Structure cell
      integer    cstrcl
c                          Internal node
      integer    cintnd
c                          H-boundary
      integer    chbou
c                          Q-boundary
      integer    cqbou
c                          QH-boundary
      integer    cqhbou
c                          HQ-boundary
      integer    chqbou
c                          No layer defined
      integer    cnlayd
c                          Layer defined
      integer    cylayd
c                          Reduction function straight
      integer    crdstr
c                          Sinus reduction function
      integer    crdsin
c                          Exchange in gridpoint
      integer    cexigp
c                          Exchange between gridpoint
      integer    cexbgp
c                          Exchange in node
      integer    cexind
c                          Exchange from Qlat to segment
      integer    cexqlt
c                          Exchange in seperated segment
      integer    cexbsc
c                          Positive pointer
      integer    cpopnt
c                          Negative pointer
      integer    cnepnt
c                          No parallel sections
      integer    cnopar
c                          Main channel
      integer    cmainc
c                          Sub section 1
      integer    csubs1
c                          Sub section 2
      integer    csubs2
c                          Equally over actual ws
      integer    ceqows
c                          Proportional to local depth
      integer    cprodp
c                          Tabulated cross section
      integer    ccrtab
c                          Circel cross section
      integer    ccrcir
c                          Sedredge cross section
      integer    ccrsed
c                          Only main channel
      integer    c1sec
c                          Main channel + sub section 1
      integer    c2sec
c                          Main channel + sub section 1 and 2
      integer    c3sec
c                          Disable slot generation
      integer    csldis
c                          Enable slot generation
      integer    cslena
c                          Simple weir
      integer    csweir
c                          Advanced weir
      integer    caweir
c                          Data base structure
      integer    cdtbst
c                          Pump
      integer    cpump
c                          General structure
      integer    cgenst
c                          Culvert
      integer    cclvrt
c                          Sluice with underflow gate
      integer    cslund
c                          Open Flume
      integer    cflume
c                          Gated culvert with pressure flow
      integer    cculpr
c                          Sluice with bottom hinged gate
      integer    cslubo
c                          Sluice with overflow/underflow gate
      integer    csovun
c                          Bridge piers
      integer    cbridg
c                          Abutment
      integer    cabutm
c                          Structure in branch
      integer    cstbra
c                          Lateral structure
      integer    cstlat
c                          Not controlled
      integer    cnocon
c                          No trigger
      integer    cnotrg
c                          Time trigger
      integer    ctitrg
c                          Hydraulic trigger
      integer    chytrg
c                          Hydraulic and/or timetrigger
      integer    candor
c                          Pump controlled upward
      integer    cpmpup
c                          Pump controlled downward
      integer    cpmpdw
c                          Time controller
      integer    ctimcn
c                          Time controller absolute times
      integer    ctmabs
c                          Time controller relative times zero
      integer    ctmrl0
c                          Time controller relative times value
      integer    ctmrlv
c                          PID controller
      integer    cpidcn
c                          Hydraulic controller
      integer    chydcn
c                          Interval controller
      integer    cintcn
c                          Control parameter: Crest height
      integer    ccpcrh
c                          Control parameter: Crest width
      integer    ccpcrw
c                          Control parameter: Gate opening heigth
      integer    ccpgat
c                          Water level controlled
      integer    cconh
c                          Discharge controlled
      integer    cconq
c                          Head difference controlled
      integer    cconhd
c                          Velocity controlled
      integer    cconu
c                          Stream direction
      integer    cconsd
c                          Pressure difference
      integer    cconpd
c                          Fixed interval
      integer    cintfx
c                          Variable interval
      integer    cintvr
c                          Fixed deadband
      integer    cdbdfx
c                          Deadband as percentage of discharge
      integer    cdbdpq
c                          Off trigger as function of time
      integer    ctrgof
c                          On trigger as function of time
      integer    ctrgon
c                          Trigger on water level
      integer    ctrgh
c                          Trigger on head difference
      integer    ctrghd
c                          Trigger on discharge
      integer    ctrgdi
c                          Trigger on gate height
      integer    ctrggh
c                          Trigger on crest height
      integer    ctrgch
c                          Trigger on crest width
      integer    ctrgcw
c                          Trigger on level of retention basin
      integer    ctrghlat
c                          Trigger on pressure difference
      integer    ctrgpd
c                          Or trigger
      integer    ctrgor
c                          And trigger
      integer    ctrgan
c                          Equal dispersion of structure
      integer    cdsequ
c                          Different dispersion of structure (1)
      integer    cdsdf1
c                          Different dispersion of structure (2)
      integer    cdsdf2
c                          Chezy value constant
      integer    cfrchc
c                          Chezy value function of Q
      integer    cfrchq
c                          Chezy value function of h
      integer    cfrchh
c                          Manning
      integer    cfrman
c                          Strickler-1  (Kn)
      integer    cfrskn
c                          Strickler-2  (Ks)
      integer    cfrsks
c                          Nikuradze
      integer    cfrnik
c                          Engelund predictor
      integer    cfreng
c                          No wind friction defined
      integer    cnwndf
c                          Wind friction defined
      integer    cywndf
c                          boundary = function of time
      integer    cbftim
c                          boundary = function of h or Q
      integer    cbfhoq
c                          boundary = fourier function
      integer    cbfour
c                          boundary = tidal components
      integer    cbtidl
c                          Qlat     = function of time
      integer    cqlftm
c                          Qlat     = function of water level
      integer    cqlfh
c                          Qlat     = function of structure
      integer    cqlfst
c                          Qlat     = function of connected station
      integer    cqlret
c                          Qlat     = function of retentiebekken
      integer    cqlcon
c                          Point discharge in 1 gridpoint
      integer    cpd1gp
c                          Point discharge in 1 gridcell
      integer    cpd1gc
c                          Traject discharge in 1 gridcell
      integer    ctd1gc
c                          Traject discharge in more gridcells
      integer    ctdmgc
c                          Salt boundary user specified
      integer    csbusr
c                          Salt boundary Thatcher-Harleman
      integer    csbthh
c                          Salt boundary Zero-flux
      integer    csbflx
c                          Morphological boundary S=f(t)
      integer    cmbsft
c                          Morphological boundary S=f(Q)
      integer    cmbsfq
c                          Morphological boundary z=f(t)
      integer    cmbzft
c                          Proportional distribution function
      integer    cdbpro
c                          Lineair distribution function
      integer    cdblin
c                          Ratio distribution function
      integer    cdbrat
c                          Power distribution function
      integer    cdbpow
c                          Engelund & Hansen transport formula
      integer    ctrfeh
c                          Meyer-Peter & Muller transport formula
      integer    ctrfmm
c                          Ackers & White transport formula
      integer    ctrfaw
c                          Van Rijn transport formula
      integer    ctrfvr
c                          Parker & Klingemann transport formula
      integer    ctrfpk
c                          User defined transport formula
      integer    ctrfud
c                          Ripple factor is constant
      integer    crfcon
c                          Ripple factor must be calculated
      integer    crfcal
c                          Dispersion function 1
      integer    cds1fu
c                          Dispersion function 2
      integer    cds2fu
c                          Dispersion Thatcher-Harleman
      integer    cdsthh
c                          Dispersion Empirical
      integer    cdsemp
c                          Dispersion is function of time
      integer    cdsftm
c                          Dispersion is function of place
      integer    cdsfpl
c                          No periodical function
      integer    ctbnpf
c                          Periodical function
      integer    ctbpfu
c                          Table Interpolate continuously
      integer    ctbico
c                          Table Interpolate discrete
      integer    ctbidi
c                          Delta check value
      real       cdchk
c                          Value to add or distract to avoid devision
c                          by zero
      real       cdval
c
c     Assign values to constants
c
c
c     grid cells types
c
      parameter (cgrdcl = 1,
     +           cstrcl = 2
     +          )
c
c     node types
c
      parameter (cintnd = 1,
     +           chbou  = 2,
     +           cqbou  = 3,
     +           cqhbou = 4,
     +           chqbou = 5
     +          )
c
c     Fixed layer constants
c
      parameter (cnlayd = 0,
     +           cylayd = 1
     +          )
c
c     Type of reduction function for fixed layers
c
      parameter (crdstr = 1,
     +           crdsin = 2
     +          )
c
c     exchange type for water quality
c
      parameter (cexigp = 1,
     +           cexbgp = 2,
     +           cexind = 3,
     +           cexqlt = 4,
     +           cexbsc = 5
     +          )
c
c     pointer types
c
      parameter (cpopnt = 1,
     +           cnepnt = -1
     +          )
c
c     section types
c
      parameter (cnopar = 0,
     +           cmainc = 1,
     +           csubs1 = 2,
     +           csubs2 = 3
     +          )
c
c     Morphology adaption options
c
      parameter (ceqows = 1,
     +           cprodp = 2
     +          )
c
c     cross section types
c
      parameter (ccrtab = 1,
     +           ccrcir = 2,
     +           ccrsed = 3
     +          )
c
c     sections types
c
      parameter (c1sec  = 0,
     +           c2sec  = 1,
     +           c3sec  = 2
     +          )
c
c     Slot disabled or enabled
c
      parameter (csldis = 0,
     +           cslena = 1
     +          )
c
c     Structure types
c
      parameter (csweir = 1,
     +           caweir = 2,
     +           cdtbst = 3,
     +           cpump  = 4,
     +           cgenst = 5,
c     Wendy structures added
     +           cclvrt = 6,
     +           cslund = 7,
     +           cflume = 8,
     +           cculpr = 9,
     +           cslubo = 10,
     +           csovun = 11,
     +           cbridg = 12,
     +           cabutm = 13
     +          )
c
c     Place of structure
c
      parameter (cstbra = 1,
     +           cstlat = 2
     +          )
c
c     trigger types
c
      parameter (cnotrg = 0,
     +           ctitrg = 1,
     +           chytrg = 2,
     +           candor = 3
     +          )
c
c     Control directions for pump
c
      parameter (cpmpup = -1,
     +           cpmpdw = 1
     +          )
c
c     controller types
c
      parameter (cnocon = 0,
     +           ctimcn = 1,
     +           chydcn = 2,
     +           cintcn = 3,
     +           cpidcn = 4
     +          )
c
c     Time controller
c
      parameter (ctmabs = 0,
     +           ctmrl0 = 1,
     +           ctmrlv = 2
     +          )
c
c     control parameter types
c
      parameter (ccpcrh = 1,
     +           ccpcrw = 2,
     +           ccpgat = 3
     +          )
c
c     controlled variables
c
      parameter (cconh  = 1,
     +           cconq  = 2,
     +           cconhd = 3,
     +           cconu  = 4,
     +           cconsd = 5,
     +           cconpd = 6
     +          )
c
c     Interval types
c
      parameter (cintfx = 1,
     +           cintvr = 2
     +          )
c
c     Deadband types
c
      parameter (cdbdfx = 1,
     +           cdbdpq = 2
     +          )
c
c     Trigger off, trigger on
c
      parameter (ctrgof = 0,
     +           ctrgon = 1
     +          )
c
c     Hydraulic trigger types
c
      parameter (ctrgh  = 1,
     +           ctrghd = 2,
     +           ctrgdi = 3,
     +           ctrggh = 4,
     +           ctrgch = 5,
     +           ctrgcw = 6,
     +           ctrghlat = 7,
     +           ctrgpd = 8
     +          )
c
c     Trigger or, trigger and
c
      parameter (ctrgor = 0,
     +           ctrgan = 1
     +          )
c
c     Dispersion for structures
c
      parameter (cdsequ = 1,
     +           cdsdf1 = 2,
     +           cdsdf2 = 3
     +          )
c
c     Friction types
c
      parameter (cfrchc = 1,
     +           cfrchq = 2,
     +           cfrchh = 3,
     +           cfrman = 4,
     +           cfrskn = 5,
     +           cfrsks = 6,
     +           cfrnik = 7,
     +           cfreng = 8
     +          )
c
c     Wind disabled, enabled
c
      parameter (cnwndf = 0,
     +           cywndf = 1
     +          )
c
c     boundary function types
c
      parameter (cbftim = 1,
     +           cbfhoq = 2,
     +           cbfour = 3,
     +           cbtidl = 4
     +          )
c
c     Lateral discharge types
c
      parameter (cqlftm = 1,
     +           cqlfh  = 2,
     +           cqlfst = 3,
     +           cqlcon = 4,
     +           cqlret = 5
     +          )
c
c     Lateral discharge location types
c
      parameter (cpd1gp = 1,
     +           cpd1gc = 2,
     +           ctd1gc = 3,
     +           ctdmgc = 4
     +          )
c
c     Salt boundary types
c
      parameter (csbusr = 1,
     +           csbthh = 2,
     +           csbflx = 3
     +          )
c
c     Morphology boundary types
c
      parameter (cmbsft = 1,
     +           cmbsfq = 2,
     +           cmbzft = 3
     +          )
c
c     Sediment distribution functions
c
      parameter (cdbpro = 1,
     +           cdblin = 2,
     +           cdbrat = 3,
     +           cdbpow = 4
     +          )
c
c     Sediment transport formulas
c
      parameter (ctrfeh = 1,
     +           ctrfmm = 2,
     +           ctrfaw = 3,
     +           ctrfvr = 4,
     +           ctrfpk = 5,
     +           ctrfud = 6
     +          )
c
c     Ripple factor constant/calculation
c
      parameter (crfcon = 0,
     +           crfcal = 1
     +          )
c
c     Dispersion function options
c
      parameter (cds1fu = 1,
     +           cds2fu = 2,
     +           cdsthh = 3,
     +           cdsemp = 4
     +          )
c
c     Dispersion function types
c
      parameter (cdsftm = 1,
     +           cdsfpl = 2
     +          )
c
c     Table period indicators
c
      parameter (ctbnpf = 0,
     +           ctbpfu = 1
     +          )
c
c     Table interpolation options
c
      parameter (ctbico = 0,
     +           ctbidi = 1
     +          )
c
c     Values for epsequ (delta check)
c
      parameter (cdchk = 1.E-26 ,
     +           cdval = 1.E-25
     +          )
