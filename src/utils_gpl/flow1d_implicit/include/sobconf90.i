!     Assign values to constants
!
!
!     grid cells types
!
      parameter  cgrdcl = 1
      parameter  cstrcl = 2
!
!     node types
!
      parameter  cintnd = 1
      parameter  chbou  = 2
      parameter  cqbou  = 3
      parameter  cqhbou = 4
      parameter  chqbou = 5
      
!
!     Fixed layer constants
!
      parameter  cnlayd = 0
      parameter  cylayd = 1
      
!
!     Type of reduction function for fixed layers
!
      parameter  crdstr = 1
      parameter  crdsin = 2
      
!
!     exchange type for water quality
!
      parameter  cexigp = 1
      parameter  cexbgp = 2
      parameter  cexind = 3
      parameter  cexqlt = 4
      parameter  cexbsc = 5
      
!
!     pointer types
!
      parameter  cpopnt = 1
      parameter  cnepnt = -1
      
!
!     section types
!
      parameter  cnopar = 0
      parameter  cmainc = 1
      parameter  csubs1 = 2
      parameter  csubs2 = 3
      
!
!     Morphology adaption options
!
      parameter  ceqows = 1
      parameter  cprodp = 2
      
!
!     cross section types
!
      parameter  ccrtab = 1
      parameter  ccrcir = 2
      parameter  ccrsed = 3
      
!
!     sections types
!
      parameter  c1sec  = 0
      parameter  c2sec  = 1
      parameter  c3sec  = 2
      
!
!     Slot disabled or enabled
!
      parameter  csldis = 0
      parameter  cslena = 1
      
!
!     Structure types
!
      parameter  csweir = 1
      parameter  caweir = 2
      parameter  cdtbst = 3
      parameter  cpump  = 4
      parameter  cgenst = 5
!     Wendy structures added
      parameter  cclvrt = 6
      parameter  cslund = 7
      parameter  cflume = 8
      parameter  cculpr = 9
      parameter  cslubo = 10
      parameter  csovun = 11
      parameter  cbridg = 12
      parameter  cabutm = 13
      
!
!     Place of structure
!
      parameter  cstbra = 1
      parameter  cstlat = 2
      
!
!     trigger types
!
      parameter  cnotrg = 0
      parameter  ctitrg = 1
      parameter  chytrg = 2
      parameter  candor = 3
      
!
!     Control directions for pump
!
      parameter  cpmpup = -1
      parameter  cpmpdw = 1
      
!
!     controller types
!
      parameter  cnocon = 0
      parameter  ctimcn = 1
      parameter  chydcn = 2
      parameter  cintcn = 3
      parameter  cpidcn = 4
      
!
!     Time controller
!
      parameter  ctmabs = 0
      parameter  ctmrl0 = 1
      parameter  ctmrlv = 2
      
!
!     control parameter types
!
      parameter  ccpcrh = 1
      parameter  ccpcrw = 2
      parameter  ccpgat = 3
      
!
!     controlled variables
!
      parameter  cconh  = 1
      parameter  cconq  = 2
      parameter  cconhd = 3
      parameter  cconu  = 4
      parameter  cconsd = 5
      parameter  cconpd = 6
      
!
!     Interval types
!
      parameter  cintfx = 1
      parameter  cintvr = 2
      
!
!     Deadband types
!
      parameter  cdbdfx = 1
      parameter  cdbdpq = 2
      
!
!     Trigger off trigger on
!
      parameter  ctrgof = 0
      parameter  ctrgon = 1
!
!     Hydraulic trigger types
!
      parameter  ctrgh  = 1
      parameter  ctrghd = 2
      parameter  ctrgdi = 3
      parameter  ctrggh = 4
      parameter  ctrgch = 5
      parameter  ctrgcw = 6
      parameter  ctrghlat = 7
      parameter  ctrgpd = 8
!
!     Trigger or trigger and
!
      parameter  ctrgor = 0
      parameter  ctrgan = 1
!
!     Dispersion for structures
!
      parameter  cdsequ = 1
      parameter  cdsdf1 = 2
      parameter  cdsdf2 = 3
!
!     Friction types
!
      parameter  cfrchc = 1
      parameter  cfrchq = 2
      parameter  cfrchh = 3
      parameter  cfrman = 4
      parameter  cfrskn = 5
      parameter  cfrsks = 6
      parameter  cfrnik = 7
      parameter  cfreng = 8
!
!     Wind disabled enabled
!
      parameter  cnwndf = 0
      parameter  cywndf = 1
!
!     boundary function types
!
      parameter  cbftim = 1
      parameter  cbfhoq = 2
      parameter  cbfour = 3
      parameter  cbtidl = 4
!
!     Lateral discharge types
!
      parameter  cqlftm = 1
      parameter  cqlfh  = 2
      parameter  cqlfst = 3
      parameter  cqlcon = 4
      parameter  cqlret = 5
!
!     Lateral discharge location types
!
      parameter  cpd1gp = 1
      parameter  cpd1gc = 2
      parameter  ctd1gc = 3
      parameter  ctdmgc = 4
!
!     Salt boundary types
!
      parameter  csbusr = 1
      parameter  csbthh = 2
      parameter  csbflx = 3
!
!     Morphology boundary types
!
      parameter  cmbsft = 1
      parameter  cmbsfq = 2
      parameter  cmbzft = 3
!
!     Sediment distribution functions
!
      parameter  cdbpro = 1
      parameter  cdblin = 2
      parameter  cdbrat = 3
      parameter  cdbpow = 4
!
!     Sediment transport formulas
!
      parameter  ctrfeh = 1
      parameter  ctrfmm = 2
      parameter  ctrfaw = 3
      parameter  ctrfvr = 4
      parameter  ctrfpk = 5
      parameter  ctrfud = 6
!
!     Ripple factor constant/calculation
!
      parameter  crfcon = 0
      parameter  crfcal = 1
!
!     Dispersion function options
!
      parameter  cds1fu = 1
      parameter  cds2fu = 2
      parameter  cdsthh = 3
      parameter  cdsemp = 4
!
!     Dispersion function types
!
      parameter  cdsftm = 1
      parameter  cdsfpl = 2
!
!     Table period indicators
!
      parameter  ctbnpf = 0
      parameter  ctbpfu = 1
!
!     Table interpolation options
!
      parameter  ctbico = 0
      parameter  ctbidi = 1
!
!     Values for epsequ (delta check)
!
      parameter  cdchk = 1.E-26 
      parameter  cdval = 1.E-25
      
