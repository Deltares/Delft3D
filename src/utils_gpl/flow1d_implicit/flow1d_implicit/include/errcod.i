!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling system
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          SOBEK
!
! Programmer:         S.L. van der Woude
!
! Module:             error codes
!
! Module description: common definition of the error codes
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
!c
!***********************************************************************
! CVS log information:
!
! $Id: errcod.i,v 1.16 1999/06/01 13:42:23 kuipe_j Exp $
!
! History:
! $Log: errcod.i,v $
! Revision 1.16  1999/06/01  13:42:23  kuipe_j
! names in messages substituted & message template
!
! Revision 1.15  1999/03/15  15:47:09  kuipe_j
! Improve Froude file
!
! Revision 1.14  1998/06/08  13:15:17  kuipe_j
! time lag hydr controller
!
! Revision 1.13  1998/02/25  12:48:53  kuipe_j
! Check on grain size added
!
! Revision 1.12  1997/11/04  14:17:37  kuipe_j
! Retention basin
!
! Revision 1.11  1997/05/26  07:45:42  kuipe_j
! Small changes
!
! Revision 1.10  1997/01/23  08:29:25  kuipe_j
! Make flow module robust
!
! Revision 1.9  1996/11/01  11:25:36  kuipe_j
! Update of Delwaq input file added
!
! Revision 1.8  1996/10/31  10:31:50  kuipe_j
! Extra resistance finished
!
! Revision 1.7  1996/09/03  14:54:15  kuipe_j
! frequency time hist,etc
!
! Revision 1.6  1996/05/28  13:36:19  kuipe_j
! Linearization =  Solution
!
! Revision 1.5  1996/04/11  08:24:03  kuipe_j
! Kalman module added
!
! Revision 1.4  1996/03/07  10:44:01  kuipe_j
! Bottom scema acc. to Lax Wendroff with flux limitter
!
! Revision 1.3  1995/09/22  10:02:57  kuipe_j
! variable dimensions, new headers
!
! Revision 1.2  1995/05/30  07:02:23  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:08:27  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/12/05  13:49:19  kuipe_j
! Release 0.06
!
! Revision 1.2  1994/02/10  09:14:47  kuipe_j
! Some minor changes for beta release
!
! Revision 1.1  1993/11/26  15:31:58  kuipe_j
! Update after finishing Sobeksel.
!
!
!
!***********************************************************************
!
!     Definition of the individual errors
!
      integer    ok
      integer    info
      integer    warnng
      integer    fatal
!
!     ------------------------------------------------------
!     Codes main module
!     ------------------------------------------------------
!                          Inquire first group failed
      integer    einqfg
!                          Inquire next group failed
      integer    einqng
!                          Error closing definition file
      integer    eclsdf
!                          Error closing data file
      integer    eclsdt
!                          Inquire group failure
      integer    einqgr
!                          Inquire cell failure
      integer    einqcl
!                          Inquire element failure
      integer    einqel
!                          Get element failure
      integer    egetel
!                          Error opening definition file
      integer    eopndf
!                          Error opening data file
      integer    eopndt
!                          Error writing release no
      integer    ewrrel
!                          Unknown element type
      integer    eelmtp
!                          Maximum Froude number in cell
      integer    efroce
!                          Time and location of maximum Froude number
      integer    efrotl
!                          Maximum Froude number in grid point
      integer    efrogr
!                          No convergence-criterion left
      integer    eflncc
!                          Too many iterations for bisection
      integer    emrbis
!                          Variable already declared
      integer    evrdec
!                          Running out of data space
      integer    eoutds
!                          Running out of name space
      integer    eoutns
!                          Flow not convergating
      integer    eflncv
!                          Morphology not convergating
      integer    emrncv
!                          Morphological time step < 1 FP
      integer    emtsfp
!                          Reducing morphological time step
      integer    erdmst
!                          Too many messages
      integer    errovf
!
!     ------------------------------------------------------
!     Codes flow module
!     ------------------------------------------------------
!                          Water level < bottom (sedredge branch)
      integer    eflhse
!                          Unexpected structure type for Qlat
      integer    eflstr
!                          Flow nodal matrix singular
      integer    eflmat
!                          Error creating water quality file
      integer    ewqcre
!                          Error appending water quality file
      integer    ewqapp
!                          Error writing time info to water quality file
      integer    ewqtim
!                          Error writing MAP info
      integer    eflmap
!                          Error writing HIS info
      integer    eflhis
!                          Error writing begin of output
      integer    eflboo
!                          Error writing end of output
      integer    efleoo
!                          Error writing restart data
      integer    eflwrd
!                          Error reading restart data
      integer    eflrrd
!                          Error writing begin of restart
      integer    eflbor
!                          Can not find restart time step
      integer    eflrtt
!                          Water level < bottom (circel branch)
      integer    eflhci
!                          Messages at time step
      integer    eflmes
!                          Extra resistance undefined
      integer    eflexr
!                          Water level < bottom
      integer    eflcr0
!                          Discharge not zero at sill      
      integer    efllim
!                          Outside data base
      integer    eflhbo
!                          Negative gate opening in structure
      integer    eflngo
!                          Negative crest width in structure
      integer    eflncw
!                          Control parameter controlled twice
      integer    eflcpt
!                          Coordinate not in branch
      integer    eflnib
!                          Froude number higher than 1. (Super-
!                          critical flow)
      integer    eflfro
!                          High Froude numbers (higher than 0.8)
      integer    eflhfn
!                          Timesteps continued without convergence
      integer    efltwc
!                          Minimum number of iterations during
!                          simulation
      integer    eflmii
!                          Maximum number of iterations during
!                          simulation
      integer    eflmxi
!                          Mean number of iterations during
!                          simulation
      integer    eflmni
!                          Roughnes or hydraulic radius out of limit
      integer    eflcrl
!                          Retention area defined at grid point
      integer    eflqlt
!                          Grain size d90 too small
      integer    eflgrn
!                          Size of buffer for discharges changed
      integer    efllag
!                          Area general structure > area branch
      integer    eflgsa
!                          Change of controlled parameter after restart
      integer    eflcha
!                          Roughness limited in main
      integer    eflrou
!                          Roughness limited in floodplains
      integer    eflrof
!                          Array grhis out of bounds
      integer    eflgrh      
!                          Time Slice start time inconsistency
      integer    efltsi
!     ------------------------------------------------------
!     Codes salt module
!     ------------------------------------------------------
!                          Salt concentration must be > 0
      integer    esacon
!                          Flood vol or flood vel < 0
      integer    esafvv
!                          Maximum no of filter steps executed
      integer    esafil
!                          Error calculating density
      integer    esaden
!                          Nodal matrix singular
      integer    esamat
!                          Salt messages at time step
      integer    esames
!                          Error writing MAP info
      integer    esamap
!                          Error writing HIS info
      integer    esahis
!                          Error writing begin of output
      integer    esaboo
!                          Error writing end of output
      integer    esaeoo
!                          Error writing restart data
      integer    esawrd
!                          Error reading restart data
      integer    esarrd
!                          Error writing begin of restart
      integer    esabor
!                          Can not find restart time step
      integer    esartt
!                          No mouth defined for branch
      integer    esabrm
!
!     ------------------------------------------------------
!     Codes sediment module
!     ------------------------------------------------------
!                          Froude number too large
      integer    esefro
!                          Discharges either pos or neg
      integer    eseqdr
!                          Sediment messages at time step
      integer    esemes
!                          Error writing begin of output
      integer    eseboo
!                          Error writing MAP info
      integer    esemap
!                          Error writing HIS info
      integer    esehis
!                          Error writing end of output
      integer    eseeoo
!                          Grain sizes too small
      integer    esegrn
!                          Froude number > .8
      integer    esefrw

!     ------------------------------------------------------
!     Codes morphology module
!     ------------------------------------------------------
!                          Inflowing boundary has no condition
      integer    emobou
!                          Levels not in increasing order
      integer    emolev
!                          Water level < bottom
      integer    emohbo
!                          Cross section level info
      integer    emocrl
!                          Messages at time step
      integer    emomes
!                          Error writing MAP info
      integer    emomap
!                          Error writing HIS info
      integer    emohis
!                          Error writing begin of output
      integer    emoboo
!                          Error writing end of output
      integer    emoeoo
!                          Error writing restart data
      integer    emowrd
!                          Error reading restart data
      integer    emorrd
!                          Error writing begin of restart
      integer    emobor
!                          Can not find restart time step
      integer    emortt
!                          Error in formula for Integral
      integer    emofor
!                          Message maximum courant number
      integer    emcour
!                          Begin coordinate not found
      integer    embeco
!                          End coordinate not found
      integer    emenco

!     ------------------------------------------------------
!     Codes water quality interface module
!     ------------------------------------------------------
!                          Error opening definition file
      integer    ewqodf
!                          Error opening data file
      integer    ewqodt
!                          Error reading element name
      integer    ewqelm
!                          Error in segment definition
      integer    ewqseg
!                          Cannot open input file
      integer    ewqopn
!                          Cannot modify input file
      integer    ewqmod

!     ------------------------------------------------------
!     Codes water graded sediment module
!     ------------------------------------------------------

!                          Downward shift of layers not allowed  
      integer    egfixs1
!                          Downward shift info
      integer    egfixs2
!                          Too low top level of multi under layer 
      integer    egsubtp
!                          Fixeded layer specified at top of multi under layer
      integer    egfixtp
!                          Too high bottom of sublayer
      integer    egsubbt 
!                          Too much layers required 
      integer    egsubtm
!                          Sum of supplied frequencies not equal 1
      integer    egfreq
!                          Incorrect underlayer level or too less layers
      integer    egunla
!                          Transport layer is empty
      integer    egnorm1 
!                          Exchange layer is empty
      integer    egnorm2 
!                          Graded sediment restart
      integer    egrest
!                          Initialization error
      integer    eginit
!                          No sediment width
      integer    egsedw
!                          Too many output functions 
      integer    egoutm 
!                          Can not find restart time step
      integer    egsrtt
!                          Error writing restart data
      integer    egswrd
!                          Error reading restart data
      integer    egsrrd
!                          Error writing begin of restart
      integer    egsbor

!     ------------------------------------------------------
!     Codes cross sectional table module
!     ------------------------------------------------------
!                          Error creating preissmann slot
      integer    ecsslt
!                          Error Nikuradse-value is too high
!                          due to adapting slot
      integer    ecsnik

!     ------------------------------------------------------
!     Codes Kalman module
!     ------------------------------------------------------
!                          Nodal administration matrix singular
      integer    ekamat
!                          Error writing MAP info
      integer    ekamap
!                          Error writing HIS info
      integer    ekahis
!                          Error writing begin of output
      integer    ekaboo
!                          Error writing end of output
      integer    ekaeoo
!                          Error writing restart data
      integer    ekawrd
!                          Error reading restart data
      integer    ekarrd
!                          Error writing begin of restart
      integer    ekabor
!                          Can not find restart time step
      integer    ekartt
!                          Messages at time step
      integer    ekames      

!     ------------------------------------------------------
!     Codes Tidel Analyses
!     ------------------------------------------------------
      integer    edeall                ! Deallocate error      
      integer    ealloc                ! Allocate error 
      integer    egales                ! Too less data 

!     ------------------------------------------------------
!     Assign a unique code for each error
!     ------------------------------------------------------
      parameter (ok     =     0,                                                        &
     &           info   =     1,                                                        &
     &           warnng =     2,                                                        &
     &           fatal  =     3)

!     ------------------------------------------------------
!     Codes main module
!     ------------------------------------------------------
      parameter (einqfg = 00001,                                                        &
     &           einqng = 00002,                                                        &
     &           eclsdf = 00003,                                                        &
     &           eclsdt = 00004,                                                        &
     &           einqgr = 00005,                                                        &
     &           einqcl = 00006,                                                        &
     &           einqel = 00007,                                                        &
     &           egetel = 00008,                                                        &
     &           eopndf = 00009,                                                        &
     &           eopndt = 00010)
     
      parameter (ewrrel = 00011,                                                        &
     &           eelmtp = 00012,                                                        &
     &           efroce = 00013,                                                        &
     &           efrotl = 00014,                                                        &
     &           efrogr = 00015,                                                        &
     &           eflncc = 00016,                                                        &
     &           emrbis = 00017,                                                        &
     &           evrdec = 00020,                                                        &
     &           eoutds = 00021,                                                        &
     &           eoutns = 00022,                                                        &
     &           errovf = 00023,                                                        &
     &           eflncv = 00030,                                                        &
     &           emrncv = 00031,                                                        &
     &           emtsfp = 00032,                                                        &
     &           erdmst = 00033)
!
!     ------------------------------------------------------
!     Codes flow module
!     ------------------------------------------------------
      parameter (eflhse = 01001,                                                        &
     &           eflstr = 01002,                                                        &
     &           eflmat = 01003,                                                        &
     &           ewqcre = 01004,                                                        &
     &           ewqapp = 01005,                                                        &
     &           ewqtim = 01006,                                                        &
     &           eflmap = 01007,                                                        &
     &           eflhis = 01008,                                                        &
     &           eflboo = 01009,                                                        &
     &           efleoo = 01010,                                                        &
     &           eflwrd = 01011,                                                        &
     &           eflrrd = 01012,                                                        &
     &           eflbor = 01013,                                                        &
     &           eflrtt = 01014,                                                        &
     &           eflhci = 01015,                                                        &
     &           eflmes = 01016,                                                        &
     &           eflexr = 01017,                                                        &
     &           eflcr0 = 01018,                                                        &
     &           efllim = 01019)
     
      parameter (eflhbo = 01025,                                                        &
     &           eflngo = 01026,                                                        &
     &           eflncw = 01027,                                                        &
     &           eflcpt = 01028,                                                        &
     &           eflnib = 01029,                                                        &
     &           eflfro = 01030,                                                        &
     &           eflhfn = 01031,                                                        &
     &           efltwc = 01032,                                                        &
     &           eflmii = 01033,                                                        &
     &           eflmxi = 01034,                                                        &
     &           eflmni = 01035,                                                        &
     &           eflcrl = 01036,                                                        &
     &           eflqlt = 01037,                                                        &
     &           eflgrn = 01038,                                                        &
     &           efllag = 01039,                                                        &
     &           eflgsa = 01040,                                                        &
     &           eflcha = 01041,                                                        &
     &           eflrou = 01042,                                                        &
     &           eflrof = 01043,                                                        &
     &           eflgrh = 01044,                                                        &
     &           efltsi = 01045)

!     ------------------------------------------------------
!     Codes salt module
!     ------------------------------------------------------
      parameter (esacon = 02001,                                                        &
     &           esafvv = 02002,                                                        &
     &           esafil = 02003,                                                        &
     &           esaden = 02004,                                                        &
     &           esamat = 02005,                                                        &
     &           esames = 02006,                                                        &
     &           esamap = 02007,                                                        &
     &           esahis = 02008,                                                        &
     &           esaboo = 02009,                                                        &
     &           esaeoo = 02010,                                                        &
     &           esawrd = 02011,                                                        &
     &           esarrd = 02012,                                                        &
     &           esabor = 02013,                                                        &
     &           esartt = 02014,                                                        &
     &           esabrm = 02015)

!     ------------------------------------------------------
!     Codes sediment module
!     ------------------------------------------------------
      parameter (esefro = 03001,                                                        &
     &           eseqdr = 03002,                                                        &
     &           esemes = 03003,                                                        &
     &           eseboo = 03004,                                                        &
     &           esemap = 03005,                                                        &
     &           esehis = 03006,                                                        &
     &           eseeoo = 03007,                                                        &
     &           esegrn = 03008,                                                        &
     &           esefrw = 03009)

!     ------------------------------------------------------
!     Codes morphology module
!     ------------------------------------------------------
      parameter (emobou = 04001,                                                        &
     &           emolev = 04002,                                                        &
     &           emohbo = 04003,                                                        &
     &           emocrl = 04004,                                                        &
     &           emomes = 04006,                                                        &
     &           emomap = 04007,                                                        &
     &           emohis = 04008,                                                        &
     &           emoboo = 04009,                                                        &
     &           emoeoo = 04010,                                                        &
     &           emowrd = 04011,                                                        &
     &           emorrd = 04012,                                                        &
     &           emobor = 04013,                                                        &
     &           emortt = 04014,                                                        &
     &           emofor = 04015,                                                        &
     &           emcour = 04016,                                                        &
     &           embeco = 04017,                                                        &
     &           emenco = 04018)

!     ------------------------------------------------------
!     Codes water quality interface module
!     ------------------------------------------------------
      parameter (ewqodf = 05001,                                                        &
     &           ewqodt = 05002,                                                        &
     &           ewqelm = 05003,                                                        &
     &           ewqseg = 05004,                                                        &
     &           ewqopn = 05005,                                                        &
     &           ewqmod = 05006)
!     ------------------------------------------------------
!     Codes water graded sediment module
!     ------------------------------------------------------
      parameter (egfixs1 = 06001,                                                        &
     &           egfixs2 = 06002,                                                        &
     &           egsubtp = 06003,                                                        &
     &           egfixtp = 06004,                                                        &
     &           egsubbt = 06005,                                                        &
     &           egsubtm = 06006,                                                        &
     &           egfreq  = 06007,                                                        &
     &           egunla  = 06008,                                                        &
     &           egnorm1 = 06009,                                                        &
     &           egnorm2 = 06010,                                                        &
     &           egrest  = 06011,                                                        &
     &           eginit  = 06012,                                                        &
     &           egsedw  = 06013,                                                        & 
     &           egoutm  = 06014,                                                        &
     &           egsrtt  = 06015,                                                        &     
     &           egswrd  = 06016,                                                        &
     &           egsrrd  = 06017,                                                        &
     &           egsbor  = 06018) 
!     ------------------------------------------------------
!     Codes cross sectional table module
!     ------------------------------------------------------
      parameter (ecsslt = 07001,                                                        &
     &           ecsnik = 07002)

!     ------------------------------------------------------
!     Codes Kalman module
!     ------------------------------------------------------
      parameter (ekamat = 08001,                                                        &
     &           ekamap = 08002,                                                        &
     &           ekahis = 08003,                                                        &
     &           ekaboo = 08004,                                                        &
     &           ekaeoo = 08005,                                                        &
     &           ekawrd = 08006,                                                        &
     &           ekarrd = 08007,                                                        &
     &           ekabor = 08008,                                                        &
     &           ekartt = 08009,                                                        &
     &           ekames = 08010)
!     ------------------------------------------------------
!     Codes Tidel Analyses
!     ------------------------------------------------------
      parameter (edeall = 09001,                                                        &
     &           ealloc = 09002,                                                        &
     &           egales = 09003)
