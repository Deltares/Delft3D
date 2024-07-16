      subroutine SODECL (appl, lhisgp ,juer, ker)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Main module
c
c Programmer:         S.L. van der Woude
c
c Module:             SODECL (SObek DECLare variables)
c
c Module description: Declare variables for each module in application.
c
c                     After reading the model information and the type
c                     of application the variables for storage of the
c                     calculated results should be declared in the memo-
c                     ry pools.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 appl              IO Application string containing switches for
c                         simulation run
c  2 juer              P  -
c  3 ker               I  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c gtclen  GeT Character LENgth
c gtcpnt  GeT Character PoiNTer
c gtipnt  GeT Integer PoiNTer
c gtrpnt  GeT Real PoiNTer
c sodefi  SObek DEclare FIle descriptors
c sodefl  SObek DEclare FLow variables
c sodein  SObek DEclare INterface variables
c sodeka  SObek DEclare KAlman variables
c sodemo  SObek DEclare MOrphology variables
c sodesa  SObek DEclare SAlt variables
c sodese  SObek DEclare SEdiment transport variables
c sodewq  SObek DEclare Water Quality interface variables
c soipar  SObek Integer PARameter
c=======================================================================
c
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: sodecl.pf,v $
c Revision 1.15  1999/03/15  15:19:37  kuipe_j
c tabs removed
c
c Revision 1.14  1997/08/21  12:48:52  kuipe_j
c Decl for only sediment
c
c Revision 1.13  1997/06/17  11:29:17  kuipe_j
c output in history format
c
c Revision 1.12  1996/12/04  11:59:26  kuipe_j
c declarations / undefined vars
c
c Revision 1.11  1996/12/02  10:03:41  kuipe_j
c avoid negative pointers
c
c Revision 1.10  1996/04/12  13:05:57  kuipe_j
c headers, minor changes
c
c Revision 1.9  1996/04/11  08:16:23  kuipe_j
c Kalman module added
c
c Revision 1.8  1996/01/17  14:47:33  kuipe_j
c header update
c
c Revision 1.7  1996/01/16  15:01:52  kuipe_j
c Restart improvements
c
c Revision 1.6  1995/09/22  10:03:40  kuipe_j
c variable dimensions, new headers
c
c Revision 1.5  1995/09/12  08:11:28  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.4  1995/08/30  12:37:30  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.3  1995/05/30  09:56:44  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:09:36  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:12:01  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:09:31  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:39:40  kuipe_j
c Initial version
c
c
c***********************************************************************
c
      include '..\include\sobdim.i'
c
c     Parameters
c
      character*(*)   appl
      integer         juer, ker
c
c     Variables
c
      integer         arexop, nboun,  nbrnod, nbran,  ncontr, nfrac,
     +                ngrid,  ngridm, nhstat, nlayer, maxlev, nmouth,
     +                nnf,    nnmu,   nnode,  nnc   , nposeg, np,
     +                nqlat,  nqstat, nsamp,  nsedrd, nstru , nucoef 
c
      integer         dsopt,  iappl,  sappl,  lappl,  ixrun

      logical         lflow,  lsalt,  lsedt,  lmorp,  lwqin,  ldlwq,
     +                lestu,  newres, lkalm,  lgrad,  lhisgp, lgrwt

      integer         gtcpnt, gtclen, gtipnt, gtrpnt, gtrlen, soipar
      external        gtcpnt, gtclen, gtipnt, gtrpnt, gtrlen, soipar
c
c     Include memory pool
c
      include '..\include\mempool.i'
c
c     Read application definition from the string pool
c
      lappl   = gtclen ( 'APPL' )
      sappl   = gtcpnt ( 'APPL' )
      appl    = ' '
      do 100 iappl = 1, lappl
         appl (iappl:iappl) = cp ( sappl )
         sappl = sappl + 1
 100  continue
c
c     Read application switches
c
      lflow = index (appl, 'FLOW') .gt. 0
      lsalt = index (appl, 'SALT') .gt. 0
      lsedt = index (appl, 'SEDT') .gt. 0
      lmorp = index (appl, 'MORP') .gt. 0
      lwqin = index (appl, 'WQIN') .gt. 0
      ldlwq = index (appl, 'DLWQ') .gt. 0
      lestu = index (appl, 'ESTU') .gt. 0
      lkalm = index (appl, 'KALM') .gt. 0
      lgrad = index (appl, 'GRAD') .gt. 0
      lgrwt = index (appl, 'GRWT') .gt. 0


      if ((lflow) .and. (ker .eq. 0)) then
c
c        --- FLOW MODULE ---
c
         nbrnod = ip ( gtipnt ('NBRNOD' ))
         ncontr = ip ( gtipnt ('NCONTR' ))
         ngrid  = ip ( gtipnt ('NGRID'  ))
         ngridm = ip ( gtipnt ('NGRIDM' ))
         nhstat = ip ( gtipnt ('NHSTAT' ))
         maxlev = ip ( gtipnt ('MAXLEV' ))
         nnode  = ip ( gtipnt ('NNODE'  ))
         nqlat  = ip ( gtipnt ('NQLAT'  ))
         nqstat = ip ( gtipnt ('NQSTAT' ))
         nstru  = ip ( gtipnt ('NSTRU'  ))
c
         arexop = gtipnt ('AREXOP')

         call sodefl ( lsalt , lkalm , ngrid  ,ngridm ,nhstat ,
     +                 maxlev ,nnode , nbrnod ,nqlat  ,nqstat ,
     +                 nstru  ,ncontr, ip(arexop)     ,lwqin  ,
     +                 lhisgp ,lgrwt , juer   ,ker   )
      endif
c
      if ((lkalm) .and. (ker .eq. 0)) then
c
c        --- KALMAN MODULE ---
c
         ngrid  = ip ( gtipnt ('NGRID'  ))
         nqlat  = ip ( gtipnt ('NQLAT'  ))
         nstru  = ip ( gtipnt ('NSTRU'  ))
         nbran  = ip ( gtipnt ('NBRAN'  ))
         nnode  = ip ( gtipnt ('NNODE'  ))
         np     = ip ( gtipnt ('NP'     ))
         nsamp  = ip ( gtipnt ('NSAMP'  ))
         nnf    = ip ( gtipnt ('NNF'    ))
         nnmu   = ip ( gtipnt ('NNMU'   ))
         nnc    = ip ( gtipnt ('NNC'    ))

         call sodeka (ngrid  ,nqlat  ,nstru  ,nbran  ,nnode  ,np     ,
     +                nsamp  ,nnc    ,nnf    ,nnmu   ,juer   ,ker    )
      endif
c
      if ((lsalt) .and. (ker .eq. 0)) then
c
c        --- SALT MODULE ---
c
         dsopt  = ip ( gtipnt ('DSOPT'  ))
         nbran  = ip ( gtipnt ('NBRAN'  ))
         nboun  = ip ( gtipnt ('NBOUN'  ))
         ngrid  = ip ( gtipnt ('NGRID'  ))
         nmouth = ip ( gtipnt ('NMOUTH' ))

         call sodesa ( dsopt, nbran, nboun, ngrid, nmouth,
     +                 juer,  ker  )
      endif

      nsedrd = 0
      if ((lsedt) .and. (ker .eq. 0)) then
c
c        --- SEDIMENT TRANSPORT MODULE ---
c
         nbran  = ip ( gtipnt ('NBRAN'  ))
         ngrid  = ip ( gtipnt ('NGRID'  ))
         nsedrd = ip ( gtipnt ('NSEDRD' ))
         nbrnod = ip ( gtipnt ('NBRNOD' ))

         call sodese ( nbran, ngrid, nsedrd, nbrnod, juer , lmorp ,
     &                 ker  )
      endif

      if ((lmorp .or. lgrad) .and. (ker .eq. 0)) then
c
c        --- MORPHOLOGY MODULE ---
c
         nbran  = ip ( gtipnt ('NBRAN'  ))
         ngrid  = ip ( gtipnt ('NGRID'  ))

         call sodemo ( lestu, nbran, ngrid, juer, ker )
      endif

      if ((lwqin) .and. (ker .eq. 0)) then
c
c        --- VARIABLES FOR FLOW MODULE AND WATER QUALITY INTERFACE
c
         ngrid  = ip ( gtipnt ('NGRID'  ))
         nqlat  = ip ( gtipnt ('NQLAT'  ))

         call sodein ( nqlat, ngrid,
     +                 juer , ker  )
      endif

      if ((ldlwq) .and. (ker .eq. 0)) then
c
c        --- WATER QUALITY RUN INCLUDING INTERFACE AND DELWAQ ---
c
         nposeg = ip ( gtipnt ('NPOSEG' ))

         call sodewq ( nposeg, juer , ker   )
      endif
c 
      if ((lgrad) .and. (ker .eq. 0)) then
c
c       --- GRADED SEDIMENT MODULE ---
c
        nbran  = ip ( gtipnt ('NBRAN'  ))
        ngrid  = ip ( gtipnt ('NGRID'  ))
        nbrnod = ip ( gtipnt ('NBRNOD' ))
        nfrac  = ip ( gtipnt ('NFRAC'  ))
        nlayer = ip ( gtipnt ('NLAYER' ))

        call GSDEGS ( nbran  ,ngrid  ,nbrnod ,nfrac ,
     +                juer   ,ker    )
      endif
c
c     Allocate memory for file descriptors
c
      if (lflow) then
c
c        Extract restart parameter
c
         ixrun   = gtrpnt ( 'FLWRUN')
         newres  = soipar ( rp(ixrun), 15) .eq. 1
      else
         newres = .false.
      endif

      if (ker .eq. 0) then
         call sodefi ( lflow, lwqin, newres, juer, ker )
      endif
c     
c     Determine number of coefficients per transport formulae
c
      nrcoefs = 1
      if (lgrad.or.lsedt) then 
         nucoef  = ip ( gtipnt ('NUCOEF'))
         if (nucoef.gt.0) then
            nrcoefs = gtrlen ('USCOEF') / nucoef
         else   
            nucoef = 1 
            ip ( gtipnt ('NUCOEF')) = nucoef
         endif
      endif 
c     
c     Determine dimension of array strpar
c
      nstru  = ip ( gtipnt ('NSTRU'))
      if (nstru.gt.0) then
         dmstrpar = gtrlen ('STRPAR') / nstru
         dmstrpar = max(dmstrpar,21)
      else   
         dmstrpar = 21 
      endif
c      
      return
      end
