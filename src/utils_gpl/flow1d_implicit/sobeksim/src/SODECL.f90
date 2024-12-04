subroutine SODECL (appl, lhisgp ,juer, ker)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Main module
!
! Programmer:         S.L. van der Woude
!
! Module:             SODECL (SObek DECLare variables)
!
! Module description: Declare variables for each module in application.
!
!                     After reading the model information and the type
!                     of application the variables for storage of the
!                     calculated results should be declared in the memo-
!                     ry pools.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 appl              IO Application string containing switches for
!                         simulation run
!  2 juer              P  -
!  3 ker               I  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! gtclen  GeT Character LENgth
! gtcpnt  GeT Character PoiNTer
! gtipnt  GeT Integer PoiNTer
! gtrpnt  GeT Real PoiNTer
! sodefi  SObek DEclare FIle descriptors
! sodefl  SObek DEclare FLow variables
! sodein  SObek DEclare INterface variables
! sodeka  SObek DEclare KAlman variables
! sodemo  SObek DEclare MOrphology variables
! sodesa  SObek DEclare SAlt variables
! sodese  SObek DEclare SEdiment transport variables
! sodewq  SObek DEclare Water Quality interface variables
! soipar  SObek Integer PARameter
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: sodecl.pf,v $
! Revision 1.15  1999/03/15  15:19:37  kuipe_j
! tabs removed
!
! Revision 1.14  1997/08/21  12:48:52  kuipe_j
! Decl for only sediment
!
! Revision 1.13  1997/06/17  11:29:17  kuipe_j
! output in history format
!
! Revision 1.12  1996/12/04  11:59:26  kuipe_j
! declarations / undefined vars
!
! Revision 1.11  1996/12/02  10:03:41  kuipe_j
! avoid negative pointers
!
! Revision 1.10  1996/04/12  13:05:57  kuipe_j
! headers, minor changes
!
! Revision 1.9  1996/04/11  08:16:23  kuipe_j
! Kalman module added
!
! Revision 1.8  1996/01/17  14:47:33  kuipe_j
! header update
!
! Revision 1.7  1996/01/16  15:01:52  kuipe_j
! Restart improvements
!
! Revision 1.6  1995/09/22  10:03:40  kuipe_j
! variable dimensions, new headers
!
! Revision 1.5  1995/09/12  08:11:28  overmar
! - Option "zomerkaden" added
! - Better linearization
! - Pseudo time
! - Iterative matrix solution
!
! Revision 1.4  1995/08/30  12:37:30  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.3  1995/05/30  09:56:44  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:09:36  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:12:01  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:09:31  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:39:40  kuipe_j
! Initial version
!
!
!***********************************************************************
!
   include '..\include\sobdim.i'
!
!     Parameters
!
   character*(*)   appl
   integer         juer, ker
!
!     Variables
!
   integer         arexop, nboun,  nbrnod, nbran,  ncontr, nfrac,&
   &ngrid,  ngridm, nhstat, nlayer, maxlev, nmouth,&
   &nnf,    nnmu,   nnode,  nnc   , nposeg, np,&
   &nqlat,  nqstat, nsamp,  nsedrd, nstru , nucoef
!
   integer         dsopt,  iappl,  sappl,  lappl,  ixrun

   logical         lflow,  lsalt,  lsedt,  lmorp,  lwqin,  ldlwq,&
   &lestu,  newres, lkalm,  lgrad,  lhisgp, lgrwt

   integer         gtcpnt, gtclen, gtipnt, gtrpnt, gtrlen, soipar
   external        gtcpnt, gtclen, gtipnt, gtrpnt, gtrlen, soipar
!
!     Include memory pool
!
   include '..\include\mempool.i'
!
!     Read application definition from the string pool
!
   lappl   = gtclen ( 'APPL' )
   sappl   = gtcpnt ( 'APPL' )
   appl    = ' '
   do 100 iappl = 1, lappl
      appl (iappl:iappl) = cp ( sappl )
      sappl = sappl + 1
100 continue
!
!     Read application switches
!
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
!
!        --- FLOW MODULE ---
!
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
!
      arexop = gtipnt ('AREXOP')

      call sodefl ( lsalt , lkalm , ngrid  ,ngridm ,nhstat ,&
      &maxlev ,nnode , nbrnod ,nqlat  ,nqstat ,&
      &nstru  ,ncontr, ip(arexop)     ,lwqin  ,&
      &lhisgp ,lgrwt , juer   ,ker   )
   endif
!
   if ((lkalm) .and. (ker .eq. 0)) then
!
!        --- KALMAN MODULE ---
!
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

      call sodeka (ngrid  ,nqlat  ,nstru  ,nbran  ,nnode  ,np     ,&
      &nsamp  ,nnc    ,nnf    ,nnmu   ,juer   ,ker    )
   endif
!
   if ((lsalt) .and. (ker .eq. 0)) then
!
!        --- SALT MODULE ---
!
      dsopt  = ip ( gtipnt ('DSOPT'  ))
      nbran  = ip ( gtipnt ('NBRAN'  ))
      nboun  = ip ( gtipnt ('NBOUN'  ))
      ngrid  = ip ( gtipnt ('NGRID'  ))
      nmouth = ip ( gtipnt ('NMOUTH' ))

      call sodesa ( dsopt, nbran, nboun, ngrid, nmouth,&
      &juer,  ker  )
   endif

   nsedrd = 0
   if ((lsedt) .and. (ker .eq. 0)) then
!
!        --- SEDIMENT TRANSPORT MODULE ---
!
      nbran  = ip ( gtipnt ('NBRAN'  ))
      ngrid  = ip ( gtipnt ('NGRID'  ))
      nsedrd = ip ( gtipnt ('NSEDRD' ))
      nbrnod = ip ( gtipnt ('NBRNOD' ))

      call sodese ( nbran, ngrid, nsedrd, nbrnod, juer , lmorp ,&
      &ker  )
   endif

   if ((lmorp .or. lgrad) .and. (ker .eq. 0)) then
!
!        --- MORPHOLOGY MODULE ---
!
      nbran  = ip ( gtipnt ('NBRAN'  ))
      ngrid  = ip ( gtipnt ('NGRID'  ))

      call sodemo ( lestu, nbran, ngrid, juer, ker )
   endif

   if ((lwqin) .and. (ker .eq. 0)) then
!
!        --- VARIABLES FOR FLOW MODULE AND WATER QUALITY INTERFACE
!
      ngrid  = ip ( gtipnt ('NGRID'  ))
      nqlat  = ip ( gtipnt ('NQLAT'  ))

      call sodein ( nqlat, ngrid,&
      &juer , ker  )
   endif

   if ((ldlwq) .and. (ker .eq. 0)) then
!
!        --- WATER QUALITY RUN INCLUDING INTERFACE AND DELWAQ ---
!
      nposeg = ip ( gtipnt ('NPOSEG' ))

      call sodewq ( nposeg, juer , ker   )
   endif
!
   if ((lgrad) .and. (ker .eq. 0)) then
!
!       --- GRADED SEDIMENT MODULE ---
!
      nbran  = ip ( gtipnt ('NBRAN'  ))
      ngrid  = ip ( gtipnt ('NGRID'  ))
      nbrnod = ip ( gtipnt ('NBRNOD' ))
      nfrac  = ip ( gtipnt ('NFRAC'  ))
      nlayer = ip ( gtipnt ('NLAYER' ))

      call GSDEGS ( nbran  ,ngrid  ,nbrnod ,nfrac ,&
      &juer   ,ker    )
   endif
!
!     Allocate memory for file descriptors
!
   if (lflow) then
!
!        Extract restart parameter
!
      ixrun   = gtrpnt ( 'FLWRUN')
      newres  = soipar ( rp(ixrun), 15) .eq. 1
   else
      newres = .false.
   endif

   if (ker .eq. 0) then
      call sodefi ( lflow, lwqin, newres, juer, ker )
   endif
!
!     Determine number of coefficients per transport formulae
!
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
!
!     Determine dimension of array strpar
!
   nstru  = ip ( gtipnt ('NSTRU'))
   if (nstru.gt.0) then
      dmstrpar = gtrlen ('STRPAR') / nstru
      dmstrpar = max(dmstrpar,21)
   else
      dmstrpar = 21
   endif
!
   return
end
