      subroutine SOINRM ( time   ,itim   ,dtf    ,lkalm ,
     +                    lmorp  ,juer   ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Main module
c
c Programmer:         S.L. van der Woude
c
c Module:             SOINRM (SObek INitialise River Morphology)
c
c Module description: This routine initialises river morphology after a
c                     restart file has been read.
c
c                     After reading a restart file from a river morpho-
c                     logy case the sediment transports must be calcula-
c                     ted. First the variables like areas, widths and
c                     friction coefficients must be calculated by cal-
c                     ling FLNP1.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  3 dtf               P  -
c  2 itim              P  -
c  6 juer              P  -
c  7 ker               P  -
c  4 lkalm             P  -
c  5 lmorp             P  -
c  1 time              P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c flnp1   FLow results on time N + 1
c gtipnt  GeT Integer PoiNTer
c gtrpnt  GeT Real PoiNTer
c sedim   SEDIMent module
c sorpar  SObek Real PARameter
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
c $Log: soinrm.pf,v $
c Revision 1.11  1998/06/11  11:47:45  kuipe_j
c Estuary special integrated
c
c Revision 1.10  1998/04/10  09:22:25  kuipe_j
c total area recalculated
c
c Revision 1.9  1997/01/23  08:30:14  kuipe_j
c Make flow module robust
c
c Revision 1.8  1996/12/04  11:59:29  kuipe_j
c declarations / undefined vars
c
c Revision 1.7  1996/04/12  13:06:08  kuipe_j
c headers, minor changes
c
c Revision 1.6  1996/04/11  08:16:34  kuipe_j
c Kalman module added
c
c Revision 1.5  1995/09/22  10:04:22  kuipe_j
c variable dimensions, new headers
c
c Revision 1.4  1995/09/12  08:11:32  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.3  1995/05/30  09:57:00  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:09:54  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:12:19  hoeks_a
c Initial check-in
c
c Revision 1.2  1994/11/28  08:28:40  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.1.1.1  1993/07/21  14:39:44  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c
c     Parameters
c
      integer  itim(2),juer   ,ker
c     real     time   ,dtf
      double precision time   ,dtf
      logical  lkalm  ,lmorp
c
c     Local variables (pointers to arrays)
c
      integer aft   , afwfqs, alfab , arex  , arexcn, arexop, bfricp,
     +        bfrict, bgout , branch, brnode, celer , cpack , dissed,
     +        e     , engpar, forcon, grsize, hlev  , hpack , maxlev,
     +        maxtab, mbdpar, mltpar, morcon, nboun , nbran , nbrnod,
     +        nellvl, ngrid , nlev  , nmlat , nnelvl, nnf   , nnode ,
     +        node  , nonall, nqlat , nsedrd, ntab  , ntabm , ntmpgr, 
     +        nucoef,of    , pfa   , prslot, psltvr, qlat  , qltpar,
     +        qpack , rc    , rpack , scifri, sdrdbf, sectc , sectv ,
     +        seddb , sedinf, sedpar, sedtr , slat  , table ,
     +        tmpgr , trform, typcr , uscoef, waoft , wft   , ws    ,
     +        x     , att   , wtt   , h2    , q2
c     Single variables
c
      real    g ,overlp
c
      integer ixpar
c
c     External functions
c
      integer  gtipnt, gtrpnt, gtdpnt
      real     sorpar
      external gtipnt, gtrpnt, gtdpnt, sorpar
c
c     Include memory pool
c
      include '..\include\mempool.i'
c
c     Extract parameters from flwpar
c
      ixpar = gtrpnt ( 'FLWPAR' )
c
      g      = sorpar ( rp(ixpar), 1 )
      overlp = sorpar ( rp(ixpar), 16)
c
c     Find starting addresses of working arrays
c
c     Single variables are read from the memory pool to simplify the
c     call for the Microsoft Fortran compiler v5
c
      aft    =     gtrpnt ( 'AFT'   )
      att    =     gtrpnt ( 'ATT'   )
      afwfqs =     gtrpnt ( 'AFWFQS')
      alfab  =     gtrpnt ( 'ALFAB' )
      arex   =     gtrpnt ( 'AREX'  )
      arexcn =     gtipnt ( 'AREXCN')
      arexop =     gtipnt ( 'AREXOP')
      bfricp =     gtrpnt ( 'BFRICP')
      bfrict =     gtipnt ( 'BFRICT')
      bgout  =     gtipnt ( 'BGOUT' )
      branch =     gtipnt ( 'BRANCH')
      brnode =     gtipnt ( 'BRNODE')
      celer  =     gtrpnt ( 'CELER' )
      cpack  =     gtrpnt ( 'CPACK' )
      dissed =     gtrpnt ( 'DISSED')
      e      =     gtrpnt ( 'E'     )
      engpar =     gtrpnt ( 'ENGPAR')
      forcon =     gtrpnt ( 'FORCON')
      grsize =     gtrpnt ( 'GRSIZE')
      hlev   =     gtdpnt ( 'HLEV'  )
      hpack  =     gtdpnt ( 'HPACK ')
      maxlev = ip (gtipnt ( 'MAXLEV'))
      maxtab = ip (gtipnt ( 'MAXTAB'))
      mbdpar =     gtipnt ( 'MBDPAR')
      mltpar =     gtrpnt ( 'MLTPAR')
      morcon =     gtrpnt ( 'MORCON')
      nboun  = ip (gtipnt ( 'NBOUN' ))
      nbran  = ip (gtipnt ( 'NBRAN' ))
      nbrnod = ip (gtipnt ( 'NBRNOD'))
      nellvl =     gtrpnt ( 'NELLVL')
      ngrid  = ip (gtipnt ( 'NGRID' ))
      nlev   =     gtipnt ( 'NLEV'  )
      nmlat  = ip (gtipnt ( 'NMLAT' ))
      nnelvl = ip (gtipnt ( 'NNELVL'))
      nnode  = ip (gtipnt ( 'NNODE' ))
      nnf    = ip (gtipnt ( 'NNF'   ))
      node   =     gtipnt ( 'NODE'  )
      nonall =     gtipnt ( 'NONALL')
      nqlat  = ip (gtipnt ( 'NQLAT' ))
      nsedrd = ip (gtipnt ( 'NSEDRD'))
      ntab   =     gtipnt ( 'NTAB'  )
      ntabm  = ip (gtipnt ( 'NTABM' ))
      ntmpgr = ip (gtipnt ( 'NTMPGR'))
      nucoef = ip (gtipnt ( 'NUCOEF'))
      of     =     gtrpnt ( 'OF'    )
      pfa    =     gtrpnt ( 'PFA'   )
      prslot =     gtrpnt ( 'PRSLOT')
      psltvr =     gtrpnt ( 'PSLTVR')
      qlat   =     gtrpnt ( 'QLAT'  )
      qltpar =     gtrpnt ( 'QLTPAR')
      qpack  =     gtdpnt ( 'QPACK' )
      rc     =     gtrpnt ( 'RC'    )
      rpack  =     gtrpnt ( 'RPACK' )
      scifri =     gtipnt ( 'SCIFRI')
      sdrdbf =     gtipnt ( 'SDRDBF')
      sectc  =     gtrpnt ( 'SECTC' )
      sectv  =     gtrpnt ( 'SECTV' )
      seddb  =     gtipnt ( 'SEDDB' )
      sedinf =     gtipnt ( 'SEDINF')
      sedpar =     gtrpnt ( 'SEDPAR')
      sedtr  =     gtrpnt ( 'SEDTR' )
      slat   =     gtrpnt ( 'SLAT'  )
      table  =     gtrpnt ( 'TABLE' )
      tmpgr  =     gtrpnt ( 'TMPGR' )
      trform =     gtrpnt ( 'TRFORM')
      typcr  =     gtipnt ( 'TYPCR' )
      uscoef =     gtrpnt ( 'USCOEF')
      waoft  =     gtrpnt ( 'WAOFT' )
      wft    =     gtrpnt ( 'WFT'   )
      wtt    =     gtrpnt ( 'WTT'   )
      ws     =     gtrpnt ( 'WS'    )
      x      =     gtrpnt ( 'X'     )
c     h2 = hp(,3)
      h2 = hpack + ngrid * 2
c     q2 = qp(,3)
      q2 = qpack + ngrid * 2
c
c     Calculate variables for time level n+1
c
      call flnp1 (   lkalm  ,   nbran  ,   ngrid  ,   nnf    ,
     +            ip(branch),ip(typcr) ,ip(bfrict),rp(bfricp),
     +            dp(h2)    ,dp(q2)    ,   maxlev ,ip(nlev)  ,
     +            dp(hlev)  ,rp(wft)   ,rp(aft)   ,   overlp ,
     +            rp(arex)  ,ip(arexcn),ip(arexop),rp(of)    ,
     +               maxtab ,   ntabm  ,ip(ntab)  ,rp(table) ,
     +            rp(sectc) ,rp(sectv) ,rp(prslot),rp(psltvr),
     +            rp(waoft) ,rp(grsize),rp(engpar),ip(scifri),
     +            rp(pfa)   ,   juer   ,rp(cpack) ,rp(rpack) ,
     +            rp(afwfqs),rp(alfab) ,
     +            rp(wtt)   ,rp(att)   ,ker    )
c
c     Calculate sediment transports
c
      call sedim (   nbran  ,   nnode  ,   nbrnod ,   nboun  ,
     +               maxlev ,   nsedrd ,   nnelvl ,
     +               nqlat  ,   nmlat  ,   ngrid  ,   maxtab ,
     +               ntabm  ,   juer   ,   itim   ,   time   ,
     +               dtf    ,   g      ,   lmorp  ,   ntmpgr ,
     +            rp(tmpgr) ,ip(branch),ip(sedinf),ip(bfrict),
     +            ip(nonall),ip(node  ),ip(seddb ),ip(brnode),
     +            ip(bgout ),ip(sdrdbf),ip(mbdpar),
     +            ip(ntab  ),rp(afwfqs),rp(waoft ),rp(ws    ),
     +            dp(hlev  ),ip(nlev  ),rp(wft   ),rp(psltvr),
     +            rp(sectv ),rp(alfab ),rp(rpack ),rp(x     ),
     +            rp(cpack ),dp(qpack ),dp(hpack ),rp(grsize),
     +            rp(forcon),rp(slat  ),rp(sedtr ),rp(celer ),
     +            rp(trform),rp(prslot),rp(dissed),rp(nellvl),
     +            rp(rc    ),rp(e     ),   nucoef ,rp(uscoef),
     +            rp(engpar),rp(sedpar),rp(morcon),rp(mltpar),
     +            rp(qltpar),rp(qlat  ),rp(table ),   .true. ,
     +               ker    )
c
      return
      end
