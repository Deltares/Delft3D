      subroutine SOWQIN ( juer   ,ker )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Main module
c
c Programmer:         J.Kuipers
c
c Module:             SOWQIN (SObek Water Quality INitial)
c
c Module description: If the application includes water quality this
c                     routine will be called.
c
c                     The exchange definitions and pointer table are    
c                     calculated.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 filnam            P  -
c  2 juer              P  -
c  3 ker               O  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c gtipnt  GeT Integer PoiNTer
c gtrpnt  GeT Real PoiNTer
c mkipnt  MaKe Integer PoiNTer
c mkrpnt  MaKe Real PoiNTer
c wqexdi  Water Quality EXchanges DImension
c wqffcr  Water Quality File Formatted CReate
c wqpntr  Water Quality print PoiNTeRs
c wqmoin  Water Quality MOdify delwaq INput file
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
c $Log: sowqin.pf,v $
c Revision 1.6  1999/03/15  15:19:56  kuipe_j
c tabs removed
c
c Revision 1.5  1998/02/13  13:23:50  kuipe_j
c Adapt to CMT
c
c Revision 1.4  1996/12/02  10:03:50  kuipe_j
c avoid negative pointers
c
c Revision 1.3  1996/11/12  15:12:33  kuipe_j
c Declare auxill. arrays later
c
c Revision 1.2  1996/11/01  11:25:45  kuipe_j
c Update of Delwaq input file added
c
c Revision 1.1  1996/10/31  13:03:51  kuipe_j
c Extra resistance finished, Exchanges are calculated
c
c
c***********************************************************************
c
      include '..\include\filsim.i'
c
c
c     Parameters
c
      integer       juer   ,ker
c
c     Variables
c
      integer       branch ,brnode ,dircon ,exdef  ,grdcon ,nbran  ,
     +              nbrnod ,nexdef ,ngrid  ,nnode  ,npntr  ,nsegmt ,
     +              nsegtb ,pntr   ,segcon ,segmnt ,segtab 

      integer       errcod ,errno  ,size 
      character*16  name
      character*80  txt
c
c     External functions
c
      integer       mkipnt, mkrpnt, gtipnt, gtrpnt
      external      mkipnt, mkrpnt, gtipnt, gtrpnt
c 
      logical       exinui, status
      integer        lupntr, luwqi1, luwqi2
      parameter     (lupntr=25, luwqi1=26, luwqi2=27)
c     Include memory pool
c
      include '..\include\mempool.i'
      include '..\include\errcod.i'
c
      size   = 1
      name   = 'NEXDEF'
      errcod = mkipnt ( name , size )
      if (errcod .lt. -1) goto 900
      if (errcod .eq. -1) then      
         exinui = .true.
         npntr  = ip (gtipnt ( 'NPNTR'  ))
      else
         exinui = .false.
      endif
c
      if (.not.exinui) then      
c
c        Exchanges and pointers are calculated in Sobeksim
c        Assign addresses
c
         branch  =     gtipnt ( 'BRANCH' )
         brnode  =     gtipnt ( 'BRNODE' )
         nbran   = ip (gtipnt ( 'NBRAN'  ))
         nbrnod  = ip (gtipnt ( 'NBRNOD' ))
         ngrid   = ip (gtipnt ( 'NGRID'  ))
         nnode   = ip (gtipnt ( 'NNODE'  ))
         nsegmt  = ip (gtipnt ( 'NSEGMT' ))
         nsegtb  = ip (gtipnt ( 'NSEGTB' ))
         segmnt  =     gtipnt ( 'SEGMNT' )
         segtab  =     gtrpnt ( 'SEGTAB' )
c
         size   = nbrnod 
         name   = 'SEGCON'
         errcod = mkipnt ( name , size )
         if (errcod .lt. 0 ) goto 900
         segcon = errcod 
c        
         name   = 'GRDCON'
         errcod = mkipnt ( name , size )
         if (errcod .lt. 0 ) goto 900
         grdcon = errcod 
c
         name   = 'DIRCON'
         errcod = mkipnt ( name , size )
         if (errcod .lt. 0 ) goto 900
         dircon = errcod 
c
         call wqexdi (   ngrid  ,   nbran  ,   nnode  ,   nbrnod ,
     &                   nsegmt ,   npntr  ,   nsegtb ,   nexdef ,
     &                ip(branch),ip(brnode),ip(segmnt),rp(segtab),
     &                ip(segcon),   juer   ,   ker    )
c
         if (ker.eq.fatal) goto 910

         size   = 1
         name   = 'NEXDEF'
         ip(gtipnt(name)) = nexdef
c
         name   = 'NPNTR'
         errcod = mkipnt ( name , size )
         if (errcod .lt. 0 ) goto 900
         ip(gtipnt(name)) = npntr 
 
         size   = nexdef*6
         name   = 'EXDEF'
         errcod = mkrpnt ( name , size )
         if (errcod .lt. 0 ) goto 900
         exdef  = errcod
c
         size   = npntr*4
         name   = 'PNTR'
         errcod = mkipnt ( name , size )
         if (errcod .lt. 0 ) goto 900
         pntr   = errcod 
c
         call wqexad (   ngrid  ,   nbran  ,   nnode  ,   nbrnod ,
     j                   nsegmt ,   npntr  ,   nsegtb ,   nexdef ,
     j                ip(branch),ip(brnode),rp(exdef) ,ip(pntr)  ,
     j                ip(segmnt),rp(segtab),ip(segcon),ip(grdcon),
     j                ip(dircon))
c
c        Write 'Pointer file'
c
         call wqffcr (lupntr ,fpoint , .false., status)
       
         call wqpntr (lupntr ,npntr  ,ip(pntr))
c
c        Modify Delwaq input file if it exists.
c
         call wqffcr (luwqi1 ,fwqino , .true. , status)

         if (status) then
            call wqffcr (luwqi2,fwqinp , .false., status)

            call wqmoin (luwqi1, luwqi2, nexdef, juer   , ker)
            
         else
            ker = warnng
            call error (juer,
     &                  'MOWQIN Cannot open input file',ewqopn,ker )
            ker = ok
         endif

      endif
c    
c     Declare auxilliary arrays with length npntr * ..
c
c     From and to lengths
c

      size   = npntr * 2
      name   = 'TOFROM'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Exchange areas
c
      size   = npntr
      name   = 'SEXAR'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Exchange flows
c
      size   = npntr
      name   = 'SEXFL'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900

      return
c
 900  continue

      ker = fatal

      if (errcod .eq. -1) errno = evrdec
      if (errcod .eq. -2) errno = eoutds
      if (errcod .eq. -3) errno = eoutns

      txt = 'MOWQIN Memory error for @' // name // '@'

      call error ( juer, txt, errno, ker )

  910 continue

      end
