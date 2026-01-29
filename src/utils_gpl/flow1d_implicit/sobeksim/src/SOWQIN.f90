subroutine SOWQIN ( juer   ,ker )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Main module
!
! Programmer:         J.Kuipers
!
! Module:             SOWQIN (SObek Water Quality INitial)
!
! Module description: If the application includes water quality this
!                     routine will be called.
!
!                     The exchange definitions and pointer table are
!                     calculated.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 filnam            P  -
!  2 juer              P  -
!  3 ker               O  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! gtipnt  GeT Integer PoiNTer
! gtrpnt  GeT Real PoiNTer
! mkipnt  MaKe Integer PoiNTer
! mkrpnt  MaKe Real PoiNTer
! wqexdi  Water Quality EXchanges DImension
! wqffcr  Water Quality File Formatted CReate
! wqpntr  Water Quality print PoiNTeRs
! wqmoin  Water Quality MOdify delwaq INput file
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
! $Log: sowqin.pf,v $
! Revision 1.6  1999/03/15  15:19:56  kuipe_j
! tabs removed
!
! Revision 1.5  1998/02/13  13:23:50  kuipe_j
! Adapt to CMT
!
! Revision 1.4  1996/12/02  10:03:50  kuipe_j
! avoid negative pointers
!
! Revision 1.3  1996/11/12  15:12:33  kuipe_j
! Declare auxill. arrays later
!
! Revision 1.2  1996/11/01  11:25:45  kuipe_j
! Update of Delwaq input file added
!
! Revision 1.1  1996/10/31  13:03:51  kuipe_j
! Extra resistance finished, Exchanges are calculated
!
!
!***********************************************************************
!
   include '..\include\filsim.i'
!
!
!     Parameters
!
   integer       juer   ,ker
!
!     Variables
!
   integer       branch ,brnode ,dircon ,exdef  ,grdcon ,nbran  ,&
   &nbrnod ,nexdef ,ngrid  ,nnode  ,npntr  ,nsegmt ,&
   &nsegtb ,pntr   ,segcon ,segmnt ,segtab

   integer       errcod ,errno  ,size
   character*16  name
   character*80  txt
!
!     External functions
!
   integer       mkipnt, mkrpnt, gtipnt, gtrpnt
   external      mkipnt, mkrpnt, gtipnt, gtrpnt
!
   logical       exinui, status
   integer        lupntr, luwqi1, luwqi2
   parameter     (lupntr=25, luwqi1=26, luwqi2=27)
!     Include memory pool
!
   include '..\include\mempool.i'
   include '..\include\errcod.i'
!
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
!
   if (.not.exinui) then
!
!        Exchanges and pointers are calculated in Sobeksim
!        Assign addresses
!
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
!
      size   = nbrnod
      name   = 'SEGCON'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0 ) goto 900
      segcon = errcod
!
      name   = 'GRDCON'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0 ) goto 900
      grdcon = errcod
!
      name   = 'DIRCON'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0 ) goto 900
      dircon = errcod
!
      call wqexdi (   ngrid  ,   nbran  ,   nnode  ,   nbrnod ,&
      &nsegmt ,   npntr  ,   nsegtb ,   nexdef ,&
      &ip(branch),ip(brnode),ip(segmnt),rp(segtab),&
      &ip(segcon),   juer   ,   ker    )
!
      if (ker.eq.fatal) goto 910

      size   = 1
      name   = 'NEXDEF'
      ip(gtipnt(name)) = nexdef
!
      name   = 'NPNTR'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0 ) goto 900
      ip(gtipnt(name)) = npntr

      size   = nexdef*6
      name   = 'EXDEF'
      errcod = mkrpnt ( name , size )
      if (errcod .lt. 0 ) goto 900
      exdef  = errcod
!
      size   = npntr*4
      name   = 'PNTR'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0 ) goto 900
      pntr   = errcod
!
      call wqexad (   ngrid  ,   nbran  ,   nnode  ,   nbrnod ,&
      &nsegmt ,   npntr  ,   nsegtb ,   nexdef ,&
      &ip(branch),ip(brnode),rp(exdef) ,ip(pntr)  ,&
      &ip(segmnt),rp(segtab),ip(segcon),ip(grdcon),&
      &ip(dircon))
!
!        Write 'Pointer file'
!
      call wqffcr (lupntr ,fpoint , .false., status)

      call wqpntr (lupntr ,npntr  ,ip(pntr))
!
!        Modify Delwaq input file if it exists.
!
      call wqffcr (luwqi1 ,fwqino , .true. , status)

      if (status) then
         call wqffcr (luwqi2,fwqinp , .false., status)

         call wqmoin (luwqi1, luwqi2, nexdef, juer   , ker)

      else
         ker = warnng
         call error (juer,&
         &'MOWQIN Cannot open input file',ewqopn,ker )
         ker = ok
      endif

   endif
!
!     Declare auxilliary arrays with length npntr * ..
!
!     From and to lengths
!

   size   = npntr * 2
   name   = 'TOFROM'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Exchange areas
!
   size   = npntr
   name   = 'SEXAR'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Exchange flows
!
   size   = npntr
   name   = 'SEXFL'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900

   return
!
900 continue

   ker = fatal

   if (errcod .eq. -1) errno = evrdec
   if (errcod .eq. -2) errno = eoutds
   if (errcod .eq. -3) errno = eoutns

   txt = 'MOWQIN Memory error for @' // name // '@'

   call error ( juer, txt, errno, ker )

910 continue

end
