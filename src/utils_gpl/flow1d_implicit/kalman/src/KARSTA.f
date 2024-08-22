      subroutine KARSTA(np     ,nnf    ,nnmu   ,itim   ,juer   ,first  ,
     &                  newres ,fd_nefis_rst, fd_nefis_new, pmat   ,
     &                  pfa    ,pmua   ,pw     ,ncelst ,inires ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KARSTA (KAlman read or write ReSTArt information)
c
c Module description: Read or write the Kalman restart information fromi
c                     or to the restart file.
c
c                     When the user restarts a simulation run from a
c                     specific point of time this routine will read the
c                     saved restart information from the previous run.
c                     The restart informa tion contains the actual
c                     correction parameters, covariances and noises at
c                     the time level the information was saved.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 11 dafdrn            P  -
c  9 dafdst            P  -
c 10 defdrn            P  -
c  8 defdst            P  -
c  6 first             I  True in case of first call.
c 17 inires            I  True when no restart info of this module has
c                         been written before.
c  4 itim              P  -
c  5 juer              P  -
c 18 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 16 ncelst            IO Actual cell number of a restart block of the
c                         restart file.
c  7 newres            I  true, if a new restart file will be made
c  2 nnf               I  Number of uncertain bed friction parameters.
c  3 nnmu              I  Number of uncertain energy loss parameters in
c                         case of free gate flow.
c  1 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
c 13 pfa               P  -
c 12 pmat              P  -
c 14 pmua              P  -
c 15 pw                P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c kadfst  KAlman DeFine reSTart group
c karest  KAlman REad reSTart information
c kawrst  KAlman WRite reSTart information
c statim  Search resTArt TIMe point
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: karsta.pf,v $
c Revision 1.5  1999/07/23  15:10:25  kuipe_j
c improve restart
c
c Revision 1.4  1998/06/18  13:32:19  kuipe_j
c Bug in resart flag solved
c
c Revision 1.3  1996/12/03  07:59:09  kuipe_j
c dimension of element names improved
c
c Revision 1.2  1996/04/12  13:05:24  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:56  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer      np     ,nnf    ,nnmu   ,ncelst ,
     &             juer   ,ker
      integer      fd_nefis_rst, fd_nefis_new, itim(2)
      real         pmat (np,np)   ,pfa(nnf)       ,pmua(nnmu) ,pw
      logical      first  ,inires ,newres

c     Declaration of local variables
c
      integer      nentri
      parameter   (nentri=5)
      integer      errr  ,nrerr ,i
      integer      ndim  (nentri)
      character*16 grnamf
      character*16 nameel(nentri)  ,quanel(nentri)  ,unitel(nentri) ,
     &             nameac(nentri+1)
      character*64 descel(nentri)
      character*8  txt
      logical      inidum
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
c
c     Definition of elements.
c
      data (ndim(i)  ,descel(i)   ,nameel(i) ,quanel(i) ,
     &      unitel(i),i=1,nentri) /
c
     & 1,'Restart time step'                 ,'ISTEP','t'    ,'-',
     & 2,'Covariances'                       ,'P'    ,'pmat ','?',
     & 1,'Correction parameters bed stress'  ,'PFA'  ,'pfa'  ,'-',
     & 1,'Correction parameters energy loss' ,'PMUA' ,'pmua' ,'-',
     & 1,'Correction parameter wind stress'  ,'PW'   ,'pw'   ,'-' /
c
      data  grnamf   /
     &      'KALMAN-RES-GROUP' /
c
      errr = 0
c
      if (first) then
c
c        Determine if it is an initial run or a restart run for this
c        module. In the latter case restart information must exist
c        already.
c
         nrerr = ekabor
         call KADFST (fd_nefis_rst, grnamf ,nentri ,np     ,nnf    ,
     &                nnmu   ,ndim   ,nameel ,quanel ,unitel ,descel ,
     &                .not.newres    ,inires ,nameac ,errr   )
         if (errr.ne.0) goto 1000
c
         if (inires) then
            if (newres) then
c
c              Initialize a new restart file
c
               nrerr = ekabor
               call KADFST (fd_nefis_new, grnamf ,nentri ,np     ,
     &                      nnf    ,nnmu   ,ndim   ,nameel ,quanel ,
     &                      unitel ,descel ,.true. ,inidum ,nameac ,
     &                      errr   )
               if (errr.ne.0) goto 1000
            endif
            ncelst = 1
         else
c
c           Read from restart file. Select proper time step first.
c
            nrerr = ekarrd
            call STATIM (fd_nefis_rst, grnamf ,nameel(1),itim,ncelst,
     &                   errr   )
            if (errr.ne.0) goto 1000
c
            call KAREST (fd_nefis_rst, grnamf ,nentri ,np     ,nnf   ,
     &                   nnmu   ,ncelst ,nameel ,pmat   ,pfa    ,pmua  ,
     &                   pw     ,errr   )
            if (errr.ne.0) goto 1000
c
            if (newres) then
c
c              Initialize a new restart file
c
               nrerr = ekabor
               call KADFST (fd_nefis_new, grnamf ,nentri ,np     ,
     &                      nnf    ,nnmu   ,ndim   ,nameel ,quanel ,
     &                      unitel ,descel ,.true. ,inidum ,nameac ,
     &                      errr   )
               if (errr.ne.0) goto 1000
               ncelst = 1
            else
c
c              Continue with existing restart file
c
               ncelst = ncelst+1
            endif
         endif
c
      else
c
c        Write to restart file.
c
         nrerr = ekawrd
         call KAWRST (fd_nefis_rst, grnamf ,nentri ,np    ,nnf    ,
     &                nnmu   ,ncelst ,itim   ,nameel ,pmat  ,pfa    ,
     &                pmua   ,pw     ,errr   )
         if (errr.ne.0) goto 1000
         ncelst = ncelst+1
c
      endif
      goto 1010
c
 1000 continue
      if (errr.gt.0) then
c
c        Could not found restart time step.
         ker = fatal
         call error (juer ,'KARSTA' ,ekartt ,ker)
      else
c
c        NEFIS error ( <0 )
         ker = fatal
         write (txt,'(i8)') errr
         call error (juer ,'KARSTA @'//txt//'@' ,nrerr ,ker)
      endif
c
 1010 continue
      end
