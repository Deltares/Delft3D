subroutine KARSTA(np     ,nnf    ,nnmu   ,itim   ,juer   ,first  ,&
&newres ,fd_nefis_rst, fd_nefis_new, pmat   ,&
&pfa    ,pmua   ,pw     ,ncelst ,inires ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KARSTA (KAlman read or write ReSTArt information)
!
! Module description: Read or write the Kalman restart information fromi
!                     or to the restart file.
!
!                     When the user restarts a simulation run from a
!                     specific point of time this routine will read the
!                     saved restart information from the previous run.
!                     The restart informa tion contains the actual
!                     correction parameters, covariances and noises at
!                     the time level the information was saved.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 11 dafdrn            P  -
!  9 dafdst            P  -
! 10 defdrn            P  -
!  8 defdst            P  -
!  6 first             I  True in case of first call.
! 17 inires            I  True when no restart info of this module has
!                         been written before.
!  4 itim              P  -
!  5 juer              P  -
! 18 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
! 16 ncelst            IO Actual cell number of a restart block of the
!                         restart file.
!  7 newres            I  true, if a new restart file will be made
!  2 nnf               I  Number of uncertain bed friction parameters.
!  3 nnmu              I  Number of uncertain energy loss parameters in
!                         case of free gate flow.
!  1 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
! 13 pfa               P  -
! 12 pmat              P  -
! 14 pmua              P  -
! 15 pw                P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! kadfst  KAlman DeFine reSTart group
! karest  KAlman REad reSTart information
! kawrst  KAlman WRite reSTart information
! statim  Search resTArt TIMe point
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: karsta.pf,v $
! Revision 1.5  1999/07/23  15:10:25  kuipe_j
! improve restart
!
! Revision 1.4  1998/06/18  13:32:19  kuipe_j
! Bug in resart flag solved
!
! Revision 1.3  1996/12/03  07:59:09  kuipe_j
! dimension of element names improved
!
! Revision 1.2  1996/04/12  13:05:24  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:56  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer      np     ,nnf    ,nnmu   ,ncelst ,&
   &juer   ,ker
   integer      fd_nefis_rst, fd_nefis_new, itim(2)
   real         pmat (np,np)   ,pfa(nnf)       ,pmua(nnmu) ,pw
   logical      first  ,inires ,newres

!     Declaration of local variables
!
   integer      nentri
   parameter   (nentri=5)
   integer      errr  ,nrerr ,i
   integer      ndim  (nentri)
   character*16 grnamf
   character*16 nameel(nentri)  ,quanel(nentri)  ,unitel(nentri) ,&
   &nameac(nentri+1)
   character*64 descel(nentri)
   character*8  txt
   logical      inidum
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
!
!     Definition of elements.
!
   data (ndim(i)  ,descel(i)   ,nameel(i) ,quanel(i) ,&
   &unitel(i),i=1,nentri) /&
!
   &1,'Restart time step'                 ,'ISTEP','t'    ,'-',&
   &2,'Covariances'                       ,'P'    ,'pmat ','?',&
   &1,'Correction parameters bed stress'  ,'PFA'  ,'pfa'  ,'-',&
   &1,'Correction parameters energy loss' ,'PMUA' ,'pmua' ,'-',&
   &1,'Correction parameter wind stress'  ,'PW'   ,'pw'   ,'-' /
!
   data  grnamf   /&
   &'KALMAN-RES-GROUP' /
!
   errr = 0
!
   if (first) then
!
!        Determine if it is an initial run or a restart run for this
!        module. In the latter case restart information must exist
!        already.
!
      nrerr = ekabor
      call KADFST (fd_nefis_rst, grnamf ,nentri ,np     ,nnf    ,&
      &nnmu   ,ndim   ,nameel ,quanel ,unitel ,descel ,&
      &.not.newres    ,inires ,nameac ,errr   )
      if (errr.ne.0) goto 1000
!
      if (inires) then
         if (newres) then
!
!              Initialize a new restart file
!
            nrerr = ekabor
            call KADFST (fd_nefis_new, grnamf ,nentri ,np     ,&
            &nnf    ,nnmu   ,ndim   ,nameel ,quanel ,&
            &unitel ,descel ,.true. ,inidum ,nameac ,&
            &errr   )
            if (errr.ne.0) goto 1000
         endif
         ncelst = 1
      else
!
!           Read from restart file. Select proper time step first.
!
         nrerr = ekarrd
         call STATIM (fd_nefis_rst, grnamf ,nameel(1),itim,ncelst,&
         &errr   )
         if (errr.ne.0) goto 1000
!
         call KAREST (fd_nefis_rst, grnamf ,nentri ,np     ,nnf   ,&
         &nnmu   ,ncelst ,nameel ,pmat   ,pfa    ,pmua  ,&
         &pw     ,errr   )
         if (errr.ne.0) goto 1000
!
         if (newres) then
!
!              Initialize a new restart file
!
            nrerr = ekabor
            call KADFST (fd_nefis_new, grnamf ,nentri ,np     ,&
            &nnf    ,nnmu   ,ndim   ,nameel ,quanel ,&
            &unitel ,descel ,.true. ,inidum ,nameac ,&
            &errr   )
            if (errr.ne.0) goto 1000
            ncelst = 1
         else
!
!              Continue with existing restart file
!
            ncelst = ncelst+1
         endif
      endif
!
   else
!
!        Write to restart file.
!
      nrerr = ekawrd
      call KAWRST (fd_nefis_rst, grnamf ,nentri ,np    ,nnf    ,&
      &nnmu   ,ncelst ,itim   ,nameel ,pmat  ,pfa    ,&
      &pmua   ,pw     ,errr   )
      if (errr.ne.0) goto 1000
      ncelst = ncelst+1
!
   endif
   goto 1010
!
1000 continue
   if (errr.gt.0) then
!
!        Could not found restart time step.
      ker = fatal
      call error (juer ,'KARSTA' ,ekartt ,ker)
   else
!
!        NEFIS error ( <0 )
      ker = fatal
      write (txt,'(i8)') errr
      call error (juer ,'KARSTA @'//txt//'@' ,nrerr ,ker)
   endif
!
1010 continue
end
