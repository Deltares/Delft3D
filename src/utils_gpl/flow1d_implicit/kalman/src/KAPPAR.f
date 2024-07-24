      subroutine KAPPAR(fd_nefis_res, nppamn ,nkppmp ,nkpptm ,ngrid  ,
     &                  itim   ,istep  ,nstep  ,first  ,writim ,juer   ,
     &                  kppmap ,kpptim ,pi     ,np     ,p2     ,nkapar ,
     &                  ncelm  ,ncelh  ,ppacpr ,buf    ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KAPPAR (KAlman Predicted PArameter Results)
c
c Module description: Write the user selected Kalman module results to
c                     the result file. The results processed in this
c                     module are predicted covariances of the correction
c                     parameters.
c
c                     The stored data can be processed further by the
c                     User Interface. The user can select functions of
c                     place or functions of time. See [S-DD-002.1MR] for
c                     a specification of the nefis names.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 22 buf(ngrid)        O  Buffer for results to be written to NEFIS.
c  2 dafdrs            P  -
c  1 defdrs            P  -
c 10 first             I  True in case of first call.
c  8 istep             I  Current time step number (t(n+1)).
c  7 itim              P  -
c 12 juer              P  -
c 23 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 13 kppmap(*)         I  Parameter list for prediction results of para-
c                         meters MAP block.
c                         (1/2/3) Report step of begin, end, resp.
c                         increment.
c                         (4) Main code parameter 1.
c                         (5) Sub code parameter 1.
c                         (.) Etc.
c 14 kpptim(*)         I  Parameter list for prediction results of para-
c                         meters HIST block.
c                         (1) Number of places.
c                         (2) Location 1.
c                         (i) Location n.
c                         (i+1) Main code parameter 1.
c                         (i+2) Sub code parameter 1.
c                         (.) Etc.
c 20 ncelh             I  Actual cell number of a history block of re-
c                         sult file.
c 19 ncelm             I  Actual cell number of a map block of result
c                         file.
c  6 ngrid             I  Number of grid points in network.
c 18 nkapar            I  = nnf + nnmu + 1
c  4 nkppmp            I  Number of entries in kppmap.
c  5 nkpptm            I  Number of entries in kpptim.
c 16 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
c  3 nppamn            I  Number of main codes of parameter prediction
c  9 nstep             I  Last time step number in simulation.
c 17 p2(np,np)         I  Matrix with covariances of waterlevels,
c                         discharges and uncertain correction parameters
c                         (bed friction, contraction in case of free gate
c                         flow and wind) on time level n+1|n (predicted
c                         values).
c 15 pi                I  prediction interval (number of prediction steps)
c 21 ppacpr(nppamn)    I  ppacpr(i) = index in block table (1...nentri)
c                         for main code i of the parameter prediction
c 11 writim            P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c flsdat  FLuSh buffers of DATa file
c putiel  PUT Integer ELement to nefis file
c putrel  PUT Real ELement to a nefis file
c resadm  RESults; make ADMinistration for element selection
c resdes  RESults; DEScription group is defined
c resini  RESults; writing is INItialized
c restim  RESults; writing of current TIMe
c yesmap  YES MAP results are written
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: kappar.pf,v $
c Revision 1.5  1999/03/15  15:52:06  kuipe_j
c tabs removed
c
c Revision 1.4  1996/12/03  07:59:08  kuipe_j
c dimension of element names improved
c
c Revision 1.3  1996/09/03  14:54:27  kuipe_j
c frequency time hist,etc
c
c Revision 1.2  1996/04/12  13:05:15  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:48  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer      nppamn ,nkppmp ,nkpptm ,ngrid  ,juer   ,istep  ,
     &             nstep  ,ncelm  ,ncelh  ,ker    ,pi     ,np     ,
     &             nkapar
      integer      fd_nefis_res, kppmap(nkppmp),
     &             kpptim(nkpptm) ,ppacpr(nppamn),itim  (2)
      real         p2(np,np)      , buf(nkapar)
      logical      first   ,writim
c
c     Declaration of local variables
c
      integer      nentri
      parameter   (nentri=2)
      integer      errr  ,i ,j ,k ,ie ,nrerr, igr, nlc, nsk, lastcod
      integer      code  (nentri)   ,usrord(1),
     &             uindex(3)        ,ibuf(1)
      character*16 grnamm          ,grnamh          ,grnamd         ,
     &             name
      character*16 nameel(nentri)  ,quanel(nentri)  ,unitel(nentri) ,
     &             nameac(nentri+1)
      character*64 descel(nentri)
      character*8  txt
      logical      llog, new    ,newuit
c
c     Declaration of external functions
c
      integer      putiel, putrel ,flsdat
      logical      yesmap
      external     putiel, putrel ,flsdat ,yesmap
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
c
      data  usrord /1/
c
c     Definition of elements.
c
      data (code(i) ,descel(i) ,nameel(i) ,quanel(i) ,
     &      unitel(i),i=1,nentri) /
c
     & 1 ,'Prediction interval'            ,'HIS_pi ' ,'pi ', '-' ,
     & 2 ,'Predicted parameter covariance' ,'HIS_pcp' ,'pcp', '-' /
c
      data  grnamm   ,grnamh ,grnamd/
     &      'KPPAR-MAP-GROUP' ,
     &      'KPPAR-HIS-GROUP' ,
     &      'KPPAR-DES-GROUP' /
c
      lastcod = nentri
c
c     Initialize.
c
      if (first) then
         nrerr = ekaboo
c
         call resadm (nentri ,code   ,ppacpr )
c
         call resini (fd_nefis_res, grnamd ,grnamm ,grnamh ,'KPPAR',
     &                nentri ,nkppmp ,nkpptm ,nkapar ,itim   ,nameel ,
     &                quanel ,unitel ,descel ,ppacpr ,kppmap ,kpptim ,
     &                ncelm  ,ncelh  ,nameac ,lastcod,errr   )
c
         if (errr.ne.0) goto 1000
      endif
c
c     Write Map results
c
      if (nkppmp .gt. 3) then
         if (yesmap(kppmap(1),kppmap(2),kppmap(3),istep)) then
            nrerr = ekamap
c
            call restim (fd_nefis_res, grnamm ,1  ,itim  ,ncelm  ,
     &                   nameac ,errr   )
            if (errr.ne.0) goto 1000
c
            call resdes ('KPPAR',fd_nefis_res, grnamd ,1 ,writim ,
     &                   kpptim ,ncelm  ,ncelh  ,errr   )
            if (errr.ne.0) goto 1000
c
            uindex(1) = ncelm
            uindex(2) = ncelm
            uindex(3) = 1
c
            do 40 i = 4,nkppmp,2
               ie = ppacpr(kppmap(i))+kppmap(i+1)
c
c              The element names of the Map block start with
c              MAP instead of HIS.
c
               name  = 'MAP'//nameel(ie)(4:)
               goto (10,20) , ie
   10          continue
                  goto 30
   20          continue
                  k = 1
                  do 25 j = ngrid*2+1, np
                     buf(k) = p2(j,j)
                     k = k + 1
   25             continue
                  goto 30
   30          continue
                  if (ie .eq. 1) then
                     ibuf(1) = pi
                     errr = putiel (fd_nefis_res, grnamm ,name   ,
     &                              uindex  ,usrord ,ibuf   )
                  else
                     errr = putrel (fd_nefis_res, grnamm ,name   ,
     &                              uindex  ,usrord ,buf    )
                  endif
                  if (errr.ne.0) goto 1000
   40       continue
         endif
      endif
c
c     Write History results.
c
      nlc = kpptim(1)
      new = mod(nkpptm-nlc,2) .eq. 0

      if ( new ) then
         nsk = 3
      else
         nsk = 0
      endif
c
      if (nkpptm .gt. 1+nsk) then
         if (new) then
            newuit = yesmap(kpptim(nlc+2),kpptim(nlc+3),kpptim(nlc+4),
     &                      istep)
         else
            newuit = .false.
         endif
         if ( (kpptim(1).gt.0 .and. .not. new) .or. newuit ) then
            nrerr = ekahis
c
            call restim (fd_nefis_res, grnamh ,1  ,itim  ,ncelh  ,
     &                   nameac ,errr   )
            if (errr.ne.0) goto 1000
c
            call resdes ('KPPAR',fd_nefis_res, grnamd ,2 ,writim ,
     &                   kpptim ,ncelm  ,ncelh  ,errr   )
            if (errr.ne.0) goto 1000
c
            uindex(1) = ncelh
            uindex(2) = ncelh
            uindex(3) = 1
c
            do 80 i = kpptim(1)+2+nsk,nkpptm,2
               ie = ppacpr(kpptim(i))+kpptim(i+1)
               goto (50,60) ,ie
   50          continue
                  goto 70
   60          continue
                  do 65 j=1,kpptim(1)
                     igr = kpptim(j+1) + 2*ngrid
                     buf(j) = p2(igr,igr)
   65             continue
                  goto 70
   70          continue
                  if (ie .eq. 1) then
                     ibuf(1) = pi
                     errr = putiel (fd_nefis_res, grnamh ,nameel(ie),
     &                              uindex  ,usrord ,ibuf   )
                  else
                     errr = putrel (fd_nefis_res, grnamh ,nameel(ie),
     &                              uindex  ,usrord ,buf    )
                  endif
                  if (errr.ne.0) goto 1000
   80       continue
         endif
      endif
c
c     Be sure that at the last step the number of cells has been
c     written correctly.
c
      if (istep.ge.nstep) then
         nrerr = ekaeoo
         llog  = .true.
c
         call resdes ('KPPAR' ,fd_nefis_res, grnamd ,3 ,llog ,
     &                 kpptim ,ncelm  ,ncelh  ,errr   )
         if (errr.ne.0) goto 1000
c
         errr = flsdat (fd_nefis_res)
         if (errr.ne.0) goto 1000
      endif
c
      return
c
 1000 continue
c
      ker = fatal
      write (txt,'(i8)') errr
      call error (juer ,'KAPPAR @'//txt//'@' ,nrerr ,ker)
c
      end
