      subroutine KAFPAR(fd_nefis_res, nfpamn ,nkfpmp ,nkfptm ,ngrid  ,
     &                  itim   ,istep  ,nstep  ,first  ,writim ,lfilt  ,
     &                  juer   ,kfpmap ,kfptim ,nnf    ,pfa    ,nnmu   ,
     &                  pmua   ,pw     ,np     ,p1     ,nkapar ,
     &                  ncelm  ,ncelh  ,fpacpr ,buf    ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KAFPAR (KAlman Filtered PArameter Results)
c
c Module description: Write the user selected Kalman module results to
c                     the result file. The results processed in this
c                     module are filtered mean and covariances of the
c                     correction parameters.
c
c                     The stored data can be processed further by the
c                     User Interface. The user can select functions of
c                     place or functions of time. See [S-DD-002.1MR] for
c                     a specification of the nefis names.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 27 buf(ngrid)        O  Buffer for results to be written to NEFIS.
c  2 dafdrs            P  -
c  1 defdrs            P  -
c 10 first             I  True in case of first call.
c 26 fpacpr(nfpamn)    I  fpacpr(i) = index in block table (1...nentri)
c                         for main code i of the parameter filter
c  8 istep             I  Current time step number (t(n+1)).
c  7 itim              P  -
c 13 juer              P  -
c 28 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 14 kfpmap(*)         I  Parameter list for residuals (MAP block).
c                         (1/2/3) Report step of begin, end, resp.
c                         increment.
c                         (4) Main code parameter 1.
c                         (5) Sub code parameter 1.
c                         (.) Etc.
c 15 kfptim(*)         I  Parameter list for residuals (HIST block).
c                         (1) Number of places.
c                         (2) Location 1.
c                         (i) Location n.
c                         (i+1) Main code parameter 1.
c                         (i+2) Sub code parameter 1.
c                         (.) Etc.
c 12 lfilt             I  = True if a filter step must be performed.
c 25 ncelh             I  Actual cell number of a history block of re-
c                         sult file.
c 24 ncelm             I  Actual cell number of a map block of result
c                         file.
c  3 nfpamn            I  Number of main codes of parameter filter
c  6 ngrid             I  Number of grid points in network.
c 23 nkapar            I  = nnf + nnmu + 1
c  4 nkfpmp            I  Number of entries in kfpmap.
c  5 nkfptm            I  Number of entries in kfptim.
c 16 nnf               I  Number of uncertain bed friction parameters.
c 18 nnmu              I  Number of uncertain energy loss parameters in
c                         case of free gate flow.
c 21 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
c  9 nstep             I  Last time step number in simulation.
c 22 p1(np,np)         I  Matrix with covariances of waterlevels,
c                         discharges and uncertain correction parameters
c                         (bed friction, contraction in case of free gate
c                         flow and wind) on time level n+1|n+1 (filtered
c                         values) or n|n (previous time step).
c 17 pfa(nnf)          I  Uncertain bed friction parameters of all
c 19 pmua(nnmu)        I  Uncertain energy loss parameters in case of
c 20 pw                I  Uncertain wind stress parameter.
c 11 writim            P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c flsdat  FLuSh buffers of DATa file
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
c $Log: kafpar.pf,v $
c Revision 1.5  1999/03/15  15:51:48  kuipe_j
c tabs removed
c
c Revision 1.4  1996/12/03  07:59:05  kuipe_j
c dimension of element names improved
c
c Revision 1.3  1996/09/03  14:54:23  kuipe_j
c frequency time hist,etc
c
c Revision 1.2  1996/04/12  13:04:55  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:32  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer      nfpamn ,nkfpmp ,nkfptm ,ngrid  ,juer   ,istep  ,
     &             nstep  ,ncelm  ,ncelh  ,ker    ,
     &             nnf    ,nnmu   ,nkapar ,np
      integer      fd_nefis_res, kfpmap(nkfpmp),
     &             kfptim(nkfptm) ,fpacpr(nfpamn),itim  (2)
      real         pfa(nnf)       ,pmua(nnmu)    ,pw
      real         p1(np,np)      ,buf(nkapar)
      logical      first   ,writim,lfilt
c
c
c     Declaration of local variables
c
      integer      nentri
      parameter   (nentri=2)
      integer      errr  ,i ,j ,k ,ie ,nrerr, igr, nlc, nsk, lastcod
      integer      code  (nentri)   ,usrord(1),
     &             uindex(3)
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
      integer      putrel ,flsdat
      logical      yesmap
      external     putrel ,flsdat ,yesmap
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
     & 1 ,'Correction parameter'          ,'HIS_pf ' ,'pf ', '-' ,
     & 1 ,'Filtered parameter covariance' ,'HIS_pcf' ,'pcf', '-' /
c
      data  grnamm   ,grnamh ,grnamd/
     &      'KFPAR-MAP-GROUP' ,
     &      'KFPAR-HIS-GROUP' ,
     &      'KFPAR-DES-GROUP' /
c 
      lastcod = nentri
c
c     Initialize.
c
      if (first) then
         nrerr = ekaboo
c
         call resadm (nentri ,code   ,fpacpr )
c
         call resini (fd_nefis_res, grnamd ,grnamm ,grnamh ,'KFPAR',
     &                nentri ,nkfpmp ,nkfptm ,nkapar ,itim   ,nameel ,
     &                quanel ,unitel ,descel ,fpacpr ,kfpmap ,kfptim ,
     &                ncelm  ,ncelh  ,nameac ,lastcod,errr   )
c
         if (errr.ne.0) goto 1000
      endif
c
      if (lfilt) then
c
c       Write Map results
c
        if (nkfpmp .gt. 3) then
          if (yesmap(kfpmap(1),kfpmap(2),kfpmap(3),istep)) then
            nrerr = ekamap
c
            call restim (fd_nefis_res, grnamm ,1  ,itim  ,ncelm  ,
     &                   nameac ,errr   )
            if (errr.ne.0) goto 1000
c
            call resdes ('KFPAR',fd_nefis_res, grnamd ,1 ,writim ,
     &                   kfptim ,ncelm  ,ncelh  ,errr   )
            if (errr.ne.0) goto 1000
c
            uindex(1) = ncelm
            uindex(2) = ncelm
            uindex(3) = 1
c
            do 40 i = 4,nkfpmp,2
               ie = fpacpr(kfpmap(i))+kfpmap(i+1)
c
c              The element names of the Map block start with
c              MAP instead of HIS.
c
               name  = 'MAP'//nameel(ie)(4:)
               goto (10,20) , ie
   10          continue
                  k = 1
                  do 12 j = 1, nnf
                     buf(k) = pfa(j)
                     k = k + 1
   12             continue
                  do 14 j = 1, nnmu
                     buf(k) = pmua(j)
                     k = k + 1
   14             continue
                  buf(k) = pw
                  goto 30
   20          continue
                  k = 1
                  do 25 j = ngrid*2+1, np
                     buf(k) = p1(j,j)
                     k = k + 1
   25             continue
                  goto 30
   30          continue
                  errr = putrel (fd_nefis_res, grnamm ,name   ,
     &                           uindex  ,usrord ,buf    )
                  if (errr.ne.0) goto 1000
   40       continue
          endif
        endif
c
c       Write History results.
c
        nlc = kfptim(1)
        new = mod(nkfptm-nlc,2) .eq. 0

        if ( new ) then
           nsk = 3
        else
           nsk = 0
        endif
c
        if (nkfptm .gt. 1+nsk) then
          if (new) then
            newuit = yesmap(kfptim(nlc+2),kfptim(nlc+3),kfptim(nlc+4),
     &                      istep)
          else
            newuit = .false.
          endif
          if ( (kfptim(1).gt.0 .and. .not. new) .or. newuit ) then
            nrerr = ekahis
c
            call restim (fd_nefis_res, grnamh ,1  ,itim  ,ncelh  ,
     &                   nameac ,errr   )
            if (errr.ne.0) goto 1000
c
            call resdes ('KFPAR',fd_nefis_res, grnamd ,2 ,writim ,
     &                   kfptim ,ncelm  ,ncelh  ,errr   )
            if (errr.ne.0) goto 1000
c
            uindex(1) = ncelh
            uindex(2) = ncelh
            uindex(3) = 1
c
            do 80 i = kfptim(1)+2+nsk,nkfptm,2
               ie = fpacpr(kfptim(i))+kfptim(i+1)
               goto (50,60) ,ie
   50          continue
                  do 55 j=1,kfptim(1)
                     igr = kfptim(j+1)
                     if (igr .ge. 1 .and. igr .le. nnf) then
                        buf(j) = pfa(igr)
                     else if (igr .gt. nnf .and. igr .lt. nkapar) then
                        buf(j) = pmua(igr-nnf)
                     else
                        buf(j) = pw
                     endif
   55             continue
                  goto 70
   60          continue
                  do 65 j=1,kfptim(1)
                     igr = kfptim(j+1) + 2*ngrid
                     buf(j) = p1(igr,igr)
   65             continue
                  goto 70
   70          continue
                  errr = putrel (fd_nefis_res, grnamh ,nameel(ie),
     &                           uindex  ,usrord ,buf    )
                  if (errr.ne.0) goto 1000
   80       continue
          endif
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
         call resdes ('KFPAR' ,fd_nefis_res, grnamd ,3 ,llog ,
     &                 kfptim ,ncelm  ,ncelh  ,errr   )
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
      call error (juer ,'KAFPAR @'//txt//'@' ,nrerr ,ker)
c
      end
