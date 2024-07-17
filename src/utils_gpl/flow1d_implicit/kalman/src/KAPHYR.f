      subroutine KAPHYR(fd_nefis_res ,nphymn ,nkphmp ,nkphtm ,ngrid  ,
     &                  itim   ,istep  ,nstep  ,first  ,writim ,juer   ,
     &                  kphmap ,kphtim ,pi     ,np     ,p2     ,
     &                  ncelm  ,ncelh  ,phycpr ,buf    ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KAPHYR (KAlman Predicted HYdraulic Results)
c
c Module description: Write the user selected Kalman module results to
c                     the result file. The results processed in this
c                     module are predicted covariances of water levels
c                     and discharges.
c
c                     The stored data can be processed further by the
c                     User Interface. The user can select functions of
c                     place or functions of time. See [S-DD-002.1MR] for
c                     a specification of the nefis names.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 21 buf(ngrid)        O  Buffer for results to be written to NEFIS.
c  2 dafdrs            P  -
c  1 defdrs            P  -
c 10 first             I  True in case of first call.
c  8 istep             I  Current time step number (t(n+1)).
c  7 itim              P  -
c 12 juer              P  -
c 22 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 13 kphmap(*)         I  Parameter list for prediction results of water
c                         levels and discharges of MAP block.
c                         (1/2/3) Report step of begin, end, resp.
c                         increment.
c                         (4) Main code parameter 1.
c                         (5) Sub code parameter 1.
c                         (.) Etc.
c 14 kphtim(*)         I  Parameter list for prediction results of water
c                         levels and discharges of HIST block.
c                         (1) Number of places.
c                         (2) Location 1.
c                         (i) Location n.
c                         (i+1) Main code parameter 1.
c                         (i+2) Sub code parameter 1.
c                         (.) Etc.
c 19 ncelh             I  Actual cell number of a history block of re-
c                         sult file.
c 18 ncelm             I  Actual cell number of a map block of result
c                         file.
c  6 ngrid             I  Number of grid points in network.
c  4 nkphmp            I  Number of entries in kphmap.
c  5 nkphtm            I  Number of entries in kphtim.
c 16 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
c  3 nphymn            I  Number of main codes of hydrodynamic prediction
c  9 nstep             I  Last time step number in simulation.
c 17 p2(np,np)         I  Matrix with covariances of waterlevels,
c                         discharges and uncertain correction parameters
c                         (bed friction, contraction in case of free gate
c                         flow and wind) on time level n+1|n (predicted
c                         values).
c 20 phycpr(nphymn)    I  phycpr(i) = index in block table (1...nentri)
c                         for main code i of the hydrodynamic prediction
c 15 pi                I  prediction interval (number of prediction steps)
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
c $Log: kaphyr.pf,v $
c Revision 1.5  1999/03/15  15:52:03  kuipe_j
c tabs removed
c
c Revision 1.4  1996/12/03  07:59:07  kuipe_j
c dimension of element names improved
c
c Revision 1.3  1996/09/03  14:54:26  kuipe_j
c frequency time hist,etc
c
c Revision 1.2  1996/04/12  13:05:13  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:46  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer      nphymn ,nkphmp ,nkphtm ,ngrid  ,juer   ,istep  ,
     &             nstep  ,ncelm  ,ncelh  ,ker    ,pi     ,np
      integer      fd_nefis_res, kphmap(nkphmp),
     &             kphtim(nkphtm) ,phycpr(nphymn),itim  (2)
      real         buf(ngrid)     ,p2(np,np)
      logical      first   ,writim
c
c     Declaration of local variables
c
      integer      nentri
      parameter   (nentri=3)
      integer      errr  ,i ,j ,ie  ,nrerr, igr     ,nsk ,nlc ,lastcod
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
     & 1 ,'Prediction interval'              ,'HIS_pi ' ,'pi ', '-'    ,
     & 2 ,'Predicted water level covariance' ,'HIS_hcp' ,'hcp', 'm2'   ,
     & 3 ,'Predicted discharge covariance'   ,'HIS_qcp' ,'qcp', 'm6/s2'/
c
      data  grnamm   ,grnamh ,grnamd/
     &      'KPHYD-MAP-GROUP' ,
     &      'KPHYD-HIS-GROUP' ,
     &      'KPHYD-DES-GROUP' /
c
      lastcod = nentri
c
c     Initialize.
c
      if (first) then
         nrerr = ekaboo
c
         call resadm (nentri ,code   ,phycpr )
c
         call resini (fd_nefis_res, grnamd ,grnamm ,grnamh ,'KPHYD',
     &                nentri ,nkphmp ,nkphtm ,ngrid  ,itim   ,nameel ,
     &                quanel ,unitel ,descel ,phycpr ,kphmap ,kphtim ,
     &                ncelm  ,ncelh  ,nameac ,lastcod,errr   )
c
         if (errr.ne.0) goto 1000
      endif
c
c     Write Map results
c
      if (nkphmp .gt. 3) then
         if (yesmap(kphmap(1),kphmap(2),kphmap(3),istep)) then
            nrerr = ekamap
c
            call restim (fd_nefis_res, grnamm ,1  ,itim  ,ncelm  ,
     &                   nameac ,errr   )
            if (errr.ne.0) goto 1000
c
            call resdes ('KPHYD',fd_nefis_res, grnamd ,1 ,writim ,
     &                   kphtim ,ncelm  ,ncelh  ,errr   )
            if (errr.ne.0) goto 1000
c
            uindex(1) = ncelm
            uindex(2) = ncelm
            uindex(3) = 1
c
            do 50 i = 4,nkphmp,2
               ie = phycpr(kphmap(i))+kphmap(i+1)
c
c              The element names of the Map block start with
c              MAP instead of HIS.
c
               name  = 'MAP'//nameel(ie)(4:)
               goto (10,20,30) , ie
   10          continue
                  goto 40
   20          continue
                  do 25 igr = 1, ngrid
                     buf(igr) = p2(igr,igr)
   25             continue
                  goto 40
   30          continue
                  do 35 igr = 1, ngrid
                     buf(igr) = p2(ngrid+igr,ngrid+igr)
   35             continue
                  goto 40
   40          continue
                  if (ie .eq. 1) then
                     ibuf(1) = pi
                     errr = putiel (fd_nefis_res, grnamm ,name   ,
     &                              uindex ,usrord ,ibuf   )
                  else
                     errr = putrel (fd_nefis_res, grnamm ,name   ,
     &                              uindex ,usrord ,buf    )
                  endif
                  if (errr.ne.0) goto 1000
   50       continue
         endif
      endif
c
c     Write History results.
c
      nlc = kphtim(1)
      new = mod(nkphtm-nlc,2) .eq. 0

      if ( new ) then
         nsk = 3
      else
         nsk = 0
      endif
c
        if (nkphtm .gt. 1+nsk) then
          if (new) then
            newuit = yesmap(kphtim(nlc+2),kphtim(nlc+3),kphtim(nlc+4),
     &                      istep)
          else
            newuit = .false.
          endif
          if ( (kphtim(1).gt.0 .and. .not. new) .or. newuit ) then
            nrerr = ekahis
c
            call restim (fd_nefis_res, grnamh ,1  ,itim  ,ncelh  ,
     &                   nameac ,errr   )
            if (errr.ne.0) goto 1000
c
            call resdes ('KPHYD',fd_nefis_res, grnamd ,2 ,writim ,
     &                   kphtim ,ncelm  ,ncelh  ,errr   )
            if (errr.ne.0) goto 1000
c
            uindex(1) = ncelh
            uindex(2) = ncelh
            uindex(3) = 1
c
            do 110 i = kphtim(1)+2+nsk,nkphtm,2
               ie = phycpr(kphtim(i))+kphtim(i+1)
               goto (70,80,90) ,ie
   70          continue
                  goto 100
   80          continue
                  do 85 j=1,kphtim(1)
                     igr = kphtim(j+1)
                     buf(j) = p2(igr,igr)
   85             continue
                  goto 100
   90          continue
                  do 95 j=1,kphtim(1)
                     igr = kphtim(j+1)
                     buf(j) = p2(igr+ngrid,igr+ngrid)
   95             continue
                  goto 100
  100          continue
                  if (ie .eq. 1) then
                     ibuf(1) = pi
                     errr = putiel (fd_nefis_res, grnamh ,nameel(ie),
     &                              uindex ,usrord ,ibuf   )
                  else
                     errr = putrel (fd_nefis_res, grnamh ,nameel(ie),
     &                              uindex ,usrord ,buf    )
                  endif
                  if (errr.ne.0) goto 1000
  110       continue
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
         call resdes ('KPHYD' ,fd_nefis_res, grnamd ,3 ,llog ,
     &                 kphtim ,ncelm  ,ncelh  ,errr   )
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
      call error (juer ,'KAPHYR @'//txt//'@' ,nrerr ,ker)
c
      end
