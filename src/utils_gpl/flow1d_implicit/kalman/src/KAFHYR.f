      subroutine KAFHYR(fd_nefis_res, nfhymn ,nkfhmp ,nkfhtm ,ngrid  ,
     &                  itim   ,istep  ,nstep  ,first  ,writim ,lfilt  ,
     &                  juer   ,kfhmap ,kfhtim ,h      ,q      ,np     ,
     &                  p1     ,ncelm  ,ncelh  ,fhycpr ,buf    ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KAFHYR (KAlman Filtered HYdraulic Results)
c
c Module description: Write the user selected Kalman module results to
c                     the result file. The results processed in this
c                     module are filtered mean and covariances of water
c                     levels and discharges.
c
c                     The stored data can be processed further by the
c                     User Interface. The user can select functions of
c                     place or functions of time. See [S-DD-002.1MR] for
c                     a specification of the nefis names.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 23 buf(ngrid)        O  Buffer for results to be written to NEFIS.
c  2 dafdrs            P  -
c  1 defdrs            P  -
c 22 fhycpr(nfhymn)    I  fhycpr(i) = index in block table (1...nentri)
c                         for main code i of the hydrodynamic filter
c 10 first             I  True in case of first call.
c 16 h(ngrid)          I  Contains water levels in every grid point. It is
c                         stored on index 2 of the packed array hpack.
c                         Flow:        current values during iteration
c                         (h*).
c                         Prediction:  last iterated value.
c                         Update:      filtered value (n+1|n+1)
c  8 istep             I  Current time step number (t(n+1)).
c  7 itim              P  -
c 13 juer              P  -
c 24 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 14 kfhmap(*)         I  Parameter list for filter results of water
c                         levels and discharges of MAP block.
c                         (1/2/3) Report step of begin, end, resp.
c                         increment.
c                         (4) Main code parameter 1.
c                         (5) Sub code parameter 1.
c                         (.) Etc.
c 15 kfhtim(*)         I  Parameter list for filter results of water
c                         levels and discharges of HIST block.
c                         (1) Number of places.
c                         (2) Location 1.
c                         (i) Location n.
c                         (i+1) Main code parameter 1.
c                         (i+2) Sub code parameter 1.
c                         (.) Etc.
c 12 lfilt             I  = True if a filter step must be performed.
c 21 ncelh             I  Actual cell number of a history block of re-
c                         sult file.
c 20 ncelm             I  Actual cell number of a map block of result
c                         file.
c  3 nfhymn            I  Number of main codes of hydrodynamic filter
c  6 ngrid             I  Number of grid points in network.
c  4 nkfhmp            I  Number of entries in kfhmap.
c  5 nkfhtm            I  Number of entries in kfhtim.
c 18 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
c  9 nstep             I  Last time step number in simulation.
c 19 p1(np,np)         I  Matrix with covariances of waterlevels,
c                         discharges and uncertain correction parameters
c                         (bed friction, contraction in case of free gate
c                         flow and wind) on time level n+1|n+1 (filtered
c                         values) or n|n (previous time step).
c 17 q(ngrid)          I  Contains discharges in every grid point. It is
c                         stored on index 2 of the packed array qpack.
c                         Flow:        current values during iteration
c                         (h*).
c                         Prediction:  last iterated value.
c                         Update:      filtered value (n+1|n+1)
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
c $Log: kafhyr.pf,v $
c Revision 1.5  1999/03/15  15:51:44  kuipe_j
c tabs removed
c
c Revision 1.4  1996/12/03  07:59:04  kuipe_j
c dimension of element names improved
c
c Revision 1.3  1996/09/03  14:54:22  kuipe_j
c frequency time hist,etc
c
c Revision 1.2  1996/04/12  13:04:51  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:28  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer      nfhymn ,nkfhmp ,nkfhtm ,ngrid  ,juer   ,istep  ,
     &             nstep  ,ncelm  ,ncelh  ,ker    ,np
      integer      fd_nefis_res, kfhmap(nkfhmp),
     &             kfhtim(nkfhtm) ,fhycpr(nfhymn),itim  (2)
      real         p1(np,np)      ,buf(ngrid)
      logical      first   ,writim    ,lfilt
      double precision h(ngrid), q(ngrid)
c
c     Declaration of local variables
c
      integer      nentri
      parameter   (nentri=4)
      integer      errr  ,i ,j ,ie  ,nrerr, igr ,nsk ,nlc ,lastcod 
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
     & 1 ,'Filtered water level'            ,'HIS_hf ' ,'hf ', 'm'    ,
     & 1 ,'Filtered water level covariance' ,'HIS_hcf' ,'hcf', 'm2'   ,
     & 2 ,'Filtered discharge'              ,'HIS_qf ' ,'qf ', 'm3/s' ,
     & 2 ,'Filtered discharge covariance'   ,'HIS_qcf' ,'qcf', 'm6/s2'/
c
      data  grnamm   ,grnamh ,grnamd/
     &      'KFHYD-MAP-GROUP' ,
     &      'KFHYD-HIS-GROUP' ,
     &      'KFHYD-DES-GROUP' /
c
      lastcod = nentri
c
c     Initialize.
c
      if (first) then
         nrerr = ekaboo
c
         call resadm (nentri ,code   ,fhycpr )
c
         call resini (fd_nefis_res, grnamd ,grnamm ,grnamh ,'KFHYD',
     &                nentri ,nkfhmp ,nkfhtm ,ngrid  ,itim   ,nameel ,
     &                quanel ,unitel ,descel ,fhycpr ,kfhmap ,kfhtim ,
     &                ncelm  ,ncelh  ,nameac ,lastcod,errr   )
c
         if (errr.ne.0) goto 1000
      endif
c
      if (lfilt) then
c
c       Write Map results
c
        if (nkfhmp .gt. 3) then
          if (yesmap(kfhmap(1),kfhmap(2),kfhmap(3),istep)) then
            nrerr = ekamap
c
            call restim (fd_nefis_res, grnamm ,1  ,itim  ,ncelm  ,
     &                   nameac ,errr   )
            if (errr.ne.0) goto 1000
c
            call resdes ('KFHYD',fd_nefis_res, grnamd ,1 ,writim ,
     &                   kfhtim ,ncelm  ,ncelh  ,errr   )
            if (errr.ne.0) goto 1000
c
            uindex(1) = ncelm
            uindex(2) = ncelm
            uindex(3) = 1
c
            do 60 i = 4,nkfhmp,2
               ie = fhycpr(kfhmap(i))+kfhmap(i+1)
c
c              The element names of the Map block start with
c              MAP instead of HIS.
c
               name  = 'MAP'//nameel(ie)(4:)
               goto (10,20,30,40) , ie
   10          continue
                  errr = putrel (fd_nefis_res, grnamm ,name   ,
     &                           uindex  ,usrord ,h      )
                  goto 50
   20          continue
                  do 25 igr = 1, ngrid
                     buf(igr) = p1(igr,igr)
   25             continue
                  errr = putrel (fd_nefis_res, grnamm ,name   ,
     &                           uindex  ,usrord ,buf    )
                  goto 50
   30          continue
                  errr = putrel (fd_nefis_res, grnamm ,name   ,
     &                           uindex  ,usrord ,q      )
                  goto 50
   40          continue
                  do 45 igr = 1, ngrid
                     buf(igr) = p1(ngrid+igr,ngrid+igr)
   45             continue
                  errr = putrel (fd_nefis_res, grnamm ,name   ,
     &                           uindex  ,usrord ,buf    )
                  goto 50
   50          continue
                  if (errr.ne.0) goto 1000
   60       continue
          endif
        endif
c
c       Write History results.
c
      nlc = kfhtim(1)
      new = mod(nkfhtm-nlc,2) .eq. 0

      if ( new ) then
         nsk = 3
      else
         nsk = 0
      endif
c
        if (nkfhtm .gt. 1+nsk) then
          if (new) then
            newuit = yesmap(kfhtim(nlc+2),kfhtim(nlc+3),kfhtim(nlc+4),
     &                      istep)
          else
            newuit = .false.
          endif
          if ( (kfhtim(1).gt.0 .and. .not. new) .or. newuit ) then
            nrerr = ekahis
c
            call restim (fd_nefis_res, grnamh ,1  ,itim  ,ncelh  ,
     &                   nameac ,errr   )
            if (errr.ne.0) goto 1000
c
            call resdes ('KFHYD',fd_nefis_res, grnamd ,2 ,writim ,
     &                   kfhtim ,ncelm  ,ncelh  ,errr   )
            if (errr.ne.0) goto 1000
c
            uindex(1) = ncelh
            uindex(2) = ncelh
            uindex(3) = 1
c
            do 120 i = kfhtim(1)+2+nsk,nkfhtm,2
               ie = fhycpr(kfhtim(i))+kfhtim(i+1)
               goto (70,80,90,100) ,ie
   70          continue
                  do 75 j=1,kfhtim(1)
                     igr = kfhtim(j+1)
                     buf(j) = sngl( h(igr) )
   75             continue
                  goto 110
   80          continue
                  do 85 j=1,kfhtim(1)
                     igr = kfhtim(j+1)
                     buf(j) = p1(igr,igr)
   85             continue
                  goto 110
   90          continue
                  do 95 j=1,kfhtim(1)
                     igr = kfhtim(j+1)
                     buf(j) = sngl( q(igr) )
   95             continue
                  goto 110
  100          continue
                  do 105 j=1,kfhtim(1)
                     igr = kfhtim(j+1) + ngrid
                     buf(j) = p1(igr,igr)
  105             continue
                  goto 110
  110          continue
                  errr = putrel (fd_nefis_res, grnamh ,nameel(ie),
     &                           uindex  ,usrord ,buf    )
                  if (errr.ne.0) goto 1000
  120       continue
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
         call resdes ('KFHYD' ,fd_nefis_res, grnamd ,3 ,llog ,
     &                 kfhtim ,ncelm  ,ncelh  ,errr   )
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
      call error (juer ,'KAFHYR @'//txt//'@' ,nrerr ,ker)
c
      end
