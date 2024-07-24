      subroutine flhy3 (fd_nefis_res ,nlaman ,nlatim ,ngrid  ,
     &                  itim   ,istep  ,nstep  ,first  ,writim ,
     &                  juer   ,lattim ,nqlat  ,qlat   ,ncllat ,
     &                  lacpre ,buf    ,ker    ,qltpar ,strhis ,
     &                  nstru  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLHY3 (FLow HYdrodynamic results 3)
c
c Module description: Subroutine FLHY3 writes the third part of the user
c                     selected flow results to the result file. The
c                     result file is processed by the User Interface.
c
c                     In subroutine FLHY3 the user selected water flow
c                     results at user supplied locations and time levels
c                     (System Specifications for 1D Modelling System
c                     Front End) will be stored on the result file. The
c                     stored data can be processed further by the User
c                     Interface.
c
c                     The user can select functions of place or func-
c                     tions of time.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 17 buf(ngrid)        O  Buffer for results to be written to NEFIS.
c  2 dafdrs            P  -
c  1 defdrs            P  -
c  9 first             I  True in case of first call.
c  7 istep             I  Current time step number (t(n+1)).
c  6 itim              P  -
c 11 juer              P  -
c 18 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 16 lacpre(nlaman)    I  lacpre(i) = index in block table (1...nentri)
c                         for main code i of the lateral discharges
c                         results.
c 12 lattim(nlatim)    I  Parameter list for HIST block with hydrodyna-
c                         mic lateral discharge results:
c                         (1)      = Number of places
c                         (2)      = Report grid point 1
c                         (3)      = Report grid point 2
c                         (i)      = Report grid point n
c                         (i+1)    = Report parameter code 1
c                         (i+2)    = Report parameter sub code 1
c                         (nlatim) = Report parameter n sub code
c 15 ncllat            I  Actual cell number of a history block for the
c                         lateral discharges results at the NEFIS re-
c                         sultfile.
c  5 ngrid             I  Number of grid points in network.
c  3 nlaman            I  Number of main codes of lateral discharges
c                         results.
c  4 nlatim            I  Number of entries in lattim.
c 13 nqlat             P  -
c  8 nstep             I  Last time step number in simulation.
c    nstru             P  -
c 14 qlat(nqlat)       I  (i) = Actual lateral discharge in station i on
c                         time level n+1/2.
c 10 writim            P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c flsdat  FLuSh buffers of DATa file
c putrel  PUT Real ELement to a nefis file
c resdes  RESults; DEScription group is defined
c resini  RESults; writing is INItialized
c restim  RESults; writing of current TIMe
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flhy3.pf,v $
c Revision 1.8  1999/03/15  14:24:44  kuipe_j
c Bug fix lateral discharge on t=0
c
c Revision 1.7  1997/11/04  14:17:23  kuipe_j
c Retention basin
c
c Revision 1.6  1997/06/17  11:26:34  kuipe_j
c output in history format
c
c Revision 1.5  1996/12/02  15:41:26  kuipe_j
c dimension for histories improvred
c
c Revision 1.4  1996/09/03  14:51:59  kuipe_j
c frequency time hist,Messages controllers added
c
c Revision 1.3  1995/11/21  14:16:21  kuipe_j
c proper initialization of error code
c
c Revision 1.2  1995/05/30  09:55:05  hoeks_a
c Minor changes
c
c Revision 1.1  1995/04/13  07:07:47  hoeks_a
c Initial check-in
c
c Revision 1.3  1995/03/08  09:07:27  kuipe_j
c Call to resdes improved
c
c Revision 1.2  1993/11/26  15:30:59  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:50  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer      nlaman ,nlatim ,ngrid  ,juer   ,istep   ,
     &             nstep  ,nqlat  ,nceld  ,ncllat ,ker
      integer      fd_nefis_res, itim(2) ,
     &             lattim(nlatim) ,lacpre(nlaman)
      integer      nstru
      real         qlat(*),buf(ngrid)
      real         qltpar(9,*)   ,strhis(13,nstru)
      logical      first   ,writim
c
c     Declaration of local variables
c
      integer      nentri ,nlamap
      parameter   (nentri=2 ,nlamap=1)
      integer      latmap(nlamap)
      integer      errr  ,i ,j ,ie  ,nrerr ,nsk ,nlc, lastcod
      integer      usrord(1),
     &             uindex(3)
      integer      istat ,istru
      character*16 grnamm          ,grnamh          ,grnamd
      character*16 nameel(nentri),quanel(nentri),unitel(nentri),
     &             nameac(nentri+1)
      character*64 descel(nentri)
      character*9  txt
      logical      llog, new       ,newuit
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
      include '..\include\sobcon.i'
c
c     Declaration of external functions
c
      integer      putrel ,flsdat
      logical      yesmap
      external     putrel ,flsdat ,yesmap
c
      data  usrord /1/
c
c     Definition of elements.
c
      data (descel(i) ,nameel(i) ,quanel(i) ,
     &      unitel(i),i=1,nentri) /
c
c      1
     &    'Lateral discharge'               ,'HIS_Qlat' ,'Qlat','m3/s',
     &    'Water level in retention area'   ,'HIS_Hlat' ,'Hlat','m' /
c
      data  grnamm   ,grnamh ,grnamd/
     &      'UNUSED' ,
     &      'FLLAT-HIS-GROUP' ,
     &      'FLLAT-DES-GROUP' /
c
      latmap(1) = 0
      lastcod   = nentri
c
c     Initialize.
c
      if (first) then
         nrerr = eflboo
c
         call resini (fd_nefis_res ,grnamd ,grnamm ,grnamh ,'FLLAT',
     &                nentri ,nlamap ,nlatim ,nqlat  ,itim   ,nameel ,
     &                quanel ,unitel ,descel ,lacpre ,latmap ,lattim ,
     &                nceld  ,ncllat ,nameac ,lastcod,errr   )
c
         if (errr.ne.0) goto 1000
      endif
c
c     Write History results.
c
      nlc = lattim(1)
      new = mod(nlatim-nlc,2) .eq. 0

      if ( new ) then
         nsk = 3
      else
         nsk = 0
      endif
c
      if (nlatim .gt. 1+nsk ) then
         if (new) then
            newuit = yesmap(lattim(nlc+2),lattim(nlc+3),lattim(nlc+4),
     +                      istep)
         else
            newuit = .false.
         endif 
         if ((lattim(1) .gt.0  .and.  .not. new)  .or. newuit) then
            nrerr = eflhis
c
            call restim (fd_nefis_res ,grnamh ,1  ,itim  ,ncllat ,
     &                   nameac ,errr   )
            if (errr.ne.0) goto 1000
c
            call resdes ('FLLAT',fd_nefis_res ,grnamd ,2 ,writim ,
     &                   lattim ,nceld  ,ncllat ,errr   )
            if (errr.ne.0) goto 1000
c
            uindex(1) = ncllat
            uindex(2) = ncllat
            uindex(3) = 1
c
            do 410 i = lattim(1)+2+nsk,nlatim,2
               ie = lacpre(lattim(i)) + lattim(i+1)
               if (ie .eq. 1) then
                  do 225 j=1,lattim(1)
                     buf(j) = qlat(lattim(j+1))
                     if (buf(j) .gt. 1.1e+20) buf(j) = 0.
  225             continue
                  errr = putrel (fd_nefis_res ,grnamh ,nameel(ie) ,
     &                           uindex  ,usrord ,buf    )
                  if (errr.ne.0) goto 1000
               else if (ie .eq. 2) then
                  do 230 j=1,lattim(1)
                     istat = lattim(j+1)
                     if (INT(qltpar(2,istat)).eq.cqlret) then
                       istru = MOD(INT(qltpar(9,istat)), 1000)
                       buf(j) = strhis(13,istru)
                     else               
                       buf(j) = 0.
                     endif      
  230             continue
                  errr = putrel (fd_nefis_res ,grnamh ,nameel(ie) ,
     &                           uindex  ,usrord ,buf    )
                  if (errr.ne.0) goto 1000
               endif
  410       continue
         endif
      endif
c
c     Be sure that at the last step the number of cells has been
c     written correctly.
c
      if (istep.ge.nstep) then
         nrerr = efleoo
         llog  = .true.
c
         call resdes ('FLLAT' ,fd_nefis_res ,grnamd ,2 ,llog ,
     &                 lattim ,nceld  ,ncllat ,errr   )
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
      call error (juer ,'FLHY3 @'//txt//'@' ,nrerr ,ker)
c
      end
