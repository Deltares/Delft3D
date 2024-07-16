      subroutine FLRSTA(lwqin  ,nqlat  ,nstru  ,ncontr ,ngrid  ,itim   ,
     &                  curtim ,juer   ,first  ,newres ,fd_nefis_rst ,
     &                  fd_nefis_new ,h2     ,q2     ,contrl ,conhis ,
     &                  strhis ,qaggr  ,qlaggr ,ncelst ,inires ,arexop ,
     &                  arexcn ,lagstm ,nlags  ,
     &                  lgrwt  ,grhis  ,buflag ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLRSTA (FLow read or write of ReSTArt information)
c
c Module description: Reading or writing of restart information.
c
c                     A parameter determines if reading or writing takes
c                     place. For an initial run no reading will be done
c                     as there is no information on the file. In that
c                     case only group definitions will be made. When the
c                     user restarts a simulation run from a specific
c                     point of time the saved restart information from a
c                     previous run will be read. Writing can be done at
c                     some interval in time. Previously in the same run
c                     written information will be lost.
c
c                     Pre condition: The NEFIS data and definition files
c                     are opened.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 25 arexcn            P  -
c 24 arexop            P  -
c 18 conhis            P  -
c 17 contrl            P  -
c  7 curtim            P  -
c 14 dafdrn            P  -
c 12 dafdst            P  -
c 13 defdrn            P  -
c 11 defdst            P  -
c  9 first             I  True in case of first call.
c 15 h2                P  -
c 23 inires            I  True when no restart info of this module has
c                         been written before.
c  6 itim              P  -
c  8 juer              P  -
c 26 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c  1 lwqin             P  -
c 22 ncelst            IO Actual cell number of a restart block of the
c                         restart file.
c  4 ncontr            P  -
c 10 newres            I  true, if a new restart file will be made
c  5 ngrid             I  Number of grid points in network.
c  2 nqlat             P  -
c  3 nstru             P  -
c 16 q2                P  -
c 20 qaggr             P  -
c 21 qlaggr            P  -
c 19 strhis            P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c fldfst  FLow DeFine group for reSTart information
c flrest  FLow REading of reSTart information
c flwrst  FLow WRiting of reSTart information
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
c $Log: flrsta.pf,v $
c Revision 1.13  1999/07/23  15:10:21  kuipe_j
c improve restart
c
c Revision 1.12  1998/06/18  13:32:16  kuipe_j
c Bug in resart flag solved
c
c Revision 1.11  1998/06/08  12:29:49  kuipe_j
c time lag hydr controller
c
c Revision 1.10  1996/12/02  15:41:27  kuipe_j
c dimension for histories improvred
c
c Revision 1.9  1996/01/17  14:38:48  kuipe_j
c header update
c
c Revision 1.8  1996/01/16  15:01:24  kuipe_j
c Restart improvements
c
c Revision 1.7  1995/10/18  08:59:26  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.6  1995/09/22  10:02:13  kuipe_j
c variable dimensions, new headers
c
c Revision 1.5  1995/09/12  08:11:03  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.4  1995/08/30  12:36:52  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.3  1995/05/30  09:55:26  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:59:26  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:08:06  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:31:32  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:55  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Include constants for array dimensions
c
      include '..\include\sobdim.i'
c
c     Declaration of parameters
c
      integer      nqlat  ,nstru   ,ncontr ,ngrid  ,ncelst ,
     &             lagstm ,nlags   ,juer   ,ker
      integer      fd_nefis_rst, fd_nefis_new, itim  (2)  ,
     &             arexop(*)       ,arexcn(ngrid,2)
      real         curtim
      real         conhis(5,*),strhis(dmstrh,*),contrl(17,*)   ,
     &             buflag(lagstm,nlags)            ,
     &             qaggr(ngrid,3)  ,qlaggr(*)      , grhis(*)
      logical      lwqin  ,first   ,inires, newres ,lgrwt
      double precision h2(ngrid), q2(ngrid)

c     Declaration of local variables
c
      integer      nentri
      parameter   (nentri=10)
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
     & 1,'Restart time step'              ,'ISTEP'  ,'t'  ,'-'    ,
     & 1,'Water levels on t=n+1'          ,'H2'     ,'h'  ,'m'    ,
     & 1,'Discharges on t=n+1'            ,'Q2'     ,'Q'  ,'m3/s' ,
     & 2,'Structure history'              ,'STRHIS' ,'<>' ,'-'    ,
     & 2,'Control history'                ,'CONHIS' ,'<>' ,'-'    ,
     & 2,'Aggregated flows WQ-interface'  ,'QAGGR'  ,'Q'  ,'m3/s' ,
     & 1,'Aggregated lateral discharges'  ,'QLAGGR' ,'Q'  ,'m3/s' ,
     & 2,'Status variable for extra area' ,'AREXCN' ,'<>' ,'-'    ,
     & 2,'Time lag in discharges        ' ,'BUFLAG' ,'<>' ,'-'    ,
     & 3,'Groundwater history           ' ,'GRHIS'  ,'<>' ,'-'    /
c
      data  grnamf   /
     &      'FLOW-RES-GROUP' /
c
      errr = 0
c
      if (first) then
c
c        Determine if it is an initial run or a restart run for this
c        module. In the latter case restart information must exist
c        already.
c
         nrerr = eflbor
         call FLDFST (fd_nefis_rst ,grnamf ,nentri ,nstru  ,ncontr ,
     &                nqlat  ,ngrid  ,lwqin  ,arexop ,ndim   ,nameel ,
     &                quanel ,unitel ,descel ,.not.newres    ,inires ,
     &                nameac ,lagstm ,nlags  ,lgrwt  ,errr   )
         if (errr.ne.0) goto 1000
c
         if (inires) then
            if (newres) then
c
c              Initialize a new restart file
c
               nrerr = eflbor
               call FLDFST (fd_nefis_new ,grnamf ,nentri ,nstru  ,
     &                      ncontr ,nqlat  ,ngrid  ,lwqin  ,arexop ,
     &                      ndim   ,nameel ,quanel ,unitel ,descel ,
     &                      .true. ,inidum ,nameac ,lagstm ,nlags  ,
     &                      lgrwt  ,errr   )
               if (errr.ne.0) goto 1000
            endif
            ncelst = 1
         else
c
c           Read from restart file. Select proper time step first.
c
            nrerr = eflrrd
            call STATIM (fd_nefis_rst ,grnamf ,nameel(1),itim,ncelst,
     &                   errr   )
            if (errr.ne.0) goto 1000
c
            call FLREST (fd_nefis_rst ,grnamf ,nentri ,nstru ,ncontr ,
     &                   ngrid  ,lwqin  ,ncelst ,nameel ,h2     ,q2    ,
     &                   conhis ,strhis ,qaggr  ,qlaggr ,arexop ,arexcn,
     &                   lagstm ,nlags  ,buflag ,lgrwt  ,grhis  ,errr  )
            if (errr.ne.0) goto 1000
c
            if (newres) then
c
c              Initialize a new restart file
c
               nrerr = eflbor
               call FLDFST (fd_nefis_new ,grnamf ,nentri ,nstru  ,
     &                      ncontr ,nqlat  ,ngrid  ,lwqin  ,arexop ,
     &                      ndim   ,nameel ,quanel ,unitel ,descel ,
     &                      .true. ,inidum ,nameac ,lagstm ,nlags  ,
     &                      lgrwt  ,errr   )
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
         nrerr = eflwrd
         call FLWRST (fd_nefis_rst ,grnamf ,nentri ,nstru  ,ncontr ,
     &                ngrid  ,lwqin  ,ncelst ,itim   ,curtim ,nameel ,
     &                h2     ,q2     ,contrl ,conhis ,strhis ,qaggr  ,
     &                qlaggr ,arexop ,arexcn ,lagstm ,nlags  ,buflag ,
     &                lgrwt  ,grhis  ,errr   )
         if (errr.ne.0) goto 1000
         ncelst = ncelst+1
c
      endif
      goto 1010
c
 1000 continue
      if (errr.gt.0) then
         if (errr.eq.4) then
c           Size of buffer has been changed
            call error (juer ,
     &       'FLRSTA  Size of discharge buffer changed at restart' ,
     &       efllag ,ker)
         else
c
c           Could not found restart time step.
c
            call error (juer ,'FLRSTA' ,eflrtt ,ker)
         endif
         ker = fatal
      else
c
c        NEFIS error ( <0 )
         ker = fatal
         write (txt,'(i8)') errr
         call error (juer ,'FLRSTA @'//txt//'@' ,nrerr ,ker)
      endif
c
 1010 continue
      end
