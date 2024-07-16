      subroutine parsdt(itim , ijaar ,imaand ,idag, iuur ,imin, isec)  

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          General Module       
c
c Programmer:         A.W.J. Koster      
c
c Module:             ParsDt(Parse Datum)                                 
c
c Module description: Subroutine PARDAT splits up the date into year,   -
c                     month, day, hours, minutes and seconds.         
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 itim              P  SOBEK Time 
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c Initial version
c
c
c***********************************************************************
c
c
c     Parameters
c
      integer       itim(2)
      integer       ijaar  , imaand , idag   , iuur   , imin  , isec
c
      ijaar  =     itim(1)         /10000
      imaand = mod(itim(1),10000  )/100
      idag   = mod(itim(1),100    )
      iuur   =     itim(2)         /1000000
      imin   = mod(itim(2),1000000)/10000
      isec   = mod(itim(2),10000  )/100
c
      return
      end
