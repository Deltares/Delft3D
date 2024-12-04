subroutine parsdt(itim , ijaar ,imaand ,idag, iuur ,imin, isec)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          General Module
!
! Programmer:         A.W.J. Koster
!
! Module:             ParsDt(Parse Datum)
!
! Module description: Subroutine PARDAT splits up the date into year,   -
!                     month, day, hours, minutes and seconds.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 itim              P  SOBEK Time
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! Initial version
!
!
!***********************************************************************
!
!
!     Parameters
!
   integer       itim(2)
   integer       ijaar  , imaand , idag   , iuur   , imin  , isec
!
   ijaar  =     itim(1)         /10000
   imaand = mod(itim(1),10000  )/100
   idag   = mod(itim(1),100    )
   iuur   =     itim(2)         /1000000
   imin   = mod(itim(2),1000000)/10000
   isec   = mod(itim(2),10000  )/100
!
   return
end
