!##################################################!
!                   TERRSTRUC                      !
!          TERRESTRIAL PLANET STRUCTURE            !
!              (c) SHENG JIN, PMO                  !
!          Email: sheng.s.jin@gmail.com            !
!##################################################!



PROGRAM TERRSTRUC

USE PARAMETERS

IMPLICIT NONE

INCLUDE 'units.h90'

!Initial conditions
DOUBLE PRECISION                             ::Pout
!DOUBLE PRECISION                            ::Tout,aplanete,Menve,YHe,...

!planet structure
DOUBLE PRECISION,DIMENSION(nvar,nfmax)       ::X
INTEGER                                      ::N_frontieres

!Planet properties
DOUBLE PRECISION                             ::Rstart



!----------------------
!get initial conditions
!----------------------
CALL initcond(Pout)


!--------------------------------
!initialize all kind of variables
!--------------------------------
CALL initialize(X,N_frontieres,Rstart)


!-----------------------
!calculate the structure
!-----------------------
CALL iterater(Pout,X,N_frontieres,Rstart)


!-----------------
!write main output
!-----------------
CALL ecriture(X,n_frontieres)


WRITE(*,*)'TERRSTRUC: Progam terminated. May the DENSITY be with you.'
WRITE(*,*)

END PROGRAM TERRSTRUC


!###############################################################
!###############################################################
!###############################################################

