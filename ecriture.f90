SUBROUTINE ecriture(X,n_frontieres)
!write main output file

USE PARAMETERS,ONLY: nfmax,nvar
USE FRACT
USE PARAEOS

IMPLICIT NONE
INCLUDE 'units.h90'
DOUBLE PRECISION,DIMENSION(nvar+1,nfmax),INTENT(IN)     ::X
INTEGER,INTENT(IN)                                      ::n_frontieres

INTEGER                                                 ::i
INTEGER                                                 ::n_frontieres_actual


LOGICAL,PARAMETER                                       ::DEBUG=.TRUE.

DOUBLE PRECISION                                        ::eta,rho

INTEGER                                                 ::iLayer
! iLayer: Core, Mantle, or Water
! 3 = Core, 2 = Mantle, 1 = Water


!internal structure: remove the negative mass and pressure part

n_frontieres_actual=n_frontieres

IF(n_frontieres>1)THEN
   DO i=1,n_frontieres
   IF( (X(1,i).gt.0) .and. (X(2,i).gt.0) .and. (X(3,i).gt.0) )THEN

      IF( X(3,i)/Mtot .lt. frac_core )THEN
          iLayer=3
          CALL RTSEC(K0_core,K0d_core,X(2,i),eta,iEOS_core)
          rho=rho0_core*eta
          WRITE(1,'(100G19.9)') i,X(1,i)/Rearth,X(3,i)/mearth,X(2,i)/Bar,iLayer,rho
      ELSE IF( X(3,i)/Mtot .lt. (frac_mantle+frac_core) )THEN
          iLayer=2
          CALL RTSEC(K0_mantle,K0d_mantle,X(2,i),eta,iEOS_mantle)
          rho=rho0_mantle*eta
          WRITE(1,'(100G19.9)') i,X(1,i)/Rearth,X(3,i)/mearth,X(2,i)/Bar,iLayer,rho
      ELSE
          iLayer=1
          CALL RTSEC(K0_water,K0d_water,X(2,i),eta,iEOS_water)
          rho=rho0_water*eta
          WRITE(1,'(100G19.9)') i,X(1,i)/Rearth,X(3,i)/mearth,X(2,i)/Bar,iLayer,rho
      END IF 


   ELSE
      n_frontieres_actual=n_frontieres_actual-1
   END IF
   END DO
ELSE
   WRITE(*,*)'n_frontieres = 1: ONLY 1 layer?'
   WRITE(*,*)
END IF

IF(DEBUG)WRITE(*,*)'n_frontieres,n_frontieres_actual',n_frontieres,n_frontieres_actual
IF(DEBUG)WRITE(*,*)

RETURN
END SUBROUTINE ecriture

