C     4X4 JACOBIAN THEN CONDENSATION WITH REGULARIZATION 
C     (energy due to viscous regularization is calculated)
      SUBROUTINE umat_linde(stre,xstate,ddsdde,sse,spd,scd,rpl,
     &  ddsddt,drplde,drpldt,stran,dstran,
     &  abqtime,dtime,temp,dtemp,predef,dpred,amat,ndi,
     &  nshr,ntens,nstate_,elconloc, nprops,pgauss,drot,
     &  pnewdt,celent,xokl,xkl,iel,iint,layer,kspt,jstep,kinc)

C     
      implicit none
C     
      CHARACTER*80 amat

      integer :: iel,iint,nstate_, i,j, ntens, kinc, ndi

      real*8 elconloc(*),emec(6),emec0(6),beta(6),stre(6),stiff(21),
     &  vj,t1l,dtime,xkl(3,3),xokl(3,3),voj,pgauss(3),
     &  time,ttime,pnewdt,ddsdde(ntens,ntens),sse,spd,scd,rpl,
     & ddsddt,drplde,drpldt,STRAN(ntens),DSTRAN(ntens),
     & abqtime,temp,dtemp,predef,dpred,celent,nshr,nprops,
     & drot,layer,kspt,jstep,test(ntens,ntens),xstate(nstate_)

      real*8 ZERO, ONE, TWO, HALF

      real*8 ATEMP,DF,DFOLD,DFV,DCDDM(NTENS,NTENS),DFVOLD,DM,
     & DMOLD, DMV, DMVOLD,EPICL,EPICT,EPISLT,EPITL,EPITT,ETA,GFFEB,
     & GFMAT,SHRLT,SHRTT,GFFIB,SIGCL,SIGCT,SIGSLT,SIGTL,SIGTT,TENL,
     & TENT,XNULT,XNUTL,XNUTT,STRANT(NTENS),CFULL(NTENS,NTENS),
     & CDFULL(NTENS,NTENS),CDTHREE(NTENS,NTENS),OLD_STRESS(NTENS),
     & DOLD_STRESS(NTENS),D_STRESS(NTENS),ATEMP1(NTENS),
     & ATEMP2(NTENS),TSTRANT(NTENS),TDDSDDE(NTENS,NTENS),
     & DDFDE(NTENS),DDMDE(NTENS),DCDDF(NTENS,NTENS),D,DDDFN,DDFDFFN,
     & DDMDFMN,EPIT,FFN,FMN,GF,TERM1,TERM2,TERM3,TERM,DFMNDE(6), 
     & DFFNDE(6)  

C****************************
C     STRANT..... STRAIN AT THE END OF THE INCREMENT
C     TSTRANT.....TEMPORARY ARRAY TO HOLD THE STRAIN FOR PLANE STRESS PROBLEM
C     CFULL.......FULL 6X6 ELASTICITY MATRIX
C     CDFULL......FULL 6X6 DAMAGED ELASTICITY MATRIX
C     DDFDE....... D DF/D E
C     DDMDE....... D DM/D E
C     DCDDF....... D C/ D DF THE DERIVATIVE OF THE FULL MATRIX OVER DF
C     DCDDM........D C/ D DM THE DERIVATIVE OF THE FULL MATRIX OVER DM
C     ATEMP1,ATEMP2...TEMPORARY ARRAY USED IN JACOBIAN CALCULATION
C     TDDSDDE.....UNCONDENSED JACOBIAN MATRIX FOR PLANE STRESS PROBLEM
C     OLD_STRESS...STRESS AT THE BEGINNING OF THE INCREMENT, SAVED FOR THE ENERGY
C                  COMPUTATION
C     DOLD_STRESS...STRESS AT THE BEGINNING OF THE INCREMENT, 
C                  IF THERE'S NO VISCOUS REGULARIZATION
C     D_STRESS...STRESS IF THERE'S NO VISCOUS REGULARIZATION, THE ABOVE IS CALCULATED
C                TO CALCULATE THE SCD, ENERGY CAUSED BY VISCOUS REGULARIZATION
C     STATEV(1)   damage variable df
C     STATEV(2)   damage variable dm
C     STATEV(3)   regularized damage variable dfv
C     STATEV(4)   regularizaed damage variable dmv
C     STATEV(5:10) TEMPORARY ARRAYS TO SAVE DOLD_STRESS
C************
C
C     GET THE MATERIAL PROPERTIES---ENGINEERING CONSTANTS
C
      TENL = elconloc(1)           !YOUNG'S MODULUS IN DIRECTION 1 (L)
      TENT = elconloc(2)           !YOUNG'S MODULUS IN DIRECTION 2 (T)
      SHRLT = elconloc(3)          !SHEAR MODULUS IN 12 PLANE
      SHRTT = elconloc(4)          !SHEAR MODULUS IN 23 PLANE
      XNULT = elconloc(5)          !POISON'S RATIO POI_12
      XNUTT = elconloc(6)          !POISON'S RATIO POI_23
      XNUTL = XNULT / TENL * TENT !POI_21
C     
C     GET THE FAILURE PROPERTIES
C
      SIGTL = elconloc(7)          !FAILURE STRESS IN 1 DIRECTION IN TENSION
      SIGCL = elconloc(8)          !FAILURE STRESS IN 1 DIRECTION IN COMPRESSION
      SIGTT = elconloc(9)          !FAILURE STRESS IN 2 DIRECTION IN TENSION
      SIGCT = elconloc(10)          !FAILURE STRESS IN 2 DIRECTION IN COMPRESSION
      SIGSLT = elconloc(11)        !FAILURE STRESS IN SHEAR IN 1-2 PLANE
      GFMAT = elconloc(12)         !FRACTURE ENERGY IN MATRIX
      GFFIB = elconloc(13)         !FRACTURE ENERGY IN FIBER
      ETA = elconloc(14)           ! VISCOSITY FOR REGULARIZATION

      ZERO = 0.0
      ONE = 1.0
      TWO = 2.0
      HALF = 0.5

C     CALCULATE THE STRAIN AT THE END OF THE INCREMENT

      open(1, file = 'damage.txt')
C     
      DO I = 1, NTENS
         STRANT(I) = STRAN(I) + DSTRAN(I)
      END DO
     
C     FILL THE 6X6 FULL STIFFNESS MATRIX
      DO I = 1, 6
         DO J = 1, 6
            CFULL(I,J)=ZERO
         END DO
      END DO
      ATEMP = ONE - TWO * XNULT * XNUTL - XNUTT ** TWO
     &     - TWO * XNULT * XNUTL * XNUTT
      CFULL(1,1) = TENL * (ONE - XNUTT ** TWO) / ATEMP
      CFULL(2,2) = TENT * (ONE - XNULT * XNUTL) / ATEMP
      CFULL(3,3) = CFULL(2,2)
      CFULL(1,2) = TENT * (XNULT + XNULT * XNUTT) / ATEMP
      CFULL(1,3) = CFULL(1,2)
      CFULL(2,3) = TENT * (XNUTT + XNULT * XNUTL) / ATEMP
      CFULL(4,4) = SHRLT
      CFULL(5,5) = SHRLT
      CFULL(6,6) = SHRTT
      DO I = 2, 6
         DO J = 1, I-1
            CFULL(I,J) = CFULL(J,I)
         END DO
      END DO

c     calculate the failure strain by failure stress

      EPITL = SIGTL / cfull(1,1) !FAILURE STRAIN 1 DIRECTION IN TENSION
      EPICL = SIGCL / cfull(1,1) !FAILURE STRAIN 1 DIRECTION IN COMPRESSION
      EPITT = SIGTT / cfull(2,2) !TENSILE FAILURE STRAIN 2 DIRECTION
      EPICT = SIGCT / cfull(2,2) !COMPRESSIVE FAILURE STRAIN 2 DIRECTION
      EPISLT = SIGSLT/ SHRLT    ! FAILURE SHEAR STRAIN ...ENGINEERING STRAIN
     
C     CHECK THE FAILURE INITIATION CONDITION
     
      DFOLD = xstate(1)
      DMOLD = xstate(2)
      DFVOLD = xstate(3)
      DMVOLD = xstate(4)


C      CALL CheckFailureIni(EPITL,EPICL,EPITT,EPICT,EPISLT,STRANT,
C     &     GFMAT,GFFIB, CELENT, CFULL, DF, DM, DDFDE, DDMDE, NTENS,
C     &  DFOLD, DMOLD,NDI)

C     The upacked CheckFailureIni subroutine is below...

      TERM1 = STRANT(2)**TWO / EPICT / EPITT
      TERM2 = (EPICT - EPITT) / EPICT / EPITT * STRANT(2)
      IF (NDI .EQ. 3) THEN
         TERM3 = (STRANT(4))**TWO / EPISLT**TWO
      ELSE IF (NDI .EQ. 2) THEN
         TERM3 = (STRANT(3))**TWO / EPISLT**TWO
      END IF
      TERM = TERM1 + TERM2 + TERM3
      IF (TERM .GT. ZERO) THEN
         FMN = SQRT(TERM)
      ELSE
         FMN = ZERO
      END IF
C
C     INITIALIZE THE ARRAY AND VARIABLE
C
      DM = ZERO
      DDMDFMN = ZERO
      DO I = 1, 6
         DFMNDE(I) = ZERO
         DDMDE(I) = ZERO
      END DO

      IF (FMN .GT. ONE) THEN
         write(1, *) 'Matrix Damage:',FMN, 'at', kinc
C     CALCULATE DM, DDMDFMN
         TERM1 = CFULL(2,2) * EPITT**2 * CELENT / GFMAT
         TERM2 = (ONE - FMN) * TERM1
         DM = ONE - EXP(TERM2) / FMN
C     CALCULATE THE DERIVATIVE OF DAMAGE VARIABLE WITH RESPECT TO FAILURE
C     RITERION
         DDMDFMN = (ONE / FMN + TERM1) * (ONE - D)
C     CALCULATE DFMNDE
         IF (DM .GT. DMOLD) THEN
            DFMNDE(2) = HALF / FMN * (TWO * STRANT(2) + EPICT - EPITT)
     &           / EPICT / EPITT
            IF (NDI .EQ. 3) THEN
               DFMNDE(4) = ONE / FMN * STRANT(4) / EPISLT**TWO
            ELSE IF (NDI .EQ. 2) THEN
               DFMNDE(4) = ONE / FMN * STRANT(3) / EPISLT**TWO
            END IF

            DO I = 1, 6
               DDMDE(I) = DFMNDE(I) * DDMDFMN
            END DO
         END IF
      END IF
      DM = MAX (DM, DMOLD)
C     
C     CHECK THE INITIATION CONDITION FOR FIBER
C     FFN=FF/EPITL>1 THEN CALCULATE THE DAMAGE VARIABLE AND DERIVATIVE
C     
      TERM1 = STRANT(1)**TWO / EPICL / EPITL
      TERM2 = (EPICL - EPITL) / EPICL / EPITL *STRANT(1)
         TERM = TERM1 + TERM2
         IF (TERM .GT. ZERO) THEN
            FFN = SQRT(TERM)
         ELSE
            FFN = ZERO
         END IF
      DF = ZERO
      DDFDFFN = ZERO
      DO I = 1, 6
            DFFNDE(I) = ZERO
            DDFDE(I) = ZERO
      END DO
      IF (FFN .GT. ONE) THEN
            write(1, *) 'Fiber Damage:',FFN, 'at', kinc
C     CALCULATE DF, DDFDFFN
            TERM1 = CFULL(1,1) * EPITL**2 * CELENT / GFFIB
            TERM2 = (ONE - FFN) * TERM1
            DF = ONE - EXP(TERM2) / FFN
C     CALCULATE THE DERIVATIVE OF DAMAGE VARIABLE WITH RESPECT TO FAILURE
C     RITERION
            DDFDFFN = (ONE / FFN + TERM1) * (ONE - D)
C     CALCULATE DFFNDE
            IF (DF .GT. DFOLD) THEN
               DFFNDE(1) = HALF/FFN*(TWO*STRANT(1)+EPICL-EPITL)/
     &         EPICL/EPITL    
               DDFDE(1) = DFFNDE(1) * DDFDFFN
            END IF
      END IF
      DF = MAX (DF, DFOLD)

   
C     ! USE VISCOUS REGULARIZATION
     
      DFV = ETA / (ETA + DTIME) * DFVOLD + DTIME / (ETA + DTIME) * DF
      DMV = ETA / (ETA + DTIME) * DMVOLD + DTIME / (ETA + DTIME) * DM
C     SAVE THE OLD STRESS TO OLD_STRESS
      DO I = 1, NTENS
         OLD_STRESS(I) = stre(I)
      END DO

C     CALL ROUTINE TO CALCULATE THE STRESS
C     CALCULATE THE STRESS IF THERE'S NO VISCOUS REGULARIZATION
      CALL GetStress(CFULL,CDFULL,DF,DM,D_STRESS,STRANT,NDI,NTENS)

C     CALCULATE THE STRESS IF THERE'S VISCOUS REGULARIZATION
      CALL GetStress(CFULL,CDFULL,DFV,DMV,stre,STRANT,NDI,NTENS)
C     GET THE OLD STRESS IF THERE'S NO VISCOUS REGULARIZATION
      DO I=1,NTENS
         DOLD_STRESS(I)=xstate(I+4)
      END DO
C     SAVE THE CURRENT STRESS IF THERE'S NO VISCOUS REGULARIZATION
      DO I=1,NTENS
         xstate(I+4)=D_STRESS(I)
      END DO
     
C     CALCULATE THE DERIVATIVE MATRIX DC/DDM, DC/DDF OF THE DAMAGED MATRIX
     
      CALL ElasticDerivative(CFULL,DMV,DFV, DCDDM,DCDDF)
     
C     UPDATE THE JACOBIAN
     
C     FULL 3D CASE
      IF (NDI .EQ. 3) THEN
         DO I = 1, NTENS
            ATEMP1(I) = ZERO
            DO J = 1, NTENS
               ATEMP1(I) = ATEMP1(I) + DCDDM(I,J) * STRANT(J)
            END DO
         END DO
         
         DO I = 1, NTENS
            ATEMP2(I) = ZERO
            DO J = 1, NTENS
               ATEMP2(I) = ATEMP2(I) + DCDDF(I,J) * STRANT(J)
            END DO
         END DO
         
         DO I = 1, NTENS
            DO J = 1, NTENS
               DDSDDE(I,J)=CDFULL(I,J) + ( ATEMP1(I) * DDMDE(J)
     &              + ATEMP2(I) * DDFDE(J) ) * DTIME / (DTIME + ETA)
            END DO
         END DO
     
C     PLANE STRESS CASE
     
      ELSE IF (NDI .EQ.2) THEN
         TSTRANT(1) = STRANT(1)
         TSTRANT(2) = STRANT(2)
         TSTRANT(3) = -CDFULL(1,3) / CDFULL(3,3) * STRANT(1)
     &        - CDFULL(2,3) / CDFULL(3,3) * STRANT(2)
         TSTRANT(4) = STRANT(3)
         DO I = 1, 4
            ATEMP1(I) = ZERO
            DO J = 1, 4
               ATEMP1(I) = ATEMP1(I) + DCDDM(I,J) * TSTRANT(J)
            END DO
         END DO
         
         DO I = 1, 4
            ATEMP2(I) = ZERO
            DO J = 1, 4
               ATEMP2(I) = ATEMP2(I) + DCDDF(I,J) * TSTRANT(J)
            END DO
         END DO
         DO I = 1,6
            DO J = 1,6
            TDDSDDE(I,J) = ZERO
            END DO
         END DO
C     TO GET THE UNCONDENSED JACOBIAN FOR PLANE STRESS CASE
         DO I = 1, NTENS
            DO J = 1, NTENS
               DDSDDE(I,J) = ZERO
            END DO
         END DO
         DO I = 1, 4
            DO J = 1, 4
               TDDSDDE(I,J)=CDFULL(I,J) + ( ATEMP1(I) * DDMDE(J)
     &              + ATEMP2(I) * DDFDE(J) ) * DTIME / (DTIME + ETA)
            END DO
         END DO
     
C     CONDENSE THE JACOBIAN MATRIX FOR PLANE STRESS PROBLEM
     
         CALL MatrixCondense(TDDSDDE,DDSDDE)
      END IF 
     
C     TO UPDATE THE STATE VARIABLE
     
      xstate(1) = DF
      xstate(2) = DM
      xstate(3) = DFV
      xstate(4) = DMV
      
     
C     TO COMPUTE THE ENERGY
     
      DO I = 1, NDI
         SSE = SSE + HALF * (stre(I) + OLD_STRESS(I)) * DSTRAN(I)
      END DO
      DO I = NDI+1, NTENS
         SSE = SSE + (stre(I) + OLD_STRESS(I)) * DSTRAN(I)
      END DO
C     TO COMPUTE THE INTERNAL ENERGY WITHOUT VISCOUS REGULARIZATION
      DO I = 1, NDI
         SCD = SCD + HALF * (stre(I) + OLD_STRESS(I)
     &        -D_STRESS(I)-DOLD_STRESS(I)) * DSTRAN(I)
      END DO
      DO I = NDI+1, NTENS
         SCD = SCD + (stre(I) + OLD_STRESS(I)
     &        -D_STRESS(I)-DOLD_STRESS(I)) * DSTRAN(I)
      END DO
      
      RETURN
      END
      
C******************************************************************************
C CALCULATE THE STRESS BASED ON THE DAMAGE VARAIBLES***************************
C******************************************************************************
      SUBROUTINE GetStress(CFULL,CDFULL,DFV,DMV,stre,STRANT,NDI,NTENS)
      real*8 CFULL(6,6),CDFULL(6,6),stre(NTENS),
     &     STRANT(6),CDTHREE(3,3)
      real*8 ZERO, ONE, DFV, DMV

      ZERO = 0.0
      ONE = 1.0
C     CDTHREE.....DAMAGED CONDENSED-ELASTICITY MATRIX FOR PLANE STRESS PROBLEM
      DO I = 1, 6
         DO J = 1, 6
            CDFULL(I,J)=CFULL(I,J)
         END DO
      END DO
      IF ( (DFV .NE. ZERO) .OR. (DMV .NE. ZERO)) THEN
         CDFULL(1,1) = (ONE - DFV) * CFULL(1,1)
         CDFULL(1,2) = (ONE - DFV) * (ONE - DMV) * CFULL(1,2)
         CDFULL(2,1) = CDFULL(1,2)
         CDFULL(2,2) = (ONE - DMV) * CFULL(2,2)
         CDFULL(1,3) = (ONE - DFV) * CFULL(1,3)
         CDFULL(3,1) = CDFULL(1,3)
         CDFULL(2,3) = (ONE- DMV) * CFULL(2,3)
         CDFULL(3,2) = CDFULL(2,3)
         CDFULL(4,4) = (ONE - DMV) * (ONE - DFV) * CFULL(4,4)
      END IF
C   UPDATE THE STRESS STATE IF 3D CASE
C
      IF (NDI .EQ. 3) THEN
         DO I = 1, NTENS
            stre(I)=ZERO
            DO J = 1, NTENS
               stre(I)=stre(I)+CDFULL(I,J) * STRANT(J)
            END DO
         END DO
C     
C     INITIALIZE THE 3X3 CONDENSED STIFFNESS MATRIX IF PLANE STRESS CASE
C     
      ELSE IF ( NDI .EQ. 2) THEN
         DO I = 1, NTENS
            DO J = 1, NTENS
               CDTHREE(I,J)=ZERO
            END DO
         END DO
C     
C     
C     CONDENSE THE UNDAMAGED STIFFNESS MATRIX
C     
         CALL MatrixCondense(CDFULL,CDTHREE)
C     
C     UPDATE THE STRESS
C     
         DO I = 1, NTENS
            stre(I)=ZERO
            DO J = 1, NTENS
               stre(I)=stre(I)+CDTHREE(I,J) * STRANT(J)
            END DO
         END DO
      END IF 
      RETURN
      END
C******************************************************************************
C     TO CHECK THE FAILURE INITIATION AND THE CORRESPONDING DERIVATIVE*********
C******************************************************************************
      SUBROUTINE CheckFailureIni(EPITL,EPICL,EPITT,EPICT,EPISLT,STRANT,
     &     GFMAT,GFFIB, CELENT, CFULL, DF, DM, DDFDE, DDMDE, NTENS,
     &     DFOLD,DMOLD,NDI)
      real*8 DDFDE(6), DDMDE(6), STRANT(6), CFULL(6,6)
      real*8 DFMNDE(6), DFFNDE(6)
      real*8 ZERO, ONE, TWO, HALF
      real*8 EPITL, EPICL, EPITT, EPICT, EPISLT,GFMAT,GFFIB,DF,DM,
     & CELENT,DFOLD,DMOLD 

      integer NTENS,NDI

      ZERO = 0.0
      ONE = 1.0
      TWO = 2.0
      HALF = 0.5
C     
C     CHECK THE INITIATION CONDITION FOR MATRIX
C     FMN=FM/EPITT > 1 THEN EVALUATE THE DAMAGE VARIABLE AND DERIVATIVE
C     
      TERM1 = STRANT(2)**TWO / EPICT / EPITT
      TERM2 = (EPICT - EPITT) / EPICT / EPITT * STRANT(2)
      IF (NDI .EQ. 3) THEN
         TERM3 = (STRANT(4))**TWO / EPISLT**TWO
      ELSE IF (NDI .EQ. 2) THEN
         TERM3 = (STRANT(3))**TWO / EPISLT**TWO
      END IF
      TERM = TERM1 + TERM2 + TERM3
      IF (TERM .GT. ZERO) THEN
         FMN = SQRT(TERM)
      ELSE
         FMN = ZERO
      END IF
C
C     INITIALIZE THE ARRAY AND VARIABLE
C
      DM = ZERO
      DDMDFMN = ZERO
      DO I = 1, 6
         DFMNDE(I) = ZERO
         DDMDE(I) = ZERO
      END DO
      IF (FMN .GT. ONE) THEN
C     CALCULATE DM, DDMDFMN
         CALL DamageEvaluation( CFULL(2,2), FMN, GFMAT, CELENT,
     &        EPITT, DM, DDMDFMN)
C     CALCULATE DFMNDE
         IF (DM .GT. DMOLD) THEN
            DFMNDE(2) = HALF / FMN * (TWO * STRANT(2) + EPICT - EPITT)
     &           / EPICT / EPITT
            IF (NDI .EQ. 3) THEN
               DFMNDE(4) = ONE / FMN * STRANT(4) / EPISLT**TWO
            ELSE IF (NDI .EQ. 2) THEN
               DFMNDE(4) = ONE / FMN * STRANT(3) / EPISLT**TWO
            END IF
            DO I = 1, 6
               DDMDE(I) = DFMNDE(I) * DDMDFMN
            END DO
         END IF
      END IF
      DM = MAX (DM, DMOLD)
C     
C     CHECK THE INITIATION CONDITION FOR FIBER
C     FFN=FF/EPITL>1 THEN CALCULATE THE DAMAGE VARIABLE AND DERIVATIVE
C     
      TERM1 = STRANT(1)**TWO / EPICL / EPITL
      TERM2 = (EPICL - EPITL) / EPICL / EPITL *STRANT(1)
         TERM = TERM1 + TERM2
         IF (TERM .GT. ZERO) THEN
            FFN = SQRT(TERM)
         ELSE
            FFN = ZERO
         END IF
      DF = ZERO
      DDFDFFN = ZERO
      DO I = 1, 6
            DFFNDE(I) = ZERO
            DDFDE(I) = ZERO
      END DO
      IF (FFN .GT. ONE) THEN
C     CALCULATE DF, DDFDFFN
            CALL DamageEvaluation( CFULL(1,1), FFN, GFFIB, CELENT,
     &          EPITL, DF, DDFDFFN)
C     CALCULATE DFFNDE
            IF (DF .GT. DFOLD) THEN
               DFFNDE(1) = HALF / FFN * (TWO * STRANT(1) + 
     &       EPICL - EPITL)/ EPICL / EPITL
               DDFDE(1) = DFFNDE(1) * DDFDFFN
            END IF
      END IF
         DF = MAX (DF, DFOLD)
      RETURN
      END
c ******************************************************************************
c   *SUBROUTINE TO EVALUATE THE DAMAGE AND THE
c     DERIVATIVE************************
C******************************************************************************
      SUBROUTINE DamageEvaluation(STIFF, FN, GF, CELENT, EPIT, D,
     &     DDDFN)
C     CALCULATE DAMAGE VARIABLE
      real*8 STIFF,GF,CELENT,EPIT,D,DDFN

      real*8 ONE

      ONE = 1.0


      TERM1 = STIFF * EPIT**2 * CELENT / GF
      TERM2 = (ONE - FN) * TERM1
      D = ONE - EXP(TERM2) / FN
C     CALCULATE THE DERIVATIVE OF DAMAGE VARIABLE WITH RESPECT TO FAILURE
C     RITERION
      DDDFN = (ONE / FN + TERM1) * (ONE - D)
      RETURN
      END
C******************************************************************************
C     * SUBROUTINE TO CONDENSE THE 4X4 MATRIX INTO 3X3 MATRIX********************
C******************************************************************************
      SUBROUTINE MatrixCondense(CFULL,CTHREE)
      
      real*8 CFULL(6,6),CTHREE(3,3)
C     
      CTHREE(1,1) = CFULL(1,1) - CFULL(1,3) * CFULL(3,1) / CFULL(3,3)
      CTHREE(1,2) = CFULL(1,2) - CFULL(1,3) * CFULL(3,2) / CFULL(3,3)
      CTHREE(2,1) = CFULL(2,1) - CFULL(2,3) * CFULL(3,1) / CFULL(3,3)
      CTHREE(2,2) = CFULL(2,2) - CFULL(2,3) * CFULL(3,2) / CFULL(3,3)
      CTHREE(3,3) = CFULL(4,4)
      RETURN
      END
C*******************************************************************************
C   * SUBROUTINE TO GET THE DERIVATIVE MATRIX OF CONDENSE DAMAGED MATRIX OVER
C**** THE DAMAGE VARIABLE******************************************************
C*******************************************************************************
      SUBROUTINE ElasticDerivative(CFULL,DMV,DFV, DCDDM,DCDDF)
      
      real*8 CFULL(6,6), DCDDM(6,6),
     &     DCDDF(6,6),DMV,DFV

      real*8 ZERO, ONE, TWO, HALF


      ZERO = 0.0
      ONE = 1.0
      TWO = 2.0
      HALF = 0.5
C     initialize the data to zero
      DO I = 1, 6
         DO J = 1, 6
            DCDDM(I,J) = ZERO
            DCDDF(I,J) = ZERO
         END DO
      END DO
C     
C     CALCULATE DC/DDF
C     
      DCDDF(1,1) = -CFULL(1,1)
      DCDDF(1,2) = -(ONE - DMV) * CFULL(1,2)
      DCDDF(2,1) = DCDDF(1,2)
      DCDDF(1,3) = -CFULL(1,3)
      DCDDF(3,1) = DCDDF(1,3)
      DCDDF(4,4) = -(ONE - DMV) * CFULL(4,4)
C     
C     CALCULATE DC/DDM
C     
      DCDDM(1,2) = - (ONE - DFV) * CFULL(1,2)
      DCDDM(2,1) = DCDDM(1,2)
      DCDDM(2,2) = -CFULL(2,2)
      DCDDM(2,3) = -CFULL(2,3)
      DCDDM(3,2) = DCDDM(2,3)
      DCDDM(4,4) = -(ONE - DFV) * CFULL(4,4)
      RETURN
      END