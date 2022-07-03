!
!     CalculiX - A 3-dimensional finite element program
!              Copyright (C) 1998-2021 Guido Dhondt
!
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation(version 2);
!
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!     GNU General Public License for more details.
!
!     You should have received a copy of the GNU General Public License
!     along with this program; if not, write to the Free Software
!     Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

       SUBROUTINE umat_tsaiwu(stre,xstate,ddsdde,sse,spd,scd,rpl,
     &  ddsddt,drplde,drpldt,stran,dstran,
     &  abqtime,dtime,temp,dtemp,predef,dpred,amat,ndi,
     &  nshr,ntens,nstate_,elconloc, nprops,pgauss,drot,
     &  pnewdt,celent,xokl,xkl,iel,iint,layer,kspt,jstep,kinc)
!
!     calculates stiffness and stresses for a user defined material
!     law
!
!     icmd=3: calculates stress at mechanical strain
!     else: calculates stress at mechanical strain and the stiffness
!           matrix
!
!     INPUT:
!
!     amat               material name
!     iel                element number
!     iint               integration point number
!
!     kode               material type (-100-#of constants entered
!                        under *USER MATERIAL): can be used for materials
!                        with varying number of constants
!
!     elconloc(*)        user defined constants defined by the keyword
!                        card *USER MATERIAL (actual # =
!                        -kode-100), interpolated for the
!                        actual temperature t1l
!
!     emec(6)            Lagrange mechanical strain tensor (component order:
!                        11,22,33,12,13,23) at the end of the increment
!                        (thermal strains are subtracted)
!     emec0(6)           Lagrange mechanical strain tensor at the start of the
!                        increment (thermal strains are subtracted)
!     beta(6)            residual stress tensor (the stress entered under
!                        the keyword *INITIAL CONDITIONS,TYPE=STRESS)
!
!     xokl(3,3)          deformation gradient at the start of the increment
!     voj                Jacobian at the start of the increment
!     xkl(3,3)           deformation gradient at the end of the increment
!     vj                 Jacobian at the end of the increment
!
!     ithermal           0: no thermal effects are taken into account
!                        >0: thermal effects are taken into account (triggered
!                        by the keyword *INITIAL CONDITIONS,TYPE=TEMPERATURE)
!     t1l                temperature at the end of the increment
!     dtime              time length of the increment
!     time               step time at the end of the current increment
!     ttime              total time at the start of the current step
!
!     icmd               not equal to 3: calculate stress and stiffness
!                        3: calculate only stress
!     ielas              0: no elastic iteration: irreversible effects
!                        are allowed
!                        1: elastic iteration, i.e. no irreversible
!                           deformation allowed
!
!     mi(1)              max. # of integration points per element in the
!                        model
!     nstate_            max. # of state variables in the model
!
!     xstateini(nstate_,mi(1),# of elements)
!                        state variables at the start of the increment
!     xstate(nstate_,mi(1),# of elements)
!                        state variables at the end of the increment
!
!     stre(6)            Piola-Kirchhoff stress of the second kind
!                        at the start of the increment
!
!     iorien             number of the local coordinate axis system
!                        in the integration point at stake (takes the value
!                        0 if no local system applies)
!     pgauss(3)          global coordinates of the integration point
!     orab(7,*)          description of all local coordinate systems.
!                        If a local coordinate system applies the global
!                        tensors can be obtained by premultiplying the local
!                        tensors with skl(3,3). skl is  determined by calling
!                        the subroutine transformatrix:
!                        call transformatrix(orab(1,iorien),pgauss,skl)
!
!
!     OUTPUT:
!
!     xstate(nstate_,mi(1),# of elements)
!                        updated state variables at the end of the increment
!     stre(6)            Piola-Kirchhoff stress of the second kind at the
!                        end of the increment
!     stiff(21):         consistent tangent stiffness matrix in the material
!                        frame of reference at the end of the increment. In
!                        other words: the derivative of the PK2 stress with
!                        respect to the Lagrangian strain tensor. The matrix
!                        is supposed to be symmetric, only the upper half is
!                        to be given in the same order as for a fully
!                        anisotropic elastic material (*ELASTIC,TYPE=ANISO).
!                        Notice that the matrix is an integral part of the
!                        fourth order material tensor, i.e. the Voigt notation
!                        is not used.
!     pnewdt             to be specified by the user if the material
!                        routine is unable to return the stiffness matrix
!                        and/or the stress due to divergence within the
!                        routine. pnewdt is the factor by which the time
!                        increment is to be multiplied in the next
!                        trial and should exceed zero but be less than 1.
!                        Default is -1 indicating that the user routine
!                        has converged.
!     ipkon(*)           ipkon(iel) points towards the position in field
!                        kon prior to the first node of the element's
!                        topology. If ipkon(iel) is smaller than 0, the
!                        element is not used.
!
      implicit none
!
      character*80 amat
!
      integer iel,iint,nstate_, i,j, ntens
!
      real*8 elconloc(*),emec(6),emec0(6),beta(6),stre(6),stiff(21),
     &  vj,t1l,dtime,xkl(3,3),xokl(3,3),voj,pgauss(3),
     &  time,ttime,pnewdt
      
      real*8 ddsdde(ntens,ntens),sse,spd,scd,rpl,ddsddt,drplde,drpldt,
     & stran(ntens),dstran(ntens),abqtime,temp,dtemp,predef,dpred,ndi,
     & celent,nshr,nprops,drot,layer,kspt,jstep,test(ntens,ntens)     
!
      real*8 xstate(nstate_)

!     material properties of the composite material       
      real*8 e11,e22,e33,g12,g13,g23,anu12,anu13,anu23, 
     &  xt,xc,yt,yc,zt,zc,s12,s13,s23,eps_xt,eps_xc,eps_yt,
     &  eps_yc,eps_zt,eps_zc,gam_s12,gam_s13,gam_23, 
     &  beta_ft,beta_fc,beta_mt,beta_mc,beta_s,delta,anu21,anu31,anu32,
     &  f1,f2,f3,f11,f22,f33,f44,f55,f66,f12,f13,f23, damage, 
     &  dmg(ntens), e(ntens), upstran(ntens), tsaiwu(ntens)

      integer max_damage, iter, iteration, kinc
!
! 
!     Input parameters for the composite material (order below)
!     ++++++++++++++++++++++++++++++++++++++++++++
!     E11, E22, E33, nu12, nu13, nu23, G12, G13, 
!     G23, Xt, Xc, Yt, Yc, Zt, Zc, S12, 
!     S13, S23, beta_fc, beta_fc, beta_mt, beta_mc, beta_s
!     ++++++++++++++++++++++++++++++++++++++++++++
!     
      
      e11=elconloc(1)
      e22=elconloc(2)
      e33=elconloc(3)
      anu12=elconloc(4)
      anu13=elconloc(5)
      anu23=elconloc(6)
      g12=elconloc(7)
      g13=elconloc(8)
      g23=elconloc(9)
      xt=elconloc(10)
      xc=elconloc(11)
      yt=elconloc(12)
      yc=elconloc(13)
      zt=elconloc(14)
      zc=elconloc(15)
      s12=elconloc(16)
      s13=elconloc(17)
      s23=elconloc(18)
      beta_ft=elconloc(19)
      beta_fc=elconloc(20)
      beta_mt=elconloc(21)
      beta_mc=elconloc(22)
      beta_s=elconloc(23)
     
      do i=1,6
            dmg(i) = xstate(i)
      end do

      open(1, file = 'damage.txt')
      
      if (kinc.eq.1) then

            dmg(:) = 1.
            e(:) = 0.
            damage = 0.0

            do i = 1, ntens
              upstran(i) = stran(i) + dstran(i)
            end do

            call update_stress(stre, upstran, 
     &    dmg, ddsdde, anu12, anu13, anu23,e11, e22, e33,
     &    g12, g13, g23, ntens)

            do iter=1,6
                  xstate(iter) = dmg(iter)
            end do

      else

            do iter=1,6
                  dmg(iter) = xstate(iter)
            end do

            do i = 1, ntens
                  upstran(i) = stran(i) + dstran(i)
            end do

            call update_stress(stre, upstran, 
     &    dmg, ddsdde, anu12, anu13, anu23,e11, e22, e33,
     &    g12, g13, g23, ntens)

            call tsaiwu_failure(xt, xc, yt, yc, zc, zt, s13, s23,
     &    s12, stre, e, ntens)

            if ((maxval(e).GT.1.0).or.(minval(dmg).LT.1.0)) then      
                  
                  call tsaiwu_failure_calc(stre, e, dmg, beta_fc, 
     &    beta_ft,beta_mc, beta_mt, beta_s, ntens, max_damage, kinc)

            end if

            call update_stress(stre, upstran, dmg, 
     &      ddsdde, anu12, anu13, anu23,e11, e22, e33, 
     &      g12, g13, g23, ntens)

            do iter=1,6
                  xstate(iter) = dmg(iter)
            end do

      end if

      return
      end

       subroutine update_stress(stre, upstran, dmg, ddsdde, anu12, 
     &  anu13, anu23,e11, e22, e33, g12, g13, g23,ntens)

              implicit none

              real*8 anu21, anu12, anu13, anu31, anu32, anu23,
     &    e11, e22, e33, g12, g13, g23, ddsdde(ntens, ntens), 
     &    stre(ntens), upstran(ntens), dmg(ntens), delta

              integer i,j, ntens   

              anu21=(e22*anu12)/e11
              anu31=(e33*anu13)/e11
              anu32=(e33*anu23)/e22

              delta = (1.d0-anu12*anu21-anu23*anu32-anu13*anu31
     &     -2.d0*anu12*anu13*anu31)
     &     /(e11*e22*e33)

              ddsdde(1,1) = ((1.d0 - anu23*anu32)/(e22*e33*delta))
     &  *abs(dmg(1))
              ddsdde(1,2) = ((anu21 + anu31*anu23)/(e22*e33*delta))
     &  *abs(dmg(1)*dmg(2))
              ddsdde(2,2) = ((1.d0 - anu31*anu13)/(e11*e33*delta))
     &  *abs(dmg(2))
              ddsdde(1,3) = ((anu31 + anu21*anu32)/(e22*e33*delta))
     &  *abs(dmg(1)*dmg(3))
              ddsdde(2,3) = ((anu32 + anu31*anu12)/(e11*e33*delta))
     &  *abs(dmg(2)*dmg(3))
              ddsdde(3,3) = ((1.d0 - anu12*anu21)/(e11*e22*delta))
     &   *abs(dmg(3))
              ddsdde(4,1) = 0.d0
              ddsdde(4,2) = 0.d0
              ddsdde(4,3) = 0.d0
              ddsdde(4,4) = g12*abs(dmg(4))
              ddsdde(5,1) = 0.d0
              ddsdde(5,2) = 0.d0
              ddsdde(5,3) = 0.d0
              ddsdde(5,4) = 0.d0
              ddsdde(5,5) = g13*abs(dmg(5))
              ddsdde(6,1) = 0.d0
              ddsdde(6,2) = 0.d0
              ddsdde(6,3) = 0.d0
              ddsdde(6,4) = 0.d0
              ddsdde(6,5) = 0.d0
              ddsdde(6,6) = g23*abs(dmg(6))

              do i = 1, ntens
                     stre(i) = 0.0d+0
                            do j = 1, ntens
                                   stre(i) = stre(i) + 
     &  ddsdde(i,j)*upstran(j)
                            end do
              end do

      return 
      end

       subroutine tsaiwu_failure(xt, xc, yt, yc, zc, zt, s13, s23,
     &    s12, stre, e, ntens)

            implicit none

            real*8 f1, f2, f3, f11, f22, f33, f44, f55, f66,
     &    f12, f13, f23, xt, xc, yt, yc, zc, zt, s13, s23,
     &    s12, damage, tsaiwu(6),e(ntens),stre(ntens)

            integer ntens, max_damage      

      !     calculate Tsai-Wu failure parameters

            f1 = (1.0d+0/xt)-(1.0d+0/xc)
            f2 = (1.0d+0/yt)-(1.0d+0/yc)
            f3 = (1.0d+0/zt)-(1.0d+0/zc)
            f11 = 1.0d+0/(xt*xc)
            f22 = 1.0d+0/(yt*yc)
            f33 = 1.0d+0/(zt*zc)
            f44 = 1.0d+0/(s13*s13)
            f55 = 1.0d+0/(s23*s23)
            f66 = 1.0d+0/(s12*s12)
            f12 = -0.5d+0*(1.0d0/sqrt(xt*xc*yt*yc))
            f13 = -0.5d+0*(1.0d0/sqrt(xt*xc*zt*zc))
            f23 = -0.5d+0*(1.0d0/sqrt(yt*yc*zt*zc))

!     calculate Tsai-Wu polinomials
      
            tsaiwu(1) = f1*stre(1) + f11*stre(1)**2
     &   + f12*stre(1)*stre(2) + 
     &   f13*stre(1)*stre(3)

            tsaiwu(2) = f2*stre(2) + f22*stre(2)**2 
     &   + f12*stre(1)*stre(2) + 
     &   f23*stre(2)*stre(3)

            tsaiwu(3) = f3*stre(3) + f33*stre(3)**2 
     &   + f13*stre(1)*stre(3) + 
     &   f23*stre(2)*stre(3)

            tsaiwu(4) = f44*stre(4)**2

            tsaiwu(5) = f55*stre(5)**2

            tsaiwu(6) = f66*stre(6)**2

            damage = tsaiwu(1) + tsaiwu(2) + tsaiwu(3) + tsaiwu(4) +
     &   tsaiwu(5) + tsaiwu(6)
            
            
            max_damage = maxloc(tsaiwu, dim=1)
            e(max_damage) = damage

      return 
      end

       subroutine tsaiwu_failure_calc(stre, e, dmg, beta_fc, beta_ft,
     &    beta_mc, beta_mt, beta_s, ntens, max_damage, kinc)

            implicit none

            real*8 stre(ntens),e(ntens),dmg(ntens),beta_fc,
     &   beta_ft, beta_mc, beta_mt, beta_s

            integer ntens, max_damage, kinc,i


            if (((e(1).GT.1.0).or.(dmg(1).LT.1.0))
     &   .and.(stre(1).GE.0.0)) then
                  write(1, *)  'tensile fiber damage detected',
     & kinc 
                  dmg(1) = dmg(1)*(0.99999-beta_ft)
                  write(1, *) 'Tsai-Wu damage coef:', e(1)
                  write(1, *) 'Updated damage coef:', dmg(1)

            else if (((e(1).GT.1.0).or.(dmg(1).LT.1.0))
     &    .and.(stre(1).LT.0.0)) then
                  write(1, *)  'comp fiber damage detected', 
     & kinc
                  dmg(1) = dmg(1)*(0.99999-beta_fc)
                  write(1, *) 'Tsai-Wu damage coef:', e(1)
                  write(1, *) 'Updated damage coef:', dmg(1)

            end if

            if (((e(2).GT.1.0).or.(dmg(2).LT.1.0))
     &   .and.(stre(2).GE.0.0)) then
                  write(1, *)  'tensile matrix damage detected', 
     & kinc
                  dmg(2) = dmg(2)*(0.99999-beta_mt)
                  write(1, *) 'Tsai-Wu damage coef:', e(2)
                  write(1, *) 'Updated damage coef:', dmg(2)

            else if (((e(2).GT.1.0).or.(dmg(2).LT.1.0))
     &    .and.(stre(2).LT.0.0)) then
                  write(1, *)  
     &    'compressive matrix damage detected at ', kinc
                  dmg(2) = dmg(2)*(0.99999-beta_mc)
                  write(1, *) 'Tsai-Wu damage coef:', e(2)
                  write(1, *) 'Updated damage coef:', dmg(2)
            end if


            if (((e(3).GT.1.0).or.(dmg(3).LT.1.0))
     &   .and.(stre(3).GE.0.0)) then
                  write(1, *)  
     &    'interlaminar tensile damage detected at ', kinc
                  dmg(3) = dmg(3)*(0.99999-beta_mt)
                  write(1, *) 'Tsai-Wu damage coef:', e(3)
                  write(1, *) 'Updated damage coef:', dmg(3)

            else if (((e(3).GT.1.0).or.(dmg(3).LT.1.0))
     &   .and.((stre(3).LT.0.0))) then
                  write(1, *)  
     &  'interlaminar compressive damage detected at ', kinc
                  dmg(3) = dmg(3)*(0.99999-beta_mc)
                  write(1, *) 'Tsai-Wu damage coef:', e(3)
                  write(1, *) 'Updated damage coef:', dmg(3)
            end if


            do i=4,6
                  if ((e(i).GT.1.0).or.(dmg(i).LT.1.0)) then
                        write(1, *)  'shear damage detected', kinc
                        dmg(i) = dmg(i)*(0.99999-beta_s)
                        write(1, *) 'Tsai-Wu damage coef:',e(i)
                        write(1, *) 'Updated damage coef:',dmg(i)
                  end if 
            end do

            do i=1,6
                  if ((dmg(i).GT.1.0).or.(dmg(i).LT.0.0)) then
                        write(1, *)  'Negative Multipliers'
                        write(1, *)  'Check properties'
                        write(1, *) 'Iteration: ', i
                  end if 
            end do

      return 
      end


