c test axis2eulerangle
      program  paxis
      implicit none
      real(8):: axis(3), theta,phi


      real(8):: r(3),axis2(3),r2

      integer:: i,j,n 
      real(8):: eps=1.0d-5

      n=10000000
      do i=1,n
       
        if (mod(i,int(n*.1)).eq.1) then
            write(*,*) i
        endif 
        call random_number(r)
        call axis2eulerangle(r,theta,phi)
        r2=0.0d0
        do j=1,3
          r2=r2+r(j)**2
        enddo
        axis=r/sqrt(r2)

      axis2=(/ cos(theta)*cos(phi), cos(theta)*sin(phi), sin(theta) /)

       if ( abs3(axis-axis2)> eps ) then 
       write(*,*) theta,phi
       write(*,'(9e20.10)') axis,axis2,axis-axis2
       endif

      enddo 

      stop

      contains

      real(8) function abs3(v)
      real(8),intent(in):: v(3)
      real(8):: r
      integer:: i
      r=0.0d0
      do i=1,3
       r=r+v(i)**2
      enddo 
      abs3= sqrt(r)
      end function abs3

      subroutine axis2eulerangle (axis0,theta,phi)
      implicit none
      real(8),intent(in):: axis0(3)
      real(8):: axis2(3),axis(3)
      real(8):: ta ,r2
      real(8),intent(out)::phi,theta
      integer:: i
      real(8):: eps =1.0d-5
      real(8):: v2d(2), sin_phi,cos_phi,pi

      axis=axis0
      pi=2.0d0*asin(1.0d0)
c      axis=(/1.0,1.0,1.0/)
c (rcos(θ)cos(φ),rcos(θ)sin(φ),rsin(θ))
      r2=0.0d0
      do i=1,3
      r2=r2+axis(i)**2
      enddo 
      r2=1.0d0/sqrt(r2)
      axis=axis*r2

c      write(*,*) axis

      if ( abs(axis(1)) < eps .and. abs(axis(2)) < eps ) then
           ! cos(t)=0
           if (theta > 0.0d0 ) then
           theta=pi *0.5 
           else
           theta=-pi *0.5 
           endif 
           phi=0.0d0 
      else 
           v2d=(/axis(2),axis(1)/)
           r2= v2d(1)**2+v2d(2)**2
           r2=1.0d0/sqrt(r2)
           v2d=v2d*r2
           call sincos2ang(v2d(1),v2d(2), phi ) 
           sin_phi=sin(phi)
           cos_phi=cos(phi)
           if (sin_phi < eps )  then
              ! use cos(phi) 
              v2d(1)= axis(3)
              v2d(2)= axis(1)/cos_phi 
              call sincos2ang(v2d(1),v2d(2), theta)  
           else 
              ! use sin(phi) 
              v2d(1)=axis(3)
              v2d(2)=axis(2)/sin_phi
              call sincos2ang(v2d(1),v2d(2), theta)  
           endif

      endif 

c      write(*,*) theta,phi
      axis2=(/ cos(theta)*cos(phi), cos(theta)*sin(phi), sin(theta) /)

c      write(*,*) axis2

      end  subroutine axis2eulerangle



      subroutine sincos2ang(vs, vc, angle)
      implicit none
      real(8),intent(in):: vs, vc
      real(8),intent(out):: angle 
      real(8):: angle_s, angle_c

      angle_s = asin(vs)
      angle_c = acos(vc) 
      if (vs.ge.0 .and.  vc.ge.0 ) then 
         !  [0:pi/2]
         angle = angle_s
      else if (vs.ge.0 .and. vc.le.0) then
         !  [pi/2:pi]
         angle = angle_c 
      else if (vs.le.0 .and. vc.le.0 ) then
         ! [pi:pi*3/2] 
         angle = -angle_c 
      else 
         ! [pi*3/2: 2*pi]
         angle = -angle_s
      endif
      
      end subroutine sincos2ang

      end program paxis


