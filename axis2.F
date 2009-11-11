      program paxis1
      implicit none
      real(8):: vs,vc,ang

      real(8):: r,pi

      pi=2.0d0*asin(1.0d0)
      write(*,*) 'pi=',pi

      do r=-pi,pi,0.1d0
        vs=sin(r); vc=cos(r)
        call sincos2ang(vs,vc,ang)
        write(*,100) r, ang,vs,sin(ang),vc,cos(ang)
      enddo 
      r=pi
        vs=sin(r); vc=cos(r)
        call sincos2ang(vs,vc,ang)
        write(*,100) r, ang,vs,sin(ang),vc,cos(ang)

 100  format(2F20.10,9E20.10)

      end 

      subroutine sincos2ang(vs, vc, angle)
      implicit none
      real(8):: vs, vc
      real(8):: angle 
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
      
      end 


