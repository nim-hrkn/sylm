      program test
      implicit none
      integer,parameter:: lmax=2
      integer,parameter:: nl=lmax+1
      integer,parameter:: lmx=2*(lmax-1)
      real(8):: r(3), yl( (lmx+1)*(lmx+1) ), r2s,c((lmx+1)*(lmx+1)) 

      integer,parameter:: mng = 5
      real(8):: dlmm(-(nl-1):(nl-1),-(nl-1):(nl-1),0:nl-1,mng) 
      integer:: ng,ig 
      real(8):: symops(3,3,mng) 

      integer,parameter:: nx=10,ny=10,nz=10
      real(8):: x(3),g,val(-nx:nx,-ny:ny, -nz:nz,(lmx+1)*(lmx+1)) 
     . ,rr,x0(3), 
     .  rotvalL(-nx:nx,-ny:ny, -nz:nz, -lmax:lmax ),
     .  valL(-nx:nx,-ny:ny, -nz:nz, -lmax:lmax )
      integer:: ix,iy,iz

      integer:: ifout=10
      character(20):: fname

      real(8):: rotz(3,3),roty(3,3),rot(3,3),rotz2(3,3),rottmp(3,3)
      real(8):: theta,pi,phi,psi,axis(3)
      integer:: il,im ,ilm,nilm,im2
      integer:: i,j,k,id
      integer:: ilmlist(2,100)
      real(8):: orig_caxis(3),arot(3),anewtmp(3),range,g0,crot(3)
      real(8):: ddot,eps=1.0d-5

      pi=2.0d0*asin(1.0d0)

      axis=(/.0,1.0,1.0/)
      write(*,130) 'rotated c-axis=', axis / sqrt(ddot(3,axis,1,axis,1))
      call axis2eulerangle(axis,theta,phi)
      write(*,*) 'theta,phi=',theta*180/pi,phi*180/pi
      call eulerangle2rot(theta,phi,psi,rot)
      orig_caxis=(/0.0d0,0.0d0,1.0d0/)
      crot=0.0d0
      do i=1,3
        do j=1,3
        crot(i)=rot(i,j)*orig_caxis(j)
        enddo
      enddo
      write(*,130) 'crot=',crot

      call eulerangle2rot(theta,phi,0.0d0,rot)

      write(*,*) 'rot'
      do i=1,3
        write(*,'(3f10.5)') rot(1:3,i)
      enddo

c check
      orig_caxis=(/0.0d0,0.0d0,1.0d0/)
      crot=0.0d0
      do i=1,3
        do j=1,3
        crot(i)=rot(i,j)*orig_caxis(j)
        enddo
      enddo
      write(*,130) 'crot=',crot
c check end 

      call writemesh(rot,nl,lmax,lmx,'b.cube')

      anewtmp=(/1.0d0,0.d0,1.0d0/)

      call new_a_axis (axis,theta,phi, anewtmp,psi )

      write(*,130) 'anewtmp=',anewtmp
      write(*,130) 'psi(deg)=',psi/pi*180.0

 130  format(a,3F20.10)

      call eulerangle2rot(theta,phi,psi,rot)

      write(*,*) 'rot'
      do i=1,3
        write(*,'(3f10.5)') rot(1:3,i)
      enddo

      orig_caxis=(/0.0d0,0.0d0,1.0d0/)
      crot=0.0d0
      do i=1,3
        do j=1,3
        crot(i)=rot(i,j)*orig_caxis(j)
        enddo
      enddo
      write(*,130) 'crot=',crot

      call writemesh(rot,nl,lmax,lmx,'c.cube')

      end 
