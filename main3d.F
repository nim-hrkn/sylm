      program test
      use m_eulerangle 
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
      real(8):: theta,pi,phi,psi,axisc(3)
      integer:: il,im ,ilm,nilm,im2
      integer:: i,j,k,id
      integer:: ilmlist(2,100)
      real(8):: orig_caxis(3),arot(3),anewtmp(3),range,g0,crot(3)
      real(8):: ddot,eps=1.0d-5,axisa(3)

      pi=2.0d0*asin(1.0d0)

 130  format(a,9F20.10) 

      axisc=(/1.0d0,1.0d0,0.5d0/)
      axisa=(/1.0d0,0.0d0,.0d0/)

c#if 0
c      call axisac2rot(axisa,axisc,rot,0.0d0)
c
c      call writemesh(rot,nl,lmax,lmx,'b.cube')
c
c      write(*,*) '----------------------'
c
c#endif
      call axisac2rot(axisa,axisc,rot)
      do i=1,3
        write(*,*) rot(:,i)
      enddo

      call writemesh(rot,nl,lmax,lmx,'c.cube')

      end 


