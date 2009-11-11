      program test
      integer,parameter:: nl=2
      integer,parameter:: lmx=2*(nl-1)
      real(8):: r(3), yl( (lmx+1)*(lmx+1) ), r2s,c((lmx+1)*(lmx+1)) 


      integer,parameter:: nx=10,ny=10,nz=10
      real(8):: x(3),g,val(-nx:nx,-ny:ny, -nz:nz,(lmx+1)*(lmx+1)) 
     . ,rr,x0(3),eps=1.0d-5
      integer:: ix,iy,iz

      integer:: ifout=10
      character(20):: fname

      write(*,*) nl,lmx, (lmx+1)*(lmx+1) 
      do ix=-nx,nx
      do iy=-ny,ny
      do iz=-nz,nz
         x0(1)=dble(ix)/nx
         x0(2)=dble(iy)/ny
         if (nz.eq.0) then
           x0(3)=0.0d0
         else
         x0(3)=dble(iz)/nz
         endif
         
         rr=x0(1)**2+x0(2)**2+x0(3)**2 
         if (rr> eps) then
         x=x0/sqrt(rr) 
         endif 
         call sylm(x,yl,lmx,r2s)
         call sylmnc(c,lmx)
         g=exp(-rr/(nx/2)**2) 

         do id=1, (lmx+1)**2
         val(ix,iy,iz,id)=yl(id)*g*c(id)
         enddo

c         do id=1,(lmx+1)**2
c         write(10+id,'(3i5,10F15.5)') ix,iy,iz,x0,yl(id)*g*c(id)
c         enddo

      enddo
      enddo
c         do id=1,(lmx+1)**2
c         write(10+id,*) ' '
c         enddo
      enddo

      write(*,*) id, 'max','min'
      do id=1,(lmx+1)**2
        write(*,*) id, maxval(val(:,:,:,id)), minval(val(:,:,:,id))
      enddo

      do id=1,(lmx+1)**2

      ifout=10+id
      write(fname,'(i02,a)')ifout,'.cube'
      open(file=fname,unit=ifout) 
      write(ifout,*)'TEST', id
      write(ifout,*)'TEST'
      write(ifout,110) 4, -1.0d0, -1.0d0, -1.0d0
 110  format(I5,5F12.6)
 120  format(6E13.6)
      write(ifout,110) 2*nx+1, 1.0d0/nx, 0.0d0, 0.0d0
      write(ifout,110) 2*ny+1, 0.0d0,1.0d0/ny, 0.0d0
      write(ifout,110) 2*nz+1, 0.0d0,0.0d0,1.0d0/nz
      write(ifout,110) 1, 0.0d0, 0.0d0, 0.0d0, 0.0d0
      write(ifout,110) 2, 0.0d0, 0.0d0, 0.0d0, 1.0d0
      write(ifout,110) 3, 0.0d0, 2.0d0, 0.0d0, 0.0d0
      write(ifout,110) 4, 0.0d0, 0.0d0, 3.0d0, 0.0d0

c      write(ifout,'(3x2i4)') 1 , 1

      do ix=-nx,nx
      do iy=-ny,ny
         write(ifout,120) val(ix,iy,:,id)
      enddo
      enddo

      close(ifout)
      enddo
      
      end program test

