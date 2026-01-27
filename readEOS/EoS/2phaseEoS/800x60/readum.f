      parameter (Nx0=601,Nt0=801)
      dimension umbar(Nt0,Nx0),umstr(Nt0,Nx0)
* baryon chemical potential
      open (unit=10,status='old',file='umbar.dat')
      DO i=1,Nt0
	 read (10,*) (umbar(i,j),j=1,Nx0)
	 read (10,*) (umstr(i,j),j=1,Nx0)
      END DO
      close (10)
* baryon chemical potential
      open (unit=10,status='old',file='umbar.dat')
      DO i=1,Nt0
	 write (10,*) (umbar(i,j),j=1,Nx0)
      END DO
      close (10)
* strange potential
      open (unit=10,status='old',file='umstr.dat')
      DO i=1,Nt0
	 write (10,*) (umstr(i,j),j=1,Nx0)
      END DO
      close (10)

      STOP
      END