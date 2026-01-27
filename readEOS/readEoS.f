      SUBROUTINE READEOS
*--------------------------------------------------------------*
* INPUT:                                                       *
* Xmin   = lower limit of baryon density , in units N0;        *
* Xmax   = upper limit of baryon density , in units N0;        *
* Tmax   = upper limit of temperature,  MeV;                   *
* nunit  = reference number of the disk file                   *
* fname  ='./EoS/xxxxxxEoS/enpr800x60.dat'      = file name    *
*         for baryon-rich matter                               *
* fnamebo='./EoS/xxxxxxEoS/epn800b0.dat'        = file name    *
*         for baryon-free matter                               *
* wplname='./EoS/xxxxxxEoS/800x60/wpl800x60.dat'= file name    *
*         of weights of QGP (=V_{QGP}/V_{tot})                 *
*    These are paths from directory where the code is executed *
*--------------------------------------------------------------*
* OUTPUT:                                                      *
* ======================                                       *
* Nx0= number of points in [Xmin,Xmax], linear    scale        *
* Nt0= number of points in [0,Tmax],    linear    scale        *
* Read from disk:                                              *
* ecmp(x)) = energy density at T=0, units (Mn*N0)              *
* pcmp(x)  = pressure at T=0, units (Mn*N0)                    *
* enth(T,x)= barion-rich thermal energy density, units (Mn*N0) *
* prth(T,x)= barion-rich thermal pressure, units (Mn*N0)       *
* yth (T)  = barion-free thermal energy density, units (Mn*N0) *
* pth (T)  = barion-free thermal pressure, units (Mn*N0)       *
*            before reading off yth and pth are working arrays *
* wpl(T,x) = weights of QGP in barion-rich fluid               *
* wpl0(T)  = weights of QGP in barion-free fluid               *
*--------------------------------------------------------------*
*   sr* = scalar density                                       *
*   rr* = vector density (zero components)                     *
*   *Qrk = for unbound Quarks                                  *
*   *mes = for mesons                                          *
*   *Glu = for gluons                                          *
*   *bar = for baryons                                         *
*   *bar = for baryons                                         *
*  umbar = baryon chemical potential [MeV]                     *
*  umstr = strange potential [MeV]                             *
*                                                              *
*--------------------------------------------------------------*
      common /vsdens/srQrk(Nt0,Nx0),srmes(Nt0,Nx0),
     *               srGlu(Nt0,Nx0),srbar(Nt0,Nx0),
     *               rrQrk(Nt0,Nx0),rrmes(Nt0,Nx0),
     *               rrGlu(Nt0,Nx0),rrbar(Nt0,Nx0),
     *               umbar(Nt0,Nx0),umstr(Nt0,Nx0)
      common /thenprt/ enth(Nt0,Nx0),prth(Nt0,Nx0),
     *                 yth(Nt0),pth(Nt0),temp(Nt0)
      common/compression/ecmp(Nx0),pcmp(Nx0),xb(Nx0)
      common /qgp/ wpl0(Nt0),wpl(Nt0,Nx0)
*--------------------------------------------------------------*
      character*(*)  fname,fnameb0,wplname,eostxt,fumbar,fumstr
      parameter (Nx0=601,Nt0=801)
      parameter (Xmin=0.01,Xmax=60.,Tmax=800.)
      parameter (nunit=12)
      parameter (lout=11)
*--------------------------------------------------------------*
*
      fname  ='./EoS/mixEoS/enpr800x60.dat'      
*         for baryon-rich matter                               *
      fnameb0='./EoS/mixEoS/epn800b0.dat'        
*         for baryon-free matter                               *
      wplname='./EoS/mixEoS/wpl800x60.dat'
*         weights of QGP (=V_{QGP}/V_{tot})                    *
      fumbar='./EoS/mixEoS//800x60/umbar.dat'
*         Baryonic chemical potential [in MeV]                 *
      fumstr='./EoS/mixEoS//800x60/umstr.dat
*         Strange chemical potential [in MeV]                  *
*
*--baryon rich-------------------------------------------------
      tmstep=Tmax/float(Nt0-1) ! temperature step
      xbstep=(Xmax-Xmin)/float(Nx0-1) ! step in nB/n0
      Tmin=0.
      do i=1,Nt0
       temp(i)=Tmin+float(i-1)*tmstep
      enddo
      do j=1,Nt0
       xb(j)=xmin+(xmax-xmin)*float(j-1)/float(Nx0-1)
      enddo
      open (unit=nunit,status='old',file=fname)
      read(nunit,*)(ecmp(j),j=1,Nx0)
*      print *,'ecmp=',ecmp(1),ecmp(Nx0)
      read(nunit,*)(pcmp(j),j=1,Nx0)
*      print *,'pcmp=',pcmp(1),pcmp(Nx0)
      do i=1,Nt0
       read(nunit,*)(enth(i,j),j=1,Nx0)
*       print *,'enth, T',i
      enddo
      do i=1,Nt0
       read(nunit,*)(prth(i,j),j=1,Nx0)
*       print *,'prth, T',i
      enddo
      close (nunit)
*
*--baryon free--------------------------------------------------
      open (unit=nunit,status='old',file=fnameb0)
      read(nunit,*)(yth(i),i=2,Nt0)
      read(nunit,*)(pth(i),i=2,Nt0)
      close (nunit)
      yth(1)=0.
      pth(1)=0.
*
*--QGP fraction--------------------------------------------------
      open (unit=nunit,status='old',file=wplname)
       wpl0(1)=0.
       read (nunit,*)(wpl0(i),i=2,Nt0)
       do i=1,Nt0
        read(nunit,*)(wpl(i,j),j=1,Nx0)
       enddo
      close (nunit)
*
*---Baryonic chemical potential----------------------------------
      open (unit=nunit,status='old',file=fumbar)
       do i=1,Nt0
        read(nunit,*)(umbar(i,j),j=1,Nx0)
       enddo
      close (nunit)
*
*---Strange chemical potential-----------------------------------
      open (unit=nunit,status='old',file=fumstr)
      do i=1,Nt0
       read(nunit,*)(umstr(i,j),j=1,Nx0)
      enddo
      close (unit=nunit)
*----------------------------------------------------------------
*
      eostxt='./EoS.txt'
      OPEN (unit=lout,file=eostxt,
     *      status='unknown',form='formatted')
        write (lout,'(1x,23(1h*),'' report***'',22(1h*)/
     *  1x,1h*,2x,''Xmin='',f5.1,'', Xmax='',f5.1,
     *  '', Tmax='',f6.1,T63,1h*/1x,1h*,2x,''Nx0='',i3,
     *  '', Nt0='',i3,T63,1h*/1x,1h*,2x,''fname='',a50,T63,1h*/
     *  1x,1h*,2x,''fnameB0='',a50,T63,1h*/
     *  1x,62(1h*))') Xmin,Xmax,Tmax,Nx0,Nt0,fname,fnameb0
        write (lout,'(a50)') fnameb0
      close (lout)
*----------------------------------------------------------------
      END

