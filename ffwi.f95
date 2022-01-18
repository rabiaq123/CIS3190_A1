!     program no.: f-4
!     fortran iv program to calculate canadian forest
!     fire weather index for a dec pdp 11 at p.f.e.s.
!     reads data and prints out in metric units.
      implicit none

      real :: fo, po, dot, r, rain, f, fr, ra, wmo, ed, ew, wm, z, x, ffm, t, tx, rk, pr, rw, wmi, b, wmr, dmc, pe
      real :: smi, dr, fm, sf, si, bui, p, cc, bb, sl, fwi, c, dc
      integer :: j, m, l, i, ndays, idays, nn, ih, h, iw, w, iffm, idmc, idc, isi, ibui, ifwi
      integer, dimension(12) :: lmon
      real, dimension(12) :: el, fl

      ! dimension lmon(12), el(12), fl(12)
      write(*,1004)
1004  format(2x,'program no.: f-4')
100   format(i2,f4.1,f4.1)
101   format(f4.1,i4,i4,f4.1)
102   format(f4.1,f4.1,f5.1,i2,i2)
!
!     reads length of months, and day length factors
!
      do 20 j=1,12
      read(*,100) lmon(j), el(j), fl(j)
20    continue
!
!     reads initial values of ffmc, dmc, dc, starting month and number
!     of days of data starting month.
!
      read(*,102) fo,po,dot,m,ndays
      do 25 j=m,12
      nn=lmon(j)
1002  format(10(/),1x,'  date  temp  rh   wind  rain   ffmc   dmc   dc   isi   bui   fwi'/)
      if(j.eq.m) go to 304
      idays=1
      go to 302
304   idays=lmon(j)-ndays+1
!
!     reads daily weather data
!
302   l=0
      do 25 i=idays,nn
      l=l+1
      read(*,101,end=2000) t,ih,iw,r
      if(l.ne.1) go to 301
      write(*,1002)
301   tx=t
      h=ih
      w=iw
      rain=r
!
!     fine fuel moisture code
!
      if(r.gt.0.5) go to 10
      r=0.0
      fr=fo
      go to 150
10    ra=r
      if(ra.le.1.45) go to 6
      if(ra-5.75) 9,9,12
6     f=123.85-(55.6*alog(ra+1.016))
      go to 13
9     f=57.87-(18.2*alog(ra-1.016))
      go to 13
12    f=40.69-(8.25*alog(ra-1.905))
13    c=8.73*exp(-0.1117*fo)
      fr=(fo/100.)*f+(1.0-c)
      if(fr.ge.0.) go to 150
      fr=0.0
150   wmo=101.-fr
      ed=0.942*(h**0.679)+(11.*exp((h-100.)/10.))+0.18*(21.1-t)*(1.-1./exp(0.115*h))
      if(wmo-ed) 26,27,28
26    ew=0.618*(h**0.753)+(10.*exp((h-100.)/10.))+0.18*(21.1-t)*(1.-1./exp(0.115*h))
      if(wmo.lt.ew) go to 29
27    wm=wmo
      go to 30
28    z=0.424*(1.-(h/100.)**1.7)+(0.0694*(w**0.5))*(1.-(h/100.)**8)
      x=z*(0.463*(exp(0.0365*t)))
      wm=ed+(wmo-ed)/10.**x
      go to 30
29    wm=ew-(ew-wmo)/1.9953
30    ffm=101.-wm
      if(ffm.gt.101.) go to 32
      if(ffm) 33,34,34
32    ffm=101.
      go to 34
33    ffm=0.0
!
!     duff moisture code
!
34    if(t+1.1.ge.0.) go to 41
      t=-1.1
41    rk=1.894*(t+1.1)*(100.-h)*(el(j)*0.0001)
43    if(r.gt.1.5) go to 45
      pr=po
      go to 250
45    ra=r
      rw=0.92*ra-1.27
      wmi=20.0+280./exp(0.023*po)
      if(po.le.33.) go to 50
      if(po-65.) 52,52,53
50    b=100./(0.5+0.3*po)
      go to 55
52    b=14.-1.3*alog(po)
      go to 55
53    b=6.2*alog(po)-17.2
55    wmr=wmi+(1000.*rw)/(48.77+b*rw)
      pr=43.43*(5.6348-alog(wmr-20.))
250   if(pr.ge.0.) go to 61
      pr=0.0
61    dmc=pr+rk
!
!     drought code
!
      if(t+2.8.ge.0.) go to 65
      t=-2.8
65    pe=(.36*(t+2.8)+fl(j))/2.
      if(r.le.2.8) go to 300
      ra=r
      rw=0.83*ra-1.27
      smi=800.*exp(-dot/400.)
      dr=dot-400.*alog(1.+((3.937*rw)/smi))
      if(dr.gt.0.) go to 83
      dr=0.0
83    dc=dr+pe
      go to 350
300   dr=dot
      go to 83
350   if(dc.ge.0.) go to 85
      dc=0.0
!
!     initial spread index, buildup index, fire weather index
!
85    fm=101.-ffm
      sf=19.1152*exp(-0.1386*fm)*(1.+fm**4.65/7950000.)
      si=sf*exp(0.05039*w)
93    bui=(0.8*dc*dmc)/(dmc+0.4*dc)
      if(bui.ge.dmc) go to 95
      p=(dmc-bui)/dmc
      cc=0.92+(0.0114*dmc)**1.7
      bui=dmc-(cc*p)
      if(bui.lt.0.) bui=0.
95    if(bui.gt.80.) go to 60
      bb=0.1*si*(0.626*bui**0.809+2.)
      go to 91
60    bb=0.1*si*(1000./(25.+108.64/exp(0.023*bui)))
91    if(bb-1.0.le.0.) go to 98
      sl=2.72*(0.43*alog(bb))**0.647
      fwi=exp(sl)
      go to 400
98    fwi=bb
400   idc=dc+0.5
      iffm=ffm+0.5
      idmc=dmc+0.5
      isi=si+0.5
      ibui=bui+0.5
      ifwi=fwi+0.5
      write(*,1001) j,i,tx,ih,iw,rain,iffm,idmc,idc,isi,ibui,ifwi
1001  format(1x,2i3,f6.1,i4,i6,f7.1,6i6)
      fo=ffm
      po=dmc
      dot=dc
25    continue
2000  stop
      end
