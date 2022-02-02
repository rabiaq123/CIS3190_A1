      program ffwi
!     program no.: f-4
!     fortran iv program to calculate canadian forest
!     fire weather index for a dec pdp 11 at p.f.e.s.
!     reads data and prints out in metric units.
      implicit none

      real :: prev_ffmc, prev_dmc, prev_dc, noon_rain, rain, f, fr, ra, wmo, ed, ew, wm, z, x, ffm, noon_temp, temp 
      real :: rk, pr, rw, wmi, b, wmr, dmc, pe
      real :: smi, dr, fm, sf, si, bui, p, cc, bb, sl, fwi, c, dc
      integer :: j, l, i
      integer :: start_month, days_of_data, idays, days_in_month, noon_humidity, humidity, noon_wind, wind
      integer :: iffm, idmc, idc, isi
      integer :: ibui, ifwi
      integer, dimension(12) :: len_month
      real, dimension(12) :: day_length_dmc, day_length_dc

      write(*,1004)
1004  format(2x,'Forest Fire Weather Index')


!     INPUT VALUE FORMATTING

!     lengths of months and day-length factors
100   format(i2,f4.1,f4.1)
!     daily weather readings (temp, relative humidity, wind, rain)
101   format(f4.1,i4,i4,f4.1)
!     moisture codes' std starting values, starting month, and # of days data is provided
102   format(f4.1,f4.1,f5.1,i2,i2)

!     INPUT FILE READING 

!     read length of months and day-length factors
      do 20 j=1,12
      read(*,100) len_month(j), day_length_dmc(j), day_length_dc(j)
20    continue

!     read initial values of ffmc, dmc, dc, starting month, and
!     # of days weather data is provided that month.
      read(*,102) prev_ffmc, prev_dmc, prev_dc, start_month, days_of_data
      do 25 j=start_month,12
      days_in_month=len_month(j)
1002  format(10(/),1x,'  date  temp  rh   wind  rain   ffmc   dmc   dc   isi   bui   fwi'/)
      if(j==start_month) go to 304
      idays=1
      go to 302
304   idays=len_month(j)-days_of_data+1

!     read daily weather data
302   l=0
      do 25 i=idays,days_in_month
      l=l+1
      read(*,101,end=2000) noon_temp,noon_humidity,noon_wind,noon_rain
      if(l/=1) go to 301
      write(*,1002)
301   temp=noon_temp
      humidity=noon_humidity
      wind=noon_wind
      rain=noon_rain

!     fine fuel moisture code
      if(noon_rain>0.5) go to 10
      noon_rain=0.0
      fr=prev_ffmc
      go to 150
10    ra=noon_rain
      if(ra<=1.45) go to 6
      if(ra-5.75) 9,9,12
6     f=123.85-(55.6*alog(ra+1.016))
      go to 13
9     f=57.87-(18.2*alog(ra-1.016))
      go to 13
12    f=40.69-(8.25*alog(ra-1.905))
13    c=8.73*exp(-0.1117*prev_ffmc)
      fr=(prev_ffmc/100.)*f+(1.0-c)
      if(fr>=0.) go to 150
      fr=0.0
150   wmo=101.-fr
      ed=0.942*(humidity**0.679)+(11.*exp((humidity-100.)/10.))+0.18*(21.1-noon_temp)*(1.-1./exp(0.115*humidity))
      if(wmo-ed) 26,27,28
26    ew=0.618*(humidity**0.753)+(10.*exp((humidity-100.)/10.))+0.18*(21.1-noon_temp)*(1.-1./exp(0.115*humidity))
      if(wmo<ew) go to 29
27    wm=wmo
      go to 30
28    z=0.424*(1.-(humidity/100.)**1.7)+(0.0694*(wind**0.5))*(1.-(humidity/100.)**8)
      x=z*(0.463*(exp(0.0365*noon_temp)))
      wm=ed+(wmo-ed)/10.**x
      go to 30
29    wm=ew-(ew-wmo)/1.9953
30    ffm=101.-wm
      if(ffm>101.) go to 32
      if(ffm) 33,34,34
32    ffm=101.
      go to 34
33    ffm=0.0

!     duff moisture code
34    if(noon_temp+1.1>=0.) go to 41
      noon_temp=-1.1
41    rk=1.894*(noon_temp+1.1)*(100.-humidity)*(day_length_dmc(j)*0.0001)
43    if(noon_rain>1.5) go to 45
      pr=prev_dmc
      go to 250
45    ra=noon_rain
      rw=0.92*ra-1.27
      wmi=20.0+280./exp(0.023*prev_dmc)
      if(prev_dmc<=33.) go to 50
      if(prev_dmc-65.) 52,52,53
50    b=100./(0.5+0.3*prev_dmc)
      go to 55
52    b=14.-1.3*alog(prev_dmc)
      go to 55
53    b=6.2*alog(prev_dmc)-17.2
55    wmr=wmi+(1000.*rw)/(48.77+b*rw)
      pr=43.43*(5.6348-alog(wmr-20.))
250   if(pr>=0.) go to 61
      pr=0.0
61    dmc=pr+rk

!     drought code
      if(noon_temp+2.8>=0.) go to 65
      noon_temp=-2.8
65    pe=(.36*(noon_temp+2.8)+day_length_dc(j))/2.
      if(noon_rain<=2.8) go to 300
      ra=noon_rain
      rw=0.83*ra-1.27
      smi=800.*exp(-prev_dc/400.)
      dr=prev_dc-400.*alog(1.+((3.937*rw)/smi))
      if(dr>0.) go to 83
      dr=0.0
83    dc=dr+pe
      go to 350
300   dr=prev_dc
      go to 83
350   if(dc>=0.) go to 85
      dc=0.0

!     initial spread index, buildup index, fire weather index
85    fm=101.-ffm
      sf=19.1152*exp(-0.1386*fm)*(1.+fm**4.65/7950000.)
      si=sf*exp(0.05039*wind)
93    bui=(0.8*dc*dmc)/(dmc+0.4*dc)
      if(bui>=dmc) go to 95
      p=(dmc-bui)/dmc
      cc=0.92+(0.0114*dmc)**1.7
      bui=dmc-(cc*p)
      if(bui<0.) bui=0.
95    if(bui>80.) go to 60
      bb=0.1*si*(0.626*bui**0.809+2.)
      go to 91
60    bb=0.1*si*(1000./(25.+108.64/exp(0.023*bui)))
91    if(bb-1.0<=0.) go to 98
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
      write(*,1001) j,i,temp,noon_humidity,noon_wind,rain,iffm,idmc,idc,isi,ibui,ifwi
1001  format(1x,2i3,f6.1,i4,i6,f7.1,6i6)
      prev_ffmc=ffm
      prev_dmc=dmc
      prev_dc=dc
25    continue
2000  stop
      end
