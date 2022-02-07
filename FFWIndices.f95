module FFWIndices

contains


! perform all calculations and print values to output file
subroutine perform_calcs(output_fname, len_month, day_length_dmc, day_length_dc, prev_ffmc, prev_dmc, prev_dc, & 
                start_month, days_of_data, num_daily_entries, temp_arr, rain_arr, humidity_arr, wind_arr)
    implicit none 

    character(len=20), intent(in) :: output_fname
    ! for section 1 of the input file
    integer, dimension(12), intent(in) :: len_month
    real, dimension(12), intent(in) :: day_length_dmc, day_length_dc
    ! for section 2 of the input file
    real, intent(inout) :: prev_ffmc, prev_dmc, prev_dc 
    integer, intent(in) :: start_month, days_of_data, num_daily_entries
    ! for section 3 of the input file
    real, dimension(366), intent(in) :: temp_arr, rain_arr
    integer, dimension(366), intent(in) :: humidity_arr, wind_arr

    integer :: idx = 1, data_start_date
    real :: prev_rain, effective_rain
    ! local variables for daily weather data
    real :: rain, temp
    integer :: days_in_month, humidity, wind
    ! variables used for final output
    integer :: month, date, int_ffmc, int_dmc, int_dc, int_isi, int_bui, int_fwi
    ! FFMC, DMC, DC variables
    real :: ffmc, curr_final_mc, dmc, dc
    ! FWI, ISI, BUI variables
    real :: isi, bui, fwi

    ! formatting for headings in output file
13  format(/,1x,'  date  temp  rh   wind  rain   ffmc   dmc   dc   isi   bui   fwi')
    ! formatting for data to be output to the requested file
15  format(1x,2i3,f6.1,i4,i6,f7.1,6i6)

    ! open output file for printing
    open(unit=25,file=output_fname,status='replace',action='write')
    call output_legend()

    do month=start_month,12
        days_in_month=len_month(month)
        if(month==start_month) then
            data_start_date=len_month(month)-days_of_data+1
        else
            data_start_date=1
        end if

        ! calculate and print results
        do date=data_start_date,days_in_month
            ! exit loop if all daily weather data entries have been accounted for in output file calculations
            if (idx == num_daily_entries + 1) exit
            if(date == data_start_date) write(25,13)
            temp=temp_arr(idx)
            rain=rain_arr(idx)
            humidity=humidity_arr(idx)
            wind=wind_arr(idx)
            
            ! calculate fine fuel moisture code, duff moisture code, and drought code
            call calc_ffmc(ffmc, rain, humidity, wind, temp, prev_ffmc, prev_rain, curr_final_mc)
            call calc_dmc(dmc, month, day_length_dmc, temp, prev_rain, humidity, rain, prev_dmc, effective_rain)
            call calc_dc(dc, temp, day_length_dc, month, rain, prev_dc, prev_rain, effective_rain)
            
            ! calculate initial spread index, buildup index, and fire weather index
            call calc_isi(isi, wind, ffmc)
            call calc_bui(bui, dc, dmc)
            call calc_fwi(fwi, bui, isi)

            ! convert necessary values to int format and write to file
            call round_results(int_dc, int_ffmc, int_dmc, int_isi, int_bui, int_fwi, dc, ffmc, dmc, isi, bui, fwi)
            write(25,15) month, date, temp_arr(idx), humidity_arr(idx), wind_arr(idx), rain_arr(idx), & 
                        int_ffmc, int_dmc, int_dc, int_isi,int_bui,int_fwi

            idx = idx + 1
            prev_ffmc=ffmc
            prev_dmc=dmc
            prev_dc=dc
        end do
    end do

    ! close file after all data has been stored
    close(25, status='keep')

    return
end subroutine perform_calcs


! print legend for headings in output file
subroutine output_legend()
    implicit none 

    write(25,*) 'FOREST FIRE WEATHER INDEX CALCULATIONS'
    write(25,*) '--------------------------------------'
    write(25,*) 'LEGEND AND UNITS'
    write(25,*) 'temp   - temperature (°C)'
    write(25,*) 'rh     - relative humidity (%)'
    write(25,*) 'wind   - wind speed (km/h)'
    write(25,*) 'rain   - rainfall (mm)'
    write(25,*) 'ffmc   - fine fuel moisture code'
    write(25,*) 'dmc    - duff moisture code'
    write(25,*) 'dc     - drought code'
    write(25,*) 'isi    - initial spread index'
    write(25,*) 'bui    - buildup index'
    write(25,*) 'fwi    - fire weather index'

    return 
end subroutine output_legend


! calculate fine fuel moisture code
subroutine calc_ffmc(ffmc, rain, humidity, wind, temp, prev_ffmc, prev_rain, curr_final_mc)
    implicit none 

    real, intent(in) :: temp, prev_ffmc
    real, intent(out):: prev_rain, curr_final_mc, ffmc
    real, intent(inout) :: rain
    integer, intent(in) :: humidity, wind
    real :: rain_func_ffmc, correction_term_ffmc, prev_mc, drying_emc, wetting_emc
    real :: log_drying_rate, intermediate_drying_rate, post_rain_ffmc

    ! rainfall routine must be skipped in dry weather
    if(rain > 0.5) then
        prev_rain=rain
        if(prev_rain <= 1.45) then
            rain_func_ffmc=123.85-(55.6*alog(prev_rain+1.016))
        else 
            if(prev_rain < 5.75) then
                rain_func_ffmc=57.87-(18.2*alog(prev_rain-1.016))
            else if(prev_rain == 5.75) then
                rain_func_ffmc=57.87-(18.2*alog(prev_rain-1.016))
            else
                rain_func_ffmc=40.69-(8.25*alog(prev_rain-1.905))
            end if
        end if
        correction_term_ffmc=8.73*exp(-0.1117*prev_ffmc)
        post_rain_ffmc=(prev_ffmc/100.)*rain_func_ffmc+(1.0-correction_term_ffmc)
        ! FFMC after rain cannot theoretically be less than 0
        if(post_rain_ffmc<0.) post_rain_ffmc=0.0
    else 
        ! not enough rain to have it affect the FFMC
        rain=0.0
        post_rain_ffmc=prev_ffmc
    end if

    prev_mc=101.-post_rain_ffmc
    drying_emc=0.942*(humidity**0.679)+(11.*exp((humidity-100.)/10.))+0.18*(21.1-temp)*(1.-1./exp(0.115*humidity))

    ! calculating current day's final moisture content (mc)
    if(prev_mc-drying_emc < 0) then
        ! if starting mc is less than drying emc, calculate wetting emc 
        wetting_emc=0.618*(humidity**0.753)+(10.*exp((humidity-100.)/10.))+0.18*(21.1-temp)*(1.-1./exp(0.115*humidity))
        ! if starting mc is also less than wetting emc, calculate day's final mc
        if(prev_mc < wetting_emc) curr_final_mc=wetting_emc-(wetting_emc-prev_mc)/1.9953
    else if(prev_mc == drying_emc) then
        ! if starting mc and drying emc are the same, make day's final mc equal to the previous mc
        curr_final_mc=prev_mc
    else
        ! perform the following if previous mc is greater than drying emc
        intermediate_drying_rate=0.424*(1.-(humidity/100.)**1.7)+(0.0694*(wind**0.5))*(1.-(humidity/100.)**8)
        log_drying_rate=intermediate_drying_rate*(0.463*(exp(0.0365*temp)))
        curr_final_mc=drying_emc+(prev_mc-drying_emc)/10.**log_drying_rate
    end if 

    ! calculating FFMC
    ffmc=101.-curr_final_mc
    if(ffmc > 101.) then
        ffmc=101.
    else if(ffmc < 0) then 
        ffmc=0.0
    end if

    return
end subroutine calc_ffmc


! calculate duff moisture code
subroutine calc_dmc(dmc, month, day_length_dmc, temp, prev_rain, humidity, rain, prev_dmc, effective_rain) 
    implicit none 

    integer, intent(in) :: humidity, month
    real, intent(in) :: rain, prev_dmc
    real, dimension(12), intent(in) :: day_length_dmc
    real, intent(out) :: effective_rain, dmc
    real, intent(inout) :: temp, prev_rain
    real :: drying_factor_dmc, mc_dmc, slope_func_dmc, mc_post_rain_dmc, post_rain_dmc

    ! values less than -1.1 for noon temperature cannot be used to calculate drying factor (i.e. log drying rate in DMC)
    if(temp < -1.1) temp=-1.1
    drying_factor_dmc=1.894*(temp+1.1)*(100.-humidity)*(day_length_dmc(month)*0.0001)

    ! rainfall routine must be skipped in dry weather
    if(rain > 1.5) then
        prev_rain=rain
        effective_rain=0.92*prev_rain-1.27
        mc_dmc=20.0+280./exp(0.023*prev_dmc)
        ! calculating slope variable in DMC rain effect
        if(prev_dmc <= 33.) then
            slope_func_dmc=100./(0.5+0.3*prev_dmc)
        else
            if (prev_dmc < 65.) then
                slope_func_dmc=14.-1.3*alog(prev_dmc)
            else if (prev_dmc == 65.) then
                slope_func_dmc=14.-1.3*alog(prev_dmc)
            else
                slope_func_dmc=6.2*alog(prev_dmc)-17.2
            end if
        end if 
        mc_post_rain_dmc=mc_dmc+(1000.*effective_rain)/(48.77+slope_func_dmc*effective_rain)
        post_rain_dmc=43.43*(5.6348-alog(mc_post_rain_dmc-20.))
    else 
        post_rain_dmc=prev_dmc
    end if

    ! DMC after rain cannot theoretically be less than 0
    if(post_rain_dmc < 0.) post_rain_dmc=0.0
    dmc=post_rain_dmc+drying_factor_dmc

    return
end subroutine calc_dmc


! calculate drought code
subroutine calc_dc(dc, temp, day_length_dc, month, rain, prev_dc, prev_rain, effective_rain)
    implicit none 
    
    integer, intent(in) :: month
    real, dimension(12), intent(in) :: day_length_dc
    real, intent(in) :: rain, prev_dc
    real, intent(out) :: dc
    real, intent(inout) :: temp, prev_rain, effective_rain
    real :: drying_factor_dc, post_rain_dc, moisture_equivalent_dc

    ! values less than -2.8 for noon temperature cannot be used to calculate drying factor (i.e. potential evapotranspiration)
    if(temp < -2.8) temp=-2.8
    drying_factor_dc=(.36*(temp+2.8)+day_length_dc(month))/2.

    ! calculating DC after rain; rainfall routine must be skipped in dry weather
    if(rain <= 2.8) then
        ! if rainfall in open is not greater than 2.8mm, DC after rain is equal to starting or yesterday's DC
        post_rain_dc=prev_dc
    else 
        prev_rain=rain
        effective_rain=0.83*prev_rain-1.27
        moisture_equivalent_dc=800.*exp(-prev_dc/400.)
        post_rain_dc=prev_dc-400.*alog(1.+((3.937*effective_rain)/moisture_equivalent_dc))
        if(post_rain_dc<=0.) post_rain_dc=0.0
    end if

    ! DC after rain cannot theoretically be less than 0
    dc=post_rain_dc+drying_factor_dc
    if(dc < 0.) dc=0.0

    return 
end subroutine calc_dc


! calculate initial spread index
subroutine calc_isi(isi, wind, ffmc)
    implicit none 

    integer, intent(in) :: wind 
    real, intent(out) :: isi 
    real, intent(inout) :: ffmc
    real :: curr_ff_mc, ff_moisture_func

    curr_ff_mc=101.-ffmc
    ff_moisture_func=19.1152*exp(-0.1386*curr_ff_mc)*(1.+curr_ff_mc**4.65/7950000.)
    isi=ff_moisture_func*exp(0.05039*wind)

    return
end subroutine calc_isi


! calculate buildup index
subroutine calc_bui(bui, dc, dmc)
    implicit none 

    real, intent(in) :: dc, dmc 
    real, intent(out) :: bui
    real :: fix_bui_ratio_func, fix_bui_dmc_func

    bui=(0.8*dc*dmc)/(dmc+0.4*dc)
    if(bui < dmc) then
        ! ratio function to correct BUI when less than DMC
        fix_bui_ratio_func=(dmc-bui)/dmc
        ! DMC function to correct BUI when less than DMC
        fix_bui_dmc_func=0.92+(0.0114*dmc)**1.7
        bui = dmc-(fix_bui_dmc_func*fix_bui_ratio_func)
        if(bui < 0.) bui=0.
    end if

    return
end subroutine calc_bui


! calculate fire weather index
subroutine calc_fwi(fwi, bui, isi)
    implicit none 

    real, intent(in) :: bui, isi 
    real, intent(out) :: fwi 
    real :: intermediate_fwi, log_final_fwi

    ! calculating intermediate FWI
    if(bui > 80.) then
        intermediate_fwi=0.1*isi*(1000./(25.+108.64/exp(0.023*bui)))
    else 
        intermediate_fwi=0.1*isi*(0.626*bui**0.809+2.)
    end if

    ! calculating final FWI
    if(intermediate_fwi <= 1.0) then
        ! if intermediate FWI is not greater than 1, let intermediate and final FWI values be equal
        fwi=intermediate_fwi
    else 
        ! else calculate final FWI from its logarithm
        log_final_fwi=2.72*(0.43*alog(intermediate_fwi))**0.647
        fwi=exp(log_final_fwi)
    end if

    return 
end subroutine calc_fwi


! all moisture code and indices' calculations should be in float, but results should be integers
subroutine round_results(int_dc, int_ffmc, int_dmc, int_isi, int_bui, int_fwi, dc, ffmc, dmc, isi, bui, fwi)
    implicit none

    integer, intent(out) :: int_dc, int_ffmc, int_dmc, int_isi, int_bui, int_fwi
    real, intent(in) :: dc, ffmc, dmc, isi, bui, fwi

    int_dc=nint(dc)
    int_ffmc=nint(ffmc)
    int_dmc=nint(dmc)
    int_isi=nint(isi)
    int_bui=nint(bui)
    int_fwi=nint(fwi)

    return
end subroutine round_results


end module FFWIndices
