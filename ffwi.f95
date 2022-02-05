program ffwi
! program no.: f-95
! fortran 95 program to calculate canadian forest
! fire weather index for a DEC PDP-11 computer.
! reads data and prints out in metric units.
    use FFWIndices
    implicit none 

    ! variables for file reading
    character(len=20) :: fname
    logical :: lexist
    ! variables used for calculations
    integer, dimension(12) :: len_month
    real, dimension(12) :: day_length_dmc, day_length_dc
    real :: prev_ffmc, prev_dmc, prev_dc
    integer :: start_month, days_of_data, num_daily_entries
    real, dimension(365) :: temp_arr, rain_arr
    integer, dimension(365) :: humidity_arr, wind_arr

    write(*,1004)
    1004  format(5x,'FOREST FIRE WEATHER INDEX')

    ! error checking and reading input file
    write(*,*) 'Enter the filename to read from:'
    read (*,'(A)') fname
    inquire(file=fname, exist=lexist)
    if (lexist) then
        call read_file(fname, len_month, day_length_dmc, day_length_dc, prev_ffmc, prev_dmc, prev_dc, & 
                start_month, days_of_data, num_daily_entries, temp_arr, rain_arr, humidity_arr, wind_arr)
    else
        write (*,*) 'Invalid input file name. Exiting program...'
    end if 

    call perform_calcs(len_month, day_length_dmc, day_length_dc, prev_ffmc, prev_dmc, prev_dc, & 
                start_month, days_of_data, num_daily_entries, temp_arr, rain_arr, humidity_arr, wind_arr)


contains 


! read input file
subroutine read_file(fname, len_month, day_length_dmc, day_length_dc, prev_ffmc, prev_dmc, prev_dc, &
            start_month, days_of_data, num_daily_entries, temp_arr, rain_arr, humidity_arr, wind_arr)
    implicit none 
    
    character (len=20), intent(in) :: fname
    integer :: stat, index

    ! variables used for calculations
    integer, dimension(12), intent(out) :: len_month
    real, dimension(12), intent(out) :: day_length_dmc, day_length_dc
    real, intent(out) :: prev_ffmc, prev_dmc, prev_dc
    integer, intent(out) :: start_month, days_of_data, num_daily_entries
    real, dimension(365), intent(out) :: temp_arr, rain_arr
    integer, dimension(365), intent(out) :: humidity_arr, wind_arr

    ! opening file to read; unit is used to represent file - can be anything but the value of 6
    open(unit=20,file=fname,status='old',action='read')

    ! read length of months and day-length factors
    do index=1,12
        read(20,'(i2,f4.1,f4.1)') len_month(index), day_length_dmc(index), day_length_dc(index)
    end do

    ! read initial moisture code values, starting month, and # of days weather data is provided that month
    read(20,'(f4.1,f4.1,f5.1,i2,i2)') prev_ffmc, prev_dmc, prev_dc, start_month, days_of_data

    ! read daily weather data 
    index = 1
    do
        read(20,'(f4.1,i4,i4,f4.1)',IOSTAT=stat) temp_arr(index), humidity_arr(index), wind_arr(index), rain_arr(index)
        if (IS_IOSTAT_END(stat)) exit
        index = index + 1
    end do
    num_daily_entries = index-1

    ! close file after all data has been stored
    close(20, status='keep')

    return
end subroutine read_file


end program ffwi
