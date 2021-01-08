program Program
    
    implicit none

    real :: current, leakage_current, charge, boltzmann_const
    real, dimension(3) :: T_Fahrenheit, T_Kelvin
    integer :: i = 1
    integer :: voltage
    leakage_current = 2.0e-6
    charge = 1.602e-19
    boltzmann_const = 1.38e-23

    open (unit = 1, file = "Input.txt", status = 'old' )
        read (1,*,end = 10)  T_Fahrenheit(1), T_Fahrenheit(2), T_Fahrenheit(3)
    10 close (unit = 1)
    !CLosing Input file after reading from it

    do while ( i <= 3)
        T_Kelvin(i) = ( T_Fahrenheit(i) - 32 ) * ( 5.0 / 9.0 ) + 273.15
        i = i + 1
    end do

    i = 1
    open(unit=2 , file="Output.txt")
        do while (i <= 3)
            voltage = -1.0
            write (2, *) "At a temperature of ", T_Fahrenheit(i), "°F or", T_Kelvin(i), "K"
            write (2, *)
            write (2, *)
            write (2, *) "  Voltage             Current"
            write (2, *)

            do voltage = -10, 6
                current = leakage_current * ( exp(( charge * (voltage / 10.0) ) / ( boltzmann_const * T_Kelvin(i) )) - 1 )
                write (2, 3) (voltage / 10.0), "        ",current 
                3 format (f8.1, a10 , f13.7)
            end do
            write (2, *)
            i = i + 1
        end do
    close ( unit = 2 ) !Closing the file after printing in it

end program Program