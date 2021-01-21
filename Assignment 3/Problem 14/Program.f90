module ConverterModule
    implicit none
    
    contains
        function CoordinateConvertor(cartesian)
            implicit none
            real :: pi = 4 * atan (1.0)
            real, dimension(3) :: cartesian, spherical, CoordinateConvertor
            spherical(1) = sqrt((cartesian(1))**2 + (cartesian(2))**2 + (cartesian(3))**2)
            spherical(2) = 180 * acos(cartesian(1)/spherical(1)) / pi
            spherical(3) = 180 * acos(cartesian(2)/spherical(1)) / pi
            CoordinateConvertor = spherical
        end function CoordinateConvertor
end module ConverterModule
program Program
    use ConverterModule
    implicit none
    real, dimension(3) :: cartesian, spherical
    integer :: i, j
    open (unit = 1, file = "Input.txt", status = 'old' )
        read (1,*,end = 10) (cartesian(j),  j = 1, 3)
    10 close (unit = 1)
    spherical = CoordinateConvertor(cartesian)
    open(unit=2 , file="Output.txt")
        write (2, *) "The spherical coordinates of the point are:", (spherical(j),  j = 1, 3)
    close ( unit = 2 ) !Closing the file after printing in it
end program Program