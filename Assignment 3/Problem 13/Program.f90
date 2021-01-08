program Program
    implicit none
        real, dimension(3, 3) :: Matrix
        integer i, j, a, b, c, d
        real :: det3, det2
        det3 = 0.0
        open (unit = 1, file = "Input.txt", status = 'old' )
        do i = 1, 3
            do j = 1, 3
                read (1,*,end = 10) Matrix(i, j) 
            end do
        end do
        10 close (unit = 1)
        !CLosing Input file after reading from it
        do i = 1, 3
            det2 = determ(a, b, c, d)
            det3 = det3 + ((-1) ** (i + 1)) * det2
        end do
        open(unit=2 , file="Output.txt")
            write (2, *) "The determinant of given matrix is", det3
        close ( unit = 2 ) !Closing the file after printing in it
end program Program

function determ(a, b, c, d) result(det)
    implicit none
    real :: a, b, c, d
    real :: det
    det = a * d - b * c
end function determ