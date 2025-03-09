program ana
    real :: string_length, time_end, x_step, t_step, phase_vel, lambda, initial_pos, width, g
    common initial_pos, width, phase_vel, string_length

    integer :: nt, nx, i, j

    real, dimension(:,:), allocatable :: y, ya

    write(*,'("Enter the length of the string : ")', advance = "no")
    read *, string_length
    write(*, '("Emter the final time : ")', advance = "no")
    read *, time_end
    write(*,'("Enter the constant A : ")', advance = "no")
    read *, phase_vel
    write(*,'("Enter the x step size : ")', advance = "no")
    read *, x_step
    write(*,'("Enter the t step size : ")', advance = "no")
    read *, t_step

    initial_pos = string_length/2
    width = string_length/5
    nt = time_end / t_step
    nx = string_length / x_step

    lambda = phase_vel*phase_vel*t_step*t_step/(x_step*x_step)
    allocate(y(0:nx, 0:nt), ya(0:nx, 0:nt))

    open(19, file = "wavepulse_a.txt")
    !Analytical Methos
    do j = 0, nt
            do i = 0, nx
                    ya(i,j) = g(i*x_step, j*t_step)
                    write(19, '(F0.4, 2X, F0.4)') i*x_step, ya(i,j)
            end do
            write(19,'(/)') 
    end do
    close(19)
    do j = 0, nt
            y(0, j) = 0
            y(nx, j) = 0
    end do
   do i = 1, nx - 1
    y(i,0) = g(i*x_step,0.0)
end do

! For the second initial values the pulse has moved a distance v dt along the string
init_pos = init_pos + phase_vel*t_step
do i = 1, nx-1
    y(i,1) = g(i*x_step,t_step)
end do

! Now do the calculation
do j = 1, nt-1
    do i = 1, nx-1
      y(i,j+1) = 2*y(i,j) +  lambda*(y(i+1,j) - 2*y(i,j) + y(i-1,j)) - y(i,j-1)
    end do
end do

open(19, file = "wavepulse.txt")
do j = 0, nt
    do i = 0, nx
        write(19,'(F0.4, 1X, F0.4, 1x, F0.4)') x_step*i, y(i,j)
    end do
    write(19, '(/)')
end do

close(19)

    deallocate(ya,y)
end program ana

function g(x,t)
    real x, t, g, initial_pos, width, phase_vel, string_length
    common initial_pos, width, phase_vel, string_length

    g = 0.5*exp(-(((x - phase_vel*t) + initial_pos)/width)**2)
    g = g - 0.5*exp(-(((x + phase_vel*t) + initial_pos - string_length)/width)**2)

    return
end function g