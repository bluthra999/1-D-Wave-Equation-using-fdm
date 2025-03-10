program ana
    implicit none
    real :: string_length, time_end, x_step, t_step, phase_vel, lambda, initial_pos, width, g
    common initial_pos, width, phase_vel, string_length
    integer :: nt, nx, i, j

    real, dimension(:,:), allocatable :: ya

  
    write(*, '("The following program is to calculate the position of a wave genrated by a streched string is space-time")')
    write(*, '("Using 1-D Wave Equation", /, 15X, "u_tt = phase_vel^2 u_xx", /, /)')
    write(*, '("Enter the length of the string : ")', advance = "no")
    read *, string_length
    write(*, '("Enter the final time t : ")', advance = "no")
    read *, time_end
    write(*, '("Enter the step size in x : ")', advance = "no")
    read *,x_step
    write(*, '("Enter the step size in t : ")', advance = "no")
    read *, t_step
    write(*, '("Enter the constant A : ")', advance = "no")
    read *, phase_vel


    initial_pos = string_length/2
    width = string_length/5
    nt = time_end / t_step + 0.5
    nx = string_length / x_step + 0.5

    lambda = phase_vel*phase_vel*t_step*t_step/(x_step*x_step)
    allocate(ya(0:nx, 0:nt))

    open(19, file = "wavepulse_a.txt")
    !Analytical Methos
    do j = 0, nt
        write(19,'(a,f0.4)') "# Time: ", j*t_step
        do i = 0, nx
            ya(i,j) = g(i*x_step, j*t_step)
            write(19, '(F0.4, 2X, F0.4)') i*x_step, ya(i,j)
        end do
        write(19,'(/)') 
    end do
    close(19)
   

    deallocate(ya)
end program ana

real function g(x,t)
    implicit none
    real :: x, t
    real :: x2
    real :: initial_pos, width, phase_vel, string_length
    common initial_pos, width, phase_vel, string_length

    g = 0

    ! This is the right moving pulse
    x2 = -x + t*phase_vel + initial_pos
    ! x2 needs to be in the range -string_length < x2 < +string_length
    ! to simulate an infinite train of pulses
    x2 = mod(x2 + string_length, 2*string_length) - string_length
    g = g + exp(-(x2/width)**2)

    ! And we subtract the left moving pulse
    x2 = x + t*phase_vel - initial_pos - string_length
    ! x2 needs to be in the range -string_length < x2 < +string_length
    ! to simulate an infinite train of pulses
    x2 = mod(x2 + string_length, 2*string_length) - string_length
    g = g - exp(-(x2/width)**2)

    return
end function g