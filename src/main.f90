! main.f90
!
! Orbit simulation on Fortran with SDL2.
!
! Author:  Ali Efe Karagul
! GitHub:  https://github.com/A713F3/Orbit.F90

program main
    use, intrinsic :: iso_c_binding, only: c_associated, c_int8_t, c_null_char, c_ptr
    use, intrinsic :: iso_fortran_env, only: stdout => output_unit, stderr => error_unit
    use :: sdl2
    use :: circle_mod
    use :: simulation
    implicit none

    integer, parameter :: SCREEN_WIDTH  = 640
    integer, parameter :: SCREEN_HEIGHT = 480

    type(c_ptr)     :: window
    type(c_ptr)     :: renderer
    type(sdl_event) :: event
    integer         :: rc, i
    character(len=15) :: window_title

    integer :: EULER_STEP_SIZE = 10000

    integer(kind=c_int) :: mouse_x, mouse_y
    integer :: tick_a, tick_b, delta

    ! Initialise SDL.
    if (sdl_init(SDL_INIT_VIDEO) < 0) then
        write (stderr, *) 'SDL Error: ', sdl_get_error()
        stop
    end if

    ! Create the SDL window.
    window = sdl_create_window('Orbit Simulation' // c_null_char, &
                               SDL_WINDOWPOS_UNDEFINED, &
                               SDL_WINDOWPOS_UNDEFINED, &
                               SCREEN_WIDTH, &
                               SCREEN_HEIGHT, &
                               SDL_WINDOW_SHOWN)

    if (.not. c_associated(window)) then
        write (stderr, *) 'SDL Error: ', sdl_get_error()
        stop
    end if

    ! Create renderer.
    renderer = sdl_create_renderer(window, -1, 0)

    call init_simulation()

    ! Main loop.
    do while (.true.)
        tick_a = sdl_get_ticks()
        delta = tick_a - tick_b

        if (sdl_poll_event(event) > 0) then
            select case (event%type)
                case (SDL_QUITEVENT)
                    exit

                case (SDL_MOUSEBUTTONDOWN)
                    rc = sdl_get_mouse_state(mouse_x, mouse_y)

                    call add_planet(real(mouse_x), real(mouse_y))
            end select
        end if

        ! Clear screen.
        rc = sdl_set_render_draw_color(renderer, uint8(0), uint8(0), uint8(0), uint8(SDL_ALPHA_OPAQUE))
        rc = sdl_render_clear(renderer)


        rc = sdl_set_render_draw_color(renderer, uint8(100), uint8(100), uint8(100), uint8(SDL_ALPHA_OPAQUE))
        rc = render_simulation(renderer)

        do i=1, EULER_STEP_SIZE
            call simulate(real(1.0/EULER_STEP_SIZE))
        end do

        tick_b = tick_a
        ! Render to window.
        call sdl_render_present(renderer)

        write (window_title, "(A5, I10)") "FPS: ", int(1000/delta)
        call sdl_set_window_title(window, window_title)

        call sdl_delay(10)
    end do

    ! Quit.
    call sdl_destroy_renderer(renderer)
    call sdl_destroy_window(window)
    call sdl_quit()

contains 



end program main