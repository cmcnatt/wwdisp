!----------------------------------------------------------------------------------------
!
!	wwdisp - Computes the water wave dispersion relation
!   Copyright (C) 2014  Cameron McNatt
!
!   This program is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   This program is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with this program.  If not, see <http://www.gnu.org/licenses/>.
!
!	Contact: cameron.mcnatt@gmail.com
!
!----------------------------------------------------------------------------------------
    
module params
!----------------------------------------------------------------------------------------
!
!	Module to store global gravity, pi, and tolerance constancts
!
!----------------------------------------------------------------------------------------
	real, parameter :: g = 9.80665, pi = 3.14159, tol = 0.0001, lim = 500
end module params

program wwdisp
!---------------------------------------------------------------------------------------
!
! Main program:
!
!	wwdisp (water wave dispersion relation) finds the wave number (length) of a water 
!	wave at a given water depth and wave period.
!
!	For more info type: wwdisp --help
!
!---------------------------------------------------------------------------------------
	use params, only : pi
  	implicit none

	
  	interface
    	! interface to function to compute progressive wave number
		function kprog(h, omega)
      		real, intent(in) 	:: h, omega
      		real 				:: kprog
    	end function kprog

		! interface to function to compute evanescent wave numbers
        function kevan(h, omega, m)
      		real, intent(in) 		:: h, omega
            integer, intent(in)		:: m
            real, dimension(99)		:: kevan
    	end function kevan
    end interface

	character(len=3), parameter	:: ver = '1.0'					! version
    logical						:: hasi, haso, quitrun			
  	character(len=20)			:: wavetype, arg, iname, oname
    character(len=6)			:: userin 
  	integer						:: n, narg, nke, ierr
    real, dimension(99)			:: ke
    real 						:: h, T, kp, lambda


  	! Check command line arguments
    hasi = .false.
    haso = .false.
    nke = 0
    narg = command_argument_count()

    do n = 1,narg
      	call get_command_argument(n, arg)

        select case(adjustl(arg))
          	case ('-i')
            	hasi = .true.
                call get_command_argument(n + 1, iname)
            case ('-o')
            	haso = .true.
                call get_command_argument(n + 1, oname)
            case ('-m')
                call get_command_argument(n + 1, arg)
                read(arg,'(i2)') nke
            case ('--help')
            	write(*,*)
            	write(*,*) 'wwdisp ', ver
                write(*,*)
                write(*,*) 'For linear water waves, this program computes both '
                write(*,*) 'the progressive and evanescent wave numbers.'  
            	write(*,*)
                write(*,*) 'If run without any arguments, it will run a command '
                write(*,*) 'line program that reads in user input water depth and '
                write(*,*) 'wave period and returns the wave length.'
                write(*,*)
                write(*,*) 'It also takes optional arguements. Not that with -i '
                write(*,*) 'and -o, wave numbers are produced instead of wave '
                write(*,*) 'lengths.'
                write(*,*)
                write(*,*) '-i <input_file>: reads an input file where the first '
                write(*,*) '		line is the water depth and each ' 
                write(*,*) '		subsequent line is a wave period. If no '
                write(*,*) '		output file is specified, then wave	'
                write(*,*) '		numbers are written to the command line.'
                write(*,*)
                write(*,*) '-o <output_file>: writes the wave numbers to the '
                write(*,*) '		specified output file.'
                write(*,*)
                write(*,*) '-m <number>: computes evanescent wave numbers'
                write(*,*) '		specified by the number.'
                write(*,*)
                write(*,*) 'If the solution does not converge then the wave number '
                write(*,*) 'is set to -1.'
                write(*,*)
                write(*,*) 'Report bugs to: cameron.mcnatt@gmail.com'
                write(*,*) 'wwdisp home page: '
                write(*,*) '<https://github.com/camalamadingdong/wwwdisp>'
                write(*,*)
                call exit(0)
            case ('--version')
            	write(*,*) 'wwdisp ', ver
                write(*,*) 'Copyright (C) 2014 Cameron McNatt'
                write(*,*) 'License GPLv3+: GNU GPL version 3 or later &
                	&<http://gnu.org/licenses/gpl.html>'
                write(*,*) 'This is free software: you are free to change and &
                	&redistribute it.'
				write(*,*) 'There is NO WARRANTY, to the extent permitted by law.'
                write(*,*)
                call exit(0)
        end select
    end do

	write(*,*)
	write(*,*) 'The program finds the wave length of a water wave at a'
    write(*,*) 'given water depth and wave period.'
	write(*,*)

    if (nke > 0) then
      	write(*,'(i3,a)') nke, ' evanescent wave modes requested'
        write(*,*)
    end if

	if (haso) then
    	! open output file
    	open(10, file=oname)
        write(*,*) 'Will write wave numbers to output file: ', trim(oname)
        write(*,*)
    end if
    
    if (hasi) then
    	! read input file
        write(*,*) 'Reading input file: ', trim(iname)
      	open(11,file=iname,iostat=ierr)
        if (ierr /= 0) then
          	write(*,*) 'Error openning input file.'
            write(*,*) 'Program exiting.'
            call exit(ierr)
        end if
        
        read(11,*,iostat=ierr) h
        if (ierr /= 0) then
          	write(*,*) 'Error on read of depth value.'
            write(*,*) 'Program exiting.'
            call exit(ierr)
        end if

		if (.not. haso) then
	        write(*,*)
    	    write(*,*) 'The wave numbers are:'
            write(*,*) '    kprog, kevan(1), kevan(2), ...'
        	write(*,*)
        end if
  
        do
          	read(11,*,iostat=ierr) T
            kp = kprog(h, 2*pi/T)
            ke = kevan(h, 2*pi/T, nke)
            if (ierr == 0) then
              	if (haso) then
                	! write to output file
                	write(10,'(100f10.3)') kp, (ke(n), n = 1,nke) 
                else
                	! write to screen
	              	write(*,'(100f10.3)') kp, (ke(n), n = 1,nke) 
                end if
            else if (ierr > 0) then
              	write(*,*) 'Error on read of wave period.'
            	write(*,*) 'Program exiting.'
            	call exit(ierr)
            else
            	exit
            end if
        end do
        close(11)
    else
    	! if no input file, run command line version
        do
			write(*,'(a,$)') ' Enter the water depth (m): '
			read(*,*) h
			if (h <= 0) then
            	write(*,*)
  				write(*,*) 'ERROR: The depth must be greater than 0.'
                write(*,*)
            else
              	exit
			end if
        end do

        n = 1
        quitrun = .false.
		do
			do
            	if (n == 1) then
                	write(*,'(a,$)') ' Enter the wave period (s): '
                else
                  	write(*,'(a,$)') ' Enter another wave period (s) (or q to quit): '
                end if
                read(*,*) userin
                if (userin(1:1) == 'q' .or. userin(1:1) == 'Q') then
                  	quitrun = .true.
                    exit
                else
                    read(userin,*) T
                end if
				if (T <= 0) then
                	write(*,*)
  					write(*,*) 'ERROR: The period must be greater than 0.'
                    write(*,*)
        	    else
           		  	exit
				end if
        	end do

            if (quitrun) then
              	exit
            end if

            kp = kprog(h, 2*pi/T)

          	if (kp > 0) then
            	lambda = 2*pi/kp
            	if (nke > 0) then 
            		ke = kevan(h, 2*pi/T, nke)
            	end if

            	if (haso) then
	            	write(10,'(100f7.3)') kp, (ke(n), n = 1,nke) 
           		end if

				if (lambda/h < 2.) then
  					wavetype = 'a deep'
				else if (lambda/h > 20.) then
  					wavetype = 'a shallow'
				else
  					wavetype = 'an intermediate'
				end if

				write(*,*)
				write(*,'(a, f6.2, a)') ' The wave length is: ', lambda, ' m.'
				write(*,*) 'It is ', trim(wavetype), ' water wave.'
            	if (nke > 0) then
              		write(*,*) 'The wave numbers are:' 
                	write(*,*) '    kprog, kevan(1), kevan(2), ...' 
              		write(*,'(100f10.3)') kp, (ke(n), n = 1,nke) 
				end if
                
            else
              	write(*,*)
                write(*,*) 'Method did not converge for progressive wave number.'
            end if
        	write(*,*)
            write(*,'(a)') '---------------------------------------------------------------'
            write(*,*)
            
			n = n + 1
        end do  
	end if

    if (haso) then
    	close(10)
    end if

	write(*,*)
    write(*,*) 'Computation complete. Have a harmonious day.'

end program wwdisp

function kprog(h, omega)
!------------------------------------------------------------------------------
!
!	Finds the progressive wave number k0 from the dispersion relation:
!		omega**2 = g*k0*tanh(k0*h)
!
!------------------------------------------------------------------------------
	use params
	implicit none

	real, intent(in) 	:: h, omega
	real 				:: kprog, kh, tanhkh, const, f, fp
    integer				:: i

	if (h <= 0) then
  		kprog = -1
  		return
	end if

    ! uses Newton-Raphson method to solve for (kh)
    !
    ! f = h*ommega**2/g - (kh)*tanh(kh)
    ! f' = -tanh(kh) - (kh)*(1 - tanh(kh)**2)

	kh = omega*omega/g*h	! initial guess - deep water wave
    const = kh				! constant in f equation
    
	i = 0
	do
   		tanhkh = tanh(kh)
  		f = const - kh*tanhkh
        fp = kh*tanhkh*tanhkh - tanhkh - kh
  		kh = kh - f/fp
  		if (abs(f) < tol) then
    		exit
  		end if
          
        i = i + 1
        if (i > lim) then	! if the method does not coverge, set the wave number to -1
          	kh = -h
          	exit
        end if
	end do

	kprog = kh/h

end function kprog

function kevan(h, omega, m)
!------------------------------------------------------------------------------
!
!	Finds the evanecent wave numbers km of dispersion relation:
!		omega**2 = -g*kn*tan(kn*h)
!
!	There are multiple zeros of the above equation and so the number of
!	desired solutions is specified by the input variable, m.
!
!------------------------------------------------------------------------------
	use params
	implicit none

	real, intent(in) 		:: h, omega
    integer, intent(in)		:: m
    real, dimension(99)		:: kevan
    integer 				:: n, i
	real 					:: knh, const, ul, ll, f

	! uses Bisection method to find zeros of
    !
	!	f = omega**2/g*h + (knh)*tan(knh)
    !
	! for each n

    
    const = omega*omega/g*h
    kevan = 0;

	do n = 1,m			
    					! starting point 
    	ul = n*pi	  	! upper limit: tan(ul) = 0, f = omega**2/g*h
        ll = ul - pi/2	! lower limit: tan(ll) = -inf, f = -inf 
        				! 	(when approached from second quadrant)
        				! so zero has to be somewhere between these two

        i = 0

        do 
          	knh = (ul + ll)/2		! try half way point
            f = const + knh*tan(knh)
          	if (abs(f) < tol) then
            	exit
            else if (f < 0) then
              	ll = knh			! less than zero, move up lower limit
            else
              	ul = knh			! greater than zero, move down upper limit
            end if

            i = i + 1
            if (i > lim) then		! if the method does not coverge, set the wave number to -1
              	knh = -h
            	exit
            end if
        end do

        kevan(n) = knh/h
    end do

end function kevan
