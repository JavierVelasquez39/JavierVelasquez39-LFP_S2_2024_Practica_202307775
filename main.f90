module Automodule
    implicit none

    type :: Equipo
        character(len=256) :: nombre
        integer :: cantidad
        real :: precio
        character(len=256) :: ubicacion
        contains
            procedure :: inicializar
            procedure :: agregarStock
            procedure :: quitarStock
    end type Equipo

    contains

    ! Subrutina para inicializar el tipo de dato Equipo
    subroutine inicializar(this, nombre, cantidad, precio, ubicacion)
        class(Equipo), intent(inout) :: this
        character(len=256), intent(in) :: nombre
        integer, intent(in) :: cantidad
        real, intent(in) :: precio
        character(len=256), intent(in) :: ubicacion

        this%nombre = nombre
        this%cantidad = cantidad
        this%precio = precio
        this%ubicacion = ubicacion
    end subroutine inicializar

    ! Subrutina para agregar stock al tipo de dato Equipo
    subroutine agregarStock(this, cantidad, ubicacion)
        class(Equipo), intent(inout) :: this
        integer, intent(in) :: cantidad
        character(len=256), intent(in) :: ubicacion

        if (this%ubicacion == ubicacion) then
            this%cantidad = this%cantidad + cantidad
        endif
    end subroutine agregarStock

    ! Subrutina para quitar stock al tipo de dato Equipo
    subroutine quitarStock(this, cantidad)
        class(Equipo), intent(inout) :: this
        integer, intent(in) :: cantidad

        integer :: resultante
        resultante = this%cantidad - cantidad

        if(resultante>=0)then 
            this%cantidad = this%cantidad - cantidad
        else
            print *, "No se puede quitar esa cantidad de stock, stock negativo"
        endif

    end subroutine quitarStock

end module Automodule

module global_vars
    use Automodule
    integer :: n=1
    integer :: i
    type(Equipo), dimension(100) :: inventario
end module global_vars

program main
    use Automodule
    use global_vars
    implicit none
    integer :: op ! Variable para leer la opción

    print *, "Practica 1 - Lenguajes Formales y de Programacion"
    do
        print *, "-------------------------"
        print *, "# Sistema de inventario: "
        print *, "-------------------------"
        print *, "1. Cargar Inventario Inicial"
        print *, "2. Cargar Instrucciones de Movimiento"
        print *, "3. Crear informe de Inventario"
        print *, "4. Salir"
        print *, "Ingrese una opcion"
        read *, op ! Leer la opcion del usuario

        select case (op)
            case (1)
                call cargarInventarioArchivo()
                print *, "Número de elementos en el inventario: ", n-1  ! <--- Cambio aquí
                do i=1, n-1
                    print *, "Equipo: ", trim(inventario(i)%nombre)
                    print *, "Cantidad: ", inventario(i)%cantidad
                    print *, "Precio: ", inventario(i)%precio
                    print *, "Ubicacion: ", trim(inventario(i)%ubicacion)
                    read *
                end do
            case (2)
                call accionesArchivo()
                print *, "Acciones realizadas en el inventario:"
                do i=1, n-1
                    print *, "Equipo: ", trim(inventario(i)%nombre)
                    print *, "Cantidad: ", inventario(i)%cantidad
                    print *, "Precio: ", inventario(i)%precio
                    print *, "Ubicacion: ", trim(inventario(i)%ubicacion)
                    read *
                end do
            case (3)
                !call crearInformeInventario()
            case (4)
                print *, "Saliendo del programa..."
                exit
            case default
                print *, "Opcion no valida, presiona cualquier tecla para continuar"
                read *
        end select
        call system("cls") ! Limpiar la pantalla
    end do
end program main

subroutine cargarInventarioArchivo()
    use Automodule
    use global_vars
    implicit none
    integer :: iunit, ios, pos, cantidad_int
    real :: precio_real
    character(len=256) :: nombre, cantidad, precio, ubicacion, linea, comando

    ! Asignar unidad al archivo
    iunit = 10

    open(unit=iunit, file="entrada.inv", status="old", action="read", iostat=ios)

    if (ios /= 0) then
        print *, "Error al abrir el archivo de entrada.inv"
        stop
    endif

    do
        read(iunit, '(A)', iostat=ios) linea
        if (ios /= 0) exit
        linea = trim(linea)
        

        ! Encuentra el primer espacio para extraer el comando
        pos = index(linea, " ")
        if (pos > 0) then
            comando = linea(1:pos-1)
            linea = trim(linea(pos+1:))

            ! Extraer nombre
            pos = index(linea, ";")
            if (pos > 0) then
                nombre = linea(1:pos-1)
                linea = trim(linea(pos+1:))
                print *, "Nombre: ", nombre  ! <--- Depuración

                ! Extraer cantidad
                pos = index(linea, ";")
                if (pos > 0) then
                    cantidad = linea(1:pos-1)
                    linea = trim(linea(pos+1:))
                    read(cantidad, '(I10)', iostat=ios) cantidad_int
                    print *, "Cantidad: ", cantidad_int  ! <--- Depuración

                    ! Extraer precio
                    pos = index(linea, ";")
                    if (pos > 0) then
                        precio = linea(1:pos-1)
                        linea = trim(linea(pos+1:))
                        read(precio, '(F10.2)', iostat=ios) precio_real
                        print *, "Precio: ", precio_real  ! <--- Depuración

                        ! Extraer ubicacion
                        ubicacion = linea
                        print *, "Ubicacion: ", ubicacion  ! <--- Depuración

                        if (comando == "crear_inventario") then
                            call crearInventario(nombre, cantidad_int, precio_real, ubicacion)
                        endif
                    endif
                endif
            endif
        endif
    end do
    close(unit=iunit)
end subroutine cargarInventarioArchivo

subroutine crearInventario(nombre, cantidad, precio, ubicacion)
    use Automodule
    use global_vars
    implicit none
    ! dummy arguments
    character(len=256), intent(in) :: nombre
    integer, intent(in) :: cantidad
    real, intent(in) :: precio
    character(len=256), intent(in) :: ubicacion

    type(Equipo) :: equipoNuevo
    call equipoNuevo%inicializar(nombre, cantidad, precio, ubicacion)
    inventario(n) = equipoNuevo
    n = n + 1
    print *, "Inventario actualizado. n = ", n  ! <--- Depuración
end subroutine crearInventario


subroutine accionesArchivo()
    use Automodule
    use global_vars
    integer :: iunit, ios, pos, cantidad_int
    character(len=256) :: nombre, cantidad, ubicacion, linea, comando

    iunit = 11 

    ! Abre el archivo en modo lectura 
    open(unit=iunit, file="instrucciones.mov", status="old", action="read", iostat=ios)

    ! Verifica si el archivo se abrió correctamente
    if (ios /= 0) then
        print *, "Error al abrir el archivo de instrucciones.mov"
        stop
    endif

    ! Lee el archivo línea por línea
    do 
        read(iunit, '(A)', iostat=ios) linea
        if (ios /= 0) exit 
        linea = trim(linea)

        ! Encuentra el primer espacio para extraer el comando 
        pos = index(linea, ' ')
        if (pos > 0) then 
            comando = linea(1:pos-1)
            linea = trim(linea(pos+1:))

            ! Separar por ';' 
            pos = index(linea, ';')
            if (pos > 0) then 
                nombre = linea(1:pos-1)
                linea = trim(linea(pos+1:))

                pos = index(linea, ';')
                if (pos > 0) then 
                    cantidad = linea(1:pos-1)
                    linea = trim(linea(pos+1:))
                    read(cantidad, '(I10)', iostat=ios) cantidad_int

                    ! Extraer ubicación
                    pos = index(linea, ';')
                    if (pos > 0) then
                        ubicacion = linea(1:pos-1)
                        linea = trim(linea(pos+1:))

                        if (comando == "agregar_stock") then
                            call agregar_stock(nombre, cantidad_int, ubicacion)
                        elseif (comando == "eliminar_equipo") then
                            call eliminar_equipo(nombre, cantidad_int, ubicacion)
                        endif
                    endif
                endif
            endif
        endif
    end do
    close(unit = iunit)
    read *
end subroutine accionesArchivo

subroutine agregar_stock(nombre, cantidad, ubicacion)
    use Automodule
    use global_vars
    ! use dummy arguments
    character(len=256), intent(in) :: nombre
    integer, intent(in) :: cantidad
    character(len=256), intent(in) :: ubicacion

    do i=1, n-1 
        if(inventario(i)%nombre == nombre .and. inventario(i)%ubicacion == ubicacion) then
            call inventario(i)%agregarStock(cantidad, ubicacion)
            print *, "Agregado stock: ", cantidad, " de ", trim(nombre), " en ", trim(ubicacion)
        endif
    end do
end subroutine agregar_stock

subroutine eliminar_equipo(nombre, cantidad, ubicacion)
    use Automodule
    use global_vars
    ! use dummy arguments
    character(len=256), intent(in) :: nombre
    integer, intent(in) :: cantidad
    character(len=256), intent(in) :: ubicacion

    do i=1, n-1
        if (inventario(i)%nombre == nombre .and. inventario(i)%ubicacion == ubicacion) then
            call inventario(i)%quitarStock(cantidad)
            print *, "Quitado stock: ", cantidad, " de ", trim(nombre), " en ", trim(ubicacion)
        endif 
    end do 
end subroutine eliminar_equipo