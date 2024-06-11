
{-\Desde tiempos inmemoriales los viajes en el tiempo han seducido a muchas personas.
¡Hoy los vamos a programar!
Como cualquier viaje, tenemos a la persona que lo realiza y de ella se conoce:
su nombre,su edad,los recuerdos que obtuvo durante los viajes que hizo,los viajes que realizó.
Al realizar un viaje (llegar al destino) la persona que lo hace puede sufrir distintos cambios o transformaciones. 
Por ejemplo, una transformación podría ser que si viaja al lejano oeste pierde todos los recuerdos que empiezan con una vocal.
Otra podría ser que si viaja al futuro a Hill Valley su edad aumenta, porque se encuentra con su hijo. Las transformaciones son
propias del viaje que realiza y podrían ser aún más que las dos que se nombran como ejemplo.
De cada viaje conocemos:
Si van al futuro o al pasado,El nombre del lugar donde van,Una lista de transformaciones que le suceden al viajero
al llegar a ese lugar,i el viaje es al pasado entonces sabemos los recuerdos,Si el viaje es al futuro sabemos la cantidad de
años luz que el viajero tuvo que realizar,El año al que están viajando
De los recuerdos conocemos:
Su nombre del recuerdo, el lugar de donde proviene-}

--1. Definir las funciones que permitan obtener:
-- a. Dado un viajero su nombre,
-- b. Dado un viaje, su nombre
-- c. Dado un recuerdo, su nombre y el lugar de donde proviene 

data Viajero = Viajero { nombreViajero::String, edad:: Int, recuerdos::[Recuerdo], viajes::[Viaje]}
data Recuerdo = Recuerdo { nombreRecuerdo::String, lugar::String } deriving Show

data Viaje =
    ViajePasado { destino::String, transformaciones::[Viajero->Viajero], recuerdosViaje::[Recuerdo], anioDestino:: Int}
    | ViajeFuturo { destino:: String, transformaciones::[Viajero->Viajero], cantidadAniosLuz::Int, anioDestino::Int}


-- RECUERDOS
burbulina = Recuerdo {  nombreRecuerdo="cumpleanios de Burbulina", lugar="playland"}
fiesta = Recuerdo {nombreRecuerdo= "Fiesta fin de anioo", lugar= "UNDAV"}
casamiento = Recuerdo {nombreRecuerdo= "Casamiento hermana", lugar= "New York"}
bolasaurio = Recuerdo {nombreRecuerdo= "LLegada de Bolasaurio", lugar= " mi casa"}
eclipse = Recuerdo {nombreRecuerdo = "Vi el eclipse lunar", lugar= "esquina de plaza"}
viaje1957 = Recuerdo {nombreRecuerdo= "Viaje a 1957 ", lugar= "Lejano Oeste"}
--VIAJEROS
gonzalez = Viajero { nombreViajero="Claudio", edad =23, recuerdos=[fiesta,burbulina], viajes=[vamosChina]}
alma = Viajero "Alma" 28 [fiesta] [viajeFuturama,vamosKorea]
marta= Viajero {nombreViajero ="Marta", edad =88, recuerdos = [fiesta,burbulina,bolasaurio,casamiento, eclipse], viajes = [viajeFuturama,vamosChina]}
pepe= Viajero {nombreViajero ="Pepe", edad =23, recuerdos = [fiesta,burbulina,bolasaurio,casamiento, eclipse], viajes = [vamosChina,vamosKorea]}
mora= Viajero {nombreViajero ="Mora", edad =18, recuerdos = [fiesta,burbulina,bolasaurio,casamiento, eclipse], viajes = [vamosKorea]}

--VIAJES

viajeFuturama = ViajeFuturo "Futurama" [aumentarEdad,cambiarNombre]  20 3000
vamosKorea = ViajeFuturo {destino = "Visita a Korea ", transformaciones =[cambiarNombre,aumentarEdad],  cantidadAniosLuz = 80, anioDestino =3500}
viajeJapon = ViajeFuturo {destino = " Vacaciones en Japon ", transformaciones =[cambiarNombre,aumentarEdad],  cantidadAniosLuz = 80, anioDestino =3500}
viajeLejanoOeste = ViajePasado "Lejano Oeste" [perderRecuerdos]  [fiesta,eclipse] 1957
vamosChina= ViajePasado {destino = "caida muralla china ", transformaciones =[disminuirEdad,cambiarNombre,aumentarEdad,aumentarEdad], recuerdosViaje = [ burbulina,bolasaurio,casamiento,fiesta,eclipse], anioDestino =2000}
vamosAlpasado= ViajePasado {destino = "ir 20 años atras ", transformaciones =[disminuirEdad], recuerdosViaje = [bolasaurio,eclipse], anioDestino =2004}

 --Funciones = transformaciones  
cambiarNombre (Viajero _ edad recuerdos viajes)= Viajero "Pikachu" edad recuerdos viajes
aumentarEdad (Viajero nombre edad recuerdos viajes) = Viajero nombre (edad+10) recuerdos viajes
disminuirEdad (Viajero nombre edad recuerdos viajes) = Viajero nombre (edad-20) recuerdos viajes
duplicarEdad (Viajero nombre edad recuerdos viajes) = Viajero nombre (edad*2) recuerdos viajes
nacerDeNuevo (Viajero nombre edad recuerdos viajes) = Viajero nombre (edad - edad) recuerdos viajes
perderRecuerdos (Viajero nombre edad _ viajes) = Viajero nombre edad [] viajes


--MOSTRAR VIAJE 
instance Show Viaje where
    show (ViajeFuturo destino _ _ _) = destino
    show (ViajePasado destino _ _ _) = destino

instance Show Viajero where
    show (Viajero nombre edad recuerdos viajes ) = nombre ++ " "++ show edad ++" "++  show recuerdos ++ " " ++ show viajes
--1.a  Dado un viajero su nombre --> se usa nombreViajero
--1.b  Dado un viaje su nombre --> se usa destino
--1.c. Dado un recuerdo, su nombre y el lugar de donde proviene
nombreYLugar (Recuerdo nombreRecuerdo lugar ) =  (nombreRecuerdo,lugar )
--2. Definir una función que permita obtener los recuerdos y los viajes de un viajero.
recuerdosYlugares (Viajero _ _ recuerdos viajes) = (recuerdos, viajes)

--3. Hacer una función que permita saber si un viaje es interesante. Un viaje es interesante si:
-- a. Si el destino del viaje es el lejano oeste
-- b. Si es un viaje al pasado y el viajero se puede traer más de 5 recuerdos
-- c. Todos los viajes al futuro son interesantes.

--esViajeInteresante (ViajePasado destino _ recuerdos _)= length recuerdos>= 5 || destino =="lejano oeste"
--esViajeInteresante (ViajeFuturo _ _ _ _)=True

esViajeInteresante (ViajeFuturo {})= True
esViajeInteresante (ViajePasado "Lejano Oeste" _ _ _) =  True
esViajeInteresante (ViajePasado _ _ recuerdos _) = length recuerdos>= 5
--4. Hacer una función que dada una lista de viajes, permita mostrar los nombres y los años de todos los viajes que son interesantes.
--viajesAlFuturo = [viajeFuturama,vamosKorea ]
--viajesAlPasado = [vamosAlpasado ,viajeLejanoOeste]
--viajesConCincoOMasRecuerdos = [ViajePasado ]

nombresYAniosViajes = map (\ viaje -> (destino viaje, anioDestino viaje))
nombresYAniosViajesInteresantes  viajes = nombresYAniosViajes (filter esViajeInteresante viajes)
--5. Hacer una función que dada una lista de viajes, un año inicio y un año fin, se pueda obtener cuáles son los nombres y el año de 
--todos los viajes entre los años que están en el rango pasado por parámetro.
estaEnRango  inicio fin viaje = anioDestino viaje <= fin  && anioDestino viaje >= inicio
viajesEnRango inicio fin viajes = nombresYAniosViajes (filter (estaEnRango inicio fin) viajes)

--6. Definir una función que permita hacer que el viajero realice una lista de viajes, se le apliquen las transformaciones
-- necesarias y obtenga los recuerdos.

obtenerRecuerdosViaje (ViajePasado _ _ recuerdosViaje _  ) = recuerdosViaje
obtenerRecuerdosViaje (ViajeFuturo {} ) = []

obtenerRecuerdosViajero (Viajero _ _ recuerdos _) = recuerdos

actualizarViajesYRecuerdos (Viajero nombre edad recuerdosViajero viajesViajero ) viaje = Viajero nombre edad (recuerdosViajero ++ obtenerRecuerdosViaje viaje) (viajesViajero ++ [viaje])
viajarA viajero viaje = foldl (\viajero transformacion -> transformacion viajero) (actualizarViajesYRecuerdos viajero viaje) (transformaciones viaje)

viarjarMuchos viajero viajes = foldl viajarA viajero viajes

--7. Hacer la función estadística que reciba una función de condición, una función de transformación y una lista. Luego, 
--usarla para resolver las siguientes consultas:
    --a. Dada una lista, encontrar todos los nombres de los viajes que tienen más de 3 transformaciones.
    --b. Dada una lista de viajes, obtener la suma de todos los años luz que suman.Tener en cuenta que los viajes al pasado no suman años luz.
    --c. Dada una lista, obtener los nombres de todos los viajes. 
--Nota: sólo se puede hacer la función estadística y usar la misma en forma de consulta en los puntos a, b y c. No se pueden usar funciones
estadistica condicion transformacion lista = transformacion (filter condicion lista)
--7.a estadistica ((> 3) . length . transformaciones) (map destino) listaDeViajes
--7.b estadistica (esViajeFuturo ) (sum . map cantidadAniosLuz) listaDeViajes
--7.c estadistica (\_ -> True) (map destino) listaDeViajes    
esViajeFuturo (ViajePasado {}) = False
esViajeFuturo (ViajeFuturo {}) = True

viajesAlFuturo = [viajeFuturama,vamosKorea ]
viajesAlPasado = [vamosAlpasado ,viajeLejanoOeste]
viajesConCincoOMasRecuerdos = [vamosChina ]
listaDeViajes = [viajeFuturama,vamosKorea,vamosAlpasado ,viajeLejanoOeste,vamosChina ]