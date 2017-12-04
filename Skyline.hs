-- Diego Díaz Alvarellos
-- ddiaz136

module Skyline where

-- Cabecera del programa Skyline.hs
-- Práctica de Teoría de los Lenguajes de Programación
-- Curso 2015-2016

-- Definicion de tipos

type Edificio = (Int,Int,Int)
-- Edificio (x,y,altura)
type Coordenada = (Int,Int)
-- Coordenada (x, altura)
type Skyline = [Coordenada]


-- PARTE 1 : Obtener las coordenadas del Skyline
-------------------------------------------------
-------------------------------------------------

-- Funcion 1
-- resuelveSkyline: se trata de la función principal que implementa el algoritmo del Skyline mediante un esquema Divide y Vencerás. 

resuelveSkyline :: [Edificio] -> Skyline
resuelveSkyline [] = []
resuelveSkyline [(x,y,h)] = edificioAskyline (x,y,h)
resuelveSkyline lista = combina(resuelveSkyline (fst(divide(lista))), resuelveSkyline (snd(divide(lista))))

-- Funcion 2
-- edificioAskyline: es la función que será llamada por resuelveSkyline cuando el esquema Divide y Vencerás 
-- no realiza nuevas subdivisiones del problema. Es decir, cuando se encuentra con el caso trivial. 

edificioAskyline :: Edificio -> Skyline
edificioAskyline (x1,x2,h) = [(x1,h),(x2,0)]

-- Funcion 3
-- divide: es la función que será llamada por resuelveSkyline para realizar la división de la lista de edificios 
-- en dos mitades de igual tamaño 

divide :: [Edificio] -> ([Edificio],[Edificio])
divide lista =
   (lista1, lista2)
   where lista1 = take n lista
              where n = (length lista) `div` 2
         lista2 = drop n lista
              where n = (length lista) `div` 2

-- Funcion 4
-- combina: es la función que será llamada por resuelveSkyline para combinar las soluciones parciales de los dos subproblemas. 
-- Recibirá dos líneas de horizonte y las combinará en una única línea utilizando el proceso descrito en la sección 2.1.1.
-- La linea de horizonte se inicializa a 0.
combina :: (Skyline, Skyline) -> Skyline
combina (lista1, lista2) = (lista1,0) `merge` (lista2,0)

merge (        [],    _) (        ys,    _) = ys
merge (        xs,    _) (        [],    _) = xs

merge ((x, xh):xs, xh_p) ((y, yh):ys, yh_p)
  |             x >  y           =                    merge ((y, yh):ys, yh_p) ((x, xh):xs, xh_p)
  |             x == y           = (x, max xh yh  ) : merge (        xs,   xh) (        ys,   yh)
  | max xh_p yh_p /= max xh yh_p = (x, max xh yh_p) : merge (        xs,   xh) ((y, yh):ys, yh_p)
  |                    otherwise =                    merge (        xs,   xh) ((y, yh):ys, yh_p)


-- PARTE 2:  Dibujar los edificios
------------------------------------
------------------------------------

-- Función principal 
dibujaSkyline :: Skyline -> [Char]
dibujaSkyline [] = []
dibujaSkyline ciudad = dibuja (altura(ciudad), alturasSkyline(ciudad)) ++ suelo(alturasSkyline(ciudad))

-- Importante para que en WinGHCi funcione con los saltos de linea
-- putStr(dibujaTodos(5,[0,0,0,5,5,5,3,3,3,2,4,4]))

dibuja (n, [])    = unlines $ []
dibuja (1, lista) = unlines $ [dibujaLinea(1, lista)]
dibuja (n, lista) = dibujaLinea(n,lista) ++ "\n" ++ dibuja(n-1, lista)

-- ESTA ES LA VERSION sin salto de linea \n

dibujaS (n, [])    = unlines $ []
dibujaS (1, lista) = unlines $ [dibujaLinea(1, lista)]
dibujaS (n, lista) = dibujaLinea(n,lista) ++ dibujaS(n-1, lista)


-- 1.Transformamos la lista de coordenadas del skyline en una lista de alturas para cada coordenada x del horizonte.
---------------------------------------------------------------------------------------------------------------------
alturasSkyline :: Skyline -> [Int]
alturasSkyline p = reverse (concatena(p))

-- Concatena las subsoluciones. Devuelve la lista de alturasSkyline invertida.
concatena :: Skyline -> [Int]
concatena p
  | null p         = []
  | length(p) ==1  = solarInicio(p)
  | length(p) > 0  = bloque(last2(p)) ++ concatena(quitaUltimo(p))

quitaUltimo :: Skyline-> Skyline
quitaUltimo []  = []
quitaUltimo [x] = []
quitaUltimo xs  = init xs

last2 :: Skyline -> Skyline
last2 [] = []
last2 coordenadas = ([(ante), (ultimo)])
    where ultimo = last(coordenadas)
          ante   = last(quitaUltimo(coordenadas))

solarInicio :: Skyline -> [Int]
solarInicio [] = []
solarInicio ([(x,h)])
 | x>0            = [0] ++ solarInicio([(x-1,h)])
 | otherwise      = []

-- Devuelve la lista de altura de un bloque
bloque :: Skyline -> [Int]
bloque [] = []
bloque ([(x,h),(y,h2)])
 | y>x  = [h] ++ bloque([(x,h),(y-1,h2)])
 | otherwise    = []


-- 2.Calculamos la altura máxima del skyline.
---------------------------------------------
altura [(k,p)] = p
altura ((k,p):resto) = if p > altura (resto) then p else altura (resto)


-- 3.Para generar cada línea del dibujo del skyline.
----------------------------------------------------
dibujaLinea :: (Int, [Int]) -> String
dibujaLinea (n, []) = ""
dibujaLinea (n, p:resto) 
  | p >= n    = "*" ++ dibujaLinea(n,resto)
  | otherwise = " " ++ dibujaLinea(n,resto)


-- 4. Simular el suelo.
-----------------------
suelo :: [Int] -> String
suelo = flip replicate '-' . length