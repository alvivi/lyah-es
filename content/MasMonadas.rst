

Unas cuantas mónadas más
========================


.. image:: /images/clint.png
   :align: right
   :alt: Amigo, existen dos tipos de personas en el mundo: los que aprenden
         Haskell y lo que trabajan programando en Java.

Hemos visto como podemos utilizar las mónadas para tomar valores con un cierto
contexto y aplicarles funciones utilizando ``>>=`` o la notación ``do``, lo
cual nos permite centrarnos en los valores en si mientras que el contexto se
trata automáticamente.

Ya hemos conocemos la mónada ``Maybe`` y hemos visto como añade un contexto de
que existan posibles fallos. Ya hemos aprendido a utilizar la mónada lista y
hemos visto como nos permite introducir un no determinismo en nuestros
programas. También hemos aprendido a trabajar con la mónada ``IO``, ¡incluso
antes de que supiéramos de la existencia de las mónadas!

En este capítulo, vamos a ver una cuantas mónadas más. Veremos como éstas
pueden conseguir que nuestros programas sean más claros permitiéndonos manejar
todo tipo de valores como si fueran monádicos. El hecho de ver unas cuantas
mónadas más también reforzará nuestro conocimiento acerca de ellas.

Todas las mónadas que vamos a ver forman parte del paquete ``mtl``. Un
paquete de Haskell es una colección de módulos. El paquete ``mtl`` viene con
la plataforma Haskell así que probablemente ya lo tengas instalado. Para
comprobarlo, ejecuta ``ghc-pkg list`` en la línea de comandos. Así podrás ver
todos los paquetes que tienes instalados y uno de ellos debe ser ``mtl``,
seguido de un número de versión.


¿Writer? No la conozco
----------------------


Hemos cargado una pistola con la mónada ``Maybe``, la mónada lista y la mónada
``IO``. Ahora vamos a hacer sitio en la recámara para la mónada ``Writer`` y
ver que pasa cuando la disparamos.

Mientras que ``Maybe`` sirve para valores con el contexto adicional de un
posible fallo y las listas son para valores no deterministas, la mónada
``Writer`` sirve para valores que tienen una especie de registres como
contexto. La mónada ``Writer`` nos permite realizar cómputos de forma que los
valores del registro se combinan en un solo registro que será adjuntado al
resulto final.

Por ejemplo, podríamos querer equipar algunos valores con unas cadenas que
explicaran lo que esta sucediendo, probablemente para luego depurar el
código. La siguiente función toma el número de bandidos de una banda y nos
dice si es una gran banda o no. Una función muy simple: ::

    isBigGang :: Int -> Bool
    isBigGang x = x > 9

Ahora, en lugar de que nos devuelva solo ``True`` o ``False``, queremos que
nos devuelve también una cadena de registro que nos indique que ha hecho la
función. Para ello solo tenemos que devolver una cadena junto al valor
``Bool``: ::

    isBigGang :: Int -> (Bool, String)
    isBigGang x = (x > 9, "Compared gang size to 9.")

Así que ahora en vez de devolver una valor ``Bool``, devuelve una tupla cuyo
primer es el resultado original y el segundo es la cadena que acompaña al
resultado. Ahora este resultado tiene añadido un cierto contexto. Vamos a
probarla:

.. code-block:: console

    ghci> isBigGang 3
    (False,"Compared gang size to 9.")
    ghci> isBigGang 30
    (True,"Compared gang size to 9.")

.. image:: /images/tuco.png
   :align: left
   :alt: Cuando vayas al retrete, haz tus necesidades, no hables.

Hasta aquí todo bien. ``isBigGang`` toma un valor normal y devuelve un valor
con un determinado contexto. Como ya sabemos, pasar a esta función un valor
normal no causa ningún problema. Pero, ¿y si ya tenemos un valor que tiene
adjuntado una cadena, como por ejemplo ``(3, "Smallish gang.")``, y queremos
pasarlo a ``isBigGang``? Parece que una vez más nos topamos con la misma
pregunta: si tenemos una función que toma un valor normal y devuelve un valor
con un cierto contexto, ¿cómo extraemos el valor de ese contexto y se lo
pasamos a la función?

Cuando estábamos explorando la mónada ``Maybe`` creamos la función
``applyMaybe``, la cual tomaba un valor de tipo ``Maybe a`` y una función del
tipo ``a -> Maybe b`` y pasa ese valor ``Maybe a`` a la función, incluso
aunque la función toma una valor del tipo ``a`` y no ``Maybe a``. Conseguíamos
hacer esto teniendo en cuenta el contexto de los valores ``Maybe``, el cual
era el de los valores con un posible fallo. Dentro de la función
``a -> Maybe b`` éramos capaces de tratar ese valor con absoluta normalidad,
ya que ``applyMaybe`` (que luego vino a ser ``>>=``) se encargaba de
comprobar si el valor era ``Nothing`` o un valor ``Just``.

Del mismo modo, vamos a crear una función que tome un valor con un registro
añadido, como por ejemplo ``(a, String)``, y una función del tipo
``a -> (b, String)`` a la que pasaremos el valor inicial. La llamaremos
``applyLog``. Como un valor del tipo ``(a, String)`` no lleva asociado ningún
contexto de un posible fallo, sino únicamente un registro adicional,
``applyLog`` se encargará de que el registro de la variable original no se
pierda concatenándolo con el registro del resultado de la función. Aquí tienes
la implementación de ``applyLog``: ::

    applyLog :: (a,String) -> (a -> (b,String)) -> (b,String)
    applyLog (x,log) f = let (y,newLog) = f x in (y,log ++ newLog)

Cuando tenemos un valor dentro de un contexto y queremos pasar dicho valor
a una función, normalmente intentamos separar el valor real del contexto,
luego intentamos aplicar la función sobre ese valor y para terminar volvemos
a considerar el contexto. Con la mónada ``Maybe``, primero comprobamos si el
valor era del tipo ``Just x`` y si lo era, tomábamos el valor ``x`` y lo
aplicábamos a la función. En este caso es fácil encontrar el valor real, ya
que estamos trabajando con una dupla que contiene el valor y un registro.
Primero tomamos el valor, que es ``x`` y le aplicamos la función ``f``.
Obtenemos una dupla de ``(y, newLog)``, donde ``y`` es el nuevo resultado y
``newLog`` es el nuevo registro. Sin embargo, si devolviéramos esto como
resultado, el registro antiguo no se incluiría en el resultado, así que
devolvemos una dupla ``(y,log ++ newLog)``. Utilizamos ``++`` para concatenar
ambos registros.

Aquí tienes ``applyLog`` en acción:

.. code-block:: console

    ghci> (3, "Smallish gang.") `applyLog` isBigGang
    (False,"Smallish gang.Compared gang size to 9")
    ghci> (30, "A freaking platoon.") `applyLog` isBigGang
    (True,"A freaking platoon.Compared gang size to 9")

El resultado es similar al anterior, solo que el número de bandidos en la
banda va acompañado de un registro. Unos cuantos ejemplos más:

.. code-block:: console

    ghci> ("Tobin","Got outlaw name.") `applyLog` (\x -> (length x, "Applied length."))
    (5,"Got outlaw name.Applied length.")
    ghci> ("Bathcat","Got outlaw name.") `applyLog` (\x -> (length x, "Applied length"))
    (7,"Got outlaw name.Applied length")

Fíjate en el interior de la función lambda, ``x`` es un cadena normal y no una
tupla. Además ``applyLog`` se encarga de concatenar los registros.


Monoides al rescate
'''''''''''''''''''

.. note:: ¡Asegurate de saber lo que son los :ref:`monoides <monoides>` si
          quieres continuar!

Ahora mismo, ``applyLog`` toma valores del tipo ``(a,String)``, pero, ¿existe
alguno motivo especial por el que lo registros deban ser del tipo ``String``?
Utilizamos ``++`` para unir los registros, así que, ¿no debería aceptar
cualquier tipo de listas, y no solo listas de caracteres? Pues sí, debería.
Podemos cambiar su tipo a: ::

    applyLog :: (a,[c]) -> (a -> (b,[c])) -> (b,[c])

Ahora, el registro es una lista. El tipo de valores que contiene la lista debe
ser el mismo tipo de que tienen los elementos de la lista original, a la vez
que deben ser iguales a los que devuelve la función. De otro modo, no
podríamos utilizar ``++`` para unirlos.

¿Debería función con cadenas de bytes? No hay ninguna razón para que no
funcionase. Sin embargo, el tipo que hemos utilizado solo acepta listas.
Parece que tendremos que crear una ``applyLog`` solo para cadenas de bytes
¡Pero espera! Tanto las listas como los cadenas de bytes son monoides. Como
tal, ambas poseen instancias de la clase de tipos ``Monoid``, lo cual
significa que ambas implementan la función ``mappend``. Y tanto par las listas
como para las cadenas de bytes, ``mappend`` sirve para unir. Mira:

.. code-block:: console

    ghci> [1,2,3] `mappend` [4,5,6]
    [1,2,3,4,5,6]
    ghci> B.pack [99,104,105] `mappend` B.pack [104,117,97,104,117,97]
    Chunk "chi" (Chunk "huahua" Empty)

¡Genial! Ahora ``applyLog`` pede funcionar con cualquier monoide. Tenemos que
cambiar la declaración de tipo para que lo refleje, y también la
implementación ya que tenemos cambiar ``++`` por ``mappend``: ::

    applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)
    applyLog (x,log) f = let (y,newLog) = f x in (y,log `mappend` newLog)

Como el valor que acompaña al valor original ahora puede ser cualquier tipo
de monoide, ya no tenemos que porque ver la dupla como una valor y un
registro, sino como una valor y un monoide. Por ejemplo, podemos tener una
tupla que tenga el nombre de un producto y su precio como valor monoidal.
Simplemente tenemos que utilizar el ``newtype`` ``Sum`` para asegurarnos de
que los precios se suman. Aquí tienes un ejemplo de una función que añade
la bebida para cierto tipo de comida de cowboy: ::

    import Data.Monoid

    type Food = String
    type Price = Sum Int

    addDrink :: Food -> (Food,Price)
    addDrink "beans" = ("milk", Sum 25)
    addDrink "jerky" = ("whiskey", Sum 99)
    addDrink _ = ("beer", Sum 30)

Utilizamos cadenas para representar las comidas y un ``Int`` dentro de un
``newtype`` ``Sum`` para mantener el precio total. Recuerda, cuando utilizamos
``mappend`` con ``Sum`` el resultado será la suma de ambos parámetros:

.. code-block:: console

    ghci> Sum 3 `mappend` Sum 9
    Sum {getSum = 12}

La función ``addDrink`` es bastante simple. Si estamos comiendo alubias,
devuelve ``"milk"`` junto ``Sum 25``, es decir 25 centavos dentro de un
``Sum``. Si estamos comiendo cecina bebemos whisky y si estamos comiendo
cualquier otra cosa bebemos cerveza. Aplicar esta función a una comida no
sería muy interesante, pero si utilizamos ``applyLog`` para pasar una comida
junto a un precio a esta función la cosa se vuelve más interesante:

.. code-block:: console

    ghci> ("beans", Sum 10) `applyLog` addDrink
    ("milk",Sum {getSum = 35})
    ghci> ("jerky", Sum 25) `applyLog` addDrink
    ("whiskey",Sum {getSum = 124})
    ghci> ("dogmeat", Sum 5) `applyLog` addDrink
    ("beer",Sum {getSum = 35})

La leche cuesta ``25`` centavos, pero si comemos alubias que cuestan ``10``
centavos, acabaremos pagando ``35`` centavos. Ahora se ve claramente como el
valor que acompañamos no tiene porque ser siempre un registro, puede ser
cualquier tipo de monoide y como se unan ambos valores dependerá de ese
monoide. Cuando utilizamos registros, se concatenan, cuando utilizamos
números, se suman, etc.

Como el valor que devuelve ``addDrink`` es una dupla del tipo
``(Food,Price)``, podemos pasar el resultado a ``addDrink`` de nuevo, de forma
que el resultado nos diga que vamos a beber y cuanto nos a costado en total.
Aquí tienes una muestra:

.. code-block:: console

    ghci> ("dogmeat", Sum 5) `applyLog` addDrink `applyLog` addDrink
    ("beer",Sum {getSum = 65})

Si añadimos una bebida a un poco de carne de perro obtendremos una cerveza
y otros ``30`` centavos de más, ``("beer", Sum 35)``. Si utilizamos
``applyLog`` para pasar este último valor a ``addDrink``, obtenemos otra
cerveza y el resultado final será ``("beer", Sum 35)``.


El tipo ``Writer``
''''''''''''''''''


Ahora que hemos visto que un valor junto a un monoide puede actuar como un
valor monoidal, vamos a explorar la instancia de ``Monad`` para esos valores.
El módulo ``Contol.Monad.Writer`` exporta el tipo ``Writer w a`` junto su
instancia de ``Monad`` y algunas funciones útiles para trabajar con valores de
este tipo.

Primero vamos a explorar el tipo en si mismo. Para adjuntar un monoide a un
valor solo tenemos que ponerlos juntos en una dupla. El tipo ``Writter w a``
es solo un ``newtype`` de la dupla. Su definición es muy simple: ::

    newtype Writer w a = Writer { runWriter :: (a, w) }

Gracias a que esta definido con ``newtype`` podemos crear una instancia de
``Monad`` que se comporte de forma diferente a la instancia de las tuplas
normales. El parámetro de tipo ``a`` representa el tipo del valor mientras
que el parámetro de tipo ``w`` representa el valor monádico que adjuntamos al
valor.

Su instancia de ``Monad`` se define así: ::

    instance (Monoid w) => Monad (Writer w) where
        return x = Writer (x, mempty)
        (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')

.. image:: /images/angeleyes.png
   :align: right
   :alt: Cuando vayas al retrete, haz tus necesidades, no hables.

Antes de nada vamos a ver ``>>=``. Su implementación es básicamente la misma
que ``applyLog``, solo que ahora la dupla está contenida en el ``newtype``
``Writer``, así que tenemos que extraerla con ayuda de un patrón. Tomamos el
valor ``x`` y le aplicamos la función ``f``. Esto nos da un valor del tipo
``Writer w a`` que, con ayuda de una expresión ``let, lo ajustamos a un
patrón. Llamamos ``y`` al nuevo resultado y utilizamos ``mappend`` para
combinar el monodie antiguo con el nuevo. Juntamos ambos valores en una dupla
, luego dentro del constructor ``Writer`` y por fin este será el resultado
final.

¿Qué pasa con ``return``? Tiene que tomar un valor e introducirlo en el
contexto mínimo por defecto que pueda albergar dicho valor como resultado
¿Cúal será ese contexto para los valores del tipo ``Writer``? Tiene sentido
que si queremos que el valor del monoide afecte tan poco como sea posible
utilizar ``mempty``. Utilizamos ``mempty`` como identadad para los valores
monoidales, como ``""``, ``Sum 0``, cadenas de bytes vacías, etc. Siempre que
utilicemos ``mempty`` junto a ``mappend`` y algún otro valor monoidal, el
resultado será el valor monoidal. Así que si utilizamos ``return`` para crear
un valor del tipo ``Writer`` y luego utilizamos ``>>=`` para pasárselo a una
función, el valor monoidal resultante será igual al que devuelva la función.
Vamos a utitlizar ``return`` con el número ``3`` unas cuantas veces, pero
cada vez con un monoide distinto:

.. code-block:: console

    ghci> runWriter (return 3 :: Writer String Int)
    (3,"")
    ghci> runWriter (return 3 :: Writer (Sum Int) Int)
    (3,Sum {getSum = 0})
    ghci> runWriter (return 3 :: Writer (Product Int) Int)
    (3,Product {getProduct = 1})

Como ``Writer`` no tiene una instancia de ``Show``, tenemos que utilizar
``runWriter`` para convertir los valores de ``Writer`` en tuplas normales que
puedan ser mostradas. Para ``String``, el valor monoidal es la cadena vacía.
Con ``Sum``, es ``0`` porque si sumamos 0 a algo, el resultado será el mismo.
Para ``Product`` la identidad es ``1``.

La instancia de ``Writer`` no posee ninguna implementación de ``fail``, así
que si un ajuste de patrones falla dentro de un bloque ``do`` se llamará a la
función ``error``.


Utilizando la notación ``do`` junto a ``Writer``
''''''''''''''''''''''''''''''''''''''''''''''''

Ahora que tenemos una instancia de ``Monad`` podemos utilizar la notación
``do`` con valores ``Writer``. Es útil para cuando tenemos varios valores del
tipo ``Writer`` y queremos hacer cosas con ellas. Al igual que la demás
mónadas, podemos tratar estos valores como valores normales dejando que se
ocupen del contexto por nosotros. En este caso, toda los valores monoidales
se unen con ``mappend`` y por lo tanto se reflejan en el resultado final. Aquí
tiene un ejemplo de uso de la notación ``do`` con ``Writer``: ::

    import Control.Monad.Writer

    logNumber :: Int -> Writer [String] Int
    logNumber x = Writer (x, ["Got number: " ++ show x])

    multWithLog :: Writer [String] Int
    multWithLog = do
        a <- logNumber 3
        b <- logNumber 5
        return (a*b)

``logNumber`` toma un número y crea un valor ``Writer`` a partir de él.
Utilizamos una lista de cadenas como monoide de forma que adjuntamos una
lista unitaria que dice que número hemos utilizado. ``multWithLog`` es un
valor del tipo ``Writer`` que multiplica un ``3`` y un ``5`` y se asegura que
los registros de ambos números aparezcan en el resultado final. Utilizamos
``resultado`` para devolver ``a*b`` como resultado. Como ``return`` toma un
valor y lo introduce en el contexto mínimo por defecto, podemos estar seguros
de que no añadirá nada al registro. Esto es lo que vemos si lo ejecutamos:

.. code-block:: console

    ghci> runWriter multWithLog
    (15,["Got number: 3","Got number: 5"])

A veces solo queremos que cierto valor monoidal sea incluido llegado el
momento. Para ello tenemos la función ``tell`` que forma parte de la clase de
tipos ``MonadWriter``. Para la instancia de ``Writer``, toma un valor
monoidal, como ``["This is going on"], y crea un valor del tipo ``Writer``
con resultado ``()`` y como valor monoidal adjunto el valor que le hayamos
pasado. Cuando tenemos un resultado como ``()`` no lo ligamos a ninguna
variable. Aquí tienes como se vería ``multWithLog`` con un reporte adicional:
::

    multWithLog :: Writer [String] Int
    multWithLog = do
        a <- logNumber 3
        b <- logNumber 5
        tell ["Gonna multiply these two"]
        return (a*b)

Es importante que ``return (a*b)`` esté en la última línea porque la última
línea de una expresión ``do`` es el resultado final del bloque entero. Si
hubiésemos puesto ``tell`` en la última línea, ``()`` hubiera sido el
resultado final de esta expresión ``do``. Hubiéramos perdido el resultado de
la multiplicación, además que el tipo de la expresión hubiera sido
``multWithLog :: Writer () Int``. Sin embargo, el registro hubira sido el
mismo. Aquí lo tienes en acción:

.. code-block:: console

    ghci> runWriter multWithLog
    (15,["Got number: 3","Got number: 5","Gonna multiply these two"])


Añadiendo registros a los programas
'''''''''''''''''''''''''''''''''''

El algoritmo de Euclides es un algoritmo que toma dos números y calcula su
máximo común divisor. Es decir, el número más grande que puede dividir a
ambos. Haskell ya posee la función ``gcb``, que hace exactamente esto, pero
vamos a implementarla de nuevo para añadirle un registro. Aquí esta el
algoritmo normal: ::

    gcd' :: Int -> Int -> Int
    gcd' a b
        | b == 0    = a
        | otherwise = gcd' b (a `mod` b)

El algoritmo es muy sencillo. Primero, comprueba si el segundo número es 0.
Si lo es, entonces el resultado es el primer número. Si no lo es, entonces el
resultado es el máximo común divisor del segundo número y del resto de dividir
el primer número por el segundo. Por ejemplo, si queremos saber el máximo
común divisor de 8 y 3 simplemente tenemos que seguir el algoritmo. Como 3 no
es 0, tenemos que encontrar el máximo común divisor de de 3 y 2 (si dividimos
8 por 3, el resto es 2). Luego, tenemos que encontrar el máximo común divisor
de 3 y 2. 2 aún no es igual 0, así que tenemos 2 y 1. El segundo número aún
no es 0 así que volvemos a aplicar el algoritmo para obtener 1 y 0, ya que
dividir 2 por 1 nos da como resto 0. Finalmente, como el segundo número es 0,
el resultado final es 1. Vamos a ver si Haskell opina lo mismo:

.. code-block:: console

    ghci> gcd' 8 3
    1

Lo hace. Ahora, queremos adjuntar un contexto a este resultado, y el contexto
será un valor monoidal a modo de registro. Como antes, utilizaremos una lista
de cadenas como monoide. De este modo, el tipo de la nueva función ``gcd'``
será: ::

    gcd' :: Int -> Int -> Writer [String] Int

Todo lo que nos queda por hacer es añadir a la función los valores del
registro. Así será el código: ::

    import Control.Monad.Writer

    gcd' :: Int -> Int -> Writer [String] Int
    gcd' a b
        | b == 0 = do
            tell ["Finished with " ++ show a]
            return a
        | otherwise = do
            tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
            gcd' b (a `mod` b)

Esta función toma dos valores ``Int`` normales y devuelve un
``Writer [String] Int``. Es decir, un ``Int`` que contiene un contexto de
registro. En caso de que ``b`` sea ``0``, en lugar de únicamente devolver
``a`` como resultado, utilizamos una expresión ``do`` para unir un valor del
tipo ``Writer`` con el resultado. Primero utilizamos ``tell`` para indicar
que hemos terminado  luego utilizamos ``return`` para devolver ``a`` como
resultado del bloque ``do``. En lugar de utilizar esta expresión ``do``
podíamos haber utilizado simplemente: ::

    Writer (a, ["Finished with " ++ show a])

Aún así la expresión ``do`` parece más legible. Luego tenemos el caso en el
que ``b`` no es igual a ``0``. En este caso, indicamos que vamos a utilizar
``mod`` para averiguar cual es el resto de dividir ``a`` por ``b``. La
segunda línea del bloque ``do`` simplemente llama de forma de recursiva a
``gcd'``. Recuerda que ``gcd'`` al final devuelve un valor del tipo
``Writer``, así que es perfectamente válido que ``gcd' b (a `mod` b)`` sea
una línea de la expresión ``do``.

Vamos a probar esta nueva versión de ``gcd'``. Su resultado es del tipo
``Writer [String] Int`` así que debemos extraer la dupla de este ``newtype``.
Luego, el primer componente de la dupla será el resultado. ::

    ghci> fst $ runWriter (gcd' 8 3)
    1

¡Bien! Ahora, ¿qué pasa cono el registro? Como el registro es una lista de
cadenas, vamos a utilizar ``mapM_ putStrLn`` par mostrar las cadenas por
pantalla:

.. code-block:: console

    ghci> mapM_ putStrLn $ snd $ runWriter (gcd' 8 3)
    8 mod 3 = 2
    3 mod 2 = 1
    2 mod 1 = 0
    Finished with 1

Es genial como hemos sido capaces de cambiar el algoritmo original a uno que
devuelva lo que está sucediendo simplemente cambiando los valores normales por
valores monádicos y dejando que la implementación de ``>>=`` para ``Writer``
se encargue de los registros por nosotros. Podemos añadir este mecanismo de
registro casi a cualquier función. Solo tenemos que remplazar los valores
normales por valores del tipo ``Writer`` y cambiar la aplicación normal de
funciones por ``>>=`` (o por expresiones ``do`` si vemos que es más legible).


Construcción de listas ineficiente
''''''''''''''''''''''''''''''''''

Cuando utilizamos la mónada ``Writer`` hay que tener cuidado con que monoide
utilizar, ya que utilizar listas como monoides puede resultar en una ejecución
muy lenta. Esto se debe al uso de ``++`` de ``mappend``, añadir una lista al
final de otra puede ser muy costoso si una lista es muy larga.

En la función ``gcd'``, el registro es rápido porque la lista se acaba
pareciendo a esto: ::

    a ++ (b ++ (c ++ (d ++ (e ++ f))))

Las listas son estructuras de datos que se construyen de izquierda a derecha,
y esto último es eficiente porque primero construimos la parte izquierda de la
lista y solo después de construirla añadimos una lista más larga a la derecha.
Pero si no tenemos cuidado al utilizar la mónada ``Writer`` podemos producir
listas que se parezcan a: ::

    ((((a ++ b) ++ c) ++ d) ++ e) ++ f

Esta lista se asocia por la izquierda en vez de por la derecha. No es
eficiente porque cada vez que queramos añadir la parte derecha a la parte
izquierda tiene que construir la parte izquierda desde el principio.

La siguiente función funciona igual que ``gdc'``, solo que registra las
cadenas al revés. Primero produce el registro del procedimiento y luego añade
el paso actual al final del registro. ::

    import Control.Monad.Writer

    gcdReverse :: Int -> Int -> Writer [String] Int
    gcdReverse a b
        | b == 0 = do
            tell ["Finished with " ++ show a]
            return a
        | otherwise = do
            result <- gcdReverse b (a `mod` b)
            tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
            return result

Primero realiza el paso de recursión y liga el resultado a ``result``. Luego,
añade el paso actual al registro, pero el paso actual debe ir al final del
registro que a sido producido por la recursión. Al final, devuelve el
resultado de la recursión como resultado final.

.. code-block:: console

    ghci> mapM_ putStrLn $ snd $ runWriter (gcdReverse 8 3)
    Finished with 1
    2 mod 1 = 0
    3 mod 2 = 1
    8 mod 3 = 2

Es ineficiente porque acaba asociando el uso de ``++`` por la izquierda en
lugar de por la derecha.


Listas de diferencia
''''''''''''''''''''

.. image:: /images/cactus.png
   :align: left
   :alt: Cactus

Como la listas a veces son ineficientes cuando se concatenan repetidamente de
esta forma, lo mejor es utilizar un estructura de datos que cuando se
concatene sea siempre eficiente. Una estructura de este tipo es la lista de
diferencia. Una lista de diferencia es similar a una lista, solo que en lugar
de ser una lista normal, es una función que toma un lista y la antepone a
otra lista. La lista de diferencia equivalente a la lista ``[1,2,3]`` sería
la función ``\xs -> [1,2,3] ++ xs``. Un lista vacía normal ``[]`` equivaldría
a ``\xs -> [] ++ xs``.

Lo interesante de las listas de diferencia es que soportan la concatenación de
forma eficiente. Cuando añadimos los listas normales con ``++``, hay que
recorrer toda la lista de la izquierda hasta el final y luego añadir la otra
ahí. Pero, ¿y si tomamos el enfoque de las listas de diferencia y
representamos las listas como funciones? Bueno, entones añadir dos listas
diferentes sería: ::

    f `append` g = \xs -> f (g xs)

Recuerda que ``f`` y ``g`` son funciones que toman lista y la anteponen a
otra lista. Así que, por ejemplo, si la función ``f`` es ``("dog"++)`` (que
es otra forma de decir que es ``\xs -> "dog" ++ xs``) y la función ``g`` es
``("meat"++)``, entonces ``f `append` g`` crea una nueva función que será
equivalente a: ::

    \xs -> "dog" ++ ("meat" ++ xs)

Hemos concatenado dos listas de diferencia creando una nueva función que
primero aplica una lista de diferencia y luego aplica la otra.

Vamos a crear un ``newtype`` para estas listas de diferencia de forma que
podamos darle fácilmente una instancia de ``Monoid``. ::

    newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

El tipo que estamos definiendo es ``[a] -> [a]`` porque las listas de
diferencia son solo funciones que toma una lista y devuelven otra. Convertir
listas normales en listas de diferencia y viceversa es fácil: ::

    toDiffList :: [a] -> DiffList a
    toDiffList xs = DiffList (xs++)

    fromDiffList :: DiffList a -> [a]
    fromDiffList (DiffList f) = f []

Para crear una lista de diferencia a partir de una lista normal solo tenemos
que hacer lo que ya hicimos antes, crear una función que añada una lista a
ella. Como una lista de diferencia es una función que antepone algo a una
lista, si queremos ese algo tenemos que aplicar la función a la lista
vacía.

Aquí esta la instancia de ``Monoid``: ::

    instance Monoid (DiffList a) where
        mempty = DiffList (\xs -> [] ++ xs)
        (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

Fíjate que ``mempty`` es igual a ``id`` y ``mappend`` es en realidad una
composición de funciones. Vamos a ver como funciona:

.. code-block:: console

    ghci> fromDiffList (toDiffList [1,2,3,4] `mappend` toDiffList [1,2,3])
    [1,2,3,4,1,2,3]

Ahora podemos aumentar la eficiencia de la función ``gcdReverse`` haciendo
que utilice listas de diferencia en lugar de listas normales: ::

    import Control.Monad.Writer

    gcd' :: Int -> Int -> Writer (DiffList String) Int
    gcd' a b
        | b == 0 = do
            tell (toDiffList ["Finished with " ++ show a])
            return a
        | otherwise = do
            result <- gcd' b (a `mod` b)
            tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
            return result

Solo tenemos que cambiar el tipo del monoide de ``[String]`` a ``DiffList
String`` y luego cuando utilizamos ``tell`` convertir las listas normales a
listas de diferencia con ``toDiffList``. Vamos a ver si se parecen:

.. code-block:: console

    ghci> mapM_ putStrLn . fromDiffList . snd . runWriter $ gcdReverse 110 34
    Finished with 2
    8 mod 2 = 0
    34 mod 8 = 2
    110 mod 34 = 8

Ejecutamos ``gcdReverse 110 34``, luego utilizamos ``runWriter`` para extraer
desde ``newtype``, luego aplicamos ``snd`` para obtener el registro, y para
terminar aplicamos ``fromDiffList`` para convertir la lista de diferencia en
una lista normal que luego mostramos por pantalla.


Comparando el rendimiento
'''''''''''''''''''''''''

Para hacernos una idea de cuanto mejoran el rendimiento las listas de
diferencia, considera esta función que simplemente hace una cuenta atrás hasta
cero, pero produce el registro al revés, al igual que ``gcdReverse``: ::

    finalCountDown :: Int -> Writer (DiffList String) ()
    finalCountDown 0 = do
        tell (toDiffList ["0"])
    finalCountDown x = do
        finalCountDown (x-1)
        tell (toDiffList [show x])

Si le pasamos un ``0``, lo registra. Para cualquier otro número, primero
cuenta su predecesor y luego añade el número actual al registro. Así que
si aplicamos ``finalCountDown`` a ``100``, la cadena ``"100"`` será la última
en registrar.

De cualquier modo, si cargamos esta función en *GHCi*  y la aplicamos a un
número muy grande, como ``500000``, veremos que empieza a contar desde ``0``
rápidamente.

.. code-block:: console

    ghci> mapM_ putStrLn . fromDiffList . snd . runWriter $ finalCountDown 500000
    0
    1
    2
    ...

Sin embargo, si cambiamos la función para que utilice listas normales: ::

    finalCountDown :: Int -> Writer [String] ()
    finalCountDown 0 = do
        tell ["0"]
    finalCountDown x = do
        finalCountDown (x-1)
        tell [show x]

Y luego le decimos a *GHCi* que empiece a contar:

.. code-block:: console

    ghci> mapM_ putStrLn . snd . runWriter $ finalCountDown 500000

Veremos que va muy despacio.

Por supuesto, esta no es la forma correcta y científica de probar lo rápidos
que son nuestros programas, pero al menos podemos ver que para este caso,
utilizar listas de diferencia produce resultados de la forma apropiada
mientras que las listas normales tardan una eternidad.

Por cierto, te estará rondando por la cabeza el estribillo de la canción
*Final Countdown* de *Europe*, así que, ¡disfrútala!


¿Reader? O no, otra vez la misma broma...
-----------------------------------------

En el capítulo que hablábamos de los
:ref:`funtores aplicativos <cfuntores>`, vimos que el tipo función,
``(->) r`` posee una instancia de ``Functor``. Al mapear una función ``f``
sobre una función ``g`` creamos una función que tomará los mismo parámetros
que ``g``, aplicará la función ``g`` y luego aplicará ``f`` a su resultado.
Básicamente estamos creando una función igual que ``g``, solo que en vez de
devolver su resultado, devuelve el resultado de aplicar ``f``. Por ejemplo:

.. code-block:: console

    ghci> let f = (*5)
    ghci> let g = (+3)
    ghci> (fmap f g) 8
    55

También vimos que las funciones son funtores aplicativos. Nos permiten operar
sobre funciones como si se tratasen de los resultados. Un ejemplo:

.. code-block:: console

    ghci> let f = (+) <$> (*2) <*> (+10)
    ghci> f 3
    19

La expresión ``(+) <$> (*2) <*> (+10)`` crea una función que toma un número,
pasa ese número a ``(*2)`` y a ``(+10)`` y luego suma ambos resultados. Por
ejemplo, si aplicamos esta función a ``3``, aplica tanto ``(*2)`` como
``(+10)`` a ``3``, obteniendo ``6`` y ``13`` y luego los suma devolviendo
``19``.

El tipo función ``(->) r`` no es solo un funtor y un funtor aplicativo, sino
también una mónada. De la misma forma que cualquier otro valor monádico que
ya hemos visto, una función puede ser vista como un valor con un contexto. El
contexto en este caso es que el valor aún no esta presente de forma que
tenemos que aplicar esa función a algo antes de obtener el resultado.

Como ya sabemos como funcionan las funciones como funtores y funtores
aplicativos, vamos a ver como luce su instancia de ``Monad``. Se encuentra en
``Control.Monad.Instances``. ::

    instance Monad ((->) r) where
        return x = \_ -> x
        h >>= f = \w -> f (h w) w

Ya vimos como se implementaba ``pure`` para las funciones y ``return`` es
básicamente lo mismo. Toma un valor y lo introduce en el contexto mínimo que
siempre tendrá como resultado ese valor. Y la única forma de crear una
función que siempre tenga el mismo resultado es ignorando completamente su
parámetro.

La implementación de ``>>=`` puede parecer algo compleja, pero en realidad en
muy sencilla. Cuando utilizamos ``>>=`` para pasar un valor monádico a una
función, el resultado siempre es un valor monádico. Así que en este caso,
pasamos una función a otra función, y resultado será también una función. Por
este motivo la definición de ``>>=`` es una función lambda. Todas las
implementaciones de ``>>=`` que hemos visto hasta ahora siempre asilaban el
resultado del valor monádico de algún modo y luego le aplicaban la función
``f``. Aquí pasa lo mismo. Para obtener el resultado de una función, tenemos
que aplicarla a algo, por este motivo hacemos ``(h w)`` aquí, para obtener el
resultado de una función y luego le aplicamos ``f``. ``f`` devuelve un valor
monádico, que es una función en este caso, así que que le aplicamos ``w`` de
nuevo.

Si no entiendes como funciona ``>>=`` en este momento, no te preocupes, con
unos cuantos ejemplos veremos que es una mónada muy simple. Aquí tienes un
ejemplo de como usar una expresión ``do`` con esta mónada: ::

    import Control.Monad.Instances

    addStuff :: Int -> Int
    addStuff = do
        a <- (*2)
        b <- (+10)
        return (a+b)

Es básicamente la misma expresión aplicativa que escribimos antes, solo que
ahora vemos las funciones como mónadas. Una expresión ``do`` siempre resulta
en un valor monádico. En este caso tomamos un número y luego aplicamos
``(*2)`` a ese número y el resultado lo ligamos a ``a``. ``(+10)`` se aplica
de nuevo al mismo número y ligamos su resultado a ``b``. ``return``, como en
todas las demás mónadas, no tiene ningún otro efecto aparte de el de crear un
valor monádico que contendrá algún resultado. En este caso crea una función
que contendrá ``(a+b)`` como resultado. Si lo probamos veremos que obtenemos
los mismos resultados:

.. code-block:: console

    ghci> addStuff 3
    19

Tanto ``(*2)`` como ``(+10)`` se aplican al número ``3``. ``return (a+b)``
también se aplica a ``3`` pero lo ignora y siempre devuelve ``(a+b)`` como
resultado. Por este motivo, la mónada de las funciones es conocida como la
mónada lectora (*reader* en inglés, en contraposición de *writer*, escritora).
Todas las funciones leen de la misma fuente. Podemos ilustrar esto incluso
mejor, podemos reescribir ``addStuff`` como: ::

    addStuff :: Int -> Int
    addStuff x = let
        a = (*2) x
        b = (+10) x
        in a+b

Podemos ver como la mónada lectora nos permite tratar a las funciones como
valores en un cierto contexto. Podemos actuar como ya conociéramos lo que van
a devolver. Lo que hacemos es unir todas las funciones en una sola y luego
pasamos el parámetro de esta función a todas las demás. Si tenemos un montón
de funciones a las que les faltan un solo parámetro y al final este parámetro
será igual para todas, podemos utilizar la mónada lectora para extraer sus
futuros resultados y la implementación de ``>>=`` se encargará de que todo
funcione al final.


Mónadas monas con estado
------------------------


.. image:: /images/texas.png
   :align: left
   :alt: No juegues con Texas.

Haskell es un lenguaje puro y como tal, los programas consisten en funciones
que no pueden cambiar ningún estado global o variables, solo pueden hacer
algunos cálculos o cómputos y devolver resultados. Esta restricción hace que
sea más fácil razonar acerca de los programas ya que no tenemos que
preocuparnos por el estado de una variable a lo largo del tiempo. Sin embargo,
algunos problemas posee de forma inherentemente estados que cambian con el
tiempo. Aunque estos estos estados no causan ningún problema a Haskell, a
veces pueden ser un poco tediosos de modelar. Por esta razón Haskell posee la
mónada estado, la cual nos permite tratar los problemas con estados como si
fueran un juego de niños y además mantiene todo el código puro.

Cuando estábamos trabajando con :ref:`números aleatorios <aleatoriedad>`,
utilizábamos funciones que tomaban un generador de aleatoriedad como parámetro
y devolvían un número aleatorio y un nuevo generador de aleatoriedad. Si
queríamos generar varios números aleatorios, siempre teníamos que utilizar el
generador de aleatoriedad que devolvió la función anterior. Si queremos crear
una función que tome un generador de aleatoriedad y devuelva el resultado de
lanzar una moneda tres veces, tenemos que hacer esto: ::

    threeCoins :: StdGen -> (Bool, Bool, Bool)
    threeCoins gen =
        let (firstCoin, newGen) = random gen
            (secondCoin, newGen') = random newGen
            (thirdCoin, newGen'') = random newGen'
        in  (firstCoin, secondCoin, thirdCoin)

Toma un generador ``gen`` y luego ``random gen`` devuelve un ``Bool`` junto
con un nuevo generador. Para lanzar la segunda moneda, utilizamos el nuevo
generador, y así sucesivamente. La mayoría de los otros lenguajes no hubieran
devuelto un nuevo generador junto con el número aleatorio. Simplemente
habrían modificado el generador original. Pero Haskell es puro, no podemos
hacer esto, así que tenemos que tomar un estado, crear un resultado a partir
de él y producir un nuevo estado que será utilizado para generar nuevos
resultados.

Si crees que para evitar tratar manualmente con estos estado en Haskell
tenemos que perder la pureza de nuestro código, estás equivocado. Existe una
pequeña mónada, llamada la mónada estado, que se encarga de manejar todo lo
relaciona con estado sin renegar a la pureza.

Así que, para entender mejor todo este concepto de cómputos con estado vamos
a darle un tipo. Antes hemos dicho que un cómputo con estado es una función
que toma un estado y produce un resultado junto a un nuevo estado. Esta
función tendría un tipo como este: ::

    s -> (a,s)

``s`` es el estado y ``a`` el resultado de estos cómputos con estado.

.. note:: En otros lenguajes, la asignación de variables puede verse como un
          especie de cómputo con estado. Por ejemplo, si hacemos ``x = 5`` en
          un lenguaje imperativo, se asignará el valor ``5`` a la variable
          ``x`` y la expresión tendrá un resultado igual a ``5``. Podemos ver
          esta funcionalidad como si la asignación fuera una función que toma
          un estado (es decir, todas las variables que han sido asignadas
          anteriormente) y devuelve un resultado (en este caso ``5``) y nuevo
          estado que será el conjunto de todas las variables anteriores más
          la nueva asignación.

Estos cómputos con estado, funciones que toman un estado y devuelven un
resultado junto con un nuevo estado, también se pueden ver como un valor en
cierto contexto. El valor real es es el resultado, mientras que el contexto es
el estado inicial del que hemos extraído el resultado, generando así un nuevo
estado.


Pilas y pilones
'''''''''''''''

Digamos que queremos modelar una pila. Tenemos un pila de cosas una encima de
otra y podemos o bien añadir otra cosa encima de la pila o bien tomar una cosa
de la cima de la pila. Cuando ponemos un objeto en la cima de la pila decimos
que estamos apilando un objeto, y cuando tomamos un objeto de la pila decimos
que estamos retirando un objeto. Si queremos el objeto que se encuentra más
abajo de la pila tenemos que retirar antes todos los objetos que se encuentran
por encima de éste.

Utilizaremos una lista para representar la pila, y su cabeza para representar
la cima de la pila. Para hacer las cosas más fáciles, vamos a crear dos
funciones: ``pop`` y ``push``. ``pop`` tomará una pila y retirará un elemento
que devolverá como resultado, junto a una nueva pila sin dicho elemento en la
cima. ``push`` tomará un elemento y una pila y luego apilará dicho elemento en
la pila. Devolverá ``()`` como resultado, junto a una nueva pila. ::

    type Stack = [Int]

    pop :: Stack -> (Int,Stack)
    pop (x:xs) = (x,xs)

    push :: Int -> Stack -> ((),Stack)
    push a xs = ((),a:xs)

A la hora de apilar un elemento devolvemos ``()`` porque el hecho de apilar un
elemento no tienen ningún resulto importante, su principal objetivo es
modificar la pila. Fíjate que en ``push`` solo hemos añadido el primer
parámetro, obteniendo así un cómputo con estado. ``pop`` ya es de por si un
cómputo con estado debido a su tipo.

Vamos a escribir un trocito de código que simule el uso de estas funciones.
Tomaremos una pila, apilaremos un ``3`` y luego retiraremos dos elementos,
para pasar el rato más que nada. ::

    stackManip :: Stack -> (Int, Stack)
    stackManip stack = let
        ((),newStack1) = push 3 stack
        (a ,newStack2) = pop newStack1
        in pop newStack2

Tomamos una pila (``stack``) y luego hacemos ``push 3 stack``, lo que nos
devuelve una tupla. La primera parte de la tupla es ``()`` y la segunda es
una nueva pila que llamaremos ``newStack1``. Luego, retiramos un número de
``newStack1``, lo cual devuelve ese número ``a`` (que es ``3``) y una nueva
pila que llamaremos ``newStack2``. Luego retiramos otro elemento de
``newStack2`` y obtenemos un número ``b`` y una pila ``newStack3``. Devolvemos
una dupla que contendrá ese número y esa tupla. Vamos a probarlo:

.. code-block:: console

    ghci> stackManip [5,8,2,1]
    (5,[8,2,1])

Genial, el resultado es ``5`` y la pila es ``[8,2,1]``. El mismo
``stackManip`` es un cómputo con estado. Hemos tomado un puñado de cómputos
con estado y de alguna forma los hemos unido todos. Mmm... Me recuerda a algo.

El código que acabamos de ver es algo tedioso ya que tenemos que pasar el
estado manualmente en cada cómputo, además de que tenemos que ligarlo a una
variable para luego pasarlo al siguiente cómputo ¿No sería mejor si, en lugar
de pasar una pila manualmente a cada función, pudiéramos escribir algo como
esto? ::

    stackManip = do
        push 3
        a <- pop
        pop

Bueno, pues usando la mónada estado podemos hacerlo. Gracias a ella podemos
tomar cómputos con estado como estos y usarlos sin tener que preocuparnos por
manejar el estado de forma manual.


La mónada estado
''''''''''''''''

El módulo ``Control.Monad.State`` contiene un ``newtype`` para los cómputos
con estado. Aquí tienes su definición: ::

    newtype State s a = State { runState :: s -> (a,s) }

Un ``State s a`` es un cómputo con estado que manipula el estado del tipo
``s`` y tiene como resultado el tipo ``a``.

Ahora que ya hemos visto como funcionan los cómputos con estado y que incluso
podemos verlos como valores en cierto contexto, vamos a comprobar su
instancia de ``Monad``: ::

    instance Monad (State s) where
        return x = State $ \s -> (x,s)
        (State h) >>= f = State $ \s -> let (a, newState) = h s
                                            (State g) = f a
                                        in  g newState

Vamos a echar un vistazo primero a ``return``. Nuestro objetivo con ``return``
es tomar un valor y crear un cómputo con estado que siempre contenga ese
valor como resultado. Por este motivo creamos una función lamba
``\s -> (s,a)``. Siempre devolvemos ``x`` como resultado del cómputo con
estado y además el estado se mantiene constante, ya que ``return`` debe
insertar un valor en el contexto mínimo. Recapitulando, ``return`` tomará un
valor y creará un cómputo con estado que revolverá ese valor como resultado
y mantendrá el estado intacto.

.. image:: /images/badge.png
   :align: right
   :alt: Soy un agente de la ley.

¿Y ``>>=``? Bueno, el resultado de pasar un cómputo con estado a una función
con ``>>=`` es un cómputo con estado ¿no? Así que empezamos construyendo el
``newtype`` ``State`` y luego utilizamos una función lambda. La función lambda
será el cómputo con estado. Pero, ¿qué es lo que hace? Bueno, de alguna forma
debemos extraer el resultado del primer cómputo con estado. Como nos
encontramos dentro de un cómputo con estado, podemos pasarle el estado actual
``s`` a ``h``, lo cual devolverá un dupla con el resultado y un nuevo estado
``(a, newState)``. Siempre que hemos implementado ``>>=``, una vez extraído el
resultado de un valor monádico aplicábamos la función ``f`` sobre éste para
obtener un nuevo valor monádico. Por ejemplo, con ``Writer``, luego de obtener
el nuevo valor monádico, aún teníamos que asegurarnos de tratar el nuevo
contexto aplicando ``mappend`` entre el valor monoidal antiguo y el nuevo.
Aquí, realizamos ``f a`` para obtener un nuevo cómputo con estado ``g``. Ahora
que ya tenemos un nuevo cómputo con estado y nuevo estado (con el nombre de
``newState``) solo tenemos que aplicar ``g`` sobre ``newState``. El resultado
será una tupla, y al mismo tiempo, el resultado final.

Así que ``>>=`` básicamente se encarga de unir dos cómputos con estado, solo
que el segundo está oculto dentro de una función que se encarga de obtener el
resultado anterior. Como ``pop`` y ``push`` son ya cómputos con estado, es
muy fácil introducirlos dentro de ``State``. ::

    import Control.Monad.State

    pop :: State Stack Int
    pop = State $ \(x:xs) -> (x,xs)

    push :: Int -> State Stack ()
    push a = State $ \xs -> ((),a:xs)

``pop`` ya es en si mismo un cómputo con estado y ``push`` es una función que
toma un ``Int`` y devuelve un cómputo con estado. Ahora podemos reescribir el
ejemplo anterior que apilaba un ``3`` y luego retiraba  dos números así: ::

    import Control.Monad.State

    stackManip :: State Stack Int
    stackManip = do
        push 3
        a <- pop
        pop

¿Ves como hemos unido un ``push`` y dos ``pop`` juntos en un solo cómputo con
estado? Cuando extraemos el contenido del ``newtype`` obtenemos una función
a la que tenemos que pasarle el estado inicial:

.. code-block:: console

    ghci> runState stackManip [5,8,2,1]
    (5,[8,2,1])

De hecho no tenemos porque ligar el segundo ``pop`` a ``a`` ya que no
utilizamos ``a`` luego. Así que podemos reescribirlo de nuevo: ::

    stackManip :: State Stack Int
    stackManip = do
        push 3
        pop
        pop

Perfecto. Ahora queremos hacer esto: retiramos un número de la pila y si dicho
número es ``5`` lo devolvemos a la pila, si no, apilamos un ``3`` y un ``8``.
Así sería el código: ::

    stackStuff :: State Stack ()
    stackStuff = do
        a <- pop
        if a == 5
            then push a
            else do
                push 3
                push 8

Bastante sencillo. Vamos a ejecutarlo junto a un estado inicial.

.. code-block:: console

    ghci> runState stackStuff [9,0,2,1,0]
    ((),[8,3,0,2,1,0])

Recuerda que las expresiones ``do`` devuelve valores monádicos y en el caso
de la mónada ``State``, cada expresión ``do`` es también una función con
estado. Como tanto ``stackStuff`` y ``stackManip`` son cómputos con estado
normales y corrientes, podemos unirlos y producir un nuevo cómputo con estado.
::

    moreStack :: State Stack ()
    moreStack = do
        a <- stackManip
        if a == 100
            then stackStuff
            else return ()

Si el resultado de ``stackManip`` sobre la pila actual es ``100``, ejecutamos
``stackStuff``, si no no hacemos nada. ``return ()`` simplemente mantiene el
estado.

El módulo ``Control.Monad.State`` contiene una clase de tipos llamada
``MonadState`` y ésta a su vez contiene dos útiles funciones: ``get`` y
``put``. Para ``State``, ``get`` se implementa así: ::

    get = State $ \s -> (s,s)

Es decir, toma el estado actual y lo devuelve como resultado. La función
``put`` toma un estado y crea una función con estado que remplazará el estado
actual por su parámetro: ::

    put newState = State $ \s -> ((),newState)

Gracias a estas funciones, podemos ver que el contenido de la pila actual o
incluso remplazar toda la pila por una nueva. ::

    stackyStack :: State Stack ()
    stackyStack = do
        stackNow <- get
        if stackNow == [1,2,3]
            then put [8,3,1]
            else put [9,2,1]

Es bueno ver como quedaría el tipo de ``>>=`` si solo funcionará con valores
del tipo ``State``: ::

    (>>=) :: State s a -> (a -> State s b) -> State s b

Fíjate en que el tipo del estado ``s`` se mantiene constante pero sin embargo
el tipo del resultado puede cambiar de ``a`` a ``b``. Esto significa que
podemos unir varios cómputos con estado cuyos resultados sean de diferentes
tipos pero el tipo de sus estados sea el mismo. Y, ¿por qué? Bueno, por
ejemplo, para ``Maybe``, ``>>=`` tiene este tipo: ::

    (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b

Tiene sentido que la mónada en si misma, ``Maybe``, no cambie. No tendría
sentido que pudiéramos usar ``>>=`` con dos mónadas distintas. Bueno, en el
caso de la mónada estado, en realidad la mónada es ``State s``, así que si
``s`` fuera distinta, estaríamos utilizando ``>>=`` entre dos mónadas
distintas.


Aleatoriedad y la mónada estado
'''''''''''''''''''''''''''''''

Al principio de esta sección vimos que como se generaban número aleatorios y
que a veces puede ser algo pesado ya que cada función aleatoria toma un
generador y devuelve un número aleatorio junto un nuevo generador, que
tendremos que utilizar en lugar del viejo para generar otro número diferente.
La mónada estado hace que trabajar con todo esto sea mucho más cómodo.

La función ``random`` del módulo ``System.Random`` tiene este tipo: ::

    random :: (RandomGen g, Random a) => g -> (a, g)

Es decir, toma un generador de aleatoriedad y produce un número aleatorio
junto un nuevo generador. Podemos ver que en realidad se trata de un cómputo
con estado, así que podemos introducirlo en el constructor ``newtype``
``State`` y luego utilizarlo como un valor monádico de forma que no nos
tengamos que preocupar por manejar el estado: ::

    import System.Random
    import Control.Monad.State

    randomSt :: (RandomGen g, Random a) => State g a
    randomSt = State random

Así que si ahora queremos lanzar tres monedas (``True`` cruz, ``False`` cara)
solo tenemos que hacer lo siguiente: ::

    import System.Random
    import Control.Monad.State

    threeCoins :: State StdGen (Bool,Bool,Bool)
    threeCoins = do
        a <- randomSt
        b <- randomSt
        c <- randomSt
        return (a,b,c)

Ahora ``threeCoins`` es un cómputo con estado y luego de tomar un generador de
aleatoriedad inicial, lo pasa al primer ``randomSt``, el cual producirá un
número aleatorio y un nuevo generador, el cual será pasado al siguiente y así
sucesivamente. Utilizamos ``return (a,b,c)`` para devolver ``(a,b,c)`` como
resultado manteniendo constante el generador más reciente.

.. code-block:: console

    ghci> runState threeCoins (mkStdGen 33)
    ((True,False,True),680029187 2103410263)

Ahora realizar todo este tipo de tareas que requieren el uso de algún tipo de
estado es mucho más cómodo.


Errores, errores, errores...
----------------------------


Sabemos que ``Maybe`` se utiliza para dar el contexto de un posible fallo a
los valores. Un valor puede ser ``Just algo`` o ``Nothing``. Sin embargo,
cuando tenemos un ``Nothing``, puede resultar útil dar alguna información del
error que ha ocurrido, lo cual no podemos hacer con ``Nothing``.

Por otra parte, el tipo ``Either e a`` nos permite incorporar el contexto de
un posible fallo al mismo tiempo que nos permite dar información acerca del
posible fallo, de forma que podemos describir que ha ido mal o dar alguna
información acerca del fallo. Un valor del tipo ``Either e a`` puede ser un
valor ``Right``, lo cual representa un respuesta correcta, o un valor
``Left``, que representa un fallo. Por ejemplo:

.. code-block:: console

    ghci> :t Right 4
    Right 4 :: (Num t) => Either a t
    ghci> :t Left "out of cheese error"
    Left "out of cheese error" :: Either [Char] b

Básicamente es como un ``Maybe`` mejorado, así que tiene sentido que sea una
mónada. También puede ser visto como un valor con el contexto de un posible
fallo, solo que ahora existe un valor añadido cuando ocurre un error.

Su instancia de ``Monad`` es parecida a la de ``Maybe`` y se encuentra en
``Control.Monad.Error``: ::

    instance (Error e) => Monad (Either e) where
        return x = Right x
        Right x >>= f = f x
        Left err >>= f = Left err
        fail msg = Left (strMsg msg)

``return``, como siempre, toma un valor y lo introduce en el contexto mínimo
por defecto. Introduce un valor en el constructor ``Right`` ya que utilizamos
``Right`` para representar un cómputo con éxito donde existe un resultado. Se
parece mucho al ``return`` de ``Maybe``.

La función ``>>=`` examina dos posibles casos: un ``Left`` y un ``Right``. En
caso de ``Right``, la función ``f`` se aplica sobre el valor interior, de
forma similar a lo que sucedía con ``Just``. En caso de que ocurra un error,
se mantiene constante el valor de ``Left``, el cual da información acerca del
error.

La instancia de ``Monad`` para ``Either e`` tiene un requerimiento adicional,
y este es que el tipo del valor que está contenido en ``Left``, el parámetro
de tipo ``e`` en este caso, tiene que formar parte de la clase de tipos
``Error``. La clase de tipos ``Error`` es para los tipos cuyos valores pueden
actuar como mensajes de error. Define la función ``strMsg``, que toma un error
en forma de cadena y devuelve ese valor en forma de error. Un buen ejemplo de
instancia de ``Error`` es el tipo ``String``. Para el caso de ``String``, la
función ``strMsg`` simplemente devuelve la cadena que se le pasa:

.. code-block:: console

    ghci> :t strMsg
    strMsg :: (Error a) => String -> a
    ghci> strMsg "boom!" :: String
    "boom!"

Como normalmente utilizamos ``String`` para describir los errores no tenemos
que preocuparnos mucho por esto. Cuando un ajuste de patrones falla dentro de
una expresión ``do``, se devuelve valor ``Left`` para representar este error.

De cualquier modo, aquí tienes unos cuantos ejemplos:

.. code-block:: console

    ghci> Left "boom" >>= \x -> return (x+1)
    Left "boom"
    ghci> Right 100 >>= \x -> Left "no way!"
    Left "no way!"

Cuando utilizamos ``>>=`` para pasar un valor ``Left`` a una función, la
función se ignora y se devuelve un ``Left`` idéntico. Cuando pasamos un valor
``Right`` a una función, la función se aplica al contenido de éste, pero en
este caso la función devuelve un valor ``Left`` de todas formas.

Si intentamos pasar una valor ``Right``a una función que también devuelve un
valor ``Right`` en *GHCi*, nos encontraremos con un error peculiar.

.. code-block:: console

    ghci> Right 3 >>= \x -> return (x + 100)

    <interactive>:1:0:
        Ambiguous type variable `a' in the constraints:
          `Error a' arising from a use of `it' at <interactive>:1:0-33
          `Show a' arising from a use of `print' at <interactive>:1:0-33
        Probable fix: add a type signature that fixes these type variable(s)

Haskell dice que no sabe que tipo elegir para la parte ``e`` del tipo
``Either e a``, incluso aunque solo mostremos la parte ``Right``. Esto se debe
a la restricción ``Error e`` de la instancia de ``Monad``. Así que si no
quieres ver más errores de este tipo cuando trabajes con la mónada ``Either``,
añade un anotación de tipo explícita:

.. code-block:: console

    ghci> Right 3 >>= \x -> return (x + 100) :: Either String Int
    Right 103

¡Bien! Ahora funciona.

Aparte de este pequeño detalle, esta mónada es muy similar a la mónada
``Maybe``. En el capítulo anterior, utilizamos varios aspectos monádicos de
``Maybe`` para simular el aterrizaje de las aves en la barra de equilibrio de
nuestro buen amigo Pierre. A modo de ejercicio, puedes reescribir estas
funciones con la mónada error de forma que cuando el funambulista se caiga,
podamos informar del número de pájaros que había en la barra cuando se cayó.


Algunas funciones monádicas útiles
----------------------------------


En esta sección vamos a ver una cuantas funciones que pueden operar con
valores monádicos o devolver valores monádicos como resultado (¡o ambas
cosas!). Normalmente no referimos a estas funciones como funciones monádicas.
Mientras que algunas de éstas nos serán totalmente desconocidas, otras son las
versiones monádicas de algunas funciones que ya conocemos, como ``filter`` o
``foldl``.


``liftM`` y sus amigos
''''''''''''''''''''''

.. image:: /images/wolf.png
   :align: right
   :alt: ¡Yo también soy un agente de la ley!

Cuando empezamos nuestro viaje hacia la cima de las mónadas, primero vimos los
funtores, que son cosas que se pueden mapear. Luego vimos que podíamos mejorar
los funtores en algo que llamamos funtores aplicativos, que permitían aplicar
funciones normales entre valores aplicativos a la vez que permitían tomar
funciones e introducirlas en el contexto por defecto. Para terminar, vimos que
podíamos mejorar los funtores aplicativos y eso nos llevaba a las mónadas, que
añadían la habilidad de poder pasas esos valores con un cierto contexto a
funciones normales.

Resumiendo, todas las mónadas son funtores aplicativos y todos los funtores
aplicativos son funtores normales. La clase de tipos ``Applicative`` posee una
restricción de clase que dice que su tipo debe poseer una instancia de la
clase ``Functor`` antes de que se puede crear un instancia de ``Applicative``.
Aunque la clase ``Monad`` debería tener la misma restricción con
``Applicative``, ya que todas las mónadas son también funtores aplicativos, no
la tiene. Esto se debe a que la clase de tipos ``Monad`` se introdujo en
Haskell antes que ``Applicative``.

Incluso aunque toda mónada es también un funtor, no tenemos que depender de
la instancia de ``Functor`` gracias a la existencia de la función ``liftM``.
``liftM`` toma una función y un valor monádico y mapea la función sobre el
valor monádico. Vamos, ¡igual que ``fmap``! Esta es su declaración de tipo: ::

    liftM :: (Monad m) => (a -> b) -> m a -> m b

Y esta es la de ``map``: ::

    fmap :: (Functor f) => (a -> b) -> f a -> f b

Si tanto la instancia de ``Functor`` como la instancia de ``Monad`` obedecen
las leyes de los funtores y de las mónadas, estas dos funciones hacen lo mismo
(todas las mónadas que hemos visto cumplen ambas). Es lo mismo que pasaba con
``pure`` y ``return``, solo que una tiene la restricción de clase
``Applicative`` y otra la de ``Monad``. Vamos a probar ``liftM``.

.. code-block:: console

    ghci> liftM (*3) (Just 8)
    Just 24
    ghci> fmap (*3) (Just 8)
    Just 24
    ghci> runWriter $ liftM not $ Writer (True, "chickpeas")
    (False,"chickpeas")
    ghci> runWriter $ fmap not $ Writer (True, "chickpeas")
    (False,"chickpeas")
    ghci> runState (liftM (+100) pop) [1,2,3,4]
    (101,[2,3,4])
    ghci> runState (fmap (+100) pop) [1,2,3,4]
    (101,[2,3,4])

Ya conocemos bastante bien como funciona ``fmap`` con los valores el tipo
``Maybe``. ``liftM`` hace lo mismo. Para los valores del tipo ``Writer``, la
función se mapea sobre el primer componente de la dupla, que es el resultado.
Hacer ``fmap`` o ``liftM`` sobre un cómputo con estado devuelve un nuevo
cómputo con estado, solo que su resultado final se vera modificado por la
función. Si no hubiésemos mapeado ``(+100)`` sobre ``pop``, el resulto
hubiese sido ``(1,[2,3,4])``.

Esta es la implementación de ``liftM``: ::

    liftM :: (Monad m) => (a -> b) -> m a -> m b  
    liftM f m = m >>= (\x -> return (f x))

O con notación ``do``: ::

    liftM :: (Monad m) => (a -> b) -> m a -> m b  
    liftM f m = do  
        x <- m  
        return (f x)

Pasamos el valor monádico ``m`` a la función y luego aplicamos la función
``f`` a su resultado, introduciendo el resultado de ésta en el contexto por
defecto. Gracias a las leyes de las mónadas, tenemos garantizado que el
contexto se mantendrá constante, solo se modificará el resultado del valor
monádico. Podemos ver que ``liftM`` está implementado sin hacer referencia a
``Functor``. Esto significa que podemos implementar ``fmap`` (o ``liftM``,
depende de ti) utilizando únicamente lo que nos ofrecen las mónadas. Por este
motivo, podemos concluir que las mónadas son más potentes que los funtores
normales.

La clase de tipos ``Applicative`` nos permite aplicar funciones entre valores
con un contexto como si se trataran de funciones normales. 

.. code-block:: console

    ghci> (+) <$> Just 3 <*> Just 5  
    Just 8  
    ghci> (+) <$> Just 3 <*> Nothing  
    Nothing

Utilizar el estilo aplicativo hace las cosas muy fáciles. ``<$>`` es ``fmap``
y ``<*>`` es una función de la clase de tipos ``Applicative`` que tiene el
siguiente tipo: ::

    (<*>) :: (Applicative f) => f (a -> b) -> f a -> f b  

Es parecida a ``fmap``, solo que la función en si misma posee un contexto.
Tenemos que extraer de alguna forma el resultado de ``f a`` para poder mapear
la función sobre ella y luego volver a introducir el resultado en un contexto.
Como todas las funciones de Haskell están currificadas por defecto, podemos
utilizar la combinación de ``<$>`` y ``<*>`` para aplicar una función sobre
varios parámetros.

De cualquier forma, resulta que al igual que ``fmap``, ``<*>`` también puede
ser implementado utilizando lo que nos ofrece la clase de tipos ``Monad``. La
función ``ap`` es básicamente ``<*>``, solo que posee un restricción de clase
``Monad`` en lugar de ``Applicative``. Aquí tienes la definición: ::

    ap :: (Monad m) => m (a -> b) -> m a -> m b  
    ap mf m = do  
        f <- mf  
        x <- m  
        return (f x)

``mf`` es un valor monádico cuyo resultado es una función. Como tanto la
función como el valor están dentro de un contexto, extraemos la función del
contexto y la llamamos ``f``. Luego extraemos el valor y lo llamamos ``x``.
Para terminar aplicamos la función sobre el valor y devolvemos el resultado.

.. code-block:: console

    ghci> Just (+3) <*> Just 4  
    Just 7  
    ghci> Just (+3) `ap` Just 4  
    Just 7  
    ghci> [(+1),(+2),(+3)] <*> [10,11]  
    [11,12,12,13,13,14]  
    ghci> [(+1),(+2),(+3)] `ap` [10,11]  
    [11,12,12,13,13,14]

Ahora podemos ver que las mónadas son también más potentes que los funtores
aplicativos, porque podemos utilizar las funciones de ``Monad`` para
implementar las de ``Applicative``. De hecho, a menudo la gente cuando sabe
que un tipo es un mónada, primero implementa la instancia de ``Monad`` y luego
crea la instancia de ``Applicative`` simplemente diciendo que ``pure`` es
``return`` y ``<*>`` es ``ap``. De forma similar, si sabemos que algo tiene
una instancia de ``Monad``, podemos crear la instancia de ``Functor``
simplemente estableciendo que ``fmap`` es igual a ``liftM``.

La función ``liftA2`` es una función de conveniencia para aplicar una función
entre dos valores aplicativos. Su definición es así de sencilla: ::

    liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c  
    liftA2 f x y = f <$> x <*> y

La función ``liftM2`` hace exactamente lo mismo, solo que posee una
restricción de clase ``Monad``. También existe ``liftM3``, ``liftM4`` y
``liftM5``.

Hemos visto como las mónadas son más potentes que los funtores aplicativos y
que los funtores normales y aunque todas las mónadas son también funtores
y funtores aplicativos, no necesariamente poseen una instancia de ``Functor``
y ``Applicative``, y esta es la  razón por la que acabamos de ver las
funciones equivalentes entre los funtores y las mónadas.


La función ``join``
'''''''''''''''''''

Piensa en esto: si el resultado de un valor monádico es otro valor monádico,
es decir, si un valor monádico es anidado dentro de otro, ¿Podemos convertir
ambos en un único valor monádico? Por ejemplo, si tenemos ``Just (Just 9)``,
¿Podemos convertirlo en ``Just 9``? Pues resulta que convertir valores
monádicos anidados en valores monádicos simples es una de las propiedades 
únicas de las mónadas. Por este motivo tiene su razón de ser la función
``join``. ::

    join :: (Monad m) => m (m a) -> m a  

Toma una un valor monádico que contiene otro valor monádico y devuelve un solo
valor monádico. Aquí tienes un ejemplo de su uso con valores ``Maybe``:

.. code-block:: console

    ghci> join (Just (Just 9))  
    Just 9  
    ghci> join (Just Nothing)  
    Nothing  
    ghci> join Nothing  
    Nothing

La primera línea tiene un cómputo correcto como resultado de otro cómputo
correcto, así que ambos son unido en un solo cómputo correcto. La segunda
línea posee un ``Nothing`` como resultado de un valor ``Just``. Antes, cuando
trabajamos con valores ``Maybe`` queríamos combinar varios valores en uno, ya
sea con ``<*>`` o con ``>>=``, todos tenían que ser valores ``Just`` para que
el resultado fuese también un valor ``Just``. Si existe un fallo en algún
punto del camino, el resultado final será un fallo y lo mismo ocurre aquí. En
la tercera línea, vemos que si intentamos unir un único fallo, el resultado
es también un fallo.

Unir, o aplanar listas es bastante intuitivo:

.. code-block:: console

    ghci> join [[1,2,3],[4,5,6]]  
    [1,2,3,4,5,6]  
    
Como puedes ver, para listas ``join`` es igual que ``concat``. Para unir un
valor ``Writer`` cuyo resultado es también un valor ``Writer`` tenemos que
aplicar ``mappend`` al valor monádico.

.. code-block:: console

    ghci> runWriter $ join (Writer (Writer (1,"aaa"),"bbb"))  
    (1,"bbbaaa")

El valor monádico exterior ``"bbb"`` se utiliza primero y luego se le añade
``"aaa"``. Dicho de otra forma, cuando queremos examinar el resultado de un
valor ``Writer``, primero tenemos que actualizar su registro y solo después de
esto podremos examinar sus contenidos.

Unir valores ``Either`` es muy parecido a unir valores ``Maybe``:

.. code-block:: console

    ghci> join (Right (Right 9)) :: Either String Int  
    Right 9  
    ghci> join (Right (Left "error")) :: Either String Int  
    Left "error"  
    ghci> join (Left "error") :: Either String Int  
    Left "error"

Si aplicamos ``join`` a un cómputo cuyo resultado sea otro cómputo con
estado, el resultado será un cómputo con estado que primero ejecutará el
cómputo exterior y luego el interior. Mira:

.. code-block:: console

    ghci> runState (join (State $ \s -> (push 10,1:2:s))) [0,0,0]  
    ((),[10,1,2,0,0,0])

Aquí la función lambda toma un estado y apila ``2`` y ``1`` sobre la pila
y devuelve como resultado ``push 10``, que es otro computo con estado. Así que
cuando todo esto se une con ``join`` y luego se ejecuta, primero se apila
``2`` y ``1`` y luego se ejecuta ``push 10``, poniendo así ``10`` en la cima
de la pila.

La implementación de ``join`` es la siguiente: ::

    join :: (Monad m) => m (m a) -> m a  
    join mm = do  
        m <- mm  
        m

Como el resultado de ``mm`` es un valor monádico, obtenemos ese resultado y
luego simplemente lo ponemos solo en la última línea porque por si solo ya es
un valor monádico. El truco aquí esta en ``m <- mm``, el contexto de la mónada
de la que estamos obteniendo un resultado se tiene en cuenta. Por este motivo,
por ejemplo, los valores ``Maybe`` solo devuelven ``Just`` cuando tanto el
valor exterior como el valor interior son ambos ``Just``. Así se vería esto
si ``mm`` fuera desde el principio ``Just (Just 8)``: ::

    joinedMaybes :: Maybe Int  
    joinedMaybes = do  
        m <- Just (Just 8)  
        m  

.. image:: /images/tipi.png
   :align: right
   :alt: ¡Incluso yo soy un ajente de la ley!
   
Quizá lo más interesante de ``join`` es que funciona para cualquier mónada,
pasar un valor monádico a una función con ``>>=`` es lo mismo que mapear esa
función sobre el valor monádico y luego utilizar ``join`` para unir el
resultado. Dicho de otro modo, ``m >>= f`` es siempre igual que
``join (fmap f m)``. Vale la pena que le dediques un poco de atención. Con
``>>=``, siempre pensamos que estamos pasando un valor monádico a una función
que toma un valor normal pero devuelve un valor monádico. Si mapeamos
directamente la función sobre el valor monádico, tendremos un valor como
resultado un valor monádico dentro de otro valor monádico. Por ejemplo,
digamos que tenemos ``Just 9`` y la función ``\x -> Just (x+1)``. Si mapeamos
esta función sobre ``Just 9`` nos dará como resultado ``Just (Just 10)``.

El hecho de que ``m >>= f`` sea siempre igual a ``join (fmap f m)`` es muy
útil porque si estamos creando una instancia de ``Monad`` para un tipo,
siempre es más fácil averiguar como se deben unir dos valores monádicos
anidados que averiguar como implementar ``>>=``.


filterM
'''''''

La función ``filter`` junto a la función ``map`` son de las funciones más
utilizadas en Haskell. Toma un predicado y una lista y la filtra de forma que
la lista resultante solo contenga los resultados que satisfagan el predicado.
::

    filter :: (a -> Bool) -> [a] -> [a] 

El predicado toma un elemento de la lista y devuelve un valor ``Bool``. Pero,
¿y si el valor ``Bool`` que devuelve el predicado fuera en realidad un valor
monádico? ¿¡Qué!? En otras palabras, ¿y si el resultado tuviera un contexto?
¿Podría funcionar? Por ejemplo, ¿qué pasaría ti todos los valores ``True`` y
``False`` que el predicado produce vienen acompañados de un valor monádico
como ``["Número 5 aceptado"]`` o ``["3 es muy pequeño"]``? Dicho así podría
funcionar. Si ese fuera el caso, cabe esperar que la lista resultante venga
con un registro que contenga el registro de todos los valores que se han
ido produciendo. Así que si el valor ``Bool`` que produce el resultado viene
con un contexto, lo normal es que la lista resultante también venga con
un contexto, de otro modo el contexto de cada ``Bool`` se perdería.

La función ``filterM`` de ``Control.Monad`` hace exactamente lo que estamos
buscando. ::

    filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]  

El predicado devuelve un valor monádico cuyo resultado es un ``Bool``, pero
como es un valor monádico, su contexto puede ser cualquier cosa, desde un
fallo hasta un no determinismo. Para asegurarnos de que el resultado final
refleja el contexto, el resultado también debe ser un valor monádico.

Vamos a tomar una lista y vamos a filtrarla de forma que solo nos quedemos con
los números que sean menores que 4. Para empezar, vamos a utilizar la función
normal ``filter``:

.. code-block:: console

    ghci> filter (\x -> x < 4) [9,1,5,2,10,3]  
    [1,2,3]

Muy fácil. Ahora, vamos hacer que este predicado, además de devolver
``True`` o ``False``, también adjunte un registro indicando lo que ha hecho.
Por supuesto vamos a utilizar la mónada ``Writer``. ::

    keepSmall :: Int -> Writer [String] Bool  
    keepSmall x  
        | x < 4 = do  
            tell ["Keeping " ++ show x]  
            return True  
        | otherwise = do  
            tell [show x ++ " is too large, throwing it away"]  
            return False

En lugar de devolver un ``Bool``, esta función devuelve un
``Writer [String] Bool``. Es un predicado monádico. Suena excesivo, ¿no crees?
Si el número es menor que ``4`` registramos que lo vamos a mantener y
luego hacemos ``return True``.

Ahora vamos a utilizar ``filterM`` con una lista. Como el predicado devuelve
un valor ``Writer``, el resultado de la lista será también un valor
``Writer``.

.. code-block:: console

    ghci> fst $ runWriter $ filterM keepSmall [9,1,5,2,10,3]  
    [1,2,3]

Examinando el resultado del valor de tipo ``Writer`` vemos que todo está en
orden. Ahora, vamos a mostrar el registro:

.. code-block:: console

    ghci> mapM_ putStrLn $ snd $ runWriter $ filterM keepSmall [9,1,5,2,10,3]  
    9 is too large, throwing it away  
    Keeping 1  
    5 is too large, throwing it away  
    Keeping 2  
    10 is too large, throwing it away  
    Keeping 3

Increíble. Simplemente utilizando un predicado monádico con ``filterM`` somos
capaces de filtrar una lista a la vez que mantenemos el contexto del que
estamos utilizando.

Existe un truco en Haskell para obtener el superconjunto de una lista (si
vemos las listas como un conjunto). El superconjunto de un conjunto es un
conjunto de todos los posible subconjuntos de éste. Así que si tenemos un
conjunto como ``[1,2,3]``, su superconjunto incluirá los siguientes
conjuntos: ::

    [1,2,3]  
    [1,2]  
    [1,3]  
    [1]  
    [2,3]  
    [2]  
    [3]  
    []

En otras palabras, obtener el superconjunto es como obtener todas las posibles
combinaciones de mantener o eliminar elementos de un conjunto. ``[2,3]`` sería
el conjunto original al que hemos eliminado el número ``1``.

Para crear una función que devuelva el superconjunto de una lista vamos a
utilizar el no determinismo. Tomamos una lista como ``[1,2,3]`` y luego
miramos el primer elemento, que es ``1``, y nos preguntamos: ¿lo debemos
mantener o lo debemos eliminar? Bueno, en realidad queremos hacer ambas cosas.
Resumiendo, vamos a filtrar una lista y vamos a un utilizar un predicado no
determinista que elimine y mantenga cada elemento de la lista. ::

    powerset :: [a] -> [[a]]  
    powerset xs = filterM (\x -> [True, False]) xs  
    
¿Qué es esto? Bueno, elegimos eliminar y mantener cada elemento,
independientemente del valor de dicho elemento. Tenemos un predicado no
determinista, así que el resultado también será no determinista y por lo tanto
su tipo será una lista de listas. Vamos a probarlo.

.. code-block:: console

    ghci> powerset [1,2,3]  
    [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]

Quizá esto puede que no se entienda a la primera, pero si consideramos las
listas como valores no deterministas que no saben que valor escoger y por
tanto deciden ser todos a la vez, es más fácil de ver.


``foldM``
'''''''''

La versión monádica de ``foldl`` es ``foldM``. Si recuerdas bien los
:ref:`pliegues <pliegues>`, sabrás que ``foldl`` toma una función binaria,
un acumulador inicial y una lista y pliega la lista desde la izquierda
reduciendo la lista a un solo valor. ``foldM`` hace básicamente lo mismo, solo
que toma una función binaria que devuelve un valor monádico.
Sorprendentemente, el resultado final también es un valor monádico. La
declaración de tipo de ``foldl`` es: ::

    foldl :: (a -> b -> a) -> a -> [b] -> a

Mientras que la de ``foldM`` es: ::

    foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a

El valor que devuelve la función binaria es un valor monádico por lo tanto
el valor final del pliegue también lo es. Vamos a sumar una lista de números
con un pliegue:

.. code-block:: console

    ghci> foldl (\acc x -> acc + x) 0 [2,8,3,1]  
    14

El acumulador inicial es ``0`` y luego se suma ``2`` al acumulador, el
resultado pasa a ser el nuevo acumulador que tiene un valor de ``2``. Luego
se suma ``8`` al acumulador devolviendo así ``10`` que pasa a ser el nuevo
acumulador y así hasta que alcance el final de la lista, donde el acumulador
final será el resultado final de la función.

¿Y si queremos sumar una lista de números y además queremos añadir la
condición de que si en la lista existe un número mayor a ``9``, todo el
cómputo falle? Tendría sentido utilizar la función binaria para comprobar si
el número actual es mayor que ``9`` y si lo es, falle, y si no continúe.
Debido a esta nueva posibilidad de fallo, vamos a hacer que la función binaria
devuelva un acumulador dentro de un tipo ``Maybe`` en lugar de un acumulador
normal. Así sería la función binaria: ::

    binSmalls :: Int -> Int -> Maybe Int  
    binSmalls acc x  
        | x > 9     = Nothing  
        | otherwise = Just (acc + x)

Como la función binaria es ahora una función monádica, ya no podemos utilizar
un pliegue normal como ``foldl``, tendremos que usar un pliegue monádico.

.. code-block:: console

    ghci> foldM binSmalls 0 [2,8,3,1]  
    Just 14  
    ghci> foldM binSmalls 0 [2,11,3,1]  
    Nothing  
    
¡Genial! Como había un número mayor que ``9``, el resultado final fue
``Nothing``. También es útil realizar un pliegue con una función binaria que
devuelva un valor ``Writer``, ya que de este modo podemos obtener un registro
conforme recorremos la lista.


Creando una versión segura de la calculadora RPN
''''''''''''''''''''''''''''''''''''''''''''''''

.. image:: /images/miner.png
   :align: left
   :alt: ¡Encontré oro!

Cuando estábamos solucionando el problema de implementar la
:ref:`calculadora RPN <rpn>`, vimos que funcionaría bien siempre y cuando la
entrada tuviera sentido. Pero si algo iba mal, el programa entero terminaba.
Ahora que ya sabemos como convertir código en su versión monádica, vamos a
hacer una versión de la calculador RPN más segura ayudándonos de la mónada
``Maybe``.

Implementamos la calculadora RPN de forma que tomaba una cadena, como
``"1 3 + 2 *"``, la dividiera en palabras para obtener algo como
``["1","3","+","2","*"]`` y luego la plegara utilizando como acumulador
inicial una pila vacía y una función binaria que apilaba números en la pila,
o sumaba los dos elementos superiores, o los dividía, etc.

Este era la función principal: ::

    import Data.List  

    solveRPN :: String -> Double  
    solveRPN = head . foldl foldingFunction [] . words

Convertíamos la expresión en una lista de cadenas, la plegábamos utilizando
una función binaria y luego devolvíamos el único elemento que quedaba en la
pila. Así era la función binaria: ::

    foldingFunction :: [Double] -> String -> [Double]  
    foldingFunction (x:y:ys) "*" = (x * y):ys  
    foldingFunction (x:y:ys) "+" = (x + y):ys  
    foldingFunction (x:y:ys) "-" = (y - x):ys  
    foldingFunction xs numberString = read numberString:xs

En este caso el acumulador del pliegue era la pila, la cual representábamos
como una lista de valores ``Double``. Conforme la función de pliegue avanzaba
por la expresión RPN, si el elemento actual era un operador, obteníamos los
dos elementos superiores de la pila, aplicábamos el operador y luego
apilábamos el resultado de nuevo. Si el elemento actual era una cadena que
representaba un número, convertíamos la cadena en un número real y lo
apilábamos.

Primero vamos a hacer que esta función pueda fallar de forma correcta. Su
declaración de tipo cambiará de esta forma: ::

    foldingFunction :: [Double] -> String -> Maybe [Double]  

Así que ahora su resultado será o bien un valor ``Just`` con una pila o bien
fallará con ``Nothing``.

La función ``reads`` es igual que ``read``, solo que devuelve una lista con
un único elemento en caso de que tenga éxito. Si no puede leer algo, devuelve
una lista vacía. Aparte de devolver el valor que lee, también devuelve la
parte de la cadena que no ha consumido. Vamos a decir que siempre tiene que
leer toda la cadena para que funcione correctamente y vamos a crear una
función ``readMaybe`` por conveniencia. ::

    readMaybe :: (Read a) => String -> Maybe a  
    readMaybe st = case reads st of [(x,"")] -> Just x  
                                    _ -> Nothing

La probamos: 

.. code-block:: console

    ghci> readMaybe "1" :: Maybe Int  
    Just 1  
    ghci> readMaybe "GO TO HELL" :: Maybe Int  
    Nothing

Vale, parece que funciona. Ahora vamos a convertir la función binaria en
una función binaria que puede fallar. ::

    foldingFunction :: [Double] -> String -> Maybe [Double]  
    foldingFunction (x:y:ys) "*" = return ((x * y):ys)  
    foldingFunction (x:y:ys) "+" = return ((x + y):ys)  
    foldingFunction (x:y:ys) "-" = return ((y - x):ys)  
    foldingFunction xs numberString = liftM (:xs) (readMaybe numberString)
    foldingFunction _ _ = fail "¡whops!"

Los primeros tres casos son iguales que los originales, solo que ahora la
pila nueva se introduce en ``Just`` (hemos utilizado ``return`` para lograrlo,
pero podíamos haber utilizado simplemente ``Just`` también). En el cuarto
caso, hacemos ``readMaybe numberString`` y luego mapeamos ``(:xs)`` sobre su
resultado. Es decir, si la pila ``xs`` es ``[1.0,2.0]`` y
``readMaybe numberString`` devuelve ``Just 3.0``, el resultado será
``[1.0,2.0,3.0]``. Si ``readMaybe numberString`` devuelve ``Nothing`` el
resultado final será ``Nothing``. Vamos a probar esta función:

.. code-block:: console

    ghci> foldingFunction [3,2] "*"  
    Just [6.0]  
    ghci> foldingFunction [3,2] "-"  
    Just [-1.0]  
    ghci> foldingFunction [] "*"  
    Nothing  
    ghci> foldingFunction [] "1"  
    Just [1.0]  
    ghci> foldingFunction [] "1 wawawawa"  
    Nothing

¡Parece que funciona! Ahora es hora de mejorar la función ``solveRPN`` ¡Aquí
la tienen! ::

    import Data.List  

    solveRPN :: String -> Maybe Double  
    solveRPN st = do  
        [result] <- foldM foldingFunction [] (words st)  
        return result

Al igual que antes, tomamos una cadena y la dividimos en palabras. Luego,
realizamos el pliegue, empezando con una pila vacía, solo que en lugar de
realizar un pliegue normal con ``foldl`` utilizamos ``foldM``. El resultado
de ``foldM`` debe ser un valor del tipo ``Maybe`` que contendrá una lista
(es decir, la pila final) que a su vez solo debería contener un valor.
Utilizamos una expresión ``do`` para obtener el valor y lo llamamos
``result``. En caso de que ``foldM`` devuelva ``Nothing``, el resultado final
será ``Nothing``, porque así es como funciona la mónada ``Maybe``. Fíjate
también en el ajuste del patrón en el interior de la expresión ``do``, de esta
forma si la lista tiene más de un solo o ningún elemento, el ajuste fallará
y se producirá un ``Nothing``. En la última línea simplemente hacemos
``return result`` para devolver el resultado de la expresión RPN dentro de un
valor del tipo ``Maybe``.

Probémoslo:

.. code-block:: console

    ghci> solveRPN "1 2 * 4 +"  
    Just 6.0  
    ghci> solveRPN "1 2 * 4 + 5 *"  
    Just 30.0  
    ghci> solveRPN "1 2 * 4"  
    Nothing  
    ghci> solveRPN "1 8 wharglbllargh"  
    Nothing

El primer fallo sucede porque la pila final no contiene un único elemento y
por tanto el ajuste de patrones contenido en la expresión ``do`` falla. El
segundo fallo se debe a que ``readMaybe`` devuelve ``Nothing``.


Componiendo funciones monádicas
'''''''''''''''''''''''''''''''

Cuando hablamos de las leyes de las mónadas, vimos que la función ``<=<`` era
parecida a la composición de funciones, solo que en lugar de tratar con
funciones normales ``a -> b``, funcionaba con funciones monádicas como
``a -> m b``. Por ejemplo: 

.. code-block:: console

    ghci> let f = (+1) . (*100)  
    ghci> f 4  
    401  
    ghci> let g = (\x -> return (x+1)) <=< (\x -> return (x*100))  
    ghci> Just 4 >>= g  
    Just 401

En este ejemplo primero componemos dos funciones normales, y luego las
aplicamos la función resultante a ``4``. Luego componemos dos funciones
monádicas, y luego le pasamos ``Just 4`` a la función resultante utilizando
``>>=``.

Si tenemos una lista de funciones, podemos componerlas en una sola gran
función utilizando ``id`` como acumulador inicial y la función ``.`` como
función binaria. O también utilizando la función ``foldr1``. Aquí tienes un
ejemplo: 

.. code-block:: console

    ghci> let f = foldr (.) id [(+1),(*100),(+1)]  
    ghci> let g = foldr1 (.) [(+1),(*100),(+1)]  
    ghci> f 1  
    201
    ghci> g 1  
    201    

La función ``f`` toma un número y luego le suma ``1``, luego multiplica el
resultado por ``100`` y luego le suma ``1`` al resultado anterior. De
todos modos, podemos componer funciones monádicas de la misma forma, solo que
en lugar de utilizar una composición de funciones normal utilizamos ``<=<``, y
en lugar de utilizar ``id`` utilizamos ``return``. No tenemos que utilizar
``foldM`` en lugar de ``foldr`` ya que ``<=<`` se encarga de que la
composición sea monádica.

Cuando vimos la mónada lista en el :ref:`capítulo anterior <mlista>`, la
utilizamos para encontrar a qué posiciones podía desplazarse un caballo en
un tablero de ajedrez con exactamente tres movimientos. Teníamos una función
que se llamaba ``moveKnight``, la cual tomaba la posición en el tablero del
caballo y devolvía todos los posibles movimientos que podía tomar. Luego,
para generar todos los posibles posiciones que podía alcanzar en tres
movimientos utilizábamos una función como estas: ::

    in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight   

Y para comprobar si el caballo podía llegar desde ``start`` hasta ``end`` en
tres movimientos utilizábamos: ::

    canReachIn3 :: KnightPos -> KnightPos -> Bool  
    canReachIn3 start end = end `elem` in3 start

Utilizando la composición de funciones podemos crear una función como ``in3``,
solo que en lugar de generar todas las posibles soluciones que puede alcanzar
el caballo en tres movimientos, podemos hacerlo para un número arbitrario de
movimientos. Si nos fijamos en ``in3``, vemos que hemos utilizado
``moveKnight`` tres veces y hemos utilizado ``>>=`` en cada paso para pasar
las posibles posiciones anteriores. Ahora vamos a hacerlo más general. ::

    import Data.List  

    inMany :: Int -> KnightPos -> [KnightPos]  
    inMany x start = return start >>= foldr (<=<) return (replicate x moveKnight)
    
Primero utilizamos ``replicate`` para crear una lista que contenga ``x`` 
veces la función ``moveKnight``. Luego, componemos monádicamente todas esas
funciones en una, lo cual resulta en una función que toma una posición incial
y mueve el caballo de forma no determinista ``x`` veces. Luego, simplemente
creamos una lista unitaria con la posición inicial con ``return`` y se la
pasamos a la función.

Ahora también podemos cambiar la función ``canReachIn3`` para que sea más
general: ::

    canReachIn :: Int -> KnightPos -> KnightPos -> Bool  
    canReachIn x start end = end `elem` inMany x start


Creando mónadas
---------------


.. image:: /images/spearhead.png
   :align: center
   :alt: kewl
   
En esta sección vamos a ver un ejemplo de como se crea un tipo, como se
identifica que se trata de una mónada y luego como darle una instancia
``Monad`` apropiada. Normalmente no nos ponemos a crear una mónada por el
simple hecho de crear mónadas. En lugar de ello, solemos crear un tipo con el
propósito de modelar un aspecto de algún problema y luego si vemos que ese
tipo representa valores con un contexto y puede comportarse como una mónada,
le damos una instancia de ``Monad``.

Como ya hemos visto, las listas se utilizan para representar valores 
no deterministas. Una lista como ``[3,5,9]`` puede ser vista como un solo
valor no determinista que no puede decidir que valor ser. Cuando pasamos una
lista a una función con ``>>=``, simplemente crea todas las posibilidades
de tomar un elemento concreto de la lista y le aplica la función, de forma que
los resultados que produzca aparezcan en el resultado final.

Si vemos la lista ``[3,5,9]`` como los número ``3``, ``5`` y ``9`` al mismo
tiempo, podemos darnos cuenta de que no tenemos ninguna información de la
probabilidad de que esos números aparezcan. ¿y si quisiéramos un modelar un
valor no determinista como ``[3,5,9]``, pero que expresará también que ``3``
tiene un 50% probabilidades de ocurrir y ``5`` y ``9`` tienen un 25% de
probabilidades? Es una pregunta muy larga, lo se, pero vamos a buscar una
respuesta.

Digamos que cada elemento de la lista va acompañado de otro valor, la
probabilidad de que ocurra. Podría tener sentido representarlo así:

.. code-block:: console

    [(3,0.5),(5,0.25),(9,0.25)]  

En las matemáticas, las probabilidades no se suelen representar con
porcentajes sino con valores reales que van desde el 0 hasta el 1. Un 0 
significa que no hay ninguna posibilidad de que ocurra un suceso mientras que
un 1 representa que el suceso va ocurrir sí o sí. Los número en coma flotante
pueden ser muy eficientes ya que tienden a perder precisión, así que Haskell
nos ofrece un tipo de dato para los números racionales de forma que no pierda
precisión. Este tipo se llama ``Rational`` y reside en el módulo
``Data.Ratio``. Para crear un número del tipo ``Rational`` lo escribimos en
forma de fracción. Separamos el numerador y el denominador por ``%``. Aquí
tienes unos ejemplos:

.. code-block:: console

    ghci> 1%4  
    1 % 4  
    ghci> 1%2 + 1%2  
    1 % 1  
    ghci> 1%3 + 5%4  
    19 % 12

La primera línea representa un cuarto. En la segunda línea sumamos dos
medios y obtenemos la unidad y en la tercera línea sumamos un tercero a
cinco cuartos y obtenemos diecinueve docenas de huevos. Vamos a utilizar
número ``Rational`` en lugar de números en coma flotante para representar las
probabilidades.

.. code-block:: console

    ghci> [(3,1%2),(5,1%4),(9,1%4)]  
    [(3,1 % 2),(5,1 % 4),(9,1 % 4)]

Vale, ``3`` tiene la mitad de posibilidades de ocurrir y ``5`` y ``9`` tienen
un cuarto de posibilidades de salir. Muy bonito.

Tomamos las listas y les añadimos un contexto adicional, así que esto también
representa valores en cierto contexto. Antes de continuar, vamos representar
esto con ``newtype`` porque algo me dice que vamos a crear alguna instancias.
::

    import Data.Ratio  

    newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show

Muy bien ¿Esto es un funtor? Bueno, la lista es un funtor, así que
probablemente esto sea un funtor también ya que solo hemos añadido algo más
de contexto. Cuando mapeamos una función sobre una lista, la aplicamos a todos
los elementos. Ahora también aplicaremos la función a todos los elementos,
solo que mantendremos las probabilidades intactas. ::

    instance Functor Prob where  
        fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p)) xs

Extraemos el valor del ``newtype`` utilizando un ajuste de patrones, aplicamos
la función ``f`` a los valores mientras nos aseguramos de mantener constantes
las probabilidades. Vamos a ver si funciona:

.. code-block:: console

    ghci> fmap negate (Prob [(3,1%2),(5,1%4),(9,1%4)])  
    Prob {getProb = [(-3,1 % 2),(-5,1 % 4),(-9,1 % 4)]}

Otra cosa que tenemos que tener en cuenta es que todas estas probabilidades
deben sumar ``1``. Si todo esto son cosas que pueden ocurrir, no tiene sentido
que la suma de sus probabilidades sea distinta de ``1``. Una moneda que al
lanzarla salga un 75% de las veces cruz y un 50% de las veces caras es algo
que solo podría en otro extraño universo.

Ahora, la gran pregunta, ¿es una mónada? Dado que las listas son mónadas,
parece que esto puede también puede ser una mónada. Primero vamos a pensar
en ``return`` ¿Cómo funciona con las listas? Toma un valor y crea una lista
unitaria que lo contenga ¿Qué sucederá aquí? Bueno, como se supone que es el
contexto mínimo por defecto, también creará una lista unitaria ¿Qué hay de la
probabilidad? Bueno, ``return x`` se supone que siempre crea un valor monádico
capaz de albergar ``x`` como resultado, así que no tiene sentido que su
probabilidad sea ``0``. Como siempre devuelve el mismo resultado, su
probabilidad debe ser ``1``.

¿Qué pasa con ``>>=``? Parece algo complicado, así que vamos a utilizar el
hecho de que ``m >>= f`` siempre sea igual a ``join (fmap f m)`` para todas
las mónadas y centrarnos en como unir una lista de probabilidades que contiene
listas de probabilidades. Como ejemplo vamos a considerar una lista donde
existe un 25% de probabilidades de que ocurra ``'a'`` o ``'b'``. Tanto ``'a'``
como ``'b'`` tienen la misma probabilidad de ocurrir. También existe un 75% de
probabilidades de que salga ``'c'`` o ``'d'``. Tanto ``'c'`` como ``'d'``
tienen la misma probabilidad de ocurrir. Aquí tienes una imagen que representa
este posible escenario.

.. image:: /images/prob.png
   :align: left
   :alt: Probabilidades

¿Cuáles son las probabilidades de cada uno de estos valores ocurra? Si
dibujamos todas estas probabilidades como cajas, cada una con una
probabilidad, ¿cuáles serían estas probabilidades? Para calcularlas, todo
lo que tenemos que hacer es multiplicar la probabilidad con la probabilidad
que lo contenga. ``'a'`` tendrá una probabilidad de un octavo, al igual que
``'b'``, ya que si multiplicamos un medio por un cuarto obtenemos un octavo.
``'c'`` ocurrirá tres octavos de las veces porque si multiplicamos un medio
por tres cuartos obtenemos tres octavos. ``'d'`` ocurrirá tres octavos. Si
sumamos todas las probabilidades dará uno como resultado.

Así se representaría la situación actual: ::

    thisSituation :: Prob (Prob Char)  
    thisSituation = Prob  
        [( Prob [('a',1%2),('b',1%2)] , 1%4 )  
        ,( Prob [('c',1%2),('d',1%2)] , 3%4)  
        ]

Fíjate que el tipo es ``Prob (Pron Char)``. Así que ahora que hemos averiguado
como unir varias listas de probabilidades anidadas, solo tenemos que escribir
el código y utilizarlo para implementar ``>>=`` como ``join (fmap f m)`` y
ya tendremos nuestra mónada lista. Aquí tenemos la función ``flattern``, tiene
este nombre porque alguien ya utilizo antes que nosotros el nombre ``join``:
::

    flatten :: Prob (Prob a) -> Prob a  
    flatten (Prob xs) = Prob $ concat $ map multAll xs  
        where multAll (Prob innerxs,p) = map (\(x,r) -> (x,p*r)) innerxs

La función ``multAll`` toma un dupla con una lista de probabilidades y una
probabilidad ``p`` y produce una nueva lista de probabilidades en las que
las probabilidades de todos los elementos han sido multiplicadas por ``p``.
Mapeamos ``multAll`` sobre cada dupla de la lista anidada y luego la aplanamos
con ``concat``. 

Ahora que ya tenemos todo lo que necesitamos podemos escribir la instancia de
``Monad``. ::

    instance Monad Prob where  
        return x = Prob [(x,1%1)]  
        m >>= f = flatten (fmap f m)  
        fail _ = Prob []

.. image:: /images/ride.png
   :align: right
   :alt: Cavalgando

Como ya hemos hecho todo el trabajo sucio, la instancia es muy simple. También
hemos definido la función ``fail``, que al igual que la instancia de la
listas, devuelve una lista vacía. Así que si ocurre un fallo en un ajuste de
patrones dentro de una expresión ``do``, ocurrirá un fallo en el contexto en
si mismo.

Es importante también comprobar si se cumple la leyes de las mónadas. La
primera ley dice que ``return x >>= f`` debe ser igual que ``f x``. Una prueba
rigurosa sería algo tediosa, pero podemos ver que si tomamos un valor y lo
introducimos en contexto mínimo por defecto con ``return`` y luego mapeamos
una función con ``fmap`` sobre este valor para después aplanar la lista de
probabilidades resultante, cada probabilidad que produzca la función será
multiplicada por ``1%1``, así que el contexto no se verá afectado. El
razonamiento por el cual ``m >>= return`` es igual a ``m`` es similar. La
tercera ley dice que ``f <=< (g <=< h)`` debe ser igual a ``(f <=< g) <=< h``.
Esta ley también se cumple ya que mantenemos la mónada lista como base de esta
nueva mónada y por que la multiplicación es asociativa. ``1%2 * (1%3 * 1%5)``
es igual que ``(1%2 * 1%3) * 1%5``.

Ahora que tenemos una mónada, ¿qué podemos hacer con ella? Bueno, nos puede
ayudar a hacer cálculos con probabilidades. Podemos tratar eventos
probabilísticos como valores con un cierto contexto y la mónada probabilidad
se encargará de que las probabilidades se reflejen en el resultado final.

Digamos que tenemos dos monedas normales y una moneda trucada que siempre
saca cruz nueve de cada diez veces. Si lanzamos todas estas monedas a la vez,
¿cuáles son las probabilidad de que todas ellas sean cruz? Primero, vamos
a crear unas listas de probabilidades para las monedas normales y para la
trucada: ::

    data Coin = Heads | Tails deriving (Show, Eq)  

    coin, loadedCoin :: Prob Coin  
    coin = Prob [(Heads,1%2),(Tails,1%2)]  

    loadedCoin = Prob [(Heads,1%10),(Tails,9%10)]

Luego creamos la acción de lanzar las monedas: ::

    import Data.List (all)  

    flipThree :: Prob Bool  
    flipThree = do  
        a <- coin  
        b <- coin  
        c <- loadedCoin  
        return (all (==Tails) [a,b,c])

Vemos que las probabilidades de que todas ellas sean cara no son muy buenas,
incluso aunque tengamos una moneda trucada.

.. code-block:: console

    ghci> getProb flipThree  
    [(False,1 % 40),(False,9 % 40),(False,1 % 40),(False,9 % 40),  
     (False,1 % 40),(False,9 % 40),(False,1 % 40),(True,9 % 40)]

Las tres serán cruz nueve veces de cuarenta lanzamientos, lo cual es menos
del 25%. Podemos ver que la mónada no sabe como unir todos los valores iguales
a ``False``, donde no todas las tres monedas fueron cruz. No es un gran 
problema, ya que podemos crear una función que tome elemento a elemento y
vaya sumando las probabilidades del mismo suceso. Ya tienes algo que hacer.

En esta sección hemos pasado de tener una pregunta (¿qué pasaría si añadimos
información sobre la probabilidad de un elemento?) a crear un tipo,
identificando una mónada y finalmente creando una instancia para trabajar con
ella. Creo que hemos hecho bastante. A esta alturas ya deberíamos tener una
buena idea que son las mónadas y como trabajar con ellas.
