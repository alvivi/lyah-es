

Un puñado de mónadas
====================


La primera vez que hablamos de los funtores, vimos que son un concepto útil
para los valores que se pueden mapear. Luego, llevamos ese concepto un poco
más lejos y vimos los funtores aplicativos, que nos permite ver los valores
de ciertos tipos de datos como una especia de valores con un contexto de forma
que podemos aplicar funciones normales sobre ellos mantenido dicho contexto.

En este capítulos hablaremos de las mónadas, que simplemente son una versión
ampliada de los funtores aplicativos, de la misma forma que los funtores
aplicativos son una versión ampliada de los funtores.

.. image:: /images/smugpig.png
   :align: right
   :alt: ¡Más chulo que tú!
   
Cuando hablamos de los funtores vimos que es era posible mapear funciones
sobre varios tipos de datos. Para lograrlo utilizábamos la clase de tipos
``Functor``. Dada una función del tipo ``a -> b`` y un dato del tipo ``f a``
nos preguntábamos cómo mapeamos esta función sobre el dato de forma que 
obtuviésemos un resulto con el tipo ``f b``. Vimos como mapear funciones sobre
datos del tipo ``Maybe a``, del tipo ``[a]``, ``IO a``, etc. Incluso vimos
como mapear funciones ``a -> b`` sobre funciones ``r -> a`` de forma
que el resultado daba funciones del tipo ``r -> b``. Para contestar a la
pregunta que nos hacíamos de como mapear una función sobre un dato, lo único
que tenemos que hacer es mirar el tipo de ``fmap``: ::

    fmap :: (Functor f) => (a -> b) -> f a -> f b  
    
Y luego hacer que funcione con el tipo de datos para el que estamos creando
la instancia de ``Functor``. 

Luego vimos que era posible mejorar los funtores. Decíamos: ¡Hey! ¿qué pasa si
tenemos una función ``a -> b`` dentro del valor de un funtor? Como por
ejemplo, ``Just (*3)``, y queremos aplicarla a ``Just 5``. ¿Qué pasaría si en
vez de aplicarla a ``Just 5`` la aplicamos a ``Nothing``? ¿O si tenemos 
``[(*2),(+4)]`` y queremos aplicarla a ``[1,2,3]``? ¿Cómo hacemos para que
funcione de forma general? Para ello utilizamos la clase de tipos
``Applicative``. ::

    (<*>) :: (Applicative f) => f (a -> b) -> f a -> f b  
    
También vimos que podíamos tomar un valor normal e introducirlo dentro de un
tipo de datos. Por ejemplo, podemos tomar un ``1`` en introducirlo en un
tipo ``Maybe`` de forma que resulte en ``Just 1``. O incluso podríamos crear
un ``[1]``. O una acción de E/S que no realizara nada y devolviera un ``1``.
La función que realiza esta acción es ``pure``.

Como ya dijimos, un valor aplicativo puede verse como un valor dentro de un
contexto. Un valor *adornado* en términos técnicos. Por ejemplo, el carácter
``'a'`` es un simple carácter normal, mientras que ``Just 'a'`` tiene añadido
un cierto contexto. En lugar de un ``Char`` tenemos un ``Maybe Char``, que nos
dice que su valor puede ser un carácter o bien la ausencia de un carácter.

También es genial ver como la clase de tipos ``Applicative`` nos permite
utilizar funciones normales con esos contextos de forma que los contextos
se mantengan. Observa:

.. code-block:: console

    ghci> (*) <$> Just 2 <*> Just 8  
    Just 16  
    ghci> (++) <$> Just "klingon" <*> Nothing  
    Nothing  
    ghci> (-) <$> [3,4] <*> [1,2,3]  
    [2,1,0,3,2,1]
    
Estupendo, ahora que los tratamos con valores aplicativos, los valores
``Maybe a`` representan cómputos que pueden fallar, ``[a]`` representan
cómputos que tienen varios resultados (cómputos no deterministas), ``IO a``
representan valores que tienen efectos secundarios, etc.

Las mónadas son una extensión natural de los funtores aplicativos y tratan
de resolver lo siguiente: si tenemos un valor en un cierto contexto, ``m a``,
¿cómo podemos aplicarle una función que toma un valor normal ``a`` y devuelve
un valor en un contexto? Es decir, ¿cómo podemos aplicarle una función del
tipo ``a -> m b``? Básicamente lo que queremos es esta función: ::

    (>>=) :: (Monad m) => m a -> (a -> m b) -> m b  
    
**Si tenemos un valor adornado y una función que toma un valor y devuelve un
valor adornado, ¿cómo pasamos el primer valor adornado a la función?**. Esta
será la pregunta principal que nos haremos cuando trabajemos con las mónadas.
Escribimos ``m a`` en lugar de ``f a`` ya que ``m`` representa mónadas, aunque
las mónadas no son más que funtores aplicativos que soportan la función
``>>=``. Llamamos *lazo* a la función ``>>=``. 

Cuando tenemos un valor normal ``a`` y una función normal ``a -> b`` es muy
fácil pasar ese valor a la función. Simplemente hay que aplicar la función
a ese valor de forma normal. Pero cuando estamos trabajando con valores que
vienen dentro de un cierto contexto, tenemos que tomarnos un tiempo para
ver como estos valores adornados se pasan a las funciones y para ver como
se comportan. No te preocupes, verás que es muy fácil.


Manos a la obra con Maybe
-------------------------


.. image:: /images/buddha.png
   :align: left
   :alt: Mónadas, saltamontes...
   
Ahora que ya tenemos una pequeña idea del cometido de la mónadas, vamos a
expandirla en detalle.

Para sorpresa de nadie, ``Maybe`` es una mónada, así que vamos a explorarlo un
poco más a ver si podemos combinar lo que ya sabemos con las mónadas.

.. note:: Llegados a este punto, asegurate de que entiendes los
          :ref:`funtores aplicativos <aplicativos>`. Será más fácil si sabes
          como funcionan varias instancias de ``Applicative`` y que tipo
          de cómputos representan, ya que las mónadas no son más que una
          expasión de los funtores aplicativos. 
          
Un valor de tipo ``Maybe a`` representa un valor del tipo ``a`` dentro del
contexto de que ocurra un posible fallo. Un valor ``Just "dharma"`` representa
que la cadena ``"dharma"`` está presente mientras que ``Nothing`` representa
su ausencia, o si vemos la cadena como el resultado de un cómputo, significará
que dicho cómputo ha fallado.

Cuando hablamos de ``Maybe`` como funtor vimos que cuando mapeamos una
función sobre él con ``fmap``, se mapea solo cuando es un valor ``Just``, de
otro modo ``Nothing`` se mantiene como resultado ya que no hay nada sobre lo
que mapear.

Como esto:

.. code-block:: console

    ghci> fmap (++"!") (Just "wisdom")  
    Just "wisdom!"  
    ghci> fmap (++"!") Nothing  
    Nothing
    
Como funtor aplicativo funciona de forma similar. Sin embargo, los funtores
aplicativos también poseen funciones dentro de los funtores. ``Maybe`` es un
funtor aplicativo de forma que cuando aplicamos ``<*>`` con una función
contenida en un ``Maybe`` a un valor contenido en un ``Maybe``, ambos deben
ser valores ``Just`` para que el resultado sea también un valor ``Just``, en
caso contrario el resultado será ``Nothing``. Tiene sentido ya que si no
tenemos o bien la función o bien el valor, no podemos crear un resultado a
partir de la nada, así que hay que propagar el fallo:

.. code-block:: console

    ghci> Just (+3) <*> Just 3  
    Just 6  
    ghci> Nothing <*> Just "greed"  
    Nothing  
    ghci> Just ord <*> Nothing  
    Nothing
    
Cuando utilizamos el estilo aplicativo con funciones normales para que actúen
con valores del tipo ``Maybe`` es similar. Todos los valores deben ser
``Just`` si queremos que el resultado también lo sea.

.. code-block:: console

    ghci> max <$> Just 3 <*> Just 6  
    Just 6  
    ghci> max <$> Just 3 <*> Nothing  
    Nothing
    
Y ahora vamos a ver como podríamos implementar ``>>=`` para ``Maybe``. Como ya
hemos dicho, ``>>=`` toma un valor monádico y una función que toma un valor
normal y devuelve otro valor monádico, de forma que aplica esta función al
valor monádico. ¿Cómo consigue hacerlo si la función solo toma valores
normales? Bueno, para lograrlo hay que tomar en cuenta el contexto de ese
valor monádico.

En este caso, ``>>=`` tomará un valor con el tipo ``Maybe a`` y una función de
tipo ``a -> ``Maybe b`` y de alguna forma aplicará esta función para dar como
resultado ``Maybe b``. Para imaginarnos como se hace, podemos apoyarnos en lo
que ya sabemos de los funtores aplicativos. Digamos que tenemos una función
del tipo ``\x -> Just (x+1)``. Toma un número, le añade ``1`` y lo introduce
en un ``Just``:

.. code-block:: console

    ghci> (\x -> Just (x+1)) 1  
    Just 2  
    ghci> (\x -> Just (x+1)) 100  
    Just 101
    
Si le pasaramos como parámetro ``1`` devolvería ``Just 2``. Si le pasaramos
``100`` devolvería ``Just 101``. Simple. Ahora viene lo bueno: ¿cómo pasamos
un dato del tipo ``Maybe`` a esta función? Si pensamos en ``Maybe`` como un
funtor aplicativo contestar a esta pregunta es bastante fácil. Si le pasamos
un valor ``Just``, toma el valor que contiene y le aplica la función. Si le
pasamos ``Nothing``, mmm, bueno, tenemos la función pero no tenemos nada que
pasarle. En este caso vamos a hacer lo mismo que hicimos anteriormente y
diremos que el resultado será ``Nothing``.

En lugar de llamar a esta función ``>>=``, vamos a llamarla ``applyMaybe`` por
ahora. Toma un ``Maybe a`` y una función que devuelve un ``Maybe b`` y se las
ingenia para aplicar esa función a ``Maybe a``. Aquí está la función: ::

    applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b  
    applyMaybe Nothing f  = Nothing  
    applyMaybe (Just x) f = f x
    
Vale, ahora vamos a jugar un poco con ella. La utilizamos de forma infija  de
forma que el valor ``Maybe`` estará en la parte izquierda y la función a
aplicar en la parte derecha:

.. code-block:: console

    ghci> Just 3 `applyMaybe` \x -> Just (x+1)  
    Just 4  
    ghci> Just "smile" `applyMaybe` \x -> Just (x ++ " :)")  
    Just "smile :)"  
    ghci> Nothing `applyMaybe` \x -> Just (x+1)  
    Nothing  
    ghci> Nothing `applyMaybe` \x -> Just (x ++ " :)")  
    Nothing

En este ejemplo vemos que cuando utilizamos ``applyMaybe`` con un valor
``Just`` y una función, la función simplemente se aplica al valor contenido en
``Just``. Cuando la utilizamos con un valor ``Nothing``, el resultado final es
``Nothing``. ¿Qué pasa si la función devuelve un ``Nothing``? Vamos ver:

.. code-block:: console

    ghci> Just 3 `applyMaybe` \x -> if x > 2 then Just x else Nothing  
    Just 3  
    ghci> Just 1 `applyMaybe` \x -> if x > 2 then Just x else Nothing  
    Nothing
    
Justo como imaginábamos. Si el valor monádico de la izquierda es ``Nothing``,
el resultado final es ``Nothing``. Y si la función de la derecha devuelve
``Nothing``, el resultado será de nuevo ``Nothing``. Es muy parecido a cuando
utilizabamos ``Maybe`` como funtor aplicativo y obteniamos como resultado
``Nothing`` si en algún lugar había un ``Nothing``.

Parace que para ``Maybe``, hemos averiguado como tomar un valor decorado y
pasarlo a una función que toma un parámetro normal y devuelve otro valor
decorado. Lo hemos conseguido teniendo en cuenta que los valores del tipo
``Maybe`` representan cómputo que pueden fallar.

Seguramente te este preguntado: ¿y esto es útil? Puede parecer que los 
funtores aplicativos son más potentes que las mónadas, ya que los funtores
aplicativos permiten tomar una función normal y hacer que opere sobre valores
con un cierto contexto. Veremos que las mónadas pueden hacer exactamente lo
mismo ya que son una versión mejorada de los funtores aplicativos, pero
también veremos que pueden hacer más cosas que los funtores aplicativos no
pueden hacer.

Volvermos con ``Maybe`` en un momento, pero primero, vamos a ver la clase de
tipos que define las mónadas.


La clase de tipos de las mónadas
--------------------------------


De la misma forma que los funtores tienen una clase ``Functor`` y que los
funtores aplicativos tienen una clase ``Applicative``, las mónadas vienen con
su propia clase de tipos: ``Monad`` ¡Wau! ¿Quíen lo hubiera imaginado? Así es
como luce su definición: ::

    class Monad m where  
        return :: a -> m a  

        (>>=) :: m a -> (a -> m b) -> m b  

        (>>) :: m a -> m b -> m b  
        x >> y = x >>= \_ -> y  

        fail :: String -> m a  
        fail msg = error msg
        
.. image:: /images/kid.png
   :align: right
   :alt: Así te ves cuando juegas con las mónadas
   
Empecemos por la primera línea. Dice ``class Monad m where``. Pero espera, ¿no
hemos dicho que las mónadas no son más que funtores aplicativos ampliados? ¿No
debería haber una resitricción de clase como
``class (Applicative m) = > Monad m where`` de forma que el tipo tenga que ser
un funtor aplicativo primero antes de ser una mónada? Bueno, debería, pero
cuando se creo Haskell, la gente que lo creo no pensó que los funtores
aplicativos encajarían bien en Haskell así que no aparece. Pero ten seguro que
cada mónada es un funtor aplicativo, incluso aunque la declaración de la clase
``Monad`` diga lo contrario.

La primera función que define la clase de tipos ``Monad`` es ``return``. Es lo
mismo que ``pure`` pero con un nombre diferente. Su tipo es
``(Monad m) => a -> m a``. Toma un valor y lo introduce en el contexto por
defecto que pueda albergar dicho valor. En otras palabras, toma un valor y lo
introduce en una mónada. Siempre hace lo mismo que la función ``pueda`` de la
clase de tipos ``Applicative``, por lo que ya estmos familiarizados al uso
de ``return``. Ya hemos utilizado ``return`` cuando trabajamos con la E/S. La
utilizabamos para crear una acción de E/S que no hiciera nada salvo contener
un valor. Con ``Maybe`` toma un valor y lo introduce en un valor ``Just``.

.. note:: Recordatorio: ``return`` no se parece en nada al ``return`` de la
          mayoría de los otro lenguajes de programación. No termina la
          ejecución ni nada por el estilo, simplemente toma un valor normal y
          lo introduce en un contexto.
          
.. image:: /images/tur2.png
   :align: left
   :alt: ¡Sí!

La siguiente función es ``>>=`` o lazo. Es como la aplicación de funciones,
solo que en lugar de tomar un valor y pasarlo a una función normal, toma un
valor monádico (es decir, un valor en un cierto contexto) y lo pasa a una
función que toma un valor normal pero devuelve otro valor monádico.

A continuación tenemos ``>>``. No le vamos a prestar mucha ateción ahora mismo
ya que viene con una implementación por defecto y prácticamente nunca
tendremos que implementarla cuando creemos instancias de ``Monad``.

La función final de la clase de tipos ``Monad`` es ``fail``. Nunca la
utilizaremos explícitamente en nuestro código. En cambio, Haskell la utilizará
para permitir fallos en una construción sintáctica para las mónadas que
veremos más adelante. No tenemos que preocuparnos demasiado con ``fail`` ahora
mismo.

Ahora que ya sabemos como luce la clase de tipos ``Monad``, vamos a ver como
es la instancia de ``Maybe`` para la clase ``Monad``: ::

    instance Monad Maybe where  
        return x = Just x  
        Nothing >>= f = Nothing  
        Just x >>= f  = f x  
        fail _ = Nothing
        
``return`` es lo mismo que ``pure``, no hay que pensar mucho. Hacemos
exactamente lo mismo que hacíamos con ``Applicative``, introducimos un valor
en ``Just``. 

La función ``>>=`` es exactamente igual ``applyMaybe``. Cuando le pasamos
un valor del tipo ``Maybe a`` a esta función, tenemos en cuenta el contexto y
devolvemos ``Nothing`` si el valor a la izquierda es ``Nothing`` ya que no
existe forma posible de aplicar la función con este valor. Si es un valor
``Just`` tomamos lo que hay dentro de él y aplicamos la función.

Podemos probar un poco ``Maybe`` como mónada:

.. code-block:: console

    ghci> return "WHAT" :: Maybe String  
    Just "WHAT"  
    ghci> Just 9 >>= \x -> return (x*10)  
    Just 90  
    ghci> Nothing >>= \x -> return (x*10)  
    Nothing
    
Nada nuevo o emocionante en la primera línea ya que ya hemos usado ``pure``
con ``Maybe`` y sabemos que ``return`` es igual que ``pure`` solo que con otro
nombre. La siguientes dos líneas muestran como funciona ``>>=`` un poco más.

Fíjate en como hemos pasado ``Just 9`` a la función ``\x -> return (x*10)``,
``x`` toma el valor ``9`` dentro de la función. Parece como si fueramos
capaces de extraer el valor de un ``Maybe`` sin utilizar un ajuste de
patrones. Y aún así no perdemos el contexto de los tipo ``Maybe``, porque
cuando es ``Nothing``, el resultado de ``>>=`` será ``Nothing`` también.


En la cuerda floja
------------------

.. image:: /images/pierre.png
   :align: left
   :alt: Pierre

Ahora que ya sabemos como parar un valor del tipo ``Maybe a`` a una función
del tipo ``a -> Maybe b`` teniendo en cuenta el contexto de un posible fallo,
vamos a ver como podemos usar ``>>=`` repetidamente para manejar varios
valores ``Maybe a``.

Pierre ha decidido tomar un descanso en su trabajo en la piscifactoria e
intentar caminar por la cuerda floja. No lo hace nada mal, pero tiene un
problema: ¡los pájaros se posan sobre su barra de equilibrio! Aterrizan y se
toman un pequeño respiro, hablan con sus respectivos amigos ovíparos y luego
se marchan en busca de algo de comida. Ha Pierre no le importaría demasiado si
el número de pájaros que se posan en cada lado de la barra fuera el mismo. Sin
embargo, a menudo, todos los pájaros se posan en el mismo lado y desequilibran
a Pierre tirándolo de la cuerda de forma embarazosa (utiliza un red de
seguridad obviamente).

Digamos que matiene el equilibrio si el número de pájaros posados a la
izquierda y a la derecha de la barra no difere en más de tres. Así que si hay
un pájaro en la parte derecha y otros cuatro pájaros en la parte izquierda no
pasa nada. Pero si un quinto pájaro aterriza en la parte derecha pierde el
quilibrio y cae.

Vamos a simular un grupo de pájaros que aterrizan o inician el vuelo desde la
barra y ver si Pierre sigue sobre la barra tras un número de eventos
relacionados con estas aves. Por ejemplo, queremos saber que le pasará a
Pierre si primero llega un pájaro al lado izquierdo de la barra, luego cuatro
pájaros más se posan sobre la parte derecha y luego el pájaro de la izquierda
decide volar de nuevo.

Podemos representar la barra con un par de enteros. El primer componente 
indicará el número de pájaros a la izquierda mientras que el segundo indicará
el número de pájaros de la derecha: ::

    type Birds = Int  
    type Pole = (Birds,Birds)
    
Primero creamos un sinónimo para ``Int``, llamado *pájaros* (``Birds``), ya
que estamos utilizando enteros para representar el número de pájaros. Luego
creamos otro sinónimo de tipos ``(Birds, Birds)`` y lo llamamos *barra*
(``Pole``).

A continuación creamos una función que toma un número de pájaros y los posa
sobre un determinado lado de la barra. Aquí están las funciones: ::

    landLeft :: Birds -> Pole -> Pole  
    landLeft n (left,right) = (left + n,right)  

    landRight :: Birds -> Pole -> Pole  
    landRight n (left,right) = (left,right + n)
    
Bastante simple. Vamos a probarlas:

.. code-block:: console

    ghci> landLeft 2 (0,0)  
    (2,0)  
    ghci> landRight 1 (1,2)  
    (1,3)  
    ghci> landRight (-1) (1,2)  
    (1,1)  
    
Para hacer que los pájaros vuelen simplemente tenmos que pasarles a estas
funciones un número negativo. Como estas funciones devuelven un valor del
tipo ``Pole``, podemos encadenarlas: 

.. code-block:: console

    ghci> landLeft 2 (landRight 1 (landLeft 1 (0,0)))  
    (3,1)
    
Cuando aplicamos la función ``landLeft 1`` a ``(0, 0)`` obtenemos ``(1, 0)``.
Luego aterrizamos un pájaro sobre el lado derecho, por lo que obtenemos
``(1, 1)``. Para terminar aterrizamos dos pájaros más sobre el lado izquierdo,
lo cual resulta en ``(3, 1)``. Aplicamos una función a algo escribirendo
primero el nombre de la función y luego sus parámetros, pero en este caso
sería mejor si la barra fuera primero y luego las funciones de aterrizar. Si
creamos una función como: ::

    x -: f = f x  

Podríamos aplicar funciones escribiendo primero el parámetro y luego el nombre
de la función:
    
.. code-block:: console

    ghci> 100 -: (*3)  
    300  
    ghci> True -: not  
    False  
    ghci> (0, 0) -: landLeft 2  
    (2,0)

Utilizando esto podemos aterrrizar varios pájaros de un forma mucho más
legible:

.. code-block:: console

    ghci> (0, 0) -: landLeft 1 -: landRight 1 -: landLeft 2  
    (3,1)
    
¡Genial! Es ejemplo es equivalente al ejemplo anterior en el que
aterrizamos varias aves en la barra, solo que se ve más limpio. Así es más
obvio que empezamos con ``(0, 0)`` y luego aterrizamos un pájaro sobre la
izquierda, otro sobre la derecha y finalmente dos más sobre la izquierda.

Hasta aquí bien, pero, ¿qué sucede si aterrizan diez pájaros sobre un lado?

.. code-block:: console

    ghci> landLeft 10 (0,3)  
    (10,3)
    
¿Diez pájaros en la parte izquierda y solo tres en la derecha? Seguro que
Pierre ya debe estar volando por los aires en esos momentos. En este ejemplo
es bastante obvio pero, ¿y si tenemos una secuencia como esta?:

.. code-block:: console

    ghci> (0,0) -: landLeft 1 -: landRight 4 -: landLeft (-1) -: landRight (-2)  
    (0,2)
    
A primera vista puede parecer que todo esta bien pero si seguimos los pasos,
veremos que en un determinado momento hay cuatro pájaros a la derecha y
ninguno a la izquierda. Para arreglar esto debemos darle una vuelta de tuerca
a las funciones ``landLeft`` y ``landRight``. A partir de lo que hemos
aprendido queremos que estas funciones sean capaces de fallar. Es decir,
queremos que devuelvan una barra si Pierre consigue mantener el equilibrio
pero que fallen en caso de que Pierre lo pierda. ¡Y qué mejor manera de
añadir el contexto de un posible fallo a un valor que utilizar ``Maybe``!
Vamos a reescribir estas funciones: ::

    landLeft :: Birds -> Pole -> Maybe Pole  
    landLeft n (left,right)  
        | abs ((left + n) - right) < 4 = Just (left + n, right)  
        | otherwise                    = Nothing  

    landRight :: Birds -> Pole -> Maybe Pole  
    landRight n (left,right)  
        | abs (left - (right + n)) < 4 = Just (left, right + n)  
        | otherwise                    = Nothing
        
En lugar de devolver un ``Pole`` estas funciones devuelven un ``Maybe Pole``.
Siguen tomando el número de pájaros y el estado de la barra anterior, pero
ahora comprueban si el número de pájaros y la posición de estos es suficiente
como para desquilibrar a Pierre. Utilizamos guardas para comprabar si
diferencia entre el número de pájaros en cada lado es menor que cuatro. Si lo
es devuelve una nueva barra dentro de un ``Just``. Si no lo es, devuelven
``Nothing``.

Vamos a jugar con estas pequeñas:

.. code-block:: console

    ghci> landLeft 2 (0,0)  
    Just (2,0)  
    ghci> landLeft 10 (0,3)  
    Nothing

¡Bien! Cuando aterrizamos pájaros sin que Pierre pierda el equilibrio
obtenemos una nueva barra dentro de un ``Just``. Pero cuando unos cunatos
pájaros de más acaban en un lado de la barra obtenemos ``Nothing``. Esto esta
muy bien pero ahora hemos perido la posibilidad de aterrizar pájaros de forma
repetiva sobre la barra. Ya no podemos usar ``landLeft 1 (landRight 1 (0,0))``
ya que cuando aplicamos ``landRight 1`` a ``(0, 0)`` no obtenemos un ``Pole``,
sino un ``Maybe Pole``. ``landLeft 1`` toma un ``Pole`` y no un
``Maybe Pole``.

Necesitamos una forma de tomar un ``Maybe Pole`` y pasarlo a una función que
toma un ``Pole`` y devuelve un ``Maybe Pole``. Por suerte tenemos ``>>=``, que
hace exáctamen lo que buscamos para ``Maybe``. Vamos a probarlo:

.. code-block:: console

    ghci> landRight 1 (0,0) >>= landLeft 2  
    Just (2,1)
    
Recuerda, ``landLeft 2`` tiene un tipo ``Pole -> Maybe Pole``. No podemos
pasarle directamente un valor del tipo ``Maybe Pole`` que es el resultado de
``landRight 1 (0, 0)``, así que utilizamos ``>>=`` que toma un valor con un
determinado contexto y se lo pasa a ``landLeft 2``. De hecho ``>>=`` nos
permite tratar valores ``Maybe`` como valores en un contexto si pasamos
``Nothing`` a ``landLeft 2``, de forma que el resultado será ``Nothing`` y el
fallo ser propagará:

.. code-block:: console

    ghci> Nothing >>= landLeft 2  
    Nothing
    
Gracias a esto ahora podemos encadenar varios aterrizajes que pueden consguir
tirar a Pierre ya que ``>>=`` nos permite pasar valores monádicos a funciones
que toman valores normales.

Aquí tienes una secuencia de aterrizajes:

.. code-block:: console

    ghci> return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2  
    Just (2,4)
    
Al principio hemos utilizado ``return`` para insertar una barra dentro de un
``Just``. Podríamos haber aplicado ``landRight 2`` directamente a ``(0, 0),
hubiéramos llegado al mismo resultado, pero de esta forma podemos utilizar
``>>=`` para cada función de forma más consistente. Se pasa ``Just (0, 0)`` a
``landRight 2``, lo que devuelve ``Just (0, 2)``. Luego se le pasa este valor
a ``landLeft 2`` obteniendo ``Just (2, 2)`` y así sucesivamente.

Recuerda el ejemplo que dijimos que tiraría a Pierre:

.. code-block:: console
    
    ghci> (0,0) -: landLeft 1 -: landRight 4 -: landLeft (-1) -: landRight (-2)  
    (0,2)
    
Como vemos no simula la interacción con las aves correctamente ya que en medio
la barra ya estaría volando por los aires pero el resultado no lo refleja.
Pero ahora vamos a probar a utilizar la aplicación monádica (``>>=``) en lugar
de la aplicación normal:

.. code-block:: console

    ghci> return (0,0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2)  
    Nothing
    
.. image:: /images/banana.png
   :align: right
   :alt: Soy un platano
    
Perfecto. El resultado final representa un fallo, que es justo lo que
esperamos. Vamos a ver como se consigue este resultado. Primero, ``return``
introduce ``(0, 0)`` en el contexto por defecto, convirtiéndolo en
``Just (0, 0)``. Luego sucede ``Just (0,0) >>= landLeft 1``. Como
``Just (0,0)`` es un valor ``Just``, ``landLeft 1`` es aplicado a ``(0, 0)``,
obteniendo así ``Just (1, 0)`` ya que Pierre sigue manteniendo el equilibrio.
Luego nos encontramos con ``Just (1,0) >>= landRight 4`` lo cual resulta en
``Just (1, 4)`` ya que Pierre sigue manteniendo el equilibrio, aunque
malamente. Se aplica ``landLeft (-1)`` a ``Just (1, 4)``, o dicho de otra
forma, se computa ``landLeft (-1) (1,4)``. Ahora, debido a como funciona
``landLeft``, esto devuelve ``Nothing`` porque nuestro esta volando por los
aires en este mismo momento. Ahora que tenemos ``Nothing`` como resultado,
éste se pasado a ``landRight (-2), pero como es un valor ``Nothing``, el
resultado es automáticamente ``Nothing`` ya que no existe ningún valor que se
puede aplicar a ``landRight (-2)``.

No podíamos haber conseguido esto utilizando solo ``Maybe`` como funtor
aplicativo. Si lo intentas te quedarás atascado, porque los funtores
aplicativos no permiten que los valores aplicativos interactuen con los
otros lo sufiente. Pueden, como mucho, ser utilizados como parámetros de una
función utilizando el estilo aplicativo. Los operadores aplicativos tomarán
los resultados y se los pasarán a la función de forma apropiada para cada
funto aplicativo y luego obtendrán un valor aplicativo, pero no existe ninguna
interacción entre ellos. Aquí, sin embargo, cada paso depende del resultado
anterior. Por cada aterrizaje se examina el resultado anterior y se comprueba
que la barra está balanceada. Esto determina si el aterrizaje se completará
o fallará.

Podemos divisar una función que ignora el número de pájaros en la barra de
equilibrio y simplemente haga que Pierre caiga. La llamaremos ``banana``: ::

    banana :: Pole -> Maybe Pole  
    banana _ = Nothing
    
Ahora podemos encadenar esta función con los aterrizajes de las aves. Siempre
hara que Pierre se caiga ya que ignora cualquier cosa que se le pasa y
devuelve un fallo. Compruebalo:

.. code-block:: console

    ghci> return (0,0) >>= landLeft 1 >>= banana >>= landRight 1  
    Nothing
    
El valor ``Just (1, 0)`` se le pasa a ``banana``, pero este produce
``Nothing``, lo cual hace que el resultado final sea ``Nothing``. Menuda
suerte.

En lugar de crear funciones que ignoren el resultado y simplemente devuelvan
un valor monádico, podemos utilizar la función ``>>`` cuya implementación por
defecto es esta: ::

    (>>) :: (Monad m) => m a -> m b -> m b  
    m >> n = m >>= \_ -> n
    
Normalmente, si pasamos un valor a una función que toma un parámetro y siempre
devuelve un mismo valor por defecto el resultado será este valor por defecto.
En cambio con la mónadas también debemos conseiderar el contexto y el
siguinificado de éstas. Aquí tienes un ejemplo de como funciona ``>>`` con
``Maybe``:

.. code-block:: console

    ghci> Nothing >> Just 3  
    Nothing  
    ghci> Just 3 >> Just 4  
    Just 4  
    ghci> Just 3 >> Nothing  
    Nothing
    
Si reemplazamos ``>>`` por ``>>= \_ ->`` es fácil de ver lo que realmente
sucede.

Podemos cambiar la función ``banana`` por ``>>`` y luego un ``Nothing``:

.. code-block:: console

    ghci> return (0,0) >>= landLeft 1 >> Nothing >>= landRight 1  
    Nothing
    
Ahí lo tienes, ¡garantizamos que Pierre se va ir al suelo!

También vale la pena echar un vistazo a como se veria esto si no hubiesemos
tratado los valores ``Maybe`` como valores en un contexto y no hubiersemos
pasado las parámetros a las funciones como hemos hecho. Así es como se vería
una serie de aterrizajes: ::

    routine :: Maybe Pole  
    routine = case landLeft 1 (0,0) of  
        Nothing -> Nothing  
        Just pole1 -> case landRight 4 pole1 of   
            Nothing -> Nothing  
            Just pole2 -> case landLeft 2 pole2 of  
                Nothing -> Nothing  
                Just pole3 -> landLeft 1 pole3  
    
.. image:: /images/centaur.png
   :align: right
   :alt: John Joe Glanton
  
Aterrizamos un pájaro y comprobamos la posibiliadad de que que ocurra un fallo
o no. En caso de fallo devolvemos ``Nothing``. En caso contrario aterrizamos
unos cuantos pájaros más a la derecha y volemos a comprobar lo mismo una y
otra vez. Convertir esto es un limpia concatenación de aplicaciones monádicas
con ``>>=`` es un ejemplo clásico de porque la mónada ``Maybe`` nos ahorra
mucho tiempo cuando tenemos una secuecnia de cómputos que dependen del
resultado de otros cómputos que pueden fallar.

Fíjate en como la implementación de ``>>=`` para ``Maybe`` interpreta
exactamente la lógica de que en caso encontrarnos con un ``Nothing``, lo
devolvemos como resultado y en caso contrario continuamos con lo que hay
dentro de ``Just``.

En esta sección hemos tomado varias funciones que ya teniamos y hemos visto
que funcionan mejor si el valor que devuelven soporta fallos. Conviertiendo
estos valores en valores del tipo ``Maybe`` y cambiando la aplicación de
funciones normal por ``>>=`` obtenemos un mecanismo para manejar fallos casi
de forma automática, ya que se supone ``>>=`` preserva el contexto del valor
que se aplica a una función. En este caso el contexto que tenían estos valores
era la posibiliadad de fallo de forma que cuando aplicábamos funciones sobre
estos valores, la posibilidad de fallo siempre era tomada en cuenta.
