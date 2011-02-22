

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


         



 



    


