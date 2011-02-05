

Funtores, funtores aplicativos y monoides
=========================================


La combinación de Haskell de la pureza, las funciones de orden superior,
tipos de datos algebraicos con parámetros, y clases de tipos nos permite
implementar el polimorfismo a un nivel superior al que pueden alcanzar otros
lenguajes. No tenemos que ver los tipos como si pertenecieran a una gran
jerarquía de tipos. En lugar de eso, vemos como pueden actuar los tipos y
luego los conectamos con las clases de tipos apropiadas. Un ``Int`` puede
actuar como un montón de cosas. Puede actuar como algo equiparable, como algo
ordenable, como algo enumerable, etc.

Las clases de tipos son abiertas, lo que significa que podemos definir nuestro
propio tipo de dato, razonar en como éste actúa y conectarlo con la clase de
tipos que define ese comportamiento. Por este motivo, y porque el fabuloso
sistema de tipos de Haskell nos permite saber mucho acerca de una función con
tan solo sabiendo su declaración de tipo, podemos crear clases de tipos que
definen comportamientos muy generales y abstractos. Ya vimos que las clases de
tipos definen operaciones para ver si dos cosas son iguales o comparar dos
cosas por un cierto orden. Son comportamientos muy abstractos a la vez que
elegantes, pero no los vemos como algo especial ya que hemos estado tratando
con ellos a lo largo de nuestras vidas. Hace poco conocimos los funtores, que
son básicamente cosas que se pueden mapear. Esto es un ejemplo de algo útil y
a la vez bastante abstracto de lo que pueden describir las clases de tipos. En
este capítulo veremos más de cerca los funtores, junto a una versión más
fuerte y útil de los funtores llamados funtores aplicativos. También daremos
un vistazo a los monoides.


De vuelta con los funtores
--------------------------

.. image:: /images/frogtor.png
   :align: right
   :alt: La ranas ni siquiera necesitan dinero.

Ya hablamos de los funtores en su pequeña :ref:`sección <funtores>`. Si
todavía no la has leído, probablemente deberías darle un vistazo ahora mismo,
o quizá luego cuando tengas más tiempo. O simplemente puedes hacer como si ya
la hubieses leído.

Aun así, vamos a recordar algo: los funtores son cosas que se puede mapear,
como las listas, ``Maybe``s, árboles, etc. En Haskell, son descritos con la
clase de tipos ``Functor``, la cual solo contiene un método de clase,
``fmap``, que tiene como tipo ``fmap :: (a -> b) -> f a -> f b``. Dice algo
como: dame una función que tome un ``a`` y devuelva un ``b`` y una caja con
una ``a`` (o varias de ellas) dentro y yo te daré una caja con una ``b`` (o
varias de ellas) dentro. En cierto modo es como si aplicará la función dentro
de la caja.

.. note:: Muchas veces utilizamos la analogía de la caja para hacernos una
          idea de como funcionan los funtores, luego, probablemente
          usemos la misma analogía para los funtores aplicativos y las
          mónadas. Al principio es una buena analogía que ayuda a la gente a
          entender los funtores, pero no la tomes al pie de la letra, ya que
          para algunos funtores la analogía de la caja tiene que ser ajusta al
          milímetro para que siga siendo verdad. Un término más correcto para
          definir lo que es un funtor sería *contexto* *computacional*. El
          contexto sería que la computación podría tener un valor, o podría
          fallar (``Maybe`` y ``Either a``) o que podría tener más valores
          (listas) o cosas por el estilo.

Si queremos que un constructor de tipos sea una instancia de ``Functor``, tiene
que pertenecer a la familia de tipos ``* -> *``, lo que significa que debe
tomar exactamente un tipo concreto como parámetro.  Por ejemplo, ``Maybe``
puede ser una instancia ya que tome un tipo como parámetro para producir un
nuevo tipo concreto, como ``Maybe Int`` o ``Maybe String``. Si un constructor
de tipos toma dos parámetros, como ``Either``, tenemos que aplicar
parcialmente el constructor de tipos hasta que solo acepte un parámetro. Así
que no podemos usar ``instance Functor Either where`` pero si podemos utilizar
``instance Functor (Either a) where`` y luego podemos pensar que ``fmap`` es
solo para ``Either a``, por lo que tendría una declaración de tipo como
``fmap :: (b -> c) -> Either a b -> Either a c``. Como puedes ver, la parte
``Either a`` es fija, ya que ``Either a`` toma solo un parámetro, mientras que
``Either`` toma dos parámetros así que
``fmap :: (b -> c) -> Either b -> Either c`` no tendría mucho sentido.

Hasta ahora hemos aprendido como unos cuantos tipos (bueno, en realidad
constructores de tipos) son instancias de ``Functor``, como ``[]``, ``Maybe``,
``Either a`` y el tipo ``Tree`` que creamos nosotros mismos. Vimos como
podíamos mapear funciones sobre ellos. En esta sección, veremos dos instancias
más de la clase funtor, en concreto ``IO`` y ``(->) r``.

Si un valor tiene el tipo, digamos, ``IO String``, significa que es una acción
que, cuando sea ejecutada, saldrá al mundo real y nos traerá una cadena, que
será devuelta como resultado. Podemos usar ``<-`` dentro de un bloque ``do``
para ligar ese resultado a un nombre. Mencionamos que las acciones E/S son
como cajas con sus pequeñitos pies que se encargan de salir al mundo real y
traernos algún valor. Podemos inspeccionar lo que nos han traído, pero si lo
hacemos el valor que devolvamos tiene que estar dentro de ``IO``. Si pensamos
en esta analogía de la caja con pies, podemos ver que ``IO`` se comporta como
un funtor.

Vamos a ver como ``IO`` es una instancia de ``Functor``. Cuando aplicamos
``fmap`` con una función sobre una acción de E/S, queremos obtener una acción
de E/S que haga lo mismo, pero que tenga la función anterior aplicada a su
resultado. ::

    instance Functor IO where  
        fmap f action = do  
            result <- action  
            return (f result)
            
El resultado de mapear algo sobre una acción de E/S será una acción de E/S,
así que justo después de la declaración usamos un bloque ``do`` para juntar
dos acciones de E/S en una nueva. En la implementación de ``fmap``, creamos
una nueva acción de E/S que primero ejecutará la acción de E/S original y
llamará a su resultado ``result``. Luego, hacemos ``return (f result)``.
``return`` es, como ya sabes, una función que crear una acción de E/S que no
hace nada salvo tener algo como resultado. La acción que produce un bloque
``do`` siempre tendrá como resultado el resultado de su última acción. Por ese
motivo utilizamos ``return`` para crear una acción de E/S que en realidad no
hace nada, salvo contener ``f result`` como resultado.

Podemos jugar con él para ver como funciona. En realidad es bastante simple.
Fíjate en el siguiente trozo de código: ::

    main = do line <- getLine   
              let line' = reverse line  
              putStrLn $ "You said " ++ line' ++ " backwards!"  
              putStrLn $ "Yes, you really said" ++ line' ++ " backwards!"  
    
Se le pregunta al usuario por una nueva línea y luego se la devolvemos al
usuario, aunque invertida. Así sería como escribiríamos lo mismo utilizando
``fmap``:  ::

    main = do line <- fmap reverse getLine  
              putStrLn $ "You said " ++ line ++ " backwards!"  
              putStrLn $ "Yes, you really said" ++ line ++ " backwards!"
              
.. image:: /images/alien.png
   :align: left
   :alt: w00ooOoooOO
   
De la misma forma que que cuando usamos ``fmap reverse`` sobre ``Just "blah"``
obtenemos ``Just "halb"``, podemos utilizar ``fmap reverse`` sobre
``getLine``. ``getLine`` es una acción de E/S que tiene el tipo ``IO String``
y al mapear ``reverse`` sobre ella nos devuelve una acción que viajará al
mundo real y nos traerá una línea de texto, a la que luego dará la vuelta
aplicando ``reverse`` a su resultado. De la misma forma que podemos aplicar
una función a algo contenido en una caja ``Maybe``, podemos aplicar una
función a lo que hay dentro de una caja ``IO``, solo que tiene que viajar al
mundo real para obtener ese algo. Luego lo ligamos a un nombre usando ``<-``,
dicho nombre será asociado al resultado que ya se le ha aplicado ``reverse``.

La acción de E/S ``fmap (++"!") getLine`` actúa como ``getLine``, solo su
resultado siempre lleva añadido un ``"!"`` al final. 

Si vemos el tipo de ``fmap`` limitado a ``IO``, tendríamos algo como
``fmap :: (a -> b) -> IO a -> IO b``. ``fmap`` toma una función y una acción
de E/S y devuelve una nueva acción de E/S que actúa como la anterior, solo que
la función se aplica al resultado contenido en la acción. 

Si alguna vez te encuentras ligando un nombre a una acción de E/S, con el
único fin de aplicarle una función para luego usarlo en algún otro lugar,
considera el uso de ``fmap``, ya que es más elegante. Si quieres aplicar
varias transformaciones al contenido de un funtor puedes declarar tu propia
función, usar una función lambda o, idealmente, utilizar la composición de
funciones: ::

    import Data.Char  
    import Data.List  

    main = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine  
              putStrLn line  
    
.. code-block:: console

    $ runhaskell fmapping_io.hs  
    hello there  
    E-R-E-H-T- -O-L-L-E-H
    
Como probablemente ya sepas, ``intersperse '-' . reverse . map toUpper`` es
una función que toma una cadena, mapea ``toUpper`` sobre ella, aplica
``reverse`` sobre el resultado anterior y luego le aplica ``intersperse '-'``.
Es como ``(\xs -> intersperse '-' (reverse (map toUpper xs)))`` solo que
más bonito. 

Otra instancia de ``Functor`` con la que hemos estado trabajando pero que no
sabíamos que era un funtor es ``(->) r``. Probablemente ahora mismo estás un
poco confundido, ya que ¿Qué diablos significa ``(->) r``? El tipo de una
función ``r -> a`` se puede reescribir como ``(->) r a``, de forma similar que
podemos escribir ``2 + 3`` como ``(+) 2 3``. Cuando nos encontramos con
``(->) r a``, vemos a ``(->)`` de forma diferente, ya que podemos verlo como
un constructor de tipos que toma dos parámetros de tipos, como ``Either``.
Pero recuerda, dijimos que un constructor de tipos debe tomar un solo
parámetro para poder ser instancia de un funtor. Por esa razón no podemos
crear una ``(->)`` instancia  de ``Functor``, pero si lo aplicamos
parcialmente ``(->) r``, no hay ningún problema. Si la sintaxis permitiera
aplicar parcialmente los constructores de tipos con secciones (de la misma
forma que podemos aplicar parcialmente ``+`` utilizando ``(2+)``, que es lo
mismo que ``(+) 2``), podríamos escribir ``(->) r`` como ``(r ->)`` ¿Cómo son
los funtores funciones? Bueno, vamos a echar un vistazo a la implementación,
que se encuentra en ``Control.Monad.Instances``.

.. note:: Normalmente identificamos a la funciones que toman cualquier cosa y
          devuleven cualquier otra cosa como ``a -> b``. ``r -> a`` es
          exactamente lo mismo, solo que hemos usado letras diferentes para
          las variables de tipo.
          
::

    instance Functor ((->) r) where  
        fmap f g = (\x -> f (g x))

Si la sintaxis lo permitiera, lo podríamos haber escrito como: ::

    instance Functor (r ->) where  
        fmap f g = (\x -> f (g x))
        
Pero no lo permite, así que lo tenemos que escribir como al principio.

Antes de nada, vamos a pensar en el tipo de ``fmap``. Sería
``fmap :: (a -> b) -> f a -> f b``. Ahora lo que tenemos que hacer es
remplazar mentalmente todas las ``f``, las cuales hacen el papel de funtor,
por ``(->) r``. Hacemos esto cada vez que queramos ver como se comporta
``fmap`` para una cierta instancia. Obtenemos
``fmap :: (a -> b) -> ((->) r a) -> ((->) r b)``. Ahora lo que podemos hacer
es escribir los tipos ``((->) r a)`` y ``((->) r b)`` de forma infija,
``r -> a`` y ``r -> b``, como hacemos normalmente con las funciones. Lo que
obtenemos es ``fmap :: (a -> b) -> (r -> a) -> (r -> b)``.

Mmmm... Vale. Si mapeamos una función sobre una función obtenemos una nueva
función, de la misma forma que si mapeamos una función sobre un ``Maybe``
obtenemos un ``Maybe`` y de la misma forma que si mapeamos una función sobre
una lista obtenemos una lista ¿Qué nos dice exactamente el tipo
``fmap :: (a -> b) -> (r -> a) -> (r -> b)``? Bueno, podemos ver que toma una
función de ``a`` a ``b`` y una función de ``r`` a ``a`` y devuelve una
función de ``r`` a ``b`` ¿Te recuerda a algo? ¡Sí! ¡Composición de funciones!
Dirigimos la salida de ``r -> a`` a la entrada de ``a -> b`` para obtener una
función ``r -> b``, lo cual es exactamente lo mismo que la composición de
funciones. Si miras como se definió la instancia arriba, podrás ver que es una
simple composición de funciones. Otra forma de escribirlo sería así: ::

    instance Functor ((->) r) where  
        fmap = (.)

De esta forma vemos de forma clara que ``fmap`` es simplemente una composición
de funciones. Ejecuta ``:m + Control.Monad.Instances``, ya que ahí está
definida esta instancia e intenta mapear algunas funciones.

.. code-block:: console

    ghci> :t fmap (*3) (+100)  
    fmap (*3) (+100) :: (Num a) => a -> a  
    ghci> fmap (*3) (+100) 1  
    303  
    ghci> (*3) `fmap` (+100) $ 1  
    303  
    ghci> (*3) . (+100) $ 1  
    303  
    ghci> fmap (show . (*3)) (*100) 1  
    "300"
    
Podemos llamar a ``fmap`` de forma infija para que se parezca a ``.``. En la
segunda línea estamos mapeando ``(*3)`` sobre ``(+100)``, lo que resulta en
una función que tomara un valor llamará a ``(+100)`` y luego a ``(*3)`` con el
resultado anterior. Llamamos a la función con ``1``.

¿Cómo encaja la analogía de la caja aquí? Bueno, si la forzamos un poco, se
ajusta. Cuando usamos ``fmap (+3)`` sobre ``Just 3`` nos es fácil imaginar a
``Maybe`` como una caja que contiene algo a lo que aplicamos la función
``(+3)`` ¿Pero qué sucede cuando usamos ``fmap (*3) (+100)``? Bueno, puedes
imaginar a ``(+100)`` como una caja que contiene el resultado final. Algo
parecido a cuando imaginábamos las acciones de E/S como una caja que salía al
mundo real y nos traía un resultado. Al usar ``fmap (*3)`` sobre ``(+100)``
creará otra función que se comportará como ``(+100)``, solo que antes de
producir el resultado, aplicará ``(*3)`` a ese resultado. Ahora podemos ver
como ``fmap`` se comporta como ``.`` para las funciones.

El hecho de que ``fmap`` se comporte como una composición de funciones cuando
se utiliza sobre funciones no es que sea especialmente útil en estos momentos,
pero al menos es interesante. También puede confundirnos ver como algunas
cosas que se comportan más como una computación que como una caja (``IO`` y
``(->) r``), son funtores. Una función mapeada sobre una computación devuelve
esa misma computación, pero a el resultado de dicha computación se le aplicará
la función mapeada.

.. image:: /images/lifter.png
   :align: right
   :alt: Desplazar una función es más fácil que desplazar una tolenada.

Antes de que veamos las reglas que ``fmap`` debe seguir, vamos a pensar sobre
el tipo de ``fmap`` una vez más. Su tipo es
``fmap :: (a -> b) -> f a -> f b``. Nos hemos olvidado de la restricción de
clase ``(Functor f) =>``, pero lo hemos hecho por brevedad ya porque estamos
hablando de funtores y sabemos que significa ``f``. La primera vez que
hablamos sobre las :ref:`Funciones <currificadas>`, dijimos que en realidad
todas las funciones de Haskell toman un solo parámetro. Una función con tipo
``a -> b -> c`` en realidad toma un solo parámetro ``a`` y luego devuelve una
función ``b -> c``, que a su vez toma otro parámetro y devuelve ``c``. Es como
si llamáramos a la función con demasiados pocos parámetros (es decir, la
aplicamos parcialmente), obtenemos una función que toma tantos parámetros como
nos hayamos dejado (si pensamos de nuevo que las funciones toman varios
parámetros). Así que ``a -> b -> c``  puede escribirse como ``a -> (b -> c)``
para hacer visible la currificación. 

Del mismo modo, si escribimos ``fmap :: (a -> b) -> (f a -> f b)``, podemos
ver a ``fmap`` no como una función que toma una función y un funtor y devuelve
otro funtor, sino como una función que toma una función y devuelve otra
función igual a la anterior, solo que toma un funtor como parámetros y
devuelve otro funtor como resultado. Toma una función ``a -> b`` y devuelve
una función ``f a -> f b``. A esto se llama *mover una función*. Vamos a
trastear un poco con esa idea utilizando el comando ``:t`` de GHCi: 

.. code-block:: console

    ghci> :t fmap (*2)  
    fmap (*2) :: (Num a, Functor f) => f a -> f a  
    ghci> :t fmap (replicate 3)  
    fmap (replicate 3) :: (Functor f) => f a -> f [a]

La expresión ``fmap (*2)`` es una función que toma un funtor ``f`` sobre
números y devuelve otro funtor sobre números. Ese funtor puede ser una lista,
un ``Maybe``, un ``Either String``, cualquier cosa. La expresión
``fmap (replicate 3)`` tomara un funtor sobre cualquier tipo y devolverá un
funtor sobre una lista de elementos de ese tipo.

.. note:: Cuando decimos *funtor sobre números*, puedes verlo como un *funtor
          contiene números*. El primero es algo más formal y más técnicamente
          correcto, pero el segundo es más fácil de captar.
          
Puedes ver ``fmap`` como una función que toma una función y un funtor y luego
mapea dicha función sobre el funtor, o puedes verlo como una función que toma
función y mueve dicha función de forma que opere sobre funtores. Ambos puntos
de vista son correctos en Haskell, equivalentes. 

El tipo ``fmap (replicate 3) :: (Functor f) => f a -> f [a]`` nos dice que la
función funcionará cuan cualquier tipo de funtor. Lo que hará exactamente
dependerá de que tipo de funtor utilicemos. Si usamos ``fmap (replicate 3)``
con una lista, la implementación de ``fmap`` para listas será utilizada, que
es ``map``. Si la usamos con un``Maybe``, aplicará ``replicate 3`` al valor
contenido en ``Just``, o si es ``Nothing``, devolverá ``Nothing``.

.. code-block:: console

    ghci> fmap (replicate 3) [1,2,3,4]  
    [[1,1,1],[2,2,2],[3,3,3],[4,4,4]]  
    ghci> fmap (replicate 3) (Just 4)  
    Just [4,4,4]  
    ghci> fmap (replicate 3) (Right "blah")  
    Right ["blah","blah","blah"]  
    ghci> fmap (replicate 3) Nothing  
    Nothing  
    ghci> fmap (replicate 3) (Left "foo")  
    Left "foo"
    
Ahora vamos a ver las **leyes de los funtores**. Para que algo sea una funtor,
debe satisfacer una serie de leyes. Se espera que todos los funtores exhiban
una serie de propiedades y comportamientos. Deben comportarse fielmente como
cosas que se puedan mapear. Al llamar ``fmap`` sobre un funtor solo debe
mapear una función sobre ese funtor, nada más. Este comportamiento se describe
en las leyes de los funtores. Hay dos de ellas que todas las instancias de
``Functor`` deben cumplir. Haskell no comprueba estas leyes automáticamente,
así que tenemos que comprobarlas nosotros mismos.

**La primera ley de funtores establece que si mapeamos la función** ``id``
**sobre un funtor, el funtor que obtenemos debe ser igual que el original**.
Si lo escribimos algo más formal, sería :js:data:`fmap id = id`. Básicamente
dice que, si usamos ``fmap id`` sobre un funtor, debe devolver lo mismo que
si aplicamos ``id`` a ese funtor. Recuerda, ``id`` es la función identidad, la
cual devuelve el parámetro original que le pasemos. También se pude definir
como ``\x -> x``. Si vemos el funtor como algo que puede ser mapeado, la ley
:js:data:`fmap id = id` es bastante trivial y obvia. 

Vamos a ver si esta ley se cumple para algunos funtores:

.. code-block:: console

    ghci> fmap id (Just 3)  
    Just 3  
    ghci> id (Just 3)  
    Just 3  
    ghci> fmap id [1..5]  
    [1,2,3,4,5]  
    ghci> id [1..5]  
    [1,2,3,4,5]  
    ghci> fmap id []  
    []  
    ghci> fmap id Nothing  
    Nothing

Si vemos la definición de ``fmap`` para, digamos, el tipo ``Maybe``, podemos
averiguar porque la primera ley se cumple: ::

    instance Functor Maybe where  
        fmap f (Just x) = Just (f x)  
        fmap f Nothing = Nothing

Imaginamos que ``if`` hace el papel del parámetro ``f`` en la implementación.
Vemos que si mapeamos ``fmap id`` sobre ``Just x``, el resultado será
``Just (id x)``, y como ``id`` simplemente devuelve su parámetro, podemos
deducir que ``Just (id x)`` es igual a ``Just x``. De esta forma ahora sabemos
que si mapeamos ``id`` sobre un valor de ``Maybe`` con un constructor de datos
``Just``, obtenemos lo mismo como resultado. 

Demostrar que al mapear ``id`` sobre un valor ``Nothing`` devuelve el mismo
valor es trivial. Así que a partir de estas dos ecuaciones de la
implementación de ``fmap`` podemos decir que la ley ``fmap id = id`` se
cumple.

.. image:: /images/justice.png
   :align: left
   :alt: La justicia es ciega, aunque también mi perro.
   
**La segunda ley dice que si mapeamos el resultado de una composición de dos
funciones sobre un funtor debe devolver lo mismo que si mapeamos una de estas
funciones sobre el funtor inicial y luego mapeamos la otra función**. Escrito
formalmente sería :js:data:`fmap (f . g) = fmap f . fmap g`. O de otra forma
sería, para cualquier funtor ``F``,
:js:data:`fmap (f . g) F = fmap f (fmap g F)`.

Si podemos demostrar que un funtor cumple las dos leyes, podemos confiar en 
que dicho funtor tendrá el mismo comportamiento que los demás funtores.
Sabremos que cuando utilizamos ``fmap`` sobre él, no pasará nada más que no
conozcamos y que se comportará como algo que puede ser mapeado, es decir, un
funtor. Puedes averiguar si se cumple la segunda ley para cierto tipo viendo
la implementación de ``fmap`` de ese tipo y utilizando luego el mismo método
que hemos utilizado para ver si ``Maybe`` cumplía la primera ley.

Si quieres, podemos comprobar como se cumple la segunda ley de los funtores
para ``Maybe``. Si hacemos ``fmap (f . g)`` sobre ``Nothing`` obtenemos
``Nothing``, ya que al mapear cualquier función sobre ``Nothing`` devuelve
``Nothing``. Si hacemos ``fmap f (fmap g Nothing)`` sobre ``Nothing``,
obtenemos ``Nothing`` por el mismo motivo. Vale, comprobar como se cumple la
segunda ley para ``Maybe`` si es un valor ``Nothing`` es bastante sencillo,
casi trivial.

¿Qué pasa cuando tenemos un valor ``Just algo``?  Bueno, si hacemos
``fmap (f . g) (Just x)``, a partir de la implementación vemos que convierte
en ``Just ((f . g) x)``, que es lo mismo que ``Just (f (g x))``. Si hacemos
``fmap f (fmap g (Just x))``, a partir de la implementación vemos que
``fmap g (Just x)`` es ``Just (g x)``. Ergo, ``fmap f (fmap g (Just x))`` es
igual a ``fmap f (Just (g x))`` y a partir de la implementación vemos que esto
es igual a ``Just (f (g x))``.

Si esta demostración te confunde un poco, no te preocupes. Asegúrate de
entender como funciona la :ref:`composición de funciones <compfunc>`. La mayor
parte de las veces puedes ver como se cumplen estas leyes de forma intuitiva
porque los tipos actúan como contenedores o funciones. También puedes
probarlas con cierta seguridad usando un montón de valores diferentes de un
cierto tipo y comprobar que, efectivamente, las leyes se cumplen.

Vamos a ver un ejemplo patológico de un constructor de tipos que tenga una
instancia de clase de tipos ``Functor`` pero que en realidad no sea un funtor,
debido a que satisface las leyes. Digamos que tenemos el siguiente tipo: ::

    data CMaybe a = CNothing | CJust Int a deriving (Show)  
    
La *C* viene de *contador*. Es un tipo de datos que se parece mucho a
``Maybe a``, solo que la parte ``Just`` contiene dos campos en lugar de uno.
El primer campo del constructor de datos ``CJust`` siempre tiene el tipo
``Int``, que es una especie de contador, mientras que el segundo campo tiene
el tipo ``a``, que procede del parámetro de tipo y su tipo será el tipo
concreto que elijamos para ``CMaybe a``. Vamos a jugar un poco con este nuevo
tipo para ver como funciona. 

.. code-block:: console

    ghci> CNothing  
    CNothing  
    ghci> CJust 0 "haha"  
    CJust 0 "haha"  
    ghci> :t CNothing  
    CNothing :: CMaybe a  
    ghci> :t CJust 0 "haha"  
    CJust 0 "haha" :: CMaybe [Char]  
    ghci> CJust 100 [1,2,3]  
    CJust 100 [1,2,3]
    
Cuando usamos el constructor ``CNothing``, no hay ningún campo que rellenar,
mientras que si usamos el constructor ``CJust``, el primer campo será un
entero y el segundo campo podrá ser de cualquier tipo. Vamos a crear una
instancia para la clase de tipos ``Functor`` de forma que cada vez que usemos
``fmap``, la función sea aplicada al segundo campo, mientras que el contador
sea incrementado en uno. ::

    instance Functor CMaybe where  
        fmap f CNothing = CNothing  
        fmap f (CJust counter x) = CJust (counter+1) (f x)
        
Se parece a la implementación de ``Maybe``, exceptuando que cuando aplicamos
``fmap`` sobre un valor que no representa una caja vacía (un valor ``CJust``),
no solo aplicamos la función al contenido de la caja, sino que además
incrementamos el contador en uno. Parece que todo está bien hasta ahora,
incluso podemos probarlo un poco:

.. code-block:: console

    ghci> fmap (++"ha") (CJust 0 "ho")  
    CJust 1 "hoha"  
    ghci> fmap (++"he") (fmap (++"ha") (CJust 0 "ho"))  
    CJust 2 "hohahe"  
    ghci> fmap (++"blah") CNothing  
    CNothing

¿Cumple con las leyes de los funtores? Para demostrar que no cumple las leyes,
basta con encontrar un contraejemplo. 

.. code-block:: console

    ghci> fmap id (CJust 0 "haha")  
    CJust 1 "haha"  
    ghci> id (CJust 0 "haha")  
    CJust 0 "haha"

¡Ah! Sabemos que la primera ley de los funtores dice que si mapeamos ``id``
sobre un funtor, debe devolver lo mismo que llamamos ``id`` con ese mismo
funtor, pero como hemos visto en este ejemplo, esta ley no se cumple para el
funtor ``CMaybe``. Aunque forma parte de la clase de tipos ``Functor``, no
cumple las leyes de los funtores y por lo tanto no es un funtor. Si alguien
usará ``CMaybe`` como un funtor, esperaría que obedeciera las leyes de los
funtores como un buen funtor. Pero ``CMaybe`` falla a la hora de ser un
funtor aunque pretende serlo, así que usarlo como un funtor nos puede llevar
a un código erróneo. Cuando utilizamos un funtor, no nos debe importar si
primero unimos unas cuantas funciones usando una composición y luego la
mapeamos sobre un funtor o si mapeamos unas cuantas funciones sobre un funtor
sucesivamente. Pero con ``CMaybe`` si importa, ya que lleva una cuenta de
cuantas veces ha sido mapeado ¡Mal! Si quisiéramos que ``CMaybe`` cumpliera
las leyes de los funtores, deberíamos hacer que el campo ``Int`` se mantuviera
constante utilizamos ``fmap``. 

En un principio las leyes de los funtores pueden parecer un poco confusas e
innecesarias, pero luego vemos que si sabemos que un tipo cumple con ambas
leyes, podemos asumir como se comportará. Si un tipo cumple las leyes de los
funtores, sabemos que si llamamos a ``fmap`` sobre un valor de ese tipo solo
mapeará la función sobre ese funtor, nada más. Esto nos lleva a un código que
es más abstracto y extensible, ya que podemos utilizar las leyes para razonar
acerca del comportamiento que un funtor debe tener y crear funciones que
operen de forma fiable sobre funtores.

Todas las instancias de los funtores de la biblioteca estándar cumplen con
estas leyes, aunque puedes comprobarlo tu mismo si no me crees. La próxima vez
que hagas una instancia ``Functor`` para un tipo, tómate tu tiempo para
asegurarte de que cumple con las leyes de los funtores. Cuando hayas trabajado
lo suficiente con los funtores, sabrás ver de forma intuitiva las propiedades
y comportamientos que tienen en común los funtores y te será muy fácil decir
si un funtor cumple o no con estas leyes. Aún sin esta experiencia, siempre
puedes leer la implementación línea a línea y ver si las leyes se cumplen o
intentar descubrir algún contraejemplo.

También podemos ver los funtores como resultados en un cierto contexto. Por
ejemplo, ``Just 3`` tiene un resultado igual a``3`` en el contexto de que
puede existir un resultado o no. ``[1,2,3]`` contiene tres resultados, ``1``,
``2`` y ``3``, en el contexto de que pueden haber varios resultados o incluso
ninguno. La función ``(+3)`` dará un resultado, dependiendo del parámetro que
se le de. 

Si ves los funtores como cosas que puede producir resultados, puedes pensar
que mapear algo sobre un funtor es como añadir una transformación al resultado
de ese funtor que modificará el resultado. Cuando hacemos
``fmap (+3) [1,2,3]``, añadimos la transformación ``(+3)`` al resultado
``[1,2,3]``, de forma que cada vez que encuentre un número en la lista
resultante, se le aplicará ``(+3)``. Otro ejemplo sería mapear sobre
funciones. Cundo hacemos ``fmap (+3) (*3)``, añadimos la transformación
``(+3)`` al resultado final de ``(*3)``. Verlo de este modo nos da un pista de
porque al usar ``fmap`` sobre funciones equivale a componer funciones
(``fmap (+3) (*3)`` es igual a ``(+3) . (*3)``, que equivale a
``\x -> ((x*3)+3)``), ya que si tomamos una función como ``(*3)`` le añadimos
la transformación ``(+3)`` a su resultado. Al final seguiremos teniendo una
función, solo que cuando le demos un número, primero se multiplicará por tres
y luego se le sumará tres, que es exactamente lo mismo que sucede con la
composición de funciones.







