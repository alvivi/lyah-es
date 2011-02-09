

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
ejemplo, ``Just 3`` tiene un resultado igual a ``3`` en el contexto de que
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


Funtores aplicativos
--------------------


.. image:: /images/present.png
   :align: right
   :alt: Ignora esta analogía.

En esta sección, daremos un vistazo a los funtores aplicativos, los cuales son
una especie de funtores aumentados, representados en Haskell por la clase de
tipos ``Applicative``, que se encuentra en ``Control.Applicative``.

Como ya sabes, las funciones en Haskell están currificadas por defecto, lo que
significa que las funciones que parecen que toman varios parámetros en
realidad solo toman un parámetro y devuelven una función que tomará el
siguiente parámetro y así sucesivamente. Si una función tiene el tipo
``a -> b -> c``, normalmente decimos que toma dos parámetros y devuelve un
``c``, pero en realidad toma un ``a`` y devuelve una función ``b -> c``. Por
este motivo podemos aplicar esta función como ``f x y`` o como ``(f x) y``.
Este mecanismo es el que nos permite aplicar parcialmente las funciones 
simplemente pasándoles menos parámetros de los que necesitan, de forma que
obtenemos nuevas funciones que probablemente pasaremos a otras funciones.

Hasta ahora, cuando mapeamos funciones sobre funtores, normalmente mapeamos
funciones que toman un solo parámetro. Pero ¿Qué sucede si mapeamos una
función como ``*``, que toma dos parámetros, sobre un funtor? Vamos a ver
varios ejemplo concretos. Si tenemos ``Just 3`` y hacemos
``fmap (*) (Just 3)`` ¿Qué obtenemos? A partir de la implementación de la
instancia ``Functor`` de ``Maybe``, sabemos que es un valor ``Just algo``,
aplicará la función ``*`` dentro de ``Just``. Así pues, al hacer
``fmap (*) (Just 3)`` obtenemos ``Just ((*) 3)``, que también puede escribirse
usando secciones como ``Just (* 3)`` ¡Interesante! ¡Ahora tenemos una función
dentro de un ``Just``!

.. code-block:: console

    ghci> :t fmap (++) (Just "hey")  
    fmap (++) (Just "hey") :: Maybe ([Char] -> [Char])  
    ghci> :t fmap compare (Just 'a')  
    fmap compare (Just 'a') :: Maybe (Char -> Ordering)  
    ghci> :t fmap compare "A LIST OF CHARS"  
    fmap compare "A LIST OF CHARS" :: [Char -> Ordering]  
    ghci> :t fmap (\x y z -> x + y / z) [3,4,5,6]  
    fmap (\x y z -> x + y / z) [3,4,5,6] :: (Fractional a) => [a -> a -> a]
    
Si mapeamos ``compare``, que tiene un tipo ``(Ord a) => a -> a -> Ordering``
sobre una lista de caracteres, obtenemos una lista de funciones del tipo
``Char -> Ordering``, ya que la función ``compare`` se aplica parcialmente
a cada uno de los caracteres de la lista. No es una lista de funciones
``(Ord a) => a -> Ordering``, ya que como el primer ``a`` ha sido fijado a
``Char`` el segundo también debe ser ``Char``.

Vemos que si aplicamos funciones con varios parámetros sobre funtores,
obtenemos funtores que contienen funciones. Así que ¿Qué podemos hacer ahora
con ellos? Bien, podemos mapear funciones que toman estas funciones como
parámetros sobre ellos, ya que cualquier cosa que este dentro de un funtor
será pasado a la función que mapeamos.

.. code-block:: console

    ghci> let a = fmap (*) [1,2,3,4]  
    ghci> :t a  
    a :: [Integer -> Integer]  
    ghci> fmap (\f -> f 9) a  
    [9,18,27,36]
    
Pero ¿Y si tenemos un valor funtor de ``Just (3 *)`` y un valor funtor de
``Just 5`` y queremos sacar la función de ``Just (3 *)`` y mapearla sobre
``Just 5``? Con los funtores normales no tendríamos mucha suerte, ya que lo
único que soportan es mapear funciones normales sobre funtores. Incluso aunque
mapeáramos ``\f -> f 9`` sobre un funtor que contuviese funciones, estaríamos
mapeando simples funciones normales. No podemos mapear funciones que están
dentro de un funtor sobre otro funtor con lo que nos ofrece ``fmap``.
Podríamos usar un ajuste de patrones con el constructor ``Just`` para extraer
la función y luego mapearla sobre ``Just 5``, pero estamos buscando algo
más abstracto, general y que funcione junto a los funtores.

Te presento la clase de tipos ``Applicative``. Reside en el módulo
``Control.Applicative`` y define dos métodos, ``pure`` y ``<*>``. No
proporciona ninguna implementación por defecto para ninguno de los dos, así
que tenemos que definir ambos si queremos que algo sea un funtor aplicativo.
La clase se define así: ::
    
    class (Functor f) => Applicative f where  
        pure :: a -> f a  
        (<*>) :: f (a -> b) -> f a -> f b  
    
Estas tres simples líneas nos dicen mucho. Vamos a empezar por la primera
línea. Empieza con la definición de la clase ``Applicative`` y también 
presenta una restricción de clase. Dice que si queremos que un constructor de
tipos forme parte de la clase de tipos ``Applicative``, tiene que ser primero
parte de clase ``Functor``. De este modo si sabemos que un constructor de
tipos es parte de la clase de tipos ``Applicative``, también lo es de
``Functor``, así que podemos usar ``fmap`` sobre él. 

El primer método que define se llama ``pure``. Su declaración de tipo es
``pure :: a -> f a``. ``f`` juega el papel del funtor aplicativo de la
instancia. Como Haskell tiene un buen sistema de tipos y como todo lo que
puede hacer una función es tomar un parámetro y devolver algún valor, podemos
deducir muchas cosas únicamente a partir de la declaración de tipos, y este
caso no es una excepción. ``pure`` debe tomar un valor de cualquier tipo y
devolver un funtor aplicativo que contiene ese valor. Cuando decimos
*que contiene*, estamos usando la analogía de la caja de nuevo, aunque ya
hemos visto que esta comparación no siempre es perfecta. Aun así, la
declaración ``a -> f a`` es bastante descriptiva. Tomamos un valor y lo
introducimos en un funtor aplicativo que contendrá ese valor como resultado.

Una forma mejor de entender ``pure`` sería decir que toma un valor y lo
introduce en una especie de contexto por defecto (o contexto puro), es decir,
el contexto mínimo para albergar ese valor.

La función ``<*>`` es realmente interesante. Tiene como declaración de tipo
``f (a -> b) -> f a -> f b`` ¿Te recuerda a algo? Por supuesto, 
``fmap :: (a -> b) -> f a -> f b``. Es una especie de ``fmap`` modificado.
Mientras que ``fmap`` toma una función y un funtor y aplica esa función dentro
del funtor, mientras que ``<*>`` toma un funtor que contenga una función y
otro funtor de forma que extrae esa función del primer funtor y la mapea sobre
el segundo funtor. Cuando decimos ``extrae``, en realidad es algo como ejecuta
y luego extrae, quizá incluso secuenciar. Lo veremos pronto.

Vamos a echar un vistazo a la implementación de la instancia ``Applicative``
de ``Maybe``. ::

    instance Applicative Maybe where  
        pure = Just  
        Nothing <*> _ = Nothing  
        (Just f) <*> something = fmap f something  
    
De nuevo, a partir de la definición de la clase venos que ``f`` toma el papel
funtor aplicativo que toma un tipo concreto como parámetro, así que escribimos
``instance Applicative Maybe where`` en lugar de
``instance Applicative (Maybe a) where``.

Antes de nada, ``pure``. Antes hemos dicho que se supone que éste toma algo
y lo introduce en un funtor aplicativo. Hemos escrito ``pure = Just``, ya que
los constructores de datos como ``Just`` son funciones normales. También
podríamos haber escrito ``pure x = Just x``. 

Luego, tenemos la definición de ``<*>``. No podemos extraer una función de
``Nothing``, ya que no hay nada dentro él. Así que decimos que si intentamos
extraer una función de un ``Nothing``, el resultado será ``Nothing``. Si vemos
la definición de clase de ``Applicative``, veremos que hay una restricción de
clase a ``Functor``, lo cual significa que podemos asumir que ambos parámetros
de ``<*>`` son funtores. Si el primer parámetro no es un ``Nothing``, si no un
``Just`` con una función en su interior, diremos que queremos mapear esa
función sobre el segundo parámetro. Esto también tiene en cuenta el caso en el
que el segundo parámetro sea ``Nothing``, ya que aplicar ``fmap`` con
cualquier función sobre ``Nothing`` devuelve ``Nothing``.

Así que para ``Maybe``, ``<*>`` extrae la función de su operando izquierdo si
es un ``Just`` y lo mapea sobre su operando derecho. Si alguno de estos
parámetros es ``Nothing``, ``Nothing`` será el resultado.

Vale, genial. Vamos a probarlo.

.. code-block:: console

    ghci> Just (+3) <*> Just 9  
    Just 12  
    ghci> pure (+3) <*> Just 10  
    Just 13  
    ghci> pure (+3) <*> Just 9  
    Just 12  
    ghci> Just (++"hahah") <*> Nothing  
    Nothing  
    ghci> Nothing <*> Just "woot"  
    Nothing
    
Vemos que tanto ``pure (+3)`` como ``Just (3)`` son iguales en este caso.
Utiliza ``pure`` cuando trabajes con valores ``Maybe`` en un contexto
aplicativo (es decir, cuando los utilices junto ``<*>``), de cualquier otro
modo sigue fiel a ``Just``. Las primeras cuatro líneas de entrada demuestran
como una función es extraída y luego mapeada, pero en este caso, podría haber
sido logrado simplemente mapeando funciones normales sobre funtores. La última
línea es interesante, ya que intentamos extraer una función de un ``Nothing``
y luego mapearla, lo cual es por supuesto ``Nothing``.

Con los funtores normales solo podemos mapear una función sobre un
funtor, luego no podemos extraer el resultado de forma general, incluso aunque
el resultado sea una función parcialmente aplicada. Los funtores aplicativos,
por otra parte, te permiten operar con varios funtores con una única función.
Mira esto:

.. code-block:: console

    ghci> pure (+) <*> Just 3 <*> Just 5  
    Just 8  
    ghci> pure (+) <*> Just 3 <*> Nothing  
    Nothing  
    ghci> pure (+) <*> Nothing <*> Just 5  
    Nothing
    
.. image:: /images/whale.png
   :align: right
   :alt: Ballenaaa.

¿Qué esta pasando aquí? Vamos a echar un vistazo paso a paso. ``<*>`` es
asociativo por la izquierda, por lo tanto ``pure (+) <*> Just 3 <*> Just 5``
es lo mismo que ``(pure (+) <*> Just 3) <*> Just 5``. Primero, la función
``+`` se introduce en un funtor, en este caso un valor ``Maybe`` que contiene
esa función. Así que al principio tenemos ``pure (+)`` que es lo mismo que
``Just (+)``. Luego tenemos ``Just (+) <*> Just 3``, cuyo resultado, debido a
que se aplica parcialmente la función, es ``Just (3+)``. Al aplicar ``3`` a la
función ``+`` obtenemos una nueva función que tomará un parámetro y le
añadirá 3. Para terminar, llegamos a ``Just (3+) <*> Just 5``, que resulta en
``Just 8``.

¿No es increíble? Los funtores aplicativos y el estilo aplicativo de hacer
``pure f <*> x <*> y <*> ...`` nos permiten tomar una función que espera
parámetros que no son necesariamente funtores y utilizarla para operar con
varios valores que están en algún contexto funtor. La función puede tomar
tantos parámetros como queramos, ya que será aplicada parcialmente paso a paso
cada vez que aparezca un ``<*>``.

Todo esto se vuelve más útil y aparente si consideramos el hecho de que
``pure f <*> x`` es igual a ``fmap f x``. Esta es una de la leyes aplicativas.
Las veremos en detalle más adelante, pero por ahora, podemos ver de forma
intuitiva su significado. Piensa un poco en ello, tiene sentido. Como ya hemos
dicho, ``pure`` inserta un valor en un contexto por defecto. Si todo lo que
hacemos es insertar una función en un contexto por defecto y luego la
extraemos para aplicarla a un valor contenido en un funtor aplicativo, es lo
mismo que simplemente mapear la función sobre ese funtor aplicativo. En
lugar de escribir ``pure f <*> x <*> y <*> ...`` podemos usar
``fmap f x <*> y <*> ...``. Por este motivo ``Control.Applicative`` exporta
una función llamada ``<$>``, que es simplemente ``fmap`` como operador infijo.
Así se define: ::

    (<$>) :: (Functor f) => (a -> b) -> f a -> f b  
    f <$> x = fmap f x
    
.. note:: Recuerda, las variables de tipo son independientes de los nombres de
          los parámetros o de otro nombres de valores. La ``f`` en la
          declaración de la función es una variable de tipo con una
          restricción de clase diciendo que cualquier constructor de tipos que
          reemplace a ``f`` de ser miembro de la clase ``Functor``. La ``f``
          que aparece en el cuerpo de la función representa la función que
          mapearemos sobre ``x``. El hecho de que usemos ``f`` para
          representar ambos no significa que representen lo mismo. 
          
El estilo aplicativo realmente destaca cuando utilizamos ``<$>``, ya que si
queremos aplicar un función ``f`` entre tres funtores aplicativos
podemos escribirlo así ``f <$> x <*> y <*> z``. Si los parámetros no fueran
funtores aplicativos sino valores normales, lo habríamos escrito así
``f x y z``.

Vamos a ver más de cerca como funciona. Tenemos un valor ``Just "johntra"`` y
un valor ``Just "volta"`` y queremos unirlos en una sola ``String`` dentro
de un funtor ``Maybe``. Hacemos esto:

.. code-block:: console

    ghci> (++) <$> Just "johntra" <*> Just "volta"  
    Just "johntravolta"
    
Antes de que veamos qué sucede aquí, compara lo anterior con esto:

.. code-block:: console

    ghci> (++) "johntra" "volta"  
    "johntravolta"

¡Bien! Para usar una función normal con funtores aplicativos, simplemente
tenemos que que esparcir unos cuantos ``<$>`` y ``<*>`` y la función
operará sobre funtores aplicativos ¿No es genial?

De cualquier modo, cuando hacemos
``(++) <$> Just "johntra" <*> Just "volta"``, primero ``(++)``, que tiene un
tipo ``(++) :: [a] -> [a] -> [a]``, se mapea sobre ``Just "johntra"``, lo cual
da como resultado un valor ``Just ("johntra"++)`` cuyo tipo es
``Maybe ([Char] -> [Char])``. Fíjate como el primer parámetro de ``(++)`` ha
desaparecido y que ``a`` se ha convertido en un ``Char``. Luego nos
encontramos con ``Just ("johntra"++) <*> Just "volta"``, que extrae la
función que se encuentra en el primer ``Just`` y la mapea sobre
``Just "volta"``, lo cual devuelve ``Just "johntravolta"``. Si alguno de los
dos valores hubiera sido ``Nothing``, el resultado habría sido ``Nothing``.

Hasta ahora, solo hemos usado ``Maybe`` en nuestros ejemplos y puede que estés 
pensado que los funtores aplicativos solo funcionan con ``Maybe``. Existen un
buen puñado de instancias de ``Applicative``, así que vamos a probarlas.

Las listas (en realidad, el constructor de tipos ``[]``) son funtores
aplicativos ¡Qué sorpresa! Aquí tienes la instancia de ``[]`` para
``Applicative``: ::

    instance Applicative [] where  
        pure x = [x]  
        fs <*> xs = [f x | f <- fs, x <- xs]
        
Antes dijimos que ``pure`` toma un valor y lo inserta en un contexto por
defecto. En otras palabras, un contexto mínimo que contenga ese valor. El
contexto mínimo para las listas sería la lista vacía, ``[]``, pero la lista
vacía representa el hecho de tener un valor, así que no puede mantener un
valor por si mismo. Por este motivo, ``pure`` toma un valor y lo introduce en
una lista unitaria. De forma similar, el contexto mínimo para el funtor
aplicativo de ``Maybe`` sería ``Nothing``, pero este representa el hecho de
no tener un valor, así que ``pure`` está implementado usando ``Just``.

.. code-block:: console

    ghci> pure "Hey" :: [String]  
    ["Hey"]  
    ghci> pure "Hey" :: Maybe String  
    Just "Hey"
    
¿Qué pasa con ``<*>``? Si vemos el tipo de ``<*>`` como si estuviera limitado
a las listas tendríamos algo como ``(<*>) :: [a -> b] -> [a] -> [b]``. Está
implementado usado :ref:`listas por comprensión <comprension>`. ``<*>`` debe
extraer de alguna forma la función que contiene el primer parámetro y mapearla
sobre el segundo parámetro. El problema es que aquí puede haber una función,
varias de ellas, o incluso ninguna. La lista de la derecha también puede
contener varios valores. Por este motivo se utiliza una lista por comprensión
para extraer valores de ambas listas. Aplicamos cada posible función de la
lista de la izquierda en cada posible valor de la lista de la derecha. El
resultado será una lista con cada posible combinación de aplicar una función
de la primera lista sobre un valor de la segunda lista.

.. code-block:: console

    ghci> [(*0),(+100),(^2)] <*> [1,2,3]  
    [0,0,0,101,102,103,1,4,9]
    
La lista de la izquierda tiene tres funciones y la lista de la derecha tiene
tres valores, así que el resultado tendrá nueve elementos. Cada función de la
lista de la izquierda se aplica a cada valor de la lista de la derecha. Si
tuviéramos funciones que tomen dos parámetros, podemos aplicar estas funciones
entre dos listas.

.. code-block:: console

    ghci> [(+),(*)] <*> [1,2] <*> [3,4]  
    [4,5,5,6,3,4,6,8]
    
Como ``<*>`` es asociativo por la izquierda, lo primero que se resuelve es
``[(+),(*)] <*> [1,2]``, que da como resultado una lista como esta
``[(1+),(2+),(1*),(2*)]``, ya que cada función de la lista de la izquierda se
aplica a cada valor de la lista de la derecha. Luego, se calcula 
``[(1+),(2+),(1*),(2*)] <*> [3,4]``, que devuelve el resultado anterior.

Usar el estilo aplicativo con listas es divertido. Mira:

.. code-block:: console

    ghci> (++) <$> ["ha","heh","hmm"] <*> ["?","!","."]  
    ["ha?","ha!","ha.","heh?","heh!","heh.","hmm?","hmm!","hmm."]
    
De nuevo, fíjate en que hemos usado una función normal que toma dos cadenas
entre dos funtores aplicativos de cadenas simplemente insertando los
operadores aplicativos apropiados.

Puedes ver las listas como computaciones no deterministas. Un valor como
``100`` o ``"que"`` puede ser visto como una computación determinista que solo
tienen un valor, mientras que una lista como ``[1,2,3]`` puede ser visto como
un computación que no puede decidir que resultado queremos, así que nos
muestra una lista con todos los resultados posibles. Así que cuando hacemos
algo como ``(+) <$> [1,2,3] <*> [4,5,6]``, puedes pensar que se trata de
sumar dos computaciones no deterministas con ``+``, para que produzca otra
computación no determinista que esté incluso menos segura de que valor es el
resultado final. 

El estilo aplicativo con listas suele ser un buen remplazo par la listas por
comprensión. En el segundo capítulo, queríamos saber todos los posibles
productos entre ``[2,5,10]`` y ``[8,10,11]``, así que hicimos esto: 

.. code-block:: console

    ghci> [ x*y | x <- [2,5,10], y <- [8,10,11]]     
    [16,20,22,40,50,55,80,100,110]
    
Simplemente extraemos valores de las dos listas y aplicamos una función para
combinar los elementos. Esto también se puede hacer usando el estilo
aplicativo: 

.. code-block:: console

    ghci> (*) <$> [2,5,10] <*> [8,10,11]  
    [16,20,22,40,50,55,80,100,110]
    
En mi opinión la segunda versión es más clara, ya que es más fácil de ver que
simplemente estamos aplicando ``*`` entre dos computaciones no deterministas.
Si quisiéramos todos los posibles productos entre ambas listas que fueran
mayores que 50, podríamos hacer algo como:

.. code-block:: console

    ghci> filter (>50) $ (*) <$> [2,5,10] <*> [8,10,11]  
    [55,80,100,110]
    
Es fácil de ver como ``pure f <*> xs`` es igual a ``fmap f xs`` con la listas.
``pure f`` es ``[f]`` y ``[f] <*> xs`` aplicará cada función que esté en la
primera lista sobre cada valor que este en la segunda lista, pero solo hay una
función en la lista de la izquierda, así que es como un ``fmap``. 

Otra instancia de ``Applicative`` con la que ya nos hemos encontrado es
``IO``. Así es como se implementa: ::

    instance Applicative IO where  
        pure = return  
        a <*> b = do  
            f <- a  
            x <- b  
            return (f x)
            
.. image:: /images/knight.png
   :align: left
   :alt: ¡Jajaja!

Como todo lo que hace ``pure`` es insertar un valor en un contexto mínimo que
pueda albergar ese valor, tiene sentido que ``pure`` sea simplemente
``return``, ya que ``return`` hace exactamente eso: crea una acción de E/S 
que no hace nada, simplemente tiene como resultado el valor que le pasemos,
pero en realidad no ejecuta ninguna operación de E/S como mostrar texto por
un terminal o leer algo de algún archivo.

Si ``<*>`` fuera exclusivo para ``IO`` su tipo sería
``(<*>) :: IO (a -> b) -> IO a -> IO b``. Tomaría una acción de E/S que
devuelve una función como resultado y otra acción de E/S y crearía una una
nueva acción de E/S a partir de estas dos, que cuando fuera ejecutada, primero
ejecutaría la primera acción para obtener la función y luego ejecutaría la
segunda acción para obtener un valor que luego aplicaría a la primera función
para obtener el resultado de la acción que crea. Hemos utilizado la *sintaxis
do* para implementarlo. Recuerda que la *sintaxis do* trata de tomar varias
acciones de E/S y unirlas en una sola, que es exactamente lo que hacemos aquí.

Con ``Maybe`` y ``[]``, podemos que pensar que ``<*>`` simplemente extrae una
función de su parámetro izquierdo y luego lo aplica al de la derecha. Con
``IO``, seguimos extrayendo una función, pero ahora también existe una
*secuenciación* , ya que estamos tomando dos acciones de E/S y las estamos
secuenciando, o uniéndolas, en una sola acción. Hay que extraer una función
de la primera acción de E/S, pero para extraer un resultado de una acción de
E/S, primero tiene que ser ejecutada.

Considera esto: ::

    myAction :: IO String  
    myAction = do  
        a <- getLine  
        b <- getLine  
        return $ a ++ b

Esta acción de E/S preguntará al usuario por dos líneas de texto y las
devolverá concatenadas. Esto se consigue gracias a que hemos unido dos 
acciones de E/S ``getLine`` y un ``return``, ya que queríamos una nueva acción
de E/S que contuviera el resultado ``a ++ b++``. Otra forma de escribir esto
sería usando el estilo aplicativo. ::

    myAction :: IO String  
    myAction = (++) <$> getLine <*> getLine
    
Lo que hacíamos antes era crear una acción de E/S que aplicará una función
entre los resultados de otras dos acciones de E/S, y esto es exactamente lo
mismo. Recuerda, ``getLine`` es una acción de E/S con el tipo
``getLine :: IO String``. Cuando utilizamos ``<*>`` entre dos funtores
aplicativos, el resultado es un funtor aplicativo, así que parece que tiene
sentido.

Si volvemos a la analogía de la caja, podemos imaginar a ``getLine`` como 
una caja que viajará al mundo real y nos traerá una cadena. Al hacer
``(++) <$> getLine <*> getLine`` creamos un nueva caja más grande, que
enviará esas dos cajas para obtener las dos líneas de la terminal y devolver
la concatenación de ambas como resultado.

El tipo de la expresión ``(++) <$> getLine <*> getLine`` es ``IO String``,
esto quiere decir que esta expresión es una acción de E/S normal y corriente
que también contiene un resultado, al igual que todas las demás acciones de
E/S. Por esta razón podemos hacer cosas como esta: ::

    main = do  
        a <- (++) <$> getLine <*> getLine  
        putStrLn $ "Las dos líneas concatenadas son: " ++ a
        
Si alguna vez te encuentras ligando una acción de E/S a algún nombre y luego 
utilizas una función sobre ella para luego devolver ese valor como resultado
usando ``return``, considera utilizar el estilo aplicativo ya que es sin
duda alguna más conciso.

Otra instancia de ``Applicative`` es ``(-> r)``, es decir, funciones. No es
una instancia muy utilizada, pero sigue siendo interesante como aplicativo,
así que vamos a ver como se implementa. 

.. note:: Si estas confudido acerca del significado de ``(-> r)``, revisa la
          sección anterior donde explicamos como ``(-> r)`` es un funtor.
          
::

    instance Applicative ((->) r) where  
        pure x = (\_ -> x)  
        f <*> g = \x -> f x (g x)
        
Insertamos un valor dentro de un funtor aplicativo con ``pure``, el resultado
que devuelva éste siempre debe ser el valor anterior. El contexto mínimo que
siga conteniendo ese valor como resultado. Por este motivo en la
implementación de la instancia funtor de las funciones, ``pure`` toma un valor
y crea una función que ignora su parámetro y devuelve siempre ese mismo valor.
Si vemos el tipo de ``pure``, pero restringido al tipo de la instancia
``(-> r)``, sería ``pure :: a -> (r -> a)``.

.. code-block:: console

    ghci> (pure 3) "blah"  
    3  
    
Gracias a la currificación, la aplicación de funciones es asociativa por la
izquierda, así que podemos omitir los paréntesis.

.. code-block:: console

    ghci> pure 3 "blah"  
    3  
    
La implementación de la instancia para ``<*>`` es un poco críptica, así que
será mejor si simplemente vemos un ejemplo de como utilizar las funciones como
funtores aplicativos en estilo aplicativo:

.. code-block:: console

    ghci> :t (+) <$> (+3) <*> (*100)  
    (+) <$> (+3) <*> (*100) :: (Num a) => a -> a  
    ghci> (+) <$> (+3) <*> (*100) $ 5  
    508

Al llamar ``<*>`` con dos funtores aplicativos obtenemos otro funtor
aplicativo, así que si utilizamos dos funciones, obtenemos de nuevo una
función. Así que, ¿Qué sucede aquí? Cuando hacemos
``(+) <$> (+3) <*> (*100)``, creamos una función que utilizará ``+`` en los
resultados de ``(+3)`` y ``(*100)`` y devolverá ese resultado. Para demostrar
este ejemplo real, hemos hecho ``(+) <$> (+3) <*> (*100) $ 5``, el primer
``5`` se aplica a ``(+3)`` y ``(*100)``, obteniendo ``8`` y ``500``. Luego, se
llama a ``+`` con ``8`` y ``500``, obteniendo ``508``.

.. code-block:: console

    ghci> (\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5  
    [8.0,10.0,2.5]
    
.. image:: /images/jazzb.png
   :align: right
   :alt: SLAP
   
Lo mismo. Hemos creado una función que llamará a ``\x y z -> [x,y,z]`` con los
resultados finales de ``(+3)``, ``(*2)`` y ``(/2)``. El ``5`` será pasado a
estas tres funciones y luego se llamará a ``\x y z -> [x, y, z]`` con los
resultados.

Puedes verlo como si las funciones fueran cajas que contienen los resultados
finales, así que si hacemos ``k <$> f <*> g`` se crea una función que llamará
a ``k`` con los resultados de ``f`` y ``g``. Cuando hacemos algo como
``(+) <$> Just 3 <*> Just 5``, estamos usando ``+`` en valores que pueden
estar ahí o no, por lo tanto el resultado será un valor o ninguno. Cuando
hacemos algo como ``(+) <$> (+10) <*> (+5)``, estamos usando ``+`` en los
futuros resultados de las funciones ``(+10)`` y ``(+5)``, y el resultado 
también será algo que producirá un valor siempre y cuando sea llamado con un
parámetro. 

No solemos utilizar las funciones como funtores aplicativos, pero siguen
siendo interesantes. Tampoco es muy importante que no entiendas como funciona
la instancia de las funciones para los funtores aplicativos, así que no te
preocupes mucho. Intenta jugar un poco con el estilo aplicativo y las
funciones para hacerte una idea de como funionan.

Una instancia de ``Applicative`` que aún no nos hemos encontrado es
``ZipList`` y reside en ``Control.Applicative``.

Este tipo sugiere que en realidad hay mas formas de utilizar las listas como
funtores aplicativos. Una forma es la que ya hemos visto, cuando utilizamos
``<*>`` con una lista de funciones y una lista de valores devuelve una lista
de todas las posibles combinaciones de aplicar esas funciones de la lista de
la izquierda a los valores de la derecha. Si hacemos algo como
``[(+3),(*2)] <*> [1,2]``, ``(+3)`` será aplicado a ``1`` y ``2`` y ``(*2)``
también será aplicado a ambos, por lo que obtendremos una lista con cuatro
elementos, ``[4,5,2,4]``.

Sin embargo, ``[(+3),(*2)] <*> [1,2]`` también podría funcionar de forma que
la primera función de la izquierda fuera aplicada a el primer valor de la
derecha y la segunda función fuera aplicada al segundo valor. Esto nos daría
una lista con dos valores, ``[4,4]``. Lo podríamos ver como
``[1 + 3, 2 * 2]``.

Como un mismo tipo no puede tener dos instancias para una misma clase de
tipos, se utiliza el tipo ``ZipList a``, que tiene un constructor ``ZipList``
con un solo campo, la lista. Aquí esta la instancia: ::

    instance Applicative ZipList where  
            pure x = ZipList (repeat x)  
            ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)
            
.. note:: Sí, también sería válido ``ZipList (zipWith ($) fs xs)``.

``<*>`` hace lo que acabamos de explicar. Aplica la primera función a el
primer valor, la segunda función al segundo valor, etc. Esto se consigue con
``zipWith (\f x -> f x) fs xs``. Debido a como funciona ``zipWith``, la lista
final será tan larga como la lista más corta de las dos. 

``pure`` es bastante interesante. Toma un valor y lo introduce en una lista 
que tiene ese valor repetido indefinidamente. ``pure "jaja"`` devolvería algo
como ``ZipList (["jaja","jaja","jaja"...``. Quizá esto sea algo confuso ya que
hemos dicho que ``pure`` debe introducir un valor en el contexto mínimo que
albergue ese valor. Y quizá estés pensado que una lista infinita difícilmente
es un contexto mínimo. Pero tiene sentido con esta listas, ya que tiene que
producir un valor en cada posición. De esta forma también se cumple la ley que
dice que ``pure f <*> xs`` debe ser igual a ``fmap f xs``. Si ``pure 3`` solo
devolviera ``ZipList [3]``, ``pure (*2) <*> ZipList [1,5,10]`` devolvería
``ZipList [2]``, ya que la lista resultante es tan larga como la mas corta de
las dos que utilizamos como parámetros. Si utilizamos una lista infinita y
otra finita, la lista resultante siempre tendrá el tamaño de la lista finita.

¿Cómo funcionan estas listas al estilo aplicativo? Veamos. El tipo
``ZipList a`` no tiene una instancia para ``Show``, así que tenemos que
utilizar la función :cpp:member:`getZipList` para extraer una lista primitiva.

.. code-block:: console

    ghci> getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100,100]  
    [101,102,103]  
    ghci> getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100..]  
    [101,102,103]  
    ghci> getZipList $ max <$> ZipList [1,2,3,4,5,3] <*> ZipList [5,3,1,2]  
    [5,3,3,4]  
    ghci> getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"  
    [('d','c','r'),('o','a','a'),('g','t','t')]
    
.. note:: La función ``(,,)`` es lo mismo que ``\x y z -> (x,y,z)``. También,
          la función ``(,)`` sería igual a ``\x y -> (x,y)``.
          
A parte de ``zipWith``, la biblioteca estándar también tiene funciones como
``zipWith3``, ``zipWith4``, y todas las demás hasta llegar a 7. ``zipWith``
toma una función que tome dos parámetros y une dos los listas con esta
función. ``zipWith3`` toma una función que tome tres parámetros y une tres
listas con ella. Gracias a estas listas y al estilo aplicativo, no tenemos
que tener una función distinta para cada número de listas que queramos unir
con una función. Lo único que tenemos que hacer es utilizar el estilo
aplicativo.

``Control.Applicative`` define una función llamada :cpp:member:`liftA2`, cuyo
tipo es ``liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c``.
Se define así: ::

    liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c  
    liftA2 f a b = f <$> a <*> b
    
Nada especial, simplemente aplica una función entre dos funtores aplicativos,
escondiendo el estilo aplicativo al que nos hemos acostumbrado. La razón por
la cual lo mostramos es para hacer más evidente porque los funtores
aplicativos son más potentes que los funtores ordinarios. Con lo funtores
ordinarios solo podemos mapear funciones sobre un funtor. Pero con los
funtores aplicativos, podemos aplicar una función con varios funtores. También
es interesante ver la el tipo de la función como
``(a -> b -> c) -> (f a -> f b -> f c)``. Si lo vemos de esta forma, podemos
decir que ``liftA2`` toma una función binaria normal y la desplaza para que
opere con dos funtores. 

Un concepto interesante: podemos tomar dos funtores aplicativos y combinarlos
en un único funtor aplicativo que contenga los resultados de ambos funtores
en forma de lista. Por ejemplo, tenemos ``Just 3`` y ``Just 4``. Vamos a
asumir que el segundo está dentro de una lista unitaria, lo cual es realmente
fácil de conseguir: 

.. code-block:: console

    ghci> fmap (\x -> [x]) (Just 4)  
    Just [4]
    
Vale, ahora tenemos ``Just 3`` y ``Just [4]`` ¿Cómo obtendríamos
``Just [3,4]``? Fácil. 

.. code-block:: console

    ghci> liftA2 (:) (Just 3) (Just [4])  
    Just [3,4]  
    ghci> (:) <$> Just 3 <*> Just [4]  
    Just [3,4]
    
Recuerda, ``:`` es una función que toma un elemento y una lista y devuelve una
lista nueva con dicho elemento al principio. Ahora que tenemos ``Just [3,4]``,
¿podríamos combinarlos con ``Just 2`` para obtener ``Just [2,3,4]``? Por
supuesto que podríamos. Parece que podemos combinar cualquier cantidad de
funtores aplicativos en uno que contenga una lista con los resultados de
dichos funtores. Vamos a intentar implementar una función que tome una lista
de funtores aplicativos y devuelva un funtor aplicativo que contenga una lista
con los resultados de los funtores. La llamaremos ``sequenceA``. ::

    sequenceA :: (Applicative f) => [f a] -> f [a]  
    sequenceA [] = pure []  
    sequenceA (x:xs) = (:) <$> x <*> sequenceA xs
    
¡Ahh, recursión! Primero, veamos su tipo. Transformará una lista funtores
aplicativos en un funtor aplicativo con un lista. Esto nos da alguna pista
para el caso base. Si queremos convertir una lista vacía en un funtor
aplicativo con una lista que contenga los resultados, simplemente insertamos
la lista en el contexto mínimo. Luego viene la recursión. Si tenemos una lista
con una cabeza y una cola (recuerda, ``x`` es un funtor aplicativo y ``xs`` es
una lista de ellos), llamamos a ``sequenceA`` con la cola para que nos
devuelva un funtor aplicativo que contenga una lista. Luego, anteponemos el
valor que contiene el funtor aplicativo ``x`` en la lista ¡Y listo!

Si hiciéramos ``sequenceA [Just 1, Just 2]``, tendríamos
``(:) <$> Just 1 <*> sequenceA [Just 2]``, que es igual a
``(:) <$> Just 1 <*> ((:) <$> Just 2 <*> sequenceA [])``. Sabemos que
``sequenceA []`` acabará siendo ``Just []``, así que ahora tendríamos
``(:) <$> Just 1 <*> ((:) <$> Just 2 <*> Just [])``, que es igual a
``(:) <$> Just 1 <*> Just [2]``, que es igual a ``Just [1,2]``.

Otra forma de implementar ``sequenceA`` es con un pliegue. Recuerda, casi
cualquier función en la que recorramos una lista elemento a elemento y vayamos
acumulando un resultando a lo largo del camino puede ser implementada con
un pliegue. ::

    sequenceA :: (Applicative f) => [f a] -> f [a]  
    sequenceA = foldr (liftA2 (:)) (pure [])
    
Empezamos recorriendo la lista por la izquierda y con un acumulador inicial
igual a ``pure []``. Aplicamos ``liftA2 (:)`` entre el acumulador y el último
elemento de la lista, lo cual resulta en un funtor aplicativo que contiene
una lista unitaria. Luego volvemos a aplicar ``liftA2 (:)`` con el último
elemento actual de la lista con el acumulador actual, y así sucesivamente
hasta que solo nos quedemos con el acumulador, que contendrá todos los
resultados de los funtores aplicativos. 

Vamos a probar nuestra función.

.. code-block:: console

    ghci> sequenceA [Just 3, Just 2, Just 1]  
    Just [3,2,1]  
    ghci> sequenceA [Just 3, Nothing, Just 1]  
    Nothing  
    ghci> sequenceA [(+3),(+2),(+1)] 3  
    [6,5,4]  
    ghci> sequenceA [[1,2,3],[4,5,6]]  
    [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]  
    ghci> sequenceA [[1,2,3],[4,5,6],[3,4,4],[]]  
    []
    
Precioso. Cuando lo utilizamos con los valores ``Maybe``, ``sequenceA`` crea
un valor ``Maybe`` con todos los resultados dentro de una lista. Si alguno
de los valores es ``Nothing``, entonces el resultado final también lo es. Esto
puede ser útil cuando tenemos una lista de valores ``Maybe`` y estamos
interesados en obtener esos valores solo si ninguno de ellos es ``Nothing``.

Cuando se utiliza con funciones, ``sequenceA`` toma una lista de funciones y
devuelve una función cuyo resultado es una lista. En el ejemplo anterior, 
creamos una función que tomará un número como parámetro, se aplica a cada una
de las funciones de la lista y luego devuelve una lista con los resultados.
``sequenceA [(+3),(+2),(+1)] 3`` llamará a ``(+3)`` con ``3``, a ``(+2)`` con
``3`` y a ``(+1)`` con ``3``, luego devolverá una lista con todos los
resultados.

Si hacemos ``(+) <$> (+3) <*> (*2)`` estamos creando una función que toma
un parámetro, lo aplica a ``(+3)`` y a ``(*2)`` y luego llama a ``+`` con
ambos resultados. Del mismo modo, si hacemos ``sequenceA [(+3),(*2)]`` estamos
creando una función que tomará un parámetro y lo aplicará a las funciones de
la lista. Pero, en lugar de llamar a ``+`` con los resultados de las
funciones, se utiliza una combinación de ``:`` y ``pure []`` para unir todos
esos resultados en una lista.

``sequenceA`` puede ser útil cuando tenemos una lista de funciones y queremos
aplicarlas todas al mismo parámetro y luego tener los resultados en una lista.
Por ejemplo, si tenemos un número y queremos saber si satisface todos los
predicados que contiene una lista. Una forma de hacerlo sería así:

.. code-block:: console

    ghci> map (\f -> f 7) [(>4),(<10),odd]  
    [True,True,True]  
    ghci> and $ map (\f -> f 7) [(>4),(<10),odd]  
    True
    
Recuerda que ``and`` toma una lista de booleanos y devuelve ``True`` si son
todos ``True``. Otra forma de hacer lo mismo sería con ``sequenceA``:

.. code-block:: console

    ghci> sequenceA [(>4),(<10),odd] 7  
    [True,True,True]  
    ghci> and $ sequenceA [(>4),(<10),odd] 7  
    True
    
``sequenceA [(>4),(<10),odd]`` crea una función que tomará un número y lo 
aplicará a todos los predicados de la lista, ``[(>4),(<10),odd]``, y devolverá
una lista con los resultados. Dicho de otra forma, convierte una lista de tipo
``(Num a) => [a -> Bool]`` en una función cuyo tipo sería
``(Num a) => a -> [Bool]`` ¿Tiene buena pinta, no?

Ya que las listas son homogéneas, todas las funciones de la lista deben tener
el mismo tipo. No podemos tener una lista como ``[ord, (+3)]``, porque ``ord``
toma un carácter y devuelve un número, mientras que ``(+3)`` toma un número y
devuelve otro número.

Cuando se utiliza con ``[]``, ``sequenceA`` toma una lista y devuelve una
lista de listas. Mmm... interesante. En realidad crea una lista que contiene
todas las combinaciones posibles de sus elementos. A título de ejemplo aquí
tienes unos cuantos usos de ``sequenceA`` con sus equivalentes usando listas
por comprensión:

.. code-block:: console

    ghci> sequenceA [[1,2,3],[4,5,6]]  
    [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]  
    ghci> [[x,y] | x <- [1,2,3], y <- [4,5,6]]  
    [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]  
    ghci> sequenceA [[1,2],[3,4]]  
    [[1,3],[1,4],[2,3],[2,4]]  
    ghci> [[x,y] | x <- [1,2], y <- [3,4]]  
    [[1,3],[1,4],[2,3],[2,4]]  
    ghci> sequenceA [[1,2],[3,4],[5,6]]  
    [[1,3,5],[1,3,6],[1,4,5],[1,4,6],[2,3,5],[2,3,6],[2,4,5],[2,4,6]]  
    ghci> [[x,y,z] | x <- [1,2], y <- [3,4], z <- [5,6]]  
    [[1,3,5],[1,3,6],[1,4,5],[1,4,6],[2,3,5],[2,3,6],[2,4,5],[2,4,6]]
    
Quizá esto es un poco difícil de entender, pero si jugamos un poco con ellos,
veremos como funciona. Digamos que tenemos ``sequenceA [[1,2],[3,4]]``. Para
ver lo que sucede, vamos a utilizar la definición
``sequenceA (x:xs) = (:) <$> x <*> sequenceA xs`` de ``sequenceA`` y el caso
base ``sequenceA [] = pure []``. No tienes porque seguir esta traza, pero si
no consigues imaginarte como funciona ``sequenceA`` con las listas puede que
te resulte de ayuda. 

 * Empezamos con ``sequenceA [[1,2],[3,4]]``.
 * Lo cual se evalúa a ``(:) <$> [1,2] <*> sequenceA [[3,4]]``.
 * Si evaluamos el ``sequenceA`` interior una vez más, obtenemos
   ``(:) <$> [1,2] <*> ((:) <$> [3,4] <*> sequenceA [])``.
 * Ahora hemos alcanzado el caso base, así que tenemos
   ``(:) <$> [1,2] <*> ((:) <$> [3,4] <*> [[]])``.
 * Evaluamos la parte ``(:) <$> [3,4] <*> [[]]``, que utilizará ``:`` con cada
   posible valor de la lista de la izquierda  (es decir ``3`` y ``4``) con
   cada posible valor de la lista de la derecha (``[]``), obteniendo así
   ``[3:[], 4:[]]``, que es ``[[3],[4]]``. Así que ahora tenemos
   ``(:) <$> [1,2] <*> [[3],[4]]``.
 * ``:`` se utiliza con cada posible valor de la lista de la izquierda (``1``
   y ``2``) con cada posible valor de la lista de la derecha (``[3]`` y
   ``[4]``), de forma que nos quedamos con ``[1:[3], 1:[4], 2:[3], 2:[4]]``,
   que es igual a ``[[1,3],[1,4],[2,3],[2,4]``. 
   
Si hacemos ``(+) <$> [1,2] <*> [4,5,6]`` estamos creando una computación
no determinista ``x + y`` donde ``x`` toma cualquier valor de ``[1,2]`` e 
``y`` toma cualquier valor de ``[4,5,6]``. Representamos la solución con una
lista con todos los posibles resultados. De forma similar, si hacemos
``sequence [[1,2],[3,4],[5,6],[7,8]]`` estamos creando una computación no
determinista ``[x,y,z,w]``, donde ``x`` toma cualquier valor de ``[1,2]``,
``y`` toma cualquier valor de ``[3,4]``, y así sucesivamente. Representamos el
resultado de la computación no determinista utilizando una lista, donde cada
elemento es una lista posible. Por este motivo el resultado es una lista de
listas.

Con acciones de E/S, ``sequenceA`` se comporta igual que ``sequence``. Toma
una lista de acciones de E/S y devuelve una acción de E/S que ejecutará cada
una de esas acciones y tendrá como resultado una lista con los resultados de
todas esas acciones. Por este motivo para convertir un valor ``[IO a]`` en
un valor ``IO [a]``, o dicho de otra forma, para crear una acción de E/S que
devuelva una lista de resultados cuando sea ejecutada, todas estas acciones
tienen que ser secuenciadas de forma que sean ejecutadas unas detrás de otra
cuando se fuerce la evaluación. No puede obtener el resultado de una acción de
E/S si no la ejecutas primero.

.. code-block:: console

    ghci> sequenceA [getLine, getLine, getLine]  
    heyh  
    ho  
    woo  
    ["heyh","ho","woo"]
    
Al igual que los funtores normales, los funtores aplicativos vienen con una
serie de leyes. La mas importante de todas es la que ya hemos mencionado,
:js:data:`pure f <*> x = fmap f x`. Como ejercicio, puedes intentar comprobar
esta ley en algunos de los funtores de los que hemos hablado. Las otras leyes
son:

 * :js:data:`pure id <*> v = v`
 * :js:data:`pure (.) <*> u <*> v <*> w = u <*> (v <*> w)`
 * :js:data:`pure f <*> pure x = pure (f x)`
 * :js:data:`u <*> pure y = pure ($ y) <*> u`

No vamos a verlas en detalle ahora mismo ya que nos tomaría unas cuantas
página y probablemente sea algo aburrido, pero, si te sientes con ganas,
puedes echarles una vistazo más de cerca y comprobar si algunas de las
instancias que hemos visto las cumplen.

Concluyendo, los funtores aplicativos no son solo interesantes, sino que
también son útiles, ya que nos permiten combinar diferentes computaciones,
como computaciones de E/S, computaciones no deterministas, computaciones que
puede fallar, etc. utilizando el estilo aplicativo. Simplemente utilizando
``<$>`` y ``<*>`` podemos utilizar funciones normales para que operen de forma
uniforme con cualquier número de funtores aplicativos y tomar ventaja de la
semántica de cada uno.
 

La palabra clave newtype
------------------------


.. image:: /images/maoi.png
   :align: left
   :alt: ¿Por qué estas tan serio?

Hasta ahora hemos creado nuestros propios tipos de datos algebraicos
utilizando el palabra clave ``data``. También hemos visto como dar sinónimos
de tipos ya existentes utilizando la palabra clave ``type``. En esta sección,
veremos como crear nuevos tipos de datos a partir de tipos datos ya existentes
utilizando la palabra clave ``newtype`` y el porqué de hacerlo de este modo.

En la sección anterior vimos que en realidad hay más de una forma para que una
lista sea un funtor aplicativo. Una manera es que ``<*>`` tome cada función de
la lista que se le pasa como parámetro izquierdo y la aplique a cada valor que
contenga la lista de la derecha, de forma que devuelva todas las posibles
combinaciones de aplicar una función de la izquierda con un valor de la
derecha. 

.. code-block:: console

    ghci> [(+1),(*100),(*5)] <*> [1,2,3]  
    [2,3,4,100,200,300,5,10,15]
    
La segundo forma es que tome la primera función de la lista de la izquierda de
``<*>`` y la aplique a el primer valor de la lista de la derecha, luego tomará
la segunda función de la lista izquierda y la aplicara al segundo valor de la
lista derecha, y así sucesivamente. Al final es algo como unir dos listas en
una. Pero las listas ya tienen una instancia para ``Applicative``, así que
¿Cómo hemos creado una segunda instancia de ``Applicative``? Si haces memoria,
recordarás que dijimos que el tipo ``ZipList a`` se utilizaba por este motivo,
el cual tiene un constructor de datos, ``ZipList``, con un solo campo. Pusimos
la lista con la que íbamos a trabajar en ese campo. Luego, como ``ZipList``
tenia su propia instancia de ``Applicative``, el comportamiento de las listas
como funtores aplicativos era diferente. Solo teníamos que utilizar el
constructor ``ZipList`` con la lista y cuando termináramos debíamos usar
``getZipList`` para recuperarla.

.. code-block:: console

    ghci> getZipList $ ZipList [(+1),(*100),(*5)] <*> ZipList [1,2,3]  
    [2,200,15]
    
Y bien ¿Qué tiene que ver todo esto con la palabra clave ``newtype``? Bueno,
piensa un poco en como deberíamos declarar el tipo de datos ``ZipList a``. Una
forma sería así: ::

    data ZipList a = ZipList [a]  
    
Un tipo que solo tiene un constructor de datos y este constructor solo tiene
un campo el cual es una lista de cosas. También podríamos utiliza la sintaxis
de registro para obtener de forma automática una función que extraiga la lista
de un ``ZipList``: ::

    data ZipList a = ZipList { getZipList :: [a] }  
    
Todo esto parece correcto y de hecho funciona bien. Simplemente hemos
utilizado la palabra clave ``data`` para insertar un tipo dentro dentro de
otro y así poder crear una segunda instancia de el tipo original.

En Haskell, la palabra clave ``newtype`` se utiliza exactamente para estos
casos en los que simplemente queremos insertar un tipo dentro de otro para que
parezca un tipo distinto. En realidad, ``ZipList`` se define así: ::

    newtype ZipList a = ZipList { getZipList :: [a] }  
    
Se utiliza ``newtype`` en lugar de ``data``. Y ¿Por qué? Te estarás
preguntando. Muy sencillo, ``newtype`` es más rápido. Si utilizamos la palabra
clave ``data`` para insertar un tipo dentro de otro, se genera cierta
sobrecarga cuando el programa se ejecuta debido a las operaciones que insertan
y extraen el tipo. Pero si utilizamos ``newtype``, Haskell sabe que lo estamos
utilizando para insertar un tipo existente en un nuevo tipo (de ahí viene el
nombre). En realidad lo que buscamos es que internamente sean iguales pero que
su tipo sea distinto. Teniendo esto en cuenta, Haskell puede deshacerse de las
operaciones de inserción y extracción una vez sepa de que tipo es cada valor.
 
Entonces ¿Por qué no utilizamos siempre ``newtype`` en lugar de ``data``?
Cuando creamos un nuevo tipo a partir de uno ya existente utilizando la
palabra clave ``newtype``, solo podemos utilizar un constructor de datos y
éste solo puede tener un campo. Mientras que con ``data`` podemos tener varios
constructores de datos y cada uno de ellos con cero o varios campos. ::

    data Profession = Fighter | Archer | Accountant  

    data Race = Human | Elf | Orc | Goblin  

    data PlayerCharacter = PlayerCharacter Race Profession
    
Cuando utilizamos ``newtype`` estamos restringidos a utilizar a utilizar un
solo constructor con un solo campo.

También podemos utilizar la palabra clave ``deriving`` con ``newtype`` de la
misma forma que hacemos con ``data``. Podemos derivar las instancias de
``Eq``, ``Ord``, ``Enum``, ``Bounded``, ``Show`` y ``Read``. Si derivamos la
instancia de una clase de tipos, el tipo original tiene que ser miembro de
dicha clase de tipos. Tiene sentido, ya que ``newtype`` solo sustituye a un
tipo existente. Si tenemos el siguiente código, podríamos mostrar por pantalla
y equiparar valores del nuevo tipo: ::

    newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)  
    
Vamos a probarlo: 

.. code-block:: console

    ghci> CharList "this will be shown!"  
    CharList {getCharList = "this will be shown!"}  
    ghci> CharList "benny" == CharList "benny"  
    True  
    ghci> CharList "benny" == CharList "oisters"  
    False

En este caso en particular, el constructor de datos tiene el siguiente tipo:
::
    
    CharList :: [Char] -> CharList  
    
Toma un valor del tipo ``[Char]``, como ``"My Sharona"`` y devuelve un valor
del tipo ``CharList``. En el ejemplo anterior lo podemos ver en
funcionamiento. Po el contrario, la función ``getCharList``, que ha sido
generada automáticamente gracias al uso de la sintaxis de registro, tiene este
tipo: ::

    getCharList :: CharList -> [Char]  
    
Toma un valor del tipo ``CharList`` y devuelve uno del tipo ``[Char]``. Estas
son las operaciones de inserción y extracción de las que antes hablábamos,
aunque también puedes verlo como una transformación de un tipo a otro. Gracias
a las propiedades de ``newtype``, estas operaciones no tendrán ningún coste
en tiempo de ejecución. 


Utilizando newtype para crear instancias de clase
'''''''''''''''''''''''''''''''''''''''''''''''''

A menudo queremos crear instancias de nuestros tipos para ciertas clases
de tipos, pero los parámetros de tipo no encajan en lo que queremos hacer. Es
muy fácil crear una instancia de ``Maybe`` para ``Functor``, ya que la clase
de tipos ``Functor`` se define como: ::

    class Functor f where  
        fmap :: (a -> b) -> f a -> f b
        
Así que simplemente tenemos que hacer esto: ::

    instance Functor Maybe where   
    
E implementar ``fmap``. Todos los parámetros de tipo encajan porque ``Maybe``
toma el lugar de ``f`` en la definición de la clase de tipos ``Functor``, de
forma que si vemos el tipo de ``fmap`` como si solo funcionara para ``Maybe``
quedaría así: ::

    fmap :: (a -> b) -> Maybe a -> Maybe b  
    
.. image:: /images/krakatoa.png
   :align: right
   :alt: ¡Muy peligroso!
 
Ahora ¿Qué pasaría si quisiéramos crear una instancia para ``Functor`` para 
las duplas de forma que cuando utilizamos ``fmap`` con una función sobre una
dupla, se aplicara la función al primer componente de la dupla? De este modo,
si hiciéramos algo como ``fmap (+3) (1,1)`` obtendríamos ``(4,1)``. Pues
resulta que escribir una instancia para lograr este comportamiento no es tan
sencillo. Con ``Maybe`` solo teníamos que utilizar
``instance Functor Maybe where`` porque solo los constructores de tipos que
toman exactamente un parámetro pueden crear una instancia para la clase
``Functor``. Pero parece que no hay ninguna forma de hacer que algo como que
el parámetro ``a`` de ``(a,b)`` acabe siendo el que cambie cuando utilicemos
``fmap``. Para solucionarlo, podemos utilizar ``newtype`` con las duplas de
forma que el segundo parámetro de tipo represente el primer parámetro de tipo
de las duplas: ::

    newtype Pair b a = Pair { getPair :: (a,b) }  
    
Y ahora podemos hacer la instancia para ``Functor`` de forma que la función
sea aplicada únicamente en la primera componente: ::

    instance Functor (Pair c) where  
        fmap f (Pair (x,y)) = Pair (f x, y)
        
Como puede observar, podemos utilizar el ajuste de patrones con tipos
definidos con ``newtype``. Utilizamos el ajuste de patrones para obtener la
dupla subyacente, luego aplicamos la función ``f`` al primer componente de la
tupla y luego utilizamos el constructor de datos ``Pair`` para convertir la
tupla de nuevo al tipo ``Pair b a``. El tipo de ``fmap`` restringido al nuevo
tipo quedaría así: ::

    fmap :: (a -> b) -> Pair c a -> Pair c b  
    
De nuevo, hemos utilizado ``instance Functor (Pair c) where`` así que 
``(Pair c)`` toma el lugar de ``f`` en la definición de clase de ``Functor``:
::

    class Functor f where  
        fmap :: (a -> b) -> f a -> f b
        
Ahora podemos convertir una dupla en un ``Pair b a``, y utilizar ``fmap`` 
sobre ella de forma que la función solo se aplique a la primera componente: ::

    ghci> getPair $ fmap (*100) (Pair (2,3))  
    (200,3)  
    ghci> getPair $ fmap reverse (Pair ("london calling", 3))  
    ("gnillac nodnol",3)

    
La pereza de newtype
''''''''''''''''''''

Ya hemos hablado de que normalmente ``newtype`` es más rápido que ``data``. Lo
único que podemos hacer cuando utilizamos ``newtype`` es convertir un tipo
existente en un nuevo tipo, aunque internamente, Haskell puede representar los
valores de los tipos definidos con ``newtype`` igual que los originales,
aunque debe tener en cuenta que sus tipos son diferentes. Esto provoca que
``newtype`` no solo sea más rápido, sino también más perezoso. Vamos a ver con
detalle que significa esto.

Como ya dijimos, Haskell perezoso por defecto, lo que significa que solo
cuando intentamos mostrar el resultado de nuestras funciones tendrá lugar el
cómputo de estos resultados. Además, solo los cómputos que son necesario para
calcular el resultado la función serán ejecutados. El valor ``undefined`` de
Haskell representa un cómputo erróneo. Si intentamos evaluarlo (es decir,
forzamos a Haskell a que lo calcule) mostrándolo por la terminal, Haskell se
lanzará un berrinche (técnicamente conocido como excepción):

.. code-block:: console

    ghci> undefined  
    *** Exception: Prelude.undefined  
    
Sin embargo, si insertamos algunos valores ``undefined`` en una lista pero
solo necesitamos la cabeza de la lista, la cual no es ``undefined``, todo
funcionará bien ya que Haskell no necesita evaluar ningún otro elemento de la
lista si solo estamos interesados en el primer elemento: 

.. code-block:: console

    ghci> head [3,4,5,undefined,2,undefined]  
    3
    
Ahora consideremos el siguiente tipo: ::

    data CoolBool = CoolBool { getCoolBool :: Bool }  
    
Es uno de los muchos tipos de datos algebraicos que se pueden definir con la
palabra clave ``data``. Tiene un único constructor de datos, y este
constructor solo posee un campo cuyo tipo es ``Bool``. Vamos a crear una
función que use un ajuste de patrones en un ``CoolBool`` y devuelva el valor
``"hola"`` independientemente de que el valor ``Bool`` contenido en
``CoolBool`` sea ``True`` o ``False``: ::

    helloMe :: CoolBool -> String  
    helloMe (CoolBool _) = "hola"

En lugar de aplicar esta función a un valor normal de ``CoolBool``, vamos a
complicarnos la vida y aplicar el valor ``undefined``.

.. code-block:: console

    ghci> helloMe undefined  
    "*** Exception: Prelude.undefined
    
¡Una excepción! ¿Por qué sucede esto? Los tipos definidos con la palabra clave
``data`` pueden tener varios constructores de datos (aunque ``CoolBool`` solo
tiene uno). Así que para saber si un valor dado a nuestra función se ajusta al
patrón ``(CoolBool _)``, Haskell tiene que evaluar el valor lo suficiente como
para saber el constructor de datos que se ha utilizado para crear el valor. Y
cuando tratamos de evaluar un valor ``undefined``, por muy poco que lo
evaluemos, se lanzará una excepción. 

En lugar de utilizar la palabra clave ``data`` para definir ``CoolBool``,
vamos a intentar utilizar ``newtype``: ::

    newtype CoolBool = CoolBool { getCoolBool :: Bool }  
    
No tenemos que cambiar nada de la función ``helloMe`` porque la sintaxis que
se utiliza en el ajuste de patrones es igual para ``data`` que para
``newtype``. Vamos a hacer lo mismo y aplicar ``helloMe`` a un valor
``undefined``:

.. code-block:: console

    ghci> helloMe undefined  
    "hola"

.. image:: /images/shamrock.png
   :align: right
   :alt: ¡Te deseo lo mejor!

¡Funcionó! Mmm... ¿Por qué? Bueno, como ya hemos dicho, cuando utilizamos
``newtype``, Haskell puede representar internamente los valores del nuevo
tipo como si se tratasen del original. No tiene que añadir ningún envoltorio a
estos valores, simplemente debe tener en cuenta de que poseen un tipo
distinto. Y como Haskell sabe que los tipos definidos con la palabra clave
``newtype`` solo pueden tener un constructor de datos, no tiene porque evaluar
el parámetro pasado a la función para estar seguro de que se ajusta al patrón
``(CoolBool _)`` ya que los tipos ``newtype`` solo pueden tener un constructor
de datos con un solo campo. 

Esta diferencia de comportamiento puede parecer trivial, pero en realidad es
muy importante ya que nos ayuda a entender que aunque los tipos definidos con
``data`` y ``newtype`` se comportan de forma muy similar desde el punto de
vista de un programador, en realidad son dos mecanismos diferentes. Mientras
``data`` se puede utilizar para crear nuestros propios tipos de datos desde
cero, ``newtype`` sirve para crear un tipo completamente nuevo a partir de uno
ya existente. Cuando utilizamos un ajuste de patrones con un tipo ``newtype``
no estamos extrayendo ningún dato de él (como ocurriría con ``data``), sería
más bien como una conversión directa entre un dato y otro.


type vs. newtpe vs. data
''''''''''''''''''''''''

Llegados a este punto, quizás estés algo confundido sobre que diferencias
existen entre ``type``, ``data`` y ``newtype``. Vamos a refrescar la memoria.

La palabra clave ``type`` se utiliza para crear sinónimos. Básicamente lo que
hacemos es dar otro nombre a un tipo que ya existe de forma que nos sea más
fácil referirnos a él. Por ejemplo: ::

    type IntList = [Int]  
    
Todo lo que hace es permitirnos llamar al tipo ``[Int]`` como ``IntList``. Se
puede utilizar indistintamente. No obtenemos ningún constructor de datos nuevo
a partir de ``IntList`` ni nada por el estilo. Como ``[Int]`` y ``IntList``
son dos formas de referirse al mismo tipo, no importa que nombre usemos en las
declaraciones de tipo: ::

    ghci> ([1,2,3] :: IntList) ++ ([1,2,3] :: [Int])  
    [1,2,3,1,2,3]
    
Utilizamos los sinónimos de tipos cuando queremos que nuestras declaraciones
de tipo sean más descriptivas, de forma que los sinónimos que demos expliquen
algo acerca de su propósito en un determinado contexto. Por ejemplo, si
utilizamos listas de asociación del tipo ``[(String,String)]`` para
representar una agenda telefónica, podemos darle el sinónimo de tipo
``PhoneBook`` de forma que las declaraciones de tipo de las funciones sean más
legibles.

La palabra clave ``newtype`` se utiliza para crear nuevos tipos a partir de
uno ya existente. Su uso es común para facilitar la declaración de ciertas
instancias de clases de tipos. Cuando utilizamos ``newtype`` con un tipo ya
existente, el tipo que obtenemos es diferente del original. Si tenemos el
siguiente tipo: ::

    newtype CharList = CharList { getCharList :: [Char] }  
    
No podemos utilizar ``++`` para concatenar un ``CharList`` con un ``[Char]``.
Ni siquiera podemos utilizar ``++`` para concatenar dos ``CharList`` porque
``++`` solo funciona con listas. ``CharList`` no es una lista, incluso aunque 
sepamos que contiene una. Sin embargo, podemos convertir dos ``CharList`` en
listas, luego utilizar ``++`` con ellas y más tarde convertir el resultado en
un ``CharList``.

Cuando utilizamos la sintaxis de registro en las declaraciones ``newtype``,
obtenemos funciones para convertir ente el nuevo tipo y el original. El nuevo
tipo no posee automáticamente las instancias para clases de tipos de las que
formaba parte el tipo original, así que tenemos que derivarlas manualmente.

En la practica, puedes considerar las declaraciones de tipo ``newtype``
iguales a las declaraciones ``data``, aunque solo puede tener un constructor
de datos y un solo campo. Si te encuentras declarando un tipo como ese,
plantéate utilizar ``newtype`` en lugar de ``type``.

La palabra clave ``data`` la utilizamos para crear nuestros propios tipos de
datos, y podemos hacer lo que se nos antoje con ellos. Pueden tener tantos
constructores de datos y tantos campos como quieras y se pueden utilizar para
implementar cualquier tipo de dato algebraico. Cualquier cosa, desde listas
hasta tipos como ``Maybe`` o árboles.

Si quieres que las declaraciones de tipo sean más descriptivas y legibles,
probablemente lo que estés buscando sean los sinónimos de tipos. Si lo que
quieres es crear un nuevo tipo que contenga a otro para poder declarar una
instancia de una clase de tipos, seguramente quieras utilizar ``newtype``. Y
si lo que quieres es crear algo completamente nuevo, apostaría a que debes
utilizar ``data``. 






