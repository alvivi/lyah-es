

Funtores, funtores aplicables y monoides
========================================


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
definen comportamientos muy generales y abstractos. Ya vimos que las clases
de tipos definen operaciones para ver si dos cosas son iguales o comparar dos
cosas por un cierto orden. Son comportamientos muy abstractos a la vez que
elegantes, pero no los vemos como algo especial ya que hemos estado tratando
con ellos a lo largo de nuestras vidas. Hace poco conocimos los funtores, que
son básicamente cosas que se pueden mapear. Esto es un ejemplo de algo útil y
a la vez bastante abstracto de lo que pueden describir las clases de tipos. En
este capítulo veremos más de cerca los funtores, junto a una versión más
fuerte y útil de los funtores llamados funtores aplicables. También daremos un
vistazo a los monoides.


De vuelta con los funtores
--------------------------

.. image:: /images/frogtor.png
   :align: right
   :alt: La ranas ni siquiera necesitan dinero.

Ya hablamos de los funtores en su pequeña :ref:`sección <funtores>`. Si
todavía no la has leído, probablemente deberías darle un vistazo ahora mismo,
o quizá luego cuando tengas más tiempo. O simplemente puedes hacer como si ya
la hubieses leído.

Aún así, vamos a recordar algo: los funtores son cosas que se puede mapear,
como las listas, ``Maybe``s, árboles, etc. En Haskell, son descritos con la
clase de tipos ``Functor``, la cual solo contiene un método de clase,
``fmap``, que tiene como tipo ``fmap :: (a -> b) -> f a -> f b``. Dice algo
como: dame una función que tome un ``a`` y devuelva un ``b`` y una caja con
una ``a`` (o varias de ellas) dentro y yo te daré una caja con una ``b`` (o
varias de ellas) dentro. En cierto modo es como si aplicará la función dentro
de la caja.

.. note:: Muchas veces se utiliza la analogía de la caja para ayudar a obtener
          una intuición de como funcionan los funtores, luego, probablemente
          usemos la misma analogía para los funtores aplicables y las mónadas.
          Al principio es una buena analogía que ayuda a la gente a entender
          los funtores, pero no la tomes al pie de la letra, ya que para
          algunos funtores la analogía de la caja tiene que ser ajusta al
          milímetro para que siga siendo verdad. Un término más correcto para
          definir lo que es un funtor sería *contexto* *computacional*. El
          contexto sería que la computación podría tener un valor o podría
          fallar (``Maybe`` y ``Either a``) o que podría tener más valores
          (listas) o demás cosas del estilo.

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

De esta forma vemos de forma clara que ``fmap`` es una simple composición de
funciones. Ejecuta ``:m + Control.Monad.Instances``, ya que ahí está definida
esta instancia e intenta jugar un poco mapeando funciones. ::

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


 




