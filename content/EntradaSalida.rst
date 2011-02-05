

Entrada y salida
================


.. image:: /images/dognap.png
   :align: right
   :alt: Secuestrador de perros

Ya hemos mencionado que Haskell es un lenguaje de programación puramente
funcional. Mientras que en un lenguaje imperativo normalmente consigues
resultados dando al computador una serie de pasos ha ejecutar, la programación
funcional es más como definir que son las cosas. En Haskell, una función no
puede cambiar un estado, como cambiar el contenido de una variable (cuando una
función cambia de estado, decimos que tiene *efectos secundarios*). La única
cosa que una función de Haskell puede hacer es devolvernos algún resultado
basándose en los parámetros que le demos. Si una función es llamada dos veces
con los mismo parámetros, tiene que devolver el mismo resultado. Mientras esto
puede parecer un poco restrictivo desde el punto de vista de un mundo
imperativo, ya hemos visto como en realidad es algo genial. En un lenguaje
imperativo no tienes ninguna garantía de que una función que solo debe jugar
con unos números no vaya a quemar tu casa, secuestrar a tu perro o rallar tu
coche con una patata mientras juega con esos números. Por ejemplo, cuando
hacemos una búsqueda binaria con un árbol, no insertamos ningún elemento en
el árbol modificando algún nodo. Nuestra función para insertar un elemento en
un árbol en realidad devuelve un nuevo árbol, ya que no puede modificar el
árbol anterior.

Como el hecho de que las funciones no sean capaces de cambiar el estado es
algo bueno, ya que nos ayuda a razonar acerca de nuestros programas, existe un
problema con esto. Si una función no puede cambiar nada en el mundo ¿Cómo se
supone que nos va a decir el resultado que ha calculado? Para conseguir que
nos diga lo que ha calculado, tiene que cambiar el estado de un dispositivo
de salida (normalmente el estado de la pantalla), lo cual emitirá fotones que
viajaran por nuestro cerebro para cambiar el estado de nuestra mente,
impresionante. 

No te desesperes, no está todo perdido. Haskell en realidad tiene un sistema
muy inteligente para tratar con funciones que tienen efectos secundarios de
forma que separa la parte de nuestro programa que es pura de la parte de
nuestro programa que es impura, la cual hace todo el trabajo sucio de hablar
con el teclado y la pantalla. Con estas partes bien separadas, podemos seguir
razonando acerca de nuestro programa puro y tomar ventaja de todo lo que nos
ofrece la pureza, como la evaluación perezosa, seguridad y modularidad
mientras nos comunicamos con el mundo exterior.


¡Hola mundo!
------------


.. image:: /images/helloworld.png
   :align: left
   :alt: ¡Hola mundo!

Hasta ahora, siempre hemos cargado nuestras funciones en GHCi para probarlas y
jugar con ellas. También hemos explorado las funciones de la librería estándar
de esta forma. Pero ahora, después de ocho capítulos, por fin vamos a escribir
nuestro primer programa de Haskell real ¡Wau! Y por supuesto, vamos a crear
el mítico ``"¡hola, mundo!"``.

.. note:: A efectos de este capítulo, voy a asumir que estás utilizando un
          sistema *unix* para aprender Haskell. Si estás en *Windows*, te
          sugiero que bajes `cygwin <http://www.cygwin.com/>`_, el cual es un
          entorno *Linux* para *Windows*, o dicho de otra forma, justo lo que
          necesitas.
          
Así que para empezar, pega lo siguiente en tu editor de texto favorito: ::

    main = putStrLn "hello, world"  
    
Acabamos de definir la función ``main`` y dentro de ella llamamos a una
función ``putStrLn`` con el parámetro ``"hello, world"``. Parece algo
corriente, pero no lo es, como veremos dentro de un momento. Guarda el fichero
como ``helloworld.hs``.

Y ahora vamos a hacer algo que no hemos hecho nunca antes ¡Vamos a compilar un
programa! ¿No estás nervioso? Abre un terminal y navega hasta el directorio 
donde se encuentra ``helloworld.hs`` y haz lo siguiente:

.. code-block:: console

    $ ghc --make helloworld  
    [1 of 1] Compiling Main             ( helloworld.hs, helloworld.o )  
    Linking helloworld ...
    
¡Vale! Con un poco de suerte habrás obtenido algo similar y podrás ejecutar
el programa haciendo ``./helloworld``.

.. code-block:: console

    $ ./helloworld  
    hello, world
    
Ahí lo tienes, nuestro primer programa compilado que muestra un mensaje por la
terminal ¡Extraordinariamente aburrido!

Vamos a examinar lo que hemos escrito. Primero, vamos a ver el tipo de la
función ``putStrLn``.

.. code-block:: console

    ghci> :t putStrLn  
    putStrLn :: String -> IO ()  
    ghci> :t putStrLn "hello, world"  
    putStrLn "hello, world" :: IO ()

Podemos leer el tipo de ``putStrLn`` como: ``putStrLn`` toma una cadena y
devuelve una acción ``IO`` que devuelve un tipo ``()`` (es decir, la tupla
vacía, también conocida como unidad). Una acción ``IO`` es algo que cuando
se realiza, cargará con una acción con algún efecto secundario (como leer
desde la entrada o mostrar cosas por pantalla) y contendrá algún tipo de de
valor dentro de él. Mostrar algo por pantalla realmente no tiene ningún tipo
de valor resultado, así que se utiliza el valor ficticio ``()``. 

.. note:: La tupla vacía tiene el valor de ``()`` y también tiene el tipo
          ``()``. Algo como ``data Nada = Nada``.
          
Y ¿Cuándo se ejecuta una acción ``IO``? Bueno, aquí es donde entra en juego
``main``. Una acción ``IO`` se ejecuta cuando cuando le damos el nombre`
``main`` y ejecutamos nuestro programa.

Tener todo tu programa en una sola acción ``IO`` puede parecer un poco
restringido. Por este motivo podemos usar la sintaxis ``do`` para juntar
varias acciones ``IO`` en una. Echa un vistazo al siguiente ejemplo: ::

    main = do  
        putStrLn "Hello, what's your name?"  
        name <- getLine  
        putStrLn ("Hey " ++ name ++ ", you rock!")
        
Ah... Interesante ¡Sintaxis nueva! Se lee de forma similar a la de un programa
imperativo. Si lo compilas y lo ejecutas, probablemente se comportará como
esperas. Fíjate que hemos utilizado un ``do`` y luego hemos puesto una serie
de pasos, exactamente igual que en un programa imperativo. Cada uno de estos
pasos es una acción ``IO``. Poniéndolas todas ellas juntas en un mismo bloque
``do`` conseguimos una sola acción ``IO``. La acción que obtenemos tiene el
tipo ``IO ()`` porque ese es el tipo de la última acción dentro del bloque.

Por este motivo, ``main`` siempre tiene que tener el tipo ``IO algo``, donde
``algo`` es algún tipo concreto. Por convenio, no se suele especificar la
declaración de tipo de ``main``.

Una cosa interesante que no hemos visto antes está en la tercera línea, la
cual es ``name <- getLine``.  Parece como si leyera una línea de texto y la
guardara en una variable llamada ``name`` ¿De verdad? Bueno, vamos a examinar
el tipo de ``getLine``.

.. code-block:: console

    ghci> :t getLine  
    getLine :: IO String

.. image:: /images/luggage.png
   :align: left
   :alt: Caja con patas

Vale. ``getLine`` es una acción ``IO`` que contiene un resultado del tipo
``String``. Parece que tiene sentido ya que esperará a que el usuario escriba
algo en la terminal y luego ese algo será representado con una cadena.
Entonces ¿Qué pasa con ``name <- getLine``? Puedes leer ese trozo de código
como: realiza la acción ``getLine`` y luego liga el resultado al valor
``name``. ``getLine`` tiene el tipo ``IO String``, así que ``name`` tendrá el
tipo ``String``. Puedes imaginar una acción ``IO`` como una caja con patas que
saldrá al mundo real y hará algo allí (como pintar un grafiti en una pared) y
quizá vuelva con algún dato dentro. Una vez se ha traído ese dato, la única
forma de abrir la caja y tomar el dato de su interior es utilizando la 
construcción ``<-``. Y si estamos extrayendo datos de una acción ``IO``, solo
podemos sacarlos cuando estemos dentro de alguna acción ``IO``. Así es como
Haskell gestiona y separa las partes puras e impuras de nuestro código.
En ese sentido ``getLine`` es impuro ya que el resultado no está garantizado
que sea el mismo cuando se llamada dos veces. Este es el porqué su resultado
está *contaminado* con constructor de tipos ``IO`` y solo podemos extraer
estos datos dentro de un código ``IO``. Y como el código ``IO`` está
contaminado también, cada cálculo que dependa en un dato contaminado con
``IO`` tendrá un resultado contaminado también.

Cuando decimos *contaminado*, no lo decimos en el sentido de que nunca más
podremos usar el resultado contenido en una acción ``IO`` en nuestro código
puro. No, cuando ligamos un valor contenido en una acción ``IO`` a un nombre
lo *descontaminamos* temporalmente. Cuando hacemos ``nombre <- getLine``,
``nombre`` es una cadena normal, ya que representa lo que hay dentro de la
caja. Podemos tener un función realmente complicada que, digamos, toma tu
nombre (una cade normal) como parámetro y predice tu suerte y todo tu futuro
basándose únicamente en tu nombre. Podría ser algo así: ::

    main = do  
        putStrLn "Hello, what's your name?"  
        name <- getLine  
        putStrLn $ "Read this carefully, because this is your future: " ++ tellFortune name
        
``tellFortune`` (o cualquier otra función a la que se le pase ``name``) no
tiene porque saber nada acerca de ``IO``, es simplemente una función normal de
tipo ``String -> String``.

Mira este trozo de código ¿Es válido? ::

    nameTag = "Hello, my name is " ++ getLine  
    
Si has dicho que no, puedes ir a por una galletita. Si haz dicho que sí, ves
olvidándote de caprichos. La razón por la que esto no funciona es que ``++``
requiere que sus dos parámetros sean del mismo tipo lista. El parámetro de la
izquierda tiene el tipo ``String`` (o ``[Char]`` si lo prefieres), mientras
que ``getLine`` tiene el tipo ``IO String``. No podemos concatenar una cadena
con una acción ``IO``. Primero debemos extraer el resultado de la acción
``IO`` para obtener un valor del tipo ``String`` y la única forma de
conseguirlo es haciendo algo como ``name <- getLine`` dentro de una acción
``IO``. Si queremos tratar con datos impuros tenemos que hacerlo en un entorno
impuro. La mancha de la impureza se propaga al igual que una plaga por nuestro
código y es nuestro deber mantener las partes ``IO`` tan pequeñas como sean
posibles.
 
Cada acción ``IO`` que es ejecutada tiene un resultado encapsulado con él.
Por este motivo podríamos haber escrito el código anterior como: ::

    main = do  
        foo <- putStrLn "Hello, what's your name?"  
        name <- getLine  
        putStrLn ("Hey " ++ name ++ ", you rock!")
      
Sin embargo, ``foo`` simplemente tendría el valor ``()`` lo cual no es de
mucha utilidad. Fíjate que no hemos ligado el último ``putStrLn`` a ningún
nombre. Esto es debido a que en un bloque ``do``, **la última acción no puede
ser ligada** como las dos primeras. Cuando nos aventuremos en el mundo de las
mónadas veremos el motivo concreto de esta restricción. Por ahora, puedes
pensar que un bloque ``do`` extrae automáticamente el valor de la última
acción y lo liga a su propio resultado.

Excepto para la última línea, cada línea de un bloque ``do`` que no se liga
puede también escribirse como una ligadura. Así que ``putStrLn "Blah"`` se
puede escribir como ``_ <- putStrLn "Blah"``. Sin embargo es algo inútil, por
lo que no usamos ``<-`` para acciones que no contienen un resultado
importante, como ``putStrLn algo``.

Los principiantes a veces piensan que hacer cosas como ``name = getLine``
leerá una línea por la entrada y la ligará a ``name``. Bueno, pues no, lo que
hace esto es darle a la acción ``getLine`` un nuevo nombre, llamado ``name``.
Recuerda que para obtener el valor contenido dentro de una acción ``IO``,
tienes que ligarlo a un nombre con ``<-`` dentro de otra acción ``IO``.

Las acciones ``IO`` solo son ejecutadas cuando se les dan el nombre de
``main`` o cuando están dentro de una acción ``IO`` más grande que hemos
compuesto con un bloque ``do``. Podemos usar un bloque ``do`` para juntar
algunas acciones ``IO`` y luego usar esa acción ``IO`` dentro de otro bloque
``do`` y así sucesivamente. De cualquier modo, al final solo se ejecutarán 
cuando sean alcanzadas por ``main``. 

Oh, cierto, también hay otro caso en el que las acciones ``IO`` son
ejecutadas. Cuando escribimos una acción ``IO`` en GHCi y pulsamos intro.

.. code-block:: console

    ghci> putStrLn "HEEY"  
    HEEY

Incluso cuando escribimos un número o una llamada a una función en GHCi, éste
lo evaluará (tanto como necesite) y luego llamará a ``show`` para mostrar esa
cadena en la terminal utilizando ``putStrLn`` de forma implícita.

¿Recuerdas las secciones ``let``? Si no, refresca tu memoria leyendo esta
:ref:`sección <leitbe>`. Tienen la forma ``let ligaduras in expresión``, donde
``ligaduras`` son los nombres que se les dan a las expresiones y ``expresión``
será la expresión donde serán evaluadas. También dijimos que las listas por
comprensión no era necesaria la parte ``in``. Bueno, puedes usarlas en un
bloque ``do`` prácticamente igual que en las listas por comprensión. Mira
esto: ::

    mport Data.Char  

    main = do  
        putStrLn "What's your first name?"  
        firstName <- getLine  
        putStrLn "What's your last name?"  
        lastName <- getLine  
        let bigFirstName = map toUpper firstName  
            bigLastName = map toUpper lastName  
        putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"

¿Ves como las acciones ``IO`` dentro del bloque ``do`` están alineadas? Fíjate
también en como la sección ``let`` está alineada con las acciones ``IO`` y los
nombres de ``let`` están alineados entre ellos. Es una buena práctica hacer
esto, ya que el sangrando es importante en Haskell. Hemos hecho ``map toUpper
firstName``, lo cual convierte algo como ``"john"`` en la cadena ``"JOHN"``.
Hemos ligado esa cadena en mayúsculas a un nombre y luego la hemos utilizado
en una cadena para mostrarla por la terminal.

Puedes estar preguntándote cuando utilizar ``<-`` y cuando utilizar ``let``.
Bueno, recuerda que ``<-`` es (por ahora) para ejecutar acciones ``IO`` y
ligar sus resultados. Sin embargo, ``map toUpper firstName`` no es una acción
``IO``. Es una expresión pura de Haskell. Así que utilizamos ``<-`` cuando
queremos ligar los resultados de una acción ``IO`` mientras que usamos ``let``
para ligar expresiones puras. Si hubiéramos hecho algo como ``let firstName =
getLine``, simplemente hubiéramos dado un nuevo nombre a la acción ``getLine``
y seguiríamos necesitado utilizar ``<-`` para ejecutar la acción.

Ahora vamos a crear un programa que lee continuamente una línea y muestra esa
línea con sus palabras al revés. La ejecución del programa se detendrá cuando
encuentre una línea vacía. Aquí tienes el programa. ::

    main = do   
        line <- getLine  
        if null line  
            then return ()  
            else do  
                putStrLn $ reverseWords line  
                main  

    reverseWords :: String -> String  
    reverseWords = unwords . map reverse . words

Para entender como funciona, puedes ejecutar el programa antes de leer el
código.

.. note:: Para ejecutar un programa puedes o bien compilarlo produciendo un
          ejecutable y luego ejecutarlo usando ``ghc --make helloworld`` y
          luego ``./helloworld`` o bien puedes usar el comando ``runhaskell``
          así: ``runhaskell helloworld.hs`` y tu programa será ejecutado al
          vuelo.
          
Primero vamos a echar un vistazo a la función ``reverseWords``. Es solo una
función normal que toma una cadena como ``"hey there man"`` y luego llama
a ``words`` lo cual produce una lista de palabras como
``["hey","there","man"]``. Luego mapeamos ``reverse`` sobre la lista,
obteniendo ``["yeh","ereht","nam"]``, luego volvemos a tener una sola cadena
utilizando ``unwords`` y el resultado final es ``"yeh ereht nam"``. Fíjate en
como hemos utilizado la composición de funciones. Sin la composición de
funciones tendríamos que haber escrito algo como ``reverseWords st = unwords
(map reverse (words st))``.

¿Qué pasa con ``main``? Primero, obtenemos una línea del terminal ejecutando
``getLine`` y la llamamos ``line``. Y ahora tenemos una expresión condicional.
Recuerda que en Haskell, cada ``if`` debe tener su ``else`` ya que toda
expresión debe tener algún tipo de valor. Usamos la condición de forma que
cuando sea cierta (en nuestro caso, para cuando la línea esté vacía)
realicemos una acción ``IO`` y cuando no lo es, realizamos la acción ubicada
en el ``else``. Por este motivo las condiciones dentro de una acción ``IO``
tienen la forma ``if condición then acción else acción``.

Vamos a echar un vistazo a lo que pasa bajo la cláusula ``else``. Como debemos
tener exactamente una sola acción ``IO`` después del ``else`` tenemos que usar
un bloque ``do`` para juntar todas la acciones en una. También podía ser
escrito así: ::

    else (do  
        putStrLn $ reverseWords line  
        main)
        
Esto hace más explícito el hecho de que un bloque ``do`` sea visto como una
sola acción ``IO``, pero es más feo. De cualquier modo, dentro del bloque
``do`` llamamos a ``reverseWords`` sobre la línea que obtuvimos de ``getLine``
y luego mostramos el resultado por la terminal. Luego de esto, simplemente
ejecutamos ``main``. Es llamado de forma recursiva y no hay ningún problema
ya que ``main`` es por si mismo una acción ``IO``. De cierto modo es como
si volviéramos al inicio del programa.

Ahora ¿Qué sucede cuando ``null line`` se evalúa a cierto? Se ejecuta la
acción que está después del ``then``. Si buscamos veremos que pone ``then`
return ()``. Si conoces algún lenguaje imperativo como *C*, *Java* 
*Python*, probablemente estés pensando que ya sabes lo que es ``return`` y 
que puedes saltarte este párrafo tan largo. Bueno, pues **el** ``return`` **de
Haskell no tiene nada que ver con el** ``return`` **de la mayoría de los otros
lenguajes**. Tiene el mismo nombre, lo cual confunde a mucha a gente, pero en
realidad es muy diferente. En los lenguajes imperativos, ``return``
normalmente termina la ejecución de un método o una subrutina y devuelve algún
tipo de valor a quien quiera que lo llamó. En Haskell (dentro de la acciones
``IO`` concretamente), lo que hace es convertir un valor puro en una acción
``IO``. Si lo piensas como en la analogía de la caja que vimos, ``return``
toma un valor y lo pone dentro de una caja. La acción ``IO`` resultante
realmente no hace nada, simplemente tiene dicho valor como resultado. Así que
en un contexto ``IO``, ``return "haha"`` tendrá el tipo ``IO String`` ¿Cuál es
el motivo de transformar un valor puro en una acción que realmente no hace
nada? ¿Por qué contaminar más nuestro programa con ``IO``? Bueno, necesitamos
alguna acción ``IO`` en caso de que encontremos una línea vacía. Por este
motivo hemos creado una acción ``IO`` que realmente no hace nada con ``return
()``. 

Al utilizar ``return`` no causamos que un bloque ``do`` termine su ejecución
ni nada parecido. Por ejemplo, este programa ejecutará hasta la última línea
sin ningún problema. ::

    main = do  
        return ()  
        return "HAHAHA"  
        line <- getLine  
        return "BLAH BLAH BLAH"  
        return 4  
        putStrLn line

Todo lo que estos ``return`` hacen es crear acciones ``IO`` que en realidad
no hacen nada excepto contener un valor, el cual es desperdiciado ya que no
se liga a ningún nombre. Podemos utilizar ``return`` en combinación con ``<-``
para ligar cosas a nombres. ::

    main = do  
        a <- return "hell"  
        b <- return "yeah!"  
        putStrLn $ a ++ " " ++ b
        
Como puedes ver, ``return`` es en cierto modo lo opuesto de ``<-``. Mientras
que ``return`` toma valores y los mete en una caja, ``<-`` toma una caja (y
la ejecuta) y saca el valor que contiene, enlazándolo a un nombre. Sin embargo
hacer estas cosas es un poco redundante, ya que puedes utilizar secciones
``let`` para conseguir lo mismo: ::

   main = do  
       let a = "hell"  
           b = "yeah"  
       putStrLn $ a ++ " " ++ b
       
Cuando tratemos con bloques ``do`` ``IO``, normalmente utilizamos ``return`` o
bien porque queremos crear una acción ``IO`` que no haga nada o bien porque
queremos que el resultado que albergue la acción ``IO`` resultante de un
bloque ``do`` no sea el valor de la última acción.

.. note:: Un bloque ``do`` puede contener una sola acción ``IO``. En ese caso,
          es lo mismo que escribir solo dicha acción. Hay gente que prefiere
          escribir ``then do return ()`` en este caso ya que el ``else``
          también tiene un ``do``.
          
Antes de que veamos como tratar con ficheros, vamos a echar un vistazo a
algunas funciones que son útiles a la hora de trabajar con ``IO``.

 * :cpp:member:`putStr` es muy parecido a ``putStrLn`` en el sentido de que toma una
   cadena y devuelve una acción que imprimirá esa cadena por la terminal, solo
   que ``putStr`` no salta a una nueva línea después de imprimir la cadena tal
   y como ``putStrLn`` hace. ::
   
        main = do   putStr "Hey, "  
                    putStr "I'm "  
                    putStrLn "Andy!"

   .. code-block:: console
   
        $ runhaskell putstr_test.hs  
        Hey, I'm Andy!
    
   Su tipo es ``putStr :: String -> IO ()``, así que el resultado contenido en
   la acción ``IO`` es la unidad. Un valor inútil, por lo que no tiene sentido
   ligarlo a nada.
   
 * :cpp:member:`putChar` toma un carácter y devuelve una acción ``IO`` que lo imprimirá
   por la terminal. ::
   
       main = do   putChar 't'  
                   putChar 'e'  
                   putChar 'h'
   
   .. code-block:: console
   
       $ runhaskell putchar_test.hs  
       teh    

   ``putStr`` en realidad está definido recursivamente con ayuda de
   ``putChar``. El caso base es la cadena vacía, así que si estamos
   imprimiendo la cadena vacía, simplemente devolvemos una acción ``IO`` que
   no haga nada utilizando ``return ()``. Si no esta vacía, imprimimos el
   primer carácter de la cadena utilizando ``putChar`` y luego imprimimos el
   resto de la cadena usando ``putStr``. ::
   
       putStr :: String -> IO ()  
       putStr [] = return ()  
       putStr (x:xs) = do  
           putChar x  
           putStr xs

   Fíjate en que podemos utilizar la recursión en ``IO`` de la misma forma que
   lo hacemos en el código puro. Al igual que en el código puro, definimos el
   caso base y luego pensamos que es realmente el resultado. Es una acción que
   primero imprime el primer carácter y luego imprime el resto de la cadena.
   
 * :cpp:member:`print` toma un valor de cualquier tipo que sea miembro de la clase
   ``Show`` (por lo que sabemos que se puede representar como una cadena),
   llama a ``show`` con ese valor para obtener su representación y luego
   muestra esa cadena por la terminal. Básicamente es ``putStrLn . show``.
   Primero ejecuta ``show`` con un valor y luego alimenta ``putStrLn`` con
   ese valor, lo cual devuelve una acción que imprimirá nuestro valor. ::
   
        main = do   print True  
                    print 2  
                    print "haha"  
                    print 3.2  
                    print [3,4,3]
                    
   .. code-block:: console
   
       $ runhaskell print_test.hs  
       True  
       2  
       "haha"  
       3.2  
       [3,4,3]
       
   Como puedes ver, es una función muy útil ¿Recuerdas cuando hablamos de que
   las acciones ``IO`` se ejecutan solo cuando son alcanzadas por ``main`` o
   cuando intentamos evaluarlas en GHCi? Cuando escribimos un valor (como
   ``3`` o ``[1,2,3]``) y pulsamos intro, GHCi en realidad utiliza ``print``
   con ese valor para mostrarlo por la terminal. 
   
   .. code-block:: console
   
        ghci> 3  
        3  
        ghci> print 3  
        3  
        ghci> map (++"!") ["hey","ho","woo"]  
        ["hey!","ho!","woo!"]  
        ghci> print (map (++"!") ["hey","ho","woo"])  
        ["hey!","ho!","woo!"]

   Cuando queremos imprimir cadenas normalmente utilizamos ``putStrLn`` ya que
   solemos querer las dobles comillas que rodean la representación de una
   cadena, pero para mostrar valores de cualquier otro tipo se suele utilizar
   ``print``.
   
 * :cpp:member:`getChar` es una acción ``IO`` que lee un carácter por la entrada
   estándar (teclado). Por ello, su tipo es ``getChar :: IO Char``, ya que
   el resultado contenido dentro de la acción ``IO`` es un carácter. Ten en
   cuenta que debido al *buffering*, la acción de leer un carácter no se
   ejecuta hasta que el usuario pulsa la tecla intro. ::
        
        main = do     
            c <- getChar  
            if c /= ' '  
                then do  
                    putChar c  
                    main  
                else return ()
   
   Este programa parece que debe leer un carácter y comprobar si es un
   espacio. Si lo es, detiene la ejecución del programa y si no lo es, lo
   imprime por la terminal y luego repite su ejecución. Bueno, parece que
   hace esto, pero no lo hace de la forma que esperamos. Compruébalo.
   
   .. code-block:: console
   
        $ runhaskell getchar_test.hs  
        hello sir  
        hello
        
   La segunda línea es la salida. Hemos escrito ``hello sir`` y luego hemos
   pulsado intro. Debido al *buffering*, la ejecución del programa solo 
   empieza después de ejecutar intro y no después de cada carácter pulsado.
   Una vez pulsamos intro, actúa como si hubiéramos escrito esos caracteres
   desde el principio. Intenta jugar un poco con este programa para entender
   como funciona.
   
 * La función :cpp:member:`when` se encuentra en el módulo ``Control.Monad`` (para
   acceder a ella haz ``import Control.Monad``). Es interesante ya que dentro
   de un bloque ``do`` parece como si fuese una sentencia de control de flujo,
   pero en realidad es una función normal. Toma un valor booleano y una acción
   ``IO`` de forma que si el valor booleano es ``True``, devolverá la misma
   acción que le suministremos. Sin embargo, si es falso, nos devolverá una
   acción ``return ()``, acción que no hace absolutamente nada. Aquí tienes
   como podríamos haber escrito el trozo de código anterior que mostraba el
   uso de ``getChar`` utilizando ``when``: ::
   
         import Control.Monad   

         main = do  
             c <- getChar  
             when (c /= ' ') $ do  
                 putChar c  
                 main

   Como puedes ver, es útil para encapsular el patrón `ìf algo then do acción
   else return ()``. También existe la función ``unless`` que es exactamete
   igual a ``when`` solo que devuleve la acción original cuando ecuentra
   ``False`` en lugar de ``True``.
   
   
 * :cpp:member:`sequence` toma una lista de acciones ``IO`` y devuevle una acción que
   realizará todas esas acciones una detrás de otra. El resultado contenido en
   la acción ``IO`` será una lista con todos los resultados de todas las
   acciones ``IO`` que fueron ejecutadas. Su tipo es ``sequence :: [IO a] ->`
   IO [a]``. Hacer esto: ::
   
        main = do  
            a <- getLine  
            b <- getLine  
            c <- getLine  
            print [a,b,c]
            
   Es exactamente lo mismo que hacer: ::
   
       main = do  
           rs <- sequence [getLine, getLine, getLine]  
           print rs
           
   Así que ``sequence [getLine, getLine, getLine]`` crea una acción ``IO`` que
   ejecutará ``getLine`` tres veces. Si ligamos esa acción a un nombre, el
   resultado será una lista que contendrá todos los resultados, en nuestro
   caso, una lista con tres líneas que haya introducido el usuario.
   
   Un uso común de ``sequence`` es cuando mapeamos funciones como ``print`` o
   ``putStrLn`` sobre listas. Al hacer ``map print [1,2,3,4]`` no creamos
   una acción ``IO``. Creará una lista de acciones ``IO``, ya que es lo mismo
   que si escribiéramos ``[print 1, print 2, print 3, print 4]``. Si queremos
   transformar esa lista de acciones en una única acción IO, tenemos que
   secuenciarla.
   
   .. code-block:: console
   
        ghci> sequence (map print [1,2,3,4,5])  
        1  
        2  
        3  
        4  
        5  
        [(),(),(),(),()]
        
   ¿Qué es eso de ``[(),(),(),(),()]``? Bueno, cuando evaluamos una acción
   ``IO`` en GHCi es ejecutada y su resultado se muestra por pantalla, a no
   ser que el resultado sea ``()``, en cuyo caso no se muestra. Por este
   motivo al evaluar ``putStrLn "hehe"`` GHCi solo imprime ``"hehe"`` (ya que
   el resultado contenido en la acción ``putStrLn "hehe"`` es ``()``). Sin
   embargo cuando utilizamos ``getLine`` en GHCi, el resultado de esa acción
   si es impreso por pantalla, ya que ``getLine`` tiene el tipo ``IO String``.
   
 * Como mapear una función que devuelve una acción ``IO`` sobre una lista y
   luego secuenciarla es algo muy común, se introducieron las funciones
   auxiliares :cpp:member:`mapM` y :cpp:member:`mapM_`. ``mapM`` toma una función y una lista,
   mapea la función sobre la lista y luego la secuencia. ``mapM_`` hace lo
   mismo, solo que después se deshace del resultado. Normalmente utilizamos
   ``mapM_`` cuando no nos importa el resultado de las acciones secuenciadas.
   
   .. code-block:: console
   
        ghci> mapM print [1,2,3]  
        1  
        2  
        3  
        [(),(),()]  
        ghci> mapM_ print [1,2,3]  
        1  
        2  
        3
        
 * :cpp:member:`forever` toma una acción ``IO`` y devuelve otra acción ``IO`` que 
   simplemente repetirá la primera acción indefinidamente. Está situada en
   ``Control.Monad``. Este pequeño programa preguntará al usuario por una
   cadena y luego la devolverá en mayúsculas, indefinidamente: ::
   
        import Control.Monad  
        import Data.Char  

        main = forever $ do  
            putStr "Give me some input: "  
            l <- getLine  
            putStrLn $ map toUpper l
            
 * :cpp:member:`forM` (situado en ``Control.Monad``) es como ``mapM`` solo que tiene
   sus parámetros cambiados de sitio. El primer parámetro es la lista y el
   segundo la función a mapear sobre la lista, la cual luego será secuenciada
   ¿Para qué es útil? Bueno, con un uso creativo de funciones lambda y la
   notación ``do`` podemos hacer cosas como estas: ::
   
        import Control.Monad  

        main = do   
            colors <- forM [1,2,3,4] (\a -> do  
                putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"  
                color <- getLine  
                return color)  
            putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "  
            mapM putStrLn colors
            
   ``(\a -> do ... )`` es una función que toma un número y devuelve una acción
   ``IO``. Tenemos que rodearla con paréntesis, ya que de otro modo la función
   lambda pensaría que las dos últimas líneas le pertenecen. Fíjate que usamos
   ``return color`` dentro del bloque ``do``. Lo hacemos así para que la
   acción ``IO`` que define el bloque ``do`` tenga como resultado el color
   que deseamos. Realmente no tenemos que hacerlos porque ``getLine`` ya
   lo tienen contenido. Hacer ``color <- getLine`` para luego hacer ``return
   color`` es simplemente extraer  el resultado de ``getLine`` para luego
   volver a insertarlo otra vez, así que es lo mismo que hacer solo
   ``getLine``.``forM`` (llamado con sus dos parámetros) produce una acción
   ``IO``, cuyo resultado ligaremos a ``colors``. ``colors`` es una simple
   lista que contiene cadenas. Al final, imprimimos todos esos colores
   haciendo ``mapM putStrLn colors``.
   
   Puedes verlo en el sentido de que ``forM`` crea una acción ``IO`` para
   cada elemento de una lista. Lo que haga cada acción dependerá del elemento
   que haya sido utilizado para crear la acción. Al final, realiza todas
   esas acciones y liga todos los resultados a algo. No tenemos porque
   ligarlo, podemos simplemente desecharlo.
   
   .. code-block:: console
   
        $ runhaskell form_test.hs  
        Which color do you associate with the number 1?  
        white  
        Which color do you associate with the number 2?  
        blue  
        Which color do you associate with the number 3?  
        red  
        Which color do you associate with the number 4?  
        orange  
        The colors that you associate with 1, 2, 3 and 4 are:  
        white  
        blue  
        red  
        orange  
        
   En realidad podríamos haber hecho lo mismo sin ``forM``, solo que con
   ``forM`` es más legible. Normalmente usamos ``forM`` cuando queremos
   mapear y secuenciar algunas acciones que hemos definido utilizando la
   notación ``do``. Del mismo modo, podríamos haber remplazado la última línea
   por ``forM colors putStrLn``.
   
En esta sección hemos aprendido las bases de la entrada y salida. También
hemos visto que son las acciones ``IO``, como nos permiten realizar acciones
de entrada y salida y cuando son realmente ejecutadas. Las acciones ``IO`` son
valores al igual que cualquier otro valor en Haskell. Podemos pasarlas como
parámetros en las funciones y las funciones pueden devolver acciones como
resultados. Lo que tiene de especial es cuando son alcanzadas por ``main``
(o son el resultado de una sentencia en GHCi), son ejecutadas. Y es en ese
momento cuando pueden escribir cosas en tu pantalla o reproducir 
`Yakety Sax <http://www.youtube.com/watch?v=ZnHmskwqCCQ>`_
por tus altavoces. Cada acción ``IO`` también puede contener un resultado que
nos dirá que ha podido obtener del mundo real.

No pienses en la función ``putStrLn`` como una función que toma una cadena y
la imprime por pantalla. Piensa que es una función que toma una cadena y
devuelve una ación ``IO``. Esa acción ``IO``, cuando sea ejecutada, imprimirá
por pantalla dicha cadena.


Ficheros y flujos de datos
--------------------------


.. image:: /images/streams.png
   :align: right
   :alt: Corrientes

``getChar`` es una acción de E/S que lee un solo carácter desde la terminal.
``getLine`` es una acción de E/S que lee una línea desde la terminal. Estas
funciones son bastante sencillas y la mayoría de lenguajes tienen funciones
o sentencias similares. Pero ahora vamos a ver :cpp:member:`getContents`.
``getContents`` es una acción de E/S que lee cualquier cosa de la entrada
estándar hasta que encuentre un carácter de fin de fichero. Su tipo es
``getContents :: IO String``. Lo bueno de ``getContents`` es que realiza una
E/S perezosa. Cuando hacemos ``foo <- getContents``, no lee todos los datos de
entrada de golpe, los almacena en memoria y luego los liga a ``foo``. No ¡Es
perezoso! Dirá "Sí, sí, ya leeré la entrada de la terminal luego, cuando
de verdad lo necesites". 
  
``getContents`` es realmente útil cuando estamos redirigiendo la salida de un
programa a la entrada de otro programa. En caso de que no sepas como funciona
la redirección en sistemas *unix*, aquí tienes una pequeña introducción. Vamos
a crear un fichero de texto que contenga este pequeño 
`haiku <http://es.wikipedia.org/wiki/Haiku>`_:

.. code-block:: none

    I'm a lil' teapot  
    What's with that airplane food, huh?  
    It's so small, tasteless

Sí, tienes razón, este haiku apesta. Si conoces alguna buena guía sobre haikus
házmelo saber. 

Ahora recuerda aquel pequeño programa que escribimos cuando explicamos la
función ``forever``. Le pedía al usuario una línea y la devolvía en
mayúsculas, luego volvía a hace lo mismo indefinidamente. Solo para que no
tengas que desplazarte hacia atrás, aquí tienes el código de nuevo: ::

    import Control.Monad  
    import Data.Char  

    main = forever $ do  
        putStr "Give me some input: "  
        l <- getLine  
        putStrLn $ map toUpper l
        
Vamos a guardar este programa como ``capslocker.hs`` o algo parecido y lo
compilamos. Y ahora, vamos a utilizar redirecciones *unix* para suministrar 
nuestro fichero de texto directamente a nuestro pequeño programa. Nos vamos
a ayudar del uso del programa GNU ``cat``, el cual muestra por la terminal el
contenido del fichero que le pasemos como parámetro ¡Mira!

.. code-block:: console

    $ ghc --make capslocker   
    [1 of 1] Compiling Main             ( capslocker.hs, capslocker.o )  
    Linking capslocker ...  
    $ cat haiku.txt  
    I'm a lil' teapot  
    What's with that airplane food, huh?  
    It's so small, tasteless  
    $ cat haiku.txt | ./capslocker  
    I'M A LIL' TEAPOT  
    WHAT'S WITH THAT AIRPLANE FOOD, HUH?  
    IT'S SO SMALL, TASTELESS  
    capslocker <stdin>: hGetLine: end of file
    
Como puedes ver, para redireccionar la salida de un programa (en nuestro caso
``cat``) a la entrada de otro (``capslocker``) se consigue con el carácter
``|``. Lo que acabamos de hacer sería equivalente a ejecutar ``capslocker``,
escribir nuestro haiku en la terminal y luego introducir el carácter de fin
de fichero (normalmente esto se consigue pulsando *Ctrl+D*). Es como ejecutar
``cat haiku.txt`` y decir: "Alto espera, no muestres esto por pantalla,
pásaselo a ``capslocker``".
 
Así que lo que estamos haciendo al utilizar ``forever`` es básicamente tomar
la entrada y transformarla en algún tipo de salida. Por este motivo podemos
utilizar ``getContents`` para hacer nuestro programa mejor e incluso más
corto. ::

    import Data.Char  

    main = do  
        contents <- getContents  
        putStr (map toUpper contents)
        
Ejecutamos la acción de E/S ``getContents`` y nombramos la cadena que produce
como ``contents``. Luego, trazamos ``toUpper`` sobre la cadena y mostramos
el resultado por la terminal. Ten en cuenta que las cadenas son básicamente
listas, las cuales son perezosas, y ``getContents`` es una acción de E/S
perezosa. Por lo tanto no intentará leer todo el contenido de golpe para
guardarlo en memoria antes de mostrarlo en mayúsculas por la terminal. En
realidad mostrará la versión en mayúsculas conforme vaya leyendo, ya que solo
lee una línea de la entrada cuando realmente lo necesita. 

.. code-block:: console

    $ cat haiku.txt | ./capslocker  
    I'M A LIL' TEAPOT  
    WHAT'S WITH THAT AIRPLANE FOOD, HUH?  
    IT'S SO SMALL, TASTELESS
    
Genial, funciona ¿Qué pasaría si ejecutamos ``capslocker`` e intentamos
escribir líneas de texto nosotros mismos? 

.. code-block:: console

    $ ./capslocker  
    hey ho  
    HEY HO  
    lets go  
    LETS GO

Salimos pulsando *Ctrl+D*. Como ves, muestra nuestra entrada en mayúsculas
línea por línea. Cuando el resultado de ``getContents`` se liga a
``contents``, no se representa en memoria como una cadena real, si no más bien
como una promesa de que al final producirá una cadena. Cuando trazamos
``toUpper`` sobre ``contents``, también es una promesa de que se trazará esa
función sobre el contenido final. Por último, cuando se ejecuta ``putStr`` le
dice a la promesa anterior: "Hey ¡Necesito una línea en mayúsculas!". Entonces
es cuando en realidad ``getContents`` lee la entrada y le pasa una línea al
código que le ha pedido que produzca algo tangible. Ese código traza
``toUpper`` sobre esa línea y le pasa el resultado a ``putStr``, y ésta se
encarga de mostrarla. Luego ``putStr`` dice: "Hey, necesito la siguiente
línea ¡Vamos!" y se repite hasta que no hay mas datos en la entrada, lo cual
se representa con el carácter de fin de fichero.

Vamos a crear un programa que tome algunas líneas y luego solo muestre
aquellas que tengan una longitud menor de 10 caracteres. Observa: ::

    main = do  
        contents <- getContents  
        putStr (shortLinesOnly contents)  

    shortLinesOnly :: String -> String  
    shortLinesOnly input =   
        let allLines = lines input  
            shortLines = filter (\line -> length line < 10) allLines  
            result = unlines shortLines  
        in  result
        
Hemos hecho la parte de nuestro programa dedicada a E/S tan pequeña como a
sido posible. Ya que nuestro programa se supone que toma una entrada y muestra
una salida basándose en la entrada, podemos implementarlo leyendo los
contenidos de la entrada, ejecutando una función sobre ellos y luego mostramos
lo que nos devuelve esa función.

La función ``shortLinesOnly`` funciona así: toma una cadena, como
``"short\nlooooooooooooooong\nshort again"``. Esta cadena tiene tres líneas,
dos de ellas son cortas y la del medio es larga. Ejecuta la función ``lines``
sobre esa cadena, de forma que obtenemos ``["short", "looooooooooooooong",`
`"short again"]`` que luego ligamos a ``allLines``. Luego esta lista de
cadenas es filtrada de forma que solo las líneas que sean menores de 10
caracteres de longitud permanecen en la lista, produciendo ``["short",
"short again"]``. Finalmente ``unlines`` concatena la lista en una única
cadena, devolviendo ``"short\nshort again"``. Vamos a probarlo.

.. code-block:: console

    i'm short  
    so am i  
    i am a loooooooooong line!!!  
    yeah i'm long so what hahahaha!!!!!!  
    short line  
    loooooooooooooooooooooooooooong  
    short
    
.. code-block:: console

    $ ghc --make shortlinesonly  
    [1 of 1] Compiling Main             ( shortlinesonly.hs, shortlinesonly.o )  
    Linking shortlinesonly ...  
    $ cat shortlines.txt | ./shortlinesonly  
    i'm short  
    so am i  
    short
    
Redireccionamos los contenidos de ``shortlines.txt`` a la entrada de
``shortlinesonly``, de forma que obtenemos únicamente las líneas cortas.

Este patrón de tomar una cadena como entrada, transformarla con una función y
mostrar el resultado de esa transformación es tan común que existe una función
que hace esto más fácil, la función :cpp:member:`interact`. ``interact`` toma una
función del tipo ``String -> String`` como parámetro y devuelve una acción de
E/S que tomara la entrada del programa, ejecutará la función sobre ella y
mostrará por pantalla el resultado de esta función. Vamos a modificar nuestro
programa para que utilice esta función. ::

    main = interact shortLinesOnly  

    shortLinesOnly :: String -> String  
    shortLinesOnly input =   
        let allLines = lines input  
            shortLines = filter (\line -> length line < 10) allLines  
            result = unlines shortLines  
        in  result
        
Con el propósito de mostrar que podemos conseguir lo mismo con mucho menos
código (incluso aunque sea un poco menos legible) y demostrar nuestras
habilidades de composición de funciones, vamos a modificarlo un poco más. ::

    main = interact $ unlines . filter ((<10) . length) . lines  
    
Wau ¡Lo hemos reducido a una única línea de código!

``interact`` se puede utilizar para crear programas a los que se les
redireccionará algún contenido y luego mostrará un resultado, o para crear
programas que parezcan que leen una línea escrita por el usuario desde la
entrada, muestren un resultado basándose en esa línea y luego continúen con
otra línea. En realidad no hay ninguna diferencia entre ellos, simplemente
depende de como lo use el usuario.

Vamos a crear un programa que lea continuamente una línea y nos diga si esa
línea es un palíndromo o no. Podríamos simplemente utilizar ``getLine`` para
leer una línea, mostrar al usuario si es palíndroma o no, y volver a ejecutar
``main``. Pero es más simple si utilizamos ``interact``. Cuando utilices
``interact``, piensa en que tienes que hacer para transformar la entrada del
programa en la salida que deseas. En nuestro caso, tenemos que remplazar cada
línea de la entrada en ``"palindrome"`` o ``"not a palindrome"``. Así que
tenemos que transformar algo como ``"elephant\nABCBA\nwhatever"`` en
``"not a palindrome\npalindrome\nnot a palindrome"`` ¡Vamos a intentarlo! ::

    respondPalindromes contents = unlines (map f (lines contents))  
        where isPalindrome xs = xs == reverse xs  
              f xs = if isPalindrome xs then "palindrome" else "not a palindrome"

Vamos a escribirlo en estilo libre de puntos: ::

    respondPalindromes = unlines . map f . lines  
        where isPalindrome xs = xs == reverse xs
              f xs = if isPalindrome xs then "palindrome" else "not a palindrome"
        
Sencillo. Primero convierte algo como ``"elephant\nABCBA\nwhatever"`` en
``["elephant", "ABCBA", "whatever"]`` y luego traza ``f`` sobre la lista,
devolviendo ``["not a palindrome", "palindrome", "not a palindrome"]``. Por
último utiliza ``unlines`` para concatenar la lista de cadenas en una sola
cadena. Ahora podemos hacer: ::

    main = interact respondPalindromes  
    
Vamos a comprobarlo.

.. code-block:: console

    $ runhaskell palindromes.hs  
    hehe  
    not a palindrome  
    ABCBA  
    palindrome  
    cookie  
    not a palindrome

Incluso aunque hemos creado un programa que transforma una gran cadena de
entrada en otra, actúa como si hubiéramos hecho un programa que lee línea a
línea. Esto se debe a que Haskell es perezoso y quiere mostrar la primera
línea del resultado, pero no lo puede hacer porque aún no tiene la primera
línea de la entrada. Así que tan pronto tenga la primera línea de la entrada,
mostrará la primera línea de la salida. Salimos del programa utilizando el
carácter de fin de fichero.

También podemos utilizar el programa redireccionando el contenido de un
fichero. Digamos que tenemos este fichero: 

.. code-block:: none

    dogaroo  
    radar  
    rotor  
    madam

Y lo hemos guardado como ``words.txt``. Así sería como redireccionaríamos el
fichero a la entrada de nuestro programa.

.. code-block:: console

    $ cat words.txt | runhaskell palindromes.hs  
    not a palindrome  
    palindrome  
    palindrome  
    palindrome
    
De nuevo, obtenemos la misma salida que si hubiésemos ejecutado nuestro
programa y hubiésemos tecleado nosotros mismos las palabras. Simplemente no
vemos la entrada de ``palindromes.hs`` porque ha sido redireccionada desde
un fichero.

Probablemente ya sepas como funciona E/S perezosa y como se puede aprovechar.
Puedes pensar en términos que como se supone que debe ser la salida y escribir
una función que haga la transformación. En la E/S perezosa, no se consume nada
de la entrada hasta que realmente tenga que hacerse, es decir, cuando queramos
mostrar por pantalla algo que depende de la entrada. 

Hasta ahora, hemos trabajado con la E/S mostrando y leyendo cosas de la
terminal ¿Pero qué hay de escribir y leer ficheros? Bueno, de cierto modo, ya
lo hemos hecho. Se puede pensar que leer algo desde la terminal es como leer
algo desde un fichero especial. Lo mismo ocurre a la hora de escribir en la
terminal, es parecido a escribir en un fichero. Podemos llamar a estos dos
ficheros ``stdout`` y ``stdin``, que representan la salida estándar y la
entrada estándar respectivamente. Teniendo esto en cuenta, veremos que
escribir y leer ficheros es muy parecido a escribir en la salida estándar y
leer desde la entrada estándar.

Empezaremos con un programa realmente simple que abre un fichero llamado
``girlfriend.txt``, que contiene un verso del éxito Nº 1 de *Avril Lavigne*,
*Girlfriend*, y lo muestra por la terminal. Aquí tienes ``girlfriend.txt``:

.. code-block:: none

    Hey! Hey! You! You!   
    I don't like your girlfriend!   
    No way! No way!   
    I think you need a new one!
    
Y aquí tienes nuestro programa: ::

    import System.IO  

    main = do  
        handle <- openFile "girlfriend.txt" ReadMode  
        contents <- hGetContents handle  
        putStr contents  
        hClose handle
        
Ejecutándolo, obtenemos el resultado esperado:

.. code-block:: console

    $ runhaskell girlfriend.hs  
    Hey! Hey! You! You!  
    I don't like your girlfriend!  
    No way! No way!  
    I think you need a new one!
    
Vamos a analizarlo línea a línea. La primera línea son solo cuatro
exclamaciones que intentan llamar nuestra atención. En la segunda línea, Avril
nos dice que no le gusta nuestra actual pareja. La tercera línea tiene como
objetivo enfatizar su desacuerdo, mientras que la cuarta nos sugiere que
busquemos una nueva novia.

¡Genial! Ahora vamos a analizar nuestro programa línea a línea. El programa
tiene varias acciones de E/S unidas en un bloque ``do``. En la primera línea
del bloque ``do`` vemos que hay una función nueva llamada :cpp:member:`openFile`. Su
tipo es el siguiente: ``openFile :: FilePath -> IOMode -> IO Handle``. Si lo
lees en voz alta dice: ``openFile`` toma la ruta de un fichero y un ``IOMode``
y devuelve una acción de E/S que abrirá el fichero indicado y contendrá un
manipulador como resultado.

``FilePath`` es simplemente un :ref:`sinónimo de tipo <sinonimos>` de
``String``, se define como: ::

    type FilePath = String  
    
``IOMode`` es un tipo que se define como: ::

    data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode  
    
.. image:: /images/file.png
   :align: left
   :alt: Preso

De la misma forma que aquel tipo que creamos que representaba los siete días
de la semana, este tipo es una enumeración que representa lo que queremos
hacer con un fichero abierto. Muy simple. Fíjate que el tipo es ``IOMode`` y
no ``IO Mode``. ``IO Mode`` sería una acción de E/S que contendría un valor
del tipo ``Mode`` como resultado, pero ``IOMode`` es simplemente una
enumeración.

Al final esta función devuelve una acción de E/S que abrirá el fichero
indicado del modo indicado. Si ligamos la acción a algo al final obtenemos
un ``Handle``. Un valor del tipo ``Handle`` representa donde está nuestro
fichero. Lo usaremos para manipular el fichero de forma que sepamos de donde
leer y escribir datos. Sería un poco estúpido abrir un fichero y no ligar el
manipulador ya que no podríamos hacer nada con ese fichero. En nuestro caso
ligamos el manipulador a ``handle``.

En la siguiente línea vemos una función llamada :cpp:member:`hGetContents`. Toma un
``Handle``, de forma que sabe de donde tiene que leer el contenido, y devuelve
una ``IO String``, una acción de E/S que contiene como resultado el contenido
del fichero. Esta función se parece mucho a ``getContents``. La única
diferencia es que ``getContents`` lee automáticamente desde la entrada
estándar (es decir desde la terminal), mientras que ``hGetContents`` toma el
manipulador de un fichero que le dice donde debe leer. Por lo demás, funcionan
exactamente igual. Al igual que ``getContents``, ``hGetContents`` no leerá 
todo el contenido de un fichero de golpe si con forme lo vaya necesitando.
Esto es muy interesante ya que podemos tratar a ``contents`` como si fuera
todo el contenido del fichero, solo que en realidad no estará cargado en
la memoria. En caso de que leyéramos un fichero enorme, ejecutar
``hGetContents`` no saturaría la memoria ya que solo se leerá lo que se vaya
necesitando.

Fíjate en la diferencia entre el manipulador utilizado para representar el
fichero y los contenidos del fichero, ligados en nuestro programa a ``handle``
y ``contents``. El manipulador es algo que representa el fichero con el que
estamos trabajando. Si te imaginas el sistema de ficheros como si fuera un
gran libro y cada fichero fuera un capítulo del libro, el manipulador sería
como un marcador que nos indica por donde estamos leyendo (o escribiendo) en
un capítulo, mientras que el contenido sería el capítulo en si.

Con ``putStr contents`` simplemente mostramos el contenido del fichero por
la salida estándar. Luego ejecutamos :cpp:member:`hClose`, el cual toma un
manipulador y devuelve una acción de E/S que cierra el fichero ¡Tienes que
cerrar tu mismo cada fichero que abras con ``openFile``! 

Otra forma de hacer lo que mismo que acabamos de hacer es utilizando la
función :cpp:member:`withFile`, cuya declaración de tipo es ``withFile :: FilePath ->
IOMode -> (Handle -> IO a) -> IO a``. Toma la ruta de un fichero, un
``IOMode`` y luego toma una función que a su vez toma un manipulador y
devuelve una acción de E/S. ``withFile`` devuelve una acción de E/S que
abrirá el fichero indicado, hará algo con él y luego cerrará el fichero. El
resultado contenido en la acción de E/S final es el mismo que el resultado
contenido en la acción de E/S de la función que se le pasa como parámetro.
Quizá te suente un poco complicado, pero es realmente simple, especialmente
con la ayuda de las lambdas. Aquí tienes nuestro programa anterior reescrito
utilizando ``withFile``: ::

    import System.IO     

    main = do     
        withFile "girlfriend.txt" ReadMode (\handle -> do  
            contents <- hGetContents handle     
            putStr contents)
            
Como puedes ver ambos son muy parecidos. ``(\handle -> ... )`` es la función
que toma un manipulador y devuleve una acción de E/S y de forma habitual esta
función se implementea utilizando lambdas. La razón por la que debe tomar
una función que devuelva una acción de E/S en lugar de tomar directamente una
acción de E/S para hacer algo y luego cerrar el fichero, es para que la
función que le pasemos sepa sobre que fichero operar. De este modo,
``withFile`` abre un fichero y le pasa el manipulador a la función que le
demos. Obtiene una acción de E/S como resultado y luego crear una acción de
E/S que se comporte de la misma forma, solo que primero cierra el fichero.
Así sería como implementaríamos la función ``withFile``: ::

    withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a  
    withFile' path mode f = do  
        handle <- openFile path mode   
        result <- f handle  
        hClose handle  
        return result
        
.. image:: /images/edd.png
   :align: right
   :alt: Edd
   
Sabemos que el resultado debe ser una acción de E/S así que podemos empezar
directamente con un ``do``. Primero abrimos el fichero y obtenemos el
manipulador. Luego aplicamos ``handle`` a nuestra función y obtenemos una
acción de E/S que realizará todo el trabajo. Ligamos esa acción a ``result``,
cerramos el fichero y hacemos ``return result``. Al realizar el ``return``
sobre el resultado que contenia la acción de E/S que obtuvimos de ``f``,
hacemos que nuestra acción de E/S contenga el mismo resultado que obtuvimos
de ``f handle``. Así que si ``f handle`` devuleve una acción que lea un
número de líneas de la entrada estándar y luego las escriba en el fichero,
de forma que contenga como resultado el número de líneas que ha leído, la
acción resultante de ``withFile'`` también tendrá como resultado el número de
líneas leidas.

De la misma forma que ``hGetContents`` funciona igual que ``getContents`` pero
sobre el fichero indicado, existen también :cpp:member:`hGetLine`, :cpp:member:`hPutStr`,
:cpp:member:`hPutStrLn`, :cpp:member:`hGetChar`, etc. Funcionan exactamente igual que sus
homónimas, solo que toman un manipulador como parámetro y operar sobre el
fichero indicado en lugar de sobre la entrada o salida estándar. Por ejemplo,
``putStrLn`` es una función que toma una cadena y devuelve una acción de E/S
que mostrará esa cadena por la terminal seguida de un salto de línea.
``hPutStrLn`` toma un manipulador y una cadena y devuelve una acción de E/S
que escribirá esa cadena en el fichero indicado, seguido de un salto de línea.
Del mismo modo, ``hGetLine`` toma un manipulador y devuelve una acción de E/S
que lee una línea de su fichero.

Cargar ficheros y luego tratar sus conteidos como cadenas es algo tan común
que tenemos estas tres pequeñas funciones que hacen nuestra vida más fácil:

 * :cpp:member:`readFile` tiene la declaración de tipo ``readFile :: FilePath -> IO
   String``. Recueda, ``FilePath`` es solo un sinónimo de ``String``.
   ``readFile`` toma la ruta de un fichero y devuelve un acción de E/S que
   leerá ese fichero (de forma perezosa) y ligará sus contenidos a una cadena.
   Normalmente es más cómodo que hacer ``openFile`` y ligar su manipulador
   para luego utilizar ``hGetContents``. Aquí tienes como sería nuestro
   ejemplo anterior utilizando ``readFile``: ::
   
        import System.IO  

        main = do  
            contents <- readFile "girlfriend.txt"  
            putStr contents
            
   Como no obtenemos un manipulador con el cual identificar nuestro fichero,
   no podemos cerrarlo manualmente, así que Haskell se encarga de cerrarlo por
   nosotros cuando utilizamos ``readFile``.
   
 * :cpp:member:`writeFile` tiene el tipo ``FilePath -> String -> IO ()``. Toma la
   ruta de un fichero y una cadena que escribir en ese fichero y devuelve una
   acción de E/S que se encargará de escribirla. En caso de que el fichero
   indicado ya exista, sobreescribirá el fichero desde el incio. Aquí tienes
   como convertir ``girlfriend.txt`` en una versión en mayúsculas y guardarlo
   en ``girlfriendcaps.txt``: ::
   
        import System.IO     
        import Data.Char  

        main = do     
            contents <- readFile "girlfriend.txt"     
            writeFile "girlfriendcaps.txt" (map toUpper contents)
            
   .. code-block:: console
        
        $ runhaskell girlfriendtocaps.hs  
        $ cat girlfriendcaps.txt  
        HEY! HEY! YOU! YOU!  
        I DON'T LIKE YOUR GIRLFRIEND!  
        NO WAY! NO WAY!  
        I THINK YOU NEED A NEW ONE!
        
 * :cpp:member:`appendFile` tiene el mismo tipo que ``writeFile``, solo que
   ``appendFile`` no sobreescribe el fichero desde el principio en caso de que
   el fichero indicado ya exista, sino que añade contiendo al final del
   fichero.

   Digamos que tenemos un fichero ``todo.txt`` que contiene una tarea que
   debemos realizar en cada línea. Ahora vamos a crear un programa que tome
   una línea por la entrada estándar y la añada a nuestra lista de tareas. ::
   
        import System.IO     

        main = do     
            todoItem <- getLine  
            appendFile "todo.txt" (todoItem ++ "\n")
            
   .. code-block:: console
   
        $ runhaskell appendtodo.hs  
        Iron the dishes  
        $ runhaskell appendtodo.hs  
        Dust the dog  
        $ runhaskell appendtodo.hs  
        Take salad out of the oven  
        $ cat todo.txt  
        Iron the dishes  
        Dust the dog  
        Take salad out of the oven
        
   Tenemos que añadir ``"\n"`` al final de cada línea ya que ``getLine`` no
   nos devuelve el carácter de fin de línea al final.
   
Oh, una cosa más. Hemos hablado de como al hacer ``contents <- hGetContents
handle`` no se provoca que el fichero enetero sea leído de golpe y guardado en
memoria. Es una acción de E/S perezosa, así que al hacer esto: ::

    main = do   
        withFile "something.txt" ReadMode (\handle -> do  
            contents <- hGetContents handle  
            putStr contents)
            
En realidad es como redireccionar el fichero a la salida. De la misma forma
que puedes tratar las cadenas como flujos de datos también puedes tratar
los ficheros como flujos de datos. Esto leerá una línea cada vez y la mostrará
por pantalla. Probablemente te estes preguntado ¿Con qué frecuencia se accede
al disco? ¿Qué tamaño tiene cada transferencia? Bueno, para ficheros de texto,
el tamaño por defecto para el búfer es una línea. Esto siginfica que la parte
más pequeña que se puede leer de fichero de una sola vez es una línea. Por
este motivo el ejemplo anterior en realidad leía una línea, la mostraba, leía
otra línea, la mostraba, etc. Para ficheros binarios, el tamaño del búfer
suele ser de un bloque. Esto significa que los ficheros binarios se leen de
bloques en bloques. El tamaño de un bloque es el que le apetezca a tu sistema
operativo.

Puedes controlar como se comporta exactamente el búfer utilizando la función
``hSetBuffering``. Ésta toma un manipulador y un ``BufferMode`` y devuelve una
acción de E/S que estable las propiedades del búfer para ese fichero.
``BufferMode`` es una simple tipo de enumeración y sus posibles valores son:
``NoBuffering``, ``LineBuffering`` or ``BlockBuffering (Maybe Int)``. El
``Maybe Int`` indica el tamaño del bloque, en bytes. Si es ``Nothing``, el
sistema operativo determinará el tamaño apropiado. ``NoBuffering`` significa
que se escribirá o se leera un carácter cada vez. Normalmente ``NoBuffering``
no es muy eficiente ya que tiene que acceder al disco muchas veces.

Aquí tienes nuestro ejemplo anterior, solo que esta vez leerá bloques de
2048 bytes en lugar de línea por línea. ::

    main = do   
        withFile "something.txt" ReadMode (\handle -> do  
            hSetBuffering handle $ BlockBuffering (Just 2048)  
            contents <- hGetContents handle  
            putStr contents)
            
Leer ficheros con bloques grandes nos puede ayudar si queremos minimizar el
acceso a disco o cuando nuestro fichero en realidad es un rescurso de una red
muy lenta.

También podemos utilizr :cpp:member:`hFlush`, que es una función que toma un 
manipulador y devuelve una acción de E/S que vaciará el búfer del fichero
asociado al manipulador. Cuando usamos un búfer de líneas, el búfer se vacía
depués de cada línea. Cuando utilizmos un búfer de bloques, el búfer se vacía
depués de que se lea o escriba un bloque. También se vacía después de cerrar
un manipulador. Esto signfica que cuando alcanzemos un salto de línea, el
mecanismode de lectura (o escritura) informará de todos los datos hasta el
momento. Pero podemos utilizar ``hFlush`` para forzar ese informe de datos.
Depués de realizar el vaciado, los datos están disponibles para cualquier otro
programa que este ejecutandose.

Para entender mejor el búfer de bloques, imagina que la taza de tu retrete
está configurada para vaciarse cuando alcance los cuatro litros de agua en su
interior. Así que empiezas a verter agua en su interior y cuando alcanza
la marca de los cuatro litros automaticamente se vacía, y los datos que
contenian el agua que has vertido hasta el momento son leidos o escritos. Pero
también puedes vaciar manualmente el retrete pulsando el botón que éste posee.
Esto hace que el retrete se vacie y el agua (datos) dentro del retrete es
leida o escrita. Solo por si no te has dado cuenta, vacia manualmente el
retrete es una metáfora para ``hFlush``. Quizá este no sea una buena
analogía en el mundo de las analogías estándar de la programación, pero quería
un objeto real que se pudiera vaciar.

Ya hemos creado un programa que añade una tarea a nuestra lista de tareas
pendientes ``todo.txt``, así que ahora vamos a crear uno que elimine una
tarea. Voy a mostrar el código a continuación y luego recorerremos el progeama
juntos para que veas que es realmente fácil. Usaremos una cuantas funciones
nuevas que se encuentran en ``System.Directory`` y una funcón nueva de
``System.IO``.

De todas formas, aquí tienes el programa que elimina una tarea de
``todo.txt``: ::

    import System.IO  
    import System.Directory  
    import Data.List  

    main = do        
        handle <- openFile "todo.txt" ReadMode  
        (tempName, tempHandle) <- openTempFile "." "temp"  
        contents <- hGetContents handle  
        let todoTasks = lines contents     
            numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks     
        putStrLn "These are your TO-DO items:"  
        putStr $ unlines numberedTasks  
        putStrLn "Which one do you want to delete?"     
        numberString <- getLine     
        let number = read numberString     
            newTodoItems = delete (todoTasks !! number) todoTasks     
        hPutStr tempHandle $ unlines newTodoItems  
        hClose handle  
        hClose tempHandle  
        removeFile "todo.txt"  
        renameFile tempName "todo.txt"
        
Primero abrirmos el fichero ``todo.txt`` en modo lectura y ligamos el
manipulador a ``handle``.

A continuación, utilizamos una función que aún no conocemos y que proviene
de ``System.IO``, :cpp:member:`openTempFile`. Su nombre es bastante auto descriptivo.
Toma la ruta de un directorio temporal y una plantilla para nombres para un
fichero y abre un fichero temporal. Hemos utilizado ``"."`` para el directorio
temporal porque ``"."`` representa el directorio actual en cualquier S.O.
Utilizamos ``"temp"`` como plantilla para el nombre del fichero, de forma que
que el fichero temporal tendra como nombre *temp* más algunos carácteres
aleatorios. Devuelve una acción de E/S que crea un fichero temporal y el
resultado de esa acción es una dupla que contiene: el nombre del fichero
temporal y el manipulador asociado a ese fichero. Podríamo haber abierto
algún fichero normal como ``todo2.txt`` o algo parecido pero es un práctica
mejor utilizar ``openTempFile`` y asegurarse así de que no sobreescribimos
nada.

La razón por la que no hemos utilizado ``getCurrentDirectory`` para obtener
el directorio actual y luego pasarselo a ``openTempFile`` es porque ``"."``
representa el directorio actual tanto es sitemas *unix* como en *Windows*.

Luego ligamos los contenido de ``todo.txt`` a ``contents``. Después dividimos
esa cadena en una lista de cadenas, una cadena por línea. Así que`
``todoTasks`` es ahora algo como ``["Iron the dishes", "Dust the dog",`
`"Take salad out of the oven"]``. Unimos los números desde el 0 en adelante
y esa lista con una función que toma un número, digamos 3, y una cadena, como
``"hey"``, así que ``numberedTasks`` sería ``["0 - Iron the dishes", "1 -`
Dust the dog" ...``. Concatenamos esa lista de cadenas en una sola cadena
delimitada por saltos de línea con ``unlines`` y la mostramos por la terminal.
Fíjate que en lugar de hacer esto podríamos haber hecho algo como ``mapM
putStrLn numberedTasks``.

Le preguntamos al usuario que tarea quiere eliminar y esperamos que introduzca
un número. Digamos que queremos eliminar la número 1, que es ``Dust the dog``,
así que introducimos ``1``. ``numberString`` es ahora ``"1"`` y como queremos
un número y no una cadena, utilizamos ``read`` sobre ella para obtener un
``1`` y ligarlo a ``number``.

Intenta recordar las funciones ``delete`` y ``!!`` del módulo ``Data.List``.
``!!`` devuelve un elemento de una lista dado un índice y ``delete`` elimina
la primera ocurrencia de un elemento en una lista, y devuelve una nueva lista
sin dicho elemento. ``(todoTasks !! number)``, con ``number`` a ``1``, 
devuelve ``"Dust the dog"``. Ligamos ``todoTasks`` sin la primera ocurrencia
de ``"Dust the dog"`` a ``newTodoItems`` y luego unimos todo en una sola
cadena utilizando ``unlines`` antes de escribirlo al fichero temporal que
hemos abierto. El fichero original permanece sin cambios y el fichero temporal
ahora contiene todas las tareas que contiene el original, excepto la que
queremos eliminar.

Después de cerrar ambos archivos, tanto el original como el temporal,
eliminamos el original con :cpp:member:`removeFile`, que, como puedes ver, toma la
ruta de un fichero y lo elimina. Después de eliminar el ``todo.txt`` original,
utilizamos :cpp:member:`renameFile` para renombrar el fichero temporal a
``todo.txt``. Ten cuidad, tanto ``removeFile`` como ``renameFile`` (ambas
contenidas en ``System.Directory``) toman rutas de ficheros y no
manipuladores.

¡Y eso es todo! Podríamos haberlo hecho en menos líneas, pero tenemos cuidado
de no sobreescribir ningún fichero existente y preguntamos educadamente al
sistema operativo que nos diga donde podemos ubicar nuestro fichero temporal
¡Vamos a probarlo!

.. code-block:: console

    $ runhaskell deletetodo.hs  
    These are your TO-DO items:  
    0 - Iron the dishes  
    1 - Dust the dog  
    2 - Take salad out of the oven  
    Which one do you want to delete?  
    1  

    $ cat todo.txt  
    Iron the dishes  
    Take salad out of the oven  

    $ runhaskell deletetodo.hs  
    These are your TO-DO items:  
    0 - Iron the dishes  
    1 - Take salad out of the oven  
    Which one do you want to delete?  
    0  

    $ cat todo.txt  
    Take salad out of the oven


Parámetros de la línea de comandos
----------------------------------


.. image:: /images/arguments.png
   :align: right
   :alt: Parámetros

Prácticamente es una obligación trabajar con parámetros de la línea de
comandos cuando estamos creando un programa que se ejecuta en la terminal. Por
suerte, la biblioteca estándar de Haskell tiene una buena forma de obtener los
parámetros de la línea de comandos.

En la sección anterior, creamos un programa para añadir tareas a nuestra lista
de tareas pendientes y otro programa para eliminar tareas de dicha lista. Hay
dos problemas con el enfoque que tomamos. La primera es que fijamos el nombre
del fichero que contenía la lista en nuestro código fuente. Simplemente
decidimos que sería ``todo.txt`` y el usuario nunca podría trabajar con varias
listas.

Una forma de solventar este problema sería preguntar siempre al usuario con
que lista trabajar. Utilizamos este enfoque cuando quisimos saber que tarea
quería el usuario eliminar. Funciona, pero hay mejores opciones ya que
requiere que el usuario ejecute el programa, espere a que el programa le
pregunte algo y luego decirle lo que necesita. A esto se llama programa
interactivo y el problema de los programas interactivos es: ¿Qué pasa si
queremos automatizar la ejecución del programa? Como en un fichero de comandos
por lotes que ejecuta un programa o varios de ellos.

Por este motivo a veces es mejor que el usuario diga al programa que tiene que
hacer cuando lo ejecuta, en lugar de que el programa tenga que preguntar al
usuario una vez se haya ejecutado. Y que mejor forma de que el usuario diga al
programa que quiere que haga cuando se ejecute que con los parámetros de la
línea de comandos.

El módulo ``System.Environment`` tiene dos acciones de E/S muy interesante.
Una es :cpp:member:`getArgs`, cuya declaración de tipo es ``getArgs :: IO [String]``
y es una acción de E/S que obtendrá los parámetros con los que el programa fue
ejecutado y el resultado que contiene son dichos parámetros en forma de lista.
:cpp:member:`getProgName` tiene el tipo ``IO String`` y es una acción de E/S que
contiene el nombre del programa.

Aquí tienes un pequeño programa que demuestra el comportamiento de estas
funciones: ::

    import System.Environment   
    import Data.List  
  
    main = do  
       args <- getArgs  
       progName <- getProgName  
       putStrLn "The arguments are:"  
       mapM putStrLn args  
       putStrLn "The program name is:"  
       putStrLn progName
   
Ligamos ``getArgs`` y ``getProgName`` a ``args`` y ``progName``. Mostramos
``The arguments are:`` y luego para cada parámetro en ``args`` hacemos
``putStrLn``. Al final también mostramos el nombre del programa. Vamos a
compilar esto como ``arg-test``.

.. code-block:: console

    $ ./arg-test first second w00t "multi word arg"  
    The arguments are:  
    first  
    second  
    w00t  
    multi word arg  
    The program name is:  
    arg-test
    
Bien. Armados con este conocimiento podemos crear aplicaciones de línea de
comandos interesantes. De hecho vamos a continuar y a crear una. En la
sección anterior creamos programas separados para añadir tareas y para
eliminarlas. Ahora vamos a crear un programa con ambas funcionalidades, lo que
haga dependerá de los parámetros de la línea de comandos. También vamos a
permitir que puede trabajar con ficheros diferentes y no solo ``todo.txt``.

Llamaremos al programa ``todo`` y será capaz de hacer tres cosas:

 * Ver las tareas
 * Añadir una tarea
 * Eliminar una tarea
 
No nos vamos a preocupar sobre posibles errores en la entrada ahora mismo.

Nuestro programa estará creado de forma que si queremos añadir la tarea
``Find the magic sword of power`` en el fichero ``todo.txt``, tendremos que
escribir ``todo add todo.txt "Find the magic sword of power"`` en la terminal.
Para ver las tareas simplemente ejecutamos ``todo view todo.txt`` y para
eliminar la tarea con índice 2 hacemos ``todo remove todo.txt 2``.

Empezaremos creando una lista de asociación. Será una simple lista de
asociación que tenga como claves los parámetros de la línea de comandos y
funciones como sus correspondientes valores. Todas estas funciones serán del
tipo ``[String] -> IO ()``. Tomarán la lista de parámetros de la línea de
comandos y devolverán una acción de E/S que se encarga de mostrar las tareas,
añadir una tarea o eliminar una tarea. ::

    import System.Environment   
    import System.Directory  
    import System.IO  
    import Data.List  
  
    dispatch :: [(String, [String] -> IO ())]  
    dispatch =  [ ("add", add)  
                , ("view", view)  
                , ("remove", remove)  
                ]

Tenemos que definir ``main``, ``add``, ``view`` y ``remove``, así que
empecemos con ``main``. ::

    main = do  
        (command:args) <- getArgs  
        let (Just action) = lookup command dispatch  
        action args
        
Primero, ligamos los parámetros a ``(command:args)``. Si te acuerdas del
ajuste de patrones, esto significa que el primer parámetro se ligará con
``command`` y el resto de ellos con ``args``. Si ejecutamos nuestro programa
como ``todo add todo.txt "Spank the monkey"``, ``command`` será ``"add"`` y
``args`` será ``["todo.txt", "Spank the monkey"]``.

En la siguiente línea buscamos el comando en lista de asociación. Como
``"add"`` se asocia con ``add``, obtendremos ``Just add`` como resultado.
Utilizamos de nuevo el ajuste de patrones para extraer esta función del tipo
``Maybe`` ¿Qué pasaría si el comando no estuviese en la lista de asociación?
Bueno, entonces devolvería ``Nothing``, pero ya hemos dicho que no nos vamos
a preocupar demasiado de los errores en la entrada, así que el ajuste de
patrones fallaría y junto a él nuestro programa.

Para terminar, llamamos a la función ``action`` con el resto de la lista de
parámetros. Esto devolverá una acción de E/S que o bien añadirá una tarea, o
bien mostrará una lista de tareas, o bien eliminará una tarea. Y como está
acción es parte del bloque ``do`` de ``main``, se ejecutará. Si seguimos el
ejemplo que hemos utilizado hasta ahora nuestra función ``action`` será
``add``, la cual será llamada con ``args`` (es decir con ``["todo.txt",
"Spank the monkey"]``) y devolverá una acción que añadirá la tarea ``Spank
the monkey`` a ``todo.txt``. 

¡Genial! Todo lo que nos queda ahora es implementar las funciones ``add``,
``view`` y ``remove``. Empecemos con ``add``: ::

    add :: [String] -> IO ()  
    add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
    
Si ejecutamos nuestro programa como ``todo add todo.txt "Spank the monkey"``,
``"add"`` será ligado a ``command`` en el primer ajuste de patrones del bloque
``main``, mientras que ``["todo.txt", "Spank the monkey"]`` será pasado a la
función que obtengamos de la lista de asociación. Así que como no estamos
preocupándonos acerca de posibles entradas erróneas, podemos usar el ajuste
de patrones directamente sobre una lista con esos dos elementos y devolver una
acción de E/S que añada la tarea al final de fichero, junto con un salto de
línea.

A continuación vamos a implementar la funcionalidad de mostrar la lista de
tareas. Si queremos ver los elementos de un fichero, ejecutamos ``todo view
todo.txt``. Así que en el primer ajuste de patrones, ``command`` será
``"view"`` y ``args`` será ``["todo.txt"]``. ::

    view :: [String] -> IO ()  
    view [fileName] = do  
        contents <- readFile fileName  
        let todoTasks = lines contents  
            numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks  
        putStr $ unlines numberedTasks
        
Ya hicimos algo muy parecido en el programa que eliminaba tareas a la hora de
mostrar las tareas para que el usuario pudiera elegir una, solo que aquí solo
mostramos las tareas.

Y para terminar implementamos ``remove``. Será muy similar al programa que
eliminaba tareas, así que si hay algo que no entiendas revisa la explicación
que dimos en su momento. La principal diferencia es que no fijamos el nombre
del fichero a ``todo.txt`` sino que lo obtenemos como parámetro. Tampoco
preguntamos al usuario el índice de la tarea a eliminar ya que también lo
obtenemos como un parámetro más. ::

    remove :: [String] -> IO ()  
    remove [fileName, numberString] = do  
        handle <- openFile fileName ReadMode  
        (tempName, tempHandle) <- openTempFile "." "temp"  
        contents <- hGetContents handle  
        let number = read numberString  
            todoTasks = lines contents  
            newTodoItems = delete (todoTasks !! number) todoTasks  
        hPutStr tempHandle $ unlines newTodoItems  
        hClose handle  
        hClose tempHandle  
        removeFile fileName  
        renameFile tempName fileName
        
Abrimos el fichero basándonos en ``fileName`` y abrimos un fichero temporal,
eliminamos la línea con índice de línea que el usuario desea eliminar, lo
escribimos en un fichero temporal, eliminamos el fichero original y
renombramos el fichero temporal a ``fileName``.

¡Aquí tienes el programa entero en todo su esplendor! ::

    import System.Environment   
    import System.Directory  
    import System.IO  
    import Data.List  

    dispatch :: [(String, [String] -> IO ())]  
    dispatch =  [ ("add", add)  
                , ("view", view)  
                , ("remove", remove)  
                ]  

    main = do  
        (command:args) <- getArgs  
        let (Just action) = lookup command dispatch  
        action args  

    add :: [String] -> IO ()  
    add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")  

    view :: [String] -> IO ()  
    view [fileName] = do  
        contents <- readFile fileName  
        let todoTasks = lines contents  
            numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks  
        putStr $ unlines numberedTasks  

    remove :: [String] -> IO ()  
    remove [fileName, numberString] = do  
        handle <- openFile fileName ReadMode  
        (tempName, tempHandle) <- openTempFile "." "temp"  
        contents <- hGetContents handle  
        let number = read numberString  
            todoTasks = lines contents  
            newTodoItems = delete (todoTasks !! number) todoTasks  
        hPutStr tempHandle $ unlines newTodoItems  
        hClose handle  
        hClose tempHandle  
        removeFile fileName  
        renameFile tempName fileName
        
.. image:: /images/salad.png
   :align: left
   :alt: Ensalada

Resumiendo: creamos una lista de asociación que asocie los comandos con
funciones que tomen argumentos de la línea de comandos y devuelvan acciones
de E/S. Vemos que comando quiere ejecutar el usuario y obtenemos la función
apropiada a partir de la lista de asociación. Llamamos a esa función con el
resto de parámetros de la línea de comandos y obtenemos una acción de E/S que
realizará la acción apropiada cuando sea ejecutada.

En otros lenguajes, deberíamos haber implementado esto utilizando un gran
``switch`` o cualquier otra cosa, pero gracias a las funciones de orden
superior se nos permite crear una lista de asociación que nos devolverá la
acción de E/S adecuada para cada comando que pasemos por la línea de comandos.

¡Vamos a probar nuestra aplicación!

.. code-block:: console

    $ ./todo view todo.txt  
    0 - Iron the dishes  
    1 - Dust the dog  
    2 - Take salad out of the oven  

    $ ./todo add todo.txt "Pick up children from drycleaners"  

    $ ./todo view todo.txt  
    0 - Iron the dishes  
    1 - Dust the dog  
    2 - Take salad out of the oven  
    3 - Pick up children from drycleaners  

    $ ./todo remove todo.txt 2  

    $ ./todo view todo.txt  
    0 - Iron the dishes  
    1 - Dust the dog  
    2 - Pick up children from drycleaners

Otra cosa interesante acerca de esto es que bastante sencillo añadir
funcionalidad adicional. Simplemente tenemos que agregar un elemento más en
la lista de asociación y luego implementar la función correspondiente. Como
ejercicio puedes implementar el comando ``bump`` que tomará un fichero y un
y un índice de una tarea y hará que dicha tarea aparezca al principio de la
lista de tareas pendientes.

Puedes hacer que este programa fallé de forma más elegante en caso de que
reciba unos parámetros erróneos (como por ejemplo ``todo UP YOURS HAHAHAH``)
creando una acción de E/S que simplemente informe que ha ocurrido un error
(digamos ``errorExit :: IO ()``) y luego comprar si hay algún parámetro
erróneo para realizar el informe. Otra forma sería utilizando excepciones, lo
cual veremos dentro de poco.


Aleatoriedad
------------


.. image:: /images/random.png
   :align: right
   :alt: Aleatorio

Muchas veces mientras programamos, necesitamos obtener algunos datos
aleatorios. Quizá estemos haciendo un juego en el que se tenga que lanzar un
dado o quizá necesitemos generar algunos datos para comprobar nuestro
programa. Hay mucho usos para los datos aleatorios. Bueno, en realidad,
pseudo-aleatorios, ya que todos sabemos que la única fuente verdadera de
aleatoriedad en un mono sobre un monociclo con un trozo de queso en un mano
y su trasero en la otra. En esta sección, vamos a ver como Haskell genera
datos aparentemente aleatorios. 

En la mayoría de los otros lenguajes, tenemos funciones que nos devuelven
números aleatorios. Cada vez que llamas a la función, obtienes (con suerte)
un número aleatorio diferente. Bueno, recuerda, Haskell es un leguaje
funcional puro. Por lo tanto posee transparencia referencial. Lo que
significa que una función, dados los mismo parámetros, debe producir el mismo
resultado. Esto es genial ya que nos permite razonar sobre los programas de
una forma diferente y nos permite retrasar la evaluación de las operaciones
hasta que realmente las necesitemos. Si llamamos a una función, podemos estar
seguros de que no hará cualquier otra cosa antes de darnos un resultado. Todo
lo que importa es su resultado. Sin embargo, esto hace un poco complicado
obtener números aleatorios. Si tenemos una función como: ::

    randomNumber :: (Num a) => a  
    randomNumber = 4
    
No será muy útil como función de números aleatorios ya que siempre nos
devolverá el mismo ``4``, aunque puedo asegurar que ese 4 es totalmente
aleatorio ya que acabo de lanzar un dado para obtenerlo. 

¿Qué hacen demás lenguajes para generar número aparentemente aleatorios?
Bueno, primero obtienen algunos datos de tu computadora, como la hora actual,
cuanto y a donde has movido el ratón, el ruido que haces delante del 
computador, etc. Y basándose en eso, devuelve un número que parece aleatorio.
La combinación de esos factores (la aleatoriedad) probablemente es diferente
en cada instante de tiempo, así que obtienes números aleatorios diferentes.

Así que en Haskell, podemos crear un número aleatorio si creamos una función
que tome como parámetro esa aleatoriedad y devuelva un número (o cualquier
otro tipo de dato) basándose en ella.

Utilizaremos el módulo ``System.Random``. Contiene todas las funciones que
calmaran nuestra sed de aleatoriedad. Vamos a jugar con una de las funciones
que exporta, llamada :cpp:member:`random`. Su declaración de tipo es ``random ::
(RandomGen g, Random a) => g -> (a, g)`` ¡Wau! Hay nuevas clases de tipos en
esta declaración. La clase de tipos :cpp:class:`RandomGen` es para tipos que pueden
actuar como fuentes de aleatoriedad. La clase de tipos :cpp:class:`Random` es para
tipos que pueden tener datos aleatorios. Un dato booleano puede tener valores
aleatorios, ``True`` o ``False``. Un número también puede tomar un conjunto
de diferentes valores alotarios ¿Puede el tipo función tomar valores
aleatorios? No creo. Si traducimos la declaración de tipo de ``random`` al
español temos algo como: toma un generador aleatorio (es decir nuestra fuente
de aleatoriedad) y devuelve un valor aleatorio y un nuevo generador aleatorio
¿Por qué devuelve un nuevo generador junto al valor aleatorio? Lo veremos
enseguida.

Para utilizar la función ``random``, primero tenemos que obtener uno de esos
generadores aleatorios. El módulo ``System.Random`` exporta un tipo
interensante llamado :cpp:type:`StdGen` que posee una instancia para la clase de
tipos ``RandomGen``. Podemos crear un ``StdGen`` manualmente o podemos decirle
al sistema que nos de uno basandose en un motón de cosas aleatorias.

Para crear manualmente un generador aletario, utilizamos la función
:cpp:member:`mkStdGen`. Tiene el tipo ``Int -> StdGen``. Toma un entero y basándose
en eso, nos devuelve un generador aleatorio. Bien, vamos a intentar utilizar
el tandem ``random`` ``mkStdGen`` para obtener un número aleatorio. 

.. code-block:: console

    ghci> random (mkStdGen 100)  
    <interactive>:1:0:  
        Ambiguous type variable `a' in the constraint:  
          `Random a' arising from a use of `random' at <interactive>:1:0-20  
        Probable fix: add a type signature that fixes these type variable(s)

¿Qué pasa? Ah, vale, la función ``random`` puede devolver cualquier tipo que
sea miembro de la clase de tipos ``Random``, así que tenemos que decir a
Haskell exactamente que tipo queremos. Recuerda también que devuelve un valor
aleatorio y un generador.

.. code-block:: console

    ghci> random (mkStdGen 100) :: (Int, StdGen)  
    (-1352021624,651872571 1655838864)
    
¡Por fin, un número que parece aleatorio! El primer componente de la dupla es
nuestro número aleatorio mientras que el segundo componente es una
representación textual del nuevo generador ¿Qué sucede si volvemos a llamar
``random`` con el mismo generador?

.. code-block:: console

    ghci> random (mkStdGen 100) :: (Int, StdGen)  
    (-1352021624,651872571 1655838864)

Por supuesto. El mismo resultado para los mismos parámetros. Vamos a probar
dándole como parámetro un generador diferente.

.. code-block:: console

    ghci> random (mkStdGen 949494) :: (Int, StdGen)  
    (539963926,466647808 1655838864)
    
Genial, un número diferente. Podemos usar la anotación de tipo con muchos
otros tipos.    

.. code-block:: console

    ghci> random (mkStdGen 949488) :: (Float, StdGen)  
    (0.8938442,1597344447 1655838864)  
    ghci> random (mkStdGen 949488) :: (Bool, StdGen)  
    (False,1485632275 40692)  
    ghci> random (mkStdGen 949488) :: (Integer, StdGen)  
    (1691547873,1597344447 1655838864)
    
Vamos a crear una función que simule lanzar una modena tres veces. Si
``random`` no devolviera un generador nuevo junto con el valor aleatorio, 
tendríamos que hacer que esta función tomara tres generadores como parámetros
y luego devolver un resultado por cada uno de ellos. Pero esto parece que no
es muy correcto ya que si un generador puede crear un valor aleatorio del
tipo ``Int`` (el cual puede tener una gran variedad de posibles valores)
debería ser capaz de simular tres lazamientos de una moneda (que solo puede
tener ocho posibles valores). Así que este es el porqué de que ``random``
devuelva un nuevo generador junto al valor generado.

Represtaremos el resultado del lanzamiento de una moneda con un simple
``Bool``. ``True`` para cara, ``False`` para cruz. ::

    threeCoins :: StdGen -> (Bool, Bool, Bool)  
    threeCoins gen =   
        let (firstCoin, newGen) = random gen  
            (secondCoin, newGen') = random newGen  
            (thirdCoin, newGen'') = random newGen'  
        in  (firstCoin, secondCoin, thirdCoin)
        
Llamamos a ``random`` con el generador que obtivimos como parámetro y
obtenemos el resultado de lanzar una moneda junto a un nuevo generador. Luego
volvemos a llamar la misma función, solo que esta vez con nuestro nuevo
generador, de forma que obtenemos el segundo resultado. Si la hubiéramos
llamado con el mismo generador las tres veces, todos los resultados hubieran
sido iguales y por tanto solo hubiéramos podido obtener como resultados
``(False, False, False)`` o ``(True, True, True)``.

.. code-block:: console

    ghci> threeCoins (mkStdGen 21)  
    (True,True,True)  
    ghci> threeCoins (mkStdGen 22)  
    (True,False,True)  
    ghci> threeCoins (mkStdGen 943)  
    (True,False,True)  
    ghci> threeCoins (mkStdGen 944)  
    (True,True,True)
    
Fíjate que no hemos tendio que hacer ``random gen :: (Bool, StdGen)``. Se
debe a que ya hemos especificado en la declaración de tipo de la función que
queremos valores booleanos. Por este motivo Haskell puede inferir que queremos
valores booleanos.

¿Y qué pasaría si quisiéramos lanzar la moneda cuatro veces? ¿Y cinco? Bien,
para eso tenemos la función llamada :cpp:member:`randoms` que toma un generador y
devulve una secuencia infinita de valores aletorios.

.. code-block:: console

    ghci> take 5 $ randoms (mkStdGen 11) :: [Int]  
    [-1807975507,545074951,-1015194702,-1622477312,-502893664]  
    ghci> take 5 $ randoms (mkStdGen 11) :: [Bool]  
    [True,True,True,True,False]  
    ghci> take 5 $ randoms (mkStdGen 11) :: [Float]  
    [7.904789e-2,0.62691015,0.26363158,0.12223756,0.38291094]
    
¿Por qué ``randoms`` no devuelve un nuevo generador junto con la lista?
Podemos implementar la función ``randoms`` de forma muy sencilla como: ::

    randoms' :: (RandomGen g, Random a) => g -> [a]  
    randoms' gen = let (value, newGen) = random gen in value:randoms' newGen
    
Una función recursiva. Obtenemos un valor aleatorio y nuevo generador a parir
del generador actual y creamos una lista que tenga el valor aleatorio como
cabeza y una lista de valores aloratorios basada en el nuevo generador como
cola. Como queremos ser capazes de generar una cantidad infinita valores
aleatorios, no podemos devolver un nuevo generador.

Podríamos crear una función que generara secuencias de números aletorios
finitas y devolviera también un nuevo generador. ::

    finiteRandoms :: (RandomGen g, Random a, Num n) => n -> g -> ([a], g)  
    finiteRandoms 0 gen = ([], gen)  
    finiteRandoms n gen =   
        let (value, newGen) = random gen  
            (restOfList, finalGen) = finiteRandoms (n-1) newGen  
        in  (value:restOfList, finalGen)
        
De nuevo, una funcón recursiva. Decimos que si queremos cero valores
alatorios, devolvemos una lista vacía y el generador que se nos dió. Para
cualquier otra cantidad de valores aleatorios, primero obtenemos un número
aleatorio y nuevo generador. Esto será la cabeza. Luego decimos que la cola
será ``n-1`` valores aleatorios generadors con el nuevo generador. Terminamos
devolviendo la cabeza junto el resto de la lista y el generador que obtuvimos
cuando generamos los ``n-1`` valores aleatorios.

¿Y si queremos obtener un valor aleatorio dentro de un determindo rango? Todos
los enteros que hemos generador hasta ahora son escandalosamente grandes o
pequeños ¿Y si queremos lanzar un dado? Bueno, para eso utilizamos
:cpp:member:`randomR`. Su declaración de tipo es ``randomR :: (RandomGen g, Random a)
:: (a, a) -> g -> (a, g)``, lo que significa que tiene comportamiento similar
a ``random``, solo que primero toma una dupla de valores que establecerán el
límite superior e inferior de forma que el valor aleatorio generado esté 
dentro de ese rango.

.. code-block:: console

    ghci> randomR (1,6) (mkStdGen 359353)  
    (6,1494289578 40692)  
    ghci> randomR (1,6) (mkStdGen 35935335)  
    (3,1250031057 40692)
    
También existe ``randomRs``, la cual produce una secuencia de valores
aleatorios dentro de nuestro rango. 

.. code-block:: console

    ghci> take 10 $ randomRs ('a','z') (mkStdGen 3) :: [Char]  
    "ndkxbvmomg"

Genial, tiene pinta de ser una contraseña de alto secreto.

Puedes estar preguntándote que tienes que ver esta sección con la E/S. Hasta
ahora no hemos visto nada relacionado con la E/S. Bien, hasta ahora siempre
hemos creado nuestro generador de forma manual basándonos en algún entero
arbitrario. El problema es que, en los programas reales, siempre devolverán
los mismos números aleatorios, lo cual no es muy buena idea. Por este motivo
``System.Random`` nos ofrece la acción de E/S :cpp:member:`getStdGen` que tiene 
el tipo ``IO StdGen``. Cuando se inicia la ejecución de un programa, éste 
pregunta al sistema por un buen generador de valores aleatorios y lo almacena
en algo llamado generador global. ``getStdGen`` trae ese generador para que
podamos ligarlo a algo. 

Aquí tienes un programa simple que genera una cadena aleatoria. ::

    import System.Random  
  
    main = do  
        gen <- getStdGen  
        putStr $ take 20 (randomRs ('a','z') gen)

.. code-block:: console

    $ runhaskell random_string.hs  
    pybphhzzhuepknbykxhe  
    $ runhaskell random_string.hs  
    eiqgcxykivpudlsvvjpg  
    $ runhaskell random_string.hs  
    nzdceoconysdgcyqjruo  
    $ runhaskell random_string.hs  
    bakzhnnuzrkgvesqplrx
    
Ten cuidad ya que al llamar dos veces a ``getStdGen`` estamos preguntándole
dos veces al sistema por el mismo generador global. Si hacemos algo como: ::

    import System.Random  

    main = do  
        gen <- getStdGen  
        putStrLn $ take 20 (randomRs ('a','z') gen)  
        gen2 <- getStdGen  
        putStr $ take 20 (randomRs ('a','z') gen2)
        
Obtendremos la misma cadena mostrada dos veces. Una forma de obtener dos
cadenas diferentes de 20 caracteres de longitud es crear una lista infinita
y tomar los 20 primeros caracteres y mostrarlos en una línea, luego tomamos
los 20 siguientes y los mostramos en una segunda línea. Para realizar esto
podemos utilizar la función ``splitAt`` de ``Data.List``, que divide una
lista en un índice dado y devuelve una dupla que tiene la primera parte como
primer componente y la segunda parte como segundo componente. ::

    import System.Random  
    import Data.List  

    main = do  
        gen <- getStdGen  
        let randomChars = randomRs ('a','z') gen  
            (first20, rest) = splitAt 20 randomChars  
            (second20, _) = splitAt 20 rest  
        putStrLn first20  
        putStr second20
        
Otra forma de hacerlo es utilizando la acción :cpp:member:`newStdGen` que divide el
generador de valores aleatorios en dos nuevos generadores. Actualiza el
generador global con uno de ellos y el toro lo de vuelve como resultado de la
acción. ::

    import System.Random  

    main = do     
        gen <- getStdGen     
        putStrLn $ take 20 (randomRs ('a','z') gen)     
        gen' <- newStdGen  
        putStr $ take 20 (randomRs ('a','z') gen')
        
No solo obtenemos un nuevo generador cuando ligamos ``newStdGen``, sino que
el generador global también se actualiza, así que si después utilizamos
``getStdGen`` obtendremos otro generador que será diferente a ``gen``.

Vamos a crear un programa que haga que nuestro usuario adivine el número en
el que estamos pensado. ::

    import System.Random  
    import Control.Monad(when)  

    main = do  
        gen <- getStdGen  
        askForNumber gen  

    askForNumber :: StdGen -> IO ()  
    askForNumber gen = do  
        let (randNumber, newGen) = randomR (1,10) gen :: (Int, StdGen)  
        putStr "Which number in the range from 1 to 10 am I thinking of? "  
        numberString <- getLine  
        when (not $ null numberString) $ do  
            let number = read numberString  
            if randNumber == number   
                then putStrLn "You are correct!"  
                else putStrLn $ "Sorry, it was " ++ show randNumber  
            askForNumber newGen
            
.. image:: /images/jackofdiamonds.png
   :align: left
   :alt: Sota de diamantes

Hemos creado la función ``askForNumber``, que toma un generador de valores
aleatorios y devuelve una acción de E/S que preguntará al usuario por un
número y le dirá si ha acertado o no. Dentro de esta función, primero
generamos un número aleatorio y nuevo generador basándonos en el generador
que obtuvimos como parámetro, los llamamos ``randNumber`` y ``newGen``.
Digamos que el número generado es el ``7``. Luego preguntamos al usuario en
que número estamos pensando. Ejecutamos ``getLine`` y ligamos el resultado a
``numberString``. Cuando el usuario introduce ``7``, ``numberString`` se
convierte en ``"7"``. Luego, utilizamos una cláusula ``when`` para comprobar
si la cadena que ha introducido el usuario está vacía. Si lo está, una acción
de E/S vacía (``return ()``) se ejecutará, terminando así nuestro programa. Si
no lo está, la acción contenida en el bloque ``do`` se ejecutará. Utilizamos
``read`` sobre ``numberString`` para convertirla en un número, el cual 
ahora será ``7``.

.. note:: Si el usuario introduce algo que ``read`` no pueda leer (como
          ``"haha"``), nuestro programa terminará bruscamente con un mensaje
          de error bastante horrendo. Si no te apetece que el programa termine
          de esta forma, utiliza la función :cpp:member:`reads`, que devuelve una
          lista vacía cuando no puede leer una cadena. Cuando si puede
          devuelve una lista unitaria que contiene una dupla con nuestro valor
          deseado como primer componente y una cadena con lo que no ha
          consumido como segundo componente.

Comprobamos si el número que han introducido es igual al número que hemos
generado aleatoriamente y damos al usuario un mensaje apropiado. Luego
llamamos a ``askForNumber`` de forma recursiva, solo que esta vez con el
nuevo generador que hemos obtenido, de forma que obtenemos una acción de E/S
como la que acabamos de ejecutar, solo que depende de un generador diferente.

``main`` consiste básicamente en obtener el generador de valores aleatorio y
llamar a ``askForNumber`` con el generador inicial.

¡Aquí tienes nuestro programa en acción!

.. code-block:: console

    $ runhaskell guess_the_number.hs  
    Which number in the range from 1 to 10 am I thinking of? 4  
    Sorry, it was 3  
    Which number in the range from 1 to 10 am I thinking of? 10  
    You are correct!  
    Which number in the range from 1 to 10 am I thinking of? 2  
    Sorry, it was 4  
    Which number in the range from 1 to 10 am I thinking of? 5  
    Sorry, it was 10  
    Which number in the range from 1 to 10 am I thinking of?
    
Otra forma de hacer el mismo programa sería: ::

    import System.Random  
    import Control.Monad(when)  

    main = do  
        gen <- getStdGen  
        let (randNumber, _) = randomR (1,10) gen :: (Int, StdGen)     
        putStr "Which number in the range from 1 to 10 am I thinking of? "  
        numberString <- getLine  
        when (not $ null numberString) $ do  
            let number = read numberString  
            if randNumber == number  
                then putStrLn "You are correct!"  
                else putStrLn $ "Sorry, it was " ++ show randNumber  
            newStdGen  
            main
            
Es muy similar a la versión anterior, solo que en lugar de hacer una función
que tome un generador y luego se llame a si misma de forma recursiva, hacemos
todo el trabajo en ``main``. Después de decir al usuario si el número que
pensaba es correcto o no, actualizamos el generador global y volvemos a llamar
a ``main``. Ambas implementaciones son válidas pero a mi me gusta más la
primera ya que el ``main`` realiza menos acciones y también nos proporciona
una función que podemos reutilizar.


Cadenas de bytes
----------------


.. image:: /images/chainchomp.png
   :align: right
   :alt: Como un cadena normal, solo que muerde...

Las listas son unas estructuras de datos estupendas además útiles. Hasta ahora
las hemos utilizado en cualquier sitio. Hay una multitud de funciones que
operan con ellas y la evaluación perezosa de Haskell nos permite
intercambiarlas por los bucles a la hora de realizar filtrados y trazados, ya
que la evaluación solo ocurre cuando realmente se necesita, de modo que las
listas infinitas (¡incluso listas infinitas de listas infinitas!) no son un
problema para nosotros. Por este motivo las listas también se pueden utilizar
para representar flujos de datos, ya sea para leer desde la entrada estándar
o desde un fichero. Podemos abrir un fichero y leerlo como si se tratase de
una cadena, incluso aunque solo se acceda hasta donde alcancen nuestras
necesidades.

Sin embargo, procesar ficheros como cadenas tiene un inconveniente: suele ser
lento. Como sabes, ``String`` es sinónimo de tipo de ``[Char]``. ``Char`` no
tiene un tamaño fijo, ya que puede tomar varios bytes para representar un
carácter. Ademas, las listas son perezosas. Si tienes un lista como
``[1,2,3,4]``, se evaluará solo cuando sea completamente necesario. Así que
la lista entera es una especie de promesa de que en algún momento será una
lista. Recuerda que ``[1,2,3,4]`` es simplemente una decoración sintáctica
para ``1:2:3:4:[]``. Cuando el primer elemento de la lista es forzado a 
evaluarse (digamos que mostrándolo por pantalla), el resto de la lista
``2:3:4:[]`` sigue siendo una promesa de una lista, y así continuamente. Así
que puedes pensar en las listas como si se tratasen de promesas de que el
el siguiente elemento será entregado una vez sea necesario. No hace falta
pensar mucho para concluir que procesar una simple lista de números como
una serie de promesas no de debe ser la cosa más eficiente del mundo.

Esta sobrecarga no nos suele preocupar la mayor parte del tiempo, pero si
debería hacerlo al la hora de leer y manipular ficheros de gran tamaño. Por
esta razón Haskell posee **cadenas de bytes**. Las cadenas de bytes son una
especie de listas, solo que cada elemento tiene el tamaño de un byte (o 8
bits). La forma en la que son evaluadas es también diferente.

Existen dos tipos de cadenas de bytes: las estrictas y las perezosas. Las
estrictas residen en ``Data.ByteString`` y no posee ninguna evaluación
perezosa. No hay ninguna promesa involucrada, un cadena de bytes estricta
representa una serie de bytes en un vector. No podemos crear cosas como
cadenas de bytes infinitas. Si evaluamos el primer byte de un cadena de bytes
estricta evaluamos toda la cadena. La ventaja es que hay menos sobrecarga ya
que no implica ningún *thunk* (término técnico de *promesa*). La desventaja es
que consumirán memoria mucho más rápido ya que se leen en memoria de un solo
golpe.

El otro tipo de cadenas de bytes reside en ``Data.ByteString.Lazy``. Son
perezosas, pero no de la misma forma que las listas. Como ya hemos dicho, hay
tantos *thunks* como elementos en una cadena normal. Este es el porqué de que
sean lentas en algunas situaciones. Las cadenas de bytes perezosas toman otra
enfoque, se almacenan en bloques de 64KB de tamaño. De esta forma, si
evaluamos un byte en una cadena de bytes perezosa (mostrándolo por pantalla o
algo parecido), los primeros 64KB serán evaluados. Luego de estos, solo existe
una promesa de que los siguientes serán evaluados. Las cadenas de bytes
perezosas son como una especie de lista de cadenas de bytes de 64KB. Cuando
procesemos ficheros utilizando cadenas de bytes perezosas, los contenidos del
fichero serán leídos bloque a bloque. Es genial ya que no llevará la memoria
hasta sus límite y probablemente 64KB caben perfectamente en la memoria
cache L2 de tu procesador.

Si miras la `Documentación <http://www.haskell.org/ghc/docs/latest/html/libraries/bytestring/Data-ByteString-Lazy.html>`_
de ``Data.ByteString.Lazy``, verás que exporta un montón de funciones que
tienen el mismo nombre que las de ``Data.List``, solo que en sus declaraciones
de tipo tienen ``ByteString`` en lugar de ``[a]`` y ``Word8`` de la ``a`` de
su interior. Las funciones con nombres similares se comportan prácticamente
igual salvo que unas trabajan con listas y las otras con cadenas de bytes.
Como importan nombres de funciones iguales, vamos a importarlas de forma
cualificada en nuestro código y luego lo cargaremos en GHCi para jugar con
con las cadenas de bytes. ::

    import qualified Data.ByteString.Lazy as B  
    import qualified Data.ByteString as S
    
``B`` posee las cadenas de bytes perezosas mientras que ``S`` contiene las
estrictas. Utilizaremos casi siempre la versión perezosa.

La función :cpp:member:`pack` tiene un tipo ``[Word8] -> ByteString``. Lo cual
significa que toma una lista de bytes del tipo ``Word8`` y devuelve una
``ByteString``.  Puedes verlo como si tomara un lista, que es perezosa, y la
hace menos perezosa, de forma que sigue siendo perezosa solo que a intervalos
de 64KB.

¿Qué sucede con el tipo ``Word8``? Bueno, es como ``Int``, solo que tiene un
rango mucho más pequeño, de 0 a 255. Representa un número de 8b. Y al igual
que ``Int``, es miembro de la clase ``Num``. Por ejemplo, sabemos que el
valor 5 es polimórfico ya que puede comportarse como cualquier tipo numeral. 
Bueno, pues también puede tomar el tipo ``Word8``.

.. code-block:: console

    ghci> B.pack [99,97,110]  
    Chunk "can" Empty  
    ghci> B.pack [98..120]  
    Chunk "bcdefghijklmnopqrstuvwx" Empty
    
Como puede ver, normalmente no tienes que preocupar mucho del tipo ``Word8``,
ya que el sistema de tipos puede hacer que los números tomen ese tipo. Si
tratas de utilizar un número muy grande, como 336, como un ``Word8``,
simplemente se truncará de forma binaria al valor 80.

Hemos empaquetado solo unos pocos valores dentro de una cadena de bytes, de
forma que caben dentro de un mismo bloque (``Chunk``). El ``Empty`` es como
``[]`` para las listas.

:cpp:member:`unpack` es la versión inversa de de ``pack``. Toma una cadena de bytes y
la convierte en una lista de bytes.

:cpp:member:`fromChunks` toma una lista de cadenas de bytes estrictas y la convierte
en una cadena de bytes perezosa.  :cpp:member:`toChunks` toma una cadena de bytes
perezosa y la convierte en una estricta.

.. code-block:: console

    ghci> B.fromChunks [S.pack [40,41,42], S.pack [43,44,45], S.pack [46,47,48]]  
    Chunk "()*" (Chunk "+,-" (Chunk "./0" Empty))
    
Esto es útil cuando tienes un montón de cadenas de bytes estrictas y quieres
procesarlas eficientemente sin tener que unirlas en memoria en una más grande
primero. 

La versión de ``:`` para cadenas de bytes se conoce como :cpp:member:`cons`. Toma un
byte y una cadena de bytes y pone dicho byte al principio. Aunque es perezosa,
generará un nuevo bloque para ese elemento aunque dicho bloque aún no este
lleno. Por este motivo es mejor utilizar la versión estricta de ``cons``,
:cpp:member:`cons'`, si vas a insertar un montón de bytes al principio de una cadena
de bytes.

.. code-block:: console

    ghci> B.cons 85 $ B.pack [80,81,82,84]  
    Chunk "U" (Chunk "PQRT" Empty)  
    ghci> B.cons' 85 $ B.pack [80,81,82,84]  
    Chunk "UPQRT" Empty  
    ghci> foldr B.cons B.empty [50..60]  
    Chunk "2" (Chunk "3" (Chunk "4" (Chunk "5" (Chunk "6" (Chunk "7" (Chunk "8" (Chunk "9" (Chunk ":" (Chunk ";" (Chunk "<" Empty))))))))))  
    ghci> foldr B.cons' B.empty [50..60]  
    Chunk "23456789:;<" Empty
    
Como puedes ver :cpp:member:`empty` crea una cadena de bytes vacía ¿Puedes ver las
diferencias entre ``cons`` y ``cons'``? Con ayuda de ``foldr`` hemos empezado
con una cadena de bytes vacía y luego hemos recorrido la lista de números
desde la derecha, añadiendo cada número al principio de la cadena de bytes.
Cuando utilizamos ``cons``, acabamos con un bloque por cada byte, lo cual no
es muy útil para nuestros propósitos.

De cualquier modo, los módulo de cadenas de bytes tienen un montón de
funciones análogas a las de ``Data.List``, incluyendo, pero no limitándose, a
``head``, ``tail``, ``init``, ``null``, ``length``, ``map``, ``reverse``,
``foldl``, ``foldr``, ``concat``, ``takeWhile``, ``filter``, etc.

También contienen funciones con el mismo nombre y comportamiento que algunas
funciones que se encuentran en ``System.IO``, solo que ``String`` se remplaza
por ``ByteString``. Por ejemplo, la función ``readFile`` de ``System.IO``
tiene el tipo ``readFile :: FilePath -> IO String``, mientras que
:cpp:member:`readFile` de los módulos de cadenas de bytes tiene el tipo
``readFile :: FilePath -> IO ByteString``. Ten cuidado, si estás utilizando la
versión estricta de cadenas de bytes e intentas leer un fichero, se leerá en
memoria de un solo golpe. Con las cadenas de bytes perezosas se leerá bloque
a bloque.

Vamos a crear un programa simple que tome dos rutas de ficheros como
parámetros de la línea de comandos y copie el contenido del primero en el
segundo. Ten en cuenta que ``System.Directory`` ya contiene una función
llamada ``copyFile``, pero vamos a implementar nuestro programa así de todas
formas. ::

    import System.Environment  
    import qualified Data.ByteString.Lazy as B  

    main = do  
        (fileName1:fileName2:_) <- getArgs  
        copyFile fileName1 fileName2  

    copyFile :: FilePath -> FilePath -> IO ()  
    copyFile source dest = do  
        contents <- B.readFile source  
        B.writeFile dest contents
        
Creamos nuestra propia función que toma dos ``FilePath`` (recuerda,
``FilePath`` es solo un sinónimo de ``String``) y devuelve una acción de E/S
que copiará el contenido de un fichero utilizando cadenas de bytes. En la
función ``main``, simplemente obtenemos los parámetros y llamamos a nuestra
función con ellos para obtener una acción de E/S que será ejecutada. 

.. code-block:: console

    $ runhaskell bytestringcopy.hs something.txt ../../something.txt
    
Fíjate que un programa que no utilice cadenas de bytes puede tener el mismo
parecido, la única diferencia sería que en lugar de escribir ``B.readFile`` y
``B.writeFile`` usaríamos ``readFile`` y ``writeFile``. Muchas veces podemos
convertir un programa que utilice cadenas a un programa que utilice cadenas
de bytes simplemente utilizando los módulos correctos y cualificando algunas
funciones. A veces, pueden necesitar convertir funciones que trabajan con
cadenas para que funcionen con cadenas de bytes, pero no es demasiado difícil.

Siempre que necesites un mayor rendimiento en programas que lean montones
de datos en forma de cadenas, intenta utilizar cadenas de bytes, tendrás
grandes posibilidades de conseguir un rendimiento mayor con muy poco esfuerzo.
Normalmente yo suelo crear programas que trabajan con cadenas normales y luego
las convierto a cadenas de bytes de el rendimiento no se ajusta a los
objetivos.


Excepciones
-----------


.. image:: /images/timber.png
   :align: left
   :alt: ¡Árbol va!
   
Todos los lenguajes tienen procedimientos, funciones o trozos de código que
fallan de alguna forma. Es una ley de vida. Lenguajes diferentes tienen
formas diferentes de manejar estos fallos. En *C*, solemos utilizar un valor
de retorno anormal (como -1 o un puntero nulo) para indicar que el valor
devuelto no debe ser tratado de forma normal. *Java* y *C#*, por otra parte,
tienden a utilizar excepciones para controlar estos fallos. Cuando se lanza
una excepción, la ejecución de código salta a algún lugar que hemos definido
para realice las tareas apropiadas e incluso quizá relance la excepción para
que sea tratada en otro lugar.

Haskell tiene un buen sistema de tipos. Los tipos de datos algebraicos nos
permiten tener tipos como ``Maybe`` y ``Either`` que podemos utilizar para
representar resultados que son válidos y que no lo son. En *C*, devolver,
digamos -1, cuando suceda un error es una cuestión de convención. Solo tiene
un significado especial para los humanos. Si no tenemos cuidado, podemos
tratar esos datos anormales como válidos de forma que nuestro código termine
siendo un auténtico desastre. El sistema de tipos de Haskell nos da la
seguridad que necesitamos en este aspecto. Una función ``a -> Maybe b``
indica claramente que puede producir un ``b`` envuelto por un ``Just`` o bien
puede devolver ``Nothing``. El tipo es completamente diferente a ``a -> b`` y
si intentamos utilizar estas dos funciones indistintamente, el sistema de
tipos se quejará. 

Aunque aún teniendo tipos expresivos que soporten operaciones erróneas,
Haskell sigue teniendo soporte para excepciones, ya tienen más sentido en
el contexto de la E/S. Un montón de cosas pueden salir mal cuando estamos
tratando con el mundo exterior ya que no es muy fiable. Por ejemplo, cuando
abrimos un fichero, bastantes cosas pueden salir mal. El fichero puede estar
protegido, puede no existir o incluso que no exista un soporte físico para él.
Así que está bien poder saltar a algún lugar de nuestro código que se encargue
de un error cuando dicho error suceda.

Vale, así que el código de E/S (es decir, código impuro) puede lanzar
excepciones. Tiene sentido ¿Pero qué sucede con el código puro? Bueno, también
puede lanzar excepciones. Piensa en las funciones ``div`` y ``head``. Tienen
los tipos ``(Integral a) => a -> a ->`` y ``[a] -> a`` respectivamente. No
hay ningún ``Maybe`` ni ``Either`` en el tipo que devuelven pero aun así
pueden fallar. ``div`` puede fallar si intentas dividir algo por cero y
``head`` cuando le das una lista vacía. 

.. code-block:: console

    ghci> 4 `div` 0  
    *** Exception: divide by zero  
    ghci> head []  
    *** Exception: Prelude.head: empty list
 
.. image:: /images/police.png
   :align: left
   :alt: ¡Alto ahí cirminal!
   
El código puro puede lanzar excepciones, pero solo pueden ser capturadas en
las partes de E/S de nuestro código (cuando estamos dentro de un bloque ``do``
que es alcanzado por ``main``). Esto ocurre así porque no sabemos cuando (o
si) algo será evaluado en el código puro ya que se evalúa de forma perezosa y
no tiene definido un orden de ejecución concreto, mientras que las partes de
E/S sí lo tienen.

Antes hablábamos de como debíamos permanecer el menor tiempo posible en las
partes de E/S de nuestro programa. La lógica de nuestro programa debe
permanecer mayoritariamente en nuestras funciones puras, ya que sus resultados
solo dependen de los parámetros con que las llamemos. Cuando tratas con
funciones puras, solo tenemos que preocuparnos de que devuelve una función, ya
que no puede hacer otra cosa. Esto hace nuestra vida más sencilla. Aunque
realizar algunas tareas en la parte E/S es fundamental (como abrir un fichero
y cosas así), deben permanecer al mínimo. Las funciones puras son perezosas
por defecto, lo que significa que no sabemos cuando serán evaluadas y
realmente tampoco nos debe preocupar. Sin embargo, cuando las funciones puras
empiezan a lanzar excepciones, si importa cuando son evaluadas. Por este
motivo solo podemos capturar excepciones lanzadas desde código puro en las
partes de E/S de nuestro programa. Y como queremos mantener las partes de E/S
al mínimo esto no nos beneficia mucho. Sin embargo, si no las capturamos en
una parte de E/S de nuestro código, el programa se abortará ¿Solución? No
mezcles las excepciones con código puro. Toma ventaja del potente sistema de
tipos de Haskell y utiliza tipos como ``Either`` y ``Maybe`` para representar
resultados que pueden ser erróneos.

Por este motivo, por ahora solo veremos como utilizar las excepciones de E/S.
Las excepciones de E/S ocurren cuando algo va mal a la hora de comunicamos con
el mundo exterior. Por ejemplo, podemos tratar de abrir un fichero y luego
puede ocurrir que ese fichero ha sido eliminado o algo parecido. Fíjate en el
siguiente programa, el cual abre un fichero que ha sido obtenido como
parámetro  y nos dice cuantas líneas contiene. ::

    import System.Environment  
    import System.IO  

    main = do (fileName:_) <- getArgs  
              contents <- readFile fileName  
              putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"  
    
Un programa muy simple. Realizamos la acción de E/S ``getArgs`` y ligamos la
primera cadena de la cadena que nos devuelve a ``fileName``. Luego llamamos a
los contenidos de fichero como ``contents``. Para terminar, aplicamos
``lines`` a esos contenidos para obtener una lista de lineas y luego obtenemos
la longitud de esa lista y la mostramos utilizando ``show``. Funciona de la
forma esperada, pero ¿Qué sucede cuando le damos el nombre de un fichero que
no existe?

.. code-block:: console

    $ runhaskell linecount.hs i_dont_exist.txt  
    linecount.hs: i_dont_exist.txt: openFile: does not exist (No such file or directory)

¡Ajá! Obtenemos un error de *GHC* que nos dice que ese fichero no existe.
Nuestro programa falla ¿Qué pasaría si quisiéramos mostrar un mensaje más
agradable en caso de que el fichero no exista? Una forma de hacerlo sería
comprobando si el fichero existe antes de intentar abrirlo utilizando la
función :cpp:member:`doesFileExist` de ``System.Directory``. ::

    import System.Environment  
    import System.IO  
    import System.Directory  

    main = do (fileName:_) <- getArgs  
              fileExists <- doesFileExist fileName  
              if fileExists  
                  then do contents <- readFile fileName  
                          putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"  
                  else do putStrLn "The file doesn't exist!"

Hicimos ``fileExists <- doesFileExist fileName`` porque ``doesFileExist``
tiene como declaración de tipo ``doesFileExist :: FilePath -> IO Bool``, lo
que significa que devuelve una acción de E/S que tiene como resultado un valor
booleano que nos dice si el fichero existe o no. No podemos utilizar
``doesFileExist`` directamente en una expresión ``if``. 

Otra solución sería utilizando excepciones. Es perfectamente aceptable
utilizarlas en este contexto. Un fichero que no existe es una excepción que
se lanza desde la E/S, así que capturarla en la E/S es totalmente aceptable.

Para tratar con esto utilizando excepciones, vamos a aprovecharnos de la
función :cpp:member:`catch` de ``System.IO.Error``. Su declaración de tipo es
``catch :: IO a -> (IOError -> IO a) -> IO a``. Toma dos parámetros. El
primero es una acción de E/S. Por ejemplo, podría ser una acción que trate de
abrir un fichero. El segundo es lo que llamamos un manipulador. Si la primera
acción de E/S que le pasemos a ``catch`` lanza un excepción, la excepción pasa
al manipulador que decide que hacer. Así que el resultado final será una
acción que o bien actuará como su primer parámetro o bien hará lo que diga el
manipulador en caso de que la primera acción de E/S lance una excepción. 

.. image:: /images/puppy.png
   :align: right
   :alt: Cachorrito

Si te es familiar los bloques *try-catch* de lenguajes como *Java* o *Python*,
la función ``catch`` es similar a ellos. El primer parámetro es lo que hay que
intentar hacer, algo así como lo que hay dentro de un bloque *try*. El segundo
parámetro es el manipulador que toma una excepción, de la misma forma que la
mayoría de los bloques *catch* toman excepciones que puedes examinar para ver
que ha ocurrido. El manipulador es invocado si se lanza una excepción. 

El manipulador toma un valor del tipo ``IOError``, el cual es un valor que
representa que ha ocurrido una excepción de E/S. También contienen información
acerca de la excepción que ha sido lanzada. La implementación de este tipo
depende de la implementación del propio lenguaje, por lo que no podemos
inspeccionar valores del tipo ``IOError`` utilizando el ajuste de patrones
sobre ellos, de la misma forma que no podemos utilizar el ajuste de patrones
con valores del tipo ``IO algo``. Sin embargo, podemos utilizar un montón de
predicados útiles para examinar los valores del tipo ``IOError`` como veremos
en unos segundos.

Así que vamos a poner en uso a nuestro nuevo amigo ``catch``. ::

    import System.Environment  
    import System.IO  
    import System.IO.Error  

    main = toTry `catch` handler  

    toTry :: IO ()  
    toTry = do (fileName:_) <- getArgs  
               contents <- readFile fileName  
               putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"  

    handler :: IOError -> IO ()  
    handler e = putStrLn "Whoops, had some trouble!"
    
Lo primero de todo, puedes ver como hemos utilizado las comillas simples para
utilizar esta función de forma infija, ya que toma dos parámetros. Utilizarla
de forma infija la hace mas legible. Así que ``toTry `catch` handler`` es lo
mismo que ``catch toTry handler``, además concuerda con su tipo. ``toTry`` es
una acción de E/S que intentaremos ejecutar y ``handler`` es la función que
toma un ``IOError`` y devuelve una acción que será ejecutada en caso de que
suceda una excepción. 

Vamos a probarlo:

.. code-block:: console

    $ runhaskell count_lines.hs i_exist.txt  
    The file has 3 lines!  

    $ runhaskell count_lines.hs i_dont_exist.txt  
    Whoops, had some trouble!
    
No hemos comprobado que tipo de ``IOError`` obtenemos dentro de ``handler``. 
Simplemente decimos ``"Whoops, had some trouble!"`` para cualquier tipo de
error. Capturar todos los tipos de excepciones un mismo manipulador no es una
buena práctica en Haskell ni en ningún otro lenguaje ¿Qué pasaría si se
lanzara alguna otra excepción que no queremos capturar, como si interrumpimos 
el programa o algo parecido? Por esta razón vamos a hacer lo mismo que se
suele hacer en otros lenguajes: comprobaremos que tipo de excepción estamos
capturando. Si la excepción es del tipo que queremos capturar, haremos nuestro
trabajo. Si no, relanzaremos esa misma excepción. Vamos a modificar nuestro
programa para que solo capture las excepciones debidas a que un fichero no
exista. ::

    import System.Environment  
    import System.IO  
    import System.IO.Error  

    main = toTry `catch` handler  

    toTry :: IO ()  
    toTry = do (fileName:_) <- getArgs  
               contents <- readFile fileName  
               putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"  

    handler :: IOError -> IO ()  
    handler e  
        | isDoesNotExistError e = putStrLn "The file doesn't exist!"  
        | otherwise = ioError e
        
Todo permanece igual excepto el manipulador, el cual hemos modificado para
que capture únicamente un grupo de excepciones de E/S. Hemos utilizado dos
nuevas funciones de ``System.IO.Error``, :cpp:member:`isDoesNotExistError` y
:cpp:member:`ioError`. ``isDoesNotExistError`` es un predicado sobre ``IOError``, o
lo que es lo mismo, es una función que toma un valor del tipo ``IOError`` y
devuelve ``True`` o ``False``, por lo que su declaración de tipo es
``isDoesNotExistError :: IOError -> Bool``. Hemos utilizado esta función con
la excepción que se le pasa al manipulador para ver si el error fue debido a
que no existía un fichero. Utilizamos también la sintaxis de
:ref:`guardas <guardas>`, aunque podríamos haber utilizado un ``if else``. En
caso de que la excepción no fuera lanzada debido a que no se encuentre un
fichero, relanzamos la excepción que se le pasó al manipulador utilizando la
función ``ioError``. Su declaración de tipo es ``ioError :: IOException ->
IO a``, así que toma un ``IOError`` y produce un acción de E/S que lanza esa
excepción. La acción de E/S tiene el tipo ``IO a`` ya que realmente nunca
devolverá un valor.

Resuminedo, si la excepción lanzada dentro de la acción de E/S ``toTry`` que
hemos incluido dentro del bloque ``do`` no se debe a que no exista un fichero,
``toTry `catch` handler`` capturará esa excepción y la volverá a lanzar.

Existen varios predicados que trabajan con ``IOError`` que podemos utilizar
junto las guardas, ya que, si una guarda no se evalua a ``True``, se seguirá
evaluando la siguiente guarda. Los predicados que trabajan con ``IOError``
son:

 * :cpp:member:`isAlreadyExistsError`
 * :cpp:member:`isDoesNotExistError`
 * :cpp:member:`isAlreadyInUseError`
 * :cpp:member:`isFullError`
 * :cpp:member:`isEOFError`
 * :cpp:member:`isIllegalOperation`
 * :cpp:member:`isPermissionError`
 * :cpp:member:`isUserError`

La moyoría de éstas se explican por si mismas. ``isUserError`` se evalua a
``True`` cuando utilizamos la función :cpp:member:`userError` para crear la
excepción, lo cual se utiliza para crear excepciones en nuestro código y
acompañarlas con una cadena. Por ejemplo, puedes utilizar algo como
``ioError $ userError "remote computer unplugged!"``, aunque es preferible que
utilices los tipos ``Either`` y ``Maybe`` para representar posibles fallos en
lugar de lanzar excepciones por ti mismo con ``userError``. 

Podríamos tener un manipulador que se pareciera a algo como esto: ::

    handler :: IOError -> IO ()  
    handler e  
        | isDoesNotExistError e = putStrLn "The file doesn't exist!"  
        | isFullError e = freeSomeSpace  
        | isIllegalOperation e = notifyCops  
        | otherwise = ioError e
        
Donde ``notifyCops`` y ``freeSomeSpace`` son acciones de E/S que hemos
definido. Asegurate de relanzar las excepciones que no cumplan tu criterio, de
lo contrario harás que tu programa falle de forma sigilosa cuando no debería.

``System.IO.Error`` también exporta algunas funciones que nos permiten
preguntar a estas excepciones por algunos atributos, como qué manipulador 
causó el error, o qué ruta de fichero lo provocó. Estas funciones comienzan
por ``ioe`` y puedes ver la `lista completa <http://www.haskell.org/ghc/docs/6.10.1/html/libraries/base/System-IO-Error.html#3>`_
en la documentación. Digamos que queremos mostrar la ruta de un fichero que
provocó un error. No podemos mostrar el ``fileName`` que obtuvimos de
``getArgs``, ya que solo un valor del tipo ``IOError`` se pasa al manipulador
y manipulador no sabe nada más. Una función depende exclusivamente de los 
parámetros con los que fue llamada. Por esta razón podemos utilizar la
función :cpp:member:`ioeGetFileName`, cuya declaración de tipo es ``ioeGetFileName ::
IOError -> Maybe FilePath``. Toma un ``IOError`` como parámetro y quizá
devuelva un ``FilePath`` (que es un sinónimo de ``String``, así que es
prácticamente lo mismo). Básicamente lo que hace es extraer la ruta de un
fichero de un ``IOError``, si puede. Vamos a modificar el programa anterior
para que muestre la ruta del fichero que provocó una posible excepción. ::

    import System.Environment     
    import System.IO     
    import System.IO.Error     

    main = toTry `catch` handler     

    toTry :: IO ()     
    toTry = do (fileName:_) <- getArgs     
               contents <- readFile fileName     
               putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"     

    handler :: IOError -> IO ()     
    handler e     
        | isDoesNotExistError e =   
            case ioeGetFileName e of Just path -> putStrLn $ "Whoops! File does not exist at: " ++ path  
                                     Nothing -> putStrLn "Whoops! File does not exist at unknown location!"  
        | otherwise = ioError e

Si la guarda donde se encuentra ``isDoesNotExistError`` se evalua a ``True``,
utilizamos una expresión ``case`` para llamar a ``ioeGetFileName`` con ``e``
y aplicamos un ajuste de patrones con el ``Maybe`` que devuelve. Normalmente
utilizamos las expresiones ``case`` cuando queremos aplicar un ajuste de
patrones sin tener que crear una nueva función.

No tienes porque utilizar un manipulador para capturar todas las excepciones
que ocurran en la parte de E/S de tu programa. Puedes cubrir ciertas partes de
tu código de E/S con ``catch`` o puedes cubrir varias de ellas con ``catch``
y utilizar diferentes manipuladores. Algo como: ::

    main = do toTry `catch` handler1  
              thenTryThis `catch` handler2  
              launchRockets
              
Aquí, ``toTry`` utiliza ``handler1`` como manipulador y ``thenTryThis``
utiliza ``handler2``. ``launchRockets`` no es ningún parámetro de nignún
``catch``, así que cualquier excepción que lanze abortará el programa, a no
ser que ``launchRockets`` utilice internamente un ``catch`` que gestione sus
propias excepciones. Por supuesto ``toTry``, ``thenTryThis`` y
``launchRockets`` son acciones de E/S que han sido unidas con un bloque ``do``
e hipotéticamente definidas en algún lugar. Es similar a los bloques
*try-catch* que aparecen en otro lenguajes, donde puedes utilizar un solo
bloque *try-catch* para envolver a todo el programa o puede utilizar un
enfoque más detallado y utilizar bloques diferentes en diferentes partes
del programa.

Ahora ya sabes como tratar las excepciones de la E/S. No hemos visto como
lanzar excepciones desde código puro y trabajar con ellas, porque, como ya
hemos dicho, Haskell ofrece mejores formas de informar de errores sin recurrir
a partes de la E/S. Incluso aun teniendo que trabajar con acciones de la E/S
que puede fallar, prefiero tener tipos como ``IO (Either a b)``, que indiquen
que son acciones de E/S normales solo que su resultado será del tipo
``Either a b``, así que o bien devolverán ``Left a`` o  ``Right b``.
