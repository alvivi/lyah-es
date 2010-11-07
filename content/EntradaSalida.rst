

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

Excepto para la última linea, cada línea de un bloque ``do`` que no se liga
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
coge un valor y lo pone dentro de una caja. La acción ``IO`` resultante
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

 * :dfn:`putStr` es muy parecido a ``putStrLn`` en el sentido de que toma una
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
   
 * :dfn:`putChar` toma un carácter y devuelve una acción ``IO`` que lo imprimirá
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
   
 * :dfn:`print` toma un valor de cualquier tipo que sea miembro de la clase
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
   
 * :dfn:`getChar` es una acción ``IO`` que lee un carácter por la entrada
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
   
 * La función :dfn:`when` se encuentra en el módulo ``Control.Monad`` (para
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
   
   
 * :dfn:`sequence` toma una lista de acciones ``IO`` y devuevle una acción que
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
   auxiliares :dfn:`mapM` y :dfn:`mapM_`. ``mapM`` toma una función y una lista,
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
        
 * :dfn:`forever` toma una acción ``IO`` y devuelve otra acción ``IO`` que 
   simplemente repetirá la primera acción indefinidamente. Está situada en
   ``Control.Monad``. Este pequeño programa preguntará al usuario por una
   cadena y luego la devolverá en mayúsculas, indefinidamente: ::
   
        import Control.Monad  
        import Data.Char  

        main = forever $ do  
            putStr "Give me some input: "  
            l <- getLine  
            putStrLn $ map toUpper l
            
 * :dfn:`forM` (situado en ``Control.Monad``) es como ``mapM`` solo que tiene
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
o sentencias similares. Pero ahora vamos a ver :dfn:`getContents`.
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
que hace esto más fácil, la función :dfn:`interact`. ``interact`` toma una
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
del bloque ``do`` vemos que hay una función nueva llamada :dfn:`openFile`. Su
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

En la siguiente línea vemos una función llamada :dfn:`hGetContents`. Toma un
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
la salida estándar. Luego ejecutamos :dfn:`hClose`, el cual toma un
manipulador y devuelve una acción de E/S que cierra el fichero ¡Tienes que
cerrar tu mismo cada fichero que abras con ``openFile``! 

Otra forma de hacer lo que mismo que acabamos de hacer es utilizando la
función :dfn:`withFile`, cuya declaración de tipo es ``withFile :: FilePath ->
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
sobre el fichero indicado, existen también :dfn:`hGetLine`, :dfn:`hPutStr`,
:dfn:`hPutStrLn`, :dfn:`hGetChar`, etc. Funcionan exactamente igual que sus
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

 * :dfn:`readFile` tiene la declaración de tipo ``readFile :: FilePath -> IO
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
   
 * :dfn:`writeFile` tiene el tipo ``FilePath -> String -> IO ()``. Toma la
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
        
 * :dfn:`appendFile` tiene el mismo tipo que ``writeFile``, solo que
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

También podemos utilizr :dfn:`hFlush`, que es una función que toma un 
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
de ``System.IO``, :dfn:`openTempFile`. Su nombre es bastante auto descriptivo.
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
eliminamos el original con :dfn:`removeFile`, que, como puedes ver, toma la
ruta de un fichero y lo elimina. Después de eliminar el ``todo.txt`` original,
utilizamos :dfn:`renameFile` para renombrar el fichero temporal a
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





   



