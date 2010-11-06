

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
donde se encuentra ``helloworld.hs`` y haz lo siguiente: ::

    $ ghc --make helloworld  
    [1 of 1] Compiling Main             ( helloworld.hs, helloworld.o )  
    Linking helloworld ...
    
¡Vale! Con un poco de suerte habrás obtenido algo similar y podrás ejecutar
el programa haciendo ``./helloworld``. ::

    $ ./helloworld  
    hello, world
    
Ahí lo tienes, nuestro primer programa compilado que muestra un mensaje por la
terminal ¡Extraordinariamente aburrido!

Vamos a examinar lo que hemos escrito. Primero, vamos a ver el tipo de la
función ``putStrLn``. ::

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
el tipo de ``getLine``. ::

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
ejecutadas. Cuando escribimos una acción ``IO`` en GHCi y pulsamos intro. ::

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
          
Antes de que veamos como tratar con archivos, vamos a echar un vistazo a
algunas funciones que son útiles a la hora de trabajar con ``IO``.

 * ``putStr`` es muy parecido a ``putStrLn`` en el sentido de que toma una
   cadena y devuelve una acción que imprimirá esa cadena por la terminal, solo
   que ``putStr`` no salta a una nueva línea después de imprimir la cadena tal
   y como ``putStrLn`` hace. ::
   
        main = do   putStr "Hey, "  
                    putStr "I'm "  
                    putStrLn "Andy!"

   ::
   
        $ runhaskell putstr_test.hs  
        Hey, I'm Andy!
    
   Su tipo es ``putStr :: String -> IO ()``, así que el resultado contenido en
   la acción ``IO`` es la unidad. Un valor inútil, por lo que no tiene sentido
   ligarlo a nada.
   
 * ``putChar`` toma un carácter y devuelve una acción ``IO`` que lo imprimirá
   por la terminal. ::
   
       main = do   putChar 't'  
                   putChar 'e'  
                   putChar 'h'
   
   ::
   
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
   
 * ``print`` toma un valor de cualquier tipo que sea miembro de la clase
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
                    
   ::
   
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
   con ese valor para mostrarlo por la terminal. ::
   
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
   
 * ``getChar`` es una acción ``IO`` que lee un carácter por la entrada
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
   hace esto, pero no lo hace de la forma que esperamos. Compruébalo. ::
   
        $ runhaskell getchar_test.hs  
        hello sir  
        hello
        
   La segunda línea es la salida. Hemos escrito ``hello sir`` y luego hemos
   pulsado intro. Debido al *buffering*, la ejecución del programa solo 
   empieza después de ejecutar intro y no después de cada carácter pulsado.
   Una vez pulsamos intro, actúa como si hubiéramos escrito esos caracteres
   desde el principio. Intenta jugar un poco con este programa para entender
   como funciona.
   
 * La función ``when`` se encuentra en el módulo ``Control.Monad`` (para
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
   
   
 * ``sequence`` toma una lista de acciones ``IO`` y devuevle una acción que
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
   secuenciarla. ::
   
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
   auxiliares ``mapM`` y ``mapM_``. ``mapM`` toma una función y una lista,
   mapea la función sobre la lista y luego la secuencia. ``mapM_`` hace lo
   mismo, solo que después se deshace del resultado. Normalmente utilizamos
   ``mapM_`` cuando no nos importa el resultado de las acciones secuenciadas.
   ::
   
        ghci> mapM print [1,2,3]  
        1  
        2  
        3  
        [(),(),()]  
        ghci> mapM_ print [1,2,3]  
        1  
        2  
        3
        
 * ``forever`` toma una acción ``IO`` y devuelve otra acción ``IO`` que 
   simplemente repetirá la primera acción indefinidamente. Está situada en
   ``Control.Monad``. Este pequeño programa preguntará al usuario por una
   cadena y luego la devolverá en mayúsculas, indefinidamente: ::
   
        import Control.Monad  
        import Data.Char  

        main = forever $ do  
            putStr "Give me some input: "  
            l <- getLine  
            putStrLn $ map toUpper l
            
 * ``forM`` (situado en ``Control.Monad``) es como ``mapM`` solo que tiene
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
   ligarlo, podemos simplemente desecharlo. ::
   
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
momento cuando pueden escribir cosas en tu pantalla o reproducir *Yakety Sax*
por tus altavoces. Cada acción ``IO`` también puede contener un resultado que
nos dirá que ha podido obtener del mundo real.

No pienses en la función ``putStrLn`` como una función que toma una cadena y
la imprime por pantalla. Piensa que es una función que toma una cadena y
devuelve una ación ``IO``. Esa acción ``IO``, cuando sea ejecutada, imprimirá
por pantalla dicha cadena.
  
  
 
       