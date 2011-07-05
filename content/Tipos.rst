Tipos y clases de tipos
=======================

Cree en el tipo
---------------


.. image:: /images/cow.png
   :align: left
   :alt: ¡Vaca!

Anteriormente mencionamos que Haskell tiene un sistema de tipos estático. Se
conoce el tipo de cada expresión en tiempo de compilación, lo que produce
código más seguro. Si escribimos un programa que intenta dividir un valor del
tipo booleano por un número, no llegará a compilarse. Esto es bueno ya que es
mejor capturar este tipo de errores en tiempo de compilación en lugar de que
el programa falle. Todo en Haskell tiene un tipo, de forma que el compilador
puede razonar sobre el programa antes de compilarlo.

Al contrario que Java o C, Haskell posee inferencia de tipos. Si escribimos un
número, no tenemos que especificar que eso es un número. Haskell puede
deducirlo él solo, así que no tenemos que escribir explícitamente los tipos de
nuestras funciones o expresiones para conseguir resultados. Ya hemos cubierto
parte de las bases de Haskell con muy poco conocimiento de los tipos. Sin
embargo, entender el sistema de tipos es una parte muy importante para dominar
Haskell.

Un tipo es como una etiqueta que posee toda expresión. Esta etiqueta nos dice
a que categoría de cosas se ajusta la expresión. La expresión ``True`` es un
booleano, ``"Hello"`` es una cadena, etc.

Ahora vamos a usar GHCi para examinar los tipos de algunas expresiones. Lo
haremos gracias al comando ``:t``, el cual, seguido de una expresión válida
nos dice su tipo. Vamos a dar un vistazo:

.. code-block:: console

    ghci> :t 'a'
    'a' :: Char
    ghci> :t True
    True :: Bool
    ghci> :t "HOLA!"
    "HELLO!" :: [Char]
    ghci> :t (True, 'a')
    (True, 'a') :: (Bool, Char)
    ghci> :t 4 == 5
    4 == 5 :: Bool

.. image:: /images/bomb.png
   :align: right
   :alt: Bomba

Podemos ver que ejecutando el comando ``:t`` sobre una expresión se muestra
esa misma expresión seguida de ``::`` y de su tipo. ``::`` se puede leer como
*tiene el tipo*. Los tipos explícitos siempre se escriben con su primera letra
en mayúsculas. ``'a'``, como hemos visto, tiene el tipo ``Char``. El nombre de
este tipo viene de "Character" (carácter en inglés). ``True`` tiene el tipo
``Bool``. Tiene sentido. Pero, ¿qué es esto? Examinando el tipo de
``"HOLA!"`` obtenemos ``[Char]``. Los corchetes definen una lista. Así que
leemos esto como una *lista de caracteres*. Al contrario que las listas, cada
tamaño de tupla tiene su propio tipo. Así que la expresión ``(True, 'a')``
tiene el tipo ``(Bool, Char)``, mientras que la expresión ``('a', 'b', 'c')``
tiene el tipo ``(Char, Char, Char)``. ``4 == 5`` siempre devolverá ``False``
así que esta expresión tiene el tipo ``Bool``.

Las funciones también tiene tipos. Cuando escribimos nuestras propias
funciones podemos darles un tipo explícito en su declaración. Generalmente
está bien considerado escribir los tipos explícitamente en la declaración de
un función, excepto cuando éstas son muy cortas. De aquí en adelante les
daremos tipos explícitos a todas las funciones que creemos. ¿Recuerdas la
lista intensional que filtraba solo las mayúsculas de una cadena? Aquí tienes
como se vería con su declaración de tipo: ::

    removeNonUppercase :: [Char] -> [Char]
    removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

``removeNonUppercase`` tiene el tipo ``[Char] -> [Char]``, que significa que
es una función que toma una cadena y devuelve otra cadena. El tipo ``[Char]``
es sinónimo de ``String`` así que sería más elegante escribir el tipo como
``removeNonUppercase :: String -> String``. Anteriormente no le dimos un tipo
a esta función ya que el compilador puede inferirlo por si solo. Pero, ¿cómo
escribimos el tipo de una función que toma varios parámetros? Aquí tienes una
función que toma tres enteros y los suma: ::

    addThree :: Int -> Int -> Int -> Int
    addThree x y z = x + y + z

Los parámetros están separados por ``->`` y no existe ninguna diferencia
especial entre los parámetros y el tipo que devuelve la función. El tipo que
devuelve la función es el último elemento de la declaración y los parámetros
son los restantes. Más tarde veremos porque simplemente están separados por
``->`` en lugar de tener algún tipo de distinción más explícita entre los
tipos de parámetros y el tipo de retorno, algo como ``Int, Int, Int -> Int``.

Si escribimos una función y no tenemos claro el tipo que debería tener,
siempre podemos escribir la función sin su tipo y ejecutar el comando ``:t``
sobre ella. Las funciones también son expresiones así que no hay ningún
problema en usar ``:t`` con ellas.

Aquí tienes una descripción de los tipos más comunes:

 * :cpp:type:`Int` representa enteros. Se utiliza para representar número
   enteros, por lo que ``7`` puede ser un ``Int`` pero ``7.2`` no puede.
   ``Int`` está acotado, lo que significa que tiene un valor máximo y un valor
   mínimo. Normalmente en máquinas de 32bits el valor máximo de ``Int`` es
   2147483647 y el mínimo -2147483648.

 * :cpp:type:`Integer` representa... esto... enteros también. La diferencia es
   que no están acotados así que pueden representar números muy grandes. Sin
   embargo, ``Int`` es más eficiente. ::

       factorial :: Integer -> Integer
       factorial n = product [1..n]

   .. code-block:: console

       ghci> factorial 50
       30414093201713378043612608166064768844377641568960512000000000000

 * :cpp:type:`Float` es un número real en coma flotante de simple precisión.
   ::

       circumference :: Float -> Float
       circumference r = 2 * pi * r

   .. code-block:: console

       ghci> circumference 4.0
       25.132742

 * :cpp:type:`Double` es un número real en coma flotante de... ¡Doble 
   precisión!. ::

       circumference' :: Double -> Double
       circumference' r = 2 * pi * r

   .. code-block:: console

       ghci> circumference' 4.0
       25.132741228718345

 * :cpp:type:`Bool` es el tipo booleano. Solo puede tener dos valores:
   ``True`` o ``False``.

 * :cpp:type:`Char` representa un carácter. Se define rodeado por comillas
   simples. Una lista de caracteres es una cadena.

Las tuplas también poseen tipos pero dependen de su longitud y del tipo de sus
componentes, así que teóricamente existe una infinidad de tipos de tuplas y
eso son demasiados tipos como para cubrirlos en esta guía. La tupla vacía
es también un tipo :cpp:type:`()` el cual solo puede contener un valor:
``()``.


Variables de tipo
-----------------

¿Cual crees que es el tipo de la función ``head``? Como ``head`` toma una
lista de cualquier tipo y devuelve su primer elemento... ¿Cual podrá ser?
Vamos a verlo:

.. code-block:: console

    ghci> :t head
    head :: [a] -> a

.. image:: /images/box.png
   :align: left
   :alt: Caja

Hmmm... ¿Qué es ``a``? ¿Es un tipo? Si recuerdas antes dijimos que los tipos
deben comenzar con mayúsculas, así que no puede ser exactamente un tipo. Como
no comienza con una mayúscula en realidad es una **variable de tipo**. Esto
significa que ``a`` puede ser cualquier tipo. Es parecido a los tipos
genéricos de otros lenguajes, solo que en Haskell son mucho más potentes ya
que nos permite definir fácilmente funciones muy generales siempre que no
hagamos ningún uso especifico del tipo en cuestión. Las funciones que tienen
variables de tipos son llamadas **funciones polimórficas**. La declaración de
tipo ``head`` representa una función que toma una lista de cualquier tipo y
devuelve un elemento de ese mismo tipo.

Aunque las variables de tipo pueden tener nombres más largos de un solo
carácter, normalmente les damos nombres como a, b, c, d, etc.

¿Recuerdas ``fst``? Devuelve el primer componente de una dupla. Vamos a
examinar su tipo.

.. code-block:: console

    ghci> :t fst
    fst :: (a, b) -> a

Como vemos, ``fst`` toma una dupla que contiene dos tipos y devuelve un
elemento del mismo tipo que el primer componente de la dupla. Ese es el porqué
de que podamos usar ``fst`` con duplas que contengan cualquier combinación de
tipos. Ten en cuenta que solo porque ``a`` y ``b`` son diferentes variables de
tipo no tienen porque ser diferentes tipos. Simplemente representa que el
primer componente y el valor que devuelve la función son del mismo tipo.


.. _clases-de-tipo-1:

Clases de tipos paso a paso (1ª parte)
--------------------------------------


.. image:: /images/classes.png
   :align: right
   :alt: Clases

Las clases de tipos son una especie de interfaz que define algún tipo de
comportamiento. Si un tipo es miembro de una clase de tipos, significa que ese
tipo soporta e implementa el comportamiento que define la clase de tipos. La
gente que viene de lenguajes orientados a objetos es propensa a confundir las
clases de tipos porque piensan que son como las clases en los lenguajes
orientados a objetos. Bien, pues no lo son. Una aproximación más adecuada
sería pensar que son como las interfaces de Java, o los protocolos de
Objective-C, pero mejor.

¿Cuál es la declaración de tipo de la función ``==``?

.. code-block:: console

    ghci> :t (==)
    (==) :: (Eq a) => a -> a -> Bool

.. note::

    El operador de igualdad ``==`` es una función. También lo son ``+``,
    ``-``, ``*``, ``/`` y casi todos los operadores. Si el nombre de una
    función está compuesta solo por caracteres especiales (no alfanuméricos),
    es considerada una función infija por defecto. Si queremos examinar su
    tipo, pasarla a otra función o llamarla en forma prefija debemos rodearla
    con paréntesis. Por ejemplo: ``(+) 1 4`` equivale a ``1 + 4``.

Interesante. Aquí vemos algo nuevo, el símbolo ``=>``. Cualquier cosa antes
del símbolo ``=>`` es una restricción de clase. Podemos leer la declaración de
tipo anterior como: la función de igualdad toma dos parámetros que son del
mismo tipo y devuelve un ``Bool``. El tipo de estos dos parámetros debe ser
miembro de la clase ``Eq`` (esto es la restricción de clase).

La clase de tipos ``Eq`` proporciona una interfaz para las comparaciones de
igualdad. Cualquier tipo que tenga sentido comparar dos valores de ese tipo
por igualdad debe ser miembro de la clase ``Eq``. Todos los tipos estándar de
Haskell excepto el tipo IO (un tipo para manejar la entrada/salida) y las
funciones forman parte de la clase ``Eq``.

La función ``elem`` tiene el tipo ``(Eq a) => a -> [a] -> Bool`` porque usa
``==`` sobre los elementos de la lista para saber si existe el elemento
indicado dentro de la lista.

Algunas clases de tipos básicas son:

 * :cpp:class:`Eq` es utilizada por los tipos que soportan comparaciones por
   igualdad. Los miembros de esta clase implementan las funciones ``==`` o
   ``/=`` en algún lugar de su definición. Todos los tipos que mencionamos
   anteriormente forman parte de la clase ``Eq`` exceptuando las funciones,
   así que podemos realizar comparaciones de igualdad sobre ellos.
   
   .. code-block:: console

       ghci> 5 == 5
       True
       ghci> 5 /= 5
       False
       ghci> 'a' == 'a'
       True
       ghci> "Ho Ho" == "Ho Ho"
       True
       ghci> 3.432 == 3.432
       True

 * :cpp:class:`Ord` es para tipos que poseen algún orden.
 
   .. code-block:: console

       ghci> :t (>)
       (>) :: (Ord a) => a -> a -> Bool

   Todos los tipos que hemos llegado a ver excepto las funciones son parte de
   la clase ``Ord``. ``Ord`` cubre todas las funciones de comparación como
   ``>``, ``<``, ``>=`` y ``<=``. La función :cpp:member:`compare` toma dos
   miembros de la clase ``Ord`` del mismo tipo y devuelve su orden. El orden
   está representado por el tipo :cpp:type:`Ordering` que puede tener tres
   valores distintos: ``GT``, ``EQ`` y ``LT`` los cuales representan
   *mayor que*, *igual que* y *menor que*, respectivamente.

   Para ser miembro de ``Ord``, primero un tipo debe ser socio del prestigioso
   y exclusivo club ``Eq``.
   
   .. code-block:: console

       ghci> "Abrakadabra" < "Zebra"
       True
       ghci> "Abrakadabra" `compare` "Zebra"
       LT
       ghci> 5 >= 2
       True
       ghci> 5 `compare` 3
       GT

 * Los miembros de :cpp:class:`Show` pueden ser representados por cadenas.
   Todos los tipos que hemos visto excepto las funciones forman parte de
   ``Show``. la función más utilizada que trabaja con esta clase de tipos es
   la función :cpp:member:`show`. Toma un valor de un tipo que pertenezca a
   la clase ``Show`` y lo representa como una cadena de texto.
   
   .. code-block:: console

       ghci> show 3
       "3"
       ghci> show 5.334
       "5.334"
       ghci> show True
       "True"

 * :cpp:class:`Read` es como la clase de tipos opuesta a ``Show``. La función
   :cpp:member:`read` toma una cadena y devuelve un valor del tipo que es
   miembro de ``Read``.
   
   .. code-block:: console

       ghci> read "True" || False
       True
       ghci> read "8.2" + 3.8
       12.0
       ghci> read "5" - 2
       3
       ghci> read "[1,2,3,4]" ++ [3]
       [1,2,3,4,3]

   Hasta aquí todo bien. Una vez más, todo los tipos que hemos visto excepto
   las funciones forman parte de esta clase de tipos. Pero, ¿Qué pasa si
   simplemente usamos ``read "4"``? 
   
   .. code-block:: console

       ghci> read "4"
       <interactive>:1:0:
           Ambiguous type variable `a' in the constraint:
             `Read a' arising from a use of `read' at <interactive>:1:0-7
           Probable fix: add a type signature that fixes these type variable(s)

   Lo que GHCi no está intentado decir es que no sabe que queremos que
   devuelva. Ten en cuenta que cuando usamos anteriormente ``read`` lo hicimos
   haciendo algo luego con el resultado. De esta forma, GHCi podía inferir el
   tipo del resultado de la función ``read``. Si usamos el resultado de
   aplicar la función como un booleano, Haskell sabe que tiene que devolver un
   booleano. Pero ahora, lo único que sabe es que queremos un tipo de la clase
   ``Read``, pero no cual. Vamos a echar un vistazo a la declaración de tipo
   de la función ``read``.
   
   .. code-block:: console

       ghci> :t read
       read :: (Read a) => String -> a

   ¿Ves? Devuelve un tipo que es miembro de la clase ``Read``, pero si luego
   no lo usamos en ningún otro lugar, no hay forma de saber que tipo es. Por
   este motivo utilizamos las **anotaciones de tipo** explícitas. Las
   anotación de tipo son una forma de decir explícitamente el tipo que debe
   tener una expresión. Lo hacemos añadiendo ``::`` al final de la expresión y
   luego especificando el tipo. Observa:
   
   .. code-block:: console

       ghci> read "5" :: Int
       5
       ghci> read "5" :: Float
       5.0
       ghci> (read "5" :: Float) * 4
       20.0
       ghci> read "[1,2,3,4]" :: [Int]
       [1,2,3,4]
       ghci> read "(3, 'a')" :: (Int, Char)
       (3, 'a')

   La mayoría de expresiones son del tipo que el compilador puede inferir por
   si solo. Pero a veces, el compilador desconoce el tipo de valor que debe
   devolver una expresión como ``read "5"``, que podría ser ``Int``,
   ``Double``, etc. Para saberlo, Haskell debe en realidad evaluar
   ``read "5"``. Pero como Haskell es un lenguaje con tipos estáticos, debe
   conocer todos los tipos antes de que el código sea compilado (o en GHCi,
   evaluado). Así que con esto le estamos diciendo a Haskell: "Ey, esta
   expresión debe ser de este tipo en caso de que no sepas cual es".

 * Los miembros de la clase :cpp:class:`Enum` son tipos secuencialmente
   ordenados, es decir, pueden ser enumerados. La principal ventaja de la
   clase de tipos ``Enum`` es que podemos usar los miembros en las listas
   aritméticas. También tienen definidos los sucesores y predecesores, por lo
   que podemos usar las funciones ``succ`` y ``pred``. Los tipos de esta clase
   son: ``()``, ``Bool``, ``Char``, ``Ordering``, ``Int``, ``Integer``,
   ``Float`` y ``Double``.
   
   .. code-block:: console

       ghci> ['a'..'e']
       "abcde"
       ghci> [LT .. GT]
       [LT,EQ,GT]
       ghci> [3 .. 5]
       [3,4,5]
       ghci> succ 'B'
       'C'

 * Los miembros de :cpp:class:`Bounded` poseen límites inferiores y
   superiores, es decir están acotados.
 
   .. code-block:: console

       ghci> minBound :: Int
       -2147483648
       ghci> maxBound :: Char
       '\1114111'
       ghci> maxBound :: Bool
       True
       ghci> minBound :: Bool
       False

   ``minBound`` y ``maxBound`` son interesantes ya que tienen el tipo
   ``(Bounded a) => a``. Es decir, son constantes polimórficas.

   Todas las tuplas son también ``Bounded`` si sus componentes los son
   también.
   
   .. code-block:: console

       ghci> maxBound :: (Bool, Int, Char)
       (True,2147483647,'\1114111')

 * :cpp:class:`Num` es la clase de tipos numéricos. Sus miembros tienen la
   propiedad de poder comportarse como números. Vamos a examinar el tipo de
   un número.
   
   .. code-block:: console

       ghci> :t 20
       20 :: (Num t) => t

   Parece que todos los números son también constantes polimórficas. Pueden
   actuar como si fueran cualquier tipo de la clase ``Num``.
   
   .. code-block:: console

       ghci> 20 :: Int
       20
       ghci> 20 :: Integer
       20
       ghci> 20 :: Float
       20.0
       ghci> 20 :: Double
       20.0

   Estos son los tipo estándar de la clase ``Num``. Si examinamos el tipo de
   ``*`` veremos que puede aceptar cualquier tipo de número.
   
   .. code-block:: console

       ghci> :t (*)
       (*) :: (Num a) => a -> a -> a

   Toma dos números del mismo tipo y devuelve un número del mismo tipo. Esa es
   la razón por la que ``(5 :: Int) * (6 :: Integer)`` lanzará un error
   mientras que ``5 * (6 :: Integer)`` funcionará correctamente y producirá un
   ``Interger``, ya que ``5`` puede actuar como un ``Integer`` o un ``Int``.

   Para unirse a ``Num``, un tipo debe ser amigo de ``Show`` y ``Eq``.

 * :cpp:class:`Integral` es también un clase de tipos numérica. ``Num``
   incluye todos los números, incluyendo números reales y enteros.
   ``Integral`` únicamente incluye números enteros. ``Int`` e ``Integer`` son
   miembros de esta clase.

 * :cpp:class:`Floating` incluye únicamente números en coma flotante, es decir
   ``Float`` y ``Double``.

Una función muy útil para trabajar con números es :cpp:member:`fromIntegral`.
Tiene el tipo ``fromIntegral :: (Num b, Integral a) => a -> b``. A partir de
esta declaración podemos decir que toma un número entero y lo convierte en un
número más general. Esto es útil cuando estas trabajando con números reales y
enteros al mismo tiempo. Por ejemplo, la función ``length`` tiene el tipo
``length :: [a] -> Int`` en vez de tener un tipo más general como
``(Num b) => length :: [a] -> b``. Creo que es por razones históricas o algo
parecido, en mi opinión, es absurdo. De cualquier modo, si queremos obtener el
tamaño de una lista y sumarle ``3.2``, obtendremos un error al intentar sumar
un entero con uno en coma flotante. Para solucionar esto, hacemos
``fromIntegral (length [1,2,3,4]) + 3.2``.

Fíjate que en la declaración de tipo de ``fromIntegral`` hay varias
restricciones de clase. Es completamente válido como puedes ver, las
restricciones de clase deben ir separadas por comas y entre paréntesis.
