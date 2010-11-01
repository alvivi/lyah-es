

La sintaxis de las funciones
============================

Ajuste de patrones 
------------------

.. image:: /images/pattern.png
   :align: right
   :alt: Patrones

En este capítulo cubriremos algunas de las construcciones sintácticas de Haskell
más interesantes, empezando con el ajuste de patrones. Un ajuste de patrones
consiste en una especificación de pautas que deben ser seguidas por los datos,
los cuales pueden ser deconstruidos permitiéndonos acceder a sus componentes.

.. note::
    A esta construcción se conoce más comúnmente con el término de
    *pattern* *matching*. 

Podemos separar los cuerpos que definen una función a la hora de declarar una
función como tal. Esto suele llevarnos a un código mucho más elegante, limpio y
fácil de leer. Podemos usar el ajuste de patrones con cualquier tipo de dato:
números, caracteres, listas, tuplas, etc. Vamos a crear una función muy trivial
que compruebe si el número que le damos es un siete o no. ::

    lucky :: (Integral a) => a -> String  
    lucky 7 = "LUCKY NUMBER SEVEN!"  
    lucky x = "Sorry, you're out of luck, pal!"   

Cuando llamamos a ``lucky``, los patrones son verificados de arriba a abajo y
cuando un patrón concuerda con el valor asociado, se utiliza el cuerpo de la
función asociado. En este caso, la única forma de que un número concuerde con el
primer patrón es que dicho número sea 7. Si no lo es, se evaluara el siguiente
patrón, el cual coincide con cualquier valor y lo liga a ``x``. Esta función
podría estar implementada usando una sentencia ``if``. Pero, ¿Qué pasaría si
quisiéramos una función que nombrara los número del 1 al 5, o
``"Not between 1 and 5"`` para cualquier otro número? Si no tuviéramos el
ajuste de patrones deberíamos crear un enrevesado árbol ``if then else``.
Sin embargo con él: ::
       
    sayMe :: (Integral a) => a -> String  
    sayMe 1 = "One!"  
    sayMe 2 = "Two!"  
    sayMe 3 = "Three!"  
    sayMe 4 = "Four!"  
    sayMe 5 = "Five!"  
    sayMe x = "Not between 1 and 5"

Ten en cuenta que si movemos el último patrón (el más general) al inicio,
siempre obtendríamos ``"Not between 1 and 5"`` como respuesta, ya que el primer
patrón encajaría con cualquier número y no habría ninguna posibilidad de que
se comprobaran los demás patrones.

¿Recuerdas la función factorial que implementamos anteriormente? Definimos el
factorial de un número ``n`` como ``product [1..n]``. También podemos
implementar una función factorial recursiva, de forma parecida a como lo
haríamos en matemáticas. Empezamos diciendo que el factorial de 0 es 1. Luego 
afirmamos que el factorial de cualquier otro número entero positivo es ese
entero multiplicado por el factorial de su predecesor. ::

    factorial :: (Integral a) => a -> a  
    factorial 0 = 1  
    factorial n = n * factorial (n - 1)  

Esta es la primera vez que definimos una función recursiva. La recursión es
importante en Haskell, hablaremos de ello más adelante. Pero en pocas palabras
esto es lo que pasa cuando intentamos obtener el factorial de, digamos 3.
Primero intenta calcular ``3 * factorial 2``. El factorial de 2 es
``2 * factorial 1``, así que ahora tenemos ``3 * (2 * factorial 1)``. 
``factorial 1`` es ``1 * factorial 0``, lo que nos lleva a
``3 * (2 * (1 * factorial 0))``. Ahora viene el truco, hemos definido el
factorial de 0 para que sea simplemente 1, y como se encuentra con ese patrón
antes de otro más general, obtenemos 1. Así que el resultado equivale a 
``3 * (2 * (1 * 1))``. Si hubiésemos escrito el segundo patrón al inicio,
hubiese aceptado todos los números, incluyendo el 0 y nuestro cálculo nunca
terminaría. Este es el motivo por el que el orden es importante a la hora de
definir los patrones y es siempre mejor definir los patrones más específicos los
primeros dejando los más generales al final. 

Los patrones también pueden fallar. Si definimos una función como esta: ::

    charName :: Char -> String  
    charName 'a' = "Albert"  
    charName 'b' = "Broseph"  
    charName 'c' = "Cecil"  

E intentamos ejecutarla con un valor no esperado, esto es lo que pasa: ::

    ghci> charName 'a'  
    "Albert"  
    ghci> charName 'b'  
    "Broseph"  
    ghci> charName 'h'  
    "*** Exception: tut.hs:(53,0)-(55,21): Non-exhaustive patterns in function charName

Se queja de que tenemos un ajuste de patrones no exhaustivo y ciertamente así
es. Cuando utilizamos patrones siempre tenemos que incluir uno general para
asegurarnos que nuestro programa no fallará.

El ajuste de patrones también pueden ser usado con las tuplas. ¿Cómo crearíamos
una función que tomara dos vectores 2D (representados con duplas) y devolviera la
suma de ambos? Para sumar dos vectores sumamos primero sus componentes ``x`` y
sus componentes ``y`` de forma separada. Así es como lo haríamos si no 
existiese el ajuste de patrones: ::

    addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
    addVectors a b = (fst a + fst b, snd a + snd b)  

Bien, funciona, pero hay mejores formas de hacerlo. Vamos a modificar la
función para que utilice un ajuste de patrones. ::

    addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
    addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)  

¡Ahí lo tienes! Mucho mejor. Ten en cuenta que es un patrón general, es decir,
se verificará para cualquier par. El tipo de ``addVectors`` es en ambos casos
el mismo ``addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)``, por lo que
está garantizado que tendremos dos duplas como parámetros.

``fst`` y ``snd`` extraen componentes de los pares. Pero, ¿Qué pasa con las
triplas? Bien, como no tenemos funciones que hagan esto, vamos a crearlas 
nosotros. ::

    first :: (a, b, c) -> a  
    first (x, _, _) = x  
  
    second :: (a, b, c) -> b  
    second (_, y, _) = y  
  
    third :: (a, b, c) -> c  
    third (_, _, z) = z  

El ``_`` significa lo mismo que en listas por comprensión. Significa que
realmente no nos importa lo que es esa componente, así que escribimos solo
``_``.

Esto me recuerda que también puedes usar el ajuste de patrones en las listas por
comprensión. Fíjate: :: 

    ghci> let xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]  
    ghci> [a+b | (a,b) <- xs]  
    [4,7,6,8,11,4] 

En caso de que se produzca un fallo en el patrón, simplemente pasará al
siguiente elemento.

Las listas también pueden ser usadas en un ajuste de patrones. Puedes comparar
contra la lista vacía ``[]`` o cualquier patrón que involucre a ``:`` y la lista
vacía. Como ``[1,2,3]`` es solo otra forma de expresar ``1:2:3:[]``, también
puedes usar el patrón anterior. Un patrón como ``x:xs`` ligará la cabeza de la
lista con ``x`` y el resto con ``xs``, incluso cuando la lista tenga solo un
elemento, en cuyo caso ``xs`` acabará siendo la lista vacía.

.. note::
    El patrón ``x:xs`` es muy utilizado, especialmente con las funciones
    recursivas. Los patrones que contengan un ``:`` solo aceptarán listas con
    más de un elemento.

Si quieres ligar, digamos, los tres primeros elementos de una lista a variables
y el resto a otra variable puedes usar algo como ``x:y:z:zs``. Sin embargo esto 
solo aceptará listas que tengan al menos 3 elementos. 

Ahora que ya sabemos usar patrones con las listas vamos a implementar nuestra
propia función ``head``. ::

    head' :: [a] -> a  
    head' [] = error "Can't call head on an empty list, dummy!"  
    head' (x:_) = x  

Comprobamos que funciona: ::

    ghci> head' [4,5,6]  
    4  
    ghci> head' "Hello"  
    'H'

¡Bien! Fíjate que si queremos ligar varias variables (incluso aunque alguna de
ellas sea ``_`` y realmente no la queremos ligar) debemos rodearlas con
paréntesis. También fíjate con la función ``error`` que utilizamos. Ésta toma
una cadena y genera un error en tiempo de ejecución, usado esa cadena como
información acerca del tipo de error que ocurrió. Provoca que el programa
termine, lo cual no es bueno usar a menudo. De todas formas, llamar a ``head``
con una lista vacía no tiene sentido.

Vamos a crear una función que nos diga algunos de los primeros elementos que
contiene una lista. ::

    tell :: (Show a) => [a] -> String  
    tell [] = "The list is empty"  
    tell (x:[]) = "The list has one element: " ++ show x  
    tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
    tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y  

Esta función es segura ya que tiene en cuenta la posibilidad de una lista vacía,
una lista con un elemento, una lista con dos elementos y una lista con más de 
dos elementos. Date cuenta que podríamos escribir ``(x:[])`` y ``(x:y:[])`` como
``[x]`` y ``[x,y]`` sin usar paréntesis. Pero no podemos escribir ``(x:y:_)``
usando corchetes ya que acepta listas con más de dos elementos.

Ya implementamos la función ``length`` usando listas de comprensión. Ahora vamos
a implementarla con un poco de recursión. ::

    length' :: (Num b) => [a] -> b  
    length' [] = 0  
    length' (_:xs) = 1 + length' xs  

Esto es similar a la función factorial que escribimos antes. Primero definimos
el resultado de una entrada conocida, la lista vacía. Esto también es conocido
como el caso base. Luego en el segundo patrón dividimos la lista, su cabeza y
el resto. Decimos que la longitud es 1 más el tamaño del resto de la lista. 
Usamos ``_`` para la cabeza de la lista ya que realmente no nos interesa su
contenido. Fíjate que también hemos tenido en cuenta todos los posibles casos
de listas. El primer patrón acepta la lista vacía, y el segundo todas las demás.

Vamos a ver que pasa si llamamos a ``length'`` con ``"ham"``. Primero se
comprobaría si es una lista vacía, como no lo es continuaríamos al siguiente
patrón. Éste es aceptado y nos dice que la longitud es ``1 + length' "am"``, ya
que hemos divido la cadena en cabeza y cola, y nos hemos desecho de la cabeza. 
Vale. La ``length'`` de ``"am"`` es, de forma similar, ``1 + length' "m"``. Así
que ahora mismo tenemos ``1 + (1 + length' "m")``. ``length' "m"`` es
``1 + length' ""`` (También lo podríamos escribir como ``1 + length' []``). Y
como tenemos definido ``length' []`` a 0, al final tenemos
``1 + (1 + (1 + 0))``.

Ahora implementemos ``sum``. Sabemos que la suma de una lista vacía es 0, lo 
cual escribimos como un patrón. También sabemos que la suma de una lista es la
cabeza más la suma del resto de la cola, y si lo escribimos obtenemos: ::

    sum' :: (Num a) => [a] -> a  
    sum' [] = 0  
    sum' (x:xs) = x + sum' xs  

También existe una cosa llamada *patrones* *como* (o patrones as, en inglés).
Son útiles para descomponer algo usando un patrón, de forma que se ligue con los
nombres que queramos, y además mantenemos una referencia a ese algo como un
todo. Realizamos esto poniendo un nombre y un ``@`` delante del patrón. Por
ejemplo, el patrón ``xs@(x:y:ys)``. Este patrón se ajustará exactamente a lo
mismo que lo haría ``x:y:ys`` pero podríamos acceder fácilmente a la lista
completa usando ``xs`` en lugar de tener que repetirnos escribiendo ``x:y:ys``
en el cuerpo de la función. Un ejemplo rápido: ::

    capital :: String -> String  
    capital "" = "Empty string, whoops!"  
    capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

::

    ghci> capital "Dracula"  
    "The first letter of Dracula is D"

Normalmente usamos los *patrones* *como* para evitar repetirnos cuando estamos
ajustando un patrón más grande y tenemos que usarlo entero otra vez en algún
lugar del cuerpo de la función.

Una cosa más, no puedes usar ``++`` en los ajustes de patrones. Si has intentado
usar un patrón ``(xs ++ ys)`` ¿Qué habría en la primera y en la segunda lista?
No tiene mucho sentido. Tendría más sentido ajustar patrones como
``(xs ++ [x,y,z])`` o simplemente ``(xs ++ [x])`` pero dada la naturaleza de las
listas no podemos hacer esto.


¡Guardas, Guardas!
------------------

.. image:: /images/guards.png
    :align: left
    :alt: Guardas

Mientras que los patrones son una forma de asegurarnos que un valor tiene una
determinada forma y deconstruirlo, las guardas son una forma de comprobar si
alguna propiedad de una valor (o varios de ellos) es cierta o falsa. Eso suena
mucho como una sentencia ``if`` y de hecho es muy similar. La cuestión es que las
guardas son mucho más legibles cuando tienes varias condiciones y encajan muy 
bien con los patrones.

En lugar de explicar su sintaxis, simplemente empecemos y creamos una función
que utilice guardas. Vamos a crear una función simple que te regañará de forma
diferente en función de tu 
`IMC <http://es.wikipedia.org/wiki/%C3%8Dndice_de_masa_corporal>`_
(índice de masa corporal). Tu IMC es igual a tu altura dividida por tu peso al
cuadrado. Si tu IMC es menor que 18,5 tienes infrapeso. Si estas en algún entre
18,5 y 25 eres del montón. Si tienes entre 25 y 30 tienes sobrepeso y si tienes
más de 30 eres obeso. Así que aquí tienes la función (No estamos calculando 
nada ahora, simplemente obtiene un IMC y te lo muestra) ::

    bmiTell :: (RealFloat a) => a -> String  
    bmiTell bmi  
        | bmi <= 18.5 = "Tienes infrapeso ¡Eres un emo!"  
        | bmi <= 25.0 = "Supuestamente eres normal. Ptsss, espero que seas feo."  
        | bmi <= 30.0 = "¡Estás gordo! Pierde algo de peso gordito."  
        | otherwise   = "¡Enhorabuena, eres una ballena!" 
    
Las guardas se indican con barras que siguen al nombre de la función y sus
parámetros. Normalmente tienen una sangría están alineadas. Una
guarda es básicamente una expresión booleana. Si se evalúa a ``True``, entonces
el cuerpo de la función correspondiente es utilizado. Si se evalúa a ``False``,
se comprueba la siguiente guarda y así sucesivamente. Si llamamos a esta
función con ``24.3``, primero comprobará si es menor o igual que ``18.5``. Como
no lo es, seguirá a la siguiente guarda. Se comprueba la segunda guarda y como
24,3 es menor que 25, la segunda cadena es devuelta. 

Esto recuerda mucho a un gran árbol ``if then else`` de los lenguajes
imperativos, solo que mucho mejor y más legible. Generalmente los arboles ``if
else`` muy grandes están mal vistos, pero hay ocasiones en que un problema se
define de forma discreta y no hay forma de solucionarlo. Las guardas son una
buena alternativa para esto.

Muchas veces la última guarda es ``otherwise``. ``otherwise`` está definido
simplemente como ``otherwise = True`` y acepta todo. Esto muy similar al ajuste
de patrones, solo se aceptan si la entrada satisface un patrón pero las guardas
comprueban condiciones booleanas. Si todas las guardas de una función se evalúan
a ``False`` (y no hemos dado otra guarda ``otherwise``), la evaluación falla y
continuará hacia el siguiente **patrón**. Ese es la razón por la que los
patrones y las guardas encajen tan bien juntas. Si existe ningún patrón ni 
ninguna guarda aceptable se lanzará un error. 

Por supuesto podemos usar guardas con con funciones que tomen tantos parámetros
como quieran. En lugar de dejar que el usuario tenga que calcular su propio IMC
por su cuenta antes de llamar a la función, vamos a modificar la función para
que tome la altura y el peso y lo calcule por nosotros. ::

    bmiTell :: (RealFloat a) => a -> a -> String  
    bmiTell weight height  
        | weight / height ^ 2 <= 18.5 = "Tienes infrapeso ¡Eres un emo!"  
        | weight / height ^ 2 <= 25.0 = "Supuestamente eres normal. Ptsss, espero que seas feo."  
        | weight / height ^ 2 <= 30.0 = "¡Estás gordo! Pierde algo de peso gordito."  
        | otherwise                   = "¡Enhorabuena, eres una ballena!" 
    
Vamos a ver si estoy gordo... ::

    ghci> bmiTell 85 1.90  
    "Supuestamente eres normal. Ptsss, espero que seas feo."  

¡Yeah! No estoy gordo, pero Haskell me acaba de llamar feo...

Fíjate que no hay un ``=`` después del nombre de la función y sus parámetros,
antes de la primera guarda. Muchos novatos obtienen error sintácticos por
ponerlos ahí.

Otro ejemplo muy simple: vamos a implementar nuestra función ``max``. Si
recuerdas, puede tomar dos cosas que puedan ser comparadas y devuelve la mayor. 
::

    max' :: (Ord a) => a -> a -> a  
    max' a b   
        | a > b     = a  
        | otherwise = b
    
Las guardas también pueden ser escritas en una sola línea, aunque advierto que
es mejor no hacerlo ya que son mucho menos legibles, incluso con funciones
cortas. Pero para demostrarlo podemos definir ``max'`` como: ::

    max' :: (Ord a) => a -> a -> a  
    max' a b | a > b = a | otherwise = b

¡Arg! No se lee fácilmente. Sigamos adelante, vamos a implementar nuestro
propio ``compare`` usando guardas. ::

    myCompare :: (Ord a) => a -> a -> Ordering  
    a `myCompare` b  
        | a > b     = GT  
        | a == b    = EQ  
        | otherwise = LT
        
::

    ghci> 3 `myCompare` 2  
    GT    

.. note::
    No solo podemos llamar a funciones de forma infija usando las comillas, sino
    que también podemos definirlas de esta forma. A veces es más fácil leerlo
    de esta forma.

    
¿Dónde?
-------

En la sección anterior definimos la función que calculaba el IMC así: ::

    bmiTell :: (RealFloat a) => a -> a -> String  
    bmiTell weight height  
        | weight / height ^ 2 <= 18.5 = "Tienes infrapeso ¡Eres un emo!"  
        | weight / height ^ 2 <= 25.0 = "Supuestamente eres normal. Ptsss, espero que seas feo."  
        | weight / height ^ 2 <= 30.0 = "¡Estás gordo! Pierde algo de peso gordito."  
        | otherwise                   = "¡Enhorabuena, eres una ballena!" 
    
Si te fijas notarás que nos repetimos tres veces. Nos repetimos tres veces. 
Repetirse (tres veces) mientras estas programando es tan deseable como que te
den una patada donde más te duela. Ya que estamos repitiendo la misma expresión
tres veces sería ideal si pudiésemos calcularla una sola vez, ligarla a un
nombre y utilizarlo en lugar de la expresión. Bien, podemos modificar nuestra
función de esta forma: ::

    bmiTell :: (RealFloat a) => a -> a -> String  
    bmiTell weight height  
        | bmi <= 18.5 = "Tienes infrapeso ¡Eres un emo!"  
        | bmi <= 25.0 = "Supuestamente eres normal. Ptsss, espero que seas feo."  
        | bmi <= 30.0 = "¡Estás gordo! Pierde algo de peso gordito."  
        | otherwise   = "¡Enhorabuena, eres una ballena!" 
        where bmi = weight / height ^ 2
        
Hemos puesto la palabra reservada ``where`` después de las guardas
(Normalmente es mejor alinearla con el resto de las barras) y luego definimos
varios nombres o funciones. Estos nombres son visibles en las guardas y nos
dan la ventaja de no tener que repetirnos. Si decidimos que tenemos que
calcular el IMC de otra forma solo tenemos que modificarlo en un lugar.
También mejora la legibilidad ya que da nombre a las cosas y hace que nuestros
programas sean más rápidos ya que cosas como nuestro ``bmi`` solo debe
calcularse una vez. Podríamos pasarnos un poco y presentar una función como
esta: ::

    bmiTell :: (RealFloat a) => a -> a -> String  
    bmiTell weight height  
        | bmi <= skinny = "Tienes infrapeso ¡Eres un emo!"  
        | bmi <= normal = "Supuestamente eres normal. Ptsss, espero que seas feo."  
        | bmi <= fat    = "¡Estás gordo! Pierde algo de peso gordito."  
        | otherwise     = "¡Enhorabuena, eres una ballena!" 
        where bmi = weight / height ^ 2  
              skinny = 18.5  
              normal = 25.0  
              fat = 30.0 
    
Los nombres que definamos en la sección ``where`` de una función son solo
visibles desde esa función, así que no nos tenemos que preocupar de ellos a la
hora de crear más nombres en otras funciones. Si no alineamos la sección
``where`` bien y de forma correcta, Haskell se confundirá porque no sabrá a que
grupo pertenece.

Los nombres definidos con ``where`` no se comparten entre los cuerpos de
diferentes patrones de una función. Si quieres que varios patrones accedan al
mismo nombre compartido debes definirlo de forma global. 

También puedes usar el ajuste de patrones con las secciones ``where``. Podríamos
reescribir la sección ``where`` de nuestra función anterior como: ::

    ...  
    where bmi = weight / height ^ 2  
          (skinny, normal, fat) = (18.5, 25.0, 30.0)
          
Vamos a crear otra función trivial en el que dado un nombre y su apellido
devuelva sus iniciales. ::

    initials :: String -> String -> String  
    initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
        where (f:_) = firstname  
              (l:_) = lastname   
    
Podríamos haber realizado el ajuste de patrones directamente en los parámetros
de la función (En realidad hubiese sido más corto y elegante) pero así puedes
ver lo que es posible hacer con las secciones ``where``. 

De la misma forma que hemos definido constantes en los bloques ``where`` también
puedes definir funciones. Manteniendonos fieles a nuestro programa de salud
vamos a hacer una función que tome una lista de duplas de pesos y estaturas 
y devuelva una lista de IMCs. ::

    calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
    calcBmis xs = [bmi w h | (w, h) <- xs]  
        where bmi weight height = weight / height ^ 2 
    
¡Ahí lo tienes! La razón por la que hemos creado la función ``bmi`` en este
ejemplo es que no podemos calcular simplemente un IMC desde los parámetros de
nuestra función. Tenemos que examinar todos los elementos de la lista y calcular
su IMC para cada dupla. 

Las secciones ``where`` también pueden estar anidadas. Es algo común crear una
función y definir algunas funciones auxiliares en la sección ``where`` y luego
definir otras funciones auxiliares dentro de cada uno de ellas.


Let it be
---------

Muy similar a las secciones ``where`` son las secciones ``let``. Las secciones
``where`` son una construcción sintáctica que te dejan ligar variables al final
de una función de forma que toda la función pueda acceder a ella, incluyendo
las guardas.  Las secciones ``let`` te dejan ligar variables en cualquier lugar
y son expresiones ellas mismas, pero son muy locales, así que no pueden 
extenderse entre las guardas. Tal y como todas las construcciones de Haskell
que te permiten ligar valores a nombres, las secciones ``let`` permiten usar
ajuste de patrones ¡Vamos a verlo en acción! Así es como podríamos definir una
función que nos diera el área de un cilindro basado en su altura y su radio. ::

    cylinder :: (RealFloat a) => a -> a -> a  
    cylinder r h = 
        let sideArea = 2 * pi * r * h  
            topArea = pi * r ^2  
        in  sideArea + 2 * topArea

.. image:: /images/letitbe.png
    :align: right
    :alt: Let it be
    
Su forma es ``let <`definiciones`> in <expresión>``. Los nombres que definas es
la sección ``let`` son accesibles en la parte ``in``. Como puedes ver, también
podríamos haber definido esto con una sección ``where``. Fíjate también que los
nombres están alineados en la misma columna. Así que ¿Cuál es la diferencia 
entre ellos? Por ahora parece que ``let`` pone las definiciones primero y luego
la expresión que las utiliza mientras que ``where`` lo hace en el orden inverso.

La diferencia es que las secciones ``let`` son expresiones por si mismas. Las
secciones ``where`` son simplemente construcciones sintácticas. ¿Recuerdas
cuando explicamos las sentencias ``if`` y se explicó que como son una expresión
pueden ser usadas en casi cualquier lugar? ::

    ghci> [if 5 > 3 then "Woo" else "Boo", if 'a' > 'b' then "Foo" else "Bar"]  
    ["Woo", "Bar"]  
    ghci> 4 * (if 10 > 5 then 10 else 0) + 2  
    42
    
También puedes hacer lo mismo con las secciones ``let``. ::

    ghci> 4 * (let a = 9 in a + 1) + 2  
    42
    
También pueden ser utilizadas definir funciones en un ámbito local: ::

    ghci> [let square x = x * x in (square 5, square 3, square 2)]  
    [(25,9,4)]

Si queremos ligar varias variables en una solo línea, obviamente no podemos
alinear las definiciones en la misma columna. Por este motivo podemos separarlas
con puntos y comas. ::

    ghci> (let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar)  
    (6000000,"Hey there!")
    
No tienes que porque poner el último punto y coma pero puedes hacerlo si
quieres. Como ya hemos dicho, puedes utilizar ajustes de patrones con las
secciones ``let``. Son muy útiles para desmantelar tuplas en sus componentes y
ligarlos a nombres y demás. ::

    ghci> (let (a,b,c) = (1,2,3) in a+b+c) * 100  
    600
    
También puedes usar las secciones ``let`` dentro de las listas por comprensión.
Vamos a reescribir nuestro anterior ejemplo que calculaba una lista de duplas de
de alturas y pesos para que use un ``let`` dentro de una lista por comprensión
en lugar de definir una función auxiliar con un ``where``. ::

    calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
    calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2] 
    
Incluimos un ``let`` dentro de una lista por comprensión como si fuera un
predicado, solo que no filtra elementos, únicamente liga nombres. Los nombres
definidos en la sección ``let`` de dentro de la lista son visibles desde la
función de salida (la parte anterior a ``|``) y todos los predicados y secciones
que vienen después de la ligadura. Podríamos hacer que nuestra función
devolviera el IMC solo para la gente obesa: ::

    calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
    calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]
    
No podemos usar el nombre ``bmi`` dentro de la parte ``(w, h) <- xs`` ya que
está definida antes de la sección ``let``.

Omitimos la parte ``in`` de las secciones ``let`` dentro de las lista por
comprensión porque la visibilidad de los nombres está predefinida en estos
casos. Sin embargo, podemos usar una sección ``let in`` en un predicado y los
nombres definidos solo serán visibles en este predicado. La parte ``in`` también
puede ser omitida cuando definimos funciones y constantes dentro del intérprete
``GHCi``. Si lo hacemos, los nombres serán visibles durante toda la sesión
interactiva. ::

    ghci> let zoot x y z = x * y + z  
    ghci> zoot 3 9 2  
    29  
    ghci> let boot x y z = x * y + z in boot 3 4 2  
    14  
    ghci> boot  
    <interactive>:1:0: Not in scope: `boot'
    
Si las secciones ``let`` son tan interesantes ¿Por qué no usarlas siempre en
lugar de las secciones ``where``? Bueno, como las secciones ``let`` son
expresiones y son bastante locales en su ámbito, no pueden ser usadas entre
guardas. Hay gente que prefiere las secciones ``where`` porque los nombres
vienen después de la función que los utiliza. De esta forma, el cuerpo de la
función esta más cerca de su nombre y declaración de tipo y algunos piensan
que es más legible.


Expresiones case
----------------

.. image:: /images/case.png
   :align: right
   :alt: Expresiones case

Muchos lenguajes imperativos (Como C, C++, Java, etc.) tienen sintaxis ``case``
y si alguna vez has programado en ellos, probablemente sepas acerca de que va
esto. Se trata de tomar una variable y luego ejecutar bloques de código para
ciertos valores específicos de esa variable y luego incluir quizá algún bloque
siempre se ejecute en caso de que la variable tenga algún valor que no se ajuste
con ninguno de los anteriores. 

Haskell toma este concepto y lo lleva un paso más allá. Como su nombre indica
las expresiones ``case`` son, bueno, expresiones, como las expresiones
``if else`` o las secciones ``let``. No solo podemos evaluar expresiones
basandonos en los posibles valores de un variable sino que podemos realizar
ajuste de patrones. Mmmm... tomar un valor, realizar un ajuste de patrones sobre
él, evaluar trozos de código basados en su valor ¿Dónde hemos oído esto antes?
Oh sí, en los ajuste de patrones de los parámetros de una función. Bueno, en
realidad es simplemente azúcar sintáctico para las expresiones ``case``. Estos
dos trozos de código hacen lo mismo y son intercambiables: ::

    head' :: [a] -> a  
    head' [] = error "No head for empty lists!"  
    head' (x:_) = x
    
::

    head' :: [a] -> a  
    head' xs = case xs of [] -> error "No head for empty lists!"  
                          (x:_) -> x

Como puedes ver la sintaxis para las expresiones ``case`` es muy simple. ::

    case expresion of patron -> resultado  
                      patron -> resultado  
                      patron -> resultado  
                      ...

La expresión es ajustada contra los patrones. La acción de ajuste de patrones
se comporta como se espera: el primer patrón que se ajuste es el que se utiliza.
Si no se puede ajustar a ningún patrón de la expresión ``case`` se lanzará un
error de ejecución. 

Mientras que el ajuste de patrones de los parámetros de una función puede ser
realizado únicamente al definir una función, las expresiones ``case`` pueden
ser utilizadas casi en cualquier lugar. Por ejemplo: ::

    describeList :: [a] -> String  
    describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                                   [x] -> "a singleton list."   
                                                   xs -> "a longer list."

Son útiles para realizar un ajuste de patrones en medio de una expresión. Como
el ajuste de patrones es azúcar sintáctico para las expresiones ``case``,
podríamos tener definido algo como esto: ::

    describeList :: [a] -> String  
    describeList xs = "The list is " ++ what xs  
        where what [] = "empty."  
              what [x] = "a singleton list."  
              what xs = "a longer list."
                                                                        