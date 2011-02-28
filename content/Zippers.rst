

Zippers
=======


.. image:: /images/60sdude.png
   :align: right
   :alt: ¡Soy parte e la élite!


Mientras que la pureza de Haskell nos da un montón de beneficios, nos hace
abordar algunos problemas de forma muy diferente a como lo haríamos en
Haskell. Debido a la transparencia referencial de Haskell, un valor es
exactamente igual a otro si ambos representan la misma cosa.

Si tenemos tres árboles llenos de cincos y queremos cambiar uno de ellos a
seis, tenemos que tener algún modo de decir qué cinco en concreto del árbol
queremos modificar. Tenemos que conocer la posición que ocupa en el árbol. En
los lenguajes imperativos podemos ver en que parte de la memoria se encuentra
el cinco que queremos modificar y ya esta. Pero en Haskell, un cinco es
exactamente igual a cualquier otro cinco, así que no podemos elegir uno
basándonos en que posición ocupa en la memoria. Tampoco podemos *cambiar*
nada. Cuando decimos que vamos a modificar un árbol, en realidad significa que
vamos a tomar un árbol y devolver uno nuevo que será similar al original, pero
algo diferente.

Una cosa que podemos hacer es recordar el camino que seguimos para llegar al
elemento que queremos modificar desde la raíz del árbol. Podríamos decir, toma
este árbol, vez a la izquierda, ves a la derecha, vuelve a ir a la izquierda y
modifica el elemento que se encuentre allí. Aunque esto funcionaría, puede ser
ineficiente. Si luego queremos modificar un elemento que se encuentra al lado
del elemento que acabamos de modificar, tenemos que recorrer de nuevo todo el
camino empezando por la raíz. 

En este capítulo veremos como podemos tomar una estructura de datos cualquiera
y centrarnos en la forma en la que modificamos y nos desplazamos por sus
elementos de forma eficiente.


Dando un paseo
--------------


Como aprendimos en clase de ciencias naturales, existen mucho tipos de árboles
diferentes, así que vamos a elegir una semilla y plantar el nuestro. Aquí la
tienes: ::

    data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)  

Así que este árbol es o bien ``Empty`` o bien es un nodo que contiene dos
sub-árboles. Aquí tienes un ejemplo de árbol de este tipo, ¡gratis! ::

    freeTree :: Tree Char  
    freeTree =   
        Node 'P'  
            (Node 'O'  
                (Node 'L'  
                    (Node 'N' Empty Empty)  
                    (Node 'T' Empty Empty)  
                )  
                (Node 'Y'  
                    (Node 'S' Empty Empty)  
                    (Node 'A' Empty Empty)  
                )  
            )  
            (Node 'L'  
                (Node 'W'  
                    (Node 'C' Empty Empty)  
                    (Node 'R' Empty Empty)  
                )  
                (Node 'A'  
                    (Node 'A' Empty Empty)  
                    (Node 'C' Empty Empty)  
                )  
            )

Y así es su representación gráfica/artística:

.. image:: /images/pollywantsa.png
   :align: center
   :alt: Árbol.

¿Ves esa ``W``? Digamos que queremos cambiarla por una ``P`` ¿Cómo lo hacemos?
Bueno, una forma sería utilizando un ajuste de patrones sobre el árbol hasta
que encontremos el elemento que buscamos, es decir, vamos por la derecha,
luego por la izquierda y modificamos el elemento. Así sería: ::

    changeToP :: Tree Char -> Tree Char  
    changeToP (Node x l (Node y (Node _ m n) r)) = Node x l (Node y (Node 'P' m n) r)

¡Aarg! No solo es feo si no también algo confuso ¿Qué hace esto? Bueno,
utilizamos un ajuste de patrones sobre el árbol y llamamos a su elemento raíz
``x`` (que en este caso será ``'P'``) y su sub-árbol izquierdo ``l``. En lugar
de dar un nombre a su sub-árbol derecho, utilizamos otro patrón sobre él. 
Continuamos ese ajuste de patrones hasta que alcanzamos el sub-árbol cuya
raíz es ``'W'``. Una vez hemos llegado, reconstruimos el árbol, solo que en
lugar de que ese sub-árbol contenga una ``'W'`` contendrá una ``'P'``.

¿Existe alguna forma de hacer esto mejor? Podríamos crear una función que tome
un árbol junto a una lista de direcciones. Las direcciones será o bien ``L``
(izquierda) o bien ``R`` (derecha), de forma que cambiamos el elemento una vez
hemos seguido todas las direcciones. ::

    data Direction = L | R deriving (Show)  
    type Directions = [Direction]  

    changeToP :: Directions-> Tree Char -> Tree Char  
    changeToP (L:ds) (Node x l r) = Node x (changeToP ds l) r  
    changeToP (R:ds) (Node x l r) = Node x l (changeToP ds r)  
    changeToP [] (Node _ l r) = Node 'P' l r  
    
Si el primer elemento de la lista de direcciones es ``L``, creamos un árbol
que igual al anterior solo que su sub-árbol izquierdo ahora contendrá el 
elemento modificado a ``P``. Cuando llamamos recursivamente a ``changeToP``,
le pasamos únicamente la cola de la listas de direcciones, porque sino
volvería a tomar la misma dirección. Hacemos lo mismo en caso de ``R``. Si la
lista de direcciones está vacía, significa que hemos alcanzado nuestro
destino, así que devolvemos un árbol idéntico al que hemos recibido, solo que
este nuevo árbol tendrá ``'P'`` como elemento raíz. 

Para evitar tener que mostrar el árbol entero, vamos a crear una función que
tome una lista de direcciones y nos devuelva el elemento que se encuentra en
esa posición. ::

    elemAt :: Directions -> Tree a -> a  
    elemAt (L:ds) (Node _ l _) = elemAt ds l  
    elemAt (R:ds) (Node _ _ r) = elemAt ds r  
    elemAt [] (Node x _ _) = x

Esta función es muy parecida a ``changeToP``, solo que en lugar de reconstruir
el árbol paso a paso, ignora cualquier cosa excepto su destino. Vamos a
cambiar ``'W'`` por ``'P'`` y luego comprobaremos si el árbol se ha modificado
correctamente:

.. code-block:: console

    ghci> let newTree = changeToP [R,L] freeTree  
    ghci> elemAt [R,L] newTree  
    'P'

Genial, parece que funciona. En estas funciones, la lista de direcciones actúa
como especie de objetivo, ya que señala un sub-árbol concreto del árbol
principal. Por ejemplo, una lista de direcciones como ``[R]`` señala el
sub-árbol izquierdo que cuelga de la raíz. Una lista de direcciones vacía
señala el mismo árbol principal.

Aunque estas técnicas parecen correctas, pueden ser más bien ineficientes,
especialmente si queremos modificar elementos de forma repetida. Digamos que
tenemos un árbol inmenso y una larga lista de direcciones que señala un
elemento que se encuentra al final del árbol. Utilizamos esta lista de
direcciones para recorrer el árbol y modificar dicho elemento. Si queremos
modificar un elemento que se encuentra cerca del elemento que acabamos de
modificar, tenemos que empezar otra ves desde la raíz del árbol y volver a
recorrer de nuevo todo el camino.

En la siguiente sección veremos un forma mejor de señalar un sub-árbol, una
que nos permita señalar de forma eficiente a los sub-árbol vecinos.


Un rastro de migas
------------------


.. image:: /images/bread.png
   :align: right
   :alt: fiu fiu fiuuu

Vale, así que para  centrarnos o señalar un solo sub-árbol, buscamos algo
mejor que una simple lista de direcciones que parta siempre desde la raíz
¿Ayudaría si comenzamos desde la raíz y nos movemos a la izquierda o la
derecha y al mismo tiempo dejáramos una especie de rastro? Es decir, si vamos
a la izquierda, recordamos que hemos ido por la izquierda, y si vamos por la
derecha, recordamos que hemos ido por la derecha. Podemos intentarlo.

Para representar este rastro, usaremos también una lista de direcciones (es
decir, o bien ``L`` o bien ``R``), solo que en lugar de llamarlo
``Directions`` (direcciones) lo llamaremos ``Breadcrumbs`` (rastro), ya que
iremos dejando las direcciones que hemos tomado a lo largo del camino. ::

    type Breadcrumbs = [Direction]  

Aquí tienes una función que toma un árbol y un rastro y se desplaza al
sub-árbol izquierdo añadiendo ``L`` a la cabeza de la lista que representa el
rastro: ::

    goLeft :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)  
    goLeft (Node _ l _, bs) = (l, L:bs)

Ignoramos el elemento raíz y el sub-árbol derecho y simplemente devolvemos
el sub-árbol izquierdo junto al rastro anterior añadiéndole ``L``. Aquí tienes
la función que se desplaza a la derecha: ::

    goRight :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)  
    goRight (Node _ _ r, bs) = (r, R:bs)

Funciona del mismo modo. Vamos a utilizar estas funciones para tomen el
árbol ``freeTree`` y se desplacen primero a la derecha y luego a la izquierda.

.. code-block:: console

    ghci> goLeft (goRight (freeTree, []))  
    (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty),[L,R])

.. image:: /images/almostzipper.png
   :align: left
   :alt: ¡Casi, casi! ¡Pero no!
   
Vale, ahora tenemos un árbol que tiene ``'W'`` como elemento raíz, ``'C'``
como sub-árbol izquierdo y ``'R'`` como sub-árbol derecho. El rastro es
``[L,R]`` porque primero fuimos a la derecha y luego a la izquierda.

Para que recorrer el árbol sea más cómodo vamos crear la función ``-:`` que
definiremos así: ::

    x -: f = f x  

La cual nos permite aplicar funciones a valores escribiendo primero el valor,
luego ``-:`` y al final la función. Así que en lugar de hacer
``goRight (freeTree, [])``, podemos escribir ``(freeTree, []) -: goRight``.
Usando esta función podemos reescribir el código anterior para parezca más
que primero vamos a la derecha y luego a la izquierda: ::

    ghci> (freeTree, []) -: goRight -: goLeft  
    (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty),[L,R])


Volviendo atrás
'''''''''''''''

¿Qué pasa si queremos volver por el camino que hemos tomado? Gracias al rastro
sabemos que el árbol actual es el sub-árbol izquierdo del sub-árbol derecho
que colgaba del árbol principal, pero nada más. No nos dice nada acerca del
padre del sub-árbol actual para que podamos volver hacia arriba. Parece que
aparte del las direcciones que hemos tomado, el rastro también debe contener
toda la información que desechamos por el camino. En este caso, el sub-árbol
padre que contiene también el sub-árbol izquierdo que no tomamos.

En general, un solo rastro debe contener toda la información suficiente para
poder reconstruir el nodo padre. De esta forma, tenemos información sobre
todas las posibles rutas que no hemos tomado y también conocemos el camino
que hemos tomado, pero debe contener información acerca del sub-árbol en
el que nos encontramos actualmente, si no, estaríamos duplicando información.

Vamos a modificar el tipo rastro para que también contenga la información 
necesaria para almacenar todos los posibles caminos que vamos ignorando
mientras recorremos el árbol. En lugar de utilizar ``Direction``, creamos un
nuevo tipo de datos: ::

    data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)  

Ahora, en lugar de tener únicamente ``L``, tenemos ``LeftCrumb`` que contiene
también el nodo desde el cual nos hemos desplazado y el sub-árbol derecho que
no hemos visitado. En lugar de ``R``, ahora tenemos ``RightCrumb`` que
contiene el nodo desde el cual nos hemos desplazado y el sub-árbol izquierdo
que hemos ignorado.

Ahora estos rastros contienen toda la información necesaria para reconstruir
el árbol que estamos recorriendo. Así que en lugar de ser un rastro normal,
son como una especie de discos de datos que vamos dejando por el camino, ya
que contienen mucha más información a parte del camino tomado.

Básicamente, ahora cada rastro es como un sub-árbol cojo. Cuando nos
adentramos en un árbol, el rastro almacena toda la información del nodo que
nos alejamos exceptuando el sub-árbol que estamos recorriendo. También tenemos
que fijarnos en la información que vamos ignorando, por ejemplo, en caso
de ``LeftCrumb`` sabemos que nos acabamos de desplazar por el sub-árbol
izquierdo, así que no guardamos ninguna información de este sub-árbol.

Vamos a modificar el sinónimo de tipo ``Breadcrumbs`` para refleje este
cambio: ::

    type Breadcrumbs a = [Crumb a]  

A continuación vamos modificar las funciones ``goLeft`` y ``goRight`` para
que almacenen en el rastro la información de los caminos que no hemos tomado,
en lugar de ignorar esta información como hacíamos antes. Así sería
``goLeft``: ::

    goLeft :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)  
    goLeft (Node x l r, bs) = (l, LeftCrumb x r:bs)

Es muy parecida a la versión anterior de ``goLeft``, solo que en lugar de
añadir ``L`` a la cabeza de la lista de rastros, añadimos un elemento
``LeftCrumb`` para representar que hemos tomado el camino izquierdo y además
indicamos el nodo desde el que nos hemos desplazado (es decir ``x``) y el
camino que no hemos tomado (es decir, el sub-árbol derecho, ``r``).

Fíjate que esta función asume que el árbol en el que nos encontramos no es
``Empty``. Un árbol vacío no tiene ningún sub-árbol, así que si intentamos
movernos por un árbol vacío, obtendremos un error a la hora de ajustar los
patrones.

``goRight`` es parecido: ::

    goRight :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)  
    goRight (Node x l r, bs) = (r, RightCrumb x l:bs)

Ahora somos totalmente capaces de movernos de izquierda a derecha. Lo que aún
no podemos hacer es volver por el camino recorrido utilizando la información
que indica los nodos padres que hemos recorrido. Aquí tienes la función
``goUp``: ::

    goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)  
    goUp (t, LeftCrumb x r:bs) = (Node x t r, bs)  
    goUp (t, RightCrumb x l:bs) = (Node x l t, bs)

.. image:: /images/asstronaut.png
   :align: left
   :alt: Astronauta.
   
No encontramos en el árbol ``t`` y tenemos que comprobar el último ``Crumb``.
Si es un ``LeftCrumb``, entonces reconstruimos un nuevo árbol donde ``t`` es
el sub-árbol izquierdo y utilizamos la información del sub-árbol derecho que
no hemos visitado junto al elemento del nodo padre para reconstruir un nuevo
``Node``. Como hemos utilizado el rastro anterior para recrear el nuevo
nodo, por decirlo de algún modo, la lista de rastros ya no tiene que contener
este último rastro.

Fíjate que esta función genera un error en caso que ya nos encontremos en la
cima del árbol. Luego veremos como utilizar la mónada ``Maybe`` para
representar los posibles fallos de desplazamiento.

Gracias al par formado por ``Tree a`` y ``Breadcrumbs a``, tenemos toda la
información necesaria para reconstruir el árbol entero y también tenemos
señalado un nodo concreto. Este modelo nos permite también movernos fácilmente
hacia arriba, izquierda o derecha. Todo par que contenga una parte
seleccionada de una estructura y todo la parte que rodea a esa parte
seleccionada se llama *zipper*, esto es así porque se parece a la acción de
aplicar ``zip`` sobre listas normales de duplas. Un buen sinónimo de tipo
sería: ::

    type Zipper a = (Tree a, Breadcrumbs a)  

Preferiría llamar al sinónimo de tipos ``Focus`` ya que de esta forma es más
claro que estamos seleccionando una parte de la estructura, pero el termino
*zipper* se utiliza ampliamente, así que nos quedamos con ``Zipper``.


Manipulando árboles seleccionados
'''''''''''''''''''''''''''''''''

Ahora que nos podemos mover de arriba a abajo, vamos a crear una función que
modifique el elemento raíz del sub-árbol que seleccione un *zipper*. ::

    modify :: (a -> a) -> Zipper a -> Zipper a  
    modify f (Node x l r, bs) = (Node (f x) l r, bs)  
    modify f (Empty, bs) = (Empty, bs)

Si estamos seleccionando un nodo, modificamos su elemento raíz con la función
``f``. Si estamos seleccionando un árbol vacío, dejamos éste como estaba.
Ahora podemos empezar con un árbol, movernos a donde queramos y modificar un
elemento, todo esto mientras mantenemos seleccionado un elemento de forma que
nos podemos desplazar fácilmente de arriba a abajo. Un ejemplo: 

.. code-block:: console

    ghci> let newFocus = modify (\_ -> 'P') (goRight (goLeft (freeTree,[]))) 
    
Vamos a la izquierda, luego a la derecha y luego remplazamos el elemento raíz
del sub-árbol en el que nos encontramos por ``'P'``. Se lee mejor si
utilizamos ``-:``:

.. code-block:: console

    ghci> let newFocus = (freeTree,[]) -: goLeft -: goRight -: modify (\_ -> 'P')  

Luego podemos desplazarnos hacía arriba y remplazar el elemento por una
misteriosa ``'X'``:

.. code-block:: console

    ghci> let newFocus2 = modify (\_ -> 'X') (goUp newFocus)  

O con ``-:``:

.. code-block:: console

    ghci> let newFocus2 = newFocus -: goUp -: modify (\_ -> 'X')  

Movernos hacia arriba es fácil gracias a que el rastro que vamos dejando que
contiene los caminos que no hemos tomado, así que, es como deshacer el camino.
Por esta razón, cuando queremos movernos hacia arriba no tenemos que volver
a empezar desde la raíz inicial, simplemente podemos volver por el camino que
hemos tomado.

Cada nodo posee dos sub-árboles, incluso aunque los dos sub-árboles sean
árboles vacíos. Así que si estamos seleccionando un sub-árbol vacío, una cosa
que podemos hacer es remplazar un sub-árbol vació por un árbol que contenga
un nodo. ::

    attach :: Tree a -> Zipper a -> Zipper a  
    attach t (_, bs) = (t, bs)

Tomamos un árbol y un *zipper* y devolvemos un nuevo *zipper* que tendrá
seleccionado el árbol que pasemos como parámetro. Esta función no solo nos
permite añadir nodos a las hojas de un árbol, sino que también podemos
remplazar sub-árboles enteros. Vamos a añadir un árbol a la parte inferior
izquierda de ``freeTree``:

.. code-block:: console

    ghci> let farLeft = (freeTree,[]) -: goLeft -: goLeft -: goLeft -: goLeft  
    ghci> let newFocus = farLeft -: attach (Node 'Z' Empty Empty)

``newFocus`` ahora selecciona un nuevo árbol que ha sido añadido al árbol
original. Si utilizáramos ``goUp`` para subir por el árbol, veríamos que sería
igual que ``freeTree`` pero con un nodo adicional ``'Z'`` en su parte
inferior izquierda.


Me voy a la cima del árbol, donde el aire está limpio y fresco
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Crear una función que seleccione la cima del árbol, independientemente del
nodo seleccionado, es realmente fácil: ::

    topMost :: Zipper a -> Zipper a  
    topMost (t,[]) = (t,[])  
    topMost z = topMost (goUp z)

Si nuestro rastro está vacío, significa que ya estamos en la cima del árbol,
así que solo tenemos que devolver el mismo nodo que está seleccionado. De
otro modo, solo tenemos que seleccionar el nodo padre del actual y volver a
aplicar de forma recursiva ``topMost``. Ahora podemos dar vueltas por un
árbol, ir a la izquierda o a la derecha, aplicar ``modify`` o ``attach`` para
realizar unas cuantas modificaciones, y luego, gracias a ``topMost``, volver
a selecciona la raíz principal del árbol y ver si hemos modificado
correctamente el árbol.



