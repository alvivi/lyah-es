

Creando nuestros propios tipos y clases de tipo
===============================================


En capítulos anteriores vimos algunos tipos y clases de tipo de Haskell. ¡En
este capítulo vamos a ver como crearlos nosotros mismos! ¿A qué no te lo
esperabas?


Introducción a los tipos de datos algebraicos
---------------------------------------------


Hasta ahora hemos jugado con muchos tipos: ``Bool``, ``Int``, ``Char``,
``Maybe``, etc. Pero ¿Cómo los creamos? Bueno, una forma es usar la palabra
clave ``data`` para definir un tipo. Vamos a ver como está definido el tipo
``Bool`` en la librería estándar: ::

    data Bool = False | True  

``data`` significa que vamos a definir un nuevo tipo de dato. La parte a la
izquierda del ``=`` denota el tipo, que es ``Bool``. La parte a la derecha
son los **constructores de datos**. Estos especifican los diferentes valores
que puede tener un tipo. El ``|`` se puede leer como una *o*. Así que lo
podemos leer como: El tipo ``Bool`` puede tener un valor ``True`` o ``False``.
Tanto el nombre del tipo como el de los constructores de datos deben tener
la primera letra en mayúsculas. 

De la misma forma podemos pensar que el tipo ``Int`` está definido como: ::

    data Int = -2147483648 | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647 

.. image:: /images/caveman.png
   :align: left
   :alt: Hombre de las cavernas

El primer y el último constructor de datos son el mínimo y el máximo valor
posible del tipo ``Int``. En realidad no está definido así, los tres puntos
están ahí porque hemos omitido una buena cantidad de números, así que esto
es solo para motivos ilustrativos.

Ahora vamos a pensar en como definiríamos una figura en Haskell. Una forma
sería usar tuplas. Un círculo podría ser ``(43.1, 55.0, 10.4)`` donde el
primer y el segundo campo son las coordenadas del centro del círculo mientras
que el tercer campo sería el radio. Suena bien, pero esto nos permitiría
también definir un vector 3D o cualquier otra cosa. Una solución mejor sería
crear nuestro propio tipo que represente una figura. Digamos que una figura
solo puede ser un círculo o un rectángulo: ::

    data Shape = Circle Float Float Float | Rectangle Float Float Float Float 

¿Qué es esto? Piensa un poco a que se parece. El constructor de datos`
``Circle`` tiene tres campos que toman valores en coma flotante. Cuando
creamos un constructor de datos, opcionalmente podemos añadir tipos después
de él de forma que estos serán los valores que contenga. Aquí, los primeros
dos componentes son las coordenadas del centro, mientras que el tercero es
el radio. El constructor de datos ``Rectangle`` tiene cuatro campos que
aceptan valores en coma flotante. Los dos primeros representan las coordenadas
de la esquina superior izquierda y los otros dos las coordenadas de la
inferior derecha.

Ahora, cuando hablamos de campos, en realidad estamos hablando de parámetros.
Los constructores de datos son en realidad funciones que devuelven un valor
del tipo para el que fueron definidos. Vamos a ver la declaración de tipo de
estos dos constructores de datos. ::

    ghci> :t Circle  
    Circle :: Float -> Float -> Float -> Shape  
    ghci> :t Rectangle  
    Rectangle :: Float -> Float -> Float -> Float -> Shape

Bien, los constructores de datos son funciones como todo lo demás ¿Quíen lo
hubiera pensado? Vamos a hacer una función que tome una figura y devuleva su
superficie o área: ::

    surface :: Shape -> Float  
    surface (Circle _ _ r) = pi * r ^ 2  
    surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

La primera cosa destacable aquí es la declaración de tipo. Dice que toma una
figura y devuelve un valor en coma flotante. No podemos escribir una
declaración de tipo como ``Circle -> Float`` ya que ``Circle`` no es un tipo,
``Shape`` si lo es. Del mismo modo no podemos declarar una función cuya
declaración de tipo sea ``True -> Int``. La siguiente cosa que podemos
destacar es que podemos usar el ajuste de patrones con los constructores. Ya
hemos utilizado el ajuste de patrones con constructores anteriormente (en
realidad todo el tiempo) cuando ajustamos valores como ``[]``, ``False``,
``5``, solo que esos valores no tienen campos. Simplemente escribimos el 
constructor y luego ligamos sus campos a nombres. Como estamos interesados en
el radio, realmente no nos importan los dos primeros valores que nos dicen
donde esta el círculo. ::

    ghci> surface $ Circle 10 20 10  
    314.15927  
    ghci> surface $ Rectangle 0 0 100 100  
    10000.0

Bien ¡Funciona! Pero si intentamos mostrar por pantalla ``Circle 10 20 5``
en una sesión de GHCi obtendremos un error. Esto sucede porque Haskell aún no
sabe como representar nuestro tipo con una cadena. Recuerda que cuando
intentamos mostrar un valor por pantalla, primero Haskell ejecuta la función
``show`` para obtener la representación en texto de un dato y luego lo muestra
en la terminal. Para hacer que nuestro tipo ``Shape`` forme parte de la clase
de tipo ``Show`` hacemos esto: ::

    data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show) 

No vamos a preocuparnos ahora mismo acerca de derivar. Simplemente diremos que
si añadimos ``deriving (Show)`` al final de una declaración de tipo,
automáticamente Haskell hace que ese tipo forme parte de la clase de tipo
``Show``. Así que ahora ya podemos hacer esto: ::

    ghci> Circle 10 20 5  
    Circle 10.0 20.0 5.0  
    ghci> Rectangle 50 230 60 90  
    Rectangle 50.0 230.0 60.0 90.0
 
Los constructores de datos son funciones, así que podemos mapearlos,
aplicarlos parcialmente o cualquier otra cosa. Si queremos una lista de
círculos concéntricos con diferente radio podemos escribir esto: ::

    ghci> map (Circle 10 20) [4,5,6,6]  
    [Circle 10.0 20.0 4.0,Circle 10.0 20.0 5.0,Circle 10.0 20.0 6.0,Circle 10.0 20.0 6.0]

Nuestro tipo de dato es bueno, pero podría se mejor. Vamos a crear un tipo de
dato intermedio que defina un punto en espacio bidimensional. Luego lo
usaremos para hacer nuestro tipo más evidente. ::

    data Point = Point Float Float deriving (Show)  
    data Shape = Circle Point Float | Rectangle Point Point deriving (Show)  

Te habrás dado cuenta de que hemos usado el mismo nombre para el tipo que para
el constructor de datos. No tiene nada de especial, es algo común usar el
mismo nombre que el del tipo si solo hay un constructor de datos. Así que
ahora ``Circle`` tiene dos campos, uno es el del tipo ``Point`` y el otro del
tipo ``Float``. De esta forma es más fácil entender que es cada cosa. Lo mismo
sucede para el rectángulo. Tenemos que modificar nuestra función ``surface``
para que refleje estos cambios.

    surface :: Shape -> Float  
    surface (Circle _ r) = pi * r ^ 2  
    surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)  

Lo único que hemos cambiado han sido los patrones. Hemos descartado
completamente el punto en el patrón del círculo. Por otra parte, en  el patrón
del rectángulo, simplemente hemos usado un ajuste de patrones anidado para
obtener las coordenadas de los puntos. Si hubiésemos querido hacer una
referencia directamente a los puntos por cualquier motivo podríamos haber
utilizado un patrón *como*. ::

    ghci> surface (Rectangle (Point 0 0) (Point 100 100))  
    10000.0  
    ghci> surface (Circle (Point 0 0) 24)  
    1809.5574

¿Cómo sería una función que desplaza una figura? Tomaría una figura, la
cantidad que se debe desplazar en el eje *x*, la cantidad que se debe desplazar
en el eje *y* y devolvería una nueva figura con las mismas dimensiones pero
desplazada. ::

    nudge :: Shape -> Float -> Float -> Shape  
    nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r  
    nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))  

Bastante sencillo. Añadimos las cantidades a desplazar a los puntos que
representan la posición de las figuras. ::

    ghci> nudge (Circle (Point 34 34) 10) 5 10  
    Circle (Point 39.0 44.0) 10.0 

Si no queremos trabajar directamente con puntos, podemos crear funciones
auxiliares que creen figuras de algún tamaño en el centro del eje de
coordenadas de modo que luego las podamos desplazar. ::

    baseCircle :: Float -> Shape  
    baseCircle r = Circle (Point 0 0) r  
  
    baseRect :: Float -> Float -> Shape  
    baseRect width height = Rectangle (Point 0 0) (Point width height)

::

    ghci> nudge (baseRect 40 100) 60 23  
    Rectangle (Point 60.0 23.0) (Point 100.0 123.0)

Como es lógico, podemos exportar nuestros datos en los módulos. Para hacerlo,
solo tenemos que escribir el nombre del tipo juntos a las funciones
exportadas, y luego añadirles unos paréntesis que contengan los constructores
de datos que queramos que se exporten, separados por comas. Si queremos que se
exporten todos los constructores de datos para un cierto tipo podemos usar
``..``.

Si quisiéramos exportar las funciones y tipos que acabamos de crear en un
módulo, podríamos empezar con esto: ::

    module Shapes   
    ( Point(..)  
    , Shape(..)  
    , surface  
    , nudge  
    , baseCircle  
    , baseRect  
    ) where

Haciendo ``Shape (..)`` estamos exportando todos los constructores de datos 
de ``Shape``, lo que significa que cualquiera que importe nuestro módulo puede
crear figuras usando los constructores ``Circle`` y ``Rectangle``. Sería lo
mismo que escribir ``Shape (Rectangle, Circle)``. 

También podríamos optar por no exportar ningún constructor de datos para
``Shape`` simplemente escribiendo ``Shape`` en dicha sentencia. De esta forma,
aquel que importe nuestro módulo solo podrá crear figuras utilizando las
funciones auxiliares ``baseCircle`` y ``baseRect``. ``Data.Map`` utiliza este
método. No puedes crear un diccionario utilizando ``Map.Map [(1,2),(3,4)]`` ya
que no se exporta el constructor de datos. Sin embargo, podemos crear un
diccionario utilizando funciones auxiliares como ``Map.fromList``. Recuerda,
los constructores de datos son simples funciones que toman los campos del tipo
como parámetros y devuelven un valor de un cierto tipo (como ``Shape``) como
resultado. Así que cuando elegimos no exportarlos, estamos previniendo que la
gente que importa nuestro módulo pueda utilizar esas funciones, pero si alguna
otra función devuelve devuelve el tipo que estamos exportando, las podemos
utilizar para crear nuestros propios valores de ese tipo.

No exportar los constructores de datos de un tipo de dato lo hace más
abstracto en el sentido de que oculta su implementación. Sin embargo, los
usuarios del módulo no podrán usar el ajuste de patrones sobre ese tipo. 


Sintaxis de registro
--------------------


.. image:: /images/record.png
   :align: right 
   :alt: Tocadiscos

Bien, se nos ha dado la tarea de crear un tipo que describa a una persona.
La información que queremos almacenar de cada persona es: nombre, apellidos,
edad, altura, número de teléfono y el sabor de su helado favorito. No se nada
acerca de ti, pero para mi es todo lo que necesito saber de una persona.
¡Vamos allá! ::

    data Person = Person String String Int Float String String deriving (Show)

Vale. El primer campo es el nombre, el segundo el apellido, el tercero su
edad y seguimos contando. Vamos a crear una persona. ::

    ghci> let guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"  
    ghci> guy  
    Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"

Parece interesante, pero desde luego no muy legible ¿Y si queremos crear una
función que obtenga información por separado de una persona? Una función que
obtenga el nombre de una persona, otra función que obtenga el apellido, etc.
Bueno, las tendríamos que definir así: ::

    firstName :: Person -> String  
    firstName (Person firstname _ _ _ _ _) = firstname  
  
    lastName :: Person -> String  
    lastName (Person _ lastname _ _ _ _) = lastname  
  
    age :: Person -> Int  
    age (Person _ _ age _ _ _) = age  
  
    height :: Person -> Float  
    height (Person _ _ _ height _ _) = height  
  
    phoneNumber :: Person -> String  
    phoneNumber (Person _ _ _ _ number _) = number  
  
    flavor :: Person -> String  
    flavor (Person _ _ _ _ _ flavor) = flavor

¡Fiuuu! La verdad es que no me divertido escribiendo esto. A parte de que este
método sea un lío y un poco ABURRIDO de escribir, funciona. ::

    ghci> let guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"  
    ghci> firstName guy  
    "Buddy"  
    ghci> height guy  
    184.2  
    ghci> flavor guy  
    "Chocolate"

Ahora es cuando piensas: debe de haber un método mejor. Pues no, lo siento 
mucho.

Estaba de broma :P Si que lo hay. Los creadores de Haskell fueron muy
inteligentes y anticiparon este escenario. Incluyeron un método alternativo 
de definir tipos de dato. Así es como podríamos conseguir la misma
funcionalidad con la sintaxis de registro. ::

    data Person = Person { firstName :: String  
                         , lastName :: String  
                         , age :: Int  
                         , height :: Float  
                         , phoneNumber :: String  
                         , flavor :: String  
                         } deriving (Show)
                     
En lugar de nombrar los campos uno tras otro separados por espacios,
utilizamos un par de llaves. Dentro, primero escribimos el nombre de un campo,
por ejemplo ``firstName`` y luego escribimos unos dobles puntos ``::``
(también conocido como *Paamayim Nekudotayim* xD) y luego especificamos el
tipo. El tipo de dato resultante es exactamente el mismo. La principal
diferencia es que de esta forma se crean funciones que obtienen esos campos
del tipo de dato. Al usar la sintaxis de registro con este tipo de dato,
Haskell automáticamente crea estas funciones: ``firstName``, ``lastName``,
``age``, ``height``, ``phoneNumber`` y ``flavor``. ::

    ghci> :t flavor  
    flavor :: Person -> String  
    ghci> :t firstName  
    firstName :: Person -> String

Hay otro beneficio cuando utilizamos la sintaxis de registro. Cuando derivamos
``Show`` para un tipo, mostrará los datos de forma diferente si utilizamos la
sintaxis de registro para definir e instanciar el tipo. Supongamos que tenemos
un tipo que representa un coche. Queremos mantener un registro de la compañía
que lo hizo, el nombre del modelo y su años de producción. Mira. ::

    data Car = Car String String Int deriving (Show)  

::

    ghci> Car "Ford" "Mustang" 1967  
    Car "Ford" "Mustang" 1967  

Si lo definimos usando la sintaxis de registro, podemos crear un coche nuevo
de esta forma: ::

    data Car = Car {company :: String, model :: String, year :: Int} deriving (Show) 

::

    ghci> Car {company="Ford", model="Mustang", year=1967}  
    Car {company = "Ford", model = "Mustang", year = 1967}

Cuando creamos un coche nuevo, no hace falta poner los campos en el orden
adecuado mientras que los pongamos todos. Pero si no usamos la sintaxis de
registro debemos especificarlos en su orden correcto.

Utiliza la sintaxis de registro cuando un constructor tenga varios campos y no
sea obvio que campo es cada uno. Si definimos el tipo de un vector 3D como 
``data Vector = Vector Int Int Int``, es bastante obvio que esos campos son
las componentes del vector. Sin embargo, en nuestros tipo ``Person`` y
``Car``, no es tan obvio y nos beneficia mucho el uso de esta sintaxis.


Parámetros de tipo
------------------


Un constructor de datos puede tomar algunos valores como parámetros y producir
un nuevo valor. Por ejemplo, el constructor ``Car`` toma tres valores y
produce un valor del tipo coche. De forma similar, un **constructor de tipos**
puede tomar tipos como parámetros y producir nuevos tipos. Esto puede parecer
un poco recursivo al principio, pero no es nada complicado. Si has utilizado
las plantillas de *C++* te será familiar. Para obtener una imagen clara de
como los parámetros de tipo funcionan en realidad, vamos a ver un ejemplo de
como un tipo que ya conocemos es implementado. ::

    data Maybe a = Nothing | Just a  

.. image:: /images/yeti.png
   :align: left
   :alt: Yeti
    
La ``a`` es un parámetro de tipo. Debido a que hay un parámetro de tipo
involucrado en esta definición, llamamos a ``Maybe`` un constructor de tipos.
Dependiendo de lo que queramos que este tipo contenga cuando un valor no es
``Nothing``, este tipo puede acabar produciendo tipos como ``Maybe Int``,
``Maybe Car``, ``Maybe String``, etc. Ningún valor puede tener un tipo que sea
simplemente ``Maybe``, ya que eso no es un tipo por si mismo, es un
constructor de tipos. Para que sea un tipo real que algún valor pueda tener,
tiene que tener todos los parámetros de tipo definidos.

Si pasamos ``Char`` como parámetro de tipo a ``Maybe``, obtendremos el tipo
``Maybe Char``. Por ejemplo, el valor ``Just 'a'`` tiene el tipo ``Maybe
Char``.

Puede que no lo sepas, pero utilizamos un tipo que tenía un parámetro de tipo
antes de que empezáramos a utilizar el tipo ``Maybe``. Ese tipo es el tipo
lista. Aunque hay un poco decoración sintáctica, el tipo lista toma un
parámetro para producir un tipo concreto. Los valores pueden tener un tipo
``[Int]``, un tipo ``[Char]``, ``[[String]]``, etc. pero no puede haber un
valor cuyo tipo sea simplemente ``[]``.

Vamos a jugar un poco con el tipo ``Maybe``. ::

    ghci> Just "Haha"  
    Just "Haha"  
    ghci> Just 84  
    Just 84  
    ghci> :t Just "Haha"  
    Just "Haha" :: Maybe [Char]  
    ghci> :t Just 84  
    Just 84 :: (Num t) => Maybe t  
    ghci> :t Nothing  
    Nothing :: Maybe a  
    ghci> Just 10 :: Maybe Double  
    Just 10.0

Los parámetros de tipo son útiles ya que nos permiten crear diferentes tipos
dependiendo del tipo que queramos almacenar en nuestros tipos de datos (valga
la redundancia). Cuando hacemos ``:t Just "Haha"`` el motor de inferencia de
tipos deduce que el tipo debe ser ``Maybe [Char]``, ya que la ``a`` en ``Just
a`` es una cadena, luego el ``a`` en ``Maybe a`` debe ser también una cadena.

Como habrás visto el tipo de ``Nothing`` es ``Maybe a``. Su tipo es
polimórfico. Si una función requiere un ``Maybe Int`` como parámetro le
podemos pasar  un ``Nothing`` ya que no contiene ningún valor. El tipo ``Maybe
a`` puede comportarse como un ``Maybe Int``, de la misma forma que ``5`` puede
comportarse como un ``Int`` o como un ``Double``. De forma similar el tipo de
las listas vacías es ``[a]``. Una lista vacía puede comportarse como cualquier
otra lista. Por eso podemos hacer cosas como ``[1,2,3] ++ []`` y
``["ha","ha","ha"] ++ []``.

El uso de parámetros de tipo nos puede beneficiar, pero solo en los casos que
tenga sentido. Normalmente los utilizamos cuando nuestro tipo de dato
funcionará igual sin importar el tipo de dato que contenga, justo como nuestro
``Maybe a``. Si nuestro tipo es como una especie de caja, es un buen lugar
para usar los parámetros de tipo. Podríamos cambiar nuestro tipo ``Car`` de:
::

    data Car = Car { company :: String  
                   , model :: String  
                   , year :: Int  
                   } deriving (Show)

A: ::

    data Car a b c = Car { company :: a  
                         , model :: b  
                         , year :: c   
                         } deriving (Show)

Pero ¿Tiene algún beneficio? La respuesta es: probablemente no, ya que al
final acabaremos escribiendo funciones que solo funcionen con el tipo ``Car
String String Int``. Por ejemplo, dada la primera definición de ``Car``,
podríamos crear una función que mostrara las propiedades de un coche con un
pequeño texto: ::

    tellCar :: Car -> String  
    tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

::

    ghci> let stang = Car {company="Ford", model="Mustang", year=1967}  
    ghci> tellCar stang  
    "This Ford Mustang was made in 1967"  

¡Una función muy bonita! La declaración de tipo es simple y funciona
perfectamente. Ahora ¿Cómo sería si ``Car`` fuera en realidad ``Car a b c``?
::

    tellCar :: (Show a) => Car String String a -> String  
    tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

Tenemos que forzar a que la función tome un ``Car`` del tipo ``(Show a) => Car
String String a``. Podemos ver como la definición de tipo es mucho más
complicada y el único beneficio que hemos obtenido es que podamos usar
cualquier tipo que sea una instancia de la clase de tipo ``Show`` como
parámetro ``c``. ::

    ghci> tellCar (Car "Ford" "Mustang" 1967)  
    "This Ford Mustang was made in 1967"  
    ghci> tellCar (Car "Ford" "Mustang" "nineteen sixty seven")  
    "This Ford Mustang was made in \"nineteen sixty seven\""  
    ghci> :t Car "Ford" "Mustang" 1967  
    Car "Ford" "Mustang" 1967 :: (Num t) => Car [Char] [Char] t  
    ghci> :t Car "Ford" "Mustang" "nineteen sixty seven"  
    Car "Ford" "Mustang" "nineteen sixty seven" :: Car [Char] [Char] [Char]

.. image:: /images/meekrat.png
   :align: right
   :alt: Suricato

A la hora de la verdad, acabaríamos utilizando ``Car String String Int`` la
mayor parte del tiempo y nos daríamos cuenta de que parametrizar el tipo
``Car`` realmente no importa. Normalmente utilizamos los parámetros de tipo
cuando el tipo que está contenido dentro del tipo de dato no es realmente
importante a la hora de trabajar con éste. Una lista de cosas es una lista
de cosas y no importa que sean esas cosas, funcionará igual. Si queremos sumar
una lista de números, mas tarde podemos especificar en la propia función de
suma de que queremos específicamente una lista de números. Lo mismo pasa con
``Maybe``. ``Maybe`` representa la opción de tener o no tener un valor.
Realmente no importa de que tipo sea ese valor.

Otro ejemplo de un tipo parametrizado que ya conocemos es el tipo ``Map k v``
de ``Data.Map``. ``k`` es el tipo para las claves del diccionario mientras que
``v`` es el tipo de los valores. Este es un buen ejemplo en donde los
parámetros de tipo son útiles. Al tener los diccionarios parametrizados nos
permiten asociar cualquier tipo con cualquier otro tipo, siempre que la clave
del tipo sea de la clase de tipo ``Ord``. Si estuviéramos definiendo el tipo
diccionario podríamos añadir una restricción de clase en la definición: ::

    data (Ord k) => Map k v = ...  

Sin embargo, existe un consenso en el mundo Haskell de que **nunca debemos
añadir restricciones de clase a las definiciones de tipo**. ¿Por qué? Bueno,
porque no nos beneficia mucho, pero al final acabamos escribiendo más
restricciones de clase, incluso aunque no las necesitemos. Si ponemos o no
podemos la restricción de clase ``Ord k`` en la definición de tipo de ``Map k
v``, tendremos que poner de todas formas la restricción de clase en las
funciones que asuman que las claves son ordenables. Pero si no ponemos la
restricción en la definición de tipo, no tenemos que poner ``(Ord k) =>`` en
la declaración de tipo de las funciones que no les importe si la clave puede
es ordenable o no. Un ejemplo de esto sería la función ``toList`` que
simplemente convierte un diccionario en una lista de asociación. Su
declaración de tipo es ``toList :: Map k a -> [(k, a)]``. Si ``Map k v``
tuviera una restricción en su declaración, el tipo de ``toList`` debería haber
sido ``toList :: (Ord k) => Map k a -> [(k, a)]`` aunque la función no 
necesite comparar ninguna clave.

Así que no pongas restricciones de clase en las declaraciones de tipos aunque
tenga sentido, ya que al final las vas a tener que poner de todas formas en
las declaraciones de tipo de las funciones.

Vamos a implementar un tipo para vectores 3D y crear algunas operaciones con
ellos. Vamos a usar un tipo parametrizado ya que, aunque normalmente contendrá
números, queremos que soporte varios tipos de ellos. ::

    data Vector a = Vector a a a deriving (Show)  
  
    vplus :: (Num t) => Vector t -> Vector t -> Vector t  
    (Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)  
  
    vectMult :: (Num t) => Vector t -> t -> Vector t  
    (Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)  
  
    scalarMult :: (Num t) => Vector t -> Vector t -> t  
    (Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n

``vplus`` sirve para sumar dos vectores. Los vectores son sumados simplemente
sumando sus correspondientes componentes. ``scalarMult`` calcula el producto
escalar de dos vectores y ``vectMult`` calcula el producto de un vector y un
escalar. Estas funciones pueden operar con tipos como ``Vector Int``,
``Vector Integer``, ``Vector Float`` o cualquier otra cosa mientras ``a`` de
``Vector a`` sea miembro de clase de tipo ``Num``. También, si miras la
declaración de tipo de estas funciones, veras que solo pueden operar con
vectores del mismo tipo y los números involucrados (como en ``vectMult``) 
también deben ser del mismo tipo que el que contengan los vectores. Fíjate en
que no hemos puesto una restricción de clase ``Num`` en la declaración del
tipo ``Vector``, ya que deberíamos haberlo repetido también en las
declaraciones de las funciones.

Una vez más, es muy importante distinguir entre constructores de datos y
constructores de tipo. Cuando declaramos un tipo de dato, la parte anterior al
``=`` es el constructor de tipo, mientras que la parte que va después
(posiblemente separado por ``|``) son los constructores de datos. Dar a una
función el tipo ``Vector t t t -> Vector t t t -> t`` sería incorrecto ya que
hemos usado tipos en la declaración y el constructor de tipo vector toma un
solo parámetro, mientras que el constructor de datos toma tres. Vamos a jugar
un poco con los vectores: ::

    ghci> Vector 3 5 8 `vplus` Vector 9 2 8  
    Vector 12 7 16  
    ghci> Vector 3 5 8 `vplus` Vector 9 2 8 `vplus` Vector 0 2 3  
    Vector 12 9 19  
    ghci> Vector 3 9 7 `vectMult` 10  
    Vector 30 90 70  
    ghci> Vector 4 9 5 `scalarMult` Vector 9.0 2.0 4.0  
    74.0  
    ghci> Vector 2 9 3 `vectMult` (Vector 4 9 5 `scalarMult` Vector 9 2 4)  
    Vector 148 666 222


Instancias derivadas
--------------------


.. image:: /images/gob.png
   :align: right
   :alt: Gob

En la sección :ref:`clases-de-tipo-1`, explicamos las bases de las clases de
tipo. Dijimos que una clase de tipo es una especie de interfaz que define un
comportamiento. Un tipo puede ser una **instancia** de esa clase si soporta
ese comportamiento. Ejemplo: El tipo ``Int`` es una instancia de la clase
``Eq``, ya que la clase de tipo ``Eq`` define el comportamiento de cosas que
se pueden equiparar. Y como los enteros se pueden equiparar, ``Int`` es parte
de la clase ``Eq``. La utilidad real está en las funciones que actúan como
interfaz de ``Eq``, que son ``==`` y ``/=``. Si un tipo forma parte de la
clase ``Eq``, podemos usar las funciones como ``==`` con valores de ese tipo.
Por este motivo, expresiones como ``4 == 4`` y ``"foo" /= "bar"`` son
correctas.

Mencionamos también que las clases de tipo suelen ser confundidas con las
clases de lenguajes como Java, Python, C++ y demás, cosa que más tarde
desconcierta a la gente. En estos lenguajes, las clases son como un modelo del
cual podemos crear objetos que contienen un estado y pueden hacer realizar
algunas acciones. Las clases de tipo son más bien como las interfaces. No
creamos instancias a partir de las interfaces. En su lugar, primero creamos
nuestro tipo de dato y luego pensamos como qué puede comportarse. Si puede
comportarse como algo que puede ser equiparado, hacemos que sea miembro de la
clase ``Eq``. Si puede ser puesto en algún orden, hacemos que sea miembro de
la clase ``Ord``.

Más adelante veremos como podemos hacer manualmente que nuestros
tipos sean una instancia de una clase de tipo implementando las funciones
que esta define. Pero ahora, vamos a ver como Haskell puede automáticamente
hacer que nuestros tipos pertenezcan a una de las siguientes clases: ``Eq``,
``Ord``, ``Enum``, ``Bounded``, ``Show`` y ``Read``. Haskell puede derivar
el comportamiento de nuestros tipos en estos contextos si usamos la palabra
clave ``deriving`` cuando los definimos.

Considera el siguiente tipo de dato: ::

    data Person = Person { firstName :: String  
                         , lastName :: String  
                         , age :: Int  
                         }
                     
Describe a una persona. Vamos a asumir que ninguna persona tiene la misma
combinación de nombre, apellido y edad. Ahora, si tenemos registradas a dos
personas ¿Tiene sentido saber si estos dos registros pertenecen a la misma
persona? Parece que sí. Podemos compararlos por igualdad y ver si son iguales
o no. Por esta razón tiene sentido que este tipo se miembro de la clase de
tipo ``Eq``. Derivamos la instancia: ::

    data Person = Person { firstName :: String  
                         , lastName :: String  
                         , age :: Int  
                         } deriving (Eq)

Cuando derivamos una instancia de ``Eq`` para un tipo y luego intentamos
comparar dos valores de ese tipo usando ``==`` o ``/=``, Haskell comprobará
si los constructores de tipo coinciden (aunque aquí solo hay un constructor
de tipo) y luego comprobará si todos los campos de ese constructor coinciden
utilizando el operador ``=`` para cada par de campos. Solo tenemos que tener
en cuenta una cosa, todos los campos del tipo deben ser también miembros de la
clase de tipo ``Eq``. Como ``String`` y ``Int`` ya son miembros, no hay ningún
problema. Vamos a comprobar nuestra instancia ``Eq``. ::

    ghci> let mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}  
    ghci> let adRock = Person {firstName = "Adam", lastName = "Horovitz", age = 41}  
    ghci> let mca = Person {firstName = "Adam", lastName = "Yauch", age = 44}  
    ghci> mca == adRock  
    False  
    ghci> mikeD == adRock  
    False  
    ghci> mikeD == mikeD  
    True  
    ghci> mikeD == Person {firstName = "Michael", lastName = "Diamond", age = 43}  
    True

Como ahora ``Person`` forma parte de la clase ``Eq``, podemos utilizarlo como
``a`` en las funciones que tengan una restricción de clase del tipo ``Eq a``
en su declaración, como ``elem``. ::

    ghci> let beastieBoys = [mca, adRock, mikeD]  
    ghci> mikeD `elem` beastieBoys  
    True

Las clases de tipo ``Show`` y ``Read`` son para cosas que pueden ser
convertidas a o desde cadenas, respectivamente. Como pasaba con ``Eq``, si un
constructor de tipo tiene campos, su tipo debe ser miembro de la clase`
``Show`` o ``Read`` si queremos que también forme parte de estas clases. 

Vamos a hacer que nuestro tipo de dato ``Person`` forme parte también de las
clases ``Show`` y ``Read``. ::

    data Person = Person { firstName :: String  
                         , lastName :: String  
                         , age :: Int  
                         } deriving (Eq, Show, Read)
                     
Ahora podemos mostrar una persona por la terminal. ::

    ghci> let mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}  
    ghci> mikeD  
    Person {firstName = "Michael", lastName = "Diamond", age = 43}  
    ghci> "mikeD is: " ++ show mikeD  
    "mikeD is: Person {firstName = \"Michael\", lastName = \"Diamond\", age = 43}"  

Si hubiésemos intentado mostrar en la terminal una persona antes de hacer que
el tipo ``Person`` formara parte de la clase ``Show``, Haskell se hubiera
quejado, diciéndonos que no sabe como representar una persona con una cadena.
Pero ahora que hemos derivado la clase ``Show`` ya sabe como hacerlo.

``Read`` es prácticamente la clase inversa de ``Show``. ``Show`` sirve para
convertir nuestro tipo a una cadena, ``Read`` sirve para convertir una cadena
a nuestro tipo. Aunque recuerda que cuando uses la función ``read`` hay que
utilizar una anotación de tipo explícita para decirle a Haskell que tipo
queremos como resultado. Si no ponemos el tipo que queremos como resultado
explícitamente, Haskell no sabrá que tipo queremos. ::

    ghci> read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" :: Person  
    Person {firstName = "Michael", lastName = "Diamond", age = 43}

No hace falta utilizar una anotación de tipo explícita en caso de que usemos
el resultado de la función ``read`` de forma que Haskell pueda inferir el
tipo. ::

    ghci> read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" == mikeD  
    True

También podemos leer tipos parametrizados, pero tenemos que especificar todos
los parámetros del tipo. Así que no podemos hacer
``read "Just 't'" :: Maybe a``  pero si podemos hacer ``read "Just 't'" ::
Maybe Char``.

Podemos derivar instancias para la clase de tipo ``Ord``, la cual es para
tipos cuyos valores puedan ser ordenados. Si comparamos dos valores del mismo
tipo que fueron definidos usando diferentes constructores, el valor cuyo 
constructor fuera definido primero es considerado menor que el otro. Por
ejemplo, el tipo ``Bool`` puede tener valores ``False`` o ``True``. Con el
objetivo de ver como se comporta cuando es comparado, podemos pensar que está
implementado de esta forma: ::

    data Bool = False | True deriving (Ord)  

Como el valor ``False`` está definido primero y el valor ``True`` está
definido después, podemos considerar que ``True`` es mayor que ``False``.

    ghci> True `compare` False  
    GT  
    ghci> True > False  
    True  
    ghci> True < False  
    False

En el tipo ``Maybe a``, el constructor de datos ``Nothing`` esta definido
antes que el constructor ``Just``, así que un valor ``Nothing`` es siempre más
pequeño que cualquier valor ``Just algo``, incluso si ese algo es menos un
billon de trillones. Pero si comparamos dos valores ``Just``, entonces se
compara lo que hay dentro de él. ::

    ghci> Nothing < Just 100  
    True  
    ghci> Nothing > Just (-49999)  
    False  
    ghci> Just 3 `compare` Just 2  
    GT  
    ghci> Just 100 > Just 50  
    True

No podemos hacer algo como ``Just (*3) > Just (*2)``, ya que ``(*3)`` y
``(*2)`` son funciones, las cuales no tienen definida una instancia de
``Ord``.

Podemos usar fácilmente los tipos de dato algebraicos para crear
enumeraciones, y las clases de tipo ``Enum`` y ``Bounded`` nos ayudarán a
ello. Considera el siguiente tipo de dato: ::

    data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday  

Como ningún contructore de datos tiene argumentos, podemos hacerlo miembro de
la clase de tipo ``Enum``. La clase ``Enum`` son para cosas que tinen un
predecesor y sucesor. Tambien podemos hacerlo miembro de la clase de tipo
``Bounded``, que es para cosas que tengan un valor mínimo posible y valor
máximo posible. Ya que nos ponemos, vamos a hacer que este tipo tenga una
instancia para todas las clases de tipo derivables que hemos visto y veremos
que podemos hacer con él. ::

    data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
               deriving (Eq, Ord, Show, Read, Bounded, Enum)

Como es parte de las clases de tipo ``Show`` y ``Read``, podemos convertir
valores de est tipo a y desde cadenas. ::

    ghci> Wednesday  
    Wednesday  
    ghci> show Wednesday  
    "Wednesday"  
    ghci> read "Saturday" :: Day  
    Saturday

Como es parte de las clases de tipo ``Eq`` y ``Ord``, podemos comparar o
equiparar días. ::

    ghci> Saturday == Sunday  
    False  
    ghci> Saturday == Saturday  
    True  
    ghci> Saturday > Friday  
    True  
    ghci> Monday `compare` Wednesday  
    LT

También forma parte de ``Bounded``, así que podemos obtener el día mas bajo
o el día más alto. ::

    ghci> minBound :: Day  
    Monday  
    ghci> maxBound :: Day  
    Sunday

También es una instancia de la clase ``Enum``. Podemos obtener el predecesor
y el sucesor de un día e incluso podemos crear listas de rangos con ellos. ::

    ghci> succ Monday  
    Tuesday  
    ghci> pred Saturday  
    Friday  
    ghci> [Thursday .. Sunday]  
    [Thursday,Friday,Saturday,Sunday]  
    ghci> [minBound .. maxBound] :: [Day]  
    [Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday]

Bastante impresionante.


Sinónimos de tipo
-----------------


Anteriormente mencionamos que los tipos ``[Char]`` y ``String`` eran 
equivalentes e intercambiables. Esto está implementado con los **sinónimos de
tipo**. Los sinónimos de tipo no hacen nada por si solo, simplemente dan a
algún tipo un nombre diferente, de forma que obtenga algún significado para
alguien que está leyendo nuestro código o documentación. Aquí tienes como
define la librería estándar ``String`` como sinónimo de ``[Char]``. ::

    type String = [Char]  

.. image:: /images/chicken.png
   :align: left
   :alt: Gallina

Acabamos de intrudir la palabra clave ``type``. Esta palabra clave podría
inducir a errores a algunos, ya que en realidad no estamos haciendo haciendo
nada nuevo (lo hacemos con la palabra clave ``data``). Simplemente estamos
dando un sinónimos a un tipo que ya existe.

Si hacemos una función que convierta una cadena a mayúscuals y la llamamos
``toUpperString`` o algo parecido, podemos darle una declaración de tipo como
``toUpperString :: [Char] -> [Char]`` o ``toUpperString :: String -> String``.
Ambas son esecialmente lo mismo, solo que la última es más legible.

Cuando estabamos hablando del módulo ``Data.Map``, primero presentamos una
agenda de teléfonos representada con una lista de asociación para luego
convertirla en un diccionario. Como ya sabemos, una lista de asociación no
es más que una lista de duplas clave-valor. Vamos a volver a ver la lista que
teníamos. ::

    phoneBook :: [(String,String)]  
    phoneBook =      
        [("betty","555-2938")     
        ,("bonnie","452-2928")     
        ,("patsy","493-2928")     
        ,("lucille","205-2928")     
        ,("wendy","939-8282")     
        ,("penny","853-2492")     
        ]

Vemos que el tipo de ``phoneBook`` es ``[(String,String)]``. Esto nos dice que
es una lista de asociación que asocia cadenas con cadena, pero nada más. Vamos
a crear un sinónimo de tipo para transmitir algo más de información en la
declaración de tipo. ::

    type PhoneBook = [(String,String)]  
    
Ahora la declaración de tipo de nuestra función ``phoneBook`` sería
``phoneBook :: PhoneBook``. Vamos a hacer un sinónimo de tipo para las cadenas
también. ::

    type PhoneNumber = String  
    type Name = String  
    type PhoneBook = [(Name,PhoneNumber)]  

Dar un sinónimo al tipo ``String`` es algo que suelen hacer los programadores
de Haskell cuando quieren transmitir algo más de información acerca del
cometido de las cadenas en sus funciones y que representan.

Así que ahora, cuando implementemos una función que tome el nombre y el número
de teléfono y busque si esa combinación está en nuestra agenda telefónica,
podremos darle una declaración de tipo muy descriptiva: ::

    inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool  
    inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook

Si decidimo no utilizar sinónimos de tipo, nuestra función tendría la
declaración de tipo ``String -> String -> [(String,String)] -> Bool``. En
este caso, la declaración de tipo que utiliza los sinónimos de tipo es mucho
más clara y fácil de entender. Sin embargo, no debes abusar de ellos.
Utilizamos los sinónimos de tipo o bien para indicar que representa un tipo
que ya existe en nuestras funciones (y de esta forma nuestras delcaraciones
de tipo se convierten en la mejor documentación) o bien cuando algo tiene
un tipo muy largo que se repite mucho (como ``[(String,String)]``) y tiene
un significado concreto para nosotros.

Los sinónimos de tipo también pueden ser parametrizados. Si queremos un tipo
que represente las listas de asociación pero también queremos que sea lo
suficientemente general como para utilizar cualquier tipo de clave y valor,
podemos utilizar esto: ::

    type AssocList k v = [(k,v)]  
    
Con esto, una función que tomara un valor por clave en una lista de
asociación puede tener el tipo ``(Eq k) => k -> AssocList k v -> Maybe v``.
``AssocList`` es un constructor de tipo que toma dos tipos y produce un tipo
concreto, como ``AssocList Int String`` por ejemplo. 

.. note:: Cuando hablamos de tipos concretos nos referimos a tipos
          completamente aplicados, como ``Map Int String``. A veces, los
          chicos y yo decimos que ``Maybe`` es un tipo, pero no queremos
          referirnos a eso, ya que cualquier idiota sabe que ``Maybe`` es un
          constructor de tipo. Cuando aplico un tipo extra a ``Maybe``, como
          ``Maybe String``, entonces tengo un tipo concreto. Ya sabes, los
          valores solo pueden tener tipos que sean tipos concretos.
          Concluyendo, vive rápido, quiere mucho y no dejes que nadie te
          tome el pelo.
          
De la misma forma que podemos aplicar parcialmente funciones para obtener
nuevas funciones, podemos aplicar parcialmente los parámetros de tipo y
obtener nuevos constructores de tipo. De la misma forma que llamamos a la
funciones con parámetros de menos para obtener nuevas funciones, podemos
especificar un constructor de tipo con parámetros de menos y obtener un
constructor de tipo parcialmente aplicado. Si queremos un tipo que represente
un diccionario (de ``Data.Map``) que asocie enteros con cualquier otra cosa,
podemos utilizar esto: :: 

    type IntMap v = Map Int v  

O bien esto otro: ::

    type IntMap = Map Int  
    
De cualquier forma, el constructor de tipo ``IntMap`` tomará un parámetro
y ese será el tipo con el que se asociarán los enteros.

.. note:: Si vas a intentar implementar esto, seguramente imporatarás de forma
          cualificada el módulo ``Data.Map``. Cuando realizas una importación
          cualificada, los constructores de tipo también deben estar
          precedidos con el nombre del módulo. Así que tienes que escribir
          algo como ``type IntMap = Map.Map Int``.
          
Asegurate de que realmente entiendes la diferencia entre constructores de
tipos y constructores de datos. Solo porque hayamos creado un sinónimo llamado
``IntMap`` o ``AssocList`` no significa que podamos hacer cosas como 
``AssocList [(1,2),(4,5),(7,9)]``. Lo único que significa es que podemos 
referirnos a ese tipo usando nombres diferentes. Podemos hacer
``[(1,2),(3,5),(8,9)] :: AssocList Int Int``, lo cual hará que los número de
adentro asuman el tipo ``Int``, pero podemos seguir usando esta lista como
si fuera una lista que albergara duplas de enteros. Lo sinónimos de tipo
(y los tipos en general) solo pueden ser utlizados en la porción de Haskell
dedicada a los tipos. Estaremos en esta porción de Haskell cuando estemos
definiendo tipos nuevos (tanto en las declaraciones ``data`` como en las de
``type``) o cuando nos situemos después de un ``::``. ``::`` se utiliza
solo para las declaraciones o anotaciones de tipo. 

Otro tipo de dato interesante que toma dos tipos como parámetro es el tipo
``Either a b``. Así es como se define más o menos: ::

    data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)  

Tiene dos constructores de datos. Si se utiliza ``Left``, entonces contiene
datos del tipo ``a`` y si se utiliza ``Right`` contiene datos del tipo ``b``.
Podemos utilizar este tipo para encapsular un valor de un tipo u otro y así
obtener un valor del tipo ``Either a b``. Normalmente utilizaremos un
ajuste de patrones con ambos, ``Left`` y ``Right``, y nos diferenciaremos
según sea uno u otro. ::

    ghci> Right 20  
    Right 20  
    ghci> Left "w00t"  
    Left "w00t"  
    ghci> :t Right 'a'  
    Right 'a' :: Either a Char  
    ghci> :t Left True  
    Left True :: Either Bool b  

Hasta ahora hemos visto que ``Maybe a`` es utilizado para representar
resultados de cálculos que podrían haber fallado o no. Pero a veces,
``Maybe a`` no es suficientemente bueno ya que ``Nothing`` únicamente nos
informa de que algo ha fallado. Esto esta bien para funciones que solo pueden
fallar de una forma o si no nos interesa saber porque y como han fallado.
Una búqueda en un ``Data.Map`` solo falla cuando la clave que estamos buscando
no se encuentra en el diccionario, así que sabemos exacmente que ha pasado.
Sin embargo, cuando estamos interesados en el cómo o el porqué a fallado algo,
solemos utilizar como resultado el tipo ``Either a b``, donde ``a`` es alguna
especie de tipo que pueda decirnos algo sobre un posible fallo, y ``b`` es
el tipo de un cálculo satisfactorio. Por lo tanto, los errores usan el
constructor de datos ``Left`` mientras que los resultado usan ``Right``.

Un ejemplo: un instituto posee taquillas para que sus estudiantes tengan un
lugar donde guardar sus posters de *Guns'n'Roses*. Cada taquilla tiene una
combinación. Cuando un estudiante quiere una taquilla nueva, le dice al
supervisor de las taquillas que número de taquilla quiere y él le da un
código para esa taquilla. Sin embargo, si alguien ya está usando la taquilla,
no le puede decir el código y tienen que elegir una taquilla diferente.
Utilizaremos un diccionario de ``Data.Map`` para representar las taquillas. 
Asociará el número de la taquilla con duplas que contengan si la taquilla está
en uso o no y el código de la taquilla. ::

    import qualified Data.Map as Map  
  
    data LockerState = Taken | Free deriving (Show, Eq)  
  
    type Code = String  
  
    type LockerMap = Map.Map Int (LockerState, Code)

Bastante simple. Hemo creado un nuevo tipo de dato para representar si una
taquilla esta libre o no, y hemos creado un sinónimo para representar el
código de una taquilla. También creado otro sinónimo para el tipo que asocia
los los números de las taquillas con las duplas de estado y código. Ahora,
vamos a hacer una función que busque un número de taquilla en el diccionario.
Vamos a usar el tipo ``Either String Code`` para representar el resultado,
ya que nuestra búsqueda puede fallar de dos formas: la taquilla ya ha sido
tomada, en cuyo caso decimos quien la posee o si el no hay ninguna taquilla
con ese número. Si la búqueda falla, vamos a utilizar una cadena para obtener
el por qué. ::

    lockerLookup :: Int -> LockerMap -> Either String Code  
    lockerLookup lockerNumber map =   
        case Map.lookup lockerNumber map of   
            Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"  
            Just (state, code) -> if state /= Taken   
                                    then Right code  
                                    else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

Hacemos una búsqueda normal en un diccionario. Si obtenemos ``Nothing``,
devolvemos un valor con el tipo ``Left String`` que diga que esa taquilla no
existe. Si la encontramos, hacemos una comprobación adicional para ver si la
taquilla está libre. Si no lo está, devolvemos un ``Left`` diciendo que la
taquilla a sido tomada. Si lo está, devolvemos un valor del tipo ``Right
Code``, el cual daremos al estudiante. En realidad es un ``Right String``,
aunque hemos creado un sinónimo para añadir un poco más de información en
la declaración de tipo. Aquí tienes un diccionario de ejemplo: ::

    lockers :: LockerMap  
    lockers = Map.fromList   
        [(100,(Taken,"ZD39I"))  
        ,(101,(Free,"JAH3I"))  
        ,(103,(Free,"IQSA9"))  
        ,(105,(Free,"QOTSA"))  
        ,(109,(Taken,"893JJ"))  
        ,(110,(Taken,"99292"))  
        ]

Vamos a buscar el código de unas cuantas taquillas: ::
    
    ghci> lockerLookup 101 lockers  
    Right "JAH3I"  
    ghci> lockerLookup 100 lockers  
    Left "Locker 100 is already taken!"  
    ghci> lockerLookup 102 lockers  
    Left "Locker number 102 doesn't exist!"  
    ghci> lockerLookup 110 lockers  
    Left "Locker 110 is already taken!"  
    ghci> lockerLookup 105 lockers  
    Right "QOTSA"

Podríamos haber utlizado el tipo ``Maybe a`` para representar el resultado
pero entonces no sabríamos el motivo por el cual no podemos obtener el código.
Ahora, tenemos información acerca del fallo en nuestro tipo del resultado.
