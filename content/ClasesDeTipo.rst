

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



 

