Introducción
============


Sobre esta guía
---------------

Bienvenido a *¡Aprende Haskell por el bien de todos!* Si estás leyendo esto
probablemente quieras aprender Haskell. Pues bien, has venido al sitio
adecuado, pero primero vamos a hablar un poco sobre esta guía.

Decidí escribir esta guía porque quería arraigar mi propio conocimiento de
Haskell y porque pensé que podía ayudar a la gente que empezaba con Haskell.
Existen bastantes manuales y guías sobre Haskell por la red. Cuando empecé con
Haskell no lo leí un único documento. La forma en la que aprendí Haskell fue
leyendo varios artículos y guías, porque explicaban el mismo concepto de
diferentes formas. Así, yendo a través de varios documentos, fui capaz de
juntar todas las piezas y entonces todo encajó. De modo que, esto es un
intento más de añadir otro útil documento para aprender Haskell de forma que
tengas más oportunidades de encontrar uno que te guste.

.. image:: /images/bird.png
   :align: left
   :alt: pájaro

Esta guía está dirigida a personas que tengan experiencia en lenguajes de
programación imperativa (C, C++, Java, Python...) pero que no hayan programado
antes en ningún lenguaje funcional (Haskell, ML, OCaml...). Aunque apuesto que
incluso si no tienes experiencia como programador, un tipo inteligente como tú
podrá seguir adelante y aprender Haskell.

El canal *#Haskell* de la red *freenode* es un buen lugar para preguntar dudas
si te sientes estancado y sabes inglés. La gente es bastante amable, paciente
y comprensible con los que empiezan.

Intenté aprender Haskell, dos veces, antes de conseguir entenderlo. Al
principio todo parecía extraño. Pero una vez que se iluminó el camino y tras
saltar el primer obstáculo, fue un cómodo paseo. Creo que lo que trato de
decir es que Haskell es genial y si estás interesado en la programación
deberías aprenderlo incluso aunque te sea totalmente extraño. Aprender Haskell
es como aprender a programar por primera vez ¡Es divertido! Te fuerza a que
pienses diferente, lo cual nos lleva a la siguiente sección...


Entonces, ¿qué es Haskell?
--------------------------

.. image:: /images/fx.png
   :align: right
   :alt: f(x)

Haskell es un **lenguaje de programación puramente funcional**. En los
lenguajes imperativos obtenemos resultados dándole al computador una secuencia
de tareas que luego éste ejecutará. Mientras las ejecuta, puede cambiar de
estado. Por ejemplo, establecemos la variable ``a`` a 5, realizamos algunas
tareas y luego cambiamos el valor de la variable anterior. Estos lenguajes
poseen estructuras de control de flujo para realizar ciertas acciones varias
veces (``for``, ``while``...). Con la programación puramente funcional no
decimos al computador lo que tiene que hacer, sino más bien, decimos cómo son
las cosas. El factorial de un número es el producto de todos los números desde
el 1 hasta ese número, la suma de una lista de números es el primer número más
la suma del resto de la lista, etc. Expresamos la forma de las funciones.
Además no podemos establecer una variable a algo y luego establecerla a otra
cosa. Si decimos que ``a`` es 5, luego no podemos decir que es otra cosa
porque acabamos de decir que es 5 ¿Acaso somos unos mentirosos? De este modo,
en los lenguajes puramente funcionales, una función no tiene efectos
secundarios. Lo único que puede hacer una función es calcular y devolver algo
como resultado. Al principio esto puede parecer una limitación pero en
realidad tiene algunas buenas consecuencias: si una función es llamada dos
veces con los mismos parámetros, obtendremos siempre el mismo resultado. A
esto lo llamamos *transparencia referencial* y no solo permite al compilador
razonar acerca de el comportamiento de un programa, sino que también nos
permite deducir fácilmente (e incluso demostrar) que una función es correcta y
así poder construir funciones más complejas uniendo funciones simples.

.. image:: /images/lazy.png
   :align: right
   :alt: perezoso

Haskell es **perezoso**. Es decir, a menos que le indiquemos lo contrario,
Haskell no ejecutará funciones ni calculará resultados hasta que se vea
realmente forzado a hacerlo. Esto funciona muy bien junto con la transparencia
referencial y permite que veamos los programas como una serie de
transformaciones de datos. Incluso nos permite hacer cosas interesantes como
estructuras de datos infinitas. Digamos que tenemos una lista de números
inmutables ``xs = [1,2,3,4,5,6,7,8]`` y una función ``doubleMe`` que
multiplica cada elemento por 2 y devuelve una nueva lista. Si quisiéramos
multiplicar nuestra lista por 8 en un lenguaje imperativo he hiciéramos
``doubleMe(doubleMe(doubleMe(xs)))``, probablemente el computador recorrería
la lista, haría una copia y devolvería el valor. Luego, recorrería otras dos
veces más la lista y devolvería el valor final. En un lenguaje perezoso, llamar a
``doubleMe`` con una lista sin forzar que muestre el valor acaba con un
programa diciéndote "Claro claro, ¡luego lo hago!". Pero cuando quieres ver el
resultado, el primer ``doubleMe`` dice al segundo que quiere el resultado,
¡ahora! El segundo dice al tercero eso mismo y éste a regañadientes devuelve
un 1 duplicado, lo cual es un 2. El segundo lo recibe y devuelve un 4 al
primero. El primero ve el resultado y dice que el primer elemento de la lista
es un 8. De este modo, el computador solo hace un recorrido a través de la
lista y solo cuando lo necesitamos. Cuando queremos calcular algo a partir de
unos datos iniciales en un lenguaje perezoso, solo tenemos que tomar estos
datos e ir transformándolos y moldeándolos hasta que se parezcan al resultado
que deseamos.

.. image:: /images/boat.png
   :align: right
   :alt: bote

Haskell es un lenguaje **tipificado estáticamente**. Cuando compilamos un
programa, el compilador sabe que trozos del código son enteros, cuales son
cadenas de texto, etc. Gracias a esto un montón de posibles errores son
capturados en tiempo de compilación. Si intentamos sumar un número y una
cadena de texto, el compilador nos regañará. Haskell usa un fantástico sistema
de tipos que posee inferencia de tipos. Esto significa que no tenemos que
etiquetar cada trozo de código explícitamente con un tipo porque el sistema de
tipos lo puede deducir de forma inteligente. La inferencia de tipos también
permite que nuestro código sea más general, si hemos creado una función que
toma dos números y los suma y no establecemos explícitamente sus tipos, la
función aceptará cualquier par de parámetros que actúen como números.

Haskell es elegante y conciso. Se debe a que utiliza conceptos de alto nivel.
Los programas Haskell son normalmente más cortos que los equivalentes
imperativos. Y los programas cortos son más fáciles de mantener que los
largos, además de que poseen menos errores.

Haskell fue creado por unos tipos muy inteligentes (todos ellos con sus
respectivos doctorados). El proyecto de crear Haskell comenzó en 1987 cuando
un comité de investigadores se pusieron de acuerdo para diseñar un lenguaje
revolucionario. En el 2003 el informe Haskell fue publicado, definiendo así
una versión estable del lenguaje.


Qué necesitas para comenzar
---------------------------

Un editor de texto y un compilador de Haskell. Probablemente ya tienes
instalado tu editor de texto favorito así que no vamos a perder el tiempo con
esto. Ahora mismo, los dos principales compiladores de Haskell son GHC
(Glasgow Haskell Compiler) y Hugs. Para los propósitos de esta guía usaremos
GHC. No voy a cubrir muchos detalles de la instalación. En Windows es cuestión
de descargarse el instalador, pulsar "siguiente" un par de veces y luego
reiniciar el ordenador. En las distribuciones de Linux basadas en Debian se
puede instalar con ``apt-get`` o instalando un paquete ``deb``. En MacOS es
cuestión de instalar un ``dmg`` o utilizar ``macports``. Sea cual sea tu
plataforma, `aquí <http://hackage.haskell.org/platform/>`_ tienes más
información.

GHC toma un script de Haskell (normalmente tienen la extensión *.hs*) y lo
compila, pero también tiene un modo interactivo el cual nos permite
interactuar con dichos scripts. Podemos llamar a las funciones de los scripts
que hayamos cargado y los resultados serán mostrados de forma inmediata. Para
aprender es mucho más fácil y rápido en lugar de tener que compilar y ejecutar
los programas una y otra vez. El modo interactivo se ejecuta tecleando
``ghci`` desde tu terminal. Si hemos definido algunas funciones en un fichero
llamado, digamos, ``misFunciones.hs``, podemos cargar esas funciones tecleando
``:l misFunciones``, siempre y cuando ``misFunciones.hs`` esté en el mismo
directorio en el que fue invocado ``ghci``. Si modificamos el script *.hs* y
queremos observar los cambios tenemos que volver a ejecutar ``:l
misFunciones`` o ejecutar ``:r`` que es equivalente ya que recarga el script
actual. Trabajaremos definiendo algunas funciones en un fichero *.hs*, las
cargamos y pasamos el rato jugando con ellas, luego modificaremos el fichero
*.hs* volviendo a cargarlo y así sucesivamente. Seguiremos este proceso
durante toda la guía.
