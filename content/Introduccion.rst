Introducción
============


Sobre este tutorial
-------------------

Bienvenido a *¡Aprende Haskell por el bien de todos!* Si estás leyendo esto, es
probable que quieras aprender Haskell. Bien has venido al sitio adecuado, pero
primero vamos a hablar un poco sobre este tutorial.

He decidido escribir este tutorial porque quiero arraigar mi propio conocimiento
sobre Haskell y porque pienso que puedo ayudar a la gente que se inicia en
Haskell desde mi perspectiva. Existen bastantes tutoriales sobre Haskell
flotando por la red. Cuando me inicié en Haskell no lo hice desde un solo
documento. La forma en la que aprendí Haskell fue leyendo varios artículos y
tutoriales porque cada uno explicaba algo de una forma diferente en el que lo
hacia otro. Así, yendo a través de varios documentos, fui capaz de juntar todas
las piezas y entonces todo encajó. De modo que este es otro intento de añadir
otro útil documento para aprender Haskell de forma que tengas más oportunidades
de encontrar uno que te guste.

.. image:: /images/bird.png
   :align: left
   :alt: bird

Este tutorial está dirigido a personas que tengan experiencia en lenguajes de
programación imperativa (C, C++, Java, Python...) pero no hayan programado antes
en ningún lenguaje funcional (Haskell, ML, OCaml...). Aunque apuesto que incluso
si no tienes ninguna experiencia significativa en programación, un tipo
inteligente como tú podrá continuar y aprender Haskell.

El canal *#Haskell* de la red *freenode* es un buen lugar para preguntar dudas
si te sientes estancado. La gente es extremadamente amable, paciente y entiende
a los novatos.

Tuve dos intentos fallidos de aprender Haskell antes de por fin captarlo, todo
parecía totalmente extraño para mí. Pero una vez se iluminó camino y tras saltar
la primera valla, fue un cómodo paseo. Creo que lo que trato de decir es:
Haskell es estupendo y si estás interesado en la programación deberías
aprenderlo incluso aunque te sea totalmente extraño. Aprender Haskell es como
aprender a programar por primera vez ¡Es divertido! Te fuerza a que pienses
diferente, lo cual nos lleva a la siguiente sección...


Entonces ¿Qué es Haskell?
-------------------------

.. image:: /images/fx.png
   :align: right
   :alt: bird

Haskell es un *lenguaje de programación funcional puro*. En los lenguajes
imperativos obtienes los resultados dándole al computador una secuencias de
tareas y luego éste las ejecuta. Mientras las ejecuta, puede cambiar de estado.
Por ejemplo, establecemos la variable ``a`` a 5, realizamos algunas cosas y
luego la establecemos a otro valor. Tienes estructuras de control de flujo para
realizar ciertas acciones varias veces (for, while...). En programación
puramente funcional no dices al computador que tiene que hacer, sino más bien
dices como son las cosas. El factorial de un número es el producto de todos los
números desde 1 hasta ese número, la suma de una lista de números es el primer
número más la suma del resto de la lista, y así sucesivamente. Expresas cual
es la forma de las funciones. Además no puedes establecer una variable a algo y
luego establecerla a otra cosa. Si dices que ``a`` es 5, no puedes decir que es
otra cosa después porque acabas de decir que es 5 ¿Qué eres, alguna clase de
mentiroso? De este modo, en los lenguajes puramente funcionales, una función no
tiene efectos secundarios. La única cosa que puede hacer una función es
calcular algo y devolverlo como resultado. Al principio, esto puede parecer
algún tipo de limitación pero en realidad tiene algunas muy buenas
consecuencias: si una función es llamada dos veces con los mismos parámetros,
está garantizado que obtendremos el mismo resultado. Esto es llamado
*transparencia referencial* y no solo permite al compilador razonar acerca de el
comportamiento de un programa, sino que también te permite deducir fácilmente
(he incluso demostrar) que una función es correcta y entonces construir
funciones más complejas simplemente enlazando funciones simples.

.. image:: /images/lazy.png
   :align: right
   :alt: bird

Haskell es *perezoso*. Esto significa que a menos que le especifiques lo
contrario, Haskell no ejecutará las funciones y calculará cosas hasta que se
vea realmente forzado a mostrarte un resultado. Esto funciona bien junto con la
transparencia referencial y permite que veas los programas como una serie de
transformaciones de datos. Incluso te permite hacer cosas interesantes como
estructuras de datos infinitas. Digamos que tenemos una lista de números
inmutables ``xs = [1,2,3,4,5,6,7,8]`` y una función ``doubleMe`` que multiplica
cada elemento por 2 y devuelve una nueva lista. Si queremos multiplicar nuestra
lista por 8 en un lenguaje imperativo he hiciéramos
``doubleMe(doubleMe(doubleMe(xs)))``, probablemente pasaría una vez a través de
la lista, haría una copia y devolvería el valor. Luego, probablemente recorrería
otras dos veces la lista y devolvería un valor. En un lenguaje perezoso, llamar
a ``doubleMe`` con una lista sin forzar que muestre el valor acaba con un
programa diciéndote "Claro claro, ¡Luego lo hago!". Pero cuando quieres ver el
resultado, el primer ``doubleMe`` dice al segundo que quiere el resultado
¡Ahora! El segundo dice al tercero eso mismo y éste a regañadientes devuelve un
1 duplicado, lo cual es un 2. El segundo lo recibe y devuelve a un 4 al primero.
El primero ve el resultado y dice que el primer elemento de la lista es un 8.
Así que solo hace un recorrido a través de la lista y solo cuando lo necesitas.
De esta forma cuando quieres algo de un lenguaje perezoso, simplemente puedes
tomar los datos iniciales y eficientemente transformarlos y ensamblarlos hasta
que se parezca hasta lo que quieres obtener al final.

.. image:: /images/boat.png
   :align: right
   :alt: bird

Haskell es *estáticamente tipado*. Cuando compilas tu programa, el compilador
sabe que piezas del código son un entero, cuales una cadena, etc.. Esto
significa que un montón de posibles errores son atrapados en tiempo de
compilación. Si intentas sumar un número y una cadena, el compilador se quejará.
Haskell usa un fantástico sistema de tipos que posee inferencia de tipos. Esto
significa que no tienes que etiquetar cada trozo de código explícitamente con un
tipo porque el sistema de tipos lo puede deducir de forma inteligente. La
inferencia de tipos también permite que tu código sea más general, si has hecho
una función que toma dos números y los sumas y no estableces de forma explícita
sus tipos, la función aceptará cualquier par de parámetros que actúen como
números.

Haskell es elegante y conciso. Esto es debido ha que utiliza conceptos de alto
nivel, los programas Haskell son normalmente más cortos que los equivalentes
imperativos. Y los programas cortos son más fáciles de mantener que los largos y
tienen menos errores.

Haskell fue creado por unos tipos realmente inteligentes (con sus doctorados).
El trabajo en Haskell comenzó en 1987 cuando un comité de investigadores se
pusieron de acuerdo para diseñar un lenguaje genial. En el 2003 el informe
Haskell fue publicado, el cual define una versión estable del lenguaje.


Qué necesitas para comenzar
---------------------------

Un editor de texto y un compilador de Haskell. Probablemente ya tienes instalado
tu editor de texto favorito así que no vamos a perder el tiempo con esto. Los
dos principales compiladores de Haskell ahora mismo son GHC (Glasgow Haskell
Compiler) y Hugs. Para los propósito de este tutorial usaremos GHC. No voy a
cubrir muchos detalles de la instalación. En Windows es cuestión de descargarse
el instalador,  pulsar "siguiente" un par de veces y luego reiniciar tu
ordenador. En las distribuciones de Linux basadas en Debian simplemente haz
``apt-get install ghc6 libghc6-mtl-dev`` y sonríe. No tengo un mac, pero he oído
que si tienes  `MacPorts <http://www.macports.org/>`_ puedes obtener GHC
simplemente ejecutando ``sudo port install ghc``. También creo que puedes
desarrollar con Haskell con ese peculiar ratón con un solo botón, pero no estoy
seguro.

GHC puede tomar un script de Haskell (normalmente tienen la extensión .hs) y
compilarlo pero también tiene un modo interactivo el cual te permite
interactuar con dichos scripts. Puedes llamar a funciones de los scripts que
hayas cargado y los resultados serán mostrados de forma inmediata. Para aprender
es mucho más fácil y rápido que compilar cada vez que hagas un cambio y luego
ejecutar tu programa desde una consola. El modo interactivo es invocado
tecleando ``ghci`` desde tu terminal. Si has definido algunas funciones en un
fichero llamado, digamos, ``myFunctions.hs``, puedes cargar esas funciones
tecleando ``:l myFunctions`` y jugar con ellas, siempre que ``myFunctions.hs``
esté en el mismo directorio en el que fue invocado ``ghci``. Si modificas el
script .hs, simplemente ejecuta ``:l myFunctions`` o haz ``:r``, lo cual es
equivalente ya recarga el script actual. Mi forma de trabajo usual cuando juego
con estas cosas es definir algunas funciones en un fichero .hs, las cargo y paso
el rato con ellas, luego modifico el fichero .hs volviendo a cargarlo y así
sucesivamente. Esto es también lo que vamos a hacer aquí.
