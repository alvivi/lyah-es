

Funtores, funtores aplicables y monoides
========================================


La combinación de Haskell de la pureza, las funciones de orden superior,
tipos de datos algebraicos con parámetros, y clases de tipos nos permite
implementar el polimorfismo a un nivel superior al que pueden alcanzar otros
lenguajes. No tenemos que ver los tipos como si pertenecieran a una gran
jerarquía de tipos. En lugar de eso, vemos como pueden actuar los tipos y
luego los conectamos con las clases de tipos apropiadas. Un ``Int`` puede
actuar como un montón de cosas. Puede actuar como algo equiparable, como algo
ordenable, como algo enumerable, etc.

Las clases de tipos son abiertas, lo que significa que podemos definir nuestro
propio tipo de dato, razonar en como éste actúa y conectarlo con la clase de
tipos que define ese comportamiento. Por este motivo, y porque el fabuloso
sistema de tipos de Haskell nos permite saber mucho acerca de una función con
tan solo sabiendo su declaración de tipo, podemos crear clases de tipos que
definen comportamientos muy generales y abstractos. Ya vimos que las clases
de tipos definen operaciones para ver si dos cosas son iguales o comparar dos
cosas por un cierto orden. Son comportamientos muy abstractos a la vez que
elegantes, pero no los vemos como algo especial ya que hemos estado tratando
con ellos a lo largo de nuestras vidas. Hace poco conocimos los funtores, que
son básicamente cosas que se pueden mapear. Esto es un ejemplo de algo útil y
a la vez bastante abstracto de lo que pueden describir las clases de tipos. En
este capítulo veremos más de cerca los funtores, junto a una versión más
fuerte y útil de los funtores llamados funtores aplicables. También daremos un
vistazo a los monoides.
 
