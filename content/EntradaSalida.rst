

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

