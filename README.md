# Chetto_Chatty

## Introducción para SODX
### Projecte format per Toni Cifré, Daniel Beltrán i Jaume Florit. 

#### En aquesta pràctica el que inicialment havíem plantetjat és realitzar una expansió del Laboratori 1 (chat). Alguns dels següents punts eren les idees inicials que teniem per implementar, tot i que, a dia d'avui, algunes d'elles no s'han implementat i s'han ficat d'altres.

  - Els usuaris puguin assignar-se un color
  - Creació i administració de canals del chat
  - Eliminació de paraules que no sigui apropiades pel chat.
  - Implementació d'una interfície gràfica (resultava molt més problemàtic).

#### Aquests son alguns dels exemples que havíem plantejat amb anterioritat. Els punts finals que implementa el chat són:
  - Manipulació d'opcions d'usuari (Les dades es guarden en un fitxer extern que permet guardar la configuració).
  - Canvi de nom d'un usuari
  - Assignació de colors per als noms dels usuaris
  - Llistats de clients
  - Eliminar un client concret o eliminar tot el seu conjunt
  - Canviar i crear grups
  - Eliminar grups
  

---------------------------------------------------------------------------------------------------------------------------
## IMPLEMENTACIÓ I UTILITZACIÓ

Iniliciación de un nodo de erlang:
> erl -name node@127.0.0.1 -setcookie secret


Compilación y arranque del servidor:
```erlang
c(server2).
server2:start().
```
Arrancar un servidor unido al primero:
```erlang
c(server2).
server2:start({myserver, 'server_node@127.0.0.1'}).
```

Compilación y arranque de un clients:
```erlang
c(client).
client:start({myserver, 'server_node@127.0.0.1'}, "Name").
```

## Funcionalidades del servidor
##### - List_clients
Imprime una lista de todos los clienes conectados entre los distintos servidores.

##### - List_groups
imprime todos los grupos creados y cada uno de los usuarios que hay dentro de ellos.

##### - Remove_all_clients
Elimina todos los clientes del servidor en concreto.

##### - Remove_client
Elimina a un cliente en concreto.


##### - exit
Redirige a los usuarios a otro servidor disponible.

## Funcionalidades del cliente
##### - All
Envia el mensaje deseado a todos los clientes independiente-mente del gripo en el que esten.
##### - private
Envia un mensaje privado a un solo cliente.
##### - Change_group
Cambia el cliente de grupo o crea un nuevo grupo en el caso de que el grupo no este creado.
##### - Settings
Abre la lista de comandas que nos permite editar las preferencias de colores y de nombre de nuestro usuario. Dentro de esta lista de comandas tenemos:
  - changeName: Nos permite cambiar de nombre
  - changeMyColor: Nos permite cambiar el color que aparecen nuestro nombre de usuario al enviar un mensaje.
  - changeServerColor: Permite cambiar el color del nombre del Servidor.
  - changeWarningColor: Permite cambiar el color de los mensajes de emergencia.
  - changeInfoColor: Permite cambiar el color de los mensajes informativos.
  - changeMyMessageColor: Permite cambiar el color del mensaje que nosotros hemos enviado
  - changeMessageColor: Permite cambiar el color del mensaje recibido.
  - exit: Nos permite salir de la lista de comandas
  - help: Nos muestra los comandos a realizar
  
Todas estas preferencias son guardadas en un archivo para manteneralas y evitar su perdida de sesión a sesión.

-----------------------------------------------------------------------------------------------------------------------------
## Conclusió
Des del nostre punt de vista, aquest projecte ens ha anat bé per aprendre amb molt més detall la funcionalitat i programació de l'Erlang. En qualsevol cas, hem invertit molt temps i esforç en aprendre bé la seva programació ja que en la majoria de cassos en ha resultat ser molt complexe. Creiem que hem assolit bé els continguts que se'ns demanava per a la pràctica encara que ens hauria agradat disposar de més temps per expandir el chat a més funcionalitats.
