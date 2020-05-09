-module(client).

-import(lists,[last/1]).

-include("settings.hrl").

%% Exported Functions
-export([start/1, init_client/2]).

%% API Functions
start(ServerPid) ->
	ClientPid = spawn(client, init_client, [ServerPid, ?MyName]),
	process_commands(ServerPid, ?MyName, ClientPid, "Global").

init_client(ServerPid, MyName) ->
	ServerPid ! {client_join_req, MyName, self()},
	process_requests(MyName).

%% Local Functions
%% This is the background task logic
process_requests(MyName) ->
	receive
		{join, Name} ->
			io:format("~s ~s ~s~n", [color:rgb(?InfoMessageColor,"[JOIN]"),color:rgb(?InfoMessageColor, Name), color:rgb(?InfoMessageColor,"joined")]), 
			process_requests(MyName);
		{leave, Name} ->
			io:format("~s ~s ~s~n", [color:rgb(?InfoMessageColor,"[EXIT]"),color:rgb(?InfoMessageColor, Name), color:rgb(?InfoMessageColor,"exited")]), 
			process_requests(MyName);
		{message, Name, Text} ->
			if Name == ?MyName ->
				 io:format("[~s] ~s", [color:rgb( ?MyNameColor, Name), color:rgb(?MyMessageColor,Text)]);
			 Name == "Server" ->
			 	 io:format("[~s] ~s", [color:rgb( ?ServerNameColor, Name), color:rgb(?InfoMessageColor,Text)]);
			 true ->
			 	 io:format("[~s] ~s", [Name, color:rgb(?MessageColor,Text)])
		 	 end,
			process_requests(MyName);

		{message_private, Name, Text} ->
			io:format("<~s> ~s", [Name, Text]),
			process_requests(MyName);

		{message_group, Name, Text} ->
			io:format("(~s) ~s", [Name, Text]),
			process_requests(MyName);


		{change_server, Servers} ->
			if length(Servers) > 0 ->
					io:format("~s",[color:rgb(?InfoMessageColor,"El servidor se ha desconectado.\nReconectando con otro servidor...\n")]),
					NewServer = last(Servers),
					start(NewServer),
					ok;
				true ->
					io:format("~s",[color:rgb(?WarningColor,"El servidor se ha desconectado y no existen mas servidores.\nEscribe 'exit' para salir.\n")])
			end;
		exit ->
			io:format("~s", [color:rgb(?WarningColor, "El servidor te ha expulsado del canal, Introduce 'exit' para salir.\n")]),
			ok
	end.

%% This is the main task logic
process_commands(ServerPid, MyName, ClientPid, Group) ->
	%% Read from the standard input and send to server
	Text = io:get_line("-> "),
	if  Text == "exit\n" ->
			ServerPid ! {client_leave_req, MyName, ClientPid, Group},
			ok;

		Text == "settings\n" ->
			io:format("Bienvenido al apartado opciones y preferencias, si quiere conocer las comandas disponibles, escriba 'help'\n"),
			{ok, File} = file:read_file("settings.hrl"),
			settings(ServerPid, MyName, ClientPid, Group, File);

		Text == "All\n" ->
			Message = io:get_line("Mensaje general\n-> "),
			ServerPid ! {send, MyName, Message},
			process_commands(ServerPid, MyName, ClientPid, Group);
		
		Text == "private\n" ->
			Aux = io:get_line("Nombre del usuario: "),
			Name = re:replace(Aux, "\\s+", "", [global,{return,list}]),
			Message = io:get_line("Mensaje privado\n-> "),
			ServerPid ! {set_private_send, MyName, ClientPid, Name, Message},
			process_commands(ServerPid, MyName, ClientPid, Group);

		Text == "Change_group\n" ->
			Aux = io:get_line("Introduce el nombre del grupo: "),
			NameGroup = re:replace(Aux, "\\s+", "", [global,{return,list}]),
			ServerPid ! {change_group, MyName, ClientPid, Group, NameGroup},
			process_commands(ServerPid, MyName, ClientPid, NameGroup);

		Text == "help\n" ->
       		io:format("~s",[color:rgb(?InfoMessageColor,"Lista de comandas disponibles:\n")]),
       		io:format("~s",[color:rgb(?InfoMessageColor,"'All' -> Envia un mensaje a todos los clientes independientemente de en que grupo esten. \n")]),
       		io:format("~s",[color:rgb(?InfoMessageColor,"'private' -> Envia un mensaje a un usuario en concreto.\n")]),
       		io:format("~s",[color:rgb(?InfoMessageColor,"'Change_group' -> Cambiar de grupo (se pueden crear grupos nuevos).\n")]),
       		io:format("~s",[color:rgb(?InfoMessageColor,"'exit' -> Desconnexión del servidor.\n")]),
       		io:format("~s",[color:rgb(?InfoMessageColor,"'settings' -> Abre el editor de opciones y preferencias\n")]),
        	process_commands(ServerPid, MyName, ClientPid, Group);

		true ->
			ServerPid ! {send, MyName, Text},
			process_commands(ServerPid, MyName, ClientPid, Group)
	end.

settings(ServerPid, MyName, ClientPid, Group, Content) ->
	Text = io:get_line("Opciones y preferencias: "),
	if Text == "exit\n" ->
		 io:format("Para actualizar los cambios debera recargar el modulo y reiniciar el cliente\n"),
		 file:write_file("settings.hrl", Content),
	     process_commands(ServerPid, MyName, ClientPid, Group);

		Text == "help\n" ->	
		 io:format("Comandos:\n
 	exit -> Permite salir de opciones y preferencias\n
	help -> Imprime por pantalla la ayuda\n
 	changeName: Nos permite cambiar de nombre\n
	changeMyColor: Nos permite cambiar el color que aparecen nuestro nombre de usuario al enviar un mensaje.\n
	changeServerColor: Permite cambiar el color del nombre del Servidor.\n
	changeWarningColor: Permite cambiar el color de los mensajes de emergencia.\n
	changeInfoColor: Permite cambiar el color de los mensajes informativos.\n
	changeMyMessageColor: Permite cambiar el color del mensaje que nosotros hemos enviado\n
	changeMessageColor: Permite cambiar el color del mensaje recibido.\n\n"),
		 settings(ServerPid, MyName, ClientPid, Group, Content);

		Text == "changeName\n" ->
			 NewName = string:strip(io:get_line("Nombre nuevo: "), right, $\n),
			 changeName(ServerPid, NewName, MyName, ClientPid, Content, Group);

		Text == "changeMyColor\n" ->
			 io:format("Introduzca el nuevo color en formato RGB: "),
			 {R, Aux} = string:to_integer(string:chomp(io:get_line("R (Enteros del 0 al 5): "))),
			 {G, Aux} = string:to_integer(string:chomp(io:get_line("G (Enteros del 0 al 5): "))),
			 {B, Aux} = string:to_integer(string:chomp(io:get_line("B (Enteros del 0 al 5): "))),
			 NewColor = [R, G, B],
			 changeMyColor(ServerPid, NewColor, ?MyNameColor, ClientPid, Content, Group);
		 
 		Text == "changeWarningColor\n" ->
			 io:format("Introduzca el nuevo color en formato RGB: "),
			 {R, Aux} = string:to_integer(string:chomp(io:get_line("R (Enteros del 0 al 5): "))),
			 {G, Aux} = string:to_integer(string:chomp(io:get_line("G (Enteros del 0 al 5): "))),
			 {B, Aux} = string:to_integer(string:chomp(io:get_line("B (Enteros del 0 al 5): "))),
			 NewColor = [R, G, B],
			 changeWarningColor(ServerPid, NewColor, ?WarningColor, ClientPid, Content, Group);
		
		Text == "changeServerColor\n" ->
			 io:format("Introduzca el nuevo color en formato RGB:\n"),
			 {R, Aux} = string:to_integer(string:chomp(io:get_line("R (Enteros del 0 al 5): "))),
			 {G, Aux} = string:to_integer(string:chomp(io:get_line("G (Enteros del 0 al 5): "))),
			 {B, Aux} = string:to_integer(string:chomp(io:get_line("B (Enteros del 0 al 5): "))),
			 NewColor = [R, G, B],
			 changeServerColor(ServerPid, NewColor, ?ServerNameColor, ClientPid, Content, Group);

		 Text == "changeInfoColor\n" ->
			 io:format("Introduzca el nuevo color en formato RGB:\n"),
			 {R, Aux} = string:to_integer(string:chomp(io:get_line("R (Enteros del 0 al 5): "))),
			 {G, Aux} = string:to_integer(string:chomp(io:get_line("G (Enteros del 0 al 5): "))),
			 {B, Aux} = string:to_integer(string:chomp(io:get_line("B (Enteros del 0 al 5): "))),
			 NewColor = [R, G, B],
			 changeInfoColor(ServerPid, NewColor, ?ServerNameColor, ClientPid, Content, Group);

		 Text == "changeMyMessageColor\n" ->
			 io:format("Introduzca el nuevo color en formato RGB:\n"),
			 {R, Aux} = string:to_integer(string:chomp(io:get_line("R (Enteros del 0 al 5): "))),
			 {G, Aux} = string:to_integer(string:chomp(io:get_line("G (Enteros del 0 al 5): "))),
			 {B, Aux} = string:to_integer(string:chomp(io:get_line("B (Enteros del 0 al 5): "))),
			 NewColor = [R, G, B],
			 changeMyMessageColor(ServerPid, NewColor, ?MyMessageColor, ClientPid, Content, Group);

		 Text == "changeMessageColor\n" ->
			 io:format("Introduzca el nuevo color en formato RGB:\n"),
			 {R, Aux} = string:to_integer(string:chomp(io:get_line("R (Enteros del 0 al 5): "))),
			 {G, Aux} = string:to_integer(string:chomp(io:get_line("G (Enteros del 0 al 5): "))),
			 {B, Aux} = string:to_integer(string:chomp(io:get_line("B (Enteros del 0 al 5): "))),
			 NewColor = [R, G, B],
			 changeMessageColor(ServerPid, NewColor, ?MessageColor, ClientPid, Content, Group);

		true ->
			io:format("~s~n",[color:rgb(?WarningColor,"No he entendido la comanda, escriba help para sacar la ayuda o vuelva a probar")]),
	 		settings(ServerPid, MyName, ClientPid, Group, Content)
	 end.

changeName(ServerPid, NewName, OldName, ClientPid, Content, Group) ->
	io:format("Su nombre ~s ha sido cambiado por ~s~n", [ color:true("00FF00", OldName), color:true("00FF00", NewName)]),
	NewLine = ["-define ( MyName, << ",34, NewName, 34," >> ).\n"],
	OldLine = ["-define ( MyName, << ",34, OldName, 34," >> ).\n"],
	actualizeContent(Content, NewLine, OldLine, ServerPid, ClientPid, Group).

changeMyColor(ServerPid, NewColor, OldColor, ClientPid, Content, Group) ->
	io:format("El color ha sido cambiado por ~s~n", [color:rgb(NewColor, "este")]),
	NewLine = ["-define ( MyNameColor, ",lists:flatten(io_lib:format("~p", [NewColor]))," ).\n"],
	OldLine = ["-define ( MyNameColor, ",lists:flatten(io_lib:format("~p", [OldColor]))," ).\n"],
	actualizeContent(Content, NewLine, OldLine, ServerPid, ClientPid, Group).

changeWarningColor(ServerPid, NewColor, OldColor, ClientPid, Content, Group) ->
	io:format("El color ha sido cambiado por ~s~n", [color:rgb(NewColor, "este")]),
	NewLine = ["-define ( WarningColor, ",lists:flatten(io_lib:format("~p", [NewColor]))," ).\n"],
	OldLine = ["-define ( WarningColor, ",lists:flatten(io_lib:format("~p", [OldColor]))," ).\n"],
	actualizeContent(Content, NewLine, OldLine, ServerPid, ClientPid, Group).

changeServerColor(ServerPid, NewColor, OldColor, ClientPid, Content, Group) ->
	io:format("El color ha sido cambiado por ~s~n", [color:rgb(NewColor, "este")]),
	NewLine = ["-define ( ServerNameColor, ",lists:flatten(io_lib:format("~p", [NewColor]))," ).\n"],
	OldLine = ["-define ( ServerNameColor, ",lists:flatten(io_lib:format("~p", [OldColor]))," ).\n"],
	actualizeContent(Content, NewLine, OldLine, ServerPid, ClientPid, Group).

changeInfoColor(ServerPid, NewColor, OldColor, ClientPid, Content, Group) ->
	io:format("El color ha sido cambiado por ~s~n", [color:rgb(NewColor, "este")]),
	NewLine = ["-define ( InfoMessageColor, ",lists:flatten(io_lib:format("~p", [NewColor]))," ).\n"],
	OldLine = ["-define ( InfoMessageColor, ",lists:flatten(io_lib:format("~p", [OldColor]))," ).\n"],
	actualizeContent(Content, NewLine, OldLine, ServerPid, ClientPid, Group).

changeMyMessageColor(ServerPid, NewColor, OldColor, ClientPid, Content, Group) ->
	io:format("El color ha sido cambiado por ~s~n", [color:rgb(NewColor, "este")]),
	NewLine = ["-define ( MyMessageColor, ",lists:flatten(io_lib:format("~p", [NewColor]))," ).\n"],
	OldLine = ["-define ( MyMessageColor, ",lists:flatten(io_lib:format("~p", [OldColor]))," ).\n"],
	actualizeContent(Content, NewLine, OldLine, ServerPid, ClientPid, Group).

changeMessageColor(ServerPid, NewColor, OldColor, ClientPid, Content, Group) ->
	io:format("El color ha sido cambiado por ~s~n", [color:rgb(NewColor, "este")]),
	NewLine = ["-define ( MessageColor, ",lists:flatten(io_lib:format("~p", [NewColor]))," ).\n"],
	OldLine = ["-define ( MessageColor, ",lists:flatten(io_lib:format("~p", [OldColor]))," ).\n"],
	actualizeContent(Content, NewLine, OldLine, ServerPid, ClientPid, Group).


actualizeContent(Content, NewLine, OldLine, ServerPid, ClientPid, Group) ->
	ContentList = string:replace(Content, OldLine, NewLine, all),
	settings(ServerPid, ?MyName, ClientPid, Group ,ContentList).