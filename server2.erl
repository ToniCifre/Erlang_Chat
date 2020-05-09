-module(server2).

-import(proplists,[get_value/2]).
%% Exported Functions
-export([start/0, start/1, init_server/0, init_server/1, process_requests/3]).

%% API Functions
start() ->
	ServerPid = spawn(server2, init_server, []),
	register(myserver, ServerPid),
	process_commands(ServerPid).

start(BootServer) ->
	ServerPid = spawn(server2, init_server, [BootServer]),
	register(myserver, ServerPid),
	process_commands(ServerPid).

init_server() ->
	process_requests([], [self()], [{"Global", []}]).

init_server(BootServer) ->
	BootServer ! {server_join_req, self()},
	process_requests([], [], []).

process_requests(Clients, Servers, Groups) ->
	receive
		%% Messages between client and server
		% A new client joins
		{client_join_req, Name, From} ->
			io:format("[JOIN] ~s~n", [Name]),
			NewClients = [{Name, From}|Clients],

			Clients_in_group = get_value("Global", Groups),
			NewClients_in_group = [{Name, From}|Clients_in_group],
			NewGroups = lists:keyreplace("Global", 1, Groups, {"Global", NewClients_in_group}),

			broadcast_server(Servers, {join, Name}),
			broadcast_server(Servers, {set_groups, NewGroups}),
			process_requests(NewClients, Servers, NewGroups);
		% A client leaves
		{client_leave_req, Name, From, Group} ->
			io:format("[EXIT] ~s~n", [Name]),
			NewClients = lists:delete({Name, From}, Clients),

			Clients_in_group = get_value(Group, Groups),
			NewClients_in_group = lists:delete({Name, From}, Clients_in_group),
			NewGroups = lists:keyreplace(Group, 1, Groups, {Group, NewClients_in_group}),

			broadcast_server(Servers, {leave, Name}),
			broadcast_server(Servers, {set_groups, NewGroups}),
			From ! exit,
			process_requests(NewClients, Servers, NewGroups);

		{change_group, Name, From,OldGroup, NewGroup} ->
			Aux = get_value(NewGroup, Groups),
			if Aux =:= undefined->
					io:format("Creant nou grup\n"),
					NewGroups = [{NewGroup, [{Name, From}]}|Groups];

				true->
					NewClients_in_group = [{Name, From}|Aux],
					NewGroups = lists:keyreplace(NewGroup, 1, Groups, {NewGroup, NewClients_in_group})

			end,

			Clients_in_group = get_value(OldGroup, NewGroups),
			NewClients = lists:delete({Name, From}, Clients_in_group),
			NewGroups2 = lists:keyreplace(OldGroup, 1, NewGroups, {OldGroup, NewClients}),

			broadcast_server(Servers, {set_groups, NewGroups2}),
			process_requests(Clients, Servers, NewGroups2);
		% A client sends a message
		{send, Name, Text} ->
			io:format("~p: ~s", [Name, Text]),
			broadcast_server(Servers, {message, Name, Text}),
			process_requests(Clients, Servers, Groups);
		% A client sends a message
		{send_group, Name, Group, Text} ->
			io:format("~p in group ~s: ~s", [Name, Group, Text]),
			Clients_in_group = get_value(Group, Groups),
			broadcast(Clients_in_group, {message_group, Name, Text}),
			process_requests(Clients, Servers, Groups);
		% El cliente envia un mensaje privado
		{set_private_send, From, FromPID, To, Text} ->
			io:format("<~p>: ~s", [From, Text]),
			broadcast_server(Servers, {private_send, From, FromPID, To, Text}),
			process_requests(Clients, Servers, Groups);
		% Cada uno de los servidores mira si existe ese cliente i lo envia exclusivamente a el
		{private_send, From, FromPID, To, Text} ->
			Aux = get_value(To, Clients),
			if Aux =:= undefined->
					io:format("No tengo este usuario ~s~n", [To]);
				true->
				    Aux ! {message_private, From, Text},
				    FromPID ! {message_private, From, Text}
			end,
			process_requests(Clients, Servers, Groups);


%% Messages between servers
		{request_list_users, From} ->
			io:format("--- List Of Clients ---~n"),
			broadcast_server(Servers, {send_list_of_users, From}),
			process_requests(Clients, Servers, Groups);

		{send_list_of_users, From} ->
			From ! {print_users, Clients},
			process_requests(Clients, Servers, Groups);

		{print_users, List_of_clients} ->
			print_name(List_of_clients),
			process_requests(Clients, Servers, Groups);

		print_groups ->
			print_groups(Groups),
			io:format("~n"),
			process_requests(Clients, Servers, Groups);

		remove_all_users ->
			remove_user(Clients),
			io:format("All Clients removed~n"),
			process_requests([], Servers, Groups);

		{remove_user_req, Client} ->
			broadcast_server(Servers, {remove_user, Client}),
			process_requests(Clients, Servers, Groups);
		{remove_user, Client} ->
			Aux = get_value(Client, Clients),
			if Aux =:= undefined->
					io:format("El usuario ~s no esta alojado en este servidor~n", [Client]);
				true->
				    Aux ! exit,
					io:format("Cliente Expulsado~n")
			end,
			
			process_requests(Clients, Servers, Groups);

		% Stop the server
		disconnect ->
			NewServers = lists:delete(self(), Servers),
			io:format("Redireccionant clients!~n"),
			broadcast(Clients, {change_server, NewServers}),
			broadcast_server(NewServers, {update_servers, NewServers}),
			io:format("Bye!~n"),
			unregister(myserver);
		% Server joins
		{server_join_req, From} ->
			io:format("New server joins!~n"),
			From ! {set_groups, Groups},
			NewServers = [From|Servers],
			broadcast_server(NewServers, {update_servers, NewServers}),
			process_requests(Clients, NewServers, Groups);
		% Server list update
		{update_servers, NewServers} ->
			io:format("Server list update! ~w~n", [NewServers]),
			process_requests(Clients, NewServers, Groups);
		% Server list update
		{set_groups, NewGroup} ->
			io:format("Group list update!~n"),
			process_requests(Clients, Servers, NewGroup);

		{client_change_name, OldName, NewName, From} ->
			AuxList = lists:delete({From, OldName}, Clients),
			NewClients = [{From, NewName} | AuxList],
			io:format("[~s]: ~s has changed the name to ~s~n", [ color:true("800080", "Server"), color:true("00FF00", OldName), color:true("00FF00", NewName)]),
			process_requests(Clients, NewClients, Servers);
			
		% Other messages are relayed to clients
		RelayMessage ->
			io:format("Relaying message...~n"),
			broadcast(Clients, RelayMessage),
			process_requests(Clients, Servers, Groups)

	end.

%% Local Functions
broadcast(PeerList, Message) ->
	Fun = fun(Peer) -> 
		X = element(2, Peer),
		X ! Message 
	end,
	lists:map(Fun, PeerList).

%% Local Functions
broadcast_server(PeerList, Message) ->
	Fun = fun(Peer) -> Peer ! Message end,
	lists:map(Fun, PeerList).


%% This is the main task logic
process_commands(ServerPid) ->
	%% Read from the standard input and send to server
	Text = io:get_line("-> "),
    if Text == "List_clients\n" ->
        ServerPid ! {request_list_users, ServerPid},
        process_commands(ServerPid);

       Text == "List_groups\n" ->
        ServerPid ! print_groups,
        process_commands(ServerPid);

       Text == "Remove_all_clients\n" ->
       	ServerPid ! remove_all_users,
       	process_commands(ServerPid);

       Text == "Remove_client\n" ->
       	Aux = io:get_line("Introduce el nombre del cliente-> "),
       	Name = re:replace(Aux, "\\s+", "", [global,{return,list}]),
       	ServerPid ! {remove_user, Name},
       	process_commands(ServerPid);

       	Text == "help\n" ->
       		io:format("Lista de comandas disponibles:~n"),
       		io:format("'List_clients' -> Devuelve una lista de todos los clientes distribuidos entre los diferentes servidores.~n"),
       		io:format("'List_groups' -> Devuelve una lista de todos los grupos creados.~n"),
       		io:format("'Remove_all_clients' -> Echa a todos los clientes alojados en el servidor.~n"),
       		io:format("'Remove_client' -> Petición a todos los servidores para eliminar a un cliente en especifico.~n"),
       		io:format("'exit' -> Redirecciona a todos los clientes a otro servidor disponible i desconecta el servidor actual.~n"),
        	process_commands(ServerPid);

        Text == "exit\n" ->
       		ServerPid ! disconnect;

       true ->
        io:format("Este comando no existe, Introduce 'help' para ver las opciones.~n"),
        process_commands(ServerPid)
    end.
	

print_name(Clients) ->
	P = fun(Client) ->
			Name = element(1, Client),
			io:format("~s~n", [Name])
		end,
	lists:map(P, Clients).

print_groups(Groups) ->
	P = fun(Grpup) ->
			Name = element(1, Grpup),
			Clients = element(2, Grpup),
			io:format("\n--- ~s ---  ~w clients ~n", [Name, length(Clients)]),
			print_name(Clients)
		end,
	lists:map(P, Groups).

remove_user(Clients) ->
	io:format("Removing clients...~n"),
	P = fun(Client) ->
			Direction = element(2, Client),
			Direction ! exit
		end,
	lists:map(P, Clients).