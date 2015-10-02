-module(server).
-export([loop/2, initial_state/1]).
-include_lib("./defs.hrl").

% Produce initial state
initial_state(ServerName) ->
    #server_st{
        name = ServerName
    }.

%% ---------------------------------------------------------------------------

loop(St, Message) ->
    Clients  = St#server_st.clients,
    Channels = St#server_st.channels,
    io:format("Got message: ~p~n",[Message]),
    case Message of
        
        %% Connect, no shit?
        {connect, Name, Pid} ->
            User = find_user_by_name(Name,Clients),
            case User of
                error -> {ok, St#server_st{clients=[#user{name=Name,pid=Pid}|Clients]}};
                _  -> 
                    case User#user.pid of
                        Pid -> {user_already_connected, St};
                        _   -> {name_taken, St}
                    end
            end;
        
        %% Disconnect, duh
        {disconnect, Name} ->
            case find_user_by_name(Name, Clients) of
                error -> {user_not_connected, St};
                User  -> 
                    NewClients = lists:delete(User, Clients),
                    io:format("Users left: ~p~n",[NewClients]),
                    {ok, St#server_st{clients=NewClients}}
            end;

        %% Request to change name
        {nick, Name, Pid} -> 
            case user_exists(Name, Clients) of
                false -> 
                    User = find_user_by_pid(Pid, Clients),
                    NewClients = lists:delete(User,Clients),
                    {ok, St#server_st{
                        clients = [ User#user{name = Name} | NewClients]
                    }};
                true -> {{"EXIT","Name is already taken"},St}
            end;

        %% Returns the name onnected to the pid sending
        {whoami, Pid} ->
            case find_user_by_pid(Pid, Clients) of
                error -> exit(user_not_found);
                User  -> {{nick, User#user.name },St}
            end;
        _            -> {not_implemented,St}
    end.



%% ---------------------------------------------------------------------------
%%  Helpers

find_user_by_name(Name, [User | Users]) ->
    case User#user.name of
        Name -> User;
        _    -> find_user_by_name(Name, Users)
    end;

find_user_by_name(_ , []) -> error.

user_exists(Name, [User | Users]) ->
    case User#user.name of
        Name -> true;
        _    -> user_exists(Name, Users)
    end;

user_exists(_ , []) ->
    false.


find_user_by_pid(Pid, [User | Users]) ->
    io:format("Finding ~p among ~p~n",[Pid,[User | Users]]),
    case User#user.pid of
        Pid -> User;
        _   -> find_user_by_pid(Pid, Users)
    end;

find_user_by_pid(_, []) ->
    error.