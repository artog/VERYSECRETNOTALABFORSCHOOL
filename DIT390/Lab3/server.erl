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
            case lists:keymember(Name, 3, Clients) of
                false -> {user_not_connected, St};
                true  -> 
                    User = find_user_by_name(Name, Clients),
                    Pid = User#user.pid,
                    case lists:any(fun(X) -> X =:= true end, lists:map(fun(C) -> lists:member(Pid, C#channel.clients) end, Channels)) of
                        false -> {ok, St#server_st{clients=lists:keydelete(Name, 3, Clients)}};
                        true  -> {leave_channels_first, St}
                    end
            end;

        {join, Channel, Client} ->
            case lists:keyfind(Channel, 2, Channels) of
                false -> 
                    User = find_user_by_pid(Client, Clients),
                    io:format("New channel ~p with user ~p:~p~n",[Channel,Client, User]),
                    Pid = genserver:start(
                        list_to_atom(Channel), 
                        #channel{
                            name=Channel,
                            clients=[User#user.pid]
                        }, 
                        fun channel_loop/2
                    ),
                    {{ok,Pid}, St#server_st{
                        channels=[#channel{
                            name=Channel, 
                            clients=[Client], 
                            pid = Pid
                        } | Channels]
                    }};
                C     -> 
                    case lists:member(Client, C#channel.clients) of
                        false ->
                            NewClients =  [Client|C#channel.clients],
                            io:format("~p now have clients ~p~n",[C#channel.name, NewClients]),
                            Ref = make_ref(),
                            C#channel.pid ! {request, self(), Ref, {update_clients, NewClients}},
                            {{ok,C#channel.pid}, St#server_st{channels=lists:keyreplace(Channel, 2, Channels, C#channel{clients=NewClients})}};
                        true  -> {user_already_joined, St}
                    end
            end;

        {leave, Channel, Client} ->
            case lists:keyfind(Channel, 2, Channels) of
                false -> {user_not_joined, St};
                C     ->
                    case lists:member(Client, C#channel.clients) of
                        false -> {user_not_joined, St};
                        true  -> 
                            NewClients =  lists:delete(Client, C#channel.clients),
                            Ref = make_ref(),
                            C#channel.pid ! {request, self(), Ref, {update_clients, NewClients}},
                            
                            {ok, St#server_st{channels=lists:keyreplace(Channel, 2, Channels, C#channel{clients=NewClients})}}
                    end
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
                true -> {nick_taken,St}
            end;

        %% Returns the name onnected to the pid sending
        {whoami, Pid} ->
            case find_user_by_pid(Pid, Clients) of
                error -> exit(user_not_found);
                User  -> {{nick, User#user.name },St}
            end;


        {send_message, SenderPid, Channel, Msg} ->
            Sender = find_user_by_pid(SenderPid, Clients),
            SenderName = Sender#user.name,
            case lists:keyfind(Channel, 2, Channels) of
                false -> {unkown_channel, St};
                C     ->
                    case lists:member(SenderPid, C#channel.clients) of
                        false -> {user_not_joined, St};
                        true  -> 
                            ChanRecord = lists:keyfind(Channel,2, Channels),
                            Pids = ChanRecord#channel.clients,
                            lists:foreach(fun(Pid) -> 
                                % User = find_user_by_pid(Pid, Clients),
                                case Pid of 
                                    SenderPid -> true;
                                    _          -> send_message(Pid, Channel, SenderName, Msg)
                                end
                            end, Pids),
                            {ok,St}
                    end
            end;
        _            -> {not_implemented,St}
    end.

%% ---------------------------------------------------------------------------
%% Channel loop

channel_loop(St, {send_message, Sender, Msg}) ->
    Channel = St#channel.name, 
    io:format("~p sends ~p to ~p~n",[Sender#user.pid,Msg,St#channel.clients]),
    lists:foreach(fun(Pid) -> 
        case Sender#user.pid of 
            Pid -> true;
            _               -> send_message(Pid, Channel, Sender#user.name, Msg)
        end
    end, St#channel.clients),
    {ok,St};

channel_loop(St, {update_clients, NewList}) ->
    io:format("Got new client list:  ~p~n",[NewList]),
    {ok, St#channel{clients=NewList}}.

%% ---------------------------------------------------------------------------
%%  Helpers

send_message(Pid, Channel, Name, Msg) ->
    Data = {incoming_msg, Channel, Name, Msg},
    Ref = make_ref(),  
    Pid ! {request, self(), Ref, Data}.


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
    case User#user.pid of
        Pid -> User;
        _   -> find_user_by_pid(Pid, Users)
    end;

find_user_by_pid(_, []) ->
    error.