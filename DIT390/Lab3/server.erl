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
    case Message of
        
        %% Connect, no shit?
        {connect, Name} ->
            case lists:member(Name, Clients) of
                false -> {ok, St#server_st{clients=[Name|Clients]}};
                true  -> {user_already_connected, St}
            end;
        
        %% Disconnect, duh
        {disconnect, Name} ->
            case lists:member(Name, Clients) of
                false -> {user_not_connected, St};
                true  -> {ok, St#server_st{clients=lists:delete(Name, Clients)}}
            end;

        %% Request to change name
        {nick, Name} -> 
            case find_user(Name, Clients) of
                error -> {
                    ok, 
                    St#server_st{
                        clients = lists:append([Name],Clients)
                    }
                }
            end;

        _            -> exit(not_implemented)
    end.



%% ---------------------------------------------------------------------------
%%  Helpers

find_user(Name, [User | Users]) ->
    case User of
        Name -> User;
        _    -> find_user(Name, Users)
    end;

find_user(_ , []) ->
    error.