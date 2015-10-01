-module(server).
-export([loop/2, initial_state/1]).
-include_lib("./defs.hrl").

% Produce initial state
initial_state(ServerName) ->
    #server_st{name = ServerName, clients = []}.

%% ---------------------------------------------------------------------------

loop(St, Message) ->
    Clients  = St#server_st.clients,
    Channels = St#server_st.channels,
    case Message of
        {connect, Name} ->
            case lists:member(Name, Clients) of
                false -> {ok, St#server_st{clients=[Name|Clients]}};
                true  -> {user_already_connected, St}
            end;
        {disconnect, Name} ->
            case lists:member(Name, Clients) of
                false -> {user_not_connected, St};
                true  -> {ok, St#server_st{clients=lists:delete(Name, Clients)}}
            end
    end.