-module(client).
-export([loop/2, initial_state/2]).
-include_lib("./defs.hrl").

%% Produce initial state
initial_state(Nick, GUIName) ->
    #client_st { gui = GUIName, name = Nick }.

%% ---------------------------------------------------------------------------

%% loop handles each kind of request from GUI

%% Connect to server
loop(St, {connect, Server}) ->
    case genserver:request(list_to_atom(Server), {connect, St#client_st.name}) of
        ok -> {ok, St#client_st{server=list_to_atom(Server)}};
        user_already_connected -> {{error, user_already_connected, "You are already connected."}, St};
        server_not_reached -> {{error, server_not_reached, "Server couldn't be reached."}, St}
    end;

%% Disconnect from server
loop(St, disconnect) -> 
    case St#client_st.server of
        not_connected   -> {{error,user_not_connected,"Not connected to a server"},St};
        Server          -> 
            case genserver:request(Server, {disconnect, St#client_st.name}) of
                ok -> {ok, St#client_st{server=not_connected}};
                user_not_connected -> {{error, user_not_connected, "You are not connected."}, St};
                leave_channels_first -> {{error, leave_channels_first, "Leave channels first."}, St};
                server_not_reached -> {{error, server_not_reached, "Server couldn't be reached."}, St}
            end
    end;

% Join channel
loop(St, {join, Channel}) ->
    case genserver:request(St#client_st.server, {join, St#client_st.name}) of
        ok -> {ok, St#client_st{channel=Channel}};
        user_already_joined -> {error, user_already_joined, "User already joined."}
    end;

%% Leave channel
loop(St, {leave, Channel}) ->
    case genserver:request(St#client_st.server, {leave, St#client_st.name}) of
        ok -> {ok, St#client_st{channel=undefined}};
        user_not_joined -> {error, user_not_joined, "User havn't joined channel."}
    end;

% Sending messages
loop(St, {msg_from_GUI, Channel, Msg}) ->
    case St#client_st.server of
        not_connected   -> {{error,user_not_connected,"Not connected to a server"},St};
        Server          -> 
            case catch(gen_server:request(Server, {send_message, St#client_st.name, Channel, Msg})) of
                {"EXIT","Timeout"} -> {{error,timeout,"Request timed out"}, St};
                {"EXIT",Reason}    -> {{error,error,Reason}, St};
                ok                 -> {ok,St}
            end
    end;
    % {ok, St} ;
    % {{error, not_implemented, "Not implemented"}, St} ;

%% Get current nick
loop(St, whoami) ->
    case St#client_st.server of
        not_connected   -> {{error,user_not_connected,"Not connected to a server"},St};
        Server          -> 
            case catch(gen_server:request(Server, whoami)) of
                {"EXIT","Timeout"}  -> {{error,timeout,"Request timed out"}, St};
                {"EXIT",Reason}     -> {{error,error,Reason}, St};
                {nick,Nick}         -> {Nick,St}
            end
    end;

            

    % {"nick", St} ;
    % {{error, not_implemented, "Not implemented"}, St} ;

%% Change nick
loop(St, {nick, Nick}) ->
    case St#client_st.server of
        not_connected   -> {{error,user_not_connected,"Not connected to a server"},St};
        Server          -> 
            case catch(gen_server:request(Server, {nick, Nick})) of
                {"EXIT","Timeout"}  -> {{error,timeout,"Request timed out"}, St};
                {"EXIT",Reason}     -> {{error,error,Reason}, St};
                {nick,Nick}         -> {ok,St#client_st{name=Nick}}
            end
    end;
    % {ok, St} ;
    % {{error, not_implemented, "Not implemented"}, St} ;

%% Incoming message
loop(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {ok, St}.
