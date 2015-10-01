-module(server).
-export([loop/2, initial_state/1]).
-include_lib("./defs.hrl").

% Produce initial state
initial_state(ServerName) ->
    #server_st{
        users = [],
        name = ServerName
    }.

%% ---------------------------------------------------------------------------

loop(St, Message) ->
    Users = St#server_st.users,
    case Message of

        %% Request to change nick
        {nick, Nick} -> 
            case find_user(Nick, Users) of
                error -> {
                    ok, 
                    St#server_st{
                        users = lists:append([Nick],Users)
                    }
                }
            end;

        _            -> exit(not_implemented)
    end.



%% ---------------------------------------------------------------------------
%%  Helpers

find_user(Nick, [User | Users]) ->
    case User of
        Nick -> User;
        _    -> find_user(Nick, Users)
    end;

find_user(_ , []) ->
    error.