-module('DS_Project').
-import(lists,[nth/2, map/2, seq/2, append/2]).
-import(string,[tokens/2]).
-export([start/1]).

read_file(File) ->
    case file:read_line(File) of
        {ok, Data} ->
            [First|Rest] = string:tokens(Data,"\t\n"),
            [{First,string:join(Rest,"\t")} | read_file(File)];
        eof -> []
    end.

each_process(Index, Alpha, Weights, PIDS, Value) ->
    map(
        fun(T)->
            Queen_ID = T,
            Sum0 = 0,
            Sum1 = 0,
            KWValue = nth(Index,Weights),

            if KWValue > 0 ->
                    map(
                        fun(K) ->
                            if K /= Index ->
                                    nth(K,PIDS) ! {Value},
                                    io:fwrite("Sending ~p to ~p from ~p ~n", [Value,K, Index]);
                                true ->
                                    K
                            end
                        end,
                    seq(1,length(PIDS)));
                true ->
                    T
            end,
            Sum = map(
                fun(K) ->
                    io:fwrite("Process ~p~n",[Index]),
                    Temp_KWValue = nth(K,Weights),
                    if KWValue > 0 andalso K /= Index->
                            io:fwrite("Process ~p receiving from Process ~p~n",[Index, K]),
                            receive
                                {Value} ->
                                    io:fwrite("Process ~p received Value received ~p from ~p~n",[Index, Value, K]),
                                    if Value == 1 ->
                                            {0, nth(K,Weights)};
                                        true ->
                                            {nth(K,Weights), 0}
                                    end
                            end;
                        true ->
                            {Sum0, Sum1}
                    end
                end,
            seq(1,length(PIDS))),
            io:fwrite("Process ~p ~p~n",[Index, Sum])
        end,
    seq(1,Alpha)).

start(Token) ->
    {ok, FileRead} = file:open(nth(1,Token),[read]),
%    {ok, FileWrite} = file:open(nth(2,Token), [write]),
    
    Data = read_file(FileRead),
    {Num,[]}=nth(1,Data),
    Num_Proc = nth(1,tokens(Num," ")),
    {Value,[]}=nth(2,Data),
    Values = tokens(Value," "),
    {Weight,[]}=nth(2,Data),
    Weights = tokens(Weight," "),
    {Faulty,[]}=nth(3,Data),
    FaultyProc = tokens(Faulty," "),
    Alpha = 1,

    Rest_PIDS = map(
        fun(T) -> 
            Pid = spawn(fun() -> 
                        receive
                            {PIDS} ->
                                each_process(T, Alpha, Weights, PIDS, nth(T,Values))                            
                        end
                    end),
            Pid
        end,
        seq(1,list_to_integer(Num_Proc))),
    PIDS = append([], Rest_PIDS),
    io:fwrite("~p ~n",[PIDS]),
    map(
        fun(T) ->
            nth(T,PIDS) ! {PIDS}
        end,
    seq(2,list_to_integer(Num_Proc))),
    each_process(1, Alpha, Weights, PIDS, nth(1,Values)).

    % map(
    %    fun(N) ->
    %        {Da,[]} = nth(N,Data),
    %        Value = tokens(Da," "),
    %        Temp = [lists:sublist(Value, X, length(Value) div Num + 1) || X <- lists:seq(1,length(Value),length(Value) div Num + 1)],
    %        MID = self(),
        %    map(
        %     fun(T) -> 
        %         Pid = spawn(fun() -> 
        %                 io:fwrite("Hello world!~n",[])
        %                 end),
        %         Pid
        %     end,
    %         seq(1,length(Temp))),
    %         FinalArray = receivedarray([], length(Value)),
    %         print_array(FinalArray, FileWrite)
    %    end,
    % seq(1,length(Data))).
