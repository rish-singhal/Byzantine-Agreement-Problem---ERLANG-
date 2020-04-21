-module('new').
-import(lists,[nth/2, map/2, seq/2, append/2]).
-import(string,[tokens/2]).
-export([each_process/8]).
-export([start/1, print/1, startphase/3, startphase1/3, startphase2/3]).

read_file(File) ->
    case file:read_line(File) of
        {ok, Data} ->
            [First|Rest] = string:tokens(Data,"\t\n"),
            [{First,string:join(Rest,"\t")} | read_file(File)];
        eof -> []
    end.

each_process(MyID, Alpha, Value, Weights, MyWeight, MyValue, S0, S1) ->
    Weight = nth(MyID, Weights),
    receive
        {print, SPID}->
            io:fwrite("Process: ~p, ~p, ~p, ~p, ~p, ~p, ~p ,~p ~n",[MyID, Alpha, Value, Weight, MyWeight, MyValue, S0, S1]),
             SPID ! {done},
            each_process(MyID, Alpha, Value, Weights, MyWeight, MyValue, S0, S1);
        
        {phaseinit, SPID} ->
            SPID ! {done},
            each_process(MyID, Alpha, Value, Weights, 0, MyValue, 0, 0);
        
        {phase1, PIDS, SPID} ->
            if Weight < 0 ->
                XValue = 0;
            true ->
                XValue = Value
            end,    
            map(
                    fun(K) ->
                            nth(K,PIDS) ! {phase1val, XValue, MyID},
                            io:fwrite("Sending ~p to ~p from ~p ~n", [XValue,K,MyID])
                    end,
            seq(1,length(PIDS))),
            SPID ! {done},
            each_process(MyID, Alpha, Value, Weights, MyWeight, MyValue, S0, S1);  
        
        {phase1val, RValue, RID } ->
            if Weight > 0 ->
                if RValue /= 0 ->
                    US0 = S0 + list_to_float(nth(RID, Weights)),
                    US1 = S1;
                true ->
                    US0 = S0,
                    US1 = S1 + list_to_float(nth(RID, Weights))
                end
            end,     
            if
                S1 > 0.5 ->
                    io:fwrite("Process: ~p, ~p, ~p, ~p, ~p, ~p, ~p ,~p ~n",[MyID, Alpha, Value, Weights, US1, 1, US0, US1]),
                    each_process(MyID, Alpha, Value, Weights, US1, 1, US0, US1);
                true->
                    io:fwrite("Process: ~p, ~p, ~p, ~p, ~p, ~p, ~p ,~p ~n",[MyID, Alpha, Value, Weights, US0, 0, US0, US1]),
                    each_process(MyID, Alpha, Value, Weights, US0, 0, US0, US1)
            end;

        {phase2, QID, PIDS, SPID} ->
            if QID == self() ->
                map(
                    fun(K) ->
                            nth(K,PIDS) ! {phase2val, MyValue},
                            io:fwrite("Queen Sending ~p to ~p from ~p ~n", [MyValue,K,MyID])
                    end,
                seq(1,length(PIDS)))
            end,
            SPID ! {done},
            each_process(MyID, Alpha, Value, Weights, MyWeight, MyValue, S0, S1);
        
        {phase2val, QueenVal} ->
            if MyWeight > 0.75 ->
                NV = MyValue;
            true ->
                NV = QueenVal
            end,
            io:fwrite("After Phase 2 Process: ~p, ~p, ~p, ~p, ~p, ~p, ~p ,~p ~n",[MyID, Alpha, NV, Weights, MyWeight, MyValue, S0, S1]),
            each_process(MyID, Alpha, NV, Weights, MyWeight, MyValue, S0, S1)
    end.


print([]) ->
    io:fwrite("~n");

print([H|P]) ->
    H ! {print, self()},
    receive 
        {done} ->
            print(P)
    end.   

startphase([] , PIDS, RID) ->
    io:fwrite("Done with initialization~n"),
    print(PIDS),
    startphase1(PIDS,PIDS,RID);

startphase([H|P] , PIDS, RID) ->
    H ! {phaseinit, self()},
    receive 
        {done} ->
            startphase(P, PIDS, RID)   
    end.

startphase1([] , PIDS, RID) ->
    io:fwrite("~nDone with phase1~n"),
    print(PIDS),
    startphase2(PIDS,PIDS, RID);


startphase1([H|P], PIDS, RID) ->
    H ! {phase1, PIDS, self()},
    receive 
        {done} ->
            startphase1(P, PIDS, RID)  
    end.

startphase2([], PIDS, RID) ->
     io:fwrite("~nDone with Phase 2~n"),
     print(PIDS),
     RID ! {done};

startphase2([H|P], PIDS, RID) ->
    H ! {phase2, H, PIDS, self()},
    receive 
        {done} ->
            startphase2(P, PIDS, RID)
    end.

startRounds( NumRound , ERound, PIDS) ->
    if NumRound > ERound ->
        io:fwrite("~nDone with All Rouds~n");
    true->
        io:fwrite("~nRound: ~p~n",[NumRound]),
        startphase(PIDS, PIDS, self()),
        receive 
            {done} ->
                startRounds(NumRound + 1 , ERound, PIDS)
        end

    end.

start(Token) ->
% taking input from file
    {ok, FileRead} = file:open(nth(1,Token),[read]),    
    Data = read_file(FileRead),

    {Num,[]}=nth(1,Data),
    Num_Proc = list_to_integer(nth(1,tokens(Num," "))),
   
    {Value,[]}=nth(2,Data),
    Values = tokens(Value," "),

    {Weight,[]}=nth(3,Data),
    Weights = tokens(Weight," "),

    {Faulty,[]}=nth(4,Data),
    FaultyProc = tokens(Faulty," "),
   
    Alpha = 1,

    io:fwrite("NumProc: ~p~n",[Num_Proc]),
    io:fwrite("Values: ~p~n",[Values]),
    io:fwrite("Weights: ~p~n",[Weights]),
    io:fwrite("FaultyProc: ~p~n",[FaultyProc]),

    PIDS = [spawn(?MODULE, each_process, [X, Alpha, list_to_integer(nth(X, Values)), Weights , -1,  -1, -1, -1]) || X  <- lists:seq(1, Num_Proc)],
    
    io:fwrite("Process PIDS: ~p ~n",[PIDS]),
    print(PIDS),
    startRounds(1,2,PIDS).
    %startphase1(Num_Proc, PIDS).
    % startphase2(Num_Proc, PIDS).
    %print(Num_Proc, PIDS),
  
   
