-module('new').
-import(lists,[nth/2, map/2, seq/2, append/2]).
-import(string,[tokens/2]).
-export([each_process/8,  read_file/1, calcaplpha/5]).
-export([start/1, print/1, startalgo/5]).

read_file(File) ->
    case file:read_line(File) of
        {ok, Data} ->
            [First|Rest] = string:tokens(Data,"\t\n"),
            [{First,string:join(Rest,"\t")} | read_file(File)];
        eof -> []
    end.

each_process(MyID, Value, Weights, MyWeight, MyValue, S0, S1, Fault) ->
    Weight = nth(MyID, Weights),
    receive
        {print, SPID}->
            io:fwrite("Process: ~p, ~p, ~p, ~p, ~p, ~p ,~p ~n",[MyID,  Value, Weight, MyWeight, MyValue, S0, S1]),
             SPID ! {done},
            each_process(MyID, Value, Weights, MyWeight, MyValue, S0, S1, Fault); 
        {phaseinit, SPID} ->
            SPID ! {done},
            each_process(MyID,  Value, Weights, 0, MyValue, 0, 0, Fault);
        
        {phase1, PIDS, SPID} ->   
            map(
                    fun(K) ->
                            if 
                                Weight < 0 ->
                                    XValue = 0;
                                true ->
                                    if 
                                        Fault /= 1 ->
                                            XValue = Value;
                                    true->  
                                            %XValue = Value
                                            XValue = rand:uniform(2) - 1
                                    end
                            end,  
                            nth(K,PIDS) ! {phase1val, XValue, MyID},
                            io:fwrite("Sending ~p to ~p from ~p ~n", [XValue,K,MyID])
                    end,
            seq(1,length(PIDS))),
            SPID ! {done},
            each_process(MyID, Value, Weights, MyWeight, MyValue, S0, S1, Fault);  
        
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
                    io:fwrite("Process: ~p, ~p, ~p, ~p, ~p, ~p ,~p ~n",[MyID, Value, Weights, US1, 1, US0, US1]),
                    each_process(MyID,  Value, Weights, US1, 1, US0, US1, Fault);
                true->
                    io:fwrite("Process: ~p, ~p, ~p, ~p, ~p, ~p ,~p ~n",[MyID, Value, Weights, US0, 0, US0, US1]),
                    each_process(MyID, Value, Weights, US0, 0, US0, US1, Fault)
            end;

        {phase2, QID, PIDS, SPID} ->
            if QID == self() ->
                io:fwrite("Queen ID: ~p~n",[MyID]),
                map(
                    fun(K) ->
                            if 
                                Fault /= 1 ->
                                        XValue = MyValue;
                                true->
                                        %XValue = Value
                                        XValue = rand:uniform(2) - 1
                            end,
                            nth(K,PIDS) ! {phase2val, XValue},
                            io:fwrite("Queen Sending ~p to ~p from ~p ~n", [XValue,K,MyID])
                    end,
                seq(1,length(PIDS)))
            end,
            SPID ! {done},
            each_process(MyID, Value, Weights, MyWeight, MyValue, S0, S1, Fault);
        
        {phase2val, QueenVal} ->
            if MyWeight > 0.75 ->
                NV = MyValue;
            true ->
                NV = QueenVal
            end,
            io:fwrite("After Phase 2 Process: ~p,  ~p, ~p, ~p, ~p, ~p ,~p ~n",[MyID, NV, Weights, MyWeight, MyValue, S0, S1]),
            each_process(MyID, NV, Weights, MyWeight, MyValue, S0, S1, Fault);
        {finalprint, SID} ->
            io:fwrite("~nFinal Vote: ~p~n", [Value]),
            SID!{doneprinting}
    end.


print([]) ->
    io:fwrite("~n");

print([H|P]) ->
    H ! {print, self()},
    receive 
        {done} ->
            print(P)
    end.   

startphase([] , PIDS, RID, QID) ->
    io:fwrite("Done with initialization~n"),
    print(PIDS),
    startphase1(PIDS,PIDS,RID, QID);

startphase([H|P] , PIDS, RID, QID) ->
    H ! {phaseinit, self()},
    receive 
        {done} ->
            startphase(P, PIDS, RID, QID)   
    end.

startphase1([] , PIDS, RID, QID) ->
    io:fwrite("~nDone with phase1~n"),
    print(PIDS),
 
    startphase2(PIDS,RID, QID);


startphase1([H|P], PIDS, RID, QID) ->
    H ! {phase1, PIDS, self()},
    receive 
        {done} ->
            startphase1(P, PIDS, RID, QID)  
    end.


startphase2(PIDS, RID, QID) ->
    QID ! {phase2, QID, PIDS, self()},
    receive 
        {done} ->
           io:fwrite("~nDone with Phase 2~n"),
            print(PIDS),
            RID ! {done}
    end.

startRounds(NumRound , ERound, PIDS) ->
    if NumRound > ERound ->
        nth(1,PIDS) ! {finalprint, self()},
        receive 
            {doneprinting} ->
             io:fwrite("~nDone with All Rouds~n")
        end;
    true->
        io:fwrite("~nRound: ~p~n",[NumRound]),
        startphase(PIDS, PIDS, self(), nth(NumRound,PIDS)),
        receive 
            {done} ->
                startRounds(NumRound + 1 , ERound, PIDS)
        end

    end.

calcaplpha([C|W],Count, NSum, Sum, PIDS) ->
    NewSum = NSum + list_to_float(C),
    if 
        NewSum > Sum ->
            io:fwrite("Value of Alpha: ~p~n",[Count+1]),
            startRounds( 1 , Count + 1, PIDS);
        true ->
            calcaplpha(W,Count + 1, NewSum, Sum, PIDS)
    end.

startalgo([],[],PIDS, Sum, Weights) ->
    io:fwrite("Sum of weights of FaultyProc: ~p ~n",[Sum]),
    calcaplpha(Weights,0, 0,Sum, PIDS);


startalgo([H|P],[C|F],PIDS, Sum, Weights) ->
    if 
        C /= "0" ->
            Val = Sum + list_to_float(H);
        true ->
            Val = Sum
    end,
    startalgo(P,F,PIDS,Val,Weights).



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
    io:fwrite("NumProc: ~p~n",[Num_Proc]),
    io:fwrite("Values: ~p~n",[Values]),
    io:fwrite("Weights: ~p~n",[Weights]),
    io:fwrite("FaultyProc: ~p~n",[FaultyProc]),
    PIDS = [spawn(?MODULE, each_process, [X, list_to_integer(nth(X, Values)), Weights , 0,  0, 0, 0, list_to_integer(nth(X,FaultyProc))]) || X  <- lists:seq(1, Num_Proc)],
    io:fwrite("Process PIDS: ~p ~n",[PIDS]),
    print(PIDS),
    startalgo(Weights, FaultyProc, PIDS,0,Weights).
  
   
