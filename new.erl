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
            io:fwrite("Process: ~p, ~p, ~p, ~p, ~p, ~p ,~p ~n",[MyID,  Value, list_to_float(Weight), MyWeight, MyValue, S0, S1]),
             SPID ! {done},
            each_process(MyID, Value, Weights, MyWeight, MyValue, S0, S1, Fault); 
        {phaseinit, SPID} ->
            % to initillize MyWeight, S0, S1 to 0
            SPID ! {done},                  %.  -           -  - 
            each_process(MyID,  Value, Weights,  MyWeight, MyValue, 0, 0, Fault);
        
        {phase1, PIDS, SPID} ->   
            map(
                    fun(K) ->
                            %this is phase1
                            % if current weight is > 0 , it sends value to other processes
                            % as its own Value (or Random Value if it is faulty)
                            % otherwise it sends 0
                            if 
                                Weight > 0 ->
                                    if 
                                        Fault /= 1 ->
                                            XValue = Value;
                                        true->  
                                            %XValue = Value
                                            XValue = rand:uniform(2) - 1
                                        end,
                                    io:fwrite("Sending ~p to ~p from ~p ~n", [XValue,K,MyID]) ;
                                true ->
                                     XValue = 0
                            end,  
                            nth(K,PIDS) ! {phase1val, XValue, MyID}
                    end,
            seq(1,length(PIDS))),
            io:fwrite("~n"),
            SPID ! {done},
            each_process(MyID, Value, Weights, MyWeight, MyValue, S0, S1, Fault);  
        
        {phase1val, RValue, RID } ->
        % here values are recieved in phase 1, and according to it
        % S0, S1 values are updated
            if Weight > 0 ->
                if RValue /= 1 ->
                    US0 = S0 + list_to_float(nth(RID, Weights)),
                    US1 = S1;
                true ->
                    US0 = S0,
                    US1 = S1 + list_to_float(nth(RID, Weights))
                end
            end,     
            if
            % and if S1 is > 0.5, myvalue is set 1 
                US1 > 0.5 ->
                  %  io:fwrite("Process: ~p, ~p, ~p, ~p, ~p, ~p ,~p ~n",[MyID, Value, Weights, US1, 1, US0, US1]),
                    each_process(MyID,  Value, Weights, US1, 1, US0, US1, Fault);
                true->
            % otherwise 0
                   % io:fwrite("Process: ~p, ~p, ~p, ~p, ~p, ~p ,~p ~n",[MyID, Value, Weights, US0, 0, US0, US1]),
                    each_process(MyID, Value, Weights, US0, 0, US0, US1, Fault)
            end;

        {phase2, QID, PIDS, SPID} ->
            if QID == self() ->
                % if queenid is self [ or this process is the queen ]
                io:fwrite("Queen ID: ~p~n",[MyID]),
                map(
                    fun(K) ->
                            if 
                                Fault /= 1 ->
                                        XValue = MyValue;
                                true->
                                        XValue = rand:uniform(2) - 1
                            end,
                            nth(K,PIDS) ! {phase2val, XValue},
                            % it sends it's own value ( or random value ( if it not faulty ))
                            % to all other processes
                            io:fwrite("Queen Sending ~p to ~p from ~p ~n", [XValue,K,MyID])
                    end,
                seq(1,length(PIDS)))
            end,
            % after it's done, it sends done to phase2 function
            SPID ! {done},
            each_process(MyID, Value, Weights, MyWeight, MyValue, S0, S1, Fault);
        
        {phase2val, QueenVal} ->
            % and here all processes updates it's myweight and Values (V) according 
            % to MyWeight is > 0.75 or not
            if MyWeight > 0.75 ->
                NV = MyValue;
            true ->
                NV = QueenVal
            end,
            io:fwrite("After Phase 2 Process: ~p,  ~p, ~p, ~p, ~p, ~p ,~p ~n",[MyID, NV, Weights, MyWeight, MyValue, S0, S1]),
            each_process(MyID, NV, Weights, MyWeight, MyValue, S0, S1, Fault);
        
        {finalprint, SID} ->
        % to print the final vote
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


% when all processes are initiallized
startphase([] , PIDS, RID, QID) ->
    io:fwrite("Done with initialization~n"),
    print(PIDS),

    % first phase is initiated
    startphase1(PIDS,PIDS,RID, QID);

% to initiate phases
startphase([H|P] , PIDS, RID, QID) ->
    % initially the values of S0, S1 is set to 0.
    H ! {phaseinit, self()},
    receive 
        {done} ->
        % after current processes is done with initiallization,
        % it sends a message {done}
        % and the code proceeds with initiallizing next process
        startphase(P, PIDS, RID, QID)   
    end.


% when all processes are done with phase-1 part
startphase1([] , PIDS, RID, QID) ->
    io:fwrite("~nDone with phase1~n"),
    print(PIDS),
    % phase2 is initiallized
    startphase2(PIDS,RID, QID);


% function for phase1
startphase1([H|P], PIDS, RID, QID) ->
    % sends {phase1} message and pids to current process
    % to send it's own value to other processes if it's weight > 0
    H ! {phase1, PIDS, self()},
    receive 
        {done} ->
             % after current processes is done with sending values, 
             % it sends a message {done}
             % and the code proceeds with next process
            startphase1(P, PIDS, RID, QID)  
    end.

% this is the 2nd phase
startphase2(PIDS, RID, QID) ->
    % queen is send message {phase2} to initiate sending it's queenvalue
    % to all other processes
    QID ! {phase2, QID, PIDS, self()},
    receive 
        {done} ->
            % after queen is done with sending values, phase 2 ends
           io:fwrite("~nDone with Phase 2~n"),
            print(PIDS),
            % and sends the message {done} to startRound function to
            % move forward with next rounds...
            RID ! {done}
    end.


% function for initiating each round
% Eround == ALPHA value
startRounds(NumRound , ERound, PIDS) ->
    if NumRound > ERound ->
        % if the current round number > alpha, it's not executed,
        %           and final vote is printed "which all processes are agreeing on"
        nth(1,PIDS) ! {finalprint, self()},
        receive 
            {doneprinting} ->
             io:fwrite("~nDone with All Rouds~n")
        end;
    true->
        % till current round is less than alpha
        io:fwrite("~nRound: ~p~n",[NumRound]),
        % both the 2 phases are initiated, with QueenID as Current Round Number
            %.                                          == NumRound
        startphase(PIDS, PIDS, self(), nth(NumRound,PIDS)),
        % after both phases ends, 2nd round is called
        receive 
            {done} ->
                startRounds(NumRound + 1 , ERound, PIDS)
        end

    end.

% this is to calculate alpha value "which is stored in Count"
% α is defined as the least number of processes whose total weight exceeds " total weight of faulty 
%    processes 
calcaplpha([C|W],Count, NSum, Sum, PIDS) ->
    NewSum = NSum + list_to_float(C),
    if 
        NewSum > Sum ->
        % alpha value will be previous_count + 1 
            io:fwrite("Value of Alpha: ~p~n",[Count+1]),
        % starts round 1....(Count + 1)
            startRounds( 1 , Count + 1, PIDS);
        true ->
        % count is incremented as NewSum < Sum 
            calcaplpha(W,Count + 1, NewSum, Sum, PIDS)
    end.


% after sum is calcuated calcalpha is called to calculate alpha & initiate alpha rounds
startalgo([],[],PIDS, Sum, Weights) ->
    io:fwrite("Sum of weights of FaultyProc: ~p ~n",[Sum]),
    calcaplpha(Weights,0, 0,Sum, PIDS);


%function to calculate sum of weights for faulty processes
startalgo([H|P],[C|F],PIDS, Sum, Weights) ->
    if 
        C /= "0" ->
        % if Process is Faulty, Sum value is updated
            Val = Sum + list_to_float(H);
        true ->
            Val = Sum
    end,
    startalgo(P,F,PIDS,Val,Weights).



start(Token) ->
% taking input from file
    {ok, FileRead} = file:open(nth(1,Token),[read]),    
    Data = read_file(FileRead),

% number of processes
    {Num,[]}=nth(1,Data),
    Num_Proc = list_to_integer(nth(1,tokens(Num," "))),
  
% values of processes ( or INITIAL VOTES ) 
    {Value,[]}=nth(2,Data),
    Values = tokens(Value," "),

% weights of the processes
    {Weight,[]}=nth(3,Data),
    Weights = tokens(Weight," "),

% Faulty Process or not " according to value 1 or 0"
    {Faulty,[]}=nth(4,Data),
    FaultyProc = tokens(Faulty," "),

    io:fwrite("NumProc: ~p~n",[Num_Proc]),
    io:fwrite("Values: ~p~n",[Values]),
    io:fwrite("Weights: ~p~n",[Weights]),
    io:fwrite("FaultyProc: ~p~n",[FaultyProc]),

% schema: each_process(MyID, Value, Weights, MyWeight, MyValue, S0, S1, Fault) 
% N processes are spawned according to input
    PIDS = [spawn(?MODULE, each_process, [X, list_to_integer(nth(X, Values)), Weights , 0,  0, 0, 0, list_to_integer(nth(X,FaultyProc))]) || X  <- lists:seq(1, Num_Proc)],
    io:fwrite("Process PIDS: ~p ~n",[PIDS]),
    io:fwrite("~nFormat: MyID, Value, Weights, MyWeight, MyValue, S0, S1~n"),
    print(PIDS),

% to calculate total sum of weights for faulty processes, and then alpha
    startalgo(Weights, FaultyProc, PIDS,0,Weights).
  
   
