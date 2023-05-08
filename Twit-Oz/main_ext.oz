functor
import 
    QTk at 'x-oz://system/wp/QTk.ozf'
    System
    Application
    Open
    OS
    Property
    Browser
    Tk
    TkTools
define
    InputText 
    OutputText
    DatabaseList
    InfoGram
    TitleGram  
    LenOut
    TitleLenOut
    %%% Pour ouvrir les fichiers
    class TextFile
        from Open.file Open.text
    end

    proc {Browse Buf}
        {Browser.browse Buf}
    end

    NbThreads = 500

    %%%
    %%% Aggregates a record with another
    %%%      Struct:    New record
    %%%      Arity:     Remaining to read record keys 
    %%%      OldStruct: Old structure to aggregate with
    %%% 
    %%% Returns an aggregation of Struct and OldStruct
    %%%

    fun {UpdateOutputTree Struct Arity OldStruct}
        case Arity of 
        nil then OldStruct
        [] Prediction|T then 
            if Prediction == '' then {UpdateOutputTree Struct T OldStruct} 
            else
                local Value Val PredictionTree NewTree in 
                Value = {CondSelect OldStruct Prediction 0}
                Val = Struct.Prediction
                PredictionTree = {MakeRecord tree [Prediction]}
                PredictionTree.Prediction = Value + Val
                {UpdateOutputTree Struct T {Adjoin OldStruct PredictionTree}}
                end
            end
        end
   end 


    %%%
    %%% Reads a stream until the nil value is reached, indicating the stream termination. 
    %%% For each value in the stream, aggregate with the previously read ones
    %%%      Tree: Aggregated structure
    %%%
    %%% Returns an aggregation of all element found in stream
    %%%
    
    fun {ReadStream Stream Tree}
        case Stream of 
        nil then Tree
        [] H|T then 
            if H == nil then Tree 
            else
                {System.show H}
                {ReadStream T {UpdateOutputTree H {Arity H} Tree}}
            end
        end
    end


    %%%
    %%% Function called when the prediction task is launched
    %%%

    fun {Press}
        local PredictionTree TempPredictionTree BestPrediction SeparatedWordsStream SeparatedWordsPort Return ATree NPredictionsTree NPredictionsArray SortedArrayKeyPair SortedArity in 
            {InputText set(state:disabled)}
            {OutputText set(state:normal)}
            {OutputText set("Loading... Please wait")}
            {OutputText set(state:disabled)}

            SeparatedWordsPort = {NewPort SeparatedWordsStream}
            
            {LaunchThreads SeparatedWordsPort NbThreads}
            TempPredictionTree = prediction()
            PredictionTree = {ReadStream SeparatedWordsStream TempPredictionTree}
            ATree = prediction()
            SortedArity = {List.take {List.sort {Arity PredictionTree} fun{$ A B} PredictionTree.A > PredictionTree.B end} {String.toInt {LenOut get(1:$)}}}
            NPredictionsArray = {List.map SortedArity fun{$ A} {VirtualString.toAtom A#":"#{Int.toFloat (PredictionTree.A)}#"|"} end}

            if {Length NPredictionsArray} == 0 then Return = "Not Found" 
            else 
                Return = [NPredictionsArray] 
            end
            {OutputText set(state:normal)}
            {OutputText set(Return)}
            {OutputText set(state:disabled)}
            {InputText set(state:normal)}
            {System.show PredictionTree}
            {List.toRecord prediction {List.map SortedArity fun {$ K} K#(PredictionTree.K) end}}
        end
   end


    %%%
    %%% Lowercases a word
    %%%      Word: Word to lowercase
    %%%
    %%% Returns lowercased word
    %%%

    fun {Lower Word}
        {List.map Word Char.toLower}
    end

    %%%
    %%% Parses a line and retrieves the word after the entered text if the latter is in the line
    %%%      Line:           Line to parse
    %%%      InputTextSplit: Input text to look for
    %%%      Found:          Whether a start of match has been found, used to determine if the function found a prediction candidate or not
    %%%                      at the end of input text parsing
    %%%
    %%% Returns a prediction candidate if found, nil otherwise
    %%%

    fun {ParseLine Line InputTextSplit}
        fun {ParseLineA Line InputTextSplit InitialLength CurrentLength InitialLine}
            case Line#InputTextSplit 
            of nil#nil then nil
            [] (A|B)#(C|D) then if A == C then {ParseLineA B D InitialLength CurrentLength+1 InitialLine} else {ParseLineA B InputTextSplit InitialLength 0 InitialLine} end
            [] (H|T)#nil then 
                    if H == '' then nil
                    elseif CurrentLength == InitialLength then
                            {System.show {VirtualString.toAtom "found "#H}}{System.show {List.map InitialLine String.toAtom}}    
                        H
                    else nil 
                    end
            [] nil#(H|T) then nil
            else nil
            end
        end
    in
        {ParseLineA Line InputTextSplit {Length InputTextSplit} 0 Line}
    end


    %%%
    %%% Computes an updated prediction adding an occurrency for given prediction
    %%%      Struct:     Old prediction structure
    %%%      Prediction: Prediction to add occurrency for
    %%%
    %%% Returns updated prediction tree
    %%%

    fun {UpdatePredictionTree Struct Prediction}
        if {String.toAtom Prediction} == '' then Struct 
        else
            local Value PredictionTree in 
                Value = {CondSelect Struct {String.toAtom Prediction} 0}
                PredictionTree = {MakeRecord tree [{String.toAtom Prediction}]}
                PredictionTree.{String.toAtom Prediction} = Value + 1
                {Adjoin Struct PredictionTree}
            end
        end
    end 
    

    %%%
    %%% Check in the List if each character is a letter of an number if else replace by a space
    %%%      List: List of the character to check if it's acceptable
    %%%      ResList: Accumulator that contains the filtered list
    %%%
    %%% Returns ResList when List is nil
    %%% 

    fun{SpecialToSpace List ResList}
        case List of
        nil then ResList
        [] H|T then 
            if H < 48 then {SpecialToSpace T 32|ResList}
            elseif H > 122 then {SpecialToSpace T 32|ResList}
            else 
                if H > 96 then {SpecialToSpace T H|ResList}
                elseif H < 58 then {SpecialToSpace T H|ResList}
                else 
                {SpecialToSpace T 32|ResList}
                end
            end
        end
    end


    %%%
    %%% Take a String and replace non letter and number from
    %%%      Str: String to repace non letter and number from
    %%%
    %%% Returns updated String
    %%%

    fun {StripPonctuation Str}
        local Res in
            Res = nil
            {List.reverse {SpecialToSpace Str Res}}
        end
    end


    %%%
    %%% Parses a file a computes a record of prediction mapped with frequency after input in the given file
    %%%      File:           File to parse
    %%%      Line:           Currently parsed line
    %%%      Struct:         Up to now computed prediction record for the file
    %%%      InputTextSplit: Array of words from user input
    %%% Returns computed prediction record
    %%%

    fun {ParseFile File Line Struct InputTextSplit} 
        local AtEnd ReadLine Prediction NewTree in 
            Prediction = {ParseLine {String.tokens {StripPonctuation {Lower Line}} & } InputTextSplit}
            NewTree = {UpdatePredictionTree Struct Prediction}
            {File atEnd(AtEnd)}
            if AtEnd then NewTree
            else 
                {File getS(ReadLine)} 
                {ParseFile File ReadLine NewTree InputTextSplit}
            end
        end
    end

    %%%
    %%% Computes a record mapping a prediction with its frequency after input in the given files
    %%%      Files:          Array of files to read
    %%%      StartIndex:     Index of the first file to read (included)
    %%%      EndIndex:       Index of the last file to read  (excluded)
    %%%      CurrentIndex:   Currently read file index
    %%%      Struct:         Up to now computed prediction record
    %%%      InputTextSplit: Array of words from user input
    %%%
    %%% Returns computed prediction record
    %%%

    fun {LaunchTask Files Struct InputTextSplit}
        case Files of nil then Struct
        [] H|T then
            local Path Output File Line in 
                Path = {VirtualString.toAtom {GetSentenceFolder}#"/"#H}
                File = {New TextFile init(name:Path flags:[read])}
                {File getS(Line)}
                Output = {ParseFile File Line Struct InputTextSplit}
                {LaunchTask T Output InputTextSplit}
            end
        end
   end

    %%%
    %%% Reduces amount of words in the input to NGram 
    %%%   InputTextSplit: Arrays of words
    %%%
    %%% Returns reduced array with size NGram 
    %%%

    fun {NgramInput InputTextSplit}
        if {Length InputTextSplit} =< {String.toInt {InfoGram get(1:$)}} then InputTextSplit
        else 
            {NgramInput InputTextSplit.2}
        end
    end
    

    %%%
    %%% Strips last characters at the end of a String that are useless for the search
    %%%   S:     String to strip chars from
    %%%
    %%% Returns truncated string
    %%%

    fun {StripLastChar S} 
        fun {StringFirstChar Str}
            case Str of nil then nil
            [] H|T then 
                if H == 32 then {StringFirstChar T}
                elseif H == 10 then {StringFirstChar T}
                else Str 
                end
            end
        end
    in 
        {List.reverse {StringFirstChar {List.reverse S}}}
    end

    %%%
    %%% Recursively launches N threads 
    %%%   Input:         Array of words of the user-input text
    %%%   Port:          Port to send computing result to
    %%%   First:         Whether it is the first thread to be launched (used to add last files if {Length Files} mod N != 0)
    %%%   N:             Thread number (counting from N to 0)
    %%%   Xn:            Variable bound when thread terminates, used to know when all threads are terminated
    %%%   Files:         Array of files to read
    %%%   FilePerThread: Amount of file to read per thread
    %%%

    proc {LaunchThread Input Port First N Xn Files FilePerThread}
        local Tree FPT Content Xni Y Z in 
            Tree = tree()
            if First then FPT = FilePerThread + {Length Files} mod N else FPT = FilePerThread end
            thread 
                local R in 
                {List.takeDrop Files FPT Y Z}
                R = {LaunchTask Y Tree Input} 
                if {Length {Arity R}} \= 0 then {Send Port R} end
                Xni = Xn
                end 
            end
            if N > 1 then
                {LaunchThread Input Port false N-1 Xni Z FilePerThread}
            else
                {Wait Xni}
                {Send Port nil}
            end
        end
   end

        %%% Lance les N threads de lecture et de parsing qui liront et traiteront tous les fichiers
        %%% Les threads de parsing envoient leur resultat au port Port
    proc {LaunchThreads Port N}
        local Files FilePerThread Xn Content Input in 
            Files = {OS.getDir {GetSentenceFolder}}
            FilePerThread = {Length Files} div N
            Xn = unit
            {InputText get(Content)}
            {InputText set(state:normal)}
            {InputText set({StripLastChar Content})}
            {InputText set(state:disabled)}
            Input = {NgramInput {List.map {String.tokens {StripLastChar Content} & } Lower}}
            {LaunchThread Input Port true N Xn Files FilePerThread}
        end
    end

    proc {IncreaseGram}
        local Inc in 
            {InfoGram get(Inc)}
            {InfoGram set(state:normal)}
            {InfoGram set(1:{Int.toString {String.toInt Inc}+1})}
            {InfoGram set(state:disabled)}
        end
    end

    proc {DecreaseGram}
        local Dec in 
            {InfoGram get(Dec)}
            if Dec == "1" then {InfoGram set(1:Dec)}
            else 
                {InfoGram set(state:normal)}
                {InfoGram set(1:{Int.toString {String.toInt Dec}-1})}
                {InfoGram set(state:disabled)}
            end
        end
    end

    proc {IncreaseLen}
        local Inc in 
            {LenOut get(Inc)}
            {LenOut set(state:normal)}
            {LenOut set(1:{Int.toString {String.toInt Inc}+1})}
            {LenOut set(state:disabled)}
        end
    end

    proc {DecreaseLen}
        local Dec in 
            {LenOut get(Dec)}
            if Dec == "1" then {LenOut set(1:Dec)}
            else 
                {LenOut set(state:normal)}
                {LenOut set(1:{Int.toString {String.toInt Dec}-1})}
                {LenOut set(state:disabled)}
            end
        end
    end
    
    proc {OpenFileExplorer}
      D E L 
    in
        D={New TkTools.dialog
            tkInit(title:   'Select new file database' 
                buttons: ['Okay' # 
                        proc {$}
                        A X B in 
                        try 
                            A = {E tkReturn(get $)}
                            B = {OS.getDir A}
                            {DatabaseList insert('end' [A])}
                            {D tkClose}
                        catch _ then
                            X={New TkTools.error tkInit(master:D text: 'Directory not found')}
                        end 
                        end 
                        'Cancel' # tkClose]
            default: 1)}
        L={New Tk.label tkInit(parent:D text:'Enter directory relative path:')}
        E={New Tk.entry tkInit(parent:D bg:white foreground:black width:20)}
        {Tk.batch [pack(L E side:left pady:2#m) focus(E)]}
    end

    %%% Fetch Tweets Folder from CLI Arguments
    %%% See the Makefile for an example of how it is called
    fun {GetSentenceFolder}
        Args = {Application.getArgs record('folder'(single type:string optional:false))}
    in
        {Property.condGet 'directory' Args.'folder'}
    end
        
    proc {Main}
        %% Fonction d'exemple qui liste tous les fichiers
        %% contenus dans le dossier passe en Argument.
        %% Inspirez vous en pour lire le contenu des fichiers
        %% se trouvant dans le dossier
        %%% N'appelez PAS cette fonction lors de la phase de
        %%% soumission !!!
        % {ListAllFiles {OS.getDir TweetsFolder}}
        
        local NbThreads Description Window in
            {Property.put print foo(width:1000 depth:1000)}  
            % Creation de l interface graphique
            Description=td(
                title: "Text predictor"
                % winfo(height:PH)
                lr(
                    text(handle:InputText width:80 height:10 background:white foreground:black wrap:word) 
                    td(
                        td(
                            button(text:"Predict" width:15 action:proc{$} X in X = {Press} end)
                            button(text:"Add database" width:15 action:OpenFileExplorer)
                            listbox(
                                init:[{String.toAtom {GetSentenceFolder}}]
                                handle:DatabaseList
                                selectmode:single
                                action:
                                    proc{$}
                                        I = {DatabaseList get(firstselection:$)}
                                        L = {DatabaseList get(1:$)}
                                        S =  {List.nth L I}
                                        AZA
                                    in 
                                        {Property.put 'directory' S}
                                    end 
                                tdscrollbar:true
                            )
                        )
                    )
                )
                text(handle:OutputText width:80 height:10 background:black foreground:white glue:nw wrap:word)
                lr(text(handle:TitleGram width:20 height:1 background:white foreground:black glue:nw wrap:word)
                button(text:"-" width:8 glue:nw action: DecreaseGram)
                text(handle:InfoGram width:11 height:1 background:white foreground:black glue:nw wrap:word)
                button(text:"+" width:8 glue:nw action: IncreaseGram) glue:nw)
                lr(text(handle:TitleLenOut width:25 height:1 background:white foreground:black glue:nw wrap:word)
                button(text:"-" width:8 glue:nw action: DecreaseLen)
                text(handle:LenOut width:6 height:1 background:white foreground:black glue:nw wrap:word)
                button(text:"+" width:8 glue:nw action: IncreaseLen) glue:nw)
                action:proc{$}{Application.exit 0} end % quitte le programme quand la fenetre est fermee
            )
            
            
            % Creation de la fenetre
            Window={QTk.build Description}
            {Window show}
        
            {InputText tk(insert 'end' "Loading... Please wait.")}
            {InputText bind(event:"<Control-s>" action:proc{$} X in X = {Press} end)} % You can also bind events
            {InputText bind(event:"<Escape>" action:proc{$}{Application.exit 0} end)}
            {InputText bind(event:"<Return>" action:proc{$} X in X = {Press} end)}
        
            {InputText set(1:"")}
            {OutputText set(state:disabled)}

            {InfoGram set(1:"2")}
            {InfoGram set(state:disabled)}

            {TitleGram set("Choose N for N-Gram:")}
            {TitleGram set(state:disabled)}

            {LenOut set(1:"1")}
            {LenOut set(state:disabled)}

            {TitleLenOut set("Choose length of output:")}
            {TitleLenOut set(state:disabled)}
        end
        %%ENDOFCODE%%
    end
    {Main}
end